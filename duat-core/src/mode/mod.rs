use core::str;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

pub use self::{
    helper::{Cursor, Cursors, EditHelper, Editor},
    remap::*,
    switch::*,
};
use crate::{
    data::RwData2,
    ui::Ui,
    widgets::{File, Widget},
};

mod helper;
mod remap;

mod switch {
    use std::{
        any::{Any, TypeId},
        cell::RefCell,
        pin::Pin,
        vec::IntoIter,
    };

    use crossterm::event::KeyEvent;
    use tokio::task;

    use super::{Cursors, Mode};
    use crate::{
        context::{self, CurWidget},
        data::RwData2,
        duat_name, file_entry,
        hooks::{self, KeysSent, KeysSentTo, ModeSetTo, ModeSwitched},
        session::sender,
        ui::{DuatEvent, RawArea, Ui},
        widget_entry,
        widgets::{File, Node, Widget},
    };

    thread_local! {
        static SEND_KEYS: RefCell<Option<KeyFn>> = RefCell::new(None);
        static RESET_MODE: RefCell<Box<dyn Fn() -> ModeFuture>> =
            RefCell::new(Box::new(|| Box::pin(async { true })));
        static SET_MODE: RefCell<Option<Box<dyn FnOnce() -> ModeFuture>>> = RefCell::new(None);
        static MODE: &'static RefCell<Box<dyn Any>> =
            Box::leak(Box::new(RefCell::new(Box::new("no mode") as Box<dyn Any>)));
    }

    type KeyFn = fn(
        IntoIter<KeyEvent>,
    ) -> Pin<
        Box<dyn Future<Output = (IntoIter<KeyEvent>, Option<Box<dyn FnOnce() -> ModeFuture>>)>>,
    >;
    type ModeFuture = Pin<Box<dyn Future<Output = bool>>>;

    /// Whether or not the [`Mode`] has changed
    ///
    /// Since this function is only called by Duat, I can ensure that
    /// it will be called from the main thread, so no checks are done
    /// in that regard.
    fn take_set_mode_fn() -> Option<Box<dyn FnOnce() -> ModeFuture>> {
        SET_MODE.with(|sm| sm.borrow_mut().take())
    }

    /// Sets the new default mode
    ///
    /// This is the mode that will be set when [`mode::reset`] is
    /// called.
    ///
    /// [`mode::reset`]: reset
    pub fn set_default<M: Mode<U>, U: Ui>(mode: M) {
        context::assert_is_on_main_thread();
        RESET_MODE.with(|rm| {
            *rm.borrow_mut() = Box::new(move || Box::pin(set_mode_fn::<M, U>(mode.clone())))
        });
        SET_MODE.with(|sm| {
            let mut set_mode = sm.borrow_mut();
            let prev = set_mode.take();
            *set_mode = Some(Box::new(move || {
                if let Some(f) = prev {
                    Box::pin(async move {
                        f().await;
                        RESET_MODE.with(|rm| rm.borrow_mut()()).await
                    })
                } else {
                    RESET_MODE.with(|rm| rm.borrow_mut()())
                }
            }));
        });
    }

    /// Sets the [`Mode`], switching to the appropriate [`Widget`]
    ///
    /// [`Widget`]: Mode::Widget
    pub fn set<U: Ui>(mode: impl Mode<U>) {
        context::assert_is_on_main_thread();
        SET_MODE.with(|sm| {
            let mut set_mode = sm.borrow_mut();
            let prev = set_mode.take();
            *set_mode = Some(Box::new(move || {
                if let Some(f) = prev {
                    Box::pin(async move {
                        f().await;
                        set_mode_fn(mode).await
                    })
                } else {
                    Box::pin(set_mode_fn(mode))
                }
            }));
        })
    }

    /// Resets the mode to the [default]
    ///
    /// [default]: set_default
    pub fn reset() {
        context::assert_is_on_main_thread();
        SET_MODE.with(|sm| {
            *sm.borrow_mut() = Some(Box::new(|| RESET_MODE.with(|rm| rm.borrow_mut()())))
        })
    }

    /// Switches to the file with the given name
    pub(crate) fn reset_switch_to<U: Ui>(name: impl std::fmt::Display, switch_window: bool) {
        let windows = context::windows::<U>().borrow();
        let name = name.to_string();
        match file_entry(&windows, &name) {
            Ok((win, _, node)) => {
                if win != context::cur_window() && switch_window {
                    sender().send(DuatEvent::SwitchWindow(win)).unwrap();
                }
                let node = node.clone();
                SET_MODE.with(|sm| {
                    *sm.borrow_mut() = Some(Box::new(move || {
                        Box::pin(async move {
                            switch_widget(node).await;
                            RESET_MODE.with(|rm| rm.borrow_mut()()).await
                        })
                    }))
                });
            }
            Err(err) => context::notify(err),
        }
    }

    /// Switches to a certain widget
    pub(super) async fn switch_widget<U: Ui>(node: Node<U>) {
        if let Ok(widget) = context::cur_widget::<U>() {
            task::spawn_local(widget.node().on_unfocus());
        }

        context::set_cur(node.as_file(), node.clone());

        node.on_focus().await;
    }

    /// Sends the [`KeyEvent`] to the active [`Mode`]
    pub(super) async fn send_keys_to(keys: Vec<KeyEvent>) {
        let mut keys = Some(keys.into_iter());
        let mut send_keys = SEND_KEYS.with(|sk| sk.borrow_mut().take().unwrap());
        while keys.as_ref().unwrap().len() > 0 {
            if let (remainder, Some(set_mode)) = send_keys(keys.take().unwrap()).await {
                keys = Some(remainder);
                if set_mode().await {
                    send_keys = SEND_KEYS.with(|sk| sk.borrow_mut().take().unwrap());
                }
            }
        }
        SEND_KEYS.with(|sk| *sk.borrow_mut() = Some(send_keys));
    }

    /// Inner function that sends [`KeyEvent`]s
    #[allow(clippy::await_holding_refcell_ref)]
    async fn send_keys_fn<M: Mode<U>, U: Ui>(
        mut keys: IntoIter<KeyEvent>,
    ) -> (IntoIter<KeyEvent>, Option<Box<dyn FnOnce() -> ModeFuture>>) {
        let Ok(node) = context::cur_widget::<U>().map(CurWidget::node) else {
            unreachable!("Early, aren't we?");
        };

        let (widget, area, _) = node.parts();
        let widget = widget.try_downcast::<M::Widget>().unwrap();

        let mut sent_keys = Vec::new();
        let mode_fn = {
            // We can hold across .awaits because the mode can only be accessed by
            // this function and the mode switching functions, and once we get a
            // new mode switch function, we immediately break out of this block,
            // dropping the reference to MODE.
            let mut mode = MODE.with(|m| *m).borrow_mut();
            let mode: &mut M = mode.downcast_mut().unwrap();

            widget.write(|widget| {
                let cfg = widget.print_cfg();
                widget.text_mut().remove_cursors(area, cfg);
            });

            loop {
                if let Some(mode_fn) = take_set_mode_fn() {
                    break Some(mode_fn);
                }
                let Some(key) = keys.next() else { break None };
                sent_keys.push(key);
                mode.send_key(key, widget.clone(), area).await;
            }
        };

        widget.write(|widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().add_cursors(area, cfg);
            if let Some(main) = widget.cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
            }
        });

        tokio::task::spawn_local(hooks::trigger::<KeysSentTo<M::Widget, U>>((
            sent_keys.clone(),
            widget.clone(),
            area.clone(),
        )));
        tokio::task::spawn_local(hooks::trigger::<KeysSent>(sent_keys));

        (keys, mode_fn)
    }

    /// Function that sets [`Mode`]s, returns `true` if successful
    async fn set_mode_fn<M: Mode<U>, U: Ui>(mode: M) -> bool {
        // If we are on the correct widget, no switch is needed.
        if context::cur_widget::<U>().unwrap().type_id() != TypeId::of::<M::Widget>() {
            let node = {
                let windows = context::windows().borrow();
                let w = context::cur_window();
                if TypeId::of::<M::Widget>() == TypeId::of::<File>() {
                    let name = context::fixed_file::<U>()
                        .unwrap()
                        .read(|file, _| file.name());
                    file_entry(&windows, &name).map(|(.., node)| node.clone())
                } else {
                    widget_entry::<M::Widget, U>(&windows, w).map(|(.., node)| node.clone())
                }
            };

            match node {
                Ok(node) => switch_widget(node).await,
                Err(err) => {
                    context::notify(err);
                    return false;
                }
            };
        }

        let widget = context::cur_widget::<U>().unwrap();
        widget.node().parts();
        let (w, area) = widget
            .mutate_data_as(|w: &RwData2<M::Widget>, area, _| (w.clone(), area.clone()))
            .unwrap();

        let mut mode = hooks::trigger::<ModeSetTo<M, U>>((mode, w.clone(), area.clone())).await;

        w.write(|widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(&area, cfg);

            mode.on_switch(widget, &area);

            let cfg = widget.print_cfg();
            if let Some(main) = widget.cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), cfg);
            }
            widget.text_mut().add_cursors(&area, cfg);
        });

        crate::mode::set_send_key::<M, U>();

        context::raw_mode_name().write(|old_mode| {
            let new_mode = duat_name::<M>();
            tokio::task::spawn_local(hooks::trigger::<ModeSwitched>((*old_mode, new_mode)));
            *old_mode = new_mode;
        });

        MODE.with(|m| m.replace(Box::new(mode)));
        SEND_KEYS.with(|sk| *sk.borrow_mut() = Some(|keys| Box::pin(send_keys_fn::<M, U>(keys))));
        true
    }
}

/// A mode for a [`Widget`]
///
/// Input methods are the way that Duat decides how keys are going to
/// modify widgets.
///
/// In principle, there are two types of `Mode`, the ones which use
/// [`Cursors`], and the ones which don't. In [`Mode::send_key`], you
/// receive an [`&mut Cursors`], and if you're not using cursors, you
/// should run [`Cursors::clear`], in order to make sure there are no
/// cursors.
///
/// If a [`Mode`] has cursors, it _must_ use the [`EditHelper`] struct
/// in order to modify of the widget's [`Text`].
///
/// If your widget/mode combo is not based on cursors. You get
/// more freedom to modify things as you wish, but you should refrain
/// from using [`Cursor`]s in order to prevent bugs.
///
/// For this example, I will create a `Menu` widget, which is not
/// supposed to have [`Cursor`]s. For an example with [`Cursor`]s, see
/// [`EditHelper`]:
///
/// ```rust
/// # use duat_core::text::Text;
/// #[derive(Default)]
/// struct Menu {
///     text: Text,
///     selected_entry: usize,
///     active_etry: Option<usize>,
/// }
/// ```
/// In this widget, I will create a menu whose entries can be selected
/// by an [`Mode`].
///
/// Let's say that said menu has five entries, and one of them can be
/// active at a time:
///
/// ```rust
/// # #![feature(let_chains)]
/// # use duat_core::text::{Text, text, AlignCenter};
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// impl Menu {
///     pub fn shift_selection(&mut self, shift: i32) {
///         let selected = self.selected_entry as i32 + shift;
///         self.selected_entry = if selected < 0 {
///             4
///         } else if selected > 4 {
///             0
///         } else {
///             selected as usize
///         };
///     }
///
///     pub fn toggle(&mut self) {
///         self.active_entry = match self.active_entry {
///             Some(entry) if entry == self.selected_entry => None,
///             Some(_) | None => Some(self.selected_entry),
///         };
///     }
///
///     fn build_text(&mut self) {
///         let mut builder = Text::builder();
///         text!(builder, AlignCenter);
///
///         for i in 0..5 {
///             if let Some(active) = self.active_entry
///                 && active == i
///             {
///                 if self.selected_entry == i {
///                     text!(builder, [MenuSelActive])
///                 } else {
///                     text!(builder, [MenuActive])
///                 }
///             } else if self.selected_entry == i {
///                 text!(builder, [MenuSelected]);
///             } else {
///                 text!(builder, [MenuInactive]);
///             }
///
///             text!(builder, "Entry " i);
///         }
///
///         self.text = builder.finish();
///     }
/// }
/// ```
///
/// By making `shift_selection` and `toggle` `pub`, I can allow an end
/// user to create their own [`Mode`] for this widget.
///
/// Let's say that I have created an [`Mode`] `MenuInput` for
/// the `Menu`. This mode is actually the one that is documented on
/// the documentation entry for [`Mode`], you can check it out next,
/// to see how that was handled.
///
/// Now I'll implement [`Widget`]:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::{
/// #     mode::{Mode, KeyEvent}, form::{self, Form},
/// #     text::{text, Text}, ui::{PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
/// # };
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # impl Menu {
/// #     fn build_text(&mut self) {
/// #         todo!();
/// #     }
/// # }
/// struct MenuCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
///     type Widget = Menu;
///
///     fn build(self, on_file: bool) -> (Menu, impl Fn() -> bool + 'static, PushSpecs) {
///         let checker = || false;
///
///         let mut widget = Menu::default();
///         widget.build_text();
///
///         let specs = PushSpecs::left().with_hor_len(10.0).with_ver_len(5.0);
///
///         (widget, checker, specs)
///     }
/// }
///
/// impl<U: Ui> Widget<U> for Menu {
///     type Cfg = MenuCfg<U>;
///
///     fn cfg() -> Self::Cfg {
///         MenuCfg(PhantomData)
///     }
///
///     fn text(&self) -> &Text {
///         &self.text
///     }
///   
///     fn text_mut(&mut self) -> &mut Text {
///         &mut self.text
///     }
///
///     fn once() -> Result<(), Text> {
///         form::set_weak("MenuInactive", "Inactive");
///         form::set_weak("MenuSelected", "Inactive");
///         form::set_weak("MenuActive", Form::blue());
///         form::set_weak("MenuSelActive", Form::blue());
///         Ok(())
///     }
/// }
/// ```
///
/// We can use `let checker = || false` here, since active [`Widget`]s
/// get automatically updated whenever they are focused or a key is
/// sent.
///
/// Now, let's take a look at some [`Widget`] methods that are unique
/// to widgets that can take mode. Those are the [`on_focus`] and
/// [`on_unfocus`] methods:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::{
/// #     data::RwData, form::{self, Form}, text::{text, Text}, ui::{PushSpecs, Ui},
/// #     widgets::{Widget, WidgetCfg},
/// # };
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # struct MenuCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
/// #     type Widget = Menu;
/// #     fn build(self, on_file: bool) -> (Menu, impl Fn() -> bool + 'static, PushSpecs) {
/// #         (Menu::default(), || false, PushSpecs::left())
/// #     }
/// # }
/// impl<U: Ui> Widget<U> for Menu {
/// #     type Cfg = MenuCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         MenuCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.text
/// #     }
/// #     fn once() -> Result<(), Text> {
/// #         Ok(())
/// #     }
/// #     fn text_mut(&mut self) -> &mut Text {
/// #         &mut self.text
/// #     }
///     // ...
///     fn on_focus(&mut self, _area: &U::Area) {
///         form::set_weak("MenuInactive", "Default");
///         form::set_weak("MenuSelected", Form::new().on_grey());
///         form::set_weak("MenuSelActive", Form::blue().on_grey());
///     }
///
///     fn on_unfocus(&mut self, _area: &U::Area) {
///         form::set_weak("MenuInactive", "Inactive");
///         form::set_weak("MenuSelected", "Inactive");
///         form::set_weak("MenuSelActive", Form::blue());
///     }
/// }
/// ```
///
/// These methods can do work when the wiget is focused or unfocused.
///
/// In this case, I chose to replace the [`Form`]s with "inactive"
/// variants, to visually show when the widget is not active.
///
/// Do also note that [`on_focus`] and [`on_unfocus`] are optional
/// methods, since a change on focus is not always necessary.
///
/// Now, all that is left to do is  the `MenuInput` [`Mode`]. We just
/// need to create an empty struct and call the methods of the `Menu`:
///
/// ```rust
/// # #![feature(let_chains)]
/// # use std::marker::PhantomData;
/// # use duat_core::{
/// #     mode::{key, Cursors, Mode, KeyCode, KeyEvent}, form::{self, Form},
/// #     text::{text, Text}, ui::{PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
/// # };
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # impl Menu {
/// #     pub fn shift_selection(&mut self, shift: i32) {}
/// #     pub fn toggle(&mut self) {}
/// #     fn build_text(&mut self) {}
/// # }
/// # struct MenuCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
/// #     type Widget = Menu;
/// #     fn build(self, on_file: bool) -> (Menu, impl Fn() -> bool + 'static, PushSpecs) {
/// #         (Menu::default(), || false, PushSpecs::left())
/// #     }
/// # }
/// # impl<U: Ui> Widget<U> for Menu {
/// #     type Cfg = MenuCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         MenuCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.text
/// #     }
/// #     fn once() -> Result<(), Text> {
/// #         Ok(())
/// #     }
/// #     fn text_mut(&mut self) -> &mut Text {
/// #         &mut self.text
/// #     }
/// # }
/// #[derive(Clone)]
/// struct MenuInput;
///
/// impl<U: Ui> Mode<U> for MenuInput {
///     type Widget = Menu;
///
///     fn send_key(&mut self, key: KeyEvent, menu: &mut Menu, area: &U::Area) {
///         use KeyCode::*;
///         
///         match key {
///             key!(Down) => menu.shift_selection(1),
///             key!(Up) => menu.shift_selection(-1),
///             key!(Enter | Tab | Char(' ')) => menu.toggle(),
///             _ => {}
///         }
///     }
/// }
/// ```
/// Notice the [`key!`] macro. This macro is useful for pattern
/// matching [`KeyEvent`]s on [`Mode`]s.
///
/// [`Cursor`]: crate::mode::Cursor
/// [`print`]: Widget::print
/// [`on_focus`]: Widget::on_focus
/// [`on_unfocus`]: Widget::on_unfocus
/// [resizing]: crate::ui::Area::constrain_ver
/// [`Form`]: crate::form::Form
/// [default]: self::regular::Regular
/// [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak/index.html
/// [Kakoune]: https://github.com/mawww/kakoune
/// [`Text`]: crate::Text
/// [`&mut Cursors`]: Cursors
pub trait Mode<U: Ui>: Sized + Clone + Send + 'static {
    type Widget: Widget<U>;

    /// Sends a [`KeyEvent`] to this [`Mode`]
    #[allow(async_fn_in_trait)]
    async fn send_key(&mut self, key: KeyEvent, widget: RwData2<Self::Widget>, area: &U::Area);

    /// A function to trigger when switching to this [`Mode`]
    ///
    /// This can be some initial setup, like adding [`Tag`]s to the
    /// [`Text`] in order to show some important visual help for that
    /// specific [`Mode`].
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    #[allow(unused)]
    fn on_switch(&mut self, widget: &mut Self::Widget, area: &U::Area) {}

    /// DO NOT IMPLEMENT THIS FUNCTION, IT IS MEANT FOR `&str` ONLY
    #[doc(hidden)]
    fn just_keys(&self) -> Option<&str> {
        None
    }
}

// This implementation exists only to allow &strs to be passed to
// remaps.
impl<U: Ui> Mode<U> for &'static str {
    // Doesn't matter
    type Widget = File;

    async fn send_key(&mut self, _: KeyEvent, _: RwData2<File>, _: &<U as Ui>::Area) {
        unreachable!("&strs are only meant to be sent as AsGives, turning into keys");
    }

    fn just_keys(&self) -> Option<&str> {
        Some(self)
    }
}

/// This is a macro for matching keys in patterns:
///
/// Use this for quickly matching a [`KeyEvent`], probably inside an
/// [`Mode`]:
///
/// ```rust
/// # use duat_core::mode::{KeyEvent, KeyCode, KeyMod, key};
/// # fn test(key: KeyEvent) {
/// if let key!(KeyCode::Char('o'), KeyMod::NONE) = key { /* Code */ }
/// // as opposed to
/// if let KeyEvent {
///     code: KeyCode::Char('c'),
///     modifiers: KeyMod::NONE,
///     ..
/// } = key
/// { /* Code */ }
/// # }
/// ```
///
/// You can also assign while matching:
///
/// ```rust
/// # use duat_core::mode::{KeyEvent, KeyCode, KeyMod, key};
/// # fn test(key: KeyEvent) {
/// if let key!(code, KeyMod::SHIFT | KeyMod::ALT) = key { /* Code */ }
/// // as opposed to
/// if let KeyEvent {
///     code,
///     modifiers: KeyMod::SHIFT | KeyMod::ALT,
///     ..
/// } = key
/// { /* Code */ }
/// # }
/// ```
pub macro key {
    ($code:pat) => {
        KeyEvent { code: $code, modifiers: KeyMod::NONE, .. }
    },

    ($code:pat, $modifiers:pat) => {
        KeyEvent { code: $code, modifiers: $modifiers, .. }
    }
}

/// Return the length of a strin in chars
#[allow(dead_code)]
#[doc(hidden)]
pub const fn len_chars(s: &str) -> usize {
    let mut i = 0;
    let b = s.as_bytes();
    let mut nchars = 0;
    while i < b.len() {
        if crate::text::utf8_char_width(b[i]) > 0 {
            nchars += 1;
        }
        i += 1;
    }
    nchars
}

/// Maps each [`char`] in an `&str` to a [`KeyEvent`]
#[allow(dead_code)]
#[doc(hidden)]
pub fn key_events<const LEN: usize>(str: &str, modif: KeyMod) -> [KeyEvent; LEN] {
    let mut events = [KeyEvent::new(KeyCode::Esc, KeyMod::NONE); LEN];

    for (event, char) in events.iter_mut().zip(str.chars()) {
        *event = KeyEvent::new(KeyCode::Char(char), modif)
    }

    events
}
