use core::str;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

pub use self::{
    helper::{Cursor, Cursors, EditHelper, Editor},
    remap::*,
    switch::*,
};
use crate::{
    data::{Pass, RwData},
    file::File,
    ui::Ui,
    widget::Widget,
};

mod helper;
mod remap;

mod switch {
    use std::{
        any::{Any, TypeId},
        cell::RefCell,
        vec::IntoIter,
    };

    use crossterm::event::KeyEvent;

    use super::{Cursors, Mode};
    use crate::{
        context::{self},
        data::{Pass, RwData},
        duat_name,
        file::File,
        file_entry,
        hook::{self, KeysSent, KeysSentTo, ModeSetTo, ModeSwitched},
        session::sender,
        ui::{DuatEvent, RawArea, Ui},
        widget::{Node, Widget},
        widget_entry,
    };

    thread_local! {
        static SEND_KEYS: RefCell<Option<KeyFn>> = RefCell::new(None);
        static RESET_MODE: RefCell<Box<dyn Fn(Pass) -> bool>> =
            RefCell::new(Box::new(|_| true));
        static SET_MODE: RefCell<Option<ModeFn>> = RefCell::new(None);
        static MODE: &'static RefCell<Box<dyn Any>> =
            Box::leak(Box::new(RefCell::new(Box::new("no mode") as Box<dyn Any>)));
    }

    type KeyFn = fn(Pass, IntoIter<KeyEvent>) -> (IntoIter<KeyEvent>, Option<ModeFn>);
    type ModeFn = Box<dyn FnOnce(Pass) -> bool>;

    /// Whether or not the [`Mode`] has changed
    ///
    /// Since this function is only called by Duat, I can ensure that
    /// it will be called from the main thread, so no checks are done
    /// in that regard.
    fn take_set_mode_fn() -> Option<ModeFn> {
        SET_MODE.with(|sm| sm.borrow_mut().take())
    }

    /// Sets the new default mode
    ///
    /// This is the mode that will be set when [`mode::reset`] is
    /// called.
    ///
    /// [`mode::reset`]: reset
    pub fn set_default<M: Mode<U>, U: Ui>(mode: M) {
        // SAFETY: Since the functions here consumes a Pass, one is one
        // being used elsewhere, so I am safe to create more.
        context::assert_is_on_main_thread();
        RESET_MODE.with(|rm| {
            *rm.borrow_mut() = Box::new(move |_| {
                let mode = mode.clone();
                let pa = unsafe { Pass::new() };
                set_mode_fn::<M, U>(pa, mode)
            })
        });
        SET_MODE.with(|sm| {
            let mut set_mode = sm.borrow_mut();
            let prev = set_mode.take();
            *set_mode = Some(Box::new(move |_| {
                if let Some(f) = prev {
                    let pa = unsafe { Pass::new() };
                    f(pa);
                    let pa = unsafe { Pass::new() };
                    RESET_MODE.with(|rm| rm.borrow_mut()(pa))
                } else {
                    let pa = unsafe { Pass::new() };
                    RESET_MODE.with(|rm| rm.borrow_mut()(pa))
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
            *set_mode = Some(Box::new(move |_| {
                // SAFETY: Since this function consumes a Pass, one is one being
                // used elsewhere, so I am safe to create more.
                if let Some(prev) = prev {
                    let pa = unsafe { Pass::new() };
                    prev(pa);
                }
                let pa = unsafe { Pass::new() };
                set_mode_fn(pa, mode)
            }));
        })
    }

    /// Resets the mode to the [default]
    ///
    /// [default]: set_default
    pub fn reset() {
        context::assert_is_on_main_thread();
        SET_MODE.with(|sm| {
            *sm.borrow_mut() = Some(Box::new(|pa| RESET_MODE.with(|rm| rm.borrow_mut()(pa))))
        })
    }

    /// Switches to the [`File`] with the given name
    pub(crate) fn reset_switch_to<U: Ui>(
        pa: &Pass<'_>,
        name: impl std::fmt::Display,
        switch_window: bool,
    ) {
        let windows = context::windows::<U>().borrow();
        let name = name.to_string();
        match file_entry(pa, &windows, &name) {
            Ok((win, _, node)) => {
                if win != context::cur_window() && switch_window {
                    sender().send(DuatEvent::SwitchWindow(win)).unwrap();
                }
                let node = node.clone();
                SET_MODE.with(|sm| {
                    *sm.borrow_mut() = Some(Box::new(move |_| {
                        // SAFETY: Since this function consumes a Pass, one is one being
                        // used elsewhere, so I am safe to create more.
                        unsafe {
                            switch_widget(node);
                            let pa = Pass::new();
                            RESET_MODE.with(|rm| rm.borrow_mut()(pa))
                        }
                    }))
                });
            }
            Err(err) => context::notify(err),
        }
    }

    /// Switches to a certain widget
    ///
    /// # SAFETY
    ///
    /// This function creates its own [`Pass`]es, so you must ensure
    /// that whatever is calling it is consuming a [`Pass`].
    pub(super) unsafe fn switch_widget<U: Ui>(node: Node<U>) {
        let mut pa = unsafe { Pass::new() };

        if let Ok(widget) = context::cur_widget::<U>(&pa) {
            widget.node(&pa).on_unfocus(&mut pa);
        }

        context::set_cur(&mut pa, node.as_file(), node.clone());

        node.on_focus(&mut pa);
    }

    /// Sends the [`KeyEvent`] to the active [`Mode`]
    pub(super) fn send_keys_to(_: Pass<'_>, keys: Vec<KeyEvent>) {
        let mut keys = Some(keys.into_iter());
        let mut send_keys = SEND_KEYS.with(|sk| sk.borrow_mut().take().unwrap());

        while keys.as_ref().unwrap().len() > 0 {
            // SAFETY: Since this function is taking a Pass, but that one is
            // reliably not being used, I can safely create my own Passes.
            let pa = unsafe { Pass::new() };
            if let (remainder, Some(set_mode)) = send_keys(pa, keys.take().unwrap()) {
                keys = Some(remainder);

                if set_mode(unsafe { Pass::new() }) {
                    send_keys = SEND_KEYS.with(|sk| sk.borrow_mut().take().unwrap());
                }
            }
        }

        SEND_KEYS.with(|sk| *sk.borrow_mut() = Some(send_keys));
    }

    /// Inner function that sends [`KeyEvent`]s
    ///
    /// # SAFETY
    ///
    /// This function creates its own [`Pass`]es, so you must ensure
    /// that whatever is calling it is consuming a [`Pass`].
    #[allow(clippy::await_holding_refcell_ref)]
    unsafe fn send_keys_fn<M: Mode<U>, U: Ui>(
        mut keys: IntoIter<KeyEvent>,
    ) -> (IntoIter<KeyEvent>, Option<ModeFn>) {
        let mut pa = unsafe { Pass::new() };

        let Ok(node) = context::cur_widget::<U>(&pa).map(|cw| cw.node(&pa)) else {
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

            widget.write(&mut pa, |widget| {
                let cfg = widget.print_cfg();
                widget.text_mut().remove_cursors(area, cfg);
            });

            loop {
                if let Some(mode_fn) = take_set_mode_fn() {
                    break Some(mode_fn);
                }
                let Some(key) = keys.next() else { break None };
                sent_keys.push(key);

                // SAFETY: The other Pass will no longer be used within this block, so
                // its ok for me to create new Passes
                let pa = unsafe { Pass::new() };
                mode.send_key(pa, key, widget.clone(), area.clone());
            }
        };

        widget.write(&mut pa, |widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().add_cursors(area, cfg);
            if let Some(main) = widget.text().cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
            }
        });

        hook::trigger::<KeysSentTo<M::Widget, U>>(
            &mut pa,
            (sent_keys.clone(), widget.clone(), area.clone()),
        );
        hook::trigger::<KeysSent>(&mut pa, sent_keys);

        (keys, mode_fn)
    }

    /// Function that sets [`Mode`]s, returns `true` if successful
    fn set_mode_fn<M: Mode<U>, U: Ui>(mut pa: Pass<'_>, mode: M) -> bool {
        // If we are on the correct widget, no switch is needed.
        if context::cur_widget::<U>(&pa).unwrap().type_id() != TypeId::of::<M::Widget>() {
            let node = {
                let windows = context::windows().borrow();
                let w = context::cur_window();
                if TypeId::of::<M::Widget>() == TypeId::of::<File<U>>() {
                    let name = context::fixed_file::<U>(&pa)
                        .unwrap()
                        .read(&pa, |file, _| file.name());
                    file_entry(&pa, &windows, &name).map(|(.., node)| node.clone())
                } else {
                    widget_entry::<M::Widget, U>(&pa, &windows, w).map(|(.., node)| node.clone())
                }
            };

            match node {
                // SAFETY: Since node does not _actively_ borrow the Pass (the borrow is just
                // used to create the node, and the is dropped), pa is not borrowed here, so calling
                // this is safe.
                Ok(node) => unsafe { switch_widget(node) },
                Err(err) => {
                    context::notify(err);
                    return false;
                }
            };
        }

        let widget = context::cur_widget::<U>(&pa).unwrap();
        widget.node(&pa).parts();
        // SAFETY: Other than the internal borrow in CurWidget, no other
        // borrows happen
        let (w, area) = unsafe {
            widget
                .mutate_data_as(|w: &RwData<M::Widget>, area, _| (w.clone(), area.clone()))
                .unwrap()
        };

        let mut mode = hook::trigger::<ModeSetTo<M, U>>(&mut pa, (mode, w.clone(), area.clone()));

        w.write(&mut pa, |widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(&area, cfg);
        });

        // SAFETY: The Pass is not in use at the moment.
        mode.on_switch(unsafe { Pass::new() }, w.clone(), area.clone());

        w.write(&mut pa, |widget| {
            let cfg = widget.print_cfg();
            if let Some(main) = widget.text().cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), cfg);
            }
            widget.text_mut().add_cursors(&area, cfg);
        });

        crate::mode::set_send_key::<M, U>();

        let modes = context::raw_mode_name().write(&mut pa, |old_mode| {
            let new_mode = duat_name::<M>();
            let ret = (*old_mode, new_mode);
            *old_mode = new_mode;
            ret
        });

        hook::trigger::<ModeSwitched>(&mut pa, modes);

        MODE.with(|m| m.replace(Box::new(mode)));
        SEND_KEYS.with(|sk| {
            *sk.borrow_mut() = Some(|_, keys| {
                // SAFETY: This function is consuming a Pass, so it is valid to
                // create new Passes.
                unsafe { send_keys_fn::<M, U>(keys) }
            })
        });

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
pub trait Mode<U: Ui>: Sized + Clone + 'static {
    type Widget: Widget<U>;

    /// Sends a [`KeyEvent`] to this [`Mode`]
    #[allow(async_fn_in_trait)]
    fn send_key(&mut self, pa: Pass, key: KeyEvent, widget: RwData<Self::Widget>, area: U::Area);

    /// A function to trigger when switching to this [`Mode`]
    ///
    /// This can be some initial setup, like adding [`Tag`]s to the
    /// [`Text`] in order to show some important visual help for that
    /// specific [`Mode`].
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    #[allow(unused_variables)]
    fn on_switch(&mut self, pa: Pass, widget: RwData<Self::Widget>, area: U::Area) {}

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
    type Widget = File<U>;

    fn send_key(&mut self, _: Pass, _: KeyEvent, _: RwData<Self::Widget>, _: <U as Ui>::Area) {
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
