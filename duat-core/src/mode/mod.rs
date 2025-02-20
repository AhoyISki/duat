use core::str;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

pub use self::{
    commander::Command,
    helper::{Cursor, Cursors, EditHelper, Editor, Mover},
    inc_search::{ExtendFwd, ExtendRev, Fwd, IncSearcher, Rev},
    regular::Regular,
    remap::*,
    switch::*,
};
use crate::{data::RwData, ui::Ui, widgets::Widget};

mod commander;
mod helper;
mod inc_search;
mod regular;
mod remap;

mod switch {
    use std::{
        any::TypeId,
        sync::{
            Arc, LazyLock,
            atomic::{AtomicBool, Ordering},
        },
        vec::IntoIter,
    };

    use crossterm::event::KeyEvent;
    use parking_lot::Mutex;

    use super::Mode;
    use crate::{
        context, duat_name, file_entry,
        hooks::{self, KeySent, KeySentTo, ModeSwitched},
        ui::{Ui, Window},
        widget_entry,
        widgets::{CmdLine, CmdLineMode, File, Node},
    };

    static PRINTING_IS_STOPPED: AtomicBool = AtomicBool::new(false);
    static SEND_KEY: LazyLock<Mutex<Box<KeyFn>>> = LazyLock::new(|| Mutex::new(Box::new(|_| None)));
    static RESET_MODE: LazyLock<Mutex<Arc<dyn Fn() + Send + Sync>>> =
        LazyLock::new(|| Mutex::new(Arc::new(|| {})));
    static SET_MODE: Mutex<Option<Box<dyn FnOnce() + Send + Sync>>> = Mutex::new(None);

    type KeyFn =
        dyn FnMut(&mut IntoIter<KeyEvent>) -> Option<Box<dyn FnOnce() + Send + Sync>> + Send + Sync;

    /// Whether or not the [`Mode`] has changed
    pub fn was_set() -> Option<Box<dyn FnOnce() + Send + Sync>> {
        SET_MODE.lock().take()
    }

    /// Sets the new default mode
    ///
    /// This is the mode that will be set when [`mode::reset`] is
    /// called.
    ///
    /// [`mode::reset`]: reset
    pub fn set_default<M: Mode<U>, U: Ui>(mode: M) {
        *RESET_MODE.lock() = Arc::new(move || set_mode_fn::<M, U>(mode.clone()));
        let mut set_mode = SET_MODE.lock();
        let prev = set_mode.take();
        *set_mode = Some(Box::new(move || {
            if let Some(f) = prev {
                f()
            }
            RESET_MODE.lock()()
        }));
    }

    /// Sets the [`Mode`], switching to the appropriate [`Widget`]
    ///
    /// [`Widget`]: Mode::Widget
    pub fn set<U: Ui>(mode: impl Mode<U>) {
        let mut set_mode = SET_MODE.lock();
        let prev = set_mode.take();
        *set_mode = Some(Box::new(move || {
            if let Some(f) = prev {
                f()
            }
            set_mode_fn(mode)
        }));
    }

    /// Resets the mode to the [default]
    ///
    /// [default]: set_default
    pub fn reset() {
        *SET_MODE.lock() = Some(Box::new(|| RESET_MODE.lock()()))
    }

    /// Sets the [`CmdLineMode`]
    pub fn set_cmd<U: Ui>(mode: impl CmdLineMode<U>) {
        let Ok(cur_file) = context::cur_file::<U>() else {
            return;
        };

        if let Some(node) = cur_file.get_related_widget::<CmdLine<U>>() {
            node.try_downcast::<CmdLine<U>>()
                .unwrap()
                .write()
                .set_mode(mode);
        } else {
            let windows = context::windows::<U>().read();
            let w = context::cur_window();
            let cur_window = &windows[w];

            let mut widgets = {
                let previous = windows[..w].iter().flat_map(Window::nodes);
                let following = windows[(w + 1)..].iter().flat_map(Window::nodes);
                cur_window.nodes().chain(previous).chain(following)
            };

            if let Some(cmd_line) = widgets.find_map(|node| {
                node.data_is::<CmdLine<U>>()
                    .then(|| node.try_downcast::<CmdLine<U>>().unwrap())
            }) {
                cmd_line.write().set_mode(mode)
            }
        }
    }

    /// Switches to the file with the given name
    pub fn reset_switch_to<U: Ui>(name: impl std::fmt::Display) {
        let windows = context::windows::<U>().read();
        let name = name.to_string();
        match file_entry(&windows, &name) {
            Ok((_, node)) => {
                let node = node.clone();
                *SET_MODE.lock() = Some(Box::new(move || {
                    switch_widget(node);
                    RESET_MODE.lock().clone()()
                }));
            }
            Err(err) => context::notify(err),
        }
    }

    /// Whether or not printing has been stopped
    ///
    /// This is done when sending multiple keys at the same time
    pub(crate) fn has_printing_stopped() -> bool {
        PRINTING_IS_STOPPED.load(Ordering::Acquire)
    }

    /// Stop printing
    pub(super) fn stop_printing() {
        PRINTING_IS_STOPPED.store(true, Ordering::Release);
    }

    /// Resume printing
    pub(super) fn resume_printing() {
        PRINTING_IS_STOPPED.store(false, Ordering::Release);
    }

    /// Switches to a certain widget
    pub(super) fn switch_widget<U: Ui>(node: Node<U>) {
        if let Ok(widget) = context::cur_widget::<U>() {
            widget.node().on_unfocus();
        }

        context::set_cur(node.as_file(), node.clone());

        node.on_focus();
    }

    /// Sends the [`KeyEvent`] to the active [`Mode`]
    pub(super) fn send_keys_to(keys: Vec<KeyEvent>) {
        let mut keys = keys.into_iter();
        let mut send_key = std::mem::replace(&mut *SEND_KEY.lock(), Box::new(|_| None));
        while !keys.is_empty() {
            if let Some(set_mode) = send_key(&mut keys) {
                set_mode();
                send_key = std::mem::replace(&mut *SEND_KEY.lock(), Box::new(|_| None));
            }
        }
        *SEND_KEY.lock() = send_key;
    }

    /// Inner function that sends [`KeyEvent`]s
    fn send_keys_fn<M: Mode<U>, U: Ui>(
        mode: &mut M,
        keys: &mut IntoIter<KeyEvent>,
    ) -> Option<Box<dyn FnOnce() + Send + Sync>> {
        let Ok(widget) = context::cur_widget::<U>() else {
            return None;
        };

        widget.mutate_data(|dyn_widget, area| {
            let widget = dyn_widget.try_downcast().unwrap();
            for key in keys {
                hooks::trigger::<KeySent<U>>((key, dyn_widget.clone()));
                hooks::trigger::<KeySentTo<M::Widget, U>>((key, widget.clone()));
                mode.send_key(key, &widget, area);
                if let Some(mode) = was_set() {
                    return Some(mode);
                }
            }
            None
        })
    }

    /// Inner function that sets [`Mode`]s
    fn set_mode_fn<M: Mode<U>, U: Ui>(mut mode: M) {
        // If we are on the correct widget, no switch is needed.
        if context::cur_widget::<U>().unwrap().type_id() != TypeId::of::<M::Widget>() {
            let windows = context::windows().read();
            let w = context::cur_window();
            let entry = if TypeId::of::<M::Widget>() == TypeId::of::<File>() {
                let name = context::cur_file::<U>().unwrap().name();
                file_entry(&windows, &name)
            } else {
                widget_entry::<M::Widget, U>(&windows, w)
            };

            match entry {
                Ok((_, node)) => switch_widget(node.clone()),
                Err(err) => {
                    context::notify(err);
                    return;
                }
            };
        }

        let widget = context::cur_widget::<U>().unwrap();
        widget.mutate_data_as::<M::Widget, ()>(|w, a| {
            mode.on_switch(w, a);
        });

        crate::mode::set_send_key::<M, U>();

        context::mode_name().mutate(|mode| {
            let new_mode = duat_name::<M>();
            hooks::trigger::<ModeSwitched>((mode, new_mode));
            *mode = new_mode;
        });

        *SEND_KEY.lock() = Box::new(move |keys| send_keys_fn::<M, U>(&mut mode, keys));
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
/// #     data::RwData, mode::{Mode, KeyEvent}, form::{self, Form},
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
///     fn once() -> Result<(), duat_core::Error<()>> {
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
/// #     fn once() -> Result<(), duat_core::Error<()>> {
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
/// #     data::RwData, mode::{key, Cursors, Mode, KeyCode, KeyEvent}, form::{self, Form},
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
/// #     fn once() -> Result<(), duat_core::Error<()>> {
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
///     fn send_key(&mut self, key: KeyEvent, widget: &RwData<Menu>, area: &U::Area) {
///         use KeyCode::*;
///         let mut menu = widget.write();
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
/// [resizing]: Area::constrain_ver
/// [`Form`]: crate::form::Form
/// [default]: default::KeyMap
/// [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak/index.html
/// [Kakoune]: https://github.com/mawww/kakoune
/// [`Text`]: crate::Text
/// [`&mut Cursors`]: Cursors
pub trait Mode<U: Ui>: Sized + Clone + Send + Sync + 'static {
    type Widget: Widget<U>;

    /// Sends a [`KeyEvent`] to this [`Mode`]
    fn send_key(&mut self, key: KeyEvent, widget: &RwData<Self::Widget>, area: &U::Area);

    /// A function to trigger when switching to this [`Mode`]
    ///
    /// This can be some initial setup, like adding [`Tag`]s to the
    /// [`Text`] in order to show some important visual help for that
    /// specific [`Mode`].
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    #[allow(unused)]
    fn on_switch(&mut self, widget: &RwData<Self::Widget>, area: &U::Area) {}

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
    type Widget = crate::widgets::File;

    fn send_key(&mut self, _: KeyEvent, _: &RwData<Self::Widget>, _: &<U as Ui>::Area) {
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
        KeyEvent { code: $code, modifiers: KeyMod::NONE | KeyMod::SHIFT, .. }
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
