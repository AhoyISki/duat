//! [`Mode`]s that handle user input
//!
//! Each [`Mode`] controls a specifig type of [`Widget`], and
//! switching [`Mode`]s is how one sets the current [`Widget`]. For
//! example, the [`Standard`] (like most [`Mode`]s), controls the
//! [`File`] [`Widget`]. So when you switch to that [`Mode`], you
//! return to the active [`File`] if you were focused on another
//! [`Widget`].
//!
//! Other than the [`File`] the main [`Widget`] that is controled by
//! [`Mode`]s is the [`PromptLine`]. It is an example of a [`Widget`]
//! that has many [`Mode`]s implemented for it. Chief of which is
//! [`RunCommands`], but there is also [`IncSearch`] and
//! [`PipeSelections`], and the creation of more [`Mode`]s for the
//! [`PromptLine`] is very much encouraged.
//!
//! [`Standard`]: docs.rs/duat-utils/latest/duat_utils/modes/struct.Standard.html
//! [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
//! [`RunCommands`]: docs.rs/duat-utils/latest/duat_utils/modes/struct.RunCommands.html
//! [`IncSearch`]: docs.rs/duat-utils/latest/duat_utils/modes/struct.IncSearch.html
//! [`PipeSelections`]: docs.rs/duat-utils/latest/duat_utils/modes/struct.PipeSelections.html
use core::str;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

pub use self::{
    cursor::{Cursor, Cursors, Selection, Selections, VPoint},
    remap::*,
    switch::*,
};
use crate::{
    context::Handle,
    data::Pass,
    file::File,
    ui::{Ui, Widget},
};

mod cursor;
mod remap;

mod switch {
    use std::{
        any::{Any, TypeId},
        cell::RefCell,
        sync::LazyLock,
        vec::IntoIter,
    };

    use crossterm::event::KeyEvent;

    use super::Mode;
    use crate::{
        context::{self, Handle},
        data::{Pass, RwData},
        duat_name,
        file::File,
        file_entry,
        hook::{self, KeysSent, KeysSentTo, ModeSetTo, ModeSwitched},
        main_thread_only::MainThreadOnly,
        ui::{DuatEvent, Node, Ui},
        widget_entry,
    };

    static SEND_KEYS: MainThreadOnly<RefCell<Option<KeyFn>>> =
        MainThreadOnly::new(RefCell::new(None));
    static RESET_MODE: LazyLock<MainThreadOnly<RefCell<Box<dyn FnMut(&mut Pass) -> bool>>>> =
        LazyLock::new(|| MainThreadOnly::new(RefCell::new(Box::new(|_| true))));
    static SET_MODE: MainThreadOnly<RefCell<Option<ModeFn>>> =
        MainThreadOnly::new(RefCell::new(None));
    static MODE: LazyLock<MainThreadOnly<RefCell<Box<dyn Any>>>> =
        LazyLock::new(|| MainThreadOnly::new(RefCell::new(Box::new("no mode") as Box<dyn Any>)));

    type KeyFn = fn(&mut Pass, IntoIter<KeyEvent>) -> (IntoIter<KeyEvent>, Option<ModeFn>);
    type ModeFn = Box<dyn FnOnce(&mut Pass) -> bool>;

    /// Whether or not the [`Mode`] has changed
    ///
    /// Since this function is only called by Duat, I can ensure that
    /// it will be called from the main thread, so no checks are done
    /// in that regard.
    pub(crate) fn take_set_mode_fn() -> Option<ModeFn> {
        // SAFETY: The caller of this function's caller has a Pass argument.
        unsafe { SET_MODE.get() }.borrow_mut().take()
    }

    /// Sets the new default mode
    ///
    /// This is the mode that will be set when [`mode::reset`] is
    /// called.
    ///
    /// [`mode::reset`]: reset
    pub fn set_default<M: Mode<U>, U: Ui>(mode: M) {
        context::assert_is_on_main_thread();
        *unsafe { RESET_MODE.get() }.borrow_mut() = Box::new(move |pa| {
            let mode = mode.clone();
            set_mode_fn::<M, U>(pa, mode)
        });
        let set_mode = unsafe { SET_MODE.get() };
        let prev = set_mode.take();
        *set_mode.borrow_mut() = Some(Box::new(move |pa| unsafe {
            if let Some(f) = prev {
                f(pa);
                RESET_MODE.get().borrow_mut()(pa)
            } else {
                RESET_MODE.get().borrow_mut()(pa)
            }
        }));
    }

    /// Sets the [`Mode`], switching to the appropriate [`Widget`]
    ///
    /// [`Widget`]: Mode::Widget
    pub fn set<U: Ui>(mode: impl Mode<U>) {
        context::assert_is_on_main_thread();
        let set_mode = unsafe { SET_MODE.get() };
        let prev = set_mode.take();
        *set_mode.borrow_mut() = Some(Box::new(move |pa| {
            // SAFETY: Since this function consumes a Pass, one is one being
            // used elsewhere, so I am safe to create more.
            if let Some(prev) = prev {
                prev(pa);
            }
            set_mode_fn(pa, mode)
        }));
    }

    /// Resets the mode to the [default]
    ///
    /// [default]: set_default
    pub fn reset() {
        context::assert_is_on_main_thread();
        let set_mode = unsafe { SET_MODE.get() };
        *set_mode.borrow_mut() = Some(Box::new(|pa| unsafe { RESET_MODE.get() }.borrow_mut()(pa)));
    }

    /// Switches to the [`File`] with the given name
    pub(crate) fn reset_switch_to<U: Ui>(
        pa: &Pass,
        name: impl std::fmt::Display,
        switch_window: bool,
    ) {
        let windows = context::windows::<U>().borrow();
        let name = name.to_string();
        match file_entry(pa, &windows, &name) {
            Ok((win, _, node)) => {
                if win != context::cur_window() && switch_window {
                    crate::context::sender()
                        .send(DuatEvent::SwitchWindow(win))
                        .unwrap();
                }
                let node = node.clone();
                // SAFETY: There is a Pass argument.
                let set_mode = unsafe { SET_MODE.get() };
                *set_mode.borrow_mut() = Some(Box::new(move |pa| unsafe {
                    switch_widget(pa, node);
                    RESET_MODE.get().borrow_mut()(pa)
                }))
            }
            Err(err) => context::error!("{err}"),
        }
    }

    /// Switches to a certain widget
    pub(super) fn switch_widget<U: Ui>(pa: &mut Pass, node: Node<U>) {
        if let Ok(widget) = context::cur_widget::<U>(pa) {
            widget.node(pa).on_unfocus(pa);
        }

        context::set_cur(pa, node.as_file(), node.clone());
        node.on_focus(pa);
    }

    /// Sends the [`KeyEvent`] to the active [`Mode`]
    pub(super) fn send_keys_to(pa: &mut Pass, keys: Vec<KeyEvent>) {
        let mut keys = Some(keys.into_iter());
        // SAFETY: There is a Pass argument.
        let send_keys = unsafe { SEND_KEYS.get() };
        let mut sk = send_keys.take().unwrap();

        while keys.as_ref().unwrap().len() > 0 {
            // SAFETY: Since this function is taking a Pass, but that one is
            // reliably not being used, I can safely create my own Passes.
            if let (remainder, Some(set_mode)) = sk(pa, keys.take().unwrap())
                && set_mode(pa)
            {
                keys = Some(remainder);
                sk = send_keys.take().unwrap();
            } else {
                // You probably don't really want to send the remaining keys to the
                // current mode if set_mode fails.
                break;
            }
        }

        send_keys.replace(Some(sk));
    }

    /// Inner function that sends [`KeyEvent`]s
    ///
    /// # SAFETY
    ///
    /// This function creates its own [`Pass`]es, so you must ensure
    /// that whatever is calling it is consuming a [`Pass`].
    #[allow(clippy::await_holding_refcell_ref)]
    unsafe fn send_keys_fn<M: Mode<U>, U: Ui>(
        pa: &mut Pass,
        mut keys: IntoIter<KeyEvent>,
    ) -> (IntoIter<KeyEvent>, Option<ModeFn>) {
        let Ok(node) = context::cur_widget::<U>(pa).map(|cw| cw.node(pa)) else {
            unreachable!("Early, aren't we?");
        };

        let (widget, a, _) = node.parts();
        let w = widget.try_downcast::<M::Widget>().unwrap();

        let mut sent_keys = Vec::new();

        let mode_fn = {
            // SAFETY: This function's caller has a Pass argument.
            let mut mode = unsafe { MODE.get() }.borrow_mut();
            let mode: &mut M = mode.downcast_mut().unwrap();

            let handle = Handle::from_parts(w.clone(), a.clone());
            loop {
                if let Some(mode_fn) = take_set_mode_fn() {
                    break Some(mode_fn);
                }
                let Some(key) = keys.next() else { break None };
                sent_keys.push(key);

                mode.send_key(pa, key, handle.clone());
            }
        };

        hook::trigger(
            pa,
            KeysSentTo((sent_keys.clone(), Handle::from_parts(w.clone(), a.clone()))),
        );
        hook::trigger(pa, KeysSent(sent_keys));

        (keys, mode_fn)
    }

    /// Function that sets [`Mode`]s, returns `true` if successful
    fn set_mode_fn<M: Mode<U>, U: Ui>(pa: &mut Pass, mode: M) -> bool {
        // If we are on the correct widget, no switch is needed.
        if context::cur_widget::<U>(pa).unwrap().type_id() != TypeId::of::<M::Widget>() {
            let node = {
                let windows = context::windows().borrow();
                let w = context::cur_window();
                if TypeId::of::<M::Widget>() == TypeId::of::<File<U>>() {
                    let name = context::fixed_file::<U>(pa)
                        .unwrap()
                        .read(pa, |file, _| file.name());
                    file_entry(pa, &windows, &name).map(|(.., node)| node.clone())
                } else {
                    widget_entry::<M::Widget, U>(pa, &windows, w).map(|(.., node)| node.clone())
                }
            };

            match node {
                Ok(node) => switch_widget(pa, node),
                Err(err) => {
                    context::error!("{err}");
                    return false;
                }
            };
        }

        let widget = context::cur_widget::<U>(pa).unwrap();
        widget.node(pa).parts();
        // SAFETY: Other than the internal borrow in CurWidget, no other
        // borrows happen
        let (w, area) = unsafe {
            widget
                .mutate_data_as(|w: &RwData<M::Widget>, area, _| (w.clone(), area.clone()))
                .unwrap()
        };

        let mst = ModeSetTo((Some(mode), Handle::from_parts(w.clone(), area.clone())));
        let mut mode = hook::trigger(pa, mst).0.0.take().unwrap();

        mode.on_switch(pa, Handle::from_parts(w.clone(), area.clone()));

        // SAFETY: There is a Pass argument.
        unsafe {
            crate::mode::set_send_key::<M, U>();
        }

        let (old, new) = context::raw_mode_name().write(pa, |old_mode| {
            let new_mode = duat_name::<M>();
            let ret = (*old_mode, new_mode);
            *old_mode = new_mode;
            ret
        });

        hook::trigger(pa, ModeSwitched((old, new)));

        unsafe {
            MODE.get().replace(Box::new(mode));
            SEND_KEYS
                .get()
                .replace(Some(|pa, keys| send_keys_fn::<M, U>(pa, keys)));
        }

        true
    }
}

/// A mode for a [`Widget`]
///
/// Input methods are the way that Duat decides how keys are going to
/// modify widgets.
///
/// In principle, there are two types of `Mode`, the ones which use
/// [`Selections`], and the ones which don't. In [`Mode::send_key`],
/// you receive an [`&mut Selections`], and if you're not using
/// selections, you should run [`Selections::clear`], in order to make
/// sure there are no selections.
///
/// If a [`Mode`] has selections, it _must_ use the [`EditHelper`]
/// struct in order to modify of the widget's [`Text`].
///
/// If your widget/mode combo is not based on selections. You get
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
/// # use duat_core::prelude::*;
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
///         builder.push(AlignCenter);
///
///         for i in 0..5 {
///             if let Some(active) = self.active_entry
///                 && active == i
///             {
///                 if self.selected_entry == i {
///                     builder.push(form::id_of!("MenuSelActive"))
///                 } else {
///                     builder.push(form::id_of!("MenuActive"))
///                 }
///             } else if self.selected_entry == i {
///                 builder.push(form::id_of!("MenuSelected"))
///             }
///
///             builder.push(txt!("Entry {i}"));
///         }
///
///         self.text = builder.build();
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
/// # use duat_core::prelude::*;
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # impl Menu {
/// #     fn build_text(&mut self) { todo!(); }
/// # }
/// struct MenuCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
///     type Widget = Menu;
///
///     fn build(
///         self,
///         _: &mut Pass,
///         _: Option<FileHandle<U>>,
///     ) -> (Menu, PushSpecs) {
///         let mut widget = Menu::default();
///         widget.build_text();
///
///         let specs = PushSpecs::left().with_hor_len(10.0).with_ver_len(5.0);
///
///         (widget, specs)
///     }
/// }
///
/// impl<U: Ui> Widget<U> for Menu {
///     type Cfg = MenuCfg<U>;
///
///     fn update(_: &mut Pass, handle: Handle<Self, U>) {}
///
///     fn needs_update(&self) -> bool {
///         false
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
///     fn cfg() -> Self::Cfg {
///         MenuCfg(PhantomData)
///     }
///
///     fn once() -> Result<(), Text> {
///         form::set_weak("MenuSelected", "Inactive");
///         form::set_weak("MenuActive", Form::blue());
///         form::set_weak("MenuSelActive", Form::blue());
///         Ok(())
///     }
/// }
/// ```
///
/// One thing that you'll notice is the definition of
/// [`Widget::needs_update`]. It can always return `false` because the
/// `Menu` only needs to update after keys are sent, and sent keys
/// automatically trigger [`Widget::update`].
///
/// Now, let's take a look at some [`Widget`] methods that are unique
/// to widgets that can take mode. Those are the [`on_focus`] and
/// [`on_unfocus`] methods:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::prelude::*;
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # struct MenuCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
/// #     type Widget = Menu;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Menu, PushSpecs) { todo!() }
/// # }
/// impl<U: Ui> Widget<U> for Menu {
/// #     type Cfg = MenuCfg<U>;
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!() }
/// #     fn text_mut(&mut self) -> &mut Text { todo!() }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
///     // ...
///     fn on_focus(_: &mut Pass, _: Handle<Self, U>) {
///         form::set_weak("Default.Menu", "Default");
///         form::set_weak("MenuSelected", Form::new().on_grey());
///         form::set_weak("MenuSelActive", Form::blue().on_grey());
///     }
///
///     fn on_unfocus(_: &mut Pass,_: Handle<Self, U>) {
///         form::set_weak("Default.Menu", "Inactive");
///         form::set_weak("MenuSelected", "Inactive");
///         form::set_weak("MenuSelActive", Form::blue());
///     }
/// }
/// ```
///
/// These methods can do work when the wiget is focused or unfocused.
///
/// In this case, I chose to replace the [`Form`]s with "inactive"
/// variants, to visually show when the widget is not active. Also,
/// the usage of [`form::set_weak`] means that if an end user used
/// [`form::set`] on one of these [`Form`]s, that will be prioritized.
///
/// Do also note that [`on_focus`] and [`on_unfocus`] are optional
/// methods, since a [`Widget`] doesn't necessarily need to change on
/// focus/unfocus.
///
/// Now, all that is left to do is  the `MenuInput` [`Mode`]. We just
/// need to create an empty struct and call the methods of the `Menu`:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::prelude::*;
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
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Menu, PushSpecs) { todo!() }
/// # }
/// # impl<U: Ui> Widget<U> for Menu {
/// #     type Cfg = MenuCfg<U>;
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!() }
/// #     fn text_mut(&mut self) -> &mut Text { todo!() }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// # }
/// #[derive(Clone)]
/// struct MenuInput;
///
/// impl<U: Ui> Mode<U> for MenuInput {
///     type Widget = Menu;
///
///     fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
///         use KeyCode::*;
///
///         handle.write(pa, |menu, _| {
///             match key {
///                 key!(Down) => menu.shift_selection(1),
///                 key!(Up) => menu.shift_selection(-1),
///                 key!(Enter | Tab | Char(' ')) => menu.toggle(),
///                 _ => {}
///             }
///         });
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
/// [resizing]: crate::ui::RawArea::constrain_ver
/// [`Form`]: crate::form::Form
/// [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak/index.html
/// [`form::set_weak`]: crate::form::set_weak
/// [`form::set`]: crate::form::set
/// [Kakoune]: https://github.com/mawww/kakoune
/// [`Text`]: crate::Text
/// [`&mut Selections`]: Selections
pub trait Mode<U: Ui>: Sized + Clone + 'static {
    /// The [`Widget`] that this [`Mode`] controls
    type Widget: Widget<U>;

    /// Sends a [`KeyEvent`] to this [`Mode`]
    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>);

    /// A function to trigger when switching to this [`Mode`]
    ///
    /// This can be some initial setup, like adding [`Tag`]s to the
    /// [`Text`] in order to show some important visual help for that
    /// specific [`Mode`].
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    #[allow(unused_variables)]
    fn on_switch(&mut self, pa: &mut Pass, handle: Handle<Self::Widget, U>) {}

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

    fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {
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
