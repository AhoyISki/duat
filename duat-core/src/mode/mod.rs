//! [`Mode`]s that handle user input
//!
//! Each [`Mode`] controls a specifig type of [`Widget`], and
//! switching [`Mode`]s is how one sets the current [`Widget`]. For
//! example, the [`Standard`] (like most [`Mode`]s), controls the
//! [`Buffer`] [`Widget`]. So when you switch to that [`Mode`], you
//! return to the active [`Buffer`] if you were focused on another
//! [`Widget`].
//!
//! Other than the [`Buffer`] the main [`Widget`] that is controled by
//! [`Mode`]s is the [`PromptLine`]. It is an example of a [`Widget`]
//! that has many [`Mode`]s implemented for it. Chief of which is
//! [`RunCommands`], but there is also [`IncSearch`] and
//! [`PipeSelections`], and the creation of more [`Mode`]s for the
//! [`PromptLine`] is very much encouraged.
//!
//! [`Standard`]: docs.rs/duat/latest/duat/modes/struct.Standard.html
//! [`PromptLine`]: docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
//! [`RunCommands`]: docs.rs/duat/latest/duat/modes/struct.RunCommands.html
//! [`IncSearch`]: docs.rs/duat/latest/duat/modes/struct.IncSearch.html
//! [`PipeSelections`]: docs.rs/duat/latest/duat/modes/struct.PipeSelections.html
use core::str;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

pub use crossterm::event::{KeyCode, KeyEvent, KeyEventKind};

/// Key modifiers, like Shift, Alt, Super, Shift + Alt, etc
pub type KeyMod = crossterm::event::KeyModifiers;

pub use self::{
    cursor::{Cursor, Cursors, PointOrPoints, Selection, Selections, VPoint},
    patterns::*,
    remap::*,
    switch::*,
};
use crate::{buffer::Buffer, context::Handle, data::Pass, session::DuatEvent, ui::Widget};

mod cursor;
mod patterns;
mod remap;
mod switch;

/// A blank [`Mode`], intended for plugin authors to use
///
/// This [`Mode`] just resets to the default [`Buffer`] [`Mode`], no
/// matter what key is pressed. It is instead used for mapping keys to
/// other [`Mode`]s in a common place:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # mod plugin0 {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone, Copy, Debug)]
/// #     pub struct PluginMode0;
/// #     impl<U: Ui> Mode<U> for PluginMode0 {
/// #         type Widget = Buffer<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// # mod plugin1 {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone, Copy, Debug)]
/// #     pub struct PluginMode1;
/// #     impl<U: Ui> Mode<U> for PluginMode1 {
/// #         type Widget = Buffer<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// # mod duat_kak {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone, Copy, Debug)]
/// #     pub struct Normal;
/// #     impl<U: Ui> Mode<U> for Normal {
/// #         type Widget = Buffer<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use plugin0::*;
/// use plugin1::*;
///
/// fn setup() {
///     map::<User>("0", PluginMode0);
///     map::<User>("1", PluginMode1);
///     map::<duat_kak::Normal>(" ", User);
/// }
/// ```
///
/// This way, there is a common "hub" for mappings, which plugins can
/// use in order to map their own [`Mode`]s without interfering with
/// the user's mapping.
#[derive(Clone, Copy, Debug)]
pub struct User;

impl Mode for User {
    type Widget = Buffer;

    fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget>) {
        reset::<Buffer>();
    }
}

static KEYS_WERE_SENT: AtomicUsize = AtomicUsize::new(0);

/// Wether any keys were sent via [`mode::send_keys`]
///
/// [`mode::send_keys`]: send_keys
pub(crate) fn keys_were_sent(_: &mut Pass) -> bool {
    if KEYS_WERE_SENT.load(Ordering::Relaxed) > 0 {
        KEYS_WERE_SENT.fetch_sub(1, Ordering::Relaxed);
        true
    } else {
        false
    }
}

/// Sends a sequence of [`KeyEvent`]s
///
/// Unlike with [`mode::map`] or [`mode::alias`], the sent keys are
/// allowed to be remapped to something else.
pub fn send_keys(keys: impl AsRef<str>) {
    let keys = str_to_keys(keys.as_ref());
    if !keys.is_empty() {
        KEYS_WERE_SENT.fetch_add(1, Ordering::Relaxed);
        crate::context::sender()
            .send(DuatEvent::KeysSent(keys))
            .unwrap();
    }
}

/// Wether the reverse modifier should be [alt] as opposed to [shift]
///
/// [shift]: KeyMod::SHIFT
/// [alt]: KeyMod::ALT
static ALT_IS_REFERSE: AtomicBool = AtomicBool::new(false);

/// Wether [alt] should be the reverse [modifier], instead of [shift]
///
/// On most editing models, the key that reverses actions (mostly
/// searching), is the [shift] key, (like `shift + n` to go to the
/// previouse match). In other situations though, that may not be the
/// case, like with [`duat-kak`], where that key is [alt] (for
/// consistency reasons).
///
/// Changing this key via [`set_alt_is_reverse`] does not cause any
/// internal changes in [`duat-core`] or [`duat`]. It is only
/// meant to serve as a general setting for plugins to follow.
///
/// [modifier]: KeyMod
/// [shift]: KeyMod::SHIFT
/// [alt]: KeyMod::ALT
/// [`duat-kak`]: docs.rs/duat-kak/latest/duat_kak
/// [`duat-core`]: docs.rs/duat-core/latest/duat_core
/// [`duat`]: docs.rs/duat/latest/duat
pub fn alt_is_reverse() -> bool {
    ALT_IS_REFERSE.load(Ordering::Relaxed)
}

/// Sets wether [alt] should be the reverse [modifier], instead of
/// [shift]
///
/// On most editing models, the key that reverses actions (mostly
/// searching), is the [shift] key, (like `shift + n` to go to the
/// previouse match). In other situations though, that may not be the
/// case, like with [`duat-kak`], where that key is [alt] (for
/// consistency reasons).
///
/// The value of this setting can be retrieved with
/// [`alt_is_reverse`].
///
/// [modifier]: KeyMod
/// [shift]: KeyMod::SHIFT
/// [alt]: KeyMod::ALT
/// [`duat-kak`]: docs.rs/duat-kak/latest/duat_kak
/// [`duat-core`]: docs.rs/duat-core/latest/duat_core
/// [`duat`]: docs.rs/duat/latest/duat
pub fn set_alt_is_reverse(value: bool) -> bool {
    ALT_IS_REFERSE.swap(value, Ordering::Relaxed)
}

/// A mode for a [`Widget`]
///
/// [`Mode`]s are the way that Duat decides how keys are going to
/// modify widgets.
///
/// For this example, I will create a `Menu` widget. This example
/// doesn't make use of the [`Cursor`] methods from the [`Handle`].
/// Those are methods that modify [`Selection`]s, and can use them to
/// modify the [`Text`] in a declarative fashion. For an example with
/// [`Cursor`]s, see the documentation for [`Handle`]s.
///
/// First, the [`Widget`] itself:
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
/// In this widget, the entries will be selectable via a [`Mode`], by
/// pressing the up and down keys. Let's say that said menu has five
/// entries, and one of them can be active at a time:
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
/// Now I'll implement [`Widget`]:
///
/// ```rust
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # impl Menu {
/// #     fn build_text(&mut self) { todo!(); }
/// # }
/// use duat_core::prelude::*;
///
/// struct MenuCfg;
///
/// impl<U: Ui> WidgetCfg<U> for MenuCfg {
///     type Widget = Menu;
///
///     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Menu, PushSpecs) {
///         let mut widget = Menu::default();
///         widget.build_text();
///
///         let specs = PushSpecs::left().hor_len(10.0).ver_len(5.0);
///
///         (widget, specs)
///     }
/// }
///
/// impl<U: Ui> Widget<U> for Menu {
///     type Cfg = MenuCfg;
///
///     fn update(_: &mut Pass, handle: &Handle<Self, U>) {}
///
///     fn needs_update(&self, _: &Pass) -> bool {
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
///     fn opts() -> Self::Cfg {
///         MenuCfg
///     }
///
///     fn once() -> Result<(), Text> {
///         form::set_weak("menu.active", Form::blue());
///         form::set_weak("menu.selected", "accent");
///         form::set_weak("menu.selected.active", "menu.selected");
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
/// Now, let's take a look at some [`Widget`] methods that are used
/// when the [`Widget`] is supposed to be handled by [`Mode`]s. Those
/// are the [`on_focus`] and [`on_unfocus`] methods:
///
/// ```rust
/// # use duat_core::prelude::*;
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # struct MenuCfg;
/// # impl<U: Ui> WidgetCfg<U> for MenuCfg {
/// #     type Widget = Menu;
/// #     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Menu, PushSpecs) { todo!() }
/// # }
/// impl<U: Ui> Widget<U> for Menu {
/// #     type Cfg = MenuCfg;
/// #     fn opts() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!() }
/// #     fn text_mut(&mut self) -> &mut Text { todo!() }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, _: &Handle<Self, U>) {}
/// #     fn needs_update(&self, _: &Pass) -> bool { todo!(); }
///     // ...
///     fn on_focus(_: &mut Pass, handle: &Handle<Self, U>) {
///         handle.set_mask("inactive");
///     }
///
///     fn on_unfocus(_: &mut Pass, handle: &Handle<Self, U>) {
///         handle.set_mask("inactive");
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
/// # struct MenuCfg;
/// # impl<U: Ui> WidgetCfg<U> for MenuCfg {
/// #     type Widget = Menu;
/// #     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Menu, PushSpecs) { todo!() }
/// # }
/// # impl<U: Ui> Widget<U> for Menu {
/// #     type Cfg = MenuCfg;
/// #     fn opts() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!() }
/// #     fn text_mut(&mut self) -> &mut Text { todo!() }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, _: &Handle<Self, U>) {}
/// #     fn needs_update(&self, _: &Pass) -> bool { todo!(); }
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
///         let menu = handle.write(pa);
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
/// [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak/index.html
/// [`form::set_weak`]: crate::form::set_weak
/// [`form::set`]: crate::form::set
/// [Kakoune]: https://github.com/mawww/kakoune
/// [`Text`]: crate::text::Text
/// [`&mut Selections`]: Selections
#[allow(unused_variables)]
pub trait Mode: Sized + Clone + Send + 'static {
    /// The [`Widget`] that this [`Mode`] controls
    type Widget: Widget;

    /// Sends a [`KeyEvent`] to this [`Mode`]
    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle<Self::Widget>);

    /// A function to trigger after switching to this [`Mode`]
    ///
    /// This can be some initial setup, like adding [`Tag`]s to the
    /// [`Text`] in order to show some important visual help for that
    /// specific [`Mode`].
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    fn on_switch(&mut self, pa: &mut Pass, handle: Handle<Self::Widget>) {}

    /// A function to trigger before switching off this [`Mode`]
    ///
    /// This can be some final cleanup like removing the [`Text`]
    /// entirely, for example.
    ///
    /// You might think "Wait, can't I just do these things before
    /// calling [`mode::set`] or [`mode::reset`]?". Yeah, you could,
    /// but these functions can fail, so you would do cleanup without
    /// actually leaving the [`Mode`]. [`before_exit`] _only_ triggers
    /// if the switch was actually successful.
    ///
    /// [`Text`]: crate::text::Text
    /// [`mode::set`]: set
    /// [`mode::reset`]: reset
    /// [`before_exit`]: Mode::before_exit
    fn before_exit(&mut self, pa: &mut Pass, handle: Handle<Self::Widget>) {}

    /// DO NOT IMPLEMENT THIS FUNCTION, IT IS MEANT FOR `&str` ONLY
    #[doc(hidden)]
    fn just_keys(&self) -> Option<&str> {
        None
    }
}

// This implementation exists only to allow &strs to be passed to
// remaps.
impl Mode for &'static str {
    // Doesn't matter
    type Widget = Buffer;

    fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget>) {
        unreachable!("&strs are only meant to be sent as AsGives, turning into keys");
    }

    fn just_keys(&self) -> Option<&str> {
        Some(self)
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
