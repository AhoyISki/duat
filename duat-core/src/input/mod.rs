/// An [`InputMethod`] for the [`CommandLine`]
mod commander;
/// The default [`InputMethod`], inspired by VSCode
mod default;
/// A helper struct, used in aiding "cursored" [`InputMethod`]s
mod helper;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

pub use self::{
    commander::Commander,
    default::KeyMap,
    helper::{Cursor, Cursors, EditHelper, Editor, Mover},
};
use crate::{
    data::{Context, RwData},
    text::SavedMatches,
    ui::Ui,
    widgets::{ActiveWidget, File},
};

/// An input method for an [`ActiveWidget`]
///
/// Input methods are the way that Duat decides how keys are going to
/// modify widgets.
///
/// The reasoning for them being separate from the widgets is to allow
/// an end user to replace input methods. For example, by default
/// [`File`]s are created with the [default] key mapping, inspired by
/// Visual Studio Code. But there are other input methods, such as the
/// one defined by [`duat-kak`], which is obviously inspired by the
/// [Kakoune] text editor.
///
/// In principle, there are two types of `InputMethod`, the ones with
/// [`Cursors`], and the ones without them. This is determined by the
/// [`InputMethod::cursors`] method, and will determine how the widget
/// can be modified by input. If you have an `InputMethod` that has
/// cursors, you should use the [`EditHelper`] struct in order to aide
/// in the modification of the widget's [`Text`].
///
/// If your widget/input method combo is not based on cursors. You get
/// more freedom to modify things as you wish, but you should refrain
/// from using [`Cursor`]s in order to prevent bugs.
///
/// [default]: default::KeyMap
/// [`duat-kak`]: https://docs.rs/duat-kak/latest/duat_kak/index.html
/// [Kakoune]: https://github.com/mawww/kakoune
/// [`Text`]: crate::Text
pub trait InputMethod<U>: Send + Sync + 'static
where
    U: Ui,
{
    type Widget: ActiveWidget<U>
    where
        Self: Sized;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        context: Context<U>,
    ) where
        Self: Sized;

    fn cursors(&self) -> Option<&Cursors> {
        None
    }

    /// Handles incremental search from [`IncSearch`]
    ///
    /// This should work similarly to [`send_key`], i.e., treat this
    /// like if a key was sent to the [`InputMethod`]. When
    /// implementing this, you should use [`EditHelper::new_inc`]
    /// instead of [`new`], which, when using [`Mover`]s (by
    /// [`move_main`], [`move_each`], etc), allow the use of the
    /// [`search_inc`] methods, making use of the requested
    /// incremental search.
    ///
    /// [`IncSearch`]: crate::widgets::IncSearch
    /// [`send_key`]: InputMethod::send_key
    /// [`new`]: EditHelper::new
    /// [`Mover`]: helper::Mover
    /// [`move_main`]: EditHelper::move_main
    /// [`move_each`]: EditHelper::move_each
    /// [`search_inc`]: helper::Mover::search_inc
    fn handle_inc_search(&mut self, _inc_matches: &mut SavedMatches) {
        unimplemented!(
            "This InputMethod does not handle incremental search, but a request was sent, so it \
             should do so."
        );
    }

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
    }

    fn on_unfocus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
    }
}

pub trait InputForFiles<U>: Sized + InputMethod<U, Widget = File>
where
    U: Ui,
{
    fn set_cursors(&mut self, cursors: Cursors);
}

/// Returns a sequence of [`KeyEvent`]s
///
/// This macro reads much like mappings in vim/neovim, and is meant to
/// be used in remappings. It can map keys, control sequences, and key
/// modifiers:
///
/// ```rust
/// # use duat_core::input::{keys, KeyEvent, KeyCode::*, KeyMod};
/// let macroed = keys!("abc" C-Esc A-S-"çø""[]");
/// let manual = [
///     KeyEvent::new(Char('a'), KeyMod::NONE),
///     KeyEvent::new(Char('b'), KeyMod::NONE),
///     KeyEvent::new(Char('c'), KeyMod::NONE),
///     KeyEvent::new(Esc, KeyMod::CONTROL),
///     KeyEvent::new(Char('ç'), KeyMod::SHIFT | KeyMod::ALT),
///     KeyEvent::new(Char('ø'), KeyMod::SHIFT | KeyMod::ALT),
///     KeyEvent::new(Char('['), KeyMod::NONE),
///     KeyEvent::new(Char(']'), KeyMod::NONE),
/// ];
/// assert_eq!(macroed, manual);
/// ```
///
/// A key sequence is defined by a [`&str`] literal, and
/// [modifiers](KeyMod) will affect the following key sequence or
/// control sequence. You can stack modifiers in order to create
/// sequences that require multiple modifiers.
///
/// The following control sequences are available:
/// Enter, Tab, Backspace, Del, Esc, Up, Down, Left, Right, PageUp,
/// PageDown, Home, End, Insert, F1, F2, F3, F4, F5, F6, F7, F8, F9,
/// F10, F11, F12,
///
/// The following key modifiers are available:
///
/// * `C-`: Control,
/// * `A-`: Alt,
/// * `S-`: Shift,
/// * `Meta-`: Meta,
/// * `Super-`: Super,
/// * `Hyper-`: Hyper,
///
/// This macro is statically checked, you will not be able to compile
/// a configuration crate/plugin with a faulty sequence.
pub macro keys {
    (@len Enter) => { 1 },
    (@len Tab) => { 1 },
    (@len Backspace) => { 1 },
    (@len Del) => { 1 },
    (@len Esc) => { 1 },
    (@len Up) => { 1 },
    (@len Down) => { 1 },
    (@len Left) => { 1 },
    (@len Right) => { 1 },
    (@len PageUp) => { 1 },
    (@len PageDown) => { 1 },
    (@len Home) => { 1 },
    (@len End) => { 1 },
    (@len Insert) => { 1 },
    (@len F1) => { 1 },
    (@len F2) => { 1 },
    (@len F3) => { 1 },
    (@len F4) => { 1 },
    (@len F5) => { 1 },
    (@len F6) => { 1 },
    (@len F7) => { 1 },
    (@len F8) => { 1 },
    (@len F9) => { 1 },
    (@len F10) => { 1 },
    (@len F11) => { 1 },
    (@len F12) => { 1 },
    // Otherwise, "-" would match with a "Negative" literal.
    (@len -) => { 0 },
    (@len $chars:literal) => {
        len_chars($chars)
    },
    (@len $not_chars:tt) => { 0 },

    // Special key codes.
    (@code Enter) => { KeyCode::Enter },
    (@code Tab) => { KeyCode::Tab },
    (@code Backspace) => { KeyCode::Backspace },
    (@code Del) => { KeyCode::Del },
    (@code Esc) => { KeyCode::Esc },
    (@code Up) => { KeyCode::Up },
    (@code Down) => { KeyCode::Down },
    (@code Left) => { KeyCode::Left },
    (@code Right) => { KeyCode::Right },
    (@code PageUp) => { KeyCode::PageUp },
    (@code PageDown) => { KeyCode::PageDown },
    (@code Home) => { KeyCode::Home },
    (@code End) => { KeyCode::End },
    (@code Insert) => { KeyCode::Insert },
    (@code F1) => { KeyCode::F(1) },
    (@code F2) => { KeyCode::F(2) },
    (@code F3) => { KeyCode::F(3) },
    (@code F4) => { KeyCode::F(4) },
    (@code F5) => { KeyCode::F(5) },
    (@code F6) => { KeyCode::F(6) },
    (@code F7) => { KeyCode::F(7) },
    (@code F8) => { KeyCode::F(8) },
    (@code F9) => { KeyCode::F(9) },
    (@code F10) => { KeyCode::F(10) },
    (@code F11) => { KeyCode::F(11) },
    (@code F12) => { KeyCode::F(12) },
    (@code $invalid_code:ident) => {
        compile_error!(concat!(
            "KeyCode ",
            stringify!($invalid_code),
            " is not a valid control sequence"
        ))
    },

    // Key modifiers
    (@key $keys:ident, $i:expr, $mod:expr, C- $($rest:tt)*) => {
        *$mod = $mod.union(KeyMod::CONTROL);
        keys!(@key $keys, $i, $mod, $($rest)*)
    },
    (@key $keys:ident, $i:expr, $mod:expr, A- $($rest:tt)*) => {
        *$mod = $mod.union(KeyMod::ALT);
        keys!(@key $keys, $i, $mod, $($rest)*)
    },
    (@key $keys:ident, $i:expr, $mod:expr, S- $($rest:tt)*) => {
        *$mod = $mod.union(KeyMod::SHIFT);
        keys!(@key $keys, $i, $mod, $($rest)*)
    },
    (@key $keys:ident, $i:expr, $mod:expr, Meta- $($rest:tt)*) => {
        *$mod = $mod.union(KeyMod::META);
        keys!(@key $keys, $i, $mod, $($rest)*)
    },
    (@key $keys:ident, $i:expr, $mod:expr, Super- $($rest:tt)*) => {
        *$mod = $mod.union(KeyMod::SUPER);
        keys!(@key $keys, $i, $mod, $($rest)*)
    },
    (@key $keys:ident, $i:expr, $mod:expr, Hyper- $($rest:tt)*) => {
        *$mod = $mod.union(KeyMod::HYPER);
        keys!(@key $keys, $i, $mod, $($rest)*)
    },

    // Escape codes and literals
    (@key $keys:ident, $i:expr, $mod:expr, $code:ident $($rest:tt)*) => {
        $keys[*$i] = KeyEvent::new(keys!(@code $code), *$mod);
        *$mod = KeyMod::NONE;
        *$i += 1;
        keys!(@key $keys, $i, $mod, $($rest)*)
    },
    (@key $keys:ident, $i:expr, $mod:expr, $chars:literal $($rest:tt)*) => {
        const LIT_LEN: usize = len_chars($chars);
        let added = key_events::<LIT_LEN>($chars, *$mod);
        *$mod = KeyMod::NONE;

        let mut j = 0;
        while j < added.len() {
            $keys[*$i + j] = added[j];
            j += 1
        }
        *$i += j;

        keys!(@key $keys, $i, $mod, $($rest)*)
    },

    // Anything else
    (@key $keys:ident, $i:expr, $mod:expr, $unrecognized:tt $($rest:tt)*) => {
        compile_error!(concat!(
            "Expected a string literal or control sequence, got ",
            stringify!($unrecognized)
        ));
    },
    (@key $keys:ident, $i:expr, $mod:expr, ) => {{}},

    ($($keys:tt)*) => {{
        const LEN_CHARS: usize = $(keys!(@len $keys) + )* 0;

        let mut keys = [KeyEvent::new(KeyCode::Esc, KeyMod::NONE); LEN_CHARS];
        let mut i = 0;
        let mut modif = KeyMod::NONE;

        {
        keys!(@key keys, &mut i, &mut modif, $($keys)*);
        }

        keys
    }},
}

/// This is a macro for matching keys in patterns:
///
/// Use this for quickly matching a [`KeyEvent`], probably inside an
/// [`InputMethod`]:
///
/// ```rust
/// # use duat_core::input::{KeyEvent, KeyCode, KeyMod, key};
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
/// # use duat_core::input::{KeyEvent, KeyCode, KeyMod, key};
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

/// Return the lenght of a strin in chars
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
