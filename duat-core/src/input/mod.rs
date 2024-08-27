mod commander;
mod default;
mod helper;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

pub macro key {
    ($code:pat) => {
        KeyEvent { code: $code, modifiers: KeyMod::NONE, .. }
    },

    ($code:pat, $modifiers:pat) => {
        KeyEvent { code: $code, modifiers: $modifiers, .. }
    }
}

#[allow(unused_attributes)]
#[allow(unused_assignments)]
pub macro keys {
    (@len Enter) => { 1 },
    (@len Space) => { 1 },
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
    // Otherwise, "-" would match with a literal.
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
        let added = keys::<LIT_LEN>($chars, *$mod);
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

const fn asdlkfj() {
    let keys = keys!("asdf" C-Enter);
}

pub use self::{
    commander::Commander,
    default::KeyMap,
    helper::{Cursor, Cursors, EditHelper},
};
use crate::{
    data::{Context, RwData},
    ui::Ui,
    widgets::{ActiveWidget, File},
};

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
        globals: Context<U>,
    ) where
        Self: Sized;

    fn cursors(&self) -> Option<&Cursors> {
        None
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

const fn len_chars(s: &str) -> usize {
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

const fn keys<const LEN: usize>(string: &str, modif: KeyMod) -> [KeyEvent; LEN] {
    let mut chars = [KeyEvent::new(KeyCode::Esc, KeyMod::NONE); LEN];

    let bytes = string.as_bytes();
    let mut i = 0;
    let mut char_i = 0;
    let mut char = [0; 4];
    let mut char_byte = 0;
    let mut max_char_byte = 0;

    while i < bytes.len() {
        if max_char_byte == 0 {
            max_char_byte = crate::text::utf8_char_width(bytes[i]);
            continue;
        }

        char[char_byte] = bytes[i];
        char_byte += 1;
        i += 1;

        if char_byte == max_char_byte {
            let u32 = u32::from_ne_bytes(char);
            let char = unsafe { char::from_u32_unchecked(u32) };
            chars[char_i] = KeyEvent::new(KeyCode::Char(char), modif);

            char_i += 1;
            char_byte = 0;
            max_char_byte = 0;
        }
    }

    chars
}
