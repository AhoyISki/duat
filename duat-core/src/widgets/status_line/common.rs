//! Common items in a [`StatusLine`]
//!
//! These functions are meant to be simple to use, you can just put
//! them inside a [`status!`] macro, and they will be understood with
//! no other meddling.
//!
//! Examples of functions in here are [`main_fmt`], which will show a
//! formatted version of the main [`Cursor`], and [`mode_fmt`] which
//! will show a formatted version of the current [`Mode`] of Duat.
//!
//! [`StatusLine`]: super::StatusLine
//! [`status!`]: super::status
//! [`Mode`]: crate::input::Mode
use crossterm::event::{KeyEvent};

use crate::{
    context,
    data::DataMap,
    input::{self, Cursors},
    text::{Text, text},
    widgets::File,
};

/// The active mode of Duat, in lowercase
pub fn mode() -> DataMap<&'static str, String> {
    context::mode_name().map(|mode| mode.to_lowercase())
}

/// The active mode of Duat, formatted
///
/// # Formatting
///
/// ```text
/// [Mode] mode
/// ```
pub fn mode_fmt() -> DataMap<&'static str, Text> {
    context::mode_name().map(|mode| text!([Mode] { mode.to_lowercase() }))
}

/// The byte of the main cursor in the file. Indexed at 1
pub fn main_byte(cursors: &Cursors) -> usize {
    cursors.main().clone().byte() + 1
}

/// The char of the main cursor in the file. Indexed at 1
pub fn main_char(cursors: &Cursors) -> usize {
    cursors.main().clone().char() + 1
}

/// The col of the main cursor in the file. Indexed at 1
pub fn main_col(cursors: &Cursors) -> usize {
    cursors.main().clone().column() + 1
}

/// The line of the main cursor in the file. Indexed at 1
pub fn main_line(cursors: &Cursors) -> usize {
    cursors.main().clone().line() + 1
}

/// The main cursor, formatted
///
/// # Formatting
///
/// ```text
/// [Coord] col [Separator] ":" [Coord] line [Separator] "/" [Coord] lines
/// ```
pub fn main_fmt(file: &File, cursors: &Cursors) -> Text {
    text!(
        [Coord] { main_col(cursors) } [Separator] ":"
        [Coord] { main_line(cursors) } [Separator] "/"
        [Coord] { file.len_lines() }
    )
}

/// The number of cursors
pub fn selections(cursors: &Cursors) -> usize {
    cursors.len()
}

/// The number of cursors, formatted
///
/// # Formatting
///
/// ```text
/// [Selections] "1 sel"
/// ```
///
/// When there is one [`Cursor`]
///
/// ```text
/// [Selections] n " sels"
/// ```
///
/// When there are more
pub fn selections_fmt(cursors: &Cursors) -> Text {
    if cursors.len() == 1 {
        text!([Selections] "1 sel")
    } else {
        text!([Selections] { cursors.len() } "sels")
    }
}

pub fn cur_sequence_fmt() -> DataMap<Vec<KeyEvent>, Text> {
    use crossterm::event::KeyCode::*;

    let data = input::cur_sequence();
    data.map(|keys| {
        let mut seq = Text::builder();

        for key in keys {
            match key.code {
                Backspace => text!(seq, [SeqSpecialKey] "BS"),
                Enter => text!(seq, [SeqSpecialKey] "Enter"),
                Left => text!(seq, [SeqSpecialKey] "Left"),
                Right => text!(seq, [SeqSpecialKey] "Right"),
                Up => text!(seq, [SeqSpecialKey] "Up"),
                Down => text!(seq, [SeqSpecialKey] "Down"),
                Home => text!(seq, [SeqSpecialKey] "Home"),
                End => text!(seq, [SeqSpecialKey] "End"),
                PageUp => text!(seq, [SeqSpecialKey] "PageU"),
                PageDown => text!(seq, [SeqSpecialKey] "PageD"),
                Tab => text!(seq, [SeqSpecialKey] "Tab"),
                BackTab => text!(seq, [SeqSpecialKey] "BTab"),
                Delete => text!(seq, [SeqSpecialKey] "Del"),
                Insert => text!(seq, [SeqSpecialKey] "Ins"),
                F(num) => text!(seq, [SeqSpecialKey] "F" num),
                Char(char) => text!(seq, [SeqCharKey] char),
                Null => text!(seq, [SeqSpecialKey] "Null"),
                Esc => text!(seq, [SeqSpecialKey] "Esc"),
                CapsLock => text!(seq, [SeqSpecialKey] "CapsL"),
                ScrollLock => text!(seq, [SeqSpecialKey] "ScrollL"),
                NumLock => text!(seq, [SeqSpecialKey] "NumL"),
                PrintScreen => text!(seq, [SeqSpecialKey] "PrSc"),
                Pause => text!(seq, [SeqSpecialKey] "Pause"),
                Menu => text!(seq, [SeqSpecialKey] "Menu"),
                KeypadBegin => text!(seq, [SeqSpecialKey] "KeypadBeg"),
                Media(m_code) => text!(seq, [SeqSpecialKey] "Media" m_code),
                Modifier(m_code) => text!(seq, [SeqSpecialKey] "Mod" m_code),
            }
        }

        seq.finish()
    })
}
