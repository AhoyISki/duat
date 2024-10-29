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
//! [`Cursor`]: crate::input::Cursor
//! [`Mode`]: crate::input::Mode
use crossterm::event::KeyEvent;

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
    cursors.get_main().unwrap_or_default().byte() + 1
}

/// The char of the main cursor in the file. Indexed at 1
pub fn main_char(cursors: &Cursors) -> usize {
    cursors.get_main().unwrap_or_default().char() + 1
}

/// The col of the main cursor in the file. Indexed at 1
pub fn main_col(cursors: &Cursors) -> usize {
    cursors.get_main().unwrap_or_default().column() + 1
}

/// The line of the main cursor in the file. Indexed at 1
pub fn main_line(cursors: &Cursors) -> usize {
    cursors.get_main().unwrap_or_default().line() + 1
}

/// The main cursor, formatted
///
/// # Formatting
///
/// ```text
/// [Coord] col [Separator] ":" [Coord] line [Separator] "/" [Coord] lines
/// ```
pub fn main_fmt(file: &File, cursors: &Cursors) -> Text {
    if cursors.is_empty() {
        return Text::new();
    }
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
/// When there is more than one [`Cursor`]
///
/// [`Cursor`]: crate::input::Cursor
pub fn selections_fmt(cursors: &Cursors) -> Text {
    if cursors.len() == 1 {
        text!([Selections] "1 sel")
    } else {
        text!([Selections] { cursors.len() } "sels")
    }
}

pub fn cur_map_fmt() -> DataMap<(Vec<KeyEvent>, bool), Text> {
    let data = input::cur_sequence();
    data.map(|(keys, is_alias)| {
        if *is_alias {
            Text::new()
        } else {
            input::keys_to_text(keys)
        }
    })
}
