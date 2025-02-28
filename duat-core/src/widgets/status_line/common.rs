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
//! [`Cursor`]: crate::mode::Cursor
//! [`Mode`]: crate::mode::Mode
use crossterm::event::KeyEvent;

use crate::{
    context,
    data::DataMap,
    mode::{self, Cursors},
    text::{Text, text},
    widgets::File,
};

/// A formatting of the file's name
///
/// Includes wether or not the file is written and wether or not it
/// exists.
///
/// # Formatting
///
/// If the file's `name` was set:
///
/// ```text
/// [File] name
/// ```
///
/// If it has unwritten changes, a `[UnsavedChanges] "[+]"` will be
/// appended. If it doesn't exist, a `[NewFile] "[new file]"` will be
/// appended.
///
/// If the file's `name` was not set:
///
/// ```text
/// [ScratchFile] `name`
/// ```
pub fn file_fmt(file: &File) -> Text {
    let mut b = Text::builder();

    if let Some(name) = file.name_set() {
        text!(b, [File] name);
        if !file.exists() {
            text!(b, [NewFile] "[new file]");
        } else if file.text().has_unsaved_changes() {
            text!(b, [UnsavedChanges] "[+]");
        }
    } else {
        text!(b, [ScratchFile] { file.name() });
    }

    b.finish()
}

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
    cursors.get_main().unwrap_or_default().vcol() + 1
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
pub fn main_fmt(file: &File) -> Text {
    let cursors = file.cursors().unwrap();
    if cursors.is_empty() {
        return Text::default();
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
/// [`Cursor`]: crate::mode::Cursor
pub fn selections_fmt(cursors: &Cursors) -> Text {
    if cursors.len() == 1 {
        text!([Selections] "1 sel")
    } else {
        text!([Selections] { cursors.len() } "sels")
    }
}

/// The unprocessed mapped [keys] sent to Duat
///
/// # Formatting
///
/// For every key, if they are a normal `char`:
///
/// ```text
/// [SeqCharKey]char
/// ```
///
/// Otherwise if they are a `special` key:
///
/// ```text
/// [SeqSpecialKey]special
/// ```
/// [keys]: KeyEvent
pub fn cur_map_fmt() -> DataMap<(Vec<KeyEvent>, bool), Text> {
    let data = mode::cur_sequence();
    data.map(|(keys, is_alias)| {
        if *is_alias {
            Text::default()
        } else {
            mode::keys_to_text(keys)
        }
    })
}
