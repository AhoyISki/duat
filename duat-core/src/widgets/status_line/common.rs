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
use crate::{
    context,
    data::DataMap,
    input::{Cursor, Cursors},
    text::{Text, text},
    widgets::File,
};

pub fn mode() -> DataMap<&'static str, String> {
    context::mode_name().map(|mode| mode.to_lowercase())
}

pub fn mode_fmt() -> DataMap<&'static str, Text> {
    context::mode_name().map(|mode| text!([Mode] { mode.to_lowercase() }))
}

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte(cursors: &Cursors) -> usize {
    main_cursor(cursors).byte() + 1
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char(cursors: &Cursors) -> usize {
    main_cursor(cursors).char() + 1
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col(cursors: &Cursors) -> usize {
    main_cursor(cursors).column() + 1
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line(cursors: &Cursors) -> usize {
    main_cursor(cursors).line() + 1
}

/// A convenience function that prints the main cursor alongside the
/// lenght of the file, in lines.
pub fn main_fmt(file: &File, cursors: &Cursors) -> Text {
    text!(
        [Coord] { main_col(cursors) } [Separator] ":"
        [Coord] { main_line(cursors) } [Separator] "/"
        [Coord] { file.len_lines() }
    )
}

pub fn selections(cursors: &Cursors) -> usize {
    cursors.len()
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt(cursors: &Cursors) -> Text {
    if cursors.len() == 1 {
        text!([Selections] "1 sel")
    } else {
        text!([Selections] { cursors.len() } "sels")
    }
}

fn main_cursor(cursors: &Cursors) -> Cursor {
    cursors.main().clone()
}
