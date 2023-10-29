use parsec_core::{
    input::{Cursors, InputMethod},
    position::Cursor,
    text::{text, Text},
    widgets::File,
};

use crate::Ui;

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte(input: &dyn InputMethod<Ui>) -> usize {
    main_cursor(input).byte()
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char(input: &dyn InputMethod<Ui>) -> usize {
    main_cursor(input).char()
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col(input: &dyn InputMethod<Ui>) -> usize {
    main_cursor(input).col()
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line(input: &dyn InputMethod<Ui>) -> usize {
    main_cursor(input).line()
}

/// A convenience function that prints the main cursor alongside the
/// lenght of the file, in lines.
pub fn main_fmt(file: &File<Ui>, input: &dyn InputMethod<Ui>) -> Text {
    let cursor = main_cursor(input);
    text!(
        [Coord] { cursor.col() } [Separator] ":"
        [Coord] { cursor.line() } [Separator] "/"
        [Coord] { file.len_lines() }
    )
}

pub fn selections(input: &dyn InputMethod<Ui>) -> usize {
    input.cursors().map(|cursors| cursors.len()).unwrap_or(0)
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt(input: &dyn InputMethod<Ui>) -> String {
    let len = input.cursors().map(|cursors| cursors.len()).unwrap_or(0);
    if len == 1 {
        String::from("1 sel")
    } else {
        format!("{len} sels")
    }
}

fn main_cursor(input: &dyn InputMethod<Ui>) -> Cursor {
    input
        .cursors()
        .map(Cursors::main)
        .cloned()
        .unwrap_or(Cursor::default())
}
