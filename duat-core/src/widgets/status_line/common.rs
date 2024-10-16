use crate::{
    input::{Cursor, Cursors, InputMethod},
    text::{Text, text},
    ui::Ui,
    widgets::File,
};

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte<U: Ui>(input: &dyn InputMethod<U>) -> usize {
    main_cursor(input).byte() + 1
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char<U: Ui>(input: &dyn InputMethod<U>) -> usize {
    main_cursor(input).char() + 1
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col<U: Ui>(input: &dyn InputMethod<U>) -> usize {
    main_cursor(input).column() + 1
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line<U: Ui>(input: &dyn InputMethod<U>) -> usize {
    main_cursor(input).line() + 1
}

/// A convenience function that prints the main cursor alongside the
/// lenght of the file, in lines.
pub fn main_fmt<U: Ui>(file: &File, input: &dyn InputMethod<U>) -> Text {
    text!(
        [Coord] { main_col(input) } [Separator] ":"
        [Coord] { main_line(input) } [Separator] "/"
        [Coord] { file.len_lines() }
    )
}

pub fn selections<U: Ui>(input: &dyn InputMethod<U>) -> usize {
    input.cursors().map(|cursors| cursors.len()).unwrap_or(0)
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt<U: Ui>(input: &dyn InputMethod<U>) -> Text {
    let len = input.cursors().map(|cursors| cursors.len()).unwrap_or(0);
    if len == 1 {
        text!([Selections] "1 sel")
    } else {
        text!([Selections] len "sels")
    }
}

fn main_cursor<U: Ui>(input: &dyn InputMethod<U>) -> Cursor {
    input
        .cursors()
        .map(Cursors::main)
        .cloned()
        .expect("This InputMethod was supposed to have a cursors in it.")
}
