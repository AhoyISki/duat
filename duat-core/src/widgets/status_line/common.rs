use crate::{
    input::{Cursor, Cursors, InputMethod},
    text::{text, Text},
    ui::Ui,
    widgets::File,
};

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte<U>(input: &dyn InputMethod<U>) -> Option<usize>
where
    U: Ui,
{
    main_cursor(input).map(|c| c.byte() + 1)
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char<U>(input: &dyn InputMethod<U>) -> Option<usize>
where
    U: Ui,
{
    main_cursor(input).map(|c| c.char() + 1)
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col<U>(input: &dyn InputMethod<U>) -> Option<usize>
where
    U: Ui,
{
    main_cursor(input).map(|c| c.column() + 1)
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line<U>(input: &dyn InputMethod<U>) -> Option<usize>
where
    U: Ui,
{
    main_cursor(input).map(|c| c.line() + 1)
}

/// A convenience function that prints the main cursor alongside the
/// lenght of the file, in lines.
pub fn main_fmt<U>(file: &File, input: &dyn InputMethod<U>) -> Text
where
    U: Ui,
{
    text!(
        [Coord] { main_col(input) } [Separator] ":"
        [Coord] { main_line(input) } [Separator] "/"
        [Coord] { file.len_lines() }
    )
}

pub fn selections<U>(input: &dyn InputMethod<U>) -> usize
where
    U: Ui,
{
    input.cursors().map(|cursors| cursors.len()).unwrap_or(0)
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt<U>(input: &dyn InputMethod<U>) -> Text
where
    U: Ui,
{
    let len = input.cursors().map(|cursors| cursors.len()).unwrap_or(0);
    if len == 1 {
        text!([Selections] "1 sel")
    } else {
        text!([Selections] len "sels")
    }
}

fn main_cursor<U>(input: &dyn InputMethod<U>) -> Option<&Cursor>
where
    U: Ui,
{
    input.cursors().map(Cursors::main)
}
