use crate::{
    input::{Cursors, InputMethod},
    position::Cursor,
    text::{text, Text},
    ui::Ui,
    widgets::File,
};

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte<U>(input: &dyn InputMethod<U>) -> usize
where
    U: Ui,
{
    main_cursor(input).byte()
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char<U>(input: &dyn InputMethod<U>) -> usize
where
    U: Ui,
{
    main_cursor(input).char()
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col<U>(input: &dyn InputMethod<U>) -> usize
where
    U: Ui,
{
    main_cursor(input).vcol()
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line<U>(input: &dyn InputMethod<U>) -> usize
where
    U: Ui,
{
    main_cursor(input).line()
}

/// A convenience function that prints the main cursor alongside the
/// lenght of the file, in lines.
pub fn main_fmt<U>(file: &File, input: &dyn InputMethod<U>) -> Text
where
    U: Ui,
{
    let cursor = main_cursor(input);
    text!(
        [Coord] { cursor.vcol() } [Separator] ":"
        [Coord] { cursor.line() } [Separator] "/"
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

fn main_cursor<U>(input: &dyn InputMethod<U>) -> Cursor
where
    U: Ui,
{
    input
        .cursors()
        .map(Cursors::main)
        .cloned()
        .unwrap_or(Cursor::default())
}
