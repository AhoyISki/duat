use crate::{
    input::{Cursors, InputMethod},
    position::Cursor,
};

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte(input: &dyn InputMethod) -> usize {
    main_cursor(input).byte()
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char(input: &dyn InputMethod) -> usize {
    main_cursor(input).char()
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col(input: &dyn InputMethod) -> usize {
    main_cursor(input).col()
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line(input: &dyn InputMethod) -> usize {
    main_cursor(input).line()
}

pub fn selections(input: &dyn InputMethod) -> usize {
    input.cursors().map(|cursors| cursors.len()).unwrap_or(0)
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt(input: &dyn InputMethod) -> String {
    let len = input.cursors().map(|cursors| cursors.len()).unwrap_or(0);
    if len == 1 {
        String::from("1 sel")
    } else {
        format!("{len} sels")
    }
}

fn main_cursor(input: &dyn InputMethod) -> Cursor {
    input
        .cursors()
        .map(Cursors::main)
        .cloned()
        .unwrap_or(Cursor::default())
}
