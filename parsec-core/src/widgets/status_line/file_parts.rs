use crate::{
    input::{Cursors, InputMethod},
    position::Cursor,
    widgets::File,
};

/// The name of the file.
pub fn file_name(file: &File) -> String {
    file.name().unwrap_or(String::from("*scratch file*"))
}

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
    input
        .cursors()
        .expect("The given implementor of InputMethod is not configured to have Cursors")
        .len()
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt(input: &dyn InputMethod) -> String {
    let cursors = input
        .cursors()
        .expect("The given implementor of InputMethod is not configured to have Cursors");
    if cursors.len() == 1 {
        String::from("1 sel")
    } else {
        format!("{} sels", cursors.len())
    }
}

/// Returns a [`String`] with the number of lines in the file.
pub fn len_lines(file: &File) -> usize {
    file.len_lines()
}

/// Returns a [`String`] with the number of chars in the file.
pub fn len_chars(file: &File) -> usize {
    file.len_chars()
}

/// Returns a [`String`] with the number of bytes in the file.
pub fn len_bytes(file: &File) -> usize {
    file.len_bytes()
}

fn main_cursor(input: &dyn InputMethod) -> Cursor {
    input
        .cursors()
        .map(Cursors::main)
        .cloned()
        .unwrap_or(Cursor::default())
}
