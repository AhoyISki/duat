use crate::{input::InputMethod, join, position::Cursor, widgets::FileWidget};

/// The name of the file.
pub fn file_name(file: &FileWidget, _input: &dyn InputMethod) -> String {
    file.name().unwrap_or(String::from("*scratch file*"))
}

pub fn main_cursor<'a>(_file: &FileWidget, input: &'a dyn InputMethod) -> &'a Cursor {
    input
        .cursors()
        .expect("The given implementor of InputMethod is not configured to have Cursors")
        .main()
}

/// The byte of the main cursor in the file. Indexed at 1.
pub fn main_byte(file: &FileWidget, input: &dyn InputMethod) -> usize {
    main_cursor(file, input).byte()
}

/// The char of the main cursor in the file. Indexed at 1.
pub fn main_char(file: &FileWidget, input: &dyn InputMethod) -> usize {
    main_cursor(file, input).char()
}

/// The col of the main cursor in the file. Indexed at 1.
pub fn main_col(file: &FileWidget, input: &dyn InputMethod) -> usize {
    main_cursor(file, input).col()
}

/// The line of the main cursor in the file. Indexed at 1.
pub fn main_line(file: &FileWidget, input: &dyn InputMethod) -> usize {
    main_cursor(file, input).line()
}

pub fn selections(_file: &FileWidget, input: &dyn InputMethod) -> usize {
    input
        .cursors()
        .expect("The given implementor of InputMethod is not configured to have Cursors")
        .len()
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections_fmt(_file: &FileWidget, input: &dyn InputMethod) -> String {
    let cursors = input
        .cursors()
        .expect("The given implementor of InputMethod is not configured to have Cursors");
    if cursors.len() == 1 {
        String::from("1 sel")
    } else {
        join![cursors.len(), "sels"]
    }
}

/// Returns a [`String`] with the number of lines in the file.
pub fn len_lines(file: &FileWidget, _input: &dyn InputMethod) -> usize {
    file.len_lines()
}

/// Returns a [`String`] with the number of chars in the file.
pub fn len_chars(file: &FileWidget, _input: &dyn InputMethod) -> usize {
    file.len_chars()
}

/// Returns a [`String`] with the number of bytes in the file.
pub fn len_bytes(file: &FileWidget, _input: &dyn InputMethod) -> usize {
    file.len_bytes()
}
