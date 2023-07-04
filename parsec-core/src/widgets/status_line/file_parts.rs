use crate::{join, ui::Ui, widgets::FileWidget};

/// The name of the file.
pub fn file_name<U>() -> impl Fn(&FileWidget<U>) -> String
where
    U: Ui
{
    |file| file.name().to_string()
}

pub fn main_cursor<U>() -> impl Fn(&FileWidget<U>) -> crate::position::Cursor
where
    U: Ui
{
    |file| file.main_cursor()
}

/// The line of the main cursor in the file.
pub fn main_line<U>() -> impl Fn(&FileWidget<U>) -> usize
where
    U: Ui
{
    |file| file.main_cursor().line()
}

/// The col of the main cursor in the file.
pub fn main_col<U>() -> impl Fn(&FileWidget<U>) -> usize
where
    U: Ui
{
    |file| file.main_cursor().col()
}

/// Returns a [`String`] with the number of selections in the file.
pub fn selections<U>() -> impl Fn(&FileWidget<U>) -> String
where
    U: Ui
{
    |file| {
        if file.cursors().len() == 1 {
            String::from("1 sel")
        } else {
            join![file.cursors().len(), "sels"]
        }
    }
}

/// Returns a [`String`] with the number of lines in the file.
pub fn len_lines<U>() -> impl Fn(&FileWidget<U>) -> usize
where
    U: Ui
{
    |file| file.len_lines()
}

/// Returns a [`String`] with the number of chars in the file.
pub fn len_chars<U>() -> impl Fn(&FileWidget<U>) -> usize
where
    U: Ui
{
    |file| file.len_chars()
}

/// Returns a [`String`] with the number of bytes in the file.
pub fn len_bytes<U>() -> impl Fn(&FileWidget<U>) -> usize
where
    U: Ui
{
    |file| file.len_bytes()
}