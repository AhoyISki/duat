use crate::{ui::Ui, widgets::FileWidget};

/// The number of lines in the file.
pub fn len_lines<U>() -> impl Fn(&FileWidget<U>) -> String
where
    U: Ui
{
    |file| file.len_lines().to_string()
}

/// The line of the main cursor in the file.
pub fn main_line<U>() -> impl Fn(&FileWidget<U>) -> String
where
    U: Ui
{
    |file| file.main_cursor().line().to_string()
}

/// The col of the main cursor in the file.
pub fn main_col<U>() -> impl Fn(&FileWidget<U>) -> String
where
    U: Ui
{
    |file| file.main_cursor().col().to_string()
}

/// The number of selections in the file.
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

/// The name of the file.
pub fn file_name<U>() -> impl Fn(&FileWidget<U>) -> String
where
    U: Ui
{
    |file| file.name().to_string()
}
