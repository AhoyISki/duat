use core::fmt;
use std::marker::PhantomData;

use cursor::TextPos;
use input::{EditingScheme, FileRemapper};
use layout::Layout;
use ui::Ui;

pub mod action;
pub mod config;
pub mod cursor;
mod file;
pub mod input;
pub mod layout;
pub mod tags;
pub mod ui;

/// General struct for an application, that includes a `Ui`, `Layout`, and `EditingScheme`s.
pub struct Application<L, E, U>
where
    L: Layout<U>,
    E: EditingScheme,
    U: Ui,
{
    layout: L,
    key_remapper: FileRemapper<E>,
    // ↓ stupid ↓ //
    _phantom_stuff: (PhantomData<E>, PhantomData<U>),
}

impl<L, E, U> Application<L, E, U>
where
    L: Layout<U>,
    E: EditingScheme,
    U: Ui,
{
    /// Returns a new instance of `Application`.
    pub fn new(layout: L, key_remapper: FileRemapper<E>) -> Self {
        Application {
            layout,
            key_remapper,
            _phantom_stuff: (PhantomData::default(), PhantomData::default()),
        }
    }
}

/// Given a position (which is assumed to be on the line), will return the position at its start.
pub fn get_line_start(pos: TextPos, line: &String) -> TextPos {
    TextPos { byte: pos.byte - line.char_indices().take(pos.col).count(), col: 0, row: pos.row }
}

/// Creates a vector of `&str`s from a `String`, making sure to keep at least one empty
/// string at the end, in case of an empty, or `\n` terminated string.
fn split_string_lines(string: &String) -> Vec<String> {
    if string.is_empty() {
        vec![String::from("")]
    } else {
        let mut lines: Vec<String> = string.split_inclusive('\n').map(|s| s.to_string()).collect();
        if string.ends_with('\n') {
            lines.push(String::from(""));
        }
        lines
    }
}

/// Gets the line-byte at a given col in a string.
pub fn get_byte_at_col(col: usize, text: &String) -> Option<usize> {
    text.char_indices().nth(col).map(|c| c.0)
}

/// An empty list of `String`s, representing an empty edit/file.
pub fn empty_edit() -> Vec<String> {
    vec![String::from("")]
}

////////// Ad-hoc functions until they eventually get stabilized.
pub fn saturating_add_signed(lhs: usize, rhs: isize) -> usize {
    if rhs > 0 {
        lhs.saturating_add(rhs as usize)
    } else {
        lhs.saturating_sub(rhs.abs() as usize)
    }
}

//////////// Useful for testing.
pub static mut FOR_TEST: bool = false;
pub static mut FOR_TEST_2: bool = false;

/// Internal macro used to log information.
#[macro_export]
macro_rules! log_info {
    ($($text:tt)*) => {
        {
            use std::{fs, io::Write};
            let mut log = fs::OpenOptions::new().append(true).open("log").unwrap();
            log.write_fmt(format_args!($($text)*)).unwrap();
        }
    }
}
