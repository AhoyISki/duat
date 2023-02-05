use std::cmp::min;

use cursor::TextPos;
use text::Text;
use ui::{EndNode, Ui, Label, Area};
use widgets::file_widget::PrintInfo;

pub mod action;
pub mod config;
pub mod cursor;
pub mod text;
pub mod input;
pub mod widgets;
pub mod tags;
pub mod ui;

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
pub fn get_byte_at_col(col: usize, text: &String) -> usize {
    text.char_indices().nth(col).map(|c| c.0).unwrap_or(text.len())
}

/// An empty list of `String`s, representing an empty edit/file.
pub fn empty_edit() -> Vec<String> {
    vec![String::from("")]
}

// NOTE: Will definitely break once folding becomes a thing.
/// The last line that could possibly be printed.
pub fn max_line(text: &Text, print_info: &PrintInfo, node: &EndNode<impl Ui>) -> usize {
    min(print_info.top_row + node.label.read().area().height(), text.lines().len() - 1)
}

//////////// Useful for testing.
pub static mut FOR_TEST: usize = 0;

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
