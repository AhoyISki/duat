//! Common items in a [`StatusLine`]
//!
//! These functions are meant to be simple to use, you can just put
//! them inside a [`status!`] macro, and they will be understood with
//! no other meddling.
//!
//! Examples of functions in here are [`main_fmt`], which will show a
//! formatted version of the main [`Cursor`], and [`mode_fmt`] which
//! will show a formatted version of the current [`Mode`] of Duat.
//!
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`status!`]: crate::widgets::status
//! [`Cursor`]: crate::mode::Cursor
//! [`Mode`]: crate::mode::Mode
use std::sync::LazyLock;

use duat_core::{
    context,
    data::{DataMap, RwData},
    hooks::{self, KeySent},
    mode::{self, KeyEvent},
    text::{Text, add_text, text},
    ui::RawArea,
    widgets::File,
};

/// [`StatusLine`] part: The [`File`]'s name, formatted
///
/// Includes wether or not the file is written and wether or not it
/// exists.
///
/// # Formatting
///
/// If the file's `name` was set:
///
/// ```text
/// [File]{name}
/// ```
///
/// If it has unwritten changes, a `[File.unsaved][[+]]` will be
/// appended. If it doesn't exist, a `[File.new][[new file]]` will be
/// appended.
///
/// If the file's `name` was not set:
///
/// ```text
/// [File.new.scratch]{scratch_name}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn file_fmt(file: &File) -> Text {
    let mut b = Text::builder();

    if let Some(name) = file.name_set() {
        add_text!(b, "[File]{name}");
        if !file.exists() {
            add_text!(b, "[File.new][[new file]]");
        } else if file.text().has_unsaved_changes() {
            add_text!(b, "[File.unsaved][[+]]");
        }
    } else {
        add_text!(b, "[File.new.scratch]{}", file.name());
    }

    b.finish()
}

/// [`StatusLine`] part: The active [mode] of Duat
///
/// This mode is completely unprocessed, so something like
/// [`IncSearch`] would show up like:
///
/// ```text
/// Prompt<IncSearch<{IncSearcher}, Ui> Ui>
/// ```
///
/// Which is rather undesirable. If you don't want that to happen, one
/// simple trick is this:
///
/// ```rust
/// # use duat_core::status;
/// let mode = status::mode_name().map(|mode| {
///     let mode = match mode.split_once('<') {
///         Some((mode, _)) => mode,
///         None => mode,
///     };
///     // Further processing...
/// });
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [mode]: mode::Mode
/// [`IncSearch`]: mode::IncSearch
pub fn mode_name() -> DataMap<&'static str, &'static str> {
    context::mode_name()
}

/// [`StatusLine`] part: The active mode of Duat, formatted
///
/// # Formatting
///
/// ```text
/// [Mode]{mode}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn mode_fmt() -> DataMap<&'static str, Text> {
    context::mode_name().map(|mode| {
        let mode = mode.to_lowercase();
        let mode = match mode.split_once('<') {
            Some((mode, _)) => mode,
            None => &mode,
        };
        text!("[Mode]{mode}")
    })
}

/// [`StatusLine`] part: Byte of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_byte(file: &File) -> usize {
    file.cursors().get_main().unwrap().byte() + 1
}

/// [`StatusLine`] part: Char of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_char(file: &File) -> usize {
    file.cursors().get_main().unwrap().char() + 1
}

/// [`StatusLine`] part: Line of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_line(file: &File) -> usize {
    file.cursors().get_main().unwrap().line() + 1
}

/// [`StatusLine`] part: Column of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_col(file: &File, area: &impl RawArea) -> usize {
    let main = file.cursors().get_main().unwrap();
    main.v_caret(file.text(), area, file.print_cfg()).char_col()
}

/// [`StatusLine`] part: Desired wrapped column of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_dwcol(file: &File, area: &impl RawArea) -> usize {
    let main = file.cursors().get_main().unwrap();
    main.v_caret(file.text(), area, file.print_cfg())
        .desired_wrapped_col()
}

/// [`StatusLine`] part: The main cursor, formatted
///
/// # Formatting
///
/// ```text
/// [Coord]{col}[Separator]:[Coord]{line}[Separator]/[Coord]{lines}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_fmt(file: &File, area: &impl RawArea) -> Text {
    text!(
        "[Coord]{}[Separator]:[Coord]{}[Separator]/[Coord]{}",
        main_col(file, area),
        main_line(file),
        file.len_lines()
    )
}

/// [`StatusLine`] part: The number of cursors
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn selections(file: &File) -> usize {
    file.cursors().len()
}

/// [`StatusLine`] part: The number of cursors, formatted
///
/// # Formatting
///
/// When there is one [`Cursor`]:
///
/// ```text
/// [Selections]1 sel
/// ```
///
/// When there is more than one [`Cursor`]:
///
/// ```text
/// [Selections]{n} sels
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`Cursor`]: crate::mode::Cursor
pub fn selections_fmt(file: &File) -> Text {
    if file.cursors().len() == 1 {
        text!("[Selections]1 sel")
    } else {
        text!("[Selections]{} sels", file.cursors().len())
    }
}

/// [`StatusLine`] part: The [keys] sent to be mapped, formatted
///
/// # Formatting
///
/// For every key, if they are a normal `char`:
///
/// ```text
/// [SeqCharKey]{char}
/// ```
///
/// Otherwise if they are a `special` key:
///
/// ```text
/// [SeqSpecialKey]{special}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [keys]: KeyEvent
pub fn cur_map_fmt() -> DataMap<(Vec<KeyEvent>, bool), Text> {
    mode::cur_sequence().map(|(keys, is_alias)| {
        if is_alias {
            Text::default()
        } else {
            mode::keys_to_text(&keys)
        }
    })
}

/// [`StatusLine`] part: The last typed [key]
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [key]: KeyEvent
pub fn last_key() -> RwData<String> {
    static LAST_KEY: LazyLock<RwData<String>> = LazyLock::new(|| {
        let last_key = RwData::new(String::new());

        hooks::add::<KeySent>({
            let last_key = last_key.clone();
            move |key| {
                *last_key.write() = mode::keys_to_string(&[key]);
            }
        });

        last_key
    });

    LAST_KEY.clone()
}
