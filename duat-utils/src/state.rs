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
//! [`Cursor`]: duat_core::mode::Cursor
//! [`Mode`]: duat_core::mode::Mode
use duat_core::{data::DataMap, hook::KeysSent, prelude::*};

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
pub fn file_fmt(file: &File<impl Ui>) -> Text {
    let mut b = Text::builder();

    if let Some(name) = file.name_set() {
        b.push(txt!("[File]{name}"));
        if !file.exists() {
            b.push(txt!("[File.new][[new file]]"));
        } else if file.text().has_unsaved_changes() {
            b.push(txt!("[File.unsaved][[+]]"));
        }
    } else {
        b.push(txt!("[File.new.scratch]{}", file.name()));
    }

    b.build()
}

/// [`StatusLine`] part: The currently active [mode] of Duat
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
/// # use duat_core::state;
/// let mode = state::mode_name().map(|mode| {
///     let mode = match mode.split_once('<') {
///         Some((mode, _)) => mode,
///         None => mode,
///     };
///     // Further processing...
/// });
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [mode]: duat_core::mode::Mode
/// [`IncSearch`]: crate::modes::IncSearch
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
        txt!("[Mode]{mode}").build()
    })
}

/// [`StatusLine`] part: Byte of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_byte(file: &File<impl Ui>) -> usize {
    file.cursors().get_main().unwrap().byte() + 1
}

/// [`StatusLine`] part: Char of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_char(file: &File<impl Ui>) -> usize {
    file.cursors().get_main().unwrap().char() + 1
}

/// [`StatusLine`] part: Line of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_line(file: &File<impl Ui>) -> usize {
    file.cursors().get_main().unwrap().line() + 1
}

/// [`StatusLine`] part: Column of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_col<U: Ui>(file: &File<U>, area: &U::Area) -> usize {
    let main = file.cursors().get_main().unwrap();
    main.v_caret(file.text(), area, file.print_cfg()).char_col()
}

/// [`StatusLine`] part: Desired wrapped column of the main cursor
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_dwcol<U: Ui>(file: &File<U>, area: &U::Area) -> usize {
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
pub fn main_fmt<U: Ui>(file: &File<U>, area: &U::Area) -> Text {
    txt!(
        "[Coord]{}[Separator]:[Coord]{}[Separator]/[Coord]{}",
        main_col(file, area),
        main_line(file),
        file.len_lines()
    )
    .build()
}

/// [`StatusLine`] part: The number of cursors
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn selections(file: &File<impl Ui>) -> usize {
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
/// [`Cursor`]: duat_core::mode::Cursor
pub fn selections_fmt(file: &File<impl Ui>) -> Text {
    if file.cursors().len() == 1 {
        txt!("[Selections]1 sel").build()
    } else {
        txt!("[Selections]{} sels", file.cursors().len()).build()
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
            mode::keys_to_text(&keys).build()
        }
    })
}

/// [`StatusLine`] part: The last typed [key]
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [key]: KeyEvent
pub fn last_key() -> RwData<String> {
    thread_local! {
        static LAST_KEY: RwData<String> = {
            let last_key = RwData::new(String::new());

            hook::add::<KeysSent>({
                let last_key = last_key.clone();
                move |pa, keys| {
                    last_key.write(pa, |lk| *lk = mode::keys_to_string(keys));
                }
            });

            last_key
        };
    }

    LAST_KEY.with(|lk| lk.clone())
}
