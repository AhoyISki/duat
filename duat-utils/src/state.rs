//! Common items in a [`StatusLine`]
//!
//! These functions are meant to be simple to use, you can just put
//! them inside a [`status!`] macro, and they will be understood with
//! no other meddling.
//!
//! Examples of functions in here are [`main_txt`], which will show a
//! formatted version of the main [`Cursor`], and [`mode_txt`] which
//! will show a formatted version of the current [`Mode`] of Duat.
//!
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`status!`]: crate::widgets::status
//! [`Cursor`]: duat_core::mode::Cursor
//! [`Mode`]: duat_core::mode::Mode
use duat_core::{
    data::{DataMap, RwData},
    hook::KeysSent,
    prelude::*,
};

/// [`StatusLine`] part: The [`File`]'s name, formatted
///
/// The name of a [`File`] widget is the same as the path, but it
/// strips away the current directory. If it can't, it will try to
/// strip away the home directory, replacing it with `"~"`. If
/// that also fails, it will just show the full path.
///
/// This status part also includes wether or not the file is written
/// and wether or not it exists.
///
/// # Formatting
///
/// If the file's `name` was set:
///
/// ```text
/// [file]{name}
/// ```
///
/// If it has unwritten changes, a `[file.unsaved][[+]]` will be
/// appended. If it doesn't exist, a `[file.new][[new file]]` will be
/// appendedk.
///
/// If the file's `name` was not set:
///
/// ```text
/// [file.new.scratch]{scratch_name}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn name_txt(file: &File<impl Ui>) -> Text {
    let mut b = Text::builder();

    b.push(file.name_txt());
    if !file.exists() {
        b.push(txt!("[file.new][[new file]]"));
    } else if file.text().has_unsaved_changes() {
        b.push(txt!("[file.unsaved][[+]]"));
    }

    b.build()
}

/// [`StatusLine`] part: The [`File`]'s path, formatted
///
/// This status part also includes wether or not the file is written
/// and wether or not it exists.
///
/// # Formatting
///
/// If the file's `path` was set:
///
/// ```text
/// [file]{path}
/// ```
///
/// If it has unwritten changes, a `[file.unsaved][[+]]` will be
/// appended. If it doesn't exist, a `[file.new][[new file]]` will be
/// appendedk.
///
/// If the file's `path` was not set:
///
/// ```text
/// [file.new.scratch]{scratch_path}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn path_txt(file: &File<impl Ui>) -> Text {
    let mut b = Text::builder();

    b.push(file.name_txt());
    if !file.exists() {
        b.push(txt!("[file.new][[new file]]"));
    } else if file.text().has_unsaved_changes() {
        b.push(txt!("[file.unsaved][[+]]"));
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
/// # duat_core::doc_duat!(duat);
/// # use duat_utils::{state, widgets::status};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
///         let mode_upper = state::mode_name(pa).map(pa, |mode| {
///             let mode = match mode.split_once('<') {
///                 Some((mode, _)) => mode,
///                 None => mode,
///             };
///             txt!("[mode]{}", mode.to_uppercase()).build()
///         });
///
///         cfg.fmt(status!(
///             "{name_txt}{Spacer}[mode]{mode_upper} {sels_txt} {main_txt}"
///         ))
///     });
/// }
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [mode]: duat_core::mode::Mode
/// [`IncSearch`]: crate::modes::IncSearch
pub fn mode_name(pa: &Pass) -> DataMap<&'static str, &'static str> {
    context::mode_name(pa)
}

/// [`StatusLine`] part: The active mode of Duat, formatted
///
/// # Formatting
///
/// ```text
/// [mode]{mode}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn mode_txt(pa: &Pass) -> DataMap<&'static str, Text> {
    context::mode_name(pa).map(pa, |mode| {
        let mode = mode.to_lowercase();
        let mode = match mode.split_once('<') {
            Some((mode, _)) => mode,
            None => &mode,
        };
        txt!("[mode]{mode}").build()
    })
}

/// [`StatusLine`] part: Byte of the main selection
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_byte(file: &File<impl Ui>) -> usize {
    file.selections().get_main().unwrap().byte() + 1
}

/// [`StatusLine`] part: Char of the main selection
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_char(file: &File<impl Ui>) -> usize {
    file.selections().get_main().unwrap().char() + 1
}

/// [`StatusLine`] part: Line of the main selection
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_line(file: &File<impl Ui>) -> usize {
    file.selections().get_main().unwrap().line() + 1
}

/// [`StatusLine`] part: Column of the main selection
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_col<U: Ui>(file: &File<U>, area: &U::Area) -> usize {
    let main = file.selections().get_main().unwrap();
    main.v_caret(file.text(), area, file.print_cfg()).char_col()
}

/// [`StatusLine`] part: The main selection, formatted
///
/// # Formatting
///
/// ```text
/// [coord]{col}[separator]:[coord]{line}[separator]/[coord]{lines}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn main_txt<U: Ui>(file: &File<U>, area: &U::Area) -> Text {
    txt!(
        "[coord]{}[separator]:[coord]{}[separator]/[coord]{}",
        main_col(file, area),
        main_line(file),
        file.len_lines()
    )
    .build()
}

/// [`StatusLine`] part: The number of selections
///
/// [`StatusLine`]: crate::widgets::StatusLine
pub fn selections(file: &File<impl Ui>) -> usize {
    file.selections().len()
}

/// [`StatusLine`] part: The number of selections, formatted
///
/// # Formatting
///
/// When there is one [`Cursor`]:
///
/// ```text
/// [selections]1 sel
/// ```
///
/// When there is more than one [`Cursor`]:
///
/// ```text
/// [selections]{n} sels
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`Cursor`]: duat_core::mode::Cursor
pub fn sels_txt(file: &File<impl Ui>) -> Text {
    if file.selections().len() == 1 {
        txt!("[selections]1 sel").build()
    } else {
        txt!("[selections]{} sels", file.selections().len()).build()
    }
}

/// [`StatusLine`] part: The [keys] sent to be mapped, formatted
///
/// # Formatting
///
/// For every key, if they are a normal `char`:
///
/// ```text
/// [key]{char}
/// ```
///
/// Otherwise if they are a `special` key:
///
/// ```text
/// [key.special]{special}
/// ```
///
/// [`StatusLine`]: crate::widgets::StatusLine
/// [keys]: KeyEvent
pub fn cur_map_txt(pa: &Pass) -> DataMap<(Vec<KeyEvent>, bool), Text> {
    mode::cur_sequence(pa).map(pa, |(keys, is_alias)| {
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

            hook::add_no_alias::<KeysSent>({
                let last_key = last_key.clone();
                move |pa, keys| {
                    *last_key.write(pa) = mode::keys_to_string(keys)
                }
            });

            last_key
        };
    }

    LAST_KEY.with(|lk| lk.clone())
}
