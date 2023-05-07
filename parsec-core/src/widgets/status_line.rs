//! A [`NormalWidget`] that serves the purpose of showing general
//! information.
//!
//! This widget has an associated [`FileWidget<U>`] that's used as a
//! reference. This conotates that a [`StatusLine<U>`] must be tied to
//! a single file, but this is not really the case. The end user may
//! be able to open multiple files and have just a single
//! [`StatusLine<U>`] that displays the information of only the
//! currently active [`FileWidget<U>`]. Given an existing
//! [`Session<U>`][crate::Session], one can do the following. TODO.
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{any::Any, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use super::{file_widget::FileWidget, NormalWidget, Widget};
use crate::{
    data::{DownCastableData, RoData},
    tags::{
        form::{FormPalette, COORDS, FILE_NAME, SELECTIONS, SEPARATOR},
        Tag
    },
    text::{Text, TextBuilder},
    ui::{PushSpecs, Ui},
    updaters, Manager
};

/// A struct that holds mutable readers, either from a file, or from
/// individual [`RoData<T>`]s
pub enum Reader<U>
where
    U: Ui
{
    Var(Box<dyn Fn() -> String>),
    File(Box<dyn Fn(&FileWidget<U>) -> String>)
}

impl<U> Reader<U>
where
    U: Ui
{
    /// Reads the current state of [`self`] and applies a function
    /// that returns a [`String`].
    fn read(&self, file: &FileWidget<U>) -> String {
        match self {
            Reader::Var(obj_fn) => obj_fn(),
            Reader::File(file_fn) => file_fn(file)
        }
    }
}

/// Part of the [`StatusLine<U>`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub enum StatusPart<U>
where
    U: Ui
{
    Dynamic(Reader<U>),
    Static(String)
}

impl<U> StatusPart<U>
where
    U: Ui
{
    /// Consumes [`self`] and modifies a [`TextBuilder<U>`], adding
    /// swappable ranges, text, and [`Tag`]s.
    fn process(
        self, builder: &mut TextBuilder<U>, file: &FileWidget<U>, palette: &FormPalette
    ) -> Option<Reader<U>> {
        match self {
            StatusPart::Dynamic(Reader::Var(obj_fn)) => {
                builder.push_swappable(obj_fn());
                Some(Reader::Var(obj_fn))
            }
            StatusPart::Dynamic(Reader::File(file_fn)) => {
                builder.push_swappable(file_fn(file));
                Some(Reader::File(file_fn))
            }
            StatusPart::Static(text) => {
                push_forms_and_text(text.as_str(), builder, palette);
                None
            }
        }
    }
}

/// Consumes a [`StatusPart::Static`], pushing text and [`Tag`] to a
/// [`TextBuilder<U>`]
fn push_forms_and_text<U>(text: &str, builder: &mut TextBuilder<U>, palette: &FormPalette)
where
    U: Ui
{
    let mut prev_l_index = None;
    for (next_l_index, _) in text.match_indices('[').chain([(text.len(), "[")]) {
        let Some(l_index) = prev_l_index else {
            builder.push_text(&text[..next_l_index]);
            prev_l_index = Some(next_l_index);
            continue;
        };

        if let Some((text_start, (_, form_id))) = text[(l_index + 1)..next_l_index]
            .find(']')
            .map(|r_index| {
                let form_name = &text[(l_index + 1)..=(l_index + r_index)];
                palette.get_from_name(form_name).map(|form_id| (l_index + r_index + 2, form_id))
            })
            .flatten()
        {
            builder.push_tag(Tag::PushForm(form_id));
            builder.push_text(&text[text_start..next_l_index]);
        } else {
            builder.push_text(&text[l_index..next_l_index]);
        }

        prev_l_index = Some(next_l_index);
    }
}

/// A [`NormalWidget`] that can display information about Parsec and
/// its extensions.
///
/// The [`StatusLine<U>`] is built around a [`Vec<Reader<U>>`], which
/// determines exacly how it will be updated. There is a default form
/// for the [`Vec<Reader<U>>`] in [`StatusLine::default_fn()`], which
/// you can read if you want to.
///
/// # Examples
///
/// Here's a very simple example, that will show the name of the file,
/// as well as the main cursor's coordinates.
///
/// ```rust
/// # let file = todo!();
/// # let palette = todo!();
/// let parts = vec![
///     text("file name: [FileName]"),
///     f_var(|file| file.name()),
///     text("[Default] main cursor: [Coords]"),
///     f_var(|file| file.main_cursor()),
/// ];
///
/// clippable_fn(file, parts, palette);
/// ```
///
/// The `"[FileName]"`, `"[Default]"` and `"[Coords]"` additions serve
/// to change the active [`Form`][crate::tags::form::Form] to print
/// the next characters.
pub struct StatusLine<U>
where
    U: Ui
{
    file: RoData<FileWidget<U>>,
    builder: TextBuilder<U>,
    readers: Vec<Reader<U>>,
    _clippable: bool
}

impl<U> StatusLine<U>
where
    U: Ui
{
    /// Returns a function that outputs a new instance of
    /// [`StatusLine<U>`].
    fn new_fn(
        file: RoData<FileWidget<U>>, builder: TextBuilder<U>, readers: Vec<Reader<U>>,
        _clippable: bool
    ) -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, _| {
            let updaters = updaters![(file.clone())];
            Widget::normal(
                Arc::new(RwLock::new(StatusLine {
                    file,
                    builder,
                    readers,
                    _clippable
                })),
                updaters
            )
        })
    }

    /// A [`StatusLine<U>`] that gives way to others, and when there
    /// is not enough space, gets clipped first.
    pub fn clippable_fn(
        file: RoData<FileWidget<U>>, parts: Vec<StatusPart<U>>, palette: &FormPalette
    ) -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        let mut text_builder = TextBuilder::default();
        let readers = {
            let mut readers = Vec::new();
            let file = file.read();
            for part in parts.into_iter() {
                if let Some(reader) = part.process(&mut text_builder, &file, palette) {
                    readers.push(reader);
                }
            }
            readers
        };

        StatusLine::new_fn(file, text_builder, readers, true)
    }

    /// A [`StatusLine<U>`] that takes precedence over others, and
    /// when there is not enough space, gets clipped last.
    pub fn unclippable_fn(
        file: RoData<FileWidget<U>>, parts: Vec<StatusPart<U>>, palette: &FormPalette
    ) -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        let mut text_builder = TextBuilder::default();
        let readers = {
            let mut readers = Vec::new();
            let file = file.read();
            for part in parts.into_iter() {
                if let Some(reader) = part.process(&mut text_builder, &file, palette) {
                    readers.push(reader);
                }
            }
            readers
        };

        StatusLine::new_fn(file, text_builder, readers, false)
    }

    /// Returns a function that outputs the default version of
    /// [`StatusLine<U>`].
    pub fn default_fn(
        file: RoData<FileWidget<U>>
    ) -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        let name = Reader::File(file_name());
        let sels = Reader::File(file_selections());
        let col = Reader::File(main_col());
        let line = Reader::File(main_line());
        let lines = Reader::File(file_lines_len());

        let text_builder = {
            let file = file.read();
            let mut builder = TextBuilder::default();

            builder.push_tag(Tag::PushForm(FILE_NAME));
            builder.push_swappable(name.read(&file));
            builder.push_text(" ");
            builder.push_tag(Tag::PushForm(SELECTIONS));
            builder.push_swappable(sels.read(&file));
            builder.push_text(" ");
            builder.push_tag(Tag::PushForm(COORDS));
            builder.push_swappable(col.read(&file));
            builder.push_tag(Tag::PushForm(SEPARATOR));
            builder.push_text(":");
            builder.push_tag(Tag::PushForm(COORDS));
            builder.push_swappable(line.read(&file));
            builder.push_tag(Tag::PushForm(SEPARATOR));
            builder.push_text("/");
            builder.push_tag(Tag::PushForm(COORDS));
            builder.push_swappable(lines.read(&file));

            builder
        };

        let readers = vec![name, sels, col, line, lines];

        StatusLine::new_fn(file, text_builder, readers, true)
    }

    /// Changes the reference [`FileWidget<U>`] for the
    /// [`StatusLine<U>`]
    pub fn set_file(&mut self, file: RoData<FileWidget<U>>) {
        self.file = file;
    }
}

impl<U> NormalWidget<U> for StatusLine<U>
where
    U: Ui + 'static
{
    fn update(&mut self, _label: &U::Label) {
        let file = self.file.read();

        for (index, reader) in self.readers.iter().enumerate() {
            self.builder.swap_range(index, reader.read(&file));
        }
    }

    fn needs_update(&self) -> bool {
        true
    }

    fn text(&self) -> &Text<U> {
        &self.builder.text()
    }
}

impl<U> DownCastableData for StatusLine<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// An updateable piece of text, created by reading from a
/// [`FileWidget<U>`].
///
/// Whenever the [`FileWidget<U>`] is updated, the [`StatusLine<U>`]
/// will be updated according to this function.
///
/// # Examples
///
/// This will print the `row` of the main cursor on the
/// [`FileWidget<U>`]
///
/// ```rust
/// f_var(move |file| file.main_cursor().row());
/// ```
pub fn f_var<U, S>(file_fn: impl Fn(&FileWidget<U>) -> S + 'static) -> StatusPart<U>
where
    U: Ui,
    S: ToString
{
    let file_fn = Box::new(move |file: &FileWidget<U>| file_fn(file).to_string());
    StatusPart::Dynamic(Reader::File(file_fn))
}

/// An updateable piece of text, usually based on a [`RoData<T>`].
///
/// Most of the time, `var_fn` will be a function that captures some
/// sort of [`RoData<T>`]. This function will read the updated value
/// of `T` and return a [`String`].
///
/// # Examples
///
/// On the crate [parsec-kak][https://docs.rs/parsec-kak], the
/// [`Editor`][https://docs.rs/parsec-kak/struct.Editor] struct has an
/// [`RoData<T>`] that represents the current mode. Since it
/// implements display, one can do the following:
///
/// ```rust
/// let editor = parsec_kak::Editor::new();
/// let mode = editor.cur_mode();
/// var(move || mode.to_string());
/// ```
///
/// You could also (as you may be able to guess) do more with the
/// `mode` variable:
///
/// ```rust
/// let editor = parsec_kak::Editor::new();
/// let mode = editor.cur_mode();
/// var(move || {
///     let mode = mode.to_string();
///     if mode.as_str() == "normal" {
///         String::default()
///     } else {
///         mode.to_uppercase()
///     }
/// });
/// ```
///
/// In this case, the mode will only be displayed if it is not normal,
/// and will be uppercased.
pub fn var<U, S>(var_fn: impl Fn() -> S + 'static) -> StatusPart<U>
where
    U: Ui,
    S: ToString
{
    let var_fn = Box::new(move || var_fn().to_string());
    StatusPart::Dynamic(Reader::Var(var_fn))
}

/// A Piece of text to be used inside a [`StatusLine<U>`].
///
/// # Examples
///
/// This text can be colored, by including form names inside braces,
/// e.g.:
///
/// ```
/// text("[FileName]My file name[Default]")
/// ```
///
/// When printed, this text will apply the `"FileName"` [`Form`],
/// print "My file name", and then return the [`Form`] to `"Default"`.
///
/// [`Form`]: crate::tags::form::Form
pub fn text<U>(text: impl ToString) -> StatusPart<U>
where
    U: Ui
{
    StatusPart::Static(text.to_string())
}

////////// Functions used in the default `StatusLine<U>`.

/// The number of lines in the file.
fn file_lines_len<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.len_lines().to_string())
}

/// The line of the main cursor in the file.
fn main_line<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.main_cursor().row().to_string())
}

/// The col of the main cursor in the file.
fn main_col<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.main_cursor().col().to_string())
}

/// The number of selections in the file.
fn file_selections<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| {
        if file.cursors().len() == 1 {
            String::from("1 sel")
        } else {
            join![file.cursors().len(), "sels"]
        }
    })
}

/// The name of the file.
fn file_name<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.name().to_string())
}

/// A convenience macro to join any number of variables that can be
/// turned into `String`s.
///
/// # Examples
///
/// ```
/// use parsec_core::widgets::status_line::join;
///
/// let my_text =
///     join!["number: ", 235, String::from(", floating: "), 3.14f32];
/// assert!(my_text == String::from("number: 235, floating: 3.14"));
/// ```
#[macro_export]
macro_rules! join {
    ($($var:expr),*) => {
        [$($var.to_string()),*].join("")
    }
}
pub use join;
