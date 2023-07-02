//! A [`NormalWidget`] that serves the purpose of showing general
//! information.
//!
//! This widget has an associated [`FileWidget<U>`] that's used as a
//! reference. This conotates that a [`StatusLine<U>`] must be tied to
//! a single file, but this is not really the case. The end user may
//! be able to open multiple files and have just a single
//! [`StatusLine<U>`] that displays the information of only the
//! currently active [`FileWidget<U>`]:
//!
//! ```rust
//! # use parsec_core::{
//! #     tags::form::FormPalette,
//! #     text::PrintCfg,
//! #     ui::{ModNode, PushSpecs, Constraint, Ui},
//! #     widgets::StatusLine,
//! #     session::Parsec
//! # };
//! #
//! # fn test_fn<U>(ui: U, print_cfg: PrintCfg, palette: FormPalette)
//! # where
//! #     U: Ui
//! # {
//! let constructor_hook = move |mod_node: ModNode<U>, file| {
//!     let specs = PushSpecs::below(Constraint::Length(1.0));
//!     mod_node.push(StatusLine::default_fn(), specs);
//! };
//!
//! let mut parsec = Parsec::new(ui, print_cfg, palette, constructor_hook);
//! let specs = PushSpecs::below(Constraint::Length(1.0));
//! parsec.push(StatusLine::default_global_fn(), specs);
//! # }
//! ```
//!
//! In the example above, every time a [`FileWidget<U>`] is opened
//! with a new file, a [`StatusLine<U>`] will be pushed below it, as
//! seen in the `constructor_hook`. This [`StatusLine<U>`] will show
//! information about that specific [`FileWidget<U>`].
//!
//! Also in the example above, you can see that a second
//! [`StatusLine<U>`] is pushed by [`PushSpecs::below()`]. As such,
//! this widget will be placed below all others, and the
//! [`default_global_fn()`][StatusLine::default_global_fn()] means
//! that it is global, and instead of pointing to a specific
//! [`FileWidget<U>`], it will change to always point at the currently
//! active one.
//!
//! In a real life situation, you would choose only one of these
//! aproaches, as having two [`StatusLine<U>`]s showing the same
//! information at the same time is quite redundant. But this is a
//! good showing for the flexibility of this widget.

#[macro_use]
mod status_helpers;
pub mod file_parts;

use std::os::linux::raw::stat;

use file_parts::{file_name, len_lines, main_col, main_line, selections};
pub use status_helpers::status_parts;

use self::Reader::*;
use super::{file_widget::FileWidget, Widget};
use crate::{
    data::{DownCastableData, ReadableData, RoNestedData},
    tags::{form::FormPalette, Tag},
    text::{Text, TextBuilder},
    ui::{Constraint, PushSpecs, Ui},
    Controler
};

/// A struct that holds mutable readers, either from a file, or from
/// individual [`RoData<T>`]s
pub enum Reader<U>
where
    U: Ui
{
    Var(Box<dyn Fn() -> String + 'static>),
    File(Box<dyn Fn(&FileWidget<U>) -> String + 'static>)
}

impl<U> Reader<U>
where
    U: Ui
{
    /// Reads the current state of [`self`] and applies a function
    /// that returns a [`String`].
    fn read(&self, file: &FileWidget<U>) -> String {
        match self {
            Var(var_fn) => var_fn(),
            File(file_fn) => file_fn(file)
        }
    }
}

enum ReaderOrText<U>
where
    U: Ui
{
    Reader(Reader<U>),
    Text(String)
}

/// Part of the [`StatusLine<U>`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct StatusPart<U>
where
    U: Ui
{
    reader_or_text: ReaderOrText<U>,
    checker: Option<Box<dyn Fn() -> bool>>
}

impl<U> StatusPart<U>
where
    U: Ui
{
    /// Consumes [`self`] and modifies a [`TextBuilder`], adding
    /// swappable ranges, text, and [`Tag`]s.
    fn process(
        self, builder: &mut TextBuilder, file: &FileWidget<U>, palette: &FormPalette
    ) -> (Option<Reader<U>>, Option<Box<dyn Fn() -> bool>>) {
        match self.reader_or_text {
            ReaderOrText::Reader(Reader::Var(obj_fn)) => {
                builder.push_swappable(obj_fn());
                (Some(Reader::Var(obj_fn)), self.checker)
            }
            ReaderOrText::Reader(Reader::File(file_fn)) => {
                builder.push_swappable(file_fn(file));
                (Some(Reader::File(file_fn)), self.checker)
            }
            ReaderOrText::Text(text) => {
                push_forms_and_text(text.as_str(), builder, palette);
                (None, self.checker)
            }
        }
    }
}

impl<U> From<char> for StatusPart<U>
where
    U: Ui
{
    fn from(value: char) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(String::from(value)),
            checker: None
        }
    }
}

impl<U> From<&'_ str> for StatusPart<U>
where
    U: Ui
{
    fn from(value: &'_ str) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(String::from(value)),
            checker: None
        }
    }
}

impl<U> From<String> for StatusPart<U>
where
    U: Ui
{
    fn from(value: String) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(value),
            checker: None
        }
    }
}

impl<U, S, ReadFn, CheckFn> From<(ReadFn, CheckFn)> for StatusPart<U>
where
    U: Ui,
    S: ToString,
    ReadFn: Fn() -> S + 'static,
    CheckFn: Fn() -> bool + 'static
{
    fn from((reader, checker): (ReadFn, CheckFn)) -> Self {
        let reader = move || reader().to_string();
        StatusPart {
            reader_or_text: ReaderOrText::Reader(Reader::Var(Box::new(reader))),
            checker: Some(Box::new(checker))
        }
    }
}

impl<U, S, ReadFn> From<ReadFn> for StatusPart<U>
where
    U: Ui,
    S: ToString,
    ReadFn: Fn(&FileWidget<U>) -> S + 'static
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &FileWidget<U>| reader(file).to_string();
        StatusPart {
            reader_or_text: ReaderOrText::Reader(Reader::File(Box::new(reader))),
            checker: None
        }
    }
}

/// Pushes the [`Form`][crate::tags::form::Form]s found in the `text`,
/// while keeping the rest of the text intact.
fn push_forms_and_text(text: &str, builder: &mut TextBuilder, palette: &FormPalette) {
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
/// use parsec_core::{
///     data::RoData,
///     tags::form::FormPalette,
///     ui::{PushSpecs, Ui},
///     widgets::{
///         file_parts::{file_name, main_cursor},
///         status_parts, FileWidget, StatusLine, Widget
///     },
///     Manager
/// };
/// fn test_fn<U>(
///     file_fn: impl Fn() -> RoData<FileWidget<U>>,
///     palette_fn: impl Fn() -> FormPalette
/// ) -> impl FnOnce(&Manager<U>) -> (Widget<U>, PushSpecs)
/// where
///     U: Ui
/// {
///     let file: RoData<FileWidget<U>> = file_fn();
///     let palette: FormPalette = palette_fn();
///
///     let parts = status_parts![
///         "file name: [FileName]",
///         file_name(),
///         "[Default] main cursor: [Coords]",
///         main_cursor(),
///     ];
///
///     StatusLine::parts_fn(parts)
/// }
/// ```
///
/// The `"[FileName]"`, `"[Default]"` and `"[Coords]"` additions serve
/// to change the active [`Form`][crate::tags::form::Form] to print
/// the next characters.
pub struct StatusLine<U>
where
    U: Ui
{
    file: RoNestedData<FileWidget<U>>,
    builder: TextBuilder,
    readers: Vec<Reader<U>>
}

impl<U> StatusLine<U>
where
    U: Ui
{
    fn new(
        file: RoNestedData<FileWidget<U>>, builder: TextBuilder, readers: Vec<Reader<U>>
    ) -> Self {
        Self {
            file,
            builder,
            readers
        }
    }

    pub fn parts_fn(
        parts: Vec<StatusPart<U>>
    ) -> impl FnOnce(&Controler<U>) -> (Self, Box<dyn Fn() -> bool>, PushSpecs) + 'static {
        move |controler| {
            let file = controler.active_file();
            let palette = &controler.palette;
            let (builder, readers, checker) = build_parts(&file.read(), parts, palette);

            (
                StatusLine::new(RoNestedData::new(file.clone()), builder, readers),
                Box::new(move || file.has_changed() || checker()),
                PushSpecs::below(Constraint::Length(1.0))
            )
        }
    }

    pub fn parts_global_fn(
        parts: Vec<StatusPart<U>>
    ) -> impl FnOnce(&Controler<U>) -> (StatusLine<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        move |controler| {
            let file = controler.dynamic_active_file();
            let palette = &controler.palette;
            let (builder, readers, checker) =
                file.inspect(|file| build_parts(file, parts, palette));
            (
                StatusLine::new(file.clone(), builder, readers),
                Box::new(move || file.has_changed() || checker()),
                PushSpecs::below(Constraint::Length(1.0))
            )
        }
    }

    /// Returns a function that outputs the default version of
    /// [`StatusLine<U>`].
    pub fn default_fn()
    -> impl FnOnce(&Controler<U>) -> (StatusLine<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        move |controler| {
            let palette = &controler.palette;
            let file = controler.active_file();
            let parts = default_parts();
            let (builder, readers, _) = build_parts(&file.read(), parts, palette);

            (
                StatusLine::new(RoNestedData::new(file.clone()), builder, readers),
                Box::new(move || file.has_changed()),
                PushSpecs::below(Constraint::Length(1.0))
            )
        }
    }

    /// Returns a function that outputs the default version of
    /// [`StatusLine<U>`].
    pub fn default_global_fn()
    -> impl FnOnce(&Controler<U>) -> (Self, Box<dyn Fn() -> bool>, PushSpecs) {
        move |controler| {
            let palette = &controler.palette;
            let parts = default_parts();
            let file = controler.dynamic_active_file();
            let (builder, readers, _) = file.inspect(|file| build_parts(file, parts, palette));
            let status_line = StatusLine::new(file.clone(), builder, readers);

            (
                status_line,
                Box::new(move || file.has_changed()),
                PushSpecs::below(Constraint::Length(1.0))
            )
        }
    }
}

impl<U> Widget<U> for StatusLine<U>
where
    U: Ui + 'static
{
    fn update(&mut self, _area: &U::Area) {
        self.file.inspect(|file| {
            for (index, reader) in self.readers.iter().enumerate() {
                self.builder.swap_range(index, reader.read(&file));
            }
        });
    }

    fn text(&self) -> &Text {
        &self.builder.text()
    }

    fn is_slow(&self) -> bool {
        false
    }
}

impl<U> DownCastableData for StatusLine<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

fn build_parts<U>(
    file: &FileWidget<U>, parts: Vec<StatusPart<U>>, palette: &FormPalette
) -> (TextBuilder, Vec<Reader<U>>, impl Fn() -> bool)
where
    U: Ui
{
    let mut builder = TextBuilder::default();
    let mut checkers = Vec::new();
    let readers = {
        let mut readers = Vec::new();
        for part in parts.into_iter() {
            let (reader, checker) = part.process(&mut builder, &file, &palette);
            reader.map(|reader| readers.push(reader));
            checker.map(|checker| checkers.push(checker));
        }
        readers
    };

    let checker = move || checkers.iter().any(|checker| checker());

    (builder, readers, checker)
}

fn default_parts<U>() -> Vec<StatusPart<U>>
where
    U: Ui
{
    status_parts![
        "[FileName]",
        file_name::<U>(),
        " [Selections]",
        selections(),
        " [Coords]",
        main_col(),
        "[Separator]:[Coords]",
        main_line(),
        "[Separator]/[Coords]",
        len_lines()
    ]
}
