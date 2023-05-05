#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{any::Any, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use super::{file_widget::FileWidget, NormalWidget, Widget};
use crate::{
    config::{DownCastableData, RoData},
    tags::{
        form::{FormPalette, COORDS, FILE_NAME, SELECTIONS, SEPARATOR},
        Tag
    },
    text::{Text, TextBuilder},
    ui::{PushSpecs, Ui},
    updaters, SessionManager};

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
    fn read(&self, file: &FileWidget<U>) -> String {
        match self {
            Reader::Var(obj_fn) => obj_fn(),
            Reader::File(file_fn) => file_fn(file)
        }
    }
}

pub enum StatusPart<U>
where
    U: Ui
{
    Dynamic(Reader<U>),
    Static(&'static str)
}

impl<U> StatusPart<U>
where
    U: Ui
{
    fn process(
        self, text_builder: &mut TextBuilder<U>, file: &FileWidget<U>, palette: &FormPalette
    ) -> Option<Reader<U>> {
        match self {
            StatusPart::Dynamic(Reader::Var(obj_fn)) => {
                text_builder.push_swappable(obj_fn());
                Some(Reader::Var(obj_fn))
            }
            StatusPart::Dynamic(Reader::File(file_fn)) => {
                text_builder.push_swappable(file_fn(file));
                Some(Reader::File(file_fn))
            }
            StatusPart::Static(text) => {
                push_forms_and_text(text, text_builder, palette);
                None
            }
        }
    }
}

fn push_forms_and_text<U>(text: &str, text_builder: &mut TextBuilder<U>, palette: &FormPalette)
where
    U: Ui
{
    let mut prev_l_index = None;
    for (next_l_index, _) in text.match_indices('[').chain([(text.len(), "[")]) {
        let Some(l_index) = prev_l_index else {
            text_builder.push_text(&text[..next_l_index]);
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
            text_builder.push_tag(Tag::PushForm(form_id));
            text_builder.push_text(&text[text_start..next_l_index]);
        } else {
            text_builder.push_text(&text[l_index..next_l_index]);
        }

        prev_l_index = Some(next_l_index);
    }
}

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
    pub fn clippable_fn(
        file: RoData<FileWidget<U>>, status_parts: Vec<StatusPart<U>>, palette: &FormPalette
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        let mut text_builder = TextBuilder::default();
        let readers = {
            let mut readers = Vec::new();
            let file = file.read();
            for part in status_parts.into_iter() {
                if let Some(reader) = part.process(&mut text_builder, &file, palette) {
                    readers.push(reader);
                }
            }
            readers
        };

        StatusLine::new_fn(file, text_builder, readers, true)
    }

    pub fn unclippable_fn(
        file: RoData<FileWidget<U>>, status_parts: Vec<StatusPart<U>>, palette: &FormPalette
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        let mut text_builder = TextBuilder::default();
        let readers = {
            let mut readers = Vec::new();
            let file = file.read();
            for part in status_parts.into_iter() {
                if let Some(reader) = part.process(&mut text_builder, &file, palette) {
                    readers.push(reader);
                }
            }
            readers
        };

        StatusLine::new_fn(file, text_builder, readers, false)
    }

    fn new_fn(
        file: RoData<FileWidget<U>>, builder: TextBuilder<U>, readers: Vec<Reader<U>>,
        _clippable: bool
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
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

    pub fn default_fn(
        file: RoData<FileWidget<U>>
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
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

    pub fn set_file(&mut self, file_widget: RoData<FileWidget<U>>) {
        self.file = file_widget;
    }
}

fn file_lines_len<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.len_lines().to_string())
}

fn main_line<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.main_cursor().row().to_string())
}

fn main_col<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.main_cursor().col().to_string())
}

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

fn file_name<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui
{
    Box::new(|file| file.name().to_string())
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

pub fn f_var<U, S>(file_fn: impl Fn(&FileWidget<U>) -> S + 'static) -> StatusPart<U>
where
    U: Ui,
    S: ToString
{
    let file_fn = Box::new(move |file: &FileWidget<U>| file_fn(file).to_string());
    StatusPart::Dynamic(Reader::File(file_fn))
}

pub fn var<U, S>(var_fn: impl Fn() -> S + 'static) -> StatusPart<U>
where
    U: Ui,
    S: ToString
{
    let var_fn = Box::new(move || var_fn().to_string());
    StatusPart::Dynamic(Reader::Var(var_fn))
}

pub fn text<U>(text: &'static str) -> StatusPart<U>
where
    U: Ui
{
    StatusPart::Static(text)
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
