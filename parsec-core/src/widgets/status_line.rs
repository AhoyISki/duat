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
        Tag,
    },
    text::{Text, TextBuilder},
    ui::{EndNode, PushSpecs, Ui},
    updaters, SessionManager,
};

pub enum Reader<U>
where
    U: Ui,
{
    Obj(Box<dyn Fn() -> String>),
    File(Box<dyn Fn(&FileWidget<U>) -> String>),
}

impl<U> Reader<U>
where
    U: Ui,
{
    fn read(&self, file_widget: &FileWidget<U>) -> String {
        match self {
            Reader::Obj(obj_fn) => obj_fn(),
            Reader::File(file_fn) => file_fn(file_widget),
        }
    }
}

pub enum StatusPart<U>
where
    U: Ui,
{
    Dynamic(Reader<U>),
    Static(&'static str),
}

impl<U> StatusPart<U>
where
    U: Ui,
{
    fn process(
        self,
        text_builder: &mut TextBuilder<U>,
        file_widget: &FileWidget<U>,
        palette: &FormPalette,
    ) -> Option<Reader<U>> {
        match self {
            StatusPart::Dynamic(Reader::Obj(obj_fn)) => {
                text_builder.push_swappable(obj_fn());
                Some(Reader::Obj(obj_fn))
            }
            StatusPart::Dynamic(Reader::File(file_fn)) => {
                text_builder.push_swappable(file_fn(file_widget));
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
    U: Ui,
{
    let mut prev_l_index = None;
    for (next_l_index, _) in text.match_indices('[').chain([(text.len(), "[")]) {
        let Some(l_index) = prev_l_index else {
            text_builder.push_text(&text[..next_l_index]);
            prev_l_index = Some(next_l_index);
            continue;
        };

        if let Some((r_index, (_, form_id))) = text[(l_index + 1)..next_l_index]
            .find(']')
            .map(|r_index| {
                palette.get_from_name(&text[l_index..r_index]).map(|form_id| (r_index, form_id))
            })
            .flatten()
        {
            text_builder.push_tag(Tag::PushForm(form_id));
            text_builder.push_text(&text[(r_index + 1)..next_l_index]);
        } else {
            text_builder.push_text(&text[l_index..next_l_index]);
        }

        prev_l_index = Some(next_l_index);
    }
}

pub struct StatusLine<U>
where
    U: Ui,
{
    file_widget: RoData<FileWidget<U>>,
    text_builder: TextBuilder<U>,
    readers: Vec<Reader<U>>,
    clippable: bool,
}

impl<U> StatusLine<U>
where
    U: Ui,
{
    pub fn clippable_fn(
        file_widget: RoData<FileWidget<U>>,
        status_parts: Vec<StatusPart<U>>,
        palette: &FormPalette,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        let mut text_builder = TextBuilder::default();
        let mut readers = Vec::new();
        let file = file_widget.read();
        for part in status_parts.into_iter() {
            if let Some(reader) = part.process(&mut text_builder, &file, palette) {
                readers.push(reader);
            }
        }

        drop(file);
        StatusLine::new_fn(file_widget, text_builder, readers, false)
    }

    pub fn unclippable_fn(
        file_widget: RoData<FileWidget<U>>,
        status_parts: Vec<StatusPart<U>>,
        palette: &FormPalette,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        let mut text_builder = TextBuilder::default();
        let mut readers = Vec::new();
        let file = file_widget.read();
        for part in status_parts.into_iter() {
            if let Some(reader) = part.process(&mut text_builder, &file, palette) {
                readers.push(reader);
            }
        }

        drop(file);
        StatusLine::new_fn(file_widget, text_builder, readers, false)
    }

    fn new_fn(
        file_widget: RoData<FileWidget<U>>,
        text_builder: TextBuilder<U>,
        readers: Vec<Reader<U>>,
        clippable: bool,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, _| {
            let updaters = updaters![(file_widget.clone())];
            Widget::normal(
                Arc::new(RwLock::new(StatusLine {
                    file_widget,
                    text_builder,
                    readers,
                    clippable,
                })),
                updaters,
            )
        })
    }

    pub fn default_fn(
        file_widget: RoData<FileWidget<U>>,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        let name = Reader::File(file_name());
        let sels = Reader::File(file_selections());
        let col = Reader::File(main_col());
        let line = Reader::File(main_line());
        let lines = Reader::File(file_lines_len());

        let file = file_widget.read();
        let mut text_builder = TextBuilder::default();

        text_builder.push_tag(Tag::PushForm(FILE_NAME));
        text_builder.push_swappable(name.read(&file));
        text_builder.push_text(" ");
        text_builder.push_tag(Tag::PushForm(SELECTIONS));
        text_builder.push_swappable(sels.read(&file));
        text_builder.push_text(" ");
        text_builder.push_tag(Tag::PushForm(COORDS));
        text_builder.push_swappable(col.read(&file));
        text_builder.push_tag(Tag::PushForm(SEPARATOR));
        text_builder.push_text(":");
        text_builder.push_tag(Tag::PushForm(COORDS));
        text_builder.push_swappable(line.read(&file));
        text_builder.push_tag(Tag::PushForm(SEPARATOR));
        text_builder.push_text("/");
        text_builder.push_tag(Tag::PushForm(COORDS));
        text_builder.push_swappable(lines.read(&file));

        let readers = vec![name, sels, col, line, lines];

        drop(file);
        StatusLine::new_fn(file_widget, text_builder, readers, true)
    }

    pub fn set_file(&mut self, file_widget: RoData<FileWidget<U>>) {
        self.file_widget = file_widget;
    }
}

fn file_lines_len<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui,
{
    Box::new(|file| file.len_lines().to_string())
}

fn main_line<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui,
{
    Box::new(|file| file.main_cursor().row().to_string())
}

fn main_col<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui,
{
    Box::new(|file| file.main_cursor().col().to_string())
}

fn file_selections<U>() -> Box<dyn Fn(&FileWidget<U>) -> String>
where
    U: Ui,
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
    U: Ui,
{
    Box::new(|file| file.name().to_string())
}

impl<U> NormalWidget<U> for StatusLine<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        "parsec-status-line"
    }

    fn update(&mut self, _end_node: &mut EndNode<U>) {
        let file = self.file_widget.read();

        for (index, reader) in self.readers.iter().enumerate() {
            self.text_builder.swap_range(index, reader.read(&file));
        }
    }

    fn needs_update(&self) -> bool {
        true
    }

    fn text(&self) -> &Text<U> {
        &self.text_builder.text()
    }
}

impl<U> DownCastableData for StatusLine<U>
where
    U: Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[macro_export]
macro_rules! status_format {
	// File closures are bounded by "f(" and ")".
    (@process_tokens $builder:ident, f(|$file:ident| $closure:expr), $remainder:tt) => {
        let closure = |$file| { $closure.to_string() };
        $builder.push(RopePart::FileFn(Box::new(closure)));

        startus_format!(@process_tokens $builder, $remainder);
    };

	// Object closures are bounded by "o(" and ")".
    (@process_tokens $builder:ident, o(|$obj:ident| $closure:expr), $remainder:tt) => {
        let closure = |$obj| { $closure.to_string() };
        let part = RopePart::new_obj_fn(&$obj, closure);
        $builder.push(part);

        status_format!(@process_tokens $builder, $remainder);
    };

	// Objects that implement `ToString` are also bounded by "o(" and ")".
    (@process_tokens $builder:ident, o($obj:expr), $remainder:tt) => {
        let closure = |data| { data.to_string() };
        let part = RopePart::new_obj_fn(&$obj, closure);
        $builder.push(part);

        status_format!(@process_tokens $builder, $remainder);
    };

	// Forms preceed a ":"
    (@process_tokens $builder:ident, $form:ident:, $remainder:tt) => {
        builder.push(RopePart::Form(stringify!($form)));

        status_format!(@process_tokens $builder, $remainder);
    };

	// Remaining expressions are assumed to be text.
    (@process_tokens $builder:ident, $text:expr, $remainder:tt) => {
        builder.push(RopePart::Text(String::from($text)));

        status_format!(@process_tokens $builder, $remainder);
    };

    (
        $palette:expr, left: $left:tt, center: $center:tt, right: $right:tt,
    ) => {
        {
            let mut left_builder = RopeBuilder::new();
            $(
                status_format!(@process_tokens left_builder, $left:tt);
            )*

            let mut center_builder = RopeBuilder::new();
            $(
                status_format!(@process_tokens center_builder, $left:tt);
            )*

            let mut right_builder = RopeBuilder::new();
            $(
                status_format!(@process_tokens right_builder, $left:tt);
            )*

    		*format.left_text_mut() = $left.to_string();
    		*format.center_text_mut() = $center.to_string();
    		*format.right_text_mut() = $right.to_string();

    		format
        }
    };
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
