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
//! #     session::Session
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
//! let mut session = Session::new(ui, print_cfg, palette, constructor_hook);
//! let specs = PushSpecs::below(Constraint::Length(1.0));
//! session.push(StatusLine::default_global_fn(), specs);
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
pub mod file_parts;

pub use file_parts::*;

use self::Reader::*;
use super::{file_widget::FileWidget, PassiveWidget, Widget};
use crate::{
    data::{AsAny, RoNestedData},
    forms::Form,
    input::InputMethod,
    status_parts,
    text::{Text, TextBuilder, Tag},
    ui::{Area, PushSpecs, Ui},
    Controler, PALETTE,
};

/// A struct that holds mutable readers, either from a file, or from
/// individual [`RoData<T>`]s
pub enum Reader {
    Var(Box<dyn FnMut() -> Text + Send + Sync + 'static>),
    File(Box<dyn FnMut(&FileWidget, &dyn InputMethod) -> Text + Send + Sync + 'static>),
}

impl Reader {
    /// Reads the current state of [`self`] and applies a function
    /// that returns a [`String`].
    fn read(&self, file: &FileWidget, input: &dyn InputMethod) -> Text {
        match self {
            Var(var_fn) => var_fn(),
            File(file_fn) => file_fn(file, input),
        }
    }
}

enum ReaderOrText {
    Reader(Reader),
    Text(String),
}

/// Part of the [`StatusLine<U>`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct StatusPart {
    reader_or_text: ReaderOrText,
    checker: Option<Box<dyn FnMut() -> bool + Send + Sync>>,
}

impl StatusPart {
    /// Consumes [`self`] and modifies a [`TextBuilder`], adding
    /// swappable ranges, text, and [`Tag`]s.
    fn process(
        self,
        builder: &mut TextBuilder,
        file: &FileWidget,
        input: &dyn InputMethod,
    ) -> (
        Option<Reader>,
        Option<Box<dyn FnMut() -> bool + Send + Sync>>,
    ) {
        match self.reader_or_text {
            ReaderOrText::Reader(Reader::Var(mut obj_fn)) => {
                builder.push_text(obj_fn());
                (Some(Reader::Var(obj_fn)), self.checker)
            }
            ReaderOrText::Reader(Reader::File(mut file_fn)) => {
                builder.push_text(file_fn(file, input));
                (Some(Reader::File(file_fn)), self.checker)
            }
            ReaderOrText::Text(text) => {
                push_forms_and_text(text.as_str(), builder);
                (None, self.checker)
            }
        }
    }
}

impl From<char> for StatusPart {
    fn from(value: char) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(String::from(value)),
            checker: None,
        }
    }
}

impl From<&'_ str> for StatusPart {
    fn from(value: &'_ str) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(String::from(value)),
            checker: None,
        }
    }
}

impl From<String> for StatusPart {
    fn from(value: String) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(value),
            checker: None,
        }
    }
}

impl<T, ReadFn, CheckFn> From<(ReadFn, CheckFn)> for StatusPart
where
    T: Into<Text>,
    ReadFn: Fn() -> T + Send + Sync + 'static,
    CheckFn: Fn() -> bool + Send + Sync + 'static,
{
    fn from((reader, checker): (ReadFn, CheckFn)) -> Self {
        let reader = move || reader().into();
        StatusPart {
            reader_or_text: ReaderOrText::Reader(Reader::Var(Box::new(reader))),
            checker: Some(Box::new(checker)),
        }
    }
}

impl<T, ReadFn> From<ReadFn> for StatusPart
where
    T: Into<Text>,
    ReadFn: Fn(&FileWidget, &dyn InputMethod) -> T + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |file: &FileWidget, input: &dyn InputMethod| reader(file, input).into();
        StatusPart {
            reader_or_text: ReaderOrText::Reader(Reader::File(Box::new(reader))),
            checker: None,
        }
    }
}

/// Pushes the [`Form`][crate::tags::form::Form]s found in the `text`,
/// while keeping the rest of the text intact.
fn push_forms_and_text(text: &str, builder: &mut TextBuilder) {
    let mut prev_l_index = None;
    for (next_l_index, _) in text.match_indices('[').chain([(text.len(), "[")]) {
        let Some(l_index) = prev_l_index else {
            builder.push_str(&text[..next_l_index]);
            prev_l_index = Some(next_l_index);
            continue;
        };

        if let Some((text_start, id)) = text[(l_index + 1)..next_l_index].find(']').map(|r_index| {
            let name = &text[(l_index + 1)..=(l_index + r_index)];
            let (_, id) = PALETTE.from_name(name);

            (l_index + r_index + 2, id)
        }) {
            builder.push_tag(Tag::PushForm(id));
            builder.push_str(&text[text_start..next_l_index]);
        } else {
            builder.push_str(&text[l_index..next_l_index]);
        }

        prev_l_index = Some(next_l_index);
    }
}

pub struct StatusLineCfg {
    parts: Option<Vec<StatusPart>>,
    is_global: bool,
    specs: PushSpecs,
}

impl StatusLineCfg {
    pub fn new() -> Self {
        Self {
            parts: None,
            is_global: false,
            specs: PushSpecs::below().with_lenght(1.0),
        }
    }

    pub fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        move |controler| {
            PALETTE.try_set_form("FileName", Form::new().yellow().italic());
            PALETTE.try_set_form("Selections", Form::new().dark_blue());
            PALETTE.try_set_form("Coords", Form::new().dark_red());
            PALETTE.try_set_form("Separator", Form::new().cyan());

            let parts = self.parts.unwrap_or_else(default_parts);

            let (file, input) = if self.is_global {
                (controler.dyn_active_file(), controler.dyn_active_input())
            } else {
                (controler.active_file(), controler.active_input())
            };

            let (builder, readers, checker) =
                file.inspect(|file| input.inspect(|input| build_parts(file, input, parts)));

            let checker = {
                let file = file.clone();
                Box::new(move || file.has_changed() || checker())
            };

            let widget = Widget::passive(StatusLine {
                file,
                input,
                builder,
                readers,
            });
            (widget, checker, self.specs)
        }
    }

    pub fn with_parts(self, parts: Vec<StatusPart>) -> Self {
        Self {
            parts: Some(parts),
            ..self
        }
    }

    pub fn global(self) -> Self {
        Self {
            is_global: true,
            ..self
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_lenght(1.0),
            ..self
        }
    }
}

impl Default for StatusLineCfg {
    fn default() -> Self {
        Self::new()
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
/// # use parsec_core::{
/// #     data::RoData,
/// #     tags::form::FormPalette,
/// #     ui::{PushSpecs, Ui},
/// #     widgets::{
/// #         file_parts::{file_name, main_cursor},
/// #         status_parts, FileWidget, StatusLine, WidgetType
/// #     },
/// #     Controler
/// # };
/// # fn test_fn<U>(
/// #     file_fn: impl Fn() -> RoData<FileWidget<U>>,
/// #     palette_fn: impl Fn() -> FormPalette
/// # ) -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs)
/// # where
/// #     U: Ui
/// # {
/// let file: RoData<FileWidget<U>> = file_fn();
/// let palette: FormPalette = palette_fn();
///
/// let parts = status_parts![
///     "file name: [FileName]",
///     file_name::<U>(),
///     "[Default] main cursor: [Coords]",
///     main_cursor(),
/// ];
///
/// StatusLine::parts_fn(parts)
/// # }
/// ```
///
/// The `"[FileName]"`, `"[Default]"` and `"[Coords]"` additions serve
/// to change the active [`Form`][crate::tags::form::Form] to print
/// the next characters.
pub struct StatusLine {
    file: RoNestedData<FileWidget>,
    input: RoNestedData<dyn InputMethod>,
    builder: TextBuilder,
    readers: Vec<Reader>,
}

impl StatusLine {
    pub fn config() -> StatusLineCfg {
        StatusLineCfg::new()
    }
}

impl PassiveWidget for StatusLine {
    fn build<U>(controler: &Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        Self::config().builder()(controler)
    }

    fn update(&mut self, _area: &impl Area) {
        self.input.inspect(|input| {
            self.file.inspect(|file| {
                for (index, reader) in self.readers.iter().enumerate() {
                    self.builder.swap_range(index, reader.read(file, input));
                }
            });
        });
    }

    fn text(&self) -> &Text {
        self.builder.text()
    }
}

impl AsAny for StatusLine {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

unsafe impl Send for StatusLine {}
unsafe impl Sync for StatusLine {}

fn build_parts(
    file: &FileWidget,
    input: &dyn InputMethod,
    parts: Vec<StatusPart>,
) -> (TextBuilder, Vec<Reader>, impl Fn() -> bool + Send + Sync) {
    let mut builder = TextBuilder::default();
    let mut checkers = Vec::new();

    builder.push_tag(BuilderTag::AlignRight);

    let readers = {
        let mut readers = Vec::new();
        for part in parts.into_iter() {
            let (reader, checker) = part.process(&mut builder, file, input);
            if let Some(reader) = reader {
                readers.push(reader)
            }
            if let Some(checker) = checker {
                checkers.push(checker)
            }
        }
        readers
    };

    let checker = move || checkers.iter().any(|checker| checker());

    (builder, readers, checker)
}

fn default_parts() -> Vec<StatusPart> {
    status_parts![
        "[FileName]",
        file_name,
        " [Selections]",
        selections_fmt,
        " [Coords]",
        main_col,
        "[Separator]:[Coords]",
        main_line,
        "[Separator]/[Coords]",
        len_lines
    ]
}

pub macro status_cfg {
    (@push $builder:expr, $text:expr) => {

    },
    (@parse_text $builder:expr, $part:tt $($parts:tt)*) => {
    },

    ($($parts:tt)*) => {
        use crate::{input::InputMethod, widgets::FileWidget};

        let text_fn = |file: &FileWidget, input: &dyn InputMethod| {
            let mut builder = crate::text::TextBuilder::new();
            status_cfg!(@text builder, $($parts)*)
        };
    }
}
