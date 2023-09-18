//! A [`NormalWidget`] that serves the purpose of showing general
//! information.
//!
//! This widget has an associated [`FileWidget`] that's used as a
//! reference. This conotates that a [`StatusLine`] must be tied to
//! a single file, but this is not really the case. The end user may
//! be able to open multiple files and have just a single
//! [`StatusLine`] that displays the information of only the
//! currently active [`FileWidget`]:
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
//! In the example above, every time a [`FileWidget`] is opened
//! with a new file, a [`StatusLine`] will be pushed below it, as
//! seen in the `constructor_hook`. This [`StatusLine`] will show
//! information about that specific [`FileWidget`].
//!
//! Also in the example above, you can see that a second
//! [`StatusLine`] is pushed by [`PushSpecs::below()`]. As such,
//! this widget will be placed below all others, and the
//! [`default_global_fn()`][StatusLine::default_global_fn()] means
//! that it is global, and instead of pointing to a specific
//! [`FileWidget`], it will change to always point at the currently
//! active one.
//!
//! In a real life situation, you would choose only one of these
//! aproaches, as having two [`StatusLine`]s showing the same
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
    text::Text,
    ui::{Area, PushSpecs, Ui},
    Controler,
};

/// A struct that holds mutable readers, either from a file, or from
/// individual [`RoData<T>`]s
pub enum Reader {
    Var(Box<dyn Fn() -> Text + Send + Sync + 'static>),
    File(Box<dyn Fn(&FileWidget, &dyn InputMethod) -> Text + Send + Sync + 'static>),
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
    Text(Text),
}

/// Part of the [`StatusLine`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct StatusPart {
    reader_or_text: ReaderOrText,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
}

impl StatusPart {
    fn check(&self) -> bool {
        match &self.checker {
            Some(checker) => checker(),
            None => false,
        }
    }

    fn read(&self, file: &FileWidget, input: &dyn InputMethod) -> Text {
        match &self.reader_or_text {
            ReaderOrText::Reader(reader) => reader.read(file, input),
            ReaderOrText::Text(text) => text.clone(),
        }
    }
}

impl From<char> for StatusPart {
    fn from(value: char) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(Text::from(value)),
            checker: None,
        }
    }
}

impl From<&'_ str> for StatusPart {
    fn from(value: &'_ str) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(Text::from(value)),
            checker: None,
        }
    }
}

impl From<String> for StatusPart {
    fn from(value: String) -> Self {
        StatusPart {
            reader_or_text: ReaderOrText::Text(Text::from(value)),
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

pub struct StatusLineCfg {
    text_fn: Box<dyn Fn(&FileWidget, &dyn InputMethod) -> Text>,
    checker: Box<dyn Fn() -> bool>,
    is_global: bool,
    specs: PushSpecs,
}

impl StatusLineCfg {
    pub fn new() -> Self {
        status_cfg!(
            [FileName] file_name " " [Selections] selections_fmt " "
            [Coords] main_col [Separator] ":" [Coords] main_line [Separator]
            "/" [Coords] len_lines
        )
    }

    pub fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        move |controler| {
            let (file, input) = if self.is_global {
                (controler.dyn_active_file(), controler.dyn_active_input())
            } else {
                (controler.active_file(), controler.active_input())
            };

            let checker = {
                let file = file.clone();
                Box::new(move || file.has_changed() || (self.checker)())
            };

            let widget = Widget::passive(StatusLine {
                file,
                input,
                text_fn: self.text_fn,
                text: Text::default_string(),
            });

            (widget, checker, self.specs)
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
/// The [`StatusLine`] is built around a [`Vec<Reader>`], which
/// determines exacly how it will be updated. There is a default form
/// for the [`Vec<Reader>`] in [`StatusLine::default_fn()`], which
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
    text_fn: Box<dyn Fn(&FileWidget, &dyn InputMethod) -> Text>,
    text: Text,
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
                self.text = (self.text_fn)(file, input);
            });
        });
    }

    fn text(&self) -> &Text {
        &self.text
    }
}

impl AsAny for StatusLine {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

unsafe impl Send for StatusLine {}
unsafe impl Sync for StatusLine {}

pub macro status_cfg {
    (@push_text $builder:expr, $_file:expr, $_input:expr, [$form:ident]) => {
        use std::sync::LazyLock;
        static FORM_ID: LazyLock<crate::forms::FormId> = LazyLock::new(|| {
            let name = stringify!($form);
            crate::PALETTE.from_name(name).1
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push_text $builder:expr, $_file:expr, $_input:expr, [[$form_id:expr]]) => {
        $builder.push_tag(crate::text::Tag::PushForm($form_id))
    },

    // Other tags
    (@push_text $builder:expr, $_file:expr, $_input:expr, (($tag:expr))) => {
        $builder.push_tag($tag)
    },

    // Failure
    (@push_text $_builder:expr, $_file:expr, $_input:expr, ($_not_allowed:expr)) => {
        compile_error!(
            "Expressions are not allowed in place of tag identifiers. If you wanted to add text \
             using an expression, remove the parentheses and replace them with braces. If you \
             wanted to add a tag with an expression, surround it with another pair of parentheses \
             (e.g. `((my_expr))`)"
        );
    },

    // Direct Text insertions.
    (@push_text $builder:expr, $file:expr, $input:expr, $text:expr) => {
        use std::sync::LazyLock;
        static PART: LazyLock<crate::widgets::StatusPart> = LazyLock::new(|| {
            StatusPart::from($text)
        });

        $builder.push_text(PART.read($file, $input))
    },

    (@parse_text $_builder:expr, $_file:expr, $_input:expr,) => {},

    (@parse_text $builder:expr, $file:expr, $input:expr, $part:tt $($parts:tt)*) => {
        status_cfg!(@push_text $builder, $file, $input, $part);
        status_cfg!(@parse_text $builder, $file, $input, $($parts)*);
    },

	////////// Checker parsing
    (@push_check $_needs_update:expr, [$_form:ident]) => {},
    (@push_check $_needs_update:expr, [[$_form_id:expr]]) => {},

    // Other tags
    (@push_check $_needs_update:expr, (($_tag:expr))) => {},

    // Failure
    (@push_check $_needs_update:expr, ($_not_allowed:expr)) => {},

    // Direct Text insertions.
    (@push_check $needs_update:expr, $text:expr) => {
        use std::sync::LazyLock;
        static PART: LazyLock<crate::widgets::StatusPart> = LazyLock::new(|| {
            StatusPart::from($text)
        });

        *$needs_update |= PART.check()
    },

    (@parse_check $_needs_update:expr,) => {},

    (@parse_check $needs_update:expr, $part:tt $($parts:tt)*) => {
        status_cfg!(@push_check $needs_update, $part);
        status_cfg!(@parse_check $needs_update, $($parts)*);
    },

    ($($parts:tt)*) => {{
        use crate::{
            PALETTE,
            input::InputMethod,
            text::build,
            ui::PushSpecs,
            widgets::{FileWidget, StatusLineCfg},
        };

        let text_fn = |file: &FileWidget, input: &dyn InputMethod| {
            let mut builder = build!((AlignRight));
            status_cfg!(@parse_text builder, file, input, $($parts)*);
            builder.finish()
        };

        let checker = || {
            let mut needs_update = false;
            status_cfg!(@parse_check &mut needs_update, $($parts)*);
            needs_update
        };

        PALETTE.try_set_form("FileName", Form::new().yellow().italic());
        PALETTE.try_set_form("Selections", Form::new().dark_blue());
        PALETTE.try_set_form("Coords", Form::new().dark_red());
        PALETTE.try_set_form("Separator", Form::new().cyan());

        StatusLineCfg {
            text_fn: Box::new(text_fn),
            checker: Box::new(checker),
            is_global: false,
            specs: PushSpecs::below().with_lenght(1.0)
        }
    }}
}
