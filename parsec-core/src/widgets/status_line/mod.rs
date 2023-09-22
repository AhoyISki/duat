//! A [`NormalWidget`] that serves the purpose of showing general
//! information.
//!
//! This widget has an associated [`File`] that's used as a
//! reference. This conotates that a [`StatusLine`] must be tied to
//! a single file, but this is not really the case. The end user may
//! be able to open multiple files and have just a single
//! [`StatusLine`] that displays the information of only the
//! currently active [`File`]:
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
//! In the example above, every time a [`File`] is opened
//! with a new file, a [`StatusLine`] will be pushed below it, as
//! seen in the `constructor_hook`. This [`StatusLine`] will show
//! information about that specific [`File`].
//!
//! Also in the example above, you can see that a second
//! [`StatusLine`] is pushed by [`PushSpecs::below()`]. As such,
//! this widget will be placed below all others, and the
//! [`default_global_fn()`][StatusLine::default_global_fn()] means
//! that it is global, and instead of pointing to a specific
//! [`File`], it will change to always point at the currently
//! active one.
//!
//! In a real life situation, you would choose only one of these
//! aproaches, as having two [`StatusLine`]s showing the same
//! information at the same time is quite redundant. But this is a
//! good showing for the flexibility of this widget.

#[macro_use]
pub mod file_parts;

pub use self::file_parts::*;
use super::{file_widget::File, PassiveWidget, Widget};
use crate::{
    data::{FileReader, RoData},
    forms::Form,
    input::InputMethod,
    text::{Text, TextBuilder},
    ui::{Area, PushSpecs, Ui},
    Controler, ACTIVE_FILE,
};

/// A struct that reads state in order to return [`Text`].
enum Appender<T> {
    NoArgs(Box<dyn Fn() -> Text + Send + Sync + 'static>),
    FromFile(Box<dyn Fn(&T) -> Text + Send + Sync + 'static>),
    DynInput(Box<dyn Fn(&dyn InputMethod) -> Text + Send + Sync + 'static>),
    Text(Text),
}

/// Part of the [`StatusLine`], can either be a
/// [`&'static str`][str], or a dynamically updated [`Reader`].
pub struct StatusPart<T: 'static> {
    appender: Appender<T>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
}

impl<T: 'static> StatusPart<T> {
    pub fn check(&self) -> bool {
        match &self.checker {
            Some(checker) => checker(),
            None => false,
        }
    }

    fn appender_fn(
        self,
    ) -> Box<dyn Fn(&mut TextBuilder, &RoData<File>, &RoData<dyn InputMethod>) + Send + Sync> {
        match self.appender {
            Appender::NoArgs(f) => Box::new(move |builder, _, _| builder.push_text(f())),
            Appender::FromFile(f) => Box::new(move |builder, file, input| {
                let text = input
                    .inspect_as::<T, Text>(&f)
                    .or_else(|| file.inspect_as::<T, Text>(&f));

                if let Some(text) = text {
                    builder.push_text(text)
                }
            }),
            Appender::DynInput(f) => {
                Box::new(move |builder, _, input| builder.push_text(f(&*input.read())))
            }
            Appender::Text(text) => Box::new(move |builder, _, _| builder.push_text(text.clone())),
        }
    }
}

impl From<char> for StatusPart<()> {
    fn from(value: char) -> Self {
        StatusPart {
            appender: Appender::Text::<()>(Text::from(value)),
            checker: None,
        }
    }
}

impl From<&'_ str> for StatusPart<()> {
    fn from(value: &'_ str) -> Self {
        StatusPart {
            appender: Appender::Text::<()>(Text::from(value)),
            checker: None,
        }
    }
}

impl From<String> for StatusPart<()> {
    fn from(value: String) -> Self {
        StatusPart {
            appender: Appender::Text::<()>(Text::from(value)),
            checker: None,
        }
    }
}

impl<T, ReadFn, CheckFn> From<(ReadFn, CheckFn)> for StatusPart<()>
where
    T: Into<Text>,
    ReadFn: Fn() -> T + Send + Sync + 'static,
    CheckFn: Fn() -> bool + Send + Sync + 'static,
{
    fn from((reader, checker): (ReadFn, CheckFn)) -> Self {
        let reader = move || reader().into();
        StatusPart {
            appender: Appender::NoArgs::<()>(Box::new(reader)),
            checker: Some(Box::new(checker)),
        }
    }
}

// TODO: Try the stupid ass TryFrom gambit.
impl<T, U, ReadFn> From<ReadFn> for StatusPart<U>
where
    T: Into<Text>,
    ReadFn: Fn(&U) -> T + Send + Sync + 'static,
{
    fn from(reader: ReadFn) -> Self {
        let reader = move |u: &U| reader(u).into();
        StatusPart {
            appender: Appender::FromFile(Box::new(reader)),
            checker: None,
        }
    }
}

pub struct StatusLineCfg {
    text_fn: Box<dyn Fn(&RoData<File>, &RoData<dyn InputMethod>) -> Text>,
    checker: Box<dyn Fn() -> bool>,
    is_global: bool,
    specs: PushSpecs,
}

impl StatusLineCfg {
    pub fn new() -> Self {
        status_cfg!("HAHAHA")
    }

    pub fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        move |_| {
            let (reader, checker): (Option<FileReader>, Box<dyn Fn() -> bool>) = if self.is_global {
                (
                    None,
                    Box::new(move || ACTIVE_FILE.has_changed() || (self.checker)()),
                )
            } else {
                let reader = ACTIVE_FILE.current();
                (
                    Some(ACTIVE_FILE.current()),
                    Box::new(move || reader.has_changed() || (self.checker)()),
                )
            };

            let widget = Widget::passive(StatusLine {
                reader,
                text_fn: self.text_fn,
                text: Text::default(),
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
/// #         status_parts, File, StatusLine, WidgetType
/// #     },
/// #     Controler
/// # };
/// # fn test_fn<U>(
/// #     file_fn: impl Fn() -> RoData<File<U>>,
/// #     palette_fn: impl Fn() -> FormPalette
/// # ) -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs)
/// # where
/// #     U: Ui
/// # {
/// let file: RoData<File<U>> = file_fn();
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
    reader: Option<FileReader>,
    text_fn: Box<dyn Fn(&RoData<File>, &RoData<dyn InputMethod>) -> Text>,
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
        self.text = if let Some(reader) = &self.reader {
            reader.inspect_data(&self.text_fn)
        } else {
            ACTIVE_FILE.inspect_data(&self.text_fn)
        };
    }

    fn text(&self) -> &Text {
        &self.text
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
    (@push_text $builder:expr, $_file:expr, $_input:expr, ($tag:expr)) => {
        $builder.push_tag($tag)
    },

    // Direct Text insertions.
    (@push_text $builder:expr, $file:expr, $input:expr, $text:expr) => {
        use std::sync::LazyLock;
        static APPENDER: LazyLock<
            Box<dyn Fn(&mut TextBuilder, &RoData<File>, &RoData<dyn InputMethod>) + Send + Sync>,
        > = LazyLock::new(|| StatusPart::from($text).appender_fn());

        APPENDER($builder, $file, $input)
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
        static CHECKER: LazyLock<Option<Box<dyn Fn() -> bool + Send + Sync>>> = LazyLock::new(|| {
            StatusPart::from($text).checker
        });

        *$needs_update |= CHECKER.as_ref().is_some_and(|checker| checker());
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
            widgets::{File, StatusLineCfg},
        };

        let text_fn = |file: &RoData<File>, input: &RoData<dyn InputMethod>| {
            let mut builder = build![(AlignRight)];
            status_cfg!(@parse_text &mut builder, file, input, $($parts)*);
            build!(builder, "\n");
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
