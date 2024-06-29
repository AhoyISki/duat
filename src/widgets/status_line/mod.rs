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
//! # use duat_core::{
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

pub mod common;
mod state;

use common::{main_col, main_line, selections_fmt};
use duat_core::{
    data::FileReader,
    palette::{self, Form},
    text::{text, Builder, Tag, Text},
    ui::PushSpecs,
    widgets::{File, PassiveWidget, Widget, WidgetCfg},
    Context,
};

pub use self::state::State;
use crate::Ui;

pub struct StatusLineCfg {
    text_fn: TextFn,
    checker: Box<dyn Fn() -> bool>,
    specs: PushSpecs,
}

impl StatusLineCfg {
    pub fn new() -> Self {
        status!(
            [File] { File::name } " " [Selections] selections_fmt " "
            [Coords] main_col [Separator] ":" [Coords] main_line
            [Separator] "/" [Coords] { File::len_lines }
        )
    }

    pub fn new_with(text_fn: TextFn, checker: Box<dyn Fn() -> bool>, specs: PushSpecs) -> Self {
        Self {
            text_fn,
            checker,
            specs,
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_lenght(1.0),
            ..self
        }
    }
}

impl WidgetCfg<Ui> for StatusLineCfg {
    type Widget = StatusLine;

    fn build(
        self,
        context: Context<Ui>,
        on_file: bool,
    ) -> (Widget<Ui>, impl Fn() -> bool, PushSpecs) {
        let (reader, checker) = if on_file {
            let reader = context.current_file.constant();
            let checker = move || reader.has_changed() || (self.checker)();
            (
                context.current_file.constant(),
                Box::new(checker) as Box<dyn Fn() -> bool>,
            )
        } else {
            let reader = context.current_file.adaptive();
            let checker = move || reader.has_changed() || (self.checker)();
            (
                context.current_file.adaptive(),
                Box::new(checker) as Box<dyn Fn() -> bool>,
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

impl Default for StatusLineCfg {
    fn default() -> Self {
        Self::new()
    }
}

/// A [`NormalWidget`] that can display information about Duat and
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
/// # use duat_core::{
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
///     "file name: [File]",
///     file_name::<U>(),
///     "[] main cursor: [Coords]",
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
    reader: FileReader<Ui>,
    text_fn: TextFn,
    text: Text,
}

impl StatusLine {
    pub fn config() -> StatusLineCfg {
        StatusLineCfg::new()
    }
}

impl PassiveWidget<Ui> for StatusLine {
    fn build(context: Context<Ui>, on_file: bool) -> (Widget<Ui>, impl Fn() -> bool, PushSpecs) {
        Self::config().build(context, on_file)
    }

    fn update(&mut self, _area: &<Ui as duat_core::ui::Ui>::Area) {
        self.text = (self.text_fn)(&self.reader);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn once(_context: Context<Ui>) {
        palette::set_weak_form("File", Form::new().yellow().italic());
        palette::set_weak_form("Selections", Form::new().dark_blue());
        palette::set_weak_form("Coord", Form::new().dark_red());
        palette::set_weak_form("Separator", Form::new().cyan());
    }
}

unsafe impl Send for StatusLine {}
unsafe impl Sync for StatusLine {}

pub macro status {
    (@append $text_fn:expr, $checker:expr, []) => {{
        let form_id = palette::__weakest_id_of_name("Default");

        let text_fn = move |builder: &mut Builder, reader: &FileReader<Ui>| {
            $text_fn(builder, reader);
            builder.push_tag(Tag::PushForm(form_id));
        };

        (text_fn, $checker)
    }},

    // Insertion of directly named forms.
    (@append $text_fn:expr, $checker:expr, [$form:ident]) => {{
        let form_id = palette::__weakest_id_of_name(stringify!($form));

        let text_fn = move |builder: &mut Builder, reader: &FileReader<Ui>| {
            $text_fn(builder, reader);
            builder.push_tag(Tag::PushForm(form_id));
        };

        (text_fn, $checker)
    }},

	// Insertion of text, reading functions, or tags.
    (@append $text_fn:expr, $checker:expr, $text:expr) => {{
        let (mut appender, checker) = State::from($text).fns();

        let checker = move || { $checker() || checker() };

        let text_fn = move |builder: &mut Builder, reader: &FileReader<Ui>| {
            $text_fn(builder, reader);
            appender(builder, reader);
        };

        (text_fn, checker)
    }},

    (@parse $text_fn:expr, $checker:expr,) => { ($text_fn, $checker) },

    (@parse $text_fn:expr, $checker:expr, $part:tt $($parts:tt)*) => {{
        let (mut text_fn, checker) = status!(@append $text_fn, $checker, $part);
        status!(@parse text_fn, checker, $($parts)*)
    }},

    (@parse $($parts:tt)*) => {{
        let text_fn = |_: &mut Builder, _: &FileReader<Ui>| {};
        let checker = || { false };
        status!(@parse text_fn, checker, $($parts)*)
    }},

    ($($parts:tt)*) => {{
		#[allow(unused_mut)]
        let (mut text_fn, checker) = status!(@parse $($parts)*);

        let text_fn = move |reader: &FileReader<Ui>| {
            let mut builder = Builder::new();
            text!(builder, { Tag::StartAlignRight });
            text_fn(&mut builder, reader);
            text!(builder, "\n");
            builder.finish()
        };

        StatusLineCfg::new_with(
            Box::new(text_fn),
            Box::new(checker),
            PushSpecs::below().with_lenght(1.0)
        )
    }}
}

type TextFn = Box<dyn FnMut(&FileReader<Ui>) -> Text>;
