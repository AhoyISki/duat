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

pub mod file_parts;
mod state;

use file_parts::{main_col, main_line, selections_fmt};
use parsec_core::{
    data::{FileReader, RoData},
    input::InputMethod,
    palette::{self, Form},
    text::{text, Builder, Text},
    ui::PushSpecs,
    widgets::{File, PassiveWidget, Widget, WidgetCfg},
    Globals,
};

pub use self::state::State;
use crate::Ui;

pub struct StatusLineCfg {
    text_fn: TextFn,
    checker: Box<dyn Fn() -> bool>,
    is_global: bool,
    specs: PushSpecs,
}

impl StatusLineCfg {
    pub fn new() -> Self {
        status!(
            [FileName] { File::name } " " [Selections] selections_fmt " "
            [Coords] main_col [Separator] ":" [Coords] main_line
            [Separator] "/" [Coords] { File::len_lines }
        )
    }

    pub fn new_with(
        text_fn: TextFn,
        checker: Box<dyn Fn() -> bool>,
        is_global: bool,
        specs: PushSpecs,
    ) -> Self {
        Self {
            text_fn,
            checker,
            is_global,
            specs,
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

impl WidgetCfg<Ui> for StatusLineCfg {
    type Widget = StatusLine;

    fn build(self, globals: Globals<Ui>) -> (Widget<Ui>, impl Fn() -> bool, PushSpecs) {
        let (reader, checker) = if self.is_global {
            let reader = globals.current_file.adaptive();
            let checker = move || reader.has_changed() || (self.checker)();
            (
                globals.current_file.adaptive(),
                Box::new(checker) as Box<dyn Fn() -> bool>,
            )
        } else {
            let reader = globals.current_file.constant();
            let checker = move || reader.has_changed() || (self.checker)();
            (
                globals.current_file.constant(),
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
    fn build(globals: Globals<Ui>) -> (Widget<Ui>, impl Fn() -> bool, PushSpecs) {
        Self::config().build(globals)
    }

    fn update(&mut self, _area: &<Ui as parsec_core::ui::Ui>::Area) {
        self.text = self
            .reader
            .inspect_data(|file, _, input| (self.text_fn)(file, input));
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn once(_globals: Globals<Ui>) {
        palette::set_weak_form("FileName", Form::new().yellow().italic());
        palette::set_weak_form("Selections", Form::new().dark_blue());
        palette::set_weak_form("Coords", Form::new().dark_red());
        palette::set_weak_form("Separator", Form::new().cyan());
    }
}

unsafe impl Send for StatusLine {}
unsafe impl Sync for StatusLine {}

pub macro status {
    // Insertion of directly named forms.
    (@append $text_fn:expr, $checker:expr, [$form:ident]) => {{
        let form_id = parsec_core::palette::weakest_id_of_name(stringify!($form));

        let text_fn = move |builder: &mut Builder,
                            file: &RoData<File<Ui>>,
                            input: &RoData<dyn InputMethod<Ui>>| {
            $text_fn(builder, file, input);
            builder.push_tag(parsec_core::text::Tag::PushForm(form_id));
        };

        (text_fn, $checker)
    }},

	// Insertion of text, reading functions, or tags.
    (@append $text_fn:expr, $checker:expr, $text:expr) => {{
        let (mut appender, checker) = State::from($text).fns();

        let checker = move || { $checker() || checker() };

        let text_fn = move |builder: &mut Builder,
                            file: &RoData<File<Ui>>,
                            input: &RoData<dyn InputMethod<Ui>>| {
            $text_fn(builder, file, input);
            appender(builder, file, input);
        };

        (text_fn, checker)
    }},

    (@parse $text_fn:expr, $checker:expr,) => { ($text_fn, $checker) },

    (@parse $text_fn:expr, $checker:expr, $part:tt $($parts:tt)*) => {{
        let (mut text_fn, checker) = status!(@append $text_fn, $checker, $part);
        status!(@parse text_fn, checker, $($parts)*)
    }},

    (@parse $($parts:tt)*) => {{
        let text_fn = |_: &mut Builder, _: &RoData<File<Ui>>, _: &RoData<dyn InputMethod<Ui>>| {};
        let checker = || { false };
        status!(@parse text_fn, checker, $($parts)*)
    }},

    ($($parts:tt)*) => {{
        use parsec_core::{
            data::RoData,
            input::InputMethod,
            text::{text, Tag, Builder},
            ui::PushSpecs,
            widgets::{File},
        };

		#[allow(unused_mut)]
        let (mut text_fn, checker) = status!(@parse $($parts)*);

        let text_fn = move |file: &RoData<File<Ui>>, input: &RoData<dyn InputMethod<Ui>>| {
            let mut builder = Builder::new();
            text!(builder, { Tag::StartAlignRight });
            text_fn(&mut builder, file, input);
            text!(builder, "\n");
            builder.finish()
        };

        StatusLineCfg::new_with(
            Box::new(text_fn),
            Box::new(checker),
            false,
            PushSpecs::below().with_lenght(1.0)
        )
    }}
}

type TextFn = Box<dyn FnMut(&RoData<File<Ui>>, &RoData<dyn InputMethod<Ui>>) -> Text>;
