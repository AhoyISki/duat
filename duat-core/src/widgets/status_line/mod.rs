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

use std::sync::LazyLock;

use common::{main_col, main_line, selections_fmt};

pub use self::state::State;
use crate::{
    data::{Context, FileReader},
    palette::{self, Form},
    text::{text, Builder, PrintCfg, Tag, Text},
    ui::{PushSpecs, Ui},
    widgets::{File, PassiveWidget, Widget, WidgetCfg},
};

pub struct StatusLineCfg<U>
where
    U: Ui,
{
    text_fn: TextFn<U>,
    checker: Box<dyn Fn() -> bool>,
    specs: PushSpecs,
}

impl<U> StatusLineCfg<U>
where
    U: Ui,
{
    pub fn new() -> Self {
        status!(
            U,
            [File] { File::name } " " [Selections] selections_fmt " "
            [Coords] main_col [Separator] ":" [Coords] main_line
            [Separator] "/" [Coords] { File::len_lines }
        )
    }

    pub fn new_with(text_fn: TextFn<U>, checker: Box<dyn Fn() -> bool>, specs: PushSpecs) -> Self {
        Self {
            text_fn,
            checker,
            specs,
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_ver_length(1.0),
            ..self
        }
    }
}

impl<U> WidgetCfg<U> for StatusLineCfg<U>
where
    U: Ui,
{
    type Widget = StatusLine<U>;

    fn build(
        self,
        context: Context<U>,
        on_file: bool,
    ) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let (reader, checker) = {
            let reader = match on_file {
                true => context.fixed_reader().unwrap(),
                false => context.dyn_reader().unwrap(),
            };
            let checker = {
                let reader = reader.clone();
                move || reader.has_changed() || (self.checker)()
            };
            (reader, Box::new(checker) as Box<dyn Fn() -> bool>)
        };

        let widget = Widget::passive(StatusLine {
            reader,
            text_fn: self.text_fn,
            text: Text::default(),
        });

        (widget, checker, self.specs)
    }
}

impl<U> Default for StatusLineCfg<U>
where
    U: Ui,
{
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
pub struct StatusLine<U>
where
    U: Ui,
{
    reader: FileReader<U>,
    text_fn: TextFn<U>,
    text: Text,
}

impl<U> StatusLine<U>
where
    U: Ui,
{
    pub fn config() -> StatusLineCfg<U> {
        StatusLineCfg::new()
    }
}

impl<U> PassiveWidget<U> for StatusLine<U>
where
    U: Ui,
{
    fn build(context: Context<U>, on_file: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        Self::config().build(context, on_file)
    }

    fn update(&mut self, _area: &U::Area) {
        self.text = (self.text_fn)(&self.reader);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn once(_context: Context<U>) {
        palette::set_weak_form("File", Form::new().yellow().italic());
        palette::set_weak_form("Selections", Form::new().dark_blue());
        palette::set_weak_form("Coord", Form::new().dark_red());
        palette::set_weak_form("Separator", Form::new().cyan());
    }

    fn print_cfg(&self) -> &PrintCfg {
        static CFG: LazyLock<PrintCfg> = LazyLock::new(|| PrintCfg::new().width_wrapped());
        &CFG
    }
}

unsafe impl<U> Send for StatusLine<U> where U: Ui {}
unsafe impl<U> Sync for StatusLine<U> where U: Ui {}

pub macro status {
    (@append $ui:ty, $text_fn:expr, $checker:expr, []) => {{
        let form_id = palette::__weakest_id_of_name("Default");

        let text_fn = move |builder: &mut Builder, reader: &FileReader<$ui>| {
            $text_fn(builder, reader);
            builder.push_tag(Tag::PushForm(form_id));
        };

        (text_fn, $checker)
    }},

    // Insertion of directly named forms.
    (@append $ui:ty, $text_fn:expr, $checker:expr, [$form:ident]) => {{
        let form_id = palette::__weakest_id_of_name(stringify!($form));

        let text_fn = move |builder: &mut Builder, reader: &FileReader<$ui>| {
            $text_fn(builder, reader);
            builder.push_tag(Tag::PushForm(form_id));
        };

        (text_fn, $checker)
    }},

	// Insertion of text, reading functions, or tags.
    (@append $ui:ty, $text_fn:expr, $checker:expr, $text:expr) => {{
        let (mut appender, checker) = State::from($text).fns();

        let checker = move || { $checker() || checker() };

        let text_fn = move |builder: &mut Builder, reader: &FileReader<$ui>| {
            $text_fn(builder, reader);
            appender(builder, reader);
        };

        (text_fn, checker)
    }},

    (@parse $ui:ty, $text_fn:expr, $checker:expr,) => { ($text_fn, $checker) },

    (@parse $ui:ty, $text_fn:expr, $checker:expr, $part:tt $($parts:tt)*) => {{
        let (mut text_fn, checker) = status!(@append $ui, $text_fn, $checker, $part);
        status!(@parse $ui, text_fn, checker, $($parts)*)
    }},

    (@parse $ui:ty, $($parts:tt)*) => {{
        let text_fn = |_: &mut Builder, _: &FileReader<$ui>| {};
        let checker = || { false };
        status!(@parse $ui, text_fn, checker, $($parts)*)
    }},

    ($ui:ty, $($parts:tt)*) => {{
		#[allow(unused_mut)]
        let (mut text_fn, checker) = status!(@parse $ui, $($parts)*);

        let text_fn = move |reader: &FileReader<$ui>| {
            let mut builder = Builder::new();
            text!(builder, { "" });
            text_fn(&mut builder, reader);
            text!(builder, "\n");
            builder.finish()
        };

        StatusLineCfg::new_with(
            Box::new(text_fn),
            Box::new(checker),
            PushSpecs::below().with_ver_length(10.0)
        )
    }}
}

type TextFn<U> = Box<dyn FnMut(&FileReader<U>) -> Text>;
