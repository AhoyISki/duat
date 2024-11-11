//! A widget that shows general information, usually about a [`File`]
//!
//! The [`StatusLine`] is a very convenient widget when the user
//! simply wants to show some informatioon. The information, when
//! relevant, can automatically be tied to the active file, saving
//! some keystrokes for the user's configuration.
//!
//! There is also the [`status!`] macro, which is an extremely
//! convenient way to modify the text of the status line, letting you
//! place forms, in the same way that [`text!`] does, and
//! automatically recognizing a ton of different types of functions,
//! that can read from the file, from other places, from [data] types,
//! etc.
//!
//! [data]: crate::data
pub mod common;
mod state;

use std::fmt::Alignment;

use common::{main_col, main_line, selections_fmt};

pub use self::state::State;
use crate::{
    context::{self, FileReader},
    forms::{self, Form},
    text::{AlignCenter, AlignRight, Builder, PrintCfg, Tag, Text, text},
    ui::{PushSpecs, Ui},
    widgets::{File, Widget, WidgetCfg},
};

pub struct StatusLineCfg<U: Ui> {
    pre_fn: Box<dyn FnMut(crate::text::Builder, &FileReader<U>) -> Text>,
    checker: Box<dyn Fn() -> bool>,
    specs: PushSpecs,
    alignment: Alignment,
}

impl<U: Ui> StatusLineCfg<U> {
    pub fn new() -> Self {
        status!(
            [File] { File::name } " " [Selections] selections_fmt " "
            [Coords] main_col [Separator] ":" [Coords] main_line
            [Separator] "/" [Coords] { File::len_lines }
        )
    }

    pub fn new_with(
        (pre_fn, checker): (
            Box<dyn FnMut(Builder, &FileReader<U>) -> Text + 'static>,
            Box<dyn Fn() -> bool + 'static>,
        ),
        specs: PushSpecs,
    ) -> Self {
        Self {
            pre_fn,
            checker,
            specs,
            alignment: Alignment::Right,
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_ver_len(1.0),
            ..self
        }
    }

    pub fn align_left(self) -> Self {
        Self { alignment: Alignment::Left, ..self }
    }

    pub fn push_left(self) -> Self {
        Self {
            alignment: Alignment::Left,
            specs: self.specs.to_left(),
            ..self
        }
    }

    pub fn push_left_centered(self) -> Self {
        Self {
            alignment: Alignment::Center,
            specs: self.specs.to_left(),
            ..self
        }
    }
}

impl<U: Ui> WidgetCfg<U> for StatusLineCfg<U> {
    type Widget = StatusLine<U>;

    fn build(mut self, on_file: bool) -> (Self::Widget, impl Fn() -> bool, PushSpecs) {
        let (reader, checker) = {
            let reader = match on_file {
                true => context::fixed_reader().unwrap(),
                false => context::dyn_reader().unwrap(),
            };
            let checker = {
                let reader = reader.clone();
                move || reader.has_changed() || (self.checker)()
            };
            (reader, Box::new(checker) as Box<dyn Fn() -> bool>)
        };

        let text_fn: TextFn<U> = match self.alignment {
            Alignment::Left => Box::new(move |file| (self.pre_fn)(Text::builder(), file)),
            Alignment::Right => Box::new(move |file| {
                let mut builder = Text::builder();
                text!(builder, AlignRight);
                (self.pre_fn)(builder, file)
            }),
            Alignment::Center => Box::new(move |file| {
                let mut builder = Text::builder();
                text!(builder, AlignCenter);
                (self.pre_fn)(builder, file)
            }),
        };

        let widget = StatusLine { reader, text_fn, text: Text::default() };
        (widget, checker, self.specs)
    }
}

impl<U: Ui> Default for StatusLineCfg<U> {
    fn default() -> Self {
        Self::new()
    }
}

/// A widget to show information, usually about a [`File`]
///
/// This widget is updated whenever any of its parts needs to be
/// updated, and it also automatically adjusts to where it was pushed.
/// For example, if you push it with [`OnFileOpen`], it's information
/// will point to the [`File`] to which it was pushed. However, if you
/// push it with [`OnWindowOpen`], it will always point to the
/// currently active [`File`]:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen, OnWindowOpen}, ui::Ui,
/// #     widgets::{CmdLine, File, LineNumbers, Widget, StatusLine, status, common::*},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder| {
///     builder.push(LineNumbers::cfg());
///     builder.push(status!([File] { File::name }));
/// });
///
/// hooks::remove("WindowWidgets");
/// hooks::add::<OnWindowOpen<U>>(|builder| {
///     let (status_area, _) = builder.push(status!(
///         [File] { File::name } " " selections_fmt " " main_fmt
///     ));
///     builder.push_to(CmdLine::cfg().left_ratioed(2, 3), status_area);
/// });
/// # }
/// ```
///
/// In the above example, each file would have a status line with the
/// name of the file, and there would be a global status line, showing
/// more information about the currently active file.
///
/// You will usually want to create [`StatusLine`]s via the
/// [`status!`] macro, since that is how you can customize it.
/// Although, if you want the regular status line, you can just:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen}, ui::{Ui}, widgets::{LineNumbers, Widget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder| {
///     builder.push(LineNumbers::cfg());
///     builder.push(StatusLine::cfg());
/// });
/// # }
/// ```
///
/// [`OnFileOpen`]: crate::hooks::OnFileOpen
/// [`OnWindowOpen`]: crate::hooks::OnWindowOpen
pub struct StatusLine<U: Ui> {
    reader: FileReader<U>,
    text_fn: TextFn<U>,
    text: Text,
}

impl<U: Ui> Widget<U> for StatusLine<U> {
    type Cfg = StatusLineCfg<U>;

    fn cfg() -> Self::Cfg {
        StatusLineCfg::new()
    }

    fn update(&mut self, _area: &U::Area) {
        self.text = (self.text_fn)(&self.reader);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() {
        forms::set_weak("DefaultStatus", "Default");
        forms::set_weak("File", Form::yellow().italic());
        forms::set_weak("Selections", Form::dark_blue());
        forms::set_weak("Coord", Form::dark_red());
        forms::set_weak("Separator", Form::cyan());
        forms::set_weak("Mode", Form::green());
    }

    fn print_cfg(&self) -> PrintCfg {
        PrintCfg::new().width_wrapped()
    }
}

unsafe impl<U: Ui> Send for StatusLine<U> {}
unsafe impl<U: Ui> Sync for StatusLine<U> {}

/// The macro that creates a [`StatusLine`]
///
/// This macro works like the [`text!`] macro, in  that [`Form`]s are
/// pushed with `[{FormName}]`. However, [`text!`]  is evaluated
/// immediately, while [`status!`] is evaluated when  updates occur.
///
/// The macro will mostly read from the [`File`] widget and its
/// related structs. In order to do that, it will accept functions as
/// arguments. These functions take the following parameters:
///
/// * The [`&File`] widget;
/// * The [`&Cursors`] of the [`File`]
/// * A specific [`&impl Widget`], which will surrounds the [`File`];
///
/// Here's some examples:
///
/// ```rust
/// # use duat_core::{
/// #     mode::Cursors, text::{Text, text}, ui::Ui, widgets::{File, status},
/// #     hooks::{self, OnWindowOpen}
/// # };
/// fn name_but_funky(file: &File) -> String {
///     let mut name = String::new();
///     
///     for byte in unsafe { name.as_bytes_mut().iter_mut().step_by(2) } {
///         *byte = byte.to_ascii_uppercase();
///     }
///     
///     name
/// }
///
/// fn powerline_main_fmt(file: &File, cursors: &Cursors) -> Text {
///    let cursor = cursors.main();
///
///    text!(
///        [Separator] "" [Coord] { cursor.col() }
///        [Separator] "" [Coord] { cursor.line() }
///        [Separator] "" [Coord] { file.len_lines() }
///    )
/// }
///
/// # fn test<U: Ui>() {
/// hooks::add::<OnWindowOpen<U>>(|builder| {
///     builder.push(status!([File] name_but_funky [] " " powerline_main_fmt));
/// });
/// # }
/// ```
///
/// As you can see, you can also pass multiple of these arguments,
/// here's the pairings you can make:
///
/// * A [`File`] and the [`&dyn Mode`];
/// * A [`File`] and a [`&impl Mode`];
/// * A [`File`] and a [`&impl Widget`];
///
/// Now, there are other types of arguments that you can also pass.
/// They update differently from the previous ones. The previous
/// arguments update when the [`File`] updates. The following types of
/// arguments update independently or not at all:
///
/// * A [`Text`] argument can include [`Form`]s and buttons;
/// * Any [`impl Display`], such as numbers, strings, chars, etc;
/// * [`RwData`] and [`RoData`]s of the previous two types. These will
///   update whenever the data inside is changed;
/// * An [`(FnMut() -> Arg, FnMut() -> bool)`] tuple, where `Arg ==
///   Text || impl Display`. The first function returns what will be
///   shown, while the second function tells it to update;
///
/// Here's some examples:
///
/// ```rust
/// # use std::sync::atomic::{AtomicUsize, Ordering};
/// # use duat_core::{
/// #     data::RwData, mode::Mode, text::text, ui::Ui, widgets::{File, status},
/// #     hooks::{self, OnWindowOpen}
/// # };
/// # fn test<U: Ui>() {
/// let changing_text = RwData::new(text!("Prev text"));
///
/// fn counter() -> usize {
///     static COUNT: AtomicUsize = AtomicUsize::new(0);
///     COUNT.fetch_add(1, Ordering::Relaxed)
/// }
///
/// hooks::add::<OnWindowOpen<U>>({
///     let changing_text = changing_text.clone();
///     move |builder| {
///         let changing_text = changing_text.clone();
///         
///         let checker = {
///             let changing_text = changing_text.clone();
///             move || changing_text.has_changed()
///         };
///         
///         let text = text!("Static text");
///         
///         builder.push(status!(changing_text " " (counter, checker) " " text));
///     }
/// });
///
/// // When I do this, the StatusLine will instantly update
/// // Both the `changing_text` and `counter` will change.
/// *changing_text.write() = text!( "New text 😎");
/// # }
/// ```
///
/// [`&File`]: File
/// [`&Cursors`]: crate::mode::Cursors
/// [`&impl Widget`]: Widget
/// [`impl Display`]: std::fmt::Display
/// [`RwData`]: crate::data::RwData
/// [`RoData`]: crate::data::RoData
/// [`FnMut() -> Arg`]: FnMut
/// [`(FnMut() -> Arg, FnMut() -> bool)`]: FnMut
pub macro status {
    (@append $pre_fn:expr, $checker:expr, []) => {{
        let form_id = forms::id_of!("DefaultStatus");

        let pre_fn = move |builder: &mut Builder, reader: &FileReader<_>| {
            $pre_fn(builder, reader);
            builder.push(Tag::PushForm(form_id));
        };

        (pre_fn, $checker)
    }},

    // Insertion of directly named forms.
    (@append $pre_fn:expr, $checker:expr, [$form:ident]) => {{
        let id = forms::id_of!(stringify!($form));

        let pre_fn = move |builder: &mut Builder, reader: &FileReader<_>| {
            $pre_fn(builder, reader);
            builder.push(Tag::PushForm(id));
        };

        (pre_fn, $checker)
    }},

    // Insertion of text, reading functions, or tags.
    (@append $pre_fn:expr, $checker:expr, $text:expr) => {{
        let (mut appender, checker) = State::from($text).fns();

        let checker = move || { $checker() || checker() };

        let pre_fn = move |builder: &mut Builder, reader: &FileReader<_>| {
            $pre_fn(builder, reader);
            appender(builder, reader);
        };

        (pre_fn, checker)
    }},

    (@parse $pre_fn:expr, $checker:expr,) => {{
        (
            Box::new(move |mut builder: Builder, reader: &FileReader<_>| {
                $pre_fn(&mut builder, reader);
                builder.finish()
            }),
            Box::new($checker)
        )
    }},

    (@parse $pre_fn:expr, $checker:expr, $part:tt $($parts:tt)*) => {{
        #[allow(unused_mut)]
        let (mut pre_fn, checker) = status!(@append $pre_fn, $checker, $part);
        status!(@parse pre_fn, checker, $($parts)*)
    }},

    (@parse $($parts:tt)*) => {{
        let pre_fn = |_: &mut Builder, _: &FileReader<_>| {};
        let checker = || { false };
        status!(@parse pre_fn, checker, $($parts)*)
    }},

    ($($parts:tt)*) => {{
        StatusLineCfg::new_with(
            status!(@parse $($parts)*),
            PushSpecs::below().with_ver_len(1.0)
        )
    }}
}

type TextFn<U> = Box<dyn FnMut(&FileReader<U>) -> Text>;
