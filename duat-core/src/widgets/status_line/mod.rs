//! A widget that shows general information, usually about a [`File`]
//!
//! The [`StatusLine`] is a very convenient widget when the user
//! simply wants to show some informatioon. The information, when
//! relevant, can automatically be tied to the active file, saving
//! some keystrokes for the user's configuration.
//!
//! There is also the [`status!`] macro, which is an extremely
//! convenient way to modify the text of the status line, letting you
//! place form, in the same way that [`text!`] does, and
//! automatically recognizing a ton of different types of functions,
//! that can read from the file, from other places, from [data] types,
//! etc.
//!
//! [data]: crate::data
mod state;

pub use self::state::State;
use crate::{
    cfg::PrintCfg,
    context::{self, DynamicFile, FixedFile},
    form::{self, Form},
    status::{file_fmt, main_fmt, mode_fmt, mode_name, selections_fmt},
    text::{AlignRight, Builder, Spacer, Text, text},
    ui::{PushSpecs, Side, Ui},
    widgets::{Widget, WidgetCfg},
};

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
/// #     hooks::{self, OnFileOpen, OnWindowOpen}, ui::Ui, status::*,
/// #     widgets::{PromptLine, File, LineNumbers, Widget, StatusLine, Notifier, status},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder| {
///     builder.push(LineNumbers::cfg());
///     builder.push(status!(file_fmt));
/// });
///
/// hooks::remove("WindowWidgets");
/// hooks::add::<OnWindowOpen<U>>(|builder| {
///     let (child, _) = builder.push(PromptLine::cfg());
///     let status = status!(mode_fmt " " selections_fmt " " main_fmt);
///     builder.push_to(child.clone(), status.right_ratioed(2, 3));
///     builder.push_to(child, Notifier::cfg());
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
/// [`File`]: super::File
/// [`OnFileOpen`]: crate::hooks::OnFileOpen
/// [`OnWindowOpen`]: crate::hooks::OnWindowOpen
pub struct StatusLine<U: Ui> {
    reader: Reader<U>,
    text_fn: TextFn<U>,
    text: Text,
}

impl<U: Ui> Widget<U> for StatusLine<U> {
    type Cfg = StatusLineCfg<U>;

    fn cfg() -> Self::Cfg {
        status!(file_fmt " " mode_fmt " " selections_fmt " " main_fmt)
    }

    fn update(&mut self, _area: &U::Area) {
        self.text = (self.text_fn)(&mut self.reader);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("File", Form::yellow().italic());
        form::set_weak("Selections", Form::dark_blue());
        form::set_weak("Coord", Form::dark_yellow());
        form::set_weak("Separator", Form::cyan());
        form::set_weak("Mode", Form::green());
        Ok(())
    }
}

#[doc(hidden)]
pub struct StatusLineCfg<U: Ui> {
    pre_fn: Option<Box<dyn FnMut(crate::text::Builder, &mut Reader<U>) -> Text + Send>>,
    checker: Option<Box<dyn Fn() -> bool + Send + Sync>>,
    specs: PushSpecs,
}

impl<U: Ui> StatusLineCfg<U> {
    #[doc(hidden)]
    pub fn new_with(
        (pre_fn, checker): (
            Box<dyn FnMut(Builder, &mut Reader<U>) -> Text + 'static + Send>,
            Box<dyn Fn() -> bool + 'static + Send + Sync>,
        ),
        specs: PushSpecs,
    ) -> Self {
        Self {
            pre_fn: Some(pre_fn),
            checker: Some(checker),
            specs,
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_ver_len(1.0),
            ..self
        }
    }

    pub fn right_ratioed(self, den: u16, div: u16) -> Self {
        Self {
            specs: self.specs.to_right().with_hor_ratio(den, div),
            ..self
        }
    }
}

impl<U: Ui> WidgetCfg<U> for StatusLineCfg<U> {
    type Widget = StatusLine<U>;

    fn build(self, on_file: bool) -> (Self::Widget, impl Fn() -> bool, PushSpecs) {
        let (reader, checker) = {
            let reader = match on_file {
                true => Reader::Fixed(context::fixed_file().unwrap()),
                false => Reader::Dynamic(context::dyn_file().unwrap()),
            };
            let file_checker = reader.checker();
            let checker = match self.checker {
                Some(checker) => checker,
                // mode checker because mode_name is used in the default
                None => Box::new(crate::status::mode_name().checker()),
            };
            (reader, move || file_checker() || checker())
        };

        let text_fn: TextFn<U> = match self.pre_fn {
            Some(mut pre_fn) => Box::new(move |reader| pre_fn(Text::builder(), reader)),
            None => {
                let cfg = match self.specs.side() {
                    Side::Above | Side::Below => {
                        let mode_upper_fmt = mode_name().map(|mode| {
                            let mode = match mode.split_once('<') {
                                Some((mode, _)) => mode,
                                None => mode,
                            };
                            text!([Mode] { mode.to_uppercase() })
                        });
                        status!(mode_upper_fmt Spacer file_fmt " " selections_fmt " " main_fmt)
                    }
                    Side::Right => {
                        status!(AlignRight file_fmt " " mode_fmt " " selections_fmt " " main_fmt)
                    }
                    Side::Left => unreachable!(),
                };

                let mut pre_fn = cfg.pre_fn.unwrap();
                Box::new(move |reader| pre_fn(Text::builder(), reader))
            }
        };

        let widget = StatusLine { reader, text_fn, text: Text::default() };
        (widget, checker, self.specs)
    }
}

pub enum Reader<U: Ui> {
    Fixed(FixedFile<U>),
    Dynamic(DynamicFile<U>),
}

impl<U: Ui> Reader<U> {
    fn checker(&self) -> Box<dyn Fn() -> bool + Send + Sync + 'static> {
        match self {
            Reader::Fixed(ff) => Box::new(ff.checker()),
            Reader::Dynamic(df) => Box::new(df.checker()),
        }
    }

    fn inspect_related<W: 'static, R: 'static>(
        &mut self,
        f: impl FnOnce(&W, &U::Area) -> R,
    ) -> Option<R> {
        match self {
            Reader::Fixed(ff) => ff.inspect_related(f),
            Reader::Dynamic(df) => df.inspect_related(f),
        }
    }
}

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
/// #     mode::Cursors, text::{Text, text}, ui::Area, widgets::{File, status},
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
/// fn powerline_main_fmt(file: &File, area: &impl Area) -> Text {
///    let cursors = file.cursors();
///    let cfg = file.print_cfg();
///    let v_caret= cursors.get_main().unwrap().v_caret(file.text(), area, cfg);
///
///    text!(
///        [Separator] "" [Coord] { v_caret.visual_col() }
///        [Separator] "" [Coord] { v_caret.line() }
///        [Separator] "" [Coord] { file.len_lines() }
///    )
/// }
///
/// # fn test<Ui: duat_core::ui::Ui>() {
/// hooks::add::<OnWindowOpen<Ui>>(|builder| {
///     builder.push(status!([File] name_but_funky [] " " powerline_main_fmt));
/// });
/// # }
/// ```
///
/// Now, there are other types of arguments that you can also pass.
/// They update differently from the previous ones. The previous
/// arguments update when the [`File`] updates. The following types of
/// arguments update independently or not at all:
///
/// - A [`Text`] argument can include [`Form`]s and buttons;
/// - Any [`impl Display`], such as numbers, strings, chars, etc;
/// - [`RwData`] or [`DataMap`]s of the previous two types. These will
///   update whenever the data inside is changed;
/// - An [`(FnMut() -> Text | impl Display, FnMut() -> bool)`] tuple.
///   The first function returns what will be shown, while the second
///   function tells it to update;
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
/// // both the `changing_text` and `counter`.
/// *changing_text.write() = text!( "New text 😎");
/// # }
/// ```
///
/// [`File`]: super::File
/// [`&File`]: super::File
/// [`&Cursors`]: crate::mode::Cursors
/// [`&impl Widget`]: Widget
/// [`impl Display`]: std::fmt::Display
/// [`RwData`]: crate::data::RwData
/// [`DataMap`]: crate::data::DataMap
/// [`FnMut() -> Arg`]: FnMut
/// [`(FnMut() -> Text | impl Display, FnMut() -> bool)`]: FnMut
pub macro status {
    (@append $pre_fn:expr, $checker:expr, []) => {{
        let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
            $pre_fn(builder, reader);
            builder.push(form::DEFAULT_ID);
        };

        (pre_fn, $checker)
    }},
    // Insertion of directly named form.
    (@append $pre_fn:expr, $checker:expr, [$form:ident $(.$suffix:ident)*]) => {{
        let id = crate::form::id_of!(concat!(
            stringify!($form) $(, stringify!(.), stringify!($suffix))*
        ));

        let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
            $pre_fn(builder, reader);
            builder.push(id);
        };

        (pre_fn, $checker)
    }},
    (@push $builder:expr, [$($other:tt)+]) => {
        compile_error!(concat!(
            "Forms should be a list of identifiers separated by '.'s, received \"",
             stringify!($($other)+),
             "\" instead"
        ))
    },

    // Insertion of text, reading functions, or tags.
    (@append $pre_fn:expr, $checker:expr, $text:expr) => {{
        let (mut appender, checker) = State::from($text).fns();

        let checker = move || { $checker() || checker() };

        let pre_fn = move |builder: &mut Builder, reader: &mut Reader<_>| {
            $pre_fn(builder, reader);
            appender(builder, reader);
        };

        (pre_fn, checker)
    }},

    (@parse $pre_fn:expr, $checker:expr,) => {{
        (
            Box::new(move |mut builder: Builder, reader: &mut Reader<_>| {
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
        let pre_fn = |_: &mut Builder, _: &mut Reader<_>| {};
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

type TextFn<U> = Box<dyn FnMut(&mut Reader<U>) -> Text + Send>;
