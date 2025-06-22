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

use std::{cell::RefCell, rc::Rc};

use duat_core::{prelude::*, text::Builder, ui::Side};

pub use self::{macros::status, state::State};
use crate::state::{file_fmt, main_fmt, mode_fmt, mode_name, sels_fmt};

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
/// use duat_core::{
///     hook::{OnFileOpen, OnWindowOpen},
///     prelude::*,
/// };
/// use duat_utils::{state::*, widgets::*};
///
/// fn setup_generic_over_ui<U: Ui>() {
///     hook::remove("FileWidgets");
///     hook::add::<OnFileOpen<U>, U>(|pa, builder| {
///         builder.push(pa, LineNumbers::cfg());
///         builder.push(pa, status!("{file_fmt}").above());
///     });
///
///     hook::remove("WindowWidgets");
///     hook::add::<OnWindowOpen<U>, U>(|pa, builder| {
///         let footer = FooterWidgets::new(status!("{mode_fmt} {sels_fmt} {main_fmt}"));
///         builder.push(pa, footer);
///     });
/// }
/// ```
///
/// In the above example, each file would have a status line with the
/// name of the file, and by pushing [`FooterWidgets`], you will push
/// a [`StatusLine`], [`PromptLine`] and [`Notifications`] combo to
/// each window. This [`StatusLine`] will point to the currently
/// active [`File`], instead of a specific one.
///
/// You will usually want to create [`StatusLine`]s via the
/// [`status!`] macro, since that is how you can customize it.
/// Although, if you want the regular status line, you can just:
///
/// ```rust
/// use duat_core::{hook::OnFileOpen, prelude::*};
/// use duat_utils::widgets::*;
///
/// fn setup_generic_over_ui<U: Ui>() {
///     hook::remove("FileWidgets");
///     hook::add::<OnFileOpen<U>, U>(|pa, builder| {
///         builder.push(pa, LineNumbers::cfg());
///         builder.push(pa, StatusLine::cfg().above());
///     });
/// }
/// ```
///
/// [`File`]: duat_core::file::File
/// [`OnFileOpen`]: duat_core::hook::OnFileOpen
/// [`OnWindowOpen`]: duat_core::hook::OnWindowOpen
/// [`PromptLine`]: super::PromptLine
/// [`Notifications`]: super::Notifications
/// [`FooterWidgets`]: super::FooterWidgets
pub struct StatusLine<U: Ui> {
    handle: FileHandle<U>,
    text_fn: TextFn<U>,
    text: Text,
    checker: Box<dyn Fn() -> bool>,
}

impl<U: Ui> Widget<U> for StatusLine<U> {
    type Cfg = StatusLineCfg<U>;

    fn update(pa: &mut Pass, handle: Handle<Self, U>) {
        let text = handle.read(pa, |wid, _| wid.text_fn.borrow_mut()(pa, &wid.handle));
        handle.widget().replace_text(pa, text);
    }

    fn needs_update(&self) -> bool {
        self.handle.has_changed() || (self.checker)()
    }

    fn cfg() -> Self::Cfg {
        macros::status!("{file_fmt} {mode_fmt} {sels_fmt} {}", main_fmt)
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("file", Form::yellow().italic());
        form::set_weak("selections", Form::dark_blue());
        form::set_weak("coord", Form::dark_yellow());
        form::set_weak("separator", Form::cyan());
        form::set_weak("mode", Form::green());
        Ok(())
    }
}

/// The [`WidgetCfg`] for a [`StatusLine`]
pub struct StatusLineCfg<U: Ui> {
    builder: Option<BuilderFn<U>>,
    checker: Option<Box<dyn Fn() -> bool>>,
    specs: PushSpecs,
}

impl<U: Ui> StatusLineCfg<U> {
    #[doc(hidden)]
    pub fn new_with(
        (builder, checker): (BuilderFn<U>, Box<dyn Fn() -> bool>),
        specs: PushSpecs,
    ) -> Self {
        Self {
            builder: Some(builder),
            checker: Some(checker),
            specs,
        }
    }

    /// Replaces the previous formatting with a new one
    pub fn replace(self, new: Self) -> Self {
        Self { specs: self.specs, ..new }
    }

    /// Puts the [`StatusLine`] above, as opposed to below
    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_ver_len(1.0),
            ..self
        }
    }

    /// Puts the [`StatusLine`] below, this is the default
    pub fn below(self) -> Self {
        Self {
            specs: PushSpecs::below().with_ver_len(1.0),
            ..self
        }
    }

    /// Puts the [`StatusLine`] on the right, instead of below
    ///
    /// use this if you want a single line [`StatusLine`],
    /// [`PromptLine`]/[`Notifications`] combo.
    ///
    /// [`PromptLine`]: super::PromptLine
    /// [`Notifications`]: super::Notifications
    pub fn right_ratioed(self, den: u16, div: u16) -> Self {
        Self {
            specs: self.specs.to_right().with_hor_ratio(den, div),
            ..self
        }
    }

    /// The [`PushSpecs`] in use
    pub fn specs(&self) -> PushSpecs {
        self.specs
    }
}

impl<U: Ui> WidgetCfg<U> for StatusLineCfg<U> {
    type Widget = StatusLine<U>;

    fn build(self, pa: &mut Pass, handle: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let handle = match handle {
            Some(handle) => handle,
            None => context::dyn_file(pa).unwrap(),
        };

        let checker = match self.checker {
            Some(checker) => checker,
            // mode checker because mode_name is used in the default
            None => Box::new(crate::state::mode_name().checker()),
        };

        let text_fn: TextFn<U> = match self.builder {
            Some(mut text_fn) => Rc::new(RefCell::new(
                for<'a, 'b> move |pa: &'a Pass, handle: &'b FileHandle<U>| -> Text {
                    text_fn(pa, Text::builder(), handle)
                },
            )),
            None => {
                let cfg = match self.specs.side() {
                    Side::Above | Side::Below => {
                        let mode_upper_fmt = mode_name().map(|mode| {
                            let mode = match mode.split_once('<') {
                                Some((mode, _)) => mode,
                                None => mode,
                            };
                            txt!("[mode]{}", mode.to_uppercase()).build()
                        });
                        macros::status!("{mode_upper_fmt}{Spacer}{file_fmt} {sels_fmt} {main_fmt}")
                    }
                    Side::Right => {
                        macros::status!("{AlignRight}{file_fmt} {mode_fmt} {sels_fmt} {main_fmt}")
                    }
                    Side::Left => unreachable!(),
                };

                let mut text_fn = cfg.builder.unwrap();
                Rc::new(RefCell::new(
                    for<'a, 'b> move |pa: &'a Pass, handle: &'b FileHandle<U>| -> Text {
                        text_fn(pa, Text::builder(), handle)
                    },
                ))
            }
        };

        let widget = StatusLine {
            handle,
            text_fn,
            text: Text::default(),
            checker: Box::new(checker),
        };
        (widget, self.specs)
    }
}

impl<U: Ui> Default for StatusLineCfg<U> {
    fn default() -> Self {
        StatusLine::cfg()
    }
}

mod macros {
    /// The macro that creates a [`StatusLine`]
    ///
    /// This macro works like the [`txt!`] macro, in  that [`Form`]s
    /// are pushed with `[{FormName}]`. However, [`txt!`]  is
    /// evaluated immediately, while [`status!`] is evaluated when
    /// updates occur.
    ///
    /// The macro will mostly read from the [`File`] widget and its
    /// related structs. In order to do that, it will accept functions
    /// as arguments. These functions take the following
    /// parameters:
    ///
    /// * The [`&File`] widget;
    /// * The [`&Selections`] of the [`File`]
    /// * A specific [`&impl Widget`], which is glued to the [`File`];
    ///
    /// Here's some examples:
    ///
    /// ```rust
    /// use duat_core::{hook::OnWindowOpen, prelude::*};
    /// use duat_utils::widgets::status;
    ///
    /// fn name_but_funky<U: Ui>(file: &File<U>) -> String {
    ///     let mut name = String::new();
    ///
    ///     for byte in unsafe { name.as_bytes_mut().iter_mut().step_by(2) } {
    ///         *byte = byte.to_ascii_uppercase();
    ///     }
    ///
    ///     name
    /// }
    ///
    /// fn powerline_main_fmt<U: Ui>(file: &File<U>, area: &U::Area) -> Text {
    ///     let selections = file.selections();
    ///     let cfg = file.print_cfg();
    ///     let v_caret = selections
    ///         .get_main()
    ///         .unwrap()
    ///         .v_caret(file.text(), area, cfg);
    ///
    ///     txt!(
    ///         "[separator][coord]{}[separator][coord]{}[separator][coord]{}",
    ///         v_caret.visual_col(),
    ///         v_caret.line(),
    ///         file.len_lines()
    ///     )
    ///     .build()
    /// }
    ///
    /// fn setup_generic_over_ui<U: Ui>() {
    ///     hook::add::<OnWindowOpen<U>, U>(|pa, builder| {
    ///         builder.push(pa, status!("[file]{name_but_funky}[] {powerline_main_fmt}"));
    ///     });
    /// }
    /// ```
    ///
    /// Now, there are other types of arguments that you can also
    /// pass. They update differently from the previous ones. The
    /// previous arguments update when the [`File`] updates. The
    /// following types of arguments update independently or not
    /// at all:
    ///
    /// - A [`Text`] argument can include [`Form`]s and buttons;
    /// - Any [`impl Display`], such as numbers, strings, chars, etc;
    /// - [`RwData`] or [`DataMap`]s of the previous two types. These
    ///   will update whenever the data inside is changed;
    /// - An [`(FnMut() -> Text | impl Display, FnMut() -> bool)`]
    ///   tuple. The first function returns what will be shown, while
    ///   the second function tells it to update;
    ///
    /// Here's an examples:
    ///
    /// ```rust
    /// use std::sync::atomic::{AtomicUsize, Ordering};
    ///
    /// use duat_core::{data::RwData, hook::OnWindowOpen, prelude::*};
    /// use duat_utils::widgets::status;
    ///
    /// # fn test<U: Ui>() {
    /// let changing_text = RwData::new(txt!("Prev text").build());
    ///
    /// fn counter(pa: &Pass) -> usize {
    ///     static COUNT: AtomicUsize = AtomicUsize::new(0);
    ///     COUNT.fetch_add(1, Ordering::Relaxed)
    /// }
    ///
    /// hook::add::<OnWindowOpen<U>, U>({
    ///     let changing_text = changing_text.clone();
    ///     move |pa, builder| {
    ///         let changing_text = changing_text.clone();
    ///         let checker = changing_text.checker();
    ///
    ///         let text = txt!("Static text").build();
    ///
    ///         builder.push(
    ///             pa,
    ///             status!("{changing_text} [counter]{}[] {text}", (counter, checker)),
    ///         );
    ///     }
    /// });
    /// # }
    /// ```
    ///
    /// In the above example, I added some dynamic [`Text`], through
    /// the usage of an [`RwData<Text>`], I added some static
    /// [`Text`], some [`Form`]s (`"counter"` and `"default"`) and
    /// even a counter, which will update whenever `changing_text`
    /// is altered.
    ///
    /// [`StatusLine`]: super::StatusLine
    /// [`txt!`]: duat_core::text::txt
    /// [`File`]: duat_core::file::File
    /// [`&File`]: duat_core::file::File
    /// [`&Selections`]: duat_core::mode::Selections
    /// [`&impl Widget`]: duat_core::ui::Widget
    /// [`impl Display`]: std::fmt::Display
    /// [`Text`]: duat_core::text::Text
    /// [`RwData`]: duat_core::data::RwData
    /// [`DataMap`]: duat_core::data::DataMap
    /// [`FnMut() -> Arg`]: FnMut
    /// [`(FnMut() -> Text | impl Display, FnMut() -> bool)`]: FnMut
    /// [`RwData<Text>`]: duat_core::data::RwData
    /// [`Form`]: duat_core::form::Form
    pub macro status($($parts:tt)*) {{
        #[allow(unused_imports)]
        use $crate::{
            private_exports::{
                duat_core::{context::FileHandle, data::Pass, text::Builder, ui::PushSpecs},
                format_like, parse_form, parse_status_part, parse_str
            },
            widgets::StatusLineCfg,
        };

        let text_fn= |_: &Pass, _: &mut Builder, _: &FileHandle<_>| {};
        let checker = || false;

        let (mut text_fn, checker) = format_like!(
            parse_str,
            [('{', parse_status_part, false), ('[', parse_form, true)],
            (text_fn, checker),
            $($parts)*
        );

        StatusLineCfg::new_with(
            {
                (
                    Box::new(move |pa: &Pass, mut builder: Builder, handle: &FileHandle<_>| {
                        text_fn(pa, &mut builder, &handle);
                        builder.build()
                    }),
                    Box::new(checker),
                )
            },
            PushSpecs::below().with_ver_len(1.0),
        )
    }}
}

type TextFn<U> = Rc<RefCell<dyn FnMut(&Pass, &FileHandle<U>) -> Text>>;
type BuilderFn<U> = Box<dyn FnMut(&Pass, Builder, &FileHandle<U>) -> Text>;
