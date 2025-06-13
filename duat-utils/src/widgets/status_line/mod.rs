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

use duat_core::{
    context::{self, FileHandle},
    data::{Pass, RwData},
    form::{self, Form},
    text::{AlignRight, Builder, Spacer, Text, txt},
    ui::{PushSpecs, Side, Ui},
    widget::{Widget, WidgetCfg},
};

pub use self::{macros::status, state::State};
use crate::state::{file_fmt, main_fmt, mode_fmt, mode_name, selections_fmt};

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
    handle: FileHandle<U>,
    text_fn: TextFn<U>,
    text: Text,
    checker: Box<dyn Fn() -> bool>,
}

impl<U: Ui> Widget<U> for StatusLine<U> {
    type Cfg = StatusLineCfg<U>;

    fn update(mut pa: Pass, widget: RwData<Self>, _: &<U as Ui>::Area) {
        let text = widget.read(&pa, |wid| wid.text_fn.borrow_mut()(&pa, &wid.handle));
        widget.replace_text(&mut pa, text);
    }

    fn needs_update(&self) -> bool {
        self.handle.has_changed() || (self.checker)()
    }

    fn cfg() -> Self::Cfg {
        macros::status!("{file_fmt} {mode_fmt} {selections_fmt} {}", main_fmt)
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

    fn build(self, pa: Pass, handle: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let handle = match handle {
            Some(handle) => handle,
            None => context::dyn_file(&pa).unwrap(),
        };

        let checker = match self.checker {
            Some(checker) => checker,
            // mode checker because mode_name is used in the default
            None => Box::new(crate::state::mode_name().checker()),
        };

        let text_fn: TextFn<U> = match self.builder {
            Some(mut text_fn) => Rc::new(RefCell::new(
                for<'a, 'b, 'c> move |pa: &'b Pass<'a>, handle: &'c FileHandle<U>| -> Text {
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
                            txt!("[Mode]{}", mode.to_uppercase()).build()
                        });
                        macros::status!(
                            "{mode_upper_fmt}{Spacer}{file_fmt} {selections_fmt} {main_fmt}"
                        )
                    }
                    Side::Right => {
                        macros::status!(
                            "{AlignRight}{file_fmt} {mode_fmt} {selections_fmt} {main_fmt}"
                        )
                    }
                    Side::Left => unreachable!(),
                };

                let mut text_fn = cfg.builder.unwrap();
                Rc::new(RefCell::new(
                    for<'a, 'b, 'c> move |pa: &'b Pass<'a>, handle: &'c FileHandle<U>| -> Text {
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

mod macros {
    /// The macro that creates a [`StatusLine`]
    ///
    /// This macro works like the [`text!`] macro, in  that [`Form`]s
    /// are pushed with `[{FormName}]`. However, [`text!`]  is
    /// evaluated immediately, while [`status!`] is evaluated when
    /// updates occur.
    ///
    /// The macro will mostly read from the [`File`] widget and its
    /// related structs. In order to do that, it will accept functions
    /// as arguments. These functions take the following
    /// parameters:
    ///
    /// * The [`&File`] widget;
    /// * The [`&Cursors`] of the [`File`]
    /// * A specific [`&impl Widget`], which will surrounds the
    ///   [`File`];
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
    pub macro status($($parts:tt)*) {{
        #[allow(unused_imports)]
        use $crate::{
            private_exports::{
                duat_core::{context::FileHandle, data::Pass, text::Builder, ui::PushSpecs},
                format_like, parse_form, parse_status_part, parse_str
            },
            widgets::StatusLineCfg,
        };

        let text_fn= |_: &Pass<'_>, _: &mut Builder, _: &FileHandle<_>| {};
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
                    Box::new(move |pa: &Pass<'_>, mut builder: Builder, handle: &FileHandle<_>| {
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

type TextFn<U> = Rc<RefCell<dyn FnMut(&Pass<'_>, &FileHandle<U>) -> Text>>;
type BuilderFn<U> = Box<dyn FnMut(&Pass<'_>, Builder, &FileHandle<U>) -> Text>;
