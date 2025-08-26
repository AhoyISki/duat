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

use duat_core::{context::DynFile, prelude::*, text::Builder, ui::Side};

pub use self::{macros::status, state::State};
use crate::state::{main_txt, mode_txt, name_txt, sels_txt};

/// A widget to show information, usually about a [`File`]
///
/// This widget is updated whenever any of its parts needs to be
/// updated, and it also automatically adjusts to where it was pushed.
/// For example, if you push it to a file (via `hook::add::<File>`,
/// for example), it's information will point to the [`File`] to which
/// it was pushed. However, if you push it with [`WindowCreated`], it
/// will always point to the currently active [`File`]:
///
/// ```rust
/// # use duat_core::doc_duat as duat;
/// # use duat_utils::widgets::{FooterWidgets, status};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<File>(|_, (cfg, builder)| {
///         builder.push(status!("{name_txt}").above());
///         cfg
///     });
///
///     hook::remove("WindowWidgets");
///     hook::add::<WindowCreated>(|pa, builder| {
///         builder.push(FooterWidgets::new(status!(
///             "{} {sels_txt} {main_txt}",
///             mode_txt(pa)
///         )));
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
/// # use duat_core::doc_duat as duat;
/// # use duat_utils::widgets::{LineNumbers, StatusLine};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("FileWidgets");
///     hook::add::<File>(|_, (cfg, builder)| {
///         builder.push(LineNumbers::cfg());
///         builder.push(StatusLine::cfg().above());
///         cfg
///     });
/// }
/// ```
///
/// [`File`]: duat_core::file::File
/// [`WindowCreated`]: duat_core::hook::WindowCreated
/// [`PromptLine`]: super::PromptLine
/// [`Notifications`]: super::Notifications
/// [`FooterWidgets`]: super::FooterWidgets
pub struct StatusLine<U: Ui> {
    file_handle: FileHandle<U>,
    text_fn: TextFn<U>,
    text: Text,
    checker: Box<dyn Fn(&Pass) -> bool + Send>,
}

impl<U: Ui> Widget<U> for StatusLine<U> {
    type Cfg = StatusLineCfg<U>;

    fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
        if let FileHandle::Dynamic(dyn_file) = &mut handle.write(pa).file_handle {
            dyn_file.swap_to_current();
        }

        let sl = handle.read(pa);

        handle.write(pa).text = match &sl.file_handle {
            FileHandle::Fixed(file) => (sl.text_fn)(pa, file),
            FileHandle::Dynamic(dyn_file) => (sl.text_fn)(pa, dyn_file.handle()),
        };
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        let file_changed = match &self.file_handle {
            FileHandle::Fixed(handle) => handle.has_changed(),
            FileHandle::Dynamic(dyn_file) => dyn_file.has_changed(pa),
        };

        file_changed || (self.checker)(pa)
    }

    fn cfg() -> Self::Cfg {
        StatusLineCfg {
            fns: None,
            specs: PushSpecs::below().ver_len(1.0),
        }
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
    fns: Option<(BuilderFn<U>, CheckerFn)>,
    specs: PushSpecs,
}

impl<U: Ui> StatusLineCfg<U> {
    #[doc(hidden)]
    pub fn new_with(fns: (BuilderFn<U>, CheckerFn), specs: PushSpecs) -> Self {
        Self { fns: Some(fns), specs }
    }

    /// Replaces the previous formatting with a new one
    pub fn fmt(self, new: Self) -> Self {
        Self { specs: self.specs, ..new }
    }

    /// Puts the [`StatusLine`] above, as opposed to below
    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().ver_len(1.0),
            ..self
        }
    }

    /// Puts the [`StatusLine`] below, this is the default
    pub fn below(self) -> Self {
        Self {
            specs: PushSpecs::below().ver_len(1.0),
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
            specs: self.specs.to_right().hor_ratio(den, div),
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

    fn build(self, pa: &mut Pass, info: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
        let (builder_fn, checker_fn) = if let Some((builder, checker)) = self.fns {
            (builder, checker)
        } else {
            let mode_txt = mode_txt(pa);
            let cfg = match self.specs.side() {
                Side::Above | Side::Below => {
                    macros::status!("{mode_txt}{Spacer}{name_txt} {sels_txt} {main_txt}")
                }
                Side::Right => {
                    macros::status!("{AlignRight}{name_txt} {mode_txt} {sels_txt} {main_txt}",)
                }
                Side::Left => unreachable!(),
            };

            cfg.fns.unwrap()
        };

        let widget = StatusLine {
            file_handle: match info.file() {
                Some(handle) => FileHandle::Fixed(handle),
                None => FileHandle::Dynamic(context::dyn_file(pa).unwrap()),
            },
            text_fn: Box::new(move |pa, fh| {
                let builder = Text::builder();
                builder_fn(pa, builder, fh)
            }),
            text: Text::default(),
            checker: Box::new(checker_fn),
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
    /// as arguments. These functions take the following parameters:
    ///
    /// * The [`&File`] widget;
    /// * A specific [`&impl Widget`], which is glued to the [`File`];
    ///
    /// Both of these can also have a second argument of type
    /// [`&Area`]. This will include the [`Widget`]'s [`Area`] when
    /// creating the status part. Additionally, you may include a
    /// first argument of type [`&Pass`] (e.g. `fn(&Pass, &File)`,
    /// `fn(&Pass, &Widget, &Area), etc.), giving you _non mutating_
    /// access to global state.
    ///
    /// Here's some examples:
    ///
    /// ```rust
    /// # use duat_core::doc_duat as duat;
    /// # use duat_utils::widgets::status;
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn name_but_funky(file: &File) -> String {
    ///     file.name()
    ///         .chars()
    ///         .enumerate()
    ///         .map(|(i, char)| {
    ///             if i % 2 == 1 {
    ///                 char.to_uppercase().to_string()
    ///             } else {
    ///                 char.to_lowercase().to_string()
    ///             }
    ///         })
    ///         .collect()
    /// }
    ///
    /// fn powerline_main_txt(file: &File, area: &Area) -> Text {
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
    /// fn setup() {
    ///     hook::add::<WindowCreated>(|_, builder| {
    ///         builder.push(status!("[file]{name_but_funky}[] {powerline_main_txt}"));
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
    /// - A [`Text`] argument, which can be formatted in a similar way
    ///   throught the [`txt!`] macro;
    /// - Any [`impl Display`], such as numbers, strings, chars, etc.
    ///   [`impl Debug`] types also work, when including the usual
    ///   `":?"` and derived suffixes;
    /// - [`RwData`] or [`DataMap`]s of the previous two types. These
    ///   will update whenever the data inside is changed;
    /// - An [`(Fn(&Pass) -> Text/Display/Debug, Fn(&Pass) -> bool)`]
    ///   tuple. The first function returns what will be shown, while
    ///   the second function checks for updates, which will call the
    ///   first function again;
    ///
    /// Here's an examples:
    ///
    /// ```rust
    /// # use duat_core::doc_duat as duat;
    /// # use duat_utils::widgets::status;
    /// setup_duat!(setup);
    /// use std::sync::atomic::{AtomicUsize, Ordering};
    ///
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     let changing_str = RwData::new("Initial text".to_string());
    ///
    ///     fn counter(update: bool) -> usize {
    ///         static COUNT: AtomicUsize = AtomicUsize::new(0);
    ///         if update {
    ///             COUNT.fetch_add(1, Ordering::Relaxed) + 1
    ///         } else {
    ///             COUNT.load(Ordering::Relaxed)
    ///         }
    ///     }
    ///
    ///     hook::add::<WindowCreated>({
    ///         let changing_str = changing_str.clone();
    ///         move |_, builder| {
    ///             let changing_str = changing_str.clone();
    ///             let checker = changing_str.checker();
    ///
    ///             let text = txt!("Static text").build();
    ///
    ///             let counter = move |_: &File| counter(checker());
    ///
    ///             builder.push(status!("{changing_str} [counter]{counter}[] {text}",));
    ///         }
    ///     });
    ///
    ///     cmd::add!("set-text", |pa, new: &str| {
    ///         *changing_str.write(pa) = new.to_string();
    ///         Ok(None)
    ///     })
    /// }
    /// ```
    ///
    /// In the above example, I added some dynamic [`Text`], through
    /// the usage of an [`RwData<Text>`], I added some static
    /// [`Text`], some [`Form`]s (`"counter"` and `"default"`) and
    /// even a counter,.
    ///
    /// [`StatusLine`]: super::StatusLine
    /// [`txt!`]: duat_core::text::txt
    /// [`File`]: duat_core::file::File
    /// [`&File`]: duat_core::file::File
    /// [`&Selections`]: duat_core::mode::Selections
    /// [`&impl Widget`]: duat_core::ui::Widget
    /// [`impl Display`]: std::fmt::Display
    /// [`impl Debug`]: std::fmt::Debug
    /// [`Text`]: duat_core::text::Text
    /// [`RwData`]: duat_core::data::RwData
    /// [`DataMap`]: duat_core::data::DataMap
    /// [`FnOnce(&Pass) -> RwData/DataMap`]: FnOnce
    /// [`(Fn(&Pass) -> Text/Display/Debug, Fn(&Pass) -> bool)`]: Fn
    /// [`RwData<Text>`]: duat_core::data::RwData
    /// [`Form`]: duat_core::form::Form
    /// [`&Area`]: duat_core::ui::Area
    /// [`Area`]: duat_core::ui::Area
    /// [`Widget`]: duat_core::ui::Widget
    /// [`&Pass`]: duat_core::data::Pass
    pub macro status($($parts:tt)*) {{
        #[allow(unused_imports)]
        use $crate::{
            private_exports::{
                duat_core::{context::Handle, data::Pass, file::File, ui::PushSpecs, text::Builder},
                format_like, parse_form, parse_status_part, parse_str
            },
            widgets::StatusLineCfg,
        };

        let text_fn = |_: &Pass, _: &mut Builder, _: &Handle<File<_>, _>| {};
        let checker = |_: &Pass| false;

        let (text_fn, checker) = format_like!(
            parse_str,
            [('{', parse_status_part, false), ('[', parse_form, true)],
            (text_fn, checker),
            $($parts)*
        );

        StatusLineCfg::new_with(
            (
                Box::new(move |pa: &Pass, mut builder: Builder, handle: &Handle<File<_>, _>| {
                    text_fn(pa, &mut builder, &handle);
                    builder.build()
                }),
                Box::new(checker)
            ),
            PushSpecs::below().ver_len(1.0),
        )
    }}
}

type TextFn<U> = Box<dyn Fn(&Pass, &Handle<File<U>, U>) -> Text + Send>;
type BuilderFn<U> = Box<dyn Fn(&Pass, Builder, &Handle<File<U>, U>) -> Text + Send>;
type CheckerFn = Box<dyn Fn(&Pass) -> bool + Send>;

enum FileHandle<U: Ui> {
    Fixed(Handle<File<U>, U>),
    Dynamic(DynFile<U>),
}
