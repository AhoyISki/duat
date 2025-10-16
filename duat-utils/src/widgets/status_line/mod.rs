//! A widget that shows general information, usually about a [`Buffer`]
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

use duat_core::{
    context::DynBuffer,
    data::DataMap,
    prelude::*,
    text::Builder,
    ui::{PushTarget, Side},
};

pub use self::{macros::status, state::State};
use crate::state::{main_txt, mode_txt, name_txt, sels_txt};

/// A widget to show information, usually about a [`Buffer`]
///
/// This widget is updated whenever any of its parts needs to be
/// updated, and it also automatically adjusts to where it was pushed.
/// For example, if you push it to a file (via `hook::add::<Buffer>`,
/// for example), it's information will point to the [`Buffer`] to which
/// it was pushed. However, if you push it with [`WindowCreated`], it
/// will always point to the currently active [`Buffer`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_utils::widgets::{FooterWidgets, status};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<Buffer>(|_, (cfg, builder)| {
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
/// active [`Buffer`], instead of a specific one.
///
/// You will usually want to create [`StatusLine`]s via the
/// [`status!`] macro, since that is how you can customize it.
/// Although, if you want the regular status line, you can just:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_utils::widgets::{LineNumbers, StatusLine};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("BufferWidgets");
///     hook::add::<Buffer>(|_, (cfg, builder)| {
///         builder.push(LineNumbers::cfg());
///         builder.push(StatusLine::cfg().above());
///         cfg
///     });
/// }
/// ```
///
/// [`Buffer`]: duat_core::buffer::Buffer
/// [`WindowCreated`]: duat_core::hook::WindowCreated
/// [`PromptLine`]: super::PromptLine
/// [`Notifications`]: super::Notifications
/// [`FooterWidgets`]: super::FooterWidgets
pub struct StatusLine {
    file_handle: BufferHandle,
    text_fn: TextFn,
    text: Text,
    checker: Box<dyn Fn(&Pass) -> bool + Send>,
}

impl StatusLine {
    fn new(builder: StatusLineBuilder, file_handle: BufferHandle) -> Self {
        let (builder_fn, checker) = if let Some((builder, checker)) = builder.fns {
            (builder, checker)
        } else {
            let Some(mode_txt) = builder.mode_txt else {
                unreachable!("mode_txt not xor with the default config");
            };

            let cfg = match builder.specs.side {
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

        Self {
            file_handle,
            text_fn: Box::new(move |pa, fh| {
                let builder = Text::builder();
                builder_fn(pa, builder, fh)
            }),
            text: Text::new(),
            checker: Box::new(checker),
        }
    }

    /// Replaces this `StatusLine` with a new one
    pub fn fmt(&mut self, new: StatusLineBuilder) {
        let handle = self.file_handle.clone();
        *self = StatusLine::new(new, handle);
    }

    /// Returns a [`StatusLineBuilder`], which can be used to push
    /// around `StatusLine`s
    ///
    /// The same can be done more conveniently with the [`status!`]
    /// macro, which is imported by default in the configuration
    /// crate.
    pub fn builder(pa: &Pass) -> StatusLineBuilder {
        StatusLineBuilder {
            mode_txt: Some(mode_txt(pa)),
            fns: None,
            ..
        }
    }
}

impl Widget for StatusLine {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        if let BufferHandle::Dynamic(dyn_file) = &mut handle.write(pa).file_handle {
            dyn_file.swap_to_current();
        }

        let sl = handle.read(pa);

        handle.write(pa).text = match &sl.file_handle {
            BufferHandle::Fixed(file) => (sl.text_fn)(pa, file),
            BufferHandle::Dynamic(dyn_file) => (sl.text_fn)(pa, dyn_file.handle()),
        };

        // Do this in case the Buffer is never read during Text construction
        match &handle.read(pa).file_handle {
            BufferHandle::Fixed(handle) => handle.declare_as_read(),
            BufferHandle::Dynamic(dyn_file) => dyn_file.declare_as_read(),
        }
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        let file_changed = match &self.file_handle {
            BufferHandle::Fixed(handle) => handle.has_changed(pa),
            BufferHandle::Dynamic(dyn_file) => dyn_file.has_changed(pa),
        };
        let checkered = (self.checker)(pa);

        file_changed || checkered
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}

/// The [`WidgetCfg`] for a [`StatusLine`]
#[doc(hidden)]
pub struct StatusLineBuilder {
    mode_txt: Option<DataMap<&'static str, Text>>,
    fns: Option<(BuilderFn, CheckerFn)>,
    specs: PushSpecs = PushSpecs { side: Side::Below, height: Some(1.0), .. },
}

impl StatusLineBuilder {
    /// Push the [`StatusLine`]
    ///
    /// If the handle's [`Widget`] is a [`Buffer`], then this
    /// `StatusLine` will refer to it when printing information about
    /// `Buffer`s. Otherwise, the `StatusLine` will print information
    /// about the currently active `Buffer`.
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<StatusLine> {
        let specs = self.specs;
        let status_line = StatusLine::new(self, match push_target.try_downcast() {
            Some(handle) => BufferHandle::Fixed(handle),
            None => BufferHandle::Dynamic(context::dyn_file(pa)),
        });

        push_target.push_outer(pa, status_line, specs)
    }

    #[doc(hidden)]
    pub fn new_with(fns: (BuilderFn, CheckerFn)) -> Self {
        Self { mode_txt: None, fns: Some(fns), .. }
    }

    /// Puts the [`StatusLine`] above, as opposed to below
    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs { side: Side::Above, ..self.specs },
            ..self
        }
    }

    /// Puts the [`StatusLine`] below, this is the default
    pub fn below(self) -> Self {
        Self {
            specs: PushSpecs { side: Side::Below, ..self.specs },
            ..self
        }
    }

    /// Puts the [`StatusLine`] on the right
    pub(crate) fn right(self) -> Self {
        Self {
            specs: PushSpecs { side: Side::Right, ..self.specs },
            ..self
        }
    }

    /// The [`PushSpecs`] in use
    pub fn specs(&self) -> PushSpecs {
        self.specs
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
    /// The macro will mostly read from the [`Buffer`] widget and its
    /// related structs. In order to do that, it will accept functions
    /// as arguments. These functions take the following parameters:
    ///
    /// * The [`&Buffer`] widget;
    /// * A specific [`&impl Widget`], which is glued to the [`Buffer`];
    ///
    /// Both of these can also have a second argument of type
    /// [`&Area`]. This will include the [`Widget`]'s [`Area`] when
    /// creating the status part. Additionally, you may include a
    /// first argument of type [`&Pass`] (e.g. `fn(&Pass, &Buffer)`,
    /// `fn(&Pass, &Widget, &Area), etc.), giving you _non mutating_
    /// access to global state.
    ///
    /// Here's some examples:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat_utils::widgets::status;
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn name_but_funky(file: &Buffer) -> String {
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
    /// fn powerline_main_txt(file: &Buffer, area: &Area) -> Text {
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
    /// previous arguments update when the [`Buffer`] updates. The
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
    /// # duat_core::doc_duat!(duat);
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
    ///             let counter = move |_: &Buffer| counter(checker());
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
    /// [`Buffer`]: duat_core::buffer::Buffer
    /// [`&Buffer`]: duat_core::buffer::Buffer
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
                duat_core::{context::Handle, data::Pass, buffer::Buffer, ui::PushSpecs, text::Builder},
                format_like, parse_form, parse_status_part, parse_str
            },
            widgets::StatusLineBuilder,
        };

        let text_fn = |_: &Pass, _: &mut Builder, _: &Handle<Buffer>| {};
        let checker = |_: &Pass| false;

        let (text_fn, checker) = format_like!(
            parse_str,
            [('{', parse_status_part, false), ('[', parse_form, true)],
            (text_fn, checker),
            $($parts)*
        );

        StatusLineBuilder::new_with(
            (
                Box::new(move |pa: &Pass, mut builder: Builder, handle: &Handle<Buffer>| {
                    text_fn(pa, &mut builder, &handle);
                    builder.build()
                }),
                Box::new(checker)
            ),
        )
    }}
}

type TextFn = Box<dyn Fn(&Pass, &Handle<Buffer>) -> Text + Send>;
type BuilderFn = Box<dyn Fn(&Pass, Builder, &Handle<Buffer>) -> Text + Send>;
type CheckerFn = Box<dyn Fn(&Pass) -> bool + Send>;

#[derive(Clone)]
enum BufferHandle {
    Fixed(Handle<Buffer>),
    Dynamic(DynBuffer),
}
