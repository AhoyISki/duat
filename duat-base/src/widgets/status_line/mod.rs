//! A widget that shows general information, usually about a
//! [`Buffer`]
//!
//! The [`StatusLine`] is a very convenient widget when the user
//! simply wants to show some informatioon. The information, when
//! relevant, can automatically be tied to the active buffer, saving
//! some keystrokes for the user's configuration.
//!
//! There is also the [`status!`] macro, which is an extremely
//! convenient way to modify the text of the status line, letting you
//! place form, in the same way that [`text!`] does, and
//! automatically recognizing a ton of different types of functions,
//! that can read from the buffer, from other places, from [data]
//! types, etc.
//!
//! [data]: crate::data
//! [`Buffer`]: duat_core::buffer::Buffer
use duat_core::{
    context::{self, DynBuffer, Handle},
    data::Pass,
    text::{AlignRight, Builder, Spacer, Text},
    ui::{PushSpecs, PushTarget, Side, Widget},
};

pub use self::{macros::status, state::State};
use crate::state::{main_txt, mode_txt, name_txt, sels_txt};

mod state;

/// A widget to show information, usually about a [`Buffer`]
///
/// This widget is updated whenever any of its parts needs to be
/// updated, and it also automatically adjusts to where it was pushed.
/// For example, if you push it to a buffer (via
/// `hook::add::<Buffer>`, for example), it's information will point
/// to the [`Buffer`] to which it was pushed. However, if you push it
/// with [`WindowCreated`], it will always point to the currently
/// active [`Buffer`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::widgets::status;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     opts::one_line_footer();
///     opts::set_status(|pa| status!("{AlignRight}{} {sels_txt} {main_txt}", mode_txt()));
///
///     hook::add::<Buffer>(|pa, handle| {
///         status!("{AlignCenter}{name_txt}")
///             .above()
///             .push_on(pa, handle);
///         Ok(())
///     });
/// }
/// ```
///
/// In the code above, I'm modifying the "global" `StatusLine` through
/// [`opts::set_status`] (this can be done with [hooks] as well, but
/// this method is added for convenience's sake). This is in
/// conjunction with [`opts::one_line_footer`], which will place
/// the [`PromptLine`] and `StatusLine` on the same line.
///
/// After that, I'm _also_ pushing a new `StatusLine` above every
/// opened [`Buffer`], showing that `Buffer`]'s name, centered.
///
/// You will usually want to create `StatusLine`s via the
/// [`status!`] macro, since that is how you can customize it.
/// Although, if you want the regular status line, you can call
/// [`StatusLine::builder`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::widgets::StatusLine;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<Buffer>(|pa, handle| {
///         StatusLine::builder().above().push_on(pa, handle);
///         Ok(())
///     });
/// }
/// ```
///
/// [`Buffer`]: duat_core::buffer::Buffer
/// [`WindowCreated`]: duat_core::hook::WindowCreated
/// [`PromptLine`]: super::PromptLine
/// [`Notifications`]: super::Notifications
/// [`FooterWidgets`]: super::FooterWidgets
/// [`opts::set_status`]: https://docs.rs/duat/latest/duat/opts/fn.set_status.html
/// [`opts::one_line_footer`]: https://docs.rs/duat/latest/duat/opts/fn.one_line_footer.html
/// [hooks]: duat_core::hook
pub struct StatusLine {
    buffer_handle: BufferHandle,
    text_fn: TextFn,
    text: Text,
    checker: Box<dyn Fn(&Pass) -> bool + Send>,
}

impl StatusLine {
    fn new(builder: StatusLineFmt, buffer_handle: BufferHandle) -> Self {
        let (builder_fn, checker) = if let Some((builder, checker)) = builder.fns {
            (builder, checker)
        } else {
            let mode_txt = mode_txt();

            let opts = match builder.specs.side {
                Side::Above | Side::Below => {
                    macros::status!("{mode_txt}{Spacer}{name_txt} {sels_txt} {main_txt}")
                }
                Side::Right => {
                    macros::status!("{AlignRight}{name_txt} {mode_txt} {sels_txt} {main_txt}",)
                }
                Side::Left => unreachable!(),
            };

            opts.fns.unwrap()
        };

        Self {
            buffer_handle,
            text_fn: Box::new(move |pa, fh| {
                let builder = Text::builder();
                builder_fn(pa, builder, fh)
            }),
            text: Text::new(),
            checker: Box::new(checker),
        }
    }

    /// Replaces this `StatusLine` with a new one
    pub fn fmt(&mut self, new: StatusLineFmt) {
        let handle = self.buffer_handle.clone();
        *self = StatusLine::new(new, handle);
    }

    /// Returns a [`StatusLineFmt`], which can be used to push
    /// around `StatusLine`s
    ///
    /// The same can be done more conveniently with the [`status!`]
    /// macro, which is imported by default in the configuration
    /// crate.
    pub fn builder() -> StatusLineFmt {
        StatusLineFmt { fns: None, .. }
    }
}

impl Widget for StatusLine {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        if let BufferHandle::Dynamic(dyn_file) = &mut handle.write(pa).buffer_handle {
            dyn_file.swap_to_current();
        }

        let sl = handle.read(pa);

        handle.write(pa).text = match &sl.buffer_handle {
            BufferHandle::Fixed(buffer) => (sl.text_fn)(pa, buffer),
            BufferHandle::Dynamic(dyn_file) => (sl.text_fn)(pa, dyn_file.handle()),
        };

        // Do this in case the Buffer is never read during Text construction
        match &handle.read(pa).buffer_handle {
            BufferHandle::Fixed(handle) => handle.declare_as_read(),
            BufferHandle::Dynamic(dyn_file) => dyn_file.declare_as_read(),
        }
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        let buffer_changed = match &self.buffer_handle {
            BufferHandle::Fixed(handle) => handle.has_changed(pa),
            BufferHandle::Dynamic(dyn_buf) => dyn_buf.has_changed(pa),
        };
        let checkered = (self.checker)(pa);

        buffer_changed || checkered
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}

/// A builder for [`StatusLine`]s
///
/// This struct is created by the [`status!`] macro, and its purpose
/// is mainly to allow formatting of the `StatusLine`.
///
/// There is also the [`StatusLineFmt::above`] method, which places
/// the `StatusLine` above, rather than below.
#[derive(Default)]
pub struct StatusLineFmt {
    fns: Option<(BuilderFn, CheckerFn)>,
    specs: PushSpecs = PushSpecs { side: Side::Below, height: Some(1.0), .. },
}

impl StatusLineFmt {
    /// Push the [`StatusLine`]
    ///
    /// If the handle's [`Widget`] is a [`Buffer`], then this
    /// `StatusLine` will refer to it when printing information about
    /// `Buffer`s. Otherwise, the `StatusLine` will print information
    /// about the currently active `Buffer`.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<StatusLine> {
        let specs = self.specs;
        let status_line = StatusLine::new(self, match push_target.try_downcast() {
            Some(handle) => BufferHandle::Fixed(handle),
            None => BufferHandle::Dynamic(context::dynamic_buffer(pa)),
        });

        push_target.push_outer(pa, status_line, specs)
    }

    /// Returns a new `StatusLineFmt`, meant to be called only be the
    /// [`status!`] macro
    #[doc(hidden)]
    pub fn new_with(fns: (BuilderFn, CheckerFn)) -> Self {
        Self { fns: Some(fns), .. }
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
    /// as arguments. These functions can take any of the following
    /// parameters, with up to 8 arguments each:
    ///
    /// - [`&Buffer`]: The `Buffer` in question.
    /// - [`&Handle`]: The `Handle` of said `Buffer`.
    /// - [`&Area`]: The `Area` of said `Buffer`.
    /// - [`&Pass`]: For global reading access.
    /// - [`&Text`]: The `Text` of the `Buffer`.
    /// - [`&Selections`]: The `Selections` of the `Buffer`.
    /// - [`&Selection`]: The main `Selection` of the `Buffer`.
    /// - [`&Window`]: The `Window` the `Buffer` is situated in.
    ///
    /// Here's some examples:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat_base::widgets::status;
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn name_but_funky(buf: &Buffer) -> String {
    ///     buf.name()
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
    /// fn powerline_main_txt(buffer: &Buffer, area: &Area) -> Text {
    ///     let selections = buffer.selections();
    ///     let opts = buffer.get_print_opts();
    ///     let v_caret = selections
    ///         .get_main()
    ///         .unwrap()
    ///         .v_caret(buffer.text(), area, opts);
    ///
    ///     txt!(
    ///         "[separator][coord]{}[separator][coord]{}[separator][coord]{}",
    ///         v_caret.visual_col(),
    ///         v_caret.line(),
    ///         buffer.len_lines()
    ///     )
    /// }
    ///
    /// fn setup() {
    ///     opts::set_status(|pa| status!("[buffer]{name_but_funky}{Spacer}{powerline_main_txt}"));
    /// }
    /// ```
    ///
    /// There are other types of arguments you can push, not
    /// necessarily tied to a `Buffer`:
    ///
    /// - Static arguments:
    ///   - A [`Text`] argument, which can be formatted in a similar
    ///     way throught the [`txt!`] macro;
    ///   - Any [`impl Display`], such as numbers, strings, chars,
    ///     etc. [`impl Debug`] types also work, when including the
    ///     usual `":?"` and derived suffixes;
    /// - Dynamic arguments:
    ///   - An [`RwData`] or [`DataMap`]s of the previous two types.
    ///     These will update whenever the data inside is changed;
    ///
    /// Here's an examples:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat_base::widgets::status;
    /// setup_duat!(setup);
    /// use std::sync::atomic::{AtomicUsize, Ordering};
    ///
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     let changing_str = data::RwData::new("Initial text".to_string());
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
    ///         move |pa, window| {
    ///             let changing_str = changing_str.clone();
    ///             let checker = changing_str.checker();
    ///
    ///             let text = txt!("Static text");
    ///             let counter = move || counter(checker());
    ///
    ///             status!("{changing_str} [counter]{counter}[] {text}")
    ///                 .above()
    ///                 .push_on(pa, window);
    ///             Ok(())
    ///         }
    ///     });
    ///
    ///     cmd::add!("set-str", |pa, new: &str| {
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
    /// [`&Handle`]: duat_core::context::Handle
    /// [`&Area`]: duat_core::ui::Area
    /// [`&Pass`]: duat_core::data::Pass
    /// [`&Text`]: duat_core::text::Text
    /// [`&Selections`]: duat_core::mode::Selections
    /// [`&Selection`]: duat_core::mode::Selection
    /// [`&Window`]: duat_core::ui::Window
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
    pub macro status($($parts:tt)*) {{
        #[allow(unused_imports)]
        use $crate::{
            private_exports::{
                duat_core::{context::Handle, data::Pass, ui::PushSpecs, text::Builder},
                format_like, parse_form, parse_status_part, parse_str
            },
            widgets::StatusLineFmt,
        };

        let text_fn = |_: &Pass, _: &mut Builder, _: &Handle| {};
        let checker = |_: &Pass| false;

        let (text_fn, checker) = format_like!(
            parse_str,
            [('{', parse_status_part, false), ('[', parse_form, true)],
            (text_fn, checker),
            $($parts)*
        );

        StatusLineFmt::new_with(
            (
                Box::new(move |pa: &Pass, mut builder: Builder, handle: &Handle| {
                    builder.no_space_after_empty = true;
                    text_fn(pa, &mut builder, &handle);
                    builder.build()
                }),
                Box::new(checker)
            ),
        )
    }}
}

type TextFn = Box<dyn Fn(&Pass, &Handle) -> Text + Send>;
type BuilderFn = Box<dyn Fn(&Pass, Builder, &Handle) -> Text + Send>;
type CheckerFn = Box<dyn Fn(&Pass) -> bool + Send>;

#[derive(Clone)]
enum BufferHandle {
    Fixed(Handle),
    Dynamic(DynBuffer),
}
