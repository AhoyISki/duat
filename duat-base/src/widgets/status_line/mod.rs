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

pub use self::state::State;
use crate::state::{main_txt, mode_txt, name_txt, sels_txt};

mod state;
#[macro_use]
mod macros;
#[doc(inline)]
pub use crate::__status__ as status;

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
                    status!("{mode_txt}{Spacer}{name_txt} {sels_txt} {main_txt}")
                }
                Side::Right => {
                    status!("{AlignRight}{name_txt} {mode_txt} {sels_txt} {main_txt}",)
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
        StatusLineFmt { fns: None, ..Default::default() }
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
pub struct StatusLineFmt {
    fns: Option<(BuilderFn, CheckerFn)>,
    specs: PushSpecs,
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
        let status_line = StatusLine::new(
            self,
            match push_target.try_downcast() {
                Some(handle) => BufferHandle::Fixed(handle),
                None => BufferHandle::Dynamic(context::dynamic_buffer(pa)),
            },
        );

        push_target.push_outer(pa, status_line, specs)
    }

    /// Returns a new `StatusLineFmt`, meant to be called only be the
    /// [`status!`] macro
    #[doc(hidden)]
    pub fn new_with(fns: (BuilderFn, CheckerFn)) -> Self {
        Self { fns: Some(fns), ..Default::default() }
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

impl Default for StatusLineFmt {
    fn default() -> Self {
        Self {
            fns: None,
            specs: PushSpecs {
                side: Side::Below,
                height: Some(1.0),
                ..Default::default()
            },
        }
    }
}

type TextFn = Box<dyn Fn(&Pass, &Handle) -> Text + Send>;
type BuilderFn = Box<dyn Fn(&Pass, Builder, &Handle) -> Text + Send>;
type CheckerFn = Box<dyn Fn(&Pass) -> bool + Send>;

#[derive(Clone)]
enum BufferHandle {
    Fixed(Handle),
    Dynamic(DynBuffer),
}
