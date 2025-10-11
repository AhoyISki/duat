//! Builder helpers for Duat
//!
//! These builders are used primarily to push widgets to either a
//! [`File`] or a window. They offer a convenient way to make massive
//! changes to the layout, in a very intuitive "inner-outer" order,
//! where widgets get pushed to a "main area" which holds all of the
//! widgets that were added to that helper.
//!
//! This pushing to an unnamed, but known area makes the syntax for
//! layout modification fairly minimal, with minimal boilerplate.
//!
//! [`File`]: crate::file::File
use std::marker::PhantomData;

use super::{PushSpecs, Ui, Widget};
use crate::{
    context::{self, Handle},
    data::Pass,
};

/// A struct for determining what the how the Ui should be constructed
///
/// This struct is used from the [`WidgetCreated`] [hook] every time a
/// new [`Widget`] is created, in order to let you push other
/// [`Widget`]s around that one. This makes the extension of Duat's
/// interface quite friendly.
///
/// It will also be called once per window, from the [`WindowCreated`]
/// [hook], letting you place [`Widget`]s "around" the common [`File`]
/// area, at the edges of the screen.
///
/// # Examples
///
/// Here's some examples involving one of the main ways this will be
/// done, in the [`File`] [`Widget`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<File>(|_, (cfg, builder)| {
///         builder.push(LineNumbers::cfg());
///         cfg
///     });
/// }
/// ```
///
/// In the example above, I pushed a [`LineNumbers`] widget to the
/// [`File`]. By default, this widget will go on the left, but you can
/// change that:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<File>(|_, (cfg, builder)| {
///         let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///         builder.push(line_numbers_cfg);
///         cfg
///     });
/// }
/// ```
///
/// Note that I also made another change to the widget, it will now
/// show relative numbers, instead of absolute, like it usually does.
///
/// By default, there already exists a [hook group] that adds widgets
/// to a file, the `"FileWidgets"` group. If you want to get rid of
/// this group in order to create your own widget layout, you should
/// use [`hook::remove`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("FileWidgets");
///     hook::add::<File>(|_, (cfg, builder)| {
///         builder.push(LineNumbers::cfg().relative().on_the_right());
///         // Push a StatusLine to the bottom.
///         builder.push(StatusLine::cfg());
///         // Push a PromptLine to the bottom.
///         builder.push(PromptLine::cfg());
///         cfg
///     });
/// }
/// ```
///
/// [`File`]: crate::file::File
/// [`LineNumbers`]: https://crates.io/duat-utils/latest/duat_utils/wigets/struct.LineNumbers.html
/// [hook group]: crate::hook::add_grouped
/// [`hook::remove`]: crate::hook::remove
/// [`WindowCreated`]: crate::hook::WindowCreated
pub struct UiBuilder<U: Ui> {
    win: usize,
    _ghost: PhantomData<U>,
}

impl<U: Ui> UiBuilder<U> {
    /// Returns a new [`UiBuilder`], with no main [`Widget`]
    ///
    /// This is used when creating
    pub(super) fn new(win: usize) -> Self {
        Self { win, _ghost: PhantomData }
    }

    /// Pushes a widget to the main area of the [`UiBuilder`]
    ///
    /// If this [`Widget`] is being pushed to a [`File`]'s group,
    /// then this [`Widget`] will be included in that [`File`]'s
    /// group. This means that, if that [`File`] is moved around
    /// or deleted, this [`Widget`] (and all others in its group)
    /// will follow suit.
    ///
    /// When you push a [`Widget`], it is placed on an edge of the
    /// area, and a new parent area may be created to hold both
    /// widgets. If created, that new area will be used for pushing
    /// widgets in the future.
    ///
    /// This means that, if you push widget *A* to the left, then you
    /// push widget *B* to the bottom, you will get this layout:
    ///
    /// ```text
    /// ╭───┬──────────╮
    /// │   │          │
    /// │ A │   File   │
    /// │   │          │
    /// ├───┴──────────┤
    /// │      B       │
    /// ╰──────────────╯
    /// ```
    ///
    /// Here's an example of such a layout:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::remove("FileWidgets");
    ///     hook::add::<File>(|_, (cfg, builder)| {
    ///         builder.push(LineNumbers::cfg().rel_abs());
    ///         builder.push(status!("{name_txt} {selections_txt} {main_txt}"));
    ///         cfg
    ///     });
    /// }
    /// ```
    ///
    /// In this case, each file will have [`LineNumbers`] with
    /// relative/absolute numbering, and a [`StatusLine`] showing
    /// the file's name, how many selections are in it, and its main
    /// selection.
    ///
    /// [`File`]: crate::file::File
    /// [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    /// [`WindowCreated`]: crate::hook::WindowCreated
    pub fn push_inner<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        widget: W,
        specs: PushSpecs,
    ) -> Handle<W, U> {
        let files = context::windows::<U>()
            .get(pa, self.win)
            .unwrap()
            .files_area
            .clone();

        context::windows::<U>().push_widget(pa, (&files, specs), widget).unwrap()
    }

    /// Docs: TODO
    pub fn push_outer<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        widget: W,
        specs: PushSpecs,
    ) -> Handle<W, U> {
        let master = context::windows::<U>()
            .get(pa, self.win)
            .unwrap()
            .master_area
            .clone();

        context::windows::<U>().push_widget(pa, (&master, specs), widget).unwrap()
    }
}
