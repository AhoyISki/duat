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
//! *DO NOTE THAT THE EXAMPLES IN HERE USE WIDGETS FROM `duat-utils`*
//!
//! And because that is the case, in order to save space, the hidden
//! doc code is complete gibberish, don't try to use that.
//!
//! [`File`]: crate::file::File
use std::sync::Mutex;

use super::{RawArea, Ui};
use crate::{
    context::{self, FileHandle},
    data::Pass,
    duat_name,
    file::{File, ReaderCfg},
    hook::Hookable,
    ui::{Node, Widget, WidgetCfg},
};

/// A constructor helper for [`File`] initiations
///
/// This helper is used primarily to push widgets around the file in
/// question, and is only obtainable in a [`OnFileOpen`] hook:
///
/// ```rust
/// # use duat_core::{hook::OnFileOpen, prelude::*, ui::FileBuilder};
/// # struct LineNumbers<U: Ui>(std::marker::PhantomData<U>);
/// # impl<U: Ui> Widget<U> for LineNumbers<U> {
/// #     type Cfg = LineNumbersOptions<U>;
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!(); }
/// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
/// #     type Widget = LineNumbers<U>;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
/// #         todo!();
/// #     }
/// # }
/// # fn test<U: Ui>() {
/// hook::add::<OnFileOpen<U>>(|pa: &mut Pass, builder: &mut FileBuilder<U>| {
///     builder.push(pa, LineNumbers::cfg());
/// });
/// # }
/// ```
///
/// In the example above, I pushed a [`LineNumbers`] widget to the
/// [`File`]. By default, this widget will go on the left, but you can
/// change that:
///
/// ```rust
/// # use duat_core::{hook::OnFileOpen, prelude::*};
/// # struct LineNumbers<U: Ui>(std::marker::PhantomData<U>);
/// # impl<U: Ui> Widget<U> for LineNumbers<U> {
/// #     type Cfg = LineNumbersOptions<U>;
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!(); }
/// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
/// # impl<U> LineNumbersOptions<U> {
/// #     pub fn relative(self) -> Self { todo!(); }
/// #     pub fn on_the_right(self) -> Self { todo!(); }
/// # }
/// # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
/// #     type Widget = LineNumbers<U>;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
/// #         todo!();
/// #     }
/// # }
/// # fn test<U: Ui>() {
/// hook::add::<OnFileOpen<U>>(|pa, builder| {
///     let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///     builder.push(pa, line_numbers_cfg);
/// });
/// # }
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
/// # use duat_core::{hook::OnFileOpen, prelude::*};
/// # struct LineNumbers<U: Ui>(std::marker::PhantomData<U>);
/// # impl<U: Ui> Widget<U> for LineNumbers<U> {
/// #     type Cfg = LineNumbersOptions<U>;
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!(); }
/// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
/// # impl<U> LineNumbersOptions<U> {
/// #     pub fn relative(self) -> Self { todo!(); }
/// #     pub fn on_the_right(self) -> Self { todo!(); }
/// # }
/// # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
/// #     type Widget = LineNumbers<U>;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
/// #         todo!();
/// #     }
/// # }
/// # type StatusLine<U> = LineNumbers<U>;
/// # type PromptLine<U> = LineNumbers<U>;
/// # fn test<U: Ui>() {
/// hook::remove("FileWidgets");
/// hook::add::<OnFileOpen<U>>(|pa, builder| {
///     let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///     builder.push(pa, line_numbers_cfg);
///     // Push a StatusLine to the bottom.
///     builder.push(pa, StatusLine::cfg());
///     // Push a PromptLine to the bottom.
///     builder.push(pa, PromptLine::cfg());
/// });
/// # }
/// ```
///
/// [`File`]: crate::file::File
/// [`OnFileOpen`]: crate::hook::OnFileOpen
/// [`LineNumbers`]: https://crates.io/duat-utils/latest/duat_utils/wigets/struct.LineNumbers.html
/// [hook group]: crate::hook::add_grouped
/// [`hook::remove`]: crate::hook::remove
pub struct FileBuilder<U: Ui> {
    window_i: usize,
    handle: FileHandle<U>,
    area: U::Area,
}

impl<U: Ui> FileBuilder<U> {
    /// Creates a new [`FileBuilder`].
    pub(crate) fn new(pa: &mut Pass, node: Node<U>, window_i: usize) -> Self {
        let name = node.read_as(pa, |f: &File<U>| f.name()).unwrap();
        let handle = context::file_named(pa, name).unwrap();
        let area = node.area().clone();

        Self { window_i, handle, area }
    }

    /// Pushes a widget to the main area of this [`File`]
    ///
    /// This widget will be a satellite of the file. This means that,
    /// if the file is destroyed, the widget will be destroyed as
    /// well, if it is moved, the widget will be moved with it, etc.
    ///
    /// When you push a widget, it is placed on the edge of the file.
    /// The next widget is placed on the edge of the area containing
    /// the file and the previous widget, and so on.
    ///
    /// This means that, if you push widget *A* to the left of the
    /// file, then you push widget *B* to the bottom of the window,
    /// you will get this layout:
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
    /// # use duat_core::{hook::OnFileOpen, prelude::*, ui::FileBuilder};
    /// # struct LineNumbers<U: Ui>(std::marker::PhantomData<U>);
    /// # impl<U: Ui> Widget<U> for LineNumbers<U> {
    /// #     type Cfg = LineNumbersOptions<U>;
    /// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
    /// #     fn needs_update(&self) -> bool { todo!(); }
    /// #     fn cfg() -> Self::Cfg { todo!() }
    /// #     fn text(&self) -> &Text { todo!(); }
    /// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
    /// #     fn once() -> Result<(), Text> { Ok(()) }
    /// # }
    /// # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
    /// # impl<U> LineNumbersOptions<U> {
    /// #     pub fn rel_abs(self) -> Self { todo!(); }
    /// # }
    /// # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
    /// #     type Widget = LineNumbers<U>;
    /// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// # macro_rules! status { ($str:literal) => { LineNumbers::cfg() } }
    /// # fn test<U: Ui>() {
    /// hook::remove("FileWidgets");
    /// hook::add::<OnFileOpen<U>>(|pa: &mut Pass, builder: &mut FileBuilder<U>| {
    ///     let line_numbers_cfg = LineNumbers::cfg().rel_abs();
    ///     builder.push(pa, line_numbers_cfg);
    ///
    ///     let status_line_cfg = status!("{file_fmt} {selections_fmt} {main_fmt}");
    ///     builder.push(pa, status_line_cfg);
    /// });
    /// # }
    /// ```
    ///
    /// In this case, each file will have [`LineNumbers`] with
    /// relative/absolute numbering, and a [`StatusLine`] showing
    /// the file's name and how many selections are in it.
    ///
    /// [`File`]: crate::file::File
    /// [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    #[inline(never)]
    pub fn push<W: WidgetCfg<U>>(&mut self, pa: &mut Pass, cfg: W) -> (U::Area, Option<U::Area>) {
        run_once::<W::Widget, U>();
        let (widget, specs) = cfg.build(pa, Some(self.handle.clone()));

        let mut windows = context::windows().borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(pa, widget, &self.area, specs, true, true);

        self.get_areas(pa, window, node, parent)
    }

    fn get_areas(
        &mut self,
        pa: &mut Pass,
        window: &mut super::Window<U>,
        node: Node<U>,
        parent: Option<U::Area>,
    ) -> (U::Area, Option<U::Area>) {
        self.handle
            .write_related_widgets(pa, |related| related.push(node.clone()));

        if let Some(parent) = &parent
            && parent.is_master_of(&window.files_area)
        {
            window.files_area = parent.clone();
            self.area = parent.clone();
        }

        (node.area().clone(), parent)
    }

    /// Pushes a widget to a specific area around a [`File`]
    ///
    /// This method can be used to get some more advanced layouts,
    /// where you have multiple widgets parallel to each other, yet on
    /// the same edge.
    ///
    /// One of the main ways in which this is used is when using a
    /// hidden [`Notifier`] widget alongside the [`PromptLine`]
    /// widget.
    ///
    /// ```rust
    /// # use duat_core::{hook::OnFileOpen, prelude::*};
    /// # struct StatusLine<U: Ui>(std::marker::PhantomData<U>);
    /// # impl<U: Ui> Widget<U> for StatusLine<U> {
    /// #     type Cfg = StatusLineOptions<U>;
    /// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
    /// #     fn needs_update(&self) -> bool { todo!(); }
    /// #     fn cfg() -> Self::Cfg { todo!() }
    /// #     fn text(&self) -> &Text { todo!(); }
    /// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
    /// #     fn once() -> Result<(), Text> { Ok(()) }
    /// # }
    /// # struct StatusLineOptions<U>(std::marker::PhantomData<U>);
    /// # impl<U> StatusLineOptions<U> {
    /// #     pub fn left_ratioed(self, _: usize, _: usize) -> Self { todo!(); }
    /// # }
    /// # impl<U: Ui> WidgetCfg<U> for StatusLineOptions<U> {
    /// #     type Widget = StatusLine<U>;
    /// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// # type PromptLine<U> = StatusLine<U>;
    /// # type Notifier<U> = StatusLine<U>;
    /// # type LineNumbers<U> = StatusLine<U>;
    /// # macro_rules! status { ($str:literal) => { StatusLine::cfg() } }
    /// # fn test<U: Ui>() {
    /// hook::remove("FileWidgets");
    /// hook::add::<OnFileOpen<U>>(|pa, builder| {
    ///     builder.push(pa, LineNumbers::cfg());
    ///
    ///     let status_cfg = status!("{file_fmt} {selections_fmt} {main_fmt}");
    ///     let (child, _) = builder.push(pa, status_cfg);
    ///     let prompt_cfg = PromptLine::cfg().left_ratioed(3, 5);
    ///     let (child, _) = builder.push_to(pa, child, prompt_cfg);
    ///     builder.push_to(pa, child, Notifier::cfg());
    /// });
    /// # }
    /// ```
    ///
    /// Pushing directly to the [`PromptLine`]'s [`U::Area`] means
    /// that they'll share a parent that holds only them. This can
    /// then be exploited by the `"HidePromptLine"` [hook group],
    /// which is defined as:
    /// ```rust
    /// # use duat_core::{hook::{FocusedOn, UnfocusedFrom}, prelude::*, ui::Constraint};
    /// # struct PromptLine<U: Ui>(std::marker::PhantomData<U>);
    /// # impl<U: Ui> Widget<U> for PromptLine<U> {
    /// #     type Cfg = PromptLineOptions<U>;
    /// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
    /// #     fn needs_update(&self) -> bool { todo!(); }
    /// #     fn cfg() -> Self::Cfg { todo!() }
    /// #     fn text(&self) -> &Text { todo!(); }
    /// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
    /// #     fn once() -> Result<(), Text> { Ok(()) }
    /// # }
    /// # struct PromptLineOptions<U>(std::marker::PhantomData<U>);
    /// # impl<U: Ui> WidgetCfg<U> for PromptLineOptions<U> {
    /// #     type Widget = PromptLine<U>;
    /// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// # fn test<U: duat_core::ui::Ui>() {
    /// hook::add_grouped::<UnfocusedFrom<PromptLine<U>, U>>(
    ///     "HidePromptLine",
    ///     |_, handle| {
    ///         handle.area().constrain_ver([Constraint::Len(0.0)]).unwrap();
    ///     },
    /// );
    /// hook::add_grouped::<FocusedOn<PromptLine<U>, U>>(
    ///     "HidePromptLine",
    ///     |_, handle| {
    ///         handle
    ///             .area()
    ///             .constrain_ver([Constraint::Ratio(1, 1), Constraint::Len(1.0)])
    ///             .unwrap();
    ///     },
    /// );
    /// # }
    /// ```
    ///
    /// [`File`]: crate::file::File
    /// [`Notifier`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.Notifier.html
    /// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    /// [`U::Area`]: Ui::Area
    /// [hook group]: crate::hook::add_grouped
    #[inline(never)]
    pub fn push_to<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        area: U::Area,
        cfg: W,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W::Widget, U>();
        let (widget, specs) = cfg.build(pa, Some(self.handle.clone()));

        let mut windows = context::windows().borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(pa, widget, &area, specs, true, true);
        self.handle
            .write_related_widgets(pa, |related| related.push(node.clone()));
        (node.area().clone(), parent)
    }

    /// Adds a [`Reader`] to this [`File`]
    ///
    /// [`Reader`]s read changes to [`Text`] and can act accordingly
    /// by adding or removing [`Tag`]s from it. They can also be
    /// accessed via a public API, in order to be used for other
    /// things, like the treesitter [`Reader`], which, internally,
    /// creates the syntax tree and does syntax highlighting, but
    /// externally it can also be used for indentation of [`Text`] by
    /// [`Mode`]s
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    /// [`Reader`]: crate::file::Reader
    /// [`Mode`]: crate::mode::Mode
    pub fn add_reader(&mut self, pa: &mut Pass, reader_cfg: impl ReaderCfg<U>) {
        self.handle.write(pa, |file, _| {
            // SAFETY: Because this function takes in a &mut Pass, it is safe
            // to create new ones inside.
            let mut pa = unsafe { Pass::new() };
            file.add_reader(&mut pa, reader_cfg)
        })
    }

    /// The [`File`] that this hook is being applied to
    pub fn read<Ret>(&mut self, pa: &Pass, f: impl FnOnce(&File<U>, &U::Area) -> Ret) -> Ret {
        self.handle.read(pa, f)
    }

    /// Mutable reference to the [`File`] that this hooks is being
    /// applied to
    pub fn write<Ret>(
        &mut self,
        pa: &mut Pass,
        f: impl FnOnce(&mut File<U>, &U::Area) -> Ret,
    ) -> Ret {
        self.handle.write(pa, f)
    }
}

/// A constructor helper for window initiations
///
/// This helper is used primarily to push widgets around the window,
/// surrounding the "main area" which contains all [`File`]s and
/// widgets added via the [`OnFileOpen`] hook.
///
/// It is used whenever the [`OnWindowOpen`] hook is triggered, which
/// happens whenever a new window is opened:
///
/// ```rust
/// # use duat_core::{hook::OnWindowOpen, prelude::*};
/// # struct PromptLine<U: Ui>(std::marker::PhantomData<U>);
/// # impl<U: Ui> Widget<U> for PromptLine<U> {
/// #     type Cfg = PromptLineOptions<U>;
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!(); }
/// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # struct PromptLineOptions<U>(std::marker::PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for PromptLineOptions<U> {
/// #     type Widget = PromptLine<U>;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
/// #         todo!();
/// #     }
/// # }
/// # type StatusLine<U> = PromptLine<U>;
/// # fn test<U: Ui>() {
/// hook::add::<OnWindowOpen<U>>(|pa, builder| {
///     // Push a StatusLine to the bottom.
///     builder.push(pa, StatusLine::cfg());
///     // Push a PromptLine to the bottom.
///     builder.push(pa, PromptLine::cfg());
/// });
/// # }
/// ```
///
/// Contrast this with the example in the [`FileBuilder`] docs, where
/// a similar hook is added, but with [`OnFileOpen`] instead of
/// [`OnWindowOpen`]. In that scenario, the widgets are added to each
/// file that is opened, while in this one, only one instance of these
/// widgets will be created per window.
///
/// The existence of these two hooks lets the user make some more
/// advanced choices on the layout:
///
/// ```rust
/// # use duat_core::{hook::{OnFileOpen, OnWindowOpen}, prelude::*};
/// # struct PromptLine<U: Ui>(std::marker::PhantomData<U>);
/// # impl<U: Ui> Widget<U> for PromptLine<U> {
/// #     type Cfg = PromptLineOptions<U>;
/// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// #     fn cfg() -> Self::Cfg { todo!() }
/// #     fn text(&self) -> &Text { todo!(); }
/// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # struct PromptLineOptions<U>(std::marker::PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for PromptLineOptions<U> {
/// #     type Widget = PromptLine<U>;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
/// #         todo!();
/// #     }
/// # }
/// # type StatusLine<U> = PromptLine<U>;
/// # type LineNumbers<U> = PromptLine<U>;
/// # fn test<U: Ui>() {
/// hook::remove("FileWidgets");
/// hook::add::<OnFileOpen<U>>(|pa, builder| {
///     builder.push(pa, LineNumbers::cfg());
///     builder.push(pa, StatusLine::cfg());
/// });
///
/// hook::remove("WindowWidgets");
/// hook::add::<OnWindowOpen<U>>(|pa, builder| {
///     builder.push(pa, PromptLine::cfg());
/// });
/// # }
/// ```
///
/// In this case, each file gets a [`StatusLine`], and the window will
/// get one [`PromptLine`], after all, what is the point of having
/// more than one command line?
///
/// You can go further with this idea, like a status line on each
/// file, that shows different information from the status line for
/// the whole window, and so on and so forth.
///
/// [`File`]: crate::file::File
/// [`OnFileOpen`]: crate::hook::OnFileOpen
/// [`OnWindowOpen`]: crate::hook::OnWindowOpen
/// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
/// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
pub struct WindowBuilder<U: Ui> {
    window_i: usize,
    area: U::Area,
}

impl<U: Ui> WindowBuilder<U> {
    /// Creates a new [`WindowBuilder`].
    pub(crate) fn new(window_i: usize) -> Self {
        let windows = context::windows::<U>().borrow();
        let area = windows[window_i].files_area.clone();
        Self { window_i, area }
    }

    /// Pushes a [widget] to an edge of the window, given a [cfg]
    ///
    /// This widget will be pushed to the "main" area, i.e., the area
    /// that contains all other widgets. After that, the widget's
    /// parent will become the main area, which can be pushed onto
    /// again.
    ///
    /// This means that, if you push widget *A* to the right of the
    /// window, then you push widget *B* to the bottom of the window,
    /// and then you push widget *C* to the left of the window,you
    /// will end up with something like this:
    ///
    /// ```text
    /// ╭───┬───────────┬───╮
    /// │   │           │   │
    /// │   │ main area │ A │
    /// │ C │           │   │
    /// │   ├───────────┴───┤
    /// │   │       B       │
    /// ╰───┴───────────────╯
    /// ```
    ///
    /// This method returns the [`Area`] created for this widget, as
    /// well as an [`Area`] for the parent of the two widgets, if a
    /// new one was created.
    ///
    /// [widget]: Widget
    /// [cfg]: WidgetCfg
    /// [`Area`]: crate::ui::Ui::Area
    #[inline(never)]
    pub fn push<W: WidgetCfg<U>>(&mut self, pa: &mut Pass, cfg: W) -> (U::Area, Option<U::Area>) {
        run_once::<W::Widget, U>();
        let (widget, specs) = cfg.build(pa, None);

        let mut windows = context::windows().borrow_mut();
        let window = &mut windows[self.window_i];

        let (child, parent) = window.push(pa, widget, &self.area, specs, false, false);

        if let Some(parent) = &parent {
            self.area = parent.clone();
        }

        (child.area().clone(), parent)
    }

    /// Pushes a widget to a specific area
    ///
    /// Unlike [`push`], this method will push the widget to an area
    /// that is not the main area. This can be used to create more
    /// intricate layouts.
    ///
    /// For example, let's say I push a [`StatusLine`] below the main
    /// area, and then I push a [`PromptLine`] on the left of the
    /// status's area:
    ///
    /// ```rust
    /// # use duat_core::{hook::OnWindowOpen, prelude::*};
    /// # struct PromptLine<U: Ui>(std::marker::PhantomData<U>);
    /// # impl<U: Ui> Widget<U> for PromptLine<U> {
    /// #     type Cfg = PromptLineOptions<U>;
    /// #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
    /// #     fn needs_update(&self) -> bool { todo!(); }
    /// #     fn cfg() -> Self::Cfg { todo!() }
    /// #     fn text(&self) -> &Text { todo!(); }
    /// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
    /// #     fn once() -> Result<(), Text> { Ok(()) }
    /// # }
    /// # struct PromptLineOptions<U>(std::marker::PhantomData<U>);
    /// # impl<U> PromptLineOptions<U> {
    /// #     pub fn left_ratioed(self, _: usize, _: usize) -> Self { todo!(); }
    /// # }
    /// # impl<U: Ui> WidgetCfg<U> for PromptLineOptions<U> {
    /// #     type Widget = PromptLine<U>;
    /// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// # type StatusLine<U> = PromptLine<U>;
    /// # fn test<U: Ui>() {
    /// hook::add::<OnWindowOpen<U>>(|pa, builder| {
    ///     // StatusLine goes below by default
    ///     let (status_area, _) = builder.push(pa, StatusLine::cfg());
    ///     let prompt_cfg = PromptLine::cfg().left_ratioed(3, 5);
    ///     builder.push_to(pa, status_area, prompt_cfg);
    /// });
    /// # }
    /// ```
    ///
    /// The following would happen:
    ///
    /// ```text
    /// ╭────────────────────────────────────╮
    /// │                                    │
    /// │              main area             │
    /// │                                    │
    /// ├─────────────┬──────────────────────┤
    /// │ PromptLine  │      StatusLine      │
    /// ╰─────────────┴──────────────────────╯
    /// ```
    ///
    /// This is the layout that Kakoune uses by default, and is also
    /// avaliable as commented option in the default config.
    ///
    /// [`push`]: Self::push
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    /// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    #[inline(never)]
    pub fn push_to<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        area: U::Area,
        cfg: W,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W::Widget, U>();
        let (widget, specs) = cfg.build(pa, None);

        let mut windows = context::windows().borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(pa, widget, &area, specs, true, false);

        (node.area().clone(), parent)
    }
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: Widget::once
fn run_once<W: Widget<U>, U: Ui>() {
    static ONCE_LIST: Mutex<Vec<&'static str>> = Mutex::new(Vec::new());

    let mut once_list = ONCE_LIST.lock().unwrap();
    if !once_list.contains(&duat_name::<W>()) {
        W::once().unwrap();
        once_list.push(duat_name::<W>());
    }
}

/// [`Hookable`]: Triggers when a [`File`] is opened
///
/// # Arguments
///
/// - The file [builder], which can be used to push widgets to the
///   file, and to eachother.
///
/// [`File`]: crate::file::File
/// [builder]: FileBuilder
pub struct OnFileOpen<U: Ui>(FileBuilder<U>);

impl<U: Ui> Hookable for OnFileOpen<U> {
    type Input<'h> = &'h mut FileBuilder<U>;

    fn get_input<'h>(&'h mut self) -> Self::Input<'h> {
        &mut self.0
    }
}

/// [`Hookable`]: Triggers when a new window is opened
///
/// # Arguments
///
/// - The window [builder], which can be used to push widgets to the
///   edges of the window, surrounding the inner file region.
///
/// [builder]: WindowBuilder
pub struct OnWindowOpen<U: Ui>(WindowBuilder<U>);

impl<U: Ui> Hookable for OnWindowOpen<U> {
    type Input<'h> = &'h mut WindowBuilder<U>;

    fn get_input<'h>(&'h mut self) -> Self::Input<'h> {
        &mut self.0
    }
}
