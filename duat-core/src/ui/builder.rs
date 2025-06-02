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
//! [`File`]: crate::widgets::File
use std::sync::Mutex;

use super::{RawArea, Ui};
use crate::{
    context::{self, FileHandle},
    data::Pass,
    duat_name,
    file::{File, ReaderCfg},
    widgets::{Node, Widget, WidgetCfg},
};

/// A constructor helper for [`File`] initiations
///
/// This helper is used primarily to push widgets around the file in
/// question, and is only obtainable in a [`OnFileOpen`] hook:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen},
/// #     ui::{FileBuilder, Ui},
/// #     widgets::{LineNumbers, Widget},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnFileOpen<U>>(|builder: &mut FileBuilder<U>| {
///     builder.push(LineNumbers::cfg());
/// });
/// # }
/// ```
///
/// In the example above, I pushed a [`LineNumbers`] widget to the
/// [`File`]. By default, this widget will go on the left, but you can
/// change that:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen},
/// #     ui::{FileBuilder, Ui},
/// #     widgets::{LineNumbers, Widget},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnFileOpen<U>>(|builder: &mut FileBuilder<U>| {
///     let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///     builder.push(line_numbers_cfg);
/// });
/// # }
/// ```
///
/// Note that I also made another change to the widget, it will now
/// show [relative] numbers, instead of [absolute], like it usually
/// does.
///
/// By default, there already exists a [hook group] that adds widgets
/// to a file, the `"FileWidgets"` group. If you want to get rid of
/// this group in order to create your own widget layout, you should
/// use [`hooks::remove`]:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen},
/// #     ui::{FileBuilder, Ui},
/// #     widgets::{PromptLine, LineNumbers, Widget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder: &mut FileBuilder<U>| {
///     let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///     builder.push(line_numbers_cfg);
///     // Push a StatusLine to the bottom.
///     builder.push(StatusLine::cfg());
///     // Push a PromptLine to the bottom.
///     builder.push(PromptLine::cfg());
/// });
/// # }
/// ```
///
/// [`File`]: crate::widgets::File
/// [`OnFileOpen`]: crate::hooks::OnFileOpen
/// [`LineNumbers`]: crate::widgets::LineNumbers
/// [relative]: crate::widgets::LineNumbersOptions::relative
/// [absolute]: crate::widgets::LineNumbersOptions::absolute
/// [hook group]: crate::hooks::add_grouped
/// [`hooks::remove`]: crate::hooks::remove
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
    /// # use duat_core::{
    /// #     hooks::{self, OnFileOpen}, status::*,
    /// #     ui::{FileBuilder, Ui}, widgets::{File, LineNumbers, Widget, status},
    /// # };
    /// # fn test<U: Ui>() {
    /// hooks::remove("FileWidgets");
    /// hooks::add::<OnFileOpen<U>>(|builder: &mut FileBuilder<U>| {
    ///     let line_numbers_cfg = LineNumbers::cfg().rel_abs();
    ///     builder.push(line_numbers_cfg);
    ///
    ///     let status_line_cfg = status!(file_fmt " " selections_fmt " " main_fmt);
    ///     builder.push(status_line_cfg);
    /// });
    /// # }
    /// ```
    ///
    /// In this case, each file will have [`LineNumbers`] with
    /// [`relative/absolute`] numbering, and a [`StatusLine`] showing
    /// the file's name and how many selections are in it.
    ///
    /// [`File`]: crate::widgets::File
    /// [`LineNumbers`]: crate::widgets::LineNumbers
    /// [`relative/absolute`]: crate::widgets::LineNumbersOptions::rel_abs
    /// [`StatusLine`]: crate::widgets::StatusLine
    pub fn push<W: Widget<U>>(
        &mut self,
        pa: &mut Pass,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>();
        // SAFETY: Exclusive borrow of Pass means I can create my own.
        let (widget, specs) = cfg.build(unsafe { Pass::new() }, Some(self.handle.clone()));

        let mut windows = context::windows().borrow_mut();
        let window = &mut windows[self.window_i];

        let (child, parent) = {
            let (node, parent) = window.push(&mut *pa, widget, &self.area, specs, true, true);

            self.handle
                .write_related_widgets(&mut *pa, |related| related.push(node.clone()));

            if let Some(parent) = &parent
                && parent.is_master_of(&window.files_area)
            {
                window.files_area = parent.clone();
            }

            (node.area().clone(), parent)
        };

        if let Some(parent) = &parent {
            self.area = parent.clone();
        }

        (child, parent)
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
    /// # fn mode_fmt(file: &File) -> Text {
    /// #     todo!();
    /// # }
    /// # use duat_core::{
    /// #     hooks::{self, OnFileOpen}, text::Text, ui::{FileBuilder, Ui}, status::*,
    /// #     widgets::{PromptLine, File, LineNumbers, Notifier, Widget, status},
    /// # };
    /// # fn test<U: Ui>() {
    /// hooks::remove("FileWidgets");
    /// hooks::add::<OnFileOpen<U>>(|builder: &mut FileBuilder<U>| {
    ///     builder.push(LineNumbers::cfg());
    ///     
    ///     let (child, _) = builder.push(
    ///         status!(file_fmt " " selections_fmt " " main_fmt)
    ///     );
    ///     let (child, _) = builder.push_to(child, PromptLine::cfg().left_ratioed(3, 5));
    ///     builder.push_to(child, Notifier::cfg());
    /// });
    /// # }
    /// ```
    ///
    /// Pushing directly to the [`PromptLine`]'s [`Area`] means that
    /// they'll share a parent that holds only them. This can then be
    /// exploited by the `"HidePromptLine"` [hook group], which is
    /// defined as:
    ///
    /// ```rust
    /// # use duat_core::{hooks::{self, *}, widgets::PromptLine, ui::{Area, Constraint}};
    /// # fn test<Ui: duat_core::ui::Ui>() {
    /// hooks::add_grouped::<UnfocusedFrom<PromptLine<Ui>, Ui>>("HidePromptLine", |(_, area)| {
    ///     area.constrain_ver([Constraint::Len(0.0)]).unwrap();
    /// });
    /// hooks::add_grouped::<FocusedOn<PromptLine<Ui>, Ui>>("HidePromptLine", |(_, area)| {
    ///     area.constrain_ver([Constraint::Ratio(1, 1), Constraint::Len(1.0)])
    ///         .unwrap();
    /// });
    /// # }
    /// ```
    ///
    /// [`File`]: crate::widgets::File
    /// [`Notifier`]: crate::widgets::Notifier
    /// [`PromptLine`]: crate::widgets::PromptLine
    /// [hook group]: crate::hooks::add_grouped
    pub fn push_to<W: Widget<U>>(
        &mut self,
        pa: &mut Pass,
        area: U::Area,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>();
        // SAFETY: Exclusive borrow of Pass means I can create my own.
        let (widget, specs) = cfg.build(unsafe { Pass::new() }, Some(self.handle.clone()));

        let mut windows = context::windows().borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(&mut *pa, widget, &area, specs, true, true);
        self.handle
            .write_related_widgets(&mut *pa, |related| related.push(node.clone()));
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
    /// [`Reader`]: crate::text::Reader
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
/// # use duat_core::{
/// #     hooks::{self, OnWindowOpen},
/// #     ui::{Ui, WindowBuilder},
/// #     widgets::{PromptLine, Widget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnWindowOpen<U>>(|builder: &mut WindowBuilder<U>| {
///     // Push a StatusLine to the bottom.
///     builder.push(StatusLine::cfg());
///     // Push a PromptLine to the bottom.
///     builder.push(PromptLine::cfg());
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
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen, OnWindowOpen},
/// #     ui::{WindowBuilder, Ui},
/// #     widgets::{PromptLine, LineNumbers, Widget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder| {
///     builder.push(LineNumbers::cfg());
///     builder.push(StatusLine::cfg());
/// });
///
/// hooks::remove("WindowWidgets");
/// hooks::add::<OnWindowOpen<U>>(|builder| {
///     builder.push(PromptLine::cfg());
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
/// [`File`]: crate::widgets::File
/// [`OnFileOpen`]: crate::hooks::OnFileOpen
/// [`OnWindowOpen`]: crate::hooks::OnWindowOpen
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`PromptLine`]: crate::widgets::PromptLine
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
    pub fn push<W: Widget<U>>(
        &mut self,
        pa: &mut Pass,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>();
        // SAFETY: Exclusive borrow of Pass means I can create my own.
        let (widget, specs) = cfg.build(unsafe { Pass::new() }, None);

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
    /// # use duat_core::{
    /// #     ui::{Ui, WindowBuilder},
    /// #     widgets::{PromptLine, Widget, StatusLine},
    /// # };
    /// # fn test<U: Ui>(builder: &mut WindowBuilder<U>) {
    /// // StatusLine goes below by default
    /// let (status_area, _) = builder.push(StatusLine::cfg());
    /// let cmd_line_cfg = PromptLine::cfg().left_ratioed(3, 5);
    /// builder.push_to(status_area, cmd_line_cfg);
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
    /// This is the layout that Kakoune uses by default, and the
    /// default for Duat as well.
    ///
    /// [`push`]: Self::push
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`PromptLine`]: crate::widgets::PromptLine
    pub fn push_to<W: Widget<U>>(
        &mut self,
        pa: &mut Pass,
        area: U::Area,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>();
        // SAFETY: Exclusive borrow of Pass means I can create my own.
        let (widget, specs) = cfg.build(unsafe { Pass::new() }, None);

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
