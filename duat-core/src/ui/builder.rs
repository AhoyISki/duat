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
use std::{cell::RefCell, sync::LazyLock};

use parking_lot::RwLock;

use super::{Area, Ui, Window};
use crate::{
    data::{Context, RwData},
    duat_name,
    widgets::{PassiveWidget, WidgetCfg},
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
/// #     widgets::{LineNumbers, PassiveWidget},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnFileOpen<U>>(|builder: &FileBuilder<U>| {
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
/// #     widgets::{LineNumbers, PassiveWidget},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnFileOpen<U>>(|builder: &FileBuilder<U>| {
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
/// use [`hooks::remove_group`]:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen},
/// #     ui::{FileBuilder, Ui},
/// #     widgets::{CommandLine, LineNumbers, PassiveWidget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove_group("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder: &FileBuilder<U>| {
///     let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///     builder.push(line_numbers_cfg);
///     // Push a StatusLine to the bottom.
///     builder.push(StatusLine::cfg());
///     // Push a CommandLine to the bottom.
///     builder.push(CommandLine::cfg());
/// });
/// # }
/// ```
///
/// [`File`]: crate::widgets::File
/// [`OnFileOpen`]: crate::hooks::OnFileOpen
/// [`LineNumbers`]: crate::widgets::LineNumbers
/// [relative]: crate::widgets::LineNumbersCfg::relative
/// [absolute]: crate::widgets::LineNumbersCfg::absolute
/// [hook group]: crate::hooks::add_grouped
/// [`hooks::remove_group`]: crate::hooks::remove_group
pub struct FileBuilder<U>
where
    U: Ui,
{
    windows: &'static RwData<Vec<Window<U>>>,
    window_i: usize,
    mod_area: RwLock<U::Area>,
    context: Context<U>,
}

impl<U> FileBuilder<U>
where
    U: Ui,
{
    /// Creates a new [`FileBuilder`].
    pub(crate) fn new(
        windows: &'static RwData<Vec<Window<U>>>,
        mod_area: U::Area,
        window_i: usize,
        context: Context<U>,
    ) -> Self {
        Self {
            windows,
            window_i,
            mod_area: RwLock::new(mod_area),
            context,
        }
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
    /// #     hooks::{self, OnFileOpen},
    /// #     ui::{FileBuilder, Ui},
    /// #     widgets::{File, LineNumbers, PassiveWidget, common::selections_fmt, status},
    /// # };
    /// # fn test<U: Ui>() {
    /// hooks::remove_group("FileWidgets");
    /// hooks::add::<OnFileOpen<U>>(|builder: &FileBuilder<U>| {
    ///     let line_numbers_cfg = LineNumbers::cfg().rel_abs();
    ///     builder.push(line_numbers_cfg);
    ///
    ///     let status_line_cfg = status!({ File::name } " " selections_fmt);
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
    /// [`relative/absolute`]: crate::widgets::LineNumbersCfg::rel_abs
    /// [`StatusLine`]: crate::widgets::StatusLine
    pub fn push<W: PassiveWidget<U>>(
        &self,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, true);

        let mut windows = self.windows.write();
        let mut mod_area = self.mod_area.write();
        let window = &mut windows[self.window_i];

        let related = widget.as_passive().clone();

        let (child, parent) = {
            let (child, parent) = window.push(widget, &*mod_area, checker, specs, true);

            self.context.cur_file().unwrap().add_related_widget((
                related,
                child.clone(),
                duat_name::<W>(),
            ));

            if let Some(parent) = &parent {
                if parent.is_master_of(&window.files_area) {
                    window.files_area = parent.clone();
                }
            }

            (child, parent)
        };

        if let Some(parent) = &parent {
            *mod_area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a widget to a specific area around a [`File`]
    ///
    /// This method can be used to get some more advanced layouts,
    /// where you have multiple widgets paralel to each other, yet on
    /// the same edge.
    ///
    /// One example of where you might want to do this is if you want
    /// to have multiple [`StatusLine`]s in a single [`File`], each
    /// showing their own distinct information:
    ///
    /// ```rust
    /// # fn mode_fmt(file: &File) -> Text {
    /// #     todo!();
    /// # }
    /// # use duat_core::{
    /// #     hooks::{self, OnFileOpen},
    /// #     text::Text,
    /// #     ui::{FileBuilder, Ui},
    /// #     widgets::{
    /// #         CommandLine, File, LineNumbers, PassiveWidget,
    /// #         common::{selections_fmt, main_fmt}, status
    /// #     },
    /// # };
    /// # fn test<U: Ui>() {
    /// hooks::remove_group("FileWidgets");
    /// hooks::add::<OnFileOpen<U>>(|builder: &FileBuilder<U>| {
    ///     builder.push(LineNumbers::cfg());
    ///     
    ///     let right_status = status!(
    ///         { File::name } " " selections_fmt " " main_fmt
    ///     );
    ///     let (right_status_area, _) = builder.push(right_status);
    ///
    ///     let left_status = status!(mode_fmt).push_left();
    ///     builder.push_to(left_status, right_status_area);
    ///
    ///     builder.push(CommandLine::cfg());
    /// });
    /// # }
    /// ```
    ///
    /// [`File`]: crate::widgets::File
    /// [`StatusLine`]: crate::widgets::StatusLine
    pub fn push_to<W: PassiveWidget<U>>(
        &self,
        cfg: impl WidgetCfg<U, Widget = W>,
        area: U::Area,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, true);

        let mut windows = self.windows.write();
        let window = &mut windows[self.window_i];

        let related = widget.as_passive().clone();

        let (child, parent) = window.push(widget, &area, checker, specs, true);
        self.context.cur_file().unwrap().add_related_widget((
            related,
            child.clone(),
            duat_name::<W>(),
        ));
        (child, parent)
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
/// #     widgets::{CommandLine, PassiveWidget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnWindowOpen<U>>(|builder: &WindowBuilder<U>| {
///     // Push a StatusLine to the bottom.
///     builder.push(StatusLine::cfg());
///     // Push a CommandLine to the bottom.
///     builder.push(CommandLine::cfg());
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
/// The existance of these two hooks lets the user make some more
/// advanced choices on the layout:
///
/// ```rust
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen, OnWindowOpen},
/// #     ui::{WindowBuilder, Ui},
/// #     widgets::{CommandLine, LineNumbers, PassiveWidget, StatusLine},
/// # };
/// # fn test<U: Ui>() {
/// hooks::remove_group("FileWidgets");
/// hooks::add::<OnFileOpen<U>>(|builder| {
///     builder.push(LineNumbers::cfg());
///     builder.push(StatusLine::cfg());
/// });
///
/// hooks::remove_group("WindowWidgets");
/// hooks::add::<OnWindowOpen<U>>(|builder| {
///     builder.push(CommandLine::cfg());
/// });
/// # }
/// ```
///
/// In this case, each file gets a [`StatusLine`], and the window will
/// get one [`CommandLine`], after all, what is the point of having
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
/// [`CommandLine`]: crate::widgets::CommandLine
pub struct WindowBuilder<U>
where
    U: Ui,
{
    windows: &'static RwData<Vec<Window<U>>>,
    window_i: usize,
    mod_area: RefCell<U::Area>,
    context: Context<U>,
}

impl<U> WindowBuilder<U>
where
    U: Ui,
{
    /// Creates a new [`WindowBuilder`].
    pub(crate) fn new(
        windows: &'static RwData<Vec<Window<U>>>,
        window_i: usize,
        context: Context<U>,
    ) -> Self {
        let mod_area = windows.read()[window_i].files_area.clone();
        Self {
            windows,
            window_i,
            mod_area: RefCell::new(mod_area),
            context,
        }
    }

    /// Pushes a [widget] to an edge of the window, given a [cfg]
    ///
    /// This widget will be pushed to the "main" area, i.e., the area
    /// that contains all other widgets. After that, the widget's
    /// parent will become the main area, which can be [pushed] onto
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
    /// [widget]: PassiveWidget
    /// [cfg]: WidgetCfg
    pub fn push<W: PassiveWidget<U>>(
        &self,
        cfg: impl WidgetCfg<U, Widget = W>,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, false);

        let mut windows = self.windows.write();
        let mut mod_area = self.mod_area.borrow_mut();
        let window = &mut windows[self.window_i];

        let (child, parent) = window.push(widget, &*mod_area, checker, specs, false);

        if let Some(parent) = &parent {
            *mod_area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a widget to a specific area
    ///
    /// Unlike [`push`], this method will push the widget to an area
    /// that is not the main area. This can be used to create more
    /// intricate layouts.
    ///
    /// For example, let's say I push a [`StatusLine`] below the main
    /// area, and then I push a [`CommandLine`] on the left of the
    /// status's area:
    ///
    /// ```rust
    /// # use duat_core::{
    /// #     ui::{Ui, WindowBuilder},
    /// #     widgets::{CommandLine, PassiveWidget, StatusLine},
    /// # };
    /// # fn test<U: Ui>(builder: &WindowBuilder<U>) {
    /// // StatusLine goes below by default
    /// let (status_area, _) = builder.push(StatusLine::cfg());
    /// let cmd_line_cfg = CommandLine::cfg().left_ratioed(3, 5);
    /// builder.push_to(cmd_line_cfg, status_area);
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
    /// │ CommandLine │      StatusLine      │
    /// ╰─────────────┴──────────────────────╯
    /// ```
    ///
    /// This is the layout that Kakoune uses by default, and the
    /// default for Duat as well.
    ///
    /// [`push`]: Self::push
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`CommandLine`]: crate::widgets::CommandLine
    pub fn push_to<W: PassiveWidget<U>>(
        &self,
        cfg: impl WidgetCfg<U, Widget = W>,
        area: U::Area,
    ) -> (U::Area, Option<U::Area>) {
        run_once::<W, U>(self.context);
        let (widget, checker, specs) = cfg.build(self.context, false);

        let mut windows = self.windows.write();
        let window = &mut windows[self.window_i];

        window.push(widget, &area, checker, specs, true)
    }
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: PassiveWidget::once
fn run_once<W: PassiveWidget<U>, U: Ui>(context: Context<U>) {
    static ONCE_LIST: LazyLock<RwData<Vec<&'static str>>> =
        LazyLock::new(|| RwData::new(Vec::new()));

    let mut once_list = ONCE_LIST.write();
    if !once_list.contains(&duat_name::<W>()) {
        W::once(context);
        once_list.push(duat_name::<W>());
    }
}
