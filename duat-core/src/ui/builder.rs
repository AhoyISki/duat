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
use std::sync::Mutex;

use super::{RawArea, Ui};
use crate::{
    context::{self, FileHandle},
    data::Pass,
    duat_name,
    file::{File, ParserCfg},
    hook::{self, WidgetCreated},
    ui::{AreaId, Node, Widget, WidgetCfg},
};

/// A trait used to make [`Ui`] building generic
///
/// Its main implementors are [`FileBuilder`] and [`WindowBuilder`].
pub trait UiBuilder<U: Ui>: Sized {
    /// Pushes a [`Widget`] to this [`UiBuilder`], on its main
    /// [`U::Area`]
    ///
    /// [`U::Area`]: Ui::Area
    fn push_cfg<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>);

    /// Pushes a [`Widget`] to this [`UiBuilder`], on a given
    /// [`U::Area`]
    ///
    /// [`U::Area`]: Ui::Area
    fn push_cfg_to<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        area: AreaId<U>,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>);
}

impl<U: Ui> UiBuilder<U> for FileBuilder<U> {
    fn push_cfg<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        run_once::<W::Widget, U>();

        let cfg = {
            let wc = WidgetCreated::<W::Widget, U>((Some(cfg), Some(self.handle.clone())));
            hook::trigger(pa, wc).0.0.unwrap()
        };

        let (widget, specs) = cfg.build(pa, Some(self.handle.clone()));

        let mut windows = context::windows(pa).borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(pa, widget, &self.area, specs, true, true);

        self.get_areas(pa, window, node, parent)
    }

    fn push_cfg_to<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        area: AreaId<U>,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        run_once::<W::Widget, U>();

        let cfg = {
            let wc = WidgetCreated::<W::Widget, U>((Some(cfg), Some(self.handle.clone())));
            hook::trigger(pa, wc).0.0.unwrap()
        };

        let (widget, specs) = cfg.build(pa, Some(self.handle.clone()));

        let mut windows = context::windows(pa).borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(pa, widget, &area.0, specs, true, true);

        self.get_areas(pa, window, node, parent)
    }
}

/// A constructor helper for [`File`] initiations
///
/// This helper is used primarily to push widgets around the file in
/// question, and is only obtainable in a [`OnFileOpen`] hook:
///
/// ```rust
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<OnFileOpen>(|pa, builder| {
///         builder.push(pa, LineNumbers::cfg());
///     });
/// }
/// ```
///
/// In the example above, I pushed a [`LineNumbers`] widget to the
/// [`File`]. By default, this widget will go on the left, but you can
/// change that:
///
/// ```rust
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<OnFileOpen>(|pa, builder| {
///         let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///         builder.push(pa, line_numbers_cfg);
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
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("FileWidgets");
///     hook::add::<OnFileOpen>(|pa, builder| {
///         let line_numbers_cfg = LineNumbers::cfg().relative().on_the_right();
///         builder.push(pa, line_numbers_cfg);
///         // Push a StatusLine to the bottom.
///         builder.push(pa, StatusLine::cfg());
///         // Push a PromptLine to the bottom.
///         builder.push(pa, PromptLine::cfg());
///     });
/// }
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
    /// # use duat_core::doc_duat as duat;
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::remove("FileWidgets");
    ///     hook::add::<OnFileOpen>(|pa, builder| {
    ///         let line_numbers_cfg = LineNumbers::cfg().rel_abs();
    ///         builder.push(pa, line_numbers_cfg);
    ///
    ///         let status_line_cfg = status!("{file_fmt} {selections_fmt} {main_fmt}");
    ///         builder.push(pa, status_line_cfg);
    ///     });
    /// }
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
    pub fn push<W: WidgetAlias<U, impl BuilderDummy>>(
        &mut self,
        pa: &mut Pass,
        widget: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        widget.push(pa, self)
    }

    /// Pushes a widget to a specific area around a [`File`]
    ///
    /// This method can be used to get some more advanced layouts,
    /// where you have multiple widgets parallel to each other, yet on
    /// the same edge.
    ///
    /// One of the main ways in which this is used is when using a
    /// hidden [`Notifications`] widget alongside the [`PromptLine`]
    /// widget.
    ///
    /// ```rust
    /// # use duat_core::doc_duat as duat;
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::remove("FileWidgets");
    ///     hook::add::<OnFileOpen>(|pa, builder| {
    ///         builder.push(pa, LineNumbers::cfg());
    ///
    ///         let status_cfg = status!("{file_fmt} {selections_fmt} {main_fmt}");
    ///         let (child, _) = builder.push(pa, status_cfg);
    ///         let prompt_cfg = PromptLine::cfg().left_ratioed(3, 5);
    ///         let (child, _) = builder.push_to(pa, child, prompt_cfg);
    ///         builder.push_to(pa, child, Notifications::cfg());
    ///     });
    /// }
    /// ```
    ///
    /// Pushing directly to the [`PromptLine`]'s [`U::Area`] means
    /// that they'll share a parent that holds only them.
    ///
    /// [`File`]: crate::file::File
    /// [`Notifications`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.Notifications.html
    /// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    /// [`U::Area`]: Ui::Area
    /// [hook group]: crate::hook::add_grouped
    #[inline(never)]
    pub fn push_to<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        area: AreaId<U>,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        self.push_cfg_to(pa, area, cfg)
    }

    /// Adds a [`Parser`] to this [`File`]
    ///
    /// [`Parser`]s read changes to [`Text`] and can act accordingly
    /// by adding or removing [`Tag`]s from it. They can also be
    /// accessed via a public API, in order to be used for other
    /// things, like the treesitter [`Parser`], which, internally,
    /// creates the syntax tree and does syntax highlighting, but
    /// externally it can also be used for indentation of [`Text`] by
    /// [`Mode`]s
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Text`]: crate::text::Text
    /// [`Parser`]: crate::file::Parser
    /// [`Mode`]: crate::mode::Mode
    pub fn add_parser(&mut self, pa: &mut Pass, reader_cfg: impl ParserCfg<U>) {
        self.handle.add_parser(pa, reader_cfg);
    }

    /// The [`File`] that this hook is being applied to
    pub fn read<Ret>(&self, pa: &Pass, f: impl FnOnce(&File<U>, &U::Area) -> Ret) -> Ret {
        self.handle.read(pa, f)
    }

    /// Mutable reference to the [`File`] that this hooks is being
    /// applied to
    pub fn write<Ret>(&self, pa: &mut Pass, f: impl FnOnce(&mut File<U>, &U::Area) -> Ret) -> Ret {
        self.handle.write(pa, f)
    }

    fn get_areas(
        &mut self,
        pa: &mut Pass,
        window: &mut super::Window<U>,
        node: Node<U>,
        parent: Option<U::Area>,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        self.handle
            .write_related_widgets(pa, |related| related.push(node.clone()));

        if let Some(parent) = &parent {
            if parent.is_master_of(&window.files_area) {
                window.files_area = parent.clone();
            }
            self.area = parent.clone();
        }

        (AreaId(node.area().clone()), parent.map(AreaId))
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
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<OnWindowOpen>(|pa, builder| {
///         // Push a StatusLine to the bottom.
///         builder.push(pa, StatusLine::cfg());
///         // Push a PromptLine to the bottom.
///         builder.push(pa, PromptLine::cfg());
///     });
/// }
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
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("FileWidgets");
///     hook::add::<OnFileOpen>(|pa, builder| {
///         builder.push(pa, LineNumbers::cfg());
///         builder.push(pa, StatusLine::cfg());
///     });
///
///     hook::remove("WindowWidgets");
///     hook::add::<OnWindowOpen>(|pa, builder| {
///         builder.push(pa, PromptLine::cfg());
///     });
/// }
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

impl<U: Ui> UiBuilder<U> for WindowBuilder<U> {
    fn push_cfg<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        run_once::<W::Widget, U>();

        let cfg = {
            let wc = WidgetCreated::<W::Widget, U>((Some(cfg), None));
            hook::trigger(pa, wc).0.0.unwrap()
        };

        let (widget, specs) = cfg.build(pa, None);

        let mut windows = context::windows(pa).borrow_mut();
        let window = &mut windows[self.window_i];

        let (child, parent) = window.push(pa, widget, &self.area, specs, false, false);

        if let Some(parent) = &parent {
            self.area = parent.clone();
        }

        (AreaId(child.area().clone()), parent.map(AreaId))
    }

    fn push_cfg_to<W: WidgetCfg<U>>(
        &mut self,
        pa: &mut Pass,
        id: AreaId<U>,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        run_once::<W::Widget, U>();
        let (widget, specs) = cfg.build(pa, None);

        let mut windows = context::windows(pa).borrow_mut();
        let window = &mut windows[self.window_i];

        let (node, parent) = window.push(pa, widget, &id.0, specs, true, false);

        if *id.area(pa) == self.area
            && let Some(parent) = &parent
        {
            self.area = parent.clone();
        }

        (AreaId(node.area().clone()), parent.map(AreaId))
    }
}

impl<U: Ui> WindowBuilder<U> {
    /// Creates a new [`WindowBuilder`].
    pub(crate) fn new(pa: &Pass, window_i: usize) -> Self {
        let windows = context::windows::<U>(pa).borrow();
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
    pub fn push<W: WidgetAlias<U, impl BuilderDummy>>(
        &mut self,
        pa: &mut Pass,
        widget: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        widget.push(pa, self)
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
    /// # use duat_core::doc_duat as duat;
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::add::<OnWindowOpen>(|pa, builder| {
    ///         // StatusLine goes below by default
    ///         let (status_area, _) = builder.push(pa, StatusLine::cfg());
    ///         let prompt_cfg = PromptLine::cfg().left_ratioed(3, 5);
    ///         builder.push_to(pa, status_area, prompt_cfg);
    ///     });
    /// }
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
        area: AreaId<U>,
        cfg: W,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        self.push_cfg_to(pa, area, cfg)
    }
}

/// An alias to allow generic, yet consistent utilization of
/// [`UiBuilder`]s
///
/// This trait lets you do any available actions with [`UiBuilder`]s,
/// like pushing multiple [`Widget`]s, for example.
///
/// The reason for using this, rather than have your type just take a
/// [`UiBuilder`] parameter in its creation function, is mostly for
/// consistency's sake, since [`Ui`] building is done by pushing
/// [`Widget`]s into the builder, this lets you push multiple
/// [`Widget`]s, without breaking that same consistency.
pub trait WidgetAlias<U: Ui, D: BuilderDummy = WidgetCfgDummy> {
    /// "Pushes [`Widget`]s to a [`UiBuilder`], in a specific
    /// [`U::Area`]
    ///
    /// [`U::Area`]: Ui::Area
    fn push(self, pa: &mut Pass, builder: &mut impl UiBuilder<U>)
    -> (AreaId<U>, Option<AreaId<U>>);
}

impl<W: WidgetCfg<U>, U: Ui> WidgetAlias<U> for W {
    fn push(
        self,
        pa: &mut Pass,
        builder: &mut impl UiBuilder<U>,
    ) -> (AreaId<U>, Option<AreaId<U>>) {
        builder.push_cfg(pa, self)
    }
}

/// A dummy trait, meant for specialization
pub trait BuilderDummy {}

pub struct WidgetCfgDummy;

impl BuilderDummy for WidgetCfgDummy {}

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
