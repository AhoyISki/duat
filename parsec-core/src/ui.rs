use std::{
    fmt::Debug,
    sync::atomic::{AtomicUsize, Ordering}
};

use crate::{
    data::{RoData, RwData},
    log_info,
    position::Pos,
    tags::form::FormPalette,
    text::{PrintCfg, Text, TextBit},
    widgets::{file_widget::FileWidget, ActionableWidget, NormalWidget, Widget},
    Manager
};

fn unique_file_id() -> usize {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

/// A direction, where a [`Widget<U>`] will be placed in relation to
/// another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Side {
    Above,
    Right,
    Below,
    Left
}

#[derive(Debug, Clone, Copy)]
pub enum Constraint {
    Ratio(u16, u16),
    Percent(u16),
    Length(f64),
    Min(f64),
    Max(f64)
}

/// Information on how a [`Widget<U>`] should be pushed onto another.
///
/// The [`Side`] member determines what direction to push into, in
/// relation to the original widget.
///
/// The [`Split`] can be one of two types:
/// - [`Min(min_len)`][Split::Min] represents the minimum length, in
///   the [`Side`]'s [`Axis`], that this new widget needs.
/// - [`Locked(locked_len)`][Split::Locked] represents a length, in
///   the [`Side`]'s [`Axis`], that cannot be altered by any means.
///
/// So if, for example, if a widget is pushed with
///
/// ```rust
/// # use parsec_core::ui::{PushSpecs, Side, Split};
/// # fn test_fn() -> PushSpecs {
/// PushSpecs {
///     side: Side::Left,
///     split: Split::Min(3)
/// }
/// # }
/// ```
///
/// into another widget, then it will be placed on the left side of
/// that widget, and will have a minimum `width` of `3`.
///
/// If it were pushed to either [`Side::Top`] or [`Side::Bottom`], it
/// would instead have a minimum `height` of `3`.
#[derive(Debug, Clone, Copy)]
pub struct PushSpecs {
    side: Side,
    pub constraint: Option<Constraint>
}

impl PushSpecs {
    /// Returns a new instance of [`PushSpecs`].
    pub fn left(constraint: Constraint) -> Self {
        PushSpecs {
            side: Side::Left,
            constraint: Some(constraint)
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn right(constraint: Constraint) -> Self {
        PushSpecs {
            side: Side::Right,
            constraint: Some(constraint)
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn above(constraint: Constraint) -> Self {
        PushSpecs {
            side: Side::Above,
            constraint: Some(constraint)
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn below(constraint: Constraint) -> Self {
        PushSpecs {
            side: Side::Below,
            constraint: Some(constraint)
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn left_free() -> Self {
        PushSpecs {
            side: Side::Left,
            constraint: None
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn right_free() -> Self {
        PushSpecs {
            side: Side::Right,
            constraint: None
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn above_free() -> Self {
        PushSpecs {
            side: Side::Above,
            constraint: None
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn below_free() -> Self {
        PushSpecs {
            side: Side::Below,
            constraint: None
        }
    }

    pub fn comes_earlier(&self) -> bool {
        matches!(self.side, Side::Left | Side::Above)
    }
}

// TODO: Add a general scrolling function.
pub trait PrintInfo: Default {
    /// Scrolls the [`Text<U>`] (up or down) until the main cursor is
    /// within the [`ScrollOff`][crate::text::ScrollOff] range.
    fn scroll_to_gap<U>(&mut self, text: &Text<U>, pos: Pos, area: &U::Area, cfg: &PrintCfg)
    where
        U: Ui;

    /// Returns the character index of the first character that would
    /// be printed.
    fn first_char<U>(&self, text: &Text<U>) -> usize
    where
        U: Ui;
}

/// An [`Area`] that supports printing [`Text<U>`].
///
/// These represent the entire GUI of Parsec, the only parts of the
/// screen where text may be printed.
pub trait Area {
    type PrintInfo: PrintInfo + Clone + Copy;

    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    fn set_as_active(&mut self);

    /// Prints the [`Text`][crate::text::Text] via an [`Iterator`].
    fn print<U>(
        &mut self, text: &Text<U>, info: Self::PrintInfo, cfg: PrintCfg, palette: &FormPalette
    ) where
        U: Ui + ?Sized;

    fn change_constraint(&mut self, constraint: Constraint) -> Result<(), ()>;

    //////////////////// Queries
    /// The amount of rows of the screen that the [`Iterator`] takes
    /// up.
    ///
    /// Must take the [`PrintCfg`] into account, as in, if the
    /// [`WrapMethod`][crate::text::WrapMethod] is
    /// [`NoWrap`][crate::text::WrapMethod::NoWrap],
    /// then the number of rows must equal the number of lines on the
    /// [`Iterator`].
    fn vis_rows(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, max_index: usize
    ) -> usize;

    /// Returns the positional index of the char that comes after the
    /// [`TextBit`][crate::text::TextBit] [`Iterator`] wraps `wrap`
    /// times.
    fn char_at_wrap(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> Option<usize>;

    /// Gets the visual width of the [`Iterator`].
    fn get_width(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, max_index: usize,
        wrap_around: bool
    ) -> usize;

    /// Gets the column at `dist` from the left side on [`Iterator`].
    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize;

    /// A unique identifier to this [`Area`].
    fn index(&self) -> usize;
}

/// Elements related to the [`Widget<U>`]s.
struct Node<U>
where
    U: Ui
{
    widget: Widget<U>,
    index: usize,
    file_id: Option<usize>
}

/// A constructor helper for [`Widget<U>`]s.
///
/// When pushing [`Widget`]s to the layout, this struct can be used to
/// further actions to be taken. It is used in contexts where a widget
/// has just been inserted to the screen, inside closures.
///
/// Here, [`LineNumbers<U>`][crate::widgets::LineNumbers<U>] is pushed
/// to the left of a widget (which in this case is a [`FileWidget<U>`]
///
/// ```rust
/// # use parsec_core::{
/// #     data::RoData,
/// #     ui::{ModNode, PushSpecs, Side, Split, Ui},
/// #     widgets::{FileWidget, LineNumbers}
/// # };
/// fn file_fn<U>(
///     mut mod_node: ModNode<U>, file: RoData<FileWidget<U>>
/// ) where
///     U: Ui
/// {
///     let specs = PushSpecs::new(Side::Left, Split::Locked(1));
///     mod_node.push_widget(LineNumbers::default_fn(file), specs);
/// }
/// ```
///
/// By using the `file_fn()` function as the `constructor_hook`
/// argument for [`Session::new()`][crate::Session::new()], every file
/// that is opened will have a
/// [`LineNumbers<U>`][crate::widgets::LineNumbers] widget attached to
/// the [`Left`][Side::Left] side of the [`FileWidget<U>`].
pub struct ModNode<'a, U>
where
    U: Ui
{
    manager: &'a mut Manager<U>,
    mod_area: AtomicUsize
}

impl<'a, U> ModNode<'a, U>
where
    U: Ui + 'static
{
    /// Pushes a [`Widget<U>`] to [`self`], given [`PushSpecs`] and a
    /// constructor function.
    ///
    /// Do note that this function will should change the index of
    /// [`self`], such that subsequent pushes are targeted at the
    /// parent.
    ///
    /// # Returns
    ///
    /// The first element is the `area_index` of the newly created
    /// [`Widget<U>`], you can use it to push new [`Widget<U>`]s.
    /// The second element, of type [`Option<usize>`] is
    /// [`Some(..)`] only when a new parent was created to
    /// accomodate the new [`Widget<U>`], and represents the new
    /// `area_index` of the old [`Widget<U>`], which has now
    /// become a child.
    ///
    /// # Examples
    ///
    /// Pushing on [`Side::Left`], when [`self`] has an index of `0`:
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │                 │     │╭──2───╮╭───1───╮│
    /// │                 │ --> ││      ││       ││
    /// │                 │     ││      ││       ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// So a subsequent use of [`push_widget`][Self::push_widget] on
    /// [`Side::Bottom`] would push to the bottom of "both 1 and 2":
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> │╰──────╯╰───────╯│
    /// ││      ││       ││     │╭───────3───────╮│
    /// │╰──────╯╰───────╯│     │╰───────────────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// If you wish to, for example, push on [`Side::Bottom`] of `1`,
    /// checkout [`push_widget_to_area`][Self::push_widget_to_area].
    pub fn push_widget(
        &self, constructor: impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U>, specs: PushSpecs
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(self.manager, specs);
        let context = self.manager.commands.read().file_id;
        let (new_child, new_parent) = self.manager.windows.mutate(|windows| {
            let window = &mut windows[self.manager.active_window];
            window.push_widget(self.mod_area.load(Ordering::Relaxed), widget, specs, context)
        });

        let window = &self.manager.windows.read()[self.manager.active_window];
        let mut area = window.window.get_area(new_child).unwrap();
        let node = window.nodes.iter().find(|Node { index, .. }| *index == new_child);
        node.map(|Node { widget, .. }| widget).unwrap().update(&mut area);

        if let Some(new_parent) = new_parent {
            self.mod_area.store(new_parent, Ordering::Relaxed);
        }

        (new_child, new_parent)
    }

    /// Pushes a [`Widget<U>`] to a specific `area`, given
    /// [`PushSpecs`] and a constructor function.
    ///
    /// # Examples
    ///
    /// Given that [`self`] has an index of `0`, and other widgets
    /// have already been pushed, one can push to a specific
    /// [`Widget<U>`], given an area index.
    ///
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> ││      │╰───────╯│
    /// ││      ││       ││     ││      │╭───3───╮│
    /// │╰──────╯╰───────╯│     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    pub fn push_widget_to_area(
        &self, constructor: impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U>, index: usize,
        specs: PushSpecs
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(self.manager, specs);
        let context = self.manager.commands.read().file_id;
        let (new_area, pushed_area) = self.manager.windows.mutate(|windows| {
            let window = &mut windows[self.manager.active_window];
            window.push_widget(index, widget, specs, context)
        });

        let window = &self.manager.windows.read()[self.manager.active_window];
        let mut area = window.window.get_area(new_area).unwrap();
        let node = window.nodes.iter().find(|Node { index, .. }| *index == new_area);
        node.map(|Node { widget, .. }| widget).unwrap().update(&mut area);

        (new_area, pushed_area)
    }

    pub fn palette(&self) -> &FormPalette {
        &self.manager.palette
    }

    pub fn manager(&self) -> &Manager<U> {
        &self.manager
    }
}

pub(crate) fn activate_hook<U, Nw>(
    manager: &mut Manager<U>, mod_area: usize,
    constructor_hook: &mut dyn FnMut(ModNode<U>, RoData<Nw>)
) where
    U: Ui,
    Nw: NormalWidget<U>
{
    let (widget, file_id) = manager.windows.inspect(|windows| {
        let window = &windows[manager.active_window];

        let node = window.nodes.iter().find(|Node { index, .. }| *index == mod_area).unwrap();

        let widget = node.widget.try_downcast::<Nw>().unwrap_or_else(|| {
            panic!("The widget in question is not of type {}", std::any::type_name::<Nw>())
        });

        (widget, node.file_id)
    });
    let old_file_id = file_id
        .map(|file_id| manager.commands.write().file_id.replace(file_id))
        .flatten();

    let mod_node = ModNode {
        manager,
        mod_area: AtomicUsize::from(mod_area)
    };
    (constructor_hook)(mod_node, widget);

    manager.commands.write().file_id = old_file_id;
}

/// A dimension on screen, can either be horizontal or vertical.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    Horizontal,
    Vertical
}

impl Axis {
    pub fn perp(&self) -> Self {
        match self {
            Axis::Horizontal => Axis::Vertical,
            Axis::Vertical => Axis::Horizontal
        }
    }
}

impl From<PushSpecs> for Axis {
    fn from(value: PushSpecs) -> Self {
        if let Side::Above | Side::Below = value.side {
            Axis::Vertical
        } else {
            Axis::Horizontal
        }
    }
}

/// An abstract representation of a "viewport" of Parsec.
///
/// Only one [`Window`] may be shown at a time, and they contain all
/// [`Widget<U>`]s that should be displayed, both static and floating.
pub trait Window: 'static {
    type Area: Area + Send + Sync;

    /// Gets the [`Area`][Window::Area] associated with a given
    /// `area_index`.
    ///
    /// If the [`Area`][Window::Area] in question is not a
    /// [`Area`][Window::Area], then returns [`None`].
    fn get_area(&self, area_index: usize) -> Option<Self::Area>;

    /// Wether or not the layout of the `Ui` (size of widgets, their
    /// positions, etc) has changed.
    fn layout_has_changed(&self) -> bool;

    /// Bisects the [`Area`][Window::Area] with the given index into
    /// two.
    ///
    /// Will return 2 indices, the first one is the index of a new
    /// area. The second is an index for a newly created parent
    ///
    /// As an example, assuming that [`self`] has an index of `0`,
    /// pushing an area to [`self`] on [`Side::Left`] would create
    /// 2 new areas:
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │                 │     │╭──2───╮╭───1───╮│
    /// │      self       │ --> ││      ││ self  ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// This means that `0` is now the index of the newly created
    /// parent, `2` is the index of the new area, and `1` is the new
    /// index of the initial area. In the end, [`Window::bisect()`]
    /// should return `(2, Some(1))`.
    ///
    /// That doesn't always happen though. For example, pushing
    /// another area to the [`Side::Right`] of `1`, `2`, or `0`, in
    /// this situation, should not result in the creation of a new
    /// parent:
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭─2─╮╭──1──╮╭─3─╮│
    /// ││      ││ self  ││     ││   ││self ││   ││
    /// │╰──────╯╰───────╯│     │╰───╯╰─────╯╰───╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// And so [`Window::bisect()`] should return `(3, None)`.
    fn bisect(&mut self, index: usize, specs: PushSpecs, is_glued: bool) -> (usize, Option<usize>);

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), ()>;
}

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: 'static {
    type PrintInfo: PrintInfo + Clone + Copy;
    type Area: Area<PrintInfo = Self::PrintInfo> + Send + Sync;
    type Window: Window<Area = Self::Area> + Clone + Send + Sync;

    /// Initiates and returns a new [`Window`][Ui::Window].
    ///
    /// Also returns the newly created [`Area`][Ui::Area] of that
    /// [`Window`][Ui::Window].
    fn new_window(&mut self) -> (Self::Window, Self::Area);

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);
}

/// A container for a [`Window`] in Parsec.
pub struct ParsecWindow<U>
where
    U: Ui
{
    window: U::Window,
    nodes: Vec<Node<U>>,
    files_parent: usize
}

impl<U> ParsecWindow<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of [`ParsecWindow<U>`].
    pub fn new(ui: &mut U, widget: Widget<U>) -> Self {
        let (window, mut initial_area) = ui.new_window();
        widget.update(&mut initial_area);

        let main_node = Node {
            widget,
            index: initial_area.index(),
            file_id: Some(unique_file_id())
        };
        let parsec_window = ParsecWindow {
            window,
            nodes: vec![main_node],
            files_parent: 0
        };

        parsec_window
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    fn push_widget(
        &mut self, index: usize, widget: Widget<U>, specs: PushSpecs, file_id: Option<usize>
    ) -> (usize, Option<usize>) {
        self.inner_push_widget(index, widget, specs, file_id)
    }

    fn inner_push_widget(
        &mut self, index: usize, widget: Widget<U>, specs: PushSpecs, file_id: Option<usize>
    ) -> (usize, Option<usize>) {
        let is_file = widget.raw_inspect(|widget| widget.as_any().is::<FileWidget<U>>());
        let is_glued = file_id.is_some() && !is_file;
        let (new_child, new_parent) = self.window.bisect(index, specs, is_glued);

        let node = Node {
            widget,
            index: new_child,
            file_id
        };

        self.nodes.push(node);
        (new_child, new_parent)
    }

    /// Pushes a [`FileWidget<U>`] to another, and then activates a
    /// special hook.
    ///
    /// This function will push to the edge of `self.files_parent`.
    /// This is an area, usually in the center, that contains all
    /// [`FileWidget<U>`]s, and their associated [`Widget<U>`]s,
    /// with others being at the perifery of this area.
    pub fn push_file(&mut self, widget: Widget<U>, specs: PushSpecs) -> (usize, Option<usize>) {
        let index = self.files_parent;
        let file_id = unique_file_id();

        let (file_area, new_parent) = self.push_widget(index, widget, specs, Some(file_id));
        if let Some(new_parent) = new_parent {
            self.files_parent = new_parent;
        }

        (file_area, new_parent)
    }

    /// Pushes a [`Widget<U>`] to the master node of the current
    /// window.
    pub fn push_to_master(
        &mut self, widget: Widget<U>, specs: PushSpecs
    ) -> (usize, Option<usize>) {
        self.push_widget(0, widget, specs, None)
    }

    /// Returns an [`Iterator`] over the [`Widget<U>`]s of [`self`].
    pub fn widgets(&self) -> impl Iterator<Item = (&Widget<U>, U::Area, Option<usize>)> + '_ {
        self.nodes.iter().map(|node| {
            let area = self.window.get_area(node.index).unwrap();
            (&node.widget, area, node.file_id)
        })
    }

    /// Returns an [`Iterator`] over the [`ActionableWidget`]s of
    /// [`self`].
    pub(crate) fn actionable_widgets(
        &self
    ) -> impl Iterator<Item = (&RwData<dyn ActionableWidget<U>>, U::Area, Option<usize>)> + '_ {
        self.nodes.iter().filter_map(|node| {
            node.widget.as_actionable().map(|widget| {
                let area = self.window.get_area(node.index).unwrap();
                (widget, area, node.file_id)
            })
        })
    }

    /// Returns an [`Iterator`] over the file names of open
    /// [`FileWidget<U>`]s.
    pub fn file_names(&self) -> impl Iterator<Item = (usize, String)> + Clone + '_ {
        self.nodes
            .iter()
            .filter_map(|Node { widget, .. }| widget.as_actionable())
            .enumerate()
            .filter_map(|(index, widget)| {
                widget
                    .read()
                    .as_any()
                    .downcast_ref::<FileWidget<U>>()
                    .map(|file_widget| (index, file_widget.name().to_string()))
            })
    }

    /// if the layout of [`Window<U>`] has changed (areas resized, new
    /// areas opened, etc), updates and prints all [`Widget<U>`]s on
    /// the [`Window`].
    pub(crate) fn print_if_layout_changed(&self, palette: &FormPalette) {
        if self.window.layout_has_changed() {
            for node in &self.nodes {
                let mut area = self.window.get_area(node.index).unwrap();
                node.widget.update(&mut area);
                node.widget.print(&mut area, &palette);
            }
        }
    }
}

pub struct RoWindow<'a, U>(&'a ParsecWindow<U>)
where
    U: Ui;

impl<'a, U> RoWindow<'a, U>
where
    U: Ui
{
    /// Similar to the [`Iterator::fold`] operation, folding each
    /// [`&FileWidget<U>`][FileWidget`] by applying an operation,
    /// returning a final result.
    ///
    /// The reason why this is a `fold` operation, and doesn't just
    /// return an [`Iterator`], is because `f` will act on a
    /// reference, as to not do unnecessary cloning of the widget's
    /// inner [`RwData<W>`], and because [`Iterator`]s cannot return
    /// references to themselves.
    pub fn fold_files<B>(&self, init: B, mut f: impl FnMut(B, &FileWidget<U>) -> B) -> B {
        self.0
            .nodes
            .iter()
            .filter_map(|Node { widget, .. }| widget.as_actionable())
            .fold(init, |accum, widget| {
                if let Some(file_widget) =
                    widget.raw_read().as_any().downcast_ref::<FileWidget<U>>()
                {
                    f(accum, file_widget)
                } else {
                    accum
                }
            })
    }

    /// Similar to the [`Iterator::fold`] operation, folding each
    /// [`&dyn NormalWidget<U>`][NormalWidget] by applying an
    /// operation, returning a final result.
    ///
    /// The reason why this is a `fold` operation, and doesn't just
    /// return an [`Iterator`], is because `f` will act on a
    /// reference, as to not do unnecessary cloning of the widget's
    /// inner [`RwData<W>`], and because [`Iterator`]s cannot return
    /// references to themselves.
    pub fn fold_widgets<B>(&self, init: B, mut f: impl FnMut(B, &dyn NormalWidget<U>) -> B) -> B {
        self.0.nodes.iter().fold(init, |accum, Node { widget, .. }| {
            let f = &mut f;
            widget.raw_inspect(move |widget| f(accum, widget))
        })
    }
}

pub struct RoWindows<U>(RoData<Vec<ParsecWindow<U>>>)
where
    U: Ui;

impl<U> RoWindows<U>
where
    U: Ui
{
    pub fn new(windows: RoData<Vec<ParsecWindow<U>>>) -> Self {
        RoWindows(windows)
    }

    pub fn inspect_nth<B>(&self, index: usize, f: impl FnOnce(RoWindow<U>) -> B) -> Option<B> {
        let windows = self.0.read();
        windows.get(index).map(|window| f(RoWindow(window)))
    }
}
