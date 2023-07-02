use std::{
    fmt::Debug,
    sync::{atomic::AtomicUsize, RwLock}
};

use crate::{
    data::{RawReadableData, ReadableData, RoData, RwData},
    position::Pos,
    tags::form::FormPalette,
    text::{PrintCfg, Text, TextBit},
    widgets::{ActionableWidget, FileWidget, NormalWidget, Widget},
    Controler
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    Ratio(u16, u16),
    Percent(u16),
    Length(f64),
    Min(f64),
    Max(f64)
}

/// Information on how a [`Widget<U>`] should be pushed onto another.
///
/// The side member determines what direction to push into, in
/// relation to the original widget.
///
/// The [`Constraint`] can be one of five types:
///
/// - [`Min(min)`][Constraint::Min] represents the minimum length, in
///   the side's [`Axis`], that this new widget needs.
/// - [`Max(max)`][Constraint::Max] represents the minimum length, in
///   the side's [`Axis`], that this new widget needs.
/// - [`Length(len)`][Constraint::Length] represents a length, in the
///   side's [`Axis`], that cannot be altered by any means.
/// - [`Ratio(den, div)`][Constraint::Ratio] represents a ratio
///   between the length of the child and the length of the parent.
/// - [`Percent(per)`][Constraint::Percent] represents the percent of
///   the parent that the child must take. Must go from 0 to 100
///   percent.
///
/// So if, for example, if a widget is pushed with
/// [`PushSpecs::left(Constraint::Min(3.0)`][Self::left()]
///
/// into another widget, then it will be placed on the left side of
/// that widget, and will have a minimum `width` of `3`.
///
/// If it were pushed with either [`PushSpecs::above()`] or
/// [`PushSpecs::below()`], it would instead have a minimum `height`
/// of `3`.
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
pub trait PrintInfo: Default + Clone + Copy {
    type Area: Area;

    /// Scrolls the [`Text`] (up or down) until the main cursor is
    /// within the [`ScrollOff`][crate::text::ScrollOff] range.
    fn scroll_to_gap(&mut self, text: &Text, pos: Pos, area: &Self::Area, cfg: &PrintCfg);

    /// Returns the character index of the first character that would
    /// be printed.
    fn first_char(&self, text: &Text) -> usize;
}

/// An [`Area`] that supports printing [`Text`].
///
/// These represent the entire GUI of Parsec, the only parts of the
/// screen where text may be printed.
pub trait Area: Send + Sync {
    type AreaIndex: Clone + Copy;
    type PrintInfo: PrintInfo;

    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    fn set_as_active(&mut self);

    /// Prints the [`Text`][crate::text::Text] via an [`Iterator`].
    fn print(&mut self, text: &Text, info: Self::PrintInfo, cfg: PrintCfg, palette: &FormPalette);

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
    fn visible_rows(
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
    fn index(&self) -> Self::AreaIndex;
}

/// Elements related to the [`Widget<U>`]s.
struct Node<U>
where
    U: Ui
{
    widget: Widget<U>,
    index: U::AreaIndex,
    file_id: Option<usize>
}

/// A constructor helper for [`Widget<U>`]s.
///
/// When pushing [`Widget<U>`]s to the layout, this struct can be used
/// to further actions to be taken. It is used in contexts where a
/// widget has just been inserted to the screen, inside closures.
///
/// Here, [`LineNumbers<U>`][crate::widgets::LineNumbers<U>] is pushed
/// to the left of a widget (which in this case is a [`FileWidget<U>`]
///
/// ```rust
/// # use parsec_core::{
/// #     data::RoData,
/// #     ui::{ModNode, PushSpecs, Constraint, Ui},
/// #     widgets::{FileWidget, LineNumbers}
/// # };
/// fn file_fn<U>(
///     mut mod_node: ModNode<U>, file: RoData<FileWidget<U>>
/// ) where
///     U: Ui
/// {
///     let specs = PushSpecs::left(Constraint::Length(1.0));
///     mod_node.push_specd(LineNumbers::default_fn());
/// }
/// ```
///
/// By using the `file_fn()` function as the `constructor_hook`
/// argument for [`Session::new()`][crate::Session::new()], every file
/// that is opened will have a
/// [`LineNumbers<U>`][crate::widgets::LineNumbers] widget attached to
/// it.
pub struct ModNode<'a, U>
where
    U: Ui
{
    manager: &'a mut Controler<U>,
    is_file: bool,
    mod_area: RwLock<U::AreaIndex>
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
    pub fn push(
        &self, f: impl FnOnce(&Controler<U>) -> (Widget<U>, PushSpecs), specs: PushSpecs
    ) -> (U::AreaIndex, Option<U::AreaIndex>) {
        let (widget, _) = f(self.manager);
        let file_id = self.manager.commands.read().file_id;
        let (new_child, new_parent) = self.manager.windows.mutate(|windows| {
            let window = &mut windows[self.manager.active_window];
            let mod_area = self.mod_area.read().unwrap();
            let (new_child, new_parent) = window.push(*mod_area, widget, specs, file_id, true);

            if let (Some(new_parent), true) = (new_parent, self.is_file) {
                if window.window.is_senior(new_parent, window.files_node) {
                    window.files_node = new_parent;
                }
            }

            (new_child, new_parent)
        });

        let window = &self.manager.windows.read()[self.manager.active_window];
        let mut area = window.window.get_area(new_child).unwrap();
        let node = window.nodes.iter().find(|Node { index, .. }| *index == new_child);
        node.map(|Node { widget, .. }| widget).unwrap().update(&mut area);

        if let Some(new_parent) = new_parent {
            *self.mod_area.write().unwrap() = new_parent;
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
    pub fn push_to(
        &self, f: impl FnOnce(&Controler<U>) -> (Widget<U>, PushSpecs), index: U::AreaIndex,
        specs: PushSpecs
    ) -> (U::AreaIndex, Option<U::AreaIndex>) {
        let (widget, _) = f(self.manager);
        let file_id = self.manager.commands.read().file_id;
        let (new_area, pushed_area) = self.manager.windows.mutate(|windows| {
            let window = &mut windows[self.manager.active_window];
            window.push(index, widget, specs, file_id, true)
        });

        let window = &self.manager.windows.read()[self.manager.active_window];
        let mut area = window.window.get_area(new_area).unwrap();
        let node = window.nodes.iter().find(|Node { index, .. }| *index == new_area);
        node.map(|Node { widget, .. }| widget).unwrap().update(&mut area);

        (new_area, pushed_area)
    }

    pub fn push_specd(
        &self, f: impl FnOnce(&Controler<U>) -> (Widget<U>, PushSpecs)
    ) -> (U::AreaIndex, Option<U::AreaIndex>) {
        let (widget, specs) = (f)(self.manager);
        let file_id = self.manager.commands.read().file_id;
        let (new_child, new_parent) = self.manager.windows.mutate(|windows| {
            let window = &mut windows[self.manager.active_window];
            let mod_area = self.mod_area.read().unwrap();
            let (new_child, new_parent) = window.push(*mod_area, widget, specs, file_id, true);

            if let (Some(new_parent), true) = (new_parent, self.is_file) {
                if window.window.is_senior(new_parent, window.files_node) {
                    window.files_node = new_parent;
                }
            }

            (new_child, new_parent)
        });

        let window = &self.manager.windows.read()[self.manager.active_window];
        let mut area = window.window.get_area(new_child).unwrap();
        let node = window.nodes.iter().find(|Node { index, .. }| *index == new_child);
        node.map(|Node { widget, .. }| widget).unwrap().update(&mut area);

        if let Some(new_parent) = new_parent {
            *self.mod_area.write().unwrap() = new_parent;
        }

        (new_child, new_parent)
    }

    pub fn palette(&self) -> &FormPalette {
        &self.manager.palette
    }

    pub fn manager(&self) -> &Controler<U> {
        &self.manager
    }

    pub fn get_area(&self, index: U::AreaIndex) -> Option<U::Area> {
        let windows = self.manager.windows.read();
        windows[self.manager.active_window].window.get_area(index)
    }
}

pub(crate) fn activate_hook<U, Nw>(
    manager: &mut Controler<U>, mod_area: U::AreaIndex,
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

    if let Ok(file_widget) = widget.clone().try_downcast::<FileWidget<U>>() {
        *manager.active_file.write() = file_widget;
    }

    let mod_node = ModNode {
        manager,
        is_file: std::any::TypeId::of::<Nw>() == std::any::TypeId::of::<FileWidget<U>>(),
        mod_area: RwLock::new(mod_area)
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
pub trait Window: 'static + Send + Sync {
    type Area: Area;
    type AreaIndex: Clone + Copy;

    /// Gets the [`Area`][Window::Area] associated with a given
    /// `area_index`.
    ///
    /// If the [`Area`][Window::Area] in question is not a
    /// [`Area`][Window::Area], then returns [`None`].
    fn get_area(&self, area_index: Self::AreaIndex) -> Option<Self::Area>;

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
    fn bisect(
        &mut self, index: Self::AreaIndex, specs: PushSpecs, is_glued: bool
    ) -> (Self::AreaIndex, Option<Self::AreaIndex>);

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), ()>;

    fn is_senior(&self, senior: Self::AreaIndex, junior: Self::AreaIndex) -> bool;
}

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: 'static {
    type AreaIndex: Clone + Copy + PartialEq;
    type PrintInfo: PrintInfo<Area = Self::Area>;
    type Area: Area<AreaIndex = Self::AreaIndex, PrintInfo = Self::PrintInfo>;
    type Window: Window<AreaIndex = Self::AreaIndex, Area = Self::Area> + Clone + Send + Sync;

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
    files_node: U::AreaIndex,
    master_node: U::AreaIndex
}

impl<U> ParsecWindow<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of [`ParsecWindow<U>`].
    pub fn new(ui: &mut U, widget: Widget<U>) -> (Self, U::AreaIndex) {
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
            files_node: initial_area.index(),
            master_node: initial_area.index()
        };

        (parsec_window, initial_area.index())
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    pub fn push(
        &mut self, index: U::AreaIndex, widget: Widget<U>, specs: PushSpecs,
        file_id: Option<usize>, is_glued: bool
    ) -> (U::AreaIndex, Option<U::AreaIndex>) {
        let (new_child, new_parent) = self.window.bisect(index, specs, is_glued);

        let node = Node {
            widget,
            index: new_child,
            file_id
        };

        if index == self.master_node && let Some(new_master_node) = new_parent {
            self.master_node = new_master_node;
        }

        self.nodes.push(node);
        (new_child, new_parent)
    }

    /// Pushes a [`FileWidget<U>`] to the file's parent.
    ///
    /// This function will push to the edge of `self.files_parent`.
    /// This is an area, usually in the center, that contains all
    /// [`FileWidget<U>`]s, and their associated [`Widget<U>`]s,
    /// with others being at the perifery of this area.
    pub fn push_file(
        &mut self, widget: Widget<U>, specs: PushSpecs
    ) -> (U::AreaIndex, Option<U::AreaIndex>) {
        let index = self.files_node;
        let file_id = unique_file_id();

        let (file_area, new_parent) = self.push(index, widget, specs, Some(file_id), false);
        if let Some(new_parent) = new_parent {
            self.files_node = new_parent;
        }

        (file_area, new_parent)
    }

    /// Pushes a [`Widget<U>`] to the master node of the current
    /// window.
    pub fn push_to_master(
        &mut self, widget: Widget<U>, specs: PushSpecs
    ) -> (U::AreaIndex, Option<U::AreaIndex>) {
        self.push(self.master_node, widget, specs, None, false)
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
    ) -> impl Iterator<Item = (&RwData<dyn ActionableWidget<U>>, U::Area, Option<usize>)> + Clone + '_
    {
        self.nodes.iter().filter_map(|node| {
            node.widget.as_actionable().map(|widget| {
                let area = self.window.get_area(node.index).unwrap();
                (widget, area, node.file_id)
            })
        })
    }

    /// Returns an [`Iterator`] over the names of [`FileWidget<U>`]s
    /// and their respective [`ActionableWidget`] indices.
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

    pub fn try_inspect_nth<B>(&self, index: usize, f: impl FnOnce(RoWindow<U>) -> B) -> Option<B> {
        self.0
            .try_read()
            .map(|windows| windows.get(index).map(|window| f(RoWindow(window))))
            .ok()
            .flatten()
    }
}
