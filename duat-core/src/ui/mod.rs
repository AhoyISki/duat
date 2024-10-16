mod builder;
mod layout;

use std::{
    fmt::Debug,
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
};

use crossterm::event::KeyEvent;
use layout::iter_files_for_layout;
use serde::{Deserialize, Serialize};

pub use self::{
    builder::{FileBuilder, WindowBuilder},
    layout::{FileId, Layout, MasterOnLeft},
};
use crate::{
    DuatError,
    cache::load_cache,
    context,
    data::{RoData, RwData},
    forms::Painter,
    hooks::{self, OnFileOpen},
    text::{Item, Iter, IterCfg, Point, PrintCfg, RevIter, Text},
    widgets::{File, PassiveWidget, Widget},
};

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: Sized + Send + Sync + 'static {
    /// This is the underlying type that will be handled dynamically
    type StaticFns: Default + Clone + Copy + Send + Sync;
    type Area: Area<Ui = Self> + Clone + PartialEq;

    fn new(statics: Self::StaticFns) -> Self;

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    fn new_root(&mut self, cache: <Self::Area as Area>::Cache) -> Self::Area;

    /// Functions to trigger when the program begins
    fn open(&mut self);

    /// Starts the Ui
    ///
    /// This is different from [`Ui::open`], as this is going to run
    /// on reloads as well.
    fn start(&mut self, sender: Sender);

    /// Ends the Ui
    ///
    /// This is different from [`Ui::close`], as this is going to run
    /// on reloads as well.
    fn end(&mut self);

    /// Functions to trigger when the program ends
    fn close(&mut self);

    /// Stop printing updates to the window
    fn stop_printing(&mut self);

    /// Resume printing updates to the window
    fn resume_printing(&mut self);

    /// Flush the layout
    ///
    /// When this function is called, it means that Duat has finished
    /// adding or removing widgets, so the ui should calculate the
    /// layout.
    fn flush_layout(&mut self);
}

/// An [`Area`] that supports printing [`Text`]
///
/// These represent the entire GUI of Parsec, the only parts of the
/// screen where text may be printed.
pub trait Area: Send + Sync + Sized {
    // This exists solely for automatic type recognition.
    type Ui: Ui<Area = Self>;
    type ConstraintChangeErr: std::error::Error + DuatError;
    type Cache: Default + Serialize + Deserialize<'static> + 'static;
    type PrintInfo: Clone + Send + Sync;

    /// Returns the statics from `self`
    fn cache(&self) -> Option<Self::Cache>;

    /// Gets the width of the area
    fn width(&self) -> usize;

    /// Gets the height of the area
    fn height(&self) -> usize;

    /// Scrolls the [`Text`] (up or down) until the main cursor is
    /// within the [`ScrollOff`] range.
    ///
    /// [`ScrollOff`]: crate::text::ScrollOff
    fn scroll_around_point(&self, text: &Text, point: Point, cfg: PrintCfg);

    // Returns the [`Point`]s that would printed first.
    fn top_left(&self) -> (Point, Option<Point>);

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    fn set_as_active(&self);

    /// Returns `true` if this is the currently active [`Area`]
    ///
    /// Only one [`Area`] should be active at any given moment.
    fn is_active(&self) -> bool;

    /// Prints the [`Text`] via an [`Iterator`]
    fn print(&self, text: &Text, cfg: PrintCfg, painter: Painter);

    fn print_with<'a>(
        &self,
        text: &Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    );

    /// Changes the horizontal constraint of the area
    fn constrain_hor(&self, constraint: Constraint) -> Result<(), Self::ConstraintChangeErr>;

    /// Changes the vertical constraint of the area
    fn constrain_ver(&self, constraint: Constraint) -> Result<(), Self::ConstraintChangeErr>;

    /// Restores the original constraints of the widget
    fn restore_constraints(&self) -> Result<(), Self::ConstraintChangeErr>;

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), Self::ConstraintChangeErr>;

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    fn set_print_info(&self, info: Self::PrintInfo);

    //////////////////// Queries
    /// Wether or not [`self`] has changed
    ///
    /// This would mean anything relevant that wouldn't be determined
    /// by [`PrintInfo`], this is most likely going to be the bounding
    /// box, but it may be something else.
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    fn has_changed(&self) -> bool;

    /// Wether or not [`self`] is the "master" of `other`
    ///
    /// This can only happen if, by following [`self`]'s children, you
    /// would eventually reach `other`.
    fn is_master_of(&self, other: &Self) -> bool;

    /// Returns the clustered master of [`self`], if there is one
    ///
    /// If [`self`] belongs to a clustered group, return the most
    /// senior member of said cluster, which must hold all other
    /// members of the cluster.
    fn get_cluster_master(&self) -> Option<Self>;

    /// The current printing information of the area
    fn get_print_info(&self) -> Self::PrintInfo;

    /// Returns a printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns an iterator
    /// which assigns to each of them a [`Caret`]. This struct
    /// essentially represents where horizontally would this character
    /// be printed.
    ///
    /// If you want a reverse iterator, see [`Area::rev_print_iter`].
    ///
    /// [`text::Item`]: Item
    fn print_iter<'a>(
        &self,
        iter: Iter<'a>,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a
    where
        Self: Sized;

    fn print_iter_from_top<'a>(
        &self,
        text: &'a Text,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a
    where
        Self: Sized;

    /// Returns a reversed printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns a reversed
    /// iterator which assigns to each of them a [`Caret`]. This
    /// struct essentially represents where horizontally each
    /// character would be printed.
    ///
    /// If you want a forwards iterator, see [`Area::print_iter`].
    ///
    /// [`text::Item`]: Item
    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a;

    /// Bisects the [`Area`][Ui::Area] with the given index into
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
    /// ╭────────0────────╮     ╭────────1────────╮
    /// │                 │     │╭──2───╮╭───0───╮│
    /// │      self       │ --> ││      ││ self  ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// So now, there is a new area `1`, which is the parent of the
    /// areas `0` and `2`. When a new parent is created, it should be
    /// returned as the second element in the tuple.
    ///
    /// That doesn't always happen though. For example, pushing
    /// another area to the [`Side::Right`] of `1`, `2`, or `0`,
    /// in this situation, should not result in the creation of a
    /// new parent:
    ///
    /// ```text
    /// ╭────────1────────╮     ╭────────1────────╮
    /// │╭──2───╮╭───0───╮│     │╭─2─╮╭──0──╮╭─3─╮│
    /// ││      ││ self  ││     ││   ││self ││   ││
    /// │╰──────╯╰───────╯│     │╰───╯╰─────╯╰───╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// And so [`Area::bisect`] should return `(3, None)`.
    fn bisect(
        &self,
        specs: PushSpecs,
        cluster: bool,
        on_files: bool,
        cache: Self::Cache,
    ) -> (Self, Option<Self>);
}

/// A container for a master [`Area`] in Parsec
pub struct Window<U>
where
    U: Ui,
{
    nodes: Vec<Node<U>>,
    files_area: U::Area,
    master_area: U::Area,
    layout: Box<dyn Layout<U>>,
}

impl<U> Window<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of [`Window<U>`]
    pub fn new(
        ui: &mut U,
        widget: Widget<U>,
        checker: impl Fn() -> bool + 'static,
        layout: Box<dyn Layout<U>>,
    ) -> (Self, U::Area) {
        let cache = if let Some(path) = widget.inspect_as(|file: &File| file.path())
            && let Some(cache) = load_cache::<<U::Area as Area>::Cache>(path)
        {
            cache
        } else {
            <U::Area as Area>::Cache::default()
        };

        let area = ui.new_root(cache);

        widget.update(&area);

        let main_node = Node {
            widget,
            checker: Box::new(checker),
            area: area.clone(),
            busy_updating: AtomicBool::new(false),
        };

        let window = Self {
            nodes: vec![main_node],
            files_area: area.clone(),
            master_area: area.clone(),
            layout,
        };

        (window, area)
    }

    /// Pushes a [`Widget`] onto an existing one
    pub fn push(
        &mut self,
        widget: Widget<U>,
        area: &U::Area,
        checker: impl Fn() -> bool + 'static,
        specs: PushSpecs,
        cluster: bool,
    ) -> (U::Area, Option<U::Area>) {
        let cache = if let Some(path) = widget.inspect_as::<File, String>(|file| file.path())
            && let Some(cache) = load_cache::<<U::Area as Area>::Cache>(path)
        {
            cache
        } else {
            <U::Area as Area>::Cache::default()
        };

        let on_files = self.files_area.is_master_of(area);
        let (child, parent) = area.bisect(specs, cluster, on_files, cache);

        let node = Node {
            widget,
            checker: Box::new(checker),
            area: child.clone(),
            busy_updating: AtomicBool::new(false),
        };

        if *area == self.master_area
            && let Some(new_master_area) = parent.clone()
        {
            self.master_area = new_master_area;
        }

        self.nodes.push(node);
        (child, parent)
    }

    /// Pushes a [`File`] to the file's parent
    ///
    /// This function will push to the edge of `self.files_parent`
    /// This is an area, usually in the center, that contains all
    /// [`File`]s, and their associated [`Widget`]s,
    /// with others being at the perifery of this area.
    pub fn push_file(
        &mut self,
        widget: Widget<U>,
        checker: impl Fn() -> bool + 'static,
    ) -> crate::Result<(U::Area, Option<U::Area>), ()> {
        let (id, specs) = widget
            .inspect_as::<File, crate::Result<(FileId<U>, PushSpecs), ()>>(|file| {
                self.layout
                    .new_file(file, iter_files_for_layout(&self.nodes))
            })
            .unwrap()?;

        let (child, parent) = self.push(widget, &id.0, checker, specs, false);
        if let Some(parent) = &parent
            && id.0 == self.files_area
        {
            self.files_area = parent.clone();
        }

        Ok((child, parent))
    }

    /// Pushes a [`Widget`] to the master node of the current
    /// window.
    pub fn push_to_master<Checker>(
        &mut self,
        widget: Widget<U>,
        checker: Checker,
        specs: PushSpecs,
    ) -> (U::Area, Option<U::Area>)
    where
        Checker: Fn() -> bool + 'static,
    {
        let master_area = self.master_area.clone();
        self.push(widget, &master_area, checker, specs, false)
    }

    /// Returns an [`Iterator`] over the [`Widget`]s of [`self`]
    pub fn widgets(&self) -> impl DoubleEndedIterator<Item = (&Widget<U>, &U::Area)> + Clone + '_ {
        self.nodes
            .iter()
            .map(|Node { widget, area, .. }| (widget, area))
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node<U>> {
        self.nodes.iter()
    }

    /// Returns an [`Iterator`] over the names of [`File`]s
    /// and their respective [`ActiveWidget`] indices.
    ///
    /// [`ActiveWidget`]: crate::widgets::ActiveWidget
    pub fn file_names(&self) -> impl Iterator<Item = (usize, String)> + Clone + '_ {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(pos, Node { widget, .. })| {
                widget
                    .downcast::<File>()
                    .map(|file| (pos, file.read().name()))
            })
    }

    pub fn send_key(&self, key: KeyEvent) {
        if let Some(node) = self
            .nodes()
            .find(|node| context::cur_widget().unwrap().widget_ptr_eq(&node.widget))
        {
            node.widget.send_key(key, &node.area);
        }
    }

    pub fn len_widgets(&self) -> usize {
        self.nodes.len()
    }
}

/// Elements related to the [`Widget`]s
pub struct Node<U>
where
    U: Ui,
{
    widget: Widget<U>,
    checker: Box<dyn Fn() -> bool>,
    area: U::Area,
    busy_updating: AtomicBool,
}

unsafe impl<U: Ui> Send for Node<U> {}
unsafe impl<U: Ui> Sync for Node<U> {}

impl<U> Node<U>
where
    U: Ui,
{
    pub fn needs_update(&self) -> bool {
        if !self.busy_updating.load(Ordering::Acquire) {
            (self.checker)() || self.area.has_changed()
        } else {
            false
        }
    }

    pub fn update_and_print(&self) {
        self.busy_updating.store(true, Ordering::Release);

        self.widget.update_and_print(&self.area);

        self.busy_updating.store(false, Ordering::Release);
    }
}

/// A dimension on screen, can either be horizontal or vertical
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    Horizontal,
    Vertical,
}

impl Axis {
    pub fn perp(&self) -> Self {
        match self {
            Axis::Horizontal => Axis::Vertical,
            Axis::Vertical => Axis::Horizontal,
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

pub enum Event {
    Key(KeyEvent),
    Resize,
    FormChange,
    ReloadConfig,
    OpenFile(PathBuf),
    Quit,
}

pub struct Sender(mpsc::Sender<Event>);

impl Sender {
    pub fn new(sender: mpsc::Sender<Event>) -> Self {
        Self(sender)
    }

    pub fn send_key(&self, key: KeyEvent) -> Result<(), mpsc::SendError<Event>> {
        self.0.send(Event::Key(key))
    }

    pub fn send_resize(&self) -> Result<(), mpsc::SendError<Event>> {
        self.0.send(Event::Resize)
    }

    pub fn send_reload_config(&self) -> Result<(), mpsc::SendError<Event>> {
        self.0.send(Event::ReloadConfig)
    }

    pub(crate) fn _send_form_changed(&self) -> Result<(), mpsc::SendError<Event>> {
        self.0.send(Event::FormChange)
    }
}

pub struct RoWindow<'a, U>(&'a Window<U>)
where
    U: Ui;

impl<U> RoWindow<'_, U>
where
    U: Ui,
{
    /// Similar to the [`Iterator::fold`] operation, folding each
    /// [`&File`][File`] by applying an operation,
    /// returning a final result.
    ///
    /// The reason why this is a `fold` operation, and doesn't just
    /// return an [`Iterator`], is because `f` will act on a
    /// reference, as to not do unnecessary cloning of the widget's
    /// inner [`RwData<W>`], and because [`Iterator`]s cannot return
    /// references to themselves.
    pub fn fold_files<B>(&self, init: B, mut f: impl FnMut(B, &File) -> B) -> B {
        self.0
            .nodes
            .iter()
            .fold(init, |accum, Node { widget, .. }| {
                if let Some(file) = widget.downcast::<File>() {
                    f(accum, &file.read())
                } else {
                    accum
                }
            })
    }

    /// Similar to the [`Iterator::fold`] operation, folding each
    /// [`&dyn Widget<U>`][Widget] by applying an
    /// operation, returning a final result.
    ///
    /// The reason why this is a `fold` operation, and doesn't just
    /// return an [`Iterator`], is because `f` will act on a
    /// reference, as to not do unnecessary cloning of the widget's
    /// inner [`RwData<W>`], and because [`Iterator`]s cannot return
    /// references to themselves.
    pub fn fold_widgets<B>(&self, init: B, mut f: impl FnMut(B, &dyn PassiveWidget<U>) -> B) -> B {
        self.0
            .nodes
            .iter()
            .fold(init, |accum, Node { widget, .. }| {
                let f = &mut f;
                widget.raw_inspect(|widget| f(accum, widget))
            })
    }
}

pub struct RoWindows<U>(RoData<Vec<Window<U>>>)
where
    U: Ui;

impl<U> RoWindows<U>
where
    U: Ui,
{
    pub fn new(windows: RoData<Vec<Window<U>>>) -> Self {
        RoWindows(windows)
    }

    pub fn inspect_nth<B>(&self, index: usize, f: impl FnOnce(RoWindow<U>) -> B) -> Option<B> {
        let windows = self.0.read();
        windows.get(index).map(|window| f(RoWindow(window)))
    }

    pub fn try_inspect_nth<B>(&self, index: usize, f: impl FnOnce(RoWindow<U>) -> B) -> Option<B> {
        self.0
            .try_read()
            .and_then(|windows| windows.get(index).map(|window| f(RoWindow(window))))
    }
}

pub(crate) fn build_file<U: Ui>(windows: &'static RwData<Vec<Window<U>>>, mod_area: U::Area) {
    let (window_i, old_file) = {
        let windows = windows.read();
        let (window_i, node) = windows
            .iter()
            .enumerate()
            .flat_map(|(i, w)| w.nodes().map(move |n| (i, n)))
            .find(|(_, Node { area, .. })| *area == mod_area)
            .unwrap();

        let old_file = node.widget.downcast::<File>().map(|file| {
            context::cur_file().unwrap().swap((
                file,
                node.area.clone(),
                node.widget.input().unwrap().clone(),
                node.widget.related_widgets().unwrap(),
            ))
        });

        (window_i, old_file)
    };

    let builder = FileBuilder::new(windows, mod_area, window_i);
    hooks::trigger_now::<OnFileOpen<U>>(builder);

    if let Some(parts) = old_file {
        context::cur_file().unwrap().swap(parts);
    };
}

/// Information on how a [`Widget`] should be pushed onto another
///
/// This information is composed of three parts:
///
/// * A side to push;
/// * A horizontal [`Constraint`];
/// * A vertical [`Constraint`];
///
/// Constraints are demands that must be met by the widget's [`Area`],
/// on a best effort basis.
///
/// So, for example, if the [`PushSpecs`] are:
///
/// ```rust
/// use duat_core::ui::PushSpecs;
/// let specs = PushSpecs::left().with_hor_len(3.0).with_ver_ratio(2, 3);
/// ```
///
/// Then the widget should be pushed to the left, with a width of 3,
/// and its height should be equal to two thirds of the area directly
/// below.
#[derive(Debug, Clone, Copy)]
pub struct PushSpecs {
    side: Side,
    ver_con: Option<Constraint>,
    hor_con: Option<Constraint>,
}

impl PushSpecs {
    /// Returns a new instance of [`PushSpecs`]
    pub fn left() -> Self {
        Self {
            side: Side::Left,
            ver_con: None,
            hor_con: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn right() -> Self {
        Self {
            side: Side::Right,
            ver_con: None,
            hor_con: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn above() -> Self {
        Self {
            side: Side::Above,
            ver_con: None,
            hor_con: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn below() -> Self {
        Self {
            side: Side::Below,
            ver_con: None,
            hor_con: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn to_left(self) -> Self {
        Self { side: Side::Left, ..self }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn to_right(self) -> Self {
        Self { side: Side::Right, ..self }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn to_above(self) -> Self {
        Self { side: Side::Above, ..self }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn to_below(self) -> Self {
        Self { side: Side::Below, ..self }
    }

    pub fn with_ver_len(self, len: f64) -> Self {
        Self {
            ver_con: Some(Constraint::Length(len)),
            ..self
        }
    }

    pub fn with_ver_min(self, min: f64) -> Self {
        Self {
            ver_con: Some(Constraint::Min(min)),
            ..self
        }
    }

    pub fn with_ver_max(self, max: f64) -> Self {
        Self {
            ver_con: Some(Constraint::Max(max)),
            ..self
        }
    }

    pub fn with_ver_ratio(self, den: u16, div: u16) -> Self {
        Self {
            ver_con: Some(Constraint::Ratio(den, div)),
            ..self
        }
    }

    pub fn with_hor_len(self, len: f64) -> Self {
        Self {
            hor_con: Some(Constraint::Length(len)),
            ..self
        }
    }

    pub fn with_hor_min(self, min: f64) -> Self {
        Self {
            hor_con: Some(Constraint::Min(min)),
            ..self
        }
    }

    pub fn with_hor_max(self, max: f64) -> Self {
        Self {
            hor_con: Some(Constraint::Max(max)),
            ..self
        }
    }

    pub fn with_hor_ratio(self, den: u16, div: u16) -> Self {
        Self {
            hor_con: Some(Constraint::Ratio(den, div)),
            ..self
        }
    }

    pub fn axis(&self) -> Axis {
        match self.side {
            Side::Above | Side::Below => Axis::Vertical,
            Side::Right | Side::Left => Axis::Horizontal,
        }
    }

    pub fn comes_earlier(&self) -> bool {
        matches!(self.side, Side::Left | Side::Above)
    }

    pub fn ver_constraint(&self) -> Option<Constraint> {
        self.ver_con
    }

    pub fn hor_constraint(&self) -> Option<Constraint> {
        self.hor_con
    }

    pub fn constraint_on(&self, axis: Axis) -> Option<Constraint> {
        match axis {
            Axis::Horizontal => self.hor_con,
            Axis::Vertical => self.ver_con,
        }
    }

    pub fn is_resizable_on(&self, axis: Axis) -> bool {
        let con = match axis {
            Axis::Horizontal => self.hor_con,
            Axis::Vertical => self.ver_con,
        };
        matches!(con, Some(Constraint::Min(..) | Constraint::Max(..)) | None)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    Ratio(u16, u16),
    Length(f64),
    Min(f64),
    Max(f64),
}

/// A direction, where a [`Widget`] will be placed in relation to
/// another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Above,
    Right,
    Below,
    Left,
}

#[derive(Debug, Clone, Copy)]
pub struct Caret {
    pub x: usize,
    pub len: usize,
    pub wrap: bool,
}

impl Caret {
    #[inline(always)]
    pub fn new(x: usize, len: usize, wrap: bool) -> Self {
        Self { x, len, wrap }
    }
}
