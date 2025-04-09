mod builder;
mod layout;

use std::{
    fmt::Debug,
    marker::PhantomData,
    sync::{mpsc, Arc}, time::Instant,
};

use bincode::{Decode, Encode};
use crossterm::event::KeyEvent;
use layout::window_files;
use parking_lot::lock_api::Mutex;

pub use self::{
    builder::{FileBuilder, WindowBuilder},
    layout::{FileId, Layout, MasterOnLeft, WindowFiles},
};
use crate::{
    cache::load_cache,
    cfg::PrintCfg,
    data::RwData,
    form::Painter,
    text::{FwdIter, Item, Point, RevIter, Text, TwoPoints},
    widgets::{File, Node, Widget},
};

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
///
/// # NOTE
///
/// The dependency on [`Clone`] is only here for convenience. Many
/// types require a [`Ui`] as a generic parameter, and if [`Ui`] does
/// not implement [`Clone`], deriving [`Clone`] for said types would
/// be a very manual task.
///
/// Below is the recommended implementation of [`Clone`] for all types
/// that implement [`Ui`]:
///
/// ```rust
/// # mod duat_smart_fridge {
/// #     pub struct Ui;
/// # }
/// impl Clone for duat_smart_fridge::Ui {
///     fn clone(&self) -> Self {
///         panic!("You are not supposed to clone the Ui");
///     }
/// }
/// ```
pub trait Ui: Clone + Send + Sync + 'static {
    type Area: Area<Ui = Self> + Clone + PartialEq + Send + 'static;
    type MetaStatics: Default;

    ////////// Functions executed from the outer loop

    /// Functions to trigger when the program begins
    ///
    /// These will happen in the main `duat` runner
    fn open(ms: &'static Self::MetaStatics, duat_tx: Sender);

    /// Functions to trigger when the program ends
    ///
    /// These will happen in the main `duat` runner
    fn close(ms: &'static Self::MetaStatics);

    ////////// Functions executed from within the configuration loop

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    fn new_root(ms: &'static Self::MetaStatics, cache: <Self::Area as Area>::Cache) -> Self::Area;

    /// Switches the currently active window
    ///
    /// This will only happen to with window indices that are actual
    /// windows. If at some point, a window index comes up that is not
    /// actually a window, that's a bug.
    fn switch_window(ms: &'static Self::MetaStatics, win: usize);

    /// Flush the layout
    ///
    /// When this function is called, it means that Duat has finished
    /// adding or removing widgets, so the ui should calculate the
    /// layout.
    fn flush_layout(ms: &'static Self::MetaStatics);

    /// Functions to trigger when the program reloads
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn load(ms: &'static Self::MetaStatics);

    /// Unloads the [`Ui`]
    ///
    /// Unlike [`Ui::close`], this will happen both when Duat reloads
    /// the configuration and when it closes the app.
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn unload(ms: &'static Self::MetaStatics);

    /// Removes a window from the [`Ui`]
    ///
    /// This should keep the current active window consistent. That
    /// is, if the current window was ahead of the deleted one, it
    /// should be shifted back, so that the same window is still
    /// displayed.
    fn remove_window(ms: &'static Self::MetaStatics, win: usize);
}

/// An [`Area`] that supports printing [`Text`]
///
/// These represent the entire GUI of Parsec, the only parts of the
/// screen where text may be printed.
pub trait Area: Send + Sync + Sized {
    // This exists solely for automatic type recognition.
    type Ui: Ui<Area = Self>;
    type Cache: Default + Encode + Decode<()> + 'static;
    type PrintInfo: Default + Clone + Send + Sync;

    ////////// Area modification

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
        _: DuatPermission,
    ) -> (Self, Option<Self>);

    /// Deletes this [`Area`], signaling the closing of a [`Widget`]
    ///
    /// If the [`Area`]'s parent was also deleted, return it.
    fn delete(&self, _: DuatPermission) -> Option<Self>;

    /// Swaps this [`Area`] with another one
    ///
    /// The swapped [`Area`]s will be cluster masters of the
    /// respective [`Area`]s. As such, if they belong to the same
    /// master, nothing happens.
    fn swap(&self, other: &Self, _: DuatPermission);

    /// Spawns a floating area from a [`TwoPoints`]
    ///
    /// This [`Area`] will be placed in some position "pushed" onto
    /// where those [`TwoPoints`] are on screen, and the [`PushSpecs`]
    /// are mostlya a "suggestion", that is, if it can't be placed in
    /// that specific direction, another one will be found, and if
    /// none of them fit, then the maximum allowed space on screen
    /// will be allocated to meet at least some of the demands.
    fn spawn_floating(
        &self,
        at: impl TwoPoints,
        specs: PushSpecs,
        text: &Text,
        cfg: PrintCfg,
        _: DuatPermission,
    ) -> Self;

    /// Changes the horizontal constraint of the area
    fn constrain_hor(&self, cons: impl IntoIterator<Item = Constraint>) -> Result<(), Text>;

    /// Changes the vertical constraint of the area
    fn constrain_ver(&self, cons: impl IntoIterator<Item = Constraint>) -> Result<(), Text>;

    /// Restores the original constraints of the widget
    fn restore_constraints(&self) -> Result<(), Text>;

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), Text>;

    /// Scrolls the [`Text`] (up or down) until the main cursor is
    /// within the [`ScrollOff`] range.
    ///
    /// [`ScrollOff`]: crate::cfg::ScrollOff
    fn scroll_around_point(&self, text: &Text, point: Point, cfg: PrintCfg);

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    fn set_as_active(&self);

    ////////// Printing

    /// Prints the [`Text`] via an [`Iterator`]
    fn print(&self, text: &mut Text, cfg: PrintCfg, painter: Painter);

    fn print_with<'a>(
        &self,
        text: &mut Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    );

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    fn set_print_info(&self, info: Self::PrintInfo);

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
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a
    where
        Self: Sized;

    fn print_iter_from_top<'a>(
        &self,
        text: &'a Text,
        cfg: PrintCfg,
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
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a;

    ////////// Queries

    /// Whether or not [`self`] has changed
    ///
    /// This would mean anything relevant that wouldn't be determined
    /// by [`PrintInfo`], this is most likely going to be the bounding
    /// box, but it may be something else.
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    fn has_changed(&self) -> bool;

    /// Whether or not [`self`] is the "master" of `other`
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

    /// Returns the statics from `self`
    fn cache(&self) -> Option<Self::Cache>;

    /// Gets the width of the area
    fn width(&self) -> u32;

    /// Gets the height of the area
    fn height(&self) -> u32;

    /// The first point that should be printed
    fn first_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>);

    /// The last point that should be printed
    fn last_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>);

    /// The current printing information of the area
    fn print_info(&self) -> Self::PrintInfo;

    /// Returns `true` if this is the currently active [`Area`]
    ///
    /// Only one [`Area`] should be active at any given moment.
    fn is_active(&self) -> bool;
}

/// A container for a master [`Area`] in Parsec
pub struct Window<U: Ui> {
    nodes: Vec<Node<U>>,
    files_area: U::Area,
    layout: Box<dyn Layout<U>>,
}

impl<U: Ui> Window<U> {
    /// Returns a new instance of [`Window`]
    pub(crate) fn new<W: Widget<U>>(
        ms: &'static U::MetaStatics,
        widget: W,
        checker: impl Fn() -> bool + Send + Sync + 'static,
        layout: Box<dyn Layout<U>>,
    ) -> (Self, Node<U>) {
        let widget = RwData::<dyn Widget<U>>::new_unsized::<W>(Arc::new(Mutex::new(widget)));

        let cache = if let Some(file) = widget.read_as::<File>()
            && let Some(cache) = load_cache::<<U::Area as Area>::Cache>(file.path())
        {
            cache
        } else {
            <U::Area as Area>::Cache::default()
        };

        let area = U::new_root(ms, cache);

        let node = Node::new::<W>(widget, area.clone(), checker);
        node.update();

        let window = Self {
            nodes: vec![node.clone()],
            files_area: area.clone(),
            layout,
        };

        (window, node)
    }

    pub(crate) fn from_raw(
        files_area: U::Area,
        nodes: Vec<Node<U>>,
        layout: Box<dyn Layout<U>>,
    ) -> Self {
        let files_area = files_area.get_cluster_master().unwrap_or(files_area);
        Self { nodes, files_area, layout }
    }

    /// Pushes a [`Widget`] onto an existing one
    pub(crate) fn push<W: Widget<U>>(
        &mut self,
        widget: W,
        area: &U::Area,
        checker: impl Fn() -> bool + 'static + Send + Sync,
        specs: PushSpecs,
        do_cluster: bool,
        on_files: bool,
    ) -> (Node<U>, Option<U::Area>) {
        let widget = RwData::<dyn Widget<U>>::new_unsized::<W>(Arc::new(Mutex::new(widget)));

        let cache = if let Some(file) = widget.read_as::<File>()
            && let Some(cache) = load_cache::<<U::Area as Area>::Cache>(file.path())
        {
            cache
        } else {
            <U::Area as Area>::Cache::default()
        };

        let (child, parent) =
            area.bisect(specs, do_cluster, on_files, cache, DuatPermission::new());

        self.nodes.push(Node::new::<W>(widget, child, checker));
        (self.nodes.last().unwrap().clone(), parent)
    }

    /// Pushes a [`File`] to the file's parent
    ///
    /// This function will push to the edge of `self.files_parent`
    /// This is an area, usually in the center, that contains all
    /// [`File`]s, and their associated [`Widget`]s,
    /// with others being at the perifery of this area.
    pub(crate) fn push_file(
        &mut self,
        mut file: File,
        checker: impl Fn() -> bool + 'static + Send + Sync,
    ) -> Result<(Node<U>, Option<U::Area>), Text> {
        let window_files = window_files(&self.nodes);
        file.layout_ordering = window_files.len();
        let (id, specs) = self.layout.new_file(&file, window_files)?;

        let (child, parent) = self.push(file, &id.0, checker, specs, false, true);

        if let Some(parent) = &parent
            && id.0 == self.files_area
        {
            self.files_area = parent.clone();
        }

        Ok((child, parent))
    }

    /// Removes all [`Node`]s whose [`Area`]s where deleted
    pub(crate) fn remove_file(&mut self, name: &str) {
        let Some(node) = self
            .nodes
            .extract_if(.., |node| {
                node.as_file()
                    .is_some_and(|(f, ..)| f.read().name() == name)
            })
            .next()
        else {
            return;
        };

        // If this is the case, this means there is only one File left in this
        // Window, so the files_area should be the cluster master of that
        // File.
        if let Some(parent) = node.area().delete(DuatPermission::new())
            && parent == self.files_area
        {
            let files = self.file_nodes();
            let (only_file, _) = files.first().unwrap();
            self.files_area = only_file
                .area()
                .get_cluster_master()
                .unwrap_or(only_file.area().clone())
        }

        if let Some(related) = node.related_widgets() {
            let nodes = related.read();
            for node in self.nodes.extract_if(.., |node| nodes.contains(node)) {
                node.area().delete(DuatPermission::new());
            }
        }
    }

    /// Takes all [`Node`]s related to a given [`Node`]
    pub(crate) fn take_file_and_related_nodes(&mut self, node: &Node<U>) -> Vec<Node<U>> {
        if let Some(related) = node.related_widgets() {
            let layout_ordering = node.widget().read_as::<File>().unwrap().layout_ordering;

            let nodes = related.read();
            let nodes = self
                .nodes
                .extract_if(.., |n| nodes.contains(n) || n == node)
                .collect();

            for node in &self.nodes {
                if let Some(mut file) = node.widget().write_as::<File>() {
                    if file.layout_ordering > layout_ordering {
                        file.layout_ordering -= 1;
                    }
                }
            }

            nodes
        } else {
            Vec::new()
        }
    }

    pub(crate) fn insert_file_nodes(&mut self, layout_ordering: usize, nodes: Vec<Node<U>>) {
        if let Some(i) = self.nodes.iter().position(|node| {
            node.widget()
                .read_as::<File>()
                .is_some_and(|file| file.layout_ordering >= layout_ordering)
        }) {
            for node in self.nodes[i..].iter() {
                node.widget().write_as::<File>().unwrap().layout_ordering += 1;
            }
            self.nodes.splice(i..i, nodes);
        } else {
            self.nodes.extend(nodes);
        }
    }

    pub fn nodes(&self) -> impl ExactSizeIterator<Item = &Node<U>> + DoubleEndedIterator {
        self.nodes.iter()
    }

    /// Returns an [`Iterator`] over the names of [`File`]s
    /// and their respective [`Widget`] indices
    ///
    /// [`Widget`]: crate::widgets::Widget
    pub fn file_names(&self) -> Vec<String> {
        window_files(&self.nodes)
            .into_iter()
            .map(|f| f.0.widget().read_as::<File>().unwrap().name())
            .collect()
    }

    pub fn file_paths(&self) -> Vec<String> {
        window_files(&self.nodes)
            .into_iter()
            .map(|f| f.0.widget().read_as::<File>().unwrap().name())
            .collect()
    }

    pub fn file_nodes(&self) -> WindowFiles<U> {
        window_files(&self.nodes)
    }

    pub fn len_widgets(&self) -> usize {
        self.nodes.len()
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

    /// Returns `true` if the axis is [`Horizontal`].
    ///
    /// [`Horizontal`]: Axis::Horizontal
    #[must_use]
    pub fn is_hor(&self) -> bool {
        matches!(self, Self::Horizontal)
    }

    /// Returns `true` if the axis is [`Vertical`].
    ///
    /// [`Vertical`]: Axis::Vertical
    #[must_use]
    pub fn is_ver(&self) -> bool {
        matches!(self, Self::Vertical)
    }
}

impl From<PushSpecs> for Axis {
    fn from(value: PushSpecs) -> Self {
        match value.side {
            Side::Above | Side::Below => Axis::Vertical,
            _ => Axis::Horizontal,
        }
    }
}

#[derive(Debug)]
pub enum DuatEvent {
    Key(KeyEvent),
    Resize,
    FormChange,
    MetaMsg(Text),
    ReloadConfig,
    OpenFile(String),
    CloseFile(String),
    SwapFiles(String, String),
    OpenWindow(String),
    SwitchWindow(usize),
    ReloadStarted(Instant),
    Quit,
}

pub struct Sender(&'static mpsc::Sender<DuatEvent>);

impl Sender {
    pub fn new(sender: &'static mpsc::Sender<DuatEvent>) -> Self {
        Self(sender)
    }

    pub fn send_key(&self, key: KeyEvent) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::Key(key))
    }

    pub fn send_reload_config(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::ReloadConfig)
    }

    pub fn send_resize(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::Resize)
    }

    pub(crate) fn send_form_changed(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::FormChange)
    }
}

pub struct RoWindow<'a, U: Ui>(&'a Window<U>);

impl<U: Ui> RoWindow<'_, U> {
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
        self.0.nodes.iter().fold(init, |accum, node| {
            if let Some(file) = node.try_downcast::<File>() {
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
    pub fn fold_widgets<B>(&self, init: B, mut f: impl FnMut(B, &dyn Widget<U>) -> B) -> B {
        self.0.nodes.iter().fold(init, |accum, node| {
            let f = &mut f;
            node.raw_inspect(|widget| f(accum, widget))
        })
    }
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
    ver_cons: [Option<Constraint>; 4],
    hor_cons: [Option<Constraint>; 4],
}

impl PushSpecs {
    /// Returns a new instance of [`PushSpecs`]
    pub fn left() -> Self {
        Self {
            side: Side::Left,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn right() -> Self {
        Self {
            side: Side::Right,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn above() -> Self {
        Self {
            side: Side::Above,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Returns a new instance of [`PushSpecs`]
    pub fn below() -> Self {
        Self {
            side: Side::Below,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
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

    pub fn with_ver_len(mut self, len: f32) -> Self {
        for con in self.ver_cons.iter_mut() {
            match con {
                Some(Constraint::Len(_)) | None => {
                    *con = Some(Constraint::Len(len));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_ver_min(mut self, min: f32) -> Self {
        for con in self.ver_cons.iter_mut() {
            match con {
                Some(Constraint::Min(_)) | None => {
                    *con = Some(Constraint::Min(min));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_ver_max(mut self, max: f32) -> Self {
        for con in self.ver_cons.iter_mut() {
            match con {
                Some(Constraint::Max(_)) | None => {
                    *con = Some(Constraint::Max(max));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_ver_ratio(mut self, den: u16, div: u16) -> Self {
        for con in self.ver_cons.iter_mut() {
            match con {
                Some(Constraint::Ratio(..)) | None => {
                    *con = Some(Constraint::Ratio(den, div));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_hor_len(mut self, len: f32) -> Self {
        for con in self.hor_cons.iter_mut() {
            match con {
                Some(Constraint::Len(_)) | None => {
                    *con = Some(Constraint::Len(len));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_hor_min(mut self, min: f32) -> Self {
        for con in self.hor_cons.iter_mut() {
            match con {
                Some(Constraint::Min(_)) | None => {
                    *con = Some(Constraint::Min(min));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_hor_max(mut self, max: f32) -> Self {
        for con in self.hor_cons.iter_mut() {
            match con {
                Some(Constraint::Max(_)) | None => {
                    *con = Some(Constraint::Max(max));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn with_hor_ratio(mut self, den: u16, div: u16) -> Self {
        for con in self.hor_cons.iter_mut() {
            match con {
                Some(Constraint::Ratio(..)) | None => {
                    *con = Some(Constraint::Ratio(den, div));
                    break;
                }
                _ => {}
            }
        }
        self
    }

    pub fn axis(&self) -> Axis {
        match self.side {
            Side::Above | Side::Below => Axis::Vertical,
            Side::Right | Side::Left => Axis::Horizontal,
        }
    }

    pub fn side(&self) -> Side {
        self.side
    }

    pub fn comes_earlier(&self) -> bool {
        matches!(self.side, Side::Left | Side::Above)
    }

    pub fn ver_cons(&self) -> impl Iterator<Item = Constraint> {
        self.ver_cons.into_iter().flatten()
    }

    pub fn hor_cons(&self) -> impl Iterator<Item = Constraint> {
        self.hor_cons.into_iter().flatten()
    }

    pub fn cons_on(&self, axis: Axis) -> impl Iterator<Item = Constraint> {
        match axis {
            Axis::Horizontal => self.hor_cons.into_iter().flatten(),
            Axis::Vertical => self.ver_cons.into_iter().flatten(),
        }
    }

    pub fn is_resizable_on(&self, axis: Axis) -> bool {
        let cons = match axis {
            Axis::Horizontal => &self.hor_cons,
            Axis::Vertical => &self.ver_cons,
        };
        cons.iter()
            .flatten()
            .all(|con| matches!(con, Constraint::Min(..) | Constraint::Max(..)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    Len(f32),
    Min(f32),
    Max(f32),
    Ratio(u16, u16),
}

impl Eq for Constraint {}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for Constraint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        fn discriminant(con: &Constraint) -> usize {
            match con {
                Constraint::Len(_) => 0,
                Constraint::Min(_) => 1,
                Constraint::Max(_) => 2,
                Constraint::Ratio(..) => 3,
            }
        }
        match (self, other) {
            (Constraint::Len(lhs), Constraint::Len(rhs)) => lhs.partial_cmp(rhs),
            (Constraint::Min(lhs), Constraint::Min(rhs)) => lhs.partial_cmp(rhs),
            (Constraint::Max(lhs), Constraint::Max(rhs)) => lhs.partial_cmp(rhs),
            (Constraint::Ratio(lhs_den, lhs_div), Constraint::Ratio(rhs_den, rhs_div)) => {
                (lhs_den, lhs_div).partial_cmp(&(rhs_den, rhs_div))
            }
            (lhs, rhs) => discriminant(lhs).partial_cmp(&discriminant(rhs)),
        }
    }
}

impl Ord for Constraint {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Constraint {
    /// Returns `true` if the constraint is [`Len`].
    ///
    /// [`Len`]: Constraint::Len
    #[must_use]
    pub fn is_len(&self) -> bool {
        matches!(self, Self::Len(..))
    }

    /// Returns `true` if the constraint is [`Min`].
    ///
    /// [`Min`]: Constraint::Min
    #[must_use]
    pub fn is_min(&self) -> bool {
        matches!(self, Self::Min(..))
    }

    /// Returns `true` if the constraint is [`Max`].
    ///
    /// [`Max`]: Constraint::Max
    #[must_use]
    pub fn is_max(&self) -> bool {
        matches!(self, Self::Max(..))
    }

    /// Returns `true` if the constraint is [`Ratio`].
    ///
    /// [`Ratio`]: Constraint::Ratio
    #[must_use]
    pub fn is_ratio(&self) -> bool {
        matches!(self, Self::Ratio(..))
    }
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
    pub x: u32,
    pub len: u32,
    pub wrap: bool,
}

impl Caret {
    #[inline(always)]
    pub fn new(x: u32, len: u32, wrap: bool) -> Self {
        Self { x, len, wrap }
    }
}

/// Only Duat is allowed to alter the layout of [`Widget`]s, so this
/// struct is used to forbid the end user from doing the same.
pub struct DuatPermission(PhantomData<()>);

impl DuatPermission {
    pub(crate) fn new() -> Self {
        Self(PhantomData)
    }
}
