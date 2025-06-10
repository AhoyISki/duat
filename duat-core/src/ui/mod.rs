//! [`Ui`] structs and functions
//!
//! Although there is only a terminal [`Ui`] implemented at the
//! moment, Duat is supposed to be Ui agnostic, and I plan to create a
//! GUI app (probably in `iced` or something), and a web app as well,
//! which is honestly more of an excuse for me to become more well
//! versed on javascript.
//!
//! Each [`Ui`] is essentially a screen separated by a bunch of
//! [`Ui::Area`]s. This happens by splitting a main [`Ui::Area`]
//! continuously, by pushing [`Widget`]s on other [`Widget`]s. When a
//! [`Widget`] is pushed to another, the area of the prior [`Widget`]
//! is split in half, with [`Constraint`]s set by the [`PushSpecs`],
//! letting the user define the exact space that each [`Widget`] will
//! take up on the screen.
//!
//! Duat also supports multiple tabs, each of which is defined by a
//! main [`Ui::Area`] that was split many times over.
//!
//! The [`Ui`] also supports the concept of "clustering", that is,
//! when you push a [`Widget`] to a [`File`] via the [`OnFileOpen`]
//! [`hook`], it gets "clustered" to that [`File`]. This means a few
//! things. For one, if you close a [`File`], all of its clustered
//! [`Widget`]s will also close. If you swap two [`File`]s, what you
//! will actually swap is the [`Ui::Area`] that contains the [`File`]
//! and all of its clustered [`Widget`].
//!
//! Additionally, on the terminal [`Ui`], clustering is used to
//! determine where to draw borders between [`Ui::Area`]s, and it
//! should be used like that in other [`Ui`] implementations as well.
//!
//! [`OnFileOpen`]: crate::hook::OnFileOpen
//! [`hook`]: crate::hook
mod builder;
mod layout;

use std::{cell::RefCell, fmt::Debug, rc::Rc, sync::mpsc, time::Instant};

use bincode::{Decode, Encode};
use crossterm::event::KeyEvent;
use layout::window_files;

pub use self::{
    builder::{FileBuilder, WindowBuilder},
    layout::{FileId, Layout, MasterOnLeft},
};
use crate::{
    cache::load_cache,
    cfg::PrintCfg,
    data::{Pass, RwData},
    file::File,
    form::Painter,
    text::{Builder, FwdIter, Item, Point, RevIter, Text, TwoPoints},
    widget::{Node, Widget},
};

/// All the methods that a working gui/tui will need to implement, in
/// order to use Duat.
///
/// # NOTE
///
/// The dependencies on [`Clone`] and [`Default`] is only here for
/// convenience. Many types require a [`Ui`] as a generic parameter,
/// and if [`Ui`] does not implement [`Clone`] or [`Default`],
/// deriving [`Clone`] or [`Default`] for said types would
/// be a very manual task.
///
/// Below is the recommended implementation of [`Clone`] adn
/// [`Default`] for all types that implement [`Ui`]:
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
/// impl Default for duat_smart_fridge::Ui {
///     fn default() -> Self {
///         panic!("You are not supposed to call the Ui's default constructor");
///     }
/// }
/// ```
pub trait Ui: Default + Clone + 'static {
    /// The [`RawArea`] of this [`Ui`]
    type Area: RawArea<Ui = Self>;
    /// Variables to initialize at the Duat application, outside the
    /// config
    ///
    /// Of the ways that Duat can be extended and modified, only the
    /// [`Ui`] can be accessed by the Duat executor itself, since it
    /// is one of its dependencies. This means that it is possible to
    /// keep some things between reloads.
    ///
    /// This is particularly useful for some kinds of static
    /// variables. For example, in [`term-ui`], it makes heavy use of
    /// [`std`] defined functions to print to the terminal. Those use
    /// a static [`Mutex`] internally, and I have found that it is
    /// better to use the one from the Duat app, rather than one from
    /// the config crate
    ///
    /// [`term-ui`]: docs.rs/term-ui/latest/term_ui
    /// [`Mutex`]: std::sync::Mutex
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

    ////////// Functions executed from within the configuratio loop

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    fn new_root(
        ms: &'static Self::MetaStatics,
        cache: <Self::Area as RawArea>::Cache,
    ) -> Self::Area;

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
    /// the configuratio and when it closes the app.
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

/// An [`RawArea`] that supports printing [`Text`]
///
/// These represent the entire GUI of Duat, the only parts of the
/// screen where text may be printed.
pub trait RawArea: Clone + PartialEq + Sized + 'static {
    /// The [`Ui`] this [`RawArea`] belongs to
    type Ui: Ui<Area = Self>;
    /// Something to be kept between app instances/reloads
    ///
    /// The most useful thing to keep in this case is the
    /// [`PrintInfo`], but you could include other things
    ///
    /// [`PrintInfo`]: RawArea::PrintInfo
    type Cache: Default + Encode + Decode<()> + 'static;
    /// Information about what parts of a [`Text`] should be printed
    ///
    /// For the [`term-ui`], for example, this is quite simple, it
    /// only needs to include the real and ghost [`Point`]s on the
    /// top left corner in order to print correctly, but your own
    /// [`Ui`] could differ in what it needs to keep, if it makes
    /// use of smooth scrolling, for example.
    ///
    /// [`term-ui`]: docs.rs/term-ui/latest/term_ui
    type PrintInfo: Default + Clone + PartialEq + Eq;

    ////////// Area modification

    /// Bisects the [`RawArea`][Ui::Area] with the given index into
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
    /// And so [`RawArea::bisect`] should return `(3, None)`.
    fn bisect(
        area: MutArea<Self>,
        specs: PushSpecs,
        cluster: bool,
        on_files: bool,
        cache: Self::Cache,
    ) -> (Self, Option<Self>);

    /// Deletes this [`RawArea`], signaling the closing of a
    /// [`Widget`]
    ///
    /// If the [`RawArea`]'s parent was also deleted, return it.
    fn delete(area: MutArea<Self>) -> Option<Self>;

    /// Swaps this [`RawArea`] with another one
    ///
    /// The swapped [`RawArea`]s will be cluster masters of the
    /// respective [`RawArea`]s. As such, if they belong to the same
    /// master, nothing happens.
    fn swap(lhs: MutArea<Self>, rhs: &Self);

    /// Spawns a floating area on this [`RawArea`]
    ///
    /// This function will take a list of [`SpawnSpecs`], taking the
    /// first one that fits, and readapting as the constraints are
    /// altered
    fn spawn_floating(area: MutArea<Self>, specs: SpawnSpecs) -> Result<Self, Text>;

    /// Spawns a floating area
    ///
    /// If the [`TwoPoints`] parameter is not specified, this new
    /// [`RawArea`] will be pushed to the edges of the old one, the
    /// pushed edge being the same as the [`Side`] used for pushing.
    ///
    /// If there is a [`TwoPoints`] argument, the
    fn spawn_floating_at(
        area: MutArea<Self>,
        specs: SpawnSpecs,
        at: impl TwoPoints,
        text: &Text,
        cfg: PrintCfg,
    ) -> Result<Self, Text>;

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

    /// Tells the [`Ui`] that this [`RawArea`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`RawArea`] while deactivating
    /// any other active [`RawArea`].
    fn set_as_active(&self);

    ////////// Printing

    /// Prints the [`Text`] via an [`Iterator`]
    fn print(&self, text: &mut Text, cfg: PrintCfg, painter: Painter);

    /// Prints the [`Text`] with a callback function
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
    /// If you want a reverse iterator, see
    /// [`RawArea::rev_print_iter`].
    ///
    /// [`text::Item`]: Item
    fn print_iter<'a>(
        &self,
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a;

    /// Returns a reversed printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns a reversed
    /// iterator which assigns to each of them a [`Caret`]. This
    /// struct essentially represents where horizontally each
    /// character would be printed.
    ///
    /// If you want a forwards iterator, see [`RawArea::print_iter`].
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

    /// Returns `true` if this is the currently active [`RawArea`]
    ///
    /// Only one [`RawArea`] should be active at any given moment.
    fn is_active(&self) -> bool;
}

/// A container for a master [`RawArea`] in Duat
#[doc(hidden)]
pub struct Window<U: Ui> {
    nodes: Vec<Node<U>>,
    files_area: U::Area,
    layout: Box<dyn Layout<U>>,
}

impl<U: Ui> Window<U> {
    /// Returns a new instance of [`Window`]
    pub(crate) fn new<W: Widget<U>>(
        pa: &mut Pass,
        ms: &'static U::MetaStatics,
        widget: W,
        layout: Box<dyn Layout<U>>,
    ) -> (Self, Node<U>) {
        let widget =
            unsafe { RwData::<dyn Widget<U>>::new_unsized::<W>(Rc::new(RefCell::new(widget))) };

        let cache = widget
            .read_as(&*pa, |f: &File<U>| {
                load_cache::<<U::Area as RawArea>::Cache>(f.path())
            })
            .flatten()
            .unwrap_or_default();

        let area = U::new_root(ms, cache);

        let node = Node::new::<W>(&mut *pa, widget, area.clone());

        let window = Self {
            nodes: vec![node.clone()],
            files_area: area.clone(),
            layout,
        };

        (window, node)
    }

    /// Returns a new [`Window`] from raw elements
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
        pa: &mut Pass,
        widget: W,
        area: &U::Area,
        specs: PushSpecs,
        do_cluster: bool,
        on_files: bool,
    ) -> (Node<U>, Option<U::Area>) {
        #[inline(never)]
        fn get_areas<U: Ui>(
            pa: &mut Pass<'_>,
            area: &<U as Ui>::Area,
            specs: PushSpecs,
            do_cluster: bool,
            on_files: bool,
            widget: &RwData<dyn Widget<U> + 'static>,
        ) -> (U::Area, Option<U::Area>) {
            let cache = widget
                .read_as(&*pa, |f: &File<U>| {
                    load_cache::<<U::Area as RawArea>::Cache>(f.path())
                })
                .flatten()
                .unwrap_or_default();

            let (child, parent) = MutArea(area).bisect(specs, do_cluster, on_files, cache);

            (child, parent)
        }

        let widget =
            unsafe { RwData::<dyn Widget<U>>::new_unsized::<W>(Rc::new(RefCell::new(widget))) };

        let (child, parent) = get_areas(pa, area, specs, do_cluster, on_files, &widget);

        self.nodes.push(Node::new::<W>(&mut *pa, widget, child));

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
        pa: &mut Pass,
        mut file: File<U>,
    ) -> Result<(Node<U>, Option<U::Area>), Text> {
        let window_files = window_files(&*pa, &self.nodes);
        file.layout_order = window_files.len();
        let (id, specs) = self.layout.new_file(&file, window_files)?;

        let (child, parent) = self.push(pa, file, &id.0, specs, false, true);

        if let Some(parent) = &parent
            && id.0 == self.files_area
        {
            self.files_area = parent.clone();
        }

        Ok((child, parent))
    }

    /// Removes all [`Node`]s whose [`RawArea`]s where deleted
    pub(crate) fn remove_file(&mut self, pa: &Pass, name: &str) {
        let Some(node) = self
            .nodes
            .extract_if(.., |node| {
                node.as_file()
                    .is_some_and(|(f, ..)| f.read(pa, File::name) == name)
            })
            .next()
        else {
            return;
        };

        // If this is the case, this means there is only one File left in this
        // Window, so the files_area should be the cluster master of that
        // File.
        if let Some(parent) = MutArea(node.area()).delete()
            && parent == self.files_area
        {
            let files = self.file_nodes(pa);
            let (_, FileId(area)) = files.first().unwrap();
            self.files_area = area.clone();
        }

        if let Some(related) = node.related_widgets() {
            related.read(pa, |related| {
                for node in self.nodes.extract_if(.., |node| related.contains(node)) {
                    MutArea(node.area()).delete();
                }
            });
        }
    }

    /// Takes all [`Node`]s related to a given [`Node`]
    pub(crate) fn take_file_and_related_nodes(
        &mut self,
        pa: &mut Pass,
        node: &Node<U>,
    ) -> Vec<Node<U>> {
        if let Some(related) = node.related_widgets() {
            let lo = node
                .widget()
                .read_as(&*pa, |f: &File<U>| f.layout_order)
                .unwrap();

            let nodes = related.read(&*pa, |related| {
                self.nodes
                    .extract_if(.., |n| related.contains(n) || n == node)
                    .collect()
            });

            for node in &self.nodes {
                node.widget().write_as(&mut *pa, |f: &mut File<U>| {
                    if f.layout_order > lo {
                        f.layout_order -= 1;
                    }
                });
            }

            nodes
        } else {
            Vec::new()
        }
    }

    /// Inserts [`File`] nodes orderly
    pub(crate) fn insert_file_nodes<'a>(
        &mut self,
        pa: &mut Pass<'a>,
        layout_ordering: usize,
        nodes: Vec<Node<U>>,
    ) {
        if let Some(i) = self.nodes.iter().position(|node| {
            node.widget()
                .read_as(&*pa, |f: &File<U>| f.layout_order >= layout_ordering)
                == Some(true)
        }) {
            for node in self.nodes[i..].iter() {
                node.widget().write_as(&mut *pa, |f: &mut File<U>| {
                    f.layout_order += 1;
                });
            }
            self.nodes.splice(i..i, nodes);
        } else {
            self.nodes.extend(nodes);
        }
    }

    /// An [`Iterator`] over the [`Node`]s in a [`Window`]
    pub(crate) fn nodes(&self) -> impl ExactSizeIterator<Item = &Node<U>> + DoubleEndedIterator {
        self.nodes.iter()
    }

    /// Returns an [`Iterator`] over the names of [`File`]s
    /// and their respective [`Widget`] indices
    ///
    /// [`Widget`]: crate::widget::Widget
    pub fn file_names(&self, pa: &Pass) -> Vec<String> {
        window_files(pa, &self.nodes)
            .into_iter()
            .map(|f| f.0.read_as(pa, |f: &File<U>| f.name()).unwrap())
            .collect()
    }

    /// Returns an [`Iterator`] over the paths of [`File`]s
    /// and their respective [`Widget`] indices
    ///
    /// [`Widget`]: crate::widget::Widget
    pub fn file_paths(&self, pa: &Pass) -> Vec<String> {
        window_files(pa, &self.nodes)
            .into_iter()
            .map(|f| f.0.read_as(pa, |f: &File<U>| f.name()).unwrap())
            .collect()
    }

    /// An [`Iterator`] over the [`File`] [`Node`]s in a [`Window`]
    pub(crate) fn file_nodes(&self, pa: &Pass) -> Vec<(RwData<File<U>>, FileId<U>)> {
        window_files(pa, &self.nodes)
    }

    /// How many [`Widget`]s are in this [`Window`]
    pub(crate) fn len_widgets(&self) -> usize {
        self.nodes.len()
    }
}

/// A dimension on screen, can either be horizontal or vertical
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    /// The horizontal axis
    Horizontal,
    /// The vertical axis
    Vertical,
}

impl Axis {
    /// The [`Axis`] perpendicular to this one
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

/// An event that Duat must handle
#[doc(hidden)]
pub enum DuatEvent {
    /// A [`KeyEvent`] was typed
    Key(KeyEvent),
    /// A function was queued
    QueuedFunction(Box<dyn FnOnce(Pass) + Send>),
    /// The application resized
    Resize,
    /// A [`Form`] was altered, which one it is, doesn't matter
    ///
    /// [`Form`]: crate::form::Form
    FormChange,
    /// Send a message from the Duat application to the config
    MetaMsg(Builder),
    /// Open a new [`File`]
    OpenFile(String),
    /// Close an open [`File`]
    CloseFile(String),
    /// Swap two [`File`]s
    SwapFiles(String, String),
    /// Open a new window with a [`File`]
    OpenWindow(String),
    /// Switch to the n'th window
    SwitchWindow(usize),
    /// Started a reload/recompilation at an [`Instant`]
    ReloadStarted(Instant),
    /// The Duat app sent a message to reload the config
    ReloadConfig,
    /// Quit Duat
    Quit,
}

impl Debug for DuatEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Key(key) => f.debug_tuple("Key").field(key).finish(),
            Self::Resize => write!(f, "Resize"),
            Self::FormChange => write!(f, "FormChange"),
            Self::MetaMsg(msg) => f.debug_tuple("MetaMsg").field(msg).finish(),
            Self::ReloadConfig => write!(f, "ReloadConfig"),
            Self::OpenFile(file) => f.debug_tuple("OpenFile").field(file).finish(),
            Self::CloseFile(file) => f.debug_tuple("CloseFile").field(file).finish(),
            Self::SwapFiles(lhs, rhs) => f.debug_tuple("SwapFiles").field(lhs).field(rhs).finish(),
            Self::OpenWindow(file) => f.debug_tuple("OpenWindow").field(file).finish(),
            Self::SwitchWindow(win) => f.debug_tuple("SwitchWindow").field(win).finish(),
            Self::ReloadStarted(instant) => f.debug_tuple("ReloadStarted").field(instant).finish(),
            Self::QueuedFunction(_) => f.debug_struct("InnerFunction").finish(),
            Self::Quit => write!(f, "Quit"),
        }
    }
}

/// A sender of [`DuatEvent`]s
pub struct Sender(&'static mpsc::Sender<DuatEvent>);

impl Sender {
    /// Returns a new [`Sender`]
    pub fn new(sender: &'static mpsc::Sender<DuatEvent>) -> Self {
        Self(sender)
    }

    /// Sends a [`KeyEvent`]
    pub fn send_key(&self, key: KeyEvent) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::Key(key))
    }

    /// Sends a notice that the app has resized
    pub fn send_resize(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::Resize)
    }

    /// Sends a notice that a [`Form`] has changed
    ///
    /// [`Form`]: crate::form::Form
    pub(crate) fn send_form_changed(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::FormChange)
    }
}

/// A read-only [`Window`]
pub struct RoWindow<'a, U: Ui>(&'a Window<U>);

impl<U: Ui> RoWindow<'_, U> {
    /// Similar to the [`Iterator::fold`] operatio, folding each
    /// [`&File`][File`] by applying an operatio,
    /// returning a final result.
    ///
    /// The reason why this is a `fold` operatio, and doesn't just
    /// return an [`Iterator`], is because `f` will act on a
    /// reference, as to not do unnecessary cloning of the widget's
    /// inner [`RwData<W>`], and because [`Iterator`]s cannot return
    /// references to themselves.
    pub fn fold_files<B>(&self, pa: &Pass, init: B, mut f: impl FnMut(B, &File<U>) -> B) -> B {
        self.0.nodes.iter().fold(init, |accum, node| {
            if node.data_is::<File<U>>() {
                node.read_as(pa, |file: &File<U>| f(accum, file)).unwrap()
            } else {
                accum
            }
        })
    }

    /// Similar to the [`Iterator::fold`] operatio, folding each
    /// [`&dyn Widget<U>`][Widget] by applying an
    /// operatio, returning a final result.
    ///
    /// The reason why this is a `fold` operatio, and doesn't just
    /// return an [`Iterator`], is because `f` will act on a
    /// reference, as to not do unnecessary cloning of the widget's
    /// inner [`RwData<W>`], and because [`Iterator`]s cannot return
    /// references to themselves.
    pub fn fold_widgets<B>(
        &self,
        pa: &Pass,
        init: B,
        mut f: impl FnMut(B, &dyn Widget<U>) -> B,
    ) -> B {
        self.0.nodes.iter().fold(init, |accum, node| {
            let f = &mut f;
            node.widget().read(pa, |widget| f(accum, widget))
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
/// Constraints are demands that must be met by the widget's
/// [`RawArea`], on a best effort basis.
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
    /// Push the [`Widget`] to the left
    pub const fn left() -> Self {
        Self {
            side: Side::Left,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Push the [`Widget`] to the right
    pub const fn right() -> Self {
        Self {
            side: Side::Right,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Push the [`Widget`] above
    pub const fn above() -> Self {
        Self {
            side: Side::Above,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Push the [`Widget`] below
    pub const fn below() -> Self {
        Self {
            side: Side::Below,
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Changes the direction of pushing to the left
    pub const fn to_left(self) -> Self {
        Self { side: Side::Left, ..self }
    }

    /// Changes the direction of pushing to the right
    pub const fn to_right(self) -> Self {
        Self { side: Side::Right, ..self }
    }

    /// Changes the direction of pushing to above
    pub const fn to_above(self) -> Self {
        Self { side: Side::Above, ..self }
    }

    /// Changes the direction of pushing to below
    pub const fn to_below(self) -> Self {
        Self { side: Side::Below, ..self }
    }

    /// Sets the required vertical length
    pub const fn with_ver_len(mut self, len: f32) -> Self {
        constrain(&mut self.ver_cons, Constraint::Len(len));
        self
    }

    /// Sets the minimum vertical length
    pub const fn with_ver_min(mut self, min: f32) -> Self {
        constrain(&mut self.ver_cons, Constraint::Min(min));
        self
    }

    /// Sets the maximum vertical length
    pub const fn with_ver_max(mut self, max: f32) -> Self {
        constrain(&mut self.ver_cons, Constraint::Max(max));
        self
    }

    /// Sets the vertical ratio between it and its parent
    pub const fn with_ver_ratio(mut self, den: u16, div: u16) -> Self {
        constrain(&mut self.ver_cons, Constraint::Ratio(den, div));
        self
    }

    /// Sets the required horizontal length
    pub const fn with_hor_len(mut self, len: f32) -> Self {
        constrain(&mut self.hor_cons, Constraint::Len(len));
        self
    }

    /// Sets the minimum horizontal length
    pub const fn with_hor_min(mut self, min: f32) -> Self {
        constrain(&mut self.hor_cons, Constraint::Min(min));
        self
    }

    /// Sets the maximum horizontal length
    pub const fn with_hor_max(mut self, max: f32) -> Self {
        constrain(&mut self.hor_cons, Constraint::Max(max));
        self
    }

    /// Sets the horizontal ratio between it and its parent
    pub const fn with_hor_ratio(mut self, den: u16, div: u16) -> Self {
        constrain(&mut self.hor_cons, Constraint::Ratio(den, div));
        self
    }

    /// The [`Axis`] where it will be pushed
    ///
    /// - left/right: [`Axis::Horizontal`]
    /// - above/below: [`Axis::Vertical`]
    pub const fn axis(&self) -> Axis {
        match self.side {
            Side::Above | Side::Below => Axis::Vertical,
            Side::Right | Side::Left => Axis::Horizontal,
        }
    }

    /// The [`Side`] where it will be pushed
    pub const fn side(&self) -> Side {
        self.side
    }

    /// Wether this "comes earlier" on the screen
    ///
    /// This returns true if `self.side() == Side::Left || self.side()
    /// == Side::Above`, since that is considered "earlier" on
    /// screens.
    pub const fn comes_earlier(&self) -> bool {
        matches!(self.side, Side::Left | Side::Above)
    }

    /// An [`Iterator`] over the vertical constraints
    pub fn ver_cons(&self) -> impl Iterator<Item = Constraint> + Clone {
        self.ver_cons.into_iter().flatten()
    }

    /// An [`Iterator`] over the horizontal constraints
    pub fn hor_cons(&self) -> impl Iterator<Item = Constraint> + Clone {
        self.hor_cons.into_iter().flatten()
    }

    /// The constraints on a given [`Axis`]
    pub fn cons_on(&self, axis: Axis) -> impl Iterator<Item = Constraint> {
        match axis {
            Axis::Horizontal => self.hor_cons.into_iter().flatten(),
            Axis::Vertical => self.ver_cons.into_iter().flatten(),
        }
    }

    /// Wether it is resizable in an [`Axis`]
    ///
    /// It will be resizable if there are no [`Constraint::Len`] in
    /// that [`Axis`].
    pub const fn is_resizable_on(&self, axis: Axis) -> bool {
        let cons = match axis {
            Axis::Horizontal => &self.hor_cons,
            Axis::Vertical => &self.ver_cons,
        };

        let mut i = 0;

        while i < 4 {
            let (None | Some(Constraint::Min(..) | Constraint::Max(..))) = cons[i] else {
                return false;
            };

            i += 1;
        }

        true
    }
}

/// Much like [`PushSpecs`], but for floating [`Widget`]s
#[derive(Debug, Clone)]
pub struct SpawnSpecs {
    /// Potential spawning [`Corner`]s to connect to and from
    pub choices: Vec<[Corner; 2]>,
    ver_cons: [Option<Constraint>; 4],
    hor_cons: [Option<Constraint>; 4],
}

impl SpawnSpecs {
    /// Returns a new [`SpawnSpecs`] from possible [`Corner`]s
    pub fn new(choices: impl IntoIterator<Item = [Corner; 2]>) -> Self {
        Self {
            choices: choices.into_iter().collect(),
            ver_cons: [None; 4],
            hor_cons: [None; 4],
        }
    }

    /// Adds more [`Corner`]s as fallback to spawn on
    pub fn with_fallbacks(mut self, choices: impl IntoIterator<Item = [Corner; 2]>) -> Self {
        self.choices.extend(choices);
        self
    }

    /// Sets the required vertical length
    pub fn with_ver_len(mut self, len: f32) -> Self {
        constrain(&mut self.ver_cons, Constraint::Len(len));
        self
    }

    /// Sets the minimum vertical length
    pub fn with_ver_min(mut self, min: f32) -> Self {
        constrain(&mut self.ver_cons, Constraint::Min(min));
        self
    }

    /// Sets the maximum vertical length
    pub fn with_ver_max(mut self, max: f32) -> Self {
        constrain(&mut self.ver_cons, Constraint::Max(max));
        self
    }

    /// Sets the vertical ratio between it and its parent
    pub fn with_ver_ratio(mut self, den: u16, div: u16) -> Self {
        constrain(&mut self.ver_cons, Constraint::Ratio(den, div));
        self
    }

    /// Sets the required horizontal length
    pub fn with_hor_len(mut self, len: f32) -> Self {
        constrain(&mut self.hor_cons, Constraint::Len(len));
        self
    }

    /// Sets the minimum horizontal length
    pub fn with_hor_min(mut self, min: f32) -> Self {
        constrain(&mut self.hor_cons, Constraint::Min(min));
        self
    }

    /// Sets the maximum horizontal length
    pub fn with_hor_max(mut self, max: f32) -> Self {
        constrain(&mut self.hor_cons, Constraint::Max(max));
        self
    }

    /// Sets the horizontal ratio between it and its parent
    pub fn with_hor_ratio(mut self, den: u16, div: u16) -> Self {
        constrain(&mut self.hor_cons, Constraint::Ratio(den, div));
        self
    }

    /// An [`Iterator`] over the vertical [`Constraint`]s
    pub fn ver_cons(&self) -> impl Iterator<Item = Constraint> {
        self.ver_cons.into_iter().flatten()
    }

    /// An [`Iterator`] over the horizontal [`Constraint`]s
    pub fn hor_cons(&self) -> impl Iterator<Item = Constraint> {
        self.hor_cons.into_iter().flatten()
    }

    /// The constraints on a given [`Axis`]
    pub fn cons_on(&self, axis: Axis) -> impl Iterator<Item = Constraint> {
        match axis {
            Axis::Horizontal => self.hor_cons.into_iter().flatten(),
            Axis::Vertical => self.ver_cons.into_iter().flatten(),
        }
    }

    /// Wether it is resizable in an [`Axis`]
    ///
    /// It will be resizable if there are no [`Constraint::Len`] in
    /// that [`Axis`].
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

/// A constraint used to determine the size of [`Widget`]s
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    /// Constrain this dimension to a certain length
    Len(f32),
    /// The length in this dimension must be at least this long
    Min(f32),
    /// The length in this dimension must be at most this long
    Max(f32),
    /// The length in this dimension should be this fraction of its
    /// parent
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
    /// Put the [`Widget`] above another
    Above,
    /// Put the [`Widget`] on the right
    Right,
    /// Put the [`Widget`] on the left
    Below,
    /// Put the [`Widget`] below another
    Left,
}

/// A corner to attach a [`Widget`] to and from
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Corner {
    /// Attach on/from the top left corner
    TopLeft,
    /// Attach on/from the top
    Top,
    /// Attach on/from the top right corner
    TopRight,
    /// Attach on/from the right
    Right,
    /// Attach on/from the bottom right corner
    BottomRight,
    /// Attach on/from the bottom
    Bottom,
    /// Attach on/from the bottom left  corner
    BottomLeft,
    /// Attach on/from the left
    Left,
    /// Attach on/from the center
    Center,
}

/// A struct representing a "visual position" on the screen
///
/// This position differs from a [`VPoint`] in the sense that it
/// represents three properties of a printed character:
///
/// - The x position in which it was printed;
/// - The amount of horizontal space it occupies;
/// - Wether this character is the first on the line (i.e. it wraps)
#[derive(Debug, Clone, Copy)]
pub struct Caret {
    /// The horizontal position in which a character was printed
    pub x: u32,
    /// The horizontal space it occupied
    pub len: u32,
    /// Wether it is the first character in the line
    pub wrap: bool,
}

impl Caret {
    /// Returns a new [`Caret`]
    #[inline(always)]
    pub fn new(x: u32, len: u32, wrap: bool) -> Self {
        Self { x, len, wrap }
    }
}

const fn constrain(cons: &mut [Option<Constraint>; 4], con: Constraint) {
    let mut i = 0;

    while i < 4 {
        i += 1;

        cons[i - 1] = match (cons[i - 1], con) {
            (None, _)
            | (Some(Constraint::Len(_)), Constraint::Len(_))
            | (Some(Constraint::Min(_)), Constraint::Min(_))
            | (Some(Constraint::Max(_)), Constraint::Max(_))
            | (Some(Constraint::Ratio(..)), Constraint::Ratio(..)) => Some(con),
            _ => continue,
        };

        break;
    }
}

/// A struct used to modify the layout of [`RawArea`]s
///
/// The end user should not have access to methods that directly
/// modify the layout, like [`RawArea::delete`] or
/// [`RawArea::bisect`], since they will modify the layout without
/// any coordination with the rest of Duat, so this struct is used to
/// "hide" those methods, in order to prevent users from directly
/// accessing them.
///
/// Higher lever versions of these methods are still available to the
/// end user, in the more controled APIs of [`Area`]
pub struct MutArea<'area, A: RawArea>(pub(crate) &'area A);

impl<A: RawArea> MutArea<'_, A> {
    /// Bisects the [`RawArea`] in two
    pub fn bisect(
        self,
        specs: PushSpecs,
        cluster: bool,
        on_files: bool,
        cache: A::Cache,
    ) -> (A, Option<A>) {
        A::bisect(self, specs, cluster, on_files, cache)
    }

    /// Calls [`RawArea::delete`] on `self`
    pub fn delete(self) -> Option<A> {
        A::delete(self)
    }

    /// Calls [`RawArea::swap`] on `self`
    pub fn swap(self, other: &A) {
        A::swap(self, other);
    }

    /// Calls [`RawArea::spawn_floating`] on `self`
    pub fn spawn_floating(self, specs: SpawnSpecs) -> Result<A, Text> {
        A::spawn_floating(self, specs)
    }

    /// Calls [`RawArea::spawn_floating_at`] on `self`
    pub fn spawn_floating_at(
        self,
        specs: SpawnSpecs,
        at: impl TwoPoints,
        text: &Text,
        cfg: PrintCfg,
    ) -> Result<A, Text> {
        A::spawn_floating_at(self, specs, at, text, cfg)
    }
}

impl<A: RawArea> std::ops::Deref for MutArea<'_, A> {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// A public API for [`Ui::Area`]
#[derive(Clone, PartialEq)]
pub struct Area<U: Ui> {
    area: U::Area,
}

impl<U: Ui> std::ops::Deref for Area<U> {
    type Target = U::Area;

    fn deref(&self) -> &Self::Target {
        &self.area
    }
}
