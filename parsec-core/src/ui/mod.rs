mod builder;

use std::{
    fmt::Debug,
    sync::atomic::{AtomicBool, Ordering},
};

use crossterm::event::KeyEvent;

pub use self::builder::{FileBuilder, WindowBuilder};
use crate::{
    data::{ReadableData, RoData, RwData},
    forms::FormPalette,
    position::Point,
    text::{Item, IterCfg, PrintCfg, Text},
    widgets::{FileWidget, PassiveWidget, Widget},
    Controler,
};

/// A direction, where a [`Widget<U>`] will be placed in relation to
/// another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Side {
    Above,
    Right,
    Below,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constraint {
    Ratio(u16, u16),
    Percent(u16),
    Length(f64),
    Min(f64),
    Max(f64),
}

/// Information on how a [`Widget<U>`] should be pushed onto another.
///
/// The side member determines what direction to push into, in
/// relation to the original widget.
///
/// The [`Constraint`] can be one of five types:
///
/// - [`Min(min)`][Constraint::Min] represents the minimum length, in the side's
///   [`Axis`], that this new widget needs.
/// - [`Max(max)`][Constraint::Max] represents the minimum length, in the side's
///   [`Axis`], that this new widget needs.
/// - [`Length(len)`][Constraint::Length] represents a length, in the side's
///   [`Axis`], that cannot be altered by any means.
/// - [`Ratio(den, div)`][Constraint::Ratio] represents a ratio between the
///   length of the child and the length of the parent.
/// - [`Percent(per)`][Constraint::Percent] represents the percent of the parent
///   that the child must take. Must go from 0 to 100 percent.
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
    constraint: Option<Constraint>,
}

impl PushSpecs {
    /// Returns a new instance of [`PushSpecs`].
    pub fn left() -> Self {
        Self {
            side: Side::Left,
            constraint: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn right() -> Self {
        Self {
            side: Side::Right,
            constraint: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn above() -> Self {
        Self {
            side: Side::Above,
            constraint: None,
        }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn below() -> Self {
        Self {
            side: Side::Below,
            constraint: None,
        }
    }

    pub fn with_lenght(self, len: f64) -> Self {
        Self {
            constraint: Some(Constraint::Length(len)),
            ..self
        }
    }

    pub fn with_minimum(self, min: f64) -> Self {
        Self {
            constraint: Some(Constraint::Min(min)),
            ..self
        }
    }

    pub fn with_maximum(self, max: f64) -> Self {
        Self {
            constraint: Some(Constraint::Max(max)),
            ..self
        }
    }

    pub fn with_percent(self, percent: u16) -> Self {
        Self {
            constraint: Some(Constraint::Percent(percent)),
            ..self
        }
    }

    pub fn with_ratio(self, den: u16, div: u16) -> Self {
        Self {
            constraint: Some(Constraint::Ratio(den, div)),
            ..self
        }
    }

    pub fn comes_earlier(&self) -> bool {
        matches!(self.side, Side::Left | Side::Above)
    }

    pub fn constraint(&self) -> Option<Constraint> {
        self.constraint
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Caret {
    pub x: usize,
    pub len: usize,
    pub wrap: bool,
}

impl Caret {
    pub fn new(x: usize, len: usize, wrap: bool) -> Self {
        Self { x, len, wrap }
    }
}

/// An [`Area`] that supports printing [`Text`].
///
/// These represent the entire GUI of Parsec, the only parts of the
/// screen where text may be printed.
pub trait Area: Clone + PartialEq + Send + Sync {
    type ConstraintChangeErr: Debug;

    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Scrolls the [`Text`] (up or down) until the main cursor is
    /// within the [`ScrollOff`][crate::text::ScrollOff] range.
    fn scroll_around_point(&mut self, text: &Text, point: Point, cfg: &PrintCfg);

    /// Returns the character index of the first character that would
    /// be printed.
    fn first_char(&self) -> usize;

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    fn set_as_active(&self);

    /// Prints the [`Text`][crate::text::Text] via an [`Iterator`].
    fn print(&self, text: &Text, cfg: &PrintCfg, palette: &FormPalette);

    fn change_constraint(&self, constraint: Constraint) -> Result<(), Self::ConstraintChangeErr>;

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), Self::ConstraintChangeErr>;

    //////////////////// Queries
    /// Wether or not [`self`] has changed.
    ///
    /// This would mean anything relevant that wouldn't be determined
    /// by [`PrintInfo`], this is most likely going to be the bounding
    /// box, but it may be something else.
    fn has_changed(&self) -> bool;

    /// Wether or not [`self`] has seniority over `other`
    ///
    /// This can only happen if, by following [`self`]'s children, you
    /// would eventually reach `other`.
    fn is_senior_of(&self, other: &Self) -> bool;

    /// Returns a printing iterator.
    ///
    /// Given an [`Iterator`] with an [`Item`] of type `(usize,
    /// usize, Part)`, where:
    ///
    /// - The first `usize` is the char index from the file's start;
    /// - The second `usize` is the current line;
    /// - The [`Part`] is either a `char` or a [`Text`] modifier;
    ///
    /// Returns an [`Iterator`] with an [`Item`] of type `((usize,
    /// usize, Option<usize>), (usize, Part))`, where:
    ///
    /// * On the first tuple:
    ///   - The first `usize` is the current horizontal position;
    ///   - The second `usize` is the length of the [`Part`]. It is only greater
    ///     than 0 if the part is a `char`;
    ///   - The [`Option<usize>`] represents a wrapping. It is [`Some(usize)`],
    ///     where the number is the current line, only if the `char` wraps
    ///     around. For example, any `char` following a `'\n'` should return
    ///     `Some(current_line)`, since they show up in the next line;
    ///
    /// * On the second tuple:
    ///   - The `usize` is the char index from the file's start;
    ///   - The [`Part`] is either a `char` or a [`Text`] modifier;
    ///
    /// [`Item`]: Iterator::Item
    /// [`Option<usize>`]: Option
    /// [`Some(usize)`]: Some
    fn print_iter<'a>(
        &self,
        iter: impl Iterator<Item = Item> + Clone + 'a,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a;

    fn precise_print_iter<'a>(
        &self,
        iter: impl Iterator<Item = Item> + Clone + 'a,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a;

    /// Returns a reverse printing iterator.
    ///
    /// Given an [`Iterator`] with an [`Item`] of type `(usize,
    /// usize, Part)`, where:
    ///
    /// - The first `usize` is the char index from the file's start;
    /// - The second `usize` is the current line;
    /// - The [`Part`] is either a `char` or a [`Text`] modifier;
    ///
    /// Returns an [`Iterator`] with an [`Item`] of type `((usize,
    /// usize, Option<usize>), (usize, Part))`, where:
    ///
    /// * On the first tuple:
    ///   - The first `usize` is the current horizontal position;
    ///   - The second `usize` is the length of the [`Part`]. It is only greater
    ///     than 0 if the part is a `char`;
    ///   - The [`Option<usize>`] represents a wrapping. It is [`Some(usize)`],
    ///     where the number is the current line, only if the `char` wraps
    ///     around. For example, any `char` following a `'\n'` should return
    ///     `Some(current_line)`, since they show up in the next line;
    ///
    /// * On the second tuple:
    ///   - The `usize` is the char index from the file's start;
    ///   - The [`Part`] is either a `char` or a [`Text`] modifier;
    ///
    /// [`Item`]: Iterator::Item
    /// [`Option<usize>`]: Option
    /// [`Some(usize)`]: Some
    fn rev_print_iter<'a>(
        &self,
        iter: impl Iterator<Item = Item> + Clone + 'a,
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
    fn bisect(&self, specs: PushSpecs, cluster: bool) -> (Self, Option<Self>);
}

/// Elements related to the [`Widget<U>`]s.
pub struct Node<U>
where
    U: Ui,
{
    widget: Widget<U>,
    checker: Box<dyn Fn() -> bool>,
    area: U::Area,
    file_id: Option<FileId>,
    busy_updating: AtomicBool,
    update_scheduled: AtomicBool,
}

impl<U> Node<U>
where
    U: Ui,
{
    pub fn needs_update(&self) -> bool {
        let widget_changed = self.widget.has_changed();
        let area_changed = self.area.has_changed();
        let needs_update = (self.checker)() || widget_changed || area_changed;

        let busy_updating = self.busy_updating.fetch_or(needs_update, Ordering::Release);

        let update_scheduled = if busy_updating {
            self.update_scheduled
                .fetch_or(needs_update, Ordering::Release)
        } else {
            false
        };

        (needs_update || update_scheduled) && !busy_updating
    }

    pub fn update_and_print(&self, palette: &FormPalette) {
        self.widget.update_and_print(&self.area, palette);

        self.busy_updating.store(false, Ordering::Release);
    }
}

unsafe impl<U> Send for Node<U> where U: Ui {}
unsafe impl<U> Sync for Node<U> where U: Ui {}

/// A dimension on screen, can either be horizontal or vertical.
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

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: Sized + Default + 'static {
    type ConstraintChangeErr: Debug;
    type Area: Area<ConstraintChangeErr = Self::ConstraintChangeErr>;

    /// Initiates and returns a new "master" [`Area`].
    ///
    /// This [`Area`] must not have any parents, and must be placed on a new
    /// window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    fn new_root(&mut self) -> Self::Area;

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);
}

/// A container for a master [`Area`] in Parsec.
pub struct Window<U>
where
    U: Ui,
{
    nodes: Vec<Node<U>>,
    files_region: U::Area,
    master_area: U::Area,
}

impl<U> Window<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of [`Window<U>`].
    pub fn new(
        ui: &mut U,
        widget: Widget<U>,
        checker: impl Fn() -> bool + 'static,
    ) -> (Self, U::Area) {
        let area = ui.new_root();
        widget.update(&area);
        let main_node = Node {
            widget,
            checker: Box::new(checker),
            area: area.clone(),
            file_id: Some(unique_file_id()),
            busy_updating: AtomicBool::new(false),
            update_scheduled: AtomicBool::new(false),
        };

        *crate::CMD_FILE_ID.lock().unwrap() = main_node.file_id;

        let parsec_window = Self {
            nodes: vec![main_node],
            files_region: area.clone(),
            master_area: area.clone(),
        };

        (parsec_window, area)
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    pub fn push(
        &mut self,
        widget: Widget<U>,
        area: &U::Area,
        checker: impl Fn() -> bool + 'static,
        specs: PushSpecs,
        file_id: Option<FileId>,
        cluster: bool,
    ) -> (U::Area, Option<U::Area>) {
        let (child, parent) = area.bisect(specs, cluster);

        let node = Node {
            widget,
            checker: Box::new(checker),
            area: child.clone(),
            file_id,
            busy_updating: AtomicBool::new(false),
            update_scheduled: AtomicBool::new(false),
        };

        if *area == self.master_area && let Some(new_master_node) = parent.clone() {
            self.master_area = new_master_node;
        }

        self.nodes.push(node);
        (child, parent)
    }

    /// Pushes a [`FileWidget<U>`] to the file's parent.
    ///
    /// This function will push to the edge of `self.files_parent`.
    /// This is an area, usually in the center, that contains all
    /// [`FileWidget<U>`]s, and their associated [`Widget<U>`]s,
    /// with others being at the perifery of this area.
    pub fn push_file(
        &mut self,
        widget: Widget<U>,
        checker: impl Fn() -> bool + 'static,
        specs: PushSpecs,
    ) -> (U::Area, Option<U::Area>) {
        let area = self.files_region.clone();

        let file_id = Some(unique_file_id());
        let (child, parent) = self.push(widget, &area, checker, specs, file_id, false);
        if let Some(parent) = &parent {
            self.files_region = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to the master node of the current
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
        self.push(widget, &master_area, checker, specs, None, false)
    }

    /// Returns an [`Iterator`] over the [`Widget<U>`]s of [`self`].
    pub fn widgets(
        &self,
    ) -> impl Iterator<Item = (&Widget<U>, &U::Area, Option<FileId>)> + Clone + '_ {
        self.nodes.iter().map(
            |Node {
                 widget,
                 area,
                 file_id,
                 ..
             }| (widget, area, *file_id),
        )
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node<U>> {
        self.nodes.iter()
    }

    /// Returns an [`Iterator`] over the names of [`FileWidget<U>`]s
    /// and their respective [`ActionableWidget`] indices.
    pub fn file_names(&self) -> impl Iterator<Item = (usize, String)> + Clone + '_ {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(pos, Node { widget, .. })| {
                widget.downcast_ref::<FileWidget>().map(|file| {
                    let name = file
                        .read()
                        .name()
                        .unwrap_or_else(|| format!("*scratch file*"));
                    (pos, name)
                })
            })
    }

    pub fn send_key(&self, key: KeyEvent, controler: &Controler<U>) {
        if let Some(node) = self
            .nodes()
            .find(|node| node.widget.ptr_eq(&*controler.active_widget.read()))
        {
            node.widget.send_key(key, &node.area, controler)
        }
    }

    pub(crate) fn files_region(&self) -> &U::Area {
        &self.files_region
    }
}

pub struct RoWindow<'a, U>(&'a Window<U>)
where
    U: Ui;

impl<'a, U> RoWindow<'a, U>
where
    U: Ui,
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
    pub fn fold_files<B>(&self, init: B, mut f: impl FnMut(B, &FileWidget) -> B) -> B {
        self.0
            .nodes
            .iter()
            .fold(init, |accum, Node { widget, .. }| {
                if let Some(file) = widget.downcast_ref::<FileWidget>() {
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
    pub fn fold_widgets<B>(&self, init: B, mut f: impl FnMut(B, &dyn PassiveWidget) -> B) -> B {
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
            .map(|windows| windows.get(index).map(|window| f(RoWindow(window))))
            .ok()
            .flatten()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FileId(u16);

fn unique_file_id() -> FileId {
    use std::sync::atomic::AtomicU16;
    static COUNTER: AtomicU16 = AtomicU16::new(0);

    FileId(COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub(crate) fn build_file<U>(
    controler: &mut Controler<U>,
    mod_area: U::Area,
    f: &mut impl FnMut(&mut FileBuilder<U>, &RwData<FileWidget>),
) where
    U: Ui,
{
    let (widget, old_file, old_file_id) = controler.inspect_active_window(|window| {
        let node = window
            .nodes
            .iter()
            .find(|Node { area, .. }| *area == mod_area)
            .unwrap();

        let old_file_id = node
            .file_id
            .and_then(|file_id| crate::CMD_FILE_ID.lock().unwrap().replace(file_id));

        let old_file = node
            .widget
            .downcast_ref::<FileWidget>()
            .map(|file| std::mem::replace(&mut *controler.active_file.write(), file));

        let widget = node.widget.downcast_ref::<FileWidget>().unwrap();

        (widget, old_file, old_file_id)
    });

    let mut file_builder = FileBuilder::new(controler, mod_area);

    f(&mut file_builder, &widget);

    *crate::CMD_FILE_ID.lock().unwrap() = old_file_id;
    if let Some(file) = old_file {
        *controler.active_file.write() = file;
    };
}
