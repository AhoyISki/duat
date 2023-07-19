#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{
    fmt::Debug,
    sync::atomic::{AtomicBool, Ordering}
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use crate::{
    commands::{CommandErr, Commands},
    data::{ReadableData, RoData, RwData},
    forms::FormPalette,
    position::Pos,
    text::{PrintCfg, Text, TextBit},
    widgets::{FileWidget, Widget, WidgetType},
    Controler
};

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
        PushSpecs { side: Side::Left, constraint: Some(constraint) }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn right(constraint: Constraint) -> Self {
        PushSpecs { side: Side::Right, constraint: Some(constraint) }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn above(constraint: Constraint) -> Self {
        PushSpecs { side: Side::Above, constraint: Some(constraint) }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn below(constraint: Constraint) -> Self {
        PushSpecs { side: Side::Below, constraint: Some(constraint) }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn left_free() -> Self {
        PushSpecs { side: Side::Left, constraint: None }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn right_free() -> Self {
        PushSpecs { side: Side::Right, constraint: None }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn above_free() -> Self {
        PushSpecs { side: Side::Above, constraint: None }
    }

    /// Returns a new instance of [`PushSpecs`].
    pub fn below_free() -> Self {
        PushSpecs { side: Side::Below, constraint: None }
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
pub trait Area: Clone + PartialEq + Send + Sync {
    type PrintInfo: PrintInfo;
    type ConstraintChangeErr: Debug;

    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    fn set_as_active(&self);

    /// Prints the [`Text`][crate::text::Text] via an [`Iterator`].
    fn print(&self, text: &Text, info: Self::PrintInfo, cfg: PrintCfg, palette: &FormPalette);

    fn change_constraint(&self, constraint: Constraint) -> Result<(), Self::ConstraintChangeErr>;

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), Self::ConstraintChangeErr>;

    //////////////////// Queries
    /// The amount of rows of the screen that the [`Iterator`] takes
    /// up.
    ///
    /// Must take the [`PrintCfg`] into account, as in, if the
    /// [`WrapMethod`][crate::text::WrapMethod] is
    /// [`NoWrap`][crate::text::WrapMethod::NoWrap],
    /// then the number of rows must equal the number of lines on the
    /// [`Iterator`].
    fn visible_rows(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize;

    /// Returns the positional index of the char that comes after the
    /// [`TextBit`][crate::text::TextBit] [`Iterator`] wraps `wrap`
    /// times.
    fn char_at_wrap(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> Option<usize>;

    /// Gets the visual width of the [`Iterator`].
    fn get_width(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, wrap_around: bool
    ) -> usize;

    /// Gets the column at `dist` from the left side on [`Iterator`].
    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize;

    fn has_changed(&self) -> bool;

    fn is_senior_of(&self, other: &Self) -> bool;

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
    fn bisect(&self, specs: PushSpecs, is_glued: bool) -> (Self, Option<Self>);
}

/// Elements related to the [`Widget<U>`]s.
pub struct Node<U>
where
    U: Ui
{
    widget_type: WidgetType<U>,
    checker: Box<dyn Fn() -> bool>,
    area: U::Area,
    file_id: Option<FileId>,
    not_updated: AtomicBool
}

impl<U> Node<U>
where
    U: Ui
{
    pub fn needs_update(&self) -> bool {
        let not_updated = self.not_updated.fetch_and(false, Ordering::Acquire);
        let widget_changed = self.widget_type.has_changed();
        let area_changed = self.area.has_changed();
        (self.checker)() || widget_changed || area_changed || not_updated
    }

    pub fn try_update_and_print<'scope, 'env>(
        &'env self, scope: &'scope std::thread::Scope<'scope, 'env>, palette: &'env FormPalette
    ) {
        let succeeded = self.widget_type.try_update_and_print(scope, &self.area, palette);
        self.not_updated.store(!succeeded, Ordering::Release);
    }
}

unsafe impl<U> Send for Node<U> where U: Ui {}

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
    controler: &'a mut Controler<U>,
    is_file: bool,
    mod_area: RwLock<U::Area>
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
    pub fn push<F>(
        &self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let file_id = *crate::CMD_FILE_ID.lock().unwrap();
        let (widget, checker, _) = f(self.controler);
        let (child, parent) = self.controler.mutate_active_window(|window| {
            let mod_area = self.mod_area.read().unwrap();
            let (child, parent) = window.push(widget, &*mod_area, checker, specs, file_id, true);

            if let (Some(parent), true) = (&parent, self.is_file) {
                if parent.is_senior_of(&window.files_region) {
                    window.files_region = parent.clone();
                }
            }

            (child, parent)
        });

        if let Some(parent) = &parent {
            *self.mod_area.write().unwrap() = parent.clone();
        }

        (child, parent)
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
    pub fn push_to<F>(
        &self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), area: U::Area,
        specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let file_id = *crate::CMD_FILE_ID.lock().unwrap();
        let (widget, checker, _) = f(self.controler);
        let (child, parent) = self.controler.mutate_active_window(|window| {
            window.push(widget, &area, checker, specs, file_id, true)
        });

        (child, parent)
    }

    pub fn push_specd<F>(
        &self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs)
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let file_id = *crate::CMD_FILE_ID.lock().unwrap();
        let (widget, checker, specs) = (f)(self.controler);
        let (child, parent) = self.controler.mutate_active_window(|window| {
            let mod_area = self.mod_area.read().unwrap();
            let (child, parent) = window.push(widget, &*mod_area, checker, specs, file_id, true);

            // If a new parent is created, and it owns the old `files_region`
            // (.i.e all files), then it must become the new files_region.
            if let (Some(parent), true) = (&parent, self.is_file) {
                if parent.is_senior_of(&window.files_region) {
                    window.files_region = parent.clone();
                }
            }

            (child, parent)
        });

        if let Some(parent) = &parent {
            *self.mod_area.write().unwrap() = parent.clone();
        }

        (child, parent)
    }

    pub fn palette(&self) -> &FormPalette {
        &self.controler.palette
    }

    pub fn commands(&self) -> &RwData<Commands> {
        &self.controler.commands
    }

    pub fn run_cmd(&self, cmd: impl ToString) -> Result<Option<String>, CommandErr> {
        self.controler.run_cmd(cmd)
    }
}

pub(crate) fn activate_hook<U, W>(
    controler: &mut Controler<U>, mod_area: U::Area,
    constructor_hook: &mut dyn FnMut(ModNode<U>, RoData<W>)
) where
    U: Ui,
    W: Widget<U>
{
    let (widget, old_file, old_file_id) = controler.inspect_active_window(|window| {
        let node = window.nodes.iter().find(|Node { area, .. }| *area == mod_area).unwrap();

        let old_file_id =
            node.file_id.and_then(|file_id| crate::CMD_FILE_ID.lock().unwrap().replace(file_id));

        let old_file = node.widget_type.downcast_ref::<FileWidget<U>>().map(|file| {
            std::mem::replace(&mut *controler.active_file.write(), RoData::from(&file))
        });

        let widget = RoData::from(&node.widget_type.downcast_ref::<W>().unwrap());

        (widget, old_file, old_file_id)
    });

    let mod_node = ModNode {
        controler,
        is_file: std::any::TypeId::of::<W>() == std::any::TypeId::of::<FileWidget<U>>(),
        mod_area: RwLock::new(mod_area)
    };

    (constructor_hook)(mod_node, widget);

    *crate::CMD_FILE_ID.lock().unwrap() = old_file_id;
    if let Some(file) = old_file {
        *controler.active_file.write() = file;
    };
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
        if let Side::Above | Side::Below = value.side { Axis::Vertical } else { Axis::Horizontal }
    }
}

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: Default + 'static {
    type PrintInfo: PrintInfo<Area = Self::Area>;
    type ConstraintChangeErr: Debug;
    type Area: Area<PrintInfo = Self::PrintInfo, ConstraintChangeErr = Self::ConstraintChangeErr>;

    /// Initiates and returns a new "master" [`Area`][Ui::Area].
    ///
    /// This [`Area`][Ui::Area] must not have any parents, and must be
    /// the located on a new window, that is, a plain region with
    /// nothing in it.
    fn new_window(&mut self) -> Self::Area;

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);
}

/// A container for a master [`Area`] in Parsec.
pub struct Window<U>
where
    U: Ui
{
    nodes: Vec<Node<U>>,
    files_region: U::Area,
    master_area: U::Area
}

impl<U> Window<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of [`Window<U>`].
    pub fn new<Checker>(ui: &mut U, widget_type: WidgetType<U>, checker: Checker) -> (Self, U::Area)
    where
        Checker: Fn() -> bool + 'static
    {
        let area = ui.new_window();
        widget_type.update(&area);
        let main_node = Node {
            widget_type,
            checker: Box::new(checker),
            area: area.clone(),
            file_id: Some(unique_file_id()),
            not_updated: AtomicBool::new(false)
        };

        *crate::CMD_FILE_ID.lock().unwrap() = main_node.file_id;

        let parsec_window = Self {
            nodes: vec![main_node],
            files_region: area.clone(),
            master_area: area.clone()
        };

        (parsec_window, area)
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    pub fn push<Checker>(
        &mut self, widget_type: WidgetType<U>, area: &U::Area, checker: Checker, specs: PushSpecs,
        file_id: Option<FileId>, is_glued: bool
    ) -> (U::Area, Option<U::Area>)
    where
        Checker: Fn() -> bool + 'static
    {
        let (child, parent) = area.bisect(specs, is_glued);

        let node = Node {
            widget_type,
            checker: Box::new(checker),
            area: child.clone(),
            file_id,
            not_updated: AtomicBool::new(false)
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
        &mut self, widget_type: WidgetType<U>, specs: PushSpecs
    ) -> (U::Area, Option<U::Area>) {
        let area = self.files_region.clone();

        let checker = || false;
        let file_id = Some(unique_file_id());
        let (child, parent) = self.push(widget_type, &area, checker, specs, file_id, false);
        if let Some(parent) = &parent {
            self.files_region = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to the master node of the current
    /// window.
    pub fn push_to_master<Checker>(
        &mut self, widget_type: WidgetType<U>, checker: Checker, specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        Checker: Fn() -> bool + 'static
    {
        let master_area = self.master_area.clone();
        self.push(widget_type, &master_area, checker, specs, None, false)
    }

    /// Returns an [`Iterator`] over the [`Widget<U>`]s of [`self`].
    pub fn widgets(
        &self
    ) -> impl Iterator<Item = (&WidgetType<U>, &U::Area, Option<FileId>)> + Clone + '_ {
        self.nodes
            .iter()
            .map(|Node { widget_type, area, file_id, .. }| (widget_type, area, *file_id))
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node<U>> {
        self.nodes.iter()
    }

    /// Returns an [`Iterator`] over the names of [`FileWidget<U>`]s
    /// and their respective [`ActionableWidget`] indices.
    pub fn file_names(&self) -> impl Iterator<Item = (usize, String)> + Clone + '_ {
        self.nodes.iter().enumerate().filter_map(|(pos, Node { widget_type, .. })| {
            widget_type.downcast_ref::<FileWidget<U>>().map(|file| {
                let name = file.read().name().unwrap_or_else(|| format!("*scratch file*"));
                (pos, name)
            })
        })
    }
}

pub struct RoWindow<'a, U>(&'a Window<U>)
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
        self.0.nodes.iter().fold(init, |accum, Node { widget_type, .. }| {
            if let Some(file) = widget_type.downcast_ref::<FileWidget<U>>() {
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
        self.0.nodes.iter().fold(init, |accum, Node { widget_type: widget, .. }| {
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
    U: Ui
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
