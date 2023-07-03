use std::{
    fmt::Debug,
    sync::{atomic::AtomicUsize, Arc, RwLock}
};

use crate::{
    data::{RawReadableData, ReadableData, RoData, RwData},
    position::Pos,
    tags::form::FormPalette,
    text::{PrintCfg, Text, TextBit},
    widgets::{FileWidget, Widget},
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
pub trait Area: Clone + Send + Sync + PartialEq {
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
    fn set_as_active(&self);

    /// Prints the [`Text`][crate::text::Text] via an [`Iterator`].
    fn print(&self, text: &Text, info: Self::PrintInfo, cfg: PrintCfg, palette: &FormPalette);

    fn change_constraint(&self, constraint: Constraint) -> Result<(), ()>;

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

    fn has_changed(&self) -> bool;

    fn is_senior_of(&self, other: &Self) -> bool;
}

/// Elements related to the [`Widget<U>`]s.
pub struct Node<U>
where
    U: Ui
{
    widget: RwData<dyn Widget<U>>,
    checker: Box<dyn Fn() -> bool>,
    area: U::Area,
    file_id: Option<usize>
}

impl<U> Node<U>
where
    U: Ui
{
    pub fn needs_update(&self) -> bool {
        let widget_changed = self.widget.has_changed();
        let area_changed = self.area.has_changed();
        (self.checker)() || widget_changed || area_changed
    }

    pub fn update(&self) {
        self.widget.write().update(&self.area);
    }

    pub fn print(&self, palette: &FormPalette) {
        let widget = self.widget.read();
        widget.print(&self.area, palette)
    }

    pub fn is_slow(&self) -> bool {
        self.widget.read().is_slow()
    }
}

unsafe impl<U> Send for Node<U> where U: Ui {}
unsafe impl<U> Sync for Node<U> where U: Ui {}

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
    pub fn push<W, F>(
        &self, f: impl FnOnce(&Controler<U>) -> (W, F, PushSpecs), specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        W: Widget<U>,
        F: Fn() -> bool + 'static
    {
        let (widget, checker, _) = f(self.controler);
        let file_id = self.controler.commands.read().file_id;
        let (child, parent) = self.controler.mutate_active_window(|window| {
            let mod_area = self.mod_area.read().unwrap();
            let (child, parent) = window.push(&*mod_area, widget, checker, specs, file_id, true);

            if let (Some(parent), true) = (&parent, self.is_file) {
                if parent.is_senior_of(&window.files_area) {
                    window.files_area = parent.clone();
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
    pub fn push_to<W, F>(
        &self, f: impl FnOnce(&Controler<U>) -> (W, F, PushSpecs), area: U::Area, specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        W: Widget<U>,
        F: Fn() -> bool + 'static
    {
        let (widget, checker, _) = f(self.controler);
        let file_id = self.controler.commands.read().file_id;
        let (child, parent) = self.controler.windows.mutate(|windows| {
            let window = &mut windows[self.controler.active_window];
            window.push(&area, widget, checker, specs, file_id, true)
        });

        (child, parent)
    }

    pub fn push_specd<W, F>(
        &self, f: impl FnOnce(&Controler<U>) -> (W, F, PushSpecs)
    ) -> (U::Area, Option<U::Area>)
    where
        W: Widget<U>,
        F: Fn() -> bool + 'static
    {
        let (widget, checker, specs) = (f)(self.controler);
        let file_id = self.controler.commands.read().file_id;
        let (child, parent) = self.controler.mutate_active_window(|window| {
            let mod_area = self.mod_area.read().unwrap();
            let (child, parent) = window.push(&*mod_area, widget, checker, specs, file_id, true);

            if let (Some(parent), true) = (&parent, self.is_file) {
                if parent.is_senior_of(&window.files_area) {
                    window.files_area = parent.clone();
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

    pub fn manager(&self) -> &Controler<U> {
        &self.controler
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

        let old_file_id = node
            .file_id
            .map(|file_id| controler.commands.write().file_id.replace(file_id))
            .flatten();

        let old_file = node.widget.clone().try_downcast::<FileWidget<U>>().map(|file| {
            std::mem::replace(&mut *controler.active_file.write(), RoData::from(&file))
        }).ok();

        let widget = RoData::from(&node.widget.clone().try_downcast::<W>().unwrap());

        (widget, old_file, old_file_id)
    });

    let mod_node = ModNode {
        controler,
        is_file: std::any::TypeId::of::<W>() == std::any::TypeId::of::<FileWidget<U>>(),
        mod_area: RwLock::new(mod_area)
    };

    (constructor_hook)(mod_node, widget);

	
    controler.commands.write().file_id = old_file_id;
    old_file.map(|file| *controler.active_file.write() = file);
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
        &mut self, index: &Self::Area, specs: PushSpecs, is_glued: bool
    ) -> (Self::Area, Option<Self::Area>);

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), ()>;
}

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: 'static {
    type PrintInfo: PrintInfo<Area = Self::Area>;
    type Area: Area<PrintInfo = Self::PrintInfo>;
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
    files_area: U::Area,
    master_area: U::Area
}

impl<U> ParsecWindow<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of [`ParsecWindow<U>`].
    pub fn new<W, C>(ui: &mut U, mut widget: W, checker: C) -> (Self, U::Area)
    where
        W: Widget<U>,
        C: Fn() -> bool + 'static
    {
        let (window, mut area) = ui.new_window();
        widget.update(&mut area);

        let main_node = Node {
            widget: RwData::new_unsized(Arc::new(RwLock::new(widget))),
            checker: Box::new(checker),
            area: area.clone(),
            file_id: Some(unique_file_id())
        };

        let parsec_window = ParsecWindow {
            window,
            nodes: vec![main_node],
            files_area: area.clone(),
            master_area: area.clone()
        };

        (parsec_window, area)
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    pub fn push<W, C>(
        &mut self, area: &U::Area, widget: W, checker: C, specs: PushSpecs, file_id: Option<usize>,
        is_glued: bool
    ) -> (U::Area, Option<U::Area>)
    where
        W: Widget<U>,
        C: Fn() -> bool + 'static
    {
        let (child, parent) = self.window.bisect(area, specs, is_glued);

        let node = Node {
            widget: RwData::new_unsized(Arc::new(RwLock::new(widget))),
            checker: Box::new(checker),
            area: child.clone(),
            file_id
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
        &mut self, widget: impl Widget<U>, specs: PushSpecs
    ) -> (U::Area, Option<U::Area>) {
        let area = self.files_area.clone();
        let file_id = unique_file_id();

        let checker = || false;
        let (child, parent) = self.push(&area, widget, checker, specs, Some(file_id), false);
        if let Some(parent) = &parent {
            self.files_area = parent.clone();
        }

        (child, parent)
    }

    /// Pushes a [`Widget<U>`] to the master node of the current
    /// window.
    pub fn push_to_master<W, C>(
        &mut self, widget: W, checker: C, specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        W: Widget<U>,
        C: Fn() -> bool + 'static
    {
        let master_area = self.master_area.clone();
        self.push(&master_area, widget, checker, specs, None, false)
    }

    /// Returns an [`Iterator`] over the [`Widget<U>`]s of [`self`].
    pub fn widgets(
        &self
    ) -> impl Iterator<Item = (&RwData<dyn Widget<U>>, &U::Area, Option<usize>)> + Clone + '_ {
        self.nodes.iter().map(
            |Node {
                 widget,
                 area,
                 file_id,
                 ..
             }| (widget, area, *file_id)
        )
    }

    pub fn nodes(&self) -> impl Iterator<Item = &Node<U>> {
        self.nodes.iter()
    }

    /// Returns an [`Iterator`] over the names of [`FileWidget<U>`]s
    /// and their respective [`ActionableWidget`] indices.
    pub fn file_names(&self) -> impl Iterator<Item = (usize, String)> + Clone + '_ {
        self.nodes.iter().enumerate().filter_map(|(pos, Node { widget, .. })| {
            widget
                .read()
                .as_any()
                .downcast_ref::<FileWidget<U>>()
                .map(|file_widget| (pos, file_widget.name().to_string()))
        })
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
        self.0.nodes.iter().fold(init, |accum, Node { widget, .. }| {
            if let Some(file_widget) = widget.raw_read().as_any().downcast_ref::<FileWidget<U>>() {
                f(accum, file_widget)
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
        self.0.nodes.iter().fold(init, |accum, Node { widget, .. }| {
            let f = &mut f;
            let widget = widget.raw_read();
            f(accum, &*widget)
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
