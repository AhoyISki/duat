use std::fmt::{Debug, Display};

use crate::{
    config::{RoData, RwData},
    position::Pos,
    tags::form::FormPalette,
    text::{PrintCfg, Text, TextBit},
    widgets::{file_widget::FileWidget, ActionableWidget, NormalWidget, Widget},
    SessionManager
};

/// A representation of part of Parsec's window.
///
/// This is an abstract region of space that can be a [`Label`], which
/// may print [`Text<U>`], or it may contain other `Area`s, which may
/// be [`Label`]s themselves.
pub trait Area {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Requests a new width to the widget.
    fn request_len(&self, len: usize, side: Side) -> Result<(), ()>;

    fn bisect(&mut self, push_specs: PushSpecs, is_glued: bool) -> (usize, Option<usize>);

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), ()>;
}

// TODO: Add a general scrolling function.
pub trait PrintInfo: Default {
    /// Scrolls the [`Text<U>`] (up or down) until the main cursor is
    /// within the [`ScrollOff`][crate::text::ScrollOff] range.
    fn scroll_to_gap<U>(&mut self, text: &Text<U>, pos: Pos, label: &U::Label, cfg: &PrintCfg)
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
pub trait Label {
    type Area: Area + Clone + Display + Send + Sync;
    type PrintInfo: PrintInfo + Clone + Copy;

    /// Tells the [`Ui`] that this [`Label`] is the one that is
    /// currently focused.
    fn set_as_active(&mut self);

    /// Prints the [`Text`][crate::text::Text] via an [`Iterator`].
    fn print<U>(
        &mut self, text: &Text<U>, info: Self::PrintInfo, cfg: PrintCfg, palette: &FormPalette
    ) where
        U: Ui;

    //////////////////// Queries
    /// Counts how many times the given [`Iterator`] would wrap.
    fn wrap_count(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, max_index: usize
    ) -> usize;

    /// Returns the positional index of the char that comes after the
    /// [`TextBit`][crate::text::TextBit] [`Iterator`] wraps `wrap`
    /// times.
    fn char_at_wrap(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> Option<usize>;

    /// Gets the visual width of the [`Iterator`].
    fn get_width(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, max_index:usize) -> usize;

    /// Gets the column at `dist` from the left side on [`Iterator`].
    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize;

    /// Returns a reference to the area of [`self`].
    fn area(&self) -> &Self::Area;

    /// A unique identifier to this [`Label`].
    fn area_index(&self) -> usize;
}

/// Elements related to the [`Widget<U>`]s.
struct Node<U>
where
    U: Ui
{
    widget: Widget<U>,
    area_index: usize
}

/// A constructor helper for [`Widget<U>`]s.
///
/// When pushing [`Widget`]s to the layout, this struct can be used to
/// further actions to be taken. It is used in contexts where a widget
/// has just been inserted to the screen, inside closures.
///
/// # Examples
///
/// Here, [`LineNumbers<U>`][crate::widgets::LineNumbers<U>] is pushed
/// to the left of a widget (which in this case is a [`FileWidget<U>`]
/// ```rust
/// let file_fn = Box::new(move |mut mod_node: ModNode<U>, file: RoData<FileWidget<U>>| {
/// 	let push_specs = PushSpecs::new_glued(Side::Left, Split::Locked(1));
/// 	mod_node.push_widget(LineNumbers::default_fn(file), push_specs);
/// })
/// ```
pub struct ModNode<'a, U>
where
    U: Ui
{
    session_manager: &'a mut SessionManager,
    window: &'a mut ParsecWindow<U>,
    area_index: usize
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
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │                 │     │╭──2───╮╭───1───╮│
    /// │                 │ --> ││      ││       ││
    /// │                 │     ││      ││       ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    ///
    /// So a subsequent use of [`push_widget`][Self::push_widget] on
    /// [`Side::Bottom`] would push to the bottom of "both 1 and 2":
    ///
    /// ╭────────0────────╮     ╭────────0────────╮
    /// │╭──2───╮╭───1───╮│     │╭──2───╮╭───1───╮│
    /// ││      ││       ││ --> │╰──────╯╰───────╯│
    /// ││      ││       ││     │╭───────3───────╮│
    /// │╰──────╯╰───────╯│     │╰───────────────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    ///
    /// If you wish to, for example, push on [`Side::Bottom`] of `1`,
    /// checkout [`push_widget_to_area`][Self::push_widget_to_area].
    pub fn push_widget(
        &mut self, constructor: impl FnOnce(&SessionManager, PushSpecs) -> Widget<U>,
        push_specs: PushSpecs
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(self.session_manager, push_specs);
        self.window.push_glued_widget(widget, self.area_index, push_specs)
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
        &mut self, constructor: impl FnOnce(&SessionManager, PushSpecs) -> Widget<U>,
        area_index: usize, push_specs: PushSpecs
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(self.session_manager, push_specs);
        self.window.push_widget(widget, area_index, push_specs)
    }

    pub fn palette(&self) -> &FormPalette {
        &self.session_manager.palette
    }
}

/// How an [`Area`] is pushed onto another.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Split {
    Locked(usize),
    Min(usize)
}

impl Debug for Split {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Split::Locked(len) => f.write_fmt(format_args!("Locked({})", len)),
            Split::Min(len) => f.write_fmt(format_args!("Locked({})", len))
        }
    }
}

impl Default for Split {
    fn default() -> Self {
        Split::Min(0)
    }
}

impl Split {
    /// The length of this [`Split`].
    pub fn len(&self) -> usize {
        match self {
            Split::Locked(len) | Split::Min(len) => *len
        }
    }
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

impl From<Side> for Axis {
    fn from(value: Side) -> Self {
        if let Side::Top | Side::Bottom = value {
            Axis::Vertical
        } else {
            Axis::Horizontal
        }
    }
}

/// A direction, where a [`Widget<U>`] will be placed in relation to
/// another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Top,
    Right,
    Bottom,
    Left
}

impl Side {
    /// The opposite of this `Side`.
    pub fn opposite(&self) -> Side {
        match self {
            Side::Top => Side::Bottom,
            Side::Bottom => Side::Top,
            Side::Left => Side::Right,
            Side::Right => Side::Left
        }
    }
}

/// Information on how a [`Widget<U>`] should be pushed onto another.
///
/// The `glued` member indicates wether or not a [`Widget<U>`] should
/// "stick together" in case of movement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PushSpecs {
    pub side: Side,
    pub split: Split
}

impl PushSpecs {
    /// Returns a new instance of [`PushSpecs`] that is not glued.
    pub fn new(side: Side, split: Split) -> Self {
        PushSpecs { side, split }
    }
}

/// An abstract representation of a "viewport" of Parsec.
///
/// Only one [`Window`] may be shown at a time, and they contain all
/// [`Widget<U>`]s that should be displayed, both static and floating.
pub trait Window: 'static {
    type Area: Area + Clone + Display + Send + Sync;
    type Label: Label<Area = Self::Area> + Send + Sync;

    /// Gets the [`Area`][Window::Area] associated with a given
    /// `area_index`.
    fn get_area(&self, area_index: usize) -> Option<Self::Area>;

    /// Gets the [`Label`][Window::Label] associated with a given
    /// `area_index`.
    ///
    /// If the [`Area`][Window::Area] in question is not a
    /// [`Label`][Window::Label], then returns [`None`].
    fn get_label(&self, area_index: usize) -> Option<Self::Label>;

    /// Wether or not the layout of the `Ui` (size of widgets, their
    /// positions, etc) has changed.
    fn layout_has_changed(&self) -> bool;
}

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: 'static {
    type Area: Area + Clone + Display + Send + Sync;
    type PrintInfo: PrintInfo + Clone + Copy;
    type Label: Label<Area = Self::Area, PrintInfo = Self::PrintInfo> + Send + Sync;
    type Window: Window<Area = Self::Area, Label = Self::Label> + Clone + Send + Sync;

    /// Initiates and returns a new [`Window`][Ui::Window].
    ///
    /// Also returns the newly created [`Label`][Ui::Label] of that
    /// [`Window`][Ui::Window].
    fn new_window(&mut self) -> (Self::Window, Self::Label);

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
    pub fn new<W>(
        ui: &mut U, widget: Widget<U>, session_manager: &mut SessionManager,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<W>)
    ) -> Self
    where
        W: NormalWidget<U>
    {
        let (window, mut initial_label) = ui.new_window();
        widget.update(&mut initial_label);

        let main_node = Node {
            widget,
            area_index: initial_label.area_index()
        };
        let mut parsec_window = ParsecWindow {
            window,
            nodes: vec![main_node],
            files_parent: 0
        };

        parsec_window.activate_hook(initial_label.area_index(), session_manager, constructor_hook);

        parsec_window
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    pub fn push_widget(
        &mut self, widget: Widget<U>, area_index: usize, push_specs: PushSpecs
    ) -> (usize, Option<usize>) {
        self.inner_push_widget(area_index, push_specs, false, widget)
    }

    /// Pushes a [`Widget<U>`] onto an existing one.
    pub fn push_glued_widget(
        &mut self, widget: Widget<U>, area_index: usize, push_specs: PushSpecs
    ) -> (usize, Option<usize>) {
        self.inner_push_widget(area_index, push_specs, true, widget)
    }

    fn inner_push_widget(
        &mut self, area_index: usize, push_specs: PushSpecs, is_glued: bool, widget: Widget<U>
    ) -> (usize, Option<usize>) {
        let mut area = self.window.get_area(area_index).unwrap();
        let (new_area, pushed_area) = area.bisect(push_specs, is_glued);
        let label = self.window.get_label(new_area).unwrap();
        widget.update(&label);

        if let Some(new_area_index) = pushed_area {
            self.nodes.iter_mut().for_each(|node| {
                if node.area_index == area_index {
                    node.area_index = new_area_index
                }
            });
        }

        let node = Node {
            widget,
            area_index: new_area
        };
        self.nodes.push(node);
        (new_area, pushed_area)
    }

    /// Pushes a [`Widget<U>`] onto an existing one and activates a
    /// hook function.
    pub fn push_hooked_widget<W>(
        &mut self, widget: Widget<U>, area_index: usize, push_specs: PushSpecs,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<W>), session_manager: &mut SessionManager
    ) -> (usize, Option<usize>)
    where
        W: NormalWidget<U>
    {
        let (new_area, opt_parent) = self.push_widget(widget, area_index, push_specs);

        self.activate_hook(new_area, session_manager, constructor_hook);

        (new_area, opt_parent)
    }

    /// Pushes a [`Widget<U>`] onto an existing one and activates a
    /// hook function.
    pub fn push_glued_hooked_widget<W>(
        &mut self, widget: Widget<U>, area_index: usize, push_specs: PushSpecs,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<W>), session_manager: &mut SessionManager
    ) -> (usize, Option<usize>)
    where
        W: NormalWidget<U>
    {
        let (new_area, opt_parent) = self.push_glued_widget(widget, area_index, push_specs);

        self.activate_hook(new_area, session_manager, constructor_hook);

        (new_area, opt_parent)
    }

    fn activate_hook<W>(
        &mut self, new_area: usize, session_manager: &mut SessionManager,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<W>)
    ) where
        W: NormalWidget<U>
    {
        let node = self
            .nodes
            .iter()
            .find(|Node { area_index, .. }| *area_index == new_area)
            .unwrap();

        let Some(widget) = node.widget.try_downcast::<W>() else {
            panic!("The widget is not of type {}", std::any::type_name::<W>());
        };

        let mod_node = ModNode {
            session_manager,
            window: self,
            area_index: new_area
        };

        (constructor_hook)(mod_node, widget);
    }

    /// Pushes a [`FileWidget<U>`] to another, and then activates a
    /// special hook.
    ///
    /// This function will push to the edge of `self.files_parent`.
    /// This is an area, usually in the center, that contains all
    /// [`FileWidget<U>`]s, and their associated [`Widget<U>`]s,
    /// with others being at the perifery of this area.
    pub fn push_file(
        &mut self, widget: Widget<U>, push_specs: PushSpecs,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<FileWidget<U>>),
        session_manager: &mut SessionManager
    ) -> (usize, Option<usize>) {
        let node_index = self.files_parent;

        let (new_index, opt_parent) = self.push_hooked_widget::<FileWidget<U>>(
            widget,
            node_index,
            push_specs,
            constructor_hook,
            session_manager
        );

        (new_index, opt_parent)
    }

    /// Pushes a [`Widget<U>`] to the master node of the current
    /// window.
    pub fn push_to_master(
        &mut self, widget: Widget<U>, push_specs: PushSpecs
    ) -> (usize, Option<usize>) {
        self.push_widget(widget, 0, push_specs)
    }

    /// Returns an [`Iterator`] over the [`Widget<U>`]s of [`self`].
    pub fn widgets(&self) -> impl Iterator<Item = (&Widget<U>, U::Label)> + '_ {
        self.nodes.iter().map(
            |Node {
                 widget, area_index, ..
             }| {
                let label = self.window.get_label(*area_index).unwrap();
                (widget, label)
            }
        )
    }

    /// Returns an [`Iterator`] over the [`ActionableWidget`]s of
    /// [`self`].
    pub fn actionable_widgets(
        &self
    ) -> impl Iterator<Item = (&RwData<dyn ActionableWidget<U>>, U::Label)> + '_ {
        self.nodes.iter().filter_map(
            |Node {
                 widget, area_index, ..
             }| {
                widget.get_actionable().map(|widget| {
                    let label = self.window.get_label(*area_index).unwrap();
                    (widget, label)
                })
            }
        )
    }

    /// Returns an [`Iterator`] over the file names of open
    /// [`FileWidget<U>`]s.
    pub fn file_names(&self) -> impl Iterator<Item = (usize, String)> + Clone + '_ {
        self.nodes
            .iter()
            .filter_map(|Node { widget, .. }| widget.get_actionable())
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
                let mut label = self.window.get_label(node.area_index).unwrap();
                node.widget.update(&mut label);
                node.widget.print(&mut label, &palette);
            }
        }
    }
}
