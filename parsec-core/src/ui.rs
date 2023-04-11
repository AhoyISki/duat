use std::fmt::{Debug, Display};

use ropey::RopeSlice;

use crate::{
    config::{Config, RoData, RwData, TabPlaces, WrapMethod},
    tags::form::{CursorStyle, Form},
    text::PrintStatus,
    widgets::{file_widget::FileWidget, ActionableWidget, Widget},
    SessionManager,
};

pub trait Area {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Requests a new width to the widget.
    fn request_len(&self, len: usize, side: Side) -> Result<(), ()>;

    fn bisect(&mut self, push_specs: PushSpecs) -> (usize, Option<usize>);

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, text: &str) -> Result<(), ()>;
}

/// A label that prints text to screen. Any area that prints will be a
/// `Label` in the `Ui`.
pub trait Label<A>
where
    A: Area,
{
    /// Returns a reference to the area of `self`.
    fn area(&self) -> &A;

    //////////////////// Forms
    /// Changes the form for subsequent characters.
    fn set_form(&mut self, form: Form);

    /// Places the primary cursor on the current printing position.
    fn place_main_cursor(&mut self, style: CursorStyle);

    /// Places an extra cursor on the current printing position.
    fn place_extra_cursor(&mut self, style: CursorStyle);

    /// Tells the [`Ui`] that this [`Label`] is the one that is
    /// currently focused.
    fn set_as_active(&mut self);

    //////////////////// Printing
    /// Tell the area that printing has begun.
    ///
    /// This function should at the very least move the cursor to the
    /// top left position in the area.
    fn start_printing(&mut self, config: &Config);

    /// Tell the area that printing has ended.
    ///
    /// This function should clear the lines below the last printed
    /// line, and flush the contents if necessary.
    fn stop_printing(&mut self);

    /// Prints a character at the current position and moves the
    /// printing position forward.
    fn print(&mut self, ch: char, x_shift: usize) -> PrintStatus;

    /// Moves to the next line. If succesful, returns `Ok(())`,
    /// otherwise, returns `Err(())`.
    ///
    /// This function should also make sure that there is no leftover
    /// text after the current line's end.
    fn next_line(&mut self) -> PrintStatus;

    /// Counts how many times the given string would wrap.
    fn wrap_count(
        &self,
        slice: RopeSlice,
        wrap_method: WrapMethod,
        tab_places: &TabPlaces,
    ) -> usize;

    /// Returns the column that comes after the [slice][RopeSlice]
    /// wraps `wrap` times.
    fn col_at_wrap(
        &self,
        slice: RopeSlice,
        wrap: usize,
        wrap_method: WrapMethod,
        tab_places: &TabPlaces,
    ) -> usize;

    /// Gets the visual width to a given column.
    fn get_width(&self, slice: RopeSlice, tab_places: &TabPlaces) -> usize;

    /// Gets the column at the given distance from the left side.
    fn col_at_dist(&self, slice: RopeSlice, dist: usize, tab_places: &TabPlaces) -> usize;
}

/// Container for middle and end nodes.
pub struct Node<U>
where
    U: Ui,
{
    widget: Widget<U>,
    config: RwData<Config>,
    area_index: usize,
}

impl<U> Node<U>
where
    U: Ui,
{
    pub(crate) fn update_and_print(&self, label: &mut U::Label, is_active: bool) {
        let config = self.config.read();
        self.widget.update(label, &config);
        if is_active {
            label.set_as_active()
        }

        self.widget.print(label, &config);
    }
}

pub struct ModNode<'a, U>
where
    U: Ui,
{
    session_manager: &'a mut SessionManager,
    window: &'a mut ParsecWindow<U>,
    area_index: usize,
}

impl<'a, U> ModNode<'a, U>
where
    U: Ui + 'static,
{
    pub fn push_widget(
        &mut self,
        constructor: impl FnOnce(&SessionManager, PushSpecs) -> Widget<U>,
        push_specs: PushSpecs,
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(self.session_manager, push_specs);
        self.window.push_widget(self.area_index, widget, push_specs)
    }

    pub fn push_widget_to_area(
        &mut self,
        constructor: impl FnOnce(&SessionManager, PushSpecs) -> Widget<U>,
        area_index: usize,
        push_specs: PushSpecs,
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(self.session_manager, push_specs);
        self.window.push_widget(area_index, widget, push_specs)
    }
}

/// A way of splitting areas.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Split {
    Locked(usize),
    Min(usize),
}

impl Debug for Split {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Split::Locked(len) => f.write_fmt(format_args!("Locked({})", len)),
            Split::Min(len) => f.write_fmt(format_args!("Locked({})", len)),
        }
    }
}

impl Default for Split {
    fn default() -> Self {
        Split::Min(0)
    }
}

impl Split {
    pub fn len(&self) -> usize {
        match self {
            Split::Locked(len) | Split::Min(len) => *len,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    Horizontal,
    Vertical,
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

/// A direction, where a `Widget<U>` will be placed in relation to
/// another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Top,
    Right,
    Bottom,
    Left,
}

impl Side {
    /// The opposite of this `Side`.
    pub fn opposite(&self) -> Side {
        match self {
            Side::Top => Side::Bottom,
            Side::Bottom => Side::Top,
            Side::Left => Side::Right,
            Side::Right => Side::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PushSpecs {
    pub side: Side,
    pub split: Split,
    pub glued: bool,
}

impl PushSpecs {
    pub fn new(side: Side, split: Split) -> Self {
        PushSpecs {
            side,
            split,
            glued: false,
        }
    }

    pub fn new_glued(side: Side, split: Split) -> Self {
        PushSpecs {
            side,
            split,
            glued: true,
        }
    }
}

pub trait Window: 'static {
    type Area: Area + Clone + Display + Send + Sync;
    type Label: Label<Self::Area> + Clone + Send + Sync;

    fn get_area(&self, area_index: usize) -> Option<Self::Area>;

    fn get_label(&self, area_index: usize) -> Option<Self::Label>;

    /// Wether or not the layout of the `Ui` (size of widgets, their
    /// positions, etc) has changed.
    fn layout_has_changed(&self) -> bool;
}

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: 'static {
    type Area: Area + Clone + Display + Send + Sync;
    type Label: Label<Self::Area> + Clone + Send + Sync;
    type Window: Window<Area = Self::Area, Label = Self::Label> + Clone;

    fn new_window(&mut self) -> (Self::Window, Self::Label);

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);
}

/// A "viewport" of Parsec. It contains a group of widgets that can be
/// displayed at the same time.
pub struct ParsecWindow<U>
where
    U: Ui,
{
    window: U::Window,
    nodes: Vec<Node<U>>,
    active_area: usize,
    files_parent: usize,
    config: RwData<Config>,
}

impl<U> ParsecWindow<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `NodeManager`.
    pub fn new(
        ui: &mut U,
        widget: Widget<U>,
        config: Config,
        session_manager: &mut SessionManager,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<FileWidget<U>>),
    ) -> Self {
        let (window, mut initial_label) = ui.new_window();
        widget.update(&mut initial_label, &config);
        let config = RwData::new(config);

        let actionable = widget.get_actionable().unwrap();
        let ro_widget = RoData::from(actionable);

        let main_node = Node {
            widget,
            config: config.clone(),
            area_index: 0,
        };
        let mut parsec_window = ParsecWindow {
            window,
            nodes: vec![main_node],
            active_area: 0,
            files_parent: 0,
            config,
        };

        let file = ro_widget.try_downcast::<FileWidget<U>>().unwrap();

        let mod_node = ModNode {
            session_manager,
            window: &mut parsec_window,
            area_index: 0,
        };
        (constructor_hook)(mod_node, file);

        parsec_window
    }

    /// Pushes a `Widget` onto an
    fn push_widget(
        &mut self,
        area_index: usize,
        widget: Widget<U>,
        push_specs: PushSpecs,
    ) -> (usize, Option<usize>) {
        let mut area = self.window.get_area(area_index).unwrap();
        let (new_area, pushed_area) = area.bisect(push_specs);
        let label = self.window.get_label(new_area).unwrap();
        widget.update(&label, &self.config.read());

        if let Some(new_area_index) = pushed_area {
            self.nodes.iter_mut().for_each(|node| {
                if node.area_index == area_index {
                    node.area_index = new_area_index
                }
            });
        }

        let node = Node {
            widget,
            config: self.config.clone(),
            area_index: new_area,
        };
        self.nodes.push(node);

        (new_area, pushed_area)
    }

    pub fn push_hooked(
        &mut self,
        widget: Widget<U>,
        area_index: usize,
        push_specs: PushSpecs,
        constructor_hook: &dyn Fn(ModNode<U>),
        session_manager: &mut SessionManager,
    ) -> (usize, Option<usize>) {
        let (new_area, opt_parent) = self.push_widget(area_index, widget, push_specs);

        let mod_node = ModNode {
            session_manager,
            window: self,
            area_index: new_area,
        };
        (constructor_hook)(mod_node);

        (new_area, opt_parent)
    }

    pub fn push_file<C>(
        &mut self,
        widget: Widget<U>,
        push_specs: PushSpecs,
        session_manager: &mut SessionManager,
        constructor_hook: C,
    ) -> (usize, Option<usize>)
    where
        C: Fn(ModNode<U>, RoData<FileWidget<U>>),
    {
        let node_index = self.files_parent;
        let (new_index, opt_parent) = self.push_widget(node_index, widget, push_specs);
        let node = self
            .nodes
            .iter()
            .find(|Node { area_index, .. }| *area_index == new_index)
            .unwrap();

        let Some(actionable) = node.widget.get_actionable() else {
            unreachable!();
        };

        let widget = RoData::from(actionable);
        let file = widget.try_downcast::<FileWidget<U>>().unwrap();

        let mod_node = ModNode {
            session_manager,
            window: self,
            area_index: new_index,
        };
        (constructor_hook)(mod_node, file);

        self.active_area = new_index;

        (new_index, opt_parent)
    }

    /// Pushes a `Widget` to the parent of all files.
    /// Pushes a `Widget` to the master node of the current window.
    pub fn push_to_master(
        &mut self,
        widget: Widget<U>,
        push_specs: PushSpecs,
    ) -> (usize, Option<usize>) {
        self.push_widget(0, widget, push_specs)
    }

    pub fn widgets(&self) -> impl Iterator<Item = (&Widget<U>, U::Label, &RwData<Config>)> + '_ {
        self.nodes.iter().map(
            |Node {
                 widget,
                 config,
                 area_index,
                 ..
             }| {
                let label = self.window.get_label(*area_index).unwrap();
                (widget, label, config)
            },
        )
    }

    pub fn actionable_widgets(
        &self,
    ) -> impl Iterator<Item = (&RwData<dyn ActionableWidget<U>>, U::Label, &RwData<Config>)> + '_
    {
        self.nodes.iter().filter_map(
            |Node {
                 widget,
                 config,
                 area_index,
                 ..
             }| {
                widget.get_actionable().map(|widget| {
                    let label = self.window.get_label(*area_index).unwrap();
                    (widget, label, config)
                })
            },
        )
    }

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

    pub(crate) fn print_if_layout_changed(&self) {
        if self.window.layout_has_changed() {
            for node in &self.nodes {
                let mut label = self.window.get_label(node.area_index).unwrap();
                node.update_and_print(&mut label, node.area_index == self.active_area);
            }
        }
    }
}
