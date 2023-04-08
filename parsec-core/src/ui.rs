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
    fn set_form(&self, form: Form);

    /// Clears the current form.
    fn clear_form(&self);

    /// Places the primary cursor on the current printing position.
    fn place_main_cursor(&self, style: CursorStyle);

    /// Places an extra cursor on the current printing position.
    fn place_extra_cursor(&self, style: CursorStyle);

    /// Tells the [`Ui`] that this [`Label`] is the one that is
    /// currently focused.
    fn set_as_active(&self);

    //////////////////// Printing
    /// Tell the area that printing has begun.
    ///
    /// This function should at the very least move the cursor to the
    /// top left position in the area.
    fn start_printing(&self, config: &Config);

    /// Tell the area that printing has ended.
    ///
    /// This function should clear the lines below the last printed
    /// line, and flush the contents if necessary.
    fn stop_printing(&self);

    /// Prints a character at the current position and moves the
    /// printing position forward.
    fn print(&self, ch: char, x_shift: usize) -> PrintStatus;

    /// Moves to the next line. If succesful, returns `Ok(())`,
    /// otherwise, returns `Err(())`.
    ///
    /// This function should also make sure that there is no leftover
    /// text after the current line's end.
    fn next_line(&self) -> PrintStatus;

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
    pub(crate) fn update_and_print(&self, label: &U::Label, is_active: bool) {
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
    window: &'a mut Window<U>,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Split {
    Locked(usize),
    Min(usize),
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

/// All the methods that a working gui/tui will need to implement, in
/// order to use Parsec.
pub trait Ui: 'static {
    type Area: Area + Display + Send + Sync;
    type Label: Label<Self::Area> + Send + Sync;

    /// Bisects the `Self::Area`, returning a new
    /// `Self::Label<Self::Area>` that will occupy the region. If
    /// required, also returns a new `Self::Container<Self::Area>`,
    /// which will contain both the old `Self::Area` and the new
    /// `Self::Label`.
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::ui::Ui;
    /// let mut ui: Self = foo();
    /// // A container with a horizontal axis.
    /// let mut container: Self::Container = bar(Side::Left);
    /// let split: Split = baz();
    ///
    /// // A new container is not needed here, since the split is parallel to the container's axis.
    /// let (_, none_container) = ui.bisect_area(&mut container.mut_area(), Side::Right, split);
    /// assert!(none_container.is_none());
    ///
    /// // A new container is needed here, since the split is perpendicular to the container's axis.
    /// let (_, some_container) = ui.bisect_area(&mut area, Side::Top, split);
    /// assert!(some_container.is_some());
    /// ```
    fn bisect_area(&mut self, area_index: usize, push_specs: PushSpecs) -> (usize, Option<usize>);

    /// Returns a `Self::Label` representing the maximum possible
    /// extent an area could have.
    fn maximum_label(&mut self) -> Self::Label;

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);

    /// Functions to trigger once every `Label` has been printed.
    fn finish_all_printing(&mut self);

    /// Wether or not the layout of the `Ui` (size of widgets, their
    /// positions, etc) has changed.
    fn layout_has_changed(&self) -> bool;

    fn get_area(&self, area_index: usize) -> &Option<Self::Area>;

    fn get_label(&self, area_index: usize) -> Option<&Self::Label>;
}

/// A "viewport" of Parsec. It contains a group of widgets that can be
/// displayed at the same time.
pub struct Window<U>
where
    U: Ui,
{
    ui: U,
    nodes: Vec<Node<U>>,
    active_area: usize,
    last_index: usize,
    files_parent: usize,
    config: RwData<Config>,
}

impl<U> Window<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `NodeManager`.
    pub fn new(
        mut ui: U,
        widget: Widget<U>,
        config: Config,
        session_manager: &mut SessionManager,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<FileWidget<U>>),
    ) -> Self {
        let mut label = ui.maximum_label();
        widget.update(&mut label, &config);
        let config = RwData::new(config);

        let actionable = widget.get_actionable().unwrap();
        let ro_widget = RoData::from(actionable);

        let main_node = Node {
            widget,
            config: config.clone(),
            area_index: 0,
        };
        let mut window = Window {
            ui,
            nodes: vec![main_node],
            active_area: 0,
            last_index: 0,
            files_parent: 0,
            config,
        };

        let file = ro_widget.try_downcast::<FileWidget<U>>().unwrap();

        let mod_node = ModNode {
            session_manager,
            window: &mut window,
            area_index: 0,
        };
        (constructor_hook)(mod_node, file);

        window
    }

    /// Pushes a `Widget` onto an
    fn push_widget(
        &mut self,
        area_index: usize,
        widget: Widget<U>,
        push_specs: PushSpecs,
    ) -> (usize, Option<usize>) {
        self.last_index += 1;

        let (new_area, opt_parent) = self.ui.bisect_area(area_index, push_specs);
        let label = self.ui.get_label(new_area).unwrap();
        widget.update(&label, &self.config.read());

        let node = Node {
            widget,
            config: self.config.clone(),
            area_index,
        };
        self.nodes.push(node);

        (new_area, opt_parent)
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

    /// Triggers the functions to use when the program starts.
    pub(crate) fn startup(&mut self) {
        self.ui.startup();
    }

    /// Triggers the functions to use when the program ends.
    pub(crate) fn shutdown(&mut self) {
        self.ui.shutdown();
    }

    pub fn widgets(&self) -> impl Iterator<Item = (&Widget<U>, &U::Label, &RwData<Config>)> + '_ {
        self.nodes.iter().map(
            |Node {
                 widget,
                 config,
                 area_index,
                 ..
             }| {
                let label = self.ui.get_label(*area_index).unwrap();
                (widget, label, config)
            },
        )
    }

    pub fn actionable_widgets(
        &self,
    ) -> impl Iterator<Item = (&RwData<dyn ActionableWidget<U>>, &U::Label, &RwData<Config>)> + '_
    {
        self.nodes.iter().filter_map(
            |Node {
                 widget,
                 config,
                 area_index,
                 ..
             }| {
                widget.get_actionable().map(|widget| {
                    let label = self.ui.get_label(*area_index).unwrap();
                    (widget, label, config)
                })
            },
        )
    }

    pub fn file_names(&self) -> impl Iterator<Item = String> + DoubleEndedIterator + Clone + '_ {
        self.nodes.iter().filter_map(|Node { widget, .. }| {
            widget
                .get_actionable()
                .map(|widget| {
                    if let Ok(widget) = widget.try_read() {
                        let identifier = widget.identifier();
                        if let Some(prefix) = identifier.get(..13) {
                            if prefix == "parsec-file: " {
                                return Some(String::from(identifier));
                            }
                        }
                    };

                    None
                })
                .flatten()
        })
    }

    pub(crate) fn print_if_layout_changed(&self) {
        if self.ui.layout_has_changed() {
            for node in &self.nodes {
                let label = self.ui.get_label(node.area_index).unwrap();
                node.update_and_print(label, node.area_index == self.active_area);
            }
        }
    }
}
