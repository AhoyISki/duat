use std::fmt::{Debug, Display};

use ropey::RopeSlice;

use crate::{
    config::{Config, RoData, RwData, TabPlaces, WrapMethod},
    tags::form::{CursorStyle, Form},
    widgets::{file_widget::FileWidget, ActionableWidget, Widget},
    SessionManager, text::PrintStatus,
};

pub trait Area {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Requests a new width to the widget.
    fn request_len(&mut self, len: usize, side: Side) -> Result<(), ()>;

    /// Requests that the width be enough to fit a certain piece of text.
    fn request_width_to_fit(&mut self, text: &str) -> Result<(), ()>;
}

/// A label that prints text to screen. Any area that prints will be a `Label` in the `Ui`.
pub trait Label<A>
where
    A: Area,
{
    /// Returns a mutable reference to the area of `self`.
    fn area_mut(&mut self) -> &mut A;

    /// Returns a reference to the area of `self`.
    fn area(&self) -> &A;

    //////////////////// Forms
    /// Changes the form for subsequent characters.
    fn set_form(&mut self, form: Form);

    /// Clears the current form.
    fn clear_form(&mut self);

    /// Places the primary cursor on the current printing position.
    fn place_main_cursor(&mut self, style: CursorStyle);

    /// Places an extra cursor on the current printing position.
    fn place_extra_cursor(&mut self, style: CursorStyle);

    /// Tells the `UiManager` that this `Label` is the one that is currently focused.
    fn set_as_active(&mut self);

    //////////////////// Printing
    /// Tell the area that printing has begun.
    ///
    /// This function should at the very least move the cursor to the top left position in the
    /// area.
    fn start_printing(&mut self);

    /// Tell the area that printing has ended.
    ///
    /// This function should clear the lines below the last printed line, and flush the contents if
    /// necessary.
    fn stop_printing(&mut self);

    /// Prints a character at the current position and moves the printing position forward.
    fn print(&mut self, ch: char, x_shift: usize) -> PrintStatus;

    /// Moves to the next line. If succesful, returns `Ok(())`, otherwise, returns `Err(())`.
    ///
    /// This function should also make sure that there is no leftover text after the current line's
    /// end.
    fn next_line(&mut self) -> Result<(), ()>;

    /// Counts how many times the given string would wrap.
    fn wrap_count(&self, text: &str, wrap_method: WrapMethod, tab_places: &TabPlaces) -> usize;

    /// Gets the visual width to a given column.
    fn get_width(&self, text: &str, tab_places: &TabPlaces) -> usize;

    /// Gets the column at the given distance from the left side.
    fn col_at_dist(&self, text: RopeSlice, dist: usize, tab_places: &TabPlaces) -> usize;
}

/// A node that contains other nodes.
pub struct MidNode<U>
where
    U: Ui + ?Sized,
{
    area: U::Area,
    config: Config,
}

impl<U: Debug> Debug for MidNode<U>
where
    U: Ui + ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MidNode").field("area", &self.area).finish()
    }
}

impl<U> MidNode<U>
where
    U: Ui + 'static,
{
    fn new_from(container: U::Area, node: &Node<U>) -> Self {
        MidNode { area: container, config: node.config() }
    }
}

/// Node that contains a `Label`.
pub struct EndNode<U>
where
    U: Ui + ?Sized,
{
    pub label: U::Label,
    pub config: Config,
    pub(crate) is_active: bool,
}

impl<U: Debug> Debug for EndNode<U>
where
    U: Ui + ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EndNode")
            .field("label", &self.label)
            .field("is_active", &self.is_active)
            .finish()
    }
}

impl<U> EndNode<U>
where
    U: Ui + 'static,
{
    fn new_from(label: U::Label, node: &Node<U>) -> Self {
        EndNode { label: label, config: node.config(), is_active: false }
    }

    /// Returns a reference to the `Config` of the node.
    pub fn config(&self) -> &Config {
        &self.config
    }

    pub(crate) fn get_width(&self, text: &str) -> usize {
        self.label.get_width(text, &self.config.tab_places)
    }
}

/// Container for middle and end nodes.
#[derive(Debug)]
pub enum Node<U>
where
    U: Ui + ?Sized,
{
    MidNode {
        mid_node: RwData<MidNode<U>>,
        children: Vec<Node<U>>,
        node_index: NodeIndex,
    },
    EndNode {
        end_node: RwData<EndNode<U>>,
        widget: Widget<U>,
        identifier: String,
        node_index: NodeIndex,
    },
}

impl<U> Node<U>
where
    U: Ui + 'static,
{
    fn config(&self) -> Config {
        match self {
            Node::MidNode { mid_node, .. } => mid_node.read().config.clone(),
            Node::EndNode { end_node, .. } => end_node.read().config.clone(),
        }
    }

    fn node_index(&self) -> NodeIndex {
        match self {
            Node::MidNode { node_index, .. } => *node_index,
            Node::EndNode { node_index, .. } => *node_index,
        }
    }

    pub(crate) fn find(&self, node_index: NodeIndex) -> Option<&Node<U>> {
        if self.node_index() == node_index {
            return Some(self);
        } else if let Node::MidNode { children, .. } = self {
            for child in children {
                if let Some(node) = child.find(node_index) {
                    return Some(node);
                }
            }
        }
        None
    }

    fn find_mut(&mut self, node_index: NodeIndex) -> Option<&mut Node<U>> {
        if self.node_index() == node_index {
            return Some(self);
        } else if let Node::MidNode { children, .. } = self {
            for child in children {
                if let Some(node) = child.find_mut(node_index) {
                    return Some(node);
                }
            }
        }
        None
    }

    /// Returns `Some((self, split_of_node))` if it is the parent of the given `NodeIndex`.
    fn find_mut_siblings(&mut self, node_index: NodeIndex) -> Option<(&mut Vec<Node<U>>, usize)> {
        let Node::MidNode { children, .. } = self else {
            return None;
        };

        if let Some(pos) = children.iter().position(|child| child.node_index() == node_index) {
            Some((children, pos))
        } else {
            for child in children {
                if let Some((mid_node, pos)) = child.find_mut_siblings(node_index) {
                    return Some((mid_node, pos));
                }
            }

            None
        }
    }

    fn bisect(
        &self, side: Side, split: Split, widget: Widget<U>, last_index: NodeIndex, ui: &mut U,
    ) -> (Node<U>, Option<MidNode<U>>) {
        let (label, container) = match self {
            Node::MidNode { mid_node, .. } => {
                ui.bisect_area(&mut mid_node.write().area, side, split)
            }
            Node::EndNode { end_node, .. } => {
                ui.bisect_area(end_node.write().label.area_mut(), side, split)
            }
        };

        let mut end_node = EndNode::new_from(label, &self);
        widget.update(&mut end_node);
        let end_node = RwData::new(end_node);
        let identifier = widget.identifier();
        let end_node = Node::EndNode { end_node, node_index: last_index, widget, identifier };

        let mid_node = container.map(|container| MidNode::new_from(container, &self));

        (end_node, mid_node)
    }

    /// Rebranches a tree, assuming that the parent of a given `Node` has been changed.
    fn replace_with_mid(&mut self, mut new_node: Node<U>, end_node: Node<U>, side: Side) {
        std::mem::swap(self, &mut new_node);

        let Node::MidNode { children, .. } = self else {
    		unreachable!();
		};

        children.push(new_node);
        let insert_index = if let Side::Top | Side::Left = side { 0 } else { 1 };
        children.insert(insert_index, end_node);
    }
}

pub struct ModNode<'a, U>
where
    U: Ui,
{
    session_manager: &'a mut SessionManager,
    window: &'a mut Window<U>,
    node_index: NodeIndex,
}

impl<'a, U> ModNode<'a, U>
where
    U: Ui + 'static,
{
    pub fn push_widget(
        &mut self, constructor: Box<dyn FnOnce(&SessionManager) -> Widget<U>>, side: Side,
        split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>) {
        let widget = (constructor)(self.session_manager);
        self.window.push_widget(self.node_index, widget, side, split, glued)
    }

    pub fn push_widget_to_node<C>(
        &mut self, constructor: C, node_index: NodeIndex, side: Side, split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>)
    where
        C: Fn(&SessionManager) -> Widget<U>,
    {
        let widget = (constructor)(self.session_manager);
        self.window.push_widget(node_index, widget, side, split, glued)
    }
}

/// A way of splitting areas.
#[derive(Debug, Clone, Copy)]
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
        if let Side::Top | Side::Bottom = value { Axis::Vertical } else { Axis::Horizontal }
    }
}

/// A direction, where a `Widget<U>` will be placed in relation to another.
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

/// All the methods that a working gui/tui will need to implement, in order to use Parsec.
pub trait Ui: Debug + 'static {
    type Area: Area + Debug + Clone + Display + Send + Sync;
    type Label: Label<<Self as Ui>::Area> + Debug + Clone + Send + Sync;

    /// Bisects the `Self::Area`, returning a new `Self::Label<Self::Area>` that will occupy the
    /// region. If required, also returns a new `Self::Container<Self::Area>`, which will contain
    /// both the old `Self::Area` and the new `Self::Label`.
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
    fn bisect_area(
        &mut self, area: &mut Self::Area, side: Side, split: Split,
    ) -> (Self::Label, Option<Self::Area>);

    /// Returns a `Self::Label` representing the maximum possible extent an area could have.
    fn maximum_label(&mut self) -> Self::Label;

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);

    /// Functions to trigger once every `Label` has been printed.
    fn finish_all_printing(&mut self);

    /// Wether or not the layout of the `Ui` (size of widgets, their positions, etc) has changed.
    fn layout_has_changed(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeIndex(pub(crate) usize);

/// A "viewport" of Parsec. It contains a group of widgets that can be displayed at the same time.
#[derive(Debug)]
pub struct Window<U>
where
    U: Ui,
{
    ui: U,
    main_node: Node<U>,
    _floating_nodes: Vec<Node<U>>,
    last_index: NodeIndex,
    files_parent: NodeIndex,
}

impl<U> Window<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `NodeManager`.
    pub fn new(
        mut ui: U, widget: Widget<U>, config: Config, session_manager: &mut SessionManager,
        constructor_hook: &dyn Fn(ModNode<U>, RoData<FileWidget<U>>),
    ) -> Self {
        let mut end_node = EndNode { label: ui.maximum_label(), config, is_active: true };
        widget.update(&mut end_node);

        let Widget::Actionable(rw_data) = &widget else {
            unreachable!();
        };
        let ro_widget = RoData::from(rw_data);

        let identifier = widget.identifier();
        let main_node = Node::EndNode {
            end_node: RwData::new(end_node),
            widget,
            identifier,
            node_index: NodeIndex(0),
        };
        let mut window = Window {
            ui,
            main_node,
            _floating_nodes: Vec::new(),
            last_index: NodeIndex(0),
            files_parent: NodeIndex(0),
        };

        let file = ro_widget.try_downcast::<FileWidget<U>>().unwrap();

        let mod_node = ModNode { session_manager, window: &mut window, node_index: NodeIndex(0) };
        (constructor_hook)(mod_node, file);

        window
    }

    /// Pushes a `Widget` onto an
    pub(crate) fn push_widget(
        &mut self, node_index: NodeIndex, widget: Widget<U>, side: Side, split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.last_index.0 += 1;

        let target_node = self.main_node.find_mut(node_index).expect("Node not found");
        let (new_child, new_parent) =
            target_node.bisect(side, split, widget, self.last_index, &mut self.ui);

        if let Some(mid_node) = new_parent {
            let mid_node = RwData::new(mid_node);
            self.last_index.0 += 1;

            // Here, I swap the `NodeIndex`es in order to keep the same node "position".
            let new_parent = Node::MidNode {
                mid_node,
                children: Vec::new(),
                node_index: target_node.node_index(),
            };
            match target_node {
                Node::MidNode { node_index: index, .. } => *index = self.last_index,
                Node::EndNode { node_index: index, .. } => *index = self.last_index,
            }

            target_node.replace_with_mid(new_parent, new_child, side);

            if !glued && node_index == self.files_parent {
                self.files_parent = self.last_index;
            }

            (NodeIndex(self.last_index.0 - 1), Some(self.last_index))
        } else {
            drop(target_node);
            if let Some((children, pos)) = self.mut_parent_of(node_index) {
                if let Side::Top | Side::Left = side {
                    children.insert(pos, new_child);
                } else {
                    children.insert(pos + 1, new_child);
                }
            }

            (self.last_index, None)
        }
    }

    pub fn push_hooked(
        &mut self, widget: Widget<U>, node_index: NodeIndex, side: Side, split: Split, glued: bool,
        constructor_hook: &dyn Fn(ModNode<U>), session_manager: &mut SessionManager,
    ) -> (NodeIndex, Option<NodeIndex>) {
        let (new_node, maybe_node) = self.push_widget(node_index, widget, side, split, glued);

        let mod_node = ModNode { session_manager, window: self, node_index: new_node };
        (constructor_hook)(mod_node);

        (new_node, maybe_node)
    }

    pub fn push_file<C>(
        &mut self, widget: Widget<U>, side: Side, split: Split, glued: bool,
        session_manager: &mut SessionManager, constructor_hook: C,
    ) -> (NodeIndex, Option<NodeIndex>)
    where
        C: Fn(ModNode<U>, RoData<FileWidget<U>>),
    {
        let node_index = self.files_parent;
        let (new_index, maybe_index) = self.push_widget(node_index, widget, side, split, glued);
        let node = self.find(new_index).unwrap();

        let Node::EndNode { widget: Widget::Actionable(widget), .. } = node else {
            unreachable!();
        };
        let widget = RoData::from(widget);
        let file = widget.try_downcast::<FileWidget<U>>().unwrap();

        let mod_node = ModNode { session_manager, window: self, node_index: new_index };
        (constructor_hook)(mod_node, file);

        (new_index, maybe_index)
    }

    /// Pushes a `Widget` to the parent of all files.
    /// Pushes a `Widget` to the master node of the current window.
    pub fn push_to_master(
        &mut self, widget: Widget<U>, side: Side, split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.push_widget(NodeIndex(0), widget, side, split, glued)
    }

    /// Triggers the functions to use when the program starts.
    pub(crate) fn startup(&mut self) {
        self.ui.startup();
    }

    /// Triggers the functions to use when the program ends.
    pub(crate) fn shutdown(&mut self) {
        self.ui.shutdown();
    }

    fn find(&self, node_index: NodeIndex) -> Option<&Node<U>> {
        self.main_node.find(node_index)
    }

    /// Returns the parent of a given `NodeIndex`, if it exists.
    fn mut_parent_of(&mut self, node_index: NodeIndex) -> Option<(&mut Vec<Node<U>>, usize)> {
        if let Some(node) = self.main_node.find_mut_siblings(node_index) {
            return Some(node);
        }
        None
    }

    pub fn widgets(&self) -> Widgets<U> {
        Widgets { window: self, cur_node_index: NodeIndex(0) }
    }

    pub fn actionable_widgets(&self) -> ActionableWidgets<U> {
        ActionableWidgets { window: self, cur_node_index: NodeIndex(0) }
    }

    pub fn files(&self) -> impl Iterator<Item = String> + DoubleEndedIterator + Clone + '_ {
        ActionableWidgets { window: self, cur_node_index: NodeIndex(0) }.filter_map(
            |(widget, ..)| {
                let widget = match widget.try_read() {
                    Ok(widget) => widget,
                    Err(_) => return None,
                };

                let identifier = widget.identifier();
                if let Some(prefix) = identifier.get(..13) {
                    if prefix == "parsec-file: " {
                        return Some(String::from(identifier));
                    }
                }

                None
            },
        )
    }

    pub(crate) fn print_if_layout_changed(&self) {
        if self.ui.layout_has_changed() {
            for (widget, end_node) in self.widgets() {
                widget.print(&mut end_node.write());
            }
        }
    }
}

pub struct Widgets<'a, U>
where
    U: Ui,
{
    window: &'a Window<U>,
    cur_node_index: NodeIndex,
}

impl<'a, U> Iterator for Widgets<'a, U>
where
    U: Ui + 'static,
{
    type Item = (&'a Widget<U>, &'a RwData<EndNode<U>>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.window.find(self.cur_node_index) {
                Some(Node::EndNode { end_node, widget, .. }) => {
                    self.cur_node_index.0 += 1;
                    return Some((widget, end_node));
                }
                None => return None,
                _ => self.cur_node_index.0 += 1,
            };
        }
    }
}

pub struct ActionableWidgets<'a, U>
where
    U: Ui,
{
    window: &'a Window<U>,
    cur_node_index: NodeIndex,
}

impl<'a, U> Clone for ActionableWidgets<'a, U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        ActionableWidgets { ..*self }
    }
}

impl<'a, U> Iterator for ActionableWidgets<'a, U>
where
    U: Ui + 'static,
{
    type Item = (&'a RwData<dyn ActionableWidget<U>>, &'a str, &'a RwData<EndNode<U>>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.window.find(self.cur_node_index) {
                Some(Node::EndNode {
                    end_node,
                    widget: Widget::Actionable(widget),
                    identifier,
                    ..
                }) => {
                    self.cur_node_index.0 += 1;
                    return Some((&widget, &identifier, &end_node));
                }
                None => return None,
                _ => self.cur_node_index.0 += 1,
            };
        }
    }
}

impl<'a, U> DoubleEndedIterator for ActionableWidgets<'a, U>
where
    U: Ui + 'static,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            let node_index = NodeIndex(self.window.last_index.0 - self.cur_node_index.0);
            match self.window.find(node_index) {
                Some(Node::EndNode {
                    end_node,
                    widget: Widget::Actionable(widget),
                    identifier,
                    ..
                }) => {
                    self.cur_node_index.0 += 1;
                    return Some((widget, &identifier, &end_node));
                }
                None => return None,
                _ => self.cur_node_index.0 += 1,
            };
        }
    }
}
