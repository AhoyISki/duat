use std::{
    fmt::Display,
    iter::{Cycle, Filter},
    sync::{Arc, Mutex},
};

use crate::{
    config::{Config, TabPlaces, WrapMethod},
    tags::form::{CursorStyle, Form},
    widgets::{ActionableWidget, Widget},
};

pub trait Area: PartialEq + Eq + Clone {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Requests a new width to the widget.
    fn request_len(&mut self, len: usize, side: Side) -> Result<(), ()>;

    /// Requests that the width be enough to fit a certain piece of text.
    fn request_width_to_fit(&mut self, text: &str) -> Result<(), ()>;
}

/// A `Label` or `Container` container, that holds exactly two in total.
pub trait Container<A>
where
    A: Area,
{
    /// Returns a mutable reference to the area of `self`.
    fn area_mut(&mut self) -> &mut A;

    /// Returns a reference to the area of `self`.
    fn area(&self) -> &A;
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
    fn print(&mut self, ch: char);

    /// Moves to the next line. If succesful, returns `Ok(())`, otherwise, returns `Err(())`.
    ///
    /// This function should also make sure that there is no leftover text after the current line's
    /// end.
    fn next_line(&mut self) -> Result<(), ()>;

    /// Counts how many times the given string would wrap.
    fn wrap_count(&self, text: &str, wrap_method: WrapMethod) -> usize;

    /// Gets the visual width to a given column.
    fn get_width(&self, text: &str, tab_places: &TabPlaces) -> usize;

    /// Gets the column at the given distance from the left side.
    fn get_col_at_dist(&self, text: &str, dist: usize, tab_places: &TabPlaces) -> usize;
}

/// A node that contains other nodes.
pub struct MidNode<U>
where
    U: Ui + ?Sized,
{
    container: U::Container,
    config: Config,
}

impl<U> MidNode<U>
where
    U: Ui,
{
    fn new_from(container: U::Container, node: &Node<U>) -> Self {
        MidNode { container, config: node.config() }
    }
}

/// Node that contains a `Label`.
pub struct EndNode<U>
where
    U: Ui + ?Sized,
{
    pub(crate) label: U::Label,
    pub(crate) config: Config,
    pub(crate) is_active: bool,
}

impl<U> EndNode<U>
where
    U: Ui,
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
pub enum Node<U>
where
    U: Ui + ?Sized,
{
    MidNode { mid_node: Arc<Mutex<MidNode<U>>>, children: Vec<Node<U>>, node_index: NodeIndex },
    EndNode { end_node: Arc<Mutex<EndNode<U>>>, widget: Widget<U>, node_index: NodeIndex },
}

impl<U> Node<U>
where
    U: Ui,
{
    fn config(&self) -> Config {
        match self {
            Node::MidNode { mid_node, .. } => mid_node.lock().unwrap().config.clone(),
            Node::EndNode { end_node, .. } => end_node.lock().unwrap().config.clone(),
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
                ui.bisect_area(mid_node.lock().unwrap().container.area_mut(), side, split)
            }
            Node::EndNode { end_node, .. } => {
                ui.bisect_area(end_node.lock().unwrap().label.area_mut(), side, split)
            }
        };

        let end_node = Arc::new(Mutex::new(EndNode::new_from(label, &self)));
        let end_node = Node::EndNode { end_node, node_index: last_index, widget };

        let mid_node = container.map(|container| MidNode::new_from(container, &self));

        (end_node, mid_node)
    }

    /// Rebranches a tree, assuming that the parent of a given `Node` has been changed.
    fn replace_with_mid(&mut self, mut new_node: Node<U>, end_node: Node<U>, side: Side) {
        std::mem::swap(self, &mut new_node);

        let Node::MidNode { mid_node, children, node_index } = self else {
    		unreachable!();
		};
        let mut mid_node = mid_node.lock().unwrap();
        *node_index = new_node.node_index();

        children.push(new_node);
        let insert_index = if let Side::Top | Side::Left = side { 0 } else { 1 };
        children.insert(insert_index, end_node);
    }
}

pub struct ModNode<'a, U>
where
    U: Ui,
{
    node_manager: &'a mut Window<U>,
    node_index: NodeIndex,
}

impl<'a, U> ModNode<'a, U>
where
    U: Ui,
{
    pub fn push_widget(
        &mut self, widget: Widget<U>, side: Side, split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.node_manager.push_widget(self.node_index, widget, side, split, glued)
    }

    pub fn push_widget_to_node(
        &mut self, widget: Widget<U>, node_index: NodeIndex, side: Side, split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.node_manager.push_widget(node_index, widget, side, split, glued)
    }
}

/// A way of splitting areas.
#[derive(Clone, Copy)]
pub enum Split {
    Locked(usize),
    Minimum(usize),
}

impl Split {
    pub fn len(&self) -> usize {
        match self {
            Split::Locked(len) | Split::Minimum(len) => *len,
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
pub trait Ui {
    type Area: Area + Display;
    type Container: Container<<Self as Ui>::Area> + Clone + Send + Sync;
    type Label: Label<<Self as Ui>::Area> + Clone + Send + Sync;

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
    ) -> (Self::Label, Option<Self::Container>);

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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeIndex(pub(crate) usize);

/// A "viewport" of Parsec. It contains a group of widgets that can be displayed at the same time.
pub struct Window<U>
where
    U: Ui,
{
    ui: U,
    main_node: Node<U>,
    floating_nodes: Vec<Node<U>>,
    last_index: NodeIndex,
    files_parent: NodeIndex,
}

impl<U> Window<U>
where
    U: Ui,
{
    /// Returns a new instance of `NodeManager`.
    pub fn new<C>(mut ui: U, widget: Widget<U>, config: Config, constructor_hook: &C) -> Self
    where
        C: Fn(ModNode<U>),
    {
        let label = ui.maximum_label();

        let main_node = Node::EndNode {
            end_node: Arc::new(Mutex::new(EndNode { label, config, is_active: true })),
            widget,
            node_index: NodeIndex(0),
        };
        let mut node_manager = Window {
            ui,
            main_node,
            floating_nodes: Vec::new(),
            last_index: NodeIndex(0),
            files_parent: NodeIndex(0),
        };

        let mod_node = ModNode { node_manager: &mut node_manager, node_index: NodeIndex(0) };
        (constructor_hook)(mod_node);

        node_manager
    }

    fn push_hooked(
        &mut self, node_index: NodeIndex, widget: Widget<U>, side: Side, split: Split, glued: bool,
        constructor_hook: &dyn Fn(ModNode<U>),
    ) -> (NodeIndex, Option<NodeIndex>) {
        let (new_node, maybe_node) = self.push_widget(node_index, widget, side, split, glued);

        let mod_node = ModNode { node_manager: self, node_index: new_node };
        (constructor_hook)(mod_node);

        (new_node, maybe_node)
    }

    /// Pushes a `Widget` onto an
    fn push_widget(
        &mut self, node_index: NodeIndex, widget: Widget<U>, side: Side, split: Split, glued: bool,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.last_index.0 += 1;

        let Some(target_node) = self.main_node.find_mut(node_index) else {
            panic!("Node not found");
        };
        let (end_node, mid_node) =
            target_node.bisect(side, split, widget, self.last_index, &mut self.ui);

        if let Some(mid_node) = mid_node {
            let mid_node = Arc::new(Mutex::new(mid_node));
            self.last_index.0 += 1;

            // Here, I swap the `NodeIndex`es in order to keep the same node "position".
            let new_node = Node::MidNode {
                mid_node,
                children: Vec::new(),
                node_index: target_node.node_index(),
            };
            match target_node {
                Node::MidNode { node_index: index, .. } => *index = self.last_index,
                Node::EndNode { node_index: index, .. } => *index = self.last_index,
            }

            target_node.replace_with_mid(new_node, end_node, side);

            if !glued && node_index == self.files_parent {
                self.files_parent = self.last_index;
            }

            (NodeIndex(self.last_index.0 - 1), Some(self.last_index))
        } else {
            drop(target_node);
            if let Some((children, pos)) = self.mut_parent_of(node_index) {
                if let Side::Top | Side::Left = side {
                    children.insert(pos, end_node);
                } else {
                    children.insert(pos + 1, end_node);
                }
            }

            (self.last_index, None)
        }
    }

    /// Pushes a `Widget` to the parent of all files.
    pub fn push_to_files(
        &mut self, widget: Widget<U>, side: Side, split: Split, glued: bool,
        constructor_hook: &dyn Fn(ModNode<U>),
    ) -> (NodeIndex, Option<NodeIndex>) {
        let node_index = self.files_parent;
        let (new_node, maybe_node) = self.push_widget(node_index, widget, side, split, glued);

        let mod_node = ModNode { node_manager: self, node_index: new_node };
        (constructor_hook)(mod_node);

        (new_node, maybe_node)
    }

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

    fn find(&self, node_index: NodeIndex) -> &Node<U> {
        if let Some(node) = self.main_node.find(node_index) {
            return node;
        }
        panic!("This NodeIndex was not found, that shouldn't be possible.");
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

    pub fn files(
        &self,
    ) -> impl Iterator<Item = String> + DoubleEndedIterator + Clone + '_ {
        ActionableWidgets { window: self, cur_node_index: NodeIndex(0) }.filter_map(|(widget, _)| {
            let Ok(widget) = widget.try_lock() else {
                return None;
            };

			let identifier = widget.identifier();
            if let Some(prefix) = identifier.get(..13) {
                if prefix == "parsec-file: " {
                    return Some(String::from(identifier))
                }
            }

            None
        })
    }

    pub(crate) fn print_if_layout_changed(&self) {
        if self.ui.layout_has_changed() {
            for (widget, end_node) in self.widgets() {
                widget.print(&mut end_node.lock().unwrap());
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
    U: Ui,
{
    type Item = (&'a Widget<U>, &'a Arc<Mutex<EndNode<U>>>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cur_node_index > self.window.last_index {
                return None;
            }

            if let Node::EndNode { end_node, widget, .. } = self.window.find(self.cur_node_index) {
                return Some((widget, &end_node));
            };

            self.cur_node_index.0 += 1;
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
    U: Ui,
{
    type Item = (&'a Arc<Mutex<dyn ActionableWidget<U>>>, &'a Arc<Mutex<EndNode<U>>>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cur_node_index > self.window.last_index {
                return None;
            }

            if let Node::EndNode { end_node, widget, .. } = self.window.find(self.cur_node_index) {
                if let Widget::Actionable(widget) = widget {
                    return Some((&widget, &end_node));
                }
            };

            self.cur_node_index.0 += 1;
        }
    }
}

impl<'a, U> DoubleEndedIterator for ActionableWidgets<'a, U>
where
    U: Ui,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            if self.cur_node_index > self.window.last_index {
                return None;
            }

            let node_index = NodeIndex(self.window.last_index.0 - self.cur_node_index.0);
            if let Node::EndNode { end_node, widget, .. } = self.window.find(node_index) {
                if let Widget::Actionable(widget) = widget {
                    return Some((&widget, &end_node));
                }
            };

            self.cur_node_index.0 += 1;
        }
    }
}
