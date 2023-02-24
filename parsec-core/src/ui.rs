use std::{fmt::Display, mem};

use crate::{
    config::{Config, RwData, WrapMethod},
    tags::form::{CursorStyle, Form, FormPalette},
    widgets::Widget,
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
    fn place_primary_cursor(&mut self, style: CursorStyle);

    /// Places the secondary cursor on the current printing position.
    fn place_secondary_cursor(&mut self, style: CursorStyle);

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
}

/// A node that contains other nodes.
pub struct MidNode<U>
where
    U: Ui + ?Sized,
{
    old_area: U::Area,
    children: Vec<Node<U>>,
    container: U::Container,
    config: RwData<Config>,
    palette: RwData<FormPalette>,
}

impl<U> MidNode<U>
where
    U: Ui,
{
    fn new_from(container: U::Container, node: &Node<U>) -> Self {
        MidNode {
            old_area: *container.area(),
            children: Vec::new(),
            container,
            config: node.config(),
            palette: node.palette(),
        }
    }

    /// Wether or not the size has changed since last checking.
    pub fn size_changed(&mut self) -> bool {
        todo!()
        // let has_changed = *self.container.read().area() != self.old_area;
        // self.old_area = *self.container.read().area();
        // has_changed
    }

    fn replace_child_with_mid(
        &mut self, node_index: NodeIndex, node: Node<U>,
    ) -> (Node<U>, &mut MidNode<U>) {
        let index = self.children.iter_mut().position(|child| child.index() == node_index).unwrap();
        let end_node = mem::replace(&mut self.children[index], node);
        let Node::MidNode { mid_node, .. } = &mut self.children[index] else {
            unreachable!();
        };
        (end_node, mid_node)
    }
}

/// Node that contains a `Label`.
pub struct EndNode<U>
where
    U: Ui + ?Sized,
{
    pub(crate) label: U::Label,
    pub(crate) config: RwData<Config>,
    pub(crate) palette: RwData<FormPalette>,
    requested_width: Option<usize>,
    requested_height: Option<usize>,
    pub(crate) is_active: bool,
}

impl<U> EndNode<U>
where
    U: Ui,
{
    fn new_from(label: U::Label, node: &Node<U>) -> Self {
        EndNode {
            label: label,
            config: node.config(),
            palette: node.palette(),
            requested_width: None,
            requested_height: None,
            is_active: false,
        }
    }

    /// Requests a change in the width of `self`.
    pub fn request_width(&mut self, width: usize) {
        if self.label.area().width() != width {
            self.requested_width = Some(width);
        }
    }

    /// Requests a change in the height of `self`.
    pub fn request_height(&mut self, height: usize) {
        if self.label.area().height() != height {
            self.requested_height = Some(height);
        }
    }

    /// Returns a reference to the `Config` of the node.
    pub fn config(&self) -> &RwData<Config> {
        &self.config
    }

    pub fn palette(&self) -> &RwData<FormPalette> {
        &self.palette
    }

    /// Wether or not the size has changed since last checking.
    pub fn resize_requested(&self) -> bool {
        self.requested_height.is_some() || self.requested_width.is_some()
    }
}

/// Container for middle and end nodes.
pub enum Node<U>
where
    U: Ui + ?Sized,
{
    MidNode { mid_node: MidNode<U>, index: NodeIndex },
    EndNode { end_node: EndNode<U>, widget: Widget<U>, index: NodeIndex },
}

impl<U> Node<U>
where
    U: Ui,
{
    fn config(&self) -> RwData<Config> {
        match self {
            Node::MidNode { mid_node, .. } => mid_node.config.clone(),
            Node::EndNode { end_node, .. } => end_node.config.clone(),
        }
    }

    fn palette(&self) -> RwData<FormPalette> {
        match self {
            Node::MidNode { mid_node, .. } => mid_node.palette.clone(),
            Node::EndNode { end_node, .. } => end_node.palette.clone(),
        }
    }

    fn index(&self) -> NodeIndex {
        match self {
            Node::MidNode { index: node_index, .. } => *node_index,
            Node::EndNode { index: node_index, .. } => *node_index,
        }
    }

    pub(crate) fn take(
        &mut self, node_index: NodeIndex,
    ) -> Option<(Node<U>, &mut MidNode<U>, usize)>
    where
        U: Ui,
    {
        let Node::MidNode { mid_node, .. } = &mut self else {
            return None;
        };

        if let Some(pos) = mid_node.children.iter().position(|child| child.index() == node_index) {
            Some((mid_node.children.remove(pos), mid_node, pos))
        } else {
            for child in &mut mid_node.children {
                if let Some((node, mid_node, pos)) = child.take(node_index) {
                    return Some((node, mid_node, pos));
                }
            }

            None
        }
    }

    fn find_mut(&mut self, node_index: NodeIndex) -> Option<&mut Node<U>> {
        if self.index() == node_index {
            return Some(self);
        } else if let Node::MidNode { mid_node, .. } = self {
            for child in mid_node.children {
                if let Some(node) = child.find_mut(node_index) {
                    return Some(node);
                }
            }
        }
        None
    }

    /// Returns `Some((self, split_of_node))` if it is the parent of the given `NodeIndex`.
    fn find_mut_parent(&mut self, node_index: NodeIndex) -> Option<(&mut MidNode<U>, usize)> {
        let Node::MidNode { mid_node, .. } = self else {
            return None;
        };

        if let Some(pos) = mid_node.children.iter().position(|child| child.index() == node_index) {
            Some((mid_node, pos))
        } else {
            for child in &mut mid_node.children {
                if let Some((mid_node, pos)) = child.find_mut_parent(node_index) {
                    return Some((mid_node, pos));
                }
            }

            None
        }
    }

    fn area_mut(&mut self) -> &mut U::Area
    where
        U: Ui,
    {
        match self {
            Node::MidNode { mid_node, .. } => mid_node.container.area_mut(),
            Node::EndNode { end_node, .. } => end_node.label.area_mut(),
        }
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

/// The direction in which a secondary node was placed in relation to the first one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    Top,
    Right,
    Bottom,
    Left,
}

impl Side {
    pub fn opposite(&self) -> Side {
        match self {
            Side::Top => Side::Bottom,
            Side::Bottom => Side::Top,
            Side::Left => Side::Right,
            Side::Right => Side::Left,
        }
    }

    fn is_aligned(&self, axis: Axis) -> bool {
        match self {
            Side::Top | Side::Bottom => matches!(axis, Axis::Vertical),
            Side::Right | Side::Left => matches!(axis, Axis::Horizontal),
        }
    }
}

/// All the methods that a working gui/tui will need to implement, in order to use Parsec.
pub trait Ui {
    type Area: Area + Display;
    type Container: Container<<Self as Ui>::Area> + Clone + Send + Sync;
    type Label: Label<<Self as Ui>::Area> + Clone + Send + Sync;

    /// Splits an area in two, and places each of the areas on a new parent area.
    ///
    /// # Returns
    ///
    /// * The new parent area first, and the new child area second.
    ///
    /// # Arguments
    ///
    /// * label: The area that will be split.
    /// * direction: In what direction, relative to the old area, will the new area be inserted.
    /// * split: How to decide where to place the barrier between the two areas. If
    ///   `Split::Static`, the new area will have a fixed size, and resizing the parent area will
    ///   only change the size of the old area. If `Split::Ratio`, resizing the parent will
    ///   maintain a ratio between the two areas.
    /// * glued: If `true`, the two areas will become inseparable, by moving one, you will move the
    ///   other one with it.
    fn push_label(
        &mut self, area: &mut Self::Area, side: Side, split: Split,
    ) -> (Self::Label, Option<Self::Container>);

    /// Returns `Some(_)` only if the node tree contains a single `Label`, and no `Container`s.
    fn only_label(&mut self) -> Option<Self::Label>;

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);

    /// Functions to trigger once every `Label` has been printed.
    fn finish_all_printing(&mut self);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct NodeIndex(usize);

/// A manager for nodes.
pub struct NodeManager<U>
where
    U: Ui,
{
    ui_manager: U,
    windows: Vec<Node<U>>,
    floating: Vec<Node<U>>,
    last_index: usize,
}

impl<U> NodeManager<U>
where
    U: Ui,
{
    /// Returns a new instance of `NodeManager`.
    pub fn new(
        ui_manager: U, widget: Widget<U>, config: RwData<Config>, palette: RwData<FormPalette>,
    ) -> Self {
        let label = ui_manager.only_label().unwrap();

        let first_node = Node::EndNode {
            end_node: EndNode {
                label,
                config,
                palette,
                requested_width: None,
                requested_height: None,
                is_active: true,
            },
            widget,
            index: NodeIndex(0),
        };
        NodeManager { ui_manager, windows: vec![first_node], floating: Vec::new(), last_index: 0 }
    }

    pub fn push_widget(
        &mut self, index: NodeIndex, side: Side, split: Split, glued: bool, widget: Widget<U>,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.last_index += 1;

        let (end_node, mid_node) = self.split_node(index, side, split, widget);

        if let Some(mid_node) = mid_node {
            self.last_index += 1;

            self.rebranch_tree(index, mid_node, end_node, side);

            (NodeIndex(self.last_index - 1), Some(NodeIndex(self.last_index)))
        } else {
            if let Some((parent, pos)) = self.mut_parent_of(index) {
                if let Side::Top | Side::Left = side {
                    parent.children.insert(pos, end_node);
                } else {
                    parent.children.insert(pos + 1, end_node);
                }
            }

            (NodeIndex(self.last_index), None)
        }
    }

	/// Splits a given `Node` into two, and if necessary, returns a new parent for the 2 `Node`s.
    fn split_node(
        &mut self, index: NodeIndex, side: Side, split: Split, widget: Widget<U>,
    ) -> (Node<U>, Option<MidNode<U>>) {
        let node = self.find_mut(index);
        let area = node.area_mut();
        let (label, container) = self.ui_manager.push_label(area, side, split);

        let mid_node = container.map(|container| MidNode::new_from(container, &node));

        let end_node = EndNode::new_from(label, &node);
        let end_node = Node::EndNode { end_node, index: NodeIndex(self.last_index), widget };

        (end_node, mid_node)
    }

	/// Rebranches a tree, assuming that the parent of a given `Node` has been changed.
    fn rebranch_tree(
        &mut self, index: NodeIndex, mut mid_node: MidNode<U>, end_node: Node<U>, side: Side,
    ) {
        let (target_node, descendency) = self.take(index);
        mid_node.children.push(target_node);
        let insert_index = if let Side::Top | Side::Left = side { 0 } else { 1 };
        mid_node.children.insert(insert_index, end_node);

        let mid_node = Node::MidNode { mid_node, index: NodeIndex(self.last_index) };
        if let Some((parent, target_index)) = descendency {
            parent.children.insert(target_index, mid_node);
        } else {
            self.windows.push(mid_node);
        }
    }

    /// Triggers the functions to use when the program starts.
    pub(crate) fn startup(&mut self) {
        self.ui_manager.startup();
    }

    /// Triggers the functions to use when the program ends.
    pub(crate) fn shutdown(&mut self) {
        self.ui_manager.shutdown();
    }

    fn take(&mut self, index: NodeIndex) -> (Node<U>, Option<(&mut MidNode<U>, usize)>) {
        if let Some(pos) = self.windows.iter().position(|window| window.index() == index) {
            return (self.windows.remove(pos), None);
        }
        for node in &mut self.windows {
            if let Some((node, mid_node, pos)) = node.take(index) {
                return (node, Some((mid_node, pos)));
            }
        }
        panic!("This NodeIndex was not found, that shouldn't be possible.");
    }

    fn find_mut(&mut self, index: NodeIndex) -> &mut Node<U> {
        for node in &mut self.windows {
            if let Some(node) = node.find_mut(index) {
                return node;
            }
        }
        panic!("This NodeIndex was not found, that shouldn't be possible.");
    }

    fn mut_parent_of(&mut self, index: NodeIndex) -> Option<(&mut MidNode<U>, usize)> {
        for node in &mut self.windows {
            if let Some(node) = node.find_mut_parent(index) {
                return Some(node);
            }
        }
        None
    }
}
