use std::{fmt::Display, mem};

use crate::{
    config::{Config, RwData},
    tags::form::{CursorStyle, Form, FormPalette},
    widgets::Widget,
};

pub trait Area: PartialEq + Eq + Clone + Copy {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Resizes the children so they fit inside `self` properly.
    fn resize_children(
        &self, first: &mut Self, second: &mut Self, len: usize, first_dir: Side,
    ) -> Result<(), ()>;
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

    /// Wraps to the next line. If succesful, returns `Ok(())`, otherwise, returns `Err(())`.
    ///
    /// Unlike `next_line()`, this function should not remove any text.
    fn wrap_next(&mut self, indent: usize) -> Result<(), ()>;

    //////////////////// Getters
    /// Gets the length of a character.
    ///
    /// In a terminal, this would be in "cells", but in a variable width GUI, it could be in
    /// pixels, or em. It really depends on the implementation.
    fn get_char_len(&self, ch: char) -> usize;
}

/// A node that contains other nodes.
pub struct MidNode<U>
where
    U: Ui + ?Sized,
{
    old_area: U::Area,
    children: Vec<Node<U>>,
    axis: Axis,
    container: RwData<U::Container>,
    config: RwData<Config>,
    palette: RwData<FormPalette>,
    requested_width: Option<usize>,
    requested_height: Option<usize>,
}

impl<U> MidNode<U>
where
    U: Ui,
{
    fn new_from(container: U::Container, node: &Node<U>, side: Side) -> Self {
        MidNode {
            old_area: *container.area(),
            children: Vec::new(),
            axis: Axis::from(side),
            container: RwData::new(container),
            config: node.config(),
            palette: node.palette(),
            requested_width: None,
            requested_height: None,
        }
    }
    /// Requests a change in the width of `self`.
    pub fn request_width(&mut self, width: usize) {
        self.requested_width = Some(width);
    }

    /// Requests a change in the height of `self`.
    pub fn request_height(&mut self, height: usize) {
        self.requested_height = Some(height);
    }

    pub(crate) fn try_set_size(&mut self) -> Result<(), ()> {
        if let Some(width) = self.requested_width.take() {
            self.try_set_width(width)?;
        }
        if let Some(height) = self.requested_height.take() {
            self.try_set_height(height)?;
        }

        Ok(())
    }

    /// Tries to set a new width for `self`.
    fn try_set_width(&mut self, width: usize) -> Result<(), ()> {
        if let Side::Left | Side::Right = self.side {
            let mut container = self.container.write();
            let area = container.area_mut();
            resize(area, &self.parent, &mut self.sibling, width, self.side)?;

            let parent = self.parent.as_mut().unwrap();
            let mut parent = parent.write();
            let total_len = parent.container.read().area().width();
            let new_len = if self.is_second { width } else { total_len - width };
            parent.split.adapt_to_len(new_len, total_len);
        } else {
            todo!()
        }
        self.resize_children()?;

        Ok(())
    }

    /// Tries to set a new height for `self`.
    fn try_set_height(&mut self, height: usize) -> Result<(), ()> {
        if let Side::Top | Side::Bottom = self.side {
            let mut container = self.container.write();
            let area = container.area_mut();
            resize(area, &self.parent, &mut self.sibling, height, self.side)?;

            let parent = self.parent.as_mut().unwrap();
            let mut parent = parent.write();
            let total_len = parent.container.read().area().width();
            let new_len = if self.is_second { height } else { total_len - height };
            parent.split.adapt_to_len(new_len, total_len);
        } else {
            todo!()
        }
        self.resize_children()?;

        Ok(())
    }

    pub fn resize_children(&mut self) -> Result<(), ()> {
        let container = self.container.read();
        let self_area = container.area();

        let first_area = &mut self.children.0.area();
        let second_area = &mut self.children.1.area();
        let second_direction = self.children.1.direction();
        let width = self.split.get_second_len(self_area.width());

        self_area.resize_children(second_area, first_area, width, second_direction)?;
        self.children.0.set_area(*first_area);
        self.children.1.set_area(*second_area);

        self.children.0.resize_children_if_mid_node()?;
        self.children.1.resize_children_if_mid_node()?;

        Ok(())
    }

    /// Wether or not the size has changed since last checking.
    pub fn size_changed(&mut self) -> bool {
        let has_changed = *self.container.read().area() != self.old_area;
        self.old_area = *self.container.read().area();
        has_changed
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
    pub(crate) label: RwData<U::Label>,
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
            label: RwData::new(label),
            config: node.config(),
            palette: node.palette(),
            requested_width: None,
            requested_height: None,
            is_active: false,
        }
    }
    /// Requests a change in the width of `self`.
    pub fn request_width(&mut self, width: usize) {
        if self.label.read().area().width() != width {
            self.requested_width = Some(width);
        }
    }

    /// Requests a change in the height of `self`.
    pub fn request_height(&mut self, height: usize) {
        if self.label.read().area().height() != height {
            self.requested_height = Some(height);
        }
    }

    pub(crate) fn try_set_size(&mut self) -> Result<(), ()> {
        if let Some(width) = self.requested_width.take() {
            self.try_set_width(width)?;
        }
        if let Some(height) = self.requested_height.take() {
            self.try_set_height(height)?;
        }

        Ok(())
    }

    /// Requests a new width for itself, going up the tree.
    pub fn try_set_width(&mut self, width: usize) -> Result<(), ()> {
        if let Side::Left | Side::Right = self.side {
            let mut label = self.label.write();
            let area = label.area_mut();
            resize(area, &self.parent, &mut self.sibling, width, self.side)?;

            let parent = self.parent.as_mut().unwrap();
            let mut parent = parent.write();
            let total_len = parent.container.read().area().width();
            let new_len = if self.is_second { width } else { total_len - width };
            parent.split.adapt_to_len(new_len, total_len);
        } else {
            todo!()
        }

        Ok(())
    }

    /// Requests a new width for itself, going up the tree.
    pub fn try_set_height(&mut self, height: usize) -> Result<(), ()> {
        if let Side::Top | Side::Bottom = self.side {
            let mut label = self.label.write();
            let area = label.area_mut();
            resize(area, &self.parent, &mut self.sibling, height, self.side)?;

            let parent = self.parent.as_mut().unwrap();
            let mut parent = parent.write();
            let total_len = parent.container.read().area().width();
            let new_len = if self.is_second { height } else { total_len - height };
            parent.split.adapt_to_len(new_len, total_len);
        } else {
            todo!()
        }

        Ok(())
    }

    /// Returns a reference to the `Config` of the node.
    pub fn config(&self) -> &RwData<Config> {
        &self.config
    }

    pub fn palette(&self) -> &RwData<FormPalette> {
        &self.palette
    }

    pub fn label(&self) -> &RwData<U::Label> {
        &self.label
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
    // TODO: Remove pub(crate).
    pub(crate) fn area(&self) -> U::Area {
        match self {
            Node::MidNode { mid_node, .. } => {
                let container = mid_node.container.read();
                container.area().clone()
            }
            Node::EndNode { end_node, .. } => {
                let label = end_node.label.read();
                label.area().clone()
            }
        }
    }

    fn set_area(&mut self, area: U::Area) {
        match self {
            Node::MidNode { mid_node, .. } => {
                let mut container = mid_node.container.write();
                *container.area_mut() = area;
            }
            Node::EndNode { end_node, .. } => {
                let mut label = end_node.label.write();
                *label.area_mut() = area;
            }
        }
    }

    fn resize_children_if_mid_node(&mut self) -> Result<(), ()> {
        if let Node::MidNode { mid_node, .. } = self {
            mid_node.resize_children()
        } else {
            Ok(())
        }
    }

    fn try_split(&mut self, ui_manager: U, side: Side, split: Split) -> (U::Container, U::Label) {
        match self {
            Node::MidNode { mid_node, .. } => {
                let mut container = mid_node.container.write();
                let (container, label) = ui_manager.split_container(&mut *container, side, split);
                mid_node.resize_children().unwrap();
                (container, label)
            }
            Node::EndNode { end_node, .. } => {
                ui_manager.split_label(&mut *end_node.label.write(), side, split)
            }
        }
    }

    fn trim_area(&mut self, ui_manager: U, side: Side, split: Split) -> U::Label {
        match self {
            Node::MidNode { mid_node, .. } => {
                let mut container = mid_node.container.write();
                let label = ui_manager.trim_container(&mut *container, side, split);
                mid_node.resize_children().unwrap();
                label
            }
            Node::EndNode { end_node, .. } => {
                ui_manager.trim_label(&mut *end_node.label.write(), side, split)
            }
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

    /// Returns `Some(self)` if it is the parent of the given `NodeIndex`.
    fn find_mut_parent(&mut self, node_index: NodeIndex) -> Option<&mut MidNode<U>> {
        if let Node::MidNode { mid_node, .. } = self {
            let children = &mut mid_node.children;
            if children.iter_mut().find(|child| child.index() == node_index).is_some() {
                return Some(mid_node);
            } else {
                for child in children.iter_mut() {
                    if let Some(node) = child.find_mut_parent(node_index) {
                        return Some(node);
                    }
                }
            }
        }
        None
    }

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

    fn get_child(&mut self, index: usize) -> Option<&mut Node<U>> {
        match self {
            Node::MidNode { mid_node, .. } => mid_node.children.get_mut(index),
            Node::EndNode { end_node, .. } => None,
        }
    }
}

/// A way of splitting areas.
#[derive(Clone, Copy)]
pub enum Split {
    Locked(usize),
    Static(usize),
    Ratio(f32),
}

impl Split {
    fn get_second_len(&self, total: usize) -> usize {
        match self {
            Split::Locked(len) | Split::Static(len) => *len,
            Split::Ratio(ratio) => (total as f32 * ratio).floor() as usize,
        }
    }

    fn adapt_to_len(&mut self, new_len: usize, total_len: usize) {
        match self {
            Self::Static(len) => *len = new_len,
            Self::Ratio(ratio) => *ratio = new_len as f32 / total_len as f32,
            Self::Locked(_) => (),
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
    fn split_label(
        &mut self, label: &mut Self::Label, side: Side, split: Split,
    ) -> (Self::Container, Self::Label);

    /// Splits a container in two, and places each of the areas on a new parent area.
    ///
    /// # Returns
    ///
    /// * The new parent area first, and the new child area second.
    ///
    /// # Arguments
    ///
    /// * container: The area that will be split.
    /// * direction: In what direction, relative to the old area, will the new area be inserted.
    /// * split: How to decide where to place the barrier between the two areas. If
    ///   `Split::Static`, the new area will have a fixed size, and resizing the parent area will
    ///   only change the size of the old area. If `Split::Ratio`, resizing the parent will
    ///   maintain a ratio between the two areas.
    /// * glued: If `true`, the two areas will become inseparable, by moving one, you will move the
    ///   other one with it.
    fn split_container(
        &mut self, container: &mut Self::Container, side: Side, split: Split,
    ) -> (Self::Container, Self::Label);

    fn trim_label(&mut self, label: &mut Self::Label, side: Side, split: Split) -> Self::Label;

    fn trim_container(
        &mut self, container: &mut Self::Container, side: Side, split: Split,
    ) -> Self::Label;

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
    nodes: Vec<Node<U>>,
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
        let label = RwData::new(ui_manager.only_label().unwrap());

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
        NodeManager { ui_manager, nodes: vec![first_node], last_index: 0 }
    }

    fn try_trim(
        &mut self, index: NodeIndex, side: Side, split: Split, glued: bool, widget: Widget<U>,
    ) -> Result<NodeIndex, ()> {
        let parent = self.mut_parent_of(index);
        let Some(parent) = parent.filter(|parent| side.is_aligned(parent.axis)) else {
            return Err(());
        };

        let node = parent.children.iter_mut().find(|child| child.index() == index).unwrap();
        let label = node.trim_area(self.ui_manager, side, split);
        let index = NodeIndex(self.last_index);
        let node = Node::EndNode { end_node: EndNode::new_from(label, &node), widget, index };

        if let Side::Bottom | Side::Right = side {
            parent.children.push(node);
        } else {
            parent.children.insert(0, node);
        }

        Ok(NodeIndex(self.last_index))
    }

    pub fn push_widget(
        &mut self, index: NodeIndex, side: Side, split: Split, glued: bool, widget: Widget<U>,
    ) -> (NodeIndex, Option<NodeIndex>) {
        self.last_index += 1;
        if let Ok(end_node) = self.try_trim(index, side, split, glued, widget) {
            (end_node, None)
        } else if let Ok((end_node, mid_node)) = self.try_split(index, side, split, glued, widget) {
            (end_node, Some(mid_node))
        } else {
            panic!("This ish weally bad gouys!");
        }
    }

    // TODO: Move this to an owning struct.
    /// Splits a given `EndNode` into two, with a new parent `MidNode`, and a new child `EndNode`.
    pub fn try_split(
        &mut self, node_index: NodeIndex, side: Side, split: Split, glued: bool, widget: Widget<U>,
    ) -> Result<(NodeIndex, NodeIndex), ()> {
        let node = self.find_node(node_index);
        let (container, label) = node.try_split(self.ui_manager, side, split);

        let mut end_node = Node::EndNode {
            end_node: EndNode::new_from(label, &node),
            widget,
            index: NodeIndex(self.last_index),
        };

        self.last_index += 1;
        // This is the new parent node of the original node and this new one.
        let mut mid_node = Node::MidNode {
            mid_node: MidNode::new_from(container, &node, side),
            index: NodeIndex(self.last_index),
        };

        drop(node);
        if let Some(parent) = self.mut_parent_of(node_index) {
            let (orig_node, mid_node) = parent.replace_child_with_mid(node_index, mid_node);
            mid_node.children = if let Side::Top | Side::Left = side {
                vec![end_node, orig_node]
            } else {
                vec![orig_node, end_node]
            };
        }

        Ok((NodeIndex(self.last_index - 1), NodeIndex(self.last_index)))
    }

    /// Triggers the functions to use when the program starts.
    pub(crate) fn startup(&mut self) {
        self.ui_manager.startup();
    }

    /// Triggers the functions to use when the program ends.
    pub(crate) fn shutdown(&mut self) {
        self.ui_manager.shutdown();
    }

    fn find_node(&mut self, node_index: NodeIndex) -> &mut Node<U> {
        for node in &mut self.nodes {
            if let Some(node) = node.find_mut(node_index) {
                return node;
            }
        }
        panic!("This NodeIndex was not found, that shouldn't be possible.");
    }

    fn mut_parent_of(&mut self, node_index: NodeIndex) -> Option<&mut MidNode<U>> {
        for node in &mut self.nodes {
            if let Some(node) = node.find_mut_parent(node_index) {
                return Some(node);
            }
        }
        None
    }
}

fn resize<U>(
    target_area: &mut U::Area, parent: &Option<RwData<MidNode<U>>>, sibling: &mut Option<Node<U>>,
    len: usize, direction: Side,
) -> Result<(), ()>
where
    U: Ui,
{
    let parent = parent.as_ref().expect("You can't resize a parentless node!");
    let parent = parent.read();
    if let Split::Locked(_) = parent.split {
        return Err(());
    }
    let container = parent.container.read();
    let parent_area = container.area();

    let sibling = sibling.as_mut().unwrap();
    let sibling_area = &mut sibling.area();
    parent_area.resize_children(target_area, sibling_area, len, direction)?;
    sibling.set_area(*sibling_area);

    sibling.resize_children_if_mid_node()?;

    Ok(())
}
