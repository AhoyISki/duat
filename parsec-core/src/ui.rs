use std::fmt::Display;

use crate::{
    config::{Config, RwData},
    tags::form::{CursorStyle, Form, FormPalette},
};

pub trait Area: PartialEq + Eq + Clone + Copy {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Resizes the children so they fit inside `self` properly.
    fn resize_children(
        &self, first: &mut Self, second: &mut Self, len: usize, first_dir: Direction,
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

    // TODO: Give it a default form.
    /// Clears the current form.
    fn clear_form(&mut self);

    // TODO: Give it a default form.
    /// Places the primary cursor on the current printing position.
    fn place_primary_cursor(&mut self, style: CursorStyle);

    // TODO: Give it a default form.
    /// Places the secondary cursor on the current printing position.
    fn place_secondary_cursor(&mut self, style: CursorStyle);

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
    fn wrap_line(&mut self, indent: usize) -> Result<(), ()>;

    //////////////////// Getters
    /// Gets the length of a character.
    ///
    /// In a terminal, this would be in "cells", but in a variable width GUI, it could be in
    /// pixels, or em. It really depends on the implementation.
    fn get_char_len(&self, ch: char) -> usize;
}

/// A node that contains other nodes.
#[derive(Clone)]
pub struct MidNode<U>
where
    U: Ui + ?Sized,
{
    old_area: U::Area,
    container: RwData<U::Container>,
    class: RwData<String>,
    direction: Direction,
    parent: Option<RwData<MidNode<U>>>,
    sibling: Option<Node<U>>,
    // TODO: Remove pub(crate);
    pub(crate) children: (Node<U>, Node<U>),
    split: Split,
    config: RwData<Config>,
    palette: RwData<FormPalette>,
    requested_width: Option<usize>,
    requested_height: Option<usize>,
    is_second: bool,
}

impl<U> MidNode<U>
where
    U: Ui,
{
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
        if let Direction::Left | Direction::Right = self.direction {
            let mut container = self.container.write();
            let area = container.area_mut();
            resize(area, &self.parent, &mut self.sibling, width, self.direction)?;

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
        if let Direction::Top | Direction::Bottom = self.direction {
            let mut container = self.container.write();
            let area = container.area_mut();
            resize(area, &self.parent, &mut self.sibling, height, self.direction)?;

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
}

/// Node that contains a `Label`.
pub struct EndNode<U>
where
    U: Ui + ?Sized,
{
    pub(crate) label: RwData<U::Label>,
    class: RwData<String>,
    parent: Option<RwData<MidNode<U>>>,
    sibling: Option<Node<U>>,
    direction: Direction,
    pub(crate) config: RwData<Config>,
    pub(crate) palette: RwData<FormPalette>,
    applied_forms: Vec<(Form, u16)>,
    requested_width: Option<usize>,
    requested_height: Option<usize>,
    is_second: bool,
}

impl<U> EndNode<U>
where
    U: Ui,
{
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
        if let Direction::Left | Direction::Right = self.direction {
            let mut label = self.label.write();
            let area = label.area_mut();
            resize(area, &self.parent, &mut self.sibling, width, self.direction)?;

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
        if let Direction::Top | Direction::Bottom = self.direction {
            let mut label = self.label.write();
            let area = label.area_mut();
            resize(area, &self.parent, &mut self.sibling, height, self.direction)?;

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

    /// Completely clears the stack of `Form`s.
    pub fn clear_form(&mut self) {
        self.applied_forms.clear();
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
    MidNode(RwData<MidNode<U>>),
    EndNode(RwData<EndNode<U>>),
}

impl<U> Node<U>
where
    U: Ui,
{
    // TODO: Remove pub(crate).
    pub(crate) fn area(&self) -> U::Area {
        match self {
            Node::MidNode(node) => {
                let node = node.read();
                let container = node.container.read();
                container.area().clone()
            }
            Node::EndNode(node) => {
                let node = node.read();
                let label = node.label.read();
                label.area().clone()
            }
        }
    }

    fn set_area(&mut self, area: U::Area) {
        match self {
            Node::MidNode(node) => {
                let mut node = node.write();
                let mut container = node.container.write();
                *container.area_mut() = area;
            }
            Node::EndNode(node) => {
                let mut node = node.write();
                let mut label = node.label.write();
                *label.area_mut() = area;
            }
        }
    }

    fn direction(&self) -> Direction {
        match self {
            Node::MidNode(node) => node.read().direction,
            Node::EndNode(node) => node.read().direction,
        }
    }

    fn resize_children_if_mid_node(&mut self) -> Result<(), ()> {
        if let Node::MidNode(mid_node) = self {
            mid_node.write().resize_children()
        } else {
            Ok(())
        }
    }
}

impl<U> Clone for Node<U>
where
    U: Ui + ?Sized,
{
    fn clone(&self) -> Self {
        match self {
            Node::MidNode(data) => Node::MidNode(data.clone()),
            Node::EndNode(data) => Node::EndNode(data.clone()),
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

/// The direction in which a secondary node was placed in relation to the first one.
#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Top,
    Right,
    Bottom,
    Left,
}

impl Direction {
    pub fn opposite(&self) -> Direction {
        match self {
            Direction::Top => Direction::Bottom,
            Direction::Bottom => Direction::Top,
            Direction::Left => Direction::Right,
            Direction::Right => Direction::Left,
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
        &mut self, label: &mut Self::Label, direction: Direction, split: Split, glued: bool,
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
        &mut self, container: &mut Self::Container, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label);

    /// Returns `Some(_)` only if the node tree contains a single `Label`, and no `Container`s.
    fn only_label(&mut self) -> Option<Self::Label>;

    /// Functions to trigger when the program begins.
    fn startup(&mut self);

    /// Functions to trigger when the program ends.
    fn shutdown(&mut self);

    /// Functions to trigger once every `Label` has been printed.
    fn finish_all_printing(&mut self);
}

/// A manager for nodes.
pub struct NodeManager<U>(U)
where
    U: Ui;

impl<U> NodeManager<U>
where
    U: Ui,
{
    /// Returns a new instance of `NodeManager`.
    pub fn new(ui_manager: U) -> Self {
        NodeManager(ui_manager)
    }

    /// Returns an `EndNode` only if it is the only node in the `Ui`.
    pub fn only_child(
        &mut self, config: Config, palette: FormPalette, class: &str,
    ) -> Option<RwData<EndNode<U>>> {
        self.0.only_label().map(|l| {
            RwData::new(EndNode {
                label: RwData::new(l),
                class: RwData::new(String::from(class)),
                parent: None,
                sibling: None,
                direction: Direction::Top,
                config: RwData::new(config),
                palette: RwData::new(palette),
                applied_forms: Vec::new(),
                requested_width: None,
                requested_height: None,
                is_second: false,
            })
        })
    }

    // TODO: Move this to an owning struct.
    /// Splits a given `EndNode` into two, with a new parent `MidNode`, and a new child `EndNode`.
    pub fn split_end(
        &mut self, node: &mut RwData<EndNode<U>>, direction: Direction, split: Split, glued: bool,
    ) -> (RwData<MidNode<U>>, RwData<EndNode<U>>) {
        let cloned_node = node.clone();
        let mut raw_node = node.write();
        let (container, label) =
            self.0.split_label(&mut raw_node.label.write(), direction, split, glued);

        let mut end_node = RwData::new(EndNode {
            label: RwData::new(label),
            class: raw_node.class.clone(),
            parent: None,
            sibling: Some(Node::EndNode(cloned_node.clone())),
            direction,
            config: raw_node.config.clone(),
            palette: raw_node.palette.clone(),
            applied_forms: Vec::new(),
            requested_width: None,
            requested_height: None,
            is_second: true,
        });

        let mid_node = RwData::new(MidNode {
            old_area: *container.area(),
            container: RwData::new(container),
            class: raw_node.class.clone(),
            direction: raw_node.direction,
            parent: raw_node.parent.clone(),
            sibling: raw_node.sibling.clone(),
            children: (Node::EndNode(cloned_node.clone()), Node::EndNode(end_node.clone())),
            split,
            config: raw_node.config.clone(),
            palette: raw_node.palette.clone(),
            requested_width: None,
            requested_height: None,
            is_second: raw_node.is_second,
        });

        raw_node.parent = Some(mid_node.clone());
        raw_node.direction = direction.opposite();
        raw_node.sibling = Some(Node::EndNode(end_node.clone()));
        end_node.write().parent = Some(mid_node.clone());

        (mid_node, end_node)
    }

    // TODO: Fix split, if necessary.
    /// Splits a given `MidNode` into two, with a new parent `MidNode`, and a new child `EndNode`.
    pub fn split_mid(
        &mut self, node: &mut RwData<MidNode<U>>, direction: Direction, split: Split, glued: bool,
    ) -> (RwData<MidNode<U>>, RwData<EndNode<U>>) {
        let (container, label) =
            self.0.split_container(&mut node.write().container.write(), direction, split, glued);

        let cloned_node = node.clone();
        let mut raw_node = node.write();
        let mut end_node = RwData::new(EndNode {
            label: RwData::new(label),
            class: raw_node.class.clone(),
            parent: None,
            sibling: Some(Node::MidNode(cloned_node.clone())),
            direction,
            config: raw_node.config.clone(),
            palette: raw_node.palette.clone(),
            applied_forms: Vec::new(),
            requested_width: None,
            requested_height: None,
            is_second: true,
        });

        let mid_node = RwData::new(MidNode {
            old_area: *container.area(),
            container: RwData::new(container),
            class: raw_node.class.clone(),
            direction: raw_node.direction,
            parent: raw_node.parent.clone(),
            sibling: raw_node.sibling.clone(),
            children: (Node::MidNode(cloned_node.clone()), Node::EndNode(end_node.clone())),
            split,
            config: raw_node.config.clone(),
            palette: raw_node.palette.clone(),
            requested_width: None,
            requested_height: None,
            is_second: raw_node.is_second,
        });

        raw_node.parent = Some(mid_node.clone());
        raw_node.direction = direction.opposite();
        raw_node.sibling = Some(Node::EndNode(end_node.clone()));
        raw_node.resize_children().unwrap();

        end_node.write().parent = Some(mid_node.clone());

        (mid_node, end_node)
    }

    /// Triggers the functions to use when the program starts.
    pub(crate) fn startup(&mut self) {
        self.0.startup();
    }

    /// Triggers the functions to use when the program ends.
    pub(crate) fn shutdown(&mut self) {
        self.0.shutdown();
    }

    /// Triggers the functions to once every `Label` has been printed.
    pub(crate) fn finish_all_printing(&mut self) {
        self.0.finish_all_printing()
    }
}

fn resize<U>(
    target_area: &mut U::Area, parent: &Option<RwData<MidNode<U>>>, sibling: &mut Option<Node<U>>,
    len: usize, direction: Direction,
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
