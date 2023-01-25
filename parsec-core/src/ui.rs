use std::fmt::Display;

use crate::{
    config::{Config, RwData},
    log_info,
    tags::{CursorStyle, Form, FormPalette},
};

pub trait Area: PartialEq + Eq + Clone + Copy {
    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// Requests that the width be changed, and consequently changes the width of `other`.
    fn request_width_left(
        &mut self, width: usize, total: usize, other: &mut Self,
    ) -> Result<(), ()>;

    /// Requests that the width be changed, and consequently changes the width of `other`.
    fn request_width_right(
        &mut self, width: usize, total: usize, other: &mut Self,
    ) -> Result<(), ()>;

    /// Requests that the height be changed, and consequently changes the height of `other`.
    fn request_height_top(
        &mut self, height: usize, total: usize, other: &mut Self,
    ) -> Result<(), ()>;

    /// Requests that the height be changed, and consequently changes the height of `other`.
    fn request_height_bottom(
        &mut self, height: usize, total: usize, other: &mut Self,
    ) -> Result<(), ()>;
}

trait Resizable {
    fn request_width(&mut self, width: usize);

    fn request_height(&mut self, width: usize);
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
}

impl<U> MidNode<U>
where
    U: Ui,
{
    /// Requests a new width for itself, going up the tree.
    pub fn request_width(&mut self, width: usize) {
        if let Some(sibling) = &mut self.sibling {
            let mut container = self.container.write();
            let area = container.area_mut();
            let mut sibling_area = sibling.area();
            let total_width = if let Some(parent) = &self.parent {
                let parent = parent.read();
                let container = parent.container.read();
                container.area().width()
            } else {
                area.width()
            };
            // This probably means that the area is not big enough to acomodate such a change.
            if let Err(()) =
                request_width::<U>(area, &mut sibling_area, width, total_width, self.direction)
            {
                todo!();
            }
            sibling.set_area(sibling_area);

            drop(container);
            if let Err(()) = self.resize_childrens_width() {
                unimplemented!();
            }
        }
    }

    fn resize_childrens_width(&mut self) -> Result<(), ()> {
        let total_width = self.container.read().area().width();

        let (first, second) = (&mut self.children.0.area(), &mut self.children.1.area());
        log_info!("\nbefore: {}, {}", first, second);
        let first_direction = self.children.0.direction();
        if let Direction::Left | Direction::Right = first_direction {
            match self.split {
                Split::Locked(width) | Split::Static(width) => {
                    let width = total_width - width;
                    request_width::<U>(first, second, width, total_width, first_direction)?;
                }
                Split::Ratio(ratio) => {
                    let width = (total_width as f32 * ratio).floor() as usize;
                    request_width::<U>(first, second, width, total_width, first_direction)?;
                }
            }
        } else {
            if let Some(sibling) = &mut self.sibling {
                let mut sibling_area = sibling.area();
                let width = total_width;
                let total_width = total_width + sibling_area.width();
                request_width::<U>(first, &mut sibling_area, width, total_width, self.direction)?;
                request_width::<U>(second, &mut sibling_area, width, total_width, self.direction)?;
                sibling.set_area(sibling_area);
            } else {
                panic!("Something's wrong, I can feel it.");
            }
        }
        log_info!("\nafter: {}, {}", first, second);
        self.children.0.set_area(*first);
        self.children.1.set_area(*second);

        Ok(())
    }

    /// Requests a new width for itself, going up the tree.
    pub fn request_height(&mut self, height: usize) {
        if let Some(node) = &mut self.sibling {
            let mut widget = self.container.write();
            let area = widget.area_mut();
            let total_height = area.height();
            if let Err(()) = request_height::<U>(area, node, height, total_height, self.direction) {
                todo!();
            }
        }
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
    old_area: U::Area,
    pub(crate) label: RwData<U::Label>,
    class: RwData<String>,
    parent: Option<RwData<MidNode<U>>>,
    sibling: Option<Node<U>>,
    direction: Direction,
    pub(crate) config: RwData<Config>,
    pub(crate) palette: RwData<FormPalette>,
    applied_forms: Vec<(Form, u16)>,
}

impl<U> EndNode<U>
where
    U: Ui,
{
    /// Completely clears the stack of `Form`s.
    pub fn clear_form(&mut self) {
        self.applied_forms.clear();
    }

    /// Requests a new width for itself, going up the tree.
    pub fn request_width(&mut self, width: usize) {
        if let Some(sibling) = &mut self.sibling {
            let mut sibling_area = sibling.area();
            let mut label = self.label.write();
            let area = label.area_mut();
            let total_width = sibling_area.width() + area.width();
            match request_width::<U>(area, &mut sibling_area, width, total_width, self.direction) {
                Ok(()) => {
                    sibling.set_area(sibling_area);
                    if let Node::MidNode(sibling) = sibling {
                        sibling.write().resize_childrens_width().unwrap();
                    }
                }
                Err(()) => {
                    todo!();
                }
            }
        }
    }

    /// Requests a new width for itself, going up the tree.
    pub fn request_height(&mut self, height: usize) {
        if let Some(node) = &mut self.sibling {
            let mut label = self.label.write();
            let area = label.area_mut();
            if let Err(()) =
                request_height::<U>(area, node, height, area.height(), self.direction)
            {
                todo!();
            }
        }
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
    pub fn size_changed(&mut self) -> bool {
        let has_changed = *self.label.read().area() != self.old_area;
        self.old_area = *self.label.read().area();
        has_changed
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

    fn request_width(&mut self, width: usize) {
        match self {
            Node::MidNode(node) => {
                let mut node = node.write();
                node.request_width(width)
            }
            Node::EndNode(node) => {
                let mut node = node.write();
                node.request_width(width)
            }
        }
    }

    fn request_height(&mut self, height: usize) {
        match self {
            Node::MidNode(node) => {
                let mut node = node.write();
                node.request_height(height)
            }
            Node::EndNode(node) => {
                let mut node = node.write();
                node.request_height(height)
            }
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

/// The direction in which a secondary node was placed in relation to the first one.
#[derive(Clone, Copy)]
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
                old_area: *l.area(),
                label: RwData::new(l),
                class: RwData::new(String::from(class)),
                parent: None,
                sibling: None,
                direction: Direction::Top,
                config: RwData::new(config),
                palette: RwData::new(palette),
                applied_forms: Vec::new(),
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
            old_area: *label.area(),
            label: RwData::new(label),
            class: raw_node.class.clone(),
            parent: None,
            sibling: Some(Node::EndNode(cloned_node.clone())),
            direction,
            config: raw_node.config.clone(),
            palette: raw_node.palette.clone(),
            applied_forms: Vec::new(),
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
        let raw_node = node.write();
        log_info!("\nprev: {}", raw_node.container.read().area());
        drop(raw_node);
        let (container, label) =
            self.0.split_container(&mut node.write().container.write(), direction, split, glued);
        let raw_node = node.write();
        log_info!("\nprev: {}", raw_node.container.read().area());
        drop(raw_node);

        let cloned_node = node.clone();
        let mut raw_node = node.write();
        let mut end_node = RwData::new(EndNode {
            old_area: *label.area(),
            label: RwData::new(label),
            class: raw_node.class.clone(),
            parent: None,
            sibling: Some(Node::MidNode(cloned_node.clone())),
            direction,
            config: raw_node.config.clone(),
            palette: raw_node.palette.clone(),
            applied_forms: Vec::new(),
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
        });

        raw_node.parent = Some(mid_node.clone());
        raw_node.direction = direction.opposite();
        raw_node.sibling = Some(Node::EndNode(end_node.clone()));
        raw_node.resize_childrens_width().unwrap();

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

fn request_width<U>(
    first: &mut U::Area, second: &mut U::Area, width: usize, total: usize, direction: Direction,
) -> Result<(), ()>
where
    U: Ui,
{
    match direction {
        Direction::Left => first.request_width_left(width, total, second),
        Direction::Right => first.request_width_right(width, total, second),
        _ => Err(()),
    }
}

fn request_height<U>(
    area: &mut U::Area, node: &mut Node<U>, height: usize, total: usize, direction: Direction,
) -> Result<(), ()>
where
    U: Ui,
{
    match node {
        Node::MidNode(mid_node) => {
            let mut mid_node = mid_node.write();
            let mut container = mid_node.container.write();
            match direction {
                Direction::Top => area.request_height_top(height, total, container.area_mut()),
                Direction::Bottom => {
                    area.request_height_bottom(height, total, container.area_mut())
                }
                _ => Err(()),
            }
        }
        Node::EndNode(node) => {
            let mut node = node.write();
            let mut label = node.label.write();
            match direction {
                Direction::Top => area.request_height_top(height, total, label.area_mut()),
                Direction::Bottom => area.request_height_bottom(height, total, label.area_mut()),
                _ => Err(()),
            }
        }
    }
}
