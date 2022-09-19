use std::sync::{Arc, RwLock};

use crossterm::style::{Attribute, Attributes, Color, ContentStyle};

use crate::{config::ConfigOptions, tags::Form};

pub trait Container {
    // TODO: Return a result.
    /// Requests a resize to the area, based on the direction of the parent.
    fn request_len(&mut self, width: usize);

    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;

    /// The direction where the second area was placed on the first.
    fn direction(&self) -> Direction;
}

pub trait Label {
    //////////////////// Forms
    /// Changes the form for subsequent characters.
    fn set_form(&mut self, form: Form);

    // TODO: Give it a default form.
    /// Clears the current form.
    fn clear_form(&mut self);

    // TODO: Give it a default form.
    /// Places the primary cursor on the current printing position.
    fn place_primary_cursor(&mut self);

    // TODO: Give it a default form.
    /// Places the secondary cursor on the current printing position.
    fn place_secondary_cursor(&mut self);

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

    /// Moves to the next line. If that's not possible, returns false.
    ///
    /// This function should also make sure that there is no leftover text after the current line's
    /// end.
    fn next_line(&mut self) -> bool;

    // TODO: Return a result.
    /// Requests a resize to the area, based on the direction of the parent.
    fn request_len(&mut self, width: usize);

    //////////////////// Getters
    /// Gets the length of a character.
    ///
    /// In a terminal, this would be in "cells", but in a variable width GUI, it could be in
    /// pixels, or em. It really depends on the implementation.
    fn get_char_len(&self, ch: char) -> usize;

    /// Gets the width of the area.
    fn width(&self) -> usize;

    /// Gets the height of the area.
    fn height(&self) -> usize;
}

pub struct InnerMidNode<M>
where
    M: NodeManager + ?Sized,
{
    container: M::Container,
    class: String,
    parent: Option<Box<MidNode<M>>>,
    child_order: ChildOrder,
    children: (Node<M>, Node<M>),
    split: Split,
}

pub struct MidNode<M>
where
    M: NodeManager + ?Sized,
{
    node: Arc<RwLock<InnerMidNode<M>>>,
    config: ConfigOptions,
}

impl<M> Clone for MidNode<M>
where
    M: NodeManager + ?Sized,
{
    fn clone(&self) -> Self {
        MidNode { node: self.node.clone(), config: self.config.clone() }
    }
}

impl<M> MidNode<M>
where
    M: NodeManager,
{
    fn resize_second(&mut self, len: usize) {
        let node = self.node.write().unwrap();

        match node.split {
            Split::Locked(_) => panic!("Don't try to resize a locked length area!"),
            Split::Static(_) => {
                let self_len = match node.container.direction() {
                    Direction::Left | Direction::Right => {
                        self.node.read().unwrap().container.width()
                    }
                    _ => self.node.read().unwrap().container.height(),
                };

                if len < self_len {
                    match &node.children.1 {
                        Node::MidNode(node) => {
                            node.node.write().unwrap().container.request_len(len)
                        }
                        Node::EndNode(node) => node.node.write().unwrap().label.request_len(len),
                    }
                }
            }
            Split::Ratio(_) => todo!(),
        }
    }

    fn request_width(&self, width: usize) {
        let mut node = self.node.write().unwrap();
        match (node.child_order, &mut node.parent) {
            (ChildOrder::Second, Some(parent_node)) => {
                let inner_node = parent_node.node.read().unwrap();
                if let Direction::Left | Direction::Right = inner_node.container.direction() {
                    drop(inner_node);
                    parent_node.resize_second(width);
                }
            }
            // NOTE: I don't really know if I'm gonna do anything in here tbh.
            _ => todo!(),
        }
    }
}

pub struct InnerEndNode<M>
where
    M: NodeManager + ?Sized,
{
    label: M::Label,
    class: String,
    parent: Option<Box<MidNode<M>>>,
    child_order: ChildOrder,
    form_stack: Vec<(Form, u16)>,
}

pub struct EndNode<M>
where
    M: NodeManager + ?Sized,
{
    node: Arc<RwLock<InnerEndNode<M>>>,
    config: ConfigOptions,
}

impl<M> Clone for EndNode<M>
where
    M: NodeManager + ?Sized,
{
    fn clone(&self) -> Self {
        EndNode { node: self.node.clone(), config: self.config.clone() }
    }
}

impl<M> EndNode<M>
where
    M: NodeManager,
{
    pub fn make_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::from(Attribute::Reset),
        };

        let mut form = Form { style, is_final: false };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final, .. }, _) in &self.node.read().unwrap().form_stack {
            let new_foreground = style.foreground_color;
            set_var(&mut fg_done, &mut form.style.foreground_color, &new_foreground, is_final);

            let new_background = style.background_color;
            set_var(&mut fg_done, &mut form.style.background_color, &new_background, is_final);

            let new_underline = style.underline_color;
            set_var(&mut fg_done, &mut form.style.underline_color, &new_underline, is_final);

            if !attr_done {
                form.style.attributes.extend(style.attributes);
                if is_final {
                    attr_done = true
                }
            }

            if fg_done && bg_done && ul_done && attr_done {
                break;
            }
        }

        form
    }

    pub fn push_form(&mut self, forms: &[Form], id: u16) {
        let mut node = self.node.write().unwrap();
        node.form_stack.push((forms[id as usize], id));

        let form = self.make_form();

        node.label.set_form(form);
    }

    pub fn pop_form(&mut self, id: u16) {
        let mut node = self.node.write().unwrap();
        if let Some((index, _)) = node.form_stack.iter().enumerate().rfind(|(_, &(_, i))| i == id) {
            node.form_stack.remove(index);

            let form = self.make_form();

            node.label.set_form(form);
        }
    }

    fn request_width(&self, width: usize) {
        let mut node = self.node.write().unwrap();
        match (node.child_order, &mut node.parent) {
            (ChildOrder::Second, Some(parent_node)) => {
                let inner_node = parent_node.node.read().unwrap();
                if let Direction::Left | Direction::Right = inner_node.container.direction() {
                    drop(inner_node);
                    parent_node.resize_second(width);
                }
            }
            // NOTE: I don't really know if I'm gonna do anything in here tbh.
            _ => todo!(),
        }
    }

    pub fn clear_form(&mut self) {
        let mut node = self.node.write().unwrap();
        node.form_stack.clear();
    }

    pub fn height(&self) -> usize {
        let node = self.node.read().unwrap();
        node.label.height()
    }

    pub fn width(&self) -> usize {
        let node = self.node.read().unwrap();
        node.label.width()
    }

    pub fn options(&self) -> &ConfigOptions {
        &self.options()
    }

    pub(crate) fn start_printing(&mut self) {
        self.node.write().unwrap().label.start_printing();
    }

    pub(crate) fn stop_printing(&mut self) {
        self.node.write().unwrap().label.stop_printing();
    }

    pub(crate) fn print(&mut self, ch: char) {
        self.node.write().unwrap().label.print(ch);
    }

    pub(crate) fn next_line(&mut self) -> bool {
        self.node.write().unwrap().label.next_line()
    }

    pub(crate) fn place_primary_cursor(&mut self) {
        self.node.write().unwrap().label.place_primary_cursor();
    }

    pub(crate) fn place_secondary_cursor(&mut self) {
        self.node.write().unwrap().label.place_secondary_cursor();
    }
}

pub enum Node<M>
where
    M: NodeManager + ?Sized,
{
    MidNode(MidNode<M>),
    EndNode(EndNode<M>),
}

#[derive(Clone, Copy)]
enum ChildOrder {
    First,
    Second,
}

/// A way of splitting areas.
#[derive(Clone, Copy)]
pub enum Split {
    Locked(usize),
    Static(usize),
    Ratio(f32),
}

#[derive(Clone, Copy)]
pub enum Direction {
    Top,
    Right,
    Bottom,
    Left,
}

pub trait NodeManager {
    type Container: Container + Clone;
    type Label: Label + Clone;

    /// Splits an area in two, and places each of the areas on a new parent area.
    ///
    /// # Returns
    ///
    /// * The new parent area first, and the new child area second.
    ///
    /// # Arguments
    ///
    /// * area: The area that will be split.
    /// * direction: In what direction, relative to the old area, will the new area be inserted.
    /// * split: How to decide where to place the barrier between the two areas. If
    ///   `Split::Static`, the new area will have a fixed size, and resizing the parent area will
    ///   only change the size of the old area. If `Split::Ratio`, resizing the parent will
    ///   maintain a ratio between the two areas.
    /// * glued: If `true`, the two areas will become inseparable, by moving one, you will move the
    ///   other one with it.
    fn split_end(
        &mut self, node: &mut EndNode<Self>, direction: Direction, split: Split, glued: bool,
    ) -> (MidNode<Self>, EndNode<Self>);

    fn split_parent(
        &mut self, area: &mut MidNode<Self>, direction: Direction, split: Split, glued: bool,
    ) -> (MidNode<Self>, EndNode<Self>);

    /// If there is only a single area, returns it, otherwise, returns nothing.
    fn only_child(&self) -> Option<EndNode<Self>>;
}

fn set_var<T>(is_set: &mut bool, var: &mut Option<T>, maybe_new: &Option<T>, is_final: bool)
where
    T: Clone,
{
    if let (Some(new_var), false) = (maybe_new, &is_set) {
        *var = Some(new_var.clone());
        if is_final {
            *is_set = true
        };
    }
}
