use std::sync::{Arc, RwLock};

use crossterm::style::{Attribute, Attributes, Color, ContentStyle};

use crate::{config::Config, tags::Form};

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
    M: Ui + ?Sized,
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
    M: Ui + ?Sized,
{
    inner: Arc<RwLock<InnerMidNode<M>>>,
    config: Config,
}

impl<M> Clone for MidNode<M>
where
    M: Ui + ?Sized,
{
    fn clone(&self) -> Self {
        MidNode { inner: self.inner.clone(), config: self.config.clone() }
    }
}

impl<M> MidNode<M>
where
    M: Ui,
{
    fn resize_second(&mut self, len: usize) {
        let node = self.inner.write().unwrap();

        match node.split {
            Split::Locked(_) => panic!("Don't try to resize a locked length area!"),
            Split::Static(_) => {
                let self_len = match node.container.direction() {
                    Direction::Left | Direction::Right => {
                        self.inner.read().unwrap().container.width()
                    }
                    _ => self.inner.read().unwrap().container.height(),
                };

                if len < self_len {
                    match &node.children.1 {
                        Node::MidNode(node) => {
                            node.inner.write().unwrap().container.request_len(len)
                        }
                        Node::EndNode(node) => node.inner.write().unwrap().label.request_len(len),
                    }
                }
            }
            Split::Ratio(_) => todo!(),
        }
    }

    fn request_width(&self, width: usize) {
        let mut node = self.inner.write().unwrap();
        match (node.child_order, &mut node.parent) {
            (ChildOrder::Second, Some(parent_node)) => {
                let inner_node = parent_node.inner.read().unwrap();
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
    M: Ui + ?Sized,
{
    label: M::Label,
    class: String,
    parent: Option<Box<MidNode<M>>>,
    child_order: ChildOrder,
    form_stack: Vec<(Form, u16)>,
}

pub struct EndNode<M>
where
    M: Ui + ?Sized,
{
    inner: Arc<RwLock<InnerEndNode<M>>>,
    config: Config,
}

impl<M> Clone for EndNode<M>
where
    M: Ui + ?Sized,
{
    fn clone(&self) -> Self {
        EndNode { inner: self.inner.clone(), config: self.config.clone() }
    }
}

impl<M> EndNode<M>
where
    M: Ui,
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

        for &(Form { style, is_final, .. }, _) in &self.inner.read().unwrap().form_stack {
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
        let mut node = self.inner.write().unwrap();
        node.form_stack.push((forms[id as usize], id));

        let form = self.make_form();

        node.label.set_form(form);
    }

    pub fn pop_form(&mut self, id: u16) {
        let mut node = self.inner.write().unwrap();
        if let Some((index, _)) = node.form_stack.iter().enumerate().rfind(|(_, &(_, i))| i == id) {
            node.form_stack.remove(index);

            let form = self.make_form();

            node.label.set_form(form);
        }
    }

    fn request_width(&self, width: usize) {
        let mut node = self.inner.write().unwrap();
        match (node.child_order, &mut node.parent) {
            (ChildOrder::Second, Some(parent_node)) => {
                let inner_node = parent_node.inner.read().unwrap();
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
        let mut node = self.inner.write().unwrap();
        node.form_stack.clear();
    }

    pub fn height(&self) -> usize {
        let node = self.inner.read().unwrap();
        node.label.height()
    }

    pub fn width(&self) -> usize {
        let node = self.inner.read().unwrap();
        node.label.width()
    }

    pub(crate) fn start_printing(&mut self) {
        self.inner.write().unwrap().label.start_printing();
    }

    pub(crate) fn stop_printing(&mut self) {
        self.inner.write().unwrap().label.stop_printing();
    }

	// TODO: Stop using `write()` a bazillion times over.
    pub(crate) fn print(&mut self, ch: char) {
        self.inner.write().unwrap().label.print(ch);
    }

    pub(crate) fn next_line(&mut self) -> bool {
        self.inner.write().unwrap().label.next_line()
    }

    pub(crate) fn place_primary_cursor(&mut self) {
        self.inner.write().unwrap().label.place_primary_cursor();
    }

    pub(crate) fn place_secondary_cursor(&mut self) {
        self.inner.write().unwrap().label.place_secondary_cursor();
    }

    pub fn get_char_len(&self, ch: char) -> usize {
        self.inner.read().unwrap().label.get_char_len(ch)
    }

    pub fn config(&self) -> &Config {
        &self.config
    }
}

pub enum Node<M>
where
    M: Ui + ?Sized,
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

pub trait Ui {
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
    fn split_label(
        &mut self, label: &mut Self::Label, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label);

    fn split_container(
        &mut self, container: &mut Self::Container, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label);

    fn only_label(&mut self) -> Option<Self::Label>;

    fn startup(&mut self);

    fn shutdown(&mut self);
}

pub struct NodeManager<U>(U)
where
    U: Ui;

impl<U> NodeManager<U>
where
    U: Ui,
{
    pub fn new(ui_manager: U) -> Self {
        NodeManager(ui_manager)
    }

    pub fn only_child(
        &mut self, config: Option<Config>, class: Option<String>,
    ) -> Option<EndNode<U>> {
        self.0.only_label().map(|l| EndNode {
            inner: Arc::new(RwLock::new(InnerEndNode {
                child_order: ChildOrder::First,
                parent: None,
                form_stack: Vec::new(),
                class: class.unwrap_or(String::from("label")),
                label: l,
            })),
            config: config.unwrap_or(Config::default()),
        })
    }

    // TODO: Move this to an owning struct.
    /// Splits a given `EndNode` into two, with a new parent `MidNode`, and a new child `EndNode`.
    pub fn split_end(
        &mut self, node: &mut EndNode<U>, direction: Direction, split: Split, glued: bool,
    ) -> (MidNode<U>, EndNode<U>) {
        let mut inner = node.inner.write().unwrap();
        let (container, label) = self.0.split_label(&mut inner.label, direction, split, glued);
        drop(inner);

        let inner = node.inner.read().unwrap();

        let end_node = EndNode {
            inner: Arc::new(RwLock::new(InnerEndNode {
                child_order: ChildOrder::Second,
                parent: None,
                form_stack: Vec::new(),
                class: inner.class.clone(),
                label,
            })),
            config: node.config.clone(),
        };

        let mid_node = MidNode {
            inner: Arc::new(RwLock::new(InnerMidNode {
                child_order: inner.child_order,
                children: (Node::EndNode(node.clone()), Node::EndNode(end_node.clone())),
                split,
                parent: inner.parent.clone(),
                class: inner.class.clone(),
                container,
            })),
            config: node.config.clone(),
        };
        drop(inner);

        node.inner.write().unwrap().parent = Some(Box::new(mid_node.clone()));
        end_node.inner.write().unwrap().parent = Some(Box::new(mid_node.clone()));

        (mid_node, end_node)
    }

    // TODO: Fix split, if necessary.
    /// Splits a given `MidNode` into two, with a new parent `MidNode`, and a new child `EndNode`.
    pub fn split_mid(
        &mut self, node: &mut MidNode<U>, direction: Direction, split: Split, glued: bool,
    ) -> (MidNode<U>, EndNode<U>) {
        let (container, label) = self.0.split_container(
            &mut node.inner.write().unwrap().container,
            direction,
            split,
            glued,
        );

        let inner_node = node.inner.read().unwrap();

        let end_node = EndNode {
            inner: Arc::new(RwLock::new(InnerEndNode {
                child_order: ChildOrder::Second,
                parent: None,
                form_stack: Vec::new(),
                class: inner_node.class.clone(),
                label,
            })),
            config: node.config.clone(),
        };

        let mid_node = MidNode {
            inner: Arc::new(RwLock::new(InnerMidNode {
                child_order: inner_node.child_order,
                children: (Node::MidNode(node.clone()), Node::EndNode(end_node.clone())),
                split,
                parent: inner_node.parent.clone(),
                class: inner_node.class.clone(),
                container,
            })),
            config: node.config.clone(),
        };

        node.inner.write().unwrap().parent = Some(Box::new(mid_node.clone()));
        end_node.inner.write().unwrap().parent = Some(Box::new(mid_node.clone()));

        (mid_node, end_node)
    }

    pub(crate) fn startup(&mut self) {
        self.0.startup();
    }

    fn shutdown(&mut self) {
        self.0.shutdown();
    }
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
