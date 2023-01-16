use std::sync::{Arc, RwLock, RwLockWriteGuard};

use crossterm::style::{Attribute, Attributes, Color, ContentStyle};

use crate::{config::Config, tags::Form, log_info};

/// A `Label` or `Container` container, that holds exactly two in total.
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

/// A label that prints text to screen. Any area that prints will be a `Label` in the `Ui`.
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

    /// Moves to the next line. If succesful, returns `Ok(())`, otherwise, returns `Err(())`.
    ///
    /// This function should also make sure that there is no leftover text after the current line's
    /// end.
    fn next_line(&mut self) -> Result<(), ()>;

    /// Wraps to the next line. If succesful, returns `Ok(())`, otherwise, returns `Err(())`.
    ///
    /// Unlike `next_line()`, this function should not remove any text.
    fn wrap_line(&mut self) -> Result<(), ()>;

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

/// Exists only to keep these things inside an `Arc`.
pub struct InnerMidNode<U>
where
    U: Ui + ?Sized,
{
    container: U::Container,
    class: String,
    parent: Option<Box<MidNode<U>>>,
    child_order: ChildOrder,
    children: (Node<U>, Node<U>),
    split: Split,
}

/// A node that contains other nodes.
pub struct MidNode<U>
where
    U: Ui + ?Sized,
{
    inner: Arc<RwLock<InnerMidNode<U>>>,
    config: Config,
}

impl<U> Clone for MidNode<U>
where
    U: Ui + ?Sized,
{
    fn clone(&self) -> Self {
        MidNode { inner: self.inner.clone(), config: self.config.clone() }
    }
}

impl<U> MidNode<U>
where
    U: Ui,
{
    /// Resizes the second child node, in the direction that it was placed.
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

	/// Requests a new width for itself, going up the tree.
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

/// Exists only to keep these things inside an `Arc`.
pub struct InnerEndNode<U>
where
    U: Ui + ?Sized,
{
    label: U::Label,
    class: String,
    parent: Option<Box<MidNode<U>>>,
    child_order: ChildOrder,
    form_stack: Vec<(Form, u16)>,
}

/// Node that contains a `Label`.
pub struct EndNode<U>
where
    U: Ui + ?Sized,
{
    inner: Arc<RwLock<InnerEndNode<U>>>,
    config: Config,
}

impl<U> EndNode<U>
where
    U: Ui,
{
    /// Tries to request a new width for the label.
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

	/// Gets the visual height of the `Label`, in arbitrary units (cells, pixels, etc). 
    pub fn height(&self) -> usize {
        let node = self.inner.read().unwrap();
        node.label.height()
    }

	/// Gets the visual width of the `Label`, in arbitrary units (cells, pixels, etc). 
    pub fn width(&self) -> usize {
        let node = self.inner.read().unwrap();
        node.label.width()
    }

	/// Gets the visual lenght of a character, in arbitrary units (cells, pixels, etc).
    pub fn get_char_len(&self, ch: char) -> usize {
        self.inner.read().unwrap().label.get_char_len(ch)
    }

	/// Triggers the preliminary functions that need to take place before printing.
    pub(crate) fn start_printing(&mut self) {
        let mut inner = self.inner.write().unwrap();
        inner.label.start_printing();
        drop(inner);
    }

	/// Returns a `RawEndNode`, a more versatile object for interaction with `InnerEndNode`.
    pub(crate) fn raw(&self) -> RawEndNode<U> {
        RawEndNode { config: &self.config, inner: self.inner.write().unwrap() }
    }

	/// Triggers the finishing functions that need to take place after printing.
    pub(crate) fn stop_printing(&mut self) {
        let mut inner = self.inner.write().unwrap();
        inner.label.stop_printing();
        drop(inner);
    }

	/// Returns a reference to the `Config` of the node.
    pub fn config(&self) -> &Config {
        &self.config
    }
}

impl<U> Clone for EndNode<U>
where
    U: Ui + ?Sized,
{
    fn clone(&self) -> Self {
        EndNode { inner: self.inner.clone(), config: self.config.clone() }
    }
}


/// Convenient reference for an `EndNode`, used internally.
pub(crate) struct RawEndNode<'a, U>
where
    U: Ui,
{
    pub(crate) config: &'a Config,
    inner: RwLockWriteGuard<'a, InnerEndNode<U>>,
}

impl<'a, U> RawEndNode<'a, U>
where
    U: Ui,
{
    /// Generates the form to be printed, given all the previously pushed forms in the `Form` stack.
    pub fn make_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::default(),
        };

        let mut form = Form { style, is_final: false };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final, .. }, _) in &self.inner.form_stack {
            let new_foreground = style.foreground_color;
            set_var(&mut fg_done, &mut form.style.foreground_color, &new_foreground, is_final);

            let new_background = style.background_color;
            set_var(&mut bg_done, &mut form.style.background_color, &new_background, is_final);

            let new_underline = style.underline_color;
            set_var(&mut ul_done, &mut form.style.underline_color, &new_underline, is_final);

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

	/// Adds another `Form` to the stack.
    pub fn push_form(&mut self, form: Form, id: u16) {
        self.inner.form_stack.push((form, id));

        let form = self.make_form();

        self.inner.label.set_form(form);
    }

	/// Removes a `Form` from the stack with the given id.
    pub fn remove_form(&mut self, id: u16) {
        if let Some((index, _)) =
            self.inner.form_stack.iter().enumerate().rfind(|(_, &(_, i))| i == id)
        {
            self.inner.form_stack.remove(index);

            let form = self.make_form();

            self.inner.label.set_form(form);
        }
    }

	/// Completely clears the stack of `Form`s.
    pub fn clear_form(&mut self) {
        self.inner.form_stack.clear();
    }

	/// Prints a character to the screen.
    pub fn print(&mut self, ch: char) {
        self.inner.label.print(ch);
    }

	/// Places the cursor at the beginning of the next line.
    pub(crate) fn next_line(&mut self) -> Result<(), ()> {
        self.inner.label.next_line()
    }

	/// Wraps to the next line, but keeps printing the same line.
    pub(crate) fn wrap_line(&mut self) -> Result<(), ()> {
        self.inner.label.wrap_line()
    }

	/// Places the primary cursor/caret on screen.
    pub(crate) fn place_primary_cursor(&mut self) {
        self.inner.label.place_primary_cursor();
    }

	// On terminals, this is not be possible, that's why I separated these 2 functions.
	/// Places secondary cursors/carets on screen.
    pub(crate) fn place_secondary_cursor(&mut self) {
        self.inner.label.place_secondary_cursor();
    }

	/// Gets the visual height of the `Label`, in arbitrary units (cells, pixels, etc). 
    pub fn height(&self) -> usize {
        self.inner.label.height()
    }

	/// Gets the visual width of the `Label`, in arbitrary units (cells, pixels, etc). 
    pub fn width(&self) -> usize {
        self.inner.label.width()
    }

	/// Gets the visual lenght of a character, in arbitrary units (cells, pixels, etc).
    pub fn get_char_len(&self, ch: char) -> usize {
        self.inner.label.get_char_len(ch)
    }
}

/// Container for middle and end nodes.
pub enum Node<U>
where
    U: Ui + ?Sized,
{
    MidNode(MidNode<U>),
    EndNode(EndNode<U>),
}

/// The order in which a specific node was placed (chronological, not spatial).
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

/// The direction in which a secondary node was placed in relation to the first one.
#[derive(Clone, Copy)]
pub enum Direction {
    Top,
    Right,
    Bottom,
    Left,
}

/// All the methods that a working gui/tui will need to implement, in order to use Parsec.
pub trait Ui {
    type Container: Container + Clone + Send;
    type Label: Label + Clone + Send;

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
    pub fn only_child(&mut self, config: &Config, class: &str) -> Option<EndNode<U>> {
        self.0.only_label().map(|l| EndNode {
            inner: Arc::new(RwLock::new(InnerEndNode {
                child_order: ChildOrder::First,
                parent: None,
                form_stack: Vec::new(),
                class: String::from(class),
                label: l,
            })),
            config: config.clone(),
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

/// Internal method used only to shorten code in `make_form()`.
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
