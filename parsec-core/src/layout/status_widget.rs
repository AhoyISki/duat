use std::any::Any;

use crate::{
    config::{RoState, RwState},
    file::{Text, TextLine},
    ui::{EndNode, NodeManager, Ui},
};

use super::Widget;

pub trait StateToString {
    fn to_string(&self) -> String;
}

struct StringState<T, F>
where
    F: Fn(&T) -> String,
{
    state: RoState<T>,
    to_string: Box<F>,
}

impl<T, F> StringState<T, F>
where
    F: Fn(&T) -> String,
{
    /// Returns a new instance of `StringState`.
    fn new(state: RoState<T>, to_string: F) -> Self {
        StringState { state, to_string: Box::new(to_string) }
    }
}

impl<T, F> StateToString for StringState<T, F>
where
    F: Fn(&T) -> String,
{
    fn to_string(&self) -> String {
        (self.to_string)(&self.state.read())
    }
}

struct StringStateIndexed<T, F>
where
    F: Fn(&Vec<Box<T>>, usize) -> String,
{
    state: RoState<Vec<Box<T>>>,
    to_string: Box<F>,
    index: RoState<usize>,
}

impl<T, F> StringStateIndexed<T, F>
where
    F: Fn(&Vec<Box<T>>, usize) -> String,
{
    fn new(state: RoState<Vec<Box<T>>>, to_string: F, index: RoState<usize>) -> Self {
        Self { state, to_string: Box::new(to_string), index }
    }
}

impl<T, F> StateToString for StringStateIndexed<T, F>
where
    F: Fn(&Vec<Box<T>>, usize) -> String,
{
    fn to_string(&self) -> String {
        (self.to_string)(&self.state.read(), *self.index.read())
    }
}

pub struct StatusWidget<U>
where
    U: Ui,
{
    end_node: EndNode<U>,
    text: RwState<Text>,
    printables: Vec<Box<dyn StateToString>>,
    text_function: Box<dyn Fn(&Vec<Box<dyn StateToString>>) -> String>,
}

impl<U> StatusWidget<U>
where
    U: Ui,
{
    pub(super) fn new(end_node: EndNode<U>, _node_manager: &mut NodeManager<U>) -> Self {
        StatusWidget {
            printables: Vec::new(),
            end_node,
            text: RwState::new(Text::default()),
            text_function: Box::new(|_| String::from("")),
        }
    }

    pub fn push<T, F>(&mut self, state: &RoState<T>, f: F) -> usize
    where
        T: 'static,
        F: Fn(&T) -> String + 'static,
    {
        let new_state = StringState::new(state.clone(), f);
        self.printables.push(Box::new(new_state));
        self.printables.len() - 1
    }

    pub fn push_indexed<T, F>(
        &mut self, state: &RoState<Vec<Box<T>>>, f: F, index: &RoState<usize>,
    ) -> usize
    where
        T: 'static,
        F: Fn(&Vec<Box<T>>, usize) -> String + 'static,
    {
        let new_state = StringStateIndexed::new(state.clone(), f, index.clone());
        self.printables.push(Box::new(new_state));
        self.printables.len() - 1
    }

    pub fn set_function<F>(&mut self, f: F)
    where
        F: Fn(&Vec<Box<dyn StateToString>>) -> String + 'static,
    {
        self.text_function = Box::new(f);
    }
}

impl<U> Widget<U> for StatusWidget<U>
where
    U: Ui,
{
    fn end_node(&self) -> &EndNode<U> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut EndNode<U> {
        &mut self.end_node
    }

    fn needs_update(&self) -> bool {
        true
    }

    fn resize(&mut self, node: &EndNode<U>) {}

    fn text(&self) -> RoState<crate::file::Text> {
        (&self.text).into()
    }

    fn update(&mut self) {
        let mut text = self.text.write();
        text.lines.clear();
        text.lines.push(TextLine::new((self.text_function)(&self.printables)));
    }
}

unsafe impl<U> Send for StatusWidget<U> where U: Ui {}
