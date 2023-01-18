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
    string: String,
    printables: Vec<Box<dyn StateToString>>,
}

impl<U> StatusWidget<U>
where
    U: Ui,
{
    pub(super) fn new(end_node: EndNode<U>, _node_manager: &mut NodeManager<U>) -> Self {
        StatusWidget {
            end_node,
            text: RwState::new(Text::default()),
            string: String::new(),
            printables: Vec::new(),
        }
    }

    pub fn push<T, F>(&mut self, state: &RoState<T>, f: F)
    where
        T: 'static,
        F: Fn(&T) -> String + 'static,
    {
        let new_state = StringState::new(state.clone(), f);
        self.printables.push(Box::new(new_state));
    }

    pub fn push_indexed<T, F>(&mut self, state: &RoState<Vec<Box<T>>>, f: F, index: &RoState<usize>)
    where
        T: 'static,
        F: Fn(&Vec<Box<T>>, usize) -> String + 'static,
    {
        let new_state = StringStateIndexed::new(state.clone(), f, index.clone());
        self.printables.push(Box::new(new_state));
    }

    pub fn set_string<T>(&mut self, text: T)
    where
        T: ToString,
    {
        self.string = text.to_string();
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
        let mut final_string = self.string.clone();

        for (index, (pos, _)) in self.string.match_indices("{}").enumerate() {
            final_string.replace_range(pos..=(pos + 1), &self.printables[index].to_string())
        }

        text.lines.push(TextLine::new(final_string));
    }
}

unsafe impl<U> Send for StatusWidget<U> where U: Ui {}

#[macro_export]
macro_rules! form_status {
    (@ignore $ignored:expr) => {};

    (@get_obj (|$obj:ident| $internals:expr)) => {
        &$obj
    };
    (@get_obj $obj:ident) => {
        &$obj
    };

    (@get_fun (|$obj:ident| $internals:expr)) => {
        |$obj| { $internals.to_string() }
    };
    (@get_fun $obj:ident) => {
        |$obj| { $obj.to_string() }
    };

    ($status:expr => $text:expr, $($to_string:tt),*) => {
        $(
            $status.push(form_status!(@get_obj $to_string), form_status!(@get_fun $to_string));
        ),*
        let mut index = 0;
        $status.set_string($text);
    };

}

#[macro_export]
macro_rules! tester {
    (@closure |$ident:ident| $expr:expr) => {
        stringify!(|$ident| $expr)
    };
    (@closure $ident:ident) => {
        panic!(stringify!(tester!(@closure $expr)));
    };
    ($expr:tt) => {
        panic!(stringify!(tester!(@closure $expr)));
    }
}
