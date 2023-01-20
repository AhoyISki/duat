use crate::{
    config::{RoData, RwData},
    file::{Text, TextLine},
    ui::{EndNode, NodeManager, Ui},
};

use super::{file_widget::FileWidget, Widget};

pub trait DataToString {
    /// Converts the data to a `String`, usually through an embedded function.
    fn to_string(&self) -> String;

    /// Wether or not the data has changed since last read.
    fn has_changed(&self) -> bool;
}

struct DataString<T, F>
where
    F: Fn(&T) -> String,
{
    data: RoData<T>,
    to_string: Box<F>,
}

impl<T, F> DataString<T, F>
where
    F: Fn(&T) -> String,
{
    /// Returns a new instance of `StringState`.
    fn new(state: RoData<T>, to_string: F) -> Self {
        DataString { data: state, to_string: Box::new(to_string) }
    }
}

impl<T, F> DataToString for DataString<T, F>
where
    F: Fn(&T) -> String,
{
    fn to_string(&self) -> String {
        (self.to_string)(&self.data.read())
    }

    fn has_changed(&self) -> bool {
        self.data.has_changed()
    }
}

struct DataStringIndexed<T, F>
where
    F: Fn(&Vec<T>, usize) -> String,
{
    data: RoData<Vec<T>>,
    to_string: Box<F>,
    index: RoData<usize>,
}

impl<T, F> DataStringIndexed<T, F>
where
    F: Fn(&Vec<T>, usize) -> String,
{
    fn new(state: RoData<Vec<T>>, to_string: F, index: RoData<usize>) -> Self {
        Self { data: state, to_string: Box::new(to_string), index }
    }
}

impl<T, F> DataToString for DataStringIndexed<T, F>
where
    F: Fn(&Vec<T>, usize) -> String,
{
    fn to_string(&self) -> String {
        (self.to_string)(&self.data.read(), *self.index.read())
    }

    fn has_changed(&self) -> bool {
        self.data.has_changed()
    }
}

pub struct StatusWidget<U>
where
    U: Ui,
{
    end_node: RwData<EndNode<U>>,
    text: RwData<Text>,
    string: String,
    printables: Vec<Box<dyn DataToString>>,
    file: Option<RoData<FileWidget<U>>>,
    file_printables: Vec<Box<dyn Fn(&FileWidget<U>) -> String>>,
}

impl<U> StatusWidget<U>
where
    U: Ui,
{
    pub(super) fn new(end_node: RwData<EndNode<U>>, _node_manager: &mut NodeManager<U>) -> Self {
        StatusWidget {
            end_node,
            text: RwData::new(Text::default()),
            string: String::new(),
            printables: Vec::new(),
            file: None,
            file_printables: Vec::new(),
        }
    }

    pub fn push<T, F>(&mut self, state: &RoData<T>, f: F)
    where
        T: 'static,
        F: Fn(&T) -> String + 'static,
    {
        let new_state = DataString::new(state.clone(), f);
        self.printables.push(Box::new(new_state));
    }

    pub fn push_indexed<T, F>(&mut self, state: &RoData<Vec<Box<T>>>, f: F, index: &RoData<usize>)
    where
        T: 'static,
        F: Fn(&Vec<Box<T>>, usize) -> String + 'static,
    {
        let new_state = DataStringIndexed::new(state.clone(), f, index.clone());
        self.printables.push(Box::new(new_state));
    }

    pub fn push_file_var<F>(&mut self, function: F)
    where
        F: Fn(&FileWidget<U>) -> String + 'static,
    {
        self.file_printables.push(Box::new(function));
    }

    pub fn set_string<T>(&mut self, text: T)
    where
        T: ToString,
    {
        self.string = text.to_string();
    }

    pub fn set_file(&mut self, file: RoData<FileWidget<U>>) {
        self.file = Some(file);
    }
}

impl<U> Widget<U> for StatusWidget<U>
where
    U: Ui,
{
    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.end_node
    }

    fn needs_update(&self) -> bool {
        self.printables.iter().any(|p| p.has_changed())
    }

    fn resize(&mut self, node: &EndNode<U>) {}

    fn text(&self) -> RoData<crate::file::Text> {
        RoData::from(&self.text)
    }

    fn update(&mut self) {
        let mut text = self.text.write();
        text.lines.clear();
        let mut final_string = self.string.clone();
        let mut byte_diff = 0;

        for (index, (mut pos, _)) in self.string.match_indices("{}").enumerate() {
            if let Some(replacement) = &self.printables.get(index) {
                let replacement = &replacement.to_string();
                pos = pos.saturating_add_signed(byte_diff);
                byte_diff += replacement.len() as isize - 2 as isize;
                final_string.replace_range(pos..=(pos + 1), replacement)
            } else {
                panic!("There are not enough global_vars! One global_var per \"{{}}\"");
            }
        }
        if let Some(file) = &self.file {
            let file = file.read();
            for (index, (mut pos, _)) in self.string.match_indices("()").enumerate() {
                if let Some(replacement) = &self.file_printables.get(index) {
                    let replacement = &(replacement)(&file);
                    pos = pos.saturating_add_signed(byte_diff);
                    byte_diff += replacement.len() as isize - 2 as isize;
                    final_string.replace_range(pos..=(pos + 1), replacement)
                } else {
                    panic!("There are not enough file_vars! One file_var per \"()\"");
                }
            }
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
        |$obj| { $internals.read().to_string() }
    };
    (@get_fun $obj:ident) => {
        |$obj| { $obj.to_string() }
    };

    (@file_fun (|$obj:ident| $internals:expr)) => {
        |$obj| { $internals.to_string() }
    };

    (
        $status:expr => $text:expr; global_vars: $($to_string:tt),*;
        file_vars: $($file_to_string:tt),*
    ) => {
        $(
            $status.push_file_var(form_status!(@file_fun $file_to_string));
        );*
        $(
            $status.push(form_status!(@get_obj $to_string), form_status!(@get_fun $to_string));
        );*
        $status.set_string($text);
    };

    ($status:expr => $text:expr; file_vars: $($file_to_string:tt),*) => {
        $(
            $status.push_file_var(form_status!(@file_fun $file_to_string));
        );*
        $status.set_string($text);
    };

    ($status:expr => $text:expr; global_vars: $($to_string:tt),*) => {
        $(
            $status.push(form_status!(@get_obj $to_string), form_status!(@get_fun $to_string));
        );*
        $status.set_string($text);
    };
}

/// A convenience macro to join any number of variables that can be turned into `String`s.
#[macro_export]
macro_rules! join {
    ($($var:expr),*) => {
        [$($var.to_string()),*].join("")
    }
}
