use std::cmp::min;

use crate::{
    config::{RoData, RwData},
    text::{Text, TextLineBuilder},
    ui::{Area, EndNode, Label, NodeManager, Ui},
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

pub struct StatusLine<U>
where
    U: Ui,
{
    end_node: RwData<EndNode<U>>,
    text: RwData<Text>,
    left_text: String,
    center_text: String,
    right_text: String,
    text_line_builder: TextLineBuilder,
    printables: Vec<Box<dyn DataToString>>,
    file: Option<RoData<FileWidget<U>>>,
    file_printables: Vec<Box<dyn Fn(&FileWidget<U>) -> String>>,
}

impl<U> StatusLine<U>
where
    U: Ui,
{
    pub(super) fn new(end_node: RwData<EndNode<U>>, _node_manager: &mut NodeManager<U>) -> Self {
        StatusLine {
            end_node,
            text: RwData::new(Text::default()),
            left_text: String::new(),
            center_text: String::new(),
            right_text: String::new(),
            text_line_builder: TextLineBuilder::default(),
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

    pub fn set_left_text<T>(&mut self, text: T)
    where
        T: ToString,
    {
        let end_node = self.end_node.read();
        let palette = end_node.palette().read();
        let mut text = text.to_string();
        self.text_line_builder = TextLineBuilder::format_and_create(&mut text, &palette);
        self.left_text = text;
    }

    pub fn set_center_text<T>(&mut self, text: T)
    where
        T: ToString,
    {
        let end_node = self.end_node.read();
        let palette = end_node.palette().read();
        let mut text = text.to_string();
        self.text_line_builder.extend(&mut text, &palette);
        self.center_text = text;
    }

    pub fn set_right_text<T>(&mut self, text: T)
    where
        T: ToString,
    {
        let end_node = self.end_node.read();
        let palette = end_node.palette().read();
        let mut text = text.to_string();
        self.text_line_builder.extend(&mut text, &palette);
        self.right_text = text;
    }

    pub fn set_file(&mut self, file: RoData<FileWidget<U>>) {
        self.file = Some(file);
    }
}

impl<U> Widget<U> for StatusLine<U>
where
    U: Ui,
{
    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.end_node
    }

    fn update(&mut self) {
        let print_diff = &mut 0;
        let file_diff = &mut 0;

        let left = format_into_status(&self.left_text, &self, print_diff, file_diff);
        let center = format_into_status(&self.center_text, &self, print_diff, file_diff);
        let right = format_into_status(&self.right_text, &self, print_diff, file_diff);
        let width = self.end_node.read().label.read().area().width();
        let form_count = self.text_line_builder.form_count();
        let end_node = self.end_node().read();
        let label = end_node.label.read();

        let status = normalize_status::<U>(left, center, right, width, form_count, &label);
        drop(label);
        drop(end_node);

        let mut text = self.text.write();
        text.lines.clear();
        text.lines.push(self.text_line_builder.form_text_line(status));
    }

    fn needs_update(&self) -> bool {
        self.printables.iter().any(|p| p.has_changed())
    }

    fn text(&self) -> RoData<Text> {
        RoData::from(&self.text)
    }

    fn resize(&mut self, node: &EndNode<U>) {}
}

unsafe impl<U> Send for StatusLine<U> where U: Ui {}

fn format_into_status<U>(
    text: &String, status: &StatusLine<U>, print_diff: &mut usize, file_diff: &mut usize,
) -> String
where
    U: Ui,
{
    let mut final_text = text.clone();

    let (first_print_diff, first_file_diff) = (*print_diff, *file_diff);

    for (index, (mut pos, _)) in text.match_indices("{}").enumerate() {
        if let Some(replacement) = status.printables.get(index + first_print_diff) {
            let replacement = &replacement.to_string();
            let diff = final_text.len() as isize - text.len() as isize;
            pos = pos.saturating_add_signed(diff);
            final_text.replace_range(pos..=(pos + 1), replacement);

            *print_diff += 1;
        } else {
            panic!("There are not enough global_vars! One global_var per \"{{}}\"");
        }
    }

    if let Some(file) = &status.file {
        let file = file.read();
        for (index, (mut pos, _)) in text.match_indices("()").enumerate() {
            if let Some(replacement) = &status.file_printables.get(index + first_file_diff) {
                let replacement = &(replacement)(&file);
                let diff = final_text.len() as isize - text.len() as isize;
                pos = pos.saturating_add_signed(diff);
                final_text.replace_range(pos..=(pos + 1), replacement);

                *file_diff += 1;
            } else {
                panic!("There are not enough file_vars! One file_var per \"()\"");
            }
        }
    }

    final_text
}

// TODO: Unicodeify.
// TODO: Handle now atomic widths.
fn normalize_status<U>(
    left: String, center: String, right: String, width: usize, form_count: usize, label: &U::Label,
) -> String
where
    U: Ui,
{
    let left_len: usize = left.chars().map(|ch| label.get_char_len(ch)).sum();
    let center_len: usize = center.chars().map(|ch| label.get_char_len(ch)).sum();
    let right_len: usize = right.chars().map(|ch| label.get_char_len(ch)).sum();

    let left_forms: String = left.matches("[]").collect();
    let right_forms: String = right.matches("[]").collect();
    let center_forms: String = center.matches("[]").collect();

    let left_form_count = left_forms.len() / 2;
    let right_form_count = right_forms.len() / 2;

    let mod_width = width + 2 * form_count;

    let mut status = " ".repeat(mod_width);

    // Print left, right, and center.
    if left_len + center_len + right_len <= mod_width {
        let center_dist = (mod_width - center_len) / 2;
        let center_dist = if left_len + right_form_count - left_form_count > center_dist {
            left_len
        } else if right_len + left_form_count - right_form_count > center_dist {
            2 * center_dist - right_len
        } else {
            center_dist + left_form_count - right_form_count
        };

        status.replace_range((mod_width - right_len).., right.as_str());
        status.replace_range(center_dist..(center_dist + center_len), center.as_str());
        status.replace_range(0..left_len, left.as_str());

    // Print just the left and right parts.
    } else if left_len + right_len <= mod_width {
        // We need to print the center, even while not printing the central part, in order to sync
        // correctly with the `TextLineBuilder`.
        status.replace_range((mod_width - right_len).., right.as_str());
        status.replace_range(left_len..(left_len + center_forms.len()), center_forms.as_str());
        status.replace_range(0..left_len, left.as_str());

    // Print as much of the right part as possible, cutting off from the left.
    } else {
        let mut adder = 0;
        let (split_byte, _) = right
            .char_indices()
            .rev()
            .take_while(|&(_, ch)| {
                if ch != '[' && ch != ']' {
                    adder += label.get_char_len(ch);
                };
                adder <= width
            })
            .last()
            .unwrap();

        let cut_right_forms: String = right[..split_byte].matches("[]").collect();

        let printed_len: usize = right[split_byte..].chars().map(|ch| label.get_char_len(ch)).sum();

        let center_end = left_forms.len() + center_forms.len();
        let cut_right_end = center_end + cut_right_forms.len();

        status.replace_range(0..left_forms.len(), left_forms.as_str());
        status.replace_range(left_forms.len()..center_end, center_forms.as_str());
        status.replace_range(center_end..cut_right_end, cut_right_forms.as_str());
        status.replace_range((mod_width - printed_len).., &right[split_byte..]);
    }

    status
}

#[macro_export]
macro_rules! form_status {
    (@ignore $ignored:expr) => {};

    (@get_obj (|$obj:ident| $internals:expr)) => {
        &$obj
    };
    (@get_obj $obj:expr) => {
        &$obj
    };

    (@get_fun (|$obj:ident| $internals:expr)) => {
        |$obj| { $internals.read().to_string() }
    };
    (@get_fun $obj:expr) => {
        |data| { data.to_string() }
    };

    (@file_fun (|$obj:ident| $internals:expr)) => {
        |$obj| { $internals.to_string() }
    };

    (@tt_to_string: $text:expr) => {
        $text
    };

    (
        $status:expr => { left: $left:expr, center: $center:expr, right: $right:expr,
        file_vars: [$($file_to_string:tt),*], global_vars: [$($to_string:tt),*]}
    ) => {
        $(
            $status.push_file_var(form_status!(@file_fun $file_to_string));
        );*
        $(
            $status.push(form_status!(@get_obj $to_string), form_status!(@get_fun $to_string));
        );*

		$status.set_left_text($left);
		$status.set_center_text($center);
		$status.set_right_text($right);
    };
}

/// A convenience macro to join any number of variables that can be turned into `String`s.
#[macro_export]
macro_rules! join {
    ($($var:expr),*) => {
        [$($var.to_string()),*].join("")
    }
}
