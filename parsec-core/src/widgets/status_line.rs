use std::{any::Any, iter::repeat_with};

use super::{
    file_widget::{self, FileWidget},
    NormalWidget,
};
use crate::{
    config::{DownCastableData, RoData},
    tags::form::FormPalette,
    text::{Text, TextLineBuilder},
    ui::{Area, EndNode, Label, Ui},
};

pub trait DataToString {
    /// Converts the data to a `String`, usually through an embedded function.
    fn to_string(&mut self) -> String;

    /// Wether or not the data has changed since last read.
    fn has_changed(&self) -> bool;
}

struct DataString<T, F>
where
    F: Fn(&T) -> String,
    T: 'static,
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
    T: 'static,
{
    fn to_string(&mut self) -> String {
        (self.to_string)(&mut self.data.read())
    }

    fn has_changed(&self) -> bool {
        self.data.has_changed()
    }
}

struct DataStringIndexed<T, F>
where
    F: Fn(&Vec<T>, usize) -> String,
    T: 'static,
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
    T: 'static,
{
    fn to_string(&mut self) -> String {
        (self.to_string)(&mut self.data.read(), *self.index.read())
    }

    fn has_changed(&self) -> bool {
        self.data.has_changed()
    }
}

pub struct StatusLine<U>
where
    U: Ui,
{
    text: Text<U>,
    file_widget: RoData<FileWidget<U>>,
    format: StatusFormat<U>,
}

impl<U> StatusLine<U>
where
    U: Ui,
{
    pub fn new(file_widget: RoData<FileWidget<U>>, status_format: StatusFormat<U>) -> Self {
        StatusLine { text: Text::default(), file_widget, format: status_format }
    }

    pub fn set_file(&mut self, file_widget: RoData<FileWidget<U>>) {
        self.file_widget = file_widget;
    }
}

impl<U> DownCastableData for StatusLine<U>
where
    U: Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl<U> NormalWidget<U> for StatusLine<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        "parsec-status-line"
    }

    fn update(&mut self, end_node: &mut EndNode<U>) {
        if self.format.text_changed {
            self.format.text_changed = false;
            self.format.update_formater(&end_node.config().palette);
        }

        let file_widget = self.file_widget.read();
        let form_count = self.format.text_line_builder.form_count();

        let texts = substitute_printables(&mut self.format, &file_widget);
        let status = normalize_status::<U>(texts, form_count, end_node);

        self.text.clear_lines();
        self.text.push_line(self.format.text_line_builder.form_text_line(status));
    }

    fn needs_update(&self) -> bool {
        self.file_widget.has_changed() || self.format.has_changed()
    }

    fn text(&self) -> &Text<U> {
        &self.text
    }
}

unsafe impl<U> Send for StatusLine<U> where U: Ui {}

pub struct StatusFormat<U>
where
    U: Ui,
{
    text_line_builder: TextLineBuilder,
    left: String,
    center: String,
    right: String,
    global_printables: Vec<Box<dyn DataToString>>,
    file_printables: Vec<Box<dyn Fn(&FileWidget<U>) -> String>>,
    text_changed: bool,
}

impl<U> StatusFormat<U>
where
    U: Ui,
{
    /// Returns the default form of `StatusFormat<U>`.
    pub fn default(palette: &FormPalette) -> Self {
        let mut right_text = String::from("[FileName]() [Coords]() [Selections]() sel");
        StatusFormat {
            text_line_builder: TextLineBuilder::format_and_create(&mut right_text, palette),
            left: String::new(),
            center: String::new(),
            right: right_text,
            global_printables: Vec::new(),
            file_printables: Vec::new(),
            text_changed: true,
        }
    }

    pub fn push<T, F>(&mut self, state: &RoData<T>, f: F)
    where
        T: 'static,
        F: Fn(&T) -> String + 'static,
    {
        let new_state = DataString::new(state.clone(), f);
        self.global_printables.push(Box::new(new_state));
    }

    pub fn push_indexed<T, F>(&mut self, state: &RoData<Vec<Box<T>>>, f: F, index: &RoData<usize>)
    where
        T: 'static,
        F: Fn(&Vec<Box<T>>, usize) -> String + 'static,
    {
        let new_state = DataStringIndexed::new(state.clone(), f, index.clone());
        self.global_printables.push(Box::new(new_state));
    }

    pub fn push_file_var<F>(&mut self, function: F)
    where
        F: Fn(&FileWidget<U>) -> String + 'static,
    {
        self.file_printables.push(Box::new(function));
    }

    fn has_changed(&self) -> bool {
        self.global_printables.iter().any(|printable| printable.has_changed())
    }

    fn update_formater(&mut self, palette: &FormPalette) {
        self.text_line_builder = TextLineBuilder::format_and_create(&mut self.left, palette);
        self.text_line_builder.format_and_extend(&mut self.center, palette);
        self.text_line_builder.format_and_extend(&mut self.right, palette);
    }

    pub fn left_text_mut(&mut self) -> &mut String {
        &mut self.left
    }

    pub fn center_text_mut(&mut self) -> &mut String {
        &mut self.center
    }

    pub fn right_text_mut(&mut self) -> &mut String {
        &mut self.right
    }
}

fn substitute_printables<U>(
    format: &mut StatusFormat<U>, file_widget: &FileWidget<U>,
) -> [String; 3]
where
    U: Ui,
{
    let mut global_iter = format.global_printables.iter_mut();
    let mut file_iter = format.file_printables.iter_mut();

    let mut texts = [format.left.clone(), format.center.clone(), format.right.clone()];
    let mut texts_iter = texts.iter_mut().map(|text| (text.len(), text));

    let (mut orig_len, mut text) = texts_iter.next().unwrap();

    let substitutions = substitutions_iter(&format.left, &format.center, &format.right);

    for (mut pos, var, is_last_in_place) in substitutions {
        let replacement = if var == "{}" {
            let replacement = global_iter.next().expect("Not enough global substitutions!");
            replacement.to_string()
        } else {
            let replacement = file_iter.next().expect("Not enough file substitutions!");
            (replacement)(&file_widget)
        };

        pos = pos.saturating_add_signed(text.len() as isize - orig_len as isize);
        text.replace_range(pos..=(pos + 1), replacement.as_str());

        if is_last_in_place {
            let Some((next_orig_len, next_text)) = texts_iter.next() else {
                break;
            };
            (orig_len, text) = (next_orig_len, next_text);
        }
    }

    texts
}

// TODO: Evolve this into a system capable of handling non monospaced text.
fn normalize_status<U>(texts: [String; 3], form_count: usize, end_node: &EndNode<U>) -> String
where
    U: Ui,
{
    let width = end_node.label.area().width();

    let (config, label) = (end_node.config(), &end_node.label);
    let left_width: usize = label.get_width(texts[0].as_str(), &config.tab_places);
    let center_width: usize = label.get_width(texts[0].as_str(), &config.tab_places);
    let right_width: usize = label.get_width(texts[0].as_str(), &config.tab_places);

    let left_forms: String = texts[0].matches("[]").collect();
    let center_forms: String = texts[1].matches("[]").collect();
    let right_forms: String = texts[2].matches("[]").collect();

    let left_form_count = left_forms.len() / 2;
    let right_form_count = right_forms.len() / 2;

    let mod_width = width + 2 * form_count;

    let mut status = " ".repeat(mod_width);

    // Print left, right, and center.
    if left_width + center_width + right_width <= mod_width {
        let center_dist = (mod_width - center_width) / 2;
        let center_dist = if left_width + right_form_count - left_form_count > center_dist {
            left_width
        } else if right_width + left_form_count - right_form_count > center_dist {
            2 * center_dist - right_width
        } else {
            center_dist + left_form_count - right_form_count
        };

        status.replace_range((mod_width - right_width).., texts[2].as_str());
        status.replace_range(center_dist..(center_dist + center_width), texts[1].as_str());
        status.replace_range(0..left_width, texts[0].as_str());

    // Print just the left and right parts.
    } else if left_width + right_width <= mod_width {
        // We need to print the center, even while not printing the central part, in order to sync
        // correctly with the `TextLineBuilder`.
        status.replace_range((mod_width - right_width).., texts[2].as_str());
        status.replace_range(left_width..(left_width + center_forms.len()), center_forms.as_str());
        status.replace_range(0..left_width, texts[0].as_str());

    // Print as much of the right part as possible, cutting off from the left.
    } else {
        todo!();
        // let mut adder = 0;
        // let (split_byte, _) = right
        //     .char_indices()
        //     .rev()
        //     .take_while(|&(_, ch)| {
        //         if ch != '[' && ch != ']' {
        //             adder += label.get_char_len(ch);
        //         };
        //         adder <= width
        //     })
        //     .last()
        //     .unwrap();

        // let cut_right_forms: String = right[..split_byte].matches("[]").collect();

        // let printed_len: usize = right[split_byte..].chars().map(|ch|
        // label.get_char_len(ch)).sum();

        // let center_end = left_forms.len() + center_forms.len();
        // let cut_right_end = center_end + cut_right_forms.len();

        // status.replace_range(0..left_forms.len(), left_forms.as_str());
        // status.replace_range(left_forms.len()..center_end, center_forms.as_str());
        // status.replace_range(center_end..cut_right_end, cut_right_forms.as_str());
        // status.replace_range((mod_width - printed_len).., &right[split_byte..]);
    }

    status
}

/// Returns a sorted list of positions were "{}" or "()" have matched in a given [&str][str].
fn all_matches(text: &str) -> Vec<(usize, &str)> {
    let mut matches: Vec<(usize, &str)> = text.match_indices("{}").collect();
    matches.extend(text.match_indices("()"));
    matches.sort_by_key(|&(byte, _)| byte);
    matches
}

fn substitutions_iter<'a>(
    left: &'a str, center: &'a str, right: &'a str,
) -> impl Iterator<Item = (usize, &'a str, bool)> {
    let left_matches = all_matches(left);
    let center_matches = all_matches(center);
    let right_matches = all_matches(right);

    let (left_len, center_len, right_len) =
        (left_matches.len(), center_matches.len(), right_matches.len());

    let countdown_iter =
        move |len: &mut usize, (count, (byte, printable)): (usize, (usize, &'a str))| {
            let is_final = count == *len - 1;
            Some((byte, printable, is_final))
        };

    left_matches
        .into_iter()
        .enumerate()
        .scan(left_len, countdown_iter)
        .chain(center_matches.into_iter().enumerate().scan(center_len, countdown_iter))
        .chain(right_matches.into_iter().enumerate().scan(right_len, countdown_iter))
}

#[macro_export]
macro_rules! status_format {
    (@ignore $ignored:expr) => {};

    (@get_obj (|$obj:ident| $internals:expr)) => {
        &$obj
    };
    (@get_obj $obj:expr) => {
        &$obj
    };

    (@get_fun (|$obj:ident| $internals:expr)) => {
        |$obj| { $internals.to_string() }
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
        $palette:expr, left: $left:expr, center: $center:expr, right: $right:expr,
        file_vars: [$($file_to_string:tt),*], global_vars: [$($to_string:tt),*]
    ) => {
        {
            let mut format = StatusFormat::default($palette);
            $(
                format.push_file_var(status_format!(@file_fun $file_to_string));
            )*
            $(
                format.push(status_format!(@get_obj $to_string), status_format!(@get_fun $to_string));
            )*

    		*format.left_text_mut() = $left.to_string();
    		*format.center_text_mut() = $center.to_string();
    		*format.right_text_mut() = $right.to_string();

    		format
        }
    };
}

/// A convenience macro to join any number of variables that can be turned into `String`s.
///
/// # Examples
///
/// ```
/// use parsec_core::widgets::status_line::join;
///
/// let my_text = join!["number: ", 235, String::from(", floating: "), 3.14f32];
/// assert!(my_text == String::from("number: 235, floating: 3.14"));
/// ```
#[macro_export]
macro_rules! join {
    ($($var:expr),*) => {
        [$($var.to_string()),*].join("")
    }
}
