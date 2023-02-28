use super::{file_widget::FileWidget, NormalWidget};
use crate::{
    config::RoData,
    tags::form::FormPalette,
    text::{Text, TextLineBuilder},
    ui::{Area, EndNode, Label, Ui},
};

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
    text: Text<U>,
    file_widget: RoData<FileWidget<U>>,
    format: StatusFormat<U>,
}

impl<U> StatusLine<U>
where
    U: Ui,
{
    pub fn new(file_widget: RoData<FileWidget<U>>, status_format: StatusFormat<U>) -> Self {
        StatusLine {
            text: Text::default(),
            file_widget,
            format: status_format,
        }
    }

    pub fn set_file(&mut self, file_widget: RoData<FileWidget<U>>) {
        self.file_widget = file_widget;
    }
}

impl<U> NormalWidget<U> for StatusLine<U>
where
    U: Ui,
{
    fn identifier(&self) -> &str {
        "parsec-status-line"
    }

    fn update(&mut self, end_node: &mut EndNode<U>) {
        let print_diff = &mut 0;
        let file_diff = &mut 0;

        let left = sub_printables(&self.format.left_text, &self, print_diff, file_diff);
        let center = sub_printables(&self.format.center_text, &self, print_diff, file_diff);
        let right = sub_printables(&self.format.right_text, &self, print_diff, file_diff);
        let form_count = self.format.text_line_builder.form_count();

        let status = normalize_status::<U>(left, center, right, form_count, end_node);

        self.text.lines.clear();
        self.text.lines.push(self.format.text_line_builder.form_text_line(status));
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
    left_text: String,
    center_text: String,
    right_text: String,
    global_printables: Vec<Box<dyn DataToString>>,
    file_printables: Vec<Box<dyn Fn(&FileWidget<U>) -> String>>,
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
            left_text: String::new(),
            center_text: String::new(),
            right_text,
            global_printables: Vec::new(),
            file_printables: Vec::new(),
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

    fn update_formater(&mut self, end_node: &EndNode<U>) {
        let palette = &end_node.config().palette;
        self.text_line_builder = TextLineBuilder::format_and_create(&mut self.left_text, palette);
        self.text_line_builder.format_and_extend(&mut self.center_text, palette);
        self.text_line_builder.format_and_extend(&mut self.right_text, palette);
    }
}

fn sub_printables<U>(
    text: &String, status: &StatusLine<U>, global_index: &mut usize, file_index: &mut usize,
) -> String
where
    U: Ui,
{
    let mut final_text = text.clone();

    let mut vars: Vec<(usize, &str)> = text.match_indices("{}").collect();
    vars.extend(text.match_indices("()"));
    vars.sort_by_key(|&(pos, _)| pos);

    let file_widget = status.file_widget.read();
    for (mut pos, var) in vars {
        let replacement = if var == "{}" {
            if let Some(replacement) = status.format.global_printables.get(*global_index) {
                *global_index += 1;
                replacement.to_string()
            } else {
                panic!("There are not enough global_vars! One global_var per \"{{}}\"");
            }
        } else {
            if let Some(replacement) = &status.format.file_printables.get(*file_index) {
                *file_index += 1;
                (replacement)(&file_widget)
            } else {
                panic!("There are not enough file_vars! One file_var per \"()\"");
            }
        };

        pos = pos.saturating_add_signed(final_text.len() as isize - text.len() as isize);
        final_text.replace_range(pos..=(pos + 1), replacement.as_str());
    }

    final_text
}

// TODO: Evolve this into a system capable of handling non monospaced text.
fn normalize_status<U>(
    left: String, center: String, right: String, form_count: usize, end_node: &EndNode<U>,
) -> String
where
    U: Ui,
{
    let width = end_node.label.area().width();

    let (config, label) = (end_node.config(), &end_node.label);
    let left_width: usize = label.get_width(left.as_str(), &config.tab_places);
    let center_width: usize = label.get_width(left.as_str(), &config.tab_places);
    let right_width: usize = label.get_width(left.as_str(), &config.tab_places);

    let left_forms: String = left.matches("[]").collect();
    let right_forms: String = right.matches("[]").collect();
    let center_forms: String = center.matches("[]").collect();

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

        status.replace_range((mod_width - right_width).., right.as_str());
        status.replace_range(center_dist..(center_dist + center_width), center.as_str());
        status.replace_range(0..left_width, left.as_str());

    // Print just the left and right parts.
    } else if left_width + right_width <= mod_width {
        // We need to print the center, even while not printing the central part, in order to sync
        // correctly with the `TextLineBuilder`.
        status.replace_range((mod_width - right_width).., right.as_str());
        status.replace_range(left_width..(left_width + center_forms.len()), center_forms.as_str());
        status.replace_range(0..left_width, left.as_str());

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
        left: $left:expr, center: $center:expr, right: $right:expr,
        file_vars: [$($file_to_string:tt),*], global_vars: [$($to_string:tt),*]
    ) => {
        let mut format = StatusFormat::default();
        $(
            format.push_file_var(format_status!(@file_fun $file_to_string));
        )*
        $(
            format.push(format_status!(@get_obj $to_string), format_status!(@get_fun $to_string));
        )*

		format.left_text = left;
		format.center_text = center;
		format.right_text = right;
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
