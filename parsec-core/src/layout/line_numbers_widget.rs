use crate::{
    config::{RoData, RwData},
    cursor::TextCursor,
    file::Text,
    ui::{Area, EndNode, Label, NodeManager, Ui},
};

use super::{
    file_widget::{FileWidget, PrintedLines},
    Widget,
};

use std::{
    cmp::max,
    fmt::{Alignment, Write},
};

pub struct LineNumbersWidget<U>
where
    U: Ui,
{
    node: RwData<EndNode<U>>,
    printed_lines: PrintedLines<U>,
    main_cursor: RoData<usize>,
    cursors: RoData<Vec<TextCursor>>,
    text: RwData<Text>,
    min_width: usize,
    line_numbers_config: LineNumbersConfig,
}

unsafe impl<U> Send for LineNumbersWidget<U> where U: Ui {}

impl<U> LineNumbersWidget<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `LineNumbersWidget`.
    pub fn new(
        node: RwData<EndNode<U>>, _: &mut NodeManager<U>, file_widget: RwData<FileWidget<U>>,
        line_numbers_config: LineNumbersConfig,
    ) -> Box<dyn Widget<U>> {
        let file_widget = file_widget.read();

        let printed_lines = file_widget.printed_lines();
        let main_cursor = RoData::from(&file_widget.main_cursor);
        let cursors = RoData::from(&file_widget.cursors);
        let min_width = node.read().label.read().area().width();

        let mut line_numbers = LineNumbersWidget {
            node,
            printed_lines,
            main_cursor,
            cursors,
            text: RwData::new(Text::default()),
            min_width,
            line_numbers_config,
        };

        let width = line_numbers.calculate_width();
        line_numbers.node.write().request_width(width);

        line_numbers.update();

        Box::new(line_numbers)
    }

    fn calculate_width(&self) -> usize {
        let mut width = 1;
        let mut num_exp = 10;
        let len = self.printed_lines.text().read().lines().len();

        while len > num_exp {
            num_exp *= 10;
            width += 1;
        }
        max(width, self.min_width)
    }
}

impl<U> Widget<U> for LineNumbersWidget<U>
where
    U: Ui + 'static,
{
    fn update(&mut self) {
        let width = self.calculate_width();
        self.node.write().request_width(width);

        let lines = self.printed_lines.lines();
        let main_line = self.cursors.read().get(*self.main_cursor.read()).unwrap().caret().row;

        // 3 is probably the average length of the numbers, in digits, plus 1 for each "\n".
        let mut line_numbers = String::with_capacity(width * lines.len());

        for line in lines.iter() {
            let number = match self.line_numbers_config.numbering {
                Numbering::Absolute => *line,
                Numbering::Relative => usize::abs_diff(*line, main_line),
                Numbering::Hybrid => {
                    if *line != main_line {
                        usize::abs_diff(*line, main_line)
                    } else {
                        *line
                    }
                }
            };
            match self.line_numbers_config.alignment {
                Alignment::Left => write!(&mut line_numbers, "{:<width$}\n", number).unwrap(),
                Alignment::Right => write!(&mut line_numbers, "{:>width$}\n", number).unwrap(),
                Alignment::Center => write!(&mut line_numbers, "{:^width$}\n", number).unwrap(),
            }
        }

        let mut text = self.text.write();
        *text = Text::new(line_numbers, None);
    }

    fn needs_update(&self) -> bool {
        self.printed_lines.has_changed()
    }

    fn text(&self) -> RoData<Text> {
        RoData::from(&self.text)
    }

    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.node
    }
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum Numbering {
    #[default]
    /// Line numbers relative to the beginning of the file.
    Absolute,
    /// Line numbers relative to the main cursor's line, including that line.
    Relative,
    /// Relative line numbers on every line, except the main cursor's.
    Hybrid,
}

#[derive(Clone, Copy)]
pub struct LineNumbersConfig {
    pub numbering: Numbering,
    pub alignment: Alignment,
}

impl Default for LineNumbersConfig {
    fn default() -> Self {
        Self { alignment: Alignment::Left, numbering: Numbering::default() }
    }
}
