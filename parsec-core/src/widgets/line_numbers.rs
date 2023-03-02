use std::{
    any::Any,
    cmp::max,
    fmt::{Alignment, Write},
    sync::Arc,
};

use no_deadlocks::Mutex;

use super::{file_widget::FileWidget, NormalWidget, Widget};
use crate::{
    config::{DownCastableData, RoData, RwData},
    tags::form::{DEFAULT, LINE_NUMBERS, MAIN_LINE_NUMBER},
    text::{Text, TextLineBuilder},
    ui::{Area, EndNode, Label, Side, Ui},
    SessionManager,
};

pub struct LineNumbers<U>
where
    U: Ui,
{
    file_widget: RoData<FileWidget<U>>,
    text: Text<U>,
    main_line_builder: TextLineBuilder,
    other_line_builder: TextLineBuilder,
    min_width: usize,
    line_numbers_config: LineNumbersConfig,
}

unsafe impl<U> Send for LineNumbers<U> where U: Ui {}

impl<U> LineNumbers<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `LineNumbersWidget`.
    pub fn new(
        file_widget: RoData<FileWidget<U>>, line_numbers_config: LineNumbersConfig,
    ) -> Box<dyn FnOnce(&SessionManager) -> Widget<U>> {
        Box::new(move |_| {
            let line_numbers = LineNumbers {
                file_widget,
                text: Text::default(),
                main_line_builder: TextLineBuilder::from([MAIN_LINE_NUMBER, DEFAULT]),
                other_line_builder: TextLineBuilder::from([LINE_NUMBERS, DEFAULT]),
                min_width: 1,
                line_numbers_config,
            };

            Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(line_numbers))))
        })
    }

    pub fn default(
        file_widget: RoData<FileWidget<U>>,
    ) -> Box<dyn FnOnce(&SessionManager) -> Widget<U>> {
        Box::new(move |_| {
            let line_numbers = LineNumbers {
                file_widget,
                text: Text::default(),
                main_line_builder: TextLineBuilder::from([MAIN_LINE_NUMBER, DEFAULT]),
                other_line_builder: TextLineBuilder::from([LINE_NUMBERS, DEFAULT]),
                min_width: 1,
                line_numbers_config: LineNumbersConfig::default(),
            };

            Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(line_numbers))))
        })
    }

    fn calculate_width(&mut self) -> usize {
        let mut width = 1;
        let mut num_exp = 10;
        // "+ 1" because we index from 1, not from 0.
        let len = self.file_widget.read().text().lines().len() + 1;

        while len > num_exp {
            num_exp *= 10;
            width += 1;
        }
        max(width, self.min_width)
    }
}

impl<U> DownCastableData for LineNumbers<U>
where
    U: Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl<U> NormalWidget<U> for LineNumbers<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        "parsec-line-numbers"
    }

    fn update(&mut self, end_node: &mut EndNode<U>) {
        let width = self.calculate_width();
        let file = self.file_widget.read();
        end_node.label.area_mut().request_len(width, Side::Right).unwrap();

        let lines = file.printed_lines();
        let main_line = file.main_cursor().true_row();

        self.text.clear_lines();

        for line in lines.iter() {
            let mut line_number = String::with_capacity(width + 5);
            let number = match self.line_numbers_config.numbering {
                Numbering::Absolute => *line + 1,
                Numbering::Relative => usize::abs_diff(*line + 1, main_line),
                Numbering::Hybrid => {
                    if *line != main_line {
                        usize::abs_diff(*line, main_line) + 1
                    } else {
                        *line + 1
                    }
                }
            };
            match self.line_numbers_config.alignment {
                Alignment::Left => write!(&mut line_number, "[]{:<width$}[]\n", number).unwrap(),
                Alignment::Right => write!(&mut line_number, "[]{:>width$}[]\n", number).unwrap(),
                Alignment::Center => write!(&mut line_number, "[]{:^width$}[]\n", number).unwrap(),
            }
            if *line == main_line {
                self.text.push_line(self.main_line_builder.form_text_line(line_number));
            } else {
                self.text.push_line(self.other_line_builder.form_text_line(line_number));
            }
        }
    }

    fn needs_update(&self) -> bool {
        self.file_widget.has_changed()
    }

    fn text(&self) -> &Text<U> {
        &self.text
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
