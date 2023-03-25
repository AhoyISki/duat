#[cfg(not(feature = "deadlock-detection"))]
use std::sync::Mutex;
use std::{
    any::Any,
    cmp::max,
    fmt::{Alignment, Write},
    sync::Arc,
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::Mutex;

use super::{file_widget::FileWidget, NormalWidget, Widget};
use crate::{
    config::{DownCastableData, RoData},
    tags::{
        form::{LINE_NUMBERS, MAIN_LINE_NUMBER, WRAPPED_LINE_NUMBERS, WRAPPED_MAIN_LINE_NUMBER},
        Tag,
    },
    text::{Text, TextBuilder},
    ui::{Area, EndNode, Label, PushSpecs, Side, Ui},
    SessionManager,
};

pub struct LineNumbers<U>
where
    U: Ui,
{
    file_widget: RoData<FileWidget<U>>,
    text_builder: TextBuilder<U>,
    min_width: usize,
    cfg: LineNumbersCfg,
}

unsafe impl<U> Send for LineNumbers<U> where U: Ui {}

impl<U> LineNumbers<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `LineNumbersWidget`.
    pub fn config_fn(
        file_widget: RoData<FileWidget<U>>,
        cfg: LineNumbersCfg,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, push_specs| -> Widget<U> {
			let file = file_widget.clone();

            let mut line_numbers = LineNumbers {
                file_widget,
                text_builder: TextBuilder::default_string(),
                min_width: push_specs.split.len(),
                cfg,
            };

            line_numbers.update_text(push_specs.split.len());

            let has_changed = Box::new(move || file.has_changed());

            Widget::normal(Arc::new(Mutex::new(line_numbers)), vec![has_changed])
        })
    }

    pub fn default_fn(
        file_widget: RoData<FileWidget<U>>,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, push_specs| {
            let mut line_numbers = LineNumbers {
                file_widget,
                text_builder: TextBuilder::default_string(),
                min_width: push_specs.split.len(),
                cfg: LineNumbersCfg::default(),
            };

            line_numbers.update_text(push_specs.split.len());

            Widget::normal(Arc::new(Mutex::new(line_numbers)), vec![])
        })
    }

    fn calculate_width(&mut self) -> usize {
        let mut width = 1;
        let mut num_exp = 10;
        // "+ 1" because we index from 1, not from 0.
        let len = self.file_widget.read().text().len_lines() + 1;

        while len > num_exp {
            num_exp *= 10;
            width += 1;
        }
        max(width, self.min_width)
    }

    fn update_text(&mut self, width: usize) {
        let file = self.file_widget.read();
        let printed_lines = file.printed_lines();
        let main_line = file.main_cursor().true_row();

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            let tag = get_tag(*line, main_line, *is_wrapped);
            let text = get_text(*line, main_line, width, &self.cfg);

            if index < self.text_builder.ranges_len() {
                self.text_builder.swap_tag(index, tag);
                self.text_builder.swap_range(index, &text);
            } else {
                self.text_builder.push_tag(tag);
                self.text_builder.push_swappable(&text);
            }
        }

        if printed_lines.len() < self.text_builder.ranges_len() {
            self.text_builder.truncate(printed_lines.len());
        }
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
        end_node
            .label
            .area_mut()
            .request_len(width.max(self.min_width), Side::Right)
            .unwrap();

        self.update_text(width);
    }

    fn needs_update(&self) -> bool {
        self.file_widget.has_changed()
    }

    fn text(&self) -> &Text<U> {
        self.text_builder.text()
    }
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum Numbering {
    #[default]
    /// Line numbers relative to the beginning of the file.
    Absolute,
    /// Line numbers relative to the main cursor's line, including
    /// that line.
    Relative,
    /// Relative line numbers on every line, except the main cursor's.
    Hybrid,
}

#[derive(Clone, Copy)]
pub struct LineNumbersCfg {
    pub numbering: Numbering,
    pub alignment: Alignment,
}

impl Default for LineNumbersCfg {
    fn default() -> Self {
        Self {
            alignment: Alignment::Left,
            numbering: Numbering::default(),
        }
    }
}

fn get_tag(line: usize, main_line: usize, is_wrapped: bool) -> Tag {
    let tag = Tag::PushForm(match (line == main_line, is_wrapped) {
        (false, false) => LINE_NUMBERS,
        (false, true) => WRAPPED_LINE_NUMBERS,
        (true, false) => MAIN_LINE_NUMBER,
        (true, true) => WRAPPED_MAIN_LINE_NUMBER,
    });

    tag
}

fn get_text(line: usize, main_line: usize, width: usize, cfg: &LineNumbersCfg) -> String {
    let mut line_text = String::with_capacity(width);

    let number = match cfg.numbering {
        Numbering::Absolute => line + 1,
        Numbering::Relative => usize::abs_diff(line, main_line),
        Numbering::Hybrid => {
            if line != main_line {
                usize::abs_diff(line, main_line)
            } else {
                line + 1
            }
        }
    };

    match cfg.alignment {
        Alignment::Left => write!(&mut line_text, "{:<width$}\n", number).unwrap(),
        Alignment::Center => write!(&mut line_text, "{:^width$}\n", number).unwrap(),
        Alignment::Right => write!(&mut line_text, "{:>width$}\n", number).unwrap(),
    }

    line_text
}
