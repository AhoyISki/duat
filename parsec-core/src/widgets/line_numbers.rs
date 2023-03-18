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

use super::{
    file_widget::{self, FileWidget},
    NormalWidget, Widget,
};
use crate::{
    config::{DownCastableData, RoData, RwData},
    tags::{
        form::{
            Form, DEFAULT, LINE_NUMBERS, MAIN_LINE_NUMBER, WRAPPED_LINE_NUMBERS,
            WRAPPED_MAIN_LINE_NUMBER,
        },
        Tag,
    },
    text::{self, Text, TextBuilder},
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
        Box::new(move |_, push_specs| {
            let mut line_numbers = LineNumbers {
                file_widget,
                text_builder: TextBuilder::default(),
                min_width: push_specs.split.len(),
                cfg,
            };

            line_numbers.update_text(push_specs.split.len());

            Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(line_numbers))))
        })
    }

    pub fn default_fn(
        file_widget: RoData<FileWidget<U>>,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, push_specs| {
            let mut line_numbers = LineNumbers {
                file_widget,
                text_builder: TextBuilder::default(),
                min_width: push_specs.split.len(),
                cfg: LineNumbersCfg::default(),
            };

            line_numbers.update_text(push_specs.split.len());

            Widget::Normal(RwData::new_unsized(Arc::new(Mutex::new(line_numbers))))
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
        let main_line = file.main_cursor().row();

        let Some(first_line) = printed_lines.first() else {
            self.text_builder.clear();
            return;
        };
        let mut tag = get_tag(*first_line, main_line, false);
        let mut text = get_text(*first_line, main_line, width, &self.cfg);

        self.text_builder.push_tag(tag);
        self.text_builder.push_swappable(&text);

        let mut last_line = *first_line;
        for (index, line) in printed_lines.iter().enumerate() {
            if index < self.text_builder.ranges_len() {
                self.text_builder.swap_tag(index, tag);
                self.text_builder.swap_range(index, &text);
            } else {
                self.text_builder.push_tag(tag);
                self.text_builder.push_swappable(&text);
            }

            tag = get_tag(*first_line, main_line, *line == last_line);
            text = get_text(*first_line, main_line, width, &self.cfg);

            last_line = *line;
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
            .request_len(width.min(self.min_width), Side::Right)
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
    /// Line numbers relative to the main cursor's line, including that line.
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
        Numbering::Relative => usize::abs_diff(line, main_line) + 1,
        Numbering::Hybrid => {
            if line != main_line {
                usize::abs_diff(line, main_line) + 1
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
