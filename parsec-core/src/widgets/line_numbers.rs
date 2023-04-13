#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{any::Any, cmp::max, fmt::Write, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use super::{file_widget::FileWidget, NormalWidget, Widget};
use crate::{
    config::{Config, DownCastableData, RoData},
    tags::{
        form::{LINE_NUMBERS, MAIN_LINE_NUMBER, WRAPPED_LINE_NUMBERS, WRAPPED_MAIN_LINE_NUMBER},
        Tag,
    },
    text::{Text, TextBuilder},
    ui::{Area, Label, PushSpecs, Side, Ui},
    updaters, SessionManager,
};

pub struct LineNumbers<U>
where
    U: Ui,
{
    file_widget: RoData<FileWidget<U>>,
    builder: TextBuilder<U>,
    min_width: usize,
    cfg: LineNumbersCfg,
}

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
                builder: TextBuilder::<U>::default(),
                min_width: push_specs.split.len(),
                cfg,
            };
            let width = line_numbers.calculate_width();

            line_numbers.update_text(width);

            Widget::normal(Arc::new(RwLock::new(line_numbers)), updaters![file])
        })
    }

    pub fn default_fn(
        file_widget: RoData<FileWidget<U>>,
    ) -> Box<dyn FnOnce(&SessionManager, PushSpecs) -> Widget<U>> {
        Box::new(move |_, push_specs| {
            let updaters = updaters![(file_widget.clone())];

            let mut line_numbers = LineNumbers {
                file_widget,
                builder: TextBuilder::<U>::default(),
                min_width: push_specs.split.len(),
                cfg: LineNumbersCfg::default(),
            };

            line_numbers.update_text(push_specs.split.len());

            Widget::normal(Arc::new(RwLock::new(line_numbers)), updaters)
        })
    }

    fn calculate_width(&mut self) -> usize {
        let mut width = 2;
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
        let mut text = String::new();

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            let tag = get_tag(*line, main_line, *is_wrapped);
            write_text(&mut text, *line, main_line, width, &self.cfg);

            if index < self.builder.ranges_len() {
                self.builder.swap_tag(index, tag);
                self.builder.swap_range(index, &text);
            } else {
                self.builder.push_tag(tag);
                self.builder.push_swappable(&text);
            }
        }

        self.builder.truncate(printed_lines.len());
    }
}

impl<U> NormalWidget<U> for LineNumbers<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        "parsec-line-numbers"
    }

    fn update(&mut self, label: &U::Label, _config: &Config) {
        let width = self.calculate_width();
        label.area().request_len(width.max(self.min_width), Side::Right).unwrap();

        self.update_text(width);
    }

    fn needs_update(&self) -> bool {
        self.file_widget.has_changed()
    }

    fn text(&self) -> &Text<U> {
        &self.builder.text()
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

/// How to show the line numbers on screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum Alignment {
    #[default]
    Right,
    Left,
    Center,
}

/// Configuration options for the [`LineNumbers<U>`] widget.
#[derive(Default, Clone, Copy)]
pub struct LineNumbersCfg {
    pub numbering: Numbering,
    pub alignment: Alignment,
}

impl LineNumbersCfg {
    /// Returns a new instance of [`LineNumbersCfg`].
    pub fn new(numbering: Numbering, alignment: Alignment) -> Self {
        Self {
            numbering,
            alignment,
        }
    }
}

/// Gets the [`Tag`], according to line positioning.
fn get_tag(line: usize, main_line: usize, is_wrapped: bool) -> Tag {
    let tag = Tag::PushForm(match (line == main_line, is_wrapped) {
        (false, false) => LINE_NUMBERS,
        (false, true) => WRAPPED_LINE_NUMBERS,
        (true, false) => MAIN_LINE_NUMBER,
        (true, true) => WRAPPED_MAIN_LINE_NUMBER,
    });

    tag
}

/// Writes the text of the line number to a given [`String`].
fn write_text(
    text: &mut String,
    line: usize,
    main_line: usize,
    width: usize,
    cfg: &LineNumbersCfg,
) {
    text.clear();
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
        Alignment::Left => write!(text, "{:<width$}\n", number).unwrap(),
        Alignment::Center => write!(text, "{:^width$}\n", number).unwrap(),
        Alignment::Right => write!(text, "{:>width$}\n", number).unwrap(),
    }
}
