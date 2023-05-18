//! A [`NormalWidget`] that shows the visible line numbers of a
//! [`FileWidget<U>`].
//!
//! This widget has various options to configure the presentation of
//! the lines. These can be found in the [`LineNumbersCfg`] struct.
//!
//! The first option is [`Numbering`], which determines the numbers
//! that will show up on the lines. They can be
//! [`Absolute`][Numbering::Absolute], which shows the number as the
//! index from the first line, [`Relative`][Numbering::Relative],
//! which show the numbers relative to the main cursor's line, or
//! [`Hybrid`][Numbering::Hybrid], which is like
//! [`Absolute`][Numbering::Absolute] for the main cursor's line, and
//! like [`Hybrid`][Numbering::Hybrid] for all other lines. It is
//! [`Absolute`][Numbering::Absolute] by default.
//!
//! The second option is [`Alignment`], which can be
//! [`Left`][Alignment::Left], [`Right`][Alignment::Right], or
//! [`Center`][Alignment::Center], it determines the side where the
//! numbers will be printed. This struct shows up twice in
//! [`LineNumbersCfg`], once for the main cursor's line, and once for
//! all other lines. Its [`Right`][Alignment::Right] by default.
use std::{cmp::max, fmt::Write};

use super::{file_widget::FileWidget, NormalWidget, Widget};
use crate::{
    data::{DownCastableData, RoData},
    tags::{
        form::{LINE_NUMBERS, MAIN_LINE_NUMBER, WRAPPED_LINE_NUMBERS, WRAPPED_MAIN_LINE_NUMBER},
        Tag
    },
    text::{Text, TextBuilder},
    ui::{PushSpecs, Ui, Area, Constraint},
    updaters, Manager
};

/// A simple [`NormalWidget`] that shows what lines of a
/// [`FileWidget<U>`] are shown on screen.
pub struct LineNumbers<U>
where
    U: Ui
{
    file_widget: RoData<FileWidget<U>>,
    builder: TextBuilder<U>,
    cfg: LineNumbersCfg
}

impl<U> LineNumbers<U>
where
    U: Ui + 'static
{
    /// Returns a function that outputs a [`LineNumbers<U>`], taking a
    /// [`LineNumbersCfg`] as argument.
    pub fn config_fn(
        file_widget: RoData<FileWidget<U>>, cfg: LineNumbersCfg
    ) -> impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U> {
        move |_, push_specs| -> Widget<U> {
            let file = file_widget.clone();

            let mut line_numbers = LineNumbers {
                file_widget,
                builder: TextBuilder::<U>::default(),
                cfg
            };
            let width = line_numbers.calculate_width();

            line_numbers.update_text(width as usize);

            Widget::normal(line_numbers, updaters![file])
        }
    }

    /// Returns a function that outputs the default instance of
    /// [`LineNumbers<U>`].
    pub fn default_fn(
        file_widget: RoData<FileWidget<U>>
    ) -> impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U> {
        move |_, push_specs| {
            let updaters = updaters![(file_widget.clone())];

            let mut line_numbers = LineNumbers {
                file_widget,
                builder: TextBuilder::<U>::default(),
                cfg: LineNumbersCfg::default()
            };

			let width = line_numbers.calculate_width();
            line_numbers.update_text(width as usize);

            Widget::normal(line_numbers, updaters)
        }
    }

    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&mut self) -> f64 {
        let mut width = 1f64;
        let mut num_exp = 10;
        // "+ 1" because we index from 1, not from 0.
        let len = self.file_widget.read().text().len_lines() + 1;

        while len > num_exp {
            num_exp *= 10;
            width += 1f64;
        }

        width
    }

    /// Updates the [`TextBuilder<U>`]'s [`Text<U>`] with the
    /// `FileWidget::<U>::printed_lines()` slice.
    fn update_text(&mut self, width: usize) {
        let file = self.file_widget.read();
        let printed_lines = file.printed_lines();
        let main_line = file.main_cursor().true_line();
        let mut text = String::new();

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            let tag = get_tag(*line, main_line, *is_wrapped);
            write_text(&mut text, *line, main_line, *is_wrapped, width, &self.cfg);

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
    U: Ui + 'static
{
    fn update(&mut self, area: &mut U::Area) {
        let width = self.calculate_width();
        area.change_constraint(Constraint::Length(width)).unwrap();

        self.update_text(width as usize);
    }

    fn text(&self) -> &Text<U> {
        &self.builder.text()
    }
}

impl<U> DownCastableData for LineNumbers<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum Numbers {
    #[default]
    /// Line numbers relative to the beginning of the file.
    Absolute,
    /// Line numbers relative to the main cursor's line, including
    /// that line.
    Relative,
    /// Relative line numbers on every line, except the main cursor's.
    Hybrid
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum Align {
    #[default]
    Right,
    Left,
    Center
}

/// Configuration options for the [`LineNumbers<U>`] widget.
#[derive(Default, Clone, Copy)]
pub struct LineNumbersCfg {
    pub numbers: Numbers,
    pub align: Align,
    pub main_align: Align,
    pub hide_wraps: bool
}

impl LineNumbersCfg {
    /// Returns a new instance of [`LineNumbersCfg`].
    pub fn new(numbers: Numbers, align: Align, main_align: Align, hide_wraps: bool) -> Self {
        Self {
            numbers,
            align,
            main_align,
            hide_wraps
        }
    }
}

/// Gets the [`Tag`], according to line positioning.
fn get_tag(line: usize, main_line: usize, is_wrapped: bool) -> Tag {
    let tag = Tag::PushForm(match (line == main_line, is_wrapped) {
        (false, false) => LINE_NUMBERS,
        (false, true) => WRAPPED_LINE_NUMBERS,
        (true, false) => MAIN_LINE_NUMBER,
        (true, true) => WRAPPED_MAIN_LINE_NUMBER
    });

    tag
}

/// Writes the text of the line number to a given [`String`].
fn write_text(
    text: &mut String, line: usize, main_line: usize, is_wrapped: bool, width: usize,
    cfg: &LineNumbersCfg
) {
    text.clear();
    let number = match cfg.numbers {
        Numbers::Absolute => line + 1,
        Numbers::Relative => usize::abs_diff(line, main_line),
        Numbers::Hybrid => {
            if line != main_line {
                usize::abs_diff(line, main_line)
            } else {
                line + 1
            }
        }
    };

    let alignment = if line == main_line {
        cfg.main_align
    } else {
        cfg.align
    };

    if is_wrapped && cfg.hide_wraps {
        *text = " ".repeat(width) + "\n";
    } else {
        match alignment {
            Align::Left => write!(text, "{:<width$}\n", number).unwrap(),
            Align::Center => write!(text, "{:^width$}\n", number).unwrap(),
            Align::Right => write!(text, "{:>width$}\n", number).unwrap()
        }
    }
}
