//! A [`Widget`] that shows the visible line numbers of a
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
use std::fmt::Alignment;

use super::{file_widget::FileWidget, Widget, WidgetType};
use crate::{
    data::{DownCastableData, ReadableData, RoData},
    tags::{
        form::{LINE_NUMBERS, MAIN_LINE_NUMBER, WRAPPED_LINE_NUMBERS, WRAPPED_MAIN_LINE_NUMBER},
        Tag
    },
    text::{Text, TextBuilder},
    ui::{Area, Constraint, PushSpecs, Ui},
    Controler
};

/// A simple [`Widget`] that shows what lines of a
/// [`FileWidget<U>`] are shown on screen.
pub struct LineNumbers<U>
where
    U: Ui
{
    file: RoData<FileWidget<U>>,
    builder: TextBuilder,
    cfg: LineNumbersCfg
}

impl<U> LineNumbers<U>
where
    U: Ui + 'static
{
    /// Returns a function that outputs a [`LineNumbers<U>`], taking a
    /// [`LineNumbersCfg`] as argument.
    pub fn config_fn(
        cfg: LineNumbersCfg
    ) -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        move |controler| {
            let file = controler.active_file();

            let mut line_numbers = LineNumbers {
                file: file.clone(),
                builder: TextBuilder::default(),
                cfg
            };
            line_numbers.update_text();

            let widget_type = WidgetType::no_input(line_numbers);
            (widget_type, Box::new(move || file.has_changed()), PushSpecs::left_free())
        }
    }

    /// Returns a function that outputs the default instance of
    /// [`LineNumbers<U>`].
    pub fn default_fn()
    -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        LineNumbers::config_fn(LineNumbersCfg::default())
    }

    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&mut self) -> f64 {
        let mut width = 1f64;
        let mut num_exp = 10;
        // "+ 1" because we index from 1, not from 0.
        let len = self.file.read().text().len_lines() + 1;

        while len > num_exp {
            num_exp *= 10;
            width += 1.0;
        }

        width
    }

    /// Updates the [`TextBuilder`]'s [`Text`] with the
    /// `FileWidget::<U>::printed_lines()` slice.
    fn update_text(&mut self) {
        let file = self.file.read();
        let printed_lines = file.printed_lines();
        let main_line = file.main_cursor().true_line();
        let mut text = String::new();

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            let tag = get_tag(*line, main_line, *is_wrapped);
            write_text(&mut text, *line, main_line, *is_wrapped, &self.cfg);

            let align_tag = {
                let alignment = if *line == main_line {
                    self.cfg.main_alignment
                } else {
                    self.cfg.alignment
                };
                match alignment {
                    Alignment::Left => Tag::AlignLeft,
                    Alignment::Right => Tag::AlignRight,
                    Alignment::Center => Tag::AlignCenter
                }
            };

            if index < self.builder.ranges_len() {
                self.builder.swap_tag(index * 2, align_tag);
                self.builder.swap_tag(index * 2 + 1, tag);
                self.builder.swap_range(index, &text);
            } else {
                self.builder.push_tag(align_tag);
                self.builder.push_tag(tag);
                self.builder.push_swappable(&text);
            }
        }

        // log_info!("{:#?}", self.builder);

        self.builder.truncate(printed_lines.len());
    }
}

impl<U> Widget<U> for LineNumbers<U>
where
    U: Ui + 'static
{
    fn update(&mut self, area: &U::Area) {
        let width = self.calculate_width();
        area.change_constraint(Constraint::Length(width + 1.0)).unwrap();

        self.update_text();
    }

    fn text(&self) -> &Text {
        &self.builder.text()
    }

    fn is_slow(&self) -> bool {
        false
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
enum Numbers {
    #[default]
    /// Line numbers relative to the beginning of the file.
    Absolute,
    /// Line numbers relative to the main cursor's line, including
    /// that line.
    Relative,
    /// Relative line numbers on every line, except the main cursor's.
    RelAbs
}

/// Configuration options for the [`LineNumbers<U>`] widget.
#[derive(Debug, Clone, Copy)]
pub struct LineNumbersCfg {
    numbers: Numbers,
    alignment: Alignment,
    main_alignment: Alignment,
    show_wraps: bool
}

impl Default for LineNumbersCfg {
    fn default() -> Self {
        Self {
            numbers: Numbers::Absolute,
            alignment: Alignment::Left,
            main_alignment: Alignment::Right,
            show_wraps: false
        }
    }
}

impl LineNumbersCfg {
    /// Returns a new instance of [`LineNumbersCfg`].
    pub fn abs(align: Alignment, main_align: Alignment) -> Self {
        Self {
            numbers: Numbers::Absolute,
            alignment: align,
            main_alignment: main_align,
            show_wraps: false
        }
    }

    /// Returns a new instance of [`LineNumbersCfg`].
    pub fn abs_with_wraps(align: Alignment, main_align: Alignment) -> Self {
        Self {
            numbers: Numbers::Absolute,
            alignment: align,
            main_alignment: main_align,
            show_wraps: true
        }
    }

    pub fn rel(align: Alignment, main_align: Alignment) -> Self {
        Self {
            numbers: Numbers::Relative,
            alignment: align,
            main_alignment: main_align,
            show_wraps: false
        }
    }

    pub fn rel_with_wraps(align: Alignment, main_align: Alignment) -> Self {
        Self {
            numbers: Numbers::Relative,
            alignment: align,
            main_alignment: main_align,
            show_wraps: true
        }
    }

    pub fn rel_abs(align: Alignment, main_align: Alignment) -> Self {
        Self {
            numbers: Numbers::RelAbs,
            alignment: align,
            main_alignment: main_align,
            show_wraps: false
        }
    }

    pub fn rel_abs_wraps(align: Alignment, main_align: Alignment) -> Self {
        Self {
            numbers: Numbers::RelAbs,
            alignment: align,
            main_alignment: main_align,
            show_wraps: true
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
    text: &mut String, line: usize, main_line: usize, is_wrapped: bool, cfg: &LineNumbersCfg
) {
    text.clear();
    *text = if is_wrapped && !cfg.show_wraps {
        String::from("\n")
    } else {
        match cfg.numbers {
            Numbers::Absolute => (line + 1).to_string() + "\n",
            Numbers::Relative => usize::abs_diff(line, main_line).to_string() + "\n",
            Numbers::RelAbs => {
                if line != main_line {
                    usize::abs_diff(line, main_line).to_string() + "\n"
                } else {
                    (line + 1).to_string() + "\n"
                }
            }
        }
    };
}
