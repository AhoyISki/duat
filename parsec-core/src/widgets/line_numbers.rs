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

use super::{file_widget::FileWidget, PassiveWidget, Widget};
use crate::{
    data::{AsAny, ReadableData, RoData},
    forms::Form,
    input::InputMethod,
    text::{build, Tag, Text},
    ui::{Area, Constraint, PushSpecs, Ui},
    Controler, PALETTE,
};

/// A simple [`Widget`] that shows what lines of a
/// [`FileWidget`] are shown on screen.
pub struct LineNumbers {
    file: RoData<FileWidget>,
    input: RoData<dyn InputMethod>,
    text: Text,
    cfg: LineNumbersCfg,
}

impl LineNumbers {
    pub fn config() -> LineNumbersCfg {
        LineNumbersCfg::new()
    }

    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&mut self) -> f64 {
        let mut width = 1.0;
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
        let main_line: Option<usize> = self
            .input
            .read()
            .cursors()
            .map(|cursors| cursors.main().true_line());

        let file = self.file.read();
        let printed_lines = file.printed_lines();

        let mut builder = build!(((tag_from_align(self.cfg.alignment))));

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            let is_main_line = main_line.is_some_and(|main| main == *line);

            let num_text = get_text(*line, main_line, *is_wrapped && index > 0, &self.cfg);

            if is_main_line {
                build!(builder, ((tag_from_align(self.cfg.main_alignment))));
            }

            match (is_main_line, is_wrapped) {
                (false, false) => build!(builder, [LineNum]),
                (true, false) => build!(builder, [MainLineNum]),
                (false, true) => build!(builder, [WrappedLineNum]),
                (true, true) => build!(builder, [WrappedMainLineNum]),
            }

            build!(builder, num_text);

            if is_main_line {
                build!(builder, ((tag_from_align(self.cfg.alignment))));
            }
        }

        self.text = builder.finish();
    }
}

impl PassiveWidget for LineNumbers {
    /// Returns a function that outputs a [`LineNumbers<U>`], taking a
    /// [`LineNumbersCfg`] as argument.
    fn build<U>(controler: &Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        LineNumbersCfg::default().builder()(controler)
    }

    fn update(&mut self, area: &impl Area) {
        let width = self.calculate_width();
        area.change_constraint(Constraint::Length(width + 1.0))
            .unwrap();

        self.update_text();
    }

    fn text(&self) -> &Text {
        &self.text
    }
}

impl AsAny for LineNumbers {
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
    RelAbs,
}

/// Configuration options for the [`LineNumbers<U>`] widget.
#[derive(Debug, Clone, Copy)]
pub struct LineNumbersCfg {
    numbers: Numbers,
    alignment: Alignment,
    main_alignment: Alignment,
    show_wraps: bool,
    specs: PushSpecs,
}

impl Default for LineNumbersCfg {
    fn default() -> Self {
        Self::new()
    }
}

impl LineNumbersCfg {
    pub fn new() -> Self {
        Self {
            numbers: Numbers::Absolute,
            alignment: Alignment::Left,
            main_alignment: Alignment::Right,
            show_wraps: false,
            specs: PushSpecs::left(),
        }
    }

    pub fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        move |controler| {
            let file = controler.current_file();
            let specs = self.specs;

            PALETTE.try_set_form("LineNum", Form::new().grey());
            PALETTE.try_set_form("MainLineNum", Form::new().yellow());
            PALETTE.try_set_form("WrappedLineNum", Form::new().cyan().italic());
            PALETTE.set_new_ref("WrappedMainLineNum", "WrappedLineNumbers");

            let mut line_numbers = LineNumbers {
                file: file.clone(),
                input: controler.current_input(),
                text: Text::default(),
                cfg: self,
            };
            line_numbers.update_text();

            let widget = Widget::passive(line_numbers);
            (widget, Box::new(move || file.has_changed()), specs)
        }
    }

    pub fn absolute(self) -> Self {
        Self {
            numbers: Numbers::Absolute,
            ..self
        }
    }

    pub fn relative(self) -> Self {
        Self {
            numbers: Numbers::Relative,
            ..self
        }
    }

    pub fn rel_abs(self) -> Self {
        Self {
            numbers: Numbers::RelAbs,
            ..self
        }
    }

    pub fn align_left(self) -> Self {
        Self {
            main_alignment: Alignment::Left,
            alignment: Alignment::Left,
            ..self
        }
    }

    pub fn align_center(self) -> Self {
        Self {
            main_alignment: Alignment::Center,
            alignment: Alignment::Center,
            ..self
        }
    }

    pub fn align_right(self) -> Self {
        Self {
            main_alignment: Alignment::Right,
            alignment: Alignment::Right,
            ..self
        }
    }

    pub fn align_main_left(self) -> Self {
        Self {
            main_alignment: Alignment::Left,
            ..self
        }
    }

    pub fn align_main_center(self) -> Self {
        Self {
            main_alignment: Alignment::Center,
            ..self
        }
    }

    pub fn align_main_right(self) -> Self {
        Self {
            main_alignment: Alignment::Right,
            ..self
        }
    }

    pub fn show_wraps(self) -> Self {
        Self {
            show_wraps: true,
            ..self
        }
    }

    pub fn hide_wraps(self) -> Self {
        Self {
            show_wraps: false,
            ..self
        }
    }

    pub fn with_specs(self, specs: PushSpecs) -> Self {
        Self { specs, ..self }
    }
}

unsafe impl Send for LineNumbers {}
unsafe impl Sync for LineNumbers {}

/// Writes the text of the line number to a given [`String`].
fn get_text(line: usize, main: Option<usize>, is_wrapped: bool, cfg: &LineNumbersCfg) -> String {
    if is_wrapped && !cfg.show_wraps {
        String::from("\n")
    } else if let Some(main) = main {
        match cfg.numbers {
            Numbers::Absolute => (line + 1).to_string() + "\n",
            Numbers::Relative => usize::abs_diff(line, main).to_string() + "\n",
            Numbers::RelAbs => {
                if line != main {
                    usize::abs_diff(line, main).to_string() + "\n"
                } else {
                    (line + 1).to_string() + "\n"
                }
            }
        }
    } else {
        (line + 1).to_string() + "\n"
    }
}

fn tag_from_align(alignment: Alignment) -> Tag {
    match alignment {
        Alignment::Left => Tag::StartAlignLeft,
        Alignment::Right => Tag::StartAlignRight,
        Alignment::Center => Tag::StartAlignCenter,
    }
}
