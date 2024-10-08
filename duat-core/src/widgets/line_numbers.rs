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

use crate::{
    data::{Context, FileReader},
    forms::{self, Form},
    text::{text, Builder, Tag, Text},
    ui::{Area, Constraint, PushSpecs, Ui},
    widgets::{PassiveWidget, Widget, WidgetCfg},
};

/// A simple [`Widget`] that shows what lines of a
/// [`FileWidget`] are shown on screen.
pub struct LineNumbers<U>
where
    U: Ui,
{
    reader: FileReader<U>,
    text: Text,
    cfg: LineNumbersCfg,
}

impl<U> LineNumbers<U>
where
    U: Ui,
{
    pub fn cfg() -> LineNumbersCfg {
        LineNumbersCfg::new()
    }

    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&mut self) -> f64 {
        // "+ 1" because we index from 1, not from 0.
        let len = self.reader.inspect(|file, _, _| file.len_lines()) + 1;
        len.ilog10() as f64
    }

    /// Updates the [`TextBuilder`]'s [`Text`] with the
    /// `FileWidget::<U>::printed_lines()` slice.
    fn update_text(&mut self) {
        self.text = self.reader.inspect(|file, _, input| {
            let printed_lines = file.printed_lines();
            let main_line = input.cursors().map(|cursors| cursors.main().line());

            let mut builder = Text::builder();
            text!(builder, { tag_from_align(self.cfg.align) });

            for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
                let is_main_line = main_line.is_some_and(|main| main == *line);

                if is_main_line {
                    text!(builder, { tag_from_align(self.cfg.main_align) });
                }

                match (is_main_line, is_wrapped) {
                    (false, false) => text!(builder, [LineNum]),
                    (true, false) => text!(builder, [MainLineNum]),
                    (false, true) => text!(builder, [WrappedLineNum]),
                    (true, true) => text!(builder, [WrappedMainLineNum]),
                }

                let is_wrapped = *is_wrapped && index > 0;
                push_text(&mut builder, *line, main_line, is_wrapped, &self.cfg);

                if is_main_line {
                    text!(builder, { tag_from_align(self.cfg.align) });
                }
            }

            builder.finish()
        });
    }
}

impl<U> PassiveWidget<U> for LineNumbers<U>
where
    U: Ui,
{
    /// Returns a function that outputs a [`LineNumbers`], taking a
    /// [`LineNumbersCfg`] as argument.
    fn build(
        context: Context<U>,
        on_file: bool,
    ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
        LineNumbersCfg::default().build(context, on_file)
    }

    fn update(&mut self, area: &U::Area) {
        let width = self.calculate_width();
        area.constrain_hor(Constraint::Length(width + 1.0)).unwrap();

        self.update_text();
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn once(_context: Context<U>) {
        forms::set_weak("LineNum", Form::new().grey());
        forms::set_weak("MainLineNum", Form::new().yellow());
        forms::set_weak("WrappedLineNum", Form::new().cyan().italic());
        forms::set_weak_ref("WrappedMainLineNum", "WrappedLineNum");
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
    align: Alignment,
    main_align: Alignment,
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
            align: Alignment::Left,
            main_align: Alignment::Right,
            show_wraps: false,
            specs: PushSpecs::left(),
        }
    }

    pub fn absolute(self) -> Self {
        Self { numbers: Numbers::Absolute, ..self }
    }

    pub fn relative(self) -> Self {
        Self { numbers: Numbers::Relative, ..self }
    }

    pub fn rel_abs(self) -> Self {
        Self { numbers: Numbers::RelAbs, ..self }
    }

    pub fn align_left(self) -> Self {
        Self {
            main_align: Alignment::Left,
            align: Alignment::Left,
            ..self
        }
    }

    pub fn align_center(self) -> Self {
        Self {
            main_align: Alignment::Center,
            align: Alignment::Center,
            ..self
        }
    }

    pub fn align_right(self) -> Self {
        Self {
            main_align: Alignment::Right,
            align: Alignment::Right,
            ..self
        }
    }

    pub fn align_main_left(self) -> Self {
        Self { main_align: Alignment::Left, ..self }
    }

    pub fn align_main_center(self) -> Self {
        Self { main_align: Alignment::Center, ..self }
    }

    pub fn align_main_right(self) -> Self {
        Self { main_align: Alignment::Right, ..self }
    }

    pub fn show_wraps(self) -> Self {
        Self { show_wraps: true, ..self }
    }

    pub fn hide_wraps(self) -> Self {
        Self { show_wraps: false, ..self }
    }

    pub fn on_the_right(self) -> Self {
        Self { specs: self.specs.to_right(), ..self }
    }
}

impl<U> WidgetCfg<U> for LineNumbersCfg
where
    U: Ui,
{
    type Widget = LineNumbers<U>;

    fn build(self, context: Context<U>, _: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let reader = context.cur_file().unwrap().fixed_reader();
        let specs = self.specs;

        let mut line_numbers = LineNumbers {
            reader: reader.clone(),
            text: Text::default(),
            cfg: self,
        };
        line_numbers.update_text();

        let widget = Widget::passive(line_numbers);
        (widget, move || reader.has_changed(), specs)
    }
}

/// Writes the text of the line number to a given [`String`].
fn push_text(
    builder: &mut Builder,
    line: usize,
    main: Option<usize>,
    is_wrapped: bool,
    cfg: &LineNumbersCfg,
) {
    if is_wrapped && !cfg.show_wraps {
        text!(*builder, "\n");
    } else if let Some(main) = main {
        let num = match cfg.numbers {
            Numbers::Absolute => line + 1,
            Numbers::Relative => line.abs_diff(main),
            Numbers::RelAbs => {
                if line != main {
                    line.abs_diff(main)
                } else {
                    line + 1
                }
            }
        };

        text!(*builder, num "\n");
    } else {
        text!(*builder, { line + 1 } "\n");
    }
}

fn tag_from_align(alignment: Alignment) -> Tag {
    match alignment {
        Alignment::Left => Tag::StartAlignLeft,
        Alignment::Right => Tag::StartAlignRight,
        Alignment::Center => Tag::StartAlignCenter,
    }
}
