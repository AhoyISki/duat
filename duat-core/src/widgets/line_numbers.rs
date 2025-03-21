//! Line numbers for a [`File`]
//!
//! These are pretty standard like in most text editors. Usually,
//! they'll be printed on the right of the [`File`], but there is an
//! option to print them on the right, if you need such functionality.
//!
//! You can also change other things, like the
//! relativeness/absoluteness of the numbers, as well as the alignment
//! of the numbers, with one more option to change that of the main
//! cursor's line number.
//!
//! [`File`]: super::File
use std::{fmt::Alignment, marker::PhantomData};

use crate::{
    context::{self, FixedFile},
    form::{self, Form},
    text::{Builder, Tag, Text, text},
    ui::{Area, Constraint, PushSpecs, Ui},
    widgets::{Widget, WidgetCfg},
};

pub struct LineNumbers<U: Ui> {
    ff: FixedFile<U>,
    text: Text,
    cfg: LineNumbersCfg<U>,
}

impl<U: Ui> LineNumbers<U> {
    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&mut self) -> f32 {
        let len = self.ff.read().0.text().len().line();
        len.ilog10() as f32
    }

    fn update_text(&mut self) {
        let (main_line, printed_lines) = {
            let (file, _) = self.ff.read();
            let main_line = match file.cursors().is_empty() {
                true => usize::MAX,
                false => file.cursors().get_main().unwrap().line(),
            };
            (main_line, file.printed_lines().to_vec())
        };

        let mut builder = Text::builder();
        text!(builder, { tag_from_align(self.cfg.align) });

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            if main_line == *line {
                text!(builder, { tag_from_align(self.cfg.main_align) });
            }

            match (main_line == *line, is_wrapped) {
                (false, false) => text!(builder, [LineNum]),
                (true, false) => text!(builder, [MainLineNum]),
                (false, true) => text!(builder, [WrappedLineNum]),
                (true, true) => text!(builder, [WrappedMainLineNum]),
            }

            let is_wrapped = *is_wrapped && index > 0;
            push_text(&mut builder, *line, main_line, is_wrapped, &self.cfg);

            if main_line == *line {
                text!(builder, { tag_from_align(self.cfg.align) });
            }
        }

        self.text = builder.finish()
    }

    pub fn get_cfg(&self) -> LineNumbersCfg<U> {
        self.cfg.clone()
    }

    pub fn reconfigure(&mut self, cfg: LineNumbersCfg<U>) {
        self.cfg = cfg
    }
}

impl<U: Ui> Widget<U> for LineNumbers<U> {
    type Cfg = LineNumbersCfg<U>;

    fn cfg() -> Self::Cfg {
        Self::Cfg {
            num_rel: LineNum::Abs,
            align: Alignment::Left,
            main_align: Alignment::Right,
            show_wraps: false,
            specs: PushSpecs::left(),
            _ghost: PhantomData,
        }
    }

    fn update(&mut self, area: &U::Area) {
        let width = self.calculate_width();
        area.constrain_hor([Constraint::Len(width + 1.0)]).unwrap();

        self.update_text();
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("LineNum", Form::grey());
        form::set_weak("MainLineNum", Form::yellow());
        form::set_weak("WrappedLineNum", Form::cyan().italic());
        form::set_weak("WrappedMainLineNum", "WrappedLineNum");
        Ok(())
    }
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Clone, Copy)]
pub enum LineNum {
    #[default]
    /// Line numbers relative to the beginning of the file.
    Abs,
    /// Line numbers relative to the main cursor's line, including
    /// that line.
    Rel,
    /// Relative line numbers on every line, except the main cursor's.
    RelAbs,
}

/// Configuration options for the [`LineNumbers<U>`] widget.
#[derive(Debug, Clone, Copy)]
#[doc(hidden)]
pub struct LineNumbersCfg<U> {
    pub num_rel: LineNum,
    pub align: Alignment,
    pub main_align: Alignment,
    pub show_wraps: bool,
    specs: PushSpecs,
    _ghost: PhantomData<U>,
}

impl<U> LineNumbersCfg<U> {
    pub fn absolute(self) -> Self {
        Self { num_rel: LineNum::Abs, ..self }
    }

    pub fn relative(self) -> Self {
        Self { num_rel: LineNum::Rel, ..self }
    }

    pub fn rel_abs(self) -> Self {
        Self { num_rel: LineNum::RelAbs, ..self }
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

impl<U: Ui> WidgetCfg<U> for LineNumbersCfg<U> {
    type Widget = LineNumbers<U>;

    fn build(self, _: bool) -> (Self::Widget, impl Fn() -> bool, PushSpecs) {
        let ff = context::fixed_file().unwrap();
        let specs = self.specs;

        let checker = ff.checker();
        let mut widget = LineNumbers { ff, text: Text::default(), cfg: self };
        widget.update_text();

        (widget, checker, specs)
    }
}

/// Writes the text of the line number to a given [`String`].
fn push_text<U>(
    builder: &mut Builder,
    line: usize,
    main: usize,
    is_wrapped: bool,
    cfg: &LineNumbersCfg<U>,
) {
    if is_wrapped && !cfg.show_wraps {
        text!(*builder, "\n");
    } else if main != usize::MAX {
        let num = match cfg.num_rel {
            LineNum::Abs => line + 1,
            LineNum::Rel => line.abs_diff(main),
            LineNum::RelAbs => {
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

fn tag_from_align(alignment: Alignment) -> Option<Tag> {
    match alignment {
        Alignment::Left => None,
        Alignment::Right => Some(Tag::StartAlignRight),
        Alignment::Center => Some(Tag::StartAlignCenter),
    }
}
