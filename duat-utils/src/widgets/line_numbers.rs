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

use duat_core::{
    context::FileHandle,
    data::{Pass, RwData},
    form::{self, Form},
    text::{AlignCenter, AlignLeft, AlignRight, Builder, Text, text},
    ui::{Constraint, PushSpecs, RawArea, Ui},
    widgets::{Widget, WidgetCfg},
};

pub struct LineNumbers<U: Ui> {
    handle: FileHandle<U>,
    text: Text,
    cfg: LineNumbersOptions<U>,
}

impl<U: Ui> LineNumbers<U> {
    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&self, pa: &Pass) -> f32 {
        let len = self.handle.read(pa, |file, _| file.text().len().line());
        len.ilog10() as f32
    }

    fn form_text(&self, pa: &Pass) -> Text {
        let (main_line, printed_lines) = self.handle.read(pa, |file, _| {
            let main_line = if file.cursors().is_empty() {
                usize::MAX
            } else {
                file.cursors().get_main().unwrap().line()
            };

            (main_line, file.printed_lines().to_vec())
        });

        let mut builder = Text::builder();
        align(&mut builder, self.cfg.align);

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            if *line == main_line {
                align(&mut builder, self.cfg.main_align);
            }

            match (main_line == *line, is_wrapped) {
                (false, false) => {}
                (true, false) => builder.push(form::id_of!("MainLineNum")),
                (false, true) => builder.push(form::id_of!("WrappedLineNum")),
                (true, true) => builder.push(form::id_of!("WrappedMainLineNum")),
            }

            let is_wrapped = *is_wrapped && index > 0;
            push_text(&mut builder, *line, main_line, is_wrapped, &self.cfg);

            if *line == main_line {
                align(&mut builder, self.cfg.align);
            }
        }

        builder.build()
    }

    /// The options for these [`LineNumbers`]
    pub fn options(&self) -> &LineNumbersOptions<U> {
        &self.cfg
    }

    /// The mutable options for these [`LineNumbers`]
    pub fn options_mut(&mut self) -> &mut LineNumbersOptions<U> {
        &mut self.cfg
    }
}

impl<U: Ui> Widget<U> for LineNumbers<U> {
    type Cfg = LineNumbersOptions<U>;

    async fn update(mut pa: Pass<'_>, widget: RwData<Self>, area: &<U as Ui>::Area) {
        let width = widget.read(&pa, |ln| ln.calculate_width(&pa));
        area.constrain_hor([Constraint::Len(width + 1.0)]).unwrap();

        let text = widget.read(&pa, |ln| ln.form_text(&pa));
        widget.write(&mut pa, |ln| ln.text = text);
    }

    fn needs_update(&self) -> bool {
        self.handle.has_changed()
    }

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

/// Configuration options for the [`LineNumbers`] widget.
#[derive(Debug, Clone, Copy)]
#[doc(hidden)]
pub struct LineNumbersOptions<U> {
    pub num_rel: LineNum = LineNum::Abs,
    pub align: Alignment = Alignment::Left,
    pub main_align: Alignment = Alignment::Right,
    pub show_wraps: bool = false,
    specs: PushSpecs = PushSpecs::left(),
    _ghost: PhantomData<U> = PhantomData,
}

impl<U> LineNumbersOptions<U> {
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

impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
    type Widget = LineNumbers<U>;

    fn build(self, pa: Pass, handle: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let Some(handle) = handle else {
            panic!("For now, you can't push LineNumbers to something that is not a File");
        };
        let specs = self.specs;

        let mut widget = LineNumbers { handle, text: Text::default(), cfg: self };
        widget.text = widget.form_text(&pa);

        (widget, specs)
    }
}

/// Writes the text of the line number to a given [`String`].
fn push_text<U>(
    b: &mut Builder,
    line: usize,
    main: usize,
    is_wrapped: bool,
    cfg: &LineNumbersOptions<U>,
) {
    if is_wrapped && !cfg.show_wraps {
        b.push(text!("[]\n"));
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

        b.push(text!("{num}[]\n"));
    } else {
        b.push(text!("{}[]\n", line + 1));
    }
}

fn align(b: &mut Builder, alignment: Alignment) {
    match alignment {
        Alignment::Left => b.push(AlignLeft),
        Alignment::Center => b.push(AlignCenter),
        Alignment::Right => b.push(AlignRight),
    }
}
