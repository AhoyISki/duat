//! Line numbers for a [`File`]
//!
//! These are pretty standard like in most text editors. Usually,
//! they'll be printed on the right of the [`File`], but there is an
//! option to print them on the right, if you need such functionality.
//!
//! You can also change other things, like the
//! relativeness/absoluteness of the numbers, as well as the alignment
//! of the numbers, with one more option to change that of the main
//! selection's line number.
//!
//! [`File`]: duat_core::file::File
use std::{fmt::Alignment, marker::PhantomData};

use duat_core::{prelude::*, text::Builder, ui::Side};

/// Shows a column of line numbers beside the [`File`]
///
/// This can be configured through [`LineNumbers::cfg`], in order to
/// get, for example: relative numbering, different alignment,
/// hidden/shown wrapped lines, etc.
pub struct LineNumbers<U: Ui> {
    handle: Handle<File<U>, U>,
    text: Text,
    /// The numbering of lines, [`Numbering::Abs`] by default
    ///
    /// Can be:
    ///
    /// - [`Numbering::Abs`] for absolute numbering.
    /// - [`Numbering::Rel`] for relative to the main line.
    /// - [`Numbering::RelAbs`] for relative on every line other than the main line.
    pub numbering: Numbering = Numbering::Abs,
    /// Where to align the numbers, [`Alignment::Left`] by default
    pub align: Alignment = Alignment::Left,
    /// Where to align main line number, [`Alignment::Left`] by default
    pub main_align: Alignment = Alignment::Right,
    /// Wether to show wrapped line's numbers, `false` by default
    pub show_wraps: bool = false,
    /// Place this [`Widget`] on the right, `false` by default
    pub on_the_right: bool = false,
}

impl<U: Ui> LineNumbers<U> {
    /// Returns a [`LineNumbersBuilder`], used to create a new
    /// `LineNumbers`
    pub fn builder() -> LineNumbersBuilder<U> {
        LineNumbersBuilder::default()
    }

    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&self, pa: &Pass) -> f32 {
        let len = self.handle.read(pa).text().len().line();
        len.ilog10() as f32
    }

    fn form_text(&self, pa: &Pass) -> Text {
        let (main_line, printed_lines) = {
            let file = self.handle.read(pa);
            let main_line = if file.selections().is_empty() {
                usize::MAX
            } else {
                file.selections().get_main().unwrap().line()
            };

            (main_line, file.printed_lines().to_vec())
        };

        let mut builder = Text::builder();
        align(&mut builder, self.align);

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            if *line == main_line {
                align(&mut builder, self.main_align);
            }

            match (*line == main_line, is_wrapped) {
                (false, false) => {}
                (true, false) => {
                    let id = form::id_of!("linenum.main");
                    builder.push(id)
                }
                (false, true) => builder.push(form::id_of!("linenum.wrapped")),
                (true, true) => builder.push(form::id_of!("linenum.wrapped.main")),
            }

            let is_wrapped = *is_wrapped && index > 0;
            push_text(&mut builder, *line, main_line, is_wrapped, self);

            if *line == main_line {
                align(&mut builder, self.align);
            }
        }

        builder.build()
    }
}

impl<U: Ui> Widget<U> for LineNumbers<U> {
    fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
        let width = handle.read(pa).calculate_width(pa);
        handle.area(pa).set_width(width + 1.0).unwrap();

        handle.write(pa).text = handle.read(pa).form_text(pa);
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.handle.has_changed()
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("linenum.main", Form::yellow());
        form::set_weak("linenum.wrapped", Form::cyan().italic());
        form::set_weak("linenum.wrapped.main", "linenum.wrapped");
        Ok(())
    }
}

/// [`WidgetCfg`] for the [`LineNumbers`] widget
///
/// Contains a [`LineNumbersCfg`], which, unlike
/// [`LineNumbersCfg`], is modified by the `&mut` version of the
/// builder pattern.
#[derive(Default, Clone, Copy, Debug)]
#[doc(hidden)]
pub struct LineNumbersBuilder<U: Ui> {
    /// The numbering of lines, [`Numbering::Abs`] by default
    ///
    /// Can be:
    ///
    /// - [`Numbering::Abs`] for absolute numbering.
    /// - [`Numbering::Rel`] for relative to the main line.
    /// - [`Numbering::RelAbs`] for relative on every line other than the main line.
    pub numbering: Numbering = Numbering::Abs,
    /// Where to align the numbers, [`Alignment::Left`] by default
    pub align: Alignment = Alignment::Left,
    /// Where to align main line number, [`Alignment::Left`] by default
    pub main_align: Alignment = Alignment::Right,
    /// Wether to show wrapped line's numbers, `false` by default
    pub show_wraps: bool = false,
    /// Place this [`Widget`] on the right, `false` by default
    pub on_the_right: bool = false,
    _ghost: PhantomData<U> = PhantomData,
}

impl<U: Ui> LineNumbersBuilder<U> {
    pub fn push_on(self, pa: &mut Pass, handle: &Handle<File<U>, U>) -> Handle<LineNumbers<U>, U> {
        let mut line_numbers = LineNumbers {
            handle: handle.clone(),
            text: Text::default(),
            numbering: self.numbering,
            align: self.align,
            main_align: self.main_align,
            show_wraps: self.show_wraps,
            on_the_right: self.on_the_right,
        };
        line_numbers.text = line_numbers.form_text(pa);

        let specs = PushSpecs {
            side: if self.on_the_right {
                Side::Right
            } else {
                Side::Left
            },
            ..
        };

        handle.push_outer_widget(pa, line_numbers, specs)
    }
}

/// Writes the text of the line number to a given [`String`].
fn push_text<U: Ui>(
    b: &mut Builder,
    line: usize,
    main: usize,
    is_wrapped: bool,
    cfg: &LineNumbers<U>,
) {
    if (!is_wrapped || cfg.show_wraps) && main != usize::MAX {
        let num = match cfg.numbering {
            Numbering::Abs => line + 1,
            Numbering::Rel => line.abs_diff(main),
            Numbering::RelAbs => {
                if line != main {
                    line.abs_diff(main)
                } else {
                    line + 1
                }
            }
        };
        b.push(num);
    }

    b.push("\n");
    b.push(form::DEFAULT_ID);
}

fn align(b: &mut Builder, alignment: Alignment) {
    match alignment {
        Alignment::Left => b.push(AlignLeft),
        Alignment::Center => b.push(AlignCenter),
        Alignment::Right => b.push(AlignRight),
    }
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Clone, Copy)]
pub enum Numbering {
    #[default]
    /// Line numbers relative to the beginning of the file.
    Abs,
    /// Line numbers relative to the main selection's line, including
    /// that line.
    Rel,
    /// Relative line numbers on every line, except the main
    /// selection's.
    RelAbs,
}
