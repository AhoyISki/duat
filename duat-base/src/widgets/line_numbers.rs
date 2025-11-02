//! Line numbers for a [`Buffer`]
//!
//! These are pretty standard like in most text editors. Usually,
//! they'll be printed on the right of the [`Buffer`], but there is an
//! option to print them on the right, if you need such functionality.
//!
//! You can also change other things, like the
//! relativeness/absoluteness of the numbers, as well as the alignment
//! of the numbers, with one more option to change that of the main
//! selection's line number.
//!
//! [`Buffer`]: duat_core::buffer::Buffer
use std::fmt::Alignment;

use duat_core::{
    context::Handle,
    data::Pass,
    form,
    text::{AlignCenter, AlignLeft, AlignRight, Builder, Text},
    ui::{PushSpecs, Side, Widget},
};

/// Shows a column of line numbers beside the [`Buffer`]
/// 
/// There are various fields that you can use to configure how the
/// `LineNumbers` will be displayed. They control things like the
/// line numbers and the relativeness of the number displayed.
/// 
/// This is a default struct of Duat, that is, it is automatically
/// placed around every `Buffer`, but you can disable that behavior
/// by [removing] the `"BufferWidgets"` hook.
///
/// [`Buffer`]: duat_core::buffer::Buffer
/// [removing]: duat_core::hook::remove
pub struct LineNumbers {
    buffer: Handle,
    text: Text,
    /// Wether to show relative numbering
    ///
    /// The default is `false`
    pub relative: bool = false,
    /// Where to align the numbers
    ///
    /// The default is [`Alignment::Left`]
    pub align: Alignment = Alignment::Left,
    /// Where to align the main line number
    ///
    /// The default is [`Alignment::Right`]
    pub main_align: Alignment = Alignment::Right,
    /// Wether to show wrapped line's numbers
    ///
    /// The default is `false`
    pub show_wraps: bool = false,
}

impl LineNumbers {
    /// Returns a [`LineNumbersOpts`], used to create a new
    /// `LineNumbers`
    pub fn builder() -> LineNumbersOpts {
        LineNumbersOpts::default()
    }

    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&self, pa: &Pass) -> f32 {
        let len = self.buffer.read(pa).text().len().line();
        len.ilog10() as f32
    }

    fn form_text(&self, pa: &Pass) -> Text {
        let (main_line, printed_lines) = {
            let buffer = self.buffer.read(pa);
            let main_line = if buffer.selections().is_empty() {
                usize::MAX
            } else {
                buffer.selections().get_main().unwrap().line()
            };

            (main_line, buffer.printed_lines().to_vec())
        };

        let mut builder = Text::builder();
        align(&mut builder, self.align);

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            if *line == main_line {
                align(&mut builder, self.main_align);
            }

            match (*line == main_line, is_wrapped) {
                (false, false) => {}
                (true, false) => builder.push(form::id_of!("linenum.main")),
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

impl Widget for LineNumbers {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let width = handle.read(pa).calculate_width(pa);
        handle.area().set_width(pa, width + 1.0).unwrap();

        handle.write(pa).text = handle.read(pa).form_text(pa);
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        self.buffer.has_changed(pa)
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}

/// Options for cosntructing a [`LineNumbers`] [`Widget`]
///
/// For most options, you can just set them in the `Widget`
/// directly (through a [hook] or something). Right now, the
/// only option exclusive to this struct is the [`on_the_right`]
/// option, which places the `LineNumbers` on the right, as
/// opposed to on the left.
///
/// [`on_the_right`]: Self::on_the_right
/// [hook]: duat_core::hook
#[derive(Default, Clone, Copy, Debug)]
pub struct LineNumbersOpts {
    /// Wether to show relative numbering
    ///
    /// The default is `false`
    pub relative: bool = false,
    /// Where to align the numbers
    ///
    /// The default is [`Alignment::Left`]
    pub align: Alignment = Alignment::Left,
    /// Where to align the main line number
    ///
    /// The default is [`Alignment::Right`]
    pub main_align: Alignment = Alignment::Right,
    /// Wether to show wrapped line's numbers
    ///
    /// The default is `false`
    pub show_wraps: bool = false,
    /// Place this [`Widget`] on the right, as opposed to on the left
    ///
    /// The default is `false`
    pub on_the_right: bool = false,
}

impl LineNumbersOpts {
    /// Push the [`LineNumbers`] to a [`Handle`]
    pub fn push_on(self, pa: &mut Pass, handle: &Handle) -> Handle<LineNumbers> {
        let mut line_numbers = LineNumbers {
            buffer: handle.clone(),
            text: Text::default(),
            relative: self.relative,
            align: self.align,
            main_align: self.main_align,
            show_wraps: self.show_wraps,
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
fn push_text(b: &mut Builder, line: usize, main: usize, is_wrapped: bool, opts: &LineNumbers) {
    if (!is_wrapped || opts.show_wraps) && main != usize::MAX {
        let num = if opts.relative {
            if line != main {
                line.abs_diff(main)
            } else {
                line + 1
            }
        } else {
            line + 1
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
