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

use duat_core::{prelude::*, text::Builder, ui::Constraint};

/// Shows a column of line numbers beside the [`File`]
///
/// This can be configured through [`LineNumbers::cfg`], in order to
/// get, for example: relative numbering, different alignment,
/// hidden/shown wrapped lines, etc.
pub struct LineNumbers<U: Ui> {
    handle: FileHandle<U>,
    text: Text,
    /// The options of these [`LineNumbers`]
    pub opts: LineNumbersOptions<U>,
}

impl<U: Ui> LineNumbers<U> {
    /// The minimum width that would be needed to show the last line.
    fn calculate_width(&self, pa: &Pass) -> f32 {
        let len = self.handle.read(pa, |file, _| file.text().len().line());
        len.ilog10() as f32
    }

    fn form_text(&self, pa: &Pass) -> Text {
        let (main_line, printed_lines) = self.handle.read(pa, |file, _| {
            let main_line = if file.selections().is_empty() {
                usize::MAX
            } else {
                file.selections().get_main().unwrap().line()
            };

            (main_line, file.printed_lines().to_vec())
        });

        let mut builder = Text::builder();
        align(&mut builder, self.opts.align);

        for (index, (line, is_wrapped)) in printed_lines.iter().enumerate() {
            if *line == main_line {
                align(&mut builder, self.opts.main_align);
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
            push_text(&mut builder, *line, main_line, is_wrapped, &self.opts);

            if *line == main_line {
                align(&mut builder, self.opts.align);
            }
        }

        builder.build()
    }

    /// The options for these [`LineNumbers`]
    pub fn options(&self) -> &LineNumbersOptions<U> {
        &self.opts
    }

    /// The mutable options for these [`LineNumbers`]
    pub fn options_mut(&mut self) -> &mut LineNumbersOptions<U> {
        &mut self.opts
    }
}

impl<U: Ui> Widget<U> for LineNumbers<U> {
    type Cfg = LineNumbersOptions<U>;

    fn update(pa: &mut Pass, handle: Handle<Self, U>) {
        let width = handle.read(pa, |ln, _| ln.calculate_width(pa));
        handle
            .area()
            .constrain_hor([Constraint::Len(width + 1.0)])
            .unwrap();

        let text = handle.read(pa, |ln, _| ln.form_text(pa));
        handle.write(pa, |ln, _| ln.text = text);
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
        form::set_weak("linenum.main", Form::yellow());
        form::set_weak("linenum.wrapped", Form::cyan().italic());
        form::set_weak("linenum.wrapped.main", "linenum.wrapped");
        Ok(())
    }
}

/// Configuration options for the [`LineNumbers`] widget.
#[derive(Debug, Clone, Copy)]
#[doc(hidden)]
pub struct LineNumbersOptions<U> {
    num_rel: LineNum = LineNum::Abs,
    align: Alignment = Alignment::Left,
    main_align: Alignment = Alignment::Right,
    show_wraps: bool = false,
    specs: PushSpecs = PushSpecs::left(),
    _ghost: PhantomData<U> = PhantomData,
}

impl<U> LineNumbersOptions<U> {
    /// Absolute numbering, first line is 1, second is 2, etc
    pub fn absolute(self) -> Self {
        Self { num_rel: LineNum::Abs, ..self }
    }

    /// Relative numbering, cursor line is 0, surrounding is 1, etc
    pub fn relative(self) -> Self {
        Self { num_rel: LineNum::Rel, ..self }
    }

    /// A mix between [`relative`] and [`absolute`] numbering
    ///
    /// Will show the line number of the main cursor's line, while
    /// showing the distance to it on every other line.
    ///
    /// [`relative`]: Self::relative
    /// [`absolute`]: Self::absolute
    pub fn rel_abs(self) -> Self {
        Self { num_rel: LineNum::RelAbs, ..self }
    }

    /// Aligns _all_ numbers left
    ///
    /// If you want the main line's number to be aligned differently,
    /// call one of the [`align_main_*`] functions _after_ this one.
    ///
    /// [`align_main_*`]: Self::align_main_right
    pub fn align_left(self) -> Self {
        Self {
            main_align: Alignment::Left,
            align: Alignment::Left,
            ..self
        }
    }

    /// Aligns _all_ numbers to the center
    ///
    /// If you want the main line's number to be aligned differently,
    /// call one of the [`align_main_*`] functions _after_ this one.
    ///
    /// [`align_main_*`]: Self::align_main_right
    pub fn align_center(self) -> Self {
        Self {
            main_align: Alignment::Center,
            align: Alignment::Center,
            ..self
        }
    }

    /// Aligns _all_ numbers right
    ///
    /// If you want the main line's number to be aligned differently,
    /// call one of the [`align_main_*`] functions _after_ this one.
    ///
    /// [`align_main_*`]: Self::align_main_left
    pub fn align_right(self) -> Self {
        Self {
            main_align: Alignment::Right,
            align: Alignment::Right,
            ..self
        }
    }

    /// Aligns onle the main line's number to the left
    pub fn align_main_left(self) -> Self {
        Self { main_align: Alignment::Left, ..self }
    }

    /// Aligns onle the main line's number to the center
    pub fn align_main_center(self) -> Self {
        Self { main_align: Alignment::Center, ..self }
    }

    /// Aligns onle the main line's number to the right
    pub fn align_main_right(self) -> Self {
        Self { main_align: Alignment::Right, ..self }
    }

    /// Shows wrapping lines, is `false` by default
    pub fn show_wraps(self) -> Self {
        Self { show_wraps: true, ..self }
    }

    /// Hides wrapping lines, is `true` by default
    pub fn hide_wraps(self) -> Self {
        Self { show_wraps: false, ..self }
    }

    /// Place the [`LineNumbers`] on the right
    ///
    /// Do note that this has no effect if done at runtime.
    pub fn on_the_right(self) -> Self {
        Self { specs: self.specs.to_right(), ..self }
    }
}

impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
    type Widget = LineNumbers<U>;

    fn build(self, pa: &mut Pass, handle: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let Some(handle) = handle else {
            panic!("For now, you can't push LineNumbers to something that is not a File");
        };
        let specs = self.specs;

        let mut widget = LineNumbers {
            handle,
            text: Text::default(),
            opts: self,
        };
        widget.text = widget.form_text(pa);

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
    if (!is_wrapped || cfg.show_wraps) && main != usize::MAX {
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
enum LineNum {
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
