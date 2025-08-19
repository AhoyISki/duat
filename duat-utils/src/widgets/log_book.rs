use std::marker::PhantomData;

use duat_core::{
    context::{Logs, Record},
    prelude::*,
    ui::{
        Constraint::{self, Len, Ratio},
        Side,
    },
};

use crate::modes::Pager;

/// A [`Widget`] to display [`Logs`] sent to Duat
pub struct LogBook {
    logs: Logs,
    len_of_taken: usize,
    text: Text,
    format_rec: Box<dyn FnMut(Record) -> Option<Text> + Send>,
    close_on_unfocus: bool,
}

impl<U: Ui> Widget<U> for LogBook {
    type Cfg = LogBookCfg<U>;

    fn cfg() -> Self::Cfg {
        LogBookCfg {
            format_rec: Box::new(|rec| {
                use context::Level::*;
                let mut builder = match rec.level() {
                    Error => txt!("[log_book.error][[ERROR]][log_book.colon]:  "),
                    Warn => txt!("[log_book.warn][[WARNING]][log_book.colon]: "),
                    Info => txt!("[log_book.info][[INFO]][log_book.colon]:   "),
                    Debug => txt!("[log_book.debug][[DEBUG]][log_book.colon]:  "),
                    Trace => unreachable!("Trace is not meant to be useable"),
                };

                builder.push(txt!(
                    "[log_book.bracket]([log_book.target]{}[log_book.bracket])[] {}",
                    rec.metadata().target(),
                    rec.text().clone()
                ));

                Some(builder.build())
            }),
            close_on_unfocus: true,
            hidden: true,
            side: Side::Below,
            height: Ratio(1, 4),
            width: Ratio(2, 7),
            _ghost: PhantomData,
        }
    }

    fn update(pa: &mut Pass, handle: &Handle<Self, U>)
    where
        Self: Sized,
    {
        let (lb, area) = handle.write_with_area(pa);
        let Some(new_records) = lb.logs.get(lb.len_of_taken..) else {
            return;
        };

        let records_were_added = !new_records.is_empty();
        lb.len_of_taken += new_records.len();

        for rec_text in new_records.into_iter().filter_map(&mut lb.format_rec) {
            lb.text.insert_text(lb.text.len(), rec_text);
        }

        if records_were_added {
            area.scroll_to_points(&lb.text, lb.text.len(), Widget::<U>::print_cfg(lb));
        }
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.logs.has_changed()
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("log_book.error", "default.error");
        form::set_weak("log_book.warn", "default.warn");
        form::set_weak("log_book.info", "default.info");
        form::set_weak("log_book.debug", "default.debug");
        form::set_weak("log_book.colon", "prompt.colon");
        form::set_weak("log_book.bracket", "punctuation.bracket");
        form::set_weak("log_book.target", "module");

        cmd::add!("logs", |pa| {
            mode::set(Pager::<LogBook, U>::new());
            Ok(None)
        });

        Ok(())
    }

    fn print_cfg(&self) -> PrintCfg {
        *PrintCfg::new().wrap_on_word().set_scrolloff(0, 0)
    }

    fn on_focus(pa: &mut Pass, handle: &Handle<Self, U>) {
        handle.area(pa).reveal().unwrap();
    }

    fn on_unfocus(pa: &mut Pass, handle: &Handle<Self, U>) {
        if handle.read(pa).close_on_unfocus {
            handle.area(pa).hide().unwrap()
        }
    }
}

/// [`WidgetCfg`] for the [`LogBook`]
pub struct LogBookCfg<U> {
    format_rec: Box<dyn FnMut(Record) -> Option<Text> + Send>,
    close_on_unfocus: bool,
    hidden: bool,
    side: Side,
    height: Constraint,
    width: Constraint,
    _ghost: PhantomData<U>,
}

impl<U> LogBookCfg<U> {
    /// Have the [`LogBook`] be open by default
    pub fn open_by_default(self) -> Self {
        Self { hidden: false, ..self }
    }

    /// Keeps the [`LogBook`] open when unfocused, as opposed to
    /// hiding it
    pub fn keep_open_on_unfocus(self) -> Self {
        Self { close_on_unfocus: false, ..self }
    }

    /// Changes the way [`Record`]s are formatted by the [`LogBook`]
    ///
    /// This function returns an [`Option<Text>`], which means you can
    /// filter out unnecessary [`Record`]s. By default, all valid
    /// [`Record`]s (those with level [`Debug`] or higher.
    ///
    /// [`Debug`]: context::Level::Debug
    pub fn formatted(self, fmt: impl FnMut(Record) -> Option<Text> + Send + 'static) -> Self {
        Self { format_rec: Box::new(fmt), ..self }
    }

    /// Pushes the [`LogBook`] to the right, as opposed to below
    pub fn on_the_right(self) -> Self {
        Self { side: Side::Right, ..self }
    }

    /// Pushes the [`LogBook`] to the left, as opposed to below
    pub fn on_the_left(self) -> Self {
        Self { side: Side::Left, ..self }
    }

    /// Pushes the [`LogBook`] above, as opposed to below
    pub fn above(self) -> Self {
        Self { side: Side::Above, ..self }
    }

    /// Sets the height of the [`LogBook`]
    ///
    /// This is ignored if pushing to the left or to the right.
    pub fn height(self, height: usize) -> Self {
        Self { height: Len(height as f32), ..self }
    }

    /// Sets the width of the [`LogBook`]
    ///
    /// This is ignored if pushing above or below.
    pub fn width(self, width: usize) -> Self {
        Self { width: Len(width as f32), ..self }
    }

    /// Sets a ratio for the height of the [`LogBook`]
    ///
    /// This is ignored if pushing to the left or to the right.
    pub fn height_ratio(self, den: u16, div: u16) -> Self {
        Self { height: Ratio(den, div), ..self }
    }

    /// Sets a ratio for the witdh of the [`LogBook`]
    ///
    /// This is ignored if pushing above or below.
    pub fn width_ratio(self, den: u16, div: u16) -> Self {
        Self { width: Ratio(den, div), ..self }
    }
}

impl<U: Ui> WidgetCfg<U> for LogBookCfg<U> {
    type Widget = LogBook;

    fn build(mut self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
        let logs = context::logs();

        let mut text = Text::new();

        let records = logs.get(..).unwrap();
        let len_of_taken = records.len();
        for rec_text in records.into_iter().filter_map(&mut self.format_rec) {
            text.insert_text(text.len(), rec_text);
        }

        let lb = LogBook {
            logs,
            len_of_taken,
            text,
            format_rec: self.format_rec,
            close_on_unfocus: self.close_on_unfocus,
        };

        let specs = match self.side {
            Side::Right => PushSpecs::right().constrain_hor(self.width),
            Side::Left => PushSpecs::left().constrain_hor(self.width),
            Side::Above => PushSpecs::above().constrain_ver(self.height),
            Side::Below => PushSpecs::below().constrain_ver(self.height),
        };

        (lb, if self.hidden { specs.hidden() } else { specs })
    }
}
