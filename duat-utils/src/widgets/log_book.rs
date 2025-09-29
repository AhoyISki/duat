use std::marker::PhantomData;

use duat_core::{
    context::{Logs, Record},
    prelude::*,
    ui::Side,
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
            area.scroll_to_points(&lb.text, lb.text.len(), Widget::<U>::get_print_cfg(lb));
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
        form::set_weak("default.LogBook", Form::on_dark_grey());
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

    fn get_print_cfg(&self) -> PrintCfg {
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
pub struct LogBookCfg<F: FnMut(Record) -> Option<Text> + Send> {
    pub fmt: F = default_fmt,
    pub close_on_unfocus: bool = true,
    pub hidden: bool = true,
    pub side: Side = Side::Right,
    pub height: f32 = 8.0,
    pub width: f32 = 50.0,
}

impl LogBookCfg {
    fn push_on<W: Widget<U>, U: Ui>(
        mut self,
        pa: &mut Pass,
        handle: &Handle<W, U>,
    ) -> Handle<LogBook, U> {
        let logs = context::logs();

        let mut text = Text::new();

        let records = logs.get(..).unwrap();
        let len_of_taken = records.len();
        for rec_text in records.into_iter().filter_map(&mut self.fmt) {
            text.insert_text(text.len(), rec_text);
        }

        let log_book = LogBook {
            logs,
            len_of_taken,
            text,
            format_rec: self.fmt,
            close_on_unfocus: self.close_on_unfocus,
        };
        let specs = match self.side {
            Side::Right | Side::Left => PushSpecs {
                side: self.side,
                width: Some(self.width),
                hidden: self.hidden,
                ..
            },
            Side::Above | Side::Below => PushSpecs {
                side: self.side,
                height: Some(self.height),
                hidden: self.hidden,
                ..
            },
        };

        handle.push_widget(pa, log_book, specs)
    }
}

fn default_fmt(rec: Record) -> Text {
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
}
