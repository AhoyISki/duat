use duat_core::{
    context::{self, Handle, Logs, Record},
    data::Pass,
    opts::PrintOpts,
    text::{Text, txt},
    ui::{PushSpecs, PushTarget, Side, Widget},
};

/// A [`Widget`] to display [`Logs`] sent to Duat
pub struct LogBook {
    logs: Logs,
    len_of_taken: usize,
    text: Text,
    fmt: Box<dyn FnMut(Record) -> Option<Text> + Send>,
    has_updated_once: bool,
    /// Wether to close this [`Widget`] after unfocusing, `true` by
    /// default
    pub close_on_unfocus: bool,
}

impl LogBook {
    /// Reformats this `LogBook`
    pub fn fmt(&mut self, fmt: impl FnMut(Record) -> Option<Text> + Send + 'static) {
        self.fmt = Box::new(fmt)
    }

    /// Returns a [`LogBookBuilder`], so you can push `LogBook`s
    /// around
    pub fn builder() -> LogBookOpts {
        LogBookOpts::default()
    }
}

impl Widget for LogBook {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let (lb, area) = handle.write_with_area(pa);

        let Some(new_records) = lb.logs.get(lb.len_of_taken..) else {
            return;
        };

        let records_were_added = !new_records.is_empty();
        lb.len_of_taken += new_records.len();

        for rec_text in new_records.into_iter().filter_map(&mut lb.fmt) {
            lb.text.insert_text(lb.text.len(), rec_text);
        }

		if area.width() > 0.0 && area.height() > 0.0 {
            if !lb.has_updated_once {
                area.scroll_ver(&lb.text, i32::MAX, lb.get_print_opts());
                lb.has_updated_once = true;
            } else if records_were_added {
                area.scroll_ver(&lb.text, i32::MAX, lb.get_print_opts());
            }
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

    fn get_print_opts(&self) -> PrintOpts {
        let mut opts = PrintOpts::new();
        opts.dont_wrap = false;
        opts.wrap_on_word = true;
        opts
    }

    fn on_focus(pa: &mut Pass, handle: &Handle<Self>) {
        handle.area().reveal(pa).unwrap();
    }

    fn on_unfocus(pa: &mut Pass, handle: &Handle<Self>) {
        if handle.read(pa).close_on_unfocus {
            handle.area().hide(pa).unwrap()
        }
    }
}

/// Configuration for the [`LogBook`]
pub struct LogBookOpts {
    fmt: Box<dyn FnMut(Record) -> Option<Text> + Send>,
    /// Wether to close the `LogBook` when unfocusing
    pub close_on_unfocus: bool = true,
    /// Wether to hide the `LogBook` by default
    pub hidden: bool = true,
    /// To which side to push the [`LogBook`] to
    pub side: Side = Side::Right,
    /// Requested height for the [`LogBook`], ignored if pushing horizontally
    pub height: f32 = 8.0,
    /// Requested width for the [`LogBook`], ignored if pushing vertically
    pub width: f32 = 50.0,
}

impl LogBookOpts {
    /// Push a [`LogBook`] around the given [`PushTarget`]
    pub fn push_on(mut self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<LogBook> {
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
            has_updated_once: false,
            fmt: self.fmt,
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

        push_target.push_outer(pa, log_book, specs)
    }

    /// Changes the way [`Record`]s are formatted by the [`LogBook`]
    ///
    /// This function returns an [`Option<Text>`], which means you can
    /// filter out unnecessary [`Record`]s. By default, all valid
    /// [`Record`]s (those with level [`Debug`] or higher.
    ///
    /// [`Debug`]: context::Level::Debug
    pub fn fmt(self, fmt: impl FnMut(Record) -> Option<Text> + Send + 'static) -> Self {
        Self { fmt: Box::new(fmt), ..self }
    }
}

impl Default for LogBookOpts {
    fn default() -> Self {
        fn default_fmt(rec: Record) -> Option<Text> {
            use duat_core::context::Level::*;
            let mut builder = match rec.level() {
                Error => txt!("[log_book.error][[ERROR]][log_book.colon]:  "),
                Warn => txt!("[log_book.warn][[WARNING]][log_book.colon]: "),
                Info => txt!("[log_book.info][[INFO]][log_book.colon]:   "),
                Debug => txt!("[log_book.debug][[DEBUG]][log_book.colon]:  "),
                Trace => unreachable!("Trace is not meant to be useable"),
            };

            builder.push(txt!(
                "[log_book.bracket]([log_book.target][log_book.bracket])[] {}",
                rec.text().clone()
            ));

            Some(builder.build())
        }

        Self { fmt: Box::new(default_fmt), .. }
    }
}
