//! Widget to display the history of notifications sent to Duat
//!
//! This widget is kind of an extended version of the
//! [`Notifications`] widget. It is normally placed at the bottom of
//! the screen, but you can also place it somewhere else. The messages
//! can be formatted differently, and you can also filter the things
//! that you don't care about.
//!
//! [`Notifications`]: super::Notifications
use duat_core::{
    context::{self, Handle, Logs, Record},
    data::Pass,
    mode::{MouseEvent, MouseEventKind},
    opts::PrintOpts,
    text::{Spacer, Text, txt},
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

    /// Returns a [`LogBookOpts`], so you can push `LogBook`s around
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
            lb.text.insert_text(lb.text.len(), &rec_text);
        }

        if !lb.has_updated_once {
            area.scroll_ver(&lb.text, i32::MAX, lb.get_print_opts());
            lb.has_updated_once = true;
        } else if records_were_added {
            area.scroll_ver(&lb.text, i32::MAX, lb.get_print_opts());
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
        opts.wrap_lines = true;
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

    fn on_mouse_event(pa: &mut Pass, handle: &Handle<Self>, event: MouseEvent) {
        match event.kind {
            MouseEventKind::ScrollDown | MouseEventKind::ScrollUp => {
                let (lb, area) = handle.write_with_area(pa);
                let scroll = if let MouseEventKind::ScrollDown = event.kind {
                    3
                } else {
                    -3
                };

                area.scroll_ver(&lb.text, scroll, lb.get_print_opts());
            }
            _ => {}
        }
    }
}

/// Configuration for the [`LogBook`]
pub struct LogBookOpts {
    fmt: Box<dyn FnMut(Record) -> Option<Text> + Send>,
    /// Wether to close the `LogBook` when unfocusing
    pub close_on_unfocus: bool,
    /// Wether to hide the `LogBook` by default
    pub hidden: bool,
    /// To which side to push the [`LogBook`] to
    pub side: Side,
    /// Requested height for the [`LogBook`], ignored if pushing
    /// horizontally
    pub height: f32,
    /// Requested width for the [`LogBook`], ignored if pushing
    /// vertically
    pub width: f32,
}

impl LogBookOpts {
    /// Push a [`LogBook`] around the given [`PushTarget`]
    pub fn push_on(mut self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<LogBook> {
        let logs = context::logs();

        let mut text = Text::new();

        let records = logs.get(..).unwrap();
        let len_of_taken = records.len();
        for rec_text in records.into_iter().filter_map(&mut self.fmt) {
            text.insert_text(text.len(), &rec_text);
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
                ..Default::default()
            },
            Side::Above | Side::Below => PushSpecs {
                side: self.side,
                height: Some(self.height),
                hidden: self.hidden,
                ..Default::default()
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
    pub fn fmt(&mut self, fmt: impl FnMut(Record) -> Option<Text> + Send + 'static) {
        self.fmt = Box::new(fmt);
    }
}

impl Default for LogBookOpts {
    fn default() -> Self {
        fn default_fmt(rec: Record) -> Option<Text> {
            use duat_core::context::Level::*;
            let mut builder = Text::builder();

            match rec.level() {
                Error => builder.push(txt!("[log_book.error][[ERROR]][log_book.colon]:  ")),
                Warn => builder.push(txt!("[log_book.warn][[WARNING]][log_book.colon]:")),
                Info => builder.push(txt!("[log_book.info][[INFO]][log_book.colon]:   ")),
                Debug => builder.push(txt!("[log_book.debug][[DEBUG]][log_book.colon]:  ")),
                Trace => unreachable!("Trace is not meant to be useable"),
            };

            builder.push(txt!(
                "[log_book.bracket][] {}{Spacer}([log_book.location]{}[log_book.bracket])\n",
                rec.text().clone(),
                rec.location(),
            ));

            Some(builder.build())
        }

        Self {
            fmt: Box::new(default_fmt),
            close_on_unfocus: true,
            hidden: true,
            side: Side::Below,
            height: 8.0,
            width: 50.0,
        }
    }
}
