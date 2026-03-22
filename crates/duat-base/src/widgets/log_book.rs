//! Widget to display the history of notifications sent to Duat
//!
//! This widget is kind of an extended version of the
//! [`Notifications`] widget. It is normally placed at the bottom of
//! the screen, but you can also place it somewhere else. The messages
//! can be formatted differently, and you can also filter the things
//! that you don't care about.
//!
//! [`Notifications`]: super::Notifications
use std::sync::Mutex;

use duat_core::{
    context::{self, Handle, Record},
    data::Pass,
    hook::{self, FocusedOn, MsgLogged, OnMouseEvent, UnfocusedFrom},
    opts::PrintOpts,
    text::{Spacer, Text, TextMut, txt},
    ui::{PushSpecs, PushTarget, Side, Widget},
};

pub fn add_logbook_hooks() {
    use duat_core::mode::MouseEventKind::*;
    hook::add::<MsgLogged>(|pa, rec| {
        let Some(logbook) = context::handle_of::<LogBook>(pa) else {
            return;
        };

        let (lb, area) = logbook.write_with_area(pa);

        let fmt_recs = |fmt: &mut dyn FnMut(Record) -> Option<Text>| {
            if let Some(rec_text) = fmt(rec) {
                lb.text.insert_text(lb.text.len(), &rec_text);
            }
        };

        let mut global_fmt = GLOBAL_FMT.lock().unwrap();

        if let Some(fmt) = lb.fmt.as_mut() {
            fmt_recs(fmt);
        } else if let Some(fmt) = global_fmt.as_mut() {
            fmt_recs(fmt);
        } else {
            fmt_recs(&mut |rec| default_fmt(lb.show_source, rec));
        }

        area.scroll_ver(&lb.text, i32::MAX, lb.print_opts());
    });

    hook::add::<FocusedOn<LogBook>>(|pa, (_, logbook)| logbook.area().reveal(pa).unwrap());

    hook::add::<UnfocusedFrom<LogBook>>(|pa, (logbook, _)| {
        if logbook.read(pa).close_on_unfocus {
            logbook.area().hide(pa).unwrap()
        }
    });

    hook::add::<OnMouseEvent<LogBook>>(|pa, (logbook, event)| match event.kind {
        ScrollDown | ScrollUp => {
            let (lb, area) = logbook.write_with_area(pa);
            let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
            area.scroll_ver(&lb.text, scroll, lb.print_opts());
        }
        _ => {}
    });
}

#[allow(clippy::type_complexity)]
static GLOBAL_FMT: Mutex<Option<Box<dyn FnMut(Record) -> Option<Text> + Send>>> = Mutex::new(None);

/// A [`Widget`] to display [`Logs`] sent to Duat
pub struct LogBook {
    text: Text,
    fmt: Option<Box<dyn FnMut(Record) -> Option<Text> + Send>>,
    /// Wether to close this [`Widget`] after unfocusing, `true` by
    /// default
    pub close_on_unfocus: bool,
    /// Wether the source of a log should be shown
    ///
    /// Can be disabled for less noise. This option is ignored when
    /// there is [custom formatting].
    ///
    /// [custom formatting]: LogBookOpts::fmt
    pub show_source: bool,
}

impl LogBook {
    /// Reformats this `LogBook`
    pub fn fmt(&mut self, fmt: impl FnMut(Record) -> Option<Text> + Send + 'static) {
        self.fmt = Some(Box::new(fmt))
    }

    /// Returns a [`LogBookOpts`], so you can push `LogBook`s around
    pub fn builder() -> LogBookOpts {
        LogBookOpts::default()
    }
}

impl Widget for LogBook {
    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.text.as_mut()
    }

    fn print_opts(&self) -> PrintOpts {
        let mut opts = PrintOpts::new();
        opts.wrap_lines = true;
        opts.wrap_on_word = true;
        opts
    }
}

/// Configuration for the [`LogBook`]
#[derive(Clone, Copy)]
pub struct LogBookOpts {
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
    /// Wether the source of a log should be shown
    ///
    /// Can be disabled for less noise. This option is ignored when
    /// there is [custom formatting].
    ///
    /// [custom formatting]: Self::fmt
    pub show_source: bool,
}

impl LogBookOpts {
    /// Push a [`LogBook`] around the given [`PushTarget`]
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<LogBook> {
        let logs = context::logs();

        let mut text = Text::new();

        let records = logs.get(..).unwrap();

        let fmt_recs = |fmt: &mut dyn FnMut(Record) -> Option<Text>| {
            for rec_text in records.into_iter().filter_map(fmt) {
                text.insert_text(text.len(), &rec_text);
            }
        };

        let mut global_fmt = GLOBAL_FMT.lock().unwrap();

        if let Some(fmt) = global_fmt.as_mut() {
            fmt_recs(fmt);
        } else {
            fmt_recs(&mut |rec| default_fmt(self.show_source, rec));
        }

        let log_book = LogBook {
            text,
            fmt: None,
            show_source: self.show_source,
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
        *GLOBAL_FMT.lock().unwrap() = Some(Box::new(fmt));
    }
}

impl Default for LogBookOpts {
    fn default() -> Self {
        Self {
            close_on_unfocus: true,
            hidden: true,
            side: Side::Below,
            height: 8.0,
            width: 50.0,
            show_source: true,
        }
    }
}

fn default_fmt(show_source: bool, rec: Record) -> Option<Text> {
    use duat_core::context::Level::*;
    let mut builder = Text::builder();

    match rec.level() {
        Error => builder.push(txt!("[log_book.error][[ERROR]][log_book.colon]:  ")),
        Warn => builder.push(txt!("[log_book.warn][[WARNING]][log_book.colon]:")),
        Info => builder.push(txt!("[log_book.info][[INFO]][log_book.colon]:   ")),
        Debug => builder.push(txt!("[log_book.debug][[DEBUG]][log_book.colon]:  ")),
        Trace => unreachable!("Trace is not meant to be useable"),
    };

    builder.push(txt!("[log_book.bracket][] {}", rec.text().clone(),));

    if show_source && rec.level() != duat_core::context::Level::Info {
        builder.push(txt!(
            "{Spacer}([log_book.location]{}[log_book.bracket])",
            rec.location()
        ));
    }

    builder.push('\n');

    Some(builder.build())
}
