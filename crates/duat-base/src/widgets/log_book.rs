//! Widget to display the history of notifications sent to Duat
//!
//! This widget is kind of an extended version of the
//! [`Notifications`] widget. It is normally placed at the bottom of
//! the screen, but you can also place it somewhere else. The messages
//! can be formatted differently, and you can also filter the things
//! that you don't care about.
//!
//! [`Notifications`]: super::Notifications
use std::{path::Path, sync::Mutex};

use duat_core::{
    cmd,
    context::{self, Handle, Location, Record},
    data::Pass,
    hook::{self, FocusedOn, MsgLogged, OnMouseEvent, UnfocusedFrom},
    mode::{self, MouseButton, TwoPointsPlace},
    opts::PrintOpts,
    text::{Point, SpawnTag, Tagger, Text, TextMut, txt},
    ui::{DynSpawnSpecs, Orientation, PushSpecs, PushTarget, Side, Widget},
};

use crate::widgets::Info;

pub fn add_logbook_hooks() {
    use duat_core::mode::MouseEventKind::*;
    hook::add::<MsgLogged>(|pa, rec| {
        let Some(logbook) = context::handle_of::<LogBook>(pa) else {
            return;
        };

        let (lb, area) = logbook.write_with_area(pa);

        let mut fmt_recs = |fmt: &mut dyn FnMut(Record) -> Option<Text>| {
            if let Some(rec_text) = fmt(rec.clone()) {
                lb.text.insert_text(lb.text.len(), &rec_text);
                lb.location_ranges
                    .push((lb.text.last_point(), rec.location()));
            }
        };

        let mut global_fmt = GLOBAL_FMT.lock().unwrap();

        if let Some(fmt) = lb.fmt.as_mut() {
            fmt_recs(fmt);
        } else if let Some(fmt) = global_fmt.as_mut() {
            fmt_recs(fmt);
        } else {
            fmt_recs(&mut default_fmt);
        }

        area.scroll_ver(&lb.text, i32::MAX, lb.print_opts());
    });

    hook::add::<FocusedOn<LogBook>>(|pa, (_, logbook)| logbook.area().reveal(pa).unwrap());

    hook::add::<UnfocusedFrom<LogBook>>(|pa, (logbook, _)| {
        if logbook.read(pa).close_on_unfocus {
            logbook.area().hide(pa).unwrap()
        }
    });

    let location_tagger = Tagger::new();

    hook::add::<OnMouseEvent<LogBook>>(move |pa, (logbook, event)| match event.kind {
        ScrollDown | ScrollUp => {
            let (lb, area) = logbook.write_with_area(pa);
            let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
            area.scroll_ver(&lb.text, scroll, lb.print_opts());
        }
        Moved => {
            let Some(TwoPointsPlace::Within(points)) = event.points else {
                return;
            };

            let lb = logbook.write(pa);
            let (Ok(i) | Err(i)) = lb
                .location_ranges
                .binary_search_by(|(end, _)| end.cmp(&points.real));

            if let Some((_, location)) = lb.location_ranges.get(i) {
                let spawn = SpawnTag::new(
                    Info::new(txt!("[log_book.location]{location}")),
                    DynSpawnSpecs {
                        orientation: Orientation::VerLeftBelow,
                        ..DynSpawnSpecs::default()
                    },
                );
                lb.text.insert_tag(location_tagger, points.real, spawn);
            }
        }
        Down(MouseButton::Left) => {
            let Some(TwoPointsPlace::Within(points)) = event.points else {
                return;
            };

            let lb = logbook.read(pa);
            let (Ok(i) | Err(i)) = lb
                .location_ranges
                .binary_search_by(|(end, _)| end.cmp(&points.real));

            if let Some((_, location)) = lb.location_ranges.get(i).cloned()
                && cmd::call(pa, format!("edit {}", location.file())).is_ok()
            {
                let buffer = context::get_buffer_by_path(pa, Path::new(location.file())).unwrap();
                mode::reset_to(pa, &buffer);
                buffer.selections_mut(pa).remove_extras();
                buffer.edit_main(pa, |mut c| {
                    c.move_to_coords(location.line() - 1, location.column() - 1);
                });
            }
        }
        _ => {}
    });

    hook::add::<OnMouseEvent>(move |pa, _| {
        for logbook in context::windows().handles_of::<LogBook>(pa) {
            logbook.text_mut(pa).remove_tags(location_tagger, ..);
        }
    });
}

#[allow(clippy::type_complexity)]
static GLOBAL_FMT: Mutex<Option<Box<dyn FnMut(Record) -> Option<Text> + Send>>> = Mutex::new(None);

/// A [`Widget`] to display [`Logs`] sent to Duat
///
/// [`Logs`]: duat_core::context::Logs
pub struct LogBook {
    text: Text,
    location_ranges: Vec<(Point, Location)>,
    fmt: Option<Box<dyn FnMut(Record) -> Option<Text> + Send>>,
    /// Wether to close this [`Widget`] after unfocusing, `true` by
    /// default
    pub close_on_unfocus: bool,
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
        let records = logs.get(..).unwrap();

        let mut global_fmt = GLOBAL_FMT.lock().unwrap();
        let mut text = Text::new();
        let mut path_ranges = Vec::new();

        let fmt_recs = |fmt: &mut dyn FnMut(Record) -> Option<Text>| {
            for rec in records.into_iter() {
                if let Some(rec_text) = fmt(rec.clone()) {
                    text.insert_text(text.len(), &rec_text);
                    path_ranges.push((text.last_point(), rec.location()));
                }
            }
        };

        if let Some(fmt) = global_fmt.as_mut() {
            fmt_recs(fmt);
        } else {
            fmt_recs(&mut default_fmt);
        }

        let log_book = LogBook {
            text,
            location_ranges: path_ranges,
            fmt: None,
            close_on_unfocus: self.close_on_unfocus,
        };

        let specs = match self.side {
            Side::Right | Side::Left => PushSpecs {
                side: self.side,
                width: Some(self.width),
                hidden: self.hidden,
                cluster: false,
                ..Default::default()
            },
            Side::Above | Side::Below => PushSpecs {
                side: self.side,
                height: Some(self.height),
                hidden: self.hidden,
                cluster: false,
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

    builder.push(txt!(" {}", rec.text().clone(),));

    builder.push('\n');

    Some(builder.build())
}
