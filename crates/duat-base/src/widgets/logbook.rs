//! Widget to display the history of notifications sent to Duat
//!
//! This widget is kind of an extended version of the
//! [`Notifications`] widget. It is normally placed at the bottom of
//! the screen, but you can also place it somewhere else. The messages
//! can be formatted differently, and you can also filter the things
//! that you don't care about.
//!
//! [`Notifications`]: super::Notifications
use std::{
    path::Path,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Ns, cmd,
    context::{self, Handle, Location, Record},
    data::Pass,
    hook::{self, MsgLogged, OnMouseEvent, WidgetClosed, WidgetSwitched},
    mode::{self, MouseButton, TwoPointsPlace},
    opts::PrintOpts,
    text::{Point, Spawn, Text, TextMut, txt},
    ui::{DynSpawnSpecs, Orientation, PushSpecs, PushTarget, Side, Widget, Window},
};

use crate::widgets::Info;

static GLOBAL_LOGS: LazyLock<Mutex<LogBook>> = LazyLock::new(|| {
    Mutex::new(LogBook {
        text: Text::default(),
        location_ranges: Vec::new(),
        close_on_unfocus: false,
    })
});

/// Initial setup for the [`LogBook`].
pub fn logbook_setup() {
    use duat_core::mode::MouseEventKind::*;
    hook::add::<MsgLogged>(|pa, rec| {
        let local = context::handle_of::<LogBook>(pa);
        let mut global_logs = GLOBAL_LOGS.lock().unwrap();
        let (lb, data) = if let Some(logbook) = &local {
            (logbook.write(pa), Some((logbook.widget(), logbook.area())))
        } else {
            (&mut *global_logs, None)
        };

        let mut fmt_rec = |fmt: &mut dyn FnMut(Record) -> Option<Text>| {
            if let Some(rec_text) = fmt(rec.clone()) {
                lb.text.append_text(lb.text.len(), &rec_text);
                lb.location_ranges
                    .push((lb.text.end_point(), rec.location()));
            }
        };

        let mut global_fmt = GLOBAL_FMT.lock().unwrap();

        if let Some(fmt) = global_fmt.as_mut() {
            fmt_rec(fmt);
        } else {
            fmt_rec(&mut default_fmt);
        }

        if let Some((logbook, area)) = data {
            let (lb, area) = pa.write_many((logbook, area));
            area.scroll_ver(&lb.text, i32::MAX, lb.print_opts());
        }
    });

    hook::add::<WidgetSwitched>(|pa, (former, current)| {
        let get_logbook = |window: Window| {
            window
                .handles(pa)
                .filter_map(|handle| handle.get_as::<LogBook>())
                .next()
        };
        let former = former.window(pa).and_then(get_logbook);
        let current = current.window(pa).and_then(get_logbook);

        match (former, current) {
            (Some(former), Some(current)) => {
                let (former, current) = pa.write_many((&former, &current));
                std::mem::swap(former, current);
            }
            (Some(other), None) | (None, Some(other)) => {
                let mut global = GLOBAL_LOGS.lock().unwrap();
                let other = other.write(pa);
                std::mem::swap(&mut *global, other);
            }
            (None, None) => {}
        }
    })
    .lateness(0);

    hook::add::<WidgetClosed<LogBook>>(|pa, logbook| {
        if !logbook.text(pa).is_empty() {
            let mut global = GLOBAL_LOGS.lock().unwrap();
            let closed = logbook.write(pa);
            std::mem::swap(&mut *global, closed);
        }
    });

    hook::add::<WidgetSwitched>(|pa, (old, new)| {
        if let Some(logbook) = new.get_as::<LogBook>() {
            logbook.area().reveal(pa).unwrap()
        } else if let Some(logbook) = old.get_as::<LogBook>()
            && logbook.read(pa).close_on_unfocus
        {
            logbook.area().hide(pa).unwrap()
        }
    });

    let location_ns = Ns::new();

    hook::add::<OnMouseEvent<LogBook>>(move |pa, event| match event.kind {
        ScrollDown | ScrollUp => {
            let (lb, area) = event.handle.write_with_area(pa);
            let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
            area.scroll_ver(&lb.text, scroll, lb.print_opts());
        }
        Moved => {
            let Some(TwoPointsPlace::Within(points)) = event.points else {
                return;
            };

            let lb = event.handle.write(pa);
            let (Ok(i) | Err(i)) = lb
                .location_ranges
                .binary_search_by(|(end, _)| end.cmp(&points.real));

            if let Some((_, location)) = lb.location_ranges.get(i) {
                let spawn = Spawn::new(
                    Info::new(txt!("[log_book.location]{location}")),
                    DynSpawnSpecs {
                        orientation: Orientation::VerLeftBelow,
                        ..DynSpawnSpecs::default()
                    },
                );
                lb.text.insert_tag(location_ns, points.real, spawn);
            }
        }
        Down(MouseButton::Left) => {
            let Some(TwoPointsPlace::Within(points)) = event.points else {
                return;
            };

            let lb = event.handle.read(pa);
            let (Ok(i) | Err(i)) = lb
                .location_ranges
                .binary_search_by(|(end, _)| end.cmp(&points.real));

            if let Some((_, location)) = lb.location_ranges.get(i).cloned()
                && cmd::call(pa, format!("edit {}", location.file())).is_ok()
            {
                let buffer = context::buffer_from_path(pa, Path::new(location.file())).unwrap();
                mode::reset_to(pa, &buffer);
                buffer.selections_mut(pa).remove_extras();
                buffer.edit_main(pa, |mut s| {
                    s.move_to_coords(location.line() - 1, location.column() - 1);
                });
            }
        }
        _ => {}
    });

    hook::add::<OnMouseEvent>(move |pa, _| {
        for logbook in context::windows().handles_of::<LogBook>(pa) {
            logbook.text_mut(pa).remove_tags(location_ns, ..);
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
    /// Wether to close this [`Widget`] after unfocusing, `true` by
    /// default
    pub close_on_unfocus: bool,
}

impl LogBook {
    /// Reformats all `LogBook`s.
    ///
    /// Note that you can't reformat just one of them. And this
    /// function will only be applied to future entries.
    pub fn fmt(fmt: impl FnMut(Record) -> Option<Text> + Send + 'static) {
        *GLOBAL_FMT.lock().unwrap() = Some(Box::new(fmt))
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
    /// Max proportion that it the [`LogBook`] is allowed to
    /// take of the screen, `hidden` is set to `false`.
    ///
    /// If it would be larger than that proportion, it won't be
    /// shown by default.
    pub max_proportion: f32,
}

impl LogBookOpts {
    /// Push a [`LogBook`] around the given [`PushTarget`]
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<LogBook> {
        let log_book = LogBook {
            text: Text::new(),
            location_ranges: Vec::new(),
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

        let logbook = push_target.push_outer(pa, log_book, specs);

        if let Some(other) = context::handle_of::<LogBook>(pa)
            && other == logbook
        {
            let mut global_logs = GLOBAL_LOGS.lock().unwrap();
            std::mem::swap(&mut *global_logs, logbook.write(pa));
        }

        logbook
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
            max_proportion: 0.5,
        }
    }
}

fn default_fmt(rec: Record) -> Option<Text> {
    use duat_core::context::Level::*;
    let mut builder = Text::builder();

    match rec.level() {
        Error => builder.push(txt!("[logbook.error][[ERROR]][logbook.colon]:  ")),
        Warn => builder.push(txt!("[logbook.warn][[WARNING]][logbook.colon]:")),
        Info => builder.push(txt!("[logbook.info][[INFO]][logbook.colon]:   ")),
        Debug => builder.push(txt!("[logbook.debug][[DEBUG]][logbook.colon]:  ")),
        Trace => unreachable!("Trace is not meant to be useable"),
    };

    builder.push(txt!(" {}", rec.text().clone(),));

    builder.push('\n');

    Some(builder.build())
}
