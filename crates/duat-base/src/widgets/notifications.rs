//! A [`Widget`] that shows notifications
//!
//! This is a very simple [`Widget`], and will usually be placed right
//! under a [`PromptLine`], which, when the `"HidePromptLine"` [hook]
//! group exists, will be hidden when the [`PromptLine`] is not in
//! focus, allowing for the [`Notifications`] widget to pop up.
//!
//! [`PromptLine`]: super::PromptLine
//! [hook]: hooks
use std::sync::{
    Mutex, Once,
    atomic::{AtomicBool, Ordering},
};

use duat_core::{
    context::{self, Handle, Level, Record},
    data::Pass,
    hook::{self, KeySent},
    text::{Text, TextMut},
    ui::{PushSpecs, PushTarget, Side, Widget},
};

/// A [`Widget`] to show notifications
///
/// You can style modify it using the [`opts::set_notifs`] function in
/// Duat:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::widgets::{Notifications, FooterWidgets};
/// # mod opts {
/// #     pub fn set_notifs(set_fn: impl FnMut(&mut duat_base::widgets::NotificationsOpts)) {}
/// # }
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     opts::set_notifs(|opts| {
///         opts.fmt(|rec| {
///             txt!(
///                 "[notifs.bracket]([log_book.location]{}[notifs.bracket]) {}",
///                 rec.location(),
///                 rec.text().clone()
///             )
///         })
///     });
/// }
/// ```
///
/// [`FooterWidgets`]: super::FooterWidgets
/// [`PromptLine`]: super::PromptLine
/// [`StatusLine`]: super::StatusLine
/// [hook]: duat_core::hook
/// [`opts::set_notifs`]: https://docs.rs/duat/latest/duat/opts/fn.set_notifs.html
pub struct Notifications {
    logs: context::Logs,
    text: Text,
    fmt: Option<Box<dyn FnMut(Record) -> Text + Send>>,
    levels: Vec<Level>,
    last_rec: Option<usize>,
    get_mask: Option<Box<dyn FnMut(Record) -> &'static str + Send>>,
    request_width: bool,
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);
#[allow(clippy::type_complexity)]
static GLOBAL_FMT: Mutex<Option<Box<dyn FnMut(Record) -> Text + Send>>> = Mutex::new(None);
#[allow(clippy::type_complexity)]
static GLOBAL_GET_MASK: Mutex<Option<Box<dyn FnMut(Record) -> &'static str + Send>>> =
    Mutex::new(None);

impl Notifications {
    /// Returns a [`NotificationsOpts`], which can be used to push
    /// `Notifications` around
    pub fn builder() -> NotificationsOpts {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            hook::add::<KeySent>(|_, _| CLEAR_NOTIFS.store(true, Ordering::Relaxed));
        });
        NotificationsOpts::default()
    }
}

impl Widget for Notifications {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let clear_notifs = CLEAR_NOTIFS.swap(false, Ordering::Relaxed);
        let notifs = handle.write(pa);

        if notifs.logs.has_changed()
            && let Some((i, rec)) = notifs.logs.last_with_levels(&notifs.levels)
            && notifs.last_rec.is_none_or(|last_i| last_i < i)
        {
            let mut global_fmt = GLOBAL_FMT.lock().unwrap();
            let mut global_get_mask = GLOBAL_GET_MASK.lock().unwrap();

            handle.set_mask(if let Some(get_mask) = notifs.get_mask.as_mut() {
                get_mask(rec.clone())
            } else if let Some(get_mask) = global_get_mask.as_mut() {
                get_mask(rec.clone())
            } else {
                default_get_mask(rec.clone())
            });

            notifs.text = if let Some(fmt) = notifs.fmt.as_mut() {
                fmt(rec)
            } else if let Some(fmt) = global_fmt.as_mut() {
                fmt(rec)
            } else {
                default_fmt(rec)
            };
            notifs.last_rec = Some(i);

            if notifs.request_width {
                let notifs = handle.read(pa);
                let size = handle
                    .area()
                    .size_of_text(pa, notifs.print_opts(), &notifs.text)
                    .unwrap();
                handle.area().set_width(pa, size.x).unwrap();
                handle.area().set_height(pa, size.y).unwrap();
            }
        } else if clear_notifs {
            handle.set_mask("");
            if notifs.text != Text::new() {
                notifs.text = Text::new();

                if notifs.request_width {
                    let notifs = handle.read(pa);
                    let size = handle
                        .area()
                        .size_of_text(pa, notifs.print_opts(), &notifs.text)
                        .unwrap();
                    handle.area().set_width(pa, size.x).unwrap();
                    handle.area().set_height(pa, size.y).unwrap();
                }
            }
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.text.as_mut()
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.logs.has_changed() || CLEAR_NOTIFS.load(Ordering::Relaxed)
    }
}

/// A builder for the [`Notifications`] [`Widget`]
///
/// Normally, this `Widget` is placed alongside others in the
/// [`FooterWidgets`] `Widget` group.
///
/// You can create it separately with [`Notifications::builder`],
/// which will return this struct.
///
/// [`PromptLine`]: super::PromptLine
/// [hook]: hook
/// [`FooterWidgets`]: super::FooterWidgets
#[doc(hidden)]
#[derive(Clone)]
pub struct NotificationsOpts {
    allowed_levels: Vec<Level>,
    request_width: bool,
}

impl NotificationsOpts {
    /// Pushes the [`Notifications`] to another [`Widget`]
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<Notifications> {
        let notifications = Notifications {
            logs: context::logs(),
            text: Text::new(),
            fmt: None,
            get_mask: None,
            levels: self.allowed_levels,
            last_rec: None,
            request_width: self.request_width,
        };
        let specs = PushSpecs {
            side: Side::Below,
            height: Some(1.0),
            ..Default::default()
        };

        push_target.push_inner(pa, notifications, specs)
    }

    /// Changes the way [`Record`]s are formatted by [`Notifications`]
    ///
    /// This will be applied to every single [`Level`] of a
    /// [`Record`]. If you wish to limit which levels will get shown,
    /// see [`set_allowed_levels`]
    ///
    /// [`set_allowed_levels`]: Self::set_allowed_levels
    pub fn fmt(&mut self, fmt: impl FnMut(Record) -> Text + Send + 'static) {
        *GLOBAL_FMT.lock().unwrap() = Some(Box::new(fmt));
    }

    /// Changes how [`Notifications`] decides which [mask] to use
    ///
    /// [mask]: duat_core::context::Handle::set_mask
    pub fn set_mask(&mut self, get_mask: impl FnMut(Record) -> &'static str + Send + 'static) {
        *GLOBAL_GET_MASK.lock().unwrap() = Some(Box::new(get_mask));
    }

    /// Filters which [`Level`]s willl show notifications
    ///
    /// Is [`Level::Info`], [`Level::Warn`] and [`Level::Error`] by
    /// default.
    pub fn set_allowed_levels(&mut self, levels: impl IntoIterator<Item = Level>) {
        self.allowed_levels = levels.into_iter().collect();
    }

    /// Requests the width when printing to the screen
    pub(crate) fn request_width(&mut self) {
        self.request_width = true;
    }
}

impl Default for NotificationsOpts {
    fn default() -> Self {
        Self {
            allowed_levels: vec![Level::Error, Level::Warn, Level::Info],
            request_width: false,
        }
    }
}

fn default_fmt(rec: Record) -> Text {
    match rec.level() {
        Level::Error | Level::Warn | Level::Debug => rec.text().clone(),
        Level::Info => rec.text().clone(),
        Level::Trace => unreachable!(),
    }
}
fn default_get_mask(rec: Record) -> &'static str {
    match rec.level() {
        context::Level::Error => "error",
        context::Level::Warn => "warn",
        context::Level::Info => "info",
        context::Level::Debug => "debug",
        context::Level::Trace => unreachable!(),
    }
}
