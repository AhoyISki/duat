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
    Once,
    atomic::{AtomicBool, Ordering},
};

use duat_core::{
    context::{self, Handle, Level, Record},
    data::Pass,
    hook::{self, KeysSent},
    text::{Text, txt},
    ui::{PushSpecs, PushTarget, Side, Widget},
};

/// A [`Widget`] to show notifications
///
/// With the [`FooterWidgets`] group, this [`Widget`] can be conveniently 
/// placed alongside a [`PromptLine`] and a [`StatusLine`],in a combination
/// that hides the [`PromptLine`]when it is not in use, covering it with the 
/// [`Notifications`], andvice-versa. This is the default behaviour of Duat.
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_utils::widgets::{Notifications, FooterWidgets};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::remove("WindowWidgets");
///     hook::add::<WindowCreated>(|_, builder| {
///         let footer = FooterWidgets::default().notifs(Notifications::opts().formatted(|rec| {
///             txt!(
///                 "[notifs.bracket]([notifs.target]{}[notifs.bracket]) {}",
///                 rec.target(),
///                 rec.text().clone()
///             )
///         }));
///         builder.push(footer);
///     });
/// }
/// ```
///
/// [`FooterWidgets`]: super::FooterWidgets
/// [`PromptLine`]: super::PromptLine
/// [`StatusLine`]: super::StatusLine
/// [hook]: duat_core::hook
pub struct Notifications {
    logs: context::Logs,
    text: Text,
    format_rec: Box<dyn FnMut(Record) -> Text + Send>,
    levels: Vec<Level>,
    last_rec: Option<usize>,
    get_mask: Box<dyn FnMut(Record) -> &'static str + Send>,
    request_width: bool,
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);

impl Notifications {
    /// Returns a [`NotificationsOpts`], which can be used to push
    /// `Notifications` around
    pub fn builder() -> NotificationsOpts {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            hook::add::<KeysSent>(|_, _| {
                CLEAR_NOTIFS.store(true, Ordering::Relaxed);
                Ok(())
            });
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
            handle.set_mask((notifs.get_mask)(rec.clone()));
            notifs.text = (notifs.format_rec)(rec);
            notifs.last_rec = Some(i);

            if notifs.request_width {
                let notifs = handle.read(pa);
                let width = handle
                    .area()
                    .width_of_text(pa, notifs.get_print_opts(), &notifs.text)
                    .unwrap();
                handle.area().set_width(pa, width).unwrap();
            }
        } else if clear_notifs {
            handle.set_mask("");
            if notifs.text != Text::new() {
                notifs.text = Text::new();

                if notifs.request_width {
                    let notifs = handle.read(pa);
                    let width = handle
                        .area()
                        .width_of_text(pa, notifs.get_print_opts(), &notifs.text)
                        .unwrap();
                    handle.area().set_width(pa, width).unwrap();
                }
            }
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
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
pub struct NotificationsOpts {
    fmt: Box<dyn FnMut(Record) -> Text + Send>,
    get_mask: Box<dyn FnMut(Record) -> &'static str + Send>,
    allowed_levels: Vec<Level>,
    request_width: bool,
}

impl NotificationsOpts {
    /// Pushes the [`Notifications`] to another [`Widget`]
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget) -> Handle<Notifications> {
        let notifications = Notifications {
            logs: context::logs(),
            text: Text::new(),
            format_rec: self.fmt,
            get_mask: self.get_mask,
            levels: self.allowed_levels,
            last_rec: None,
            request_width: self.request_width,
        };
        let specs = PushSpecs { side: Side::Below, height: Some(1.0), .. };

        push_target.push_inner(pa, notifications, specs)
    }

    /// Changes the way [`Record`]s are formatted by [`Notifications`]
    ///
    /// This will be applied to every single [`Level`] of a
    /// [`Record`]. If you wish to limit which levels will get shown,
    /// see [`filter_levels`]
    ///
    /// [`filter_levels`]: Self::filter_levels
    pub fn fmt<T: Into<Text>>(&mut self, mut fmt: impl FnMut(Record) -> T + Send + 'static) {
        self.fmt = Box::new(move |rec| fmt(rec).into());
    }

    /// Filters which [`Level`]s willl show notifications
    ///
    /// Is [`Level::Info`], [`Level::Warn`] and [`Level::Error`] by
    /// default.
    pub fn filter_levels(&mut self, levels: impl IntoIterator<Item = Level>) {
        self.allowed_levels = levels.into_iter().collect();
    }

    /// Changes how [`Notifications`] decides which [mask] to use
    ///
    /// [mask]: duat_core::context::Handle::set_mask
    pub fn set_mask(&mut self, get_mask: impl FnMut(Record) -> &'static str + Send + 'static) {
        self.get_mask = Box::new(get_mask);
    }

    /// Requests the width when printing to the screen
    pub(crate) fn request_width(&mut self) {
        self.request_width = true;
    }
}

impl Default for NotificationsOpts {
    fn default() -> Self {
        fn default_fmt(rec: Record) -> Text {
            txt!(
                "[notifs.target]{}[notifs.colon]: {}",
                rec.target(),
                rec.text().clone()
            )
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

        Self {
            fmt: Box::new(default_fmt),
            get_mask: Box::new(default_get_mask),
            allowed_levels: vec![Level::Error, Level::Warn, Level::Info],
            request_width: false,
        }
    }
}
