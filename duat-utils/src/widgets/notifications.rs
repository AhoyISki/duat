//! A [`Widget`] that shows notifications
//!
//! This is a very simple [`Widget`], and will usually be placed right
//! under a [`PromptLine`], which, when the `"HidePromptLine"` [hook]
//! group exists, will be hidden when the [`PromptLine`] is not in
//! focus, allowing for the [`Notifications`] widget to pop up.
//!
//! [`PromptLine`]: super::PromptLine
//! [hook]: hooks
use std::sync::atomic::{AtomicBool, Ordering};

use duat_core::{
    context::{Level, Record},
    hook::KeysSent,
    prelude::*,
    ui::{PushTarget, Side},
};

/// A [`Widget`] to show notifications
///
/// With the [`FooterWidgets`] (a [`WidgetAlias`]), this [`Widget`]
/// can be conveniently placed alongside a [`PromptLine`] and a
/// [`StatusLine`], in a combination that hides the [`PromptLine`]
/// when it is not in use, covering it with the [`Notifications`], and
/// vice-versa. This is the default behaviour of Duat.
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
///         let footer = FooterWidgets::default().notifs(Notifications::cfg().formatted(|rec| {
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
/// [`WidgetAlias`]: duat_core::ui::WidgetAlias
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
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);

impl Notifications {
    /// Returns a [`NotificationsBuilder`], which can be used to push
    /// `Notifications` around
    pub fn builder() -> NotificationsBuilder {
        NotificationsBuilder::default()
    }
}

impl<U: Ui> Widget<U> for Notifications {
    fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
        let clear_notifs = CLEAR_NOTIFS.swap(false, Ordering::Relaxed);
        let notifs = handle.write(pa);

        if notifs.logs.has_changed()
            && let Some((i, rec)) = notifs.logs.last_with_levels(&notifs.levels)
            && notifs.last_rec.is_none_or(|last_i| last_i < i)
        {
            handle.set_mask((notifs.get_mask)(rec.clone()));
            notifs.text = (notifs.format_rec)(rec);
            notifs.last_rec = Some(i);
        } else if clear_notifs {
            handle.set_mask("");
            notifs.text = Text::new()
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        form::set_weak("default.Notifications.error", Form::red());
        form::set_weak("accent.error", Form::red().underlined().bold());
        form::set_weak("default.Notifications.info", Form::cyan());
        form::set_weak("accent.info", Form::blue().underlined().bold());

        hook::add_grouped::<KeysSent, U>("RemoveNotificationsOnInput", |_, _| {
            CLEAR_NOTIFS.store(true, Ordering::Relaxed);
            Ok(())
        });
        Ok(())
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.logs.has_changed() || CLEAR_NOTIFS.load(Ordering::Relaxed)
    }
}

/// A [`Widget`] to show notifications
///
/// By default, it is expected to be placed "under" a [`PromptLine`],
/// and with the `"HidePromptLine"` [hook] group, take its place when
/// the [`PromptLine`] is not in focus.
///
/// If you don't want this behaviour, see [`left_with_ratio`]
///
/// [`PromptLine`]: super::PromptLine
/// [hook]: hooks
/// [`left_with_ratio`]: NotificationsCfg::left_with_ratio
#[doc(hidden)]
pub struct NotificationsBuilder {
    fmt: Box<dyn FnMut(Record) -> Text + Send>,
    get_mask: Box<dyn FnMut(Record) -> &'static str + Send>,
    allowed_levels: Vec<Level>,
}

impl NotificationsBuilder {
    /// Pushes the [`Notifications`] to another [`Widget`]
    pub fn push_on<U: Ui>(
        self,
        pa: &mut Pass,
        push_target: &impl PushTarget<U>,
    ) -> Handle<Notifications, U> {
        let notifications = Notifications {
            logs: context::logs(),
            text: Text::new(),
            format_rec: self.fmt,
            get_mask: self.get_mask,
            levels: self.allowed_levels,
            last_rec: None,
        };
        let specs = PushSpecs { side: Side::Below, height: Some(1.0), .. };

        push_target.push_outer(pa, notifications, specs)
    }

    /// Changes the way [`Record`]s are formatted by [`Notifications`]
    ///
    /// This will be applied to every single [`Level`] of a
    /// [`Record`]. If you wish to limit which levels will get shown,
    /// see [`filter_levels`]
    ///
    /// [`filter_levels`]: Self::filter_levels
    pub fn fmt<T: Into<Text>>(self, mut fmt: impl FnMut(Record) -> T + Send + 'static) -> Self {
        Self {
            fmt: Box::new(move |rec| fmt(rec).into()),
            ..self
        }
    }

    /// Filters which [`Level`]s willl show notifications
    ///
    /// Is [`Level::Info`], [`Level::Warn`] and [`Level::Error`] by
    /// default.
    pub fn filter_levels(mut self, levels: impl IntoIterator<Item = Level>) -> Self {
        self.allowed_levels = levels.into_iter().collect();
        self
    }

    /// Changes how [`Notifications`] decides which [mask] to use
    ///
    /// [mask]: duat_core::context::Handle::set_mask
    pub fn with_mask(self, get_mask: impl FnMut(Record) -> &'static str + Send + 'static) -> Self {
        Self { get_mask: Box::new(get_mask), ..self }
    }
}

impl Default for NotificationsBuilder {
    fn default() -> Self {
        fn default_fmt(rec: Record) -> Text {
            txt!(
                "[notifs.target]{}[notifs.colon]: {}",
                rec.target(),
                rec.text().clone()
            )
            .build()
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
            allowed_levels: Default::default(),
        }
    }
}
