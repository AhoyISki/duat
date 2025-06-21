//! A [`Widget`] that shows notifications
//!
//! This is a very simple [`Widget`], and will usually be placed right
//! under a [`PromptLine`], which, when the `"HidePromptLine"` [hook]
//! group exists, will be hidden when the [`PromptLine`] is not in
//! focus, allowing for the [`Notifications`] widget to pop up.
//!
//! [`PromptLine`]: super::PromptLine
//! [hook]: hooks
use std::{
    marker::PhantomData,
    sync::atomic::{AtomicBool, Ordering},
};

use duat_core::{
    context::{Level, Record},
    form::Painter,
    hook::KeysSent,
    prelude::*,
};

/// A [`Widget`] to show notifications
///
/// By default, it is expected to be placed "under" a [`PromptLine`],
/// and with the `"HidePromptLine"` [hook] group, take its place when
/// the [`PromptLine`] is not in focus.
///
/// If you don't want this behaviour, see [`left_with_ratio`]
///
/// [`PromptLine`]: super::PromptLine
/// [hook]: duat_core::hook
/// [`left_with_ratio`]: NotificationsCfg::left_with_ratio
pub struct Notifications<U> {
    logs: context::Logs,
    text: Text,
    _ghost: PhantomData<U>,
    format_rec: Box<dyn FnMut(Record) -> Option<Text>>,
    get_mask: Box<dyn FnMut(Record) -> &'static str>,
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);

impl<U: Ui> Widget<U> for Notifications<U> {
    type Cfg = NotificationsCfg<U>;

    fn cfg() -> Self::Cfg {
        NotificationsCfg {
            format_rec: Box::new(|rec| {
                // This is so stupid
                (rec.level() < Level::Debug).then(|| {
                    txt!(
                        "[notifs.target]{}[notifs.colon]: {}",
                        rec.target(),
                        rec.text().clone()
                    )
                    .build()
                })
            }),
            get_mask: Box::new(|rec| match rec.level() {
                context::Level::Error => "error",
                context::Level::Warn => "warn",
                context::Level::Info => "info",
                context::Level::Debug => "debug",
                context::Level::Trace => unreachable!(),
            }),
            _ghost: PhantomData,
        }
    }

    fn update(pa: &mut Pass, handle: Handle<Self, U>) {
        let clear_notifs = CLEAR_NOTIFS.swap(false, Ordering::Relaxed);
        handle.write(pa, |wid, _| {
            if wid.logs.has_changed()
                && let Some(rec) = wid.logs.last()
                && let Some(text) = (wid.format_rec)(rec.clone())
            {
                handle.set_mask((wid.get_mask)(rec));
                wid.text = text
            } else if clear_notifs {
                handle.set_mask("");
                wid.text = Text::new()
            }
        });
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
        });
        Ok(())
    }

    fn needs_update(&self) -> bool {
        self.logs.has_changed() || CLEAR_NOTIFS.load(Ordering::Relaxed)
    }

    fn print(&mut self, painter: Painter, area: &<U as Ui>::Area) {
        let cfg = self.print_cfg();
        area.print(self.text_mut(), cfg, painter)
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
pub struct NotificationsCfg<U> {
    format_rec: Box<dyn FnMut(Record) -> Option<Text>>,
    get_mask: Box<dyn FnMut(Record) -> &'static str>,
    _ghost: PhantomData<U>,
}

impl<U> NotificationsCfg<U> {
    /// Changes the way [`Record`]s are formatted by [`Notifications`]
    ///
    /// This function returns an [`Option<Text>`], which means you can
    /// filter out unnecessary [`Record`]s. By default, only records
    /// with a level of [`Level::Info`] or higher will get shown.
    pub fn formatted(self, format_rec: impl FnMut(Record) -> Option<Text> + 'static) -> Self {
        Self { format_rec: Box::new(format_rec), ..self }
    }

    /// Changes how [`Notifications`] decides which [mask] to use
    ///
    /// [mask]: duat_core::context::Handle::set_mask
    pub fn with_mask(self, get_mask: impl FnMut(Record) -> &'static str + 'static) -> Self {
        Self { get_mask: Box::new(get_mask), ..self }
    }
}

impl<U: Ui> WidgetCfg<U> for NotificationsCfg<U> {
    type Widget = Notifications<U>;

    fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let widget = Notifications {
            logs: context::logs(),
            text: Text::new(),
            format_rec: self.format_rec,
            get_mask: self.get_mask,
            _ghost: PhantomData,
        };

        (widget, PushSpecs::below().with_ver_len(1.0))
    }
}

impl<U: Ui> Default for NotificationsCfg<U> {
    fn default() -> Self {
        Notifications::cfg()
    }
}
