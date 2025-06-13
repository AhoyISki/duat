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
    context::{self, FileHandle, Logs},
    data::{Pass, RwData},
    hook::{self, KeysSent},
    text::Text,
    ui::{PushSpecs, Ui},
    widget::{Widget, WidgetCfg},
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
/// [hook]: hooks
/// [`left_with_ratio`]: NotificationsCfg::left_with_ratio
pub struct Notifications<U> {
    logs: Logs,
    text: Text,
    _ghost: PhantomData<U>,
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);

impl<U: Ui> Widget<U> for Notifications<U> {
    type Cfg = NotificationsCfg<U>;

    fn cfg() -> Self::Cfg {
        NotificationsCfg(None, PhantomData)
    }

    fn update(mut pa: Pass, widget: RwData<Self>, _: &<U as Ui>::Area) {
        let clear_notifs = CLEAR_NOTIFS.swap(false, Ordering::Relaxed);
        widget.write(&mut pa, |wid| {
            if wid.logs.has_changed() {
                wid.text = match wid.logs.last().unwrap() {
                    context::Log::Text(cursorless) => cursorless.get(),
                    context::Log::CmdResult(_, cursorless) => cursorless.get(),
                };
            } else if clear_notifs {
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
        hook::add_grouped::<KeysSent>("RemoveNotificationsOnInput", |_, _| {
            CLEAR_NOTIFS.store(true, Ordering::Relaxed);
        });
        Ok(())
    }

    fn needs_update(&self) -> bool {
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
pub struct NotificationsCfg<U>(Option<(u16, u16)>, PhantomData<U>);

impl<U> NotificationsCfg<U> {
    /// Pushes to the left and sets a height
    ///
    /// You might want this if you want something like a
    /// [`StatusLine`] on the same line as the notifications and the
    /// [`PromptLine`]
    ///
    /// [`StatusLine`]: super::StatusLine
    /// [`PromptLine`]: super::PromptLine
    pub fn left_with_ratio(self, den: u16, div: u16) -> Self {
        Self(Some((den, div)), PhantomData)
    }
}

impl<U: Ui> WidgetCfg<U> for NotificationsCfg<U> {
    type Widget = Notifications<U>;

    fn build(self, _: Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let widget = Notifications {
            logs: context::logs(),
            text: Text::new(),
            _ghost: PhantomData,
        };

        let specs = if let Some((den, div)) = self.0 {
            PushSpecs::left().with_hor_ratio(den, div)
        } else {
            PushSpecs::below().with_ver_len(1.0)
        };

        (widget, specs)
    }
}
