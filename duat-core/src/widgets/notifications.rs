//! A [`Widget`] that shows notifications
//!
//! This is a very simple [`Widget`], and will usually be placed right
//! under a [`CmdLine`], which, when the `"HideCmdLine"` [hook] group
//! exists, will be hidden when the [`CmdLine`] is not in focus,
//! allowing for the [`Notifications`] widget to pop up.
//!
//! [`CmdLine`]: super::CmdLine
//! [hook]: hooks
use std::{
    marker::PhantomData,
    sync::atomic::{AtomicBool, Ordering},
};

use super::{CheckerFn, Widget, WidgetCfg};
use crate::{
    context::{self, Notifications},
    hooks::{self, KeySent},
    text::Text,
    ui::{PushSpecs, Ui},
};

/// A [`Widget`] to show notifications
///
/// By default, it is expected to be placed "under" a [`CmdLine`], and
/// with the `"HideCmdLine"` [hook] group, take its place when the
/// [`CmdLine`] is not in focus.
///
/// If you don't want this behaviour, see [`left_with_ratio`]
///
/// [`CmdLine`]: super::CmdLine
/// [hook]: hooks
/// [`left_with_ratio`]: NotificationsCfg::left_with_ratio
pub struct Notifier<U> {
    notifications: Notifications,
    text: Text,
    _ghost: PhantomData<U>,
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);

impl<U: Ui> Widget<U> for Notifier<U> {
    type Cfg = NotificationsCfg<U>;

    fn cfg() -> Self::Cfg {
        NotificationsCfg(None, PhantomData)
    }

    fn update(&mut self, _area: &<U as Ui>::Area) {
        let clear_notifs = CLEAR_NOTIFS.swap(false, Ordering::Relaxed);
        if self.notifications.has_changed() {
            let notifications = self.notifications.read();
            self.text = notifications.last().cloned().unwrap_or_default()
        } else if clear_notifs {
            self.text = Text::new()
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn once() -> Result<(), Text> {
        hooks::add_grouped::<KeySent>("RemoveNotificationsOnInput", |_| {
            CLEAR_NOTIFS.store(true, Ordering::Relaxed);
        });
        Ok(())
    }
}

/// A [`Widget`] to show notifications
///
/// By default, it is expected to be placed "under" a [`CmdLine`], and
/// with the `"HideCmdLine"` [hook] group, take its place when the
/// [`CmdLine`] is not in focus.
///
/// If you don't want this behaviour, see [`left_with_ratio`]
///
/// [`CmdLine`]: super::CmdLine
/// [hook]: hooks
/// [`left_with_ratio`]: NotificationsCfg::left_with_ratio
#[doc(hidden)]
pub struct NotificationsCfg<U>(Option<(u16, u16)>, PhantomData<U>);

impl<U> NotificationsCfg<U> {
    /// Pushes to the left and sets a height
    ///
    /// Use this if you want notifications that don't occupy the same
    /// space as a [`CmdLine`].
    ///
    /// [`CmdLine`]: super::CmdLine
    pub fn left_with_ratio(self, den: u16, div: u16) -> Self {
        Self(Some((den, div)), PhantomData)
    }
}

impl<U: Ui> WidgetCfg<U> for NotificationsCfg<U> {
    type Widget = Notifier<U>;

    fn build(self, _: bool) -> (Self::Widget, impl CheckerFn, PushSpecs) {
        let widget = Notifier {
            notifications: context::notifications(),
            text: Text::new(),
            _ghost: PhantomData,
        };

        let checker = {
            let checker = widget.notifications.checker();
            move || checker() || CLEAR_NOTIFS.load(Ordering::Relaxed)
        };

        let specs = if let Some((den, div)) = self.0 {
            PushSpecs::left().with_hor_ratio(den, div)
        } else {
            PushSpecs::below().with_ver_len(1.0)
        };

        (widget, checker, specs)
    }
}
