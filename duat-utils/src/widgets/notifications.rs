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
/// With the [`FooterWidgets`] (a [`WidgetAlias`]), this [`Widget`]
/// can be conveniently placed alongside a [`PromptLine`] and a
/// [`StatusLine`], in a combination that hides the [`PromptLine`]
/// when it is not in use, covering it with the [`Notifications`], and
/// vice-versa. This is the default behaviour of Duat.
///
/// ```rust
/// use duat_core::{hook::OnWindowOpen, prelude::*};
/// use duat_utils::{state::*, widgets::*};
///
/// fn setup_generic_over_ui<U: Ui>() {
///     hook::remove("WindowWidgets");
///     hook::add::<OnWindowOpen<U>, U>(|pa, builder| {
///         let footer = FooterWidgets::default().notifs(Notifications::cfg().formatted(|rec| {
///             txt!(
///                 "[notifs.bracket]([notifs.target]{}[notifs.bracket]) {}",
///                 rec.target(),
///                 rec.text().clone()
///             )
///         }));
///         builder.push(pa, footer);
///     });
/// }
/// ```
///
/// [`FooterWidgets`]: super::FooterWidgets
/// [`WidgetAlias`]: duat_core::ui::WidgetAlias
/// [`PromptLine`]: super::PromptLine
/// [`StatusLine`]: super::StatusLine
/// [hook]: duat_core::hook
pub struct Notifications<U> {
    logs: context::Logs,
    text: Text,
    format_rec: Box<dyn FnMut(Record) -> Text + Send>,
    levels: Vec<Level>,
    last_rec: Option<usize>,
    get_mask: Box<dyn FnMut(Record) -> &'static str + Send>,
    _ghost: PhantomData<U>,
}

static CLEAR_NOTIFS: AtomicBool = AtomicBool::new(false);

impl<U: Ui> Widget<U> for Notifications<U> {
    type Cfg = NotificationsCfg<U>;

    fn cfg() -> Self::Cfg {
        NotificationsCfg {
            format_rec: Box::new(|rec| {
                txt!(
                    "[notifs.target]{}[notifs.colon]: {}",
                    rec.target(),
                    rec.text().clone()
                )
                .build()
            }),
            get_mask: Box::new(|rec| match rec.level() {
                context::Level::Error => "error",
                context::Level::Warn => "warn",
                context::Level::Info => "info",
                context::Level::Debug => "debug",
                context::Level::Trace => unreachable!(),
            }),
            levels: vec![Level::Info, Level::Warn, Level::Error],
            _ghost: PhantomData,
        }
    }

    fn update(pa: &mut Pass, handle: Handle<Self, U>) {
        let clear_notifs = CLEAR_NOTIFS.swap(false, Ordering::Relaxed);
        handle.write(pa, |wid, _| {
            if wid.logs.has_changed()
                && let Some((i, rec)) = wid.logs.last_with_levels(&wid.levels)
                && wid.last_rec.is_none_or(|last_i| last_i < i)
            {
                handle.set_mask((wid.get_mask)(rec.clone()));
                wid.text = (wid.format_rec)(rec);
                wid.last_rec = Some(i);
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

    fn needs_update(&self, _: &Pass) -> bool {
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
    format_rec: Box<dyn FnMut(Record) -> Text + Send>,
    get_mask: Box<dyn FnMut(Record) -> &'static str + Send>,
    levels: Vec<Level>,
    _ghost: PhantomData<U>,
}

impl<U> NotificationsCfg<U> {
    /// Changes the way [`Record`]s are formatted by [`Notifications`]
    ///
    /// This will be applied to every single [`Level`] of a
    /// [`Record`]. If you wish to limit which levels will get shown,
    /// see [`filter_levels`]
    ///
    /// [`filter_levels`]: Self::filter_levels
    pub fn formatted<T: Into<Text>>(
        self,
        mut fmt: impl FnMut(Record) -> T + Send + 'static,
    ) -> Self {
        Self {
            format_rec: Box::new(move |rec| fmt(rec).into()),
            ..self
        }
    }

    /// Filters which [`Level`]s willl show notifications
    ///
    /// Is [`Level::Info`], [`Level::Warn`] and [`Level::Error`] by
    /// default.
    pub fn filter_levels(mut self, levels: impl IntoIterator<Item = Level>) -> Self {
        self.levels = levels.into_iter().collect();
        self
    }

    /// Changes how [`Notifications`] decides which [mask] to use
    ///
    /// [mask]: duat_core::context::Handle::set_mask
    pub fn with_mask(self, get_mask: impl FnMut(Record) -> &'static str + Send + 'static) -> Self {
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
            levels: self.levels,
            last_rec: None,
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
