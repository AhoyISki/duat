//! Core [`Widget`]s for usage in Duat
//!
//! The widgets defined in this module are expected to be used in
//! pretty much every `config` of Duat, since they are Duat
//! equivalents to what is commonly found in other text editors.
//!
//!
//!
//! [`Widget`]: duat_core::ui::Widget
//! [`Text`]: duat_core::text::Text
//! [`File`]: duat_core::file::File
//! [`WindowCreated`]: duat_core::hook::WindowCreated
//! [`Constraint`]: duat_core::ui::Constraint
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
use duat_core::{
    hook::{self, FocusedOn, UnfocusedFrom},
    ui::{Area, AreaId, BuilderDummy, GetAreaId, Ui, Widget, WidgetAlias},
};

pub use self::{
    line_numbers::{LineNumbers, LineNumbersOptions},
    log_book::{LogBook, LogBookCfg},
    notifications::{Notifications, NotificationsCfg},
    prompt_line::{PromptLine, PromptLineCfg},
    status_line::{State, StatusLine, StatusLineCfg, status},
};

mod line_numbers;
mod log_book;
mod notifications;
mod prompt_line;
mod status_line;

/// A footer [`WidgetAlias`] consisting of a [`StatusLine`],
/// [`PromptLine`] and [`Notifications`] combo
///
/// These are the default [`Widget`]s placed in the footer position of
/// Duat. By default, they will be placed [around the window], but
/// they can also be placed around individual [`File`]s:
///
/// ```rust
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use duat_utils::widgets::{FooterWidgets, status};
///
/// fn setup() {
///     hook::add::<File>(|pa, (cfg, builder)| {
///         builder.push(FooterWidgets::new(status!(
///             "{file_txt}{Spacer}{} {sels_txt} {main_txt}",
///             mode_txt(pa)
///         )));
///         cfg
///     });
/// }
/// ```
///
/// This specific [`Widget`] configuration makes use of [hook]s in
/// order to show the [`PromptLine`] only when it is in focus, showing
/// [`Notifications`] whenever that is not the case. Like Vim/Neovim,
/// it also takes up two lines on the screen, one for the
/// [`StatusLine`] and one for the other two [`Widget`]s.
///
/// If you want a Kakoune-like one line footer, use
/// the [`one_line`] method. You can also use the [`prompt`] and
/// [`notifs`] methods in order to specify the [`PromptLine`] and
/// [`Notifications`] [`Widget`]s, respectively.
///
/// Additionally, you can call the [`above`] method in order to place
/// the footer as a header instead.
///
/// [around the window]: duat_core::hook::WindowCreated
/// [`File`]: duat_core::file::File
/// [`one_line`]: FooterWidgets::one_line
/// [`prompt`]: FooterWidgets::prompt
/// [`notifs`]: FooterWidgets::notifs
/// [`above`]: FooterWidgets::above
#[derive(Default)]
pub struct FooterWidgets<U: Ui> {
    status_cfg: StatusLineCfg<U>,
    prompt_cfg: PromptLineCfg<U>,
    notifs_cfg: NotificationsCfg<U>,
    is_one_line: bool,
    is_above: bool,
}

impl<U: Ui> FooterWidgets<U> {
    /// Returns a new [`FooterWidgets`], with a [`StatusLine`] and
    /// default [`PromptLine`] and [`Notifications`]
    ///
    /// You can set those other two through the [`prompt`] and
    /// [`notifs`] methods.
    ///
    /// [`prompt`]: FooterWidgets::prompt
    /// [`notifs`]: FooterWidgets::notifs
    pub fn new(status_cfg: StatusLineCfg<U>) -> Self {
        Self {
            status_cfg,
            prompt_cfg: PromptLine::cfg(),
            notifs_cfg: Notifications::cfg(),
            is_one_line: false,
            is_above: false,
        }
    }

    /// Turns this footer into a Kakoune-like one liner, as opposed to
    /// a Neovim-like two liner
    pub fn one_line(self) -> Self {
        Self { is_one_line: true, ..self }
    }

    /// Changes this footer to a header
    pub fn above(self) -> Self {
        Self { is_above: true, ..self }
    }

    /// Sets the [`PromptLine`] to be used
    pub fn prompt(self, prompt_cfg: PromptLineCfg<U>) -> Self {
        Self { prompt_cfg, ..self }
    }

    /// Sets the [`Notifications`] to be used
    pub fn notifs(self, notifs_cfg: NotificationsCfg<U>) -> Self {
        Self { notifs_cfg, ..self }
    }
}

impl<U: Ui> WidgetAlias<U, FooterWidgetsDummy> for FooterWidgets<U> {
    fn push_alias(self, builder: &mut duat_core::ui::RawUiBuilder<U>) -> AreaId {
        let status_id = if self.is_above {
            builder.push(self.status_cfg.above())
        } else {
            builder.push(self.status_cfg.below())
        };

        let prompt_id = if self.is_one_line {
            builder.push_to(status_id, self.prompt_cfg.left_ratioed(3, 7).hidden())
        } else {
            builder.push_to(status_id, self.prompt_cfg.below().hidden())
        };

        let notifs_id = builder.push_to(prompt_id, self.notifs_cfg);

        hook::add::<FocusedOn<PromptLine<U>, U>, U>({
            move |pa, (_, prompt)| {
                if prompt.area_id() == prompt_id
                    && let Some(notifs_area) = notifs_id.area::<U>(pa)
                {
                    notifs_area.hide().unwrap();
                    prompt.area(pa).reveal().unwrap();
                }
            }
        });

        hook::add::<UnfocusedFrom<PromptLine<U>, U>, U>(move |pa, (prompt, _)| {
            if prompt.area_id() == prompt_id
                && let Some(notifs_area) = notifs_id.area::<U>(pa)
            {
                notifs_area.reveal().unwrap();
                prompt.area(pa).hide().unwrap();
            }
        });

        notifs_id
    }
}

#[doc(hidden)]
pub struct FooterWidgetsDummy;

impl BuilderDummy for FooterWidgetsDummy {}
