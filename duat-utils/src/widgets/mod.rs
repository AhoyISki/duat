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
    context,
    data::Pass,
    hook::{self, FocusedOn, KeysSentTo, UnfocusedFrom},
    ui::{Area, PushTarget, Ui, Widget},
};

pub use self::{
    line_numbers::{LineNumbers, LineNumbersBuilder},
    log_book::{LogBook, LogBookBuilder},
    notifications::{Notifications, NotificationsBuilder},
    prompt_line::{PromptLine, PromptLineBuilder},
    status_line::{State, StatusLine, StatusLineBuilder, status},
};
use crate::modes::Prompt;

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
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use duat_utils::widgets::{FooterWidgets, status};
///
/// fn setup() {
///     hook::add::<File>(|pa, (cfg, builder)| {
///         builder.push(FooterWidgets::new(status!(
///             "{name_txt}{Spacer}{} {sels_txt} {main_txt}",
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
    status: StatusLineBuilder<U>,
    prompt: PromptLineBuilder<U>,
    notifs: NotificationsBuilder,
    one_line: bool,
    above: bool,
}

impl<U: Ui> FooterWidgets<U> {
    /// Adds footer [`Widget`]s
    pub fn push_on(self, pa: &mut Pass, push_target: &impl PushTarget<U>) {
        let status_line = if self.above {
            self.status.above().push_on(pa, push_target)
        } else {
            self.status.push_on(pa, push_target)
        };

        let prompt_line = if self.one_line {
            let prompt_line = self.prompt.left().hidden().push_on(pa, &status_line);
            hook::add::<KeysSentTo<Prompt<U>, U>, U>({
                let prompt_line = prompt_line.clone();
                move |pa, (_, handle)| {
                    if handle == &prompt_line
                        && let Err(err) = handle
                            .area(pa)
                            .request_width_to_fit(handle.read(pa).get_print_cfg(), handle.text(pa))
                    {
                        context::error!("{err}")
                    }
                    Ok(())
                }
            });
            prompt_line
        } else {
            self.prompt.below().push_on(pa, &status_line)
        };

        let notifications = self.notifs.push_on(pa, &prompt_line);

        hook::add::<FocusedOn<PromptLine<U>, U>, U>({
            let notifications = notifications.clone();
            let prompt_line = prompt_line.clone();
            move |pa, (_, handle)| {
                Ok(if handle == &prompt_line {
                    notifications.area(pa).hide().unwrap();
                    handle.area(pa).reveal().unwrap();
                })
            }
        });

        hook::add::<UnfocusedFrom<PromptLine<U>, U>, U>({
            move |pa, (_, handle)| {
                Ok(if handle == &prompt_line {
                    notifications.area(pa).reveal().unwrap();
                    handle.area(pa).hide().unwrap();
                })
            }
        });
    }

    /// Returns a new [`FooterWidgets`], with a [`StatusLine`] and
    /// default [`PromptLine`] and [`Notifications`]
    ///
    /// You can set those other two through the [`prompt`] and
    /// [`notifs`] methods.
    ///
    /// [`prompt`]: FooterWidgets::prompt
    /// [`notifs`]: FooterWidgets::notifs
    pub fn new(status_cfg: StatusLineBuilder<U>) -> Self {
        Self {
            status: status_cfg,
            prompt: PromptLine::builder(),
            notifs: Notifications::builder(),
            one_line: false,
            above: false,
        }
    }

    /// Turns this footer into a Kakoune-like one liner, as opposed to
    /// a Neovim-like two liner
    pub fn one_line(self) -> Self {
        Self { one_line: true, ..self }
    }

    /// Changes this footer to a header
    pub fn above(self) -> Self {
        Self { above: true, ..self }
    }

    /// Sets the [`PromptLine`] to be used
    pub fn prompt(self, prompt: PromptLineBuilder<U>) -> Self {
        Self { prompt, ..self }
    }

    /// Sets the [`Notifications`] to be used
    pub fn notifs(self, notifs: NotificationsBuilder) -> Self {
        Self { notifs, ..self }
    }
}
