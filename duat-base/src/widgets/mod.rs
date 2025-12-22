//! Core [`Widget`]s for usage in Duat
//!
//! The widgets defined in this module are expected to be used in
//! pretty much every `config` of Duat, since they are Duat
//! equivalents to what is commonly found in other text editors.
//!
//! [`Widget`]: duat_core::ui::Widget
//! [`Text`]: duat_core::text::Text
//! [`Buffer`]: duat_core::buffer::Buffer
//! [`WindowCreated`]: duat_core::hook::WindowCreated
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
use duat_core::{
    data::Pass,
    hook::{self, FocusedOn, UnfocusedFrom},
    try_or_log_err,
    ui::PushTarget,
};

pub use self::{
    completions::{
        Completions, CompletionsBuilder, CompletionsKind, CompletionsList, CompletionsProvider,
        track_words,
    },
    info::Info,
    line_numbers::{LineNumbers, LineNumbersOpts},
    log_book::{LogBook, LogBookOpts},
    notifications::{Notifications, NotificationsOpts},
    prompt_line::{PromptLine, PromptLineBuilder},
    status_line::{State, StatusLine, StatusLineFmt, status},
    which_key::WhichKey,
};

mod completions;
mod info;
mod line_numbers;
mod log_book;
mod notifications;
mod prompt_line;
mod status_line;
mod which_key;

/// A group of [`Widget`]s consisting of a [`StatusLine`],
/// [`PromptLine`] and [`Notifications`] combo
///
/// These are the default [`Widget`]s placed in the footer position of
/// Duat. By default, they will be placed [around the window], but
/// they can also be placed around individual [`Buffer`]s:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::widgets::{FooterWidgets, status};
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<Buffer>(|pa, handle| {
///         FooterWidgets::new(status!(
///             "{name_txt}{Spacer}{} {sels_txt} {main_txt}",
///             mode_txt()
///         ))
///         .push_on(pa, handle);
///         Ok(())
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
/// [`Buffer`]: duat_core::buffer::Buffer
/// [`one_line`]: FooterWidgets::one_line
/// [`prompt`]: FooterWidgets::prompt
/// [`notifs`]: FooterWidgets::notifs
/// [`above`]: FooterWidgets::above
/// [`Widget`]: duat_core::ui::Widget
pub struct FooterWidgets {
    status: StatusLineFmt,
    prompt: PromptLineBuilder,
    notifs: NotificationsOpts,
    one_line: bool,
    above: bool,
}

impl FooterWidgets {
    /// Adds footer [`Widget`]s
    ///
    /// [`Widget`]: duat_core::ui::Widget
    pub fn push_on(mut self, pa: &mut Pass, push_target: &impl PushTarget) {
        let prompt_line = if self.one_line {
            self.prompt.request_width()
        } else {
            self.prompt
        };
        let prompt_line = if self.above {
            prompt_line.above().hidden().push_on(pa, push_target)
        } else {
            prompt_line.below().hidden().push_on(pa, push_target)
        };

        if self.one_line {
            self.status.right().push_on(pa, &prompt_line);
        } else {
            self.status.above().push_on(pa, &prompt_line);
        };

        let notifications = if self.one_line {
            self.notifs.request_width();
            self.notifs.push_on(pa, &prompt_line)
        } else {
            self.notifs.push_on(pa, &prompt_line)
        };

        hook::add::<FocusedOn<PromptLine>>({
            let notifications = notifications.clone();
            move |pa, (_, handle)| {
                try_or_log_err! {
                    notifications.area().hide(pa)?;
                    handle.area().reveal(pa)?;
                }
            }
        })
        .filter(prompt_line.clone());

        hook::add::<UnfocusedFrom<PromptLine>>({
            move |pa, (handle, _)| {
                try_or_log_err! {
                    notifications.area().reveal(pa)?;
                    handle.area().hide(pa)?;
                }
            }
        })
        .filter(prompt_line);
    }

    /// Returns a new [`FooterWidgets`], with a [`StatusLine`] and
    /// default [`PromptLine`] and [`Notifications`]
    ///
    /// You can set those other two through the [`prompt`] and
    /// [`notifs`] methods.
    ///
    /// [`prompt`]: FooterWidgets::prompt
    /// [`notifs`]: FooterWidgets::notifs
    pub fn new(status_cfg: StatusLineFmt) -> Self {
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
    pub fn prompt(self, prompt: PromptLineBuilder) -> Self {
        Self { prompt, ..self }
    }

    /// Sets the [`Notifications`] to be used
    pub fn notifs(self, notifs: NotificationsOpts) -> Self {
        Self { notifs, ..self }
    }
}

impl Default for FooterWidgets {
    fn default() -> Self {
        Self {
            status: StatusLine::builder(),
            prompt: PromptLine::builder(),
            notifs: Notifications::builder(),
            one_line: false,
            above: false,
        }
    }
}
