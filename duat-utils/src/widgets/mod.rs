//! APIs for the construction of widgets, and a few common ones.
//!
//! This module has the [`Widget`] trait, which is a region on the
//! window containing a [`Text`], and may be modified by user mode
//! (but not necessarily).
//!
//! With the exception of the [`File`], these widgets will show up in
//! one of three contexts:
//!
//! - Being pushed to a [`File`] via the hook [`OnFileOpen`];
//! - Being pushed to the outer edges via [`OnWindowOpen`];
//! - Being pushed to popup widgets via `OnPopupOpen` (TODO);
//!
//! These widgets can be pushed to all 4 sides of other widgets,
//! through the use of [`PushSpecs`]. When pushing widgets, you can
//! also include [`Constraint`] in order to get a specific size on the
//! screen for the widget.
//!
//! ```rust
//! # use duat_core::ui::PushSpecs;
//! let specs = PushSpecs::left().with_hor_min(10.0).with_ver_len(2.0);
//! ```
//!
//! When pushing a widget with these `specs` to another widget, Duat
//! will put it on the left, and _try_ to give it a minimum width of
//! `10.0`, and a height of `2.0`.
//!
//! The module also provides 4 native widgets, [`File`] and
//! [`PromptLine`], which can receive user mode, and [`StatusLine`]
//! and [`LineNumbers`] which are not supposed to.
//!
//! These 4 widgets are supposed to be universal, not needing a
//! specific [`Ui`] implementation to work. In contrast, you can
//! create widgets for specific [`Ui`]s. As an example, the
//! [`duat-term`] crate, which is a terminal [`Ui`] implementation for
//! Duat, defines the [`VertRule`] widget, which is a separator that
//! only makes sense in the context of a terminal.
//!
//! This module also describes a [`WidgetCfg`], which is used in
//! widget construction.
//!
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
//! [`OnFileOpen`]: crate::hooks::OnFileOpen
//! [`OnWindowOpen`]: crate::hooks::OnWindowOpen
//! [`Constraint`]: crate::ui::Constraint
pub use self::{
    line_numbers::{LineNum, LineNumbers, LineNumbersOptions},
    notifier::{NotificationsCfg, Notifier},
    prompt_line::{PromptLine, PromptLineCfg},
    status_line::{StateFrom, State, StatusLine, StatusLineCfg, status},
};

mod line_numbers;
mod notifier;
mod prompt_line;
mod status_line;
