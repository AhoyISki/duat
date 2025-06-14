//! Core [`Widget`]s for usage in Duat
//!
//! The widgets defined in this module are expected to be used in
//! pretty much every `config` of Duat, since they are Duat
//! equivalents to what is commonly found in other text editors.
//!
//! 
//!
//! [`Widget`]: duat_core::widget::Widget
//! [`Text`]: duat_core::text::Text
//! [`File`]: duat_core::file::File
//! [`OnFileOpen`]: duat_core::hook::OnFileOpen
//! [`OnWindowOpen`]: duat_core::hook::OnWindowOpen
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
//! [`OnFileOpen`]: crate::hooks::OnFileOpen
//! [`OnWindowOpen`]: crate::hooks::OnWindowOpen
//! [`Constraint`]: crate::ui::Constraint
pub use self::{
    line_numbers::{LineNum, LineNumbers, LineNumbersOptions},
    notifications::{Notifications, NotificationsCfg},
    prompt_line::{PromptLine, PromptLineCfg},
    status_line::{State, StatusLine, StatusLineCfg, status},
};

mod line_numbers;
mod notifications;
mod prompt_line;
mod status_line;
