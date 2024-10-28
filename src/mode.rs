//! Options concerning the [`File`]'s [`Mode`]
//!
//! [`File`]: crate::widgets::File
//! [`Mode`]: duat_core::input::Mode
pub use duat_core::input::*;
use duat_core::{commands, widgets::CmdLineMode};

use crate::Ui;

pub fn set_default(mode: impl Mode<Ui>) {
    commands::set_default_mode(mode);
}

pub fn set(mode: impl Mode<Ui>) {
    commands::set_mode(mode);
}

pub fn set_cmd(mode: impl CmdLineMode<Ui>) {
    commands::set_cmd_mode(mode);
}

pub fn reset() {
    commands::reset_mode();
}
