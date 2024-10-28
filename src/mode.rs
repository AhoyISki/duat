//! Options concerning the [`File`]'s [`Mode`]
//!
//! [`File`]: crate::widgets::File
//! [`ode`]: duat_core::input::Mode
pub use duat_core::input::*;
use duat_core::{commands, input, widgets::CmdLineMode};

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

pub fn map<M: Mode<Ui>>(take: impl Into<Vec<KeyEvent>>, give: impl Into<Vec<KeyEvent>>) {
    input::map::<M, Ui>(take, give);
}
