//! Options concerning the [`File`]'s [`Mode`]
//!
//! [`File`]: crate::widgets::File
//! [`Mode`]: duat_core::input::Mode
pub use duat_core::mode::*;
use duat_core::{mode, widgets::CmdLineMode};

use crate::Ui;

pub fn set_default(mode: impl Mode<Ui>) {
    mode::set_default(mode);
}

pub fn set(mode: impl Mode<Ui>) {
    mode::set(mode);
}

pub fn set_cmd(mode: impl CmdLineMode<Ui>) {
    mode::set_cmd(mode);
}

pub fn reset() {
    mode::reset();
}

pub fn map<M: Mode<Ui>>(take: &str, give: impl AsGives<Ui>) {
    mode::map::<M, Ui>(take, give);
}

pub fn alias<M: Mode<Ui>>(take: &str, give: impl AsGives<Ui>) {
    mode::alias::<M, Ui>(take, give);
}
