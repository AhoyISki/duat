//! Controls for Duat
//!
//! This module contains the expected commands of a
//! text editor that don't involve particular widgets
//! or other more specific concepts
//!
//! All of the functions listed in here also have a [command]
//! equivalent, that you can call from the [`CommandLine`].
//!
//! [command]: duat_core::commands
//! [`CommandLine`]: crate::prelude::CommandLine
use std::fmt::Display;

use duat_core::{duat_name, text::Text, widgets::ActiveWidget};

use crate::{setup::COMMANDS, Ui};

/// Canonical way to quit Duat.
///
/// By calling the quit command, all threads will finish their
/// tasks, and then Duat will execute a program closing
/// function, as defined by the [`Ui`].
#[inline(never)]
pub fn quit() {
    COMMANDS.run("quit").unwrap();
}

/// Switches to the given [`ActiveWidget`].
///
/// The widget will be chosen in the following order:
///
/// 1. The first of said widget pushed to the current [`File`].
/// 2. Other instances of it in the current window.
/// 3. Instances in other windows.
///
/// [`File`]: duat_core::file::File
/// [`Session`]: crate::sessSession
#[inline(never)]
pub fn switch_to<W: ActiveWidget<Ui>>() -> Result<Option<Text>> {
    COMMANDS.run(format!("switch-to {}", duat_name::<W>()))
}

/// Switches to/opens a [`File`] with the given name.
///
/// If you wish to specifically switch to files that are already
/// open, use [`commands::buffer`].
///
/// If there are more arguments, they will be ignored.
///
/// [`File`]: duat_core::file::File
/// [`commands::buffer`]: buffer
#[inline(never)]
pub fn edit(file: impl Display) -> Result<Option<Text>> {
    COMMANDS.run(format!("edit {}", file))
}

/// Switches to a [`File`] with the given name.
///
/// If there is no file open with that name, does nothing. Use
/// [`commands::edit`] if you wish to open files.
///
/// If there are more arguments, they will be ignored.
///
/// [`File`]: duat_core::file::File
/// [`commands::edit`]: edit
#[inline(never)]
pub fn buffer(file: impl Display) -> Result<Option<Text>> {
    COMMANDS.run(format!("buffer {}", file))
}

/// Switches to the next [`File`].
///
/// This function will only look at files that are opened in the
/// current window. If you want to include other windows in the
/// search, use [`commands::next_global_file`].
///
/// [`File`]: duat_core::file::File
/// [`commands::next_global_file`]: next_global_file
#[inline(never)]
pub fn next_file() -> Result<Option<Text>> {
    COMMANDS.run("next-file")
}

/// Switches to the previous [`File`].
///
/// This function will only look at files that are opened in the
/// current window. If you want to include other windows in the
/// search, use [`commands::prev_global_file`].
///
/// [`File`]: duat_core::file::File
/// [`commands::prev_global_file`]: prev_global_file
#[inline(never)]
pub fn prev_file() -> Result<Option<Text>> {
    COMMANDS.run("prev-file")
}

/// Switches to the next [`File`].
///
/// This function will look for files in all windows. If you want
/// to limit the search to just the current window, use
/// [`commands::next_file`].
///
/// [`File`]: duat_core::file::File
/// [`commands::next_file`]: next_file
#[inline(never)]
pub fn next_global_file() -> Result<Option<Text>> {
    COMMANDS.run("next-file --global")
}

/// Switches to the previous [`File`].
///
/// This function will look for files in all windows. If you want
/// to limit the search to just the current window, use
/// [`commands::prev_file`].
///
/// [`File`]: duat_core::file::File
/// [`commands::prev_file`]: prev_file
#[inline(never)]
pub fn prev_global_file() -> Result<Option<Text>> {
    COMMANDS.run("prev-file --global")
}

/// If not in a [`File`], switches to the last active [`File`].
///
/// This is useful if the currently active widget is not a file
/// (e.g. [`CommandLine`], a file tree, etc), and you want to
/// return to the file seamlessly.
///
/// [`File`]: duat_core::file::File
/// [`CommandLine`]: crate::widgets::CommandLine
#[inline(never)]
pub fn return_to_file() -> Result<Option<Text>> {
    COMMANDS.run("return-to-file")
}

/// Tries to alias a `caller` to an existing `command`.
///
/// Returns an [`Err`] if the `alias` is already a caller for
/// another command, or if `command` is not a real caller to an
/// exisiting [`Command`].
#[inline(never)]
pub fn alias(alias: impl ToString, command: impl ToString) -> Result<Option<Text>> {
    COMMANDS.alias(alias, command)
}

type Result<T> = duat_core::Result<T, ()>;
