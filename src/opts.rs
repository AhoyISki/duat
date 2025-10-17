//! General options for Duat
//!
//! These options apply _globally_, and mostly serve as convenience
//! methods to modify Duat's [widgets]. If you wish to apply them on a
//! case by case way, you should reach out for [hooks].
//!
//! This module is primarily concerned with adding configuration for
//! the following [`Widget`]s:
//!
//! - [`Buffer`], through [`opts::set`].
//! - [`LineNumbers`], through [`opts::set_lines`].
//! - [`StatusLine`], through [`opts::set_status`].
//! - [`PromptLine`], through [`opts::set_prompt`].
//! - [`Notifications`], through [`opts::set_notifs`].
//! - [`LogBook`], through [`opts::set_logs`].
//!
//! [widgets]: crate::widgets
//! [hooks]: crate::hook
//! [`Widget`]: crate::widgets::Widget
//! [`Buffer`]: crate::widgets::Buffer
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`PromptLine`]: crate::widgets::PromptLine
//! [`Notifications`]: crate::widgets::Notifications
//! [`LogBook`]: crate::widgets::LogBook
//! [`opts::set`]: set
//! [`opts::set_lines`]: set_lines
//! [`opts::set_status`]: set_status
//! [`opts::set_prompt`]: set_prompt
//! [`opts::set_notifs`]: set_notifs
//! [`opts::set_logs`]: set_logs
use std::sync::RwLock;

#[allow(unused_imports)]
pub use duat_core::opts::word_chars as w_chars;
pub use duat_core::opts::*;
use duat_utils::widgets::LineNumbersOpts;

/// Options for the [`Buffer`]
///
/// [`Buffer`]: crate::widgets::Buffer
pub(crate) static BUFFER_OPTS: RwLock<PrintOpts> = RwLock::new(PrintOpts::default_for_input());
pub(crate) static LINENUMBERS_OPTS: RwLock<LineNumbersOpts> = RwLock::new(LineNumbersOpts { .. });

/// Change the global [`PrintOpts`] for [`Buffer`]s
///
/// In this function, you can modify the members of `PrintOpts`, which
/// are the following:
///
/// - `opts.wrap_method: WrapMethod` - How to wrap lines in the buffer
///
///   The default is [`WrapMethod::NoWrap`]
///
/// - `opts.indent_wrapped`: bool - Whether to indent wrapped lines
///
///   The default is `true`
///
/// - `opts.tabstop: u8` - How long tabs should be on screen
///
///   The default is `4`
///
/// - `opts.new_line: NewLine` - How to show new lines
///
///   The default is [`NewLine::AlwaysAs(' ')`].
///
/// - `opts.scrolloff: ScrollOff` - How much space to keep between the
///   cursor and edges
///
///   The default is [`ScrollOff { x: 3, y: 3 }`]
///
/// - `opts.force_scrolloff: bool` - Whether to limit scrolloff at the
///   end of lines
///
///   The default is `false`
///
/// - `opts.word_chars: WordChars` - Characters to be considered part
///   of a word  The default is [`word_chars!("A-Za-z0-9_-_")`].
///
/// - `opts.show_ghosts: bool` Whether to show [ghoxt text]
///
///   The default is `true`
///   
/// - `opts.allow_overscroll: bool` Wether to allow the [`Text`] to
///   scroll until only `scrolloff.y` line are on screen
///
///   The default is `true`
///
/// Within the `setup` function, this is how you'd use this function;
///
/// ```rust
/// use duat::prelude::*;
///
/// opts::set(|opts| {
///     opts.tabstop = 2;
///     opts.scrolloff.x = 0;
/// });
/// ```
///
/// If you want to set these options on a [`Buffer`] by `Buffer`
/// basis, you should reach out for [hooks], where the same
///
/// ```rust
/// use duat::prelude::*;
///
/// hook::add::<Buffer>(|pa, handle| {
///     let buffer = handle.write(pa);
///
///     match buffer.filetype() {
///         Some("lua" | "c" | "javascript") => {
///             buffer.opts.tabstop = 2;
///         }
///         Some("markdown") => {
///             buffer.opts.word_chars = word_chars!("A-Za-z0-9_-_---");
///             buffer.opts.wrap_method = WrapMethod::Word;
///         }
///         _ => {}
///     }
///     Ok(())
/// });
/// ```
///
/// More options will come in the future!
///
/// [`Buffer`]: crate::widgets::Buffer
/// [`WrapMethod::NoWrap`]: WrapMethod
/// [`NewLine::AlwaysAs(' ')`]: NewLine
/// [`ScrollOff { x: 3, y: 3 }`]: ScrollOff
/// [`word_chars!("A-Za-z0-9_-_")`]: word_chars
/// [hooks]: crate::hook
pub fn set(set_fn: impl FnOnce(&mut PrintOpts)) {
    set_fn(&mut BUFFER_OPTS.write().unwrap())
}

/// Change the global [`PrintOpts`] for [`LineNumber`]s
///
/// In this function, you can modify the members of `PrintOpts`, which
/// are the following:
///
/// - `relative: bool` - Wether to show relative numbering
///
///   The default is `false`
///
/// - `align: std::fmt::Alignment` Where to align the numbers
///
///   The default is [`std::fmt::Alignment::Left`]
///
/// - `main_align: std::fmt::Alignment` Where to align the main line
///   number
///
///   The default is [`std::fmt::Alignment::Right`]
///
/// - `opts.show_wraps: bool` - Wether to show wrapped line's numbers
///
///   The default is `false`
///
/// - `opts.on_the_right: bool` - Place this [`Widget`] on the right,
///   as opposed to on the left
///
///   The default is `false`
///   
/// Within the `setup` function, this is how you'd use this function;
///
/// ```rust
/// use duat::prelude::*;
///
/// opts::set_lines(|opts| {
///     opts.align = std::fmt::Alignment::Right;
///     opts.relative = true
/// });
/// ```
///
/// If you want to set these options on a [`Buffer`] by `Buffer`
/// basis, you should reach out for [hooks], where the same
///
/// ```rust
/// use duat::prelude::*;
///
/// hook::add::<LineNumbers>(|pa, handle| {
///     let filetype = handle.buffer()?.filetype(pa);
///
///     handle.write(pa).relative = match filetype {
///         Some("cpp" | "rust") => true,
///         _ => false
///     };
///     Ok(())
/// });
/// ```
///
/// [`Buffer`]: crate::widgets::Buffer
/// [`LineNumber`]: crate::widgets::LineNumbers
/// [hooks]: crate::hook
pub fn set_lines(set_fn: impl FnOnce(&mut LineNumbersOpts)) {
    set_fn(&mut LINENUMBERS_OPTS.write().unwrap())
}

pub fn set_status(set_fn: impl FnOnce(&mut StatusLineOpts)) {
}
