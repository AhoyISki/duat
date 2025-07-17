//! Options concerning the printing of [`File`]s
//!
//! [`File`]: crate::widgets::File
#[allow(unused_imports)]
use duat_core::cfg::word_chars as w_chars;
pub use duat_core::cfg::{PrintCfg, WordChars};

use crate::setup::PRINT_CFG;

/// Disables wrapping for all [`File`]s
///
/// [`File`]: crate::widgets::File
#[inline(never)]
pub fn no_wrapping() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.dont_wrap(),
        None => *PrintCfg::default_for_input().dont_wrap(),
    })
}

/// Wraps [`File`]s on the right edge of the area
///
/// [`File`]: crate::widgets::File
#[inline(never)]
pub fn wrap_on_edge() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.wrap_on_edge(),
        None => *PrintCfg::default_for_input().wrap_on_edge(),
    })
}

/// Wraps [`File`]s on [word] terminations
///
/// [`File`]: crate::widgets::File
/// [word]: word_chars!
#[inline(never)]
pub fn wrap_on_words() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.wrap_on_word(),
        None => *PrintCfg::default_for_input().wrap_on_word(),
    })
}

/// Wraps [`File`]s a certain distance from the left edge
///
/// This can wrap beyond the screen, being a mix of [`no_wrapping`]
/// and [`wrap_on_edge`].
///
/// [`File`]: crate::widgets::File
#[inline(never)]
pub fn wrap_on_cap(cap: u8) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.wrap_at(cap),
        None => *PrintCfg::default_for_input().wrap_at(cap),
    })
}

/// Reindent wrapped lines on [`File`]s to the same level of
/// indentation
///
/// [`File`]: crate::widgets::File
#[inline(never)]
pub fn indent_wraps(value: bool) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.indent_wraps(value),
        None => *PrintCfg::default_for_input().indent_wraps(value),
    })
}

/// Sets the size of tabs
#[inline(never)]
pub fn tabstop(tab_size: u8) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.set_tabstop(tab_size),
        None => *PrintCfg::default_for_input().set_tabstop(tab_size),
    })
}

/// Sets a character to replace `'\n'`s with
#[inline(never)]
pub fn new_line(char: char) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.new_line_as(char),
        None => *PrintCfg::default_for_input().new_line_as(char),
    })
}

/// Sets a character to replace `'\n'` only with trailing white space
#[inline(never)]
pub fn trailing_new_line(char: char) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.trailing_new_line_as(char),
        None => *PrintCfg::default_for_input().trailing_new_line_as(char),
    })
}

/// Sets the scrolloff for [`File`]s
///
/// [`File`]: crate::widgets::File
#[inline(never)]
pub fn scrolloff(x: u8, y: u8) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.set_scrolloff(x, y),
        None => *PrintCfg::default_for_input().set_scrolloff(x, y),
    })
}

/// Returns a new [`WordChars`] composed of inclusive ranges of
/// [`char`]s
///
/// The syntax (as well as the default definition) is as follows:
///
/// ```rust
/// # use duat::print::word_chars;
/// word_chars!("a-zA-Z0-9_-_");
/// ```
pub macro word_chars($($w_chars:tt)+) {
    set_word_chars(w_chars!($($w_chars)+))
}

/// Sets the [`WordChars`]
#[doc(hidden)]
#[allow(dead_code)]
#[inline(never)]
pub fn set_word_chars(word_chars: WordChars) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(mut prev) => *prev.set_word_chars(word_chars),
        None => *PrintCfg::default_for_input().set_word_chars(word_chars),
    })
}
