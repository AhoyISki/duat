//! Options concerning the printing of [`File`]s
//!
//! [`File`]: crate::widgets::File
#[allow(unused_imports)]
use duat_core::cfg::{PrintCfg, WordChars, word_chars as w_chars};

use crate::setup::PRINT_CFG;

#[inline(never)]
pub fn no_wrapping() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.unwrapped(),
        None => PrintCfg::default_for_input().unwrapped(),
    })
}

#[inline(never)]
pub fn wrap_on_width() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.width_wrapped(),
        None => PrintCfg::default_for_input().width_wrapped(),
    })
}

#[inline(never)]
pub fn wrap_on_words() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.word_wrapped(),
        None => PrintCfg::default_for_input().word_wrapped(),
    })
}

#[inline(never)]
pub fn wrap_on_cap(cap: u8) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.cap_wrapped(cap),
        None => PrintCfg::default_for_input().cap_wrapped(cap),
    })
}

#[inline(never)]
pub fn indent_on_wrap() {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.indent_wrapped(),
        None => PrintCfg::default_for_input().indent_wrapped(),
    })
}

#[inline(never)]
pub fn tab_size(tab_size: u8) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.tab_sized(tab_size),
        None => PrintCfg::default_for_input().tab_sized(tab_size),
    })
}

#[inline(never)]
pub fn new_line(char: char) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.new_line_as(char),
        None => PrintCfg::default_for_input().new_line_as(char),
    })
}

#[inline(never)]
pub fn trailing_new_line(char: char) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.trailing_new_line_as(char),
        None => PrintCfg::default_for_input().trailing_new_line_as(char),
    })
}

#[inline(never)]
pub fn scrolloff(x: u8, y: u8) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.with_scrolloff(x, y),
        None => PrintCfg::default_for_input().with_scrolloff(x, y),
    })
}

pub macro word_chars($($w_chars:tt)+) {
    word_chars(w_chars!($($w_chars:tt)+))
}

#[allow(dead_code)]
#[inline(never)]
pub(crate) fn word_chars(word_chars: WordChars) {
    let mut print_cfg = PRINT_CFG.write().unwrap();
    let prev = print_cfg.take();

    *print_cfg = Some(match prev {
        Some(prev) => prev.with_word_chars(word_chars),
        None => PrintCfg::default_for_input().with_word_chars(word_chars),
    })
}
