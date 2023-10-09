#![allow(incomplete_features, clippy::type_complexity)]
#![feature(
    lazy_cell,
    extract_if,
    result_option_inspect,
    trait_upcasting,
    let_chains,
    option_zip,
    control_flow_enum,
    return_position_impl_trait_in_trait,
    decl_macro,
    generic_const_exprs,
    step_trait,
    type_alias_impl_trait,
    negative_impls,
    unboxed_closures
)]

use std::sync::atomic::AtomicBool;

use data::{CurrentFile, CurrentWidget};

pub mod commands;
pub mod data;
pub mod history;
pub mod input;
pub mod palette;
pub mod position;
pub mod session;
pub mod text;
pub mod ui;
pub mod widgets;

pub mod prelude {
    pub use crate::{
        commands, palette,
        session::Session,
        text::{text, PrintCfg},
        widgets::{
            main_byte, main_char, main_col, main_line, status, CommandLine, DynInput, File,
            LineNumbers, StatusLine,
        },
    };
}

// Debugging objects.
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();

// Internal control objects.
static BREAK_LOOP: AtomicBool = AtomicBool::new(false);
static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

// Public control objects.
pub static CURRENT_FILE: CurrentFile = CurrentFile::new();
pub static CURRENT_WIDGET: CurrentWidget = CurrentWidget::new();

/// Internal macro used to log information.
pub macro log_info($($text:tt)*) {{
    use std::{fs, io::Write, time::Instant};
    let mut log = fs::OpenOptions::new().append(true).open("log").unwrap();
    let mut text = format!($($text)*);
    if text.lines().count() > 1 {
        let chars = text.char_indices().filter_map(|(pos, char)| (char == '\n').then_some(pos));
        let nl_indices: Vec<usize> = chars.collect();
        for index in nl_indices.iter().rev() {
            text.insert_str(index + 1, "  ");
        }

        let duration = Instant::now().duration_since(*$crate::DEBUG_TIME_START.get().unwrap());
        write!(log, "\nat {:.4?}:\n  {text}", duration).unwrap();
    } else {
        let duration = Instant::now().duration_since(*$crate::DEBUG_TIME_START.get().unwrap());
        write!(log, "\nat {:.4?}: {text}", duration).unwrap();
    }
}}
