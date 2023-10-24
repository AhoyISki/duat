#![feature(decl_macro, lazy_cell, generic_const_exprs)]
#![allow(incomplete_features)]

use parsec_term::Ui;

mod remapper;
mod utils;
mod widgets;

pub mod prelude {
    pub use hotpatch::*;
    pub use parsec_core::palette::Form;

    pub use crate::{
        remapper::Remapable,
        utils::{config, control, finish, print, SessionStarter},
        widgets::{file_parts::*, CommandLine, LineNumbers, StatusLine},
    };
}
