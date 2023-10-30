#![feature(decl_macro, lazy_cell, generic_const_exprs)]
#![allow(incomplete_features, dead_code)]
use std::sync::mpsc;

use parsec_core::{data::RwData, widgets::File};
pub use utils::run_parsec;
mod remapper;
mod utils;
mod widgets;

// The main macro to run parsec.
pub macro run($($tree:tt)*) {
    #[no_mangle]
    fn run(prev: PrevFiles, rx: mpsc::Receiver<()>) -> PrevFiles {
        {
            $($tree)*
        };

        run_parsec(prev, rx)
    }
}

// This will eventually be a NOT AND to check if any Uis have been
// chosen at all.
// Later, I'll also have an XOR checker to make sure only one Ui was
// chosen.
#[cfg(not(feature = "term-ui"))]
compile_error! {
    "No ui has been chosen to compile Parsec with."
}

#[cfg(feature = "term-ui")]
pub type Ui = parsec_term::Ui;

pub mod prelude {
    pub use parsec_core::{
        self as core,
        file::File,
        palette::Form,
        position,
        text::{text, Builder, Text},
    };
    #[cfg(feature = "term-ui")]
    pub use parsec_term as ui;

    pub use crate::{
        run,
        utils::{control, hook, print, setup},
        widgets::{common::*, status, CommandLine, LineNumbers, StatusLine},
    };
}

type PrevFiles = Vec<(RwData<File<Ui>>, bool)>;
type RunFn = fn(PrevFiles, rx: mpsc::Receiver<()>) -> PrevFiles;