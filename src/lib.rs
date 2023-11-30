#![feature(decl_macro, lazy_cell, generic_const_exprs)]
#![allow(incomplete_features, dead_code)]

use duat_core::{data::RwData, widgets::File};
pub use utils::run_duat;
mod remapper;
mod utils;
pub mod widgets;

// The main macro to run duat.
pub macro run($($tree:tt)*) {
    use std::sync::mpsc;
    use crate::prelude::duat_core::ui;

    #[no_mangle]
    fn run(
        prev: PrevFiles,
        tx: mpsc::Sender<ui::Event>,
        rx: mpsc::Receiver<ui::Event>,
        statics: <Ui as ui::Ui>::StaticFns
    ) -> PrevFiles {
        {
            $($tree)*
        };

        run_duat(prev, tx, rx, statics)
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
pub type Ui = duat_term::Ui;

pub mod prelude {
    pub use duat_core::{
        self,
        file::File,
        palette::Form,
        position,
        text::{text, Builder, Text},
    };
    #[cfg(feature = "term-ui")]
    pub use duat_term as ui;

    pub use crate::{
        run,
        utils::{control, hook, print, setup},
        widgets::{common::*, status, CommandLine, LineNumbers, StatusLine},
    };
}

type PrevFiles = Vec<(RwData<File>, bool)>;
