#![feature(
    return_position_impl_trait_in_trait,
    decl_macro,
    lazy_cell,
    generic_const_exprs
)]
#![allow(incomplete_features)]

use std::sync::mpsc;

use hotpatch::patchable;
use parsec_core::{session::SessionCfg, Globals};
use utils::{default_cfg_fn, CfgFn, UiFn, CFG_FN, GLOBALS, UI_FN};

mod remapper;
mod utils;
mod widgets;

pub mod prelude {
    pub use hotpatch::*;
    pub use parsec_core::palette::Form;

    pub use crate::{
        finish,
        remapper::Remapable,
        utils::{config, control, print},
        widgets::{file_parts::*, CommandLine, LineNumbers, StatusLine},
        SessionStarter,
    };
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
type Ui = parsec_term::Ui;

#[allow(dead_code)]
fn main() {
    let starter: SessionStarter = parsec();

    let ui = match starter.ui_fn.write().unwrap().take() {
        Some(ui_fn) => ui_fn(),
        None => Ui::default(),
    };

    let mut cfg = SessionCfg::__new(ui, starter.globals);

    match starter.cfg_fn.write().unwrap().take() {
        Some(cfg_fn) => cfg_fn(&mut cfg),
        None => default_cfg_fn(&mut cfg),
    }

    let (tx, rx) = mpsc::channel();

    let mut session = cfg.session_from_args();

    session.start(true, rx);
}

#[patchable]
pub fn parsec() -> SessionStarter {
    finish()
}

pub struct SessionStarter {
    globals: Globals<Ui>,
    ui_fn: &'static UiFn,
    cfg_fn: &'static CfgFn,
}

pub fn finish() -> SessionStarter {
    SessionStarter {
        globals: GLOBALS,
        ui_fn: &UI_FN,
        cfg_fn: &CFG_FN,
    }
}
