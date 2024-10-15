//! Options concerning the [`File`]'s [`InputMethod`]
//!
//! [`File`]: crate::widgets::File
use duat_core::input::InputForFiles;
pub use duat_core::input::InputMethod;

use crate::{setup::CFG_FN, Ui};

#[inline(never)]
pub fn set(input: impl InputForFiles<Ui> + Clone) {
    let mut cfg_fn = CFG_FN.write().unwrap();
    let prev = cfg_fn.take();

    *cfg_fn = Some(match prev {
        Some(prev_fn) => Box::new(move |cfg| {
            prev_fn(cfg);
            cfg.set_input(input)
        }),
        None => Box::new(move |cfg| cfg.set_input(input)),
    })
}
