pub use duat_core::input::InputMethod;
use duat_core::widgets::File;

use crate::{setup::CFG_FN, Ui};

#[inline(never)]
pub fn set(input: impl InputMethod<Ui, Widget = File> + Clone) {
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
