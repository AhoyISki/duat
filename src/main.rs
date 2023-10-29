#![feature(decl_macro, lazy_cell, generic_const_exprs)]
#![allow(incomplete_features, dead_code)]
use std::{
    process::Command,
    sync::{
        atomic::{AtomicBool, AtomicU32, Ordering},
        mpsc,
    },
};

use libloading::os::unix::{Library, Symbol};
use notify::{Event, EventKind, RecursiveMode, Watcher};
use parsec_core::{session::Session, log_info};
pub use utils::run_parsec;
mod remapper;
mod utils;
mod widgets;

static FILES_CHANGED: AtomicBool = AtomicBool::new(false);
static BREAK: AtomicU32 = AtomicU32::new(NO);

fn main() {
    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    if let Some((_watcher, toml_path, so_path)) = dirs_next::config_dir().and_then(|config_dir| {
        let crate_dir = config_dir.join("parsec");

        let so_path = crate_dir.join("target/release/libconfig.so");
        let src_path = crate_dir.join("src");
        let toml_path = crate_dir.join("Cargo.toml");

        let mut watcher = notify::recommended_watcher(|res| match res {
            Ok(Event {
                kind: EventKind::Modify(_),
                ..
            }) => {
                FILES_CHANGED.store(true, Ordering::Relaxed);
                atomic_wait::wake_all(&BREAK);
            }
            Ok(_) | Err(_) => {}
        })
        .unwrap();

        watcher.watch(&src_path, RecursiveMode::Recursive).ok()?;
        watcher
            .watch(&toml_path, RecursiveMode::NonRecursive)
            .ok()?;

        Some((watcher, toml_path, so_path))
    }) {
        let mut cur_lib = unsafe { Library::new(&so_path).ok() };
        #[allow(unused_assignments)]
        let mut run = cur_lib.as_ref().and_then(find_run_fn);
        let mut is_first = true;

        loop {
            let (tx, rx) = mpsc::channel();

            let handle = if let Some(run) = run.take() {
                std::thread::spawn(move || {
                    let ret = run(is_first, rx);
                    atomic_wait::wake_one(&BREAK);
                    run.into_raw();
                    ret
                })
            } else {
                std::thread::spawn(move || {
                    let ret = run_parsec(is_first, rx);
                    BREAK.store(YES, Ordering::Relaxed);
                    atomic_wait::wake_one(&BREAK);
                    ret
                })
            };

            loop {
                atomic_wait::wait(&BREAK, NO);

                if !FILES_CHANGED.load(Ordering::Relaxed) {
                    break;
                }
                FILES_CHANGED.store(false, Ordering::Relaxed);

                let mut cargo = Command::new("cargo");
                cargo.args([
                    "build",
                    "--release",
                    "--quiet",
                    "--manifest-path",
                    toml_path.to_str().unwrap(),
                ]);

                let output = cargo.output();

                if output.is_ok() {
                    let lib = unsafe { Library::new(&so_path).ok() };
                    if lib.as_ref().and_then(find_run_fn).is_some() {
                        tx.send(()).unwrap();
                        break;
                    } else {
                        panic!();
                    }
                }
            }

            if handle.join().unwrap().is_some() {
                cur_lib.take().unwrap().close().unwrap();
                cur_lib = unsafe { Library::new(&so_path).ok() };

                run = cur_lib.as_ref().and_then(find_run_fn);
                is_first = false;
            } else {
                break;
            }
        }
    } else {
        let (_tx, rx) = mpsc::channel();
        run_parsec(true, rx);
    }
}

fn find_run_fn(
    lib: &Library,
) -> Option<Symbol<fn(bool, mpsc::Receiver<()>) -> Option<Session<Ui>>>> {
    unsafe {
        lib.get::<fn(bool, mpsc::Receiver<()>) -> Option<Session<Ui>>>(b"run")
            .ok()
    }
}

// The main macro to run parsec.
pub macro run($($tree:tt)*) {
    #[no_mangle]
    fn run(is_first: bool, rx: mpsc::Receiver<()>) -> Option<Session<Ui>> {
        {
            $($tree)*
        };

        run_parsec(is_first, rx)
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
        self,
        file::File,
        palette::Form,
        position,
        text::{text, Builder, Text},
    };

    pub use crate::{
        run,
        utils::{control, hook, print, setup},
        widgets::{common::*, status, CommandLine, LineNumbers, StatusLine},
    };
}

const NO: u32 = 0;
const YES: u32 = 1;
