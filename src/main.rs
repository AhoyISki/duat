#![feature(decl_macro)]

use std::{
    path::Path,
    process::Command,
    sync::{
        Mutex,
        atomic::AtomicU32,
        mpsc::{self, Receiver, Sender},
    },
};

use duat::{pre_setup, prelude::*, run_duat};
use duat_core::{data::RwData, ui, widgets::File};
use libloading::os::unix::{Library, Symbol};
use notify::{Event, EventKind, RecursiveMode, Watcher};

static BREAK: AtomicU32 = AtomicU32::new(0);
static TX: Mutex<Option<Sender<ui::Event>>> = Mutex::new(None);

static PROFILE: &str = if cfg!(debug_assertions) {
    "debug"
} else {
    "release"
};

fn main() {
    let statics = Statics::default();

    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    if let Some((_watcher, toml_path, so_path)) = dirs_next::config_dir().and_then(|config_dir| {
        let crate_dir = config_dir.join("duat");

        let so_path = crate_dir.join(format!("target/{PROFILE}/libconfig.so"));
        let src_path = crate_dir.join("src");
        let toml_path = crate_dir.join("Cargo.toml");

        let mut watcher = notify::recommended_watcher({
            let so_path = so_path.clone();
            let toml_path = toml_path.clone();
            move |res| {
                if let Ok(Event { kind: EventKind::Modify(_), .. }) = res {
                    if run_cargo(&toml_path).is_ok() {
                        let cur_lib = unsafe { Library::new(&so_path).ok() };
                        if cur_lib.as_ref().and_then(find_run_fn).is_some() {
                            let tx = TX.lock().unwrap();
                            if let Some(tx) = tx.as_ref() {
                                tx.send(ui::Event::ReloadConfig).unwrap();
                                atomic_wait::wake_one(&BREAK);
                            }
                        }
                    }
                }
            }
        })
        .unwrap();

        watcher.watch(&src_path, RecursiveMode::Recursive).ok()?;
        watcher
            .watch(&toml_path, RecursiveMode::NonRecursive)
            .ok()?;

        Some((watcher, toml_path, so_path))
    }) {
        run_cargo(&toml_path).unwrap();

        let mut cur_lib = unsafe { Library::new(&so_path).ok() };
        let mut run = cur_lib.as_ref().and_then(find_run_fn);
        let mut prev_files = Vec::new();

        loop {
            let (tx, rx) = mpsc::channel();
            *TX.lock().unwrap() = Some(tx.clone());

            let handle = if let Some(run) = run.take() {
                let tx = tx.clone();
                std::thread::spawn(move || {
                    let ret = run(prev_files, tx, rx, statics);
                    atomic_wait::wake_all(&BREAK);
                    ret
                })
            } else {
                let tx = tx.clone();
                std::thread::spawn(move || {
                    pre_setup();
                    let ret = run_duat(prev_files, tx, rx, statics);
                    atomic_wait::wake_all(&BREAK);
                    ret
                })
            };

            atomic_wait::wait(&BREAK, 0);

            prev_files = handle.join().unwrap();

            if prev_files.is_empty() {
                break;
            } else {
                if let Some(cur_lib) = cur_lib.take() {
                    cur_lib.close().unwrap()
                }
                cur_lib = unsafe { Library::new(&so_path).ok() };
                run = cur_lib.as_ref().and_then(find_run_fn);
            }
        }
    } else {
        let (tx, rx) = mpsc::channel();
        run_duat(Vec::new(), tx, rx, statics);
    }
}

fn run_cargo(toml_path: &Path) -> Result<std::process::Output, std::io::Error> {
    let mut cargo = Command::new("cargo");
    cargo.args([
        "build",
        "--quiet",
        "--manifest-path",
        toml_path.to_str().unwrap(),
    ]);

    if !cfg!(debug_assertions) {
        cargo.args(["--release"]);
    }

    #[cfg(feature = "deadlocks")]
    cargo.args(["--features", "deadlocks"]);

    #[cfg(feature = "wack")]
    cargo.args(["--features", "wack"]);

    cargo.output()
}

fn find_run_fn(lib: &Library) -> Option<Symbol<RunFn>> {
    unsafe { lib.get::<RunFn>(b"run").ok() }
}

type RunFn = fn(
    Vec<(RwData<File>, bool)>,
    Sender<ui::Event>,
    Receiver<ui::Event>,
    Statics,
) -> Vec<(RwData<File>, bool)>;

type Statics = <duat::Ui as ui::Ui>::StaticFns;
