#![feature(decl_macro, let_chains)]

use std::{
    path::{Path, PathBuf},
    process::Command,
    sync::{
        Mutex,
        atomic::AtomicU32,
        mpsc::{self, Receiver, Sender},
    },
    time::Duration,
};

use duat::{pre_setup, prelude::*, run_duat};
use duat_core::{data::RwData, ui, widgets::File};
use libloading::os::unix::{Library, Symbol};
use notify::{Event, EventKind, RecursiveMode, Watcher};

static SESSION_TX: Mutex<Option<Sender<ui::Event>>> = Mutex::new(None);

static PROFILE: &str = if cfg!(debug_assertions) {
    "debug"
} else {
    "release"
};

fn main() {
    let statics = Statics::default();
    let (main_tx, main_rx) = mpsc::channel();

    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    if let Some((_watcher, toml_path, so_path)) = dirs_next::config_dir().and_then(|config_dir| {
        let crate_dir = config_dir.join("duat");

        let dbg_path = crate_dir.join("target/debug/libconfig.so");
        let rel_path = crate_dir.join("target/release/libconfig.so");
        let src_path = crate_dir.join("src");
        let toml_path = crate_dir.join("Cargo.toml");

        let mut watcher = notify::recommended_watcher({
            let dbg_path = dbg_path.clone();
            let rel_path = rel_path.clone();
            let toml_path = toml_path.clone();
            move |res| {
                if let Ok(Event { kind: EventKind::Modify(_), .. }) = res {
                    // Reload only the debug build when running in debug mode.
                    // Reload first the debug, then the release build otherwise.
                    if cfg!(debug_assertions) {
                        reload_config(&main_tx, &dbg_path, &toml_path, false);
                    } else if reload_config(&main_tx, &dbg_path, &toml_path, false) {
                        reload_config(&main_tx, &rel_path, &toml_path, true);
                    }
                }
            }
        })
        .unwrap();

        watcher.watch(&src_path, RecursiveMode::Recursive).ok()?;
        watcher
            .watch(&toml_path, RecursiveMode::NonRecursive)
            .ok()?;

        Some((watcher, toml_path, match cfg!(debug_assertions) {
            true => dbg_path,
            false => rel_path,
        }))
    }) {
        run_cargo(&toml_path, !cfg!(debug_assertions)).unwrap();

        let mut library = unsafe { Library::new(&so_path).ok() };
        let mut run_fn = library.as_ref().and_then(find_run_fn);
        let mut prev_files = Vec::new();

        loop {
            let (session_tx, session_rx) = mpsc::channel();
            *SESSION_TX.lock().unwrap() = Some(session_tx.clone());

            let handle = if let Some(run) = run_fn.take() {
                let session_tx = session_tx.clone();
                std::thread::spawn(move || run(prev_files, session_tx, session_rx, statics))
            } else {
                let session_tx = session_tx.clone();
                std::thread::spawn(move || {
                    pre_setup();
                    run_duat(prev_files, session_tx, session_rx, statics)
                })
            };

            prev_files = handle.join().unwrap();

            if let Ok((new_lib, new_run_fn)) = main_rx.try_recv() {
                library.take().unwrap().close().unwrap();
                library = Some(new_lib);
                run_fn = Some(new_run_fn);
            } else {
                break;
            }
        }
    } else {
        let (tx, rx) = mpsc::channel();
        run_duat(Vec::new(), tx, rx, statics);
    }
}

fn reload_config(
    main_tx: &Sender<(Library, Symbol<RunFn>)>,
    so_path: &PathBuf,
    toml_path: &PathBuf,
    on_release: bool,
) -> bool {
    if run_cargo(&toml_path, on_release).is_ok()
        && let Some(library) = unsafe { Library::new(so_path).ok() }
        && let Some(run_fn) = find_run_fn(&library)
    {
        let session_tx = SESSION_TX.lock().unwrap();
        let session_tx = session_tx.as_ref().unwrap();
        main_tx.send((library, run_fn)).unwrap();
        session_tx.send(ui::Event::ReloadConfig).unwrap();
        true
    } else {
        false
    }
}

fn run_cargo(toml_path: &Path, on_release: bool) -> Result<std::process::Output, std::io::Error> {
    let mut cargo = Command::new("cargo");
    cargo.args([
        "build",
        "--quiet",
        "--manifest-path",
        toml_path.to_str().unwrap(),
    ]);

    if !cfg!(debug_assertions) && on_release {
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
