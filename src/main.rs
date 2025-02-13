#![feature(decl_macro, let_chains)]

use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, Sender},
    },
    time::Instant,
};

use dirs_next::{cache_dir, config_dir};
use duat::{pre_setup, prelude::*, run_duat};
use duat_core::{data::RwData, ui, widgets::File};
use libloading::os::unix::{Library, Symbol};
use notify::{Event, EventKind, RecursiveMode, Watcher};

static SESSION_TX: Mutex<Option<Sender<ui::Event>>> = Mutex::new(None);

fn main() {
    let statics = Statics::default();
    let (main_tx, main_rx) = mpsc::channel();

    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    let (_watcher, toml_path, target_dir, so_path) = {
        let Some((config_dir, cache_dir)) = config_dir().zip(cache_dir()) else {
            let (tx, rx) = mpsc::channel();
            pre_setup();
            run_duat(Vec::new(), tx, rx, statics, None);
            return;
        };
        let crate_dir = config_dir.join("duat");
        let target_dir = cache_dir.join("duat/target");
        let toml_path = crate_dir.join("Cargo.toml");

        let mut watcher = notify::recommended_watcher({
            let target_dir = target_dir.clone();
            let toml_path = toml_path.clone();
            let lock_path = crate_dir.join("Cargo.lock");

            move |res| {
                if let Ok(Event { kind: EventKind::Modify(_), paths, .. }) = res {
                    // If the only thing that changed was the lock file, ignore.
                    if paths.iter().eq([&lock_path].into_iter()) {
                        return;
                    }
                    // Reload only the debug build when running in debug mode.
                    // Reload first the debug, then the release build otherwise.
                    if cfg!(debug_assertions) {
                        reload_config(&main_tx, &target_dir, &toml_path, false);
                    } else if reload_config(&main_tx, &target_dir, &toml_path, false) {
                        reload_config(&main_tx, &target_dir, &toml_path, true);
                    }
                }
            }
        })
        .unwrap();

        watcher.watch(&crate_dir, RecursiveMode::Recursive).unwrap();

        let so_path = match cfg!(debug_assertions) {
            true => target_dir.join("debug/libconfig.so"),
            false => target_dir.join("release/libconfig.so"),
        };
        (watcher, toml_path, target_dir, so_path)
    };

    run_cargo(&toml_path, &target_dir, !cfg!(debug_assertions)).unwrap();

    let mut library = unsafe { Library::new(&so_path).ok() };
    let mut run_fn = library.as_ref().and_then(find_run_fn);
    let mut prev_files = Vec::new();
    let mut msg = None;

    loop {
        let (session_tx, session_rx) = mpsc::channel();
        *SESSION_TX.lock().unwrap() = Some(session_tx.clone());

        let handle = if let Some(run) = run_fn.take() {
            let session_tx = session_tx.clone();
            let msg = msg.take();
            std::thread::spawn(move || run(prev_files, session_tx, session_rx, statics, msg))
        } else {
            let session_tx = session_tx.clone();
            let msg = msg.take();
            std::thread::spawn(move || {
                pre_setup();
                run_duat(prev_files, session_tx, session_rx, statics, msg)
            })
        };

        prev_files = handle.join().unwrap();

        if let Ok((so_path, start, on_release)) = main_rx.try_recv() {
            library.take().unwrap().close().unwrap();
            library = Some(unsafe { Library::new(&so_path).ok().unwrap() });
            run_fn = find_run_fn(library.as_ref().unwrap());

            let profile = if on_release { "Release" } else { "Debug" };
            let secs = format!("{:.2}", start.elapsed().as_secs_f32());
            let new_msg = ok!([*a] profile [] " profile reloaded in " [*a] secs "s");

            msg = Some(new_msg);
        } else {
            break;
        }
    }
}

/// Returns [`true`] if it reloaded the library and run function
fn reload_config(
    main_tx: &Sender<(PathBuf, Instant, bool)>,
    target_dir: &Path,
    toml_path: &Path,
    on_release: bool,
) -> bool {
    static ON_ALT_PATH: AtomicBool = AtomicBool::new(false);
    let oap = ON_ALT_PATH.fetch_not(Ordering::Relaxed);
    let target_dir = target_dir.join(if oap { "target1" } else { "target0" });
    let start = Instant::now();

    let so_path = target_dir.join(if on_release {
        "release/libconfig.so"
    } else {
        "debug/libconfig.so"
    });

    if let Ok(out) = run_cargo(toml_path, &target_dir, on_release)
        && out.status.success()
        && let Some(library) = unsafe { Library::new(&so_path).ok() }
        && find_run_fn(&library).is_some()
    {
        let session_tx = SESSION_TX.lock().unwrap();
        let session_tx = session_tx.as_ref().unwrap();
        main_tx.send((so_path, start, on_release)).unwrap();
        session_tx.send(ui::Event::ReloadConfig).unwrap();
        true
    } else {
        ON_ALT_PATH.fetch_not(Ordering::Relaxed);
        false
    }
}

fn run_cargo(
    toml_path: &Path,
    target_dir: &Path,
    on_release: bool,
) -> Result<Output, std::io::Error> {
    let mut cargo = Command::new("cargo");
    cargo.args([
        "build",
        "--manifest-path",
        toml_path.to_str().unwrap(),
        "--target-dir",
        target_dir.to_str().unwrap(),
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
    Option<Text>,
) -> Vec<(RwData<File>, bool)>;

type Statics = <duat::Ui as ui::Ui>::StaticFns;
