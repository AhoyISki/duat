#![feature(decl_macro, let_chains)]

use std::{
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::{
        LazyLock,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, Sender},
    },
    time::{Duration, Instant},
};

use dirs_next::{cache_dir, config_dir};
use dlopen_rs::{Dylib, ElfLibrary, OpenFlags, Symbol};
use duat::{pre_setup, prelude::*, run_duat};
use duat_core::{
    data::RwData,
    ui::{self, Ui as UiTrait},
    widgets::File,
};
use notify::{Event, EventKind, RecursiveMode, Watcher};

static MS: LazyLock<<Ui as ui::Ui>::MetaStatics> =
    LazyLock::new(<Ui as ui::Ui>::MetaStatics::default);

fn main() {
    dlopen_rs::init();

    let (ui_tx, ui_rx) = mpsc::channel();
    let (reload_tx, reload_rx) = mpsc::channel();
    let (duat_tx, mut duat_rx) = mpsc::channel();

    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    let (_watcher, toml_path, target_dir) = {
        let Some((config_dir, cache_dir)) = config_dir().zip(cache_dir()) else {
            let (tx, rx) = mpsc::channel();
            pre_setup();
            run_duat(&MS, Vec::new(), tx, rx, ui_tx, None);
            return;
        };

        let crate_dir = config_dir.join("duat");
        let target_dir = cache_dir.join("duat/target");
        let toml_path = crate_dir.join("Cargo.toml");

        let mut watcher = notify::recommended_watcher({
            let reload_tx = reload_tx.clone();
            let duat_tx = duat_tx.clone();
            let target_dir = target_dir.clone();
            let toml_path = toml_path.clone();
            let lock_path = crate_dir.join("Cargo.lock");
            let ignored_target_path = crate_dir.join("target");

            move |res| {
                static IS_RELOADING: AtomicBool = AtomicBool::new(false);
                if let Ok(Event { kind: EventKind::Modify(_), paths, .. }) = res {
                    if IS_RELOADING.load(Ordering::Acquire)
                        || paths
                            .iter()
                            .all(|p| p.starts_with(&ignored_target_path) || p == &lock_path)
                    {
                        return;
                    }
                    IS_RELOADING.store(true, Ordering::Release);
                    // Reload only the debug build when running in debug mode.
                    // Reload first the debug, then the release build otherwise.
                    if cfg!(debug_assertions) {
                        reload_config(&reload_tx, &duat_tx, &target_dir, &toml_path, false);
                    } else if reload_config(&reload_tx, &duat_tx, &target_dir, &toml_path, false) {
                        reload_config(&reload_tx, &duat_tx, &target_dir, &toml_path, true);
                    }
                    // Sleep for one second to prevent too many activations.
                    std::thread::spawn(|| {
                        std::thread::sleep(Duration::new(1, 0));
                        IS_RELOADING.store(false, Ordering::Release);
                    });
                }
            }
        })
        .unwrap();
        watcher.watch(&crate_dir, RecursiveMode::Recursive).unwrap();
        (watcher, toml_path, target_dir)
    };

    run_cargo(&toml_path, &target_dir, !cfg!(debug_assertions)).unwrap();

    let mut prev_files = Vec::new();
    let mut msg = None;

    Ui::open(&MS, ui::Sender::new(duat_tx.clone()), ui_rx);

    let mut on_release = !cfg!(debug_assertions);

    let mut lib = {
        let so_path = target_dir.join(if on_release {
            "release/libconfig.so"
        } else {
            "debug/libconfig.so"
        });
        ElfLibrary::dlopen(so_path, OpenFlags::RTLD_NOW | OpenFlags::RTLD_LOCAL).ok()
    };

    loop {
        let mut run_fn = lib.as_ref().and_then(find_run_fn);

        let duat_tx = duat_tx.clone();
        (prev_files, duat_rx) = if let Some(run_duat) = run_fn.take() {
            let msg = msg.take();
            run_duat(&MS, prev_files, duat_tx, duat_rx, ui_tx.clone(), msg)
        } else {
            let msg = msg.take();
            pre_setup();
            run_duat(&MS, prev_files, duat_tx, duat_rx, ui_tx.clone(), msg)
        };

        if let Ok((so_path, start, new_on_release)) = reload_rx.try_recv() {
            on_release = new_on_release;
            let profile = if on_release { "Release" } else { "Debug" };
            let secs = format!("{:.2}", start.elapsed().as_secs_f32());
            let new_msg = ok!([*a] profile [] " profile reloaded in " [*a] secs "s");
            msg = Some(new_msg);
            match ElfLibrary::from_file(so_path, OpenFlags::CUSTOM_NOT_REGISTER) {
                Ok(elf_lib) => {
                    if let Some(l) = lib.take() {
                        lib = Some(elf_lib.relocate(&[l]).unwrap());
                    }
                }
                Err(err) => duat_core::log_file!("{err:?}"),
            }
        } else {
            break;
        }
    }

    Ui::close(&MS);
}

/// Returns [`true`] if it reloaded the library and run function
fn reload_config(
    reload_tx: &Sender<(PathBuf, Instant, bool)>,
    duat_tx: &Sender<ui::DuatEvent>,
    target_dir: &Path,
    toml_path: &Path,
    on_release: bool,
) -> bool {
    let start = Instant::now();

    let so_path = target_dir.join(if on_release {
        "release/libconfig.so"
    } else {
        "debug/libconfig.so"
    });

    if let Ok(out) = run_cargo(toml_path, target_dir, on_release)
        && out.status.success()
    {
        reload_tx.send((so_path, start, on_release)).unwrap();
        duat_tx.send(ui::DuatEvent::ReloadConfig).unwrap();
        true
    } else {
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

fn find_run_fn(lib: &Dylib) -> Option<Symbol<RunFn>> {
    unsafe { lib.get::<RunFn>("run").ok() }
}

type RunFn = extern "Rust" fn(
    &'static <Ui as ui::Ui>::MetaStatics,
    Vec<(RwData<File>, bool)>,
    Sender<ui::DuatEvent>,
    Receiver<ui::DuatEvent>,
    Sender<ui::UiEvent>,
    Option<Text>,
) -> (Vec<(RwData<File>, bool)>, Receiver<ui::DuatEvent>);
