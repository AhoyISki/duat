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
use duat::{Messengers, MetaStatics, pre_setup, prelude::*, run_duat};
use duat_core::{
    Mutex,
    clipboard::Clipboard,
    session::FileRet,
    ui::{self, DuatEvent, Ui as UiTrait},
};
use notify::{Event, EventKind, RecursiveMode, Watcher};

static MS: LazyLock<<Ui as ui::Ui>::MetaStatics> =
    LazyLock::new(<Ui as ui::Ui>::MetaStatics::default);
static CLIPB: LazyLock<Mutex<Clipboard>> = LazyLock::new(|| Mutex::new(Clipboard::new().unwrap()));

fn main() {
    dlopen_rs::init();

    let (ui_tx, ui_rx) = mpsc::channel();
    let (reload_tx, reload_rx) = mpsc::channel();
    let (duat_tx, mut duat_rx) = mpsc::channel();
    let duat_tx = Box::leak(Box::new(duat_tx));

    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    let (_watcher, toml_path, target_dir) = {
        let Some((config_dir, cache_dir)) = config_dir().zip(cache_dir()) else {
            pre_setup();
            run_duat((&MS, &CLIPB), Vec::new(), (duat_tx, duat_rx, ui_tx));
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

    let mut prev = Vec::new();

    Ui::open(&MS, ui::Sender::new(duat_tx), ui_rx);

    let mut lib = {
        let so_path = target_dir.join(if !cfg!(debug_assertions) {
            "release/libconfig.so"
        } else {
            "debug/libconfig.so"
        });
        ElfLibrary::dlopen(so_path, OpenFlags::RTLD_NOW | OpenFlags::RTLD_LOCAL).ok()
    };

    loop {
        let run_lib = lib.take();
        let mut run_fn = run_lib.as_ref().and_then(find_run_duat);

        (prev, duat_rx) = if let Some(run_duat) = run_fn.take() {
            run_duat((&MS, &CLIPB), prev, (duat_tx, duat_rx, ui_tx.clone()))
        } else {
            pre_setup();
            run_duat((&MS, &CLIPB), prev, (duat_tx, duat_rx, ui_tx.clone()))
        };
        drop(run_lib);

        if let Ok((so_path, start, on_release)) = reload_rx.try_recv() {
            let profile = if on_release { "Release" } else { "Debug" };
            let secs = format!("{:.2}", start.elapsed().as_secs_f32());
            let msg = ok!([*a] profile [] " profile reloaded in " [*a] secs "s");
            duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
            lib = ElfLibrary::dlopen(so_path, OpenFlags::RTLD_NOW | OpenFlags::RTLD_LOCAL).ok();
        } else {
            break;
        }
    }

    Ui::close(&MS);
}

/// Returns [`true`] if it reloaded the library and run function
fn reload_config(
    reload_tx: &Sender<(PathBuf, Instant, bool)>,
    duat_tx: &Sender<DuatEvent>,
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
        duat_tx.send(DuatEvent::ReloadConfig).unwrap();
        true
    } else {
        let msg = err!("Failed to compile " [*a] "config" [] " crate");
        duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
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

fn find_run_duat(lib: &Dylib) -> Option<Symbol<RunFn>> {
    unsafe { lib.get::<RunFn>("run").ok() }
}

type RunFn = fn(MetaStatics, Vec<FileRet>, Messengers) -> (Vec<FileRet>, Receiver<DuatEvent>);
