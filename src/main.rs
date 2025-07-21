//! The runner for Duat
#![feature(decl_macro)]

use std::{
    path::PathBuf,
    process::Command,
    sync::{
        LazyLock, Mutex,
        mpsc::{self, Receiver},
    },
    time::Instant,
};

use duat::{DuatChannel, Initials, MetaStatics, pre_setup, prelude::*, run_duat};
use duat_core::{
    clipboard::Clipboard,
    context,
    session::FileRet,
    ui::{self, DuatEvent, Ui as UiTrait},
};
use libloading::{Library, Symbol};
use notify::{
    Event, EventKind,
    RecursiveMode::*,
    Watcher,
    event::{AccessKind, AccessMode},
};

static CLIPB: LazyLock<Mutex<Clipboard>> = LazyLock::new(Mutex::default);

fn main() {
    // Initializers for access to static variables across two different
    // "duat-core instances"
    let logs = duat_core::context::Logs::new();
    log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
    context::set_logs(logs.clone());

    let forms_init = duat_core::form::get_initial();
    duat_core::form::set_initial(forms_init);

    let hooks_init = duat_core::hook::InnerHooks::default();
    duat_core::hook::set_initial(hooks_init);

    let ms: &'static <Ui as ui::Ui>::MetaStatics =
        Box::leak(Box::new(<Ui as ui::Ui>::MetaStatics::default()));

    let (reload_tx, reload_rx) = mpsc::channel();
    let (duat_tx, mut duat_rx) = mpsc::channel();
    let duat_tx = Box::leak(Box::new(duat_tx));

    let mut prev = Vec::new();

    // Assert that the configuration crate actually exists.
    let Some(crate_dir) = duat_core::crate_dir().ok().filter(|cd| cd.exists()) else {
        context::error!("No config crate found, loading default config");
        pre_setup(None, duat_tx);
        run_duat((ms, &CLIPB), Vec::new(), duat_rx);
        return;
    };

    let mut lib = {
        let so_dir = crate_dir.join(if cfg!(debug_assertions) {
            "target/debug"
        } else {
            "target/release"
        });

        let libconfig_path = so_dir.join("libconfig.so");

        if let Ok(false) | Err(_) = libconfig_path.try_exists() {
            println!("Compiling config crate for the first time, this might take a while...");
            let toml_path = crate_dir.join("Cargo.toml");
            if let Ok(status) = run_cargo(toml_path.clone(), true, true)
                && status.success()
            {
                context::info!("Compiled [a]release[] profile");
            } else {
                context::error!("Failed to compile [a]release[] profile");
            }
        }

        Some(unsafe { Library::new(libconfig_path) }.unwrap())
    };

    // The watcher is returned as to not be dropped.
    let _watcher = spawn_watcher(reload_tx, duat_tx, crate_dir);

    Ui::open(ms, ui::Sender::new(duat_tx));

    loop {
        let running_lib = lib.take();
        let mut run_fn = running_lib.as_ref().and_then(find_run_duat);

        let reload_instant;
        (prev, duat_rx, reload_instant) = std::thread::scope(|s| {
            s.spawn(|| {
                if let Some(run_duat) = run_fn.take() {
                    let initials = (logs.clone(), forms_init, hooks_init);
                    let channel = (&*duat_tx, duat_rx);
                    run_duat(initials, (ms, &CLIPB), prev, channel)
                } else {
                    context::error!("Failed to load config crate");
                    pre_setup(None, duat_tx);
                    run_duat((ms, &CLIPB), prev, duat_rx)
                }
            })
            .join()
            .unwrap()
        });

        duat_core::form::clear();

        if let Some(lib) = running_lib {
            lib.close().unwrap();
        }

        if prev.is_empty() {
            break;
        }

        let (so_path, on_release) = reload_rx.recv().unwrap();

        let profile = if on_release { "Release" } else { "Debug" };
        let time = match reload_instant {
            Some(reload_instant) => txt!(" in [a]{:.2?}", reload_instant.elapsed()),
            None => Text::builder(),
        };
        context::info!("[a]{profile}[] profile reloaded{time}");
        lib = unsafe { Library::new(so_path) }.ok();
    }

    Ui::close(ms);
}

fn spawn_watcher(
    reload_tx: mpsc::Sender<(PathBuf, bool)>,
    duat_tx: &mut mpsc::Sender<DuatEvent>,
    crate_dir: &'static std::path::Path,
) -> (notify::RecommendedWatcher, &'static std::path::Path) {
    std::fs::create_dir_all(crate_dir.join("target/debug")).unwrap();
    std::fs::create_dir_all(crate_dir.join("target/release")).unwrap();

    let mut watcher = notify::recommended_watcher({
        let reload_tx = reload_tx.clone();
        let duat_tx = duat_tx.clone();
        let mut sent_reload = false;

        move |res| match res {
            Ok(Event { kind: EventKind::Create(_), paths, .. }) => {
                if let Some(so_path) = paths.iter().find(|p| p.ends_with("libconfig.so")) {
                    let on_release = so_path.ends_with("release/libconfig.so");
                    reload_tx.send((so_path.clone(), on_release)).unwrap();
                    sent_reload = true;
                }
            }
            Ok(Event {
                kind: EventKind::Access(AccessKind::Close(AccessMode::Write)),
                paths,
                ..
            }) if paths.iter().any(|p| p.ends_with(".cargo-lock")) && sent_reload => {
                duat_tx.send(DuatEvent::ReloadConfig).unwrap();
                sent_reload = false;
            }
            _ => {}
        }
    })
    .unwrap();
    let debug_dir = crate_dir.join("target/debug");
    let release_dir = crate_dir.join("target/release");
    watcher.watch(&debug_dir, NonRecursive).unwrap();
    watcher.watch(&release_dir, NonRecursive).unwrap();
    (watcher, crate_dir)
}

fn run_cargo(
    toml_path: PathBuf,
    on_release: bool,
    print: bool,
) -> Result<std::process::ExitStatus, std::io::Error> {
    let mut cargo = Command::new("cargo");
    cargo.args(["build", "--manifest-path", toml_path.to_str().unwrap()]);

    if !cfg!(debug_assertions) && on_release {
        cargo.args(["--release"]);
    }

    #[cfg(feature = "deadlocks")]
    cargo.args(["--features", "deadlocks"]);

    if print {
        cargo.status()
    } else {
        cargo.output().map(|out| {
            if !out.status.success() {
                context::error!("{}", String::from_utf8_lossy(&out.stderr));
            }

            out.status
        })
    }
}

fn find_run_duat(lib: &Library) -> Option<Symbol<'_, RunFn>> {
    unsafe { lib.get::<RunFn>(b"run").ok() }
}

type RunFn = fn(
    Initials,
    MetaStatics,
    Vec<Vec<FileRet>>,
    DuatChannel,
) -> (Vec<Vec<FileRet>>, Receiver<DuatEvent>, Option<Instant>);
