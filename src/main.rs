//! The runner for Duat
#![feature(decl_macro)]

use std::{
    io::Read,
    path::PathBuf,
    process::{Command, Output},
    sync::{
        LazyLock, Mutex,
        mpsc::{self, Receiver},
    },
    time::Instant,
};

use duat::{Messengers, MetaStatics, pre_setup, prelude::*, run_duat};
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
    let logs = duat_core::context::Logs::new();
    log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
    context::set_logs(logs.clone());

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
        let so_path = crate_dir.join(if cfg!(debug_assertions) {
            "target/debug/libconfig.so"
        } else {
            "target/release/libconfig.so"
        });

        if let Ok(false) | Err(_) = so_path.try_exists() {
            println!("Compiling config crate for the first time, this might take a while...");
            let toml_path = crate_dir.join("Cargo.toml");
            if let Ok(out) = run_cargo(toml_path.clone(), true, true)
                && out.status.success()
            {
                context::info!("Compiled [a]release[] profile");
            } else {
                context::error!("Failed to compile [a]release[] profile");
            }
        }
        unsafe { Library::new(so_path) }.ok()
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
                    run_duat(logs.clone(), (ms, &CLIPB), prev, (duat_tx, duat_rx))
                } else {
                    context::error!("Failed to load config crate");
                    pre_setup(None, duat_tx);
                    run_duat((ms, &CLIPB), prev, duat_rx)
                }
            })
            .join()
            .unwrap()
        });

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
) -> (notify::INotifyWatcher, &'static std::path::Path) {
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

fn run_cargo(toml_path: PathBuf, on_release: bool, print: bool) -> Result<Output, std::io::Error> {
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

    if print {
        let mut child = cargo.stdout(std::process::Stdio::piped()).spawn()?;

        let mut buf = [0u8; 100];
        let mut stdout = child.stdout.take().unwrap();
        let mut print_stdout = || -> Result<usize, std::io::Error> {
            let read = stdout.read(&mut buf)?;

            println!("{}", std::str::from_utf8(&buf[0..read]).unwrap());
            Ok(read)
        };
        print_stdout()?;

        while child.try_wait()?.is_none() {
            print_stdout()?;
        }

        let out = child.wait_with_output()?;
        print_stdout()?;
        Ok(out)
    } else {
        cargo.output()
    }
}

fn find_run_duat(lib: &Library) -> Option<Symbol<'_, RunFn>> {
    unsafe { lib.get::<RunFn>(b"run").ok() }
}

type RunFn = fn(
    context::Logs,
    MetaStatics,
    Vec<Vec<FileRet>>,
    Messengers,
) -> (Vec<Vec<FileRet>>, Receiver<DuatEvent>, Option<Instant>);
