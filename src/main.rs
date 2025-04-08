#![feature(decl_macro, let_chains)]

use std::{
    io::Read,
    path::PathBuf,
    process::{Command, Output},
    sync::{
        LazyLock,
        mpsc::{self, Receiver},
    },
    time::Instant,
};

use dlopen_rs::{Dylib, ElfLibrary, OpenFlags, Symbol};
use duat::{Messengers, MetaStatics, pre_setup, prelude::*, run_duat};
use duat_core::{
    Mutex,
    clipboard::Clipboard,
    session::FileRet,
    ui::{self, DuatEvent, Ui as UiTrait},
};
use notify::{Event, EventKind, RecursiveMode::*, Watcher};

static MS: LazyLock<<Ui as ui::Ui>::MetaStatics> =
    LazyLock::new(<Ui as ui::Ui>::MetaStatics::default);
static CLIPB: LazyLock<Mutex<Clipboard>> = LazyLock::new(|| Mutex::new(Clipboard::new().unwrap()));

fn main() {
    // ????????????????????????????????????
    env_logger::init();
    dlopen_rs::init();

    let (reload_tx, reload_rx) = mpsc::channel();
    let (duat_tx, mut duat_rx) = mpsc::channel();
    let duat_tx = Box::leak(Box::new(duat_tx));

    // Assert that the configuration crate actually exists.
    // The watcher is returned as to not be dropped.
    let (_watcher, crate_dir) = {
        let Some(crate_dir) = duat_core::crate_dir().filter(|cd| cd.exists()) else {
            let msg = err!("No config crate found, loading default config");
            duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
            pre_setup();
            run_duat((&MS, &CLIPB), Vec::new(), (duat_tx, duat_rx));
            return;
        };

        let mut watcher = notify::recommended_watcher({
            let reload_tx = reload_tx.clone();
            let duat_tx = duat_tx.clone();
            move |res| {
                if let Ok(Event { kind: EventKind::Create(_), paths, .. }) = res
                    && let Some(so_path) = paths.iter().find(|path| path.ends_with("libconfig.so"))
                {
                    let on_release = so_path.ends_with("release/libconfig.so");
                    reload_tx.send((so_path.clone(), on_release)).unwrap();
                    duat_tx.send(DuatEvent::ReloadConfig).unwrap();
                }
            }
        })
        .unwrap();
        let debug_dir = crate_dir.join("target/debug");
        let release_dir = crate_dir.join("target/release");
        watcher.watch(&debug_dir, NonRecursive).unwrap();
        watcher.watch(&release_dir, NonRecursive).unwrap();
        (watcher, crate_dir)
    };

    if cfg!(debug_assertions) {
        let toml_path = crate_dir.join("Cargo.toml");
        if run_cargo(toml_path, false, true).is_err() {
            let msg = err!("Failed to compile " [*a] "config" [] " crate");
            duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
        }
    }

    let mut prev = Vec::new();

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
                let duat_tx = duat_tx.clone();
                std::thread::spawn(move || {
                    // Also compile it in debug mode, to speed up recompilation.
                    run_cargo(toml_path, false, false).unwrap();
                    let msg = hint!("Compiled " [*a] "debug" [] " profile");
                    duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
                });
            } else {
                let msg = err!("Failed to compile " [*a] "release" [] " profile");
                duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
            }
        }
        ElfLibrary::dlopen(so_path, OpenFlags::RTLD_NOW | OpenFlags::RTLD_LOCAL).ok()
    };

    Ui::open(&MS, ui::Sender::new(duat_tx));

    loop {
        let run_lib = lib.take();
        let mut run_fn = run_lib.as_ref().and_then(find_run_duat);

        let ret = if let Some(run_duat) = run_fn.take() {
            run_duat((&MS, &CLIPB), prev, (duat_tx, duat_rx))
        } else {
            let msg = err!("Failed to open load crate");
            duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
            pre_setup();
            run_duat((&MS, &CLIPB), prev, (duat_tx, duat_rx))
        };

        let Some((new_prev, ret_duat_rx, reload_instant)) = ret else {
            break;
        };

        (prev, duat_rx) = (new_prev, ret_duat_rx);

        drop(run_lib);

        let (so_path, on_release) = reload_rx.try_recv().unwrap();
        let profile = if on_release { "Release" } else { "Debug" };
        let time = format!("{:.2?}", reload_instant.elapsed());
        let msg = ok!([*a] profile [] " profile reloaded in " [*a] time);
        duat_tx.send(DuatEvent::MetaMsg(msg)).unwrap();
        lib = ElfLibrary::dlopen(so_path, OpenFlags::RTLD_NOW | OpenFlags::RTLD_LOCAL).ok();
    }

    Ui::close(&MS);
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

fn find_run_duat(lib: &Dylib) -> Option<Symbol<RunFn>> {
    unsafe { lib.get::<RunFn>("run").ok() }
}

type RunFn = fn(
    MetaStatics,
    Vec<Vec<FileRet>>,
    Messengers,
) -> Option<(Vec<Vec<FileRet>>, Receiver<DuatEvent>, Instant)>;
