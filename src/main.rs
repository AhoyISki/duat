//! The runner for Duat
#![feature(decl_macro)]

use std::{
    path::{Path, PathBuf},
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
use notify::{Event, EventKind, RecursiveMode::*, Watcher};

static CLIPB: LazyLock<Mutex<Clipboard>> = LazyLock::new(Mutex::default);

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initializers for access to static variables across two different
    // "duat-core instances"
    let logs = duat_core::context::Logs::new();
    log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
    context::set_logs(logs.clone());

    let forms_init = duat_core::form::get_initial();
    duat_core::form::set_initial(forms_init);

    let (duat_tx, mut duat_rx) = mpsc::channel();
    let duat_tx: &'static mpsc::Sender<DuatEvent> = Box::leak(Box::new(duat_tx));
    duat_core::context::set_sender(duat_tx);

    let ms: &'static <Ui as ui::Ui>::MetaStatics =
        Box::leak(Box::new(<Ui as ui::Ui>::MetaStatics::default()));

    // The watcher is returned as to not be dropped.
    let (reload_tx, reload_rx) = mpsc::channel();

    // Assert that the configuration crate actually exists.
    let Some(crate_dir) = duat_core::crate_dir().ok().filter(|cd| cd.exists()) else {
        context::error!("No config crate found, loading default config");
        pre_setup(None, duat_tx);
        run_duat((ms, &CLIPB), Vec::new(), duat_rx);
        return Ok(());
    };

    let mut lib = {
        let libconfig_path = crate_dir.join(if cfg!(debug_assertions) {
            format!("target/debug/{}", resolve_config_file())
        } else {
            format!("target/release/{}", resolve_config_file())
        });

        if let Ok(false) | Err(_) = libconfig_path.try_exists() {
            println!("Compiling config crate for the first time, this might take a while...");
            if let Ok(status) = run_cargo(crate_dir, true, true)
                && status.success()
            {
                context::info!("Compiled [a]release[] profile");
            } else {
                context::error!("Failed to compile [a]release[] profile");
            }
        }

        Some(unsafe { Library::new(libconfig_path)? })
    };

    let _watcher = match spawn_watcher(reload_tx, duat_tx, crate_dir) {
        Ok(_watcher) => Some(_watcher),
        Err(err) => {
            context::error!("Failed to spawn watcher, [a]reloading will be disabled[]: {err}");
            None
        }
    };

    Ui::open(ms, ui::Sender::new(duat_tx));

    let mut i = 0;
    let mut prev = Vec::new();
    loop {
        let running_lib = lib.take().unwrap();
        let mut run_fn = find_run_duat(&running_lib);

        let reload_instant;
        (prev, duat_rx, reload_instant) = std::thread::scope(|s| {
            s.spawn(|| {
                if let Some(run_duat) = run_fn.take() {
                    let initials = (logs.clone(), forms_init);
                    let channel = (duat_tx, duat_rx);
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
        
        running_lib.close().unwrap();
        
        if prev.is_empty() {
            break;
        }
        
        std::thread::sleep(std::time::Duration::from_secs(4));

        let (so_path, profile) = reload_rx.recv().unwrap();

        let time = match reload_instant {
            Some(reload_instant) => txt!(" in [a]{:.2?}", reload_instant.elapsed()),
            None => Text::builder(),
        };
        context::info!("[a]{profile}[] profile reloaded{time}");
        lib = unsafe { Library::new(so_path) }.ok();
        i += 1;
    }

    Ui::close(ms);

    Ok(())
}

fn spawn_watcher(
    reload_tx: mpsc::Sender<(PathBuf, String)>,
    duat_tx: &mpsc::Sender<DuatEvent>,
    crate_dir: &'static std::path::Path,
) -> Result<notify::RecommendedWatcher, Box<dyn std::error::Error>> {
    let debug_dir = crate_dir.join("target/debug");
    let release_dir = crate_dir.join("target/release");
    std::fs::create_dir_all(&debug_dir);
    std::fs::create_dir_all(&release_dir);
    //std::fs::write("log.txt", &[]).unwrap();

    let mut watcher = notify::recommended_watcher({
        let reload_tx = reload_tx.clone();
        let duat_tx = duat_tx.clone();
        let libconfig_str = resolve_config_file();

        move |res| {
      //       let mut file = std::fs::OpenOptions::new().append(true).open("log.txt").unwrap();
      //       std::io::Write::write_all(&mut file, format!("{res:#?}").as_bytes()).unwrap();
            if let Ok(Event { kind: EventKind::Create(_), paths, .. }) = res
                && let Some(out_path) = paths.iter().find(|p| p.ends_with(libconfig_str))
            {
                let profile = if let Some(parent) = out_path.parent()
                    && let Some(parent) = parent.file_name()
                {
                    parent.to_string_lossy().to_string()
                } else {
                    "release".to_string()
                };
                
                reload_tx.send((out_path.clone(), profile)).unwrap();
                // On Windows, we need to unload the dll before reloading
                // This means that this event should be sent as "reload" is
                // called, not here.
                if !cfg!(target_os = "windows") {
                    duat_tx.send(DuatEvent::ReloadConfig).unwrap();
                }
            }
        }
    })?;

    watcher.watch(&debug_dir, NonRecursive)?;
    watcher.watch(&release_dir, NonRecursive)?;

    Ok(watcher)
}

fn run_cargo(
    crate_dir: &'static Path,
    on_release: bool,
    print: bool,
) -> Result<std::process::ExitStatus, std::io::Error> {
    let mut cargo = Command::new("cargo");
    cargo
        .args(["build", "--manifest-path"])
        .arg(crate_dir.join("Cargo.toml"));

    if !cfg!(debug_assertions) && on_release {
        cargo.arg("--release");
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

#[cfg(target_os = "macos")]
const fn resolve_config_file() -> &'static str {
    "libconfig.dylib"
}

#[cfg(target_os = "windows")]
const fn resolve_config_file() -> &'static str {
    "config.dll"
}

#[cfg(not(any(target_os = "windows", target_os = "macos")))]
const fn resolve_config_file() -> &'static str {
    "libconfig.so"
}

type RunFn = fn(
    Initials,
    MetaStatics,
    Vec<Vec<FileRet>>,
    DuatChannel,
) -> (Vec<Vec<FileRet>>, Receiver<DuatEvent>, Option<Instant>);
