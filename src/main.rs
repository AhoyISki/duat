//! The runner for Duat
#![feature(decl_macro, iterator_try_collect, try_blocks)]

use std::{
    path::PathBuf,
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
    session::FileParts,
    ui::{self, DuatEvent, Ui as UiTrait},
};
use libloading::{Library, Symbol};
use notify::{Event, EventKind, RecursiveMode::*, Watcher};

static CLIPBOARD: LazyLock<Mutex<Clipboard>> = LazyLock::new(Mutex::default);

#[derive(Debug, clap::Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Files to open
    files: Vec<PathBuf>,
    /// Open the config's src/lib.rs
    #[arg(long)]
    cfg: bool,
    /// Open the config's Cargo.toml
    #[arg(long)]
    cfg_manifest: bool,
    #[cfg_attr(
        not(any(target_os = "macos", target_os = "windows")),
        doc = "Config crate path [default: ~/.config/duat]"
    )]
    #[cfg_attr(
        target_os = "macos",
        doc = "Config crate path [default: ~/Library/Application Support/duat]"
    )]
    #[cfg_attr(
        target_os = "windows",
        doc = r"Config crate path [default: ~\AppData\Roaming\duat]"
    )]
    #[arg(short, long)]
    load: Option<PathBuf>,
    /// Profile to load
    #[arg(long, default_value = "release")]
    profile: String,
    /// Open N windows [default: one per file]
    #[arg(short, long, value_name = "N", value_parser = clap::value_parser!(u16).range(1..))]
    open: Option<u16>,
    /// Recompile the config crate
    #[arg(long)]
    reload: bool,
    /// Clears the target and cache dirs
    #[arg(long)]
    clean: bool,
    /// Updates the config's dependencies
    #[arg(long)]
    update: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = <Args as clap::Parser>::parse();

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

    let (reload_tx, reload_rx) = mpsc::channel();

    // Assert that the configuration crate actually exists.
    let crate_dir = {
        let crate_dir = args
            .load
            .map(|crate_dir| {
                let path: &'static std::path::Path = crate_dir.leak();
                path
            })
            .or_else(|| {
                let config_dir = dirs_next::config_dir()?;
                let path: &'static str =
                    config_dir.join("duat").to_string_lossy().to_string().leak();

                std::fs::create_dir_all(path).ok()?;
                Some(std::path::Path::new(path))
            });

        duat_core::set_crate_dir(crate_dir);

        if let Some(crate_dir) = crate_dir {
            crate_dir
        } else {
            Ui::open(ms, ui::Sender::new(duat_tx));
            context::error!("Failed to find config crate, loading default");
            pre_setup(None, None);
            run_duat(
                (ms, &CLIPBOARD),
                vec![vec![FileParts::by_args(None, true).unwrap()]],
                duat_rx,
            );
            Ui::close(ms);
            return Ok(());
        }
    };

    if args.clean {
        use std::io::ErrorKind::*;
        let result: Result<(), std::io::Error> = try {
            cargo::clean(crate_dir, true)?;
            if let Some(cache_dir) = dirs_next::cache_dir() {
                std::fs::remove_dir_all(cache_dir.join("duat"))?
            }
        };

        if let Err(err) = result {
            match err.kind() {
                NotFound => {}
                _ => return Err(err.into()),
            }
        }
    }

    if args.update {
        use std::io::ErrorKind::*;
        let result: Result<(), std::io::Error> = try {
            cargo::update(crate_dir, true)?;
            if let Some(cache_dir) = dirs_next::cache_dir()
                && !args.clean
            {
                std::fs::remove_dir_all(cache_dir.join("duat"))?
            }
        };

        if let Err(err) = result {
            match err.kind() {
                NotFound => {}
                _ => return Err(err.into()),
            }
        }
    }

    let mut lib = {
        let libconfig_path = crate_dir.join(format!(
            "target/{}/{}",
            &args.profile,
            resolve_config_file()
        ));

        if args.reload || matches!(libconfig_path.try_exists(), Ok(false) | Err(_)) {
            if !args.reload {
                println!("Compiling config crate for the first time, this might take a while...");
            }
            if let Ok(status) = cargo::build(crate_dir, &args.profile, true)
                && status.success()
            {
                context::info!("Compiled [a]release[] profile");
            } else {
                context::error!("Failed to compile [a]release[] profile");
            }
        }

        unsafe { Library::new(libconfig_path).ok() }
    };

    // The watcher is returned as to not be dropped.
    let _watcher = match spawn_watcher(reload_tx, duat_tx, crate_dir) {
        Ok(_watcher) => Some(_watcher),
        Err(err) => {
            context::error!("Failed to spawn watcher, [a]reloading will be disabled[]: {err}");
            None
        }
    };

    Ui::open(ms, ui::Sender::new(duat_tx));

    let mut files = {
        let files: Vec<FileParts> = args
            .cfg
            .then(|| crate_dir.join("src/lib.rs"))
            .into_iter()
            .chain(args.cfg_manifest.then(|| crate_dir.join("Cargo.toml")))
            .chain(args.files)
            .enumerate()
            .map(|(i, path)| FileParts::by_args(Some(path), i == 0))
            .try_collect()?;

        if files.is_empty() {
            if args.clean || args.reload || args.update {
                return Ok(());
            } else {
                vec![vec![FileParts::by_args(None, true).unwrap()]]
            }
        } else {
            let n = (files.len() / args.open.map(|n| n as usize).unwrap_or(files.len())).max(1);
            let mut files_per_window = Vec::new();

            for (i, file) in files.into_iter().enumerate() {
                if i % n == 0 {
                    files_per_window.push(Vec::new());
                }
                files_per_window.last_mut().unwrap().push(file);
            }

            files_per_window
        }
    };

    loop {
        let running_lib = lib.take();
        let mut run_fn = running_lib.as_ref().and_then(find_run_duat);

        let reload_instant;
        (files, duat_rx, reload_instant) = std::thread::scope(|s| {
            // Initialize now in order to prevent thread activation after the
            // thread counter hook sets in.
            let clipb = &*CLIPBOARD;
            s.spawn(|| {
                if let Some(run_duat) = run_fn.take() {
                    let initials = (logs.clone(), forms_init);
                    let channel = (duat_tx, duat_rx);
                    run_duat(initials, (ms, clipb), files, channel)
                } else {
                    context::error!("Config crate not found at [a]{crate_dir}[], loading default");
                    pre_setup(None, None);
                    run_duat((ms, clipb), files, duat_rx)
                }
            })
            .join()
            .unwrap()
        });

        let Some(running_lib) = running_lib else {
            break;
        };
        running_lib.close().unwrap();

        if files.is_empty() {
            break;
        }

        let (config_path, profile) = reload_rx.recv().unwrap();

        let time = match reload_instant {
            Some(reload_instant) => txt!(" in [a]{:.2?}", reload_instant.elapsed()),
            None => Text::builder(),
        };
        context::info!("[a]{profile}[] profile reloaded{time}");
        lib = unsafe { Library::new(config_path) }.ok();
    }

    Ui::close(ms);

    Ok(())
}

fn spawn_watcher(
    reload_tx: mpsc::Sender<(PathBuf, String)>,
    duat_tx: &mpsc::Sender<DuatEvent>,
    crate_dir: &'static std::path::Path,
) -> Result<notify::RecommendedWatcher, Box<dyn std::error::Error>> {
    let target_dir = crate_dir.join("target");
    std::fs::create_dir_all(&target_dir)?;

    let mut watcher = notify::recommended_watcher({
        let reload_tx = reload_tx.clone();
        let duat_tx = duat_tx.clone();
        let libconfig_str = resolve_config_file();

        move |res| {
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

    watcher.watch(&target_dir, Recursive)?;

    Ok(watcher)
}

mod cargo {
    use std::{path::Path, process::Command};

    use duat_core::context;

    /// Build the config crate
    pub fn build(
        crate_dir: &'static Path,
        profile: &str,
        print: bool,
    ) -> Result<std::process::ExitStatus, std::io::Error> {
        let manifest_path = crate_dir.join("Cargo.toml");

        let mut cargo = Command::new("cargo");
        cargo
            .args(["build", "--profile", profile, "--manifest-path"])
            .arg(manifest_path);

        #[cfg(feature = "deadlocks")]
        cargo.args(["--features", "deadlocks"]);

        exec_cargo(cargo, print)
    }

    /// Clean the config crate
    pub fn clean(
        crate_dir: &'static Path,
        print: bool,
    ) -> Result<std::process::ExitStatus, std::io::Error> {
        let mut cargo = Command::new("cargo");
        cargo
            .args(["clean", "--manifest-path"])
            .arg(crate_dir.join("Cargo.toml"));

        exec_cargo(cargo, print)
    }

    /// Updates the config crate
    pub fn update(
        crate_dir: &'static Path,
        print: bool,
    ) -> Result<std::process::ExitStatus, std::io::Error> {
        let mut cargo = Command::new("cargo");
        cargo
            .args(["update", "--manifest-path"])
            .arg(crate_dir.join("Cargo.toml"));

        exec_cargo(cargo, print)
    }

    fn exec_cargo(
        mut cargo: Command,
        print: bool,
    ) -> Result<std::process::ExitStatus, std::io::Error> {
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
    Vec<Vec<FileParts>>,
    DuatChannel,
) -> (Vec<Vec<FileParts>>, Receiver<DuatEvent>, Option<Instant>);
