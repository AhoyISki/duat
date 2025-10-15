//! The runner for Duat
#![feature(
    decl_macro,
    iterator_try_collect,
    try_blocks,
    cfg_select,
    trim_prefix_suffix
)]

use std::{
    path::{Path, PathBuf},
    sync::{
        LazyLock, Mutex,
        mpsc::{self, Receiver},
    },
    time::Instant,
};

use duat::{Channels, Initials, MetaStatics, pre_setup, prelude::*, run_duat, utils::crate_dir};
use duat_core::{
    clipboard::Clipboard,
    context,
    session::{DuatEvent, ReloadEvent, ReloadedFile},
};
use libloading::{Library, Symbol};
use notify::{Event, EventKind, RecursiveMode::*, Watcher};

static RELOAD_INSTANT: Mutex<Option<Instant>> = Mutex::new(None);
static CLIPBOARD: LazyLock<Mutex<Clipboard>> = LazyLock::new(Mutex::default);

#[cfg(not(feature = "cli"))]
compile_error!("The Duat app needs the \"cli\" feature to work.");

#[derive(Clone, Debug, clap::Parser)]
#[command(version, about)]
struct Args {
    /// Files to open
    files: Vec<PathBuf>,
    /// Open the config's src/lib.rs
    #[arg(long)]
    cfg: bool,
    /// Open the config's Cargo.toml
    #[arg(long)]
    cfg_manifest: bool,
    #[doc = concat!("Config crate path [default: ", cfg_select! {
        target_os = "macos" => "~/Library/Application Support/duat]",
        target_os = "windows" => r"~\AppData\Roaming\duat",
        _ => "~/.config/duat]"
	})]
    /// Load the default config
    #[arg(long, conflicts_with_all = ["load", "profile", "init-config"])]
    no_load: bool,
    /// Which config path to use (path to directory containing
    /// Cargo.toml)
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
    /// Initializes a fresh configuration
    #[arg(long)]
    init_config: bool,
    /// Initializes a new Duat Plugin
    #[arg(long, value_name = "NAME", global = true)]
    init_plugin: Option<String>,
    /// The Plugin's repository
    #[arg(long, value_name = "REPO", requires = "init_plugin")]
    repository: Option<String>,
    /// The Plugin's author
    #[arg(long, value_name = "AUTHOR", requires = "init_plugin")]
    author: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = <Args as clap::Parser>::parse();

    if let Some(name) = args.init_plugin.clone() {
        return init_plugin(args, name);
    }

    // Initializers for access to static variables across two different
    // "duat-core instances"
    let logs = duat_core::context::Logs::new();
    log::set_logger(Box::leak(Box::new(logs.clone()))).unwrap();
    context::set_logs(logs.clone());

    let forms_init = duat_core::form::get_initial();
    duat_core::form::set_initial(forms_init);

    let (duat_tx, mut duat_rx) = mpsc::channel();
    duat_core::context::set_sender(duat_tx.clone());

    let ui = duat_core::ui::Ui::new::<UiImplementation>();

    // Assert that the configuration crate actually exists.
    let (crate_dir, profile) = {
        let crate_dir = args
            .load
            .clone()
            .map(|crate_dir| crate_dir.leak() as &'static Path)
            .or_else(|| Some(dirs_next::config_dir()?.join("duat").leak() as &'static Path))
            .filter(|_| !args.no_load);

        let profile: &'static str = args.profile.clone().leak();
        duat_core::utils::set_crate_dir_and_profile(crate_dir, profile);

        if let Some(crate_dir) = crate_dir {
            (crate_dir, profile)
        } else {
            ui.open(duat_core::session::DuatSender::new(duat_tx.clone()));

            if args.no_load {
                context::info!("Opened with the default configuration");
            } else {
                context::error!("Failed to find config crate, loading default");
            }

            pre_setup(None, None);
            run_duat(
                (ui, &CLIPBOARD),
                get_files(args, Path::new(""), "")?,
                duat_rx,
                None,
            );

            ui.close();
            return Ok(());
        }
    };

    if decide_on_new_config(args.init_config, crate_dir)? {
        return Ok(());
    };

    let profile_dir = match profile {
        "dev" => "debug",
        profile => profile,
    };

    if (args.clean || args.update)
        && let Some(cache_dir) = dirs_next::cache_dir()
    {
        clear_path(&cache_dir.join("duat"));
    }

    if args.clean
        && let Err(err) = cargo::clean(crate_dir, true)
    {
        match err.kind() {
            std::io::ErrorKind::NotFound => {}
            _ => return Err(err.into()),
        }
    }

    if args.update
        && let Err(err) = cargo::update(crate_dir, true)
    {
        match err.kind() {
            std::io::ErrorKind::NotFound => {}
            _ => return Err(err.into()),
        }
    }

    let mut files = get_files(args.clone(), crate_dir, profile)?;
    if files.is_empty() {
        return Ok(());
    }

    let mut lib = {
        let libconfig_path = crate_dir
            .join("target")
            .join(profile_dir)
            .join(resolve_config_file());

        if args.reload || matches!(libconfig_path.try_exists(), Ok(false) | Err(_)) {
            if let Ok(status) = cargo::build(crate_dir, profile, true)
                && status.success()
            {
                context::info!("Compiled [a]{profile}[] profile");
            } else {
                return Ok(());
            }
        }

        Some(unsafe { Library::new(libconfig_path)? })
    };

    // The watcher is returned as to not be dropped.
    let (config_tx, config_rx) = mpsc::channel();
    let _watcher = match spawn_config_watcher(config_tx.clone(), duat_tx.clone(), crate_dir) {
        Ok(_watcher) => Some(_watcher),
        Err(err) => {
            context::error!("Failed to spawn watcher, [a]reloading will be disabled[]: {err}");
            None
        }
    };

    let (reload_tx, reload_rx) = mpsc::channel();
    spawn_reloader(reload_rx, config_tx.clone(), duat_tx.clone());

    ui.open(duat_core::session::DuatSender::new(duat_tx.clone()));

    let mut reloading_profile: Option<String> = None;

    loop {
        let running_lib = lib.take();
        let mut running_duat_fn = running_lib.as_ref().and_then(find_run_duat);

        if let Some(profile) = reloading_profile {
            let time = match RELOAD_INSTANT.lock().unwrap().take() {
                Some(reload_instant) => txt!(" in [a]{:.2?}", reload_instant.elapsed()),
                None => Text::builder(),
            };
            context::info!("[a]{profile}[] profile reloaded{time}");
        }

        let (duat_tx, reload_tx) = (duat_tx.clone(), reload_tx.clone());
        (files, duat_rx) = std::thread::scope(|s| {
            // Initialize now in order to prevent thread activation after the
            // thread counter hook sets in.
            let clipb = &*CLIPBOARD;
            s.spawn(|| {
                if let Some(run_duat) = running_duat_fn.take() {
                    let initials = (logs.clone(), forms_init, (crate_dir, profile));
                    let channel = (duat_tx, duat_rx, reload_tx.clone());
                    run_duat(initials, (ui, clipb), files, channel)
                } else {
                    context::error!("No config at [a]{crate_dir}[], loading default");
                    pre_setup(None, None);
                    run_duat((ui, clipb), files, duat_rx, Some(reload_tx))
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

        let (config_path, profile) = config_rx.recv().unwrap();

        lib = unsafe { Library::new(config_path) }.ok();
        reloading_profile = Some(profile);
    }

    ui.close();

    Ok(())
}

fn get_files(
    args: Args,
    crate_dir: &'static Path,
    profile: &'static str,
) -> Result<Vec<Vec<ReloadedFile>>, Box<dyn std::error::Error>> {
    let files: Vec<ReloadedFile> = args
        .cfg
        .then(|| crate_dir.join("src").join("lib.rs"))
        .into_iter()
        .chain(args.cfg_manifest.then(|| crate_dir.join("Cargo.toml")))
        .chain(args.files)
        .enumerate()
        .map(|(i, path)| ReloadedFile::by_args(Some(path), i == 0))
        .try_collect()?;

    Ok(if files.is_empty() {
        if args.reload {
            cargo::build(crate_dir, profile, true)?;
            Vec::new()
        } else if args.clean || args.update {
            Vec::new()
        } else {
            vec![vec![ReloadedFile::by_args(None, true).unwrap()]]
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
    })
}

fn spawn_config_watcher(
    config_tx: mpsc::Sender<(PathBuf, String)>,
    duat_tx: mpsc::Sender<DuatEvent>,
    crate_dir: &'static std::path::Path,
) -> Result<notify::RecommendedWatcher, Box<dyn std::error::Error>> {
    let target_dir = crate_dir.join("target");
    std::fs::create_dir_all(&target_dir)?;

    let mut watcher = notify::recommended_watcher({
        move |res| {
            if let Ok(Event { kind: EventKind::Create(_), paths, .. }) = res
                && let Some(out_path) = paths.iter().find(|p| p.ends_with(resolve_config_file()))
                && let Some(parent) = out_path.parent()
                && let Some(grand_parent) = parent.parent()
                && grand_parent.ends_with("target")
                // If it is Some, a manual reload was already triggered.
                && RELOAD_INSTANT.lock().unwrap().is_none()
            {
                let profile = if let Some(parent) = out_path.parent()
                    && let Some(parent) = parent.file_name()
                {
                    parent.to_string_lossy().to_string()
                } else {
                    "release".to_string()
                };

                config_tx.send((out_path.clone(), profile)).unwrap();
                // On Windows, we need to unReloadResult before reloading
                // This means that this event should be sent as "reload" is
                // called, not here.
                if !cfg!(target_os = "windows") {
                    duat_tx.send(DuatEvent::ReloadSucceeded).unwrap();
                }
            }
        }
    })?;

    watcher.watch(&target_dir, Recursive)?;

    Ok(watcher)
}

fn spawn_reloader(
    reload_rx: mpsc::Receiver<ReloadEvent>,
    config_tx: mpsc::Sender<(PathBuf, String)>,
    duat_tx: mpsc::Sender<DuatEvent>,
) {
    std::thread::Builder::new()
        .name("reload".to_string())
        .spawn(move || {
            while let Ok(reload) = reload_rx.recv() {
                *RELOAD_INSTANT.lock().unwrap() = Some(Instant::now());

                let crate_dir = crate_dir().unwrap();
                if (reload.clean || reload.update)
                    && let Some(cache_dir) = dirs_next::cache_dir()
                {
                    clear_path(&cache_dir.join("duat").join("cache"));
                }

                let result: Result<std::process::ExitStatus, std::io::Error> = try {
                    if reload.clean {
                        cargo::clean(crate_dir, false)?;
                    }
                    if reload.update {
                        cargo::update(crate_dir, false)?;
                    }
                    cargo::build(crate_dir, &reload.profile, false)?
                };

                match result {
                    Err(err) => {
                        *RELOAD_INSTANT.lock().unwrap() = None;
                        context::error!(target: "reload", "{err}");
                        duat_tx.send(DuatEvent::ReloadFailed).unwrap();
                    }
                    Ok(status) => {
                        if status.success() {
                            config_tx
                                .send((
                                    crate_dir
                                        .join("target")
                                        .join(&reload.profile)
                                        .join(resolve_config_file()),
                                    reload.profile.to_string(),
                                ))
                                .unwrap();
                            duat_tx.send(DuatEvent::ReloadSucceeded).unwrap();
                        } else {
                            *RELOAD_INSTANT.lock().unwrap() = None;
                            duat_tx.send(DuatEvent::ReloadFailed).unwrap();
                        }
                    }
                }
            }
        })
        .unwrap();
}

/// Decide if a new config is necessary
///
/// Returns `true` if Duat shouldn't start.
fn decide_on_new_config(
    init_config: bool,
    crate_dir: &Path,
) -> Result<bool, Box<dyn std::error::Error>> {
    let config_is_empty = match std::fs::read_dir(crate_dir) {
        Ok(mut entries) => entries.next().is_none(),
        Err(_) => true,
    };

    if init_config || config_is_empty {
        const CONFIG_LIB: &str = include_str!("../templates/config/lib.rs");
        const CONFIG_TOML: &str = include_str!("../templates/config/Cargo_.toml");

        if config_is_empty ^ init_config {
            if config_is_empty {
                println!(
                    "Do you want to start a new config in {}? [y/N]",
                    crate_dir.to_string_lossy()
                );
            } else {
                println!(
                    "Are you sure you want to remove your current configuration at {}? [y/N]",
                    crate_dir.to_string_lossy()
                );
            }

            let mut input = String::new();
            std::io::stdin().read_line(&mut input)?;
            let ("y\n" | "Y\n") = input.as_str() else {
                println!("Operation cancelled");
                return Ok(true);
            };
        }

        clear_path(crate_dir);
        std::fs::create_dir_all(crate_dir.join("src"))?;
        std::fs::write(crate_dir.join("Cargo.toml"), CONFIG_TOML)?;
        std::fs::write(crate_dir.join("src").join("lib.rs"), CONFIG_LIB)?;
    }

    Ok(false)
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
                #[cfg(target_os = "windows")]
                if out.stderr.ends_with(b"Access is denied. (os error 5)\n") {
                    context::error!("Failed to reload config crate");
                    context::info!("On [a]Windows[], close other instances of Duat to reload");
                } else {
                    context::error!("{}", String::from_utf8_lossy(&out.stderr));
                }

                #[cfg(not(target_os = "windows"))]
                if !out.status.success() {
                    context::error!("{}", String::from_utf8_lossy(&out.stderr));
                }

                out.status
            })
        }
    }
}

/// Recursively attempts to remove every element in a path
///
/// Doesn't give up upon failing to remove some individual item.
fn clear_path(path: &Path) {
    let Ok(entries) = std::fs::read_dir(path) else {
        let _ = std::fs::remove_file(path);
        return;
    };

    for entry in entries.filter_map(Result::ok) {
        if let Ok(ft) = entry.file_type()
            && ft.is_dir()
        {
            let _ = std::fs::remove_dir_all(entry.path());
        } else {
            let _ = std::fs::remove_file(entry.path());
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

fn init_plugin(args: Args, name: String) -> Result<(), Box<dyn std::error::Error>> {
    use convert_case::{Case, Casing};
    const PLUGIN_TOML: &str = include_str!("../templates/plugin/Cargo_.toml");
    const PLUGIN_README: &str = include_str!("../templates/plugin/README.md");
    const PLUGIN_LIB: &str = include_str!("../templates/plugin/lib.rs");
    if !name.is_case(Case::Kebab) {
        println!("\x1b[33mwarning:\x1b[0m converting plugin name to kebab-case");
    }
    let kebab_name = name.to_case(Case::Kebab);
    if !kebab_name.starts_with("duat-") {
        println!(
            "\x1b[33mwarning:\x1b[0m by creating a plugin that doesn't start with \"duat\", you \
             might cause some confusion"
        );
    }
    let snake_name = name.to_case(Case::Snake);
    let pascal_name = snake_name.trim_prefix("duat_").to_case(Case::Pascal);
    let plugin_dir = PathBuf::from(&kebab_name);
    std::fs::create_dir(&plugin_dir)?;
    std::fs::create_dir(plugin_dir.join("src"))?;
    let repo = args.repository.unwrap_or("{repository_url}".to_string());
    let author = args.author.unwrap_or("{author_name}".to_string());
    let toml = PLUGIN_TOML
        .replace("plugin-name", &kebab_name)
        .replace("{repository_url}", &repo)
        .replace("{author_name}", &author);
    let readme = PLUGIN_README
        .replace("plugin-name", &kebab_name)
        .replace("plugin_name", &snake_name)
        .replace("PluginName", &pascal_name)
        .replace("{repository_url}", &repo);
    let lib = PLUGIN_LIB
        .replace("plugin_name", &snake_name)
        .replace("PluginName", &pascal_name);
    std::fs::write(plugin_dir.join("Cargo.toml"), toml)?;
    std::fs::write(plugin_dir.join("README.md"), readme)?;
    std::fs::write(plugin_dir.join("src").join("lib.rs"), lib)?;
    println!("Created a plugin crate at {kebab_name}");
    Ok(())
}

type RunFn = fn(
    Initials,
    MetaStatics,
    Vec<Vec<ReloadedFile>>,
    Channels,
) -> (Vec<Vec<ReloadedFile>>, Receiver<DuatEvent>);

#[cfg(feature = "term-ui")]
type UiImplementation = duat_term::Ui;

#[cfg(not(feature = "term-ui"))]
compile_error!("No Ui was chosen to compile Duat with");
