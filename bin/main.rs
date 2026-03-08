//! The runner for Duat
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Mutex, mpsc},
    time::SystemTime,
};

use duat::private_exports::{post_setup, pre_setup, start};
#[cfg(feature = "term-ui")]
use duat::ui::traits::RawUi;
use duat_core::{
    context::{self},
    session::{
        ReloadedBuffer,
        ipc::{InitialState, MsgFromParent, ReloadRequest},
    },
};
use notify::{Event, EventKind, Watcher};

mod ipc;

static RELOAD_INSTANT: Mutex<Option<SystemTime>> = Mutex::new(None);

#[derive(Clone, Debug, clap::Parser)]
#[command(version, about)]
struct Args {
    /// Buffers to open
    buffers: Vec<PathBuf>,
    /// Open the config's src/main.rs
    #[arg(long)]
    cfg: bool,
    /// Open the config's Cargo.toml
    #[arg(long)]
    cfg_manifest: bool,
    /// Load the default config
    #[arg(long, conflicts_with_all = ["load", "profile", "init-config"])]
    no_load: bool,
    #[cfg_attr(
        target_os = "windows",
        doc = r"Config crate path [default: ~\AppData\Roaming\duat]"
    )]
    #[cfg_attr(
        not(target_os = "windows"),
        doc = r"Config crate path [default: ~/.config/duat]"
    )]
    #[arg(short, long)]
    load: Option<PathBuf>,
    /// Profile to load
    #[arg(long, default_value = "release")]
    profile: String,
    /// Open N windows [default: one per buffer]
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
    #[arg(long, requires = "init_config")]
    /// Use github dependencies instead of crates.io
    git_deps: bool,
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
    if let Some(crate_dir) = std::env::args().nth(4)
        && crate_dir == "--"
    {
        return Ok(start(|| {
            let ui = pre_setup();
            let (already_plugged, opts) = post_setup();
            (ui, already_plugged, opts)
        })?);
    }

    std::panic::set_hook(Box::new(|panic_info| {
        use std::backtrace::{Backtrace, BacktraceStatus};
        let backtrace = Backtrace::capture();
        UiImpl::close();
        if let BacktraceStatus::Disabled | BacktraceStatus::Unsupported = backtrace.status() {
            println!("{panic_info}")
        } else {
            println!("{panic_info}\n{backtrace}")
        }
    }));

    let args = <Args as clap::Parser>::parse();
    let socket_dir: &'static Path = std::env::temp_dir()
        .join("duat")
        .join(std::process::id().to_string())
        .leak();

    if let Some(name) = args.init_plugin.clone() {
        return init_plugin(args, name);
    }

    // Assert that the configuration crate actually exists.
    let (crate_dir, mut profile) = {
        let crate_dir = args
            .load
            .clone()
            .map(|crate_dir| crate_dir.leak() as &'static Path)
            .or_else(|| Some(config_dir()?.join("duat").leak() as &'static Path))
            .filter(|_| !args.no_load);

        let profile = args.profile.clone();

        if let Some(crate_dir) = crate_dir {
            (crate_dir, profile)
        } else {
            let failed_to_load = if args.no_load { "false" } else { "true" };
            let extra_args = UiImpl::open();

            let mut child = std::process::Command::new(std::env::current_exe()?)
                .arg(socket_dir)
                .args([&profile, "--", "true", failed_to_load, "false"])
                .args(extra_args)
                .spawn()?;

            child.wait()?;

            let buffers = get_buffers(args, Path::new(""), "")?;
            ipc::send(MsgFromParent::InitialState(InitialState {
                buffers,
                structs: HashMap::new(),
                clipb: ipc::get_clipboard(),
                reload_start: None,
            }))?;

            ipc::kill_remaining_processes();
            UiImpl::close();

            return Ok(());
        }
    };

    if decide_on_new_config(args.init_config, args.git_deps, crate_dir)? {
        return Ok(());
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

    let buffers = get_buffers(args.clone(), crate_dir, &profile)?;
    if buffers.is_empty() {
        return Ok(());
    }

    let mut exe_path = crate_dir
        .join("target")
        .join(match profile.as_str() {
            "dev" => "debug",
            profile => profile,
        })
        .join(resolve_config());

    let just_compiled = if args.reload || matches!(exe_path.try_exists(), Ok(false) | Err(_)) {
        if let Ok(status) = cargo::build(crate_dir, &profile, true)
            && status.success()
        {
            "true"
        } else {
            return Ok(());
        }
    } else {
        "false"
    };

    // The watcher is returned as to not be dropped.
    let (config_tx, config_rx) = mpsc::channel();

    ipc::start(crate_dir, socket_dir, config_tx.clone())?;

    std::thread::spawn(move || {
        UiImpl::open();

        let _watcher = match spawn_config_watcher(config_tx, crate_dir) {
            Ok(_watcher) => Some(std::mem::ManuallyDrop::new(_watcher)),
            Err(err) => {
                context::error!("Failed to spawn watcher, [a]reloading disabled[]: {err}");
                None
            }
        };
    });

    let extra_args = UiImpl::open();

    let mut first_time = "true";
    let mut initial_state = Some(InitialState {
        buffers: get_buffers(args, crate_dir, &profile)?,
        structs: HashMap::new(),
        clipb: ipc::get_clipboard(),
        reload_start: None,
    });

    let error = loop {
        let child = [
            (&exe_path, "false"),
            (&std::env::current_exe().unwrap(), "true"),
        ]
        .into_iter()
        .find_map(|(path, failed_to_load)| {
            std::process::Command::new(path)
                .arg(socket_dir)
                .arg(&profile)
                .arg(crate_dir)
                .args([first_time, failed_to_load, just_compiled])
                .args(extra_args.clone())
                .spawn()
                .ok()
        });

        ipc::send(MsgFromParent::InitialState(initial_state.take().unwrap())).unwrap();

        let Some(mut child) = child else {
            break Some("Failed to load any config".to_string());
        };

        child.wait()?;

        let final_state = match ipc::recv_final() {
            ipc::PanicOrFinal::Final(final_state) => final_state,
            ipc::PanicOrFinal::Panic(panic) => break Some(panic),
        };
        if final_state.buffers.is_empty() {
            break None;
        }

        first_time = "false";
        initial_state = Some(InitialState {
            buffers: final_state.buffers,
            structs: final_state.structs,
            clipb: ipc::get_clipboard(),
            reload_start: RELOAD_INSTANT.lock().unwrap().take(),
        });

        (exe_path, profile) = config_rx.recv().unwrap();
    };

    ipc::kill_remaining_processes();
    UiImpl::close();

    if let Some(error) = error {
        println!("{error}");
    }

    Ok(())
}

fn get_buffers(
    args: Args,
    crate_dir: &'static Path,
    profile: &str,
) -> Result<Vec<Vec<ReloadedBuffer>>, Box<dyn std::error::Error>> {
    let buffers = (move || -> Result<Vec<ReloadedBuffer>, std::io::Error> {
        let mut buffers = Vec::new();

        for buffer in args
            .cfg
            .then(|| crate_dir.join("src").join("main.rs"))
            .into_iter()
            .chain(args.cfg_manifest.then(|| crate_dir.join("Cargo.toml")))
            .chain(args.buffers)
            .enumerate()
            .map(|(i, path)| ReloadedBuffer::by_args(Some(path), i == 0))
        {
            buffers.push(buffer?);
        }

        Ok(buffers)
    })()?;

    Ok(if buffers.is_empty() {
        if args.reload {
            cargo::build(crate_dir, profile, true)?;
            Vec::new()
        } else if args.clean || args.update {
            Vec::new()
        } else {
            vec![vec![ReloadedBuffer::by_args(None, true).unwrap()]]
        }
    } else {
        let n = (buffers.len() / args.open.map(|n| n as usize).unwrap_or(buffers.len())).max(1);
        let mut files_per_window = Vec::new();

        for (i, buffer) in buffers.into_iter().enumerate() {
            if i % n == 0 {
                files_per_window.push(Vec::new());
            }
            files_per_window.last_mut().unwrap().push(buffer);
        }

        files_per_window
    })
}

fn spawn_config_watcher(
    config_tx: mpsc::Sender<(PathBuf, String)>,
    crate_dir: &'static Path,
) -> Result<notify::RecommendedWatcher, Box<dyn std::error::Error>> {
    let target_dir = crate_dir.join("target");
    std::fs::create_dir_all(&target_dir)?;

    let mut watcher = notify::recommended_watcher({
        move |res| {
            if let Ok(Event { kind: EventKind::Create(_), paths, .. }) = res
                && let Some(out_path) = paths.iter().find(|p| p.ends_with(resolve_config()))
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
                ipc::send(MsgFromParent::ReloadResult(Ok(()))).unwrap();
            }
        }
    })?;

    watcher.watch(&target_dir, notify::RecursiveMode::Recursive)?;

    Ok(watcher)
}

fn try_reload(
    crate_dir: &'static Path,
    config_tx: &mpsc::Sender<(PathBuf, String)>,
    request: ReloadRequest,
) {
    *RELOAD_INSTANT.lock().unwrap() = Some(SystemTime::now());

    if (request.clean || request.update)
        && let Some(cache_dir) = dirs_next::cache_dir()
    {
        clear_path(&cache_dir.join("duat").join("cache"));
    }

    let result: Result<std::process::ExitStatus, std::io::Error> = (|| {
        if request.clean {
            cargo::clean(crate_dir, false)?;
        }
        if request.update {
            cargo::update(crate_dir, false)?;
        }
        cargo::build(crate_dir, &request.profile, false)
    })();

    match result {
        Err(err) => {
            *RELOAD_INSTANT.lock().unwrap() = None;
            ipc::send(MsgFromParent::ReloadResult(Err(err.to_string()))).unwrap();
        }
        Ok(status) => {
            if status.success() {
                config_tx
                    .send((
                        crate_dir
                            .join("target")
                            .join(&request.profile)
                            .join(resolve_config()),
                        request.profile.to_string(),
                    ))
                    .unwrap();
                ipc::send(MsgFromParent::ReloadResult(Ok(()))).unwrap();
            } else {
                *RELOAD_INSTANT.lock().unwrap() = None;
                ipc::send(MsgFromParent::ReloadResult(Err("cargo failed".to_string()))).unwrap();
            }
        }
    }
}

/// Decide if a new config is necessary
///
/// Returns `true` if Duat shouldn't start.
fn decide_on_new_config(
    init_config: bool,
    git_deps: bool,
    crate_dir: &Path,
) -> Result<bool, Box<dyn std::error::Error>> {
    let config_is_empty = match std::fs::read_dir(crate_dir) {
        Ok(mut entries) => entries.next().is_none(),
        Err(_) => true,
    };

    if init_config || config_is_empty {
        const SRC_LIB_RS: &str = include_str!("../templates/config/main.rs");
        const MANIFEST: &str = include_str!("../templates/config/Cargo_.toml");

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
            // I hate windows...
            let ("y\n" | "Y\n" | "y\r\n" | "Y\r\n") = input.as_str() else {
                println!("Operation cancelled");
                return Ok(true);
            };
        }

        clear_path(crate_dir);
        std::fs::create_dir_all(crate_dir.join("src"))?;
        std::fs::write(crate_dir.join("src").join("main.rs"), SRC_LIB_RS)?;

        if git_deps {
            std::fs::write(crate_dir.join("Cargo.toml"), MANIFEST)?;
        } else {
            let manifest: String = MANIFEST
                .split_inclusive("\n")
                .filter(|line| !line.starts_with("git = \""))
                .collect();
            std::fs::write(crate_dir.join("Cargo.toml"), manifest)?;
        }
    }

    Ok(false)
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

#[cfg(target_os = "windows")]
const fn resolve_config() -> &'static str {
    "duat.exe"
}

#[cfg(not(any(target_os = "windows")))]
const fn resolve_config() -> &'static str {
    "duat"
}

#[cfg(not(target_os = "macos"))]
fn config_dir() -> Option<PathBuf> {
    dirs_next::config_dir()
}

#[cfg(target_os = "macos")]
fn config_dir() -> Option<PathBuf> {
    dirs_next::home_dir().map(|dir| dir.join(".config"))
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
    let pascal_name = snake_name.to_case(Case::Pascal);
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
    std::fs::write(plugin_dir.join("src").join("main.rs"), lib)?;
    println!("Created a plugin crate at {kebab_name}");
    Ok(())
}

mod cargo {
    use std::{path::Path, process::Command};

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

                out.status
            })
        }
    }
}

#[cfg(feature = "term-ui")]
type UiImpl = duat_term::Ui;

#[cfg(not(feature = "term-ui"))]
compile_error!("No Ui was chosen to compile Duat with");
