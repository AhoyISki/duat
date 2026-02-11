use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    fs,
    path::PathBuf,
    process::{Command, ExitStatus},
    sync::{LazyLock, Mutex},
    thread::JoinHandle,
};

use duat_core::context::{self, Handle};
use indoc::formatdoc;
use libloading::Library;
use tree_sitter::Language;

use self::list::LANGUAGE_OPTIONS;

static FAILED_COPILATION: LazyLock<Mutex<HashSet<String>>> = LazyLock::new(Mutex::default);
static COMPILATIONS: LazyLock<Mutex<HashMap<String, Compilation>>> = LazyLock::new(Mutex::default);

type Compilation = (JoinHandle<Option<ExitStatus>>, Vec<Handle>);

mod list;

pub fn get_language(filetype: &str, handle: &Handle) -> Option<Language> {
    static LIBRARIES: Mutex<Vec<Library>> = Mutex::new(Vec::new());

    if FAILED_COPILATION.lock().unwrap().contains(filetype) {
        return None;
    }

    let parsers_dir = get_parsers_dir()?;
    let options = LANGUAGE_OPTIONS.get(filetype)?;

    let lib_dir = parsers_dir.join("lib");

    if fs::create_dir_all(&lib_dir).is_err() {
        return None;
    }

    let crate_dir = parsers_dir.join(format!("ts-{}", options.crate_name));
    let manifest_path = crate_dir.join("Cargo.toml");

    let lang = options.crate_name.replace("-", "_");
    let language_path = lib_dir.join(resolve_lib_file(&lang));

    if let Ok(lib) = unsafe { Library::new(&language_path) } {
        let language = unsafe {
            let (symbol, _) = options.symbols[0];
            let lang_fn = lib
                .get::<fn() -> Language>(symbol.to_lowercase().as_bytes())
                .ok()?;

            lang_fn()
        };

        LIBRARIES.lock().unwrap().push(lib);

        Some(language)
    } else if let Ok(true) = fs::exists(&crate_dir) {
        let fail = || {
            context::error!("Failed to compile tree-sitter language for {filetype}");
            FAILED_COPILATION
                .lock()
                .unwrap()
                .insert(filetype.to_string());
            None
        };

        let mut compilations = COMPILATIONS.lock().unwrap();

        if let Entry::Occupied(mut child) = compilations.entry(filetype.to_string()) {
            let (join_handle, handles) = child.get_mut();
            if !join_handle.is_finished() {
                if !handles.contains(handle) {
                    handles.push(handle.clone());
                }
                return None;
            }
            let (_, (join_handle, _)) = child.remove_entry();

            match join_handle.join().unwrap() {
                Some(status) if status.success() => {
                    if fs::copy(
                        crate_dir
                            .join("target")
                            .join("release")
                            .join(resolve_lib_file(&lang)),
                        language_path,
                    )
                    .is_err()
                    {
                        return fail();
                    };

                    let mut cargo = Command::new("cargo");
                    _ = cargo
                        .args(["clean", "--manifest-path"])
                        .arg(manifest_path)
                        .output();

                    get_language(filetype, handle)
                }
                _ => fail(),
            }
        } else {
            context::info!("Compiling tree-sitter parser for [a]{filetype}");
            let mut cargo = Command::new("cargo");

            cargo
                .args(["build", "--release", "--manifest-path"])
                .arg(&manifest_path);

            let join_handle = std::thread::spawn({
                let filetype = filetype.to_string();
                move || {
                    let out = cargo.output().ok()?;

                    if out.status.success() {
                        let children = COMPILATIONS.lock().unwrap();
                        let (_, handles) = children.get(&filetype).unwrap();
                        for handle in handles {
                            handle.request_update();
                        }
                    }

                    Some(out.status)
                }
            });

            compilations.insert(filetype.to_string(), (join_handle, vec![handle.clone()]));

            None
        }
    } else {
        let lib_rs: String = options
            .symbols
            .iter()
            .map(|(symbol, is_function)| {
                let fn_name = symbol.to_lowercase();
                let language = if *is_function {
                    format!("ts::{symbol}()")
                } else {
                    format!("ts::{symbol}.into()")
                };

                formatdoc! {"
                    #[unsafe(no_mangle)]    
                    pub fn {fn_name}() -> tree_sitter::Language {{
                        {language}
                    }}
                "}
            })
            .collect();

        let crate_name = options.crate_name;
        let git = options.git;
        let version = options.crate_version.unwrap_or("*");

        let cargo_toml = formatdoc! {r#"
            [package]
            name = "ts-{crate_name}"
            version = "0.1.0"
            edition = "2024"
            description = "Dynamic wrapper for tree-sitter-{crate_name}"

            [lib]
            name = "{lang}"
            crate-type = ["dylib"]

            [dependencies]
            tree-sitter = "*"

            [dependencies.ts]
            version = "{version}"
            git = "{git}"
            package = "tree-sitter-{crate_name}"
        "#};

        fs::create_dir_all(crate_dir.join("src")).ok()?;
        fs::write(manifest_path, cargo_toml).ok()?;
        fs::write(crate_dir.join("src/lib.rs"), lib_rs).ok()?;

        get_language(filetype, handle)
    }
}

fn get_parsers_dir() -> Option<PathBuf> {
    let workspace_dir = duat_core::utils::plugin_dir("duat-treesitter").ok()?;
    let parsers_dir = workspace_dir.join("parsers");
    fs::create_dir_all(&parsers_dir).ok()?;

    Some(parsers_dir)
}

struct LanguageOptions {
    git: &'static str,
    symbols: &'static [(&'static str, bool)],
    crate_name: &'static str,
    crate_version: Option<&'static str>,
    _maintainers: &'static [&'static str],
}

impl LanguageOptions {
    fn pair_const(
        lang: &'static str,
        git: &'static str,
        _maintainers: &'static [&'static str],
    ) -> (&'static str, Self) {
        let options = Self {
            git,
            symbols: &[("LANGUAGE", false)],
            crate_name: crate_name(lang),
            crate_version: None,
            _maintainers,
        };

        (lang, options)
    }

    fn pair_fn(
        lang: &'static str,
        git: &'static str,
        _maintainers: &'static [&'static str],
    ) -> (&'static str, Self) {
        let options = Self {
            git,
            symbols: &[("language", true)],
            crate_name: crate_name(lang),
            crate_version: None,
            _maintainers,
        };

        (lang, options)
    }

    fn pairs_with_symbol(
        lang: &'static str,
        git: &'static str,
        symbols: &'static [(&'static str, bool)],
        _maintainers: &'static [&'static str],
    ) -> (&'static str, LanguageOptions) {
        let options = Self {
            git,
            symbols,
            crate_name: crate_name(lang),
            crate_version: None,
            _maintainers,
        };

        (lang, options)
    }

    fn pairs_with_symbol_and_crate(
        lang: &'static str,
        git: &'static str,
        symbols: &'static [(&'static str, bool)],
        (crate_name, crate_version): (&'static str, Option<&'static str>),
        _maintainers: &'static [&'static str],
    ) -> (&'static str, LanguageOptions) {
        let options = Self {
            git,
            symbols,
            crate_name,
            crate_version,
            _maintainers,
        };

        (lang, options)
    }
}

fn crate_name(lang: &'static str) -> &'static str {
    if lang.contains("_") {
        lang.replace("_", "-").leak()
    } else {
        lang
    }
}

#[cfg(target_os = "macos")]
fn resolve_lib_file(lang: &str) -> String {
    format!("lib{lang}.dylib")
}

#[cfg(target_os = "windows")]
fn resolve_lib_file(lang: &str) -> String {
    format!("{lang}.dll")
}

#[cfg(not(any(target_os = "windows", target_os = "macos")))]
fn resolve_lib_file(lang: &str) -> String {
    format!("lib{lang}.so")
}
