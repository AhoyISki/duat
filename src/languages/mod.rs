use std::{path::PathBuf, process::Command};

use dlopen_rs::{Dylib, OpenFlags};
use duat_core::Mutex;
use indoc::{formatdoc, indoc};
use tree_sitter::Language;
use tree_sitter_language::LanguageFn;

use self::list::LANGUAGE_OPTIONS;

mod list;

pub fn get_language(filetype: &str) -> Option<Language> {
    static LIBRARIES: Mutex<Vec<Dylib>> = Mutex::new(Vec::new());

    let workspace = get_workspace_dir()?;
    let options = LANGUAGE_OPTIONS.get(filetype)?;

    let lib_dir = workspace.join("lib");
    let crate_dir = workspace.join(options.crate_name);
    let manifest_path = crate_dir.join("Cargo.toml");
    let so_path = lib_dir.join(format!("lib{}.so", options.crate_name));

    if let Ok(lib) = dlopen_rs::ElfLibrary::dlopen(
        so_path,
        OpenFlags::RTLD_NOW | OpenFlags::CUSTOM_NOT_REGISTER,
    ) {
        let language = if options.is_function {
            let symbol = unsafe { lib.get::<fn() -> Language>(options.symbol).ok()? };
            symbol()
        } else {
            let symbol = unsafe { lib.get::<LanguageFn>(options.symbol).ok()? };
            (*symbol).into()
        };

        // Push after getting the symbol, to ensure that the lib does indeed
        // have it.
        LIBRARIES.lock().push(lib);

        Some(language)
    } else if let Ok(true) = std::fs::exists(&manifest_path) {
        let mut cargo = Command::new("cargo");

        cargo.args([
            "build",
            "--release",
            "--manifest-path",
            manifest_path.to_str().unwrap(),
            "-Z",
            "unstable-options",
            "--artifact-dir",
            lib_dir.to_str().unwrap(),
        ]);

        if let Ok(out) = cargo.output()
            && out.status.success()
        {
            get_language(filetype)
        } else {
            None
        }
    } else {
        const LIB_RS: &str = "pub use ts::*;";

        let crate_name = options.crate_name;

        let cargo_toml = formatdoc!(
            r#"
                [package]
				name = "ts-{crate_name}"
    		    version = "0.1.0"
        		edition = "2024"
        		description = "Dynamic wrapper for tree-sitter-{crate_name}"

                [lib]
                name = "{crate_name}"
                crate-type = ["dylib"]

				[dependencies.ts]
    		    version = "*"
        		git = "{}"
                package = "tree-sitter-{crate_name}""#,
            options.git
        );

        std::fs::create_dir_all(crate_dir.join("src")).ok()?;
        std::fs::write(manifest_path, cargo_toml).ok()?;
        std::fs::write(crate_dir.join("src/lib.rs"), LIB_RS).ok()?;

        get_language(filetype)
    }
}

fn get_workspace_dir() -> Option<PathBuf> {
    const BASE_WORKSPACE_TOML: &str = indoc!(
        r#"
        [workspace]
        resolver = "2"
        members = [ "*" ]"#
    );

    let workspace_dir = duat_core::plugin_dir("duat-treesitter")?;
    let manifest_path = workspace_dir.join("parsers/Cargo.toml");

    if let Ok(false) | Err(_) = std::fs::exists(&manifest_path) {
        std::fs::write(manifest_path, BASE_WORKSPACE_TOML).ok()?
    }

    Some(workspace_dir)
}

struct LanguageOptions {
    git: &'static str,
    symbol: &'static str,
    is_function: bool,
    crate_name: &'static str,
    _maintainers: &'static [&'static str],
}

impl LanguageOptions {
    fn pair(
        lang: &'static str,
        git: &'static str,
        _maintainers: &'static [&'static str],
    ) -> (&'static str, Self) {
        let options = Self {
            git,
            symbol: "LANGUAGE",
            is_function: false,
            crate_name: crate_name(lang),
            _maintainers,
        };

        (lang, options)
    }

    fn pair_with_symbol(
        lang: &'static str,
        git: &'static str,
        (symbol, is_function): (&'static str, bool),
        maintainers: &'static [&'static str],
    ) -> (&'static str, LanguageOptions) {
        let options = Self {
            git,
            symbol,
            is_function,
            crate_name: crate_name(lang),
            _maintainers: maintainers,
        };

        (lang, options)
    }

    fn pair_with_symbol_and_crate(
        lang: &'static str,
        git: &'static str,
        (symbol, is_function): (&'static str, bool),
        crate_name: &'static str,
        maintainers: &'static [&'static str],
    ) -> (&'static str, LanguageOptions) {
        let options = Self {
            git,
            symbol,
            is_function,
            crate_name,
            _maintainers: maintainers,
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
