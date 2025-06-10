//! This module populates a configuration crate for Duat
//!
//! It will create a directory for a `config` crate in
//! `$XDG_CONFIG_HOME/duat`, only if said directory doesn't exist.
//!
//! Additionlly, it also changes the dependencies to their respective
//! git versions, if the `git-deps` feature was enabled.
//!
//! In the future, it will also be able to make modifications based on
//! the desired `Ui`, and maybe even other choices for default `Mode`.
use core::str;
use std::{
    fs::{self, File},
    io::Write,
};

const UI_TO_USE: &[u8] = b"features = [\"term-ui\"]";

const LIB: &[u8] = include_bytes!("config/src/lib.rs");
const TOML: &[u8] = include_bytes!("config/Cargo_.toml");

fn main() -> std::io::Result<()> {
    let Some(config_path) = dirs_next::config_dir().filter(|p| p.exists()) else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Config path not found",
        ));
    };

    let dest = config_path.join("duat");

    if dest.try_exists()? {
        return Ok(());
    }

    fs::create_dir_all(dest.join("src"))?;

    File::create(dest.join("src/lib.rs"))?.write_all(LIB)?;

    let cut_toml: String = if cfg!(feature = "git-deps") {
        unsafe { str::from_utf8_unchecked(TOML) }.to_string()
    } else {
        unsafe { str::from_utf8_unchecked(TOML) }
            .split_inclusive('\n')
            .filter(|l| !l.contains("git = \""))
            .collect()
    };

    let mut toml = File::create(dest.join("Cargo.toml"))?;
    toml.write_all(cut_toml.as_bytes()).unwrap();
    toml.write_all(UI_TO_USE).unwrap();

    Ok(())
}
