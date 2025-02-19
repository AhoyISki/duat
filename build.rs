use core::str;
use std::{
    fs::{self, File},
    io::Write,
};

const UI_TO_USE: &[u8] = b"features = [\"term-ui\"]";

const LIB: &[u8] = include_bytes!("config/src/lib.rs");
const TOML: &[u8] = include_bytes!("config/Cargo.toml");

fn main() {
    let Some(config_path) = dirs_next::config_dir() else {
        return;
    };

    if !config_path.exists() {
        return;
    }

    let dest = config_path.join("duat");

    if dest.exists() {
        return;
    }

    if fs::create_dir_all(&dest).is_err() {
        return;
    };

    if fs::create_dir_all(dest.join("src")).is_err() {
        return;
    };

    let mut src = File::create(dest.join("src/lib.rs")).unwrap();
    src.write_all(LIB).unwrap();

    let mut toml = File::create(dest.join("Cargo.toml")).unwrap();

    let cut_toml: String = unsafe { str::from_utf8_unchecked(TOML) }
        .split_inclusive('\n')
        .filter(|l| !l.contains("path = \""))
        .collect();

    toml.write_all(cut_toml.as_bytes()).unwrap();
    toml.write_all(UI_TO_USE).unwrap();
}
