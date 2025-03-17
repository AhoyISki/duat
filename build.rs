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

    if dest.exists() {
        return Ok(());
    }

    fs::create_dir_all(dest.join("src"))?;

    File::create(dest.join("src/lib.rs"))?.write_all(LIB)?;

    let cut_toml: String = if cfg!(feature = "git-deps") {
        unsafe { str::from_utf8_unchecked(TOML) }.to_string()
    } else {
        unsafe { str::from_utf8_unchecked(TOML) }
            .lines()
            .filter(|l| !l.contains("git = \""))
            .collect()
    };

    let mut toml = File::create(dest.join("Cargo.toml"))?;
    toml.write_all(cut_toml.as_bytes()).unwrap();
    toml.write_all(UI_TO_USE).unwrap();

    let toml_path = dest.join("src/Cargo.toml");
    println!("\nCompiling config crate:\n");

    let mut cmd = std::process::Command::new("cargo");
    cmd.args([
        "build",
        "--release",
        "--manifest-path",
        toml_path.to_str().unwrap(),
    ])
    .spawn()?
    .wait()?;

    Ok(())
}
