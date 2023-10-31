use std::{fs, io::Write, path::PathBuf};

const UI_TO_USE: &[u8] = b"features = [\"term-ui\"]";

fn main() {
    if let Some(config_path) = dirs_next::config_dir() {
        if !config_path.exists() {
            return;
        }

        let dest = config_path.join("duat");
        let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("default-config");

        if dest.exists() {
            return;
        }

        if fs::create_dir_all(&dest).is_err() {
            return;
        };

        if fs::create_dir_all(dest.join("src")).is_err() {
            return;
        };

        fs::copy(source.join("src/lib.rs"), dest.join("src/lib.rs")).unwrap();
        fs::copy(source.join("Cargo.toml"), dest.join("Cargo.toml")).unwrap();

        let mut toml = fs::OpenOptions::new()
            .append(true)
            .open(dest.join("Cargo.toml"))
            .unwrap();

        toml.write_all(UI_TO_USE).unwrap();
    }
}
