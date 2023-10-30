use std::{fs::{self, DirEntry}, path::PathBuf};

fn main() {
    if let Some(config_path) = dirs_next::config_dir() {
        let parsec_path = config_path.join("parsec");

        if fs::create_dir_all(&parsec_path).is_err() {
            return;
        }

        let default_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("default-config");

        let _ = fs::copy(default_path, parsec_path);
    }
}
