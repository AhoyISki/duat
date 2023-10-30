use std::{
    fs, io,
    path::{Path, PathBuf},
};

fn main() {
    if let Some(config_path) = dirs_next::config_dir() {
        if !config_path.exists() {
            return;
        }

        let dest = config_path.join("duat");
        let source = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("default-config");

        copy_dir_all(source, dest).unwrap();
    }
}

fn copy_dir_all(source: impl AsRef<Path>, dest: impl AsRef<Path>) -> io::Result<()> {
    fs::create_dir_all(&dest)?;
    for entry in fs::read_dir(source)? {
        let entry = entry?;

        let dest = dest.as_ref().join(entry.file_name());
        if dest.exists() {
            continue;
        }

        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir_all(entry.path(), dest)?;
        } else {
            fs::copy(entry.path(), dest)?;
        }
    }
    Ok(())
}
