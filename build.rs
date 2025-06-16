use std::{fs, io, path::Path};

fn main() -> std::io::Result<()> {
    if std::env::var("DOCS_RS").is_ok() {
        return Ok(());
    }
    let Some(config_path) = dirs_next::data_local_dir().filter(|p| p.exists()) else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Config path not found",
        ));
    };

    let dest = config_path.join("duat/plugins/duat-treesitter/queries/");

    if dest.try_exists()? {
        return Ok(());
    }

    copy_dir_all("queries", dest)
}

fn copy_dir_all(src: impl AsRef<Path>, dst: impl AsRef<Path>) -> io::Result<()> {
    fs::create_dir_all(&dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        if ty.is_dir() {
            copy_dir_all(entry.path(), dst.as_ref().join(entry.file_name()))?;
        } else {
            fs::copy(entry.path(), dst.as_ref().join(entry.file_name()))?;
        }
    }
    Ok(())
}
