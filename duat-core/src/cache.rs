//! Caching utilities for Duat
//!
//! The cache in Duat is used when restoring previous information from
//! a previous Duat instance, or when reloading Duat's configuration
//! crate.
//!
//! One example for where this is used is the [`Ui`], which has
//! information about how much a given file should be scrolled when
//! opening it in Duat. Another example is [`Cursors`], which stores a
//! list of open [`Cursor`]s, to start the file on.
//!
//! Plugins are able to cache any type that implements [`Serialize`]
//! and [`Deserialize`]. Duat provides these traits from [`serde`],
//! but in order to derive them, you will need a small addition on the
//! declaration of the type:
//!
//! ```rust
//! # use duat_core::cache::{Deserialize, Serialize};
//! #[derive(Deserialize, Serialize)]
//! #[serde(crate = "duat_core::cache::serde")]
//! enum MyCacheableEnum {
//!     // ...
//! }
//! ```
//!
//! [`Ui`]: crate::ui::Ui
//! [`Cursors`]: crate::mode::Cursors
//! [`Cursor`]: crate::mode::Cursor
//! [`Point`]: crate::text::Point
use std::{any::TypeId, io::Write, path::PathBuf};

use base64::Engine;
pub use serde::{self, Deserialize, Serialize};

use crate::{duat_name, src_crate};

/// Tries to load the cache stored by Duat for the given type
///
/// The cache must have been previously stored by [`store_cache`]. If
/// it does not exist, or the file can't be correctly interpreted,
/// returns [`None`]
pub(super) fn load_cache<C>(path: impl Into<PathBuf>) -> Option<C>
where
    C: Deserialize<'static> + 'static,
{
    let path: PathBuf = path.into();
    if TypeId::of::<C>() == TypeId::of::<()>() {
        return None;
    }

    let file_name = path.file_name()?.to_str()?;
    let mut src = dirs_next::cache_dir()?;

    let encoded: String = {
        let base64 = base64::prelude::BASE64_URL_SAFE.encode(path.to_str().unwrap());
        base64.chars().step_by(5).collect()
    };

    src.push("duat/structs");
    src.push(format!("{encoded}:{file_name}"));
    src.push(format!("{}::{}", src_crate::<C>(), duat_name::<C>()));

    let contents = std::fs::read(src).ok()?.leak();
    bincode::deserialize(contents).ok()
}

/// Stores the cache for the given type for that file
///
/// The cache will be stored under
/// `$cache/duat/{base64_path}:{file_name}/{crate}::{type}`.
/// The cache will then later be loaded by [`load_cache`].
pub(super) fn store_cache<C>(path: impl Into<PathBuf>, cache: C)
where
    C: Serialize + 'static,
{
    let path: PathBuf = path.into();
    if TypeId::of::<C>() == TypeId::of::<()>() {
        return;
    }

    let file_name = path.file_name().unwrap().to_str().unwrap();
    let Some(mut src) = dirs_next::cache_dir() else {
        return;
    };

    let encoded: String = {
        let base64 = base64::prelude::BASE64_URL_SAFE.encode(path.to_str().unwrap());
        base64.chars().step_by(5).collect()
    };

    src.push("duat/structs");
    src.push(format!("{encoded}:{file_name}"));

    if !src.exists() {
        std::fs::create_dir_all(src.clone()).unwrap();
    }

    src.push(format!("{}::{}", src_crate::<C>(), duat_name::<C>()));

    let mut contents = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(src)
        .unwrap();

    contents
        .write_all(&bincode::serialize(&cache).unwrap())
        .unwrap();
}

/// Deletes the cache for all types for `path`
///
/// This is done if the file no longer exists, in order to prevent
/// incorrect storage.
pub(super) fn delete_cache(path: impl Into<PathBuf>) {
    let path: PathBuf = path.into();
    let file_name = path.file_name().unwrap().to_str().unwrap();
    let Some(mut src) = dirs_next::cache_dir() else {
        return;
    };

    let encoded: String = {
        let base64 = base64::prelude::BASE64_URL_SAFE.encode(path.to_str().unwrap());
        base64.chars().step_by(5).collect()
    };

    src.push("duat/structs");
    src.push(format!("{encoded}:{file_name}"));

    if src.exists() {
        std::fs::remove_dir_all(src).unwrap();
    }
}
