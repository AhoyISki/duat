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
//! Plugins are able to cache any type that implements [`Encode`]
//! and [`Decode`]. Duat provides these traits from the [`bincode`]
//! crate, but in order to derive them, you will need a small addition
//! on the declaration of the type:
//!
//! ```rust
//! # use duat_core::cache::{Encode, Decode};
//! #[derive(Encode, Decode)]
//! #[bincode(crate = "duat_core::cache::bincode")]
//! enum MyCacheableEnum {
//!     // ...
//! }
//! ```
//!
//! [`Ui`]: crate::ui::Ui
//! [`Cursors`]: crate::mode::Cursors
//! [`Cursor`]: crate::mode::Cursor
//! [`Point`]: crate::text::Point
use std::{any::TypeId, fs::File, path::PathBuf};

use base64::Engine;
pub use bincode::{self, Decode, Encode};

use self::bincode::{
    config::{Configuration, Fixint, LittleEndian, NoLimit},
    encode_into_std_write,
};
use crate::{duat_name, src_crate};

/// Tries to load the cache stored by Duat for the given type
///
/// The cache must have been previously stored by [astore_cache`]. If
/// it does not exist, or the file can't be correctly interpreted,
/// returns [`None`]
pub(super) fn load_cache<C: Decode<()> + 'static>(path: impl Into<PathBuf>) -> Option<C> {
    fn contents(path: PathBuf, type_id: TypeId, type_name: String) -> Option<File> {
        if type_id == TypeId::of::<()>() {
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
        src.push(type_name);

        std::fs::OpenOptions::new().read(true).open(src).ok()
    }

    let type_name = format!("{}::{}", src_crate::<C>(), duat_name::<C>());
    let mut file = contents(path.into(), TypeId::of::<C>(), type_name)?;

    let config = Configuration::<LittleEndian, Fixint, NoLimit>::default();
    bincode::decode_from_std_read(&mut file, config).ok()
}

/// Stores the cache for the given type for that file
///
/// The cache will be stored under
/// `$cache/duat/{base64_path}:{file_name}/{crate}::{type}`.
/// The cache will then later be loaded by [`load_cache`].
pub(super) fn store_cache<C: Encode + 'static>(path: impl Into<PathBuf>, cache: C) {
    fn cache_file(path: PathBuf, type_id: TypeId, type_name: String) -> Option<File> {
        if type_id == TypeId::of::<()>() {
            return None;
        }

        let file_name = path.file_name().unwrap().to_str().unwrap();
        let mut src = dirs_next::cache_dir()?;

        let encoded: String = {
            let base64 = base64::prelude::BASE64_URL_SAFE.encode(path.to_str().unwrap());
            base64.chars().step_by(5).collect()
        };

        src.push("duat/structs");
        src.push(format!("{encoded}:{file_name}"));

        if !src.exists() {
            std::fs::create_dir_all(src.clone()).unwrap();
        }

        src.push(type_name);

        std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(src)
            .ok()
    }

    let type_name = format!("{}::{}", src_crate::<C>(), duat_name::<C>());
    let Some(mut cache_file) = cache_file(path.into(), TypeId::of::<C>(), type_name) else {
        return;
    };

    let config = Configuration::<LittleEndian, Fixint, NoLimit>::default();
    encode_into_std_write(cache, &mut cache_file, config).unwrap();
}

/// Deletes the cache for all types for `path`
///
/// This is done if the file no longer exists, in order to prevent
/// incorrect storage.
pub(super) fn delete_cache(path: impl Into<PathBuf>) {
    fn delete_cache_inner(path: PathBuf) {
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

    delete_cache_inner(path.into());
}

pub(super) fn delete_cache_for<C: 'static>(path: impl Into<PathBuf>) {
    fn delete_cache_for_inner(path: PathBuf, type_name: String) {
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
        src.push(type_name);

        if src.exists() {
            std::fs::remove_file(src).unwrap();
        }
    }

    let type_name = format!("{}::{}", src_crate::<C>(), duat_name::<C>());
    delete_cache_for_inner(path.into(), type_name);
}
