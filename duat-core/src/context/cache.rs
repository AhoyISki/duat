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
//! use duat_core::context::{Decode, Encode};
//! #[derive(Encode, Decode)]
//! #[bincode(crate = "duat_core::context::bincode")]
//! enum MyCacheableEnum {
//!     // ...
//! }
//! ```
//!
//! [`Ui`]: crate::ui::Ui
//! [`Cursors`]: crate::mode::Cursors
//! [`Cursor`]: crate::mode::Cursor
//! [`Point`]: crate::text::Point
use std::{
    ffi::OsString,
    fs::File,
    hash::{DefaultHasher, Hash, Hasher},
    marker::PhantomData,
    path::PathBuf,
};

pub use bincode::{self, Decode, Encode};

use self::bincode::{
    config::{Configuration, Fixint, LittleEndian, NoLimit},
    encode_into_std_write,
};
use crate::{
    duat_name, src_crate,
    text::{Text, txt},
};

/// Used in order to cache things
pub struct Cache(PhantomData<()>);

impl Cache {
    /// Returns a new instance of [`Cache`]
    pub(crate) fn new() -> Self {
        Self(PhantomData)
    }

    /// Tries to load the cache stored by Duat for the given type
    ///
    /// The cache must have been previously stored by [astore_cache`].
    /// If it does not exist, or the file can't be correctly
    /// interpreted, returns [`None`]
    pub fn load<C: Decode<()> + Default + 'static>(
        &self,
        path: impl Into<PathBuf>,
    ) -> Result<C, Text> {
        let mut cache_file = cache_file::<C>(path.into(), false)?;

        if cache_file.metadata()?.len() == 0 {
            return Ok(C::default());
        }

        let config = Configuration::<LittleEndian, Fixint, NoLimit>::default();
        bincode::decode_from_std_read(&mut cache_file, config).map_err(|err| txt!("{err}").build())
    }

    /// Stores the cache for the given type for that file
    ///
    /// The cache will be stored under
    /// `$cache/duat/{base64_path}:{file_name}/{crate}::{type}`.
    /// The cache can then later be loaded by [`Cache::load`].
    pub fn store<C: Encode + 'static>(
        &self,
        path: impl Into<PathBuf>,
        cache: C,
    ) -> Result<usize, Text> {
        let mut cache_file = cache_file::<C>(path.into(), true)?;

        let config = Configuration::<LittleEndian, Fixint, NoLimit>::default();
        encode_into_std_write(cache, &mut cache_file, config).map_err(|err| txt!("{err}").build())
    }

    /// Deletes the cache for all types for `path`
    ///
    /// This is done if the file no longer exists, in order to prevent
    /// incorrect storage.
    pub fn delete(&self, path: impl Into<PathBuf>) {
        fn delete_cache_inner(path: PathBuf) {
            let (Some(cache_dir), Some(file_name)) = (dirs_next::cache_dir(), path.file_name())
            else {
                return;
            };

            let mut hasher = DefaultHasher::new();
            path.hash(&mut hasher);
            let hash_value = hasher.finish();

            let cached_file_name = {
                let mut name = OsString::from(format!("{hash_value}:"));
                name.push(file_name);
                name
            };

            let src = cache_dir
                .join("duat")
                .join("structs")
                .join(cached_file_name);
            // It could fail if the directory doesn't exist, but we don't really
            // care.
            let _ = std::fs::remove_dir_all(src);
        }

        delete_cache_inner(path.into());
    }

    /// Deletes the cache for everything related to the given `path`
    pub fn delete_for<C: 'static>(&self, path: impl Into<PathBuf>) {
        let path: PathBuf = path.into();
        let (Some(cache_dir), Some(file_name)) = (dirs_next::cache_dir(), path.file_name()) else {
            return;
        };

        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        let hash_value = hasher.finish();

        let cached_file_name = {
            let mut name = OsString::from(format!("{hash_value}:"));
            name.push(file_name);
            name
        };

        let src = cache_dir
            .join("duat")
            .join("structs")
            .join(cached_file_name)
            .join(format!("{}::{}", src_crate::<C>(), duat_name::<C>()));

        if let Ok(true) = src.try_exists() {
            std::fs::remove_file(src).unwrap();
        }
    }
}

fn cache_file<C: 'static>(path: PathBuf, truncate: bool) -> std::io::Result<File> {
    let mut hasher = DefaultHasher::new();
    path.hash(&mut hasher);
    let hash_value = hasher.finish();

    let cached_file_name = {
        let mut name = OsString::from(format!("{hash_value}-"));
        name.push(path.file_name().ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::IsADirectory,
                format!("{path:?} is a directory, not a file for caching"),
            )
        })?);
        name
    };

    let src_dir = dirs_next::cache_dir()
        .ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "cache directory isn't set")
        })?
        .join("duat")
        .join("structs")
        .join(cached_file_name.clone());

    if !src_dir.exists() {
        std::fs::create_dir_all(src_dir.clone())?;
    }

    let src = src_dir.join(format!("{}-{}", src_crate::<C>(), duat_name::<C>()));

    std::fs::OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(truncate)
        .open(src)
}
