//! Caching utilities for Duat
//!
//! The cache in Duat is used primarily for restoring previous
//! information from a previous Duat instance, or when reloading
//! Duat's configuration crate.
//!
//! One example for where this is used is the [`Ui`], which has
//! information about how much a given file should be scrolled when
//! opening it in Duat. Another example is [`Cursors`], which stores a
//! list of open [`Cursor`]s, to start the file on.
//!
//! Plugins are able to store a cache for (almost) any type, by
//! wrapping the type declaration in the[`cacheable!`] macro. This
//! macro will implement the [`CacheAble`] trait, allowing a type to
//! automatically be cached by Duat:
//!
//! ```rust
//! use duat_core::cache::cacheable;
//! cacheable!(
//!     struct Foo {
//!         bar: Bar,
//!         opt: Option<usize>,
//!         arc: Arc<RefCell<Vec<bool>>>
//!     }
//! )
//!
//! cacheable!(struct Bar);
//! ```
//!
//! [`Ui`]: crate::ui::Ui
//! [`Cursors`]: crate::input::Cursors
//! [`Cursor`]: crate::input::Cursor
use std::{any::TypeId, io::Write, ops::Deref, path::PathBuf, str::from_utf8_unchecked};

use base64::Engine;

use crate::{duat_name, src_crate, Error};

/// Tries to load the cache stored by Duat for the given type
///
/// The cache must have been previously stored by [`store_cache`]. If
/// it does not exist, or the file can't be correctly interpreted,
/// returns [`None`]
pub(super) fn load_cache<C>(path: impl Into<PathBuf>) -> Option<C>
where
    C: CacheAble,
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

    src.push("duat");
    src.push(format!("{encoded}:{file_name}"));
    src.push(format!("{}::{}", src_crate::<C>(), duat_name::<C>()));

    let contents = std::fs::read_to_string(src).ok()?;
    let (cache, _) = C::from_cache(contents.as_str()).ok()?;

    Some(cache)
}

/// Stores the cache for the given type for that file
///
/// The cache will be stored under
/// `$cache/duat/{base64_path}:{file_name}/{crate}::{type}`.
/// The cache will then later be loaded by [`load_cache`].
pub(super) fn store_cache<C>(path: impl Into<PathBuf>, cache: C)
where
    C: CacheAble,
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

    src.push("duat");
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

    contents.write_all(cache.as_cache().as_bytes()).unwrap();
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

    src.push("duat");
    src.push(format!("{encoded}:{file_name}"));

    if src.exists() {
        std::fs::remove_dir_all(src).unwrap();
    }
}

impl<C> CacheAble for Option<C>
where
    C: CacheAble,
{
    fn as_cache(&self) -> String {
        match self {
            Some(value) => format!("Some {}", value.as_cache()),
            None => "None ".to_string(),
        }
    }

    fn from_cache(cache: &str) -> crate::Result<(Self, &str), Self> {
        match cache.split_whitespace().next() {
            Some("None") => Ok((None, unsafe {
                from_utf8_unchecked(&cache.as_bytes()["None ".len()..])
            })),
            Some("Some") => {
                let cache = unsafe { from_utf8_unchecked(&cache.as_bytes()["Some ".len()..]) };

                let (value, cache) = C::from_cache(cache).map_err(|_| Error::CacheNotParsed)?;

                Ok((Some(value), cache))
            }
            Some(_) => Err(Error::CacheNotParsed),
            None => Err(Error::CacheNotFound),
        }
    }
}

impl<C, E> CacheAble for Result<C, E>
where
    C: CacheAble,
    E: CacheAble,
{
    fn as_cache(&self) -> String {
        match self {
            Ok(ok) => format!("Ok {}", ok.as_cache()),
            Err(err) => format!("Err {}", err.as_cache()),
        }
    }

    fn from_cache(cache: &str) -> crate::Result<(Self, &str), Self> {
        match cache.split_whitespace().next() {
            Some("Ok") => {
                let cache = unsafe { from_utf8_unchecked(&cache.as_bytes()["Ok ".len()..]) };

                let (value, cache) = C::from_cache(cache).map_err(|_| Error::CacheNotParsed)?;

                Ok((Ok(value), cache))
            }
            Some("Err") => {
                let cache = unsafe { from_utf8_unchecked(&cache.as_bytes()["Err ".len()..]) };

                let (value, cache) = E::from_cache(cache).map_err(|_| Error::CacheNotParsed)?;

                Ok((Err(value), cache))
            }
            Some(_) => Err(Error::CacheNotParsed),
            None => Err(Error::CacheNotFound),
        }
    }
}

impl<C> CacheAble for Vec<C>
where
    C: CacheAble,
{
    fn as_cache(&self) -> String {
        format!("{} {}", self.len(), {
            self.iter()
                .map(|value| value.as_cache())
                .collect::<String>()
        })
    }

    fn from_cache(mut cache: &str) -> crate::Result<(Self, &str), Self> {
        match cache.split_whitespace().next() {
            Some(num) if let Ok(len) = num.parse::<usize>() => {
                let mut vec = Vec::with_capacity(len);
                cache = unsafe { from_utf8_unchecked(&cache.as_bytes()[(num.len() + 1)..]) };

                for _ in 0..len {
                    let (value, c) = C::from_cache(cache).map_err(Error::into_other_type)?;
                    cache = c;
                    vec.push(value);
                }

                Ok((vec, cache))
            }
            Some(_) => Err(Error::CacheNotParsed),
            None => Err(Error::CacheNotFound),
        }
    }
}

impl CacheAble for String {
    fn as_cache(&self) -> String {
        format!("{} {self} ", self.len())
    }

    fn from_cache(mut cache: &str) -> crate::Result<(Self, &str), Self> {
        match cache.split_whitespace().next() {
            Some(num) if let Ok(len) = num.parse::<usize>() => {
                cache = unsafe { from_utf8_unchecked(&cache.as_bytes()[(num.len() + 1)..]) };

                match cache.get(0..len) {
                    Some(str) => Ok((str.to_string(), unsafe {
                        from_utf8_unchecked(&cache.as_bytes()[(len + 1)..])
                    })),
                    None => Err(Error::CacheNotFound),
                }
            }
            Some(_) => Err(Error::CacheNotParsed),
            None => Err(Error::CacheNotParsed),
        }
    }
}

impl<C> CacheAble for gapbuf::GapBuffer<C>
where
    C: CacheAble,
{
    fn as_cache(&self) -> String {
        format!("{} {}", self.len(), {
            self.iter()
                .map(|value| value.as_cache())
                .collect::<String>()
        })
    }

    fn from_cache(mut cache: &str) -> crate::Result<(Self, &str), Self> {
        match cache.split_whitespace().next() {
            Some(num) if let Ok(len) = num.parse::<usize>() => {
                let mut buf = gapbuf::GapBuffer::with_capacity(len);
                cache = unsafe { from_utf8_unchecked(&cache.as_bytes()[(num.len() + 1)..]) };

                for _ in 0..len {
                    let (value, c) = C::from_cache(cache).map_err(Error::into_other_type)?;
                    cache = c;
                    buf.push_back(value);
                }

                Ok((buf, cache))
            }
            Some(_) => Err(Error::CacheNotParsed),
            None => Err(Error::CacheNotFound),
        }
    }
}

/// Marks this struct as cacheable in a file
///
/// A cacheable struct is one that can be stored in a file in order to
/// be retrieved at a later point, in a future execution or reload of
/// Duat.
///
/// # WARNING
///
/// DO NOT IMPLEMENT THIS TRAIT DIRECTLY, instead, use the
/// [`cacheable!`] macro to declare structs that may be cacheable.
pub trait CacheAble: Sized + 'static {
    /// Returns a cached version of the `struct`, for storage by Duat
    ///
    /// For most structs, this means returning something like `"123
    /// None true Ok c 3 32 45 64"`. This type of [`String`] is
    /// perfectly translatable by [`from_cache`] into the implementor
    /// type.
    ///
    /// [`from_cache`]: CacheAble::from_cache
    fn as_cache(&self) -> String;

    /// Recreates the `struct` from a cached [`String`]
    ///
    /// Reads the `&str` defined by [`as_cache`] .This may fail if the
    /// file where the cache was stored in has been tampered by an
    /// external sources.
    ///
    /// [`as_cache`]: CacheAble::as_cache
    fn from_cache(cache: &str) -> crate::Result<(Self, &str), Self>;
}

/// Macro to declare cacheable struct or enum, for use in Duat's cache
///
/// To make a struct cacheable by duat, simply wrap the struct
/// declaration around the macro:
///
/// ```rust
/// # use duat_core::cache::cacheable;
/// cacheable!(
///     struct Foo {
///         nums: Vec<usize>,
///         opt: Option<bool>,
///         atomic: AtomicUsize,
///     }
/// )
/// ```
///
/// In order to be cacheable, all fields in the struct must be one of
/// the following types:
///
/// - Any primitive ([`u32`], [`bool`], [`f64`], etc);
/// - Any atomic ([`AtomicUsize`], [`AtomicI32`], etc);
/// - Any tuple of [`CacheAble`] types (up to 8 elements);
/// - Wrapper types from [`std`] ([`Box`], [`Arc`], [`Mutex`], etc);
/// - [`Option`]s of types that are [`CacheAble`];
/// - [`Vec`]s of types that are [`CacheAble`];
/// - [`Result`]s where the `ok` and `err` variant are [`CacheAble`];
/// - [`GapBuffer`]s of types that are [`CacheAble`];
/// - Other types declared with [`cacheable!`];
///
/// [`AtomicUsize`]: std::sync::atomic::AtomicUsize
/// [`AtomicI32`]: std::sync::atomic::AtomicI32
/// [`GapBuffer`]: gapbuf::GapBuffer
/// [`Arc`]: std::sync::Arc
/// [`Mutex`]: std::sync::Mutex
pub macro cacheable {
    // Implementation for regular structs
    (
        $(#[$struct_meta:meta])*
        $v:vis struct $struct:ident { $(
            $(#[$field_meta:meta])*
            $vf:vis $field:ident: $tf:ty
        ),* $(,)? }
    ) => {
        $(#[$struct_meta])*
        $v struct $struct {
            $(
                $(#[$field_meta])*
                $vf $field: $tf,
            )*
        }

        impl CacheAble for $struct {
            fn as_cache(&self) -> String {
                let mut cache = String::new();
                $(
                    cache += <$tf as CacheAble>::as_cache(&self.$field).as_str();
                )*

                cache
            }

            fn from_cache(mut cache: &str) -> $crate::Result<(Self, &str), Self> {
                $(
                    let ($field, c) = <$tf as CacheAble>::from_cache(cache)
                        .map_err(Error::into_other_type)?;
                    cache = c;
                )*

                Ok((Self { $($field),* }, cache))
            }
        }
    },

    // Implementation for tuple structs
    (@build_tup $name:tt, $cache:expr, ($($elems:expr,)*), []) => {
        Ok(($name ($($elems),*), $cache))
    },
    (@build_tup $name:tt, $cache:expr, ($($elems:expr,)*), [$tf:ty $(, $tfs:tt)*]) => {{
        let (elem, cache) = <$tf>::from_cache($cache).map_err(Error::into_other_type)?;

        cacheable!(@build_tup $name, cache, ($($elems,)* elem,), [$($tfs),*])
    }},

    (@cache_from_tup $tup:ident, $cache:expr, [$($ids:tt),*], []) => {
        $cache
    },
    (@cache_from_tup $tup:ident, $cache:expr, [$id:tt $(, $ids:tt)*], [$tf:ty $(, $tfs:tt)*]) => {{
        $cache += (&$tup . $id).as_cache().as_str();

        cacheable!(@cache_from_tup $tup, $cache, [$($ids),*], [$($tfs),*])
    }},

    (
        $(#[$struct_meta:meta])*
        $v:vis struct $struct:ident ( $(
            $(#[$field_meta:meta])*
            $vf:vis $tf:ty
        ),* $(,)? )
    ) => {
        $(#[$struct_meta])*
        $v struct $struct (
            $(
                $(#[$field_meta])*
                $vf $tf,
            )*
        );

        impl CacheAble for $struct {
            fn as_cache(&self) -> String {
                let mut cache = String::new();
                cacheable!(@cache_from_tup self, cache, [0, 1, 2, 3, 4, 5, 6, 7, 8], [$($tf),*])
            }

            fn from_cache(cache: &str) -> $crate::Result<(Self, &str), Self> {
                cacheable!(@build_tup Self, cache, (), [$($tf),*])
            }
        }
    },

    // Implementation for enums
    (@tup_ident $name:ident $($rest:tt)*) => {
        $name
    },
    (@tup_ident ($first:tt, $($rest:tt),*)) => {
        cacheable!(@tup_ident $first)
    },
    (@tup_ident $first:tt, $($rest:tt),*) => {
        cacheable!(@tup_ident $first)
    },

    (@cache_from_variant $self:expr,) => {
        unreachable!()
    },
    (@cache_from_variant $self:expr, , $($variants:tt)*) => {
        cacheable!(@cache_from_variant $self, $($variants)*)
    },
    (@cache_from_variant
        $self:expr,
        $(#[$variant_meta:meta])*
        $variant:ident { $(
            $(#[$field_meta:meta])*
            $field:ident: $tf:ty
        ),* } $($variants:tt)*
    ) => {{
        if let Self::$variant { $($field),* } = $self {
            let mut cache = stringify!($variant).to_string() + " ";

            $(
                cache += $field.as_cache().as_str();
            )*

            return cache;
        }
        cacheable!(@cache_from_variant $self, $($variants)*)
    }},
    (@cache_from_variant
        $self:expr,
        $(#[$variant_meta:meta])*
        $variant:ident ( $(
            $(#[$field_meta:meta])*
            $tf:tt
        ),* ) $($variants:tt)*
    ) => {{
        if let Self::$variant ($(cacheable!(@tup_ident $tf)),*) = $self {
            let mut cache = stringify!($variant).to_string() + " ";

            $(
                cache += cacheable!(@tup_ident $tf).as_cache().as_str();
            )*

            return cache;
        }
        cacheable!(@cache_from_variant $self, $($variants)*)
    }},
    (@cache_from_variant
        $self:expr,
        $(#[$variant_meta:meta])*
        $variant:ident $($variants:tt)*
    ) => {{
        if let Self::$variant = $self {
            return stringify!($variant).to_string() + " ";
        }
        cacheable!(@cache_from_variant $self, $($variants)*)
    }},

    (@build_variant $cache:expr,) => {
        Err(Error::CacheNotParsed)
    },
    (@build_variant $cache:expr, , $($variants:tt)*) => {
        cacheable!(@build_variant $cache, $($variants)*)
    },
    (@build_variant
        $cache:expr,
        $(#[$variant_meta:meta])*
        $variant:ident { $(
            $(#[$field_meta:meta])*
            $field:ident: $tf:ty
        ),* } $($variants:tt)*
    ) => {{
        match $cache.split_whitespace().next() {
            Some(stringify!($variant)) => {
                $cache = unsafe {
                    from_utf8_unchecked(&$cache.as_bytes()[(stringify!($variant).len() + 1)..])
                };

                $(
                    let ($field, c) = <$tf>::from_cache($cache).map_err(Error::into_other_type)?;
                    $cache = c;
                )*

                return Ok((Self::$variant { $($field)* }, $cache));
            }
            Some(_) => {}
            None => return Err(Error::CacheNotFound),
        }
        cacheable!(@build_variant $cache, $($variants)*)
    }},
    (@build_variant
        $cache:expr,
        $(#[$variant_meta:meta])*
        $variant:ident ( $(
            $(#[$field_meta:meta])*
            $tf:tt
        ),* ) $($variants:tt)*
    ) => {{
        match $cache.split_whitespace().next() {
            Some(stringify!($variant)) => {
                $cache = unsafe {
                    from_utf8_unchecked(&$cache.as_bytes()[(stringify!($variant).len() + 1)..])
                };

                $(
                    let (cacheable!(@tup_ident $tf), c) = <$tf>::from_cache($cache)
                        .map_err(Error::into_other_type)?;
                    $cache = c;
                )*

                return Ok((Self::$variant ( $(cacheable!(@tup_ident $tf)),* ), $cache));
            }
            Some(_) => {}
            None => return Err(Error::CacheNotFound),
        }
        cacheable!(@build_variant $cache, $($variants)*)
    }},
    (@build_variant
        $cache:expr,
        $(#[$variant_meta:meta])*
        $variant:ident $($variants:tt)*
    ) => {{
        match $cache.split_whitespace().next() {
            Some(stringify!($variant)) => {
                $cache = unsafe {
                    from_utf8_unchecked(&$cache.as_bytes()[(stringify!($variant).len() + 1)..])
                };

                return Ok((Self::$variant, $cache));
            }
            Some(_) => {}
            None => return Err(Error::CacheNotFound),
        }
        cacheable!(@build_variant $cache, $($variants)*)
    }},

    (
        $(#[$enum_meta:meta])*
        $v:vis enum $enum:ident { $($variants:tt)* }
    ) => {
        impl CacheAble for $enum {
            fn as_cache(&self) -> String {
                cacheable!(@cache_from_variant self, $($variants)*)
            }

            fn from_cache(mut cache: &str) -> $crate::Result<(Self, &str), Self> {
                cacheable!(@build_variant cache, $($variants)*)
            }
        }

		// This goes after, to color in the types.
        $(#[$enum_meta])*
        $v enum $enum {
            $($variants)*
        }
    }
}

primitive_impl_cacheable!(u8);
primitive_impl_cacheable!(u16);
primitive_impl_cacheable!(u32);
primitive_impl_cacheable!(u64);
primitive_impl_cacheable!(u128);
primitive_impl_cacheable!(usize);
primitive_impl_cacheable!(i8);
primitive_impl_cacheable!(i16);
primitive_impl_cacheable!(i32);
primitive_impl_cacheable!(i64);
primitive_impl_cacheable!(i128);
primitive_impl_cacheable!(isize);
primitive_impl_cacheable!(f32);
primitive_impl_cacheable!(f64);
primitive_impl_cacheable!(bool);
primitive_impl_cacheable!(char);

atomic_impl_cacheable!(std::sync::atomic::AtomicU8);
atomic_impl_cacheable!(std::sync::atomic::AtomicU16);
atomic_impl_cacheable!(std::sync::atomic::AtomicU32);
atomic_impl_cacheable!(std::sync::atomic::AtomicU64);
atomic_impl_cacheable!(std::sync::atomic::AtomicUsize);
atomic_impl_cacheable!(std::sync::atomic::AtomicI8);
atomic_impl_cacheable!(std::sync::atomic::AtomicI16);
atomic_impl_cacheable!(std::sync::atomic::AtomicI32);
atomic_impl_cacheable!(std::sync::atomic::AtomicI64);
atomic_impl_cacheable!(std::sync::atomic::AtomicIsize);
atomic_impl_cacheable!(std::sync::atomic::AtomicBool);

tuple_impl_cacheable!();
tuple_impl_cacheable!(a: A, b: B);
tuple_impl_cacheable!(a: A, b: B, c: C);
tuple_impl_cacheable!(a: A, b: B, c: C, d: D);
tuple_impl_cacheable!(a: A, b: B, c: C, d: D, e: E);
tuple_impl_cacheable!(a: A, b: B, c: C, d: D, e: E, f: F);
tuple_impl_cacheable!(a: A, b: B, c: C, d: D, e: E, f: F, g: G);
tuple_impl_cacheable!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H);
tuple_impl_cacheable!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I);

wrapper_impl_cacheable!(C, Box<C>, Box::deref);
wrapper_impl_cacheable!(C, std::rc::Rc<C>, std::rc::Rc::deref);
wrapper_impl_cacheable!(C, std::sync::Arc<C>, std::sync::Arc::deref);
wrapper_impl_cacheable!(C, std::cell::RefCell<C>, std::cell::RefCell::borrow);
wrapper_impl_cacheable!(C, crate::RwLock<C>, crate::RwLock::read);
wrapper_impl_cacheable!(C, crate::data::RwData<C>, crate::data::RwData::read);
wrapper_impl_cacheable!(C, crate::data::RoData<C>, crate::data::RoData::read);
wrapper_impl_cacheable!(
    C,
    std::sync::Mutex<C>,
    std::sync::Mutex::lock,
    Result::unwrap
);
wrapper_impl_cacheable!(
    C,
    std::sync::RwLock<C>,
    std::sync::RwLock::read,
    Result::unwrap
);

/// [`CacheAble`] implentation for primitive types
macro primitive_impl_cacheable($t:ty) {
    impl CacheAble for $t {
        fn as_cache(&self) -> String {
            self.to_string() + " "
        }

        fn from_cache(cache: &str) -> $crate::Result<(Self, &str), Self> {
            if let Some(word) = cache.split_whitespace().next() {
                let after = unsafe { from_utf8_unchecked(&cache.as_bytes()[(word.len() + 1)..]) };

                word.parse::<$t>()
                    .map_err(|_| $crate::Error::CacheNotParsed)
                    .map(|ret| (ret, after))
            } else {
                Err(Error::CacheNotFound)
            }
        }
    }
}

/// [`CacheAble`] implementation for atomics
macro atomic_impl_cacheable($t:ty) {
    impl CacheAble for $t {
        fn as_cache(&self) -> String {
            let value = self.load(std::sync::atomic::Ordering::Relaxed);
            value.to_string() + " "
        }

        fn from_cache(cache: &str) -> $crate::Result<(Self, &str), Self> {
            if let Some(word) = cache.split_whitespace().next() {
                let after = unsafe { from_utf8_unchecked(&cache.as_bytes()[(word.len() + 1)..]) };

                let value = word.parse().map_err(|_| Error::CacheNotParsed)?;

                Ok((<$t>::new(value), after))
            } else {
                Err(Error::CacheNotFound)
            }
        }
    }
}

/// [`CacheAble`] implementation for tuples
macro tuple_impl_cacheable($($field:ident: $tf:ident),*) {
    impl<$($tf),*> CacheAble for ($($tf),*)
    where
        $($tf: CacheAble),*
    {
        #[allow(unused_mut)]
        fn as_cache(&self) -> String {
            let mut cache = String::new();
            let ($($field),*) = self;

            $(
                cache += $field.as_cache().as_str();
            )*

            cache
        }

        #[allow(unused_mut)]
        fn from_cache(mut cache: &str) -> $crate::Result<(Self, &str), Self> {
            $(
                let ($field, c) = <$tf>::from_cache(cache).map_err(Error::into_other_type)?;
                cache = c;
            )*

            Ok((($($field),*), cache))
        }
    }
}

/// [`CacheAble`] implementation for wrapper types
macro wrapper_impl_cacheable($c:ident, $t:ty, $deref:expr $(, $unwrap:expr)?) {
    impl<$c> CacheAble for $t
    where
        $c: CacheAble,
    {
        fn as_cache(&'_ self) -> String {
            let value = $deref(self);
            $(
                let value = $unwrap(value);
            )?
            value.as_cache()
        }

        fn from_cache(cache: &str) -> $crate::Result<(Self, &str), Self> {
            $c::from_cache(cache)
                .map(|(value, cache)| (<$t>::new(value), cache))
                .map_err(Error::into_other_type)
        }
    }
}
