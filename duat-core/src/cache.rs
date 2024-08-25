use std::{any::TypeId, io::Write, path::PathBuf, str::from_utf8_unchecked};

use base64::Engine;

use crate::{duat_name, src_crate, Error};

pub(super) fn get_cache<C>(path: impl Into<PathBuf>) -> Option<C>
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

pub fn delete_cache(path: impl Into<PathBuf>) {
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
    fn as_cache(&self) -> String;

    fn from_cache(cache: &str) -> crate::Result<(Self, &str), Self>;
}

pub macro cacheable {
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

        impl $crate::cache::CacheAble for $struct {
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

                Ok((
                    Self { $($field),* },
                    cache,
                ))
            }
        }
    }
}

/// [`CacheAble`] implentation for primitives.
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

/// [`CacheAble`] implementation for atomics.
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
