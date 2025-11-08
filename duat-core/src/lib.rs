//! The core of Duat, for use by Duat's built-in plugins.
//!
//! This crate isn't really meant for public use, since it is used
//! only by a select few plugins. Configuration crates and plugins
//! should make use of the [duat] crate.
//!
//! [duat]: https://crates.io/duat
#![feature(dropck_eyepatch)]
#![warn(rustdoc::unescaped_backticks)]
#![allow(clippy::single_range_in_vec_init)]

use std::any::TypeId;

#[allow(unused_imports)]
use dirs_next::cache_dir;
pub use lender;
use parking_lot::Mutex;

pub use self::{main_thread_only::MainThreadOnly, ranges::Ranges};

pub mod buffer;
pub mod cmd;
pub mod context;
pub mod data;
pub mod form;
pub mod hook;
pub mod mode;
pub mod opts;
mod ranges;
#[doc(hidden)]
pub mod session;
pub mod text;
pub mod ui;
pub mod utils;

/// A plugin for Duat
///
/// Plugins should mostly follow the builder pattern, but you can use
/// fields if you wish to. When calling [`Plugin::plug`], the plugin's
/// settings should be taken into account, and all of its setup should
/// be done:
///
/// ```rust
/// # use duat_core::{Plugin, Plugins};
/// // It's not a supertrait of Plugin, but you must implement
/// // Default in order to use the plugin.
/// #[derive(Default)]
/// struct MyPlugin(bool);
///
/// impl Plugin for MyPlugin {
///     // With the Plugins struct, you can require other plugins
///     // within your plugin
///     fn plug(self, plugins: &Plugins) {
///         //..
///     }
/// }
///
/// impl MyPlugin {
///     /// Returns a new instance of the [`MyPlugin`] plugin
///     pub fn new() -> Self {
///         Self(false)
///     }
///
///     /// Modifies [`MyPlugin`]
///     pub fn modify(self) -> Self {
///         //..
/// #       self
///     }
/// }
/// ```
///
/// [plugged]: Plugin::plug
/// [`PhantomData`]: std::marker::PhantomData
pub trait Plugin: 'static {
    /// Sets up the [`Plugin`]
    fn plug(self, plugins: &Plugins);
}

static PLUGINS: Plugins = Plugins(MainThreadOnly::new(Mutex::new(Vec::new())));

/// A struct for [`Plugin`]s to declare dependencies on other
/// [`Plugin`]s
pub struct Plugins(MainThreadOnly<Mutex<Vec<(PluginFn, TypeId)>>>);

impl Plugins {
    /// Returnss a new instance of [`Plugins`]
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub fn _new() -> &'static Self {
        &PLUGINS
    }

    /// Require that a [`Plugin`] be added
    ///
    /// This plugin may have already been added, or it might be added
    /// by this call.
    ///
    /// For built-in [`Plugin`]s, if they are required by some
    /// `Plugin`, then they will be added before that `Plugin` is
    /// added. Otherwise, they will be added at the end of the `setup`
    /// function.
    pub fn require<P: Plugin + Default>(&self) {
        // SAFETY: This function can only push new elements to the list, not
        // accessing the !Send functions within.
        let mut plugins = unsafe { self.0.get() }.lock();
        if !plugins.iter().any(|(_, ty)| *ty == TypeId::of::<P>()) {
            plugins.push((
                Some(Box::new(|plugins| P::default().plug(plugins))),
                TypeId::of::<P>(),
            ));
        };
    }
}

mod main_thread_only {
    /// A container meant for access in only the main thread
    ///
    /// Use this if you want a static value that is not
    /// [`Send`]/[`Sync`].
    #[derive(Default)]
    #[doc(hidden)]
    pub struct MainThreadOnly<T>(T);

    impl<T> MainThreadOnly<T> {
        /// Returns a new [`MainThreadOnly`]
        pub const fn new(value: T) -> Self {
            Self(value)
        }

        /// Acquires the inner value.
        ///
        /// # Safety
        ///
        /// You must ensure that this operation is taking place in the
        /// main thread of execution, although this function might
        /// take a [`Pass`] parameter later on, in order to
        /// lift that requirement.
        ///
        /// [`Pass`]: crate::data::Pass
        pub unsafe fn get(&self) -> &T {
            &self.0
        }
    }

    unsafe impl<T> Send for MainThreadOnly<T> {}
    unsafe impl<T> Sync for MainThreadOnly<T> {}
}

pub mod clipboard {
    //! Clipboard interaction for Duat
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::{Mutex, OnceLock};

    /// A clipboard for Duat, can be platform based, or local
    #[doc(hidden)]
    #[allow(private_interfaces)]
    pub enum Clipboard {
        #[cfg(target_os = "android")]
        Platform,
        #[cfg(not(target_os = "android"))]
        Platform(arboard::Clipboard, &'static ClipboardFunctions),
        Local(String),
    }

    impl Default for Clipboard {
        fn default() -> Self {
            #[cfg(not(target_os = "android"))]
            match arboard::Clipboard::new() {
                Ok(clipb) => Self::Platform(clipb, ClipboardFunctions::new()),
                Err(_) => Self::Local(String::new()),
            }

            #[cfg(target_os = "android")]
            Self::Platform
        }
    }

    static CLIPB: OnceLock<&'static Mutex<Clipboard>> = OnceLock::new();

    /// Gets a [`String`] from the clipboard
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    ///
    /// Or if there is no clipboard I guess
    pub fn get_text() -> Option<String> {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
            #[cfg(target_os = "android")]
            Clipboard::Platform => clipboard::get_text()
                .map_err(|err| crate::context::error!("{err}"))
                .ok(),
            #[cfg(not(target_os = "android"))]
            Clipboard::Platform(clipb, fns) => (fns.get_text)(clipb),
            Clipboard::Local(clipb) => Some(clipb.clone()).filter(String::is_empty),
        }
    }

    /// Sets a [`String`] to the clipboard
    pub fn set_text(text: impl std::fmt::Display) {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
            #[cfg(target_os = "android")]
            Clipboard::Platform => {
                if let Err(err) = clipboard::set_text(text.to_string()) {
                    crate::context::error!("{err}");
                }
            }
            #[cfg(not(target_os = "android"))]
            Clipboard::Platform(clipb, fns) => (fns.set_text)(clipb, text.to_string()),
            Clipboard::Local(clipb) => *clipb = text.to_string(),
        }
    }

    #[cfg(not(target_os = "android"))]
    struct ClipboardFunctions {
        get_text: fn(&mut arboard::Clipboard) -> Option<String>,
        set_text: fn(&mut arboard::Clipboard, String),
    }

    impl ClipboardFunctions {
        fn new() -> &'static Self {
            &Self {
                get_text: |clipb| clipb.get_text().ok(),
                set_text: |clipb, text| clipb.set_text(text).unwrap(),
            }
        }
    }

    pub(crate) fn set_clipboard(clipb: &'static Mutex<Clipboard>) {
        CLIPB.set(clipb).map_err(|_| {}).expect("Setup ran twice");
    }
}

////////// Text Builder macros (for pub/private bending)
#[doc(hidden)]
pub mod private_exports {
    pub use format_like::format_like;
}

/// Converts a string to a valid priority
#[doc(hidden)]
pub const fn priority(priority: &str) -> u8 {
    let mut bytes = priority.as_bytes();
    let mut val = 0;

    while let [byte, rest @ ..] = bytes {
        assert!(b'0' <= *byte && *byte <= b'9', "invalid digit");
        val = val * 10 + (*byte - b'0') as usize;
        bytes = rest;
    }

    assert!(val <= 250, "priority cannot exceed 250");

    val as u8
}

type PluginFn = Option<Box<dyn FnOnce(&Plugins)>>;
