//! The core of Duat, for use by Duat's built-in plugins.
//!
//! This crate isn't really meant for public use, since it is used
//! only by a select few plugins. Configuration crates and plugins
//! should make use of the [duat] crate.
//!
//! One thing to note about this "builti-in plugins" thing though, is
//! that the api of `duat` is a superset of `duat-core`'s api, the
//! only reason why this distinction exists is so I can include some
//! other plugins in `duat`'s api, like `duat-base`,
//! `duat-treesitter`, and `duat-lsp`.
//!
//! [duat]: https://crates.io/duat
#![warn(rustdoc::unescaped_backticks)]
#![allow(clippy::single_range_in_vec_init)]

// This is because of the weird Strs pointer trickery that I'm doing,
// usize _must_ be u64
#[cfg(target_pointer_width = "16")]
compile_error!("This crate does not support 16-bit systems.");
#[cfg(target_pointer_width = "32")]
compile_error!("This crate does not support 32-bit systems.");

use std::{any::TypeId, sync::Mutex};

#[allow(unused_imports)]
use dirs_next::cache_dir;

pub use self::ranges::Ranges;

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

/// A plugin for Duat.
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

static PLUGINS: Plugins = Plugins(Mutex::new(Vec::new()));

/// A struct for [`Plugin`]s to declare dependencies on other
/// [`Plugin`]s.
pub struct Plugins(Mutex<Vec<(PluginFn, TypeId)>>);

impl Plugins {
    /// Returnss a new instance of [`Plugins`].
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub fn _new() -> &'static Self {
        &PLUGINS
    }

    /// Require that a [`Plugin`] be added.
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
        let mut plugins = self.0.lock().unwrap();
        if !plugins.iter().any(|(_, ty)| *ty == TypeId::of::<P>()) {
            plugins.push((
                Some(Box::new(|plugins| P::default().plug(plugins))),
                TypeId::of::<P>(),
            ));
        };
    }
}

// SAFETY: The !Send functions are only accessed from the main thread
unsafe impl Send for Plugins {}
unsafe impl Sync for Plugins {}

/// Functions defined in the application loading the config.
///
/// **FOR USE BY THE DUAT EXECUTABLE ONLY**
#[doc(hidden)]
pub struct MetaFunctions {
    /// Cliboard functions.
    pub clipboard_fns: clipboard::ClipboardFns,
    /// File watching functions.
    pub notify_fns: notify::NotifyFns,
    /// Persistent process spawning functions.
    pub process_fns: process::ProcessFns,
    /// Persistent storage for structs.
    pub storage_fns: storage::StorageFns
}

pub mod clipboard {
    //! Clipboard interaction for Duat.
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::OnceLock;

    /// A clipboard for Duat, can be platform based, or local
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    #[derive(Clone, Copy)]
    pub struct ClipboardFns {
        /// The function to get the text of the clipboard.
        pub get_text: fn() -> Option<String>,
        /// The function to set the text of the clipboard.
        pub set_text: fn(String),
    }

    static CLIPBOARD_FNS: OnceLock<&ClipboardFns> = OnceLock::new();

    /// Gets a [`String`] from the clipboard.
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    pub fn get_text() -> Option<String> {
        (CLIPBOARD_FNS.get().unwrap().get_text)()
    }

    /// Sets a [`String`] to the clipboard.
    pub fn set_text(text: impl std::fmt::Display) {
        (CLIPBOARD_FNS.get().unwrap().set_text)(text.to_string())
    }

    pub(crate) fn set_clipboard(clipb: &'static ClipboardFns) {
        CLIPBOARD_FNS
            .set(clipb)
            .map_err(|_| {})
            .expect("Setup ran twice");
    }
}

pub mod notify {
    //! File watching utility for Duat.
    //!
    //! Provides a simplified interface over the [`notify`] crate.
    //!
    //! [`notify`]: https://crates.io/crates/notify
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
        sync::{
            LazyLock, Mutex, OnceLock,
            atomic::{AtomicBool, AtomicUsize, Ordering::Relaxed},
        },
        time::Duration,
    };

    use notify_types::event::{AccessKind, AccessMode, Event, EventKind};
    pub use notify_types::*;

    static WATCHERS_DISABLED: AtomicBool = AtomicBool::new(false);
    static WATCHER_COUNT: AtomicUsize = AtomicUsize::new(0);
    static NOTIFY_FNS: OnceLock<&NotifyFns> = OnceLock::new();
    static DUAT_WRITES: LazyLock<Mutex<HashMap<PathBuf, usize>>> = LazyLock::new(Mutex::default);

    /// Wether an event came from Duat or not.
    ///
    /// This is only ever [`FromDuat::Yes`] if the event is a write
    /// event resulting from [`Handle::<Buffer>::save`].
    ///
    /// This can be useful if you want to sort events based on
    /// external or internal factors. For example, duat makes use of
    /// this in order to calculate file diffs only if the file was
    /// modified from outside of duat.
    ///
    /// [`Handle::<Buffer>::save`]: crate::context::Handle::save
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum FromDuat {
        /// The event came from Duat, more specifically, it cam from a
        /// function like [`Handle::<Buffer>::save`].
        ///
        /// Another thing to note is that this will only be this value
        /// if _all_ the paths were written by Duat. If this is not
        /// the case, then it will be [`FromDuat::No`]. This usually
        /// isn't an issue, since the vast majority of events emmit
        /// only one path, but it is something to keep in mind.
        ///
        /// [`Handle::<Buffer>::save`]: crate::context::Handle::save
        Yes,
        /// The event didn't come from Duat.
        ///
        /// Note that, even if the event actually came from Duat,
        /// unless it is a write event, it will always be set to this.
        No,
    }

    /// Functions for watching [`Path`]s.
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    #[derive(Debug)]
    pub struct NotifyFns {
        /// Spawn a new [`Watcher`], returning a unique identifier for
        /// it.
        pub spawn_watcher: fn(WatcherCallback) -> std::io::Result<usize>,
        /// Watch a [`Path`] non recursively.
        ///
        /// The `usize` here is supposed to represent a unique
        /// [`Watcher`], previously returned by `spawn_watcher`.
        pub watch_path: fn(usize, &Path) -> std::io::Result<()>,
        /// Watch a [`Path`] recursively.
        ///
        /// The `usize` here is supposed to represent a unique
        /// [`Watcher`], previously returned by `spawn_watcher`.
        pub watch_path_recursive: fn(usize, &Path) -> std::io::Result<()>,
        /// Unwatch a [`Path`].
        ///
        /// The `usize` here is supposed to represent a unique
        /// [`Watcher`], previously returned by `spawn_watcher`.
        pub unwatch_path: fn(usize, &Path) -> std::io::Result<()>,
        /// Unwatch all [`Path`]s.
        ///
        /// The `usize` here is supposed to represent a unique
        /// [`Watcher`], previously returned by `spawn_watcher`.
        pub unwatch_all: fn(usize),
        /// Remove all [`Watcher`]s.
        ///
        /// This function is executed right as Duat is about to quit
        /// or reload.
        pub remove_all_watchers: fn(),
    }

    /// A [`Path`] watcher.
    ///
    /// If this struct is [`drop`]ped, the `Path`s it was watching
    /// will no longer be watched by it.
    pub struct Watcher(usize);

    impl Watcher {
        /// Spawn a new `Watcher`, with a callback function
        ///
        /// You can add paths to watch through [`Watcher::watch`] and
        /// [`Watcher::watch_recursive`].
        pub fn new(
            mut callback: impl FnMut(std::io::Result<Event>, FromDuat) + Send + 'static,
        ) -> std::io::Result<Self> {
            if WATCHERS_DISABLED.load(Relaxed) {
                return Err(std::io::Error::other(
                    "Since Duat is poised to reload, no new Watchers are allowed to be created",
                ));
            }

            let callback = WatcherCallback {
                callback: Box::new(move |event| {
                    use FromDuat::*;
                    let from_duat = if let Ok(Event {
                        kind: EventKind::Access(AccessKind::Close(AccessMode::Write)),
                        paths,
                        ..
                    }) = &event
                        && !paths.is_empty()
                    {
                        let mut duat_writes = DUAT_WRITES.lock().unwrap();
                        let mut all_are_from_duat = true;

                        for path in paths {
                            if let Some(count) = duat_writes.get_mut(path)
                                && *count > 0
                            {
                                *count -= 1;
                            } else {
                                all_are_from_duat = false;
                            }
                        }

                        if all_are_from_duat { Yes } else { No }
                    } else {
                        No
                    };

                    callback(event, from_duat);
                }),
                drop: || _ = WATCHER_COUNT.fetch_sub(1, Relaxed),
            };

            match (NOTIFY_FNS.get().unwrap().spawn_watcher)(callback) {
                Ok(id) => {
                    WATCHER_COUNT.fetch_add(1, Relaxed);
                    Ok(Self(id))
                }
                Err(err) => Err(err),
            }
        }

        /// Watch a [`Path`] non-recursively.
        pub fn watch(&self, path: &Path) -> std::io::Result<()> {
            (NOTIFY_FNS.get().unwrap().watch_path)(self.0, path)
        }

        /// Watch a [`Path`] recursively.
        pub fn watch_recursive(&self, path: &Path) -> std::io::Result<()> {
            (NOTIFY_FNS.get().unwrap().watch_path_recursive)(self.0, path)
        }

        /// Stop watching a [`Path`].
        pub fn unwatch(&self, path: &Path) -> std::io::Result<()> {
            (NOTIFY_FNS.get().unwrap().unwatch_path)(self.0, path)
        }
    }

    impl Drop for Watcher {
        fn drop(&mut self) {
            (NOTIFY_FNS.get().unwrap().unwatch_all)(self.0)
        }
    }

    /// A callback for Watcher events.
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub struct WatcherCallback {
        callback: Box<dyn FnMut(std::io::Result<Event>) + Send + 'static>,
        // This is required, so the WATCHER_COUNT is from the loaded config, not the duat
        // executable.
        drop: fn(),
    }

    impl WatcherCallback {
        /// Calls the callback.
        pub fn call(&mut self, event: std::io::Result<Event>) {
            (self.callback)(event)
        }
    }

    impl Drop for WatcherCallback {
        fn drop(&mut self) {
            (self.drop)();
        }
    }

    /// Declares that the next write event actually came from Duat,
    /// for a given path.
    pub(crate) fn set_next_write_as_from_duat(path: PathBuf) {
        *DUAT_WRITES.lock().unwrap().entry(path).or_insert(0) += 1;
    }

    /// Declares that the next write event didn't come from Duat,
    /// for a given path.
    pub(crate) fn unset_next_write_as_from_duat(path: PathBuf) {
        let mut duat_writes = DUAT_WRITES.lock().unwrap();
        let count = duat_writes.entry(path).or_insert(0);
        *count = count.saturating_sub(1);
    }
    /// Sets the functions for file watching.
    pub(crate) fn set_notify_fns(notify_fns: &'static NotifyFns) {
        NOTIFY_FNS.set(notify_fns).expect("Setup ran twice");
    }

    /// Removes all [`Watcher`]s.
    pub(crate) fn remove_all_watchers() {
        WATCHERS_DISABLED.store(true, Relaxed);
        (NOTIFY_FNS.get().unwrap().remove_all_watchers)();

        let mut watcher_count = WATCHER_COUNT.load(Relaxed);
        while watcher_count > 0 {
            watcher_count = WATCHER_COUNT.load(Relaxed);
            std::thread::sleep(Duration::from_millis(5));
        }
    }
}

pub mod process {
    //! Utilities for spawning processes that should outlive the
    //! config.
    use std::{
        io::{BufWriter, Error},
        process::{Child, ChildStderr, ChildStdin, ChildStdout, Command},
        sync::OnceLock,
    };

    pub use interrupt_read::InterruptReader;

    static PROCESS_FNS: OnceLock<&ProcessFns> = OnceLock::new();

    /// Functions for spawning persistent processes
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    #[derive(Debug)]
    pub struct ProcessFns {
        /// Spawn a [`PersistentChild`], which can outlive this config
        /// reload.
        pub spawn: fn(&mut Command) -> std::io::Result<PersistentChild>,
        /// Get a [`Child`] that was stored with `store_child`, even
        /// in a previous reload cycle.
        pub get_child: fn(String) -> Option<PersistentChild>,
        /// Store a [`Child`], so that it can be loaded with the same
        /// key, in a future reload cycle.
        pub store_child: fn(String, PersistentChild) -> Option<PersistentChild>,
        /// Interrupt all [`PersistentChild`]ren.
        pub interrupt_all: fn(),
        /// How many reader threads are currently spawned.
        pub reader_thread_count: fn() -> usize,
    }

    /// Behaves nearly identically to a regular [`Child`], except the
    /// `stdin` pipe is wrapped in a [`BufWriter`] and the `stdout`
    /// and `stderr` pipes are wrapped in an /// [`InterruptReader`].
    ///
    /// The [`InterruptReader`] is similar to a [`BufReader`] in the
    /// fact that it buffers the input bytes. However, it also comes
    /// with the ability to be interrupted from another thread.
    ///
    /// In Duat, this will be done right before the [`ConfigUnloaded`]
    /// hook is triggered, signaling that Duat is about to quit/unload
    /// the config. This will also make it so [`context::will_unload`]
    /// starts returning `true`, which can be used to stop other
    /// threads on a loop.
    ///
    /// When you're doing your reading loop from the `stdout` and
    /// `stderr`, you should add a check if the return type is
    /// `Err(err) if is_interrupt(&err)` in order to check for that
    /// possibility.
    ///
    /// The [`is_interrupt`] function just checks if the error was
    /// sent because of the aforementioned reason.
    ///
    /// If the error is of that type, it is _your_ responsability to
    /// `break` the reading loop and terminate the thread, so duat can
    /// properly reload (duat won't reload until you do so.).
    ///
    /// [`BufReader`]: std::io::BufReader
    /// [`ConfigUnloaded`]: crate::hook::ConfigUnloaded
    /// [`context::will_unload`]: crate::context::will_unload
    pub struct PersistentChild {
        /// The [`Child`] that was spawned.
        ///
        /// It is guaranteed that `stdin`, `stdout` and `stderr` will
        /// be [`None`], since those will have been moved to the
        /// `PersistentChild`'s version of them.
        pub child: Child,
        /// The handle to a [`Child`]'s standard input, buffered so
        /// you don't have to worry about unwrapping and dealing with
        /// yet to be flushed bytes inbetween reloads
        ///
        /// If you wish to reuse the stdin and are running a writing
        /// loop using the `loop` keyword, try to use `while
        /// !context::will_unload()` instead, alongside a timeout
        /// function for receiving the data that will be sent.
        pub stdin: Option<BufWriter<ChildStdin>>,
        /// A handle to the [`Child`]'s `stdout`, with buffering and
        /// the ability to be interrupted through the
        /// [`ConfigUnloaded`] hook.
        ///
        /// You should check if the reading was interrupted with
        /// [`is_interrupt`]. If that is the case, you should end your
        /// reading loop, as Duat is about to reload the
        /// configuration.
        ///
        /// [`ConfigUnloaded`]: crate::hook::ConfigUnloaded
        pub stdout: Option<InterruptReader<ChildStdout>>,
        /// A handle to the [`Child`]'s `stderr`, with buffering and
        /// the ability to be interrupted through the
        /// [`ConfigUnloaded`] hook.
        ///
        /// You should check if the reading was interrupted with
        /// [`is_interrupt`]. If that is the case, you should end your
        /// reading loop, as Duat is about to reload the
        /// configuration.
        ///
        /// [`ConfigUnloaded`]: crate::hook::ConfigUnloaded
        pub stderr: Option<InterruptReader<ChildStderr>>,
    }

    /// Spawn a new `PersistentChild`, which can be reused in
    /// future config reloads.
    pub fn spawn(command: &mut Command) -> std::io::Result<PersistentChild> {
        (PROCESS_FNS.get().unwrap().spawn)(command)
    }

    /// Get a [`Child`] process that might have been spawned in a
    /// previous reload cycle.
    ///
    /// This function is useful if you want to access spawned
    /// processes that outlive the presently loaded configuration.
    ///
    /// In order to get the `Child`, you must provide a type and a
    /// name. The type is provided in order to prevent others from
    /// accessing this child (it's not fully safe, but it takes effort
    /// to break this), while the name is used to identify this
    /// specific `Child`.
    ///
    /// For the type, you should create a new type specifically for
    /// this, and this type should not be publicly available. If the
    /// type is renamed in between reload cycles, the child will
    /// become an inaccessible zombie.
    pub fn get<KeyType: 'static>(keyname: impl std::fmt::Display) -> Option<PersistentChild> {
        let key = format!("{keyname}{}", std::any::type_name::<KeyType>());
        (PROCESS_FNS.get().unwrap().get_child)(key)
    }

    /// A combination of the [`spawn`] ang [`get`] commands, letting
    /// you "lazyly" spawn processes.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # fn do_stuff_with_line(string: String) {}
    /// use std::{
    ///     io::BufRead,
    ///     mem,
    ///     process::{Command, Stdio},
    /// };
    ///
    /// use duat::{
    ///     prelude::*,
    ///     process::{self, is_interrupt},
    /// };
    ///
    /// // Private key for processes.
    /// struct Key;
    /// struct PluginWithProcesses;
    ///
    /// impl Plugin for PluginWithProcesses {
    ///     fn plug(self, _: &Plugins) {
    ///         let pname = "process1";
    ///         let mut child = process::get_or_spawn::<Key>(pname, || {
    ///             let mut command = Command::new("your-command-name");
    ///             command
    ///                 .stdin(Stdio::piped())
    ///                 .stdout(Stdio::piped())
    ///                 .stderr(Stdio::piped());
    ///             command
    ///         })
    ///         .unwrap();
    ///
    ///         let mut stdout = child.stdout.take().unwrap();
    ///         let join_stdout = std::thread::spawn(move || {
    ///             let mut line = String::new();
    ///             loop {
    ///                 match stdout.read_line(&mut line) {
    ///                     // The Child has exited.
    ///                     Ok(0) => break stdout,
    ///                     Ok(_) => do_stuff_with_line(mem::take(&mut line)),
    ///                     // Duat is about to reload.
    ///                     Err(err) if is_interrupt(&err) => break stdout,
    ///                     Err(err) => context::error!("{err}"),
    ///                 }
    ///             }
    ///         });
    ///
    ///         let mut stderr = child.stderr.take().unwrap();
    ///         let join_stderr = std::thread::spawn(move || {
    ///             // Similar thing as above...
    ///             stderr
    ///         });
    ///
    ///         hook::add_once::<ConfigUnloaded>(move |_, _| {
    ///             let stdout = join_stdout.join().unwrap();
    ///             let stderr = join_stderr.join().unwrap();
    ///
    ///             child.stdout = Some(stdout);
    ///             child.stderr = Some(stderr);
    ///             process::store::<Key>(pname, child);
    ///         });
    ///     }
    /// }
    /// ```
    pub fn get_or_spawn<KeyType: 'static>(
        keyname: impl ToString,
        spawn: impl FnOnce() -> Command,
    ) -> std::io::Result<PersistentChild> {
        let key = format!(
            "{}{}",
            keyname.to_string(),
            std::any::type_name::<KeyType>()
        );
        let process_fns = PROCESS_FNS.get().unwrap();
        match (process_fns.get_child)(key) {
            Some(child) => Ok(child),
            None => (process_fns.spawn)(&mut spawn()),
        }
    }

    /// Store a [`PersistentChild`] process for retrieval on a future
    /// reload.
    ///
    /// In order to store the `Child`, you must provide a type and a
    /// name. The type is provided in order to prevent others from
    /// accessing this child (it's not fully safe, but it takes effort
    /// to break this), while the name is used to identify this
    /// specific `Child`.
    ///
    /// For the type, you should create a new type specifically for
    /// this, and this type should not be publicly available. If the
    /// type is renamed in between reload cycles, the child will
    /// become an inaccessible zombie.
    ///
    /// Returns [`Some`] if there was already a `PersistentChild` in
    /// storage with the same key.
    pub fn store<KeyType: 'static>(
        keyname: impl ToString,
        child: PersistentChild,
    ) -> Option<PersistentChild> {
        let key = format!(
            "{}{}",
            keyname.to_string(),
            std::any::type_name::<KeyType>()
        );
        (PROCESS_FNS.get().unwrap().store_child)(key, child)
    }

    /// Wether the [`std::io::Error`] in question is an interruption
    /// triggered right before [`ConfigUnloaded`].
    ///
    /// You should use this as a second condition to end reading
    /// loops, so you're able to restart them on the next reloading of
    /// the config crate.
    ///
    /// [`ConfigUnloaded`]: crate::hook::ConfigUnloaded
    pub fn is_interrupt(err: &Error) -> bool {
        interrupt_read::is_interrupt(err) && crate::context::will_unload()
    }

    /// Interrupts all [`Interruptor`]s
    ///
    /// This is supposed to be done after
    /// [`context::declare_will_unload`] is called.
    ///
    /// [`context::declare_will_unload`]: crate::context::declare_will_unload
    pub(crate) fn interrupt_all() {
        (PROCESS_FNS.get().unwrap().interrupt_all)()
    }

    /// How many reader threads are still running.
    pub(crate) fn reader_thread_count() -> usize {
        (PROCESS_FNS.get().unwrap().reader_thread_count)()
    }

    /// Sets the [`ProcessFns`].
    pub(crate) fn set_process_fns(process_fns: &'static ProcessFns) {
        PROCESS_FNS.set(process_fns).expect("Setup ran twice");
    }
}

pub mod storage {
    //! Utilities for storing items inbetween reloads.
    use std::sync::OnceLock;

    use bincode::{Decode, Encode, error::EncodeError};

    static STORAGE_FNS: OnceLock<&StorageFns> = OnceLock::new();

    /// Functions for storing persistent values.
    ///
    /// **FOR USE BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    #[derive(Debug)]
    pub struct StorageFns {
        /// Insert a new value into permanent storage.
        pub insert: fn(String, Vec<u8>),
        /// Get a value from permanent storage.
        pub get: fn(String) -> Option<Vec<u8>>,
    }

    /// Store a value across reload cycles.
    ///
    /// You can use this function if you want to store a value through
    /// reload cycles, retrieving it after Duat reloads.
    pub fn store<E: Encode>(key: impl std::fmt::Display, value: E) -> Result<(), EncodeError> {
        let value = bincode::encode_to_vec(value, bincode::config::standard())?;
        let key = format!("{key}{}", std::any::type_name::<E>());
        (STORAGE_FNS.get().unwrap().insert)(key, value);
        Ok(())
    }

    /// Retrieve a value that might have been stored on a previous
    /// reload cycle.
    ///
    /// If a value of type `D` was stored with this key through
    /// [`store`], then this function will return [`Some`] iff:
    ///
    /// - The type's name (through [`std::any::type_name`]) hasn't
    ///   changed.
    /// - The type's fields also haven't changed.
    pub fn get<D: Decode<()>>(key: impl std::fmt::Display) -> Option<D> {
        let key = format!("{key}{}", std::any::type_name::<D>());
        let value = (STORAGE_FNS.get().unwrap().get)(key)?;

        let (value, _) = bincode::decode_from_slice(&value, bincode::config::standard()).ok()?;
        Some(value)
    }

    /// Sets the [`StorageFns`].
    pub(crate) fn set_storage_fns(storage_fns: &'static StorageFns) {
        STORAGE_FNS.set(storage_fns).expect("Setup ran twice");
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

/// Tries to evaluate a block that returns [`Result<T, Text>`]
///
/// If the block returns [`Ok`], this macro will resolve to `T`. If it
/// returns [`Err`], it will log the error with [`context::error!`],
/// then it will return from the function. As an example, this:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # fn test() {
/// use duat::prelude::*;
///
/// let ret = try_or_log_err! {
///     let value = result_fn()?;
///     value
/// };
///
/// fn result_fn() -> Result<usize, Text> {
///     Err(txt!(":("))
/// }
/// # }
/// ```
///
/// Will expand into:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # fn test() {
/// use duat::prelude::*;
///
/// let ret = match (|| -> Result<_, Text> { Ok(result_fn()?) })() {
///     Ok(ret) => ret,
///     Err(err) => {
///         context::error!("{err}");
///         return;
///     }
/// };
///
/// fn result_fn() -> Result<usize, Text> {
///     Err(txt!(":("))
/// }
/// # }
/// ```
///
/// Note the [`Ok`] wrapping the tokens, so it works like the `try`
/// keyword in that regard.
#[macro_export]
macro_rules! try_or_log_err {
    { $($tokens:tt)* } => {
        match (|| -> Result<_, $crate::text::Text> { Ok({ $($tokens)* }) })() {
             Ok(ret) => ret,
             Err(err) => {
                 $crate::context::error!("{err}");
                 return;
             }
        }
    }
}
