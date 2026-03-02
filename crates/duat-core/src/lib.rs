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
    /// Persistent storage for structs.
    pub storage_fns: storage::StorageFns,
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

    use notify::event::{AccessKind, AccessMode, Event, EventKind};

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
        collections::HashMap,
        ffi::{OsStr, OsString},
        io::{BufRead, BufReader, ErrorKind, Read},
        process::{Child, Command, Stdio},
        sync::{LazyLock, Mutex, mpsc},
    };

    use interprocess::local_socket::prelude::*;
    pub use interrupt_read::InterruptReader;

    static PERSISTENT_CHILDREN: LazyLock<Mutex<HashMap<String, PersistentChildEntry>>> =
        LazyLock::new(Mutex::default);

    /// An entry for a [`PersistentChild`], which can retrieve the
    /// buffered bytes, keeping them for the next reload cycle.
    struct PersistentChildEntry {
        stdout_rx: mpsc::Receiver<ReaderPair>,
        stderr_rx: mpsc::Receiver<ReaderPair>,
    }

    /// A child process that will persist over multiple reload cycles.
    ///
    /// This child makes use of [`interprocess`]'s [local sockets] for
    /// communication. This is because the process will be spawned by
    /// the duat executor, and communication between it and the config
    /// child process won't be possible by regular stdio.
    ///
    /// Unless you call [`PersistentChild::kill`], duat will assume
    /// that you want it to be kept alive for future reloads.
    ///
    /// # Reloads
    ///
    /// When reloading Duat, the stdin, stdout and stderr processes
    /// are guaranteed to not lose any bytes.
    ///
    /// This is only the case, however, if you don't have some sort of
    /// buffering and/or aren't doing a deserialization attempt with
    /// said data.
    ///
    /// If you want to do deserialization (via [`bincode::Decode`]),
    /// you will want to use [`PersistentReader::decode_as`]. This
    /// method will fail to decode if a reload is requested midway
    /// through reading, but the bytes will be saved for the next
    /// reload cycle, where you can start decoding again.
    ///
    /// [local sockets]: interprocess::local_socket::Stream
    /// [`storage::store`]: crate::storage::store
    pub struct PersistentChild {
        /// The standard input of the [`Child`].
        ///
        /// Note that, unlike `stdout` and `stderr`, `stdin` is not
        /// buffered, and so it is prone to loss of data. The
        /// reasoning for this is that, afaik, at least in rust, most
        /// people send bytes to stdin by serializing a struct into a
        /// slice of bytes, then writing the whole struct at once,
        /// which is immediately followed by a flush.
        ///
        /// This means that no buffering would be done anyways
        pub stdin: Option<LocalSocketStream>,
        /// The standard output of the [`Child`].
        ///
        /// This reader is already buffered, so you don't need to wrap
        /// it in a [`BufReader`] to use it efficiently.
        ///
        /// In order to use this correctly, you must follow one of
        /// three scenarios:
        ///
        /// - A reading loop that never stops. This is the most common
        ///   usecase.
        /// - If you are going to stop, make sure that the reader is
        ///   dropped or that you have called
        ///   [`PersistentReader::give_back`]. This is to ensure that
        ///   any unread bytes are stored correctly when reloading
        ///   Duat.
        /// - If the child has been [killed], then you don't need to
        ///   do anything in particular.
        ///
        /// [killed]: PersistentChild::kill
        pub stdout: Option<PersistentReader>,
        pub stderr: Option<PersistentReader>,
        identifier: String,
    }

    /// A pair used for reading and decoding.
    struct ReaderPair {
        decode_bytes: Vec<u8>,
        conn: BufReader<LocalSocketStream>,
    }

    /// A [`Read`]er which is meant to be used across multiple reload
    /// cycles.
    ///
    /// This reader is _already buffered_, don't wrap it in a
    /// [`BufReader`], or else you _will lose bytes on reloads_.
    pub struct PersistentReader {
        pair: Option<ReaderPair>,
        pair_tx: mpsc::Sender<ReaderPair>,
    }

    impl PersistentReader {
        /// Returns this [`Read`]er (and its bytes) to be retrieved
        /// later on.
        ///
        /// Note that if the [`PersistentChild`] was already [killed],
        /// this won't do anything.
        pub fn give_back(self) {
            drop(self)
        }
    }

    impl Read for PersistentReader {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            let pair = self.pair.as_mut().unwrap();
            match pair.conn.read(buf) {
                Ok(bytes) => Ok(bytes),
                // This means that the equivalent Stream in the parent process was
                // dropped, which means we're about to reload.
                Err(err) if err.kind() == ErrorKind::BrokenPipe => {
                    _ = self.pair_tx.send(self.pair.take().unwrap());
                    // We loop forever, to abstract away the reloading of Duat in reading
                    // loops.
                    loop {
                        std::thread::park();
                    }
                }
                Err(err) => Err(err),
            }
        }
    }

    impl BufRead for PersistentReader {
        fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
            let pair = self.pair.as_mut().unwrap();
            match pair.conn.fill_buf() {
                Ok(_) => {
                    let pair = self.pair.as_ref().unwrap();
                    Ok(pair.conn.buffer())
                }
                Err(ref err) if err.kind() == ErrorKind::BrokenPipe => loop {
                    _ = self.pair_tx.send(self.pair.take().unwrap());
                    loop {
                        std::thread::park();
                    }
                },
                Err(err) => Err(err),
            }
        }

        fn consume(&mut self, amount: usize) {
            let pair = self.pair.as_mut().unwrap();
            pair.conn.consume(amount);
        }
    }

    impl Drop for PersistentReader {
        fn drop(&mut self) {
            // The entry may have already been removed.
            _ = self.pair_tx.send(self.pair.take().unwrap());
        }
    }

    /// Spawn a new `PersistentChild`, which can be reused in
    /// future config reloads.
    ///
    /// The command will forcibly make use of [`Stdio::piped`] for all
    /// stdio. This is because stdin, stdout and stderr are
    /// reserved for use by the [`Ui`] implementation, so something
    /// like [`Stdio::inherit`] would interfere with that.
    pub fn spawn_persistent(
        identifier: impl ToString,
        command: &mut Command,
    ) -> std::io::Result<PersistentChild> {
        let encode = |value: &OsStr| value.as_encoded_bytes().to_vec();

        let args = command.get_args().into_iter().map(encode).collect();
        let envs = command
            .get_envs()
            .into_iter()
            .map(|(k, v)| (encode(k), v.map(encode)))
            .collect();

        let spawn_request = PersistentCommandRequest {
            identifier: identifier.to_string(),
            program: encode(command.get_program()),
            args,
            envs,
        };

        todo!();
    }

    /// A request to spawn a new [`PersistentChild`] process.
    #[doc(hidden)]
    #[derive(bincode::Decode, bincode::Encode)]
    pub struct PersistentCommandRequest {
        identifier: String,
        program: Vec<u8>,
        args: Vec<Vec<u8>>,
        envs: Vec<(Vec<u8>, Option<Vec<u8>>)>,
    }

    impl PersistentCommandRequest {
        /// Spawn the [`Command`].
        ///
        /// Returns the identifier of this command, as well as the
        /// [`Child`] that was spawned.
        ///
        /// This should only be done in the Duat executable.
        // This will become `std::io::RawOsError` once that is stable.
        pub fn spawn(self) -> Result<(String, Child), i32> {
            let decode = |value: Vec<u8>| unsafe { OsString::from_encoded_bytes_unchecked(value) };

            let child = Command::new(decode(self.program))
                .args(self.args.into_iter().map(decode))
                .envs(
                    self.envs
                        .into_iter()
                        .filter_map(|(k, v)| Some((decode(k), decode(v?)))),
                )
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                // TODO: Deal with pre_exec closures.
                .spawn()
                .map_err(|err| err.raw_os_error().unwrap())?;

            Ok((self.identifier, child))
        }
    }

    impl std::fmt::Debug for PersistentCommandRequest {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let decode = |value: Vec<u8>| unsafe { OsString::from_encoded_bytes_unchecked(value) };

            f.debug_struct("PersistentCommandRequest")
                .field("identifier", &self.identifier)
                .field("program", &decode(self.program.clone()))
                .field(
                    "args",
                    &self.args.iter().cloned().map(decode).collect::<Vec<_>>(),
                )
                .field(
                    "envs",
                    &self
                        .envs
                        .iter()
                        .cloned()
                        .map(|(k, v)| (decode(k), v.map(decode)))
                        .collect::<Vec<_>>(),
                )
                .finish()
        }
    }

    #[derive(bincode::Decode, bincode::Encode)]
    struct StoredPersistentChild {
        identifier: String,
        bytes: Vec<u8>,
    }
}

pub mod storage {
    //! Utilities for storing items inbetween reloads.
    use std::sync::OnceLock;

    use bincode::{Decode, Encode, config::standard, error::EncodeError};

    use crate::utils::duat_name;

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
        pub get_if: for<'f> fn(Box<dyn FnMut(&str, &[u8]) -> bool + 'f>) -> Option<Vec<u8>>,
    }

    /// Store a value across reload cycles.
    ///
    /// You can use this function if you want to store a value through
    /// reload cycles, retrieving it after Duat reloads.
    pub fn store<E: Encode + 'static>(value: E) -> Result<(), EncodeError> {
        let value = bincode::encode_to_vec(value, standard())?;
        (STORAGE_FNS.get().unwrap().insert)(duat_name::<E>().to_string(), value);
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
    pub fn get_if<D: Decode<()> + 'static>(mut pred: impl FnMut(&D) -> bool) -> Option<D> {
        let d_name = duat_name::<D>();

        let value = (STORAGE_FNS.get().unwrap().get_if)(Box::new(move |duat_name, bytes| {
            if d_name == duat_name
                && let Some((value, _)) = bincode::decode_from_slice(bytes, standard()).ok()
                && pred(&value)
            {
                true
            } else {
                false
            }
        }))?;

        let (value, _) = bincode::decode_from_slice(&value, standard()).ok()?;
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
