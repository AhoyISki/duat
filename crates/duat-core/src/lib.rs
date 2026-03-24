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

pub mod clipboard {
    //! Clipboard interaction for Duat.
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::Mutex;

    use crate::session::{self, ipc::MsgFromChild};

    static CLIPBOARD: Mutex<Option<String>> = Mutex::new(None);

    /// Gets a [`String`] from the clipboard.
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    pub fn get() -> Option<String> {
        let content = if cfg!(target_os = "android") {
            None
        } else {
            session::ipc::send(MsgFromChild::RequestClipboard);
            session::ipc::recv_clipboard()
        };

        let mut clipboard = CLIPBOARD.lock().unwrap();

        if let Some(content) = content {
            *clipboard = Some(content);
        }

        clipboard.clone()
    }

    /// Sets the content of the clipboard.
    pub fn set(content: impl std::fmt::Display) {
        let content = content.to_string();
        *CLIPBOARD.lock().unwrap() = Some(content.clone());

        #[cfg(not(target_os = "android"))]
        {
            session::ipc::send(MsgFromChild::UpdateClipboard(content));
        }
    }

    /// Sets the content of the clipboard without changing the system
    /// clipboard.
    pub fn set_local(content: impl std::fmt::Display) {
        let content = content.to_string();
        *CLIPBOARD.lock().unwrap() = Some(content.clone());
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
        sync::{LazyLock, Mutex},
        time::{Duration, Instant},
    };

    pub use notify::event;
    use notify::{
        Config, RecommendedWatcher, RecursiveMode, Watcher as NWatcher,
        event::{AccessKind, AccessMode, Event, EventKind},
    };

    static DUAT_WRITES: LazyLock<Mutex<HashMap<PathBuf, DuatWrite>>> =
        LazyLock::new(Mutex::default);

    /// A record of write events that came from Duat.
    #[derive(Default)]
    struct DuatWrite {
        count: usize,
        last: Option<Instant>,
    }

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

    /// A [`Path`] watcher.
    ///
    /// If this struct is [`drop`]ped, the `Path`s it was watching
    /// will no longer be watched by it.
    pub struct Watcher(Mutex<RecommendedWatcher>);

    impl Watcher {
        /// Spawn a new `Watcher`, with a callback function
        ///
        /// You can add paths to watch through [`Watcher::watch`] and
        /// [`Watcher::watch_recursive`].
        pub fn new(
            mut callback: impl FnMut(notify::Result<Event>, FromDuat) + Send + 'static,
        ) -> notify::Result<Self> {
            Ok(Self(Mutex::new(RecommendedWatcher::new(
                move |event| {
                    use FromDuat::*;
                    let from_duat = if let Ok(Event {
                        kind: EventKind::Access(AccessKind::Close(AccessMode::Write)),
                        paths,
                        ..
                    }) = &event
                        && !paths.is_empty()
                    {
                        let now = Instant::now();
                        let mut duat_writes = DUAT_WRITES.lock().unwrap();
                        let mut all_are_from_duat = true;

                        for path in paths {
                            if let Some(dw) = duat_writes.get_mut(path)
                                && (dw.count > 0
                                    || dw.last.is_some_and(|instant| {
                                        now.duration_since(instant) < Duration::from_millis(2)
                                    }))
                            {
                                dw.count = dw.count.saturating_sub(1);
                                dw.last = Some(now);
                            } else {
                                all_are_from_duat = false;
                            }
                        }

                        if all_are_from_duat { Yes } else { No }
                    } else {
                        No
                    };

                    callback(event, from_duat);
                },
                Config::default(),
            )?)))
        }

        /// Watch a [`Path`] non-recursively.
        pub fn watch(&self, path: &Path) -> notify::Result<()> {
            self.0
                .lock()
                .unwrap()
                .watch(path, RecursiveMode::NonRecursive)
        }

        /// Watch a [`Path`] recursively.
        pub fn watch_recursive(&self, path: &Path) -> notify::Result<()> {
            self.0.lock().unwrap().watch(path, RecursiveMode::Recursive)
        }

        /// Stop watching a [`Path`].
        pub fn unwatch(&self, path: &Path) -> notify::Result<()> {
            self.0.lock().unwrap().unwatch(path)
        }
    }

    impl Drop for Watcher {
        fn drop(&mut self) {}
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
        DUAT_WRITES.lock().unwrap().entry(path).or_default().count += 1;
    }

    /// Declares that the next write event didn't come from Duat,
    /// for a given path.
    pub(crate) fn unset_next_write_as_from_duat(path: PathBuf) {
        let mut duat_writes = DUAT_WRITES.lock().unwrap();
        let dw = duat_writes.entry(path).or_default();
        dw.count = dw.count.saturating_sub(1);
    }
}

pub mod process {
    //! Utilities for spawning processes that should outlive the
    //! config.
    use std::{
        ffi::{OsStr, OsString},
        io::{BufRead, BufWriter, Read, Write},
        process::{Child, Command, Stdio},
        sync::{
            Mutex,
            atomic::{AtomicUsize, Ordering},
            mpsc,
        },
        time::Duration,
    };

    use bincode::{
        Decode, Encode, config,
        error::{DecodeError, EncodeError},
    };
    use interprocess::local_socket::prelude::*;
    pub use interrupt_read::InterruptReader;

    use crate::session::{
        self,
        ipc::{MsgFromChild, ProcessReader},
    };

    /// Spawn a new `PersistentChild`, which can be reused in
    /// future config reloads.
    ///
    /// The command will forcibly make use of [`Stdio::piped`] for all
    /// stdio. This is because stdin, stdout and stderr are
    /// reserved for use by the [`Ui`] implementation, so something
    /// like [`Stdio::inherit`] would interfere with that.
    ///
    /// [`Ui`]: crate::ui::Ui
    pub fn spawn_persistent(command: &mut Command) -> std::io::Result<PersistentChild> {
        let encode = |value: &OsStr| value.as_encoded_bytes().to_vec();

        let args = command.get_args().map(encode).collect();
        let envs = command
            .get_envs()
            .map(|(k, v)| (encode(k), v.map(encode)))
            .collect();

        session::ipc::send(MsgFromChild::SpawnProcess(PersistentSpawnRequest {
            program: encode(command.get_program()),
            args,
            envs,
        }));

        match session::ipc::recv_spawn() {
            Ok(id) => Ok(PersistentChild::new(id, Vec::new(), Vec::new())),
            Err(err) => Err(std::io::Error::from_raw_os_error(err)),
        }
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
    /// # Later retrieval
    ///
    /// If you want to retrieve this `PersistentChild` on a future
    /// reload cycle. You will need to store it by calling
    /// [`storage::store`], from duat's [`storage`] module. You can
    /// only do this once, trying it again will (logically) cause a
    /// panic.
    ///
    /// Since this struct implements [`Decode`] and
    /// [`Encode`], it can be stored and retrieved, even if
    /// it is part of another struct.
    ///
    /// If you don't call `storage::store`, it is assumed that you no
    /// longer need the process, and it will be killed.
    ///
    /// # Unread bytes
    ///
    /// When reloading Duat, the stdin, stdout and stderr processes
    /// are guaranteed to not lose any bytes.
    ///
    /// This is only the case, however, if you don't have some sort of
    /// buffering and/or aren't doing a deserialization attempt with
    /// said data.
    ///
    /// If you want to do deserialization (via [`Decode`]),
    /// you will want to use [`PersistentReader::decode_bytes_as`].
    /// This method will fail to decode if a reload is requested
    /// midway through reading, but the bytes will be saved for
    /// the next reload cycle, where you can start decoding again.
    ///
    /// [local sockets]: interprocess::local_socket::Stream
    /// [`storage::store`]: crate::storage::store
    /// [`storage`]: crate::storage
    pub struct PersistentChild {
        /// The standard input of the [`Child`].
        ///
        /// This struct will send the bytes to an ipc enabled
        /// [`LocalSocketStream`], which will in turn be sent to the
        /// process indirectly, since said process is owned by the
        /// parent executor, not the child.
        pub stdin: Option<PersistentWriter>,
        stdout: Mutex<Option<PersistentReader>>,
        stderr: Mutex<Option<PersistentReader>>,
        stdout_rx: mpsc::Receiver<ReaderPair>,
        stderr_rx: mpsc::Receiver<ReaderPair>,
        id: usize,
    }

    impl PersistentChild {
        fn new(id: usize, stdout_bytes: Vec<u8>, stderr_bytes: Vec<u8>) -> Self {
            let (stdin, [stdout, stderr]) =
                session::ipc::connect_process_channel(id, stdout_bytes, stderr_bytes).unwrap();
            let (stdout_tx, stdout_rx) = mpsc::channel();
            let (stderr_tx, stderr_rx) = mpsc::channel();

            Self {
                stdin: Some(PersistentWriter(RawPersistentWriter(BufWriter::new(stdin)))),
                stdout: Mutex::new(Some(PersistentReader {
                    pair: Some(ReaderPair { decode_bytes: Vec::new(), conn: stdout }),
                    pair_tx: stdout_tx,
                })),
                stderr: Mutex::new(Some(PersistentReader {
                    pair: Some(ReaderPair { decode_bytes: Vec::new(), conn: stderr }),
                    pair_tx: stderr_tx,
                })),
                stdout_rx,
                stderr_rx,
                id,
            }
        }

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
        /// For decoding [`bincode::Decode`] types, you should make
        /// use of the [`PersistentReader::decode_bytes_as`] function,
        /// since that one is not prone to losses if it is interrupted
        /// by a reload.
        ///
        /// [killed]: PersistentChild::kill
        /// [`BufReader`]: std::io::BufReader
        pub fn get_stdout(&self) -> Option<PersistentReader> {
            self.stdout.lock().unwrap().take()
        }

        /// The standard error of the [`Child`].
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
        /// [`BufReader`]: std::io::BufReader
        pub fn get_stderr(&self) -> Option<PersistentReader> {
            self.stderr.lock().unwrap().take()
        }

        /// Kill the [`Child`] process.
        pub fn kill(self) -> std::io::Result<()> {
            session::ipc::send(MsgFromChild::KillProcess(self.id));
            session::ipc::recv_kill().map_err(std::io::Error::from_raw_os_error)
        }
    }

    impl<Context> Decode<Context> for PersistentChild {
        fn decode<D: bincode::de::Decoder<Context = Context>>(
            decoder: &mut D,
        ) -> Result<Self, DecodeError> {
            let stored = StoredPersistentChild::decode(decoder)?;
            Ok(Self::new(
                stored.id,
                stored.stdout_bytes,
                stored.stderr_bytes,
            ))
        }
    }

    impl Encode for PersistentChild {
        /// Encodes the `PersistentChild`
        ///
        /// This can only be done once, trying to do it again will
        /// result in a panic.
        #[track_caller]
        fn encode<E: bincode::enc::Encoder>(
            &self,
            encoder: &mut E,
        ) -> Result<(), bincode::error::EncodeError> {
            session::ipc::send(MsgFromChild::InterruptWrites(self.id));

            let (stdout, stderr) = (
                self.stdout.lock().unwrap().take(),
                self.stderr.lock().unwrap().take(),
            );

            let (stdout_bytes, stderr_bytes) = match (stdout, stderr) {
                (None, None) => {
                    let stdout_bytes = self.stdout_rx.recv().unwrap().consume();
                    let stderr_bytes = self.stderr_rx.recv().unwrap().consume();
                    (stdout_bytes, stderr_bytes)
                }
                (None, Some(mut stderr)) => {
                    let stdout_bytes = self.stdout_rx.recv().unwrap().consume();
                    let stderr_bytes = stderr.pair.take().unwrap().consume();
                    (stdout_bytes, stderr_bytes)
                }
                (Some(mut stdout), None) => {
                    let stdout_bytes = stdout.pair.take().unwrap().consume();
                    let stderr_bytes = self.stderr_rx.recv().unwrap().consume();
                    (stderr_bytes, stdout_bytes)
                }
                (Some(mut stdout), Some(mut stderr)) => {
                    let stdout_bytes = stdout.pair.take().unwrap().consume();
                    let stderr_bytes = stderr.pair.take().unwrap().consume();
                    (stderr_bytes, stdout_bytes)
                }
            };

            StoredPersistentChild { id: self.id, stdout_bytes, stderr_bytes }.encode(encoder)
        }
    }

    impl Drop for PersistentChild {
        fn drop(&mut self) {}
    }

    /// A pair used for reading and decoding.
    struct ReaderPair {
        decode_bytes: Vec<u8>,
        conn: ProcessReader,
    }

    impl ReaderPair {
        /// Consumes the reader, returning all unread bytes.
        fn consume(mut self) -> Vec<u8> {
            _ = self.conn.read_to_end(&mut self.decode_bytes);
            self.decode_bytes
        }
    }

    /// A [`Read`]er which is meant to be used across multiple reload
    /// cycles.
    ///
    /// This reader is _already buffered_, don't wrap it in a
    /// [`BufReader`], or else you _will lose bytes on reloads_.
    ///
    /// [`BufReader`]: std::io::BufReader
    pub struct PersistentReader {
        pair: Option<ReaderPair>,
        pair_tx: mpsc::Sender<ReaderPair>,
    }

    impl PersistentReader {
        /// Attempts to decode the bytes as a type.
        pub fn decode_bytes_as<D: Decode<()>>(&mut self) -> Result<D, DecodeError> {
            struct RepeatReader<'p>(&'p mut PersistentReader);

            impl<'p> Read for RepeatReader<'p> {
                fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
                    let reader = &mut self.0;
                    let pair = reader.pair.as_mut().unwrap();

                    match pair.conn.read(buf) {
                        Ok(0) => {
                            _ = reader.pair_tx.send(reader.pair.take().unwrap());
                            // We loop forever, to abstract away the reloading of Duat in reading
                            // loops.
                            loop {
                                std::thread::park();
                            }
                        }
                        Ok(num_bytes) => {
                            pair.decode_bytes.extend_from_slice(&buf[..num_bytes]);
                            Ok(num_bytes)
                        }
                        Err(err) => Err(err),
                    }
                }
            }

            let value = bincode::decode_from_std_read(&mut RepeatReader(self), config::standard())?;
            self.pair.as_mut().unwrap().decode_bytes.clear();

            Ok(value)
        }

        /// Returns this [`Read`]er (and its bytes) to be retrieved
        /// later on.
        ///
        /// Note that if the [`PersistentChild`] was already [killed],
        /// this won't do anything.
        ///
        /// [killed]: PersistentChild::kill
        pub fn give_back(self) {
            drop(self)
        }
    }

    impl Read for PersistentReader {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            let pair = self.pair.as_mut().unwrap();
            match pair.conn.read(buf) {
                // This means that the equivalent Stream in the parent process was
                // dropped, which means we're about to reload.
                Ok(0) => {
                    _ = self.pair_tx.send(self.pair.take().unwrap());
                    // We loop forever, to abstract away the reloading of Duat in reading
                    // loops.
                    loop {
                        std::thread::park();
                    }
                }
                Ok(bytes) => Ok(bytes),
                Err(err) => Err(err),
            }
        }
    }

    impl BufRead for PersistentReader {
        fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
            let pair = self.pair.as_mut().unwrap();
            match pair.conn.fill_buf() {
                Ok([]) => {
                    _ = self.pair_tx.send(self.pair.take().unwrap());
                    // We loop forever, to abstract away the reloading of Duat in reading
                    // loops.
                    loop {
                        std::thread::park();
                    }
                }
                Ok(_) => {
                    let pair = self.pair.as_ref().unwrap();
                    Ok(pair.conn.buffer())
                }
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
            if let Some(pair) = self.pair.take() {
                // The entry may have already been removed.
                _ = self.pair_tx.send(pair);
            }
        }
    }

    /// A [`Write`]r "lock" for a [`PersistentChild`].
    ///
    /// This struct will send bytes to the `stdin` of a
    /// `PersistentChild`. This is done indirectly through a
    /// [`LocalSocketStream`], since the child process doesn't have
    /// direct access to the stdin of the child, as it belongs to the
    /// parent process.
    ///
    /// This writer is more of a "writer lock" over the actual inner
    /// writer. This is because of reloads.
    ///
    /// When duat reloads, the child process is finished. This could
    /// prematurely end `write` calls of separate threads, leading to
    /// the loss, duplication, or corruption of data.
    ///
    /// That's why this struct has the [`PersistentWriter::on_writer`]
    /// method. This method will give you mutable access to the writer
    /// while preventing duat from reloading.
    ///
    /// You should make use of it in order to "confirm" that a value
    /// has actually been written. Any confirmation outside of this
    /// method can't be trusted.
    pub struct PersistentWriter(RawPersistentWriter);

    impl PersistentWriter {
        /// Calls a function on the inner [`Write`]r.
        ///
        /// This will also prevent Duat from reloading, allowing for
        /// lossless and duplicationless data sending.
        #[track_caller]
        pub fn on_writer<R>(
            &mut self,
            f: impl FnOnce(&mut RawPersistentWriter) -> std::io::Result<R>,
        ) -> std::io::Result<R> {
            use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

            WRITERS_WRITING.fetch_add(1, Ordering::Acquire);
            let result = catch_unwind(AssertUnwindSafe(move || f(&mut self.0)));
            WRITERS_WRITING.fetch_sub(1, Ordering::Release);

            match result {
                Ok(result) => result,
                Err(panic) => resume_unwind(panic),
            }
        }
    }

    /// The writer from [`PersistentWriter::on_writer`].
    ///
    /// This struct is basically just a wrapper over a
    /// [`BufWriter<LocalSocketStream>`], but it also comes with the
    /// [`encode_as_bytes`] method, which lets you encode a value as
    /// bytes in a more convenient, less prone to error way, than
    /// using [`bincode`] by itself.
    ///
    /// [`encode_as_bytes`]: RawPersistentWriter::encode_as_bytes
    pub struct RawPersistentWriter(BufWriter<LocalSocketStream>);

    impl RawPersistentWriter {
        /// Encode the value as bytes, in a duat compatible way.
        ///
        /// Note that you have to call [`Write::flush`] in order to
        /// actually send the data.
        pub fn encode_as_bytes(&mut self, value: impl Encode) -> Result<usize, EncodeError> {
            bincode::encode_into_std_write(value, &mut self.0, config::standard())
        }
    }

    impl Write for RawPersistentWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.0.flush()
        }
    }

    /// A request to spawn a new [`PersistentChild`] process.
    #[doc(hidden)]
    #[derive(Decode, Encode)]
    pub struct PersistentSpawnRequest {
        program: Vec<u8>,
        args: Vec<Vec<u8>>,
        envs: Vec<(Vec<u8>, Option<Vec<u8>>)>,
    }

    impl PersistentSpawnRequest {
        /// Spawn the [`Command`].
        ///
        /// Returns the id of this command, as well as the
        /// [`Child`] that was spawned.
        ///
        /// This should only be done in the Duat executable.
        // This will become `std::io::RawOsError` once that is stable.
        pub fn spawn(self) -> Result<(String, Child), i32> {
            let decode = |value: Vec<u8>| unsafe { OsString::from_encoded_bytes_unchecked(value) };

            let caller = decode(self.program.clone()).to_string_lossy().to_string();

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

            Ok((caller, child))
        }
    }

    impl std::fmt::Debug for PersistentSpawnRequest {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let decode = |value: Vec<u8>| unsafe { OsString::from_encoded_bytes_unchecked(value) };

            f.debug_struct("PersistentSpawnRequest")
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

    /// A stored [`PersistentChild`].
    #[derive(Decode, Encode)]
    struct StoredPersistentChild {
        id: usize,
        stdout_bytes: Vec<u8>,
        stderr_bytes: Vec<u8>,
    }

    /// Wait for a [`PersistentChild`] writers to be done writing.
    pub(crate) fn wait_for_writers() {
        while WRITERS_WRITING.load(Ordering::Relaxed) > 0 {
            std::thread::sleep(Duration::from_millis(1));
        }
    }

    static WRITERS_WRITING: AtomicUsize = AtomicUsize::new(0);
}

pub mod storage {
    //! Utilities for storing items inbetween reloads.
    use std::{
        collections::{HashMap, hash_map::Entry},
        io::Cursor,
        sync::Mutex,
    };

    pub use bincode;
    use bincode::{BorrowDecode, Decode, Encode, config, error::EncodeError};

    use crate::data::Pass;

    static STORED: Mutex<Option<HashMap<String, MaybeTypedValues>>> = Mutex::new(None);

    /// Store a value across reload cycles.
    ///
    /// You can use this function if you want to store a value through
    /// reload cycles, retrieving it after Duat reloads.
    ///
    /// The [`Pass`] argument is used here to ensure that you're doing
    /// this from the main thread, since storing from other threads
    /// could result in the object not _actually_ being stored, if
    /// this function is called in the very small time interval
    /// inbetween duat taking the stored objects out and unloading the
    /// config.
    pub fn store<E>(_: &Pass, value: E) -> Result<(), EncodeError>
    where
        E: Encode + Send + 'static,
    {
        let key = std::any::type_name_of_val(&value).to_string();

        let mut stored_guard = STORED.lock().unwrap_or_else(|err| err.into_inner());
        let stored = stored_guard.as_mut().unwrap();

        match stored.entry(key.clone()) {
            Entry::Occupied(mut occupied_entry) => match occupied_entry.get_mut() {
                MaybeTypedValues::NotTyped(list_of_bytes) => {
                    let mut encoded = std::mem::take(list_of_bytes);
                    bincode::encode_into_std_write(value, &mut encoded, config::standard())?;
                    occupied_entry.insert(MaybeTypedValues::typed(PartiallyDecodedValues::<E> {
                        encoded: Mutex::new(Cursor::new(encoded)),
                        decoded: Vec::new(),
                    }));
                }
                MaybeTypedValues::Typed(typed, _) => {
                    let values: &mut PartiallyDecodedValues<E> = typed.downcast_mut().unwrap();
                    bincode::encode_into_std_write(
                        value,
                        encoded(&values.encoded).get_mut(),
                        config::standard(),
                    )?;
                }
            },
            Entry::Vacant(vacant_entry) => {
                let mut encoded = Vec::new();
                bincode::encode_into_std_write(value, &mut encoded, config::standard())?;
                vacant_entry.insert(MaybeTypedValues::typed(PartiallyDecodedValues::<E> {
                    encoded: Mutex::new(Cursor::new(encoded)),
                    decoded: Vec::new(),
                }));
            }
        };

        Ok(())
    }

    /// Retrieve a value that might have been stored on a previous
    /// reload cycle.
    ///
    /// This will call the predicate on each stored value of type `D`
    /// (not including those stored as fields in structs), and will
    /// extract the value if the predicate returns `true`.
    ///
    /// If the layout of the type has changed inbetween reloads, all
    /// values of said type will be discarded.
    ///
    /// # Note
    ///
    /// This function relies on the name of the stored type. This is
    /// because more reliable indicators (like [`TypeId`]) can't be
    /// relied to be the same inbetween reloads, while a `&str` can.
    ///
    /// This does mean however, that changing the path of a type will
    /// immediately invalidade the stored values, so you should avoid
    /// doing that.
    ///
    /// [`TypeId`]: std::any::TypeId
    pub fn get_if<D>(mut pred: impl FnMut(&D) -> bool) -> Option<D>
    where
        D: Decode<()> + Encode + Send + 'static,
    {
        let key = std::any::type_name::<D>();

        let mut stored_guard = STORED.lock().unwrap_or_else(|err| err.into_inner());
        let stored = stored_guard.as_mut().unwrap();

        let entry = stored.get_mut(key)?;

        let values: &mut PartiallyDecodedValues<D> = match entry {
            MaybeTypedValues::NotTyped(list_of_bytes) => {
                *entry = MaybeTypedValues::typed(PartiallyDecodedValues::<D> {
                    encoded: Mutex::new(Cursor::new(std::mem::take(list_of_bytes))),
                    decoded: Vec::new(),
                });

                let MaybeTypedValues::Typed(values, _) = entry else {
                    unreachable!();
                };

                values.downcast_mut().unwrap()
            }
            MaybeTypedValues::Typed(values, _) => values.downcast_mut().unwrap(),
        };

        if let Some(value) = values.decoded.extract_if(.., |value| pred(value)).next() {
            Some(value)
        } else {
            let mut encoded = encoded(&values.encoded);
            let iter = std::iter::from_fn(|| {
                while encoded.get_ref().len() - encoded.position() as usize > 0 {
                    if let Ok(value) =
                        bincode::decode_from_std_read(&mut *encoded, config::standard())
                    {
                        return Some(value);
                    }
                }

                None
            });

            for value in iter {
                if pred(&value) {
                    return Some(value);
                } else {
                    values.decoded.push(value);
                }
            }

            None
        }
    }

    /// Take the stored structs for retrieval on a future reload.
    pub(crate) fn get_structs() -> HashMap<String, MaybeTypedValues> {
        let mut stored_guard = STORED.lock().unwrap_or_else(|err| err.into_inner());
        let mut stored = stored_guard.take().unwrap();
        stored.retain(|_, maybe_typed| {
            if let MaybeTypedValues::Typed(values, to_bytes) = maybe_typed {
                let values = to_bytes(values.as_mut());
                if values.is_empty() {
                    false
                } else {
                    *maybe_typed = MaybeTypedValues::NotTyped(values);
                    true
                }
            } else {
                true
            }
        });

        stored
    }

    /// A possibly typed list of stored values.
    #[doc(hidden)]
    pub enum MaybeTypedValues {
        NotTyped(Vec<u8>),
        Typed(
            Box<dyn std::any::Any + Send>,
            fn(&(dyn std::any::Any + Send)) -> Vec<u8>,
        ),
    }

    impl MaybeTypedValues {
        fn typed<E: Encode + Send + 'static>(values: PartiallyDecodedValues<E>) -> Self {
            Self::Typed(Box::new(values), |values| {
                let values: &PartiallyDecodedValues<E> = values.downcast_ref().unwrap();
                let mut encoded = encoded(&values.encoded);

                for value in values.decoded.iter() {
                    _ = bincode::encode_into_std_write(
                        value,
                        encoded.get_mut(),
                        config::standard(),
                    );
                }

                let position = encoded.position();
                encoded.get_ref()[position as usize..].to_vec()
            })
        }
    }

    impl std::fmt::Debug for MaybeTypedValues {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::NotTyped(arg0) => f.debug_tuple("NotTyped").field(arg0).finish(),
                Self::Typed(..) => f.debug_tuple("Typed").finish_non_exhaustive(),
            }
        }
    }

    impl<Context> Decode<Context> for MaybeTypedValues {
        fn decode<D: bincode::de::Decoder<Context = Context>>(
            decoder: &mut D,
        ) -> Result<Self, bincode::error::DecodeError> {
            Ok(Self::NotTyped(Decode::decode(decoder)?))
        }
    }

    impl<'de, Context> BorrowDecode<'de, Context> for MaybeTypedValues {
        fn borrow_decode<D: bincode::de::BorrowDecoder<'de, Context = Context>>(
            decoder: &mut D,
        ) -> Result<Self, bincode::error::DecodeError> {
            Ok(Self::NotTyped(Decode::decode(decoder)?))
        }
    }

    impl Encode for MaybeTypedValues {
        fn encode<E: bincode::enc::Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
            match self {
                MaybeTypedValues::NotTyped(list_of_bytes) => {
                    Encode::encode(&list_of_bytes, encoder)
                }
                MaybeTypedValues::Typed(any, get_list_of_bytes) => {
                    Encode::encode(&get_list_of_bytes(any.as_ref()), encoder)
                }
            }
        }
    }

    struct PartiallyDecodedValues<T> {
        encoded: Mutex<Cursor<Vec<u8>>>,
        decoded: Vec<T>,
    }

    /// Set the initial list of stored structs.
    pub(crate) fn set_structs(structs: HashMap<String, MaybeTypedValues>) {
        *STORED.lock().unwrap() = Some(structs);
    }

    /// Get the encoded [`Cursor`] bytes.
    fn encoded(encoded: &Mutex<Cursor<Vec<u8>>>) -> std::sync::MutexGuard<'_, Cursor<Vec<u8>>> {
        encoded.lock().unwrap_or_else(|err| err.into_inner())
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
