use std::sync::{
    Mutex, OnceLock,
    atomic::{AtomicUsize, Ordering},
};

pub use log::{Level, Metadata};

pub use self::macros::*;
use crate::text::{Selectionless, Text};

mod macros {
    /// Logs an error to Duat
    ///
    /// Use this, as opposed to [`warn!`], [`info!`] or [`debug!`],
    /// if you want to tell the user that something explicitely
    /// failed, and they need to find a workaround, like failing
    /// to write to/read from a file, for example.
    ///
    /// This error follows the same construction as the [`txt!`]
    /// macro, and will create a [`Record`] inside of the [`Logs`],
    /// which can be accessed by anyone, at any time.
    ///
    /// The [`Record`] added to the [`Logs`] is related to
    /// [`log::Record`], from the [`log`] crate. But it differs in the
    /// sense that it is always `'static`, and instead of having an
    /// [`std::fmt::Arguments`] inside, it contains a [`Text`], making
    /// it a better fit for Duat.
    ///
    /// The connection to [`log::Record`] also means that external
    /// libraries can log information using the [`log`] crate, and it
    /// will also show up in Duat's [`Logs`]s, but reformatted to be a
    /// [`Text`] instead.
    ///
    /// [`txt!`]: crate::text::txt
    /// [`Record`]: super::Record
    /// [`Logs`]: super::Logs
    /// [`Text`]: crate::text::Text
    pub macro error {
        (target: $target:expr, $($arg:tt)+) => ({
            $crate::private_exports::log!(
                $target.to_string().leak(),
                $crate::context::Level::Error,
                $($arg)+
            )
        }),
        ($($arg:tt)+) => (
            $crate::private_exports::log!(
                module_path!().to_string().leak(),
                $crate::context::Level::Error,
                $($arg)+
            )
        )
    }

    /// Logs an warning to Duat
    ///
    /// Use this, as opposed to [`error!`], [`info!`] or [`debug!`],
    /// if you want to tell the user that something was partially
    /// successful, or that a failure happened, but
    /// it's near inconsequential.
    ///
    /// This error follows the same construction as the [`txt!`]
    /// macro, and will create a [`Record`] inside of the [`Logs`],
    /// which can be accessed by anyone, at any time.
    ///
    /// The [`Record`] added to the [`Logs`] is related to
    /// [`log::Record`], from the [`log`] crate. But it differs in the
    /// sense that it is always `'static`, and instead of having an
    /// [`std::fmt::Arguments`] inside, it contains a [`Text`], making
    /// it a better fit for Duat.
    ///
    /// The connection to [`log::Record`] also means that external
    /// libraries can log information using the [`log`] crate, and it
    /// will also show up in Duat's [`Logs`]s, but reformatted to be a
    /// [`Text`] instead.
    ///
    /// [`txt!`]: crate::text::txt
    /// [`Record`]: super::Record
    /// [`Logs`]: super::Logs
    /// [`Text`]: crate::text::Text
    pub macro warn {
        (target: $target:expr, $($arg:tt)+) => ({
            $crate::private_exports::log!(
                $target.to_string().leak(),
                $crate::context::Level::Warn,
                $($arg)+
            )
        }),
        ($($arg:tt)+) => (
            $crate::private_exports::log!(
                module_path!().to_string().leak(),
                $crate::context::Level::Warn,
                $($arg)+
            )
        )
    }

    /// Logs an info to Duat
    ///
    /// Use this, as opposed to [`error!`], [`warn!`] or [`debug!],
    /// when you want to tell the user that something was
    /// successful, and it is important for them to know it was
    /// successful.
    ///
    /// This error follows the same construction as the [`txt!`]
    /// macro, and will create a [`Record`] inside of the [`Logs`],
    /// which can be accessed by anyone, at any time.
    ///
    /// The [`Record`] added to the [`Logs`] is related to
    /// [`log::Record`], from the [`log`] crate. But it differs in the
    /// sense that it is always `'static`, and instead of having an
    /// [`std::fmt::Arguments`] inside, it contains a [`Text`], making
    /// it a better fit for Duat.
    ///
    /// The connection to [`log::Record`] also means that external
    /// libraries can log information using the [`log`] crate, and it
    /// will also show up in Duat's [`Logs`]s, but reformatted to be a
    /// [`Text`] instead.
    ///
    /// [`txt!`]: crate::text::txt
    /// [`Record`]: super::Record
    /// [`Logs`]: super::Logs
    /// [`Text`]: crate::text::Text
    pub macro info {
        (target: $target:expr, $($arg:tt)+) => ({
            $crate::private_exports::log!(
                $target.to_string(),
                $crate::context::Level::Info,
                $($arg)+
            )
        }),
        ($($arg:tt)+) => (
            $crate::private_exports::log!(
                module_path!().to_string().leak(),
                $crate::context::Level::Info,
                $($arg)+
            )
        )
    }

    /// Logs an debug information to Duat
    ///
    /// Use this, as opposed to [`error!`], [`warn!`] or [`info!`],
    /// when you want to tell the user that something was
    /// successful, but it is not that important, or the success is
    /// only a smaller part of some bigger operation, or the success
    /// is part of something that was done "silently".
    ///
    /// This error follows the same construction as the [`txt!`]
    /// macro, and will create a [`Record`] inside of the [`Logs`],
    /// which can be accessed by anyone, at any time.
    ///
    /// The [`Record`] added to the [`Logs`] is related to
    /// [`log::Record`], from the [`log`] crate. But it differs in the
    /// sense that it is always `'static`, and instead of having an
    /// [`std::fmt::Arguments`] inside, it contains a [`Text`], making
    /// it a better fit for Duat.
    ///
    /// The connection to [`log::Record`] also means that external
    /// libraries can log information using the [`log`] crate, and it
    /// will also show up in Duat's [`Logs`]s, but reformatted to be a
    /// [`Text`] instead.
    ///
    /// [`txt!`]: crate::text::txt
    /// [`Record`]: super::Record
    /// [`Logs`]: super::Logs
    /// [`Text`]: crate::text::Text
    pub macro debug {
        (target: $target:expr, $($arg:tt)+) => ({
            $crate::private_exports::log!(
                $target.to_string().leak(),
                $crate::context::Level::Debug,
                $($arg)+
            )
        }),
        ($($arg:tt)+) => (
            $crate::private_exports::log!(
                module_path!().to_string().leak(),
                $crate::context::Level::Debug,
                $($arg)+
            )
        )
    }
}

static LOGS: OnceLock<Logs> = OnceLock::new();

/// Notifications for duat
///
/// This is a mutable, shareable, [`Send`]/[`Sync`] list of
/// notifications in the form of [`Text`]s, you can read this,
/// send new notifications, and check for updates, just like with
/// [`RwData`]s and [`Handle`]s.
///
/// [`RwData`]: crate::data::RwData
/// [`Handle`]: super::Handle
pub fn logs() -> Logs {
    LOGS.get().unwrap().clone()
}

/// The notifications sent to Duat.
///
/// This can include command results, failed mappings,
/// recompilation messages, and any other thing that you want
/// to notify about. In order to set the level of severity for these
/// messages, use the [`error!`], [`warn!`] and [`info!`] macros.
#[derive(Debug)]
pub struct Logs {
    list: &'static Mutex<Vec<Record>>,
    cutoffs: &'static Mutex<Vec<usize>>,
    cur_state: &'static AtomicUsize,
    read_state: AtomicUsize,
}

impl Clone for Logs {
    fn clone(&self) -> Self {
        Self {
            list: self.list,
            cutoffs: self.cutoffs,
            cur_state: self.cur_state,
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1),
        }
    }
}

impl Logs {
    /// Creates a new [`Logs`]
    #[doc(hidden)]
    pub fn new() -> Self {
        Self {
            list: Box::leak(Box::default()),
            cutoffs: Box::leak(Box::default()),
            cur_state: Box::leak(Box::new(AtomicUsize::new(1))),
            read_state: AtomicUsize::new(0),
        }
    }

    /// Returns an owned valued of a [`SliceIndex`]
    ///
    /// - `&'static Log` for `usize`;
    /// - [`Vec<&'static Log>`] for `impl RangeBounds<usize>`;
    ///
    /// [`SliceIndex`]: std::slice::SliceIndex
    pub fn get<I>(&self, i: I) -> Option<<I::Output as ToOwned>::Owned>
    where
        I: std::slice::SliceIndex<[Record]>,
        I::Output: ToOwned,
    {
        self.read_state
            .store(self.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
        self.list.lock().unwrap().get(i).map(ToOwned::to_owned)
    }

    /// Returns the last [`Record`], if there was one
    pub fn last(&self) -> Option<(usize, Record)> {
        self.read_state
            .store(self.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
        let list = self.list.lock().unwrap();
        list.last().cloned().map(|last| (list.len() - 1, last))
    }

    /// Gets the last [`Record`] with a level from a list
    pub fn last_with_levels(&self, levels: &[Level]) -> Option<(usize, Record)> {
        self.read_state
            .store(self.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
        self.list
            .lock()
            .unwrap()
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, rec)| levels.contains(&rec.level()).then(|| (i, rec.clone())))
    }

    /// Wether there are new notifications or not
    pub fn has_changed(&self) -> bool {
        self.cur_state.load(Ordering::Relaxed) > self.read_state.load(Ordering::Relaxed)
    }

    /// Pushes a [`CmdResult`]
    ///
    /// [`CmdResult`]: crate::cmd::CmdResult
    pub(crate) fn push_cmd_result(&self, cmd: String, result: Result<Option<Text>, Text>) {
        let is_ok = result.is_ok();
        let (Ok(Some(res)) | Err(res)) = result else {
            return;
        };

        self.cur_state.fetch_add(1, Ordering::Relaxed);

        let rec = Record {
            metadata: log::MetadataBuilder::new()
                .level(if is_ok { Level::Info } else { Level::Error })
                .target(cmd.leak())
                .build(),
            module_path: None,
            file: None,
            line: None,
            text: Box::leak(Box::new(res.no_selections())),
        };

        self.list.lock().unwrap().push(rec)
    }

    /// Pushes a new [`Record`] to Duat
    #[doc(hidden)]
    pub fn push_record(&self, rec: Record) {
        self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.list.lock().unwrap().push(rec)
    }

    /// Returns the number of [`Record`]s in the [`Logs`]
    pub fn len(&self) -> usize {
        self.list.lock().unwrap().len()
    }

    /// Wether there are any [`Record`]s in the [`Logs`]
    ///
    /// It's pretty much never `true`
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl log::Log for Logs {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() > log::Level::Debug
    }

    fn log(&self, rec: &log::Record) {
        let rec = Record {
            text: Box::leak(Box::new(
                Text::from(std::fmt::format(*rec.args())).no_selections(),
            )),
            metadata: log::MetadataBuilder::new()
                .level(rec.level())
                .target(rec.target().to_string().leak())
                .build(),
            module_path: match rec.module_path_static() {
                Some(module_path) => Some(module_path),
                None => rec
                    .module_path()
                    .map(|mp| -> &str { mp.to_string().leak() }),
            },
            file: match rec.file_static() {
                Some(file) => Some(file),
                None => rec.file().map(|mp| -> &str { mp.to_string().leak() }),
            },
            line: rec.line(),
        };

        self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.list.lock().unwrap().push(rec)
    }

    fn flush(&self) {}
}

/// A record of something that happened in Duat
///
/// Differs from [`log::Record`] in that its argument isn't an
/// [`std::fmt::Arguments`], but a [`Text`] instead.
#[derive(Clone, Debug)]
pub struct Record {
    text: &'static Selectionless,
    metadata: log::Metadata<'static>,
    module_path: Option<&'static str>,
    file: Option<&'static str>,
    line: Option<u32>,
}

impl Record {
    /// Creates a new [`Record`]
    #[doc(hidden)]
    pub fn new(
        text: Text,
        level: Level,
        target: &'static str,
        module_path: Option<&'static str>,
        file: Option<&'static str>,
        line: Option<u32>,
    ) -> Self {
        Self {
            text: Box::leak(Box::new(text.no_selections())),
            metadata: log::MetadataBuilder::new()
                .level(level)
                .target(target)
                .build(),
            module_path,
            file,
            line,
        }
    }

    /// The [`Text`] of this [`Record`]
    #[inline]
    pub fn text(&self) -> &Text {
        self.text
    }

    /// Metadata about the log directive
    #[inline]
    pub fn metadata(&self) -> log::Metadata<'static> {
        self.metadata.clone()
    }

    /// The verbosity level of the message
    #[inline]
    pub fn level(&self) -> Level {
        self.metadata.level()
    }

    /// The name of the target of the directive
    #[inline]
    pub fn target(&self) -> &'static str {
        self.metadata.target()
    }

    /// The module path of the message
    #[inline]
    pub fn module_path(&self) -> Option<&'static str> {
        self.module_path
    }

    /// The source file containing the message
    #[inline]
    pub fn file(&self) -> Option<&'static str> {
        self.file
    }

    /// The line containing the message
    #[inline]
    pub fn line(&self) -> Option<u32> {
        self.line
    }
}

/// Sets the [`Logs`]. Must use [`Logs`] created in the runner
/// app
#[doc(hidden)]
pub fn set_logs(logs: Logs) {
    LOGS.set(logs).expect("setup ran twice");
}
