//! The primary widget of Duat, used to display buffers.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`Buffer`], such as multiple [`Cursor`]s, a
//! `History` system, [`Area::PrintInfo`], etc.
//!
//! The [`Buffer`] also provides a list of printed lines through the
//! [`Buffer::printed_lines`] method. This method is notably used by
//! the [`LineNumbers`] widget, that shows the numbers of the
//! currently printed lines.
//!
//! [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
//! [`Cursor`]: crate::mode::Cursor
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use parking_lot::{Mutex, MutexGuard};

use self::parser::Parsers;
pub use self::parser::{BufferTracker, Parser};
use crate::{
    context::{self, Cache, Handle},
    data::Pass,
    form::Painter,
    hook::{self, BufferWritten},
    mode::Selections,
    opts::PrintOpts,
    text::{BuilderPart, Bytes, Text, TwoPoints, txt},
    ui::{Area, Widget},
};

mod parser;

/// The widget that is used to print and edit buffers
pub struct Buffer {
    path: PathKind,
    text: Text,
    printed_lines: Mutex<Vec<(usize, bool)>>,
    parsers: Parsers,
    sync_opts: Arc<Mutex<PrintOpts>>,
    pub(crate) layout_order: usize,
    /// The [`PrintOpts`] of this [`Buffer`]
    ///
    /// You can use this member to change the way this `Buffer` will
    /// be printed specifically.
    pub opts: PrintOpts,
    prev_opts: PrintOpts,
}

impl Buffer {
    pub(crate) fn new(path: Option<PathBuf>, opts: PrintOpts) -> Self {
        let (text, path) = match path {
            Some(path) => {
                let canon_path = path.canonicalize();
                if let Ok(path) = &canon_path
                    && let Ok(buffer) = std::fs::read_to_string(path)
                {
                    let selections = {
                        let selection = Cache::new().load(path).unwrap_or_default();
                        Selections::new(selection)
                    };
                    let text = Text::from_bytes(Bytes::new(&buffer), selections, true);
                    (text, PathKind::SetExists(path.clone()))
                } else if canon_path.is_err()
                    && let Ok(mut canon_path) = path.with_file_name(".").canonicalize()
                {
                    canon_path.push(path.file_name().unwrap());
                    (Text::new_with_history(), PathKind::SetAbsent(canon_path))
                } else {
                    (Text::new_with_history(), PathKind::new_unset())
                }
            }
            None => (Text::new_with_history(), PathKind::new_unset()),
        };

        Self {
            path,
            text,
            sync_opts: Arc::new(Mutex::new(opts)),
            printed_lines: Mutex::new(Vec::new()),
            parsers: Parsers::default(),
            layout_order: 0,
            opts,
            prev_opts: opts,
        }
    }

    ////////// Saving the Buffer

    /// Writes the buffer to the current [`PathBuf`], if one was set
    pub fn save(&mut self) -> Result<Option<usize>, Text> {
        self.save_quit(false)
    }

    /// Saves and quits, resulting in no config reload
    pub(crate) fn save_quit(&mut self, quit: bool) -> Result<Option<usize>, Text> {
        if let PathKind::SetExists(path) | PathKind::SetAbsent(path) = &self.path {
            let path = path.clone();
            if self.text.has_unsaved_changes() {
                let bytes = self
                    .text
                    .write_to(std::io::BufWriter::new(fs::File::create(&path)?))
                    .inspect(|_| self.path = PathKind::SetExists(path.clone()))?;

                let path = path.to_string_lossy().to_string();
                hook::queue(BufferWritten((path, bytes, quit)));

                Ok(Some(bytes))
            } else {
                Ok(None)
            }
        } else {
            Err(txt!("No buffer was set").build())
        }
    }

    /// Writes the buffer to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub fn save_to(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<Option<usize>> {
        self.save_quit_to(path, false)
    }

    /// Writes the buffer to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub(crate) fn save_quit_to(
        &self,
        path: impl AsRef<std::path::Path>,
        quit: bool,
    ) -> std::io::Result<Option<usize>> {
        if self.text.has_unsaved_changes() {
            let path = path.as_ref();
            let res = self
                .text
                .write_to(std::io::BufWriter::new(fs::File::create(path)?))
                .map(Some);

            if let Ok(Some(bytes)) = res.as_ref() {
                hook::queue(BufferWritten((
                    path.to_string_lossy().to_string(),
                    *bytes,
                    quit,
                )));
            }

            res
        } else {
            Ok(None)
        }
    }

    ////////// Path querying functions

    /// The full path of the buffer.
    ///
    /// If there is no set path, returns `"*scratch buffer*#{id}"`.
    pub fn path(&self) -> String {
        self.path.path()
    }

    /// The full path of the buffer.
    ///
    /// Returns [`None`] if the path has not been set yet, i.e., if
    /// the buffer is a scratch buffer.
    pub fn path_set(&self) -> Option<String> {
        self.path.path_set()
    }

    /// A [`Text`] from the full path of this [`PathKind`]
    ///
    /// # Formatting
    ///
    /// If the buffer's `path` was set:
    ///
    /// ```text
    /// [buffer]{path}
    /// ```
    ///
    /// If the buffer's `path` was not set:
    ///
    /// ```text
    /// [buffer.new.scratch]*scratch buffer #{id}*
    /// ```
    pub fn path_txt(&self) -> Text {
        self.path_kind().path_txt()
    }

    /// The buffer's name.
    ///
    /// If there is no set path, returns `"*scratch buffer #{id}*"`.
    pub fn name(&self) -> String {
        self.path.name()
    }

    /// The buffer's name.
    ///
    /// Returns [`None`] if the path has not been set yet, i.e., if
    /// the buffer is a scratch buffer.
    pub fn name_set(&self) -> Option<String> {
        self.path.name_set()
    }

    /// A [`Text`] from the name of this [`PathKind`]
    ///
    /// The name of a [`Buffer`] widget is the same as the path, but
    /// it strips away the current directory. If it can't, it will
    /// try to strip away the home directory, replacing it with
    /// `"~"`. If that also fails, it will just show the full
    /// path.
    ///
    /// # Formatting
    ///
    /// If the buffer's `name` was set:
    ///
    /// ```text
    /// [buffer]{name}
    /// ```
    ///
    /// If the buffer's `name` was not set:
    ///
    /// ```text
    /// [buffer.new.scratch]*scratch buffer #{id}*
    /// ```
    pub fn name_txt(&self) -> Text {
        self.path.name_txt()
    }

    /// The type of [`PathBuf`]
    ///
    /// This represents the three possible states for a [`Buffer`]'s
    /// [`PathBuf`], as it could either represent a real [`Buffer`],
    /// not exist, or not have been defined yet.
    pub fn path_kind(&self) -> PathKind {
        self.path.clone()
    }

    /// Returns the currently printed set of lines.
    ///
    /// These are returned as a `usize`, showing the index of the line
    /// in the buffer, and a `bool`, which is `true` when the line is
    /// wrapped.
    pub fn printed_lines(&self) -> MutexGuard<'_, Vec<(usize, bool)>> {
        self.printed_lines.lock()
    }

    ////////// General querying functions

    /// The [`Bytes`] of the [`Buffer`]'s [`Text`]
    pub fn bytes(&self) -> &Bytes {
        self.text.bytes()
    }

    /// The number of bytes in the buffer.
    pub fn len_bytes(&self) -> usize {
        self.text.len().byte()
    }

    /// The number of [`char`]s in the buffer.
    pub fn len_chars(&self) -> usize {
        self.text.len().char()
    }

    /// The number of lines in the buffer.
    pub fn len_lines(&self) -> usize {
        self.text.len().line()
    }

    /// The [`Selections`] that are used on the [`Text`], if they
    /// exist
    pub fn selections(&self) -> &Selections {
        self.text.selections()
    }

    /// A mutable reference to the [`Selections`], if they exist
    pub fn selections_mut(&mut self) -> &mut Selections {
        self.text.selections_mut()
    }

    /// Whether o not the [`Buffer`] exists or not
    pub fn exists(&self) -> bool {
        self.path_set()
            .is_some_and(|p| std::fs::exists(PathBuf::from(&p)).is_ok_and(|e| e))
    }

    ////////// Parser functions

    /// Adds a [`Parser`] to this `Buffer`
    ///
    /// The [`Parser`] will be able to keep track of every single
    /// [`Change`] that takes place in the `Buffer`'s [`Text`], and
    /// can act on the `Buffer` accordingly.
    ///
    /// This function will fail if a [`Parser`] of the same type was
    /// already added to this [`Buffer`]
    pub fn add_parser<P: Parser>(
        &mut self,
        f: impl FnOnce(BufferTracker) -> P,
    ) -> Result<(), Text> {
        self.parsers.add(self, f)
    }

    /// Reads from a specific [`Parser`], if it was [added]
    ///
    /// This function will block until the [`Parser`] is ready to be
    /// read. This usually implies that the [`Parser`] is done
    /// processing all the [`Change`]s up to this point. But it could
    /// also be the case that the [`Parser`] doesn't really care about
    /// [`Change`]s.
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// If you want to read in a non blocking way, see
    /// [`Buffer::try_read_parser`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    pub fn read_parser<P: Parser, Ret>(&self, read: impl FnOnce(&P) -> Ret) -> Option<Ret> {
        self.parsers.read_parser(read)
    }

    /// Tries tor read from a specific [`Parser`], if it was [added]
    ///
    /// Unlike [`Buffer::read_parser`], this function will only be
    /// called when the [`Parser`] is ready to be read. This may not
    /// be the case if, for example, it is still processing
    /// [`Change`]s to the [`Text`]. In that case, the function will
    /// not be called and [`try_read_parser`] will return [`None`].
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    /// [`try_read_parser`]: Self::try_read_parser
    pub fn try_read_parser<P: Parser, Ret>(&self, read: impl FnOnce(&P) -> Ret) -> Option<Ret> {
        self.parsers.try_read_parser(read)
    }

    /// Writes to a specific [`Parser`], if it was [added]
    ///
    /// This function will block until the [`Parser`] is ready to be
    /// written to. This usually implies that the [`Parser`] is done
    /// processing all the [`Change`]s up to this point. But it could
    /// also be the case that the [`Parser`] doesn't really care
    /// about [`Change`]s.
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// If you want to write in a non blocking way, see
    /// [`Buffer::try_write_parser`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    pub fn write_parser<P: Parser, Ret>(&self, write: impl FnOnce(&mut P) -> Ret) -> Option<Ret> {
        self.parsers.write_parser(write)
    }

    /// Tries tor read a specific [`Parser`], if it was [added]
    ///
    /// Unlike [`Buffer::write_parser`], this function will only be
    /// called when the [`Parser`] is ready to be written to. This may
    /// not be the case if, for example, it is still processing
    /// [`Change`]s to the [`Text`]. In that case, the function will
    /// not be called and [`try_write_parser`] will return [`None`].
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    /// [`try_write_parser`]: Self::try_write_parser
    pub fn try_write_parser<P: Parser, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        self.parsers.try_write_parser(write)
    }

    /// Prepare this `Buffer` for reloading
    ///
    /// This works by creating a new [`Buffer`], which will take
    /// ownership of a stripped down version of this one's [`Text`]
    pub(crate) fn prepare_for_reloading(&mut self) -> Self {
        self.text.prepare_for_reloading();
        Self {
            path: self.path.clone(),
            text: std::mem::take(&mut self.text),
            printed_lines: Mutex::new(Vec::new()),
            parsers: Parsers::default(),
            sync_opts: Arc::default(),
            layout_order: self.layout_order,
            opts: PrintOpts::default(),
            prev_opts: PrintOpts::default(),
        }
    }
}

impl Widget for Buffer {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let parsers = std::mem::take(&mut handle.write(pa).parsers);

        let opts = handle.read(pa).opts;

        let (buffer, area) = handle.write_with_area(pa);
        if buffer.prev_opts != opts {
            *buffer.sync_opts.lock() = opts;
            buffer.prev_opts = opts;
        }

        if let Some(main) = buffer.text().selections().get_main() {
            area.scroll_around_points(
                buffer.text(),
                main.caret().to_points(),
                buffer.get_print_opts(),
            );
        }

        let (start, _) = area.start_points(&buffer.text, opts);
        let (end, _) = area.end_points(&buffer.text, opts);

        parsers.update(pa, handle, start.byte()..end.byte());

        let buffer = handle.write(pa);
        buffer.parsers = parsers;

        buffer.text.update_bounds();
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.parsers.needs_update()
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn get_print_opts(&self) -> PrintOpts {
        self.opts
    }

    fn print(&self, pa: &Pass, painter: Painter, area: &Area) {
        let opts = self.opts;
        let (start, _) = area.start_points(pa, &self.text, opts);

        let mut last_line = area
            .rev_print_iter(pa, self.text.iter_rev(start), opts)
            .find_map(|(caret, item)| caret.wrap.then_some(item.line()));

        let mut printed_lines = self.printed_lines.lock();
        printed_lines.clear();

        let mut has_wrapped = false;

        area.print_with(pa, &self.text, opts, painter, move |caret, item| {
            has_wrapped |= caret.wrap;
            if has_wrapped && item.part.is_char() {
                has_wrapped = false;
                let line = item.line();
                let wrapped = last_line.is_some_and(|ll| ll == line);
                last_line = Some(line);
                printed_lines.push((line, wrapped));
            }
        })
    }
}

impl Handle {
    /// Adds a [`Parser`] to react to [`Text`] [`Change`]s
    ///
    /// [`Change`]: crate::text::Change
    pub fn add_parser<P: Parser>(
        &self,
        pa: &mut Pass,
        f: impl FnOnce(BufferTracker) -> P,
    ) -> Result<(), Text> {
        let buffer = self.widget().read(pa);
        buffer.parsers.add(buffer, f)
    }
}

/// Represents the presence or absence of a path
#[derive(Debug, Clone)]
pub enum PathKind {
    /// A [`PathBuf`] that has been defined and points to a real
    /// buffer
    SetExists(PathBuf),
    /// A [`PathBuf`] that has been defined but isn't a real buffer
    SetAbsent(PathBuf),
    /// A [`PathBuf`] that has not been defined
    ///
    /// The number within represents a specific [`Buffer`], and when
    /// printed to, for example, the [`StatusLine`], would show up as
    /// `txt!("[buffer]*scratch buffer*#{id}")`
    ///
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    NotSet(usize),
}

impl PathKind {
    /// Returns a new unset [`PathBuf`]
    pub(crate) fn new_unset() -> PathKind {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        PathKind::NotSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    /// Returns a [`PathBuf`] if `self` is [`SetExists`] or
    /// [`SetAbsent`]
    pub fn as_path(&self) -> Option<PathBuf> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => Some(path.clone()),
            PathKind::NotSet(_) => None,
        }
    }

    /// The full path of the buffer.
    ///
    /// If there is no set path, returns `"*scratch buffer*#{id}"`.
    pub fn path(&self) -> String {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                path.to_string_lossy().to_string()
            }
            PathKind::NotSet(id) => {
                format!("*scratch buffer*#{id}")
            }
        }
    }

    /// The full path of the buffer.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn path_set(&self) -> Option<String> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                Some(path.to_string_lossy().to_string())
            }
            PathKind::NotSet(_) => None,
        }
    }

    /// The buffer's name.
    ///
    /// If there is no set path, returns `"*scratch buffer #{id}*"`.
    pub fn name(&self) -> String {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::current_dir();
                if let Ok(path) = path.strip_prefix(cur_dir) {
                    path.to_string_lossy().to_string()
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    Path::new("~").join(path).to_string_lossy().to_string()
                } else {
                    path.to_string_lossy().to_string()
                }
            }
            PathKind::NotSet(id) => format!("*scratch buffer #{id}*"),
        }
    }

    /// The buffer's name.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn name_set(&self) -> Option<String> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::current_dir();
                Some(if let Ok(path) = path.strip_prefix(cur_dir) {
                    path.to_string_lossy().to_string()
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    Path::new("~").join(path).to_string_lossy().to_string()
                } else {
                    path.to_string_lossy().to_string()
                })
            }
            PathKind::NotSet(_) => None,
        }
    }

    /// A [`Text`] from the full path of this [`PathKind`]
    ///
    /// # Formatting
    ///
    /// If the buffer's `path` was set:
    ///
    /// ```text
    /// [buffer]{path}
    /// ```
    ///
    /// If the buffer's `path` was not set:
    ///
    /// ```text
    /// [buffer.new.scratch]*scratch buffer #{id}*
    /// ```
    pub fn path_txt(&self) -> Text {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => txt!("[buffer]{path}").build(),
            PathKind::NotSet(id) => txt!("[buffer.new.scratch]*scratch buffer #{id}*").build(),
        }
    }

    /// A [`Text`] from the name of this [`PathKind`]
    ///
    /// The name of a [`Buffer`] widget is the same as the path, but
    /// it strips away the current directory. If it can't, it will
    /// try to strip away the home directory, replacing it with
    /// `"~"`. If that also fails, it will just show the full
    /// path.
    ///
    /// # Formatting
    ///
    /// If the buffer's `name` was set:
    ///
    /// ```text
    /// [buffer]{name}
    /// ```
    ///
    /// If the buffer's `name` was not set:
    ///
    /// ```text
    /// [buffer.new.scratch]*scratch buffer #{id}*
    /// ```
    pub fn name_txt(&self) -> Text {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::current_dir();
                if let Ok(path) = path.strip_prefix(cur_dir) {
                    txt!("[buffer]{path}").build()
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    txt!("[buffer]{}", Path::new("~").join(path)).build()
                } else {
                    txt!("[buffer]{path}").build()
                }
            }
            PathKind::NotSet(id) => txt!("[buffer.new.scratch]*scratch buffer #{id}*").build(),
        }
    }
}

impl<P: AsRef<Path>> From<P> for PathKind {
    fn from(value: P) -> Self {
        let path = value.as_ref();
        if let Ok(true) = path.try_exists() {
            PathKind::SetExists(path.into())
        } else {
            PathKind::SetAbsent(path.into())
        }
    }
}

impl PartialEq for PathKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::SetExists(l0) | Self::SetAbsent(l0),
                Self::SetExists(r0) | Self::SetAbsent(r0),
            ) => l0 == r0,
            (Self::NotSet(l0), Self::NotSet(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for PathKind {}

impl From<PathKind> for BuilderPart {
    fn from(value: PathKind) -> Self {
        BuilderPart::Text(value.name_txt())
    }
}
