//! The primary [`Widget`] of Duat, used to display buffers.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`Buffer`], such as multiple [`Cursor`]s, a
//! `History` system, [`RawArea::PrintInfo`], etc.
//!
//! The [`Buffer`] also provides a list of printed lines through the
//! [`Handle::printed_lines`] method. This method is notably used by
//! the [`LineNumbers`] widget, that shows the numbers of the
//! currently printed lines.
//!
//! [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
//! [`Cursor`]: crate::mode::Cursor
//! [`RawArea::PrintInfo`]: crate::ui::traits::RawArea::PrintInfo
use std::{
    collections::HashMap,
    fs,
    ops::Range,
    path::{Path, PathBuf},
    sync::{LazyLock, Mutex, MutexGuard},
};

use crossterm::event::{MouseButton, MouseEventKind};

pub use crate::buffer::{
    buffer_id::BufferId,
    history::{
        BufferParts, BufferTracker, Change, Changes, FetchedChanges, History, Moment,
        RangesToUpdate,
    },
    opts::BufferOpts,
};
use crate::{
    context::{self, Handle, cache},
    data::{Pass, RwData, WriteableTuple},
    hook::{self, BufferSaved, BufferUpdated},
    mode::{Cursor, MouseEvent, Selections},
    opts::PrintOpts,
    session::TwoPointsPlace,
    text::{Bytes, Point, Strs, Text, TextMut, TextVersion, txt},
    ui::{Area, Coord, PrintInfo, PrintedLine, Widget},
};

mod history;
mod opts;

/// The widget that is used to print and edit buffers
pub struct Buffer {
    id: BufferId,
    path: PathKind,
    text: Text,
    pub(crate) layout_order: usize,
    history: History,
    cached_print_info: Mutex<Option<CachedPrintInfo>>,
    /// The [`PrintOpts`] of this [`Buffer`]
    ///
    /// You can use this member to change the way this `Buffer` will
    /// be printed specifically.
    pub opts: BufferOpts,
    prev_opts: Mutex<PrintOpts>,
}

impl Buffer {
    /// Returns a new [`Buffer`], private for now
    pub(crate) fn new(path: Option<PathBuf>, opts: BufferOpts) -> Self {
        let (text, path) = match path {
            Some(path) => {
                let canon_path = path.canonicalize();
                if let Ok(path) = &canon_path
                    && let Ok(buffer) = std::fs::read_to_string(path)
                {
                    let selections = {
                        let selection = cache::load(path).unwrap_or_default();
                        Selections::new(selection)
                    };
                    let text = Text::from_parts(Bytes::new(&buffer), selections);
                    (text, PathKind::SetExists(path.clone()))
                } else if canon_path.is_err()
                    && let Ok(mut canon_path) = path.with_file_name(".").canonicalize()
                {
                    canon_path.push(path.file_name().unwrap());
                    (
                        Text::with_default_main_selection(),
                        PathKind::SetAbsent(canon_path),
                    )
                } else {
                    (Text::with_default_main_selection(), PathKind::new_unset())
                }
            }
            None => (Text::with_default_main_selection(), PathKind::new_unset()),
        };

        Self {
            id: BufferId::new(),
            path,
            text,
            layout_order: 0,
            history: History::new(),
            cached_print_info: Mutex::new(None),
            opts,
            prev_opts: Mutex::new(opts.to_print_opts()),
        }
    }

    ////////// Saving the Buffer

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
    /// This represents the three possible states for a `Buffer`'s
    /// `PathBuf`, as it could either represent a real `Buffer`,
    /// not exist, or not have been defined yet.
    pub fn path_kind(&self) -> PathKind {
        self.path.clone()
    }

    ////////// Auxiliatory methods for incremental parsing

    /// Resets the print info if deemed necessary, returning the final
    /// result, as well as `true` if things have changed
    ///
    /// After calling this, `self.cached_print_info` is guaranteed to
    /// be [`Some`]
    fn reset_print_info_if_needed<'b>(
        &'b self,
        area: &Area,
    ) -> MutexGuard<'b, Option<CachedPrintInfo>> {
        let opts_changed = {
            let mut prev_opts = self.prev_opts.lock().unwrap();
            let cur_opts = self.opts.to_print_opts();
            let opts_changed = *prev_opts != cur_opts;
            *prev_opts = cur_opts;
            opts_changed
        };

        let mut cached_print_info = self.cached_print_info.lock().unwrap();
        if opts_changed
            || cached_print_info.as_ref().is_none_or(|cpi| {
                self.text
                    .version()
                    .has_structurally_changed_since(cpi.text_state)
                    || area.get_print_info() != cpi.area_print_info
                    || area.top_left() != cpi.coords.0
                    || area.bottom_right() != cpi.coords.1
            })
        {
            let opts = self.opts.to_print_opts();
            let start = area.start_points(&self.text, opts).real;
            let end = area.end_points(&self.text, opts).real;
            let printed_line_numbers = area.get_printed_lines(&self.text, opts).unwrap();

            *cached_print_info = Some(CachedPrintInfo {
                range: start..end,
                printed_line_numbers,
                printed_line_ranges: None,
                _visible_line_ranges: None,
                text_state: self.text.version(),
                area_print_info: area.get_print_info(),
                coords: (area.top_left(), area.bottom_right()),
            });
        } else {
            cached_print_info.as_mut().unwrap().text_state = self.text.version();
        };

        cached_print_info
    }

    ////////// General querying functions

    /// A unique identifier for this [`Buffer`]
    ///
    /// This is more robust than identifying it by its path or name,
    /// or event [`PathKind`], since those could change, but this
    /// cannot.
    pub fn buffer_id(&self) -> BufferId {
        self.id
    }

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

    /// The [`Selections`] that are used on the [`Text`]
    pub fn selections(&self) -> &Selections {
        self.text.selections()
    }

    /// A mutable reference to the [`Selections`]
    pub fn selections_mut(&mut self) -> &mut Selections {
        self.text.selections_mut()
    }

    /// Whether o not the [`Buffer`] exists or not
    pub fn exists(&self) -> bool {
        self.path_set()
            .is_some_and(|p| std::fs::exists(PathBuf::from(&p)).is_ok_and(|e| e))
    }

    /// Prepare this `Buffer` for reloading
    ///
    /// This works by creating a new [`Buffer`], which will take
    /// ownership of a stripped down version of this one's [`Text`]
    pub(crate) fn prepare_for_reloading(&mut self) -> Self {
        self.text.prepare_for_reloading();
        Self {
            id: self.id,
            path: self.path.clone(),
            text: std::mem::take(&mut self.text),
            layout_order: self.layout_order,
            history: std::mem::replace(&mut self.history, History::new()),
            cached_print_info: Mutex::new(
                self.cached_print_info
                    .lock()
                    .unwrap_or_else(|err| err.into_inner())
                    .take(),
            ),
            opts: BufferOpts::default(),
            prev_opts: Mutex::default(),
        }
    }
}

impl Widget for Buffer {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        // Asynchronous updating of opts
        let (buffer, area) = handle.write_with_area(pa);

        if let Some(main) = buffer.text().get_main_sel() {
            area.scroll_around_points(
                buffer.text(),
                main.caret().to_two_points_after(),
                buffer.print_opts(),
            );
        }

        drop(buffer.reset_print_info_if_needed(area));

        hook::trigger(pa, BufferUpdated(handle.clone()));

        handle.text_mut(pa).update_bounds();
    }

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        let mut text_mut = self.text.as_mut();
        text_mut.attach_history(&mut self.history);
        text_mut
    }

    fn print_opts(&self) -> PrintOpts {
        self.opts.to_print_opts()
    }

    fn on_mouse_event(pa: &mut Pass, handle: &Handle<Self>, event: MouseEvent) {
        match event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                let point = match event.points {
                    Some(TwoPointsPlace::Within(points) | TwoPointsPlace::AheadOf(points)) => {
                        points.real
                    }
                    _ => handle.text(pa).last_point(),
                };

                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut c| {
                    c.unset_anchor();
                    c.move_to(point)
                })
            }
            MouseEventKind::Down(_) => {}
            MouseEventKind::Up(_) => {}
            MouseEventKind::Drag(MouseButton::Left) => {
                let point = match event.points {
                    Some(TwoPointsPlace::Within(points) | TwoPointsPlace::AheadOf(points)) => {
                        points.real
                    }
                    _ => handle.text(pa).last_point(),
                };

                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut c| {
                    c.set_anchor_if_needed();
                    c.move_to(point);
                })
            }
            MouseEventKind::Drag(_) => {}
            MouseEventKind::Moved => {}
            MouseEventKind::ScrollDown => {
                let opts = handle.opts(pa);
                let (widget, area) = handle.write_with_area(pa);
                area.scroll_ver(widget.text(), 3, opts);
            }
            MouseEventKind::ScrollUp => {
                let opts = handle.opts(pa);
                let (widget, area) = handle.write_with_area(pa);
                area.scroll_ver(widget.text(), -3, opts);
            }
            MouseEventKind::ScrollLeft => {}
            MouseEventKind::ScrollRight => {}
        }
    }
}

impl Handle {
    /// Writes the buffer to the current [`PathBuf`], if one was set
    pub fn save(&self, pa: &mut Pass) -> Result<bool, Text> {
        self.save_quit(pa, false)
    }

    /// Saves and quits, resulting in no config reload
    ///
    /// Returns `Ok(true)` if it saved, `Ok(false)` if that wasn't
    /// necessary, and `Err` if there was some problem.
    pub(crate) fn save_quit(&self, pa: &mut Pass, quit: bool) -> Result<bool, Text> {
        let buf = self.write(pa);

        if let PathKind::SetExists(path) | PathKind::SetAbsent(path) = &buf.path {
            let path = path.clone();
            if buf.text.has_unsaved_changes() {
                buf.text
                    .save_on(std::io::BufWriter::new(fs::File::create(&path)?))
                    .inspect(|_| buf.path = PathKind::SetExists(path.clone()))?;

                hook::trigger(pa, BufferSaved((self.clone(), quit)));

                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Err(txt!("No buffer was set"))
        }
    }

    /// Writes the buffer to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub fn save_to(
        &self,
        pa: &mut Pass,
        path: impl AsRef<std::path::Path>,
    ) -> std::io::Result<bool> {
        self.save_quit_to(pa, path, false)
    }

    /// Writes the buffer to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub(crate) fn save_quit_to(
        &self,
        pa: &mut Pass,
        path: impl AsRef<std::path::Path>,
        quit: bool,
    ) -> std::io::Result<bool> {
        let buf = self.write(pa);

        if buf.text.has_unsaved_changes() {
            let path = path.as_ref();
            let res = buf
                .text
                .save_on(std::io::BufWriter::new(fs::File::create(path)?));
            buf.history.declare_saved();

            if res.as_ref().is_ok() {
                hook::trigger(pa, BufferSaved((self.clone(), quit)));
            }

            res.and(Ok(true))
        } else {
            Ok(false)
        }
    }

    /// Returns the list of printed line numbers
    ///
    /// These are returned as a `usize`, showing the index of the line
    /// in the buffer, and a `bool`, which is `true` when the line is
    /// wrapped.
    ///
    /// If you want the actual content of these lines (as [`Strs`]s),
    /// check out [`Handle::printed_lines`]. If you want the content
    /// of only the _visible_ portion of these lines, check out
    /// [`Handle::visible_lines`].
    #[track_caller]
    pub fn printed_line_numbers(&self, pa: &Pass) -> Vec<PrintedLine> {
        let buffer = self.read(pa);
        let cpi = buffer.reset_print_info_if_needed(self.area().read(pa));
        cpi.as_ref().unwrap().printed_line_numbers.clone()
    }

    /// The printed [`Range<Point>`], from the top of the screen to
    /// the bottom
    ///
    /// Do note that this includes all concealed lines and parts that
    /// are out of screen. If you want only to include partially
    /// visible lines, while excluding fully hidden ones, check out
    /// [`Handle::printed_lines`]. If you want to exclude every
    /// concealed or out of screen section, check out
    /// [`Handle::visible_lines`].
    pub fn full_printed_range(&self, pa: &Pass) -> Range<Point> {
        let buffer = self.read(pa);
        let cpi = buffer.reset_print_info_if_needed(self.area().read(pa));
        cpi.as_ref().unwrap().range.clone()
    }

    /// Returns the list of printed lines
    ///
    /// These are returned as [`Strs`], which is a known subsection of
    /// a [`Bytes`] struct, from the [`Text`].
    ///
    /// Note that this function returns all portions of printed lines,
    /// not just those that are visible. This means that it will also
    /// include partially [concealed] lines and parts of the line that
    /// are out of screen.
    ///
    /// If you want a list of _only_ the visible sections, check out
    /// [`Handle::visible_lines`].
    ///
    /// If you want a [`Range<Point>`] of the printed section of the
    /// [`Text`] (including concealed lines), check out
    /// [`Handle::full_printed_range`].
    ///
    /// If you just want the line numbers of the printed lines, check
    /// out [`Handle::printed_line_numbers`].
    ///
    /// [concealed]: crate::text::Conceal
    pub fn printed_lines<'b>(&'b self, pa: &'b Pass) -> Vec<&'b Strs> {
        let buffer = self.read(pa);
        let mut cpi = buffer.reset_print_info_if_needed(self.area().read(pa));
        let cpi = cpi.as_mut().unwrap();
        let lines = &cpi.printed_line_numbers;

        let printed_lines = if let Some(printed_lines) = &cpi.printed_line_ranges {
            printed_lines
        } else {
            let mut last = None;
            cpi.printed_line_ranges.insert(
                lines
                    .iter()
                    .filter(|line| {
                        last.as_mut()
                            .is_none_or(|num| std::mem::replace(num, line.number) < line.number)
                    })
                    .map(|line| buffer.text.line(line.number).range())
                    .collect(),
            )
        };

        printed_lines
            .iter()
            .map(|range| &buffer.text[range.clone()])
            .collect()
    }

    /// A list of [`Range<usize>`]s for the byte ranges of each
    /// printed line
    ///
    /// This is just a shorthand for calling [`Handle::printed_lines`]
    /// and mapping each one via [`Strs::byte_range`].
    pub fn printed_line_ranges(&self, pa: &Pass) -> Vec<Range<usize>> {
        let lines = self.printed_lines(pa);
        lines.into_iter().map(|line| line.byte_range()).collect()
    }

    /// Only the visible parts of printed lines
    ///
    /// This is just like [`Handle::printed_lines`], but excludes
    /// _every_ section that was concealed or is not visible on
    /// screen.
    pub fn visible_lines<'b>(&'b self, _: &'b Pass) -> Vec<&'b Strs> {
        todo!();
    }
}

/// Represents the presence or absence of a path
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    /// [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
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
    ///
    /// [`SetExists`]: PathKind::SetExists
    /// [`SetAbsent`]: PathKind::SetAbsent
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
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => txt!("[buffer]{path}"),
            PathKind::NotSet(id) => txt!("[buffer.new.scratch]*scratch buffer #{id}*"),
        }
    }

    /// A [`Text`] from the name of this `PathKind`
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
                    txt!("[buffer]{path}")
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    txt!("[buffer]{}", Path::new("~").join(path))
                } else {
                    txt!("[buffer]{path}")
                }
            }
            PathKind::NotSet(id) => txt!("[buffer.new.scratch]*scratch buffer #{id}*"),
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

/// Cached information about the printing of this [`Buffer`]
struct CachedPrintInfo {
    range: Range<Point>,
    printed_line_numbers: Vec<PrintedLine>,
    printed_line_ranges: Option<Vec<Range<Point>>>,
    _visible_line_ranges: Option<Vec<Range<Point>>>,
    text_state: TextVersion,
    area_print_info: PrintInfo,
    coords: (Coord, Coord),
}

mod buffer_id {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNT: AtomicUsize = AtomicUsize::new(0);

    /// A unique identifier for a [`Buffer`]
    ///
    /// [`Buffer`]: super::Buffer
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct BufferId(usize);

    impl BufferId {
        /// Returns a new `BufferId`, uniquely identifying a
        /// [`Buffer`]
        ///
        /// [`Buffer`]: super::Buffer
        pub(super) fn new() -> Self {
            Self(COUNT.fetch_add(1, Ordering::Relaxed))
        }

        /// Sets the minimum `BufferId`, in order to prevent conflicts
        pub(crate) fn set_min(buffer_ids: impl Iterator<Item = BufferId>) {
            COUNT.store(
                buffer_ids.map(|buf_id| buf_id.0).max().unwrap_or(0) + 1,
                Ordering::Relaxed,
            );
        }
    }
}

/// A struct to associate one `T` to each [`Buffer`]
///
/// This is very useful to implement the "parser pattern", where you
/// have one parser per `Buffer`, acting on changes that take place on
/// each `Buffer`.
pub struct PerBuffer<T: 'static>(LazyLock<RwData<HashMap<BufferId, T>>>);

impl<T: 'static> PerBuffer<T> {
    /// Returns a new mapping of [`Buffer`]s to a type `T`
    ///
    /// Since this function is `const`, you can conveniently place it
    /// in a `static` variable, in order to record one `T` for every
    /// `Buffer` that you want to associate the struct to.
    ///
    /// This is very useful in order to create the "parser pattern" on
    /// a number of different `Buffer`s, letting you store things and
    /// retrieve them as needed.
    ///
    /// # Note
    ///
    /// This function will _not_ automatically add new [`Buffer`]s to
    /// the list. To do that, you should add a [hook] on [`Buffer`],
    /// that calls [`PerBuffer::register`].
    ///
    /// Additionally, you will probably also want to setup a hook
    /// that calls [`PerBuffer::unregister`] on [`BufferClosed`]
    ///
    /// [`BufferClosed`]: crate::hook::BufferClosed
    pub const fn new() -> Self {
        Self(LazyLock::new(RwData::default))
    }

    /// Register a [`Buffer`] with an initial value of `T`
    ///
    /// If there was a previous version of `T` assoiated with the
    /// `Buffer`, then the new `T` will replace that old version.
    ///
    /// You should most likely call this function on the
    /// [`WidgetOpened<Buffer>`] hook, often aliased to just
    /// `Buffer`.
    ///
    /// [`WidgetOpened<Buffer>`]: crate::hook::WidgetOpened
    pub fn register<'p>(
        &'p self,
        pa: &'p mut Pass,
        handle: &'p Handle,
        new_value: T,
    ) -> (&'p mut T, &'p mut Buffer) {
        let (list, buf) = pa.write_many((&*self.0, handle));

        let entry = list.entry(buf.buffer_id()).insert_entry(new_value);

        (entry.into_mut(), buf)
    }

    /// Unregisters a [`Buffer`]
    ///
    /// This will remove the `Buffer` from the list, making future
    /// calls to [`Self::write`] return [`None`]. You should consider
    /// doing this on the [`BufferClosed`] hook.
    /// Returns [`None`] if the `Buffer` wasn't already [registered].
    ///
    /// [`BufferClosed`]: crate::hook::BufferClosed
    /// [registered]: Self::register
    pub fn unregister(&self, pa: &mut Pass, handle: &Handle) -> Option<T> {
        let buf_id = handle.read(pa).buffer_id();
        self.0.write(pa).remove(&buf_id)
    }

    /// Gets a reference to the `T` associated with a [`Buffer`]
    ///
    /// This function lets you bipass the normal requirement of a
    /// [`Pass`] in order to acquire a `T` associated with any given
    /// `Buffer`.
    ///
    /// For now, the two types that can be used as [`BufferPass`]es
    /// are the [`Buffer`] itself and a [`Cursor<Buffer>`]. These
    /// types are allowed to do this because they are impossible
    /// to acquire without first borrowing from an
    /// [`RwData<Buffer>`], either directly or through a [`Handle`]
    ///
    /// Will return [`None`] if the `Buffer` in question wasn't
    /// [registered] or was [unregistered].
    ///
    /// # Note: Why is this safe?
    ///
    /// From the rest of the operations on this struct, you may glean
    /// that the [`PerBuffer`] struct is backed by a [`RwData`], and
    /// the only way to safely access the data in those is through a
    /// [`Pass`].
    ///
    /// So why can you suddenly do this without a `Pass`. Basically,
    /// since you can't construct a `Buffer`, the only way to actually
    /// get one is by borrowing from a [`RwData<Buffer>`] or
    /// [`Handle`].
    ///
    /// Given that, the [`Pass`] will already be borrowed, and the
    /// `Buffer` will act as an "extensionto the [`Pass`]'s borrow",
    /// and will become invalid at the same time.
    ///
    /// It is important to note that this is only safe because
    /// `Buffer`s can't be acquired without a [`Pass`].
    ///
    /// [registered]: Self::register
    /// [unregistered]: Self::unregister
    pub fn get<'b>(&'b self, buffer_pass: &'b impl BufferPass) -> Option<&'b T> {
        static PASS: Pass = unsafe { Pass::new() };
        let list = self.0.read(&PASS);
        list.get(&buffer_pass.buffer_id())
    }

    /// Gets a mutable reference to the `T` associated with a
    /// [`Buffer`]
    ///
    /// This function lets you bipass the normal requirement of a
    /// [`Pass`] in order to acquire a `T` associated with any given
    /// `Buffer`.
    ///
    /// For now, the two types that can be used as [`BufferPass`]es
    /// are the [`Buffer`] itself and a [`Cursor<Buffer>`]. These
    /// types are allowed to do this because they are impossible
    /// to acquire without first borrowing from an [`RwData<Buffer>`],
    /// either directly or through a [`Handle`]
    ///
    /// Will return [`None`] if the `Buffer` in question wasn't
    /// [registered] or was [unregistered].
    ///
    /// # Note: Why is this safe?
    ///
    /// For the same reason that [`PerBuffer::get`] is safe. However,
    /// in order to prevent multiple borrowings from happening at the
    /// same time, this will take a mutable borrow of the `Buffer`,
    /// acting much like a `&mut Pass` in that regard.
    ///
    /// [registered]: Self::register
    /// [unregistered]: Self::unregister
    pub fn get_mut<'b>(&'b self, buffer: &'b mut impl BufferPass) -> Option<&'b mut T> {
        static PASS: Pass = unsafe { Pass::new() };
        let list = self
            .0
            .write(unsafe { (&raw const PASS as *mut Pass).as_mut() }.unwrap());
        list.get_mut(&buffer.buffer_id())
    }

    /// Writes to the [`Buffer`] and the `T` at the same time
    ///
    /// Will return [`None`] if the `Buffer` in question wasn't
    /// [registered] or was [unregistered].
    ///
    /// [registered]: Self::register
    /// [unregistered]: Self::unregister
    pub fn write<'p>(
        &'p self,
        pa: &'p mut Pass,
        handle: &'p Handle,
    ) -> Option<(&'p mut T, &'p mut Buffer)> {
        let (list, buffer) = pa.write_many((&*self.0, handle));
        Some((list.get_mut(&buffer.buffer_id())?, buffer))
    }

    /// Writes to the [`Buffer`] and a tuple of [writeable] types
    ///
    /// This is an extension to the [`Pass::write_many`] method,
    /// allowing you to write to many [`RwData`]-like structs at once.
    ///
    /// Returns [`None`] if any two structs, either in the [`Handle`],
    /// [tuple], or [`PerBuffer`] point to the same thing, or if the
    /// `Buffer` wasn't [registered] or was [unregistered].
    ///
    /// [writeable]: crate::data::WriteableData
    /// [registered]: Self::register
    /// [unregistered]: Self::unregister
    pub fn write_with<'p, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p self,
        pa: &'p mut Pass,
        handle: &'p Handle,
        tup: Tup,
    ) -> Option<(&'p mut T, &'p mut Buffer, Tup::Return)> {
        let (list, buffer, ret) = pa.try_write_many((&*self.0, handle, tup)).ok()?;
        Some((list.get_mut(&buffer.buffer_id())?, buffer, ret))
    }

    /// Tries to write to a bunch of [`Buffer`]s and their respective
    /// `T`s
    ///
    /// Returns [`None`] if any two [`Handle`]s point to the same
    /// `Buffer`, or if any of the `Buffers` weren't [registered] or
    /// were [unregistered].
    ///
    /// [registered]: Self::register
    /// [unregistered]: Self::unregister
    pub fn write_many<'p, const N: usize>(
        &'p self,
        pa: &'p mut Pass,
        handles: [&'p Handle; N],
    ) -> Option<[(&'p mut T, &'p mut Buffer); N]> {
        let (list, buffers) = pa.try_write_many((&*self.0, handles)).ok()?;
        let buf_ids = buffers.each_ref().map(|buf| buf.buffer_id());
        let values = list.get_disjoint_mut(buf_ids.each_ref());

        let list = values
            .into_iter()
            .zip(buffers)
            .map(|(value, buf)| value.zip(Some(buf)))
            .collect::<Option<Vec<_>>>()?;

        list.try_into().ok()
    }

    /// Fusion of [`write_many`] and [`write_with`]
    ///
    /// Returns [`None`] if any two structs point to the same
    /// [`RwData`]-like struct, or if any of the `Buffers` weren't
    /// [registered] or were [unregistered].
    ///
    /// [`write_many`]: Self::write_many
    /// [`write_with`]: Self::write_with
    /// [registered]: Self::register
    /// [unregistered]: Self::unregister
    pub fn write_many_with<'p, const N: usize, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p self,
        pa: &'p mut Pass,
        handles: [&'p Handle; N],
        tup: Tup,
    ) -> Option<([(&'p mut T, &'p mut Buffer); N], Tup::Return)> {
        let (list, buffers, ret) = pa.try_write_many((&*self.0, handles, tup)).ok()?;
        let buf_ids = buffers.each_ref().map(|buf| buf.buffer_id());
        let values = list.get_disjoint_mut(buf_ids.each_ref());

        let list = values
            .into_iter()
            .zip(buffers)
            .map(|(value, buf)| value.zip(Some(buf)))
            .collect::<Option<Vec<_>>>()?;

        Some((list.try_into().ok()?, ret))
    }
}

impl<T: 'static> Default for PerBuffer<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// An item that identifies that you are [writing] or [reading] from
/// an [`RwData<Buffer>`]
///
/// This trait is used exclusively by the [`PerBuffer`] struct, which
/// can bipass the usual requirements that [`Pass`]es need to be used
/// to access the data in [`RwData`]-like structs.
///
/// [writing]: RwData::write
/// [reading]: RwData::read
#[doc(hidden)]
pub trait BufferPass: InnerBufferPass {
    #[doc(hidden)]
    fn buffer_id(&self) -> BufferId;
}

impl BufferPass for Buffer {
    fn buffer_id(&self) -> BufferId {
        Buffer::buffer_id(self)
    }
}
impl<'b> BufferPass for Cursor<'b, Buffer> {
    fn buffer_id(&self) -> BufferId {
        Cursor::buffer_id(self)
    }
}

trait InnerBufferPass {}

impl InnerBufferPass for Buffer {}
impl<'b> InnerBufferPass for Cursor<'b, Buffer> {}
