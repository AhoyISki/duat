//! The primary [`Widget`] of Duat, used to display buffers.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`Buffer`], such as multiple [`Cursor`]s, a
//! `History` system, [`RawArea::PrintInfo`], etc.
//!
//! The [`Buffer`] also provides a list of printed lines through the
//! [`Buffer::printed_lines`] method. This method is notably used by
//! the [`LineNumbers`] widget, that shows the numbers of the
//! currently printed lines.
//!
//! [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
//! [`Cursor`]: crate::mode::Cursor
//! [`RawArea::PrintInfo`]: crate::ui::traits::RawArea::PrintInfo
use std::{
    any::Any,
    fs,
    ops::Range,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, MutexGuard},
};

use crossterm::event::{MouseButton, MouseEventKind};

use crate::{
    Ranges,
    context::{self, Cache, Handle},
    data::Pass,
    hook::{self, BufferUpdated, BufferWritten},
    mode::{MouseEvent, Selections},
    opts::PrintOpts,
    session::TwoPointsPlace,
    text::{Bytes, Change, Moment, MomentFetcher, Point, Strs, Text, TextRange, TextState, txt},
    ui::{Area, PrintedLine, Widget},
};

/// The widget that is used to print and edit buffers
pub struct Buffer {
    path: PathKind,
    text: Text,
    pub(crate) layout_order: usize,
    stored: Mutex<Vec<Box<dyn Any + Send + 'static>>>,

    moment: Moment,
    moment_fetcher: Option<MomentFetcher>,
    cached_print_info: Mutex<Option<CachedPrintInfo>>,

    /// The [`PrintOpts`] of this [`Buffer`]
    ///
    /// You can use this member to change the way this `Buffer` will
    /// be printed specifically.
    pub opts: PrintOpts,
    sync_opts: Arc<Mutex<PrintOpts>>,
}

impl Buffer {
    /// Returns a new [`Buffer`], private for now
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
                    let text = Text::from_parts(Bytes::new(&buffer), selections, true);
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

        let moment_fetcher = Some(text.history().unwrap().new_fetcher());

        Self {
            path,
            text,
            layout_order: 0,
            stored: Mutex::default(),

            moment: Moment::default(),
            moment_fetcher,

            cached_print_info: Mutex::new(None),

            sync_opts: Arc::new(Mutex::new(opts)),
            opts,
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
                    .save_on(std::io::BufWriter::new(fs::File::create(&path)?))
                    .inspect(|_| self.path = PathKind::SetExists(path.clone()))?;

                let path = path.to_string_lossy().to_string();
                hook::queue(BufferWritten((path, bytes, quit)));

                Ok(Some(bytes))
            } else {
                Ok(None)
            }
        } else {
            Err(txt!("No buffer was set"))
        }
    }

    /// Writes the buffer to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub fn save_to(&mut self, path: impl AsRef<std::path::Path>) -> std::io::Result<Option<usize>> {
        self.save_quit_to(path, false)
    }

    /// Writes the buffer to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub(crate) fn save_quit_to(
        &mut self,
        path: impl AsRef<std::path::Path>,
        quit: bool,
    ) -> std::io::Result<Option<usize>> {
        if self.text.has_unsaved_changes() {
            let path = path.as_ref();
            let res = self
                .text
                .save_on(std::io::BufWriter::new(fs::File::create(path)?))
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
    /// This represents the three possible states for a `Buffer`'s
    /// `PathBuf`, as it could either represent a real `Buffer`,
    /// not exist, or not have been defined yet.
    pub fn path_kind(&self) -> PathKind {
        self.path.clone()
    }

    ////////// Auxiliatory methods for incremental parsing

    /// Returns a [`Moment`] with the list of [`Change`]s that took
    /// place since the last [`BufferUpdated`] triggering
    ///
    /// This also includes all `Change`s that you have made while
    /// mutably borrowing this `Buffer`
    pub fn new_changes(&mut self) -> &Moment {
        self.inter_hook_update();
        &self.moment
    }

    /// A tracker of the [`Change`]s that take place in a `Buffer`,
    /// meant for parallel parsing
    ///
    /// This tracker creates a copy of the `Buffer`'s [`Bytes`],
    /// updating them to the newest version as you call
    /// [`BufferTracker::update`]. This lets you process the `Bytes`
    /// for as long as necessary, without interrupting the user's
    /// operation.
    ///
    /// This tracker also has the ability to automatically keep track
    /// of which ranges have had changes on them. By default, this is
    /// done linewise, so if a change takes place on the line y, then
    /// [`BufferTracker::changed_ranges`]
    pub fn tracker(&self) -> BufferTracker {
        BufferTracker {
            bytes: self.bytes().clone(),
            opts: self.sync_opts.clone(),
            fetcher: self.text.history().unwrap().new_fetcher(),
            moment: Moment::default(),
            ranges: RangesTracker::ChangedLines(Ranges::new(0..self.bytes().len().byte())),
        }
    }

    /// Stores an "object" to associate it with this `Buffer`
    ///
    /// This is useful if you want to associate an object with each
    /// `Buffer` and have that object be dropped as the `Buffer` is.
    /// It is particularly useful for "parsers", a design pattern
    /// where a struct is responsible for updating the `Buffer`
    /// whenever it changes, while also being retrievable by, for
    /// example, trait methods added to the `Buffer`, for ease of
    /// convenience.
    ///
    /// An example of where this is used is in the `duat-jump-list`
    /// crate, which associates a jump list with each `Buffer`. As
    /// those `Buffer`s get removed from Duat, so too do the jump
    /// lists.
    ///
    /// If the object was already added to the `Buffer`, this new one
    /// will replace the old, returning it.
    pub fn store_object<T: Any + Send>(&self, obj: T) -> Option<T> {
        let mut stored = self.stored.lock().unwrap();
        if let Some(prev) = stored.iter_mut().find_map(|any| any.downcast_mut()) {
            Some(std::mem::replace(prev, obj))
        } else {
            stored.push(Box::new(obj));
            None
        }
    }

    /// Calls a mutating function on the stored object
    ///
    /// Within this function, any calls to [`Buffer::write_object`] on
    /// the same `T` will return [`None`], since the object is "busy"
    /// in this function. Calls on other types will still work.
    ///
    /// Returns [`None`] if there was no object.
    pub fn write_object<T: Any + Send, Ret>(&self, f: impl FnOnce(&mut T) -> Ret) -> Option<Ret> {
        let mut stored = self.stored.lock().unwrap();

        if let Some(i) = stored
            .iter()
            .position(|any| any.downcast_ref::<T>().is_some())
        {
            let mut obj = stored.remove(i);
            drop(stored);

            let ret = f(obj.downcast_mut().unwrap());

            self.stored.lock().unwrap().insert(i, obj);

            Some(ret)
        } else {
            None
        }
    }

    /// An inter hook call, in order to rectify potential [`Text`]
    /// modifications.
    pub(crate) fn inter_hook_update(&mut self) {
        for change in self.moment_fetcher.as_mut().unwrap().get_moment().changes() {
            self.moment.add_change(None, change.to_string_change());
        }
    }

    /// Resets the print info if deemed necessary, returning the final
    /// result, as well as `true` if things have changed
    ///
    /// After calling this, `self.cached_print_info` is guaranteed to
    /// be [`Some`]
    fn reset_print_info_if_needed<'a>(
        &'a self,
        area: &Area,
    ) -> MutexGuard<'a, Option<CachedPrintInfo>> {
        let mut sync_opts = self.sync_opts.lock().unwrap();
        let opts_changed = if *sync_opts != self.opts {
            *sync_opts = self.opts;
            true
        } else {
            false
        };

        let mut cached_print_info = self.cached_print_info.lock().unwrap();
        if opts_changed
            || area.has_changed()
            || cached_print_info
                .as_ref()
                .is_none_or(|cpi| self.text.text_state().has_changed_since(cpi.text_state))
        {
            let start = area.start_points(&self.text, self.opts).real;
            let end = area.end_points(&self.text, self.opts).real;
            let printed_line_numbers = area.get_printed_lines(&self.text, self.opts).unwrap();

            *cached_print_info = Some(CachedPrintInfo {
                range: start..end,
                printed_line_numbers,
                printed_line_ranges: None,
                _visible_line_ranges: None,
                text_state: self.text.text_state(),
            });
        }

        cached_print_info
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
            path: self.path.clone(),
            text: std::mem::take(&mut self.text),
            layout_order: self.layout_order,
            stored: Mutex::default(),

            moment: Moment::default(),
            moment_fetcher: self.moment_fetcher.take(),
            cached_print_info: Mutex::new(self.cached_print_info.lock().unwrap().take()),

            sync_opts: Arc::default(),
            opts: PrintOpts::default(),
        }
    }
}

impl Widget for Buffer {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        // Asynchronous updating of opts
        let (buffer, area) = handle.write_with_area(pa);

        if let Some(main) = buffer.text().selections().get_main() {
            area.scroll_around_points(
                buffer.text(),
                main.caret().to_two_points_after(),
                buffer.get_print_opts(),
            );
        }

        drop(buffer.reset_print_info_if_needed(area));
        buffer.moment = buffer.moment_fetcher.as_mut().unwrap().get_moment();

        hook::trigger(pa, BufferUpdated(handle.clone()));

        let buffer = handle.write(pa);
        buffer.moment = Moment::default();
        buffer.text.update_bounds();
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.cached_print_info
            .lock()
            .unwrap()
            .as_ref()
            .is_none_or(|cpi| self.text.text_state().has_changed_since(cpi.text_state))
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
    pub fn printed_line_numbers(&self, pa: &Pass) -> Vec<PrintedLine> {
        let buffer = self.read(pa);
        let cpi = buffer.reset_print_info_if_needed(self.area().read(pa));
        cpi.as_ref().unwrap().printed_line_numbers.clone()
    }

    /// The printed [`Range<Point>`]
    ///
    /// Do note that this includes all concealed lines and parts that
    /// are out of screen. If you want only to include partially
    /// visible lines, while excluding fully hidden ones, check out
    /// [`Handle::printed_lines`]. If you want to exclude every
    /// concealed or out of screen section, check out
    /// [`Handle::visible_lines`].
    pub fn printed_range(&self, pa: &Pass) -> Range<Point> {
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
    /// [`Handle::printed_range`].
    ///
    /// If you just want the line numbers of the printed lines, check
    /// out [`Handle::printed_line_numbers`].
    ///
    /// [concealed]: crate::text::Conceal
    pub fn printed_lines<'a>(&'a self, pa: &'a Pass) -> Vec<Strs<'a>> {
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
                    .map(|line| buffer.text.line_range(line.number))
                    .collect(),
            )
        };

        printed_lines
            .iter()
            .map(|range| buffer.text.strs(range.clone()).unwrap())
            .collect()
    }

    /// Only the visible parts of printed lines
    ///
    /// This is just like [`Handle::printed_lines`], but excludes
    /// _every_ section that was concealed or is not visible on
    /// screen.
    pub fn visible_lines<'a>(&'a self, _: &'a Pass) -> Vec<Strs<'a>> {
        todo!();
    }
}

/// A tracker for [`Change`]s that happen to a [`Buffer`]
///
/// This tracker will keep a secondary copy of the `Buffer`'s
/// [`Bytes`], which will be updated to reflect the current state of
/// the `Bytes` whenever [`BufferTracker::update`] is called.
///
/// This allows for asynchronous tracking of the `Buffer` in other
/// threads, without perturbing the experience of the user.
///
/// [`Change`]: crate::text::Change
#[derive(Debug)]
pub struct BufferTracker {
    bytes: Bytes,
    opts: Arc<Mutex<PrintOpts>>,
    fetcher: MomentFetcher,
    moment: Moment,
    ranges: RangesTracker,
}

impl BufferTracker {
    /// Updates the inner [`Bytes`] and retrieves latest [`Moment`]
    ///
    /// This will make the `ByteTracker`'s `Bytes` reflect the most up
    /// to date version of the [`Buffer`]'s `Bytes`. Keep in mind
    /// that, if you are calling this from a spawned thread, then this
    /// is _not_ a guarantee that you the two `Bytes` are the same,
    /// since they could have updated again after calling this method,
    /// on the main thread.
    ///
    /// If, however, you're calling this from [`Parser::parse`], for
    /// example, this is guaranteed to be the in sync with the
    /// `Buffer`'s `Bytes`, since you're calling it from the main
    /// thread.
    ///
    /// [`Change`]: crate::text::Change
    pub fn update(&mut self) {
        self.moment = self.fetcher.get_moment();
        for change in self.moment.changes() {
            self.bytes.apply_change(change);
            self.ranges.apply_change(change, &self.bytes);
        }
    }

    ////////// Range tracking functions

    /// Adds a byte/[`Point`] [`Range`] to be updated on
    /// [`Parser::update`]
    ///
    /// This [`Range`] will be merged with other [`Ranges`] on the
    /// list, and when a [parse is requested], the intersections
    /// between the [`Ranges`] sent to this method and the [`Range`]
    /// of the [`Text`] shown on area will be updated.
    ///
    /// For example, if you add the ranges  `3..20` and `50..87`, and
    /// the range shown on area is `17..61`, then two calls to
    /// [`Parser::update`] will be made, one with the range
    /// `17..20` and one with the range `50..61`. After that, the
    /// ranges `3..17` and `61..87` will remain on the list of
    /// [`Range`]s to be updated
    ///
    /// [parse is requested]: Self::request_parse
    pub fn add_range(&mut self, range: impl TextRange) {
        let range = range.to_range(self.bytes.len().byte());
        self.ranges.add_range(range);
    }

    /// Automatically add every [`Change`]'s [added range] to update
    ///
    /// This function turns on automatic [`Range`] addition for every
    /// [`Change`] that takes place. You can still add [`Range`]s
    /// manually through [`add_range`].
    ///
    /// [added range]: Change::added_range
    /// [`add_range`]: BufferTracker::add_range
    pub fn track_changed_ranges(&mut self) {
        self.ranges = RangesTracker::ChangedRanges(self.ranges.take_ranges());
    }

    /// Automatically add every changed line to the ranges to update
    ///
    /// This function turns on automatic [`Range`] addition for every
    /// line that a [`Change`]'s [added range] belongs to. You can
    /// still add [`Range`]s manually through [`add_range`].
    ///
    /// It's probably the most common type of automatic range tracking
    /// that you'd want, since, for example, regex patterns are very
    /// convenient to separate line-wise, since most of them don't
    /// tend to go over lines.
    ///
    /// Additionally, this option will also make it so only _whole
    /// lines_ are updated. For example, if a line is partially cutoff
    /// from the area, the range passed to [`Parser::update`] will
    /// include the parts of the line that are outside of the area.
    /// For the other tracking options, this won't be the case.
    ///
    /// This is the default method of tracking [`Change`]s.
    ///
    /// [added range]: Change::added_range
    /// [`add_range`]: BufferTracker::add_range
    pub fn track_changed_lines(&mut self) {
        self.ranges = RangesTracker::ChangedLines(self.ranges.take_ranges());
    }

    /// Disable the automatic [`Change`] tracking functionality
    ///
    /// This makes it so no [`Range`]s to update are added
    /// automatically whenever a [`Change`] takes place. Instead, they
    /// all must be added through [`add_range`].
    ///
    /// [`add_range`]: BufferTracker::add_range
    pub fn disable_change_tracking(&mut self) {
        self.ranges = RangesTracker::Manual(self.ranges.take_ranges());
    }

    /// Returns a list of [`Range<Point>`]s that need to be updated
    ///
    /// These will be defined by your [`Change`] tracking policy, so
    /// if you called [`BufferTracker::track_changed_lines`], these
    /// will be ranges of full lines, where at least one `Change` took
    /// place.
    ///
    /// This function assumes that you will act on these ranges, and
    /// as such, they no longer need to be updated, so it will take
    /// them out of the list. This greatly helps with the efficiency
    /// of Duat, since there might be many `BufferTracker`s, and
    /// minimizing the amount of work that they need to do will be
    /// great for performance.
    ///
    /// Keep in mind that this tracks the current state of the
    /// `BufferTracker`'s [`Bytes`], _not_ the [`Buffer`]'s, so you
    /// will probably want to call [`BufferTracker::update`] before
    /// calling this tbh.
    pub fn ranges_to_update_on(
        &mut self,
        range: impl TextRange,
    ) -> impl Iterator<Item = Range<Point>> {
        self.ranges
            .remove(range.to_range(self.bytes.len().byte()), &self.bytes)
    }

    ////////// Querying functions

    /// The [`Bytes`], as they are since the last call to [`update`]
    ///
    /// Do note that, because this tracks calls to [`update`], this
    /// version of the [`Bytes`] may not reflect what the [`Bytes`]
    /// look like in the [`Buffer`] _right now_, as more [`Change`]s
    /// could have taken place since the last call.
    ///
    /// [`update`]: Self::update
    /// [`Change`]: crate::text::Change
    pub fn bytes(&self) -> &Bytes {
        &self.bytes
    }

    /// The last [`Moment`] that was processed
    ///
    /// This is **not** the last [`Moment`] sent to the [`Buffer`],
    /// nor does it necessarily correspond to a [`Moment`] in the
    /// [`Text`]'s history. It's just a collection of [`Change`]s that
    /// took place in between calls to [`Self::update`].
    ///
    /// [`Change`]: crate::text::Change
    pub fn moment(&self) -> &Moment {
        &self.moment
    }

    /// The indentation in the line at a [`Point`]
    ///
    /// This value only takes ascii spaces and tabs into account,
    /// which may differ from the value from [`Text::indent`],
    /// since that one calculates the indent through the [`Area`],
    /// while this one only makes use of the [`Buffer`]'s
    /// [`PrintOpts`].
    ///
    /// [`Area`]: crate::ui::Area
    pub fn indent(&self, p: Point) -> usize {
        self.bytes.indent(p, *self.opts.lock().unwrap())
    }

    /// The [`PrintOpts`] of the [`Buffer`]
    ///
    /// Unlike the other parts of this struct, the [`PrintOpts`] will
    /// always be up to date with what it currently is in the
    /// [`Buffer`]
    pub fn opts(&self) -> PrintOpts {
        *self.opts.lock().unwrap()
    }
}

/// Tracker for which [`Range`]s to update
#[derive(Debug)]
enum RangesTracker {
    Manual(Ranges),
    ChangedRanges(Ranges),
    ChangedLines(Ranges),
}

impl RangesTracker {
    /// Manually adds a [`Range`] to be tracked
    #[track_caller]
    fn add_range(&mut self, range: Range<usize>) {
        match self {
            RangesTracker::Manual(ranges)
            | RangesTracker::ChangedRanges(ranges)
            | RangesTracker::ChangedLines(ranges) => ranges.add(range),
        }
    }

    /// Applies a [`Change`] to the [`Range`]s within
    fn apply_change(&mut self, change: Change, bytes: &Bytes) {
        match self {
            RangesTracker::Manual(ranges) => {
                ranges.shift_by(change.start().byte(), change.shift()[0])
            }
            RangesTracker::ChangedRanges(ranges) => {
                ranges.shift_by(change.start().byte(), change.shift()[0]);
                ranges.add(change.start().byte()..change.added_end().byte())
            }
            RangesTracker::ChangedLines(ranges) => {
                ranges.shift_by(change.start().byte(), change.shift()[0]);
                let start = bytes.point_at_line(change.start().line());
                let end =
                    bytes.point_at_line((change.added_end().line() + 1).min(bytes.len().line()));
                ranges.add(start.byte()..end.byte());
            }
        }
    }

    /// Takes the [`Ranges`] from the tracker
    fn take_ranges(&mut self) -> Ranges {
        match self {
            RangesTracker::Manual(ranges)
            | RangesTracker::ChangedRanges(ranges)
            | RangesTracker::ChangedLines(ranges) => std::mem::take(ranges),
        }
    }

    /// Gets the list of [`Range`]s that need to be updated
    #[track_caller]
    fn remove(
        &mut self,
        range: Range<usize>,
        bytes: &Bytes,
    ) -> impl ExactSizeIterator<Item = Range<Point>> {
        match self {
            RangesTracker::Manual(ranges)
            | RangesTracker::ChangedRanges(ranges)
            | RangesTracker::ChangedLines(ranges) => ranges
                .remove(range)
                .map(|r| bytes.point_at_byte(r.start)..bytes.point_at_byte(r.end)),
        }
    }
}

/// Cached information about the printing of this [`Buffer`]
struct CachedPrintInfo {
    range: Range<Point>,
    printed_line_numbers: Vec<PrintedLine>,
    printed_line_ranges: Option<Vec<Range<Point>>>,
    _visible_line_ranges: Option<Vec<Range<Point>>>,
    text_state: TextState,
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
