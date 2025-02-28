//! The primary data structure in Duat
//!
//! This struct is responsible for all of the text that will be
//! printed to the screen, as well as any modifications of it.
//!
//! The [`Text`] is a very versatile holder for characters, below is a
//! list of some of its capabilities:
//!
//! - Be cheaply* edited at any point, due to its two [gap buffers];
//! - Be [colored] in any way, at any point;
//! - Have any arbitrary range concealed, that is, hidden from view,
//!   but still in there;
//! - Arbitrary [ghost text], that is, [`Text`] that shows up, but is
//!   not actually part of the [`Text`], i.e., it can be easily
//!   ignored by external modifiers (like an LSP or tree-sitter) of
//!   the file, without any special checks;
//! - Left/right/center alignment of output (although that is
//!   implemented by the [`Ui`]);
//! - The ability to undo/redo changes in the history;
//! - In the future, button ranges that can interact with the mouse;
//!
//! The [`Text`] struct is created in two different ways:
//!
//! - By calling [`Text::new`] or one of its [`From`] implementations;
//! - By building it with the [`text!`] macro;
//!
//! The first method is recommended if you want a [`Text`] that will
//! be modified by input. The only real example of this is the
//! [`File`] widget.
//!
//! The second method is what should be used most of the time, as it
//! lets you quickly create formatted [`Widget`]s/[`StatusLine`] parts
//! in a very modular way:
//!
//! ```rust
//! # use duat_core::text::{text, Text};
//! fn number_of_horses(count: usize) -> Text {
//!     if count == 1 {
//!         text!([HorseCount] 1 " " [Horses] "horse")
//!     } else {
//!         text!([HorseCount] count " " [Horses] "horses")
//!     }
//! }
//! ```
//!
//! You can use this whenever you need to update a widget, for
//! example, just create a new [`Text`] to printed to the screen.
//!
//! However, when recreating the entire [`Text`] with a [`text!`]
//! macro would be too expensive, you can use [`Text`] modifying
//! functions:
//!
//! ```rust
//! # use duat_core::text::{text, Text};
//! let mut prompted = text!([Prompt] "type a key:");
//! let end = prompted.len();
//! prompted.replace_range((end, end), "a")
//! ```
//!
//! These would be used mostly on the [`File`] widget and other whose
//! [`Mode`]s make use of [`EditHelper`]s.
//!
//! [gap buffers]: GapBuffer
//! [colored]: crate::form::Form
//! [ghost text]: Tag::GhostText
//! [`Ui`]: crate::ui::Ui
//! [`File`]: crate::widgets::File
//! [`Widget`]: crate::widgets::Widget
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`Mode`]: crate::mode::Mode
//! [`EditHelper`]: crate::mode::EditHelper
mod builder;
mod history;
mod iter;
mod ops;
mod reader;
mod records;
mod search;
mod tags;
mod treesitter;

use std::{
    array::IntoIter,
    ops::{Range, RangeBounds},
    path::Path,
    rc::Rc,
    str::from_utf8_unchecked,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

pub use gapbuf::GapBuffer;
use records::Records;
use tags::{FwdTags, RevTags};

pub(crate) use self::history::History;
use self::tags::Tags;
pub use self::{
    builder::{AlignCenter, AlignLeft, AlignRight, Builder, Ghost, err, hint, ok, text},
    history::Change,
    iter::{Item, Iter, Part, RevIter},
    ops::{Point, TextRange, TwoPoints, utf8_char_width},
    reader::Reader,
    search::{Matcheable, RegexPattern, Searcher},
    tags::{Key, Keys, Tag, ToggleId},
    treesitter::TsParser,
};
use crate::{
    DuatError, cache,
    cfg::PrintCfg,
    mode::{Cursor, Cursors},
    ui::Area,
};

/// The text in a given [`Area`]
#[derive(Default)]
pub struct Text {
    buf: GapBuffer<u8>,
    tags: Box<Tags>,
    records: Box<Records<[usize; 3]>>,
    cursors: Option<Box<Cursors>>,
    // Specific to Files
    history: Option<Box<History>>,
    readers: Vec<(Box<dyn Reader>, Vec<Range<usize>>)>,
    ts_parser: Option<(Box<TsParser>, Vec<Range<usize>>)>,
    has_changed: bool,
    has_unsaved_changes: AtomicBool,
    // Used in Text building
    forced_new_line: bool,
}

impl Text {
    ////////// Creation of Text

    /// Returns a new empty [`Text`]
    pub fn new() -> Self {
        Self::from_buf(GapBuffer::default(), None, false)
    }

    pub fn new_with_cursors() -> Self {
        Self::from_buf(GapBuffer::default(), Some(Cursors::default()), false)
    }

    /// Returns a new empty [`Text`] with history enabled
    pub(crate) fn new_with_history() -> Self {
        Self::from_buf(GapBuffer::default(), Some(Cursors::default()), true)
    }

    /// Creates a [`Text`] from a file's [path]
    ///
    /// [path]: Path
    pub(crate) fn from_file(
        buf: GapBuffer<u8>,
        cursors: Cursors,
        path: impl AsRef<Path>,
        has_unsaved_changes: bool,
    ) -> Self {
        let mut text = Self::from_buf(buf, Some(cursors), true);
        text.has_unsaved_changes
            .store(has_unsaved_changes, Ordering::Relaxed);

        if let Some(history) = cache::load_cache(path.as_ref()) {
            text.history = Some(Box::new(history));
        }

        let tree_sitter = TsParser::new(&mut text, path);
        text.ts_parser = tree_sitter.map(|ts| (Box::new(ts), Vec::new()));
        text
    }

    /// Creates a [`Text`] from a [`GapBuffer`]
    pub(crate) fn from_buf(
        mut buf: GapBuffer<u8>,
        cursors: Option<Cursors>,
        with_history: bool,
    ) -> Self {
        let forced_new_line = if buf.is_empty() || buf[buf.len() - 1] != b'\n' {
            buf.push_back(b'\n');
            true
        } else {
            false
        };
        let len = buf.len();
        let chars = unsafe {
            let (s0, s1) = buf.as_slices();
            std::str::from_utf8_unchecked(s0).chars().count()
                + std::str::from_utf8_unchecked(s1).chars().count()
        };
        let lines = buf.iter().filter(|b| **b == b'\n').count();
        let tags = Box::new(Tags::with_len(buf.len()));

        Self {
            buf,
            tags,
            cursors: cursors.map(Box::new),
            records: Box::new(Records::with_max([len, chars, lines])),
            history: with_history.then(Box::default),
            readers: Vec::new(),
            ts_parser: None,
            forced_new_line,
            has_changed: false,
            has_unsaved_changes: AtomicBool::new(false),
        }
    }

    /// Returns a [`Builder`] for [`Text`]
    ///
    /// This builder can be used to iteratively create text, by
    /// assuming that the user wants no* [`Tag`] overlap, and that
    /// they want to construct the [`Text`] in [`Tag`]/content pairs.
    ///
    /// ```rust
    /// use duat_core::text::{Tag, Text, text};
    /// let mut builder = Text::builder();
    /// ```
    pub fn builder() -> Builder {
        Builder::new()
    }

    ////////// Querying functions

    /// The [`Point`] at the end of the text
    pub fn len(&self) -> Point {
        let [b, c, l] = self.records.max();
        Point::from_raw(b, c, l)
    }

    /// Whether or not there are any characters in the [`Text`]
    ///
    /// # Note
    ///
    /// This does not check for tags, so with a [`Tag::GhostText`],
    /// there could actually be a "string" of characters on the
    /// [`Text`], it just wouldn't be considered real "text".
    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    /// The `char` at the [`Point`]'s position
    pub fn char_at(&self, point: Point) -> Option<char> {
        let [s0, s1] = self.strs();
        if point.byte() < s0.len() {
            s0[point.byte()..].chars().next()
        } else {
            s1[point.byte() - s0.len()..].chars().next()
        }
    }

    /// The two [`&str`]s that compose the [buffer]
    ///
    /// In order to iterate over them, I recommend using the
    /// [`flat_map`] method:
    ///
    /// ```rust
    /// # use duat_core::text::Text;
    /// let text = Text::new();
    /// text.strs().into_iter().flat_map(str::chars);
    /// ```
    ///
    /// Do note that you should avoid iterators like [`str::lines`],
    /// as they will separate the line that is partially owned by each
    /// [`&str`]:
    ///
    /// ```rust
    /// let broken_up_line = [
    ///     "This is line 1, business as usual This is line 2, but it",
    ///     "is broken into two separate strings So 4 lines would be counted, \
    ///      instead of 3",
    /// ];
    /// ```
    ///
    /// If you want the two [`&str`]s in a range, see
    /// [`strs_in`]
    ///
    /// [`&str`]: str
    /// [buffer]: GapBuffer
    /// [`flat_map`]: Iterator::flat_map
    /// [`strs_in`]: Self::strs_in
    pub fn strs(&self) -> [&'_ str; 2] {
        let (s0, s1) = self.buf.as_slices();
        unsafe { [from_utf8_unchecked(s0), from_utf8_unchecked(s1)] }
    }

    /// Returns 2 [`&str`]s in the given [range]
    ///
    /// # Note
    ///
    /// The reason why this function returns two strings is that the
    /// contents of the text are stored in a [`GapBuffer`], which
    /// works with two strings.
    ///
    /// If you want to iterate over them, you can do the following:
    ///
    /// ```rust
    /// # use duat_core::text::{Point, Text};
    /// # let (p1, p2) = (Point::default(), Point::default());
    /// let text = Text::new();
    /// text.strs_in((p1, p2)).flat_map(str::bytes);
    /// ```
    ///
    /// Do note that you should avoid iterators like [`str::lines`],
    /// as they will separate the line that is partially owned by each
    /// [`&str`]:
    ///
    /// ```rust
    /// let broken_up_line = [
    ///     "This is line 1, business as usual.\nThis is line 2, but it",
    ///     "is broken into two separate strings.\nSo 4 lines would be counted, \
    ///      instead of 3",
    /// ];
    /// ```
    ///
    /// # [`TextRange`] behavior:
    ///
    /// If you give a single [`usize`]/[`Point`], it will be
    /// interpreted as a range from.
    ///
    /// If you want the two full [`&str`]s, see [`strs`]
    ///
    /// [`&str`]: str
    /// [range]: TextRange
    /// [`strs`]: Self::strs
    pub fn strs_in(&self, range: impl TextRange) -> IntoIter<&str, 2> {
        let range = range.to_range_fwd(self.len().byte());
        self.strs_in_range_inner(range).into_iter()
    }

    /// Returns an iterator over the lines in a given range
    ///
    /// The lines are inclusive, that is, it will iterate over the
    /// whole line, not just the parts within the range.
    pub fn lines_in(
        &mut self,
        range: impl TextRange,
    ) -> impl DoubleEndedIterator<Item = (usize, &str)> {
        let range = range.to_range_fwd(self.len().byte());
        let start = self.point_at_line(self.point_at(range.start).line());
        let end = {
            let end = self.point_at(range.end);
            let line_start = self.point_at_line(end.line());
            match line_start == end {
                true => end,
                false => self.point_at_line((end.line() + 1).min(self.len().line())),
            }
        };
        self.make_contiguous_in((start, end));
        unsafe {
            let lines = self.continuous_in_unchecked((start, end)).lines();
            let (fwd_i, rev_i) = (start.line(), end.line());
            TextLines { lines, fwd_i, rev_i }
        }
    }

    /// Returns the two `&str`s in the byte range.
    fn strs_in_range_inner(&self, range: impl RangeBounds<usize>) -> [&str; 2] {
        let (s0, s1) = self.buf.as_slices();
        let (start, end) = crate::get_ends(range, self.len().byte());
        let (start, end) = (start, end);
        // Make sure the start and end are in character bounds.
        assert!(
            [start, end]
                .into_iter()
                .filter_map(|b| self.buf.get(b))
                .all(|b| utf8_char_width(*b) > 0),
        );

        unsafe {
            let r0 = start.min(s0.len())..end.min(s0.len());
            let r1 = start.saturating_sub(s0.len()).min(s1.len())
                ..end.saturating_sub(s0.len()).min(s1.len());

            [from_utf8_unchecked(&s0[r0]), from_utf8_unchecked(&s1[r1])]
        }
    }

    /// Returns the [`TsParser`], if there is one
    ///
    /// This parser uses tree-sitter internally for things like syntax
    /// highlighing and indentation, but it have all sorts of
    /// utilities in user code as well.
    pub fn ts_parser(&self) -> Option<&TsParser> {
        self.ts_parser.as_ref().map(|(ts, _)| ts.as_ref())
    }

    /// Gets the indentation on a given [`Point`]
    ///
    /// Will either return the required indentation (in spaces) or
    /// [`None`], in which case the caller will have to decide how to
    /// proceed. This usually means "keep previous level of
    /// indentation".
    pub fn ts_indent_on(&mut self, p: Point, cfg: PrintCfg) -> Option<usize> {
        let ts = self.ts_parser.take();
        let indent = ts.as_ref().and_then(|(ts, _)| ts.indent_on(self, p, cfg));
        self.ts_parser = ts;
        indent
    }

    ////////// Point querying functions

    /// The [`Point`] corresponding to the byte position, 0 indexed
    ///
    /// If the byte position would fall in between two characters
    /// (because the first one comprises more than one byte), the
    /// first character is chosen as the [`Point`] where the byte is
    /// located.
    ///
    /// # Panics
    ///
    /// Will panic if `at` is greater than the length of the text
    #[inline(always)]
    pub fn point_at(&self, at: usize) -> Point {
        assert!(
            at <= self.len().byte(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len().byte()
        );
        let [b, c, mut l] = self.records.closest_to(at);

        let found = if at >= b {
            let [s0, s1] = self.strs_in_range_inner(b..);

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    l += (char == '\n') as usize;
                    (b + this_b, c + i, l - (char == '\n') as usize)
                })
                .take_while(|&(b, ..)| at >= b)
                .last()
        } else {
            let mut c_len = 0;
            self.strs_in_range_inner(..b)
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .enumerate()
                .map(|(i, char)| {
                    l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (b - c_len, c - (i + 1), l)
                })
                .take_while(|&(b, ..)| b >= at)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
    }

    /// The [`Point`] associated with a char position, 0 indexed
    ///
    /// # Panics
    ///
    /// Will panic if `at` is greater than the number of chars in the
    /// text.
    #[inline(always)]
    pub fn point_at_char(&self, at: usize) -> Point {
        assert!(
            at <= self.len().char(),
            "byte out of bounds: the len is {}, but the char is {at}",
            self.len().char()
        );
        let [b, c, mut l] = self.records.closest_to_by_key(at, |[_, c, _]| *c);

        let found = if at >= c {
            let [s0, s1] = self.strs_in_range_inner(b..);

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    l += (char == '\n') as usize;
                    (b + this_b, c + i, l - (char == '\n') as usize)
                })
                .take_while(|&(_, c, _)| at >= c)
                .last()
        } else {
            let mut c_len = 0;
            self.strs_in_range_inner(..)
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .enumerate()
                .map(|(i, char)| {
                    l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (b - c_len, c - (i + 1), l)
                })
                .take_while(|&(_, c, _)| c >= at)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
    }

    /// The [`Point`] where the `at`th line starts, 0 indexed
    ///
    /// If `at == number_of_lines`, returns the last point of the
    /// text.
    ///
    /// # Panics
    ///
    /// Will panic if the number `at` is greater than the number of
    /// lines on the text
    #[inline(always)]
    pub fn point_at_line(&self, at: usize) -> Point {
        assert!(
            at <= self.len().line(),
            "byte out of bounds: the len is {}, but the line is {at}",
            self.len().line()
        );
        let (b, c, mut l) = {
            let [mut b, mut c, l] = self.records.closest_to_by_key(at, |[.., l]| *l);
            self.strs_in_range_inner(..b)
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .take_while(|c| *c != '\n')
                .for_each(|char| {
                    b -= char.len_utf8();
                    c -= 1;
                });
            (b, c, l)
        };

        let found = if at >= l {
            let [s0, s1] = self.strs_in_range_inner(b..);

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    l += (char == '\n') as usize;
                    (b + this_b, c + i, l - (char == '\n') as usize)
                })
                .find(|&(.., l)| at == l)
        } else {
            let mut c_len = 0;
            self.strs_in_range_inner(..b)
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .enumerate()
                .map(|(i, char)| {
                    l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (b - c_len, c - (i + 1), l)
                })
                .take_while(|&(.., l)| l >= at)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
    }

    /// The start and end [`Point`]s for a given `at` line
    ///
    /// If `at == number_of_lines`, these points will be the same.
    ///
    /// # Panics
    ///
    /// Will panic if the number `at` is greater than the number of
    /// lines on the text
    #[inline(always)]
    pub fn points_of_line(&self, at: usize) -> (Point, Point) {
        assert!(
            at <= self.len().line(),
            "byte out of bounds: the len is {}, but the line is {at}",
            self.len().line()
        );

        let start = self.point_at_line(at);
        let end = self
            .chars_fwd(start)
            .find_map(|(p, _)| (p.line() > start.line()).then_some(p))
            .unwrap_or(start);
        (start, end)
    }

    /// The [points] at the end of the text
    ///
    /// This will essentially return the [last point] of the text,
    /// alongside the last possible [`Point`] of any
    /// [`Tag::GhostText`] at the end of the text.
    ///
    /// [points]: TwoPoints
    /// [last point]: Self::len
    pub fn len_points(&self) -> (Point, Option<Point>) {
        self.ghost_max_points_at(self.len().byte())
    }

    /// The last [`Point`] associated with a `char`
    ///
    /// This will give the [`Point`] of the last `char` of the text.
    /// The difference between this method and [`len`] is that
    /// it will return a [`Point`] one position earlier than it. If
    /// the text is completely empty, it will return [`None`].
    ///
    /// [`len`]: Self::len
    pub fn last_point(&self) -> Option<Point> {
        self.strs_in_range_inner(..)
            .into_iter()
            .flat_map(str::chars)
            .next_back()
            .map(|char| self.len().rev(char))
    }

    ////////// Points queying functions.

    /// The maximum [points] in the `at`th byte
    ///
    /// This point is essentially the [point] at that byte, plus the
    /// last possible [`Point`] of any [`Tag::GhostText`]s in that
    /// position.
    ///
    /// [points]: TwoPoints
    /// [point]: Self::point_at
    #[inline(always)]
    pub fn ghost_max_points_at(&self, at: usize) -> (Point, Option<Point>) {
        let point = self.point_at(at);
        (point, self.tags.ghosts_total_at(point.byte()))
    }

    /// Points visually after the [`TwoPoints`]
    ///
    /// If the [`TwoPoints`] in question is concealed, treats the
    /// next visible character as the first character, and returns
    /// the points of the next visible character.
    ///
    /// This method is useful if you want to iterator reversibly
    /// right after a certain point, thus including the character
    /// of said point.
    pub fn points_after(&self, tp: impl TwoPoints) -> Option<(Point, Option<Point>)> {
        self.iter_fwd(tp)
            .filter_map(|item| item.part.as_char().map(|_| item.points()))
            .chain([self.len_points()])
            .nth(1)
    }

    /// The visual start of the line
    ///
    /// This point is defined not by where the line actually begins,
    /// but by where the last '\n' was located. For example, if
    /// [`Tag`]s create ghost text or omit text from multiple
    /// different lines, this point may differ from where in the
    /// [`Text`] the physical line actually begins.
    pub fn visual_line_start(&self, p: impl TwoPoints) -> (Point, Option<Point>) {
        let (real, ghost) = p.to_points();

        let mut iter = self.iter_rev((real, ghost)).peekable();
        let mut points = (real, ghost);
        while let Some(peek) = iter.peek() {
            match peek.part {
                Part::Char('\n') => {
                    return points;
                }
                Part::Char(_) => points = iter.next().unwrap().to_points(),
                _ => drop(iter.next()),
            }
        }

        points
    }

    ////////// String modification functions

    /// Replaces a [range] in the [`Text`]
    ///
    /// # [`TextRange`] behavior:
    ///
    /// If you give a single [`usize`]/[`Point`], it will be
    /// interpreted as a range from.
    ///
    /// [range]: TextRange
    pub fn replace_range(&mut self, range: impl TextRange, edit: impl ToString) {
        let range = range.to_range_at(self.len().byte());
        let (start, end) = (self.point_at(range.start), self.point_at(range.end));
        let change = Change::new(edit, (start, end), self);

        self.has_changed = true;
        self.replace_range_inner(change.as_ref());
        self.history.as_mut().map(|h| h.add_change(None, change));
    }

    pub fn apply_change(
        &mut self,
        guess_i: Option<usize>,
        change: Change<String>,
    ) -> Option<usize> {
        self.has_changed = true;
        self.replace_range_inner(change.as_ref());
        self.history.as_mut().map(|h| h.add_change(guess_i, change))
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace
    fn replace_range_inner(&mut self, change: Change<&str>) {
        let edit = change.added_text();
        let start = change.start();
        let taken_end = change.taken_end();

        let new_len = {
            let lines = edit.bytes().filter(|b| *b == b'\n').count();
            [edit.len(), edit.chars().count(), lines]
        };

        let old_len = unsafe {
            let range = start.byte()..change.taken_end().byte();
            let str = String::from_utf8_unchecked(
                self.buf
                    .splice(range, edit.as_bytes().iter().cloned())
                    .collect(),
            );

            let lines = str.bytes().filter(|b| *b == b'\n').count();
            [str.len(), str.chars().count(), lines]
        };

        let start_rec = [start.byte(), start.char(), start.line()];
        self.records.transform(start_rec, old_len, new_len);
        self.records.insert(start_rec);

        self.tags
            .transform(start.byte()..taken_end.byte(), change.added_end().byte());

        *self.has_unsaved_changes.get_mut() = true;
    }

    /// This is used by [`Area`]s in order to update visible text
    ///
    /// In order to not update too much, an [`Area`] will request that
    /// a region of the [`Text`] (usually roughly what is shown on
    /// screen) to be updated, rather than the whole [`Text`].
    ///
    /// This should be done within the [`Area::print`] and
    /// [`Area::print_with`] functions.
    pub fn update_range(&mut self, range: (Point, Point)) {
        let within = range.0.byte()..(range.1.byte() + 1);
        let mut readers = std::mem::take(&mut self.readers);
        let mut ts = self.ts_parser.take();

        if let Some(changes) = self.history.as_mut().and_then(|h| h.unprocessed_changes()) {
            let changes: Vec<Change<&str>> = changes.iter().map(|c| c.as_ref()).collect();
            self.process_changes(&mut ts, &mut readers, &changes);
        }

        if let Some((ts, ranges)) = ts.as_mut() {
            let mut new_ranges = Vec::new();

            for range in ranges.iter() {
                let (to_check, split_off) = split_range_within(range.clone(), within.clone());
                if let Some(range) = to_check {
                    ts.update_range(self, range);
                }
                new_ranges.extend(split_off.into_iter().flatten());
            }
            *ranges = new_ranges
        }
        for (reader, ranges) in readers.iter_mut() {
            let mut new_ranges = Vec::new();

            for range in ranges.iter() {
                let (to_check, split_off) = split_range_within(range.clone(), within.clone());
                if let Some(range) = to_check {
                    reader.update_range(self, range);
                }
                new_ranges.extend(split_off.into_iter().flatten());
            }
            *ranges = new_ranges;
        }
        self.tags.update_range(within);

        self.has_changed = false;
        self.readers = readers;
        self.ts_parser = ts;
    }

    fn process_changes(
        &mut self,
        ts: &mut Option<(Box<TsParser>, Vec<Range<usize>>)>,
        readers: &mut Vec<(Box<dyn Reader>, Vec<Range<usize>>)>,
        changes: &[Change<&str>],
    ) {
        const MAX_RANGES_TO_CONSIDER: usize = 15;
        if let Some((ts, _)) = ts.as_mut() {
            ts.apply_changes(self, changes);
        }
        for (reader, _) in readers.iter_mut() {
            reader.apply_changes(self, changes);
        }

        if changes.len() <= MAX_RANGES_TO_CONSIDER {
            if let Some((ts, ranges)) = ts.as_mut() {
                for range in ts.ranges_to_update(self, changes) {
                    if !range.is_empty() {
                        merge_range_in(ranges, range);
                    }
                }
            }
            for (reader, ranges) in readers.iter_mut() {
                for range in reader.ranges_to_update(self, changes) {
                    if !range.is_empty() {
                        merge_range_in(ranges, range);
                    }
                }
            }
        // If there are too many changes, cut on processing and
        // just assume that everything needs to be updated.
        } else {
            let ts_range_iter = ts.as_mut().map(|(_, r)| r).into_iter();
            for range in ts_range_iter.chain(readers.iter_mut().map(|(_, r)| r)) {
                *range = vec![0..self.len().byte()];
            }
        }
    }

    pub fn needs_update(&self) -> bool {
        self.has_changed
            || self.ts_parser.as_ref().is_some_and(|(_, r)| !r.is_empty())
            || self.readers.iter().any(|(_, r)| !r.is_empty())
    }

    ////////// History manipulation functions

    /// Undoes the last moment, if there was one
    pub fn undo(&mut self, area: &impl Area, cfg: PrintCfg) {
        let Some(mut history) = self.history.take() else {
            return;
        };
        let mut cursors = self.cursors.take().unwrap();
        let Some(changes) = history.move_backwards() else {
            self.history = Some(history);
            self.cursors = Some(cursors);
            return;
        };

        self.apply_and_process_changes(&changes, Some((area, &mut cursors, cfg)));

        self.has_changed = true;
        self.history = Some(history);
        self.cursors = Some(cursors);
    }

    /// Redoes the last moment in the history, if there is one
    pub fn redo(&mut self, area: &impl Area, cfg: PrintCfg) {
        let Some(mut history) = self.history.take() else {
            return;
        };
        let mut cursors = self.cursors.take().unwrap();
        let Some(changes) = history.move_forward() else {
            self.history = Some(history);
            self.cursors = Some(cursors);
            return;
        };

        self.apply_and_process_changes(&changes, Some((area, &mut cursors, cfg)));

        self.has_changed = true;
        self.history = Some(history);
        self.cursors = Some(cursors);
    }

    pub fn apply_and_process_changes(
        &mut self,
        changes: &[Change<&str>],
        mut cursors_to_remake: Option<(&impl Area, &mut Cursors, PrintCfg)>,
    ) {
        if let Some((_, cursors, _)) = cursors_to_remake.as_mut() {
            cursors.clear();
        }

        for (i, change) in changes.iter().enumerate() {
            let start = change.start();
            self.replace_range_inner(*change);

            let Some((area, cursors, cfg)) = cursors_to_remake.as_mut() else {
                continue;
            };
            cursors.insert_from_parts(i, start, change.added_text().len(), self, *area, *cfg);
        }
        let mut ts = self.ts_parser.take();
        let mut readers = std::mem::take(&mut self.readers);

        self.process_changes(&mut ts, &mut readers, changes);

        self.ts_parser = ts;
        self.readers = readers;
    }

    /// Finishes the current moment and adds a new one to the history
    pub fn new_moment(&mut self) {
        if let Some(h) = self.history.as_mut() {
            h.new_moment()
        }
    }

    ////////// Writing functions

    /// Clones the inner [`GapBuffer`] as a [`String`]
    ///
    /// This function will also cut out a final '\n' from the string.
    // NOTE: Inherent because I don't want this to implement Display
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let [s0, s1] = self.strs_in_range_inner(..);
        if !s1.is_empty() {
            s0.to_string() + s1.strip_suffix('\n').unwrap_or(s1)
        } else {
            s0.strip_suffix('\n').unwrap_or(s0).to_string()
        }
    }

    /// Writes the contents of this [`Text`] to a [writer]
    ///
    /// [writer]: std::io::Write
    pub fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<usize> {
        self.has_unsaved_changes.store(false, Ordering::Relaxed);
        let (s0, s1) = self.buf.as_slices();
        Ok(writer.write(s0)? + writer.write(s1)?)
    }

    /// Wether or not the content has changed since the last [write]
    ///
    /// Returns `true` only if the actual bytes of the [`Text`] have
    /// been changed, ignoring [`Tag`]s and all the other things,
    /// since those are not written to the filesystem.
    ///
    /// [write]: Text::write_to
    pub fn has_unsaved_changes(&self) -> bool {
        self.has_unsaved_changes.load(Ordering::Relaxed)
    }

    ////////// Reload related functions

    pub(crate) fn take_buf(self) -> GapBuffer<u8> {
        self.buf
    }

    ////////// Single str acquisition functions

    /// Moves the [`GapBuffer`]'s gap, so that the `range` is whole
    ///
    /// The return value is the value of the gap, if the second `&str`
    /// is the contiguous one.
    pub(crate) fn make_contiguous_in(&mut self, range: impl TextRange) {
        let range = range.to_range_fwd(self.len().byte());
        let gap = self.buf.gap();

        if range.end <= gap || range.start >= gap {
            return;
        }

        if gap.abs_diff(range.start) < gap.abs_diff(range.end) {
            self.buf.set_gap(range.start);
        } else {
            self.buf.set_gap(range.end);
        }
    }

    /// Assumes that the `range` given is continuous in `self`
    ///
    /// You *MUST* CALL [`make_contiguous_in`] before using this
    /// function. The sole purpose of this function is to not keep the
    /// [`Text`] mutably borrowed.
    ///
    /// [`make_contiguous_in`]: Self::make_contiguous_in
    pub(crate) unsafe fn continuous_in_unchecked(&self, range: impl TextRange) -> &str {
        let range = range.to_range_fwd(self.len().byte());
        let [s0, s1] = self.strs();
        if range.end <= self.buf.gap() {
            unsafe { s0.get_unchecked(range) }
        } else {
            let gap = self.buf.gap();
            unsafe { s1.get_unchecked(range.start - gap..range.end - gap) }
        }
    }

    ////////// Tag addition/deletion functions

    /// Inserts a [`Tag`] at the given position
    pub fn insert_tag(&mut self, at: usize, tag: Tag, key: Key) {
        self.tags.insert(at, tag, key);
    }

    /// Removes the [`Tag`]s of a [key] from a region
    ///
    /// # Caution
    ///
    /// While it is fine to do this on your own widgets, you should
    /// refrain from using this function in a [`File`]s [`Text`], as
    /// it must iterate over all tags in the file, so if there are a
    /// lot of other tags, this operation may be slow.
    ///
    /// # [`TextRange`] behavior
    ///
    /// If you give it a [`Point`] or [`usize`], it will be treated as
    /// a one byte range.
    ///
    /// [key]: Keys
    /// [`File`]: crate::widgets::File
    pub fn remove_tags_on(&mut self, range: impl TextRange, keys: impl Keys) {
        let range = range.to_range_at(self.len().byte());
        if range.end == range.start + 1 {
            self.tags.remove_at(range.start, keys)
        } else {
            self.tags.remove_from(range, keys)
        }
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`File`]s, as there may be
    /// other [`Tag`] providers, and you should avoid messing with
    /// their tags.
    ///
    /// [`File`]: crate::widgets::File
    pub fn clear_tags(&mut self) {
        self.tags = Box::new(Tags::with_len(self.buf.len()));
    }

    /////////// Cursor functions

    /// Enables the usage of [`Cursors`] in this [`Text`]
    ///
    /// This is automatically done whenever you use the [`EditHelper`]
    /// struct.
    ///
    /// [`EditHelper`]: crate::mode::EditHelper
    pub fn enable_cursors(&mut self) {
        if self.cursors.is_none() {
            self.cursors = Some(Box::default())
        }
    }

    /// Removes the tags for all the cursors, used before they are
    /// expected to move
    pub(crate) fn add_cursors(&mut self, area: &impl Area, cfg: PrintCfg) {
        let Some(cursors) = self.cursors.take() else {
            return;
        };

        if cursors.len() < 500 {
            for (cursor, is_main) in cursors.iter() {
                self.add_cursor(cursor, is_main, &cursors);
            }
        } else {
            let (start, _) = area.first_points(self, cfg);
            let (end, _) = area.last_points(self, cfg);
            for (cursor, is_main) in cursors.iter() {
                let range = cursor.range(cursors.is_incl(), self);
                if range.end > start.byte() && range.start < end.byte() {
                    self.add_cursor(cursor, is_main, &cursors);
                }
            }
        }

        self.cursors = Some(cursors);
    }

    /// Adds the tags for all the cursors, used after they are
    /// expected to have moved
    pub(crate) fn remove_cursors(&mut self, area: &impl Area, cfg: PrintCfg) {
        let Some(cursors) = self.cursors.take() else {
            return;
        };

        if cursors.len() < 500 {
            for (cursor, _) in cursors.iter() {
                self.remove_cursor(cursor, &cursors);
            }
        } else {
            let (start, _) = area.first_points(self, cfg);
            let (end, _) = area.last_points(self, cfg);
            for (cursor, _) in cursors.iter() {
                let range = cursor.range(cursors.is_incl(), self);
                if range.end > start.byte() && range.start < end.byte() {
                    self.remove_cursor(cursor, &cursors);
                }
            }
        }

        self.cursors = Some(cursors)
    }

    pub(crate) fn shift_cursors(
        &mut self,
        from: usize,
        by: (i32, i32, i32),
        area: &impl Area,
        cfg: PrintCfg,
    ) {
        let Some(mut cursors) = self.cursors.take() else {
            return;
        };

        if by != (0, 0, 0) {
            cursors.shift_by(from, by, self, area, cfg);
        }

        self.cursors = Some(cursors);
    }

    /// Adds a [`Cursor`] to the [`Text`]
    fn add_cursor(&mut self, cursor: &Cursor, is_main: bool, cursors: &Cursors) {
        let (start, end) = if let Some(anchor) = cursor.anchor() {
            if anchor <= cursor.caret() {
                // If the caret is at the end, the selection should end before it,
                // so a non inclusive selection it used.
                cursor.point_range(false, self)
            } else {
                cursor.point_range(cursors.is_incl(), self)
            }
        } else {
            cursor.point_range(false, self)
        };
        let no_selection = if start == end { 1 } else { 3 };

        let tags = cursor_tags(is_main)
            .into_iter()
            .zip([cursor.caret(), end, start]);
        for (tag, p) in tags.take(no_selection) {
            let record = [p.byte(), p.char(), p.line()];
            self.records.insert(record);
            self.tags.insert(p.byte(), tag, Key::for_cursors());
        }
    }

    /// Removes a [`Cursor`] from the [`Text`]
    fn remove_cursor(&mut self, cursor: &Cursor, cursors: &Cursors) {
        let (start, end) = if let Some(anchor) = cursor.anchor()
            && anchor < cursor.caret()
        {
            // If the caret is at the end, the selection should end before it,
            // so a non inclusive selection it used.
            cursor.point_range(false, self)
        } else {
            cursor.point_range(cursors.is_incl(), self)
        };
        let skip = if start == end { 1 } else { 0 };

        for p in [start, end].into_iter().skip(skip) {
            self.tags.remove_at(p.byte(), Key::for_cursors());
        }
    }

    /////////// Iterator methods

    /// A forward iterator of the [chars and tags] of the [`Text`]
    ///
    /// [chars and tags]: Part
    pub fn iter_fwd(&self, at: impl TwoPoints) -> Iter<'_> {
        Iter::new_at(self, at)
    }

    /// A reverse iterator of the [chars and tags] of the [`Text`]
    ///
    /// [chars and tags]: Part
    pub fn iter_rev(&self, at: impl TwoPoints) -> RevIter<'_> {
        RevIter::new_at(self, at)
    }

    /// A forward iterator of the [`char`]s of the [`Text`]
    ///
    /// Each [`char`] will be accompanied by a [`Point`], which is the
    /// position where said character starts, e.g.
    /// [`Point::default()`] for the first character
    pub fn chars_fwd(&self, p: Point) -> impl Iterator<Item = (Point, char)> + '_ {
        self.strs_in_range_inner(p.byte()..)
            .into_iter()
            .flat_map(str::chars)
            .scan(p, |p, char| {
                let old_p = *p;
                *p = p.fwd(char);
                Some((old_p, char))
            })
    }

    /// A reverse iterator of the [`char`]s of the [`Text`]
    ///
    /// Each [`char`] will be accompanied by a [`Point`], which is the
    /// position where said character starts, e.g.
    /// [`Point::default()`] for the first character
    pub fn chars_rev(&self, p: Point) -> impl Iterator<Item = (Point, char)> + '_ {
        self.strs_in_range_inner(..p.byte())
            .into_iter()
            .flat_map(str::chars)
            .rev()
            .scan(p, |p, char| {
                *p = p.rev(char);
                Some((*p, char))
            })
    }

    /// A forward iterator over the [`Tag`]s of the [`Text`]
    ///
    /// # Note
    ///
    /// Duat works fine with [`Tag`]s in the middle of a codepoint,
    /// but external utilizers may not, so keep that in mind.
    pub fn tags_fwd(&self, at: usize) -> FwdTags {
        self.tags.fwd_at(at)
    }

    /// An reverse iterator over the [`Tag`]s of the [`Text`]
    ///
    /// # Note
    ///
    /// Duat works fine with [`Tag`]s in the middle of a codepoint,
    /// but external utilizers may not, so keep that in mind.
    pub fn tags_rev(&self, at: usize) -> RevTags {
        self.tags.rev_at(at)
    }

    /// The [`Cursors`] printed to this [`Text`], if they exist
    pub fn cursors(&self) -> Option<&Cursors> {
        self.cursors.as_ref().map(|b| b.as_ref())
    }

    /// A mut reference to this [`Text`]'s [`Cursors`] if they exist
    pub fn cursors_mut(&mut self) -> Option<&mut Cursors> {
        self.cursors.as_mut().map(|b| b.as_mut())
    }

    pub(crate) fn history(&self) -> Option<&History> {
        self.history.as_deref()
    }
}

impl std::fmt::Debug for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Text")
            .field_with("buf", |f| {
                write!(f, "'{}', '{}'", self.strs()[0], self.strs()[1])
            })
            .field("tags", &self.tags)
            .field("records", &self.records)
            .finish()
    }
}

impl Clone for Text {
    fn clone(&self) -> Self {
        Self {
            buf: self.buf.clone(),
            tags: self.tags.clone(),
            cursors: self.cursors.clone(),
            records: self.records.clone(),
            history: self.history.clone(),
            readers: Vec::new(),
            ts_parser: None,
            forced_new_line: self.forced_new_line,
            has_changed: self.has_changed,
            has_unsaved_changes: AtomicBool::new(false),
        }
    }
}

impl<E> From<E> for Text
where
    E: DuatError,
{
    fn from(value: E) -> Self {
        *value.into_text()
    }
}

impl From<std::io::Error> for Text {
    fn from(value: std::io::Error) -> Self {
        err!({ value.kind().to_string() })
    }
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        self.buf == other.buf && self.tags == other.tags
    }
}
impl Eq for Text {}

mod point {}

mod part {}

/// A list of [`Tag`]s to be added with a [`Cursor`]
fn cursor_tags(is_main: bool) -> [Tag; 3] {
    use tags::Tag::{ExtraCursor, MainCursor, PopForm, PushForm};

    use crate::form::{E_SEL_ID, M_SEL_ID};

    if is_main {
        [MainCursor, PopForm(M_SEL_ID), PushForm(M_SEL_ID)]
    } else {
        [ExtraCursor, PopForm(E_SEL_ID), PushForm(E_SEL_ID)]
    }
}

/// Splits a range within a region
///
/// The first return is the part of `within` that must be updated.
/// The second return is what is left of `range`.
///
/// If `range` is fully inside `within`, remove `range`;
/// If `within` is fully inside `range`, split `range` in 2;
/// If `within` intersects `range` in one side, chop it off;
fn split_range_within(
    range: Range<usize>,
    within: Range<usize>,
) -> (Option<Range<usize>>, [Option<Range<usize>>; 2]) {
    if range.start >= within.end || within.start >= range.end {
        (None, [Some(range), None])
    } else {
        let start_range = (within.start > range.start).then_some(range.start..within.start);
        let end_range = (range.end > within.end).then_some(within.end..range.end);
        let split_ranges = [start_range, end_range];
        let range_to_check = range.start.max(within.start)..(range.end.min(within.end));
        (Some(range_to_check), split_ranges)
    }
}

/// Merges a new range in a sorted list of ranges
///
/// Since ranges are not allowed to intersect, they will be sorted
/// both in their starting bound and in their ending bound.
fn merge_range_in(ranges: &mut Vec<Range<usize>>, range: Range<usize>) {
    let (r_range, start) = match ranges.binary_search_by_key(&range.start, |r| r.start) {
        // Same thing here
        Ok(i) => (i..i + 1, range.start),
        Err(i) => {
            // This is if we intersect the added part
            if let Some(older_i) = i.checked_sub(1)
                && range.start <= ranges[older_i].end
            {
                (older_i..i, ranges[older_i].start)
            // And here is if we intersect nothing on the
            // start, no changes drained.
            } else {
                (i..i, range.start)
            }
        }
    };
    let start_i = r_range.end;
    // Otherwise search ahead for another change to be merged
    let (r_range, end) = match ranges[start_i..].binary_search_by_key(&range.end, |r| r.start) {
        Ok(i) => (r_range.start..start_i + i + 1, ranges[start_i + i].end),
        Err(i) => {
            if let Some(older) = ranges.get(start_i + i)
                && older.start <= range.end
            {
                (r_range.start..start_i + i + 1, range.end.max(older.end))
            } else {
                (r_range.start..start_i + i, range.end)
            }
        }
    };

    ranges.splice(r_range, [start..end]);
}

impl From<&std::path::PathBuf> for Text {
    fn from(value: &std::path::PathBuf) -> Self {
        let value = value.to_str().unwrap_or("");
        Self::from(value)
    }
}

impl_from_to_string!(u8);
impl_from_to_string!(u16);
impl_from_to_string!(u32);
impl_from_to_string!(u64);
impl_from_to_string!(u128);
impl_from_to_string!(usize);
impl_from_to_string!(i8);
impl_from_to_string!(i16);
impl_from_to_string!(i32);
impl_from_to_string!(i64);
impl_from_to_string!(i128);
impl_from_to_string!(isize);
impl_from_to_string!(f32);
impl_from_to_string!(f64);
impl_from_to_string!(char);
impl_from_to_string!(&str);
impl_from_to_string!(String);
impl_from_to_string!(Box<str>);
impl_from_to_string!(Rc<str>);
impl_from_to_string!(Arc<str>);

/// Implements [`From<$t>`] for [`Text`]
macro impl_from_to_string($t:ty) {
    impl From<$t> for Text {
        fn from(value: $t) -> Self {
            let value = <$t as ToString>::to_string(&value);
            let buf = GapBuffer::from_iter(value.bytes());
            Self::from_buf(buf, None, false)
        }
    }
}

pub struct TextLines<'a> {
    lines: std::str::Lines<'a>,
    fwd_i: usize,
    rev_i: usize,
}

impl<'a> Iterator for TextLines<'a> {
    type Item = (usize, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        self.lines.next().map(|line| {
            self.fwd_i += 1;
            (self.fwd_i - 1, line)
        })
    }
}

impl DoubleEndedIterator for TextLines<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.lines.next_back().map(|line| {
            self.rev_i -= 1;
            (self.rev_i, line)
        })
    }
}
