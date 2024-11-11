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
//! [colored]: crate::forms::Form
//! [ghost text]: Tag::GhostText
//! [`Ui`]: crate::ui::Ui
//! [`File`]: crate::widgets::File
//! [`Widget`]: crate::widgets::Widget
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`Mode`]: crate::mode::Mode
//! [`EditHelper`]: crate::mode::EditHelper
mod builder;
mod cfg;
mod history;
mod iter;
mod part;
pub mod reader;
mod records;
mod search;
mod tags;

use std::{ops::RangeBounds, path::Path, rc::Rc, str::from_utf8_unchecked, sync::Arc};

use gapbuf::GapBuffer;
use history::History;
use records::Records;
use tags::{FwdTags, RevTags};

use self::tags::Tags;
pub use self::{
    builder::{AlignCenter, AlignLeft, AlignRight, Builder, Ghost, err, hint, ok, text},
    cfg::*,
    history::Change,
    iter::{Item, Iter, RevIter},
    part::Part,
    point::{Point, TwoPoints, utf8_char_width},
    search::{RegexPattern, Searcher},
    tags::{Key, Keys, Tag, ToggleId},
};
use crate::{
    DuatError,
    mode::{Cursor, Cursors},
    ui::Area,
};

/// The text in a given [`Area`]
#[derive(Default, Clone)]
pub struct Text {
    buf: Box<GapBuffer<u8>>,
    tags: Box<Tags>,
    records: Box<Records<(u32, u32, u32)>>,
    history: History,
}

impl Text {
    ////////// Creation of Text

    /// Returns a new, empty [`Text`]
    pub fn new() -> Self {
        Self {
            buf: Box::new(GapBuffer::new()),
            tags: Box::new(Tags::new()),
            records: Box::new(Records::new()),
            history: History::new(),
        }
    }

    /// Creates a [`Text`] from a file's [path]
    ///
    /// [path]: Path
    pub fn from_file(path: impl AsRef<Path>) -> Self {
        let file = std::fs::read_to_string(path).expect("File failed to open");
        let buf = Box::new(GapBuffer::from_iter(file.bytes()));
        let tags = Box::new(Tags::with_len(buf.len() as u32));

        Self {
            buf,
            tags,
            records: Box::new(Records::with_max((
                file.len() as u32,
                file.chars().count() as u32,
                file.bytes().filter(|b| *b == b'\n').count() as u32,
            ))),
            history: History::new(),
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
        let (b, c, l) = self.records.max();
        Point::from_raw(b, c, l)
    }

    /// Wether or not there are any characters in the [`Text`]
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
        if point.byte() < s0.len() as u32 {
            s0[point.byte() as usize..].chars().next()
        } else {
            s1[point.byte() as usize - s0.len()..].chars().next()
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
    /// [`strs_in_range`]
    ///
    /// [`&str`]: str
    /// [buffer]: GapBuffer
    /// [`flat_map`]: Iterator::flat_map
    /// [`strs_in_range`]: Self::strs_in_range
    pub fn strs(&self) -> [&'_ str; 2] {
        let (s0, s1) = self.buf.as_slices();
        unsafe { [from_utf8_unchecked(s0), from_utf8_unchecked(s1)] }
    }

    /// This method will return two [`&str`]s at the [`Point`] range
    ///
    /// This function treats any [`Point`]s outside the range as if
    /// they where the last point in the text.
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
    /// text.strs_in_range((p1, p2))
    ///     .into_iter()
    ///     .flat_map(str::bytes);
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
    /// If you want the two full [`&str`]s, see [`strs`]
    ///
    /// [`&str`]: str
    /// [`strs`]: Self::strs
    pub fn strs_in_range(&self, (p1, p2): (Point, Point)) -> [&str; 2] {
        self.strs_in_range_inner(p1.byte()..p2.byte())
    }

    /// Returns the two `&str`s in the byte range.
    fn strs_in_range_inner(&self, range: impl RangeBounds<u32>) -> [&str; 2] {
        let (s0, s1) = self.buf.as_slices();
        let (start, end) = get_ends(range, self.len().byte());
        let (start, end) = (start as usize, end as usize);

        unsafe {
            let r0 = start.min(s0.len())..end.min(s0.len());
            let r1 = start.saturating_sub(s0.len()).min(s1.len())
                ..end.saturating_sub(s0.len()).min(s1.len());

            [from_utf8_unchecked(&s0[r0]), from_utf8_unchecked(&s1[r1])]
        }
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
    /// Will panic if `at` is greater than the lenght of the text
    #[inline(always)]
    pub fn point_at(&self, at: u32) -> Point {
        assert!(
            at <= self.len().byte(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len().byte()
        );
        let (b, c, mut l) = self.records.closest_to(at);

        let found = if at >= b {
            let [s0, s1] = self.strs_in_range_inner(b..);

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    l += (char == '\n') as u32;
                    (b + this_b as u32, c + i as u32, l - (char == '\n') as u32)
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
                    l -= (char == '\n') as u32;
                    c_len += char.len_utf8() as u32;
                    (b - c_len, c - (i as u32 + 1), l)
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
    pub fn point_at_char(&self, at: u32) -> Point {
        assert!(
            at <= self.len().char(),
            "byte out of bounds: the len is {}, but the char is {at}",
            self.len().char()
        );
        let (b, c, mut l) = self.records.closest_to_by(at, |(_, c, _)| *c);

        let found = if at >= c {
            let [s0, s1] = self.strs_in_range_inner(b..);

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    l += (char == '\n') as u32;
                    (b + this_b as u32, c + i as u32, l - (char == '\n') as u32)
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
                    l -= (char == '\n') as u32;
                    c_len += char.len_utf8() as u32;
                    (b - c_len, c - (i as u32 + 1), l)
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
    pub fn point_at_line(&self, at: u32) -> Point {
        assert!(
            at <= self.len().line(),
            "byte out of bounds: the len is {}, but the line is {at}",
            self.len().line()
        );
        let (b, c, mut l) = {
            let (mut b, mut c, l) = self.records.closest_to_by(at, |(.., l)| *l);
            self.strs_in_range_inner(..b)
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .take_while(|c| *c != '\n')
                .for_each(|char| {
                    b -= char.len_utf8() as u32;
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
                    l += (char == '\n') as u32;
                    (b + this_b as u32, c + i as u32, l - (char == '\n') as u32)
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
                    l -= (char == '\n') as u32;
                    c_len += char.len_utf8() as u32;
                    (b - c_len, c - (i as u32 + 1), l)
                })
                .take_while(|&(.., l)| l >= at)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
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
    pub fn ghost_max_points_at(&self, at: u32) -> (Point, Option<Point>) {
        let point = self.point_at(at);
        (point, self.tags.ghosts_total_at(point.byte()))
    }

    /// Points visually after the [`TwoPoints`]
    ///
    /// If the [`TwoPoints`] in question is concealed, treats the
    /// next visible character as the first character, and returns
    /// the points of the next visible character.
    ///
    /// This method is useful if you want to iterater reversibly
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
    /// [`Tag`]s create ghost text or ommit text from multiple
    /// different lines, this point may differ from where in the
    /// [`Text`] the physical line actually begins.
    pub fn visual_line_start(&self, p: impl TwoPoints) -> (Point, Option<Point>) {
        let (real, ghost) = p.to_points();

        // NOTE: 20000 is a magic number, being a guess for what a reasonable
        // limit would be.
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

    pub fn replace_range(&mut self, range: (Point, Point), edit: impl ToString) {
        self.replace_range_inner(range, edit.to_string());
        self.history
            .add_change(None, Change::new(edit, range, self));
    }

    pub(crate) unsafe fn apply_desync_change(
        &mut self,
        guess_i: usize,
        change: Change,
        shift: (i32, i32, i32),
        sh_from: usize,
    ) -> (usize, i32) {
        let range = (change.start(), change.taken_end());
        self.replace_range_inner(range, change.added_text());
        self.history
            .add_desync_change(guess_i, change, shift, sh_from)
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace
    fn replace_range_inner(&mut self, (start, end): (Point, Point), edit: impl ToString) {
        let edit = edit.to_string();

        let new_len = {
            let lines = edit.bytes().filter(|b| *b == b'\n').count();
            (edit.len() as u32, edit.chars().count() as u32, lines as u32)
        };

        let old_len = unsafe {
            let range = start.byte() as usize..end.byte() as usize;
            let str = String::from_utf8_unchecked(
                self.buf
                    .splice(range, edit.as_bytes().iter().cloned())
                    .collect(),
            );

            let lines = str.bytes().filter(|b| *b == b'\n').count();
            (str.len() as u32, str.chars().count() as u32, lines as u32)
        };

        let start_rec = (start.byte(), start.char(), start.line());
        self.records.transform(start_rec, old_len, new_len);
        self.records.insert(start_rec);

        let new_end = start.byte() + edit.len() as u32;
        self.tags.transform(start.byte()..end.byte(), new_end);
    }

    ////////// History manipulation functions

    /// Undoes the last moment, if there was one
    pub fn undo(&mut self, area: &impl Area, cursors: &mut Cursors, cfg: PrintCfg) {
        let mut history = std::mem::take(&mut self.history);
        let Some(moment) = history.move_backwards() else {
            return;
        };

        cursors.clear();

        let mut shift = (0, 0, 0);

        for (i, change) in moment.iter().enumerate() {
            let start = change.start().shift_by(shift);
            let end = change.added_end().shift_by(shift);
            self.replace_range_inner((start, end), change.taken_text());

            cursors.insert_from_parts(i, start, change.taken_text().len(), self, area, cfg);

            shift.0 += change.taken_end().byte() as i32 - change.added_end().byte() as i32;
            shift.1 += change.taken_end().char() as i32 - change.added_end().char() as i32;
            shift.2 += change.taken_end().line() as i32 - change.added_end().line() as i32;
        }

        self.history = history;
    }

    /// Redoes the last moment in the history, if there is one
    pub fn redo(&mut self, area: &impl Area, cursors: &mut Cursors, cfg: PrintCfg) {
        let mut history = std::mem::take(&mut self.history);
        let Some(moment) = history.move_forward() else {
            return;
        };

        cursors.clear();

        for (i, change) in moment.iter().enumerate() {
            let start = change.start();
            let end = change.taken_end();
            self.replace_range_inner((start, end), change.added_text());

            cursors.insert_from_parts(i, start, change.added_text().len(), self, area, cfg);
        }

        self.history = history;
    }

    /// Finishes the current moment and adds a new one to the history
    pub fn new_moment(&mut self) {
        self.history.new_moment();
    }

    pub(crate) fn changes_mut(&mut self) -> &mut [Change] {
        self.history.changes_mut()
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
        let (s0, s1) = self.buf.as_slices();
        Ok(writer.write(s0)? + writer.write(s1)?)
    }

    ////////// Single str acquisition functions

    /// Moves the [`GapBuffer`]'s gap, so that the `range` is whole
    ///
    /// The return value is the value of the gap, if the second `&str`
    /// is the contiguous one.
    pub(crate) fn make_contiguous_in(&mut self, range: impl RangeBounds<u32>) {
        let (start, end) = get_ends(range, self.len().byte());
        let gap = self.buf.gap() as u32;

        if end <= gap || start >= gap {
            return;
        }

        if gap.abs_diff(start) < gap.abs_diff(end) {
            self.buf.set_gap(start as usize);
        } else {
            self.buf.set_gap(end as usize);
        }
    }

    /// Assumes that the `range` given is continuous in `self`
    ///
    /// You *MUST* CALL [`make_contiguous_in`] before using this
    /// function. The sole purpose of this function is not to keep the
    /// [`Text`] mutably borrowed.
    ///
    /// [`make_contiguous_in`]: Self::make_contiguous_in
    pub(crate) unsafe fn continuous_in_unchecked(&self, range: impl RangeBounds<u32>) -> &str {
        let (start, end) = get_ends(range, self.len().byte());
        let [s0, s1] = self.strs();
        unsafe {
            if end as usize <= self.buf.gap() {
                s0.get_unchecked(start as usize..end as usize)
            } else {
                let gap = self.buf.gap();
                s1.get_unchecked(start as usize - gap..end as usize - gap)
            }
        }
    }

    ////////// Tag addition/deletion functions

    /// Inserts a [`Tag`] at the given position
    pub fn insert_tag(&mut self, at: u32, tag: Tag, key: Key) {
        self.tags.insert(at, tag, key);
    }

    /// Removes all the [`Tag`]s from a position related to a [key]
    ///
    /// [key]: Keys
    pub fn remove_tags_on(&mut self, at: u32, keys: impl Keys) {
        self.tags.remove_at(at, keys)
    }

    /// Removes the [`Tag`]s of a [key] from the whole [`Text`]
    ///
    /// # Caution
    ///
    /// While it is fine to do this on your own widgets, you should
    /// refrain from using this function in a [`File`]s [`Text`], as
    /// it must iterate over all tags in the file, so if there are a
    /// lot of other tags, this operation may be slow.
    ///
    /// [key]: Keys
    /// [`File`]: crate::widgets::File
    pub fn remove_tags_of(&mut self, keys: impl Keys) {
        self.tags.remove_of(keys)
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`File`]s, as there may be
    /// other [`Tag`] providers, and you should avoid messing with
    /// their tags.
    ///
    /// [`File`]: crate::widgets::File
    pub fn clear_tags(&mut self) {
        self.tags = Box::new(Tags::with_len(self.buf.len() as u32));
    }

    /// Removes the tags for all the cursors, used before they are
    /// expected to move
    pub(crate) fn add_cursors(&mut self, cursors: &Cursors, area: &impl Area, cfg: PrintCfg) {
        if cursors.len() < 500 {
            for (cursor, is_main) in cursors.iter() {
                self.add_cursor(cursor, is_main, cursors);
            }
        } else {
            let start = area.first_point(self, cfg);
            let end = area.last_point(self, cfg);
            for (cursor, is_main) in cursors.iter() {
                let range = cursor.range(cursors.is_incl());
                if range.end > start.byte() && range.start < end.byte() {
                    self.add_cursor(cursor, is_main, cursors);
                }
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are
    /// expected to have moved
    pub(crate) fn remove_cursors(&mut self, cursors: &Cursors, area: &impl Area, cfg: PrintCfg) {
        if cursors.len() < 500 {
            for (cursor, _) in cursors.iter() {
                self.remove_cursor(cursor, cursors);
            }
        } else {
            let start = area.first_point(self, cfg);
            let end = area.last_point(self, cfg);
            for (cursor, _) in cursors.iter() {
                let range = cursor.range(cursors.is_incl());
                if range.end > start.byte() && range.start < end.byte() {
                    self.remove_cursor(cursor, cursors);
                }
            }
        }
    }

    /// Adds a [`Cursor`] to the [`Text`]
    fn add_cursor(&mut self, cursor: &Cursor, is_main: bool, cursors: &Cursors) {
        let (start, end) = if let Some(anchor) = cursor.anchor()
            && anchor < cursor.caret()
        {
            // If the caret is at the end, the selection should end before it,
            // so a non inclusive selection it used.
            cursor.point_range(false, self)
        } else {
            cursor.point_range(cursors.is_incl(), self)
        };
        let (caret_tag, start_tag, end_tag) = cursor_tags(is_main);

        let no_selection = if start == end { 2 } else { 0 };

        let tags = [
            (start, start_tag),
            (end, end_tag),
            (cursor.caret(), caret_tag),
        ];

        for (p, tag) in tags.into_iter().skip(no_selection) {
            let record = (p.byte(), p.char(), p.line());
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

    /// An forward iterator over the [`Tag`]s of the [`Text`]
    ///
    /// # Note
    ///
    /// Duat works fine with [`Tag`]s in the middle of a codepoint,
    /// but external utilizers may not, so keep that in mind.
    pub fn tags_fwd(&self, at: u32) -> FwdTags {
        self.tags.fwd_at(at)
    }

    /// An reverse iterator over the [`Tag`]s of the [`Text`]
    ///
    /// # Note
    ///
    /// Duat works fine with [`Tag`]s in the middle of a codepoint,
    /// but external utilizers may not, so keep that in mind.
    pub fn tags_rev(&self, at: u32) -> RevTags {
        self.tags.rev_at(at)
    }
}

impl std::fmt::Debug for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Text")
            .field(
                "buf",
                &format!("'{}', '{}'", self.strs()[0], self.strs()[1]),
            )
            .field("tags", &self.tags)
            .field("records", &self.records)
            .finish()
    }
}

impl<E> From<E> for Text
where
    E: DuatError,
{
    fn from(value: E) -> Self {
        value.into_text()
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

mod point {
    //! A [`Point`] is a position in [`Text`]
    //!
    //! [`Text`]: super::Text
    use serde::{Deserialize, Serialize};

    use super::Item;

    /// A position in [`Text`]
    ///
    /// [`Text`]: super::Text
    #[derive(
        Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
    )]
    pub struct Point {
        b: u32,
        c: u32,
        l: u32,
    }

    impl Point {
        ////////// Creation of a Point

        /// Returns a new [`Point`], at the first byte
        pub fn new() -> Self {
            Self::default()
        }

        /// Internal function to create [`Point`]s
        pub(super) fn from_raw(b: u32, c: u32, l: u32) -> Self {
            Self { b, c, l }
        }

        ////////// Querying functions

        /// The len [`Point`] of a [`&str`]
        ///
        /// This is the equivalent of [`Text::len`], but for types
        /// other than [`Text`]
        ///
        /// [`&str`]: str
        /// [`Text::len`]: super::Text::len
        /// [`Text`]: super::Text
        pub fn len_of(str: impl AsRef<str>) -> Self {
            let str = str.as_ref();
            Self {
                b: str.len() as u32,
                c: str.chars().count() as u32,
                l: str.bytes().filter(|c| *c == b'\n').count() as u32,
            }
        }

        /// Returns the byte (relative to the beginning of the file)
        /// of self. Indexed at 0
        pub fn byte(&self) -> u32 {
            self.b
        }

        /// Returns the char index (relative to the beginning of the
        /// file). Indexed at 0
        pub fn char(&self) -> u32 {
            self.c
        }

        /// Returns the line. Indexed at 0
        pub fn line(&self) -> u32 {
            self.l
        }

        ////////// Shifting functions

        /// Moves a [`Point`] forward by one character
        pub(crate) fn fwd(self, char: char) -> Self {
            Self {
                b: self.b + char.len_utf8() as u32,
                c: self.c + 1,
                l: self.l + (char == '\n') as u32,
            }
        }

        /// Moves a [`Point`] in reverse by one character
        pub(crate) fn rev(self, char: char) -> Self {
            Self {
                b: self.b - char.len_utf8() as u32,
                c: self.c - 1,
                l: self.l - (char == '\n') as u32,
            }
        }

        /// Moves a [`Point`] forward by one byte
        pub(super) fn fwd_byte(self, byte: u8) -> Self {
            Self {
                b: self.b + 1,
                c: self.c + utf8_char_width(byte),
                l: self.l + (byte == b'\n') as u32,
            }
        }

        /// Moves a [`Point`] in reverse by one byte
        pub(super) fn rev_byte(self, byte: u8) -> Self {
            Self {
                b: self.b - 1,
                c: self.c - utf8_char_width(byte),
                l: self.l - (byte == b'\n') as u32,
            }
        }

        /// Shifts the [`Point`] by a "signed point"
        ///
        /// This assumes that no overflow is going to happen
        pub(crate) fn shift_by(self, (b, c, l): (i32, i32, i32)) -> Self {
            Self {
                b: (self.b as i32 + b) as u32,
                c: (self.c as i32 + c) as u32,
                l: (self.l as i32 + l) as u32,
            }
        }
    }

    /// Two positions, one for the [`Text`], and one for [ghost text]
    ///
    /// This can either be a [`Point`] or `(Point, Option<Point>)` or
    /// even `(Point, Point)`. If a second [`Point`] is excluded, it
    /// is assumed to be [`Point::default()`], i.e., this
    /// [`TwoPoints`] represents the beginning of a [ghost text].
    ///
    /// [`Text`]: super::Text
    /// [ghost text]: super::Tag::GhostText
    pub trait TwoPoints: std::fmt::Debug + Clone + Copy {
        /// Returns two [`Point`]s, for `Text` and ghosts
        fn to_points(self) -> (Point, Option<Point>);
    }

    impl TwoPoints for Point {
        fn to_points(self) -> (Point, Option<Point>) {
            (self, None)
        }
    }

    impl TwoPoints for (Point, Point) {
        fn to_points(self) -> (Point, Option<Point>) {
            (self.0, Some(self.1))
        }
    }

    impl TwoPoints for (Point, Option<Point>) {
        fn to_points(self) -> (Point, Option<Point>) {
            self
        }
    }

    impl TwoPoints for Item {
        fn to_points(self) -> (Point, Option<Point>) {
            (self.real, self.ghost)
        }
    }

    impl std::fmt::Display for Point {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}, {}, {}", self.b, self.c, self.l)
        }
    }

    impl std::ops::Add for Point {
        type Output = Self;

        fn add(self, rhs: Self) -> Self::Output {
            Self {
                b: self.b + rhs.b,
                c: self.c + rhs.c,
                l: self.l + rhs.l,
            }
        }
    }

    impl std::ops::AddAssign for Point {
        fn add_assign(&mut self, rhs: Self) {
            *self = *self + rhs;
        }
    }

    impl std::ops::Sub for Point {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self::Output {
            Self {
                b: self.b - rhs.b,
                c: self.c - rhs.c,
                l: self.l - rhs.l,
            }
        }
    }

    impl std::ops::SubAssign for Point {
        fn sub_assign(&mut self, rhs: Self) {
            *self = *self - rhs;
        }
    }

    // https://tools.ietf.org/html/rfc3629
    const UTF8_CHAR_WIDTH: &[u8; 256] = &[
        // 1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 1
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 2
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 3
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 4
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 5
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 6
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 7
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 8
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 9
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // A
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // B
        0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // D
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // E
        4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // F
    ];

    /// Given a first byte, determines how many bytes are in this
    /// UTF-8 character
    #[inline]
    pub const fn utf8_char_width(b: u8) -> u32 {
        UTF8_CHAR_WIDTH[b as usize] as u32
    }
}

/// A list of [`Tag`]s to be added with a [`Cursor`]
fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    use tags::Tag::{ExtraCursor, MainCursor, PopForm, PushForm};

    use crate::forms::{E_SEL_ID, M_SEL_ID};

    if is_main {
        (MainCursor, PushForm(M_SEL_ID), PopForm(M_SEL_ID))
    } else {
        (ExtraCursor, PushForm(E_SEL_ID), PopForm(E_SEL_ID))
    }
}

/// Convenience function for the bounds of a range
fn get_ends(range: impl std::ops::RangeBounds<u32>, max: u32) -> (u32, u32) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => (*end + 1).min(max),
        std::ops::Bound::Excluded(end) => (*end).min(max),
        std::ops::Bound::Unbounded => max,
    };

    (start, end)
}

impl_from_to_string!(u8);
impl_from_to_string!(i8);
impl_from_to_string!(u16);
impl_from_to_string!(i16);
impl_from_to_string!(u32);
impl_from_to_string!(i32);
impl_from_to_string!(u64);
impl_from_to_string!(i64);
impl_from_to_string!(u128);
impl_from_to_string!(i128);
impl_from_to_string!(usize);
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
            let buf = Box::new(GapBuffer::from_iter(value.bytes()));
            let tags = Box::new(Tags::with_len(buf.len() as u32));

            Self {
                buf,
                tags,
                records: Box::new(Records::with_max((
                    value.len() as u32,
                    value.chars().count() as u32,
                    value.lines().count() as u32,
                ))),
                history: History::new(),
            }
        }
    }
}
