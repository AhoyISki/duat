mod builder;
mod cfg;
mod iter;
pub mod reader;
mod records;
mod search;
mod tags;
mod types;

use std::{
    ops::{Range, RangeBounds},
    path::Path,
    rc::Rc,
    str::from_utf8_unchecked,
    sync::Arc,
};

use gapbuf::GapBuffer;
use point::TwoPoints;
use records::Records;

use self::tags::{Markers, RawTag, Tags};
pub use self::{
    builder::{err, hint, ok, text, AlignCenter, AlignLeft, AlignRight, Builder, Ghost},
    cfg::*,
    iter::{Item, Iter, RevIter},
    point::{utf8_char_width, Point},
    search::{CharSet, Pattern, Searcher},
    tags::{Marker, Tag, ToggleId},
    types::Part,
};
use crate::{history::Change, input::Cursors, DuatError};

/// The text in a given area.
#[derive(Default, Clone, Eq)]
pub struct Text {
    buf: Box<GapBuffer<u8>>,
    tags: Box<Tags>,
    /// This [`Marker`] is used for the addition and removal of cursor
    /// [`Tag`]s.
    marker: Marker,
    pub records: Records<(usize, usize, usize)>,
}

impl Text {
    pub fn new() -> Self {
        Self {
            buf: Box::new(GapBuffer::new()),
            tags: Box::new(Tags::new()),
            marker: Marker::base(),
            records: Records::new(),
        }
    }

    pub fn from_file(path: impl AsRef<Path>) -> Self {
        let file = std::fs::read_to_string(path).expect("File failed to open");
        let buf = Box::new(GapBuffer::from_iter(file.bytes()));
        let tags = Box::new(Tags::with_len(buf.len()));

        Self {
            buf,
            tags,
            marker: Marker::base(),
            records: Records::with_max((
                file.len(),
                file.chars().count(),
                file.bytes().filter(|b| *b == b'\n').count(),
            )),
        }
    }

    pub fn builder() -> Builder {
        Builder::new()
    }

    pub fn insert_tag(&mut self, at: usize, tag: Tag, marker: Marker) {
        self.tags.insert(at, tag, marker);
    }

    pub fn remove_tags_on(&mut self, b: usize, markers: impl Markers) {
        self.tags.remove_at(b, markers)
    }

    pub fn clear_tags(&mut self) {
        self.tags = Box::new(Tags::with_len(self.buf.len()));
    }

    pub(crate) fn insert_str(&mut self, at: usize, str: &str) {
        self.replace_range(at..at, str);
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.replace_range(change.taken_range(), &change.added_text);
    }

    pub(crate) fn undo_change(&mut self, change: &Change, bytes: isize) {
        let start = change.start.saturating_add_signed(bytes);
        let end = change.added_end().saturating_add_signed(bytes);
        self.replace_range(start..end, &change.taken_text);
    }

    /// Removes the tags for all the cursors, used before they are
    /// expected to move.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &Cursors) {
        for (cursor, is_main) in cursors.iter() {
            let (start, end) = cursor.point_range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(is_main);

            let no_selection = if start == end { 2 } else { 0 };

            let end_byte = if let Some(anchor) = cursor.anchor()
                && anchor > start
                && cursor.is_inclusive()
            {
                end.byte() + 1
            } else {
                end.byte()
            };

            let tags = [
                (start.byte(), start_tag),
                (end_byte, end_tag),
                (cursor.caret().byte(), caret_tag),
            ];

            for (b, tag) in tags.into_iter().skip(no_selection) {
                let point = self.point_at(b);
                let record = (point.byte(), point.char(), point.line());
                self.records.insert(record);
                self.tags.insert(b, tag, self.marker);
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are
    /// expected to have moved.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &Cursors) {
        for (cursor, _) in cursors.iter() {
            let (start, end) = cursor.point_range();
            let skip = if start == end { 1 } else { 0 };

            let end_byte = if let Some(anchor) = cursor.anchor()
                && anchor > start
                && cursor.is_inclusive()
            {
                end.byte() + 1
            } else {
                end.byte()
            };

            for ch_index in [start.byte(), end_byte].into_iter().skip(skip) {
                self.tags.remove_at(ch_index, self.marker);
            }
        }
    }

    pub(crate) fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<usize> {
        let (s0, s1) = self.buf.as_slices();
        Ok(writer.write(s0)? + writer.write(s1)?)
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();

        let old_start = {
            let p = self.point_at(old.start);
            (p.byte(), p.char(), p.line())
        };

        let new_len = {
            let lines = edit.bytes().filter(|b| *b == b'\n').count();
            (edit.len(), edit.chars().count(), lines)
        };

        let old_len = unsafe {
            let str = String::from_utf8_unchecked(
                self.buf
                    .splice(old.clone(), edit.as_bytes().iter().cloned())
                    .collect(),
            );

            let lines = str.bytes().filter(|b| *b == b'\n').count();
            (str.len(), str.chars().count(), lines)
        };

        self.records.transform(old_start, old_len, new_len);

        let new_end = old.start + edit.len();
        self.tags.transform(old, new_end);
    }

    pub fn slices(&self) -> (&'_ str, &'_ str) {
        let (s0, s1) = self.buf.as_slices();
        unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) }
    }

    pub fn tags(&self) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        self.tags.iter_at(0)
    }

    pub fn tags_at(&self, at: usize) -> impl Iterator<Item = (usize, RawTag)> + Clone + '_ {
        self.tags.iter_at(at)
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + Clone + '_ {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars().chain(s1.chars())
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    #[inline(always)]
    pub fn point_at(&self, at: usize) -> Point {
        assert!(
            at <= self.len_bytes(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len_bytes()
        );
        let (b, c, mut l) = self.records.closest_to(at);

        let found = if at >= b {
            let [s0, s1] = self.strs_in_range(b..);

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
            self.strs_in_range(..b)
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
            .map(|(b, c, l)| Point::from_coords(b, c, l))
            .unwrap_or(self.len_point())
    }

    #[inline(always)]
    pub fn point_at_char(&self, at: usize) -> Point {
        assert!(
            at <= self.len_chars(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len_bytes()
        );
        let (b, c, mut l) = self.records.closest_to_by(at, |(_, c, _)| *c);

        let found = if at >= c {
            let [s0, s1] = self.strs_in_range(b..);

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
            self.strs_in_range(..b)
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
            .map(|(b, c, l)| Point::from_coords(b, c, l))
            .unwrap_or(self.len_point())
    }

    #[inline(always)]
    pub fn point_at_line(&self, at: usize) -> Point {
        assert!(
            at <= self.len_lines(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len_bytes()
        );
        let (b, c, mut l) = self.records.closest_to_by(at, |(.., l)| *l);

        let found = if at >= l {
            let [s0, s1] = self.strs_in_range(b..);

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
            self.strs_in_range(..b)
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
            .map(|(b, c, l)| Point::from_coords(b, c, l))
            .unwrap_or(self.len_point())
    }

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
    /// This method is useful if you want to iterater reversibly
    /// right after a certain point, thus including the character
    /// of said point.
    pub fn points_after(&self, tp: impl TwoPoints) -> Option<(Point, Option<Point>)> {
        self.iter_at(tp)
            .filter_map(|item| item.part.as_char().map(|_| item.points()))
            .chain([self.len_points()])
            .nth(1)
    }

    pub fn char_at(&self, point: Point) -> Option<char> {
        let (s0, s1) = self.slices();
        if point.byte() < s0.len() {
            s0[point.byte()..].chars().next()
        } else {
            s1[(point.byte() - s0.len())..].chars().next()
        }
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn len_chars(&self) -> usize {
        self.records.max().1
    }

    pub fn len_lines(&self) -> usize {
        self.records.max().2
    }

    pub fn len_point(&self) -> Point {
        let (b, c, l) = self.records.max();
        Point::from_coords(b, c, l)
    }

    pub fn len_points(&self) -> (Point, Option<Point>) {
        self.ghost_max_points_at(self.len_point().byte())
    }

    pub fn last_point(&self) -> Option<Point> {
        self.strs_in_range(..)
            .into_iter()
            .flat_map(str::chars)
            .next_back()
            .map(|char| self.len_point().rev(char))
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
        let mut iter = self.rev_iter_at((real, ghost)).peekable();
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

    /// Clones the inner [`GapBuffer`] as a [`String`]
    // NOTE: Inherent because I don't want this to implement Display
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let [s0, s1] = self.strs_in_range(..);
        s0.to_string() + s1
    }

    pub fn strs_in_point_range(&self, (p1, p2): (Point, Point)) -> [&str; 2] {
        self.strs_in_range(p1.byte()..p2.byte())
    }

    fn strs_in_range(&self, range: impl RangeBounds<usize> + std::fmt::Debug) -> [&str; 2] {
        let (s0, s1) = self.buf.as_slices();
        let (start, end) = get_ends(range, self.len_bytes());

        unsafe {
            let r0 = start.min(s0.len())..end.min(s0.len());
            let r1 = start.saturating_sub(s0.len()).min(s1.len())
                ..end.saturating_sub(s0.len()).min(s1.len());

            [from_utf8_unchecked(&s0[r0]), from_utf8_unchecked(&s1[r1])]
        }
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> Iter<'_> {
        Iter::new_at(self, Point::default())
    }

    pub fn iter_at(&self, p: impl TwoPoints) -> Iter<'_> {
        Iter::new_at(self, p)
    }

    pub fn rev_iter(&self) -> RevIter {
        RevIter::new_at(self, self.len_point())
    }

    pub fn rev_iter_at(&self, p: impl TwoPoints) -> RevIter<'_> {
        RevIter::new_at(self, p)
    }

    pub fn iter_bytes_at(&self, b: usize) -> impl Iterator<Item = u8> + '_ {
        self.strs_in_range(b..).into_iter().flat_map(str::bytes)
    }

    pub fn iter_chars_at(&self, p: Point) -> impl Iterator<Item = (Point, char)> + '_ {
        self.strs_in_range(p.byte()..)
            .into_iter()
            .flat_map(str::chars)
            .scan(p, |p, char| {
                let old_p = *p;
                *p = p.fwd(char);
                Some((old_p, char))
            })
    }

    pub fn iter_chars_at_rev(&self, p: Point) -> impl Iterator<Item = (Point, char)> + '_ {
        self.strs_in_range(..p.byte())
            .into_iter()
            .flat_map(str::chars)
            .rev()
            .scan(p, |p, char| {
                *p = p.rev(char);
                Some((*p, char))
            })
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

impl std::fmt::Debug for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Text")
            .field(
                "buf",
                &format!("'{}', '{}'", self.slices().0, self.slices().1),
            )
            .field("tags", &self.tags)
            .field("records", &self.records)
            .finish()
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

mod point {
    use serde::{Deserialize, Serialize};

    use super::Item;

    /// A position in [`Text`].
    #[derive(
        Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize,
    )]
    pub struct Point {
        b: usize,
        c: usize,
        l: usize,
    }

    impl Point {
        /// Returns a new [`Point`], at the first byte.
        pub fn new() -> Self {
            Self::default()
        }

        pub(super) fn from_coords(b: usize, c: usize, l: usize) -> Self {
            Self { b, c, l }
        }

        pub(super) fn fwd(self, char: char) -> Self {
            Self {
                b: self.b + char.len_utf8(),
                c: self.c + 1,
                l: self.l + (char == '\n') as usize,
            }
        }

        pub(super) fn fwd_byte(&self, b: u8) -> Self {
            Self {
                b: self.b + 1,
                c: self.c + utf8_char_width(b),
                l: self.l + (b == b'\n') as usize,
            }
        }

        pub(super) fn rev(self, char: char) -> Self {
            Self {
                b: self.b - char.len_utf8(),
                c: self.c - 1,
                l: self.l - (char == '\n') as usize,
            }
        }

        pub(super) fn rev_byte(&self, b: u8) -> Self {
            Self {
                b: self.b - 1,
                // Theoretically, this is incongruent with fwd_byte,
                // since it will say that the bytes 1-3 of a char
                // belong to the next one.
                // However, this doesn't really matter, since I am
                // not allowing the user to search for [u8]s, only
                // through valid utf8.
                c: self.c - utf8_char_width(b),
                l: self.l - (b == b'\n') as usize,
            }
        }

        /// Returns the byte (relative to the beginning of the file)
        /// of self. Indexed at 0.
        pub fn byte(&self) -> usize {
            self.b
        }

        /// Returns the char index (relative tow the beginning of the
        /// file). Indexed at 0.
        pub fn char(&self) -> usize {
            self.c
        }

        /// Returns the line. Indexed at 0.
        pub fn line(&self) -> usize {
            self.l
        }
    }

    pub trait TwoPoints: std::fmt::Debug + Clone + Copy {
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
    /// UTF-8 character.
    #[inline]
    pub const fn utf8_char_width(b: u8) -> usize {
        UTF8_CHAR_WIDTH[b as usize] as usize
    }
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    use tags::Tag::{ExtraCursor, MainCursor, PopForm, PushForm};

    use crate::forms::{EXTRA_SEL_FORM_ID, MAIN_SEL_FORM_ID};

    if is_main {
        (
            MainCursor,
            PushForm(MAIN_SEL_FORM_ID),
            PopForm(MAIN_SEL_FORM_ID),
        )
    } else {
        (
            ExtraCursor,
            PushForm(EXTRA_SEL_FORM_ID),
            PopForm(EXTRA_SEL_FORM_ID),
        )
    }
}

pub fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
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

macro impl_from_to_string($t:ty) {
    impl From<$t> for Text {
        fn from(value: $t) -> Self {
            let value = <$t as ToString>::to_string(&value);
            let buf = Box::new(GapBuffer::from_iter(value.bytes()));
            let tags = Box::new(Tags::with_len(buf.len()));

            Self {
                buf,
                tags,
                marker: Marker::new(),
                records: Records::with_max((
                    value.len(),
                    value.chars().count(),
                    value.lines().count(),
                )),
            }
        }
    }
}
