//! Convenience operations for the [`Text`]
//!
//! These include the [`Point`] struct and traits that are meant to
//! take many kinds of inputs, like the [`TwoPoints`], which is meant
//! to interpret up to 2 [`Point`]s as a real and ghost position in
//! the [`Text`].
//!
//! [`Text`]: super::Text
use std::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};

use bincode::{Decode, Encode};

use super::Item;

/// A position in [`Text`]
///
/// [`Text`]: super::Text
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
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
    pub(super) fn from_raw(b: usize, c: usize, l: usize) -> Self {
        let (b, c, l) = (b as u32, c as u32, l as u32);
        Self { b, c, l }
    }

    ////////// Querying functions

    /// The len [`Point`] of a [`&str`]
    ///
    /// This is the equivalent of [`Text::len`], but for types
    /// other than [`Text`]
    ///
    /// [`&str`]: str
    /// [`Text::len`]: super::Bytes::len
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
    ///
    /// You can use byte indices to index the [`Text`] or [`Bytes`]
    /// with the [`Bytes::point_at_byte`] function.
    ///
    /// [`Text`]: super::Text
    /// [`Bytes`]: super::Bytes
    /// [`Bytes::point_at_byte`]: super::Bytes::point_at_byte
    pub fn byte(&self) -> usize {
        self.b as usize
    }

    /// Returns the char index (relative to the beginning of the
    /// file). Indexed at 0
    ///
    /// This is the primary value used when indexing the [`Text`] and
    /// [`Bytes`]. That is, the [`Bytes::point_at`], [`Bytes::strs`],
    /// and most other [`Bytes`] functions rely on a character indices
    /// (or [`Point`]s) for indexing a [`Text`].
    ///
    /// [`Text`]: super::Text
    /// [`Bytes`]: super::Bytes
    /// [`Bytes::point_at`]: super::Bytes::point_at
    /// [`Bytes::strs`]: super::Bytes::strs
    pub fn char(&self) -> usize {
        self.c as usize
    }

    /// Returns the line. Indexed at 0
    ///
    /// You can use byte indices to index the [`Text`] or [`Bytes`]
    /// with the [`Bytes::point_at_line`] function.
    ///
    /// [`Text`]: super::Text
    /// [`Bytes`]: super::Bytes
    /// [`Bytes::point_at_line`]: super::Bytes::point_at_line
    pub fn line(&self) -> usize {
        self.l as usize
    }

    /// Checked [`Point`] subtraction
    pub fn checked_sub(self, rhs: Point) -> Option<Point> {
        Some(Self {
            b: self.b.checked_sub(rhs.b)?,
            c: self.c.checked_sub(rhs.c)?,
            l: self.l.checked_sub(rhs.l)?,
        })
    }

    ////////// Shifting functions

    /// Moves a [`Point`] forward by one character
    #[inline(always)]
    pub(crate) fn fwd(self, char: char) -> Self {
        Self {
            b: self.b + char.len_utf8() as u32,
            c: self.c + 1,
            l: self.l + (char == '\n') as u32,
        }
    }

    /// Moves a [`Point`] in reverse by one character
    #[inline(always)]
    pub(crate) fn rev(self, char: char) -> Self {
        Self {
            b: self.b - char.len_utf8() as u32,
            c: self.c - 1,
            l: self.l - (char == '\n') as u32,
        }
    }

    /// Shifts the [`Point`] by a "signed point"
    ///
    /// This assumes that no overflow is going to happen
    pub(crate) fn shift_by(self, [b, c, l]: [i32; 3]) -> Self {
        Self {
            b: (self.b as i32 + b) as u32,
            c: (self.c as i32 + c) as u32,
            l: (self.l as i32 + l) as u32,
        }
    }
}

impl std::fmt::Debug for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Point {{ b: {}, c: {}, l: {} }}", self.b, self.c, self.l)
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

/// Given a first byte, determines how many bytes are in this
/// UTF-8 character
#[inline]
pub const fn utf8_char_width(b: u8) -> u32 {
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

    UTF8_CHAR_WIDTH[b as usize] as u32
}

/// Ranges that can be used to index the [`Text`]
///
/// All of the [ranges] in [`std`] that implement either
/// [`RangeBounds<usize>`] or [`RangeBounds<Point>`] should work as an
/// argument. If it implements [`RangeBounds<usize>`], then the
/// `usize` represents the a char index in the [`Text`].
///
/// [`Text`]: super::Text
/// [ranges]: std::range
/// [`RangeBounds<usize>`]: std::ops::RangeBounds
/// [`RangeBounds<Point>`]: std::ops::RangeBounds
pub trait TextRange: Clone {
    /// A "forward facing range"
    ///
    /// If given a single [`usize`]/[`Point`], acts like [`RangeFrom`]
    fn to_range(self, max: usize) -> Range<usize>;
}

implTextRange!(Range, r, r.start, r.end, r.start.byte(), r.end.byte());
implTextRange!(
    RangeInclusive,
    r,
    *r.start(),
    r.end() + 1,
    r.start().byte(),
    r.end().byte() + 1
);
implTextRange!(RangeTo, r, 0, r.end, 0, r.end.byte());
implTextRange!(RangeToInclusive, r, 0, r.end, 0, r.end.byte());
implTextRange!(RangeFrom, r, r.start, MAX, r.start.byte(), MAX);

impl TextRange for RangeFull {
    fn to_range(self, max: usize) -> Range<usize> {
        0..max
    }
}

/// Either a [`TextRange`], a [`usize`] or a [`Point`]
///
/// This trait's purpose is to be used for [`Tag`] removal in the
/// [`Tags::remove`] and [`Text::remove_tags`] functions. This is
/// useful in order to reduce the number of functions exposed to API
/// users.
///
/// [`Tag`]: super::Tag
/// [`Tags::remove`]: super::Tags::remove
/// [`Text::remove_tags`]: super::Text::remove_tags
pub trait TextRangeOrPoint {
    /// Transforms `self` into a [`Range<usize>`]
    fn to_range(self, max: usize) -> Range<usize>;
}

impl TextRangeOrPoint for usize {
    fn to_range(self, max: usize) -> Range<usize> {
        max.min(self)..max.min(self + 1)
    }
}

impl TextRangeOrPoint for Point {
    fn to_range(self, max: usize) -> Range<usize> {
        max.min(self.byte())..max.min(self.byte() + 1)
    }
}

impl TextRangeOrPoint for RangeFull {
    fn to_range(self, max: usize) -> Range<usize> {
        TextRange::to_range(self, max)
    }
}

implTextRangeOrPoint!(Range);
implTextRangeOrPoint!(RangeInclusive);
implTextRangeOrPoint!(RangeTo);
implTextRangeOrPoint!(RangeToInclusive);
implTextRangeOrPoint!(RangeFrom);

/// Two positions, one for the [`Text`], and one for [ghost text]
///
/// This can either be a [`Point`] or `(Point, Option<Point>)` or
/// even `(Point, Point)`. If a second [`Point`] is excluded, it
/// is assumed to be [`Point::default()`], i.e., this
/// [`TwoPoints`] represents the beginning of a [ghost text].
///
/// [`Text`]: super::Text
/// [ghost text]: super::Ghost
pub trait TwoPoints: Clone + Copy + std::fmt::Debug {
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

const MAX: usize = usize::MAX;

macro implTextRange($range:ident, $r:ident, $sb:expr, $eb:expr, $sp:expr, $ep:expr) {
    impl TextRange for $range<usize> {
        fn to_range(self, max: usize) -> Range<usize> {
            let $r = self;
            max.min($sb)..max.min($eb)
        }
    }

    impl TextRange for $range<Point> {
        fn to_range(self, max: usize) -> Range<usize> {
            let $r = self;
            max.min($sp)..max.min($ep)
        }
    }
}

macro implTextRangeOrPoint($range:ident) {
    impl TextRangeOrPoint for $range<usize> {
        fn to_range(self, max: usize) -> Range<usize> {
            TextRange::to_range(self, max)
        }
    }

    impl TextRangeOrPoint for $range<Point> {
        fn to_range(self, max: usize) -> Range<usize> {
            TextRange::to_range(self, max)
        }
    }
}
