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
    pub fn byte(&self) -> usize {
        self.b as usize
    }

    /// Returns the char index (relative to the beginning of the
    /// file). Indexed at 0
    pub fn char(&self) -> usize {
        self.c as usize
    }

    /// Returns the line. Indexed at 0
    pub fn line(&self) -> usize {
        self.l as usize
    }

    pub fn checked_sub(self, rhs: Point) -> Option<Point> {
        Some(Self {
            b: self.b.checked_sub(rhs.b)?,
            c: self.c.checked_sub(rhs.c)?,
            l: self.l.checked_sub(rhs.l)?,
        })
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
/// These include:
///
/// - A single [`usize`] (interpretation varies)
/// - All ranges that implement [`RangeBounds<usize>`]
/// - A single [`Point`] (interpretation varies)
/// - A 2 [`Point`] tuple
/// - A 2 [`Option<Point>`] tuple
/// - And a tuple of a [`Point`] and an [`Option<Point>`]
///
/// The behavior of [`usize`] and [`Point`] depends on where they are
/// sent, but is pretty straightforward:
///
/// - On forward iterators, they act as a [`RangeFrom`]
/// - On reverse iterators, they act as a [`RangeTo`]
/// - On insertion of [`Tag`]s, they act as a 1 element range
///
/// The purpose of this trait is to minimize the number of functions
/// needed to perform tasks.
///
/// [`Text`]: super::Text
/// [`Tag`]: super::Tag
/// [`RangeBounds<usize>`]: std::ops::RangeBounds
pub trait TextRange: Clone {
    /// A "forward facing range"
    ///
    /// If given a single [`usize`]/[`Point`], acts like [`RangeFrom`]
    fn to_range_fwd(self, max: usize) -> Range<usize>;

    /// A "reverse facing range"
    ///
    /// If given a single [`usize`]/[`Point`], acts like [`RangeTo`]
    fn to_range_rev(self, max: usize) -> Range<usize>;

    /// A "single point range"
    ///
    /// If given a single [`usize`]/[`Point`], acts like one element
    fn to_range_at(self, max: usize) -> Range<usize>;
}

impl TextRange for usize {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        self.min(max)..max
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        max..self.min(max)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        self.min(max)..(self + 1).min(max)
    }
}

impl TextRange for Range<usize> {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        self.start.min(max)..self.end.min(max)
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        self.start.min(max)..self.end.min(max)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        self.start.min(max)..self.end.min(max)
    }
}

impl TextRange for RangeInclusive<usize> {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        max.min(*self.start())..max.min(self.end() + 1)
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        max.min(*self.start())..max.min(self.end() + 1)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        max.min(*self.start())..max.min(self.end() + 1)
    }
}

impl TextRange for RangeTo<usize> {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        0..max.min(self.end)
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        0..max.min(self.end)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        0..max.min(self.end)
    }
}

impl TextRange for RangeToInclusive<usize> {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        0..max.min(self.end + 1)
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        0..max.min(self.end + 1)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        0..max.min(self.end + 1)
    }
}

impl TextRange for RangeFrom<usize> {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        max.min(self.start)..max
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        max.min(self.start)..max
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        max.min(self.start)..max
    }
}

impl TextRange for RangeFull {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        0..max
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        0..max
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        0..max
    }
}

impl TextRange for Point {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        self.byte().min(max)..max
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        0..self.byte().min(max)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        self.byte().min(max)..(self.byte() + 1).min(max)
    }
}

impl TextRange for (Point, Point) {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        self.0.byte().min(max)..self.1.byte().min(max)
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        self.0.byte().min(max)..self.1.byte().min(max)
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        self.0.byte().min(max)..self.1.byte().min(max)
    }
}

impl TextRange for (Option<Point>, Option<Point>) {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        match self {
            (None, None) => (..).to_range_fwd(max),
            (None, Some(end)) => (..end.byte()).to_range_fwd(max),
            (Some(start), None) => (start.byte()..).to_range_fwd(max),
            (Some(start), Some(end)) => (start.byte()..end.byte()).to_range_fwd(max),
        }
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        match self {
            (None, None) => (..).to_range_rev(max),
            (None, Some(end)) => (..end.byte()).to_range_rev(max),
            (Some(start), None) => (start.byte()..).to_range_rev(max),
            (Some(start), Some(end)) => (start.byte()..end.byte()).to_range_rev(max),
        }
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        match self {
            (None, None) => (..).to_range_rev(max),
            (None, Some(end)) => (..end.byte()).to_range_rev(max),
            (Some(start), None) => (start.byte()..).to_range_rev(max),
            (Some(start), Some(end)) => (start.byte()..end.byte()).to_range_rev(max),
        }
    }
}

impl TextRange for (Point, Option<Point>) {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        match self {
            (start, None) => (start.byte()..).to_range_fwd(max),
            (start, Some(end)) => (start.byte()..end.byte()).to_range_fwd(max),
        }
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        match self {
            (start, None) => (start.byte()..).to_range_rev(max),
            (start, Some(end)) => (start.byte()..end.byte()).to_range_rev(max),
        }
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        match self {
            (start, None) => (start.byte()..).to_range_rev(max),
            (start, Some(end)) => (start.byte()..end.byte()).to_range_rev(max),
        }
    }
}

impl TextRange for (Option<Point>, Point) {
    fn to_range_fwd(self, max: usize) -> Range<usize> {
        match self {
            (None, end) => (..end.byte()).to_range_fwd(max),
            (Some(start), end) => (start.byte()..end.byte()).to_range_fwd(max),
        }
    }

    fn to_range_rev(self, max: usize) -> Range<usize> {
        match self {
            (None, end) => (..end.byte()).to_range_rev(max),
            (Some(start), end) => (start.byte()..end.byte()).to_range_rev(max),
        }
    }

    fn to_range_at(self, max: usize) -> Range<usize> {
        match self {
            (None, end) => (..end.byte()).to_range_rev(max),
            (Some(start), end) => (start.byte()..end.byte()).to_range_rev(max),
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
pub trait TwoPoints: Clone + Copy {
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
