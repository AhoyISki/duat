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

macro_rules! implPartialEq {
    ($self:ident: $Self:ty, $other:ident: $Other:ty, $($impl:tt)+) => {
        impl PartialEq<$Other> for $Self {
            fn eq(&self, other: &$Other) -> bool {
                let ($self, $other) = (&self, other);
                $($impl)+
            }
        }
    }
}

pub(super) use implPartialEq;

macro_rules! implTextRange {
    ($range:ident, $r:ident, $sb:expr, $eb:expr, $sp:expr, $ep:expr) => {
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
    };
}

macro_rules! implTextRangeOrIndex {
    ($range:ident) => {
        impl TextRangeOrIndex for $range<usize> {
            fn to_range(self, max: usize) -> Range<usize> {
                TextRange::to_range(self, max)
            }
        }

        impl TextRangeOrIndex for $range<Point> {
            fn to_range(self, max: usize) -> Range<usize> {
                TextRange::to_range(self, max)
            }
        }
    };
}

/// A position in [`Text`]
///
/// [`Text`]: super::Text
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
pub struct Point {
    byte: u32,
    char: u32,
    line: u32,
}

impl Point {
    /// Returns a new `Point`, at the first byte
    pub const fn new() -> Self {
        Point { byte: 0, char: 0, line: 0 }
    }

    /// A `Point` from raw indices
    pub const fn from_raw(b: usize, c: usize, l: usize) -> Self {
        let (b, c, l) = (b as u32, c as u32, l as u32);
        Self { byte: b, char: c, line: l }
    }

    /// Returns a new [`TwoPoints`] that includes the [`Ghost`]s in
    /// the same byte, if there is one
    ///
    /// [`Ghost`]: super::Ghost
    pub const fn to_two_points_before(self) -> TwoPoints {
        TwoPoints::new_before_ghost(self)
    }

    /// Returns a new [`TwoPoints`] that skips the [`Ghost`]s in the
    /// same byte, if there is one
    ///
    /// [`Ghost`]: super::Ghost
    pub const fn to_two_points_after(self) -> TwoPoints {
        TwoPoints::new_after_ghost(self)
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
            byte: str.len() as u32,
            char: str.chars().count() as u32,
            line: str.bytes().filter(|c| *c == b'\n').count() as u32,
        }
    }

    /// Returns the byte (relative to the beginning of the buffer)
    /// of self. Indexed at 0
    ///
    /// You can use byte indices to index the [`Text`] or [`Bytes`]
    /// with the [`Bytes::point_at_byte`] function.
    ///
    /// [`Text`]: super::Text
    /// [`Bytes`]: super::Bytes
    /// [`Bytes::point_at_byte`]: super::Bytes::point_at_byte
    pub const fn byte(&self) -> usize {
        self.byte as usize
    }

    /// Returns the char index (relative to the beginning of the
    /// buffer). Indexed at 0
    ///
    /// This is the primary value used when indexing the [`Text`] and
    /// [`Bytes`]. That is, the [`Bytes::point_at_byte`],
    /// [`Bytes::strs`], and most other [`Bytes`] functions rely
    /// on a character indices (or [`Point`]s) for indexing a
    /// [`Text`].
    ///
    /// [`Text`]: super::Text
    /// [`Bytes`]: super::Bytes
    /// [`Bytes::point_at_byte`]: super::Bytes::point_at_byte
    /// [`Bytes::strs`]: super::Bytes::strs
    pub const fn char(&self) -> usize {
        self.char as usize
    }

    /// Returns the line. Indexed at 0
    ///
    /// You can use byte indices to index the [`Text`] or [`Bytes`]
    /// with the [`Bytes::point_at_line`] function.
    ///
    /// [`Text`]: super::Text
    /// [`Bytes`]: super::Bytes
    /// [`Bytes::point_at_line`]: super::Bytes::point_at_line
    pub const fn line(&self) -> usize {
        self.line as usize
    }

    /// Checked [`Point`] subtraction
    pub fn checked_sub(self, rhs: Point) -> Option<Point> {
        Some(Self {
            byte: self.byte.checked_sub(rhs.byte)?,
            char: self.char.checked_sub(rhs.char)?,
            line: self.line.checked_sub(rhs.line)?,
        })
    }

    ////////// Shifting functions

    /// Moves a [`Point`] forward by one character
    #[inline(always)]
    pub(crate) const fn fwd(self, char: char) -> Self {
        Self {
            byte: self.byte + char.len_utf8() as u32,
            char: self.char + 1,
            line: self.line + (char == '\n') as u32,
        }
    }

    /// Moves a [`Point`] in reverse by one character
    #[inline(always)]
    pub(crate) const fn rev(self, char: char) -> Self {
        Self {
            byte: self.byte - char.len_utf8() as u32,
            char: self.char - 1,
            line: self.line - (char == '\n') as u32,
        }
    }

    /// Shifts the [`Point`] by a "signed point"
    ///
    /// This assumes that no overflow is going to happen
    pub(crate) const fn shift_by(self, [b, c, l]: [i32; 3]) -> Self {
        Self {
            byte: (self.byte as i32 + b) as u32,
            char: (self.char as i32 + c) as u32,
            line: (self.line as i32 + l) as u32,
        }
    }

    /// Returns a signed representation of this [`Point`]
    ///
    /// In this representation, the indices 0, 1 and 2 are the byte,
    /// char and line, respectively.
    pub(crate) const fn as_signed(self) -> [i32; 3] {
        [self.byte as i32, self.char as i32, self.line as i32]
    }
}

impl std::fmt::Debug for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Point {{ b: {}, c: {}, l: {} }}",
            self.byte, self.char, self.line
        )
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}, {}", self.byte, self.char, self.line)
    }
}

impl std::ops::Add for Point {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            byte: self.byte + rhs.byte,
            char: self.char + rhs.char,
            line: self.line + rhs.line,
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
            byte: self.byte - rhs.byte,
            char: self.char - rhs.char,
            line: self.line - rhs.line,
        }
    }
}

impl std::ops::SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

/// A [`Point`] or a `usize`, representing a byte index
///
/// In Duat, [`Point`]s are _usually_ just "thin wrappers" around a
/// byte index, useful for getting other information about a place in
/// the [`Text`], but that extra information is normally ignored when
/// doing internal calculations.
///
/// For that reason, Duat allows users to use either [`Point`]s _or_
/// byte indices in order to index the [`Text`], for convenience's
/// sake.
///
/// [`Text`]: super::Text
pub trait TextIndex: Clone + Copy + std::fmt::Debug {
    /// Converts this type into a byte index.
    fn to_byte_index(self) -> usize;
}

impl TextIndex for Point {
    fn to_byte_index(self) -> usize {
        self.byte()
    }
}

impl TextIndex for usize {
    fn to_byte_index(self) -> usize {
        self
    }
}

/// Ranges that can be used to index the [`Text`]
///
/// All of the [ranges] in [`std`] that implement either
/// [`RangeBounds<usize>`] or [`RangeBounds<Point>`] should work as an
/// argument. If it implements [`RangeBounds<usize>`], then the
/// `usize` represents the a byte index in the [`Text`].
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
/// In all cases, they represent a byte index from the start of the
/// [`Text`]
///
/// This trait's purpose is to be used for [`Tag`] removal in the
/// [`Tags::remove`] and [`Text::remove_tags`] functions. This is
/// useful in order to reduce the number of functions exposed to API
/// users.
///
/// [`Tag`]: super::Tag
/// [`Tags::remove`]: super::Tags::remove
/// [`Text::remove_tags`]: super::Text::remove_tags
/// [`Text`]: super::Text
pub trait TextRangeOrIndex {
    /// Transforms `self` into a [`Range<usize>`]
    fn to_range(self, max: usize) -> Range<usize>;
}

impl TextRangeOrIndex for usize {
    fn to_range(self, max: usize) -> Range<usize> {
        max.min(self)..max.min(self + 1)
    }
}

impl TextRangeOrIndex for Point {
    fn to_range(self, max: usize) -> Range<usize> {
        max.min(self.byte())..max.min(self.byte() + 1)
    }
}

impl TextRangeOrIndex for RangeFull {
    fn to_range(self, max: usize) -> Range<usize> {
        0..max
    }
}

implTextRangeOrIndex!(Range);
implTextRangeOrIndex!(RangeInclusive);
implTextRangeOrIndex!(RangeTo);
implTextRangeOrIndex!(RangeToInclusive);
implTextRangeOrIndex!(RangeFrom);

/// A struct used to exactly pinpoint a position in [`Text`], used
/// when printing
///
/// This struct has two inner components, a `real` [`Point`], and a
/// `ghost` [`Option<Point>`]. The second component is used whenever
/// you want to print a [`Ghost`] `Text`, either fully or partially.
///
/// The `ghost` component represents the "sum position" of all
/// `Ghost`s in that same byte. For example if there are two ghosts in
/// a single byte, if you pass `ghost == ghost1.len()`, then only the
/// second ghost will be included in this iteration.
///
/// [`TwoPoints::default`] will include the first [`Ghost`].
///
/// [`Text`]: super::Text
/// [`Ghost`]: super::Ghost
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Encode, Decode, Hash)]
pub struct TwoPoints {
    /// The real `Point` in the [`Text`]
    ///
    /// [`Text`]: super::Text
    pub real: Point,
    /// A possible point in a [`Ghost`]
    ///
    /// A value of [`None`] means that this is either at the end of
    /// the ghosts at a byte (i.e. this `TwoPoints` represents a real
    /// character), or this byte index doesn't have any ghosts at all.
    ///
    /// A value of [`Some`] means that this `TwoPoints` does _not_
    /// represent a real character, so it points to a character
    /// belonging to a [`Ghost`]
    ///
    /// If you don't know how to set this value, you should try to use
    /// the [`new`], [`new_before_ghost`] or [`new_after_ghost`]
    /// functions.
    ///
    /// [`new`]: Self::new
    /// [`new_before_ghost`]: Self::new_before_ghost
    /// [`new_after_ghost`]: Self::new_after_ghost
    /// [`Ghost`]: super::Ghost
    pub ghost: Option<Point>,
}

impl TwoPoints {
    /// Returns a fully qualified `TwoPoints`
    ///
    /// This will include a precise `real` [`Point`] as well as a
    /// precise `ghost` [`Point`].
    ///
    /// If you don't want to deal with ghosts, see
    /// [`TwoPoints::new_before_ghost`] and
    /// [`TwoPoints::new_after_ghost`].
    pub const fn new(real: Point, ghost: Point) -> Self {
        Self { real, ghost: Some(ghost) }
    }

    /// Returns a new `TwoPoints` that will include the [`Ghost`]
    /// before the real [`Point`]
    ///
    /// [`Ghost`]: super::Ghost
    pub const fn new_before_ghost(real: Point) -> Self {
        Self { real, ghost: Some(Point::new()) }
    }

    /// Returns a new `TwoPoints` that will exclude the [`Ghost`]
    /// before the real [`Point`]
    ///
    /// [`Ghost`]: super::Ghost
    pub const fn new_after_ghost(real: Point) -> Self {
        Self { real, ghost: None }
    }
}

impl std::cmp::PartialOrd for TwoPoints {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TwoPoints {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.real.cmp(&other.real) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        match (&self.ghost, &other.ghost) {
            (Some(l), Some(r)) => l.cmp(r),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        }
    }
}

const MAX: usize = usize::MAX;

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
