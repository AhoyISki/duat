use std::{
    ops::{ControlFlow, Range, RangeBounds},
    str::Utf8Error,
};

use gap_buf::GapBuffer;

pub use crate::text::bytes::strs::Strs;
use crate::{
    buffer::Change,
    opts::PrintOpts,
    text::{Point, TextIndex, TextRange, records::Records, utils::implPartialEq},
};

mod strs;

/// The bytes of a [`Text`], encoded in UTF-8
///
/// [`Text`]: super::Text
#[derive(Default, Clone)]
pub struct Bytes {
    buf: GapBuffer<u8>,
    records: Records,
    pub(super) bytes_state: u64,
}

impl Bytes {
    /// Returns a new instance of [`Bytes`]
    ///
    /// Not intended for public use, it is necessary in duat
    #[doc(hidden)]
    pub(crate) fn new(string: &str) -> Self {
        let buf = GapBuffer::from_iter(string.bytes());

        let len = buf.len();
        let chars = string.chars().count();
        let lines = buf.iter().filter(|b| **b == b'\n').count();
        Self {
            buf,
            records: Records::new([len, chars, lines]),
            bytes_state: 0,
        }
    }

    ////////// Querying functions

    /// The [`Point`] at the end of the text
    pub fn len(&self) -> Point {
        let [b, c, l] = self.records.max();
        Point::from_raw(b, c, l)
    }

    /// Whether or not there are any characters in [`Bytes`], besides
    /// the final `b'\n'`
    ///
    /// # Note
    ///
    /// This does not check for tags, so with a [`Tag::Ghost`],
    /// there could actually be a "string" of characters on the
    /// [`Text`], it just wouldn't be considered real "text". If you
    /// want to check for the `InnerTags`'b possible emptyness as
    /// well, see [`Text::is_empty_empty`].
    ///
    /// [`Tag::Ghost`]: super::Ghost
    /// [`Text`]: super::Text
    /// [`Text::is_empty_empty`]: super::Text::is_empty_empty
    pub fn is_empty(&self) -> bool {
        let (s0, s1) = self.buf.as_slices();
        (s0 == b"\n" && s1 == b"") || (s0 == b"" && s1 == b"\n")
    }

    /// The `char` at the [`Point`]'b position
    pub fn char_at(&self, p: impl TextIndex) -> Option<char> {
        if p.to_byte_index() >= self.len().byte() {
            return None;
        }

        let [s0, s1] = self.strs_inner(..).unwrap();
        Some(if p.to_byte_index() < s0.len() {
            s0[p.to_byte_index()..].chars().next().unwrap()
        } else {
            s1[p.to_byte_index() - s0.len()..]
                .chars()
                .next()
                .unwrap_or_else(|| panic!("{self:#?}"))
        })
    }

    /// A subslice of the [`Bytes`]
    ///
    /// Note that this `TextRange` is relative to the whole [`Bytes`]
    /// struct, not just this [`Strs`]. This method also clips the
    /// ranges so they fit into the range of these `Strs`.
    ///
    /// It will return [`None`] if the range does not start or end in
    /// valid utf8 boundaries.
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
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// # let (p0, p1) = (Point::default(), Point::default());
    /// # let text = Text::new();
    /// let bytes = text.bytes();
    ///
    /// for char in bytes.strs(p0..p1).unwrap().chars() {
    ///     todo!();
    /// }
    /// ```
    ///
    /// Do note that you should avoid iterators like [`str::lines`],
    /// as they will separate the line that is partially owned by each
    /// [`&str`]:
    ///
    /// ```rust
    /// let broken_up_line = [
    ///     "This is line 1, business as usual.\nThis is line 2, but it",
    ///     "is broken into two separate strings.\nSo 4 lines would be counted, instead of 3",
    /// ];
    /// ```
    ///
    /// This is one way that the inner [`GapBuffer`] could be set up,
    /// where one of the lines is split among the two slices.
    ///
    /// If you wish to iterate over the lines, see [`Bytes::lines`].
    ///
    /// [`&str`]: str
    /// [`Text`]: super::Text
    /// [range]: TextRange
    /// [`strs`]: Self::strs
    pub fn strs(&self, range: impl TextRange) -> Option<Strs<'_>> {
        let range = range.to_range(self.len().byte());

        Some(Strs::new(
            self,
            (range.start, range.end),
            self.strs_inner(range)?,
        ))
    }

    /// An [`Iterator`] over the bytes in a given _byte_ range
    ///
    /// Unlike [`strs`], this function works with _byte_ ranges, not
    /// [`TextRange`]s. That'b because [`Strs`] is supposed to return
    /// valid UTF-8 strings, which need to have valid character
    /// terminations, so they should be indexed by a character range,
    /// not a byte range.
    ///
    /// Since buffers is based on `[u8]`s, not `str`s, it doesn't have
    /// the same restrictions, so a byte range can be used instead.
    ///
    /// If the range is fully or partially out of bounds, one or both
    /// of the slices might be empty.
    ///
    /// [`strs`]: Self::strs
    #[track_caller]
    pub fn slices(&self, range: impl TextRange) -> Slices<'_> {
        let (s0, s1) = self
            .buf
            .range(range.to_range(self.len().byte()))
            .as_slices();
        Slices([s0.iter(), s1.iter()])
    }

    /// Returns an iterator over the lines in a given range
    ///
    /// The lines are inclusive, that is, it will iterate over the
    /// whole line, not just the parts within the range.
    ///
    /// [range]: TextRange
    #[track_caller]
    pub fn lines(&self, range: impl TextRange) -> Lines<'_> {
        let range = range.to_range(self.len().byte());
        let start = self.point_at_line(self.point_at_byte(range.start).line());
        let end = {
            let end = self.point_at_byte(range.end);
            let line_start = self.point_at_line(end.line());
            if line_start == end {
                end
            } else {
                self.point_at_line((end.line() + 1).min(self.len().line()))
            }
        };

        Lines::new(self, start.byte(), end.byte())
    }

    /// Returns the two `&str`s in the byte range.
    #[track_caller]
    fn strs_inner(&self, range: impl RangeBounds<usize>) -> Option<[&str; 2]> {
        let range = crate::utils::get_range(range, self.len().byte());
        use std::str::from_utf8_unchecked;

        let (s0, s1) = self.buf.as_slices();

        // Check if the slices match utf8 boundaries.
        if s0.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || s1.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || self
                .buf
                .get(range.end)
                .is_some_and(|b| utf8_char_width(*b) == 0)
        {
            return None;
        }

        Some(unsafe {
            let r0 = range.start.min(s0.len())..range.end.min(s0.len());
            let r1 = range.start.saturating_sub(s0.len()).min(s1.len())
                ..range.end.saturating_sub(s0.len()).min(s1.len());

            [from_utf8_unchecked(&s0[r0]), from_utf8_unchecked(&s1[r1])]
        })
    }

    /// The [`Point`] corresponding to the byte position, 0 indexed
    ///
    /// If the byte position would fall in between two characters
    /// (because the first one comprises more than one byte), the
    /// first character is chosen as the [`Point`] where the byte is
    /// located.
    ///
    /// # Panics
    ///
    /// Will panic if `b` is greater than the length of the text
    #[inline(always)]
    #[track_caller]
    pub fn point_at_byte(&self, b: usize) -> Point {
        assert!(
            b <= self.len().byte(),
            "byte out of bounds: the len is {}, but the byte is {b}",
            self.len().byte()
        );

        let [c_b, c_c, mut c_l] = self.records.closest_to_by_key(b, |[b, ..]| b);

        let found = if b >= c_b {
            let [s0, s1] = self.strs_inner(c_b..).unwrap();

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    c_l += (char == '\n') as usize;
                    (c_b + this_b, c_c + i, c_l - (char == '\n') as usize)
                })
                .take_while(|&(rhs, ..)| b >= rhs)
                .last()
        } else {
            let mut c_len = 0;
            self.strs_inner(..c_b)
                .unwrap()
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .enumerate()
                .map(|(i, char)| {
                    c_l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (c_b - c_len, c_c - (i + 1), c_l)
                })
                .take_while(|&(rhs, ..)| b <= rhs)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
    }

    /// The [`Point`] associated with the `c`th char
    ///
    /// # Panics
    ///
    /// Will panic if `c` is greater than the number of chars in the
    /// text.
    #[inline(always)]
    #[track_caller]
    pub fn point_at_char(&self, c: usize) -> Point {
        assert!(
            c <= self.len().char(),
            "char out of bounds: the len is {}, but the char is {c}",
            self.len().char()
        );

        let [c_b, c_c, mut c_l] = self.records.closest_to_by_key(c, |[_, c, _]| c);

        let found = if c >= c_c {
            let [s0, s1] = self.strs_inner(c_b..).unwrap();

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    c_l += (char == '\n') as usize;
                    (c_b + this_b, c_c + i, c_l - (char == '\n') as usize)
                })
                .take_while(|&(_, rhs, _)| c >= rhs)
                .last()
        } else {
            let mut c_len = 0;
            self.strs_inner(..c_b)
                .unwrap()
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .enumerate()
                .map(|(i, char)| {
                    c_l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (c_b - c_len, c_c - (i + 1), c_l)
                })
                .take_while(|&(_, rhs, _)| c <= rhs)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
    }

    /// The [`Point`] where the `l`th line starts, 0 indexed
    ///
    /// If `l == number_of_lines`, returns the last point of the
    /// text.
    ///
    /// # Panics
    ///
    /// Will panic if the number `l` is greater than the number of
    /// lines on the text
    #[inline(always)]
    #[track_caller]
    pub fn point_at_line(&self, l: usize) -> Point {
        assert!(
            l <= self.len().line(),
            "line out of bounds: the len is {}, but the line is {l}",
            self.len().line()
        );

        let (c_b, c_c, mut c_l) = {
            let [b, c, l] = self.records.closest_to_by_key(l, |[.., l]| l);
            let (b, c) = self
                .strs_inner(..b)
                .unwrap()
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .take_while(|c| *c != '\n')
                .fold((b, c), |(b, c), char| (b - char.len_utf8(), c - 1));
            (b, c, l)
        };

        let found = if l >= c_l {
            let [s0, s1] = self.strs_inner(c_b..).unwrap();

            s0.char_indices()
                .chain(s1.char_indices().map(|(b, char)| (b + s0.len(), char)))
                .enumerate()
                .map(|(i, (this_b, char))| {
                    c_l += (char == '\n') as usize;
                    (c_b + this_b, c_c + i, c_l - (char == '\n') as usize)
                })
                .find(|&(.., rhs)| l == rhs)
        } else {
            let mut c_len = 0;
            self.strs_inner(..c_b)
                .unwrap()
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .enumerate()
                .map(|(i, char)| {
                    c_l -= (char == '\n') as usize;
                    c_len += char.len_utf8();
                    (c_b - c_len, c_c - (i + 1), c_l)
                })
                .take_while(|&(.., rhs)| l <= rhs)
                .last()
        };

        found
            .map(|(b, c, l)| Point::from_raw(b, c, l))
            .unwrap_or(self.len())
    }

    /// The start and end [`Point`]s for the `l`th line
    ///
    /// If `l == number_of_lines`, these points will be the same.
    ///
    /// The second number _includes_ the `\n` at the end of the line.
    ///
    /// # Panics
    ///
    /// Will panic if the number `l` is greater than the number of
    /// lines on the text
    #[track_caller]
    pub fn line_range(&self, l: usize) -> Range<Point> {
        assert!(
            l <= self.len().line(),
            "line out of bounds: the len is {}, but the line is {l}",
            self.len().line()
        );

        let start = self.point_at_line(l);
        let (ControlFlow::Continue(end) | ControlFlow::Break(end)) = self
            .chars_fwd(start..)
            .unwrap()
            .try_fold(start, |end, (_, char)| match end.line() == start.line() {
                true => ControlFlow::Continue(end.fwd(char)),
                false => ControlFlow::Break(end),
            });

        start..end
    }

    /// The last [`Point`] associated with a `char`
    ///
    /// This will give the [`Point`] of the last `char` of the text.
    /// The difference between this method and [`len`] is that
    /// it will return a [`Point`] one position earlier than it. If
    /// the text is completely empty, it will return [`None`].
    ///
    /// [`len`]: Self::len
    pub fn last_point(&self) -> Point {
        self.len().rev('\n')
    }

    /// A forward iterator of the [`char`]s of [`Bytes`]
    ///
    /// Each [`char`] will be accompanied by a byte index, which is
    /// the position where said character starts, e.g. `0` for the
    /// first character.
    #[track_caller]
    pub fn chars_fwd(
        &self,
        range: impl TextRange,
    ) -> Option<impl Iterator<Item = (usize, char)> + '_> {
        let mut range = range.to_range(self.len().byte());
        Some(self.strs(range.clone())?.chars().map(move |char| {
            let byte = range.start;
            range.start += char.len_utf8();
            (byte, char)
        }))
    }

    /// A reverse iterator of the [`char`]s in [`Bytes`]
    ///
    /// Each [`char`] will be accompanied by a byte index, which is
    /// the position where said character starts, e.g. `0` for the
    /// first character.
    #[track_caller]
    pub fn chars_rev(
        &self,
        range: impl TextRange,
    ) -> Option<impl Iterator<Item = (usize, char)> + '_> {
        let mut range = range.to_range(self.len().byte());
        Some(self.strs(range.clone())?.chars().rev().map(move |char| {
            range.end -= char.len_utf8();
            (range.end, char)
        }))
    }

    /// Gets the indentation level on the current line
    pub fn indent(&self, p: Point, opts: PrintOpts) -> usize {
        let range = self.line_range(p.line());
        self.chars_fwd(range.start..)
            .unwrap()
            .map_while(|(_, c)| match c {
                ' ' => Some(1),
                '\t' => Some(opts.tabstop as usize),
                _ => None,
            })
            .sum()
    }

    ////////// Modification functions

    /// Replaces a [`TextRange`] with a `&str`
    ///
    /// If you want to apply a [`Change`] to the `Bytes` this way, you
    /// can use [`Change::taken_range`] as the `TextRange`, and
    /// [`Change::added_str`] as the replacement text.
    pub fn replace_range(&mut self, range: impl TextRange, new: impl AsRef<str>) {
        let edit = new.as_ref();
        let range = range.to_range(self.len().byte());

        let start = self.point_at_byte(range.start);
        let taken_len = self.point_at_byte(range.end) - start;
        let added_len = Point::len_of(edit);

        self.buf.splice(range, edit.bytes());

        let start_rec = [start.byte(), start.char(), start.line()];
        let old_len = [taken_len.byte(), taken_len.char(), taken_len.line()];
        let new_len = [added_len.byte(), added_len.char(), added_len.line()];

        self.records.transform(start_rec, old_len, new_len);
        self.records.insert(start_rec);
    }

    /// Applies a [`Change`] to the [`GapBuffer`] within
    pub(crate) fn apply_change(&mut self, change: Change<&str>) {
        let edit = change.added_str();
        let start = change.start();

        let range = start.byte()..change.taken_end().byte();
        self.buf.splice(range, edit.bytes());

        let start_rec = [start.byte(), start.char(), start.line()];
        let old_len = [
            change.taken_end().byte() - start.byte(),
            change.taken_end().char() - start.char(),
            change.taken_end().line() - start.line(),
        ];
        let new_len = [
            change.added_end().byte() - start.byte(),
            change.added_end().char() - start.char(),
            change.added_end().line() - start.line(),
        ];

        self.records.transform(start_rec, old_len, new_len);
        self.records.insert(start_rec);
    }

    /// Adds a record in the given position
    #[track_caller]
    pub(crate) fn add_record(&mut self, [b, c, l]: [usize; 3]) {
        self.records.insert([b, c, l]);
    }

    ////////// One str functions

    /// Tries to get a contiguous [`&str`] from the [`Bytes`]
    ///
    /// Returns [`None`] if the gap of the inner buffer was within the
    /// given range *OR*.
    ///
    /// [`&str`]: str
    pub fn get_contiguous(&self, range: impl TextRange) -> Option<&str> {
        let range = range.to_range(self.len().byte());
        let [s0, s1] = self.strs_inner(..).unwrap();

        if range.end <= self.buf.gap() {
            s0.get(range)
        } else {
            let gap = self.buf.gap();
            s1.get(range.start.checked_sub(gap)?..range.end.checked_sub(gap)?)
        }
    }
}

/// A [`Lender`] over the lines on [`Bytes`]
///
/// The reason for this being a [`Lender`], rather than a regular
/// [`Iterator`] is because the [`Bytes`] use a [`GapBuffer`] within,
/// which means that any line may be split in two. In order to still
/// return it as an `&str`, a new [`String`] needs to be allocated,
/// which will be owned by the [`Lines`], hence the [`Lender`] trait.
pub struct Lines<'b> {
    bytes: &'b Bytes,
    fwd_b: usize,
    rev_b: usize,
}

impl<'b> Lines<'b> {
    fn new(bytes: &'b Bytes, fwd_b: usize, rev_b: usize) -> Self {
        Self { bytes, fwd_b, rev_b }
    }
}

impl<'b> Iterator for Lines<'b> {
    type Item = Strs<'b>;

    fn next(&mut self) -> Option<Self::Item> {
        let [s0, s1] = {
            let (s0, s1) = self.bytes.buf.as_slices();
            [
                &s0[self.fwd_b.min(s0.len())..],
                &s1[self.fwd_b.saturating_sub(s0.len())..],
            ]
        };

        let fwd_b = s0
            .iter()
            .chain(s1.iter())
            .position(|b| *b == b'\n')
            .unwrap_or(self.bytes.len().byte());

        if fwd_b > self.fwd_b && fwd_b < self.rev_b {
            let range = (self.fwd_b, fwd_b);
            self.fwd_b = fwd_b;
            Some(Strs::new(self.bytes, range, unsafe {
                [
                    std::str::from_utf8_unchecked(&s0[..fwd_b.min(s0.len())]),
                    std::str::from_utf8_unchecked(&s1[..fwd_b.saturating_sub(s0.len())]),
                ]
            }))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.rev_b - self.fwd_b, Some(self.rev_b - self.fwd_b))
    }
}

impl DoubleEndedIterator for Lines<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let [s0, s1] = {
            let (s0, s1) = self.bytes.buf.as_slices();
            [
                &s0[..self.rev_b.min(s0.len())],
                &s1[..self.rev_b.saturating_sub(s0.len())],
            ]
        };

        let rev_b = {
            let mut count = 0;
            self.rev_b
                - s0.iter()
                    .chain(s1.iter())
                    .take_while(|byte| {
                        count += (**byte == b'\n') as usize;
                        count < 2
                    })
                    .count()
        };

        if rev_b < self.rev_b && rev_b > self.fwd_b {
            let range = (rev_b, self.rev_b);
            self.rev_b = rev_b;
            Some(Strs::new(self.bytes, range, unsafe {
                [
                    std::str::from_utf8_unchecked(&s0[rev_b.min(s0.len())..]),
                    std::str::from_utf8_unchecked(&s1[rev_b.saturating_sub(s0.len())..]),
                ]
            }))
        } else {
            None
        }
    }
}

impl ExactSizeIterator for Lines<'_> {}

/// An [`Iterator`] over the bytes in a [`Text`]
///
/// [`Text`]: super::Text
#[derive(Clone)]
pub struct Slices<'b>(pub(super) [std::slice::Iter<'b, u8>; 2]);

impl<'b> Slices<'b> {
    /// Converts this [`Iterator`] into an array of its two parts
    pub fn to_array(&self) -> [&'b [u8]; 2] {
        self.0.clone().map(|iter| iter.as_slice())
    }

    /// Tries to create a [`String`] out of the two buffers
    ///
    /// # Errors
    ///
    /// This function will return an error if the bounds of the slices
    /// don't correspond to utf8 character boundaries, or if the gap
    /// within these slices doesn't correspond to a utf8 character
    /// boundary.
    pub fn try_to_string(self) -> Result<String, Utf8Error> {
        let [s0, s1] = self.0.map(|arr| arr.as_slice());
        Ok([str::from_utf8(s0)?, str::from_utf8(s1)?].join(""))
    }

    /// Treats the inner slices as `&str`s and iterates over their
    /// characters
    ///
    /// You will want to use this function iff you don't want to check
    /// for character boundaries at the edges (very rarely). Otherwise
    /// [`bytes.strs({byte_range}).chars()`] instead.
    ///
    /// # Safety
    ///
    /// You must ensure that the `Slices` were acquired from valid
    /// byte ranges which coincide with character terminations. If you
    /// are unsure of that, you should use [`Strs::chars`] instead.
    ///
    /// [`bytes.strs({byte_range}).chars()`]: Bytes::strs
    pub unsafe fn chars_unchecked(self) -> impl Iterator<Item = char> {
        self.0
            .into_iter()
            .flat_map(|iter| unsafe { str::from_utf8_unchecked(iter.as_slice()) }.chars())
    }
}

impl<'b> Iterator for Slices<'b> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.0[0].next().or_else(|| self.0[1].next()).copied()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (l0, u0) = self.0[0].size_hint();
        let (l1, u1) = self.0[1].size_hint();
        (l0 + l1, Some(u0.unwrap() + u1.unwrap()))
    }
}

impl<'b> ExactSizeIterator for Slices<'b> {}

impl<'b> DoubleEndedIterator for Slices<'b> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0[1]
            .next_back()
            .or_else(|| self.0[0].next_back())
            .copied()
    }
}

/// Given a first byte, determines how many bytes are in this UTF-8
/// character.
#[must_use]
#[inline]
pub const fn utf8_char_width(b: u8) -> usize {
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
    UTF8_CHAR_WIDTH[b as usize] as usize
}

impl Eq for Bytes {}
implPartialEq!(bytes: Bytes, other: Bytes, {
    let (l_s0, l_s1) = bytes.buf.as_slices();
    let (r_s0, r_s1) = other.buf.as_slices();
    (l_s0.len() + l_s1.len() == r_s0.len() + r_s1.len()) && l_s0.iter().chain(l_s1).eq(r_s0.iter().chain(r_s1))
});
implPartialEq!(bytes: Bytes, other: &str, {
    let [s0, s1] = bytes.strs_inner(..).unwrap();
    other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
});
implPartialEq!(bytes: Bytes, other: String, bytes == &&other.as_str());
implPartialEq!(str: &str, other: Bytes, other == *str);
implPartialEq!(string: String, other: Bytes, other == *string);

impl Eq for Strs<'_> {}
implPartialEq!(strs: Strs<'_>, other: Strs<'_>, {
    let [l_s0, l_s1] = strs.to_array();
    let [r_s0, r_s1] = other.to_array();
    (l_s0.len() + l_s1.len() == r_s0.len() + r_s1.len()) && l_s0.bytes().chain(l_s1.bytes()).eq(r_s0.bytes().chain(r_s1.bytes()))
});
implPartialEq!(strs: Strs<'_>, other: &str, {
    let [s0, s1] = strs.to_array();
    other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
});
implPartialEq!(strs: Strs<'_>, other: String, strs == &&other.as_str());
implPartialEq!(str: &str, other: Strs<'_>, other == *str);
implPartialEq!(string: String, other: Strs<'_>, other == *string);

/// Implements [`From<$T>`] for [`Bytes`] where `$T: ToString`
macro_rules! implFromToString {
    ($T:ty) => {
        impl From<$T> for Bytes {
            fn from(value: $T) -> Self {
                let string = <$T as ToString>::to_string(&value);
                Bytes::new(&string)
            }
        }
    };
}

implFromToString!(u8);
implFromToString!(u16);
implFromToString!(u32);
implFromToString!(u64);
implFromToString!(u128);
implFromToString!(usize);
implFromToString!(i8);
implFromToString!(i16);
implFromToString!(i32);
implFromToString!(i64);
implFromToString!(i128);
implFromToString!(isize);
implFromToString!(f32);
implFromToString!(f64);
implFromToString!(char);
implFromToString!(&str);
implFromToString!(String);
implFromToString!(Box<str>);
implFromToString!(std::rc::Rc<str>);
implFromToString!(std::sync::Arc<str>);
implFromToString!(std::borrow::Cow<'_, str>);
implFromToString!(std::io::Error);
implFromToString!(Box<dyn std::error::Error>);

impl From<std::path::PathBuf> for Bytes {
    fn from(value: std::path::PathBuf) -> Self {
        let value = value.to_string_lossy();
        Self::from(value)
    }
}

impl From<&std::path::Path> for Bytes {
    fn from(value: &std::path::Path) -> Self {
        let value = value.to_string_lossy();
        Self::from(value)
    }
}

impl std::fmt::Debug for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Bytes")
            .field("buf", &self.strs_inner(..))
            .field("records", &self.records)
            .finish()
    }
}
