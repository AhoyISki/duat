use std::{
    iter::FusedIterator,
    ops::{Range, RangeBounds},
    str::Utf8Error,
    sync::LazyLock,
};

use gapbuf::GapBuffer;
use lender::{DoubleEndedLender, ExactSizeLender, Lender, Lending};

use super::{Point, RegexPattern, TextRange, records::Records};
use crate::{opts::PrintOpts, text::TextIndex};

static EMPTY_BYTES: LazyLock<Bytes> = LazyLock::new(Bytes::default);

/// The bytes of a [`Text`], encoded in UTF-8
///
/// [`Text`]: super::Text
#[derive(Default, Clone)]
pub struct Bytes {
    buf: GapBuffer<u8>,
    records: Records,
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
    /// want to check for the `InnerTags`'s possible emptyness as
    /// well, see [`Text::is_empty_empty`].
    ///
    /// [`Tag::Ghost`]: super::Ghost
    /// [`Text`]: super::Text
    /// [`Text::is_empty_empty`]: super::Text::is_empty_empty
    pub fn is_empty(&self) -> bool {
        let (s0, s1) = self.buf.as_slices();
        (s0 == b"\n" && s1 == b"") || (s0 == b"" && s1 == b"\n") || (s0 == b"" && s1 == b"")
    }

    /// The `char` at the [`Point`]'s position
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

    /// An [`Iterator`] over the bytes in a given _byte_ range
    ///
    /// Unlike [`strs`], this function works with _byte_ ranges, not
    /// [`TextRange`]s. That's because [`Strs`] is supposed to return
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

    /// An [`Iterator`] over the [`&str`]s of the [`Text`]
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
    #[track_caller]
    pub fn strs(&self, range: impl TextRange) -> Option<Strs<'_>> {
        let range = range.to_range(self.len().byte());
        Some(Strs {
            bytes: self,
            range: range.clone(),
            arr: self.strs_inner(range)?,
            fwd: 0,
            rev: 2,
        })
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
            match line_start == end {
                true => end,
                false => self.point_at_line((end.line() + 1).min(self.len().line())),
            }
        };

        // If the gap is outside of the range, we can just iterate through it
        // regularly
        let (fwd_i, rev_i) = (start.line(), end.line());
        if let Some(str) = self.get_contiguous(start..end) {
            let lines = [str.lines(), "".lines()];
            Lines::new(lines, None, fwd_i, rev_i)
        // If the gap is within the range, but on a line split, we
        // can just iterate through two sets of lines.
        } else if end.byte() > start.byte()
            && self.buf[self.buf.gap() - 1] != b'\n'
            && self.buf[self.buf.gap()] != b'\n'
        {
            let [s0, s1] = self.strs_inner(start.byte()..end.byte()).unwrap();
            let lines = [s0.lines(), s1.lines()];
            Lines::new(lines, None, fwd_i, rev_i)
            // Otherwise, the line that was split will need to be
            // allocated and returned separately.
        } else {
            let [s0, s1] = self.strs_inner(start.byte()..end.byte()).unwrap();

            let (before, split0) = match s0.rsplit_once('\n') {
                Some((before, split)) => (before, split),
                None => ("", s0),
            };
            let (after, split1) = match s1.split_once('\n') {
                Some((after, split)) => (after, split),
                None => ("", s1),
            };

            let lines = [before.lines(), after.lines()];
            let split_line = Some(split0.to_string() + split1);
            Lines::new(lines, split_line, fwd_i, rev_i)
        }
    }

    /// Returns the two `&str`s in the byte range.
    #[track_caller]
    fn strs_inner(&self, range: impl RangeBounds<usize>) -> Option<[&str; 2]> {
        let (start, end) = crate::utils::get_ends(range, self.len().byte());
        use std::str::from_utf8_unchecked;

        let (s0, s1) = self.buf.as_slices();

        // Check if the slices match utf8 boundaries.
        if s0.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || s1.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || self.buf.get(end).is_some_and(|b| utf8_char_width(*b) == 0)
        {
            return None;
        }

        Some(unsafe {
            let r0 = start.min(s0.len())..end.min(s0.len());
            let r1 = start.saturating_sub(s0.len()).min(s1.len())
                ..end.saturating_sub(s0.len()).min(s1.len());

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
    #[inline(always)]
    #[track_caller]
    pub fn line_range(&self, l: usize) -> Range<Point> {
        assert!(
            l <= self.len().line(),
            "line out of bounds: the len is {}, but the line is {l}",
            self.len().line()
        );

        let start = self.point_at_line(l);
        let end = self
            .chars_fwd(start..)
            .unwrap()
            .find_map(|(p, _)| (p.line() > start.line()).then_some(p))
            .unwrap_or(self.len());
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
        let strs = self.strs_inner(..).unwrap();
        let char = strs.into_iter().flat_map(str::chars).next_back().unwrap();
        self.len().rev(char)
    }

    /// A forward iterator of the [`char`]s of [`Bytes`]
    ///
    /// Each [`char`] will be accompanied by a [`Point`], which is the
    /// position where said character starts, e.g.
    /// [`Point::default()`] for the first character
    #[track_caller]
    pub fn chars_fwd(
        &self,
        range: impl TextRange,
    ) -> Option<impl Iterator<Item = (Point, char)> + '_> {
        let range = range.to_range(self.len().byte());
        let p = self.point_at_byte(range.start);
        Some(self.strs(range)?.chars().scan(p, |p, char| {
            let old_p = *p;
            *p = p.fwd(char);
            Some((old_p, char))
        }))
    }

    /// A reverse iterator of the [`char`]s in [`Bytes`]
    ///
    /// Each [`char`] will be accompanied by a [`Point`], which is the
    /// position where said character starts, e.g.
    /// [`Point::default()`] for the first character
    #[track_caller]
    pub fn chars_rev(
        &self,
        range: impl TextRange,
    ) -> Option<impl Iterator<Item = (Point, char)> + '_> {
        let range = range.to_range(self.len().byte());
        let p = self.point_at_byte(range.end);
        Some(self.strs(range)?.chars().rev().scan(p, |p, char| {
            *p = p.rev(char);
            Some((*p, char))
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

    /// Applies a [`Change`] to the [`GapBuffer`] within
    ///
    /// [`Change`]: super::Change
    pub(crate) fn apply_change(&mut self, change: super::Change<&str>) {
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
    pub(super) fn add_record(&mut self, [b, c, l]: [usize; 3]) {
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
pub struct Lines<'a> {
    lines: [std::str::Lines<'a>; 2],
    split_line: Option<String>,
    fwd_i: usize,
    rev_i: usize,
    split_line_used: bool,
}

impl<'a> Lines<'a> {
    fn new(
        lines: [std::str::Lines<'a>; 2],
        split_line: Option<String>,
        fwd_i: usize,
        rev_i: usize,
    ) -> Self {
        Self {
            lines,
            split_line,
            fwd_i,
            rev_i,
            split_line_used: false,
        }
    }
}

impl<'a, 'text> Lending<'a> for Lines<'text> {
    type Lend = (usize, &'a str);
}

impl<'a> Lender for Lines<'a> {
    fn next(&mut self) -> Option<lender::Lend<'_, Self>> {
        self.lines[0]
            .next()
            .or_else(|| {
                if self.split_line_used {
                    None
                } else {
                    self.split_line_used = true;
                    self.split_line.as_deref()
                }
            })
            .or_else(|| self.lines[1].next())
            .map(|line| {
                self.fwd_i += 1;
                (self.fwd_i - 1, line)
            })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.rev_i - self.fwd_i, Some(self.rev_i - self.fwd_i))
    }
}

impl<'a> DoubleEndedLender for Lines<'a> {
    fn next_back(&mut self) -> Option<lender::Lend<'_, Self>> {
        self.lines[1]
            .next_back()
            .or_else(|| {
                if self.split_line_used {
                    None
                } else {
                    self.split_line_used = true;
                    self.split_line.as_deref()
                }
            })
            .or_else(|| self.lines[0].next_back())
            .map(|line| {
                self.rev_i -= 1;
                (self.rev_i, line)
            })
    }
}

impl<'a> ExactSizeLender for Lines<'a> {}

/// An [`Iterator`] over the bytes in a [`Text`]
///
/// [`Text`]: super::Text
#[derive(Clone)]
pub struct Slices<'a>([std::slice::Iter<'a, u8>; 2]);

impl<'a> Slices<'a> {
    /// Converts this [`Iterator`] into an array of its two parts
    pub fn to_array(&self) -> [&'a [u8]; 2] {
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

impl<'a> Iterator for Slices<'a> {
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

impl<'a> ExactSizeIterator for Slices<'a> {}

impl<'a> DoubleEndedIterator for Slices<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0[1]
            .next_back()
            .or_else(|| self.0[0].next_back())
            .copied()
    }
}

/// An [`Iterator`] over the [`&str`]s in a [`Text`]
///
/// [`&str`]: str
/// [`Text`]: super::Text
#[derive(Clone)]
pub struct Strs<'a> {
    bytes: &'a Bytes,
    range: Range<usize>,
    arr: [&'a str; 2],
    fwd: usize,
    rev: usize,
}

impl<'a> Strs<'a> {
    /// Converts this [`Iterator`] into an array of its two parts
    pub fn to_array(&self) -> [&'a str; 2] {
        self.arr
    }

    /// Iterates over the [`char`]s of both [`&str`]s
    ///
    /// [`&str`]: str
    pub fn chars(self) -> impl DoubleEndedIterator<Item = char> + 'a {
        let [s0, s1] = self.arr;
        s0.chars().chain(s1.chars())
    }

    /// Returns `true` if the [`RegexPattern`] can be found in the
    /// [`Strs`]s
    pub fn contains<P: RegexPattern>(&self, pat: P) -> Result<bool, Box<regex_syntax::Error>> {
        self.bytes
            .search_fwd(pat, self.range.clone())
            .map(|mut iter| iter.next().is_some())
    }
}

impl Strs<'static> {
    /// An empty `Strs`, useful in some circumstances
    pub fn empty() -> Self {
        Self {
            bytes: &*EMPTY_BYTES,
            range: 0..0,
            arr: [""; 2],
            fwd: 0,
            rev: 0,
        }
    }
}

impl<'a> Iterator for Strs<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        match self.fwd {
            0 | 1 if self.fwd != self.rev => {
                self.fwd += 1;
                Some(self.arr[self.fwd - 1])
            }
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.rev - self.fwd, Some(self.rev - self.fwd))
    }
}

impl ExactSizeIterator for Strs<'_> {}

impl DoubleEndedIterator for Strs<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.rev {
            1 | 2 if self.fwd != self.rev => {
                self.rev -= 1;
                Some(self.arr[self.rev])
            }
            _ => None,
        }
    }
}

impl FusedIterator for Strs<'_> {}

impl AsRef<Bytes> for Bytes {
    fn as_ref(&self) -> &Bytes {
        self
    }
}

impl std::fmt::Display for Strs<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [s0, s1] = self.to_array();
        write!(f, "{s0}{s1}")
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

impl PartialEq for Bytes {
    fn eq(&self, other: &Self) -> bool {
        self.buf.as_slices() == other.buf.as_slices()
    }
}

impl PartialEq<&str> for Bytes {
    fn eq(&self, other: &&str) -> bool {
        let [s0, s1] = self.strs_inner(..).unwrap();
        other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
    }
}

impl PartialEq<String> for Bytes {
    fn eq(&self, other: &String) -> bool {
        let [s0, s1] = self.strs_inner(..).unwrap();
        other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
    }
}

impl PartialEq for Strs<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.to_array() == other.to_array()
    }
}

impl PartialEq<&str> for Strs<'_> {
    fn eq(&self, other: &&str) -> bool {
        let [s0, s1] = self.to_array();
        other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
    }
}

impl PartialEq<String> for Strs<'_> {
    fn eq(&self, other: &String) -> bool {
        let [s0, s1] = self.to_array();
        other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
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
