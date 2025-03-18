use std::{iter::FusedIterator, ops::RangeBounds, str::from_utf8_unchecked};

use gapbuf::GapBuffer;

use super::{Point, TextRange, records::Records, utf8_char_width};

#[derive(Default, Clone)]
pub struct Bytes {
    buf: GapBuffer<u8>,
    records: Records<[usize; 3]>,
}

impl Bytes {
    /// Returns a new instance of a [`Buffer`]
    pub(crate) fn new(string: &str) -> Self {
        let buf = GapBuffer::from_iter(string.bytes());

        let len = buf.len();
        let chars = unsafe {
            let (s0, s1) = buf.as_slices();
            std::str::from_utf8_unchecked(s0).chars().count()
                + std::str::from_utf8_unchecked(s1).chars().count()
        };
        let lines = buf.iter().filter(|b| **b == b'\n').count();
        Self {
            buf,
            records: Records::with_max([len, chars, lines]),
        }
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
        let [s0, s1] = self.strs(..).to_array();
        if point.byte() < s0.len() {
            s0[point.byte()..].chars().next()
        } else {
            s1[point.byte() - s0.len()..].chars().next()
        }
    }

    /// An [`Iterator`] over the bytes in a given [range]
    ///
    /// If the range is fully or partially out of bounds, one or both
    /// of the slices might be empty.
    ///
    /// [range]: TextRange
    pub fn buffers(&self, range: impl TextRange) -> Buffers {
        let range = range.to_range_fwd(self.buf.len());
        let (s0, s1) = self.buf.range(range).as_slices();
        Buffers([s0.iter(), s1.iter()])
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
    /// # use duat_core::text::{Point, Text};
    /// # let (p1, p2) = (Point::default(), Point::default());
    /// # let text = Text::new();
    /// buffer.strs_in((p1, p2)).flat_map(str::bytes);
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
    /// [`Text`]: super::Text
    /// [range]: TextRange
    /// [`strs`]: Self::strs
    pub fn strs(&self, range: impl TextRange) -> Strs {
        let range = range.to_range_fwd(self.buf.len());
        Strs(self.strs_in_range_inner(range).into_iter())
    }

    /// Returns an iterator over the lines in a given range
    ///
    /// The lines are inclusive, that is, it will iterate over the
    /// whole line, not just the parts within the range.
    ///
    /// # NOTE
    ///
    /// The reason why this requires mutable access is because we may
    /// need to move the [`GapBuffer`]'s gap in order to make the
    /// [range] contiguous for proper iteration.
    ///
    /// [range]: TextRange
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
        let lines = self.contiguous((start, end)).lines();
        let (fwd_i, rev_i) = (start.line(), end.line());
        TextLines { lines, fwd_i, rev_i }
    }

    /// Returns the two `&str`s in the byte range.
    fn strs_in_range_inner(&self, range: impl RangeBounds<usize>) -> [&str; 2] {
        let (s0, s1) = self.buf.as_slices();
        let (start, end) = crate::get_ends(range, self.buf.len());
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
    pub fn point_at(&self, b: usize) -> Point {
        assert!(
            b <= self.len().byte(),
            "byte out of bounds: the len is {}, but the byte is {b}",
            self.len().byte()
        );
        let [c_b, c_c, mut c_l] = self.records.closest_to(b);

        let found = if b >= c_b {
            let [s0, s1] = self.strs_in_range_inner(c_b..);

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
            self.strs_in_range_inner(..c_b)
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
    pub fn point_at_char(&self, c: usize) -> Point {
        assert!(
            c <= self.len().char(),
            "byte out of bounds: the len is {}, but the char is {c}",
            self.len().char()
        );
        let [c_b, c_c, mut c_l] = self.records.closest_to_by_key(c, |[_, c, _]| *c);

        let found = if c >= c_c {
            let [s0, s1] = self.strs_in_range_inner(c_b..);

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
            self.strs_in_range_inner(..)
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
    pub fn point_at_line(&self, l: usize) -> Point {
        assert!(
            l <= self.len().line(),
            "byte out of bounds: the len is {}, but the line is {l}",
            self.len().line()
        );
        let (c_b, c_c, mut c_l) = {
            let [mut b, mut c, l] = self.records.closest_to_by_key(l, |[.., l]| *l);
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

        let found = if l >= c_l {
            let [s0, s1] = self.strs_in_range_inner(c_b..);

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
            self.strs_in_range_inner(..c_b)
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
    /// # Panics
    ///
    /// Will panic if the number `l` is greater than the number of
    /// lines on the text
    #[inline(always)]
    pub fn points_of_line(&self, l: usize) -> [Point; 2] {
        assert!(
            l <= self.len().line(),
            "byte out of bounds: the len is {}, but the line is {l}",
            self.len().line()
        );

        let start = self.point_at_line(l);
        let end = self
            .chars_fwd(start)
            .find_map(|(p, _)| (p.line() > start.line()).then_some(p))
            .unwrap_or(start);
        [start, end]
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
        self.strs(..)
            .chars()
            .next_back()
            .map(|char| self.len().rev(char))
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

    ////////// Modification functions

    /// Applies a [`Change`] to the [`GapBuffer`] within
    ///
    /// [`Change`]: super::Change
    pub(super) fn apply_change(&mut self, change: super::Change<&str>) {
        let edit = change.added_text();
        let start = change.start();

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
    }

    /// Extends this [`Bytes`] with another
    pub(super) fn extend(&mut self, other: Self) {
        self.buf.extend(other.buf);
        self.records
            .transform(self.records.max(), [0, 0, 0], other.records.max())
    }

    /// Adds a record in the given position
    pub(super) fn add_record(&mut self, [b, c, l]: [usize; 3]) {
        self.records.insert([b, c, l]);
    }

    ////////// One str functions

    /// Gets a single [`&str`] from a given [range]
    ///
    /// This is the equivalent of calling
    /// [`Bytes::make_contiguous_in`] and [`Bytes::get_contiguous`].
    /// While this takes less space in code, calling the other two
    /// functions means that you won't be mutably borrowing the
    /// [`Bytes`] anymore, so if that matters to you, you should do
    /// that.
    ///
    /// [`&str`]: str
    /// [range]: TextRange
    pub fn contiguous(&mut self, range: impl TextRange) -> &str {
        self.make_contiguous(range.clone());
        self.get_contiguous(range).unwrap()
    }

    /// Moves the [`GapBuffer`]'s gap, so that the `range` is whole
    ///
    /// The return value is the value of the gap, if the second `&str`
    /// is the contiguous one.
    pub fn make_contiguous(&mut self, range: impl TextRange) {
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

    /// Assumes that the `range` given is contiguous in `self`
    ///
    /// You *MUST* call [`make_contiguous_in`] before using this
    /// function. The sole purpose of this function is to not keep the
    /// [`Bytes`] mutably borrowed.
    ///
    /// [`make_contiguous_in`]: Self::make_contiguous_in
    pub fn get_contiguous(&self, range: impl TextRange) -> Option<&str> {
        let range = range.to_range_fwd(self.len().byte());
        let [s0, s1] = self.strs(..).to_array();

        if range.end <= self.buf.gap() {
            s0.get(range)
        } else {
            let gap = self.buf.gap();
            s1.get(range.start - gap..range.end - gap)
        }
    }
}

impl std::fmt::Debug for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Bytes")
            .field("buf", &self.strs(..).to_array())
            .field("records", &self.records)
            .finish()
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

/// An [`Iterator`] over the bytes in a [`Text`]
///
/// [`Text`]: super::Text
#[derive(Clone)]
pub struct Buffers<'a>([std::slice::Iter<'a, u8>; 2]);

impl<'a> Buffers<'a> {
    /// Converts this [`Iterator`] into an array of its two parts
    pub fn to_array(&self) -> [&'a [u8]; 2] {
        self.0.clone().map(|iter| iter.as_slice())
    }
}

impl<'a> Iterator for Buffers<'a> {
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

impl<'a> ExactSizeIterator for Buffers<'a> {}

impl<'a> DoubleEndedIterator for Buffers<'a> {
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
pub struct Strs<'a>(std::array::IntoIter<&'a str, 2>);

impl<'a> Strs<'a> {
    /// Converts this [`Iterator`] into an array of its two parts
    pub fn to_array(&self) -> [&'a str; 2] {
        let strs = self.0.as_slice();
        [
            strs.first().copied().unwrap_or(""),
            strs.last().copied().unwrap_or(""),
        ]
    }

    /// Iterates over the [`char`]s of both [`&str`]s
    ///
    /// [`&str`]: str
    pub fn chars(self) -> impl DoubleEndedIterator<Item = char> + 'a {
        let [s0, s1] = self.to_array();
        s0.chars().chain(s1.chars())
    }
}

impl<'a> Iterator for Strs<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl ExactSizeIterator for Strs<'_> {}

impl DoubleEndedIterator for Strs<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl FusedIterator for Strs<'_> {}

impl std::fmt::Display for Strs<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [s0, s1] = self.to_array();
        write!(f, "{s0}{s1}")
    }
}
