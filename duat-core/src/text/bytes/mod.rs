use std::str::Utf8Error;

use gap_buf::GapBuffer;

pub use crate::text::bytes::strs::{Lines, Strs};
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
    #[track_caller]
    pub(crate) fn new(string: &str) -> Self {
        assert!(
            string.len() <= u32::MAX as usize,
            "For now, you can't have a Text larger than u32::MAX"
        );
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

    /// The `char` at the [`Point`]'s position
    pub fn char_at(&self, p: impl TextIndex) -> Option<char> {
        if p.to_byte_index() >= self.len().byte() {
            return None;
        }

        let [s0, s1] = self.to_array();
        Some(if p.to_byte_index() < s0.len() {
            s0[p.to_byte_index()..].chars().next().unwrap()
        } else {
            s1[p.to_byte_index() - s0.len()..]
                .chars()
                .next()
                .unwrap_or_else(|| panic!("{self:#?}"))
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
            let [s0, s1] = self[c_b..].to_array();

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
            self[..c_b]
                .to_array()
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
            let [s0, s1] = self[c_b..].to_array();

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
            self[..c_b]
                .to_array()
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
            let (b, c) = self[..b]
                .to_array()
                .into_iter()
                .flat_map(str::chars)
                .rev()
                .take_while(|c| *c != '\n')
                .fold((b, c), |(b, c), char| (b - char.len_utf8(), c - 1));
            (b, c, l)
        };

        let found = if l >= c_l {
            let [s0, s1] = self[c_b..].to_array();

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
            self[..c_b]
                .to_array()
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

    /// The [`Strs`] for the `n`th line
    ///
    /// # Panics
    ///
    /// Panics if `n >= self.len().line()`
    #[track_caller]
    pub fn line(&self, n: usize) -> &Strs {
        assert!(
            n < self.len().line(),
            "line out of bounds: the len is {}, but the line is {n}",
            self.len().line()
        );

        let start = self.point_at_line(n).byte();
        let len = self[start..]
            .to_array()
            .into_iter()
            .flat_map(str::bytes)
            .take_while(|&byte| byte != b'\n')
            .count();

        &self[start..start + (len + 1)]
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
        Some(self.get(range.clone())?.chars().map(move |char| {
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
        Some(self.get(range.clone())?.chars().rev().map(move |char| {
            range.end -= char.len_utf8();
            (range.end, char)
        }))
    }

    /// Gets the indentation level on a given line
    ///
    /// This is the total "amount of spaces", that is, how many `' '`
    /// character equivalents are here. This depends on your
    /// [`PrintOpts`] because of the `tabstop` field.
    #[track_caller]
    pub fn indent(&self, line: usize, opts: PrintOpts) -> usize {
        let point = self.point_at_line(line);
        self.line(point.line())
            .chars()
            .take_while(|&char| char == ' ' || char == '\t')
            .fold(0, |sum, char| {
                if char == ' ' {
                    sum + 1
                } else {
                    sum + opts.tabstop as usize - (opts.tabstop as usize % sum)
                }
            })
    }

    ////////// Modification functions

    /// Applies a [`Change`] to the [`GapBuffer`] within
    #[track_caller]
    pub(crate) fn apply_change(&mut self, change: Change<&str>) {
        assert!(
            self.len().byte() + change.added_str().len() - change.taken_str().len()
                <= u32::MAX as usize,
            "For now, you can't have a Text larger than u32::MAX"
        );

        assert_utf8_boundary(self, change.start().byte());
        assert_utf8_boundary(self, change.taken_end().byte());

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
        let [s0, s1] = self.to_array();

        if range.end <= self.buf.gap() {
            s0.get(range)
        } else {
            let gap = self.buf.gap();
            s1.get(range.start.checked_sub(gap)?..range.end.checked_sub(gap)?)
        }
    }
}

impl std::ops::Deref for Bytes {
    type Target = Strs;

    fn deref(&self) -> &Self::Target {
        Strs::new(self, 0, self.len().byte() as u32)
    }
}

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
    let [s0, s1] = bytes.to_array();
    other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
});
implPartialEq!(bytes: Bytes, other: String, bytes == &&other.as_str());
implPartialEq!(str: &str, other: Bytes, other == *str);
implPartialEq!(string: String, other: Bytes, other == *string);

impl Eq for &Strs {}
implPartialEq!(strs: &Strs, other: &Strs, {
    let [l_s0, l_s1] = strs.to_array();
    let [r_s0, r_s1] = other.to_array();
    (l_s0.len() + l_s1.len() == r_s0.len() + r_s1.len()) && l_s0.bytes().chain(l_s1.bytes()).eq(r_s0.bytes().chain(r_s1.bytes()))
});
implPartialEq!(strs: &Strs, other: &str, {
    let [s0, s1] = strs.to_array();
    other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
});
implPartialEq!(strs: &Strs, other: String, strs == &&other.as_str());
implPartialEq!(str: &str, other: &Strs, other == *str);
implPartialEq!(string: String, other: &Strs, other == *string);

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
            .field("buf", &self[..].to_array())
            .field("records", &self.records)
            .finish()
    }
}

#[track_caller]
fn assert_utf8_boundary(bytes: &Bytes, idx: usize) {
    assert!(
        bytes.buf.get(idx).is_none_or(|b| utf8_char_width(*b) != 0),
        "byte index {} is not a valid char boundary; it is inside '{}'",
        idx,
        {
            let (n, len) = bytes
                .buf
                .range(..idx)
                .iter()
                .rev()
                .enumerate()
                .find_map(|(i, &b)| (utf8_char_width(b) != 0).then_some((i, utf8_char_width(b))))
                .unwrap();

            String::from_utf8(
                bytes
                    .buf
                    .range(idx - (n + 1)..idx - (n + 1) + len)
                    .iter()
                    .copied()
                    .collect(),
            )
            .unwrap()
        }
    );
}
