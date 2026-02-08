//! The equivalent of [`String`] for Duat.
//!
//! You can't access this struct directly from Duat's API. This is
//! because it should only be modified alongside a [`Text`], which
//! would prevent the desynchronization of state with the
//! [`InnerTags`].
//!
//! Instead, you're only ever meant to access this through the
//! [`Strs`] struct, which is duat's equivalent of [`str`].
//!
//! [`Text`]: crate::text::Text
//! [`InnerTags`]: crate::text::InnerTags
use gap_buf::GapBuffer;

use crate::{
    buffer::Change,
    text::{
        strs::{Strs, line_ranges::LineRanges},
        utils::implPartialEq,
    },
};

/// The bytes of a [`Text`], encoded in UTF-8
///
/// [`Text`]: crate::text::Text
#[derive(Default, Clone)]
pub struct StrsBuf {
    pub(super) buf: GapBuffer<u8>,
    pub(super) line_ranges: LineRanges,
    version: u64,
}

impl StrsBuf {
    /// Returns a new instance of [`StrsBuf`]
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

        let slices = unsafe {
            let (s0, s1) = buf.as_slices();
            [str::from_utf8_unchecked(s0), str::from_utf8_unchecked(s1)]
        };

        let records = LineRanges::new(slices);

        Self { buf, line_ranges: records, version: 0 }
    }

    ////////// Modification functions

    /// Applies a [`Change`] to the [`GapBuffer`] within
    #[track_caller]
    pub(crate) fn apply_change(&mut self, change: Change<&str>) {
        assert!(
            self.len() + change.added_str().len() - change.taken_str().len() <= u32::MAX as usize,
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

        let array = unsafe {
            let (s0, s1) = self.buf.as_slices();
            [str::from_utf8_unchecked(s0), str::from_utf8_unchecked(s1)]
        };

        self.line_ranges
            .transform(start_rec, old_len, new_len, array);
    }

    /// Increment the version of the `StrsBuf` by 1
    pub fn increment_version(&mut self) {
        self.version += 1;
    }

    /// Get the current version of the `StrsBuf`
    pub fn get_version(&self) -> u64 {
        self.version
    }
}

impl std::ops::Deref for StrsBuf {
    type Target = Strs;

    fn deref(&self) -> &Self::Target {
        Strs::new(self, 0, self.buf.len() as u32)
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

impl Eq for StrsBuf {}
implPartialEq!(bytes: StrsBuf, other: StrsBuf, {
    let (l_s0, l_s1) = bytes.buf.as_slices();
    let (r_s0, r_s1) = other.buf.as_slices();
    (l_s0.len() + l_s1.len() == r_s0.len() + r_s1.len()) && l_s0.iter().chain(l_s1).eq(r_s0.iter().chain(r_s1))
});
implPartialEq!(bytes: StrsBuf, other: &str, {
    let [s0, s1] = bytes.to_array();
    other.len() == s0.len() + s1.len() && &other[..s0.len()] == s0 && &other[s0.len()..] == s1
});
implPartialEq!(bytes: StrsBuf, other: String, bytes == &&other.as_str());
implPartialEq!(str: &str, other: StrsBuf, other == *str);
implPartialEq!(string: String, other: StrsBuf, other == *string);

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

/// Implements [`From<$T>`] for [`StrsBuf`] where `$T: ToString`
macro_rules! implFromToString {
    ($T:ty) => {
        impl From<$T> for StrsBuf {
            fn from(value: $T) -> Self {
                let string = <$T as ToString>::to_string(&value);
                StrsBuf::new(&string)
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

impl From<std::path::PathBuf> for StrsBuf {
    fn from(value: std::path::PathBuf) -> Self {
        let value = value.to_string_lossy();
        Self::from(value)
    }
}

impl From<&std::path::Path> for StrsBuf {
    fn from(value: &std::path::Path) -> Self {
        let value = value.to_string_lossy();
        Self::from(value)
    }
}

impl std::fmt::Debug for StrsBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StrsBuf")
            .field("buf", &self[..].to_array())
            .field("records", &self.line_ranges)
            .finish()
    }
}

#[track_caller]
pub fn assert_utf8_boundary(bytes: &StrsBuf, idx: usize) {
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
