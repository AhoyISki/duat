use std::{ops::Range, sync::LazyLock};

use crate::text::{Bytes, Slices, TextRange};

/// A slice over the two `&str`s of a [`Bytes`].
///
/// [`&str`]: str
/// [`Text`]: crate::text::Text
#[derive(Clone, Copy)]
pub struct Strs<'b> {
    bytes: &'b Bytes,
    range: (usize, usize),
    arr: [&'b str; 2],
}

impl<'b> Strs<'b> {
    /// Returns a new `Strs`.
    pub(super) fn new(bytes: &'b Bytes, range: (usize, usize), arr: [&'b str; 2]) -> Self {
        Self { bytes, range, arr }
    }

    /// A subslice of the [`Strs`].
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
    /// [`Text`]: crate::text::Text
    /// [range]: TextRange
    /// [`strs`]: Self::strs
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn strs(&self, range: impl TextRange) -> Option<Strs<'_>> {
        let range = {
            let range = range.to_range(self.range.1);
            range.start.max(self.range.0)..range.end
        };

        Some(Strs {
            bytes: self.bytes,
            range: (range.start, range.end),
            arr: self.bytes.strs_inner(range)?,
        })
    }

    /// Returns two `&[u8]` representing a [`TextRange`] from the
    /// slice.
    ///
    /// Note that this `TextRange` is relative to the whole [`Bytes`]
    /// struct, not just this [`Strs`]. This method is also not
    /// panicking, i.e., if the range doesn't neatly fit into the
    /// `Strs`, then the parts that don't fit will simply not be
    /// included.
    pub fn slices(&self, range: impl TextRange) -> Slices<'b> {
        let range = {
            let range = range.to_range(self.range.1);
            range.start.max(self.range.0)..range.end
        };
        let (s0, s1) = self.bytes.buf.range(range).as_slices();

        Slices([s0.iter(), s1.iter()])
    }

    /// Converts this [`Iterator`] into an array of its two parts.
    pub fn to_array(&self) -> [&'b str; 2] {
        self.arr
    }

    /// Returns and [`Iterator`] over the [`char`]s of both [`&str`]s.
    ///
    /// [`&str`]: str
    pub fn chars(self) -> impl DoubleEndedIterator<Item = char> + 'b {
        let [s0, s1] = self.arr;
        s0.chars().chain(s1.chars())
    }

    /// Returns an [`Iterator`] over the [`char`]s and their indices
    /// from both [`&str`]s.
    ///
    /// [`&str`]: str
    pub fn char_indices(self) -> impl DoubleEndedIterator<Item = (usize, char)> + 'b {
        let [s0, s1] = self.arr;
        s0.char_indices()
            .map(move |(b, c)| (b + self.range.0, c))
            .chain(
                s1.char_indices()
                    .map(move |(b, c)| (b + self.range.0 + s0.len(), c)),
            )
    }

    /// A [`Range<usize>`] of the byte indices of this `Strs`.
    pub fn byte_range(&self) -> Range<usize> {
        self.range.0..self.range.1
    }

    /// The underlying [`Bytes`] struct.
    ///
    /// You can use this method to essentially get a [`Strs`] that
    /// encompasses the whole range, since [`Bytes`] implements
    /// [`Deref<Target = Strs>`] with a full range.
    ///
    /// [`Deref<Target = Strs>`]: std::ops::Deref
    pub fn bytes(&self) -> &Bytes {
        self.bytes
    }

    /// The lenght of this `Strs`, in bytes.
    pub fn len(&self) -> usize {
        self.range.1 - self.range.0
    }

	/// Wether the len of this `Strs` is equal to 0.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Strs<'static> {
    /// An empty `Strs`, useful in some circumstances.
    pub fn empty() -> Self {
        Self {
            bytes: &*EMPTY_BYTES,
            range: (0, 0),
            arr: [""; 2],
        }
    }
}

impl std::fmt::Display for Strs<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [s0, s1] = self.to_array();
        write!(f, "{s0}{s1}")
    }
}

static EMPTY_BYTES: LazyLock<Bytes> = LazyLock::new(Bytes::default);
