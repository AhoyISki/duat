use std::{ops::Range, sync::LazyLock};

use crate::text::{Bytes, Point, Slices, TextRange};

/// The [`&str`] equivalent for [`Bytes`]
///
/// [`&str`]: str
/// [`Text`]: crate::text::Text
// I need it to be like this so it is a wide pointer, which can be
// turned into a reference that can be used for indexing and std::ops::Deref.
#[repr(transparent)]
pub struct Strs([Bytes]);

impl Strs {
    /// Returns a new `Strs`.
    pub(super) fn new(bytes: &Bytes, start: u32, len: u32) -> &Self {
        // Safety: Since the ordering of fields shouldn't change, this should
        // be fine.
        let start_and_len = unsafe { std::mem::transmute::<[u32; 2], usize>([start, len]) };
        let ptr = std::ptr::slice_from_raw_parts(bytes as *const Bytes, start_and_len);

        // Safety: Since the address is valid, this should be converted to a
        // quasi wide pointer, where the metadata is size-like, but encodes
        // two values instead of one.
        unsafe { &*(ptr as *const Self) }
    }

    /// Tries to get a subslice of the [`Bytes`]
    ///
    /// It will return [`None`] if the range does not start or end in
    /// valid utf8 boundaries. If you expect the value to alway be
    /// `Some`, consider using the index operator (`[]`) instead.
    ///
    /// This method is conceptually similar to [`&str::get`], but the
    /// reference is to an [`Strs`] struct. This struct points to a
    /// subslice of the [`Bytes`]s, which is actually two slices,
    /// given the internal gap buffer representation.
    ///
    /// This type is supposed to act nearly identically with the
    /// [`str`] type, only differing in the fact that its maximum
    /// lenght is [`u32::MAX`], not [`usize::MAX`].
    ///
    /// If you wish to iterate over the lines, see [`Bytes::lines`].
    ///
    /// [`&str`]: str
    /// [`Text`]: super::Text
    /// [range]: TextRange
    /// [`strs`]: Self::strs
    /// [`&str::get`]: str::get
    pub fn get(&self, range: impl TextRange) -> Option<&Strs> {
        let formed = FormedStrs::new(self);
        let range = {
            let range = range.try_to_range(formed.len as usize)?;
            range.start + formed.start as usize..range.end + formed.start as usize
        };

        let (s0, s1) = formed.bytes.buf.range(range.clone()).as_slices();

        // Check if the slices match utf8 boundaries.
        if s0.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || s1.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || formed
                .bytes
                .buf
                .get(range.end)
                .is_some_and(|b| utf8_char_width(*b) == 0)
        {
            return None;
        }

        Some(Strs::new(
            formed.bytes,
            range.start as u32,
            range.len() as u32,
        ))
    }

    /// An empty `Strs`, useful in some circumstances.
    ///
    /// This is the equivalent of a literal `""`.
    pub fn empty() -> &'static Self {
        Strs::new(&EMPTY_BYTES, 0, 0)
    }

    /// Returns a struct of two `&[u8]` representing a [`TextRange`]
    /// from the slice.
    #[track_caller]
    pub fn slices(&self, range: impl TextRange) -> Slices<'_> {
        let formed = FormedStrs::new(self);
        let range = {
            let range = range.to_range(formed.len as usize);
            range.start + formed.start as usize..range.end + formed.start as usize
        };

        let (s0, s1) = formed.bytes.buf.range(range).as_slices();

        Slices([s0.iter(), s1.iter()])
    }

    /// Converts this `Strs` into an array of its two parts.
    pub fn to_array(&self) -> [&str; 2] {
        let formed = FormedStrs::new(self);
        let range = formed.start as usize..(formed.start + formed.len) as usize;

        let (s0, s1) = formed.bytes.buf.range(range).as_slices();

        // Safety: The creation of a &Strs necessitates a valid utf8 range.
        [unsafe { std::str::from_utf8_unchecked(s0) }, unsafe {
            std::str::from_utf8_unchecked(s1)
        }]
    }

    /// Returns and [`Iterator`] over the [`char`]s of both [`&str`]s.
    ///
    /// [`&str`]: str
    pub fn chars(&self) -> impl DoubleEndedIterator<Item = char> {
        self.to_array().into_iter().flat_map(str::chars)
    }

    /// Returns an [`Iterator`] over the [`char`]s and their indices.
    pub fn char_indices(&self) -> impl DoubleEndedIterator<Item = (usize, char)> {
        let [s0, s1] = self.to_array();

        s0.char_indices()
            .chain(s1.char_indices().map(move |(b, c)| (b + s0.len(), c)))
    }

    /// A [`Range<usize>`] of the bytes on this `Strs`.
    pub fn byte_range(&self) -> Range<usize> {
        let formed = FormedStrs::new(self);
        formed.start as usize..(formed.start + formed.len) as usize
    }

    /// A [`Range<Point>`] of the bytes on this `Strs`.
    ///
    /// If you just care about the byte indices (most likely), check
    /// out [`Strs::byte_range`] isntead.
    pub fn range(&self) -> Range<Point> {
        let formed = FormedStrs::new(self);
        let range = self.byte_range();

        formed.bytes.point_at_byte(range.start)..formed.bytes.point_at_byte(range.end)
    }

    /// The lenght of this `Strs`, in bytes.
    pub fn len(&self) -> usize {
        FormedStrs::new(self).len as usize
    }

    /// Wether the len of this `Strs` is equal to 0.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<Idx: TextRange> std::ops::Index<Idx> for Strs {
    type Output = Self;

    fn index(&self, index: Idx) -> &Self::Output {
        let formed = FormedStrs::new(self);
        let range = index.to_range(formed.len as usize);

        super::assert_utf8_boundary(formed.bytes, range.start);
        super::assert_utf8_boundary(formed.bytes, range.end);

        Self::new(
            formed.bytes,
            range.start as u32 + formed.start,
            range.len() as u32,
        )
    }
}

/// A deconstructed internal representation of an [`Strs`],
/// useful for not doing a bunch of unsafe operations.
struct FormedStrs<'b> {
    bytes: &'b Bytes,
    start: u32,
    len: u32,
}

impl<'b> FormedStrs<'b> {
    fn new(strs: &'b Strs) -> Self {
        let ptr = strs as *const Strs as *const [Bytes];
        // Safety: Since the len was created by the inverted operation, this
        // should be fine.
        let [start, len] = unsafe { std::mem::transmute::<usize, [u32; 2]>(ptr.len()) };

        Self {
            // Safety: When creating the Strs, a valid Bytes was at the address.
            bytes: unsafe { &*(ptr as *const Bytes) },
            start,
            len,
        }
    }
}

impl std::fmt::Display for Strs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [s0, s1] = self.to_array();
        write!(f, "{s0}{s1}")
    }
}

static EMPTY_BYTES: LazyLock<Bytes> = LazyLock::new(Bytes::default);

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
