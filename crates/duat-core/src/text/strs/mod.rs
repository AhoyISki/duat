//! The equivalent of [`str`] for Duat.
//!
//! This module defines the [`Strs`] struct, which is a type that,
//! much like `str`, only shows up as `&Strs`. This is the only avenue
//! of direct interaction that the end user will get in regards to the
//! bytes of the [`Text`].
//!
//! The `Strs` are backed by an [`StrsBuf`] struct, which owns the
//! allocation of the [`GapBuffer`] of the utf-8 text.
//!
//! [`Text`]: super::Text
//! [`StrsBuf`]: buf::StrsBuf
//! [`GapBuffer`]: gap_buf::GapBuffer
use std::{ops::Range, sync::LazyLock};

pub use pattern::{
    DoubleEndedSearcher as StrsDoubleEndedSearcher, Pattern as StrsPattern, SearchStep,
    Searcher as StrsSearcher,
};

pub(crate) use crate::text::strs::buf::StrsBuf;
use crate::{
    opts::PrintOpts,
    text::{
        Point, TextIndex, TextRange,
        strs::{
            buf::assert_utf8_boundary,
            iter::{
                CharIndices, MatchRanges, MatchRangesInternal, Matches, MatchesInternal,
                RMatchRanges, RMatches, RSplit, RSplitN, RSplitTerminator, Split, SplitInclusive,
                SplitInternal, SplitN, SplitNInternal, SplitTerminator,
            },
        },
    },
};

mod buf;
mod iter;
mod line_ranges;
mod memchr;
mod pattern;

/// The [`str`] equivalent for Duat.
///
/// Due to Duat's use of a [gap buffer] to represent the [`Text`],
/// this struct doesn't represent just one slice like a `&str` does.
/// Instead, it represents two slices (either being possibly empty).
///
/// This is done by making use of a custom dynamically sized type,
/// allowing for near identical utilization.
///
/// [gap buffer]: https://en.wikipedia.org/wiki/Gap_buffer
/// [`Text`]: super::Text
// I need it to be like this so it is a wide pointer, which can be
// turned into a reference that can be used for indexing and std::ops::Deref.
#[repr(transparent)]
pub struct Strs(StrsDST);

impl Strs {
    /// Returns a new `Strs`.
    pub(super) fn new(bytes: &StrsBuf, start: u32, len: u32) -> &Self {
        // Safety: Since the ordering of fields shouldn't change, this should
        // be fine.
        let start_and_len = unsafe { std::mem::transmute::<[u32; 2], usize>([start, len]) };
        let ptr = std::ptr::slice_from_raw_parts(bytes as *const StrsBuf, start_and_len);

        // Safety: Since the address is valid, this should be converted to a
        // quasi wide pointer, where the metadata is size-like, but encodes
        // two values instead of one.
        unsafe { &*(ptr as *const Self) }
    }

    ////////// Querying functions

    /// The [`Point`] at the end of the `Strs`.
    ///
    /// This is the equivalent of `strs.range().start`.
    pub fn start_point(&self) -> Point {
        let formed = FormedStrs::new(self);
        if formed.start == 0 {
            Point::default()
        } else {
            formed
                .buf
                .line_ranges
                .point_by_key(formed.start as usize, |[b, _]| b, &formed.buf.gapbuf)
                .unwrap_or_else(|| formed.buf.line_ranges.max(&formed.buf.gapbuf))
        }
    }

    /// The [`Point`] at the end of the `Strs`.
    ///
    /// This is the equivalent of `strs.range().end`.
    pub fn end_point(&self) -> Point {
        let formed = FormedStrs::new(self);

        let byte = formed.start as usize + formed.len as usize;
        if byte == formed.buf.gapbuf.len() {
            formed.buf.line_ranges.max(&formed.buf.gapbuf)
        } else {
            formed
                .buf
                .line_ranges
                .point_by_key(byte, |[b, _]| b, &formed.buf.gapbuf)
                .unwrap_or_else(|| formed.buf.line_ranges.max(&formed.buf.gapbuf))
        }
    }

    /// The `char` at a given position.
    ///
    /// This position can either be a [`Point`] or a byte index. Will
    /// return [`None`] if the position is greater than or equal to
    /// `self.len()` or if it is not located in a utf8 character
    /// boundary.
    #[track_caller]
    pub fn char_at(&self, p: impl TextIndex) -> Option<char> {
        let formed = FormedStrs::new(self);
        let range = formed
            .buf
            .gapbuf
            .range(formed.start as usize..formed.start as usize + formed.len as usize);

        if range
            .get(p.to_byte_index())
            .is_none_or(|b| utf8_char_width(*b) == 0)
        {
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

    /// The [`Point`] corresponding to the byte position, 0 indexed.
    ///
    /// If the byte position would fall in between two characters
    /// (because the first one comprises more than one byte), the
    /// first character is chosen as the [`Point`] where the byte is
    /// located.
    ///
    /// # Panics
    ///
    /// Will panic if `b` is greater than the length of the text.
    #[inline(always)]
    #[track_caller]
    pub fn point_at_byte(&self, byte: usize) -> Point {
        assert!(
            byte <= self.len(),
            "byte out of bounds: the len is {}, but the byte is {byte}",
            self.len()
        );

        let formed = FormedStrs::new(self);

        if byte == self.len() {
            self.end_point()
        } else if byte == 0 {
            Point::default()
        } else {
            formed
                .buf
                .line_ranges
                .point_by_key(byte + formed.start as usize, |[b, _]| b, &formed.buf.gapbuf)
                .unwrap()
        }
    }

    /// The [`Point`] associated with the `s`th char.
    ///
    /// # Panics
    ///
    /// Will panic if `s` is greater than the number of chars in the
    /// text.
    #[inline(always)]
    #[track_caller]
    pub fn point_at_char(&self, char: usize) -> Point {
        let end_point = self.end_point();
        assert!(
            char <= end_point.char(),
            "char out of bounds: the len is {}, but the char is {char}",
            end_point.char()
        );

        let formed = FormedStrs::new(self);

        if char == end_point.char() {
            end_point
        } else {
            let start = formed
                .buf
                .line_ranges
                .point_by_key(formed.start as usize, |[b, _]| b, &formed.buf.gapbuf)
                .unwrap();

            formed
                .buf
                .line_ranges
                .point_by_key(start.char() + char, |[_, s]| s, &formed.buf.gapbuf)
                .unwrap()
        }
    }

    /// Returns the [`Point`] at a certain `line` and `column` offset.
    ///
    /// The `column` offset is measured in characters.
    ///
    /// # Panics
    ///
    /// Panics if `line > self.end_point().line()`. If you want an
    /// [`Option`] return instead of a panic, checkout
    /// [`Strs::get_point_at_coords`].
    #[inline(always)]
    #[track_caller]
    pub fn point_at_coords(&self, line: usize, column: usize) -> Point {
        let end_point = self.end_point();
        assert!(
            line <= end_point.line(),
            "line out of bounds: the len is {}, but the line is {line}",
            end_point.line()
        );

        self.get_point_at_coords(line, column).unwrap()
    }

    /// Gets a [`Point`] from coordinates.
    ///
    /// Returns [`None`] if the `line > self.end_point().line()`. If
    /// you'd rather it panic instead, checkout
    /// [`Strs::point_at_coords`].
    pub fn get_point_at_coords(&self, line: usize, column: usize) -> Option<Point> {
        let range = self.range();
        if line > range.end.line() - range.start.line() {
            return None;
        }

        let formed = FormedStrs::new(self);

        let slices = unsafe {
            let (s0, s1) = formed.buf.gapbuf.as_slices();
            [str::from_utf8_unchecked(s0), str::from_utf8_unchecked(s1)]
        };
        let line = {
            let start = self.point_at_byte(formed.start as usize);
            start.line() + line
        };
        let column = if line == 0 && range.start != Point::default() {
            range.start.char_col(formed.buf) + column
        } else {
            column
        };

        let point = formed.buf.line_ranges.point_at_coords(line, column, slices);

        point.filter(|p| *p < range.end).or_else(|| {
            if line + 1 >= range.end.line() - range.start.line() {
                self.last_point()
            } else {
                formed
                    .buf
                    .line_ranges
                    .point_at_coords(line + 1, 0, slices)
                    .map(|point| point.rev('\n'))
            }
        })
    }

    /// The `Strs` for the `n`th line.
    ///
    /// # Panics
    ///
    /// Panics if `n >= self.len().line()`
    #[track_caller]
    pub fn line(&self, n: usize) -> &Strs {
        let end_point = self.end_point();
        assert!(
            n <= end_point.line(),
            "line out of bounds: the len is {}, but the line is {n}",
            end_point.line()
        );

        let start = self.point_at_coords(n, 0);
        let end = if n + 1 >= end_point.line() {
            end_point
        } else {
            self.point_at_coords(n + 1, 0)
        };

        &self[start..end]
    }

    /// Returns an `Strs` for the whole [`Text`] that this one belongs
    /// to.
    ///
    /// You should use this function if you want to create an api that
    /// requires the whole `Strs` to be used as an argument. You can
    /// accept any `Strs`, then just transform it to the full one
    /// using this function.
    ///
    /// If this `Strs` is from `Strs::empty`, then this will
    /// return itself.
    ///
    /// [`Text`]: super::Text
    pub fn full(&self) -> &Strs {
        FormedStrs::new(self).buf
    }

    /// The last [`Point`] associated with a `char`
    ///
    /// Returns `None` if the `Strs` is empty.
    ///
    /// [`len`]: Self::len
    /// [`Text`]: crate::text::Text
    pub fn last_point(&self) -> Option<Point> {
        Some(self.end_point().rev(self.chars().next_back()?))
    }

    /// Tries to get a subslice of the `Strs`
    ///
    /// It will return [`None`] if the range does not start or end in
    /// valid utf8 boundaries. If you expect the value to alway be
    /// `Some`, consider using the index operator (`[]`) instead.
    ///
    /// This method is conceptually similar to [`&str::get`], but the
    /// reference is to an `Strs` struct. This struct points to a
    /// subslice of the `Strs`s, which is actually two slices,
    /// given the internal gap buffer representation.
    ///
    /// This type is supposed to act nearly identically with the
    /// [`str`] type, only differing in the fact that its maximum
    /// lenght is [`u32::MAX`], not [`usize::MAX`].
    ///
    /// [`&str`]: str
    /// [`Text`]: crate::text::Text
    /// [range]: TextRange
    /// [`&str::get`]: str::get
    pub fn get(&self, range: impl TextRange) -> Option<&Strs> {
        let formed = FormedStrs::new(self);
        let range = {
            let range = range.try_to_range(formed.len as usize)?;
            range.start + formed.start as usize..range.end + formed.start as usize
        };

        let (s0, s1) = formed.buf.gapbuf.range(range.clone()).as_slices();

        // Check if the slices match utf8 boundaries.
        if s0.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || s1.first().is_some_and(|b| utf8_char_width(*b) == 0)
            || formed
                .buf
                .gapbuf
                .get(range.end)
                .is_some_and(|b| utf8_char_width(*b) == 0)
        {
            return None;
        }

        Some(Strs::new(
            formed.buf,
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
    #[inline]
    pub fn slices(&self, range: impl TextRange) -> [&[u8]; 2] {
        let formed = FormedStrs::new(self);
        let range = {
            let range = range.to_range(formed.len as usize);
            range.start + formed.start as usize..range.end + formed.start as usize
        };

        let (s0, s1) = formed.buf.gapbuf.range(range).as_slices();
        [s0, s1]
    }

    /// Converts this `Strs` into an array of its two parts.
    pub fn to_array(&self) -> [&str; 2] {
        let formed = FormedStrs::new(self);
        let range = formed.start as usize..(formed.start + formed.len) as usize;

        let (s0, s1) = formed.buf.gapbuf.range(range).as_slices();

        // Safety: The creation of a &Strs necessitates a valid utf8 range.
        [unsafe { std::str::from_utf8_unchecked(s0) }, unsafe {
            std::str::from_utf8_unchecked(s1)
        }]
    }

    /// Returns and [`Iterator`] over the [bytes] of this `Strs`
    ///
    /// [bytes]: u8
    #[inline]
    pub fn bytes(&self) -> impl DoubleEndedIterator<Item = u8> + Clone {
        self.slices(..)
            .into_iter()
            .flat_map(|slice| slice.iter().copied())
    }

    /// Returns and [`Iterator`] over the [`char`]s of this `Strs`
    pub fn chars(&self) -> impl DoubleEndedIterator<Item = char> + Clone {
        self.to_array().into_iter().flat_map(str::chars)
    }

    /// Returns an [`Iterator`] over the [`char`]s and their indices.
    pub fn char_indices(&self) -> CharIndices<'_> {
        let [s0, s1] = self.to_array();
        CharIndices::new(s0.len(), s0.char_indices(), s1.char_indices())
    }

    /// Checks that `index`-th byte is the first byte in a UTF-8 code
    /// point sequence or the end of the `Strs`.
    ///
    /// The start, end and gap of the `Strs` (when `index ==
    /// self.len()` or `index == self.to_array()[0].len()`) are
    /// considered to be boundaries.
    ///
    /// Returns `false` if `index` is greater than `self.len()`.
    pub fn is_char_boundary(&self, index: usize) -> bool {
        let [s0, s1] = self.to_array();
        if index > s0.len() {
            s1.is_char_boundary(index - s0.len())
        } else if index < s0.len() {
            s0.is_char_boundary(index)
        } else {
            true
        }
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

        formed.buf.point_at_byte(range.start)..formed.buf.point_at_byte(range.end)
    }

    /// Gets the indentation level of this `Strs`
    ///
    /// This assumes that it is a line in the `Strs`, ending with
    /// `\n` or `\r\n`.
    ///
    /// This is the total "amount of spaces", that is, how many `' '`
    /// character equivalents are here. This depends on your
    /// [`PrintOpts`] because of the `tabstop` field.
    #[track_caller]
    #[inline]
    pub fn indent(&self, opts: PrintOpts) -> usize {
        self.bytes()
            .take_while(|&byte| byte == b' ' || byte == b'\t')
            .fold(0, |sum, byte| {
                if byte == b' ' {
                    sum + 1
                } else {
                    sum + opts.tabstop as usize - (sum % opts.tabstop as usize)
                }
            })
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

    /// Get the current version of the `StrsBuf`
    ///
    /// This version is irrespective of undos/redos, that is, an undo
    /// will also bump the version number up. You can use this for
    /// communicating version changes, such as with an LSP.
    pub fn version(&self) -> u64 {
        FormedStrs::new(self).buf.version()
    }

    ////////// Pattern methods.

    /// Splits a `&Strs` by whitespace.
    ///
    /// The iterator returned will return `&Strs` that are sub-slices
    /// of the original `&Strs`, separated by any amount of
    /// whitespace.
    ///
    /// 'Whitespace' is defined according to the terms of the Unicode
    /// Derived Core Property `White_Space`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let text = Text::from("A few words");
    /// let mut iter = text.split_whitespace();
    ///
    /// assert!(iter.next().is_some_and(|s| s == "A"));
    /// assert!(iter.next().is_some_and(|s| s == "few"));
    /// assert!(iter.next().is_some_and(|s| s == "words"));
    ///
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// All kinds of whitespace are considered:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let text = Text::from(" Mary   had\ta\u{2009}little  \n\t lamb");
    /// let mut iter = text.split_whitespace();
    /// assert!(iter.next().is_some_and(|s| s == "Mary"));
    /// assert!(iter.next().is_some_and(|s| s == "had"));
    /// assert!(iter.next().is_some_and(|s| s == "a"));
    /// assert!(iter.next().is_some_and(|s| s == "little"));
    /// assert!(iter.next().is_some_and(|s| s == "lamb"));
    ///
    /// assert_eq!(None, iter.next());
    /// ```
    ///
    /// If the string is empty or all whitespace, the iterator yields
    /// no `&Strs`s:
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert_eq!(Text::from("").split_whitespace().next(), None);
    /// assert_eq!(Text::from("   ").split_whitespace().next(), None);
    /// ```
    #[must_use = "this returns the split string as an iterator, without modifying the original"]
    #[inline]
    pub fn split_whitespace(&self) -> impl DoubleEndedIterator<Item = &Strs> + Clone {
        self.split(|char: char| char.is_whitespace())
            .filter(|strs| !strs.is_empty())
    }

    /// Returns an iterator over the lines of a `&Strs`, as other
    /// `&Strs`.
    ///
    /// Lines are split at line endings that are either newlines
    /// (`\n`) or sequences of a carriage return followed by a
    /// line feed (`\r\n`).
    ///
    /// Line terminators are not included in the lines returned by the
    /// iterator.
    ///
    /// Note that any carriage return (`\r`) not immediately followed
    /// by a line feed (`\n`) does not split a line. These
    /// carriage returns are thereby included in the produced
    /// lines.
    ///
    /// The final line ending is optional. A string that ends with a
    /// final line ending will return the same lines as an
    /// otherwise identical string without a final line ending.
    ///
    /// An empty string returns an empty iterator.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let text = Text::from("foo\r\nbar\n\nbaz\r");
    /// let mut lines = text.lines();
    ///
    /// assert!(lines.next().is_some_and(|s| s == "foo"));
    /// assert!(lines.next().is_some_and(|s| s == "bar"));
    /// assert!(lines.next().is_some_and(|s| s == ""));
    /// // Trailing carriage return is included in the last line
    /// assert!(lines.next().is_some_and(|s| s == "baz\r"));
    ///
    /// assert_eq!(lines.next(), None);
    /// ```
    ///
    /// The final line does not require any ending:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let text = Text::from("foo\nbar\n\r\nbaz");
    /// let mut lines = text.lines();
    ///
    /// assert!(lines.next().is_some_and(|s| s == "foo"));
    /// assert!(lines.next().is_some_and(|s| s == "bar"));
    /// assert!(lines.next().is_some_and(|s| s == ""));
    /// assert!(lines.next().is_some_and(|s| s == "baz"));
    ///
    /// assert_eq!(lines.next(), None);
    /// ```
    ///
    /// An empty string returns an empty iterator:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let text = Text::from("");
    /// let mut lines = text.lines();
    ///
    /// assert_eq!(lines.next(), None);
    /// ```
    #[inline]
    pub fn lines(&self) -> Lines<'_> {
        Lines(self.split_inclusive('\n'))
    }

    /// Returns `true` if the given pattern matches a sub-slice of
    /// this `&Strs`.
    ///
    /// Returns `false` if it does not.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let bananas = Text::from("bananas");
    ///
    /// assert!(bananas.contains("nana"));
    /// assert!(!bananas.contains("apples"));
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[inline]
    pub fn contains<P: StrsPattern>(&self, pat: P) -> bool {
        pat.into_searcher(self).next_match().is_some()
    }

    /// Returns `true` if the given pattern matches a prefix of this
    /// `&Strs`.
    ///
    /// Returns `false` if it does not.
    ///
    /// The [pattern] can be a `&str`, in which case this function
    /// will return true if the `&str` is a prefix of this `&Strs`.
    ///
    /// The [pattern] can also be a [`char`], a slice of [`char`]s, or
    /// a function or closure that determines if a character
    /// matches. These will only be checked against the first
    /// character of this `&Strs`. Look at the second example
    /// below regarding behavior for slices of [`char`]s.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let bananas = Text::from("bananas");
    ///
    /// assert!(bananas.starts_with("bana"));
    /// assert!(!bananas.starts_with("nana"));
    /// ```
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let bananas = Text::from("bananas");
    ///
    /// // Note that both of these assert successfully.
    /// assert!(bananas.starts_with(&['b', 'a', 'n', 'a']));
    /// assert!(bananas.starts_with(&['a', 'b', 'c', 'd']));
    /// ```
    ///
    /// [pattern]: StrsPattern
    pub fn starts_with<P: StrsPattern>(&self, pat: P) -> bool {
        matches!(pat.into_searcher(self).next(), SearchStep::Match(..))
    }

    /// Returns `true` if the given pattern matches a suffix of this
    /// `&Strs`.
    ///
    /// Returns `false` if it does not.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let bananas = Text::from("bananas");
    ///
    /// assert!(bananas.ends_with("anas"));
    /// assert!(!bananas.ends_with("nana"));
    /// ```
    ///
    /// [pattern]: StrsPattern
    pub fn ends_with<P: StrsPattern>(&self, pat: P) -> bool
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        matches!(pat.into_searcher(self).next_back(), SearchStep::Match(..))
    }

    /// Returns the byte index of the first character of this string
    /// slice that matches the pattern.
    ///
    /// Returns [`None`] if the pattern doesn't match.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("Löwe 老虎 Léopard Gepardi");
    ///
    /// assert_eq!(s.find('L'), Some(0..1));
    /// assert_eq!(s.find('é'), Some(14..16));
    /// assert_eq!(s.find("pard"), Some(17..21));
    /// ```
    ///
    /// More complex patterns using point-free style and closures:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("Löwe 老虎 Léopard");
    ///
    /// assert_eq!(s.find(char::is_whitespace), Some(5..6));
    /// assert_eq!(s.find(char::is_lowercase), Some(1..3));
    /// assert_eq!(
    ///     s.find(|c: char| c.is_whitespace() || c.is_lowercase()),
    ///     Some(1..3)
    /// );
    /// assert_eq!(s.find(|c: char| (c < 'o') && (c > 'a')), Some(4..5));
    /// ```
    ///
    /// Not finding the pattern:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("Löwe 老虎 Léopard");
    ///
    /// let x: &[_] = &['1', '2'];
    ///
    /// assert_eq!(s.find(x), None);
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[inline]
    pub fn find<P: StrsPattern>(&self, pat: P) -> Option<Range<usize>> {
        pat.into_searcher(self).next_match().map(|(s, e)| s..e)
    }

    /// Returns the byte index for the first character of the last
    /// match of the pattern in this `&Strs`.
    ///
    /// Returns [`None`] if the pattern doesn't match.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("Löwe 老虎 Léopard Gepardi");
    ///
    /// assert_eq!(s.rfind('L'), Some(13..14));
    /// assert_eq!(s.rfind('é'), Some(14..16));
    /// assert_eq!(s.rfind("pard"), Some(24..28));
    /// ```
    ///
    /// More complex patterns with closures:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("Löwe 老虎 Léopard");
    ///
    /// assert_eq!(s.rfind(char::is_whitespace), Some(12..13));
    /// assert_eq!(s.rfind(char::is_lowercase), Some(20..21));
    /// ```
    ///
    /// Not finding the pattern:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("Löwe 老虎 Léopard");
    ///
    /// let x: &[_] = &['1', '2'];
    ///
    /// assert_eq!(s.rfind(x), None);
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[inline]
    pub fn rfind<P: StrsPattern>(&self, pat: P) -> Option<Range<usize>>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        pat.into_searcher(self).next_match_back().map(|(s, e)| s..e)
    }

    /// Returns an iterator over sub`&Strs` of this `&Strs`,
    /// separated by characters matched by a pattern.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// If there are no matches the full `&Strs` is returned as
    /// the only item in the iterator.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator will be a [`DoubleEndedIterator`] if the
    /// pattern allows a reverse search and forward/reverse search
    /// yields the same elements. This is true for, e.g.,
    /// [`char`], but not for `&str`.
    ///
    /// If the pattern allows a reverse search but its results might
    /// differ from a forward search, the [`rsplit`] method can be
    /// used.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let s = Text::from("Mary had a little lamb");
    /// let v: Vec<&Strs> = s.split(' ').collect();
    /// assert_eq!(v, ["Mary", "had", "a", "little", "lamb"]);
    ///
    /// let s = Text::from("");
    /// let v: Vec<&Strs> = s.split('X').collect();
    /// assert_eq!(v, [""]);
    ///
    /// let s = Text::from("lionXXtigerXleopard");
    /// let v: Vec<&Strs> = s.split('X').collect();
    /// assert_eq!(v, ["lion", "", "tiger", "leopard"]);
    ///
    /// let s = Text::from("lion::tiger::leopard");
    /// let v: Vec<&Strs> = s.split("::").collect();
    /// assert_eq!(v, ["lion", "tiger", "leopard"]);
    ///
    /// let s = Text::from("AABBCC");
    /// let v: Vec<&Strs> = s.split("DD").collect();
    /// assert_eq!(v, ["AABBCC"]);
    ///
    /// let s = Text::from("abc1def2ghi");
    /// let v: Vec<&Strs> = s.split(char::is_numeric).collect();
    /// assert_eq!(v, ["abc", "def", "ghi"]);
    ///
    /// let s = Text::from("lionXtigerXleopard");
    /// let v: Vec<&Strs> = s.split(char::is_uppercase).collect();
    /// assert_eq!(v, ["lion", "tiger", "leopard"]);
    /// ```
    ///
    /// If the pattern is a slice of chars, split on each occurrence
    /// of any of the characters:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::{Strs, Text};
    /// let s = Text::from("2020-11-03 23:59");
    /// let v: Vec<&Strs> = s.split(&['-', ' ', ':', '@'][..]).collect();
    /// assert_eq!(v, ["2020", "11", "03", "23", "59"]);
    /// ```
    ///
    /// A more complex pattern, using a closure:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("abc1defXghi");
    /// let v: Vec<&Strs> = x.split(|c| c == '1' || c == 'X').collect();
    /// assert_eq!(v, ["abc", "def", "ghi"]);
    /// ```
    ///
    /// If a string contains multiple contiguous separators, you will
    /// end up with empty strings in the output:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::Text;
    /// let x = Text::from("||||a||b|c");
    /// let d: Vec<_> = x.split('|').collect();
    ///
    /// assert_eq!(d, &["", "", "", "", "a", "", "b", "c"]);
    /// ```
    ///
    /// Contiguous separators are separated by the empty string.
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::Text;
    /// let x = Text::from("(///)");
    /// let d: Vec<_> = x.split('/').collect();
    ///
    /// assert_eq!(d, &["(", "", "", ")"]);
    /// ```
    ///
    /// Separators at the start or end of a string are neighbored
    /// by empty strings.
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::Text;
    /// let x = Text::from("010");
    /// let d: Vec<_> = x.split("0").collect();
    /// assert_eq!(d, &["", "1", ""]);
    /// ```
    ///
    /// When the empty string is used as a separator, it separates
    /// every character in the string, along with the beginning
    /// and end of the string.
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::Text;
    /// let x = Text::from("rust");
    /// let f: Vec<_> = x.split("").collect();
    /// assert_eq!(f, &["", "r", "u", "s", "t", ""]);
    /// ```
    ///
    /// Contiguous separators can lead to possibly surprising behavior
    /// when whitespace is used as the separator. This code is
    /// correct:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::Text;
    /// let x = Text::from("    a  b c");
    /// let d: Vec<_> = x.split(' ').collect();
    ///
    /// assert_eq!(d, &["", "", "", "", "a", "", "b", "c"]);
    /// ```
    ///
    /// It does _not_ give you:
    ///
    /// ```should_panic
    /// # duat_core::doc_duat!(duat);
    /// # use duat::text::Text;
    /// # let x = Text::from("    a  b c");
    /// # let d: Vec<_> = x.split(' ').collect();
    /// assert_eq!(d, &["a", "b", "c"]);
    /// ```
    ///
    /// Use [`split_whitespace`] for this behavior.
    ///
    /// [pattern]: StrsPattern
    /// [`rsplit`]: Strs::rsplit
    /// [`split_whitespace`]: Strs::split_whitespace
    #[inline]
    pub fn split<P: StrsPattern>(&self, pat: P) -> Split<'_, P> {
        Split(SplitInternal {
            start: 0,
            end: self.len(),
            matcher: pat.into_searcher(self),
            allow_trailing_empty: true,
            finished: false,
        })
    }

    /// Returns an iterator over sub`&Strs` of this `&Strs`,
    /// separated by characters matched by a pattern.
    ///
    /// Differs from the iterator produced by `split` in that
    /// `split_inclusive` leaves the matched part as the
    /// terminator of the substring.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let text = Text::from("Mary had a little lamb\nlittle lamb\nlittle lamb.");
    /// let v: Vec<&Strs> = text.split_inclusive('\n').collect();
    /// assert_eq!(v, [
    ///     "Mary had a little lamb\n",
    ///     "little lamb\n",
    ///     "little lamb."
    /// ]);
    /// ```
    ///
    /// If the last element of the string is matched,
    /// that element will be considered the terminator of the
    /// preceding substring. That substring will be the last item
    /// returned by the iterator.
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let text = Text::from("Mary had a little lamb\nlittle lamb\nlittle lamb.\n");
    /// let v: Vec<&Strs> = text.split_inclusive('\n').collect();
    /// assert_eq!(v, [
    ///     "Mary had a little lamb\n",
    ///     "little lamb\n",
    ///     "little lamb.\n"
    /// ]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[inline]
    pub fn split_inclusive<P: StrsPattern>(&self, pat: P) -> SplitInclusive<'_, P> {
        SplitInclusive(SplitInternal {
            start: 0,
            end: self.len(),
            matcher: pat.into_searcher(self),
            allow_trailing_empty: false,
            finished: false,
        })
    }

    /// Returns an iterator over sub`&Strs` of the given `&Strs`,
    /// separated by characters matched by a pattern and yielded
    /// in reverse order.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator requires that the pattern supports a
    /// reverse search, and it will be a [`DoubleEndedIterator`]
    /// if a forward/reverse search yields the same elements.
    ///
    /// For iterating from the front, the [`split`] method can be
    /// used.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("Mary had a little lamb");
    /// let v: Vec<&Strs> = x.rsplit(' ').collect();
    /// assert_eq!(v, ["lamb", "little", "a", "had", "Mary"]);
    ///
    /// let x = Text::from("");
    /// let v: Vec<&Strs> = x.rsplit('X').collect();
    /// assert_eq!(v, [""]);
    ///
    /// let x = Text::from("lionXXtigerXleopard");
    /// let v: Vec<&Strs> = x.rsplit('X').collect();
    /// assert_eq!(v, ["leopard", "tiger", "", "lion"]);
    ///
    /// let x = Text::from("lion::tiger::leopard");
    /// let v: Vec<&Strs> = x.rsplit("::").collect();
    /// assert_eq!(v, ["leopard", "tiger", "lion"]);
    /// ```
    ///
    /// A more complex pattern, using a closure:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("abc1defXghi");
    /// let v: Vec<&Strs> = x.rsplit(|c| c == '1' || c == 'X').collect();
    /// assert_eq!(v, ["ghi", "def", "abc"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`split`]: Strs::split
    #[inline]
    pub fn rsplit<P: StrsPattern>(&self, pat: P) -> RSplit<'_, P>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        RSplit(self.split(pat).0)
    }

    /// Returns an iterator over sub`&Strs` of the given `&Strs`,
    /// separated by characters matched by a pattern.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// Equivalent to [`split`], except that the trailing substring
    /// is skipped if empty.
    ///
    /// This method can be used for string data that is _terminated_,
    /// rather than _separated_ by a pattern.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator will be a [`DoubleEndedIterator`] if the
    /// pattern allows a reverse search and forward/reverse search
    /// yields the same elements. This is true for, e.g.,
    /// [`char`], but not for `&str`.
    ///
    /// If the pattern allows a reverse search but its results might
    /// differ from a forward search, the [`rsplit_terminator`]
    /// method can be used.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("A.B.");
    /// let v: Vec<&Strs> = x.split_terminator('.').collect();
    /// assert_eq!(v, ["A", "B"]);
    ///
    /// let x = Text::from("A..B..");
    /// let v: Vec<&Strs> = x.split_terminator(".").collect();
    /// assert_eq!(v, ["A", "", "B", ""]);
    ///
    /// let x = Text::from("A.B:C.D");
    /// let v: Vec<&Strs> = x.split_terminator(&['.', ':'][..]).collect();
    /// assert_eq!(v, ["A", "B", "C", "D"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`split`]: Strs::split
    /// [`rsplit_terminator`]: Strs::rsplit_terminator
    #[inline]
    pub fn split_terminator<P: StrsPattern>(&self, pat: P) -> SplitTerminator<'_, P> {
        SplitTerminator(SplitInternal {
            allow_trailing_empty: false,
            ..self.split(pat).0
        })
    }

    /// Returns an iterator over sub`&Strs` of `self`, separated by
    /// characters matched by a pattern and yielded in reverse
    /// order.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// Equivalent to [`split`], except that the trailing substring is
    /// skipped if empty.
    ///
    /// This method can be used for string data that is _terminated_,
    /// rather than _separated_ by a pattern.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator requires that the pattern supports a
    /// reverse search, and it will be double ended if a
    /// forward/reverse search yields the same elements.
    ///
    /// For iterating from the front, the [`split_terminator`] method
    /// can be used.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("A.B.");
    /// let v: Vec<&Strs> = x.rsplit_terminator('.').collect();
    /// assert_eq!(v, ["B", "A"]);
    ///
    /// let x = Text::from("A..B..");
    /// let v: Vec<&Strs> = x.rsplit_terminator(".").collect();
    /// assert_eq!(v, ["", "B", "", "A"]);
    ///
    /// let x = Text::from("A.B:C.D");
    /// let v: Vec<&Strs> = x.rsplit_terminator(&['.', ':'][..]).collect();
    /// assert_eq!(v, ["D", "C", "B", "A"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`split`]: Strs::split
    /// [`split_terminator`]: Strs::split_terminator
    #[inline]
    pub fn rsplit_terminator<P: StrsPattern>(&self, pat: P) -> RSplitTerminator<'_, P>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        RSplitTerminator(self.split_terminator(pat).0)
    }

    /// Returns an iterator over sub`&Strs` of the given `&Strs`,
    /// separated by a pattern, restricted to returning at most
    /// `n` items.
    ///
    /// If `n` sub`&Strs` are returned, the last substring (the `n`th
    /// substring) will contain the remainder of the string.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator will not be double ended, because it is
    /// not efficient to support.
    ///
    /// If the pattern allows a reverse search, the [`rsplitn`] method
    /// can be used.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("Mary had a little lambda");
    /// let v: Vec<&Strs> = x.splitn(3, ' ').collect();
    /// assert_eq!(v, ["Mary", "had", "a little lambda"]);
    ///
    /// let x = Text::from("lionXXtigerXleopard");
    /// let v: Vec<&Strs> = x.splitn(3, "X").collect();
    /// assert_eq!(v, ["lion", "", "tigerXleopard"]);
    ///
    /// let x = Text::from("abcXdef");
    /// let v: Vec<&Strs> = x.splitn(1, 'X').collect();
    /// assert_eq!(v, ["abcXdef"]);
    ///
    /// let x = Text::from("");
    /// let v: Vec<&Strs> = x.splitn(1, 'X').collect();
    /// assert_eq!(v, [""]);
    /// ```
    ///
    /// A more complex pattern, using a closure:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("abc1defXghi");
    /// let v: Vec<&Strs> = x.splitn(2, |c| c == '1' || c == 'X').collect();
    /// assert_eq!(v, ["abc", "defXghi"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`rsplitn`]: Strs::rsplitn
    #[inline]
    pub fn splitn<P: StrsPattern>(&self, n: usize, pat: P) -> SplitN<'_, P> {
        SplitN(SplitNInternal { iter: self.split(pat).0, count: n })
    }

    /// Returns an iterator over sub`&Strs` of this `&Strs`,
    /// separated by a pattern, starting from the end of the
    /// string, restricted to returning at most `n` items.
    ///
    /// If `n` sub`&Strs` are returned, the last substring (the `n`th
    /// substring) will contain the remainder of the string.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator will not be double ended, because it is
    /// not efficient to support.
    ///
    /// For splitting from the front, the [`splitn`] method can be
    /// used.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("Mary had a little lamb");
    /// let v: Vec<&Strs> = x.rsplitn(3, ' ').collect();
    /// assert_eq!(v, ["lamb", "little", "Mary had a"]);
    ///
    /// let x = Text::from("lionXXtigerXleopard");
    /// let v: Vec<&Strs> = x.rsplitn(3, 'X').collect();
    /// assert_eq!(v, ["leopard", "tiger", "lionX"]);
    ///
    /// let x = Text::from("lion::tiger::leopard");
    /// let v: Vec<&Strs> = x.rsplitn(2, "::").collect();
    /// assert_eq!(v, ["leopard", "lion::tiger"]);
    /// ```
    ///
    /// A more complex pattern, using a closure:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("abc1defXghi");
    /// let v: Vec<&Strs> = x.rsplitn(2, |c| c == '1' || c == 'X').collect();
    /// assert_eq!(v, ["ghi", "abc1def"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`splitn`]: Strs::splitn
    #[inline]
    pub fn rsplitn<P: StrsPattern>(&self, n: usize, pat: P) -> RSplitN<'_, P>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        RSplitN(self.splitn(n, pat).0)
    }

    /// Splits the string on the first occurrence of the specified
    /// delimiter and returns prefix before delimiter and suffix
    /// after delimiter.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// assert_eq!(Text::from("cfg").split_once('='), None);
    /// assert!(
    ///     Text::from("cfg=")
    ///         .split_once('=')
    ///         .is_some_and(|(l, r)| l == "cfg" && r == "")
    /// );
    /// assert!(
    ///     Text::from("cfg=foo")
    ///         .split_once('=')
    ///         .is_some_and(|(l, r)| l == "cfg" && r == "foo")
    /// );
    /// assert!(
    ///     Text::from("cfg=foo=bar")
    ///         .split_once('=')
    ///         .is_some_and(|(l, r)| l == "cfg" && r == "foo=bar")
    /// );
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[inline]
    pub fn split_once<P: StrsPattern>(&self, delimiter: P) -> Option<(&'_ Strs, &'_ Strs)> {
        let (start, end) = delimiter.into_searcher(self).next_match()?;
        Some((&self[..start], &self[end..]))
    }

    /// Splits the string on the last occurrence of the specified
    /// delimiter and returns prefix before delimiter and suffix
    /// after delimiter.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// assert_eq!(Text::from("cfg").rsplit_once('='), None);
    /// assert!(
    ///     Text::from("cfg=")
    ///         .rsplit_once('=')
    ///         .is_some_and(|(l, r)| l == "cfg" && r == "")
    /// );
    /// assert!(
    ///     Text::from("cfg=foo")
    ///         .rsplit_once('=')
    ///         .is_some_and(|(l, r)| l == "cfg" && r == "foo")
    /// );
    /// assert!(
    ///     Text::from("cfg=foo=bar")
    ///         .rsplit_once('=')
    ///         .is_some_and(|(l, r)| l == "cfg=foo" && r == "bar")
    /// );
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[inline]
    pub fn rsplit_once<P: StrsPattern>(&self, delimiter: P) -> Option<(&'_ Strs, &'_ Strs)>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        let (start, end) = delimiter.into_searcher(self).next_match_back()?;
        Some((&self[..start], &self[end..]))
    }

    /// Returns an iterator over the disjoint matches of a pattern
    /// within the given `&Strs`.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator will be a [`DoubleEndedIterator`] if the
    /// pattern allows a reverse search and forward/reverse search
    /// yields the same elements. This is true for, e.g.,
    /// [`char`], but not for `&str`.
    ///
    /// If the pattern allows a reverse search but its results might
    /// differ from a forward search, the [`rmatches`] method can
    /// be used.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("abcXXXabcYYYabc");
    /// let v: Vec<&Strs> = x.matches("abc").collect();
    /// assert_eq!(v, ["abc", "abc", "abc"]);
    ///
    /// let x = Text::from("1abc2abc3");
    /// let v: Vec<&Strs> = x.matches(char::is_numeric).collect();
    /// assert_eq!(v, ["1", "2", "3"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`rmatches`]: Strs::rmatches
    #[inline]
    pub fn matches<P: StrsPattern>(&self, pat: P) -> Matches<'_, P> {
        Matches(MatchesInternal(pat.into_searcher(self)))
    }

    /// Returns an iterator over the disjoint matches of a pattern
    /// within this `&Strs`, yielded in reverse order.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator requires that the pattern supports a
    /// reverse search, and it will be a [`DoubleEndedIterator`]
    /// if a forward/reverse search yields the same elements.
    ///
    /// For iterating from the front, the [`matches`] method can be
    /// used.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::{Strs, Text};
    ///
    /// let x = Text::from("abcXXXabcYYYabc");
    /// let v: Vec<&Strs> = x.rmatches("abc").collect();
    /// assert_eq!(v, ["abc", "abc", "abc"]);
    ///
    /// let x = Text::from("1abc2abc3");
    /// let v: Vec<&Strs> = x.rmatches(char::is_numeric).collect();
    /// assert_eq!(v, ["3", "2", "1"]);
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`matches`]: Strs::matches
    #[inline]
    pub fn rmatches<P: StrsPattern>(&self, pat: P) -> RMatches<'_, P>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        RMatches(self.matches(pat).0)
    }

    /// Returns an iterator over the disjoint matches of a pattern
    /// within this `&Strs` as well as the [range] of the match.
    ///
    /// For matches of `pat` within `self` that overlap, only the
    /// indices corresponding to the first match are returned.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator will be a [`DoubleEndedIterator`] if the
    /// pattern allows a reverse search and forward/reverse search
    /// yields the same elements. This is true for, e.g.,
    /// [`char`], but not for `&str`.
    ///
    /// If the pattern allows a reverse search but its results might
    /// differ from a forward search, the [`rmatch_ranges`]
    /// method can be used.
    ///
    /// # Examples
    ///
    /// ```
    /// duat_core::doc_duat!(duat);
    /// use std::ops::Range;
    ///
    /// use duat::text::{Strs, Text};
    ///
    /// fn assert_eq<const N: usize>(l: Vec<(Range<usize>, &Strs)>, r: [(Range<usize>, &str); N]) {
    ///     assert!(
    ///         l.iter().zip(&r).all(|(l, r)| l.0 == r.0 && l.1 == r.1),
    ///         "{l:?}, {r:?}"
    ///     );
    /// }
    ///
    /// let x = Text::from("abcXXXabcYYYabc");
    /// let v: Vec<_> = x.match_ranges("abc").collect();
    /// assert_eq(v, [(0..3, "abc"), (6..9, "abc"), (12..15, "abc")]);
    ///
    /// let x = Text::from("1abcabc2");
    /// let v: Vec<_> = x.match_ranges(char::is_numeric).collect();
    /// assert_eq(v, [(0..1, "1"), (7..8, "2")]);
    ///
    /// let x = Text::from("ababa");
    /// let v: Vec<_> = x.match_ranges("aba").collect();
    /// assert_eq(v, [(0..3, "aba")]); // only the first `aba`
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`rmatch_ranges`]: Strs::rmatch_ranges
    /// [range]: Range
    #[inline]
    pub fn match_ranges<P: StrsPattern>(&self, pat: P) -> MatchRanges<'_, P> {
        MatchRanges(MatchRangesInternal(pat.into_searcher(self)))
    }

    /// Returns an iterator over the disjoint matches of a pattern
    /// within `self`, yielded in reverse order along with the
    /// [range] of the match.
    ///
    /// For matches of `pat` within `self` that overlap, only the
    /// indices corresponding to the last match are returned.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Iterator behavior
    ///
    /// The returned iterator requires that the pattern supports a
    /// reverse search, and it will be a [`DoubleEndedIterator`]
    /// if a forward/reverse search yields the same elements.
    ///
    /// For iterating from the front, the [`match_ranges`] method can
    /// be used.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use std::ops::Range;
    ///
    /// use duat::text::{Strs, Text};
    ///
    /// fn assert_eq<const N: usize>(l: Vec<(Range<usize>, &Strs)>, r: [(Range<usize>, &str); N]) {
    ///     assert!(
    ///         l.iter().zip(&r).all(|(l, r)| l.0 == r.0 && l.1 == r.1),
    ///         "{l:?}, {r:?}"
    ///     );
    /// }
    ///
    /// let x = Text::from("abcXXXabcYYYabc");
    /// let v: Vec<_> = x.rmatch_ranges("abc").collect();
    /// assert_eq(v, [(12..15, "abc"), (6..9, "abc"), (0..3, "abc")]);
    ///
    /// let x = Text::from("1abc2abc3");
    /// let v: Vec<_> = x.rmatch_ranges(char::is_numeric).collect();
    /// assert_eq(v, [(8..9, "3"), (4..5, "2"), (0..1, "1")]);
    ///
    /// let x = Text::from("ababa");
    /// let v: Vec<_> = x.rmatch_ranges("aba").collect();
    /// assert_eq(v, [(2..5, "aba")]); // only the last `aba`
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`match_ranges`]: Strs::match_ranges
    /// [range]: Range
    #[inline]
    pub fn rmatch_ranges<P: StrsPattern>(&self, pat: P) -> RMatchRanges<'_, P>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        RMatchRanges(self.match_ranges(pat).0)
    }

    /// Returns a `&Strs` with leading and trailing whitespace
    /// removed.
    ///
    /// 'Whitespace' is defined according to the terms of the Unicode
    /// Derived Core Property `White_Space`, which includes
    /// newlines.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("\n Hello\tworld\t\n");
    ///
    /// assert_eq!("Hello\tworld", s.trim());
    /// ```
    #[inline]
    #[must_use = "this returns the trimmed string as a slice, without modifying the original"]
    pub fn trim(&self) -> &Strs {
        self.trim_matches(char::is_whitespace)
    }

    /// Returns a `&Strs` with leading whitespace removed.
    ///
    /// 'Whitespace' is defined according to the terms of the Unicode
    /// Derived Core Property `White_Space`, which includes
    /// newlines.
    ///
    /// # Text directionality
    ///
    /// A string is a sequence of bytes. `start` in this context means
    /// the first position of that byte string; for a
    /// left-to-right language like English or Russian, this will
    /// be left side, and for right-to-left languages like
    /// Arabic or Hebrew, this will be the right side.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("\n Hello\tworld\t\n");
    /// assert_eq!("Hello\tworld\t\n", s.trim_start());
    /// ```
    ///
    /// Directionality:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("  English  ");
    /// assert!(Some('E') == s.trim_start().chars().next());
    ///
    /// let s = Text::from("  עברית  ");
    /// assert!(Some('ע') == s.trim_start().chars().next());
    /// ```
    #[inline]
    #[must_use = "this returns the trimmed string as a new slice, without modifying the original"]
    pub fn trim_start(&self) -> &Strs {
        self.trim_start_matches(char::is_whitespace)
    }

    /// Returns a `&Strs` with trailing whitespace removed.
    ///
    /// 'Whitespace' is defined according to the terms of the Unicode
    /// Derived Core Property `White_Space`, which includes
    /// newlines.
    ///
    /// # Text directionality
    ///
    /// A string is a sequence of bytes. `end` in this context means
    /// the last position of that byte string; for a left-to-right
    /// language like English or Russian, this will be right side,
    /// and for right-to-left languages like Arabic or Hebrew,
    /// this will be the left side.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("\n Hello\tworld\t\n");
    /// assert_eq!("\n Hello\tworld", s.trim_end());
    /// ```
    ///
    /// Directionality:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// let s = Text::from("  English  ");
    /// assert!(Some('h') == s.trim_end().chars().rev().next());
    ///
    /// let s = Text::from("  עברית  ");
    /// assert!(Some('ת') == s.trim_end().chars().rev().next());
    /// ```
    #[inline]
    #[must_use = "this returns the trimmed string as a new slice, without modifying the original"]
    pub fn trim_end(&self) -> &Strs {
        self.trim_end_matches(char::is_whitespace)
    }

    /// Returns a `&Strs` with all prefixes and suffixes that
    /// match a pattern repeatedly removed.
    ///
    /// The [pattern] can be a [`char`], a slice of [`char`]s, or a
    /// function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert_eq!(Text::from("11foo1bar11").trim_matches('1'), "foo1bar");
    /// assert_eq!(
    ///     Text::from("123foo1bar123").trim_matches(char::is_numeric),
    ///     "foo1bar"
    /// );
    ///
    /// let x: &[_] = &['1', '2'];
    /// assert_eq!(Text::from("12foo1bar12").trim_matches(x), "foo1bar");
    /// ```
    ///
    /// A more complex pattern, using a closure:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert_eq!(
    ///     Text::from("1foo1barXX").trim_matches(|c| c == '1' || c == 'X'),
    ///     "foo1bar"
    /// );
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[must_use = "this returns the trimmed string as a new slice, without modifying the original"]
    pub fn trim_matches<P: StrsPattern>(&self, pat: P) -> &Strs
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        let mut i = 0;
        let mut j = 0;
        let mut matcher = pat.into_searcher(self);
        if let Some((a, b)) = matcher.next_reject() {
            i = a;
            j = b; // Remember earliest known match, correct it below if
            // last match is different
        }
        if let Some((_, b)) = matcher.next_reject_back() {
            j = b;
        }
        &self[i..j]
    }

    /// Returns a `&Strs` with all prefixes that match a pattern
    /// repeatedly removed.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Text directionality
    ///
    /// A string is a sequence of bytes. `start` in this context means
    /// the first position of that byte string; for a
    /// left-to-right language like English or Russian, this will
    /// be left side, and for right-to-left languages like
    /// Arabic or Hebrew, this will be the right side.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert_eq!(
    ///     Text::from("11foo1bar11").trim_start_matches('1'),
    ///     "foo1bar11"
    /// );
    /// assert_eq!(
    ///     Text::from("123foo1bar123").trim_start_matches(char::is_numeric),
    ///     "foo1bar123"
    /// );
    ///
    /// let x: &[_] = &['1', '2'];
    /// assert_eq!(Text::from("12foo1bar12").trim_start_matches(x), "foo1bar12");
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[must_use = "this returns the trimmed string as a new slice, without modifying the original"]
    pub fn trim_start_matches<P: StrsPattern>(&self, pat: P) -> &Strs {
        let mut i = self.len();
        let mut matcher = pat.into_searcher(self);
        if let Some((a, _)) = matcher.next_reject() {
            i = a;
        }
        &self[i..self.len()]
    }

    /// Returns a `&Strs` with all suffixes that match a pattern
    /// repeatedly removed.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Text directionality
    ///
    /// A string is a sequence of bytes. `end` in this context means
    /// the last position of that byte string; for a left-to-right
    /// language like English or Russian, this will be right side,
    /// and for right-to-left languages like Arabic or Hebrew,
    /// this will be the left side.
    ///
    /// # Examples
    ///
    /// Simple patterns:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert_eq!(Text::from("11foo1bar11").trim_end_matches('1'), "11foo1bar");
    /// assert_eq!(
    ///     Text::from("123foo1bar123").trim_end_matches(char::is_numeric),
    ///     "123foo1bar"
    /// );
    ///
    /// let x: &[_] = &['1', '2'];
    /// assert_eq!(Text::from("12foo1bar12").trim_end_matches(x), "12foo1bar");
    /// ```
    ///
    /// A more complex pattern, using a closure:
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert_eq!(
    ///     Text::from("1fooX").trim_end_matches(|c| c == '1' || c == 'X'),
    ///     "1foo"
    /// );
    /// ```
    ///
    /// [pattern]: StrsPattern
    #[must_use = "this returns the trimmed string as a new slice, without modifying the original"]
    pub fn trim_end_matches<P: StrsPattern>(&self, pat: P) -> &Strs
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        let mut j = 0;
        let mut matcher = pat.into_searcher(self);
        if let Some((_, b)) = matcher.next_reject_back() {
            j = b;
        }
        &self[0..j]
    }

    /// Returns a `&Strs` with the prefix removed.
    ///
    /// If the string starts with the pattern `prefix`, returns the
    /// substring after the prefix, wrapped in `Some`. Unlike
    /// [`trim_start_matches`], this method removes the prefix exactly
    /// once.
    ///
    /// If the string does not start with `prefix`, returns `None`.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert!(
    ///     Text::from("foo:bar")
    ///         .strip_prefix("foo:")
    ///         .is_some_and(|s| s == "bar")
    /// );
    /// assert_eq!(Text::from("foo:bar").strip_prefix("bar"), None);
    /// assert!(
    ///     Text::from("foofoo")
    ///         .strip_prefix("foo")
    ///         .is_some_and(|s| s == "foo")
    /// );
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`trim_start_matches`]: Self::trim_start_matches
    #[must_use = "this returns the remaining substring as a new slice, without modifying the \
                  original"]
    pub fn strip_prefix<P: StrsPattern>(&self, prefix: P) -> Option<&Strs> {
        if let SearchStep::Match(_, e) = prefix.into_searcher(self).next() {
            Some(&self[e..])
        } else {
            None
        }
    }

    /// Returns a `&Strs` with the suffix removed.
    ///
    /// If the string ends with the pattern `suffix`, returns the
    /// substring before the suffix, wrapped in `Some`.  Unlike
    /// [`trim_end_matches`], this method removes the suffix exactly
    /// once.
    ///
    /// If the string does not end with `suffix`, returns `None`.
    ///
    /// The [pattern] can be a `&str`, [`char`], a slice of [`char`]s,
    /// or a function or closure that determines if a character
    /// matches.
    ///
    /// # Examples
    ///
    /// ```
    /// # duat_core::doc_duat!(duat);
    /// use duat::text::Text;
    ///
    /// assert!(
    ///     Text::from("bar:foo")
    ///         .strip_suffix(":foo")
    ///         .is_some_and(|s| s == "bar")
    /// );
    /// assert_eq!(Text::from("bar:foo").strip_suffix("bar"), None);
    /// assert!(
    ///     Text::from("foofoo")
    ///         .strip_suffix("foo")
    ///         .is_some_and(|s| s == "foo")
    /// );
    /// ```
    ///
    /// [pattern]: StrsPattern
    /// [`trim_end_matches`]: Self::trim_end_matches
    #[must_use = "this returns the remaining substring as a new slice, without modifying the \
                  original"]
    pub fn strip_suffix<P: StrsPattern>(&self, suffix: P) -> Option<&Strs>
    where
        for<'a> P::Searcher<'a>: StrsDoubleEndedSearcher<'a>,
    {
        if let SearchStep::Match(s, _) = suffix.into_searcher(self).next_back() {
            Some(&self[..s])
        } else {
            None
        }
    }
}

impl<Idx: TextRange> std::ops::Index<Idx> for Strs {
    type Output = Self;

    #[track_caller]
    fn index(&self, index: Idx) -> &Self::Output {
        let formed = FormedStrs::new(self);
        let range = index.to_range(formed.len as usize);

        assert_utf8_boundary(formed.buf, formed.start as usize + range.start);
        assert_utf8_boundary(formed.buf, formed.start as usize + range.end);

        Self::new(
            formed.buf,
            range.start as u32 + formed.start,
            range.len() as u32,
        )
    }
}

impl std::fmt::Display for Strs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [s0, s1] = self.to_array();
        write!(f, "{s0}{s1}")
    }
}

impl std::fmt::Debug for Strs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let [s0, s1] = self.to_array();
        write!(f, "{:?}", format!("{s0}{s1}").as_str())
    }
}

static EMPTY_BYTES: LazyLock<StrsBuf> = LazyLock::new(StrsBuf::default);

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

/// An [`Iterator`] over the lines on an [`Strs`]
pub struct Lines<'s>(SplitInclusive<'s, char>);

impl<'b> Iterator for Lines<'b> {
    type Item = &'b Strs;

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.0.next()?;

        let Some(line) = line.strip_suffix('\n') else {
            return Some(line);
        };

        if let Some(line) = line.strip_suffix('\r') {
            Some(line)
        } else {
            Some(line)
        }
    }

    fn last(mut self) -> Option<Self::Item> {
        self.next_back()
    }
}

impl DoubleEndedIterator for Lines<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let line = self.0.next_back()?;

        let Some(line) = line.strip_suffix('\n') else {
            return Some(line);
        };

        if let Some(line) = line.strip_suffix('\r') {
            Some(line)
        } else {
            Some(line)
        }
    }
}

impl std::iter::FusedIterator for Lines<'_> {}

/// A deconstructed internal representation of an [`Strs`],
/// useful for not doing a bunch of unsafe operations.
struct FormedStrs<'b> {
    buf: &'b StrsBuf,
    start: u32,
    len: u32,
}

impl<'b> FormedStrs<'b> {
    fn new(strs: &'b Strs) -> Self {
        let ptr = strs as *const Strs as *const [StrsBuf];
        // Safety: Since the len was created by the inverted operation, this
        // should be fine.
        let [start, len] = unsafe { std::mem::transmute::<usize, [u32; 2]>(ptr.len()) };

        Self {
            // Safety: When creating the Strs, a valid StrsBuf was at the address.
            buf: unsafe { &*(ptr as *const StrsBuf) },
            start,
            len,
        }
    }
}

#[repr(transparent)]
struct StrsDST([StrsBuf]);
