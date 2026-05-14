//! The [`Strs`] pattern API.
//!
//! This module is largely inspired (and adapted) from
//! `core::str::pattern`, and details all the ways in which you can
//! search on `Strs`, including searching on [`Deref<Target = Strs>`]
//! types, like [`Text`].
//!
//! [`Deref<Target = Strs>`]: std::ops::Deref
//! [`Text`]: crate::text::Text
#![allow(clippy::needless_range_loop)]

use crate::text::{Strs, strs::memchr};

/// A pattern for [`Strs`].
///
/// A `Pattern` expresses that the implementing type
/// can be used as a string pattern for searching in [`&Strs`][Strs].
///
/// For example, both `'a'` and `"aa"` are patterns that
/// would match at index `1` in `Text::from("baaaab")`.
///
/// The trait itself acts as a builder for an associated
/// [`Searcher`] type, which does the actual work of finding
/// occurrences of the pattern in a string.
///
/// Depending on the type of the pattern, the behavior of methods like
/// [`str::find`] and [`str::contains`] can change. The table below
/// describes some of those behaviors.
///
/// | Pattern type             | Match condition                           |
/// |--------------------------|-------------------------------------------|
/// | `&str`                   | is substring                              |
/// | `char`                   | is contained in string                    |
/// | `&[char]`                | any char in slice is contained in string  |
/// | `F: FnMut(char) -> bool` | `F` returns `true` for a char in string   |
/// | `&&str`                  | is substring                              |
/// | `&String`                | is substring                              |
///
/// The (unstable) analogue of this trait for [`str`] is the
/// [`Pattern`] trait.
///
/// # Examples
///
/// ```
/// # duat_core::doc_duat!(duat);
/// use duat::text::Text;
///
/// let text = Text::from("abaaa");
///
/// // &str
/// assert_eq!(text.find("ba"), Some(1..3));
/// assert_eq!(text.find("bac"), None);
///
/// // char
/// assert_eq!(text.find('a'), Some(0..1));
/// assert_eq!(text.find('b'), Some(1..2));
/// assert_eq!(text.find('c'), None);
///
/// // &[char; N]
/// assert_eq!(text.find(&['b', 'a']), Some(0..1));
/// assert_eq!(text.find(&['a', 'z']), Some(0..1));
/// assert_eq!(text.find(&['c', 'd']), None);
///
/// // &[char]
/// assert_eq!(text.find(&['b', 'a'][..]), Some(0..1));
/// assert_eq!(text.find(&['a', 'z'][..]), Some(0..1));
/// assert_eq!(text.find(&['c', 'd'][..]), None);
///
/// // FnMut(char) -> bool
/// assert_eq!(
///     Text::from("abcdef_z").find(|c| c > 'd' && c < 'y'),
///     Some(4..5)
/// );
/// assert_eq!(Text::from("abcddd_z").find(|c| c > 'd' && c < 'y'), None);
/// ```
///
/// [`Pattern`]: core::str::pattern::Pattern
pub trait Pattern {
    /// Associated searcher for this pattern
    type Searcher<'s>: Searcher<'s>;

    /// Constructs the associated searcher from
    /// `self` and the `haystack` to search in.
    fn into_searcher(self, haystack: &Strs) -> Self::Searcher<'_>;
}

/// Result of calling [`Searcher::next()`] or
/// [`DoubleEndedSearcher::next_back()`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchStep {
    /// Represents a matched region of the [`Strs`].
    ///
    /// They must be complete, and never interlap.
    Match(usize, usize),
    /// Represents an unmatched region of the [`Strs`].
    ///
    /// They may be incomplete, but may never interlap.
    Reject(usize, usize),
    /// Indicates that the search is finished.
    Done,
}

/// A searcher for an [`Strs`] pattern.
///
/// This trait provides methods for searching for non-overlapping
/// matches of a pattern starting from the front (left) of a string.
///
/// It will be implemented by associated `Searcher`
/// types of the [`Pattern`] trait.
///
/// # Safety
///
/// The trait is marked unsafe because the indices returned by the
/// [`next()`][Searcher::next] methods are required to lie on
/// valid utf8 boundaries in the haystack. This enables consumers of
/// this trait to slice the haystack without additional runtime
/// checks.
pub unsafe trait Searcher<'s> {
    /// Getter for the underlying string to be searched in.
    ///
    /// Will always return the same [`&Strs`][Strs].
    fn haystack(&self) -> &'s Strs;

    /// - Returns [`Match(a, b)`][SearchStep::Match] if
    ///   `haystack[a..b]` matches the pattern.
    /// - Returns [`Reject(a, b)`][SearchStep::Reject] if
    ///   `haystack[a..b]` can not match the pattern, even partially.
    /// - Returns [`Done`][SearchStep::Done] if every byte of the
    ///   haystack has been visited.
    ///
    /// The stream of [`Match`][SearchStep::Match] and
    /// [`Reject`][SearchStep::Reject] values up to a
    /// [`Done`][SearchStep::Done] will contain index ranges that
    /// are adjacent, non-overlapping, covering the whole
    /// haystack, and laying on utf8 boundaries.
    ///
    /// A [`Match`][SearchStep::Match] result needs to contain the
    /// whole matched pattern, however
    /// [`Reject`][SearchStep::Reject] results may be split up
    /// into arbitrary many adjacent fragments. Both ranges may have
    /// zero length.
    ///
    /// As an example, the pattern `"aaa"` and the haystack
    /// `"cbaaaaab"` might produce the stream
    /// `[Reject(0, 1), Reject(1, 2), Match(2, 5), Reject(5, 8)]`
    fn next(&mut self) -> SearchStep;

    /// Finds the next [`Match`][SearchStep::Match] result. See
    /// [`next()`][Searcher::next].
    ///
    /// Unlike [`next()`][Searcher::next], there is no guarantee that
    /// the returned ranges of this and
    /// [`next_reject`][Searcher::next_reject] will overlap. This will
    /// return `(start_match, end_match)`, where start_match is
    /// the index of where the match begins, and end_match is the
    /// index after the end of the match.
    #[inline]
    fn next_match(&mut self) -> Option<(usize, usize)> {
        loop {
            match self.next() {
                SearchStep::Match(a, b) => return Some((a, b)),
                SearchStep::Done => return None,
                _ => continue,
            }
        }
    }

    /// Finds the next [`Reject`][SearchStep::Reject] result. See
    /// [`next()`][Searcher::next]
    /// and [`next_match()`][Searcher::next_match].
    ///
    /// Unlike [`next()`][Searcher::next], there is no guarantee that
    /// the returned ranges of this and
    /// [`next_match`][Searcher::next_match] will overlap.
    #[inline]
    fn next_reject(&mut self) -> Option<(usize, usize)> {
        loop {
            match self.next() {
                SearchStep::Reject(a, b) => return Some((a, b)),
                SearchStep::Done => return None,
                _ => continue,
            }
        }
    }
}

/// A reverse searcher for an [`Strs`] pattern.
///
/// This trait provides methods for searching for non-overlapping
/// matches of a pattern starting from the back (right) of a string.
///
/// It will be implemented by associated [`Searcher`]
/// types of the [`Pattern`] trait if the pattern supports
/// searching for it from the back.
///
/// The index ranges returned by this trait are not required
/// to exactly match those of the forward search in reverse.
///
/// For the reason why this trait is marked unsafe, see the
/// parent trait [`Searcher`].
///
/// # Safety
///
/// For the reason why this trait is marked unsafe, see the
/// parent trait [`Searcher`].
pub unsafe trait DoubleEndedSearcher<'s>: Searcher<'s> {
    /// Performs the next search step starting from the back.
    ///
    /// - Returns [`Match(a, b)`][SearchStep::Match] if
    ///   `haystack[a..b]` matches the pattern.
    /// - Returns [`Reject(a, b)`][SearchStep::Reject] if
    ///   `haystack[a..b]` can not match the pattern, even partially.
    /// - Returns [`Done`][SearchStep::Done] if every byte of the
    ///   haystack has been visited
    ///
    /// The stream of [`Match`][SearchStep::Match] and
    /// [`Reject`][SearchStep::Reject] values up to a
    /// [`Done`][SearchStep::Done] will contain index ranges that
    /// are adjacent, non-overlapping, covering the whole
    /// haystack, and laying on utf8 boundaries.
    ///
    /// A [`Match`][SearchStep::Match] result needs to contain the
    /// whole matched pattern, however
    /// [`Reject`][SearchStep::Reject] results may be split up
    /// into arbitrary many adjacent fragments. Both ranges may have
    /// zero length.
    ///
    /// As an example, the pattern `"aaa"` and the haystack
    /// `"cbaaaaab"` might produce the stream
    /// `[Reject(7, 8), Match(4, 7), Reject(1, 4), Reject(0, 1)]`.
    fn next_back(&mut self) -> SearchStep;

    /// Finds the next [`Match`][SearchStep::Match] result.
    /// See [`next_back()`][DoubleEndedSearcher::next_back].
    #[inline]
    fn next_match_back(&mut self) -> Option<(usize, usize)> {
        loop {
            match self.next_back() {
                SearchStep::Match(a, b) => return Some((a, b)),
                SearchStep::Done => return None,
                _ => continue,
            }
        }
    }

    /// Finds the next [`Reject`][SearchStep::Reject] result.
    /// See [`next_back()`][DoubleEndedSearcher::next_back].
    #[inline]
    fn next_reject_back(&mut self) -> Option<(usize, usize)> {
        loop {
            match self.next_back() {
                SearchStep::Reject(a, b) => return Some((a, b)),
                SearchStep::Done => return None,
                _ => continue,
            }
        }
    }
}

/////////////////////////////////////////////////////////////////////////////
// Impl for char
/////////////////////////////////////////////////////////////////////////////

/// Associated type for `<char as Pattern>::Searcher<'a>`.
#[derive(Clone, Debug)]
pub struct CharSearcher<'a> {
    haystack: &'a Strs,
    // safety invariant: `finger`/`finger_back` must be a valid utf8 byte index of `haystack`
    // This invariant can be broken *within* next_match and next_match_back, however
    // they must exit with fingers on valid code point boundaries.
    /// `finger` is the current byte index of the forward search.
    /// Imagine that it exists before the byte at its index, i.e.
    /// `haystack[finger]` is the first byte of the slice we must
    /// inspect during forward searching
    finger: usize,
    /// `finger_back` is the current byte index of the reverse search.
    /// Imagine that it exists after the byte at its index, i.e.
    /// haystack[finger_back - 1] is the last byte of the slice we
    /// must inspect during forward searching (and thus the first
    /// byte to be inspected when calling next_back()).
    finger_back: usize,
    /// The character being searched for
    needle: char,

    // safety invariant: `utf8_size` must be less than 5
    /// The number of bytes `needle` takes up when encoded in utf8.
    utf8_size: u8,
    /// A utf8 encoded copy of the `needle`
    utf8_encoded: [u8; 4],
}

impl CharSearcher<'_> {
    fn utf8_size(&self) -> usize {
        self.utf8_size.into()
    }
}

unsafe impl<'s> Searcher<'s> for CharSearcher<'s> {
    #[inline]
    fn haystack(&self) -> &'s Strs {
        self.haystack
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        let old_finger = self.finger;
        let slice = &self.haystack[self.finger..self.finger_back];
        if let Some(ch) = slice.chars().next() {
            self.finger += ch.len_utf8();
            if ch == self.needle {
                SearchStep::Match(old_finger, self.finger)
            } else {
                SearchStep::Reject(old_finger, self.finger)
            }
        } else {
            SearchStep::Done
        }
    }

    #[inline]
    fn next_match(&mut self) -> Option<(usize, usize)> {
        let [s0, s1] = self.haystack.to_array();

        loop {
            let (pre_len, bytes) = if self.finger >= s0.len() {
                let bytes = s1
                    .as_bytes()
                    .get(self.finger - s0.len()..self.finger_back - s0.len())?;
                (s0.len(), bytes)
            } else {
                let bytes = s0
                    .as_bytes()
                    .get(self.finger..self.finger_back.min(s0.len()))?;
                (0, bytes)
            };

            // SAFETY: we have an invariant that `utf8_size < 5`
            let last_byte = unsafe { *self.utf8_encoded.get_unchecked(self.utf8_size() - 1) };
            if let Some(index) = memchr::memchr(last_byte, bytes) {
                self.finger += index + 1;

                if self.finger - pre_len >= self.utf8_size() {
                    let found_char = self.finger - self.utf8_size();
                    if let Some(strs) = self.haystack.get(found_char..self.finger) {
                        // SAFETY: This array is made from the bytes of a utf8 char.
                        let str = unsafe {
                            std::str::from_utf8_unchecked(&self.utf8_encoded[0..self.utf8_size()])
                        };
                        if strs == str {
                            return Some((found_char, self.finger));
                        }
                    }
                }
            } else if self.finger < s0.len() {
                self.finger = s0.len();
            } else {
                self.finger = self.finger_back;
                return None;
            }
        }
    }
}

unsafe impl<'a> DoubleEndedSearcher<'a> for CharSearcher<'a> {
    #[inline]
    fn next_back(&mut self) -> SearchStep {
        let old_finger = self.finger_back;
        let slice = &self.haystack[self.finger..self.finger_back];
        if let Some(ch) = slice.chars().next_back() {
            self.finger_back -= ch.len_utf8();
            if ch == self.needle {
                SearchStep::Match(self.finger_back, old_finger)
            } else {
                SearchStep::Reject(self.finger_back, old_finger)
            }
        } else {
            SearchStep::Done
        }
    }

    #[inline]
    fn next_match_back(&mut self) -> Option<(usize, usize)> {
        let [s0, s1] = self.haystack.to_array();

        loop {
            let (pre_len, bytes) = if self.finger_back > s0.len() {
                let bytes = s1
                    .as_bytes()
                    .get(self.finger.saturating_sub(s0.len())..self.finger_back - s0.len())?;
                (s0.len(), bytes)
            } else {
                let bytes = s0.as_bytes().get(self.finger..self.finger_back)?;
                (0, bytes)
            };

            // SAFETY: we have an invariant that `utf8_size < 5`
            let last_byte = unsafe { *self.utf8_encoded.get_unchecked(self.utf8_size() - 1) };
            if let Some(index) = memchr::memrchr(last_byte, bytes) {
                let index = self.finger.max(pre_len) + index;
                let shift = self.utf8_size() - 1;

                if index - pre_len >= shift {
                    let found_char = index - shift;
                    if let Some(strs) = self
                        .haystack()
                        .get(found_char..(found_char + self.utf8_size()))
                    {
                        // SAFETY: This array is made from the bytes of a utf8 char.
                        let str = unsafe {
                            std::str::from_utf8_unchecked(&self.utf8_encoded[0..self.utf8_size()])
                        };
                        if strs == str {
                            self.finger_back = found_char;
                            return Some((self.finger_back, self.finger_back + self.utf8_size()));
                        }
                    }
                }

                self.finger_back = index;
            } else if self.finger_back > s0.len() {
                self.finger_back = s0.len();
            } else {
                self.finger_back = self.finger;
                return None;
            }
        }
    }

    // let next_reject_back use the default implementation from the
    // Searcher trait
}

/// Searches for chars that are equal to a given [`char`].
///
/// # Examples
///
/// ```
/// assert_eq!("Hello world".find('o'), Some(4));
/// ```
impl Pattern for char {
    type Searcher<'a> = CharSearcher<'a>;

    #[inline]
    fn into_searcher<'a>(self, haystack: &'a Strs) -> Self::Searcher<'a> {
        let mut utf8_encoded = [0; char::MAX_LEN_UTF8];
        let utf8_size = self
            .encode_utf8(&mut utf8_encoded)
            .len()
            .try_into()
            .expect("char len should be less than 255");

        CharSearcher {
            haystack,
            finger: 0,
            finger_back: haystack.len(),
            needle: self,
            utf8_size,
            utf8_encoded,
        }
    }
}

#[doc(hidden)]
trait MultiCharEq {
    fn matches(&mut self, c: char) -> bool;
}

impl<F> MultiCharEq for F
where
    F: FnMut(char) -> bool,
{
    #[inline]
    fn matches(&mut self, c: char) -> bool {
        (*self)(c)
    }
}

impl<const N: usize> MultiCharEq for [char; N] {
    #[inline]
    fn matches(&mut self, c: char) -> bool {
        self.contains(&c)
    }
}

impl<const N: usize> MultiCharEq for &[char; N] {
    #[inline]
    fn matches(&mut self, c: char) -> bool {
        self.contains(&c)
    }
}

impl MultiCharEq for &[char] {
    #[inline]
    fn matches(&mut self, c: char) -> bool {
        self.contains(&c)
    }
}

struct MultiCharEqPattern<C: MultiCharEq>(C);

#[derive(Clone, Debug)]
struct MultiCharEqSearcher<'a, C: MultiCharEq> {
    char_eq: C,
    haystack: &'a Strs,
    char_indices: super::CharIndices<'a>,
}

impl<C: MultiCharEq> Pattern for MultiCharEqPattern<C> {
    type Searcher<'a> = MultiCharEqSearcher<'a, C>;

    #[inline]
    fn into_searcher(self, haystack: &Strs) -> MultiCharEqSearcher<'_, C> {
        MultiCharEqSearcher {
            haystack,
            char_eq: self.0,
            char_indices: haystack.char_indices(),
        }
    }
}

unsafe impl<'a, C: MultiCharEq> Searcher<'a> for MultiCharEqSearcher<'a, C> {
    #[inline]
    fn haystack(&self) -> &'a Strs {
        self.haystack
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        if let Some((i, c)) = self.char_indices.next() {
            if self.char_eq.matches(c) {
                SearchStep::Match(i, i + c.len_utf8())
            } else {
                SearchStep::Reject(i, i + c.len_utf8())
            }
        } else {
            SearchStep::Done
        }
    }
}

unsafe impl<'a, C: MultiCharEq> DoubleEndedSearcher<'a> for MultiCharEqSearcher<'a, C> {
    #[inline]
    fn next_back(&mut self) -> SearchStep {
        if let Some((i, c)) = self.char_indices.next_back() {
            if self.char_eq.matches(c) {
                return SearchStep::Match(i, i + c.len_utf8());
            } else {
                return SearchStep::Reject(i, i + c.len_utf8());
            }
        }
        SearchStep::Done
    }
}

/////////////////////////////////////////////////////////////////////////////

macro_rules! pattern_methods {
    ($a:lifetime, $t:ty, $pmap:expr, $smap:expr) => {
        type Searcher<$a> = $t;

        #[inline]
        fn into_searcher<$a>(self, haystack: &$a Strs) -> $t {
            ($smap)(($pmap)(self).into_searcher(haystack))
        }
    };
}

macro_rules! searcher_methods {
    (forward, $s:lifetime) => {
        #[inline]
        fn haystack(&self) -> &$s Strs {
            self.0.haystack()
        }
        #[inline]
        fn next(&mut self) -> SearchStep {
            self.0.next()
        }
        #[inline]
        fn next_match(&mut self) -> Option<(usize, usize)> {
            self.0.next_match()
        }
        #[inline]
        fn next_reject(&mut self) -> Option<(usize, usize)> {
            self.0.next_reject()
        }
    };
    (reverse) => {
        #[inline]
        fn next_back(&mut self) -> SearchStep {
            self.0.next_back()
        }
        #[inline]
        fn next_match_back(&mut self) -> Option<(usize, usize)> {
            self.0.next_match_back()
        }
        #[inline]
        fn next_reject_back(&mut self) -> Option<(usize, usize)> {
            self.0.next_reject_back()
        }
    };
}

/// Associated type for `<[char; N] as Pattern>::Searcher<'a>`.
#[derive(Clone, Debug)]
pub struct CharArraySearcher<'a, const N: usize>(
    <MultiCharEqPattern<[char; N]> as Pattern>::Searcher<'a>,
);

/// Associated type for `<&[char; N] as Pattern>::Searcher<'a>`.
#[derive(Clone, Debug)]
pub struct CharArrayRefSearcher<'s, 'a, const N: usize>(
    <MultiCharEqPattern<&'a [char; N]> as Pattern>::Searcher<'s>,
);

impl<const N: usize> Pattern for [char; N] {
    pattern_methods!('s, CharArraySearcher<'s, N>, MultiCharEqPattern, CharArraySearcher);
}

unsafe impl<'s, const N: usize> Searcher<'s> for CharArraySearcher<'s, N> {
    searcher_methods!(forward, 's);
}

unsafe impl<'s, const N: usize> DoubleEndedSearcher<'s> for CharArraySearcher<'s, N> {
    searcher_methods!(reverse);
}

impl<'a, const N: usize> Pattern for &'a [char; N] {
    pattern_methods!('s, CharArrayRefSearcher<'s, 'a, N>, MultiCharEqPattern, CharArrayRefSearcher);
}

unsafe impl<'s, 'a, const N: usize> Searcher<'s> for CharArrayRefSearcher<'s, 'a, N> {
    searcher_methods!(forward, 's);
}

unsafe impl<'a, 'b, const N: usize> DoubleEndedSearcher<'a> for CharArrayRefSearcher<'a, 'b, N> {
    searcher_methods!(reverse);
}

/////////////////////////////////////////////////////////////////////////////
// Impl for &[char]
/////////////////////////////////////////////////////////////////////////////

/// Associated type for `<&[char] as Pattern>::Searcher<'a>`.
#[derive(Clone, Debug)]
pub struct CharSliceSearcher<'s, 'a>(<MultiCharEqPattern<&'a [char]> as Pattern>::Searcher<'s>);

unsafe impl<'s, 'a> Searcher<'s> for CharSliceSearcher<'s, 'a> {
    searcher_methods!(forward, 's);
}

unsafe impl<'s, 'a> DoubleEndedSearcher<'s> for CharSliceSearcher<'s, 'a> {
    searcher_methods!(reverse);
}

impl<'b> Pattern for &'b [char] {
    pattern_methods!('a, CharSliceSearcher<'a, 'b>, MultiCharEqPattern, CharSliceSearcher);
}

/////////////////////////////////////////////////////////////////////////////
// Impl for F: FnMut(char) -> bool
/////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct CharPredicateSearcher<'s, F>(<MultiCharEqPattern<F> as Pattern>::Searcher<'s>)
where
    F: FnMut(char) -> bool;

impl<F> std::fmt::Debug for CharPredicateSearcher<'_, F>
where
    F: FnMut(char) -> bool,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CharPredicateSearcher")
            .field("haystack", &self.0.haystack)
            .field("char_indices", &self.0.char_indices)
            .finish()
    }
}

unsafe impl<'s, F> Searcher<'s> for CharPredicateSearcher<'s, F>
where
    F: FnMut(char) -> bool,
{
    searcher_methods!(forward, 's);
}

unsafe impl<'a, F> DoubleEndedSearcher<'a> for CharPredicateSearcher<'a, F>
where
    F: FnMut(char) -> bool,
{
    searcher_methods!(reverse);
}

impl<F> Pattern for F
where
    F: FnMut(char) -> bool,
{
    pattern_methods!('a, CharPredicateSearcher<'a, F>, MultiCharEqPattern, CharPredicateSearcher);
}

/////////////////////////////////////////////////////////////////////////////
// Impl for &&str
/////////////////////////////////////////////////////////////////////////////

impl<'b> Pattern for &'_ &'b str {
    pattern_methods!('s, StrSearcher<'s, 'b>, |&s| s, |s| s);
}

/////////////////////////////////////////////////////////////////////////////
// Impl for &String
/////////////////////////////////////////////////////////////////////////////

impl<'b> Pattern for &'b String {
    pattern_methods!('s, StrSearcher<'s, 'b>, |s: &'b String| s.as_str(), |s| s);
}

/////////////////////////////////////////////////////////////////////////////
// Impl for &str
/////////////////////////////////////////////////////////////////////////////

/// Non-allocating substring search.
///
/// Will handle the pattern `""` as returning empty matches at each
/// character boundary.
///
/// # Examples
///
/// ```
/// assert_eq!("Hello world".find("world"), Some(6));
/// ```
impl<'b> Pattern for &'b str {
    type Searcher<'a> = StrSearcher<'a, 'b>;

    #[inline]
    fn into_searcher(self, haystack: &Strs) -> StrSearcher<'_, 'b> {
        StrSearcher::new(haystack, self)
    }
}

/////////////////////////////////////////////////////////////////////////////
// Two Way substring searcher
/////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug)]
/// Associated type for `<&str as Pattern>::Searcher<'a>`.
pub struct StrSearcher<'s, 'a> {
    haystack: &'s Strs,
    needle: &'a str,

    searcher: StrSearcherImpl,
}

#[derive(Clone, Debug)]
enum StrSearcherImpl {
    Empty(EmptyNeedle),
    TwoWay(TwoWaySearcher),
}

#[derive(Clone, Debug)]
struct EmptyNeedle {
    position: usize,
    end: usize,
    is_match_fw: bool,
    is_match_bw: bool,
    // Needed in case of an empty haystack, see #85462
    is_finished: bool,
}

impl<'s, 'a> StrSearcher<'s, 'a> {
    fn new(haystack: &'s Strs, needle: &'a str) -> StrSearcher<'s, 'a> {
        if needle.is_empty() {
            StrSearcher {
                haystack,
                needle,
                searcher: StrSearcherImpl::Empty(EmptyNeedle {
                    position: 0,
                    end: haystack.len(),
                    is_match_fw: true,
                    is_match_bw: true,
                    is_finished: false,
                }),
            }
        } else {
            StrSearcher {
                haystack,
                needle,
                searcher: StrSearcherImpl::TwoWay(TwoWaySearcher::new(
                    needle.as_bytes(),
                    haystack.len(),
                )),
            }
        }
    }
}

unsafe impl<'s, 'a> Searcher<'s> for StrSearcher<'s, 'a> {
    #[inline]
    fn haystack(&self) -> &'s Strs {
        self.haystack
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        match self.searcher {
            StrSearcherImpl::Empty(ref mut searcher) => {
                if searcher.is_finished {
                    return SearchStep::Done;
                }
                // empty needle rejects every char and matches every empty string
                // between them
                let is_match = searcher.is_match_fw;
                searcher.is_match_fw = !searcher.is_match_fw;
                let pos = searcher.position;
                match self.haystack[pos..].chars().next() {
                    _ if is_match => SearchStep::Match(pos, pos),
                    None => {
                        searcher.is_finished = true;
                        SearchStep::Done
                    }
                    Some(ch) => {
                        searcher.position += ch.len_utf8();
                        SearchStep::Reject(pos, searcher.position)
                    }
                }
            }
            StrSearcherImpl::TwoWay(ref mut searcher) => {
                // TwoWaySearcher produces valid *Match* indices that split at char
                // boundaries as long as it does correct matching and
                // that haystack and needle are valid UTF-8
                // *Rejects* from the algorithm can fall on any indices, but we will
                // walk them manually to the next character boundary,
                // so that they are utf-8 safe.
                if searcher.position == self.haystack.len() {
                    return SearchStep::Done;
                }
                let is_long = searcher.memory == usize::MAX;
                let arr = self.haystack.to_array().map(str::as_bytes);
                match searcher.next::<RejectAndMatch>(arr, self.needle.as_bytes(), is_long) {
                    SearchStep::Reject(a, mut b) => {
                        // skip to next char boundary
                        while !self.haystack.is_char_boundary(b) {
                            b += 1;
                        }
                        searcher.position = std::cmp::max(b, searcher.position);
                        SearchStep::Reject(a, b)
                    }
                    otherwise => otherwise,
                }
            }
        }
    }

    #[inline]
    fn next_match(&mut self) -> Option<(usize, usize)> {
        match self.searcher {
            StrSearcherImpl::Empty(..) => loop {
                match self.next() {
                    SearchStep::Match(a, b) => return Some((a, b)),
                    SearchStep::Done => return None,
                    SearchStep::Reject(..) => {}
                }
            },
            StrSearcherImpl::TwoWay(ref mut searcher) => {
                let is_long = searcher.memory == usize::MAX;
                let arr = self.haystack.to_array().map(str::as_bytes);
                // write out `true` and `false` cases to encourage the compiler
                // to specialize the two cases separately.
                if is_long {
                    searcher.next::<MatchOnly>(arr, self.needle.as_bytes(), true)
                } else {
                    searcher.next::<MatchOnly>(arr, self.needle.as_bytes(), false)
                }
            }
        }
    }
}

unsafe impl<'a, 'b> DoubleEndedSearcher<'a> for StrSearcher<'a, 'b> {
    #[inline]
    fn next_back(&mut self) -> SearchStep {
        match self.searcher {
            StrSearcherImpl::Empty(ref mut searcher) => {
                if searcher.is_finished {
                    return SearchStep::Done;
                }
                let is_match = searcher.is_match_bw;
                searcher.is_match_bw = !searcher.is_match_bw;
                let end = searcher.end;
                match self.haystack[..end].chars().next_back() {
                    _ if is_match => SearchStep::Match(end, end),
                    None => {
                        searcher.is_finished = true;
                        SearchStep::Done
                    }
                    Some(ch) => {
                        searcher.end -= ch.len_utf8();
                        SearchStep::Reject(searcher.end, end)
                    }
                }
            }
            StrSearcherImpl::TwoWay(ref mut searcher) => {
                if searcher.end == 0 {
                    return SearchStep::Done;
                }
                let is_long = searcher.memory == usize::MAX;
                let arr = self.haystack.to_array().map(str::as_bytes);

                match searcher.next_back::<RejectAndMatch>(arr, self.needle.as_bytes(), is_long) {
                    SearchStep::Reject(mut a, b) => {
                        // skip to next char boundary
                        while !self.haystack.is_char_boundary(a) {
                            a -= 1;
                        }
                        searcher.end = std::cmp::min(a, searcher.end);
                        SearchStep::Reject(a, b)
                    }
                    otherwise => otherwise,
                }
            }
        }
    }

    #[inline]
    fn next_match_back(&mut self) -> Option<(usize, usize)> {
        match self.searcher {
            StrSearcherImpl::Empty(..) => loop {
                match self.next_back() {
                    SearchStep::Match(a, b) => return Some((a, b)),
                    SearchStep::Done => return None,
                    SearchStep::Reject(..) => {}
                }
            },
            StrSearcherImpl::TwoWay(ref mut searcher) => {
                let is_long = searcher.memory == usize::MAX;
                let arr = self.haystack.to_array().map(str::as_bytes);
                // write out `true` and `false`, like `next_match`
                if is_long {
                    searcher.next_back::<MatchOnly>(arr, self.needle.as_bytes(), true)
                } else {
                    searcher.next_back::<MatchOnly>(arr, self.needle.as_bytes(), false)
                }
            }
        }
    }
}

/// The internal state of the two-way substring search algorithm.
#[derive(Clone, Debug)]
struct TwoWaySearcher {
    // constants
    /// critical factorization index
    crit_pos: usize,
    /// critical factorization index for reversed needle
    crit_pos_back: usize,
    period: usize,
    /// `byteset` is an extension (not part of the two way algorithm);
    /// it's a 64-bit "fingerprint" where each set bit `j` corresponds
    /// to a (byte & 63) == j present in the needle.
    byteset: u64,

    // variables
    position: usize,
    end: usize,
    /// index into needle before which we have already matched
    memory: usize,
    /// index into needle after which we have already matched
    memory_back: usize,
}

impl TwoWaySearcher {
    fn new(needle: &[u8], end: usize) -> TwoWaySearcher {
        let (crit_pos_false, period_false) = TwoWaySearcher::maximal_suffix(needle, false);
        let (crit_pos_true, period_true) = TwoWaySearcher::maximal_suffix(needle, true);

        let (crit_pos, period) = if crit_pos_false > crit_pos_true {
            (crit_pos_false, period_false)
        } else {
            (crit_pos_true, period_true)
        };

        if needle[..crit_pos] == needle[period..period + crit_pos] {
            let crit_pos_back = needle.len()
                - std::cmp::max(
                    TwoWaySearcher::reverse_maximal_suffix(needle, period, false),
                    TwoWaySearcher::reverse_maximal_suffix(needle, period, true),
                );

            TwoWaySearcher {
                crit_pos,
                crit_pos_back,
                period,
                byteset: Self::byteset_create(&needle[..period]),

                position: 0,
                end,
                memory: 0,
                memory_back: needle.len(),
            }
        } else {
            TwoWaySearcher {
                crit_pos,
                crit_pos_back: crit_pos,
                period: std::cmp::max(crit_pos, needle.len() - crit_pos) + 1,
                byteset: Self::byteset_create(needle),

                position: 0,
                end,
                memory: usize::MAX, // Dummy value to signify that the period is long
                memory_back: usize::MAX,
            }
        }
    }

    #[inline]
    fn byteset_create(bytes: &[u8]) -> u64 {
        bytes.iter().fold(0, |a, &b| (1 << (b & 0x3f)) | a)
    }

    #[inline]
    fn byteset_contains(&self, byte: u8) -> bool {
        (self.byteset >> ((byte & 0x3f) as usize)) & 1 != 0
    }

    #[inline]
    fn next<S>(&mut self, haystack: [&[u8]; 2], needle: &[u8], long_period: bool) -> S::Output
    where
        S: TwoWayStrategy,
    {
        let get_byte = |position: usize| {
            if position >= haystack[0].len() {
                haystack[1].get(position - haystack[0].len()).copied()
            } else {
                haystack[0].get(position).copied()
            }
        };

        let old_pos = self.position;
        let needle_last = needle.len() - 1;
        'search: loop {
            let tail_byte = match get_byte(self.position + needle_last) {
                Some(b) => b,
                None => {
                    self.position = haystack.len();
                    return S::rejecting(old_pos, self.position);
                }
            };

            if S::use_early_reject() && old_pos != self.position {
                return S::rejecting(old_pos, self.position);
            }

            if !self.byteset_contains(tail_byte) {
                self.position += needle.len();
                if !long_period {
                    self.memory = 0;
                }
                continue 'search;
            }

            let start = if long_period {
                self.crit_pos
            } else {
                std::cmp::max(self.crit_pos, self.memory)
            };
            for i in start..needle.len() {
                if Some(needle[i]) != get_byte(self.position + i) {
                    self.position += i - self.crit_pos + 1;
                    if !long_period {
                        self.memory = 0;
                    }
                    continue 'search;
                }
            }

            let start = if long_period { 0 } else { self.memory };
            for i in (start..self.crit_pos).rev() {
                if Some(needle[i]) != get_byte(self.position + i) {
                    self.position += self.period;
                    if !long_period {
                        self.memory = needle.len() - self.period;
                    }
                    continue 'search;
                }
            }

            let match_pos = self.position;

            self.position += needle.len();
            if !long_period {
                self.memory = 0;
            }

            return S::matching(match_pos, match_pos + needle.len());
        }
    }

    #[inline]
    fn next_back<S>(&mut self, haystack: [&[u8]; 2], needle: &[u8], long_period: bool) -> S::Output
    where
        S: TwoWayStrategy,
    {
        let get_byte = |position: usize| {
            if position >= haystack[0].len() {
                haystack[1].get(position - haystack[0].len()).copied()
            } else {
                haystack[0].get(position).copied()
            }
        };

        let old_end = self.end;
        'search: loop {
            let front_byte = match get_byte(self.end.wrapping_sub(needle.len())) {
                Some(b) => b,
                None => {
                    self.end = 0;
                    return S::rejecting(0, old_end);
                }
            };

            if S::use_early_reject() && old_end != self.end {
                return S::rejecting(self.end, old_end);
            }

            if !self.byteset_contains(front_byte) {
                self.end -= needle.len();
                if !long_period {
                    self.memory_back = needle.len();
                }
                continue 'search;
            }

            // See if the left part of the needle matches
            let crit = if long_period {
                self.crit_pos_back
            } else {
                std::cmp::min(self.crit_pos_back, self.memory_back)
            };
            for i in (0..crit).rev() {
                if Some(needle[i]) != get_byte(self.end - needle.len() + i) {
                    self.end -= self.crit_pos_back - i;
                    if !long_period {
                        self.memory_back = needle.len();
                    }
                    continue 'search;
                }
            }

            let needle_end = if long_period {
                needle.len()
            } else {
                self.memory_back
            };
            for i in self.crit_pos_back..needle_end {
                if Some(needle[i]) != get_byte(self.end - needle.len() + i) {
                    self.end -= self.period;
                    if !long_period {
                        self.memory_back = self.period;
                    }
                    continue 'search;
                }
            }

            let match_pos = self.end - needle.len();
            self.end -= needle.len();
            if !long_period {
                self.memory_back = needle.len();
            }

            return S::matching(match_pos, match_pos + needle.len());
        }
    }

    #[inline]
    fn maximal_suffix(arr: &[u8], order_greater: bool) -> (usize, usize) {
        let mut left = 0;
        let mut right = 1;
        let mut offset = 0;
        let mut period = 1;

        while let Some(&a) = arr.get(right + offset) {
            let b = arr[left + offset];
            if (a < b && !order_greater) || (a > b && order_greater) {
                right += offset + 1;
                offset = 0;
                period = right - left;
            } else if a == b {
                if offset + 1 == period {
                    right += offset + 1;
                    offset = 0;
                } else {
                    offset += 1;
                }
            } else {
                left = right;
                right += 1;
                offset = 0;
                period = 1;
            }
        }
        (left, period)
    }

    fn reverse_maximal_suffix(arr: &[u8], known_period: usize, order_greater: bool) -> usize {
        let mut left = 0;
        let mut right = 1;
        let mut offset = 0;
        let mut period = 1;
        let n = arr.len();

        while right + offset < n {
            let a = arr[n - (1 + right + offset)];
            let b = arr[n - (1 + left + offset)];
            if (a < b && !order_greater) || (a > b && order_greater) {
                right += offset + 1;
                offset = 0;
                period = right - left;
            } else if a == b {
                if offset + 1 == period {
                    right += offset + 1;
                    offset = 0;
                } else {
                    offset += 1;
                }
            } else {
                left = right;
                right += 1;
                offset = 0;
                period = 1;
            }
            if period == known_period {
                break;
            }
        }
        debug_assert!(period <= known_period);
        left
    }
}

/// TwoWayStrategy allows the algorithm to either skip non-matches as
/// quickly as possible, or to work in a mode where it emits Rejects
/// relatively quickly.
trait TwoWayStrategy {
    type Output;
    fn use_early_reject() -> bool;
    fn rejecting(a: usize, b: usize) -> Self::Output;
    fn matching(a: usize, b: usize) -> Self::Output;
}

/// Skip to match intervals as quickly as possible
enum MatchOnly {}

impl TwoWayStrategy for MatchOnly {
    type Output = Option<(usize, usize)>;

    #[inline]
    fn use_early_reject() -> bool {
        false
    }

    #[inline]
    fn rejecting(_a: usize, _b: usize) -> Self::Output {
        None
    }

    #[inline]
    fn matching(a: usize, b: usize) -> Self::Output {
        Some((a, b))
    }
}

/// Emit Rejects regularly
enum RejectAndMatch {}

impl TwoWayStrategy for RejectAndMatch {
    type Output = SearchStep;

    #[inline]
    fn use_early_reject() -> bool {
        true
    }

    #[inline]
    fn rejecting(a: usize, b: usize) -> Self::Output {
        SearchStep::Reject(a, b)
    }

    #[inline]
    fn matching(a: usize, b: usize) -> Self::Output {
        SearchStep::Match(a, b)
    }
}
