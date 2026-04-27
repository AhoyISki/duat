//! Iterators for `str` methods.

use std::{fmt, iter::FusedIterator, ops::Range};

use crate::text::strs::{
    Strs,
    pattern::{DoubleEndedSearcher, Pattern, Searcher},
};

/// An iterator over the [`char`]s of an [`Strs`], and their
/// positions.
///
/// This struct is created by the [`char_indices`] method on `Strs`.
/// See its documentation for more.
///
/// [`char_indices`]: Strs::char_indices
#[derive(Debug, Clone)]
pub struct CharIndices<'s> {
    s0_len: usize,
    s0_iter: std::str::CharIndices<'s>,
    s1_iter: std::str::CharIndices<'s>,
}

impl<'s> CharIndices<'s> {
    /// Creates a new `CharIndices`.
    pub(super) fn new(
        s0_len: usize,
        s0_iter: std::str::CharIndices<'s>,
        s1_iter: std::str::CharIndices<'s>,
    ) -> Self {
        Self { s0_len, s0_iter, s1_iter }
    }
}

impl<'s> Iterator for CharIndices<'s> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.s0_iter.next().or_else(|| {
            self.s1_iter
                .next()
                .map(|(idx, char)| (idx + self.s0_len, char))
        })
    }
}

impl<'s> DoubleEndedIterator for CharIndices<'s> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.s1_iter
            .next_back()
            .map(|(idx, char)| (idx + self.s0_len, char))
            .or_else(|| self.s0_iter.next_back())
    }
}

impl<'s> FusedIterator for CharIndices<'s> {}

/// This macro generates a Clone impl for string pattern API
/// wrapper types of the form X<'a, P>
macro_rules! derive_pattern_clone {
    (clone $t:ident with | $s:ident | $e:expr) => {
        impl<'a, P> Clone for $t<'a, P>
        where
            P: Pattern<Searcher<'a>: Clone>,
        {
            fn clone(&self) -> Self {
                let $s = self;
                $e
            }
        }
    };
}

/// This macro generates two public iterator structs
/// wrapping a private internal one that makes use of the `Pattern`
/// API.
///
/// For all patterns `P: Pattern` the following items will be
/// generated (generics omitted):
///
/// struct $forward_iterator($internal_iterator);
/// struct $reverse_iterator($internal_iterator);
///
/// impl Iterator for $forward_iterator
/// { /* internal ends up calling Searcher::next_match() */ }
///
/// impl DoubleEndedIterator for $forward_iterator
///       where P::Searcher: DoubleEndedSearcher
/// { /* internal ends up calling Searcher::next_match_back() */ }
///
/// impl Iterator for $reverse_iterator
///       where P::Searcher: DoubleEndedSearcher
/// { /* internal ends up calling Searcher::next_match_back() */ }
///
/// impl DoubleEndedIterator for $reverse_iterator
///       where P::Searcher: DoubleEndedSearcher
/// { /* internal ends up calling Searcher::next_match() */ }
///
/// The internal one is defined outside the macro, and has almost the
/// same semantic as a DoubleEndedIterator by delegating to
/// `pattern::Searcher` and `pattern::DoubleEndedSearcher` for both
/// forward and reverse iteration.
///
/// "Almost", because a `Searcher` and a `DoubleEndedSearcher` for a
/// given `Pattern` might not return the same elements, so actually
/// implementing `DoubleEndedIterator` for it would be incorrect.
/// (See the docs in `Strs::pattern` for more details)
///
/// However, the internal struct still represents a single ended
/// iterator from either end, and depending on pattern is also a valid
/// double ended iterator, so the two wrapper structs implement
/// `Iterator` and `DoubleEndedIterator` depending on the concrete
/// pattern type, leading to the complex impls seen above.
macro_rules! generate_pattern_iterators {
    {
        // Forward iterator
        forward:
            $(#[$forward_iterator_attribute:meta])*
            struct $forward_iterator:ident;

        // Reverse iterator
        reverse:
            $(#[$reverse_iterator_attribute:meta])*
            struct $reverse_iterator:ident;

        // Internal almost-iterator that is being delegated to
        internal:
            $internal_iterator:ident yielding ($iterty:ty);

        // Kind of delegation - either single ended or double ended
        delegate $($t:tt)*
    } => {
        $(#[$forward_iterator_attribute])*
        pub struct $forward_iterator<'a, P: Pattern>(pub(super) $internal_iterator<'a, P>);

        impl<'a, P> fmt::Debug for $forward_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: fmt::Debug>,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($forward_iterator))
                    .field(&self.0)
                    .finish()
            }
        }

        impl<'a, P: Pattern> Iterator for $forward_iterator<'a, P> {
            type Item = $iterty;

            #[inline]
            fn next(&mut self) -> Option<$iterty> {
                self.0.next()
            }
        }

        impl<'a, P> Clone for $forward_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: Clone>,
        {
            fn clone(&self) -> Self {
                $forward_iterator(self.0.clone())
            }
        }

        $(#[$reverse_iterator_attribute])*
        pub struct $reverse_iterator<'a, P: Pattern>(pub(super) $internal_iterator<'a, P>);

        impl<'a, P> fmt::Debug for $reverse_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: fmt::Debug>,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple(stringify!($reverse_iterator))
                    .field(&self.0)
                    .finish()
            }
        }

        impl<'a, P> Iterator for $reverse_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: DoubleEndedSearcher<'a>>,
        {
            type Item = $iterty;

            #[inline]
            fn next(&mut self) -> Option<$iterty> {
                self.0.next_back()
            }
        }

        impl<'a, P> Clone for $reverse_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: Clone>,
        {
            fn clone(&self) -> Self {
                $reverse_iterator(self.0.clone())
            }
        }

        impl<'a, P: Pattern> FusedIterator for $forward_iterator<'a, P> {}

        impl<'a, P> FusedIterator for $reverse_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: DoubleEndedSearcher<'a>>,
        {}

        generate_pattern_iterators!($($t)* with $forward_iterator, $reverse_iterator, $iterty);
    };
    {
        double ended; with $forward_iterator:ident, $reverse_iterator:ident, $iterty:ty
    } => {
        impl<'a, P> DoubleEndedIterator for $forward_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: DoubleEndedSearcher<'a>>,
        {
            #[inline]
            fn next_back(&mut self) -> Option<$iterty> {
                self.0.next_back()
            }
        }

        impl<'a, P> DoubleEndedIterator for $reverse_iterator<'a, P>
        where
            P: Pattern<Searcher<'a>: DoubleEndedSearcher<'a>>,
        {
            #[inline]
            fn next_back(&mut self) -> Option<$iterty> {
                self.0.next()
            }
        }
    };
    {
        single ended; with $forward_iterator:ident, $reverse_iterator:ident, $iterty:ty
    } => {}
}

derive_pattern_clone! {
    clone SplitInternal
    with |s| SplitInternal { matcher: s.matcher.clone(), ..*s }
}

pub(super) struct SplitInternal<'a, P: Pattern> {
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) matcher: P::Searcher<'a>,
    pub(super) allow_trailing_empty: bool,
    pub(super) finished: bool,
}

impl<'a, P> fmt::Debug for SplitInternal<'a, P>
where
    P: Pattern<Searcher<'a>: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SplitInternal")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("matcher", &self.matcher)
            .field("allow_trailing_empty", &self.allow_trailing_empty)
            .field("finished", &self.finished)
            .finish()
    }
}

impl<'a, P: Pattern> SplitInternal<'a, P> {
    #[inline]
    fn get_end(&mut self) -> Option<&'a Strs> {
        if !self.finished {
            self.finished = true;

            if self.allow_trailing_empty || self.end - self.start > 0 {
                return Some(&self.matcher.haystack()[self.start..self.end]);
            }
        }

        None
    }

    #[inline]
    fn next(&mut self) -> Option<&'a Strs> {
        if self.finished {
            return None;
        }

        let haystack = self.matcher.haystack();
        match self.matcher.next_match() {
            Some((a, b)) => {
                let elt = &haystack[self.start..a];
                self.start = b;
                Some(elt)
            }
            None => self.get_end(),
        }
    }

    #[inline]
    fn next_inclusive(&mut self) -> Option<&'a Strs> {
        if self.finished {
            return None;
        }

        let haystack = self.matcher.haystack();
        match self.matcher.next_match() {
            Some((_, b)) => {
                let elt = &haystack[self.start..b];
                self.start = b;
                Some(elt)
            }
            None => self.get_end(),
        }
    }

    #[inline]
    fn next_back(&mut self) -> Option<&'a Strs>
    where
        P::Searcher<'a>: DoubleEndedSearcher<'a>,
    {
        if self.finished {
            return None;
        }

        if !self.allow_trailing_empty {
            self.allow_trailing_empty = true;
            match self.next_back() {
                Some(elt) if !elt.is_empty() => return Some(elt),
                _ => {
                    if self.finished {
                        return None;
                    }
                }
            }
        }

        let haystack = self.matcher.haystack();
        match self.matcher.next_match_back() {
            Some((a, b)) => {
                let elt = &haystack[b..self.end];
                self.end = a;
                Some(elt)
            }
            None => {
                self.finished = true;
                Some(&haystack[self.start..self.end])
            }
        }
    }

    #[inline]
    fn next_back_inclusive(&mut self) -> Option<&'a Strs>
    where
        P::Searcher<'a>: DoubleEndedSearcher<'a>,
    {
        if self.finished {
            return None;
        }

        if !self.allow_trailing_empty {
            self.allow_trailing_empty = true;
            match self.next_back_inclusive() {
                Some(elt) if !elt.is_empty() => return Some(elt),
                _ => {
                    if self.finished {
                        return None;
                    }
                }
            }
        }

        let haystack = self.matcher.haystack();
        match self.matcher.next_match_back() {
            Some((_, b)) => {
                let elt = &haystack[b..self.end];
                self.end = b;
                Some(elt)
            }
            None => {
                self.finished = true;
                Some(&haystack[self.start..self.end])
            }
        }
    }
}

generate_pattern_iterators! {
    forward:
        /// Created with the method [`split`].
        ///
        /// [`split`]: Strs::split
        struct Split;
    reverse:
        /// Created with the method [`rsplit`].
        ///
        /// [`rsplit`]: Strs::rsplit
        struct RSplit;
    internal:
        SplitInternal yielding (&'a Strs);
    delegate double ended;
}

generate_pattern_iterators! {
    forward:
        /// Created with the method [`split_terminator`].
        ///
        /// [`split_terminator`]: Strs::split_terminator
        struct SplitTerminator;
    reverse:
        /// Created with the method [`rsplit_terminator`].
        ///
        /// [`rsplit_terminator`]: Strs::rsplit_terminator
        struct RSplitTerminator;
    internal:
        SplitInternal yielding (&'a Strs);
    delegate double ended;
}

derive_pattern_clone! {
    clone SplitNInternal
    with |s| SplitNInternal { iter: s.iter.clone(), ..*s }
}

pub(super) struct SplitNInternal<'a, P: Pattern> {
    pub(super) iter: SplitInternal<'a, P>,
    /// The number of splits remaining
    pub(super) count: usize,
}

impl<'a, P> fmt::Debug for SplitNInternal<'a, P>
where
    P: Pattern<Searcher<'a>: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SplitNInternal")
            .field("iter", &self.iter)
            .field("count", &self.count)
            .finish()
    }
}

impl<'a, P: Pattern> SplitNInternal<'a, P> {
    #[inline]
    fn next(&mut self) -> Option<&'a Strs> {
        match self.count {
            0 => None,
            1 => {
                self.count = 0;
                self.iter.get_end()
            }
            _ => {
                self.count -= 1;
                self.iter.next()
            }
        }
    }

    #[inline]
    fn next_back(&mut self) -> Option<&'a Strs>
    where
        P::Searcher<'a>: DoubleEndedSearcher<'a>,
    {
        match self.count {
            0 => None,
            1 => {
                self.count = 0;
                self.iter.get_end()
            }
            _ => {
                self.count -= 1;
                self.iter.next_back()
            }
        }
    }
}

generate_pattern_iterators! {
    forward:
        /// Created with the method [`splitn`].
        ///
        /// [`splitn`]: Strs::splitn
        struct SplitN;
    reverse:
        /// Created with the method [`rsplitn`].
        ///
        /// [`rsplitn`]: Strs::rsplitn
        struct RSplitN;
    internal:
        SplitNInternal yielding (&'a Strs);
    delegate single ended;
}

derive_pattern_clone! {
    clone MatchRangesInternal
    with |s| MatchRangesInternal(s.0.clone())
}

pub(super) struct MatchRangesInternal<'a, P: Pattern>(pub(super) P::Searcher<'a>);

impl<'a, P: Pattern> MatchRangesInternal<'a, P> {
    #[inline]
    fn next(&mut self) -> Option<(Range<usize>, &'a Strs)> {
        self.0
            .next_match()
            .map(|(start, end)| (start..end, &self.0.haystack()[start..end]))
    }

    #[inline]
    fn next_back(&mut self) -> Option<(Range<usize>, &'a Strs)>
    where
        P::Searcher<'a>: DoubleEndedSearcher<'a>,
    {
        self.0
            .next_match_back()
            .map(|(start, end)| (start..end, &self.0.haystack()[start..end]))
    }
}

impl<'a, P> fmt::Debug for MatchRangesInternal<'a, P>
where
    P: Pattern<Searcher<'a>: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("MatchIndicesInternal")
            .field(&self.0)
            .finish()
    }
}

generate_pattern_iterators! {
    forward:
        /// Created with the method [`match_indices`].
        ///
        /// [`match_indices`]: Strs::match_indices
        struct MatchRanges;
    reverse:
        /// Created with the method [`rmatch_indices`].
        ///
        /// [`rmatch_indices`]: Strs::rmatch_indices
        struct RMatchRanges;
    internal:
        MatchRangesInternal yielding ((Range<usize>, &'a Strs));
    delegate double ended;
}

derive_pattern_clone! {
    clone MatchesInternal
    with |s| MatchesInternal(s.0.clone())
}

pub(super) struct MatchesInternal<'a, P: Pattern>(pub(super) P::Searcher<'a>);

impl<'a, P: Pattern> MatchesInternal<'a, P> {
    #[inline]
    fn next(&mut self) -> Option<&'a Strs> {
        self.0.next_match().map(|(a, b)| &self.0.haystack()[a..b])
    }

    #[inline]
    fn next_back(&mut self) -> Option<&'a Strs>
    where
        P::Searcher<'a>: DoubleEndedSearcher<'a>,
    {
        self.0
            .next_match_back()
            .map(|(a, b)| &self.0.haystack()[a..b])
    }
}

impl<'a, P> fmt::Debug for MatchesInternal<'a, P>
where
    P: Pattern<Searcher<'a>: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("MatchesInternal").field(&self.0).finish()
    }
}

generate_pattern_iterators! {
    forward:
        /// Created with the method [`matches`].
        ///
        /// [`matches`]: Strs::matches
        struct Matches;
    reverse:
        /// Created with the method [`rmatches`].
        ///
        /// [`rmatches`]: Strs::rmatches
        struct RMatches;
    internal:
        MatchesInternal yielding (&'a Strs);
    delegate double ended;
}

/// An iterator over the substrings of a string,
/// terminated by a substring matching to a predicate function
/// Unlike `Split`, it contains the matched part as a terminator
/// of the subslice.
///
/// This struct is created by the [`split_inclusive`] method on
/// [`str`]. See its documentation for more.
///
/// [`split_inclusive`]: Strs::split_inclusive
pub struct SplitInclusive<'a, P: Pattern>(pub(super) SplitInternal<'a, P>);

impl<'a, P: Pattern> Iterator for SplitInclusive<'a, P> {
    type Item = &'a Strs;

    #[inline]
    fn next(&mut self) -> Option<&'a Strs> {
        self.0.next_inclusive()
    }
}

impl<'a, P: Pattern<Searcher<'a>: fmt::Debug>> fmt::Debug for SplitInclusive<'a, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SplitInclusive")
            .field("0", &self.0)
            .finish()
    }
}

// FIXME(#26925) Remove in favor of `#[derive(Clone)]`
impl<'a, P: Pattern<Searcher<'a>: Clone>> Clone for SplitInclusive<'a, P> {
    fn clone(&self) -> Self {
        SplitInclusive(self.0.clone())
    }
}

impl<'a, P: Pattern<Searcher<'a>: DoubleEndedSearcher<'a>>> DoubleEndedIterator
    for SplitInclusive<'a, P>
{
    #[inline]
    fn next_back(&mut self) -> Option<&'a Strs> {
        self.0.next_back_inclusive()
    }
}

impl<'a, P: Pattern> FusedIterator for SplitInclusive<'a, P> {}
