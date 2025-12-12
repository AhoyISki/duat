//! Utilities for searching the [`Text`]
//!
//! This includes some methods for the [`Text`] itself, meant for
//! general use when editing it. It also has the [`Searcher`] struct,
//! which is used when doing [incremental search] in the
//! [`PromptLine`]. This iterator is then used in a [`IncSearcher`]
//! that can decide what to do with the results.
//!
//! [incremental search]: https://docs.rs/duat/latest/duat/modes/struct.IncSearcher.html
//! [`PromptLine`]: https://docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
//! [`IncSearcher`]: https://docs.rs/duat/latest/duat/modes/trait.IncSearcher.html
//! [`Text`]: super::Text
use std::{
    collections::HashMap,
    marker::PhantomData,
    ops::{DerefMut, Range},
    sync::{LazyLock, RwLock, RwLockWriteGuard},
};

use regex_cursor::{
    Cursor, Input,
    engines::hybrid::{try_search_fwd, try_search_rev},
    regex_automata::{
        Anchored, PatternID,
        hybrid::dfa::{Cache, DFA},
        nfa::thompson,
        util::syntax,
    },
};

use super::TextRange;
use crate::text::{Bytes, Strs, Text};

/// An [`Iterator`] over the matches returned by a search on a
/// [haystack]
///
/// This is most commonly used with the [`Strs`] and [`Bytes`]
/// structs, although it is also available with `&str` and any type
/// implementing [`Deref<Target = str>`]
///
/// [haystack]: RegexHaystack
/// [`Deref<Target = str>`]: std::ops::Deref
pub struct Matches<'m, H, R, Cc = RwLockWriteGuard<'m, Cache>, C = SearchBytes<'m>>
where
    H: RegexHaystack<'m, C> + ?Sized,
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
    C: Cursor,
{
    haystack: &'m H,
    b_start: usize,
    dfas: FwdRev<&'static DFA>,
    input: FwdRev<Input<C>>,
    cache: FwdRev<Cc>,
    _ghost: PhantomData<R>,
}

impl<'m, H, R, C, Cc> Matches<'m, H, R, Cc, C>
where
    H: RegexHaystack<'m, C> + ?Sized,
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
    C: Cursor,
{
    /// Changes the [`TextRange`] to search on
    ///
    /// This _will_ reset the [`Iterator`], if it was returning
    /// [`None`] before, it might start returning [`Some`] again if
    /// the pattern exists in the specified [`Range`]
    pub fn range(self, range: impl TextRange) -> Self {
        let range = range.to_range(self.haystack.range().end);
        Self {
            input: self.haystack.get_inputs(range.clone()),
            b_start: range.start,
            ..self
        }
    }
}

impl<'m, H, R, C, Cc> Iterator for Matches<'m, H, R, Cc, C>
where
    H: RegexHaystack<'m, C> + ?Sized,
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
    C: Cursor,
{
    type Item = R::Match;

    fn next(&mut self) -> Option<Self::Item> {
        let (fwd_input, rev_input) = (&mut self.input.fwd, &mut self.input.rev);
        let h_end = try_search_fwd(self.dfas.fwd, &mut self.cache.fwd, fwd_input)
            .ok()
            .flatten()?
            .offset();

        rev_input.set_range(fwd_input.start()..h_end);
        let half = try_search_rev(self.dfas.rev, &mut self.cache.rev, rev_input)
            .ok()
            .flatten()?;
        let h_start = half.offset();

        fwd_input.set_start(h_end);

        // To not repeatedly match the same empty thing over and over.
        if h_start == h_end {
            fwd_input.set_start(h_end + 1);
        }

        Some(R::get_match(
            self.b_start + h_start..self.b_start + h_end,
            half.pattern(),
        ))
    }
}

impl<'m, H, R, C, Cc> DoubleEndedIterator for Matches<'m, H, R, Cc, C>
where
    H: RegexHaystack<'m, C> + ?Sized,
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
    C: Cursor,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let (fwd_input, rev_input) = (&mut self.input.fwd, &mut self.input.rev);
        let h_start = try_search_rev(self.dfas.rev, &mut self.cache.rev, fwd_input)
            .ok()
            .flatten()?
            .offset();

        rev_input.set_range(h_start..fwd_input.end());
        let half = try_search_fwd(self.dfas.fwd, &mut self.cache.fwd, rev_input)
            .ok()
            .flatten()?;
        let h_end = half.offset();

        fwd_input.set_end(h_start);

        if h_start == h_end {
            fwd_input.set_end(h_start.checked_sub(1)?);
        }

        Some(R::get_match(
            self.b_start + h_start..self.b_start + h_end,
            half.pattern(),
        ))
    }
}

/// A type searcheable by [`DFA`]s
///
/// This type is used to create the [`Matches`] [`Iterator`], a useful
/// and configurable iterator over the matches in the `Haystack`,
/// primarily on the [`Bytes`] type.
pub trait RegexHaystack<'h, C: Cursor> {
    /// An [`Iterator`] over the matches for a given [`RegexPattern`]
    ///
    /// This `Iterator` will search through the entire range of the
    /// haystack. If the haystack is [`Strs`], for example, then it
    /// will search through the [`Strs::byte_range`]. You can also set
    /// a custom range for search through the [`Matches::range`]
    /// method, which will reset the search to encompass the part of a
    /// [`TextRange`] that is clipped by the haystack.
    ///
    /// This `Iterator` also implements [`DoubleEndedIterator`], which
    /// means that you can get the elements in reverse order.
    ///
    /// # Panics
    ///
    /// This function will panic if the [`RegexPattern`] isn't valid.
    /// If you want a non panicking variety, check out
    /// [`RegexHaystack::try_search`]
    #[track_caller]
    fn search<R: RegexPattern>(
        &'h self,
        pat: R,
    ) -> Matches<'h, Self, R, impl DerefMut<Target = Cache>, C> {
        match self.try_search(pat) {
            Ok(matches) => matches,
            Err(err) => panic!("{err}"),
        }
    }

    /// An [`Iterator`] over the matches for a given [`RegexPattern`]
    ///
    /// This `Iterator` will search through the entire range of the
    /// haystack. If the haystack is [`Strs`], for example, then it
    /// will search through the [`Strs::byte_range`]. You can also set
    /// a custom range for search through the [`Matches::range`]
    /// method, which will reset the search to encompass the part of a
    /// [`TextRange`] that is clipped by the haystack.
    ///
    /// This `Iterator` also implements [`DoubleEndedIterator`], which
    /// means that you can get the elements in reverse order.
    ///
    /// This function will return [`Err`] if the regex pattern is not
    /// valid. If you want a panicking variety, check out
    /// [`RegexHaystack::search`]
    fn try_search<R: RegexPattern>(
        &'h self,
        pat: R,
    ) -> Result<Matches<'h, Self, R, impl DerefMut<Target = Cache>, C>, Box<regex_syntax::Error>>
    {
        let dfas = dfas_from_pat(pat)?;

        Ok(Matches {
            haystack: self,
            b_start: self.range().start,
            dfas: FwdRev { fwd: &dfas.fwd.0, rev: &dfas.rev.0 },
            input: self.get_inputs(self.range()),
            cache: FwdRev {
                fwd: dfas.fwd.1.write().unwrap(),
                rev: dfas.rev.1.write().unwrap(),
            },
            _ghost: PhantomData,
        })
    }

    /// Wether this haystack contains a match for a [`RegexPattern`]
    ///
    /// This is equivalent to calling `self.search().map(|iter|
    /// iter.next().is_some())`.
    ///
    /// This function will return [`Err`] if the regex pattern is not
    /// valid.
    fn contains_pat(&'h self, pat: impl RegexPattern) -> Result<bool, Box<regex_syntax::Error>> {
        Ok(self.try_search(pat)?.next().is_some())
    }

    /// Wether this haystack matches the [`RegexPattern`] exactly
    ///
    /// This function will return [`Err`] if the regex pattern is not
    /// valid.
    fn matches_pat(&'h self, pat: impl RegexPattern) -> Result<bool, Box<regex_syntax::Error>> {
        let mut matches = self.try_search(pat)?;
        Ok(matches
            .next()
            .is_some_and(|_| matches.input.fwd.start() == self.range().end))
    }

    /// Get [`Input`]s from this type on a given [`Range`]
    #[allow(private_interfaces)]
    fn get_inputs(&'h self, range: std::ops::Range<usize>) -> FwdRev<Input<C>>;

    /// The range that a regex search can make use of
    #[doc(hidden)]
    fn range(&self) -> Range<usize>;
}

impl<'b> RegexHaystack<'b, SearchBytes<'b>> for Text {
    #[allow(private_interfaces)]
    fn get_inputs(&'b self, range: std::ops::Range<usize>) -> FwdRev<Input<SearchBytes<'b>>> {
        let haystack = SearchBytes(self.slices(range).to_array(), 0);
        let mut rev = Input::new(haystack);
        rev.anchored(Anchored::Yes);
        FwdRev { fwd: Input::new(haystack), rev }
    }

    fn range(&self) -> Range<usize> {
        0..self.len().byte()
    }
}

impl<'b> RegexHaystack<'b, SearchBytes<'b>> for Bytes {
    #[allow(private_interfaces)]
    fn get_inputs(&'b self, range: std::ops::Range<usize>) -> FwdRev<Input<SearchBytes<'b>>> {
        let haystack = SearchBytes(self.slices(range).to_array(), 0);
        let mut rev = Input::new(haystack);
        rev.anchored(Anchored::Yes);
        FwdRev { fwd: Input::new(haystack), rev }
    }

    fn range(&self) -> Range<usize> {
        0..self.len().byte()
    }
}

impl<'b> RegexHaystack<'b, SearchBytes<'b>> for Strs<'b> {
    #[allow(private_interfaces)]
    fn get_inputs(&'b self, range: std::ops::Range<usize>) -> FwdRev<Input<SearchBytes<'b>>> {
        let haystack = SearchBytes(self.slices(range).to_array(), 0);
        let mut rev = Input::new(haystack);
        rev.anchored(Anchored::Yes);
        FwdRev { fwd: Input::new(haystack), rev }
    }

    fn range(&self) -> Range<usize> {
        self.byte_range()
    }
}

impl<'s, S: std::ops::Deref<Target = str>> RegexHaystack<'s, &'s [u8]> for S {
    #[allow(private_interfaces)]
    fn get_inputs(&'s self, range: std::ops::Range<usize>) -> FwdRev<Input<&'s [u8]>> {
        let range = range.start.min(self.len())..range.end.min(self.len());
        let mut rev = Input::new(&self.as_bytes()[range.clone()]);
        rev.set_anchored(Anchored::Yes);
        FwdRev {
            fwd: Input::new(&self.as_bytes()[range]),
            rev,
        }
    }

    fn range(&self) -> Range<usize> {
        0..self.len()
    }
}

/// A struct for incremental searching in [`IncSearch`]
///
/// [`IncSearch`]: docs.rs/duat/latest/duat/modes/struct.IncSearch.html
pub struct Searcher {
    pat: String,
    fwd_dfa: &'static DFA,
    rev_dfa: &'static DFA,
    fwd_cache: RwLockWriteGuard<'static, Cache>,
    rev_cache: RwLockWriteGuard<'static, Cache>,
}

impl Searcher {
    /// Returns a new [`Searcher`]
    pub fn new(pat: String) -> Result<Self, Box<regex_syntax::Error>> {
        let dfas = dfas_from_pat(pat.as_str())?;
        Ok(Self {
            pat,
            fwd_dfa: &dfas.fwd.0,
            rev_dfa: &dfas.rev.0,
            fwd_cache: dfas.fwd.1.write().unwrap(),
            rev_cache: dfas.rev.1.write().unwrap(),
        })
    }

    /// Searches for this `Searcher`'s pattern through [`Strs`]
    pub fn search<'b, H: RegexHaystack<'b, C>, C: Cursor>(
        &'b mut self,
        haystack: &'b H,
    ) -> Matches<'b, H, String, &'b mut Cache, C> {
        let input = haystack.get_inputs(haystack.range());

        Matches {
            haystack,
            b_start: haystack.range().start,
            dfas: FwdRev { fwd: self.fwd_dfa, rev: self.rev_dfa },
            input,
            cache: FwdRev {
                fwd: &mut self.fwd_cache,
                rev: &mut self.rev_cache,
            },
            _ghost: PhantomData,
        }
    }

    /// Whether or not the regex matches a specific pattern
    pub fn matches_pat(&mut self, cursor: impl Cursor) -> bool {
        let total_bytes = cursor.total_bytes();

        let mut input = Input::new(cursor);
        input.anchored(Anchored::Yes);

        let Ok(Some(half)) = try_search_fwd(self.fwd_dfa, &mut self.fwd_cache, &mut input) else {
            return false;
        };

        total_bytes.is_some_and(|len| len == half.offset())
    }

    /// Whether or not there even is a pattern to search for
    pub fn is_empty(&self) -> bool {
        self.pat.is_empty()
    }
}

struct DFAs {
    fwd: (DFA, RwLock<Cache>),
    rev: (DFA, RwLock<Cache>),
}

fn dfas_from_pat(pat: impl RegexPattern) -> Result<&'static DFAs, Box<regex_syntax::Error>> {
    static DFA_LIST: LazyLock<RwLock<HashMap<Patterns<'static>, &'static DFAs>>> =
        LazyLock::new(RwLock::default);

    let mut list = DFA_LIST.write().unwrap();

    let mut bytes = [0; 4];
    let pat = pat.as_patterns(&mut bytes);

    if let Some(dfas) = list.get(&pat) {
        Ok(*dfas)
    } else {
        let pat = pat.leak();
        let (fwd, rev) = pat.dfas()?;

        let (fwd_cache, rev_cache) = (Cache::new(&fwd), Cache::new(&rev));
        let dfas = Box::leak(Box::new(DFAs {
            fwd: (fwd, RwLock::new(fwd_cache)),
            rev: (rev, RwLock::new(rev_cache)),
        }));
        let _ = list.insert(pat, dfas);
        Ok(dfas)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Patterns<'a> {
    One(&'a str),
    Many(&'a [&'a str]),
}

impl Patterns<'_> {
    fn leak(&self) -> Patterns<'static> {
        match self {
            Patterns::One(str) => Patterns::One(String::from(*str).leak()),
            Patterns::Many(strs) => Patterns::Many(
                strs.iter()
                    .map(|s| {
                        let str: &'static str = s.to_string().leak();
                        str
                    })
                    .collect::<Vec<&'static str>>()
                    .leak(),
            ),
        }
    }

    fn dfas(&self) -> Result<(DFA, DFA), Box<regex_syntax::Error>> {
        let mut fwd_builder = DFA::builder();
        fwd_builder.syntax(syntax::Config::new().multi_line(true));
        let mut rev_builder = DFA::builder();
        rev_builder
            .syntax(syntax::Config::new().multi_line(true))
            .thompson(thompson::Config::new().reverse(true));

        match self {
            Patterns::One(pat) => {
                let pat = pat.replace("\\b", "(?-u:\\b)");
                syntax::parse(&pat)?;
                let fwd = fwd_builder.build(&pat).unwrap();
                let rev = rev_builder.build(&pat).unwrap();
                Ok((fwd, rev))
            }
            Patterns::Many(pats) => {
                let pats: Vec<String> =
                    pats.iter().map(|p| p.replace("\\b", "(?-u:\\b)")).collect();
                for pat in pats.iter() {
                    regex_syntax::Parser::new().parse(pat)?;
                }
                let fwd = fwd_builder.build_many(&pats).unwrap();
                let rev = rev_builder.build_many(&pats).unwrap();
                Ok((fwd, rev))
            }
        }
    }
}

/// A regex pattern to search for
///
/// It can either be a single `&str`, or a list of `&str`s, in which
/// case the matched pattern will be specified.
pub trait RegexPattern: InnerRegexPattern {
    /// Eiter a [`Range<usize>`] or `(usize, Range<usize>)`
    type Match;

    /// transforms a matched pattern into [`RegexPattern::Match`]
    fn get_match(points: Range<usize>, pattern: PatternID) -> Self::Match;
}

impl RegexPattern for &str {
    type Match = Range<usize>;

    fn get_match(points: Range<usize>, _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for String {
    type Match = Range<usize>;

    fn get_match(points: Range<usize>, _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for &String {
    type Match = Range<usize>;

    fn get_match(points: Range<usize>, _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for char {
    type Match = Range<usize>;

    fn get_match(points: Range<usize>, _pattern: PatternID) -> Self::Match {
        points
    }
}

impl<const N: usize> RegexPattern for [&str; N] {
    type Match = (usize, Range<usize>);

    fn get_match(points: Range<usize>, pattern: PatternID) -> Self::Match {
        (pattern.as_usize(), points)
    }
}

impl RegexPattern for &[&str] {
    type Match = (usize, Range<usize>);

    fn get_match(points: Range<usize>, pattern: PatternID) -> Self::Match {
        (pattern.as_usize(), points)
    }
}

trait InnerRegexPattern {
    fn as_patterns<'b>(&'b self, bytes: &'b mut [u8; 4]) -> Patterns<'b>;
}

impl InnerRegexPattern for &str {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::One(self)
    }
}

impl InnerRegexPattern for String {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::One(self)
    }
}

impl InnerRegexPattern for &String {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::One(self)
    }
}

impl InnerRegexPattern for char {
    fn as_patterns<'b>(&'b self, bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::One(self.encode_utf8(bytes) as &str)
    }
}

impl<const N: usize> InnerRegexPattern for [&str; N] {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::Many(self)
    }
}

impl InnerRegexPattern for &[&str] {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::Many(self)
    }
}

#[derive(Clone, Copy)]
#[doc(hidden)]
pub struct SearchBytes<'a>([&'a [u8]; 2], usize);

impl Cursor for SearchBytes<'_> {
    fn chunk(&self) -> &[u8] {
        self.0[self.1]
    }

    fn advance(&mut self) -> bool {
        if self.1 == 0 {
            self.1 += 1;
            true
        } else {
            false
        }
    }

    fn backtrack(&mut self) -> bool {
        if self.1 == 1 {
            self.1 -= 1;
            true
        } else {
            false
        }
    }

    fn total_bytes(&self) -> Option<usize> {
        Some(self.0[0].len() + self.0[1].len())
    }

    fn offset(&self) -> usize {
        match self.1 {
            1 => self.0[0].len(),
            _ => 0,
        }
    }
}

#[derive(Clone, Copy)]
struct FwdRev<T> {
    fwd: T,
    rev: T,
}
