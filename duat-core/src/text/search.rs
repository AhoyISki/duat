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
    ops::Range,
    sync::{LazyLock, Mutex},
};

use regex_cursor::{
    Cursor, Input,
    engines::hybrid::{try_search_fwd, try_search_rev},
    regex_automata::{
        PatternID,
        hybrid::dfa::{Cache, DFA},
        nfa::thompson,
        util::syntax,
    },
};

use super::TextRange;
use crate::text::Strs;

/// An [`Iterator`] over the matches returned by a search on a
/// [haystack]
///
/// This is most commonly used with the [`Strs`] and [`Bytes`]
/// structs, although it is also available with `&str` and any type
/// implementing [`Deref<Target = str>`]
///
/// [haystack]: RegexHaystack
/// [`Deref<Target = str>`]: std::ops::Deref
#[derive(Clone)]
pub struct Matches<'m, R> {
    haystack: [&'m [u8]; 2],
    b_start: usize,
    dfas: &'static DFAs,
    fwd_input: Input<SearchBytes<'m>>,
    rev_input: Input<SearchBytes<'m>>,
    rev_match: Option<Range<usize>>,
    _ghost: PhantomData<R>,
}

impl<'m, R> Matches<'m, R> {
    /// Changes the [`TextRange`] to search on
    ///
    /// This _will_ reset the [`Iterator`], if it was returning
    /// [`None`] before, it might start returning [`Some`] again if
    /// the pattern exists in the specified [`Range`]
    pub fn range(self, range: impl TextRange) -> Self {
        let [s0, s1] = self.haystack;
        let range = range.to_range(s0.len() + s1.len());
        let i0 = &s0[range.start.min(s0.len())..range.end.min(s0.len())];
        let i1 = &s1[range.start.saturating_sub(s0.len())..range.end.saturating_sub(s0.len())];

        Self {
            fwd_input: Input::new(SearchBytes([i0, i1], 0)),
            rev_input: Input::new(SearchBytes([i0, i1], 0)),
            b_start: range.start,
            ..self
        }
    }
}

impl<'m, R: RegexPattern> Iterator for Matches<'m, R> {
    type Item = R::Match;

    fn next(&mut self) -> Option<Self::Item> {
        let mut fwd_cache = match self.dfas.fwd_cache.lock() {
            Ok(cache) => cache,
            Err(err) => err.into_inner(),
        };

        let (fwd_input, rev_input) = (&mut self.fwd_input, &mut self.rev_input);
        let h_end = try_search_fwd(&self.dfas.fwd_dfa, &mut fwd_cache, fwd_input)
            .ok()
            .flatten()?
            .offset();

        let mut rev_cache = match self.dfas.rev_cache.lock() {
            Ok(cache) => cache,
            Err(err) => err.into_inner(),
        };

        rev_input.set_range(fwd_input.start()..h_end);
        let half = try_search_rev(&self.dfas.rev_dfa, &mut rev_cache, rev_input)
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

impl<'m, R: RegexPattern> DoubleEndedIterator for Matches<'m, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let mut rev_cache = match self.dfas.rev_cache.lock() {
            Ok(cache) => cache,
            Err(err) => err.into_inner(),
        };

        let (fwd_input, rev_input) = (&mut self.fwd_input, &mut self.rev_input);

        let h_start = try_search_rev(&self.dfas.rev_dfa, &mut rev_cache, fwd_input)
            .ok()
            .flatten()?
            .offset();

        let mut fwd_cache = match self.dfas.fwd_cache.lock() {
            Ok(cache) => cache,
            Err(err) => err.into_inner(),
        };

        rev_input.set_range(h_start..fwd_input.end());
        let half = try_search_fwd(&self.dfas.fwd_dfa, &mut fwd_cache, rev_input)
            .ok()
            .flatten()?;
        let h_end = half.offset();

        fwd_input.set_end(h_start);

        // To not repeatedly match the same empty thing over and over.
        if h_start == h_end {
            if self.rev_match == Some(self.b_start + h_start..self.b_start + h_end) {
                return None;
            } else if h_start > 0 {
                fwd_input.set_end(h_start - 1);
            }
        }

        self.rev_match = Some(self.b_start + h_start..self.b_start + h_end);

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
pub trait RegexHaystack<'h> {
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
    fn search<R: RegexPattern>(&'h self, pat: R) -> Matches<'h, R> {
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
    ) -> Result<Matches<'h, R>, Box<regex_syntax::Error>>;

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
            .is_some_and(|_| matches.fwd_input.start() == matches.fwd_input.end()))
    }
}

impl<'b> RegexHaystack<'b> for Strs {
    fn try_search<R: RegexPattern>(
        &'b self,
        pat: R,
    ) -> Result<Matches<'b, R>, Box<regex_syntax::Error>> {
        let dfas = dfas_from_pat(pat)?;

        let haystack = self.to_array().map(str::as_bytes);

        Ok(Matches {
            haystack,
            b_start: self.byte_range().start,
            dfas,
            fwd_input: Input::new(SearchBytes(haystack, 0)),
            rev_input: Input::new(SearchBytes(haystack, 0)),
            rev_match: None,
            _ghost: PhantomData,
        })
    }
}

impl<'s, S: std::ops::Deref<Target = str>> RegexHaystack<'s> for S {
    fn try_search<R: RegexPattern>(
        &'s self,
        pat: R,
    ) -> Result<Matches<'s, R>, Box<regex_syntax::Error>> {
        let dfas = dfas_from_pat(pat)?;

        let haystack = [self.deref().as_bytes(), &[]];

        Ok(Matches {
            haystack,
            b_start: 0,
            dfas,
            fwd_input: Input::new(SearchBytes(haystack, 0)),
            rev_input: Input::new(SearchBytes(haystack, 0)),
            rev_match: None,
            _ghost: PhantomData,
        })
    }
}

struct DFAs {
    fwd_dfa: DFA,
    fwd_cache: Mutex<Cache>,
    rev_dfa: DFA,
    rev_cache: Mutex<Cache>,
}

fn dfas_from_pat(pat: impl RegexPattern) -> Result<&'static DFAs, Box<regex_syntax::Error>> {
    static DFA_LIST: LazyLock<Mutex<HashMap<Patterns<'static>, &'static DFAs>>> =
        LazyLock::new(Mutex::default);

    let mut list = DFA_LIST.lock().unwrap();

    let mut bytes = [0; 4];
    let pat = pat.as_patterns(&mut bytes);

    if let Some(dfas) = list.get(&pat) {
        Ok(*dfas)
    } else {
        let pat = pat.leak();
        let (fwd_dfa, rev_dfa) = pat.dfas()?;

        let (fwd_cache, rev_cache) = (Cache::new(&fwd_dfa), Cache::new(&rev_dfa));
        let dfas = Box::leak(Box::new(DFAs {
            fwd_dfa,
            fwd_cache: Mutex::new(fwd_cache),
            rev_dfa,
            rev_cache: Mutex::new(rev_cache),
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
struct SearchBytes<'a>([&'a [u8]; 2], usize);

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
