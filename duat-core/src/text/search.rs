//! Utilities for searching the [`Text`]
//!
//! This includes some methods for the [`Text`] itself, meant for
//! general use when editing it. It also has the [`Searcher`] struct,
//! which is used when doing [incremental search] in the [`CmdLine`].
//! This iterator is then used in a [`IncSearcher`] that can decide
//! what to do with the results.
//!
//! [incremental search]: crate::widgets::IncSearch
//! [`CmdLine`]: crate::widgets::CmdLine
//! [`IncSearcher`]: crate::mode::IncSearcher
use std::{collections::HashMap, ops::RangeBounds, sync::LazyLock};

use parking_lot::{RwLock, RwLockWriteGuard};
use regex_automata::{
    Anchored, Input, PatternID,
    hybrid::dfa::{Cache, DFA},
    nfa::thompson::Config,
};

use super::{Point, Text, TextRange};

impl Text {
    pub fn search_fwd<R: RegexPattern>(
        &mut self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        let range = range.to_range_fwd(self.len().byte());
        let dfas = dfas_from_pat(pat)?;
        let haystack = unsafe {
            self.make_contiguous_in(range.clone());
            self.continuous_in_unchecked(range.clone())
        };

        let mut fwd_input = Input::new(haystack);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut fwd_cache = dfas.fwd.1.write();
        let mut rev_cache = dfas.rev.1.write();

        let ref_self = self as &Text;
        Ok(std::iter::from_fn(move || {
            let init = fwd_input.start();
            let h_end = loop {
                if let Ok(Some(half)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
                    // Ignore empty matches at the start of the input.
                    if half.offset() == init {
                        fwd_input.set_start(init + 1);
                    } else {
                        break half.offset();
                    }
                } else {
                    return None;
                }
            };

            fwd_input.set_start(h_end);
            rev_input.set_end(h_end);

            let Ok(Some(half)) = dfas.rev.0.try_search_rev(&mut rev_cache, &rev_input) else {
                return None;
            };
            let h_start = half.offset();

            let p0 = ref_self.point_at(h_start + range.start);
            let p1 = ref_self.point_at(h_end + range.start);

            Some(R::get_match((p0, p1), half.pattern()))
        }))
    }

    /// Returns an iterator over the reverse matches of the regex
    pub fn search_rev<R: RegexPattern>(
        &mut self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        let range = range.to_range_rev(self.len().byte());
        let dfas = dfas_from_pat(pat)?;
        let haystack = unsafe {
            self.make_contiguous_in(range.clone());
            self.continuous_in_unchecked(range.clone())
        };

        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack);
        let mut fwd_cache = dfas.fwd.1.write();
        let mut rev_cache = dfas.rev.1.write();

        let ref_self = self as &Text;
        let gap = range.start;
        Ok(std::iter::from_fn(move || {
            let init = rev_input.end();
            let start = loop {
                if let Ok(Some(half)) = dfas.rev.0.try_search_rev(&mut rev_cache, &rev_input) {
                    // Ignore empty matches at the end of the input.
                    if half.offset() == init {
                        rev_input.set_end(init - 1);
                    } else {
                        break half.offset();
                    }
                } else {
                    return None;
                }
            };

            rev_input.set_end(start);
            fwd_input.set_start(start);

            let Ok(Some(half)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) else {
                return None;
            };
            let end = half.offset();

            let p0 = ref_self.point_at(start + gap);
            let p1 = ref_self.point_at(end + gap);

            Some(R::get_match((p0, p1), half.pattern()))
        }))
    }

    /// Returns true if the pattern is found in the given range
    ///
    /// This is unanchored by default, if you want an anchored search,
    /// use the `"^$"` characters.
    pub fn matches(
        &mut self,
        pat: impl RegexPattern,
        range: impl TextRange,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let range = range.to_range_fwd(self.len().byte());
        let dfas = dfas_from_pat(pat)?;

        let haystack = unsafe {
            self.make_contiguous_in(range.clone());
            self.continuous_in_unchecked(range)
        };
        let fwd_input = Input::new(haystack);

        let mut fwd_cache = dfas.fwd.1.write();
        if let Ok(Some(_)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub trait Matcheable: Sized {
    fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>>;
}

impl<S: AsRef<str>> Matcheable for S {
    fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let s = self.as_ref();
        let (start, end) = crate::get_ends(range, s.len());
        let dfas = dfas_from_pat(pat)?;
        let fwd_input =
            Input::new(unsafe { std::str::from_utf8_unchecked(&s.as_bytes()[start..end]) });

        let mut fwd_cache = dfas.fwd.1.write();
        if let Ok(Some(_)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub struct Searcher {
    pat: String,
    fwd_dfa: &'static DFA,
    rev_dfa: &'static DFA,
    fwd_cache: RwLockWriteGuard<'static, Cache>,
    rev_cache: RwLockWriteGuard<'static, Cache>,
}

impl Searcher {
    pub fn new(pat: String) -> Result<Self, Box<regex_syntax::Error>> {
        let dfas = dfas_from_pat(&pat)?;
        Ok(Self {
            pat,
            fwd_dfa: &dfas.fwd.0,
            rev_dfa: &dfas.rev.0,
            fwd_cache: dfas.fwd.1.write(),
            rev_cache: dfas.rev.1.write(),
        })
    }

    pub fn search_fwd<'b>(
        &'b mut self,
        text: &'b mut Text,
        range: impl TextRange,
    ) -> impl Iterator<Item = (Point, Point)> + 'b {
        let range = range.to_range_fwd(text.len().byte());
        let haystack = unsafe {
            text.make_contiguous_in(range.clone());
            text.continuous_in_unchecked(range.clone())
        };
        let mut fwd_input = Input::new(haystack).anchored(Anchored::No);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut last_point = text.point_at(range.start);

        let fwd_dfa = &self.fwd_dfa;
        let rev_dfa = &self.rev_dfa;
        let fwd_cache = &mut self.fwd_cache;
        let rev_cache = &mut self.rev_cache;
        let gap = range.start;
        std::iter::from_fn(move || {
            let init = fwd_input.start();
            let end = loop {
                if let Ok(Some(half)) = fwd_dfa.try_search_fwd(fwd_cache, &fwd_input) {
                    // Ignore empty matches at the start of the input.
                    if half.offset() == init {
                        fwd_input.set_start(init + 1);
                    } else {
                        break half.offset();
                    }
                } else {
                    return None;
                }
            };

            fwd_input.set_start(end);
            rev_input.set_end(end);

            let half = unsafe {
                rev_dfa
                    .try_search_rev(rev_cache, &rev_input)
                    .unwrap()
                    .unwrap_unchecked()
            };
            let start = half.offset();

            let start = unsafe {
                std::str::from_utf8_unchecked(&haystack.as_bytes()[last_point.byte() - gap..start])
            }
            .chars()
            .fold(last_point, |p, b| p.fwd(b));
            let end = unsafe {
                std::str::from_utf8_unchecked(&haystack.as_bytes()[start.byte() - gap..end])
            }
            .chars()
            .fold(start, |p, b| p.fwd(b));

            last_point = end;

            Some((start, end))
        })
    }

    pub fn search_rev<'b>(
        &'b mut self,
        text: &'b mut Text,
        range: impl TextRange,
    ) -> impl Iterator<Item = (Point, Point)> + 'b {
        let range = range.to_range_rev(text.len().byte());
        let haystack = unsafe {
            text.make_contiguous_in(range.clone());
            text.continuous_in_unchecked(range.clone())
        };
        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut last_point = text.point_at(range.end);

        let fwd_dfa = &self.fwd_dfa;
        let rev_dfa = &self.rev_dfa;
        let fwd_cache = &mut self.fwd_cache;
        let rev_cache = &mut self.rev_cache;
        let gap = range.start;
        std::iter::from_fn(move || {
            let init = rev_input.end();
            let start = loop {
                if let Ok(Some(half)) = rev_dfa.try_search_rev(rev_cache, &rev_input) {
                    // Ignore empty matches at the end of the input.
                    if half.offset() == init {
                        rev_input.set_end(init - 1);
                    } else {
                        break half.offset();
                    }
                } else {
                    return None;
                }
            };

            fwd_input.set_start(start);
            rev_input.set_end(start);

            let half = fwd_dfa
                .try_search_fwd(fwd_cache, &fwd_input)
                .unwrap()
                .unwrap();

            let end = unsafe {
                std::str::from_utf8_unchecked(
                    &haystack.as_bytes()[half.offset()..(last_point.byte() - gap)],
                )
            }
            .chars()
            .fold(last_point, |p, b| p.rev(b));
            let start = unsafe {
                std::str::from_utf8_unchecked(&haystack.as_bytes()[start..(end.byte() - gap)])
            }
            .chars()
            .fold(end, |p, b| p.rev(b));

            last_point = start;

            Some((start, end))
        })
    }

    /// Whether or not the regex matches a specific pattern
    pub fn matches(&mut self, query: impl AsRef<[u8]>) -> bool {
        let input = Input::new(&query).anchored(Anchored::Yes);

        let Ok(Some(half)) = self.fwd_dfa.try_search_fwd(&mut self.fwd_cache, &input) else {
            return false;
        };

        half.offset() == query.as_ref().len()
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

    let mut list = DFA_LIST.write();

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
    Many(&'a [&'static str]),
}

impl Patterns<'_> {
    fn leak(&self) -> Patterns<'static> {
        match self {
            Patterns::One(str) => Patterns::One(String::from(*str).leak()),
            Patterns::Many(strs) => Patterns::Many(Vec::from(*strs).leak()),
        }
    }

    fn dfas(&self) -> Result<(DFA, DFA), Box<regex_syntax::Error>> {
        let mut fwd_builder = DFA::builder();
        fwd_builder.thompson(Config::new().utf8(false));
        let mut rev_builder = DFA::builder();
        rev_builder.thompson(Config::new().reverse(true).utf8(false));

        match self {
            Patterns::One(pat) => {
                regex_syntax::Parser::new().parse(pat)?;
                let fwd = fwd_builder.build(pat).unwrap();
                let rev = rev_builder.build(pat).unwrap();
                Ok((fwd, rev))
            }
            Patterns::Many(pats) => {
                for pat in *pats {
                    regex_syntax::Parser::new().parse(pat)?;
                }
                let fwd = fwd_builder.build_many(pats).unwrap();
                let rev = rev_builder.build_many(pats).unwrap();
                Ok((fwd, rev))
            }
        }
    }
}

pub trait RegexPattern: InnerRegexPattern {
    type Match: 'static;

    fn get_match(points: (Point, Point), pattern: PatternID) -> Self::Match;
}

impl RegexPattern for &str {
    type Match = (Point, Point);

    fn get_match(points: (Point, Point), _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for String {
    type Match = (Point, Point);

    fn get_match(points: (Point, Point), _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for &String {
    type Match = (Point, Point);

    fn get_match(points: (Point, Point), _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for char {
    type Match = (Point, Point);

    fn get_match(points: (Point, Point), _pattern: PatternID) -> Self::Match {
        points
    }
}

impl<const N: usize> RegexPattern for [&'static str; N] {
    type Match = (Point, Point, usize);

    fn get_match(points: (Point, Point), pattern: PatternID) -> Self::Match {
        (points.0, points.1, pattern.as_usize())
    }
}

impl RegexPattern for &[&'static str] {
    type Match = (Point, Point, usize);

    fn get_match(points: (Point, Point), pattern: PatternID) -> Self::Match {
        (points.0, points.1, pattern.as_usize())
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

impl<const N: usize> InnerRegexPattern for [&'static str; N] {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::Many(self)
    }
}

impl InnerRegexPattern for &[&'static str] {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::Many(self)
    }
}
