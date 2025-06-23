//! Utilities for searching the [`Text`]
//!
//! This includes some methods for the [`Text`] itself, meant for
//! general use when editing it. It also has the [`Searcher`] struct,
//! which is used when doing [incremental search] in the [`CmdLine`].
//! This iterator is then used in a [`IncSearcher`] that can decide
//! what to do with the results.
//!
//! [incremental search]: crate::widget::IncSearch
//! [`CmdLine`]: crate::widget::CmdLine
//! [`IncSearcher`]: crate::mode::IncSearcher
use std::{
    collections::HashMap,
    ops::RangeBounds,
    sync::{LazyLock, RwLock, RwLockWriteGuard},
};

use regex_automata::{
    Anchored, Input, PatternID,
    hybrid::dfa::{Cache, DFA},
    nfa::thompson,
    util::syntax,
};

use super::{Bytes, Point, Text, TextRange};

impl Text {
    /// Searches forward for a [`RegexPattern`] in a [range]
    ///
    /// A [`RegexPattern`] can either be a single regex string, an
    /// array of strings, or a slice of strings. When there are more
    /// than one pattern, The return value will include which pattern
    /// matched.
    ///
    /// The patterns will also automatically be cached, so you don't
    /// need to do that.
    ///
    /// [range]: TextRange
    pub fn search_fwd<R: RegexPattern>(
        &mut self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        self.bytes_mut().search_fwd(pat, range)
    }

    /// Searches in reverse for a [`RegexPattern`] in a [range]
    ///
    /// A [`RegexPattern`] can either be a single regex string, an
    /// array of strings, or a slice of strings. When there are more
    /// than one pattern, The return value will include which pattern
    /// matched.
    ///
    /// The patterns will also automatically be cached, so you don't
    /// need to do that.
    ///
    /// [range]: TextRange
    pub fn search_rev<R: RegexPattern>(
        &mut self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        self.bytes_mut().search_rev(pat, range)
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
        let range = range.to_range(self.len().byte());
        let dfas = dfas_from_pat(pat)?;

        let haystack = self.contiguous(range);
        let fwd_input = Input::new(haystack);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(_)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl Bytes {
    /// Searches forward for a [`RegexPattern`] in a [range]
    ///
    /// A [`RegexPattern`] can either be a single regex string, an
    /// array of strings, or a slice of strings. When there are more
    /// than one pattern, The return value will include which pattern
    /// matched.
    ///
    /// The patterns will also automatically be cached, so you don't
    /// need to do that.
    ///
    /// [range]: TextRange
    pub fn search_fwd<R: RegexPattern>(
        &mut self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        let range = range.to_range(self.len().byte());
        let dfas = dfas_from_pat(pat)?;
        let haystack = {
            self.make_contiguous(range.clone());
            self.get_contiguous(range.clone()).unwrap()
        };

        let mut fwd_input = Input::new(haystack);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        let mut rev_cache = dfas.rev.1.write().unwrap();

        let bytes = self as &super::Bytes;
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

            let p0 = bytes.point_at(h_start + range.start);
            let p1 = bytes.point_at(h_end + range.start);

            Some(R::get_match([p0, p1], half.pattern()))
        }))
    }

    /// Searches in reverse for a [`RegexPattern`] in a [range]
    ///
    /// A [`RegexPattern`] can either be a single regex string, an
    /// array of strings, or a slice of strings. When there are more
    /// than one pattern, The return value will include which pattern
    /// matched.
    ///
    /// The patterns will also automatically be cached, so you don't
    /// need to do that.
    ///
    /// [range]: TextRange
    pub fn search_rev<R: RegexPattern>(
        &mut self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        let range = range.to_range(self.len().byte());
        let dfas = dfas_from_pat(pat)?;
        let haystack = {
            self.make_contiguous(range.clone());
            self.get_contiguous(range.clone()).unwrap()
        };

        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack);
        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        let mut rev_cache = dfas.rev.1.write().unwrap();

        let bytes = self as &super::Bytes;
        let gap = range.start;
        Ok(std::iter::from_fn(move || {
            let init = rev_input.end();
            let start = loop {
                if let Ok(Some(half)) = dfas.rev.0.try_search_rev(&mut rev_cache, &rev_input) {
                    // Ignore empty matches at the end of the input.
                    if half.offset() == init {
                        rev_input.set_end(init.checked_sub(1)?);
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

            let p0 = bytes.point_at(start + gap);
            let p1 = bytes.point_at(end + gap);

            Some(R::get_match([p0, p1], half.pattern()))
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
        let range = range.to_range(self.len().byte());
        let dfas = dfas_from_pat(pat)?;

        let haystack = self.contiguous(range);
        let fwd_input = Input::new(haystack);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(_)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

/// A trait to match regexes on `&str`s
pub trait Matcheable: Sized {
    /// Checks if a type matches a [`RegexPattern`]
    fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>>;
}

impl<const N: usize> Matcheable for std::array::IntoIter<&str, N> {
    fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let str: String = self.as_slice().iter().copied().collect();
        let (start, end) = crate::get_ends(range, str.len());
        let dfas = dfas_from_pat(pat)?;
        let fwd_input =
            Input::new(unsafe { std::str::from_utf8_unchecked(&str.as_bytes()[start..end]) });

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(_)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl Matcheable for &'_ str {
    fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let (start, end) = crate::get_ends(range, self.len());
        let dfas = dfas_from_pat(pat)?;
        let fwd_input =
            Input::new(unsafe { std::str::from_utf8_unchecked(&self.as_bytes()[start..end]) });

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(_)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

/// A struct for incremental searching in [`IncSearch`]
///
/// [`IncSearch`]: docs.rs/duat-utils/latest/duat_utils/modes/struct.IncSearch.html
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
        let dfas = dfas_from_pat(&pat)?;
        Ok(Self {
            pat,
            fwd_dfa: &dfas.fwd.0,
            rev_dfa: &dfas.rev.0,
            fwd_cache: dfas.fwd.1.write().unwrap(),
            rev_cache: dfas.rev.1.write().unwrap(),
        })
    }

    /// Searches forward for the required regex in a [range]
    ///
    /// [range]: TextRange
    pub fn search_fwd<'b>(
        &'b mut self,
        as_mut_bytes: &'b mut impl AsMutBytes,
        range: impl TextRange,
    ) -> impl Iterator<Item = [Point; 2]> + 'b {
        let bytes = as_mut_bytes.as_mut_bytes();
        let range = range.to_range(bytes.len().byte());
        let mut last_point = bytes.point_at(range.start);

        let haystack = bytes.contiguous(range.clone());
        let mut fwd_input = Input::new(haystack).anchored(Anchored::No);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);

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

            Some([start, end])
        })
    }

    /// Searches in reverse for the required regex in a range[range]
    ///
    /// [range]: TextRange
    pub fn search_rev<'b>(
        &'b mut self,
        as_mut_bytes: &'b mut impl AsMutBytes,
        range: impl TextRange,
    ) -> impl Iterator<Item = [Point; 2]> + 'b {
        let bytes = as_mut_bytes.as_mut_bytes();
        let range = range.to_range(bytes.len().byte());
        let mut last_point = bytes.point_at(range.end);

        let haystack = bytes.contiguous(range.clone());
        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);

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

            Some([start, end])
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
                regex_syntax::Parser::new().parse(&pat)?;
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
    /// Either two [`Point`]s, or two [`Point`]s and a match index
    type Match: 'static;

    /// transforms a matched pattern into [`RegexPattern::Match`]
    fn get_match(points: [Point; 2], pattern: PatternID) -> Self::Match;
}

impl RegexPattern for &str {
    type Match = [Point; 2];

    fn get_match(points: [Point; 2], _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for String {
    type Match = [Point; 2];

    fn get_match(points: [Point; 2], _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for &String {
    type Match = [Point; 2];

    fn get_match(points: [Point; 2], _pattern: PatternID) -> Self::Match {
        points
    }
}

impl RegexPattern for char {
    type Match = [Point; 2];

    fn get_match(points: [Point; 2], _pattern: PatternID) -> Self::Match {
        points
    }
}

impl<const N: usize> RegexPattern for [&str; N] {
    type Match = (usize, [Point; 2]);

    fn get_match(points: [Point; 2], pattern: PatternID) -> Self::Match {
        (pattern.as_usize(), points)
    }
}

impl RegexPattern for &[&str] {
    type Match = (usize, [Point; 2]);

    fn get_match(points: [Point; 2], pattern: PatternID) -> Self::Match {
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

/// Either [`Text`] or [`Bytes`]
pub trait AsMutBytes {
    fn as_mut_bytes(&mut self) -> &mut Bytes;
}

impl AsMutBytes for Bytes {
    fn as_mut_bytes(&mut self) -> &mut Bytes {
        self
    }
}

impl AsMutBytes for Text {
    fn as_mut_bytes(&mut self) -> &mut Bytes {
        self.bytes_mut()
    }
}
