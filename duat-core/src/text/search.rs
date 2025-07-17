//! Utilities for searching the [`Text`]
//!
//! This includes some methods for the [`Text`] itself, meant for
//! general use when editing it. It also has the [`Searcher`] struct,
//! which is used when doing [incremental search] in the
//! [`PromptLine`]. This iterator is then used in a [`IncSearcher`]
//! that can decide what to do with the results.
//!
//! [incremental search]: https://docs.rs/duat_utils/latest/duat-utils/modes/struct.IncSearcher.html
//! [`PromptLine`]: https://docs.rs/duat_utils/latest/duat-utils/widgets/struct.PromptLine.html
//! [`IncSearcher`]: https://docs.rs/duat_utils/latest/duat-utils/modes/trait.IncSearcher.html
use std::{
    collections::HashMap,
    ops::RangeBounds,
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
        &self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        self.0.bytes.search_fwd(pat, range)
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
        &self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        self.0.bytes.search_rev(pat, range)
    }

    /// Returns true if the pattern is found in the given range
    ///
    /// This is unanchored by default, if you want an anchored search,
    /// use the `"^$"` characters.
    pub fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl TextRange,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let range = range.to_range(self.len().char());
        let dfas = dfas_from_pat(pat)?;

        let (mut fwd_input, _) = get_inputs(self, range.clone());
        fwd_input.anchored(Anchored::Yes);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(hm)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input) {
            Ok(hm.offset() + range.start == range.end)
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
        &self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        let range = range.to_range(self.len().char());
        let dfas = dfas_from_pat(pat)?;

        let b_start = self.point_at(range.start).byte();

        let (mut fwd_input, mut rev_input) = get_inputs(self, range.clone());
        rev_input.anchored(Anchored::Yes);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        let mut rev_cache = dfas.rev.1.write().unwrap();

        Ok(std::iter::from_fn(move || {
            let init = fwd_input.start();
            let h_end = loop {
                if let Ok(Some(half)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input)
                {
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

            let Ok(Some(half)) = try_search_rev(&dfas.rev.0, &mut rev_cache, &mut rev_input) else {
                return None;
            };
            let h_start = half.offset();

            let p0 = self.point_at_byte(b_start + h_start);
            let p1 = self.point_at_byte(b_start + h_end);

            crate::context::debug!("{b_start}, {p0}, {p1}");

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
        &self,
        pat: R,
        range: impl TextRange,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<regex_syntax::Error>> {
        let range = range.to_range(self.len().char());
        let dfas = dfas_from_pat(pat)?;

        let b_start = self.point_at(range.start).byte();

        let (mut fwd_input, mut rev_input) = get_inputs(self, range.clone());
        fwd_input.anchored(Anchored::Yes);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        let mut rev_cache = dfas.rev.1.write().unwrap();

        Ok(std::iter::from_fn(move || {
            let init = rev_input.end();
            let start = loop {
                if let Ok(Some(half)) = try_search_rev(&dfas.rev.0, &mut rev_cache, &mut rev_input)
                {
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

            let Ok(Some(half)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input) else {
                return None;
            };
            let end = half.offset();

            let p0 = self.point_at_byte(b_start + start);
            let p1 = self.point_at_byte(b_start + end);

            Some(R::get_match([p0, p1], half.pattern()))
        }))
    }

    /// Returns true if the pattern is found in the given range
    ///
    /// This is unanchored by default, if you want an anchored search,
    /// use the `"^$"` characters.
    pub fn matches(
        &self,
        pat: impl RegexPattern,
        range: impl TextRange,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let range = range.to_range(self.len().char());
        let dfas = dfas_from_pat(pat)?;

        let (mut fwd_input, _) = get_inputs(self, range.clone());
        fwd_input.anchored(Anchored::Yes);

        let b_end = self.point_at(range.end).byte();

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(hm)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input) {
            Ok(hm.offset() == b_end)
        } else {
            Ok(false)
        }
    }
}

/// A trait to match regexes on `&str`s
pub trait Matcheable: Sized {
    /// Returns a forward [`Iterator`] over matches of a given regex
    fn search_fwd(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<impl Iterator<Item = ([usize; 2], &str)>, Box<regex_syntax::Error>>;

    /// Returns a backwards [`Iterator`] over matches of a given regex
    fn search_rev(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<impl Iterator<Item = ([usize; 2], &str)>, Box<regex_syntax::Error>>;

    /// Checks if a type matches a [`RegexPattern`]
    fn reg_matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>>;
}

impl<S: AsRef<str>> Matcheable for S {
    fn search_fwd(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<impl Iterator<Item = ([usize; 2], &str)>, Box<regex_syntax::Error>> {
        let (start, end) = crate::get_ends(range, self.as_ref().len());
        let str = &self.as_ref()[start..end];
        let dfas = dfas_from_pat(pat)?;

        let mut fwd_input = Input::new(str);
        let mut rev_input = Input::new(str);
        rev_input.anchored(Anchored::Yes);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        let mut rev_cache = dfas.rev.1.write().unwrap();

        Ok(std::iter::from_fn(move || {
            let init = fwd_input.start();
            let h_end = loop {
                if let Ok(Some(half)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input)
                {
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

            let Ok(Some(hm)) = try_search_rev(&dfas.rev.0, &mut rev_cache, &mut rev_input) else {
                return None;
            };
            let h_start = hm.offset();

            Some(([start + h_start, start + h_end], &str[h_start..h_end]))
        }))
    }

    fn search_rev(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<impl Iterator<Item = ([usize; 2], &str)>, Box<regex_syntax::Error>> {
        let (start, end) = crate::get_ends(range, self.as_ref().len());
        let str = &self.as_ref()[start..end];
        let dfas = dfas_from_pat(pat)?;

        let mut fwd_input = Input::new(str);
        fwd_input.anchored(Anchored::Yes);
        let mut rev_input = Input::new(str);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        let mut rev_cache = dfas.rev.1.write().unwrap();

        Ok(std::iter::from_fn(move || {
            let init = rev_input.end();
            let h_start = loop {
                if let Ok(Some(half)) = try_search_rev(&dfas.rev.0, &mut rev_cache, &mut rev_input)
                {
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

            rev_input.set_end(h_start);
            fwd_input.set_start(h_start);

            let Ok(Some(hm)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input) else {
                return None;
            };
            let h_end = hm.offset();

            Some(([start + h_start, start + h_end], &str[h_start..h_end]))
        }))
    }

    fn reg_matches(
        &self,
        pat: impl RegexPattern,
        range: impl RangeBounds<usize> + Clone,
    ) -> Result<bool, Box<regex_syntax::Error>> {
        let (start, end) = crate::get_ends(range, self.as_ref().len());
        let str = &self.as_ref()[start..end];
        let dfas = dfas_from_pat(pat)?;

        let mut fwd_input = Input::new(str);
        fwd_input.anchored(Anchored::Yes);

        let mut fwd_cache = dfas.fwd.1.write().unwrap();
        if let Ok(Some(hm)) = try_search_fwd(&dfas.fwd.0, &mut fwd_cache, &mut fwd_input) {
            Ok(start + hm.offset() == end)
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
        ref_bytes: &'b impl AsRef<Bytes>,
        range: impl TextRange,
    ) -> impl Iterator<Item = [Point; 2]> + 'b {
        let bytes = ref_bytes.as_ref();
        let range = range.to_range(bytes.len().char());
        let mut last_point = bytes.point_at(range.start);

        let b_start = bytes.point_at(range.start).byte();

        let (mut fwd_input, mut rev_input) = get_inputs(bytes, range.clone());
        rev_input.set_anchored(Anchored::Yes);
        
        let fwd_dfa = &self.fwd_dfa;
        let rev_dfa = &self.rev_dfa;
        let fwd_cache = &mut self.fwd_cache;
        let rev_cache = &mut self.rev_cache;

        std::iter::from_fn(move || {
            let init = fwd_input.start();
            let h_end = loop {
                if let Ok(Some(half)) = try_search_fwd(fwd_dfa, fwd_cache, &mut fwd_input) {
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

            let h_start = unsafe {
                try_search_rev(rev_dfa, rev_cache, &mut rev_input)
                    .unwrap()
                    .unwrap_unchecked()
                    .offset()
            };

            // SAFETY: If a match occurred, since the pattern _must_ be utf8,
            // every match should also be utf8, so at the very least, this
            // sequence will be utf8.
            let start = unsafe {
                bytes
                    .buffers(last_point.byte()..b_start + h_start)
                    .chars_unchecked()
                    .fold(last_point, |p, b| p.fwd(b))
            };
            let end = unsafe {
                bytes
                    .buffers(start.byte()..b_start + h_end)
                    .chars_unchecked()
                    .fold(start, |p, b| p.fwd(b))
            };

            last_point = end;

            Some([start, end])
        })
    }

    /// Searches in reverse for the required regex in a range[range]
    ///
    /// [range]: TextRange
    pub fn search_rev<'b>(
        &'b mut self,
        ref_bytes: &'b impl AsRef<Bytes>,
        range: impl TextRange,
    ) -> impl Iterator<Item = [Point; 2]> + 'b {
        let bytes = ref_bytes.as_ref();
        let range = range.to_range(bytes.len().char());
        let mut last_point = bytes.point_at(range.end);

        let b_start = bytes.point_at(range.start).byte();

        let (mut fwd_input, mut rev_input) = get_inputs(bytes, range.clone());
        fwd_input.anchored(Anchored::Yes);

        let fwd_dfa = &self.fwd_dfa;
        let rev_dfa = &self.rev_dfa;
        let fwd_cache = &mut self.fwd_cache;
        let rev_cache = &mut self.rev_cache;
        std::iter::from_fn(move || {
            let init = rev_input.end();
            let h_start = loop {
                if let Ok(Some(half)) = try_search_rev(rev_dfa, rev_cache, &mut rev_input) {
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

            fwd_input.set_start(h_start);
            rev_input.set_end(h_start);

            let h_end = unsafe {
                try_search_fwd(fwd_dfa, fwd_cache, &mut fwd_input)
                    .unwrap()
                    .unwrap_unchecked()
                    .offset()
            };

            // SAFETY: If a match occurred, since the pattern _must_ be utf8,
            // every match should also be utf8, so at the very least, this
            // sequence will be utf8.
            let end = unsafe {
                bytes
                    .buffers(b_start + h_end..last_point.byte())
                    .chars_unchecked()
                    .fold(last_point, |p, b| p.rev(b))
            };
            let start = unsafe {
                bytes
                    .buffers(b_start + h_start..end.byte())
                    .chars_unchecked()
                    .fold(end, |p, b| p.rev(b))
            };

            last_point = start;

            Some([start, end])
        })
    }

    /// Whether or not the regex matches a specific pattern
    pub fn matches(&mut self, cursor: impl Cursor) -> bool {
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

fn get_inputs(
    bytes: &Bytes,
    range: std::ops::Range<usize>,
) -> (Input<SearchBytes<'_>>, Input<SearchBytes<'_>>) {
    let haystack = SearchBytes(bytes.strs(range).to_array().map(|str| str.as_bytes()), 0);
    let fwd_input = Input::new(haystack);
    let rev_input = Input::new(haystack);
    (fwd_input, rev_input)
}
