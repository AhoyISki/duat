use std::{collections::HashMap, sync::LazyLock};

use parking_lot::{RwLock, RwLockWriteGuard};
use regex_automata::{
    hybrid::{
        dfa::{Cache, DFA},
        BuildError,
    },
    nfa::thompson::Config,
    Anchored, Input, MatchKind, PatternID,
};
use regex_syntax::hir::{Hir, HirKind};

use super::{Point, Text};

impl Text {
    pub fn search_from<R>(
        &mut self,
        pat: R,
        at: Point,
        end: Option<Point>,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<BuildError>>
    where
        R: RegexPattern,
    {
        let dfas = dfas_from_pat(pat)?;

        let haystack = match end {
            Some(end) => unsafe {
                self.make_contiguous_in(at.byte()..end.byte());
                self.continuous_in_unchecked(at.byte()..end.byte())
            },
            None => unsafe {
                self.make_contiguous_in(at.byte()..);
                self.continuous_in_unchecked(at.byte()..)
            },
        };
        let mut fwd_input = Input::new(haystack);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut fwd_cache = dfas.fwd.1.write();
        let mut rev_cache = dfas.rev.1.write();

        let ref_self = self as &Text;
        let gap = at.byte();
        Ok(std::iter::from_fn(move || {
            let Ok(Some(half)) = dfas.fwd.0.try_search_fwd(&mut fwd_cache, &fwd_input) else {
                return None;
            };
            let end = half.offset();

            fwd_input.set_start(end);
            rev_input.set_end(end);

            let Ok(Some(half)) = dfas.rev.0.try_search_rev(&mut rev_cache, &rev_input) else {
                return None;
            };
            let start = half.offset();

            let p0 = ref_self.point_at(start + gap);
            let p1 = ref_self.point_at(end + gap);

            Some(R::get_match((p0, p1), half.pattern()))
        }))
    }

    /// Returns an iterator over the reverse matches of the regex
    pub fn search_from_rev<R>(
        &mut self,
        pat: R,
        at: Point,
        start: Option<Point>,
    ) -> Result<impl Iterator<Item = R::Match> + '_, Box<BuildError>>
    where
        R: RegexPattern,
    {
        let dfas = dfas_from_pat(pat)?;

        let haystack = match start {
            Some(start) => unsafe {
                self.make_contiguous_in(start.byte()..at.byte());
                self.continuous_in_unchecked(start.byte()..at.byte())
            },
            None => unsafe {
                self.make_contiguous_in(..at.byte());
                self.continuous_in_unchecked(..at.byte())
            },
        };
        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack);
        let mut fwd_cache = dfas.fwd.1.write();
        let mut rev_cache = dfas.rev.1.write();

        let ref_self = self as &Text;
        let gap = start.map(|p| p.byte()).unwrap_or(0);
        Ok(std::iter::from_fn(move || {
            let Ok(Some(half)) = dfas.rev.0.try_search_rev(&mut rev_cache, &rev_input) else {
                return None;
            };
            let start = half.offset();

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
}

pub struct Searcher<'a> {
    fwd_dfa: &'static DFA,
    rev_dfa: &'static DFA,
    fwd_cache: RwLockWriteGuard<'static, Cache>,
    rev_cache: RwLockWriteGuard<'static, Cache>,
    fwd_matches: &'a mut Vec<Matches>,
    rev_matches: &'a mut Vec<Matches>,
}

impl<'a> Searcher<'a> {
    pub fn search_from(
        &'a mut self,
        text: &'a mut Text,
        at: Point,
        end: Option<Point>,
    ) -> impl Iterator<Item = (Point, Point)> + 'a {
        let matches = match self
            .fwd_matches
            .binary_search_by_key(&at.byte(), |m| m.start.byte())
        {
            Ok(i) => &mut self.fwd_matches[i],
            Err(i) => {
                self.fwd_matches.insert(i, Matches::new(at));
                &mut self.fwd_matches[i]
            }
        };

        let haystack = match end {
            Some(end) => unsafe {
                text.make_contiguous_in(at.byte()..end.byte());
                text.continuous_in_unchecked(at.byte()..end.byte())
            },
            None => unsafe {
                text.make_contiguous_in(at.byte()..);
                text.continuous_in_unchecked(at.byte()..)
            },
        };
        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut match_i = 0;

        let text = text as &Text;
        let fwd_dfa = &self.fwd_dfa;
        let rev_dfa = &self.rev_dfa;
        let fwd_cache = &mut self.fwd_cache;
        let rev_cache = &mut self.rev_cache;
        let gap = at.byte();
        std::iter::from_fn(move || {
            while let Some((start, end)) = matches.list.get_mut(match_i) {
                fwd_input.set_start(start.byte() - gap);

                match fwd_dfa.try_search_fwd(fwd_cache, &fwd_input) {
                    Ok(Some(half)) => {
                        match_i += 1;
                        *end = text.point_at(half.offset() + gap);
                        matches.end = matches.end.max(*end);
                        return Some((*start, *end));
                    }
                    _ => {
                        matches.list.remove(match_i);
                    }
                }
            }

            // To prevent subsequently added matches from being checked.
            if match_i != usize::MAX {
                match_i = usize::MAX;
                fwd_input.set_anchored(Anchored::No);
                fwd_input.set_start(matches.end.byte() - gap);
            }

            let Ok(Some(half)) = fwd_dfa.try_search_fwd(fwd_cache, &fwd_input) else {
                matches.end = end.unwrap_or_else(|| text.len_point());
                return None;
            };
            let end = half.offset();

            fwd_input.set_start(end);
            rev_input.set_end(end);

            let half = rev_dfa
                .try_search_rev(rev_cache, &rev_input)
                .unwrap()
                .unwrap();
            let start = half.offset();

            let (start, end) = (text.point_at(start + gap), text.point_at(end + gap));
            matches.list.push((start, end));
            matches.end = end;

            Some((start, end))
        })
    }

    pub fn search_from_rev(
        &'a mut self,
        text: &'a mut Text,
        at: Point,
        start: Option<Point>,
    ) -> impl Iterator<Item = (Point, Point)> + 'a {
        let matches = match self
            .rev_matches
            .binary_search_by_key(&at.byte(), |m| m.end.byte())
        {
            Ok(i) => &mut self.rev_matches[i],
            Err(i) => {
                self.rev_matches.insert(i, Matches::new(at));
                &mut self.rev_matches[i]
            }
        };

        let haystack = match start {
            Some(start) => unsafe {
                text.make_contiguous_in(start.byte()..at.byte());
                text.continuous_in_unchecked(start.byte()..at.byte())
            },
            None => unsafe {
                text.make_contiguous_in(..at.byte());
                text.continuous_in_unchecked(..at.byte())
            },
        };
        let mut fwd_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut rev_input = Input::new(haystack).anchored(Anchored::Yes);
        let mut match_i = 0;

        let text = text as &Text;
        let fwd_dfa = &self.fwd_dfa;
        let rev_dfa = &self.rev_dfa;
        let fwd_cache = &mut self.fwd_cache;
        let rev_cache = &mut self.rev_cache;
        let gap = start.map(|p| p.byte()).unwrap_or(0);
        std::iter::from_fn(move || {
            while let Some((start, end)) = matches.list.get_mut(match_i) {
                fwd_input.set_start(start.byte() - gap);

                match fwd_dfa.try_search_fwd(fwd_cache, &fwd_input) {
                    Ok(Some(half)) => {
                        match_i += 1;
                        *end = text.point_at(half.offset() + gap);
                        return Some((*start, *end));
                    }
                    _ => {
                        matches.list.remove(match_i);
                    }
                }
            }

            // To prevent subsequently added matches from being checked.
            if match_i != usize::MAX {
                match_i = usize::MAX;
                rev_input.set_anchored(Anchored::No);
                rev_input.set_end(matches.start.byte() - gap);
            }

            let Ok(Some(half)) = rev_dfa.try_search_rev(rev_cache, &rev_input) else {
                matches.start = start.unwrap_or_default();
                return None;
            };
            let start = half.offset();

            fwd_input.set_start(start);
            rev_input.set_end(start);

            let half = fwd_dfa
                .try_search_rev(fwd_cache, &fwd_input)
                .unwrap()
                .unwrap();
            let end = half.offset();

            let (start, end) = (text.point_at(start + gap), text.point_at(end + gap));
            matches.list.push((start, end));
            matches.start = start;

            Some((start, end))
        })
    }
}

pub struct SavedMatches {
    pat: String,
    hir: Hir,
    fwd: Vec<Matches>,
    rev: Vec<Matches>,
}

impl SavedMatches {
    pub fn new(pat: String) -> Result<Self, Box<BuildError>> {
        let _ = dfas_from_pat(&pat)?;
        let hir = regex_syntax::Parser::new().parse(&pat).unwrap();
        Ok(Self {
            pat,
            hir,
            fwd: Vec::new(),
            rev: Vec::new(),
        })
    }

    pub fn searcher(&mut self) -> Searcher<'_> {
        let dfas = dfas_from_pat(&self.pat).unwrap();
        Searcher {
            fwd_dfa: &dfas.fwd.0,
            fwd_cache: dfas.fwd.1.write(),
            rev_dfa: &dfas.rev.0,
            rev_cache: dfas.fwd.1.write(),
            fwd_matches: &mut self.fwd,
            rev_matches: &mut self.rev,
        }
    }

    pub fn take_matches_from(&mut self, other: &Self) {
        self.fwd = other.fwd.clone();
        self.rev = other.rev.clone();
    }

    pub fn is_prefix_of(&self, other: &Self) -> bool {
        is_prefix_of(&self.hir, &other.hir)
    }

    pub fn pat_is(&self, text: &Text) -> bool {
        let (s0, s1) = text.slices();
        &self.pat[..s0.len()] == s0 && &self.pat[s0.len()..] == s1
    }
}

// NOTE: The start field is where the search is supposed to begin,
// which in reverse search, would start at the end of the searched
// range.
#[derive(Default, Clone)]
pub struct Matches {
    start: Point,
    end: Point,
    list: Vec<(Point, Point)>,
}

impl Matches {
    pub fn new(start: Point) -> Self {
        Self { start, end: start, list: Vec::new() }
    }

    pub fn transform_by(mut self, start: Point, old_end: Point, new_end: Point) -> Option<Self> {
        if start.byte() > self.end.byte() {
            Some(self)
        } else if start.byte() > self.start.byte() {
            while let Some((last_start, last_end)) = self.list.last()
                && (last_start.byte()..last_end.byte()).contains(&start.byte())
            {
                self.list.pop();
            }
            (!self.list.is_empty()).then_some(self)
        } else if old_end.byte() <= self.start.byte() {
            self.start += new_end - old_end;
            self.end += new_end - old_end;
            for (start, end) in &mut self.list {
                *start = (*start + new_end) - old_end;
                *end = (*end + new_end) - old_end;
            }
            Some(self)
        } else {
            None
        }
    }
}

struct DFAs {
    fwd: (DFA, RwLock<Cache>),
    rev: (DFA, RwLock<Cache>),
}

/// Rudimentary check for prefixes in regex patterns
///
/// This check tries to do a minimal check for capture groups,
/// ignoring them
fn is_prefix_of(prefix: &Hir, to_check: &Hir) -> bool {
    fn lowest_level_start_hir(mut kind: &HirKind) -> &HirKind {
        loop {
            if let HirKind::Empty
            | HirKind::Literal(_)
            | HirKind::Class(_)
            | HirKind::Look(_)
            | HirKind::Alternation(_) = kind
            {
                break;
            }
            match kind.subs().first() {
                Some(first) => kind = first.kind(),
                None => break,
            }
        }
        kind
    }

    fn without_captures(mut kind: &HirKind) -> &HirKind {
        while let HirKind::Capture(cap) = kind {
            kind = cap.sub.kind()
        }
        kind
    }

    if let HirKind::Empty = prefix.kind() {
        true
    } else if let HirKind::Literal(pre) = without_captures(prefix.kind())
        && let HirKind::Literal(lit) = lowest_level_start_hir(to_check.kind())
    {
        lit.0.starts_with(&pre.0)
    } else {
        let mut hir = Some(to_check);
        while let Some(h) = hir {
            if h.kind() == prefix.kind() {
                return true;
            }
            hir = h.kind().subs().first()
        }
        false
    }
}

fn dfas_from_pat(pat: impl RegexPattern) -> Result<&'static DFAs, Box<BuildError>> {
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
        let dfas = Box::new(DFAs {
            fwd: (fwd, RwLock::new(fwd_cache)),
            rev: (rev, RwLock::new(rev_cache)),
        });
        let _ = list.insert(pat, Box::leak(dfas));
        Ok(*list.get(&pat).unwrap())
    }
}

pub trait RegexPattern: InnerRegexPattern {
    type Match;

    fn get_match(points: (Point, Point), pattern: PatternID) -> Self::Match;
}

impl<'a> RegexPattern for &'a str {
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

impl<'a> RegexPattern for &'a String {
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

impl<'a> RegexPattern for &'a [&'static str] {
    type Match = (Point, Point, usize);

    fn get_match(points: (Point, Point), pattern: PatternID) -> Self::Match {
        (points.0, points.1, pattern.as_usize())
    }
}

trait InnerRegexPattern {
    fn as_patterns<'b>(&'b self, bytes: &'b mut [u8; 4]) -> Patterns<'b>;
}

impl<'a> InnerRegexPattern for &'a str {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::One(self)
    }
}

impl InnerRegexPattern for String {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::One(self)
    }
}

impl<'a> InnerRegexPattern for &'a String {
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

impl<'a> InnerRegexPattern for &'a [&'static str] {
    fn as_patterns<'b>(&'b self, _bytes: &'b mut [u8; 4]) -> Patterns<'b> {
        Patterns::Many(self)
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

    fn dfas(&self) -> Result<(DFA, DFA), Box<BuildError>> {
        let mut rev_builder = DFA::builder();
        rev_builder
            .configure(
                DFA::config()
                    .prefilter(None)
                    .specialize_start_states(false)
                    .match_kind(MatchKind::All),
            )
            .thompson(Config::new().reverse(true));

        match self {
            Patterns::One(pat) => {
                let fwd = DFA::new(pat)?;
                let rev = rev_builder.build(pat)?;
                Ok((fwd, rev))
            }
            Patterns::Many(pats) => {
                let fwd = DFA::new_many(pats)?;
                let rev = rev_builder.build_many(pats)?;
                Ok((fwd, rev))
            }
        }
    }
}
