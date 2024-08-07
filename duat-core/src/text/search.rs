pub use self::chars::CharSet;
use super::Text;
use crate::text::Point;

impl Text {
    pub fn search_from<'a>(&'a self, point: Point, pat: impl Pattern<'a>) -> impl Searcher<'a> {
        pat.searcher(self, point)
    }

    pub fn search_from_rev<'a>(&'a self, point: Point, pat: impl Pattern<'a>) -> impl Searcher<'a> {
        pat.searcher_rev(self, point)
    }
}

pub trait Pattern<'a> {
    type Searcher: Searcher<'a, Pattern = Self>;
    type SearcherRev: Searcher<'a, Pattern = Self>;

    fn searcher(self, text: &'a Text, point: Point) -> Self::Searcher
    where
        Self: Sized,
    {
        Self::Searcher::new(text, point, self)
    }

    fn searcher_rev(self, text: &'a Text, point: Point) -> Self::SearcherRev
    where
        Self: Sized,
    {
        Self::SearcherRev::new(text, point, self)
    }
}

pub trait Searcher<'a>: Iterator<Item = ((Point, Point), Self::Match)> {
    type Pattern;
    type Match;

    fn new(text: &'a Text, point: Point, pat: Self::Pattern) -> Self;
}

mod str {
    use std::{
        iter::{Chain, Rev},
        str::Bytes,
    };

    use crate::text::{Point, Text};

    pub struct Searcher<'a> {
        iter: Chain<Bytes<'a>, Bytes<'a>>,
        pat: &'a str,
        point: Point,
    }
    impl<'a> Iterator for Searcher<'a> {
        type Item = ((Point, Point), &'a str);

        fn next(&mut self) -> Option<Self::Item> {
            let mut i = 0;
            let mut p0 = self.point;

            while let Some(b) = self.iter.next() {
                self.point = self.point.fwd_byte(b);
                if i == self.pat.len() {
                    return Some(((p0, self.point), self.pat));
                } else if b == self.pat.as_bytes()[i] {
                    i += 1;
                } else {
                    p0 = self.point;
                    i = 0
                }
            }

            None
        }
    }

    impl<'a> super::Searcher<'a> for Searcher<'a> {
        type Match = &'a str;
        type Pattern = &'a str;

        fn new(text: &'a Text, point: Point, pat: &'a str) -> Self {
            let (s0, s1) = text.strs_in_range(point.byte()..);
            let iter = s0.bytes().chain(s1.bytes());

            Self { iter, pat, point }
        }
    }

    pub struct SearcherRev<'a> {
        iter: Chain<Rev<Bytes<'a>>, Rev<Bytes<'a>>>,
        pat: &'a str,
        point: Point,
    }

    impl<'a> Iterator for SearcherRev<'a> {
        type Item = ((Point, Point), &'a str);

        fn next(&mut self) -> Option<Self::Item> {
            let mut i = self.pat.len();
            let mut p1 = self.point;

            while let Some(b) = self.iter.next() {
                if i == 0 {
                    return Some(((self.point, p1), self.pat));
                } else if b == self.pat.as_bytes()[i] {
                    i -= 1;
                } else {
                    p1 = self.point;
                    i = self.pat.len();
                }
                self.point = self.point.rev_byte(b);
            }

            None
        }
    }

    impl<'a> super::Searcher<'a> for SearcherRev<'a> {
        type Match = &'a str;
        type Pattern = &'a str;

        fn new(text: &'a Text, point: Point, pat: &'a str) -> Self {
            let (s0, s1) = text.strs_in_range(..point.byte());
            let iter = s1.bytes().rev().chain(s0.bytes().rev());

            Self { iter, pat, point }
        }
    }

    impl<'a> super::Pattern<'a> for &'a str {
        type Searcher = Searcher<'a>;
        type SearcherRev = SearcherRev<'a>;
    }
}

mod chars {
    use std::{
        iter::{Chain, Rev},
        str::Chars,
    };

    use crate::text::{Point, Text, WordChars};

    pub struct Or<C1, C2>(C1, C2)
    where
        C1: CharSet,
        C2: CharSet;

    pub struct And<C1, C2>(C1, C2)
    where
        C1: CharSet,
        C2: CharSet;

    pub struct Not<C>(C)
    where
        C: CharSet;

    pub struct Searcher<'a, C>
    where
        C: CharSet,
    {
        iter: Chain<Chars<'a>, Chars<'a>>,
        pat: C,
        point: Point,
    }

    impl<'a, C> Iterator for Searcher<'a, C>
    where
        C: CharSet,
    {
        type Item = ((Point, Point), char);

        fn next(&mut self) -> Option<Self::Item> {
            while let Some(char) = self.iter.next() {
                let p0 = self.point;
                self.point = self.point.fwd(char);

                if self.pat.matches(char) {
                    return Some(((p0, self.point), char));
                }
            }

            None
        }
    }

    impl<'a, C> super::Searcher<'a> for Searcher<'a, C>
    where
        C: CharSet,
    {
        type Match = char;
        type Pattern = C;

        fn new(text: &'a Text, point: Point, pat: Self::Pattern) -> Self {
            let (s0, s1) = text.strs_in_range(point.byte()..);
            let iter = s0.chars().chain(s1.chars());

            Self { iter, pat, point }
        }
    }

    pub struct SearcherRev<'a, C>
    where
        C: CharSet,
    {
        iter: Chain<Rev<Chars<'a>>, Rev<Chars<'a>>>,
        pat: C,
        point: Point,
    }

    impl<'a, C> Iterator for SearcherRev<'a, C>
    where
        C: CharSet,
    {
        type Item = ((Point, Point), char);

        fn next(&mut self) -> Option<Self::Item> {
            while let Some(char) = self.iter.next() {
                let p1 = self.point;
                self.point = self.point.rev(char);

                if self.pat.matches(char) {
                    return Some(((self.point, p1), char));
                }
            }

            None
        }
    }

    impl<'a, C> super::Searcher<'a> for SearcherRev<'a, C>
    where
        C: CharSet,
    {
        type Match = char;
        type Pattern = C;

        fn new(text: &'a Text, point: Point, pat: Self::Pattern) -> Self {
            let (s0, s1) = text.strs_in_range(point.byte()..);
            let iter = s0.chars().rev().chain(s1.chars().rev());

            Self { iter, pat, point }
        }
    }

    impl<'a, C> super::Pattern<'a> for C
    where
        C: CharSet,
    {
        type Searcher = Searcher<'a, Self>;
        type SearcherRev = Searcher<'a, Self>;
    }

    pub trait CharSet {
        fn matches(&self, char: char) -> bool;

        fn and(self, other: impl CharSet) -> impl CharSet
        where
            Self: Sized,
        {
            And(self, other)
        }

        fn or(self, other: impl CharSet) -> impl CharSet
        where
            Self: Sized,
        {
            Or(self, other)
        }

        fn not(self) -> Not<Self>
        where
            Self: Sized,
        {
            Not(self)
        }
    }

    impl CharSet for char {
        fn matches(&self, char: char) -> bool {
            *self == char
        }
    }

    impl<const N: usize> CharSet for [char; N] {
        fn matches(&self, char: char) -> bool {
            self.contains(&char)
        }
    }

    impl CharSet for &[char] {
        fn matches(&self, char: char) -> bool {
            self.contains(&char)
        }
    }

    impl CharSet for &'_ WordChars {
        fn matches(&self, char: char) -> bool {
            self.contains(char)
        }
    }

    impl<C1, C2> CharSet for And<C1, C2>
    where
        C1: CharSet,
        C2: CharSet,
    {
        fn matches(&self, char: char) -> bool {
            self.0.matches(char) && self.1.matches(char)
        }
    }

    impl<C1, C2> CharSet for Or<C1, C2>
    where
        C1: CharSet,
        C2: CharSet,
    {
        fn matches(&self, char: char) -> bool {
            self.0.matches(char) || self.1.matches(char)
        }
    }

    impl<C> CharSet for Not<C>
    where
        C: CharSet,
    {
        fn matches(&self, char: char) -> bool {
            !self.0.matches(char)
        }
    }
}
