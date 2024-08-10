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
    use crate::text::{Point, Text};

    pub struct Searcher<'a> {
        iter: SearchIter<'a>,
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
            // This might be a limitation of the current version of rust.
            fn iter(text: &Text, point: Point) -> SearchIter<'_> {
                text.strs_in_range(point.byte()..)
                    .into_iter()
                    .flat_map(str::bytes)
            }
            let iter = iter(text, point);
            Self { iter, pat, point }
        }
    }

    pub struct SearcherRev<'a> {
        iter: SearchIterRev<'a>,
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
                } else if b == self.pat.as_bytes()[i - 1] {
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
            // This might be a limitation of the current version of rust.
            fn iter(text: &Text, point: Point) -> SearchIterRev<'_> {
                text.strs_in_range(point.byte()..)
                    .into_iter()
                    .flat_map(str::bytes)
                    .rev()
            }
            let iter = iter(text, point);
            Self { iter, pat, point }
        }
    }

    impl<'a> super::Pattern<'a> for &'a str {
        type Searcher = Searcher<'a>;
        type SearcherRev = SearcherRev<'a>;
    }

    type SearchIter<'a> = impl Iterator<Item = u8> + 'a;
    type SearchIterRev<'a> = impl Iterator<Item = u8> + 'a;
}

mod chars {
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
        iter: SearchIter<'a>,
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
            // This might be a limitation of the current version of rust.
            fn iter(text: &Text, point: Point) -> SearchIter<'_> {
                text.strs_in_range(point.byte()..)
                    .into_iter()
                    .flat_map(str::chars)
            }
            let iter = iter(text, point);
            Self { iter, pat, point }
        }
    }

    pub struct SearcherRev<'a, C>
    where
        C: CharSet,
    {
        iter: SearchIterRev<'a>,
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
            // This might be a limitation of the current version of rust.
            fn iter(text: &Text, point: Point) -> SearchIterRev<'_> {
                text.strs_in_range(point.byte()..)
                    .into_iter()
                    .flat_map(str::chars)
            }
            let iter = iter(text, point);
            Self { iter, pat, point }
        }
    }

    impl<'a, C> super::Pattern<'a> for C
    where
        C: CharSet,
    {
        type Searcher = Searcher<'a, Self>;
        type SearcherRev = SearcherRev<'a, Self>;
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

    type SearchIter<'a> = impl Iterator<Item = char> + 'a;
    type SearchIterRev<'a> = impl Iterator<Item = char> + 'a;
}
