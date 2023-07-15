use std::{
    iter::{Rev, Scan, SkipWhile},
    ops::Range
};

use any_rope::{Measurable, Rope};

use super::{Handle, TagOrSkip};
use crate::text::inner::InnerText;

#[derive(Debug)]
pub(super) enum InnerTags {
}

impl InnerTags {
}

#[derive(Clone)]
enum Iter<'a, ScanFn, SkipFn>
where
    ScanFn: FnMut(&mut usize, &TagOrSkip) -> Option<(usize, TagOrSkip)>,
    SkipFn: FnMut(&(usize, TagOrSkip)) -> bool + 'a
{
    Vec(SkipWhile<Scan<std::slice::Iter<'a, TagOrSkip>, usize, ScanFn>, SkipFn>),
    RevVec(SkipWhile<Scan<Rev<std::slice::Iter<'a, TagOrSkip>>, usize, ScanFn>, SkipFn>),
    Rope(any_rope::iter::Iter<'a, TagOrSkip>)
}

impl<'a, ScanFn, SkipFn> Iterator for Iter<'a, ScanFn, SkipFn>
where
    ScanFn: FnMut(&mut usize, &TagOrSkip) -> Option<(usize, TagOrSkip)>,
    SkipFn: FnMut(&(usize, TagOrSkip)) -> bool + 'a
{
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Vec(tags) => tags.next(),
            Iter::RevVec(tags) => tags.next(),
            Iter::Rope(tags) => tags.next()
        }
    }
}

