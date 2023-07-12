use std::{
    iter::{Rev, Scan, SkipWhile},
    ops::Range,
    slice::Iter
};

use any_rope::{Measurable, Rope};

use super::{Lock, TagOrSkip};
use crate::text::inner::InnerText;

#[derive(Debug)]
pub(super) enum InnerTags {
    Vec(Vec<TagOrSkip>),
    Rope(Rope<TagOrSkip>)
}

impl InnerTags {
    pub fn new(inner_text: &InnerText) -> Self {
        let skip = TagOrSkip::Skip(inner_text.len_chars() as u32);
        match inner_text {
            InnerText::String(_) => InnerTags::Vec(vec![skip]),
            InnerText::Rope(_) => InnerTags::Rope(Rope::from_slice(&[skip]))
        }
    }

    pub fn insert(&mut self, ch_index: usize, t_or_s: TagOrSkip) {
        match self {
            InnerTags::Vec(vec) => {
                let index = end_ch_to_index(&vec, ch_index);
                vec.insert(index, t_or_s)
            }
            InnerTags::Rope(rope) => rope.insert(ch_index, t_or_s)
        }
    }

    pub fn insert_slice(&mut self, ch_index: usize, slice: &[TagOrSkip]) {
        match self {
            InnerTags::Vec(vec) => {
                let index = end_ch_to_index(&vec, ch_index);
                vec.splice(index..index, slice.iter().map(|tag_or_skip| *tag_or_skip));
            }
            InnerTags::Rope(rope) => rope.insert_slice(ch_index, slice)
        }
    }

    pub fn remove_exclusive(&mut self, range: Range<usize>) {
        match self {
            InnerTags::Vec(vec) => {
                if range.start == range.end {
                    return;
                }

                let start = end_ch_to_index(&vec, range.start);
                let end = start_ch_to_index(&vec, range.end);
                assert!(start <= end, "{}, {}\n{:?}", range.start, range.end, vec);
                vec.splice(start..end, []);
            }
            InnerTags::Rope(rope) => rope.remove_exclusive(range)
        }
    }

    pub fn remove_on(&mut self, ch_index: usize, lock: Lock) {
        match self {
            InnerTags::Vec(vec) => {
                let start = start_ch_to_index(&vec, ch_index);
                let end = end_ch_to_index(&vec[start..], 0);
                vec.extract_if(|tag_or_skip| match tag_or_skip {
                    TagOrSkip::Tag(_, cmp_lock) => lock == *cmp_lock,
                    TagOrSkip::Skip(_) => false
                })
                .take(end)
                .skip(start)
                .last();
            }
            InnerTags::Rope(rope) => {
                let slice = rope.width_slice(ch_index..ch_index);
                let tags = slice.iter();

                let tags = tags
                    .filter_map(|(_, t_or_s)| {
                        if let TagOrSkip::Tag(tag, cmp_lock) = t_or_s && cmp_lock != lock {
                            Some(TagOrSkip::Tag(tag, cmp_lock))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<TagOrSkip>>();

                rope.remove_inclusive(ch_index..ch_index);
                rope.insert_slice(ch_index, tags.as_slice());
            }
        }
    }

    pub fn get_from_ch_index(&self, ch_index: usize) -> Option<(usize, TagOrSkip)> {
        match self {
            InnerTags::Vec(vec) => vec
                .iter()
                .scan((false, 0), |(end_found, accum), tag_or_skip| {
                    if *end_found {
                        return None;
                    }

                    let old_accum = *accum;
                    let width = tag_or_skip.width();
                    *accum += tag_or_skip.width();
                    if (*accum == ch_index && width == 0) || *accum > ch_index {
                        *end_found = true;
                    }
                    Some((old_accum, *tag_or_skip))
                })
                .last(),
            InnerTags::Rope(rope) => rope.get_from_width(ch_index)
        }
    }

    pub fn iter(&self) -> Box<dyn Iterator<Item = (usize, TagOrSkip)> + '_> {
        match self {
            InnerTags::Vec(vec) => Box::new(vec.iter().scan(0, |accum, tag_or_skip| {
                let old_accum = *accum;
                *accum += tag_or_skip.width();

                Some((old_accum, *tag_or_skip))
            })),
            InnerTags::Rope(rope) => Box::new(rope.iter())
        }
    }

    pub fn iter_at(
        &self, ch_index: usize
    ) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            InnerTags::Vec(vec) => Tags::Vec(
                vec.iter()
                    .scan(0, forward_tag_start)
                    .skip_while(move |(accum, _)| *accum < ch_index)
            ),
            InnerTags::Rope(rope) => Tags::Rope(rope.iter_at_width(ch_index))
        }
    }

    pub fn iter_at_rev(
        &self, ch_index: usize
    ) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            InnerTags::Vec(vec) => {
                let width = vec.iter().map(|t_or_s| t_or_s.width()).sum::<usize>();
                Tags::RevVec(vec.iter().rev().scan(width, reverse_tag_start).skip_while(
                    move |(accum, t_or_s)| {
                        *accum > ch_index && t_or_s.width() == 0 || *accum >= ch_index
                    }
                ))
            }
            InnerTags::Rope(rope) => Tags::Rope(rope.iter_at_width(ch_index).reversed())
        }
    }

    pub fn width(&self) -> usize {
        match self {
            InnerTags::Vec(vec) => vec.iter().map(|tag_or_skip| tag_or_skip.width()).sum(),
            InnerTags::Rope(rope) => rope.width()
        }
    }

    pub fn len(&self) -> usize {
        match self {
            InnerTags::Vec(vec) => vec.len(),
            InnerTags::Rope(rope) => rope.len()
        }
    }

    pub fn clear(&mut self) {
        match self {
            InnerTags::Vec(vec) => vec.clear(),
            InnerTags::Rope(rope) => *rope = Rope::new()
        }
    }
}

#[derive(Clone)]
pub enum Tags<'a, ScanFn, SkipFn>
where
    ScanFn: FnMut(&mut usize, &TagOrSkip) -> Option<(usize, TagOrSkip)>,
    SkipFn: FnMut(&(usize, TagOrSkip)) -> bool + 'a
{
    Vec(SkipWhile<Scan<Iter<'a, TagOrSkip>, usize, ScanFn>, SkipFn>),
    RevVec(SkipWhile<Scan<Rev<Iter<'a, TagOrSkip>>, usize, ScanFn>, SkipFn>),
    Rope(any_rope::iter::Iter<'a, TagOrSkip>)
}

impl<'a, ScanFn, SkipFn> Iterator for Tags<'a, ScanFn, SkipFn>
where
    ScanFn: FnMut(&mut usize, &TagOrSkip) -> Option<(usize, TagOrSkip)>,
    SkipFn: FnMut(&(usize, TagOrSkip)) -> bool + 'a
{
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Tags::Vec(iter) => iter.next(),
            Tags::RevVec(iter) => iter.next(),
            Tags::Rope(iter) => iter.next()
        }
    }
}

fn start_ch_to_index(slice: &[TagOrSkip], width: usize) -> usize {
    let mut index = 0;
    let mut accum = 0;

    for measurable in slice {
        let measurable_width = measurable.width();
        let next_accum = accum + measurable_width;

        if (measurable_width == 0 && next_accum == width) || next_accum > width {
            break;
        }
        accum = next_accum;
        index += 1;
    }

    index
}

fn end_ch_to_index(slice: &[TagOrSkip], width: usize) -> usize {
    let mut index = 0;
    let mut accum = 0;

    for measurable in slice {
        let measurable_width = measurable.width();
        // This makes it so that every 0 width node exactly at `width` is also
        // captured.
        if (measurable_width != 0 && accum == width) || accum > width {
            break;
        }

        accum += measurable_width;
        index += 1;
    }

    index
}

fn forward_tag_start(accum: &mut usize, t_or_s: &TagOrSkip) -> Option<(usize, TagOrSkip)> {
    let old_accum = *accum;
    *accum += t_or_s.width();

    Some((old_accum, *t_or_s))
}

fn reverse_tag_start(accum: &mut usize, t_or_s: &TagOrSkip) -> Option<(usize, TagOrSkip)> {
    *accum -= t_or_s.width();
    Some((*accum, *t_or_s))
}
