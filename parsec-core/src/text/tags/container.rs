use std::marker::PhantomData;

use any_rope::{Measurable, Rope};

use super::{Handle, Tag, TagOrSkip};

#[derive(Debug)]
pub enum Container {
    Vec(Vec<TagOrSkip>),
    Rope(Rope<TagOrSkip>)
}

impl Container {
    pub fn insert(&mut self, pos: usize, t_or_s: TagOrSkip) {
        match self {
            Container::Vec(vec) => {
                let index = end_ch_to_index(vec, pos);
                vec.insert(index, t_or_s)
            }
            Container::Rope(rope) => rope.insert(pos, t_or_s)
        }
    }

    pub fn insert_slice(&mut self, pos: usize, slice: &[TagOrSkip]) {
        match self {
            Container::Vec(vec) => {
                let index = end_ch_to_index(vec, pos);
                vec.splice(index..index, slice.iter().copied());
            }
            Container::Rope(rope) => rope.insert_slice(pos, slice)
        }
    }

    pub fn remove_inclusive_on(&mut self, pos: usize, handle: Handle) -> Vec<(usize, Tag, Handle)> {
        match self {
            Container::Vec(vec) => {
                let start = start_ch_to_index(vec, pos);
                let end = end_ch_to_index(&vec[start..], 0);
                vec.extract_if(|t_or_s| match t_or_s {
                    TagOrSkip::Tag(_, cmp_handle) => handle == *cmp_handle,
                    TagOrSkip::Skip(_) => false
                })
                .take(end)
                .skip(start)
                .filter_map(|t_or_s| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle)))
                .collect()
            }
            Container::Rope(rope) => {
                let slice = rope.width_slice(pos..pos);
                let mut removed = Vec::new();
                let kept = slice
                    .iter()
                    .filter_map(|(_, t_or_s)| {
                        if let TagOrSkip::Tag(tag, cmp_handle) = t_or_s {
                            if cmp_handle != handle {
                                return Some(TagOrSkip::Tag(tag, cmp_handle));
                            } else {
                                removed.push((pos, tag, cmp_handle))
                            }
                        }
                        None
                    })
                    .collect::<Vec<TagOrSkip>>();

                rope.remove_inclusive(pos..pos);
                rope.insert_slice(pos, kept.as_slice());

                removed
            }
        }
    }

    pub fn remove_exclusive(&mut self, range: std::ops::Range<usize>) {
        match self {
            Container::Vec(vec) => {
                if range.start == range.end {
                    return;
                }

                let start = end_ch_to_index(vec, range.start);
                let end = start_ch_to_index(vec, range.end);
                assert!(start <= end, "{}, {}\n{:?}", range.start, range.end, vec);
                vec.splice(start..end, []);
            }
            Container::Rope(rope) => rope.remove_exclusive(range)
        }
    }

    pub fn get_from_char(&self, char: usize) -> Option<(usize, TagOrSkip)> {
        match self {
            Container::Vec(vec) => vec
                .iter()
                .scan((false, 0), |(end_found, accum), tag_or_skip| {
                    if *end_found {
                        return None;
                    }

                    let old_accum = *accum;
                    let width = tag_or_skip.width();
                    *accum += tag_or_skip.width();
                    if (*accum == char && width == 0) || *accum > char {
                        *end_found = true;
                    }
                    Some((old_accum, *tag_or_skip))
                })
                .last(),
            Container::Rope(rope) => rope.get_from_width(char)
        }
    }

    pub fn iter_at(&self, pos: usize) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            Container::Vec(vec) => Iter::Vec(
                vec.iter().scan(0, forward_tag_start).skip_while(move |(accum, _)| *accum < pos)
            ),
            Container::Rope(rope) => Iter::Rope(rope.iter_at_width(pos), PhantomData)
        }
    }

    pub fn rev_iter_at(&self, pos: usize) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            Container::Vec(vec) => {
                let width = vec.iter().map(|t_or_s| t_or_s.width()).sum::<usize>();
                Iter::Vec(vec.iter().rev().scan(width, reverse_tag_start).skip_while(
                    move |(accum, t_or_s)| *accum > pos && t_or_s.width() == 0 || *accum >= pos
                ))
            }
            Container::Rope(rope) => Iter::Rope(rope.iter_at_width(pos).reversed(), PhantomData)
        }
    }
}

#[derive(Clone)]
enum Iter<'a, VecIter, RopeIter>
where
    VecIter: Iterator<Item = (usize, TagOrSkip)> + Clone + 'a,
    RopeIter: Iterator<Item = (usize, TagOrSkip)> + Clone + 'a
{
    Vec(VecIter),
    Rope(RopeIter, PhantomData<&'a ()>)
}

impl<'a, VecIter, RopeIter> Iterator for Iter<'a, VecIter, RopeIter>
where
    VecIter: Iterator<Item = (usize, TagOrSkip)> + Clone + 'a,
    RopeIter: Iterator<Item = (usize, TagOrSkip)> + Clone + 'a
{
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Vec(iter) => iter.next(),
            Iter::Rope(iter, _) => iter.next()
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
