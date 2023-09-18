use any_rope::{Measurable, Rope};

use super::{Markers, RawTag, TagOrSkip};

#[derive(Debug, Clone)]
pub enum Container {
    Vec(Vec<TagOrSkip>),
    Rope(Rope<TagOrSkip>),
}

impl Container {
    pub fn insert(&mut self, pos: usize, t_or_s: TagOrSkip) {
        match self {
            Container::Vec(vec) => {
                let index = end_pos_to_index(vec, pos);
                vec.insert(index, t_or_s)
            }
            Container::Rope(rope) => rope.insert(pos, t_or_s, usize::cmp),
        }
    }

    pub fn insert_slice(&mut self, pos: usize, slice: &[TagOrSkip]) {
        match self {
            Container::Vec(vec) => {
                let index = end_pos_to_index(vec, pos);
                vec.splice(index..index, slice.iter().cloned());
            }
            Container::Rope(rope) => rope.insert_slice(pos, slice, usize::cmp),
        }
    }

    pub fn remove_inclusive_on(
        &mut self,
        pos: usize,
        markers: impl Markers,
    ) -> Vec<(usize, RawTag)> {
        let range = markers.range();

        match self {
            Container::Vec(vec) => {
                let start = start_pos_to_index(vec, pos);
                let end = end_pos_to_index(&vec[start..], 0);

                let kept: Vec<_> = vec[start..end]
                    .iter()
                    .filter(|t_or_s| {
                        t_or_s
                            .as_tag()
                            .map_or(true, |tag| !range.contains(&tag.marker()))
                    })
                    .cloned()
                    .collect();

                vec.splice(start..end, kept)
                    .filter_map(|t_or_s| t_or_s.as_tag().map(|tag| (pos, tag)))
                    .filter(|(_, tag)| range.contains(&tag.marker()))
                    .collect()
            }
            Container::Rope(rope) => {
                let slice = rope.measure_slice(pos..pos, usize::cmp);

                let mut kept = Vec::new();
                let removed = slice
                    .iter()
                    .filter_map(|(_, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
                    .filter(|(_, tag)| {
                        if range.contains(&tag.marker()) {
                            true
                        } else {
                            kept.push(TagOrSkip::Tag(*tag));
                            false
                        }
                    })
                    .collect();

                rope.remove_inclusive(pos..pos, usize::cmp);
                rope.insert_slice(pos, &kept, usize::cmp);

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

                let start = end_pos_to_index(vec, range.start);
                let end = start_pos_to_index(vec, range.end);
                assert!(
                    start <= end,
                    "Start ({}) greater than end ({})",
                    range.start,
                    range.end
                );
                vec.splice(start..end, []);
            }
            Container::Rope(rope) => rope.remove_exclusive(range, usize::cmp),
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
                    let width = tag_or_skip.measure();
                    *accum += tag_or_skip.measure();
                    if (*accum == char && width == 0) || *accum > char {
                        *end_found = true;
                    }
                    Some((old_accum, *tag_or_skip))
                })
                .last(),
            Container::Rope(rope) => rope.get_from_measure(char, usize::cmp),
        }
    }

    pub fn iter_at(&self, pos: usize) -> Iter<ForwardTags> {
        match self {
            Container::Vec(vec) => Iter::Vec(ForwardTags::new(pos, vec)),
            Container::Rope(rope) => Iter::Rope(rope.iter_at_measure(pos, usize::cmp)),
        }
    }

    pub fn rev_iter_at(&self, pos: usize) -> Iter<ReverseTags> {
        match self {
            Container::Vec(vec) => {
                let width = vec.iter().map(|t_or_s| t_or_s.measure()).sum::<usize>();
                Iter::Vec(ReverseTags::new(width, pos, vec))
            }
            Container::Rope(rope) => Iter::Rope(rope.iter_at_measure(pos, usize::cmp).reversed()),
        }
    }
}

#[derive(Clone)]
pub enum Iter<'a, VecIter>
where
    VecIter: Iterator<Item = (usize, TagOrSkip)> + Clone,
{
    Vec(VecIter),
    Rope(any_rope::iter::Iter<'a, TagOrSkip>),
}

impl<VecIter> Iterator for Iter<'_, VecIter>
where
    VecIter: Iterator<Item = (usize, TagOrSkip)> + Clone,
{
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Vec(iter) => iter.next(),
            Iter::Rope(iter) => iter.next(),
        }
    }
}

#[derive(Clone)]
pub struct ForwardTags<'a> {
    accum: usize,
    min: usize,
    iter: std::slice::Iter<'a, TagOrSkip>,
}

impl<'a> ForwardTags<'a> {
    fn new(min: usize, slice: &'a [TagOrSkip]) -> Self {
        Self {
            accum: 0,
            min,
            iter: slice.iter(),
        }
    }
}

impl Iterator for ForwardTags<'_> {
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(t_or_s) = self.iter.next() {
            let old_accum = self.accum;
            self.accum += t_or_s.measure();
            if self.accum >= self.min {
                return Some((old_accum, *t_or_s));
            }
        }

        None
    }
}

#[derive(Clone)]
pub struct ReverseTags<'a> {
    accum: usize,
    min: usize,
    iter: std::iter::Rev<std::slice::Iter<'a, TagOrSkip>>,
}

impl<'a> ReverseTags<'a> {
    fn new(accum: usize, min: usize, slice: &'a [TagOrSkip]) -> Self {
        Self {
            accum,
            min,
            iter: slice.iter().rev(),
        }
    }
}

impl Iterator for ReverseTags<'_> {
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(t_or_s) = self.iter.next() {
            let old_accum = self.accum;
            self.accum -= t_or_s.measure();
            if old_accum <= self.min {
                return Some((self.accum, *t_or_s));
            }
        }

        None
    }
}

fn start_pos_to_index(slice: &[TagOrSkip], width: usize) -> usize {
    let mut index = 0;
    let mut accum = 0;

    for measurable in slice {
        let measurable_width = measurable.measure();
        let next_accum = accum + measurable_width;

        if (measurable_width == 0 && next_accum == width) || next_accum > width {
            break;
        }
        accum = next_accum;
        index += 1;
    }

    index
}

fn end_pos_to_index(slice: &[TagOrSkip], width: usize) -> usize {
    let mut index = 0;
    let mut accum = 0;

    for measurable in slice {
        let measurable_width = measurable.measure();
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
