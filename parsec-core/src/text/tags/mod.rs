mod inner;

use std::ops::Range;

use any_rope::{Measurable, Rope as AnyRope};

use self::inner::InnerTags;
use crate::text::inner::InnerText;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Handle(u16);

impl Default for Handle {
    fn default() -> Self {
        use std::sync::atomic::{AtomicU16, Ordering};
        static LOCK_COUNT: AtomicU16 = AtomicU16::new(0);

        Handle(LOCK_COUNT.fetch_add(1, Ordering::Acquire))
    }
}

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone, Copy, PartialEq)]
pub enum Tag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(u16),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(u16),

    /// Places the main cursor.
    MainCursor,
    /// Places an extra cursor.
    ExtraCursor,

    /// Changes the alignment of the text to the left of the area.
    /// This only takes effect after this line terminates.
    AlignLeft,
    /// Changes the alignment of the text to the right of the area.
    /// This only takes effect after this line terminates.
    AlignRight,
    /// Changes the alignemet of the text to the center of the area.  
    /// This only takes effect after this line terminates.
    AlignCenter,

    // Not Implemented:
    /// Begins a hoverable section in the file.
    HoverStart(u16),
    /// Ends a hoverable section in the file.
    HoverEnd(u16),
    /// Conceals a character with a string of text of equal lenght,
    /// permanently.
    PermanentConceal { index: u16 }
}

impl std::fmt::Debug for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tag::PushForm(index) => write!(f, "PushForm({})", index),
            Tag::PopForm(index) => write!(f, "PopForm({})", index),
            Tag::MainCursor => f.write_str("MainCursor"),
            Tag::ExtraCursor => f.write_str("ExtraCursor"),
            Tag::AlignLeft => f.write_str("AlignLeft"),
            Tag::AlignRight => f.write_str("AlignRight"),
            Tag::AlignCenter => f.write_str("AlignCenter"),
            _ => todo!()
        }
    }
}

impl Tag {
    pub fn inverse(&self) -> Option<Tag> {
        match self {
            Tag::PushForm(form_id) => Some(Tag::PopForm(*form_id)),
            Tag::PopForm(form_id) => Some(Tag::PushForm(*form_id)),
            Tag::HoverStart(index) => Some(Tag::HoverEnd(*index)),
            _ => None
        }
    }
}

#[derive(Clone, Copy)]
pub enum TagOrSkip {
    Tag(Tag, Handle),
    Skip(u32)
}

impl TagOrSkip {
    pub fn as_skip(&self) -> Option<&u32> {
        if let Self::Skip(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl std::fmt::Debug for TagOrSkip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TagOrSkip::Tag(tag, _) => write!(f, "{:?}", tag),
            TagOrSkip::Skip(skip) => write!(f, "Skip({:?})", skip)
        }
    }
}

impl Measurable for TagOrSkip {
    #[inline]
    fn width(&self) -> usize {
        match self {
            TagOrSkip::Tag(..) => 0,
            TagOrSkip::Skip(count) => *count as usize
        }
    }
}

// TODO: Generic container.
#[derive(Debug)]
pub enum Tags {
    Vec(Vec<TagOrSkip>),
    Rope(AnyRope<TagOrSkip>)
}

impl Tags {
    pub fn default_vec() -> Self {
        Tags::Vec(Vec::new())
    }

    pub fn default_rope() -> Self {
        Tags::Rope(AnyRope::new())
    }

    pub fn new(inner_text: &InnerText) -> Self {
        let skip = TagOrSkip::Skip(inner_text.len_chars() as u32);
        match inner_text {
            InnerText::String(_) => Tags::Vec(vec![skip]),
            InnerText::Rope(_) => Tags::Rope(AnyRope::from_slice(&[skip]))
        }
    }

    pub fn insert(&mut self, ch_index: usize, tag: Tag, handle: Handle) {
        assert!(
            ch_index <= self.inner.width(),
            "Char index {} too large, {:#?}",
            ch_index,
            self.inner
        );

        let Some((start, TagOrSkip::Skip(skip))) = self.inner.get_from_ch_index(ch_index) else {
            self.inner.insert(ch_index, TagOrSkip::Tag(tag, handle));
            return;
        };

        // If inserting at any of the ends, no splitting is necessary.
        if ch_index == start || ch_index == (start + skip as usize) {
            self.inner.insert(ch_index, TagOrSkip::Tag(tag, handle))
        } else {
            let insertion = [
                TagOrSkip::Skip((ch_index - start) as u32),
                TagOrSkip::Tag(tag, handle),
                TagOrSkip::Skip(start as u32 + skip - ch_index as u32)
            ];
            self.inner.insert_slice(start, &insertion);

            let skip_range = (start + skip as usize)..(start + 2 * skip as usize);
            self.inner.remove_exclusive(skip_range);
        }

        self.merge_surrounding_skips(ch_index);
    }

    /// Removes all [Tag]s associated with a given [Lock] in the
    /// `ch_index`.
    pub fn remove_on(&mut self, ch_index: usize, handle: Handle) {
        self.inner.remove_on(ch_index, handle);
        self.merge_surrounding_skips(ch_index);
    }

    pub(crate) fn transform_range(&mut self, old: Range<usize>, new: Range<usize>) {
        let Some((start, t_or_s)) = self.inner.get_from_ch_index(old.start) else {
            return;
        };

        let removal_start = start.min(old.start);
        let removal_end = {
            let (start, t_or_s) = self
                .inner
                .get_from_ch_index(old.end)
                .filter(|(end_start, _)| *end_start > start)
                .unwrap_or((start, t_or_s));

            old.end.max(start + t_or_s.width())
        };

        let range_diff = new.count() as isize - old.count() as isize;
        let skip = (removal_end - removal_start).saturating_add_signed(range_diff);

        self.inner.insert(start, TagOrSkip::Skip(skip as u32));
        self.inner.remove_exclusive((removal_start + skip)..(removal_end + skip));
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, Tag)> + '_ {
        self.inner.iter().filter_map(|(width, t_or_s)| {
            if let TagOrSkip::Tag(tag, _) = t_or_s {
                Some((width, tag))
            } else {
                None
            }
        })
    }

    pub fn iter_at(&self, ch_index: usize) -> impl Iterator<Item = (usize, Tag)> + Clone + '_ {
        self.inner.iter_at(ch_index).filter_map(move |(width, t_or_s)| {
            if let TagOrSkip::Tag(tag, _) = t_or_s {
                Some((width, tag))
            } else {
                None
            }
        })
    }

    /// Transforms any surrounding clusters of multiple skips into a
    /// single one.
    ///
    /// This is crucial to prevent the gradual deterioration of the
    /// [`InnerTags`]'s structure.
    fn merge_surrounding_skips(&mut self, from: usize) {
        let (total_skip, last_width) = {
            let mut total_skip = 0;
            let mut last_width = from;
            let mut next_tags = self
                .inner
                .iter_at(from)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = next_tags.next() {
                total_skip += skip;
                last_width = width;
            }

            (total_skip, last_width)
        };

        if last_width != from {
            // The removal happens after the insertion in order to prevent `Tag`s
            // which are meant to be separate from coming together.
            self.inner.insert(from, TagOrSkip::Skip(total_skip));
            let skip_range = (from + total_skip as usize)..(from + 2 * total_skip as usize);
            self.inner.remove_exclusive(skip_range);
        }

        let (total_skip, first_width) = {
            let mut total_skip = 0;
            let mut first_width = from;

            let mut prev_tags = self
                .inner
                .iter_at_rev(from)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = prev_tags.next() {
                total_skip += skip;
                first_width = width;
            }

            (total_skip, first_width)
        };

        if first_width != from {
            self.inner.insert(first_width, TagOrSkip::Skip(total_skip));

            let range = (first_width + total_skip as usize)..(from + total_skip as usize);
            self.inner.remove_exclusive(range);
        }
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }

    /// Returns the length of this [`Tags`].
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn width(&self) -> usize {
        self.inner.width()
    }

    pub(crate) fn as_mut_vec(&mut self) -> Option<&mut Vec<TagOrSkip>> {
        match &mut self.inner {
            InnerTags::Vec(vec) => Some(vec),
            InnerTags::Rope(_) => None
        }
    }

    pub(crate) fn as_vec(&self) -> Option<&[TagOrSkip]> {
        match &self.inner {
            InnerTags::Vec(vec) => Some(vec),
            InnerTags::Rope(_) => None
        }
    }
}

impl Tags {
    pub fn inner_insert(&mut self, ch_index: usize, t_or_s: TagOrSkip) {
        match self {
            InnerTags::Vec(vec) => {
                let index = end_ch_to_index(vec, ch_index);
                vec.insert(index, t_or_s)
            }
            InnerTags::Rope(rope) => rope.insert(ch_index, t_or_s)
        }
    }

    pub fn insert_slice(&mut self, ch_index: usize, slice: &[TagOrSkip]) {
        match self {
            InnerTags::Vec(vec) => {
                let index = end_ch_to_index(vec, ch_index);
                vec.splice(index..index, slice.iter().copied());
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

                let start = end_ch_to_index(vec, range.start);
                let end = start_ch_to_index(vec, range.end);
                assert!(start <= end, "{}, {}\n{:?}", range.start, range.end, vec);
                vec.splice(start..end, []);
            }
            InnerTags::Rope(rope) => rope.remove_exclusive(range)
        }
    }

    pub fn remove_on(&mut self, ch_index: usize, lock: Handle) {
        match self {
            InnerTags::Vec(vec) => {
                let start = start_ch_to_index(vec, ch_index);
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
            InnerTags::Vec(vec) => Iter::Vec(
                vec.iter()
                    .scan(0, forward_tag_start)
                    .skip_while(move |(accum, _)| *accum < ch_index)
            ),
            InnerTags::Rope(rope) => Iter::Rope(rope.iter_at_width(ch_index))
        }
    }

    pub fn iter_at_rev(
        &self, ch_index: usize
    ) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            InnerTags::Vec(vec) => {
                let width = vec.iter().map(|t_or_s| t_or_s.width()).sum::<usize>();
                Iter::RevVec(vec.iter().rev().scan(width, reverse_tag_start).skip_while(
                    move |(accum, t_or_s)| {
                        *accum > ch_index && t_or_s.width() == 0 || *accum >= ch_index
                    }
                ))
            }
            InnerTags::Rope(rope) => Iter::Rope(rope.iter_at_width(ch_index).reversed())
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
