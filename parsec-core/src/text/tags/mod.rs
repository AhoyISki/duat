use std::{
    iter::{Rev, Scan, SkipWhile},
    ops::Range
};

use any_rope::{Measurable, Rope as AnyRope};

use crate::text::chars::Chars;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Handle(u16);

impl Handle {
    pub fn new() -> Handle {
        use std::sync::atomic::{AtomicU16, Ordering};
        static LOCK_COUNT: AtomicU16 = AtomicU16::new(0);

        Handle(LOCK_COUNT.fetch_add(1, Ordering::Acquire))
    }
}

impl Default for Handle {
    fn default() -> Self {
        Handle::new()
    }
}

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
        match self {
            Self::Skip(v) => Some(v),
            TagOrSkip::Tag(..) => None,
        }
    }

    fn as_tag(&self) -> Option<(Tag, Handle)> {
        match self {
            TagOrSkip::Tag(tag, handle) => Some((*tag, *handle)),
            TagOrSkip::Skip(_) => None
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
    Vec {
        vec: Vec<TagOrSkip>,
        ranges: Vec<TagRange>
    },
    Rope {
        rope: AnyRope<TagOrSkip>,
        ranges: Vec<TagRange>
    }
}

impl Tags {
    pub fn default_vec() -> Self {
        Tags::Vec {
            vec: Vec::new(),
            ranges: Vec::new()
        }
    }

    pub fn default_rope() -> Self {
        Tags::Rope {
            rope: AnyRope::new(),
            ranges: Vec::new()
        }
    }

    pub fn new(inner_text: &Chars) -> Self {
        let skip = TagOrSkip::Skip(inner_text.len_chars() as u32);
        match inner_text {
            Chars::String(_) => Tags::Vec {
                vec: vec![skip],
                ranges: Vec::new()
            },
            Chars::Rope(_) => Tags::Rope {
                rope: AnyRope::from_slice(&[skip]),
                ranges: Vec::new()
            }
        }
    }

    pub fn insert(&mut self, char: usize, tag: Tag, handle: Handle) {
        assert!(char <= self.width(), "Char index {} too large, {:#?}", char, self);

        let Some((start, TagOrSkip::Skip(skip))) = self.get_from_char(char) else {
            self.raw_insert(char, TagOrSkip::Tag(tag, handle));
            return;
        };

        // If inserting at any of the ends, no splitting is necessary.
        if char == start || char == (start + skip as usize) {
            self.raw_insert(char, TagOrSkip::Tag(tag, handle))
        } else {
            let insertion = [
                TagOrSkip::Skip((char - start) as u32),
                TagOrSkip::Tag(tag, handle),
                TagOrSkip::Skip(start as u32 + skip - char as u32)
            ];
            self.raw_insert_slice(start, &insertion);

            let skip_range = (start + skip as usize)..(start + 2 * skip as usize);
            self.remove_exclusive(skip_range);
        }

        self.merge_surrounding_skips(char);
    }

    /// Removes all [Tag]s associated with a given [Lock] in the
    /// `ch_index`.
    pub fn remove_on(&mut self, ch_index: usize, handle: Handle) {
        match self {
            Tags::Vec { vec, .. } => {
                let start = start_ch_to_index(vec, ch_index);
                let end = end_ch_to_index(&vec[start..], 0);
                vec.extract_if(|tag_or_skip| match tag_or_skip {
                    TagOrSkip::Tag(_, cmp_handle) => handle == *cmp_handle,
                    TagOrSkip::Skip(_) => false
                })
                .take(end)
                .skip(start)
                .last();
            }
            Tags::Rope { rope, .. } => {
                let slice = rope.width_slice(ch_index..ch_index);
                let tags = slice.iter();

                let tags = tags
                    .filter_map(|(_, t_or_s)| {
                        if let TagOrSkip::Tag(tag, cmp_handle) = t_or_s && cmp_handle != handle {
                            Some(TagOrSkip::Tag(tag, cmp_handle))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<TagOrSkip>>();

                rope.remove_inclusive(ch_index..ch_index);
                rope.insert_slice(ch_index, tags.as_slice());
            }
        }
        self.merge_surrounding_skips(ch_index);
    }

    pub fn transform_range(&mut self, old: Range<usize>, new: Range<usize>) {
        let Some((start, t_or_s)) = self.get_from_char(old.start) else {
            return;
        };

        let removal_start = start.min(old.start);
        let removal_end = {
            let (start, t_or_s) = self
                .get_from_char(old.end)
                .filter(|(end_start, _)| *end_start > start)
                .unwrap_or((start, t_or_s));

            old.end.max(start + t_or_s.width())
        };

        let range_diff = new.count() as isize - old.count() as isize;
        let skip = (removal_end - removal_start).saturating_add_signed(range_diff);

        let removals = self
            .raw_iter_at(removal_start)
            .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle)))
            .take_while(|(pos, ..)| *pos < removal_end)
            .collect::<Vec<(usize, Tag, Handle)>>();

        self.raw_insert(start, TagOrSkip::Skip(skip as u32));
        self.remove_exclusive((removal_start + skip)..(removal_end + skip));

        self.remove_from_ranges(&removals);
    }

    pub fn iter_at(&self, ch_index: usize) -> impl Iterator<Item = (usize, Tag)> + Clone + '_ {
        self.raw_iter_at(ch_index).filter_map(move |(width, t_or_s)| {
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
                .raw_iter_at(from)
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
            self.raw_insert(from, TagOrSkip::Skip(total_skip));
            let skip_range = (from + total_skip as usize)..(from + 2 * total_skip as usize);
            self.remove_exclusive(skip_range);
        }

        let (total_skip, first_width) = {
            let mut total_skip = 0;
            let mut first_width = from;

            let mut prev_tags = self
                .raw_rev_iter_at(from)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = prev_tags.next() {
                total_skip += skip;
                first_width = width;
            }

            (total_skip, first_width)
        };

        if first_width != from {
            self.raw_insert(first_width, TagOrSkip::Skip(total_skip));

            let range = (first_width + total_skip as usize)..(from + total_skip as usize);
            self.remove_exclusive(range);
        }
    }

    fn remove_from_ranges(&mut self, removed: &[(usize, Tag, Handle)]) {
        let ranges = match self {
            Tags::Vec { ranges, .. } => ranges,
            Tags::Rope { ranges, .. } => ranges
        };
        for removal in removed {
            let pos = ranges.binary_search_by(|TagRange { start, handle, .. }| {
                start
                    .and_then(|(char, tag)| (char, tag, *handle).partial_cmp(removal))
                    .unwrap_or(std::cmp::Ordering::Less)
            });

            if let Ok(pos) = pos {
                if ranges[pos].end.is_none() {
                    ranges.remove(pos);
                } else {
                    ranges[pos].start = None;
                }
                continue;
            }

            let pos = ranges.binary_search_by(|TagRange { end, handle, .. }| {
                end.and_then(|(char, tag)| (char, tag, *handle).partial_cmp(removal))
                    .unwrap_or(std::cmp::Ordering::Greater)
            });

            if let Ok(pos) = pos {
                if ranges[pos].start.is_none() {
                    ranges.remove(pos);
                } else {
                    ranges[pos].end = None;
                }
                continue;
            }
        }
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(crate) fn as_mut_vec(&mut self) -> Option<&mut Vec<TagOrSkip>> {
        match self {
            Tags::Vec { vec, .. } => Some(vec),
            Tags::Rope { .. } => None
        }
    }

    pub(crate) fn as_vec(&self) -> Option<&[TagOrSkip]> {
        match self {
            Tags::Vec { vec, .. } => Some(vec),
            Tags::Rope { .. } => None
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Tags::Vec { vec, .. } => vec.iter().map(|tag_or_skip| tag_or_skip.width()).sum(),
            Tags::Rope { rope, .. } => rope.width()
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Tags::Vec { vec, .. } => vec.len(),
            Tags::Rope { rope, .. } => rope.len()
        }
    }

    pub fn clear(&mut self) {
        match self {
            Tags::Vec { vec, .. } => vec.clear(),
            Tags::Rope { rope, .. } => *rope = AnyRope::new()
        }
    }
}

impl Tags {
    fn raw_insert(&mut self, ch_index: usize, t_or_s: TagOrSkip) {
        match self {
            Tags::Vec { vec, .. } => {
                let index = end_ch_to_index(vec, ch_index);
                vec.insert(index, t_or_s)
            }
            Tags::Rope { rope, .. } => rope.insert(ch_index, t_or_s)
        }
    }

    fn raw_insert_slice(&mut self, ch_index: usize, slice: &[TagOrSkip]) {
        match self {
            Tags::Vec { vec, .. } => {
                let index = end_ch_to_index(vec, ch_index);
                vec.splice(index..index, slice.iter().copied());
            }
            Tags::Rope { rope, .. } => rope.insert_slice(ch_index, slice)
        }
    }

    pub fn remove_exclusive(&mut self, range: Range<usize>) {
        match self {
            Tags::Vec { vec, .. } => {
                if range.start == range.end {
                    return;
                }

                let start = end_ch_to_index(vec, range.start);
                let end = start_ch_to_index(vec, range.end);
                assert!(start <= end, "{}, {}\n{:?}", range.start, range.end, vec);
                vec.splice(start..end, []);
            }
            Tags::Rope { rope, .. } => rope.remove_exclusive(range)
        }
    }

    pub fn get_from_char(&self, char: usize) -> Option<(usize, TagOrSkip)> {
        match self {
            Tags::Vec { vec, .. } => vec
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
            Tags::Rope { rope, .. } => rope.get_from_width(char)
        }
    }

    pub fn raw_iter_at(
        &self, ch_index: usize
    ) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            Tags::Vec { vec, .. } => Iter::Vec(
                vec.iter()
                    .scan(0, forward_tag_start)
                    .skip_while(move |(accum, _)| *accum < ch_index)
            ),
            Tags::Rope { rope, .. } => Iter::Rope(rope.iter_at_width(ch_index))
        }
    }

    pub fn raw_rev_iter_at(
        &self, ch_index: usize
    ) -> impl Iterator<Item = (usize, TagOrSkip)> + Clone + '_ {
        match self {
            Tags::Vec { vec, .. } => {
                let width = vec.iter().map(|t_or_s| t_or_s.width()).sum::<usize>();
                Iter::RevVec(vec.iter().rev().scan(width, reverse_tag_start).skip_while(
                    move |(accum, t_or_s)| {
                        *accum > ch_index && t_or_s.width() == 0 || *accum >= ch_index
                    }
                ))
            }
            Tags::Rope { rope, .. } => Iter::Rope(rope.iter_at_width(ch_index).reversed())
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TagRange {
    start: Option<(usize, Tag)>,
    end: Option<(usize, Tag)>,
    handle: Handle
}

impl TagRange {
    fn contains(&self, char: usize) -> bool {
        match (self.start, self.end) {
            (None, None) => true,
            (None, Some((end, _))) => (..end).contains(&char),
            (Some((start, _)), None) => (start..).contains(&char),
            (Some((start, _)), Some((end, _))) => (start..end).contains(&char)
        }
    }
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
