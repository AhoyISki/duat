use std::{
    cmp::Ordering::*,
    ops::{Range, RangeFrom, RangeTo}
};

use any_rope::{Measurable, Rope};
pub use container::Container;

use crate::text::chars::Chars;

mod container;

const MIN_CHARS_TO_CULL: usize = 50;
const MAX_CHARS_TO_CULL: usize = 1000;

const BUMP_AMOUNT: usize = 50;
const RANGES_TO_BUMP: usize = 100;

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
    /// Changes the alignemet of the text to the center of the area.  
    /// This only takes effect after this line terminates.
    AlignCenter,
    /// Changes the alignment of the text to the right of the area.
    /// This only takes effect after this line terminates.
    AlignRight,

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
            Tag::AlignCenter => f.write_str("AlignCenter"),
            Tag::AlignRight => f.write_str("AlignRight"),
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

    fn ends_with(&self, other: &Tag) -> bool {
        match (self, other) {
            (Tag::PushForm(form), Tag::PopForm(other)) => form == other,
            (Tag::AlignCenter | Tag::AlignRight, Tag::AlignLeft) => true,
            (Tag::HoverStart(hover), Tag::HoverEnd(other)) => hover == other,
            _ => false
        }
    }

    fn is_start(&self) -> bool {
        matches!(self, Tag::PushForm(_) | Tag::AlignCenter | Tag::AlignRight | Tag::HoverStart(_))
    }

    fn is_end(&self) -> bool {
        matches!(self, Tag::PopForm(_) | Tag::AlignLeft | Tag::HoverEnd(_))
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
            TagOrSkip::Tag(..) => None
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
pub struct Tags {
    container: Container,
    pub ranges: Vec<TagRange>,
    max_to_cull: usize
}

impl Tags {
    pub fn default_vec() -> Self {
        Tags {
            container: Container::Vec(Vec::new()),
            ranges: Vec::new(),
            max_to_cull: MIN_CHARS_TO_CULL
        }
    }

    pub fn default_rope() -> Self {
        Tags {
            container: Container::Rope(Rope::new()),
            ranges: Vec::new(),
            max_to_cull: MIN_CHARS_TO_CULL
        }
    }

    pub fn new(chars: &Chars) -> Self {
        let skip = TagOrSkip::Skip(chars.len_chars() as u32);
        let container = match chars {
            Chars::String(_) => Container::Vec(vec![skip]),
            Chars::Rope(_) => Container::Rope(Rope::from_slice(&[skip]))
        };
        Tags { container, ranges: Vec::new(), max_to_cull: MIN_CHARS_TO_CULL }
    }

    pub fn insert(&mut self, pos: usize, tag: Tag, handle: Handle) {
        assert!(pos <= self.width(), "Char index {} too large, {:#?}", pos, self);

        let Some((start, TagOrSkip::Skip(skip))) = self.get_from_char(pos) else {
            self.container.insert(pos, TagOrSkip::Tag(tag, handle));
            return;
        };

        // If inserting at any of the ends, no splitting is necessary.
        if pos == start || pos == (start + skip as usize) {
            self.container.insert(pos, TagOrSkip::Tag(tag, handle))
        } else {
            let insertion = [
                TagOrSkip::Skip((pos - start) as u32),
                TagOrSkip::Tag(tag, handle),
                TagOrSkip::Skip(start as u32 + skip - pos as u32)
            ];
            self.container.insert_slice(start, &insertion);

            let skip_range = (start + skip as usize)..(start + 2 * skip as usize);
            self.container.remove_exclusive(skip_range);
        }

        self.merge_surrounding_skips(pos);
        process_entry((pos, tag, handle), &mut self.ranges, self.max_to_cull);
    }

    /// Removes all [Tag]s associated with a given [Lock] in the
    /// `ch_index`.
    pub fn remove_on(&mut self, pos: usize, handle: Handle) {
        let removed = self.container.remove_inclusive_on(pos, handle);

        self.merge_surrounding_skips(pos);
        let mut clipped = Vec::new();
        for entry in removed {
            if let Some(range) = remove_from_ranges(entry, &mut self.ranges) {
                clipped.push(range);
            }
        }
    }

    pub fn transform_range(&mut self, old: Range<usize>, new_end: usize) {
        let Some((start, t_or_s)) = self.get_from_char(old.start) else {
            panic!();
        };
        let new = old.start..new_end;

        let removal_start = start.min(old.start);
        let removal_end = {
            let (start, t_or_s) = self
                .get_from_char(old.end)
                .filter(|(end_start, _)| *end_start > start)
                .unwrap_or((start, t_or_s));

            old.end.max(start + t_or_s.width())
        };

        let range_diff = new_end as isize - old.end as isize;
        let skip = (removal_end - removal_start).saturating_add_signed(range_diff);

        let removed: Vec<(usize, Tag, Handle)> = self
            .container
            .iter_at(removal_start)
            .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle)))
            .take_while(|(pos, ..)| *pos < removal_end)
            .collect();

        self.container.insert(start, TagOrSkip::Skip(skip as u32));
        self.container.remove_exclusive((removal_start + skip)..(removal_end + skip));

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        self.shift_ranges_after(new.end, range_diff);
        self.process_ranges_containing(new, old.count());
    }

    fn shift_ranges_after(&mut self, after: usize, amount: isize) {
        for range in self.ranges.iter_mut() {
            match range {
                TagRange::From(_, range, _) => {
                    if range.start >= after {
                        *range = range.start.saturating_add_signed(amount)..
                    }
                }
                TagRange::Bounded(_, range, _) => {
                    if range.start >= after {
                        let start = range.start.saturating_add_signed(amount);
                        let end = range.end.saturating_add_signed(amount);
                        *range = start..end
                    } else if range.end >= after {
                        range.end = range.end.saturating_add_signed(amount)
                    }
                }
                TagRange::Until(_, range, _) => {
                    if range.end >= after {
                        *range = ..range.end.saturating_add_signed(amount)
                    }
                }
            }
        }
    }

    fn process_ranges_containing(&mut self, new: Range<usize>, old_count: usize) {
        let new_count = new.clone().count();
        if old_count >= self.max_to_cull && new_count < self.max_to_cull {
            self.ranges
                .extract_if(|range| {
                    if let TagRange::Bounded(_, range, ..) = range {
                        if range.start <= new.start && range.end >= new.end {
                            return range.count() <= self.max_to_cull;
                        }
                    }
                    false
                })
                .take_while(|range| range.get_start().is_some_and(|start| start <= new.start))
                .last();
        } else if old_count < self.max_to_cull && new_count >= self.max_to_cull {
            let start = new.start.saturating_sub(self.max_to_cull - old_count);
            let end = new.end + self.max_to_cull - old_count;
            for entry in
                self.container.iter_at(start).take_while(|(pos, _)| *pos < end).filter_map(
                    |(pos, t_or_s)| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle))
                )
            {
                remove_from_ranges(entry, &mut self.ranges);
                process_entry(entry, &mut self.ranges, self.max_to_cull);
            }
        }
    }

    pub fn iter_at(&self, at: usize) -> impl Iterator<Item = (usize, Tag)> + Clone + '_ {
        let ml_iter = self
            .ranges
            .iter()
            .take_while(move |range| range.get_start().is_some_and(|start| start < at))
            .filter_map(move |range| {
                range.get_end().map_or(true, |end| end > at).then(|| unsafe {
                    range.get_start().map(|start| (start, range.tag())).unwrap_unchecked()
                })
            });

        let same_line_iter = self.container.iter_at(at).filter_map(move |(width, t_or_s)| {
            if let TagOrSkip::Tag(tag, _) = t_or_s { Some((width, tag)) } else { None }
        });

        ml_iter.chain(same_line_iter)
    }

    pub fn back_check_amount(&self) -> usize {
        self.max_to_cull
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_mut_vec(&mut self) -> Option<&mut Vec<TagOrSkip>> {
        match &mut self.container {
            Container::Vec(vec) => Some(vec),
            Container::Rope(_) => None
        }
    }

    pub fn as_vec(&self) -> Option<&[TagOrSkip]> {
        match &self.container {
            Container::Vec(vec) => Some(vec),
            Container::Rope(_) => None
        }
    }

    pub fn width(&self) -> usize {
        match &self.container {
            Container::Vec(vec) => vec.iter().map(|tag_or_skip| tag_or_skip.width()).sum(),
            Container::Rope(rope) => rope.width()
        }
    }

    pub fn len(&self) -> usize {
        match &self.container {
            Container::Vec(vec) => vec.len(),
            Container::Rope(rope) => rope.len()
        }
    }

    pub fn clear(&mut self) {
        match &mut self.container {
            Container::Vec(vec) => vec.clear(),
            Container::Rope(rope) => *rope = Rope::new()
        }
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
                .container
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
            self.container.insert(from, TagOrSkip::Skip(total_skip));
            let skip_range = (from + total_skip as usize)..(from + 2 * total_skip as usize);
            self.container.remove_exclusive(skip_range);
        }

        let (total_skip, first_width) = {
            let mut total_skip = 0;
            let mut first_width = from;

            let mut prev_tags = self
                .container
                .rev_iter_at(from)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = prev_tags.next() {
                total_skip += skip;
                first_width = width;
            }

            (total_skip, first_width)
        };

        if first_width != from {
            self.container.insert(first_width, TagOrSkip::Skip(total_skip));

            let range = (first_width + total_skip as usize)..(from + total_skip as usize);
            self.container.remove_exclusive(range);
        }
    }

    pub fn get_from_char(&self, char: usize) -> Option<(usize, TagOrSkip)> {
        self.container.get_from_char(char)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TagRange {
    From(Tag, RangeFrom<usize>, Handle),
    Bounded(Tag, Range<usize>, Handle),
    Until(Tag, RangeTo<usize>, Handle)
}

impl Ord for TagRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.partial_cmp(other).unwrap_unchecked() }
    }
}

impl PartialOrd for TagRange {
    /// Entries will be ordered in the following order:
    ///
    /// - First, a mix of `TagRange::Bounded` and `TagRange::From`,
    ///   sorted by:
    ///   - Their starts;
    ///   - Their ends (if `TagRange::From`, always `Greater`);
    ///   - Their `Tag`s;
    ///   - Their `Handle`s.
    ///
    /// - After this, all of the `TagRange::Until` are placed, sorted
    ///   by:
    ///   - Their ends;
    ///   - Their `Tag`s;
    ///   - Their `Handle`s.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let ordering = match (self, other) {
            (TagRange::Bounded(..), TagRange::Until(..))
            | (TagRange::From(..), TagRange::Until(..)) => Less,
            (TagRange::Until(..), TagRange::Bounded(..))
            | (TagRange::Until(..), TagRange::From(..)) => Greater,

            (
                TagRange::Bounded(lhs_tag, lhs_range, lhs_handle),
                TagRange::Bounded(rhs_tag, rhs_range, rhs_handle)
            ) => {
                let lhs = (lhs_range.start, lhs_range.end, lhs_tag, lhs_handle);
                lhs.cmp(&(rhs_range.start, rhs_range.end, rhs_tag, rhs_handle))
            }
            (TagRange::Bounded(_, lhs, ..), TagRange::From(_, rhs, _)) => {
                let ordering = lhs.start.cmp(&rhs.start);
                if ordering == Equal { Less } else { ordering }
            }
            (
                TagRange::Until(lhs_tag, lhs_range, lhs_handle),
                TagRange::Until(rhs_tag, rhs_range, rhs_handle)
            ) => (lhs_range.end, lhs_tag, lhs_handle).cmp(&(rhs_range.end, rhs_tag, rhs_handle)),
            (TagRange::From(_, lhs, _), TagRange::Bounded(_, rhs, ..)) => {
                let ordering = lhs.start.cmp(&rhs.start);
                if ordering == Equal { Greater } else { ordering }
            }
            (
                TagRange::From(lhs_tag, lhs_range, lhs_handle),
                TagRange::From(rhs_tag, rhs_range, rhs_handle)
            ) => (lhs_range.start, lhs_tag, lhs_handle).cmp(&(rhs_range.start, rhs_tag, rhs_handle))
        };

        Some(ordering)
    }
}

impl TagRange {
    fn tag(&self) -> Tag {
        match self {
            TagRange::From(tag, ..) => *tag,
            TagRange::Bounded(tag, ..) => *tag,
            TagRange::Until(tag, ..) => *tag
        }
    }

    fn get_start(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, range, ..) => Some(range.start),
            TagRange::From(_, range, _) => Some(range.start),
            TagRange::Until(..) => None
        }
    }

    fn get_end(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, range, ..) => Some(range.end),
            TagRange::Until(_, range, _) => Some(range.end),
            TagRange::From(..) => None
        }
    }

    fn starts_with(&self, other: (usize, Tag, Handle)) -> bool {
        match self {
            TagRange::Bounded(tag, range, handle) => {
                *handle == other.2 && range.end == other.0 && *tag == other.1
            }
            TagRange::From(tag, range, handle) => {
                *handle == other.2 && range.start == other.0 && *tag == other.1
            }
            TagRange::Until(..) => false
        }
    }

    fn ends_with(&self, other: (usize, Tag, Handle)) -> bool {
        match self {
            TagRange::Bounded(tag, range, handle) => {
                *handle == other.2 && range.end == other.0 && *tag == other.1
            }
            TagRange::Until(tag, range, handle) => {
                *handle == other.2 && range.end == other.0 && *tag == other.1
            }
            TagRange::From(..) => false
        }
    }

    fn can_start_with(&self, other: (usize, Tag, Handle)) -> bool {
        match self {
            TagRange::Until(tag, range, handle) => {
                *handle == other.2 && other.0 <= range.end && other.1.ends_with(tag)
            }
            TagRange::Bounded(..) | TagRange::From(..) => false
        }
    }

    fn can_end_with(&self, other: (usize, Tag, Handle)) -> bool {
        match self {
            TagRange::From(tag, range, handle) => {
                *handle == other.2 && range.start <= other.0 && tag.ends_with(&other.1)
            }
            TagRange::Bounded(..) | TagRange::Until(..) => false
        }
    }

    fn count_ge(&self, other: usize) -> bool {
        match self {
            TagRange::Bounded(_, range, _) => range.clone().count() >= other,
            TagRange::From(..) | TagRange::Until(..) => true
        }
    }
}

/// Removes the given `(usize, Tag, Handle)` triples from any
/// range in `self.ranges`.
///
/// This will either lead to a partially unbounded range, or
/// completely remove it.
fn remove_from_ranges(entry: (usize, Tag, Handle), ranges: &mut Vec<TagRange>) -> Option<usize> {
    let mut iter = ranges.iter();
    if entry.1.is_start() {
        if let Some(index) = iter.position(|range| range.starts_with(entry)) {
            if let TagRange::Bounded(tag, range, handle) = &ranges[index] {
                ranges[index] = TagRange::Until(*tag, ..range.end, *handle);
                return Some(index);
            } else {
                ranges.remove(index);
            }
        }
    } else if entry.1.is_end() {
        if let Some(index) = iter.position(|range| range.ends_with(entry)) {
            if let TagRange::Bounded(tag, range, handle) = &ranges[index] {
                ranges[index] = TagRange::From(*tag, range.end.., *handle);
                return Some(index);
            } else {
                ranges.remove(index);
            }
        }
    }

    None
}

fn process_entry(entry: (usize, Tag, Handle), ranges: &mut Vec<TagRange>, min_to_cull: usize) {
    let range = if entry.1.is_start() {
        let (start, tag, handle) = entry;
        if let Some(range) = ranges.extract_if(|range| range.can_start_with(entry)).next() {
            let end = range.get_end().unwrap();
            TagRange::Bounded(tag, start..end, handle)
        } else {
            TagRange::From(tag, start.., handle)
        }
    } else if entry.1.is_end() {
        let (end, _, handle) = entry;
        if let Some(range) = ranges.extract_if(|range| range.can_end_with(entry)).next() {
            let start = range.get_start().unwrap();
            TagRange::Bounded(range.tag(), start..end, handle)
        } else {
            TagRange::Until(entry.1, ..entry.0, entry.2)
        }
    } else {
        return;
    };

    if range.count_ge(min_to_cull) {
        let (Ok(index) | Err(index)) = ranges.binary_search(&range);
        ranges.insert(index, range);
    }
}
