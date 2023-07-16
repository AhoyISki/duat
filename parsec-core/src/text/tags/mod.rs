use std::{
    cmp::Ordering::*,
    ops::{Range, RangeFrom, RangeTo}
};

use any_rope::{Measurable, Rope};
pub use container::Container;

use crate::text::chars::Chars;

mod container;

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
    min_ml_range: usize,
    above_min_ml_count: usize
}

impl Tags {
    pub fn default_vec() -> Self {
        Tags {
            container: Container::Vec(Vec::new()),
            ranges: Vec::new(),
            min_ml_range: 2,
            above_min_ml_count: 0
        }
    }

    pub fn default_rope() -> Self {
        Tags {
            container: Container::Rope(Rope::new()),
            ranges: Vec::new(),
            min_ml_range: 2,
            above_min_ml_count: 0
        }
    }

    pub fn new(chars: &Chars) -> Self {
        let skip = TagOrSkip::Skip(chars.len_chars() as u32);
        let container = match chars {
            Chars::String(_) => Container::Vec(vec![skip]),
            Chars::Rope(_) => Container::Rope(Rope::from_slice(&[skip]))
        };
        Tags { container, ranges: Vec::new(), min_ml_range: 2, above_min_ml_count: 0 }
    }

    pub fn insert(&mut self, pos: usize, tag: Tag, handle: Handle, chars: &Chars) {
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
        add_or_merge_entry((pos, tag, handle), &mut self.ranges, self.min_ml_range, chars);
    }

    /// Removes all [Tag]s associated with a given [Lock] in the
    /// `ch_index`.
    pub fn remove_on(&mut self, pos: usize, handle: Handle) {
        let removed = self.container.remove_inclusive_on(pos, handle);

        self.merge_surrounding_skips(pos);
        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges, &mut self.above_min_ml_count);
        }
    }

    pub fn transform_range(
        &mut self, old: Range<usize>, new_end: usize, old_nl_count: usize, chars: &Chars
    ) {
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

        let new_nl_count = chars.nl_count_in(old.start..new_end);
        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges, &mut self.above_min_ml_count);
        }
        if old_nl_count >= self.min_ml_range && new_nl_count < self.min_ml_range {
            let min_extra_nl = old_nl_count - new_nl_count;
            let ranges = self.ranges.extract_if(|range| {
                if let TagRange::Bounded(_, range, ..) = range {
                    if range.start <= old.start && range.end >= new_end {
                        let prefixed_nls = chars.nl_count_in(range.start..new.start);
                        let suffixed_nls = chars.nl_count_in(new.end..range.end);
                        return prefixed_nls + suffixed_nls < min_extra_nl;
                    }
                }
                false
            });
            ranges.last();
        } else if old_nl_count < self.min_ml_range && new_nl_count >= self.min_ml_range {
            let (start, end) = {
                let first_line = chars.char_to_line(new.start).unwrap();
                let start = chars.line_to_char(first_line).unwrap();
                let last_line = chars.char_to_line(new.end).unwrap();
                let end = chars.line_to_char(last_line + 1).unwrap_or(usize::MAX);
                (start, end)
            };

            for entry in
                self.container.iter_at(start).take_while(|(pos, _)| *pos < end).filter_map(
                    |(pos, t_or_s)| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle))
                )
            {
                remove_from_ranges(entry, &mut self.ranges, &mut self.above_min_ml_count);
                add_or_merge_entry(entry, &mut self.ranges, self.min_ml_range, chars);
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

#[derive(Debug, PartialEq, Eq)]
pub enum TagRange {
    From(Tag, RangeFrom<usize>, Handle),
    Bounded(Tag, Range<usize>, usize, Handle),
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
            (TagRange::Bounded(_, _, _, _), TagRange::Until(_, _, _))
            | (TagRange::From(_, _, _), TagRange::Until(_, _, _)) => Less,
            (TagRange::Until(_, _, _), TagRange::Bounded(_, _, _, _))
            | (TagRange::Until(_, _, _), TagRange::From(_, _, _)) => Greater,

            (
                TagRange::Bounded(lhs_tag, lhs_range, _, lhs_handle),
                TagRange::Bounded(rhs_tag, rhs_range, _, rhs_handle)
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
            TagRange::From(tag, _, _) => *tag,
            TagRange::Bounded(tag, _, _, _) => *tag,
            TagRange::Until(tag, _, _) => *tag
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
            TagRange::Bounded(tag, range, _, handle) => {
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
            TagRange::Bounded(tag, range, _, handle) => {
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
}

/// Removes the given `(usize, Tag, Handle)` triples from any
/// range in `self.ranges`.
///
/// This will either lead to a partially unbounded range, or
/// completely remove it.
fn remove_from_ranges(
    entry: (usize, Tag, Handle), ranges: &mut Vec<TagRange>, above_min_ml_count: &mut usize
) {
    let mut iter = ranges.iter();
    if entry.1.is_start() {
        if let Some(index) = iter.position(|range| range.starts_with(entry)) {
            if let TagRange::Bounded(tag, range, _, handle) = &ranges[index] {
                *above_min_ml_count -= 1;
                ranges[index] = TagRange::Until(*tag, ..range.end, *handle);
            } else {
                ranges.remove(index);
            }
        }
    } else if entry.1.is_end() {
        if let Some(index) = iter.position(|range| range.ends_with(entry)) {
            if let TagRange::Bounded(tag, range, _, handle) = &ranges[index] {
                *above_min_ml_count -= 1;
                ranges[index] = TagRange::From(*tag, range.end.., *handle);
            } else {
                ranges.remove(index);
            }
        }
    }
}

fn add_or_merge_entry(
    entry: (usize, Tag, Handle), ranges: &mut Vec<TagRange>, min_ml_range: usize, chars: &Chars
) {
    if !(entry.1.is_start() || entry.1.is_end()) {
        return;
    }

    let range = if entry.1.is_start() {
        if let Some(range) = ranges.extract_if(|range| range.can_start_with(entry)).next() {
            let end = range.get_end().unwrap();
            let nl_count = chars.nl_count_in(entry.0..end);
            (nl_count >= min_ml_range)
                .then_some(TagRange::Bounded(entry.1, entry.0..end, nl_count, entry.2))
        } else {
            Some(TagRange::From(entry.1, entry.0.., entry.2))
        }
    } else if let Some(range) = ranges.extract_if(|range| range.can_end_with(entry)).next() {
        let start = range.get_start().unwrap();
        let nl_count = chars.nl_count_in(start..entry.0);
        (nl_count >= min_ml_range)
            .then(|| TagRange::Bounded(range.tag(), start..entry.0, nl_count, entry.2))
    } else {
        Some(TagRange::Until(entry.1, ..entry.0, entry.2))
    };

    if let Some(range) = range {
        let (Ok(index) | Err(index)) = ranges.binary_search(&range);
        ranges.insert(index, range);
    }
}
