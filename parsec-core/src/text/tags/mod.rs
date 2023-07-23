use std::{
    cmp::Ordering::*,
    ops::{Range, RangeFrom, RangeTo}
};

use any_rope::{Measurable, Rope};
use container::Container;

use super::Text;
use crate::{forms::FormId, position::Pos, text::chars::Chars};

mod container;

const MIN_CHARS_TO_KEEP: usize = 50;

const BUMP_AMOUNT: usize = 50;
const LIMIT_TO_BUMP: usize = 500;

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

pub enum Tag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(FormId),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(FormId),

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

    // In the process of implementing.
    ConcealStart,
    ConcealEnd,

    GhostString(String),
    GhostText(Text),

    // Not Implemented:
    /// Begins a hoverable section in the file.
    HoverStartNew(Box<dyn Fn(Pos) + Send + Sync>, Box<dyn Fn(Pos) + Send + Sync>),
    HoverStart(ToggleId),
    /// Ends a hoverable section in the file.
    HoverEnd(ToggleId),

    LeftButtonStartNew(Box<dyn Fn(Pos) + Send + Sync>, Box<dyn Fn(Pos) + Send + Sync>),
    LeftButtonStart(ToggleId),
    LeftButtonEnd(ToggleId),

    RightButtonStartNew(Box<dyn Fn(Pos) + Send + Sync>, Box<dyn Fn(Pos) + Send + Sync>),
    RightButtonStart(ToggleId),
    RightButtonEnd(ToggleId),

    MiddleButtonStartNew(Box<dyn Fn(Pos) + Send + Sync>, Box<dyn Fn(Pos) + Send + Sync>),
    MiddleButtonStart(ToggleId),
    MiddleButtonEnd(ToggleId)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ToggleId(usize);

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(FormId),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(FormId),

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

    // In the process of implementing.
    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`ConcealEnd`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealStart,
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealEnd,

    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    Skip(usize),
    GhostText(usize),

    // Not Implemented:
    /// Begins a hoverable section in the file.
    HoverStart(ToggleId),
    /// Ends a hoverable section in the file.
    HoverEnd(ToggleId),

    LeftButtonStart(ToggleId),
    LeftButtonEnd(ToggleId),

    RightButtonStart(ToggleId),
    RightButtonEnd(ToggleId),

    MiddleButtonStart(ToggleId),
    MiddleButtonEnd(ToggleId)
}

impl RawTag {
    pub fn inverse(&self) -> Option<RawTag> {
        match self {
            RawTag::PushForm(form_id) => Some(RawTag::PopForm(*form_id)),
            RawTag::PopForm(form_id) => Some(RawTag::PushForm(*form_id)),
            RawTag::HoverStart(index) => Some(RawTag::HoverEnd(*index)),
            RawTag::HoverEnd(index) => Some(RawTag::HoverStart(*index)),
            RawTag::ConcealStart => Some(RawTag::ConcealEnd),
            RawTag::ConcealEnd => Some(RawTag::ConcealStart),
            _ => None
        }
    }

    fn ends_with(&self, other: &RawTag) -> bool {
        match (self, other) {
            (RawTag::PushForm(form), RawTag::PopForm(other)) => form == other,
            (RawTag::AlignCenter | RawTag::AlignRight, RawTag::AlignLeft) => true,
            (RawTag::HoverStart(hover), RawTag::HoverEnd(other)) => hover == other,
            (RawTag::ConcealStart, RawTag::ConcealEnd) => true,
            _ => false
        }
    }

    fn is_start(&self) -> bool {
        matches!(
            self,
            RawTag::PushForm(_)
                | RawTag::AlignCenter
                | RawTag::AlignRight
                | RawTag::HoverStart(_)
                | RawTag::ConcealStart
        )
    }

    fn is_end(&self) -> bool {
        matches!(
            self,
            RawTag::PopForm(_) | RawTag::AlignLeft | RawTag::HoverEnd(_) | RawTag::ConcealEnd
        )
    }
}

#[derive(Clone, Copy)]
pub enum TagOrSkip {
    Tag(RawTag, Handle),
    Skip(usize)
}

impl TagOrSkip {
    pub fn as_skip(&self) -> Option<&usize> {
        match self {
            Self::Skip(v) => Some(v),
            TagOrSkip::Tag(..) => None
        }
    }

    fn as_tag(&self) -> Option<(RawTag, Handle)> {
        match self {
            TagOrSkip::Tag(tag, handle) => Some((*tag, *handle)),
            TagOrSkip::Skip(_) => None
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
pub struct Tags {
    container: Container,
    ranges: Vec<TagRange>,
    texts: Vec<Text>,
    toggles: Vec<(Box<dyn Fn(Pos) + Send + Sync>, Box<dyn Fn(Pos) + Send + Sync>)>,
    min_to_keep: usize
}

impl Tags {
    pub fn default_vec() -> Self {
        Tags {
            container: Container::Vec(Vec::new()),
            ranges: Vec::new(),
            texts: Vec::new(),
            toggles: Vec::new(),
            min_to_keep: MIN_CHARS_TO_KEEP
        }
    }

    pub fn default_rope() -> Self {
        Tags {
            container: Container::Rope(Rope::new()),
            ranges: Vec::new(),
            texts: Vec::new(),
            toggles: Vec::new(),
            min_to_keep: MIN_CHARS_TO_KEEP
        }
    }

    pub fn new(chars: &Chars) -> Self {
        let skip = TagOrSkip::Skip(chars.len_chars());
        let container = match chars {
            Chars::String(_) => Container::Vec(vec![skip]),
            Chars::Rope(_) => Container::Rope(Rope::from_slice(&[skip]))
        };
        Tags {
            container,
            ranges: Vec::new(),
            texts: Vec::new(),
            toggles: Vec::new(),
            min_to_keep: MIN_CHARS_TO_KEEP
        }
    }

    pub fn iter_at(&self, at: usize) -> Iter {
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
        self.min_to_keep
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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

    pub fn get_from_char(&self, char: usize) -> Option<(usize, TagOrSkip)> {
        self.container.get_from_char(char)
    }

    pub fn clear(&mut self) {
        match &mut self.container {
            Container::Vec(vec) => vec.clear(),
            Container::Rope(rope) => *rope = Rope::new()
        }
    }

    pub fn as_mut_vec(&mut self) -> Option<&mut Vec<TagOrSkip>> {
        match &mut self.container {
            Container::Vec(vec) => Some(vec),
            Container::Rope(_) => None
        }
    }

    pub fn insert(&mut self, pos: usize, raw_tag: Tag, handle: Handle) {
        let tag = match raw_tag {
            Tag::PushForm(id) => RawTag::PushForm(id),
            Tag::PopForm(id) => RawTag::PopForm(id),
            Tag::MainCursor => RawTag::MainCursor,
            Tag::ExtraCursor => RawTag::ExtraCursor,
            Tag::AlignLeft => RawTag::AlignLeft,
            Tag::AlignCenter => RawTag::AlignCenter,
            Tag::AlignRight => RawTag::AlignRight,
            Tag::ConcealStart => RawTag::ConcealStart,
            Tag::ConcealEnd => RawTag::ConcealEnd,
            Tag::GhostString(string) => {
                let text = Text::new_string(string);
                self.texts.push(text);
                RawTag::GhostText(self.texts.len() - 1)
            }
            Tag::GhostText(text) => {
                self.texts.push(text);
                RawTag::GhostText(self.texts.len() - 1)
            }
            Tag::HoverStartNew(on_fn, off_fn) => {
                self.toggles.push((on_fn, off_fn));
                RawTag::HoverStart(ToggleId(self.toggles.len() - 1))
            }
            Tag::HoverStart(id) => RawTag::HoverStart(id),
            Tag::HoverEnd(id) => RawTag::HoverEnd(id),

            Tag::LeftButtonStartNew(on_fn, off_fn) => {
                self.toggles.push((on_fn, off_fn));
                RawTag::LeftButtonStart(ToggleId(self.texts.len() - 1))
            }
            Tag::LeftButtonStart(id) => RawTag::LeftButtonStart(id),
            Tag::LeftButtonEnd(id) => RawTag::LeftButtonEnd(id),

            Tag::RightButtonStartNew(on_fn, off_fn) => {
                self.toggles.push((on_fn, off_fn));
                RawTag::RightButtonStart(ToggleId(self.texts.len() - 1))
            }
            Tag::RightButtonStart(id) => RawTag::RightButtonStart(id),
            Tag::RightButtonEnd(id) => RawTag::RightButtonEnd(id),

            Tag::MiddleButtonStartNew(on_fn, off_fn) => {
                self.toggles.push((on_fn, off_fn));
                RawTag::MiddleButtonStart(ToggleId(self.texts.len() - 1))
            }
            Tag::MiddleButtonStart(id) => RawTag::MiddleButtonStart(id),
            Tag::MiddleButtonEnd(id) => RawTag::MiddleButtonEnd(id)
        };

        assert!(pos <= self.width(), "Char index {} too large", pos);

        let Some((start, TagOrSkip::Skip(skip))) = self.get_from_char(pos) else {
            self.container.insert(pos, TagOrSkip::Tag(tag, handle));
            return;
        };

        // If inserting at any of the ends, no splitting is necessary.
        if pos == start || pos == (start + skip as usize) {
            self.container.insert(pos, TagOrSkip::Tag(tag, handle))
        } else {
            let insertion = [
                TagOrSkip::Skip(pos - start),
                TagOrSkip::Tag(tag, handle),
                TagOrSkip::Skip(start + skip - pos)
            ];
            self.container.insert_slice(start, &insertion);

            let skip_range = (start + skip as usize)..(start + 2 * skip as usize);
            self.container.remove_exclusive(skip_range);
        }

        self.merge_surrounding_skips(pos);
        try_insert((pos, tag, handle), &mut self.ranges, self.min_to_keep, true);
        rearrange_ranges(&mut self.ranges, self.min_to_keep);
        self.cull_small_ranges()
    }

    /// Removes all [Tag]s associated with a given [Lock] in the
    /// `ch_index`.
    pub fn remove_on(&mut self, pos: usize, handle: Handle) {
        let removed = self.container.remove_inclusive_on(pos, handle);

        self.merge_surrounding_skips(pos);
        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        rearrange_ranges(&mut self.ranges, self.min_to_keep);
    }

    pub fn transform_range(&mut self, old: Range<usize>, new_end: usize) {
        let (start, t_or_s) = self.get_from_char(old.start).unwrap();
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

        let removed: Vec<(usize, RawTag, Handle)> = self
            .container
            .iter_at(old.start + 1)
            .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle)))
            .take_while(|(pos, ..)| *pos < old.end)
            .collect();

        self.container.insert(start, TagOrSkip::Skip(skip));
        self.container.remove_exclusive((removal_start + skip)..(removal_end + skip));

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        shift_ranges_after(new.end, &mut self.ranges, range_diff);
        self.process_ranges_containing(new, old.count());
        rearrange_ranges(&mut self.ranges, self.min_to_keep);
        self.cull_small_ranges()
    }

    fn process_ranges_containing(&mut self, new: Range<usize>, old_count: usize) {
        let new_count = new.clone().count();
        if old_count == new_count {
            return;
        }

        if new_count < old_count {
            self.ranges
                .extract_if(|range| {
                    if let TagRange::Bounded(_, range, ..) = range {
                        if range.start <= new.start && range.end >= new.end {
                            return range.clone().count() <= self.min_to_keep;
                        }
                    }
                    false
                })
                .take_while(|range| range.get_start().is_some_and(|start| start <= new.start))
                .last();
        } else {
            let start = new.start.saturating_sub(self.min_to_keep - old_count);
            let end = new.end + self.min_to_keep - old_count;
            let mut entry_counts = Vec::new();

            for entry in
                self.container.iter_at(start).take_while(|(pos, _)| *pos < end).filter_map(
                    |(pos, t_or_s)| t_or_s.as_tag().map(|(tag, handle)| (pos, tag, handle))
                )
            {
                let count = if let Some((count, _)) =
                    entry_counts.iter_mut().find(|(_, other)| *other == entry)
                {
                    count
                } else {
                    let count = count_entry(entry, &self.ranges);
                    entry_counts.push((count, entry));
                    &mut entry_counts.last_mut().unwrap().0
                };

                if *count == 0 {
                    try_insert(entry, &mut self.ranges, self.min_to_keep, false);
                } else {
                    *count -= 1;
                }
            }
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

    fn cull_small_ranges(&mut self) {
        let mut cullable =
            self.ranges.iter().filter(|range| matches!(range, TagRange::Bounded(..))).count();

        while cullable > LIMIT_TO_BUMP {
            self.min_to_keep += BUMP_AMOUNT;
            cullable -= self
                .ranges
                .extract_if(|range| {
                    if let TagRange::Bounded(_, bounded, _) = range {
                        bounded.clone().count() < self.min_to_keep
                    } else {
                        false
                    }
                })
                .count()
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TagRange {
    Bounded(RawTag, Range<usize>, Handle),
    From(RawTag, RangeFrom<usize>, Handle),
    Until(RawTag, RangeTo<usize>, Handle)
}

impl TagRange {
    fn tag(&self) -> RawTag {
        match self {
            TagRange::Bounded(tag, ..) => *tag,
            TagRange::From(tag, ..) => *tag,
            TagRange::Until(tag, ..) => *tag
        }
    }

    fn get_start(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded, ..) => Some(bounded.start),
            TagRange::From(_, from, _) => Some(from.start),
            TagRange::Until(..) => None
        }
    }

    fn get_end(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded, ..) => Some(bounded.end),
            TagRange::Until(_, until, _) => Some(until.end),
            TagRange::From(..) => None
        }
    }

    fn starts_with(&self, other: (usize, RawTag, Handle)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded, handle) => {
                *handle == other.2 && bounded.start == other.0 && *tag == other.1
            }
            TagRange::From(tag, from, handle) => {
                *handle == other.2 && from.start == other.0 && *tag == other.1
            }
            TagRange::Until(..) => false
        }
    }

    fn ends_with(&self, other: (usize, RawTag, Handle)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded, handle) => {
                *handle == other.2 && bounded.end == other.0 && tag.ends_with(&other.1)
            }
            TagRange::Until(tag, until, handle) => {
                *handle == other.2 && until.end == other.0 && *tag == other.1
            }
            TagRange::From(..) => false
        }
    }

    fn can_start_with(&self, other: (usize, RawTag, Handle)) -> bool {
        match self {
            TagRange::Until(tag, until, handle) => {
                *handle == other.2 && other.0 <= until.end && other.1.ends_with(tag)
            }
            TagRange::Bounded(..) | TagRange::From(..) => false
        }
    }

    fn can_end_with(&self, other: (usize, RawTag, Handle)) -> bool {
        match self {
            TagRange::From(tag, from, handle) => {
                *handle == other.2 && from.start <= other.0 && tag.ends_with(&other.1)
            }
            TagRange::Bounded(..) | TagRange::Until(..) => false
        }
    }

    fn count_ge(&self, other: usize) -> bool {
        match self {
            TagRange::Bounded(_, bounded, _) => bounded.clone().count() >= other,
            TagRange::From(..) | TagRange::Until(..) => true
        }
    }
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

/// Removes the given `(usize, Tag, Handle)` triples from any
/// range in `self.ranges`.
///
/// This will either lead to a partially unbounded range, or
/// completely remove it.
///
/// Will return the range as it was, before the removal.
fn remove_from_ranges(entry: (usize, RawTag, Handle), ranges: &mut Vec<TagRange>) {
    if entry.1.is_start() {
        let range = ranges.extract_if(|range| range.starts_with(entry)).next();
        if let Some(TagRange::Bounded(tag, bounded, handle)) = range {
            let range = TagRange::Until(tag.inverse().unwrap(), ..bounded.end, handle);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    } else if entry.1.is_end() {
        let range = ranges.extract_if(|range| range.ends_with(entry)).next();
        if let Some(TagRange::Bounded(tag, bounded, handle)) = range {
            let range = TagRange::From(tag, bounded.start.., handle);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    }
}

fn count_entry(entry: (usize, RawTag, Handle), ranges: &[TagRange]) -> usize {
    if entry.1.is_start() {
        let (start, tag, handle) = entry;
        let range = TagRange::From(tag, start.., handle);
        if let Ok(index) = ranges.binary_search(&range) {
            let start_matches = |range: &&TagRange| range.starts_with(entry);
            let next = ranges.iter().skip(index).take_while(start_matches);
            let prev = ranges.iter().take(index).rev().take_while(start_matches);
            next.chain(prev).count()
        } else {
            0
        }
    } else if entry.1.is_end() {
        ranges.iter().filter(|range| range.ends_with(entry)).count()
    } else {
        0
    }
}

fn try_insert(
    entry: (usize, RawTag, Handle), ranges: &mut Vec<TagRange>, min_to_keep: usize,
    allow_unbounded: bool
) {
    let range = if entry.1.is_start() {
        let (start, tag, handle) = entry;
        if let Some(range) = ranges.extract_if(|range| range.can_start_with(entry)).next() {
            let end = range.get_end().unwrap();
            Some(TagRange::Bounded(tag, start..end, handle))
        } else {
            allow_unbounded.then_some(TagRange::From(tag, start.., handle))
        }
    } else if entry.1.is_end() {
        let (end, tag, handle) = entry;
        if let Some(index) = ranges.iter().rev().position(|range| range.can_end_with(entry)) {
            let range = ranges.remove(ranges.len() - 1 - index);
            let start = range.get_start().unwrap();
            Some(TagRange::Bounded(range.tag(), start..end, handle))
        } else {
            allow_unbounded.then_some(TagRange::Until(tag, ..end, handle))
        }
    } else {
        return;
    };

    if let Some(range) = range.filter(|range| range.count_ge(min_to_keep)) {
        let (Ok(index) | Err(index)) = ranges.binary_search(&range);
        ranges.insert(index, range);
    }
}

fn shift_ranges_after(after: usize, ranges: &mut [TagRange], amount: isize) {
    for range in ranges.iter_mut() {
        match range {
            TagRange::Bounded(_, bounded, _) => {
                if bounded.start >= after {
                    let start = bounded.start.saturating_add_signed(amount);
                    let end = bounded.end.saturating_add_signed(amount);
                    *bounded = start..end
                } else if bounded.end >= after {
                    bounded.end = bounded.end.saturating_add_signed(amount)
                }
            }
            TagRange::From(_, from, _) => {
                if from.start >= after {
                    *from = from.start.saturating_add_signed(amount)..
                }
            }
            TagRange::Until(_, until, _) => {
                if until.end >= after {
                    *until = ..until.end.saturating_add_signed(amount)
                }
            }
        }
    }
}

fn rearrange_ranges(ranges: &mut Vec<TagRange>, min_to_keep: usize) {
    let mut mergers = Vec::new();
    for (index, range) in ranges.iter().enumerate() {
        let (start, tag, handle) = match range {
            TagRange::Bounded(tag, bounded, handle) => (bounded.start, *tag, *handle),
            TagRange::From(tag, from, handle) => (from.start, *tag, *handle),
            TagRange::Until(..) => break
        };

        let other = ranges
            .iter()
            .enumerate()
            .rev()
            .filter(|(index, _)| mergers.iter().all(|(_, other)| other != index))
            .take_while(|(_, range)| matches!(range, TagRange::Until(..)))
            .filter(|(_, until)| until.can_start_with((start, tag, handle)))
            .last();

        if let Some((other, _)) = other {
            mergers.push((index, other));
        }
    }

    for (range, until) in mergers {
        let until = ranges.remove(until);
        let range = ranges.remove(range);

        let (tag, start, handle) = match range {
            TagRange::Bounded(tag, bounded, handle) => {
                let new_until = TagRange::Until(tag.inverse().unwrap(), ..bounded.end, handle);
                let (Ok(index) | Err(index)) = ranges.binary_search(&new_until);
                ranges.insert(index, new_until);

                (tag, bounded.start, handle)
            }
            TagRange::From(tag, from, handle) => (tag, from.start, handle),
            TagRange::Until(..) => unreachable!("We filtered out this type of range.")
        };

        let TagRange::Until(_, until, _) = until else {
            unreachable!("We filtered out all other types of range.");
        };

        if (start..until.end).count() >= min_to_keep {
            let range = TagRange::Bounded(tag, start..until.end, handle);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    }
}

pub struct Iter<'a> {
    width: usize,
    tags: &'a Tags,
    iter: container::Iter<'a, container::ForwardTags<'a>>
}

impl Iterator for Iter<'_> {
    type Item = (usize, TagOrSkip);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().inspect(|(width, _)| self.width = width)
    }

    fn advance_by(&mut self, n: usize) -> Result<(), std::num::NonZeroUsize> {
        if self.tags.width() >= self.width + n {
            self.iter = self.tags.iter_at(self.width + n);
            Ok(())
        } else {
            Err(self.tags.width() - (self.width + n))
        }
    }
}
