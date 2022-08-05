use std::{cmp::max, str};

use bitflags::bitflags;
use crossterm::style::ContentStyle;
use regex::Regex;
use smallvec::SmallVec;

use crate::{action::TextRange, file::TextLine};

// NOTE: Unlike cursor and file positions, character tags are byte indexed, not character indexed.
// The reason is that modules like `regex` and `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone, Copy, Debug)]
pub enum CharTag {
    // NOTE: Terribly concocted scenarios could partially break forms identifiers.
    // Implemented:
    /// Appends a form to the stack.
    PushForm(u16),
    /// Removes a form from the stack. It won't always be the last one.
    PopForm(u16),

    /// Wraps *before* printing the character, not after.
    WrapppingChar,

    // Partially implemented:
    /// Wether the primary cursor is in this character or not.
    PrimaryCursor,

    // Not Implemented:
    /// Wether a secondary cursor is in this character or not.
    SecondaryCursor,
    /// Begins or ends a hoverable section in the file.
    HoverBound,
    /// Conceals a character with a string of text of equal lenght, permanently.
    PermanentConceal { index: u16 },
}

bitflags! {
    /// A memory and processor efficient way of keeping track of information about a line.
    #[derive(Default)]
    pub struct LineFlags: u16 {
        // Implemented:
        /// If all characters in the line are ascii.
        const PURE_ASCII = 1 << 0;
        /// If there are no double/zero width characters or tabs.
        const PURE_1_COL = 1 << 1;

        // Not Implemented:
        // Wether or not the line can fold.
        const CAN_FOLD   = 1 << 4;
        /// Wether or not the line is folded.
        const IS_FOLDED  = 1 << 5;
        /// If the line is supposed to be replaced by another line when not hovered.
        const CONCEALED  = 1 << 6;
        /// If the line is a line wise comment.
        const IS_COMMENT = 1 << 7;
        /// If the line is line wise documentation.
        const IS_DOC     = 1 << 8;
    }
}

// This type exists solely for the purpose of allowing more efficient insertion of elements
// according to the necessity of the program.
// Since all insertions (wrappings, syntax, cursors, etc) already come sorted, it makes sense to
// create efficient insertion algorithms that take that into account.
/// A vector of `CharTags`.
#[derive(Clone, Debug, Default)]
pub struct CharTags(Vec<(u32, CharTag)>);

impl CharTags {
    /// Creates a new instance of `CharTags`.
    pub fn new() -> CharTags {
        CharTags(Vec::new())
    }

    /// More efficient insertion method that requires no sorting.
    pub fn insert(&mut self, char_tag: (u32, CharTag)) {
        // It just finds the first element with a bigger number and puts the `char_tag` behind it.
        match self.0.iter().enumerate().find(|(_, (c, _))| *c > char_tag.0) {
            Some((pos, _)) => self.0.insert(pos, char_tag),
            None => self.0.push(char_tag),
        }
    }

    /// Given a sorted list of `CharTag`s, efficiently inserts them.
    pub fn insert_slice(&mut self, char_tags: &[(u32, CharTag)]) {
        self.0.reserve(char_tags.len());
        let mut new_char_tags = char_tags.iter();

        // In what positions each `CharTag` needs to be placed.
        let mut insertions: SmallVec<[usize; 30]> = SmallVec::with_capacity(char_tags.len());

        'a: while let Some(&(col, _)) = new_char_tags.next() {
            // Iterate until we get a position with a column where we can place the char_tag.
            let mut old_char_tags = self.0.iter().enumerate();
            let index = match old_char_tags.find(|(_, &(p, _))| p > col) {
                Some((new_index, _)) => new_index,
                // If no more characters are found, we can just dump the rest at the end.
                None => break 'a,
            };

            insertions.push(index as usize);
        }

        // Iterating again, but, this time, inserting each `CharTag` in its calculated position.
        let mut new_char_tags = char_tags.iter();
        for (index, pos) in insertions.iter().enumerate() {
            // If we insert at `pos`, all future `pos`s will be shifted by 1, thus `pos + index`.
            self.0.insert(pos + index, *new_char_tags.next().unwrap());
        }

        // The remaining characters here had positions bigger than any original `CharTag`.
        // As such, since they are sorted, we can just dump them at the end.
        self.0.extend(new_char_tags);
    }

    /// Returns an immutable reference to the vector.
    pub fn vec(&self) -> &Vec<(u32, CharTag)> {
        &self.0
    }

    /// The same as a regular `Vec::retain`, but we only care about what `CharTag` it is.
    pub fn retain<F>(&mut self, do_retain: F)
    where
        F: Fn((u32, CharTag)) -> bool,
    {
        self.0.retain(|&(c, t)| do_retain((c, t)))
    }
}

// TODO: Eventually make these private, so they can only be changed on the configuration.
#[derive(Clone, Copy)]
/// A style for text.
pub struct Form {
    /// The `Form`'s colors and attributes.
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should override any that come after.
    pub is_final: bool,
}

impl Form {
    /// Creates a new instance of `Form`.
    pub fn new(style: ContentStyle, is_final: bool) -> Form {
        Form { style, is_final }
    }
}

/// A matcher primarily for syntax highlighting.
#[derive(Clone, Debug)]
pub enum Matcher {
    /// A regular expression.
    Regex(Regex),
    /// A tree-sitter capture.
    TsCapture(usize),
}

impl PartialEq for Matcher {
    fn ne(&self, other: &Self) -> bool {
        match (self, other) {
            (Matcher::Regex(reg_1), Matcher::Regex(reg_2)) => reg_1.as_str() != reg_2.as_str(),
            (Matcher::TsCapture(id_1), Matcher::TsCapture(id_2)) => id_1 != id_2,
            _ => false,
        }
    }

    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Matcher::Regex(reg_1), Matcher::Regex(reg_2)) => reg_1.as_str() == reg_2.as_str(),
            (Matcher::TsCapture(id_1), Matcher::TsCapture(id_2)) => id_1 == id_2,
            _ => false,
        }
    }
}

impl Matcher {
    /// Returns an iterator over the matches on a string.
    fn match_iter<'a>(&'a self, text: &'a str, range: LineByteRange) -> MatchIter {
        match self {
            Matcher::Regex(reg) => MatchIter::Regex((reg.find_iter(text), range)),
            Matcher::TsCapture(id) => {
                // TODO: This, obviously.
                MatchIter::TsCapture((std::iter::once((*id as usize, *id as usize)), range))
            }
        }
    }
}

/// An enumerator specifically made for iterating through multiple types of matches in a string.
enum MatchIter<'a> {
    Regex((regex::Matches<'a, 'a>, LineByteRange)),
    TsCapture((std::iter::Once<(usize, usize)>, LineByteRange)),
}

impl<'a> Iterator for MatchIter<'a> {
    type Item = LineByteRange;

    /// Returns an `Option<LineByteRange>` for the next element in the iterator.
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MatchIter::Regex((regs, range)) => {
                let start = range.start;
                let (line, byte, file_byte) = (start.line, start.byte, start.file_byte);

                regs.next().map(|r| {
                    let (file_start, file_end) = (file_byte + r.start(), file_byte + r.end());
                    LineByteRange {
                        start: LineBytePos { line, byte: byte + r.start(), file_byte: file_start },
                        end: LineBytePos { line, byte: byte + r.end(), file_byte: file_end },
                    }
                })
            }
            // TODO: This, obviously.
            MatchIter::TsCapture(_ts_iter) => todo!(),
        }
    }
}

// Patterns can occupy multiple lines only if they are defined as an opening and ending bound.
/// Indicates wether a pattern should be able to occupy multiple lines, and how.
#[derive(Clone, Debug, Default)]
pub enum Pattern {
    /// This pattern will automatically match the entire file, used for default pattern_lists.
    #[default]
    Standard,
    /// A simple pattern matcher.
    Word(Matcher),
    /// A bounded pattern matcher.
    Bound(Matcher),
    /// A bounded pattern matcher, with different patterns at each end.
    Bounds(Matcher, Matcher),
}

/// An assossiation of a pattern with a spcecific form.
#[derive(Debug, Default)]
pub struct FormPattern {
    /// The index of the form assossiated with this pattern.
    form_indices: Vec<Option<u16>>,
    /// What defines what the range will be.
    pattern: Pattern,
    /// Patterns that can match inside of the original match range.
    form_pattern_list: Vec<FormPattern>,
    /// A unique identifier for this specific `FormPattern`
    id: u16,
    /// Wether or not this `FormPattern` should allow parent patterns to match in it.
    is_exclusive: bool,
}

// The next two structs are useful in the context of syntax highlighting, since the line's byte is
// useful for regex calculation, while the file's byte is useful for tree-sitter capture, while the
// column (char index) of the line is completely useless.
/// A position indexed by its byte in relation to the line, instead of column.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LineBytePos {
    /// The line in the file.
    line: usize,
    /// The byte in relation to the line.
    byte: usize,
    /// The byte in relation to the beginning of the file.
    file_byte: usize,
}

impl PartialOrd for LineBytePos {
    fn gt(&self, other: &Self) -> bool {
        self.file_byte > other.file_byte
    }

    fn ge(&self, other: &Self) -> bool {
        self.file_byte >= other.file_byte
    }

    fn lt(&self, other: &Self) -> bool {
        self.file_byte < other.file_byte
    }

    fn le(&self, other: &Self) -> bool {
        self.file_byte <= other.file_byte
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self > other {
            Some(std::cmp::Ordering::Greater)
        } else if self < other {
            Some(std::cmp::Ordering::Less)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

impl Ord for LineBytePos {
    fn clamp(self, min: Self, max: Self) -> Self
    where
        Self: Sized,
    {
        if self < min {
            min
        } else if self > max {
            max
        } else {
            self
        }
    }

    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self > other {
            std::cmp::Ordering::Greater
        } else if self < other {
            std::cmp::Ordering::Less
        } else {
            std::cmp::Ordering::Equal
        }
    }

    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        if self > other {
            self
        } else {
            other
        }
    }

    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        if self < other {
            self
        } else {
            other
        }
    }
}

/// A range of positions indexed by line byte, instead of column.
#[derive(Debug, Clone, Copy)]
struct LineByteRange {
    start: LineBytePos,
    end: LineBytePos,
}

/// A list of ranges, specifically made with "poking" in mind.
#[derive(Debug)]
struct LineByteRanges(Vec<LineByteRange>);

impl LineByteRanges {
    // NOTE: This assumes you are matchin in ascending order of position.
    /// "Pokes" a hole in the range.
    fn poke(&mut self, poke: LineByteRange) {
        // Find the range that contains the first point, in order to start poking.
        if let Some((index, &range)) =
            self.0.iter().enumerate().find(|(_, r)| r.end.file_byte > poke.start.file_byte)
        {
            // The first range after having its end removed by the poke.
            let first_cut_range = LineByteRange { start: range.start, end: poke.start };

            let mut ranges_iter = self.0.iter().enumerate().skip(index);

            // Look for a range that can fit the end of the poke, and remove all ranges in between.
            while let Some((last_checked, &range)) = ranges_iter.next() {
                if range.end.file_byte >= poke.end.file_byte {
                    let last_cut_range = LineByteRange { start: poke.end, ..range };

                    self.0.splice(index..=last_checked, [first_cut_range, last_cut_range]);

                    break;
                }
            }
        }
    }
}

/// Information about the line that isn't its text.
#[derive(Clone, Debug, Default)]
pub struct LineInfo {
    pub char_tags: CharTags,
    pub line_flags: LineFlags,
    starting_id: u16,
    ending_id: u16,
}

impl FormPattern {
    /// Matches a given pattern and its subpatterns on a range of text.
    fn match_text(
        &self, lines: &[TextLine], ranges: &mut LineByteRanges, info: &mut [(LineInfo, usize)],
        end_ranges: &[(LineBytePos, u16, usize)],
    ) -> (SmallVec<[LineByteRange; 32]>, Vec<(LineBytePos, u16, usize)>) {
        // The finalized vector with all ranges that were occupied by exclusive patterns.
        let mut poked_ranges = SmallVec::<[LineByteRange; 32]>::new();
        // The finalized list of multi-line ranges that weren't closed off.
        let mut new_end_ranges = Vec::new();

        // This line is used for referencing `info`.
        let first_line = ranges.0[0].start.line;

        for form_pattern in &self.form_pattern_list {
            // This vector is temporary so that we don't poke the same ranges multiple times.
            let mut ranges_to_poke = SmallVec::<[LineByteRange; 32]>::new();
            // Same as `ending_ranges`, but for this specific `FormPattern`.
            let mut pattern_end_ranges = Vec::new();

            for (start, end) in ranges.0.iter().map(|r| (r.start, r.end)) {
                // Clear at the start, unless we're in the last range.
                pattern_end_ranges.clear();
                let file_byte = start.file_byte;

                // An iterator over the lines in the range, it returns the truncated text of the
                // line, the line's number, and a `LineByteRange`, where the line is in the file.
                let lines_iter = lines
                    .iter()
                    .enumerate()
                    .take(end.line + 1)
                    .skip(start.line)
                    .map(|(n, l)| {
                        // If there's just one line, the range is truncated differently.
                        if start.line == end.line {
                            (n, &l.text().as_bytes()[start.byte..end.byte], start.byte)
                        } else if n == start.line {
                            (n, &l.text().as_bytes()[start.byte..], start.byte)
                        } else if n == end.line {
                            (n, &l.text().as_bytes()[..end.byte], 0)
                        } else {
                            (n, l.text().as_bytes(), 0)
                        }
                    })
                    .scan(file_byte, |acc, (n, t, s)| {
                        *acc += t.len();
                        Some((
                            n,
                            str::from_utf8(t).unwrap(),
                            LineByteRange {
                                start: LineBytePos { line: n, byte: s, file_byte: *acc - t.len() },
                                end: LineBytePos { line: n, byte: s + t.len(), file_byte: *acc },
                            },
                        ))
                    });

                // Matching on single line words.
                if let Pattern::Word(matcher) = &form_pattern.pattern {
                    for (index, text, range) in lines_iter {
                        let mut tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                        for range in matcher.match_iter(text, range) {
                            // The count is here to ensure that the order of elements remains
                            // consistent.
                            for form in form_pattern.form_indices.iter() {
                                if let &Some(form) = form {
                                    tags.push((range.start.byte as u32, CharTag::PushForm(form)));
                                }
                            }
                            for form in form_pattern.form_indices.iter() {
                                if let &Some(form) = form {
                                    tags.push((range.end.byte as u32, CharTag::PopForm(form)));
                                }
                            }

                            // If the pattern is exclusive, nothing should be allowed to match on
                            // its range.
                            // Match subpatterns.
                            let mut ranges = LineByteRanges(vec![range]);
                            let info = std::slice::from_mut(&mut info[index - start.line]);

                            // This inner vector represents all the places that were poked
                            // within the matched range.
                            let pokes = form_pattern.match_text(lines, &mut ranges, info, &[]).0;

                            // If the pattern wasn't exclusive, maybe some of its innards were?
                            if form_pattern.is_exclusive {
                                ranges_to_poke.push(range)
                            } else {
                                ranges_to_poke.extend(pokes);
                            }
                        }

                        info[index - first_line].0.char_tags.insert_slice(tags.as_slice());
                    }
                // Matching on multi-line ranges with different bounds.
                } else if let Pattern::Bounds(start_matcher, end_matcher) = &form_pattern.pattern {
                    // The finalized list of ranges where the bounds have matched.
                    let mut matched_ranges = SmallVec::<[LineByteRange; 10]>::new();
                    let mut starts = SmallVec::<[LineBytePos; 32]>::new();
                    let mut ends = SmallVec::<[LineBytePos; 32]>::new();

                    // Collect every match inside the range.
                    for (_, text, range) in lines_iter {
                        starts.extend(start_matcher.match_iter(text, range).map(|r| r.start));
                        ends.extend(end_matcher.match_iter(text, range).map(|r| r.end));
                    }

                    let mut start_iter = starts.iter();
                    let (mut current_start, mut inner_count) =
                        // If `ending_ranges[0].id` matches, this means that the line must start
                        // with this `FormPattern` and with the given `inner_count`.
                        // In any other case, there is no default starting match.
                        if let Some(pattern) = end_ranges.get(0) {
                            if pattern.1 == form_pattern.id {
                                (
                                    end_ranges.get(0).map(|&(p, _, _)| p),
                                    end_ranges.get(0).map(|&(_, _, c)| c).unwrap_or(0),
                                )
                            } else {
                                (None, 0)
                            }
                        } else {
                            (None, 0)
                        };

                    let mut next_start = None;
                    let mut iter_starts = true;

                    for end in ends {
                        if let Some(start) = next_start {
                            if end < start {
                                iter_starts = false;
                            // If we found an ending match after `next_start` we need to start a
                            // new range.
                            } else {
                                // Only update `current_start` if we have finished a range.
                                if inner_count == 0 {
                                    current_start = next_start
                                }
                                next_start = None;

                                iter_starts = true;
                                inner_count += 1;
                            }
                        }

                        if iter_starts {
                            // Iterate until we find a starting match that comes after the ending
                            // match. That might possibly be a completed range.
                            while let Some(&start) = start_iter.next() {
                                if start < end {
                                    current_start = current_start.or(Some(start));

                                    inner_count += 1;
                                } else {
                                    next_start = Some(start);

                                    break;
                                }
                            }
                        }

                        // A negative `inner_count` would match inverted ranges.
                        inner_count = inner_count.saturating_sub(1);

                        if let (Some(start), true) = (current_start, inner_count == 0) {
                            matched_ranges.push(LineByteRange { start, end });

                            current_start = None;
                        }
                    }

                    // Check for any possible remaining start matches.
                    current_start = current_start.or(next_start.or(start_iter.next().copied()));

                    // If we find any, match until the end of the range.
                    if let Some(start) = current_start {
                        matched_ranges.push(LineByteRange { start, end });
                        // If we're at the end of a line, the next lines must also match the range.
                        if end.byte == lines[end.line].text().len() {
                            info[end.line - first_line].0.ending_id = form_pattern.id;

                            pattern_end_ranges.push((end, form_pattern.id, 1 + start_iter.count()))
                        }
                    }

                    for range in matched_ranges {
                        for line in range.start.line..=range.end.line {
                            let mut tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                            // The start is the starting byte in the first line, 0 otherwise.
                            let start = if line == range.start.line {
                                range.start.byte
                            } else {
                                // On lines other than the first, the line starts with the id.
                                info[line - first_line].0.starting_id = form_pattern.id;
                                0
                            };

                            // The form of the range is applied on every line.
                            for form in form_pattern.form_indices.iter() {
                                if let &Some(form) = form {
                                    tags.push((start as u32, CharTag::PushForm(form)));
                                }
                            }

                            // Popping the form only really needs to be done at the end.
                            if line == range.end.line {
                                for form in form_pattern.form_indices.iter() {
                                    if let &Some(form) = form {
                                        tags.push((range.end.byte as u32, CharTag::PopForm(form)));
                                    }
                                }
                            } else {
                                // On lines other than the last, the line ends with the id.
                                info[line - first_line].0.ending_id = form_pattern.id;
                            }

                            info[line - first_line].0.char_tags.insert_slice(tags.as_slice());
                        }

                        let line_range =
                            (range.start.line - first_line)..=(range.end.line - first_line);
                        let info = &mut info[line_range];

                        let pattern_slice = &new_end_ranges.get(1..).unwrap_or(&[]);

                        // Apply all subpatterns on all matched ranges.
                        let mut ranges = LineByteRanges(vec![range]);
                        let (pokes, end_range) =
                            form_pattern.match_text(lines, &mut ranges, info, pattern_slice);

                        if let Some(pattern) = end_ranges.get(0) {
                            if pattern.1 == form_pattern.id {
                                // In theory, this only returns a non empty range if there were
                                // unfinished multi-line ranges at the end of the last range.
                                pattern_end_ranges.extend(end_range);
                            }
                        }

                        // If the pattern wasn't exclusive, maybe some of its innards were?
                        if form_pattern.is_exclusive {
                            ranges_to_poke.push(range)
                        } else {
                            ranges_to_poke.extend(pokes);
                        }
                    }
                }
            }

            for &range_to_poke in &ranges_to_poke {
                ranges.poke(range_to_poke);
            }

            poked_ranges.extend(ranges_to_poke);

            if let Some(last_pattern) = pattern_end_ranges.last() {
                if last_pattern.0.line == info.last().unwrap().1 {
                    // `end_ranges` can only contain one position, so we move `pattern_end_ranges`.
                    new_end_ranges = pattern_end_ranges;
                }
            }
        }

        (poked_ranges, new_end_ranges)
    }

    fn color_text(
        &self, lines: &[TextLine], range: LineByteRange,
        last_pattern: &[(LineBytePos, u16, usize)], max_line: usize,
    ) -> (Vec<(LineInfo, usize)>, Vec<(LineBytePos, u16, usize)>) {
        let (start, end) = (range.start, range.end);

        let mut lines_info: Vec<(LineInfo, usize)> =
            (start.line..=end.line).map(|l| (lines[l].info.clone(), l)).collect();

        let mut ranges = LineByteRanges(vec![range]);

        // First, match according to what pattern_id was in the start of the line.
        let pattern = self.match_text(lines, &mut ranges, &mut lines_info, last_pattern).1;

        if unsafe { crate::FOR_TEST } {
            panic!("{:#?}", pattern)
        }

        (lines_info, pattern)
    }

    /// Returns a mutable reference to the `FormPattern` with the given id.
    fn search_for_id_mut(&mut self, id: u16) -> Option<&mut FormPattern> {
        if self.id == id {
            return Some(self);
        }

        for form_pattern in &mut self.form_pattern_list {
            if form_pattern.id == id {
                return Some(form_pattern);
            } else {
                if let Some(form_pattern) = form_pattern.search_for_id_mut(id) {
                    return Some(form_pattern);
                }
            }
        }

        None
    }
}

/// Information about how to match lines beyond the last line that was matched.
struct LastPattern {
    /// The list of patterns that were last active and the number of recursions of each.
    patterns: Vec<(u16, usize)>,

    /// The last line that was checked.
    pos: LineBytePos,
}

pub struct TagManager {
    /// The forms for syntax highlighting.
    forms: Vec<Form>,

    // NOTE: This exists pretty much only for performance reasons. If you splice text that has no
    // pattern bounds in it, then you don't need to update the syntax highlighting of other lines,
    // (at least, not for that reason).
    /// The list of all patterns that are bounded.
    bounded_forms: Vec<Pattern>,

    /// The patterns associated with said forms.
    default_form: FormPattern,

    // This exists solely for pushing new `FormPatterns` into the `default_form` `FormPattern` tree.
    /// The last id that was used for a `FormPattern`.
    last_id: u16,

    /// What pattern ids were used when last matching.
    last_pattern: LastPattern,
}

impl TagManager {
    /// Returns a new instance of `TagManager`
    pub fn new() -> TagManager {
        TagManager {
            forms: Vec::new(),
            bounded_forms: Vec::new(),
            default_form: FormPattern::default(),
            last_id: 0,
            last_pattern: LastPattern {
                patterns: Vec::new(),
                pos: LineBytePos { line: 0, byte: 0, file_byte: 0 },
            },
        }
    }

    pub fn match_text_range(
        &mut self, lines: &[TextLine], range: TextRange, max_line: usize,
    ) -> Vec<(LineInfo, usize)> {
        let (start_pos, end_pos) = (range.start, range.end);
        let (start_line, end_line) = (&lines[start_pos.line], &lines[end_pos.line]);

        let mut lines_iter = lines.iter().take(range.start.line).rev();

        let mut start = LineBytePos {
            line: range.start.line,
            byte: 0,
            file_byte: range.start.byte - start_line.get_line_byte_at(start_pos.col),
        };
        while let Some(line) = lines_iter.next() {
            if line.info.ending_id != 0 || start > self.last_pattern.pos {
                start.line -= 1;
                start.file_byte -= line.text().len();
            } else {
                break;
            }
        }

        let mut lines_iter = lines.iter().skip(end_pos.line + 1);

        let mut end = LineBytePos {
            line: end_pos.line,
            byte: end_line.text().len(),
            file_byte: end_pos.byte + end_line.text().len()
                - end_line.get_line_byte_at(end_pos.col),
        };
        while let Some(line) = lines_iter.next() {
            if line.info.starting_id != 0 {
                end.line += 1;
                end.byte = line.text().len();
                end.file_byte += line.text().len();
            } else {
                break;
            }
        }

        let range = LineByteRange { start, end };

        let last_pattern: Vec<(LineBytePos, u16, usize)> = if self.last_pattern.pos < start {
            self.last_pattern.patterns.iter().map(|&(i, n)| (self.last_pattern.pos, i, n)).collect()
        } else {
            Vec::new()
        };

        let (info, last_pattern) =
            self.default_form.color_text(lines, range, &last_pattern, max_line);

        if let Some(last_pattern) = last_pattern.last() {
            self.last_pattern.pos = last_pattern.0;
        }
        self.last_pattern.patterns = last_pattern.iter().map(|&(_, i, n)| (i, n)).collect();

        info
    }

    /// Pushes a new form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_word(
        &mut self, matcher: Matcher, form_index: Option<u16>, is_exclusive: bool, id: u16,
    ) -> u16 {
        self.last_id += 1;

        let found_form_pattern = match self.default_form.search_for_id_mut(id) {
            Some(found_form_pattern) => found_form_pattern,
            None => todo!(),
        };

        let mut form_indices = found_form_pattern.form_indices.clone();
        form_indices.push(form_index);

        let form_pattern = FormPattern {
            form_indices: vec![form_index],
            pattern: Pattern::Word(matcher),
            form_pattern_list: Vec::new(),
            id: self.last_id,
            is_exclusive,
        };

        found_form_pattern.form_pattern_list.push(form_pattern);

        self.last_id
    }

    /// Pushes a new multiline form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_bounds(
        &mut self, bounds: [Matcher; 2], form_index: Option<u16>, is_exclusive: bool, id: u16,
    ) -> u16 {
        // Move out of array and get rid of it without copying.
        let [start, end] = bounds;
        self.last_id += 1;

        let found_form_pattern = match self.default_form.search_for_id_mut(id) {
            Some(found_form_pattern) => found_form_pattern,
            None => todo!(),
        };

        let mut form_indices = found_form_pattern.form_indices.clone();
        form_indices.push(form_index);

        let pattern =
            if start == end { Pattern::Bound(start) } else { Pattern::Bounds(start, end) };

        self.bounded_forms.push(pattern.clone());

        let form_pattern = FormPattern {
            form_indices: vec![form_index],
            pattern,
            form_pattern_list: Vec::new(),
            id: self.last_id,
            is_exclusive,
        };

        found_form_pattern.form_pattern_list.push(form_pattern);

        self.last_id
    }

    /// Pushes a new form onto the list.
    pub fn push_form(&mut self, style: ContentStyle, is_final: bool) {
        self.forms.push(Form { style, is_final });
    }

    pub fn forms(&self) -> &[Form] {
        self.forms.as_ref()
    }
}
