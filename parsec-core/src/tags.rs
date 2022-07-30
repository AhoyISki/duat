use std::{cmp::max, collections::btree_map::Range, str};

use bitflags::bitflags;
use crossterm::style::ContentStyle;
use regex::{bytes::{CaptureLocations, Match}, Regex};
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

    ///
    MlRangeLine(u16),

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
        const CAN_FOLD   = 1 << 6;
        /// Wether or not the line is folded.
        const IS_FOLDED  = 1 << 7;
        /// If the line is supposed to be replaced by another line when not hovered.
        const CONCEALED  = 1 << 8;
        /// If the line contains a pattern that acts as both the start and end of a multi-line form.
        const FORM_BOUND = 1 << 9;
        /// If the line is a line wise comment.
        const IS_COMMENT = 1 << 10;
        /// If the line is line wise documentation.
        const IS_DOC     = 1 << 11;
    }
}

// This type exists solely for the purpose of allowing more efficient insertion of elements
// according to the necessity of the program.
// Since all insertions (wrappings, syntax, cursors, etc) already come sorted, it makes sense to
// create efficient insertion algorithms that take that into account.
/// A vector of `CharTags`.
#[derive(Debug, Default)]
pub struct CharTags(Vec<(u32, CharTag)>);

impl CharTags {
    /// Creates a new instance of `CharTags`.
    pub fn new() -> CharTags {
        CharTags(Vec::new())
    }

    /// Creates a new instance of `CharTags`, given a slice of `CharTag`s.
    pub fn from(slice: &[(u32, CharTag)]) -> CharTags {
        CharTags(Vec::from(slice))
    }

    /// More efficient insertion method that requires no sorting.
    pub fn insert(&mut self, char_tag: (u32, CharTag)) {
        match self.0.iter().enumerate().find(|(_, (c, _))| *c > char_tag.0) {
            Some((pos, _)) => self.0.insert(pos, char_tag),
            None => self.0.push(char_tag),
        }
    }

    /// Given a sorted list of `CharTag`s, efficiently inserts them.
    pub fn insert_slice(&mut self, char_tags: &[(u32, CharTag)]) {
        // To prevent unnecessary allocations.
        self.0.reserve(char_tags.len());

        let mut new_char_tags = char_tags.iter();

        let mut insertions: SmallVec<[usize; 30]> = SmallVec::with_capacity(char_tags.len());

        'a: while let Some(&(col, _)) = new_char_tags.next() {
            // Iterate until we get a position with a column where we can place the char_tag.

            let mut old_char_tags = self.0.iter().enumerate();
            let index = match old_char_tags.find(|(_, &(p, _))| p > col) {
                Some((new_index, _)) => new_index,
                // If no more characters are found, we can just dump the rest at the end.
                None => break 'a,
            };

            // Can't mutate the vector in here.
            insertions.push(index as usize);
        }

        // Restarting the iterator.
        let mut new_char_tags = char_tags.iter();

        for (index, pos) in insertions.iter().enumerate() {
            // As I add char_tags,the positions need to be incremented.
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

    pub fn retain<F>(&mut self, do_retain: F)
    where
        F: Fn((u32, CharTag)) -> bool,
    {
        self.0.retain(|&(c, t)| do_retain((c, t)))
    }

    pub fn merge(&mut self, char_tags: CharTags) {
        self.insert_slice(char_tags.0.as_slice());
    }
}

// TODO: Eventually make these private, so only rune configuration can change them.
#[derive(Clone, Copy)]
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
#[derive(Debug)]
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
    fn find_iter<'a>(&'a self, text: &'a str) -> MatchIter {
        match self {
            Matcher::Regex(reg) => MatchIter::Regex(reg.find_iter(text)),
            Matcher::TsCapture(id) => {
                // TODO: This, obviously.
                MatchIter::TsCapture(std::iter::once((*id as usize, *id as usize)))
            }
        }
    }
}

enum MatchIter<'a> {
    Regex(regex::Matches<'a, 'a>),
    TsCapture(std::iter::Once<(usize, usize)>),
}

impl<'a> Iterator for MatchIter<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MatchIter::Regex(reg_iter) => reg_iter.next().map(|m| (m.start(), m.end())),
            // TODO: This, obviously.
            MatchIter::TsCapture(ts_iter) => ts_iter.next(),
        }
    }
}

// Patterns can occupy multiple lines only if they are defined as an opening and ending bound.
/// Indicates wether a pattern should be able to occupy multiple lines, and how.
#[derive(Debug, Default)]
pub enum Pattern {
    /// This pattern will automatically match the entire file, used for default pattern_lists.
    #[default]
    Standard,
    /// A simple pattern matcher.
    Word(Matcher),
    /// A bounded pattern matcher.
    ///
    /// Everything within the bounds matches, and is capable of matching exclusive patterns.
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
#[derive(Debug, Clone, Copy)]
struct LineBytePos {
    /// The line in the file.
    line: usize,
    /// The byte in relation to the line.
    byte: usize,
    /// The byte in relation to the beginning of the file.
    file_byte: usize,
}

/// A range of positions indexed by line byte, instead of column.
#[derive(Debug, Clone, Copy)]
struct LineByteRange {
    start: LineBytePos,
    end: LineBytePos,
}

struct LineByteRanges(Vec<LineByteRange>);

impl LineByteRanges {
    // NOTE: This assumes you are matchin in ascending order of position.
    /// "Pokes" a hole in the range.
    fn poke(&mut self, range: LineByteRange) {
        let last_ref = self.0.last_mut().unwrap();
        let last = *last_ref;

        last_ref.end = range.start;

        self.0.push(LineByteRange { start: range.end, end: last.end });
    }
}

/// Information about the line that isn't its text.
#[derive(Debug, Default)]
pub struct LineInfo {
    pub char_tags: CharTags,
    pub line_flags: LineFlags,
    pub start_pattern_id: u16,
    pub end_pattern_id: u16,
}

impl LineInfo {
    fn from(line: &TextLine) -> LineInfo {
        LineInfo {
            char_tags: CharTags::new(),
            line_flags: LineFlags::default(),
            start_pattern_id: line.info.start_pattern_id,
            end_pattern_id: line.info.end_pattern_id,
        }
    }
}

impl FormPattern {
    fn match_text(
        &self, lines: &[TextLine], ranges: &mut LineByteRanges, info: &mut [(LineInfo, usize)],
    ) -> SmallVec<[LineByteRange; 32]> {
        let mut ranges_to_poke = SmallVec::<[LineByteRange; 32]>::new();

        for form_pattern in &self.form_pattern_list {
            for (start, end) in ranges.0.iter().map(|r| (r.start, r.end)) {
                let mut file_byte = start.file_byte;

                for (index, line) in lines.iter().take(end.line + 1).skip(start.line).enumerate() {
                    let (text, line_start) = if lines.len() == 1 {
                        (&line.text().as_bytes()[start.byte..end.byte], start.byte)
                    } else if index == 0 {
                        (&line.text().as_bytes()[start.byte..], start.byte)
                    } else if index == end.line - start.line {
                        (&line.text().as_bytes()[..end.byte], 0)
                    } else {
                        (line.text().as_bytes(), 0)
                    };

                    let text = str::from_utf8(text).unwrap();

                    if let Pattern::Word(matcher) = &form_pattern.pattern {
                        let mut match_tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                        for range in matcher.find_iter(text) {
                            let start = LineBytePos {
                                line: info[index].1,
                                byte: range.0 + line_start,
                                file_byte: range.0 + file_byte,
                            };
                            let end = LineBytePos {
                                line: info[index].1,
                                byte: range.1 + line_start,
                                file_byte: range.1 + file_byte,
                            };
                            let range = LineByteRange { start, end };

                            // There may be many forms to a `FormPattern` only push the `Some`s.
                            for form in &form_pattern.form_indices {
                                if let &Some(form) = form {
                                    match_tags.push((start.byte as u32, CharTag::PushForm(form)));
                                }
                            }
                            // This looks dumb but it's correct.
                            for form in &form_pattern.form_indices {
                                if let &Some(form) = form {
                                    match_tags.push((end.byte as u32, CharTag::PopForm(form)));
                                }
                            }

                        	// If the pattern is exclusive, nothing should be allowed to match on
                            // its range.
                            if form_pattern.is_exclusive {
                                ranges_to_poke.push(range);
                            }

                            // Match subpatterns.
                            if form_pattern.form_pattern_list.len() > 0 {
                                let mut ranges = LineByteRanges(vec![range]);
                                let info = std::slice::from_mut(&mut info[index]);

                                // This inner vector represents all the places that were poked
                                // within the matched range.
                                let inner_pokes = form_pattern.match_text(lines, &mut ranges, info);

                                // If the pattern wasn't exclusive, maybe some of its innards were?
                                if !form_pattern.is_exclusive {
                                    ranges_to_poke.extend(inner_pokes);
                                }
                            }
                        }

                        info[index].0.char_tags.insert_slice(match_tags.as_slice());
                    }

                    file_byte += text.len();
                }
            }

			if ranges_to_poke.len() > 0 {
                for &range_to_poke in &ranges_to_poke {
                    ranges.poke(range_to_poke);
                }
                //panic!("{:#?}", self.form_pattern_list);
			}

            ranges_to_poke.clear();
        }

        ranges_to_poke
    }

    pub fn match_text_range(&self, lines: &[TextLine], range: TextRange) -> Vec<(LineInfo, usize)> {
        let (start, end) = (range.start, range.end);
        let mut lines_info: Vec<(LineInfo, usize)> =
            (range.lines()).map(|l| (LineInfo::from(&lines[l]), l)).collect();

        // The distance from the beginning of the real line.
        let from_zero = lines[start.line].get_line_byte_at(start.col);
        // No matter what the range is, the first iteration goes only through whole lines, since
        // we don't know if `Pattern::Word`s will be completed before the range's start.
        let start = LineBytePos { line: start.line, byte: 0, file_byte: start.byte - from_zero };

        let end_line_len = lines[end.line].text().len();
        // The distance to the end of the last line in the range.
        let to_end = end_line_len - lines[end.line].get_line_byte_at(end.col);
        let end = LineBytePos { line: end.line, byte: end_line_len, file_byte: end.byte + to_end };

        let mut ranges = LineByteRanges(vec![LineByteRange { start, end }]);

        // First, match according to what pattern_id was in the start of the line.
        let id = lines_info[0].0.start_pattern_id;
        if let (Some(form_pattern), true) = (self.search_for_id(id), id != self.id) {
            // If the pattern inherits from its parents, and the same is the case for its
            // decendants, we should get the same range back.
            // What this means is that we can pattern match from the default `FormPattern` only
            // where matches happened and `heir == true` for a given `FormPattern`.
            // NOTE: Currently this has the slight issue of `CharTag` duplication on nested ranges,
            // this should rarely be a problem, though.
            let to_poke = form_pattern.match_text(lines, &mut ranges, lines_info.as_mut_slice());
            for range in to_poke {
                ranges.poke(range);
            }
        }

        self.match_text(lines, &mut ranges, lines_info.as_mut_slice());

        lines_info
    }

    /// Returns a reference to the `FormPattern` with the given id.
    fn search_for_id(&self, id: u16) -> Option<&FormPattern> {
        for form_pattern in &self.form_pattern_list {
            if form_pattern.id == id {
                return Some(form_pattern);
            } else {
                if let Some(form_pattern) = form_pattern.search_for_id(id) {
                    return Some(form_pattern);
                }
            }
        }

        None
    }

    /// Returns a mutable reference to the `FormPattern` with the given id.
    fn search_for_id_mut(&mut self, id: u16) -> Option<&mut FormPattern> {
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

pub struct TagManager {
    /// The forms for syntax highlighting.
    forms: Vec<Form>,

    /// The patterns associated with said forms.
    default_form: FormPattern,

    // This exists solely for pushing new `FormPatterns` into the `default_form` `FormPattern` tree.
    /// 1 + the last id that was used for a `FormPattern`.
    last_id: u16,
}

impl TagManager {
    /// Returns a new instance of `TagManager`
    pub fn new() -> TagManager {
        TagManager { forms: Vec::new(), default_form: FormPattern::default(), last_id: 0 }
    }

    pub fn match_text_range(&self, lines: &[TextLine], range: TextRange) -> Vec<(LineInfo, usize)> {
        self.default_form.match_text_range(lines, range)
    }

    /// Pushes a new form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_word(&mut self, matcher: Matcher, form_index: Option<u16>, heir: bool) -> u16 {
        self.last_id += 1;

        let form_pattern = FormPattern {
            form_indices: vec![form_index],
            pattern: Pattern::Word(matcher),
            form_pattern_list: Vec::new(),
            id: self.last_id,
            is_exclusive: heir,
        };

        self.default_form.form_pattern_list.push(form_pattern);

        self.last_id
    }

    pub fn push_subword(
        &mut self, matcher: Matcher, form_index: Option<u16>, is_exclusive: bool, id: u16,
    ) {
        self.last_id += 1;

        let found = if let Some(found) = self.default_form.search_for_id_mut(id) {
            found
        } else {
            return;
        };

        let mut form_indices = found.form_indices.clone();
        form_indices.push(form_index);

        let form_pattern = FormPattern {
            form_indices,
            pattern: Pattern::Word(matcher),
            form_pattern_list: Vec::new(),
            id: self.last_id,
            is_exclusive,
        };

        found.form_pattern_list.push(form_pattern);
    }

    /// Pushes a new multiline form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_bounds(
        &mut self, bounds: [Matcher; 2], form_index: Option<u16>, heir: bool,
    ) -> u16 {
        // Move out of array and get rid of it without copying.
        let [start, end] = bounds;
        self.last_id += 1;

        let form_pattern = if start == end {
            FormPattern {
                form_indices: vec![form_index],
                pattern: Pattern::Bound(start),
                form_pattern_list: Vec::new(),
                id: self.last_id,
                is_exclusive: heir,
            }
        } else {
            FormPattern {
                form_indices: vec![form_index],
                pattern: Pattern::Bounds(start, end),
                form_pattern_list: Vec::new(),
                id: self.last_id,
                is_exclusive: heir,
            }
        };

        self.default_form.form_pattern_list.push(form_pattern);

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
