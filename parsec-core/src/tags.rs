use std::{str, cmp::max};

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
            Some((pos, (_, _))) => self.0.insert(pos, char_tag),
            None => self.0.push(char_tag),
        }
    }

    /// Given a sorted list of `CharTag`s, efficiently inserts them.
    pub fn insert_slice(&mut self, char_tags: &[(u32, CharTag)]) {
        // To prevent unnecessary allocations.
        self.0.reserve(char_tags.len());

        let mut new_char_tags = char_tags.iter();
        let mut old_char_tags = self.0.iter().enumerate();

        let mut insertions: SmallVec<[usize; 30]> = SmallVec::with_capacity(char_tags.len());

        let (mut index, mut pos) = (0, 0);

        'a: while let Some(&(col, _)) = new_char_tags.next() {
            // Iterate until we get a position with a column where we can place the char_tag.
            while col > pos {
                match old_char_tags.next() {
                    Some((new_index, &(new_pos, _))) => (index, pos) = (new_index, new_pos),
                    // If no more characters are found, we can just dump the rest at the end.
                    None => break 'a,
                }
            }
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
}

impl FormPattern {
    /// Recursively match a pattern on a piece of text.
    ///
    /// First, it will match itself in the entire segment, secondly, it will match any subpatterns
    /// on segments of itself.
    fn match_text(
        &self, lines: &[TextLine], range: LineByteRange, info: &mut [(LineInfo, usize)],
    ) -> LineBytePos {
        let (start, end) = (range.start, range.end);
        // last line is included.
        let lines_iter = lines.iter().enumerate().take(end.line + 1).skip(start.line);

        // This number is used specifically for tree-sitter, which deals in absolute bytes.
        // The use of bytes here, instead of columns on a line, allows me to completely ignore
        // where characters begin and end, and deal only in bytes.
        let mut file_byte = start.file_byte;
        // `line_number - range_start_line == the_correct_index_in_info`.
        let range_start_line = info[0].1;

        // The ranges that have been "split" from the main range, due to exclusive matching.
        let mut cutoff_ranges = Vec::new();

        // In theory, this should only be able to happen if the matching begins in the middle of a
        // multi-line range.
        let id = info[0].0.start_pattern_id;
        let start = if let (Some(form_pattern), true) = (self.search_for_id(id), id != self.id) {
            form_pattern.match_text(lines, range, info)
        } else {
            start
        };

        // This doesn't actually matter tbh. It's just convenient to return here, as the range is
        // empty anyways.
        if start.byte == end.byte {
            return end;
        }

        for (index, line) in lines_iter {
            //if start.line != 0 && self.id == 0 { panic!() }
            let info_index = index - range_start_line;

            // If the range consists of only a single line, only check the byte range.
            let (line_start, line_end) = if range.start.line == range.end.line {
                (start.byte, end.byte)
            // Otherwise, check from the line start, which is only different in the first line.
            } else if index == range.start.line {
                (start.byte, line.text().len())
            } else if index == range.end.line {
                (0, end.byte)
            } else {
                (0, line.text().len())
            };

            if line_start == 0 && info_index > 0 {
                info[info_index].0.start_pattern_id = info[info_index - 1].0.end_pattern_id;
            } else if index == 0 {
                info[info_index].0.start_pattern_id = 0;
            }

            if let (Pattern::Bounds(_, _), 0) = (&self.pattern, line_start) {
                let line_info = &mut info[info_index].0;

                for form_index in &self.form_indices {
                    if let &Some(form_index) = form_index {
                        let form_start = CharTag::PushForm(form_index);
                        line_info.char_tags.insert((0, form_start));
                    }
                }
            }

            let mut text = str::from_utf8(&line.text().as_bytes()[line_start..line_end]).unwrap();

            let new_end = if let Pattern::Bounds(_, end) = &self.pattern {
                match end.find_iter(text).next() {
                    Some((_, end_byte)) => {
                        let end_pos = LineBytePos {
                            line: index,
                            byte: end_byte + line_start,
                            file_byte: file_byte + end_byte,
                        };

                        let char_tags = &mut info[info_index].0.char_tags;

                        for form_index in &self.form_indices {
                            if let &Some(form_index) = form_index {
                                let form_start = CharTag::PopForm(form_index);
                                char_tags.insert((end_pos.byte as u32, form_start));
                            }
                        }

                        Some(end_pos)
                    }
                    None => None,
                }
            } else {
                None
            };

            if let Some(new_end) = new_end {
                text = str::from_utf8(&line.text().as_bytes()[line_start..new_end.byte]).unwrap();
            } else {
                let end_pattern_id = info[info_index].0.end_pattern_id;
                info[info_index].0.end_pattern_id = max(self.id, end_pattern_id);
            }

            for form_pattern in &self.form_pattern_list {
                // In this case, we know it'll only match a single line.
                if let Pattern::Word(Matcher::Regex(reg)) = &form_pattern.pattern {
                    let mut pattern_char_tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                    for range in reg.find_iter(text) {
                        // Adding the form changes to `char_tags`.
                        if let Some(&Some(form_index)) = form_pattern.form_indices.last() {
                            let form_start = CharTag::PushForm(form_index);
                            let form_end = CharTag::PopForm(form_index);
                            pattern_char_tags.extend_from_slice(&[
                                ((line_start + range.start()) as u32, form_start),
                                ((line_start + range.end()) as u32, form_end),
                            ]);
                        }

                        // If there are patterns that can match within this one, do a recursion.
                        if form_pattern.form_pattern_list.len() > 0 {
                            // Here, we'll simply feed the matche's range into a list of ranges that
                            // should be checked, along with the patterns to check the range with.
                            let match_range = LineByteRange {
                                start: LineBytePos {
                                    line: index,
                                    byte: range.start() + line_start,
                                    file_byte: range.start() + file_byte,
                                },
                                end: LineBytePos {
                                    line: index,
                                    byte: range.end() + line_start,
                                    file_byte: range.end() + file_byte,
                                },
                            };

                            cutoff_ranges.push((match_range, &form_pattern.form_pattern_list));
                        }
                    }
                    let char_tags = &mut info[info_index].0.char_tags;
                    char_tags.insert_slice(pattern_char_tags.as_slice());
                } else if let Pattern::Bounds(start_match, _) = &form_pattern.pattern {
                    // Iterators over matches of the start and end patterns.
                    // NOTE: While each iterator will be ordered, an end pattern will not always
                    // show up after a start pattern, leaving "dangling bounds".
                    let mut start_iter = start_match.find_iter(text);

                    // Vectors for more efficient insertion of elements into `char_tags`.
                    let mut start_tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                    while let Some(range) = start_iter.next() {
                        // The range to check starts at 0 from the start of the line, which in
                        // the case of the first line of the original range, will not necessarily
                        // be 0.
                        let start = (range.0 + line_start) as u32;

                        if let Some(&Some(form_index)) = form_pattern.form_indices.last() {
                            start_tags.push((start, CharTag::PushForm(form_index)));
                        }

                        if form_pattern.form_pattern_list.len() > 0 {
                            let range = LineByteRange {
                                start: LineBytePos {
                                    line: index,
                                    byte: range.0 + line_start,
                                    file_byte: range.0 + file_byte,
                                },
                                end,
                            };
                            let _new_end = form_pattern.match_text(lines, range, info);
                        }
                    }

                    // At this point, it is garanteed that the matched ranges will be sorted, and it
                    // is more efficient to insert them all at once.
                    let char_tags = &mut info[info_index].0.char_tags;
                    char_tags.insert_slice(start_tags.as_slice());
                }
            }

            file_byte += text.len();

            if let Some(new_end) = new_end {
                return new_end;
            } 
        }

        range.end
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

        let range = LineByteRange { start, end };

        self.match_text(lines, range, lines_info.as_mut_slice());

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
    pub fn push_word(&mut self, matcher: Matcher, form_index: Option<u16>) -> u16 {
        self.last_id += 1;

        let form_pattern = FormPattern {
            form_indices: vec![form_index],
            pattern: Pattern::Word(matcher),
            form_pattern_list: Vec::new(),
            id: self.last_id,
        };

        self.default_form.form_pattern_list.push(form_pattern);

        self.default_form.form_pattern_list.last_mut().unwrap().id
    }

    pub fn push_subword(&mut self, matcher: Matcher, form_index: Option<u16>, id: u16) {
        self.last_id += 1;

        let found = if let Some(found_form_pattern) = self.default_form.search_for_id_mut(id) {
            found_form_pattern
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
        };

        if let Some(found_form_pattern) = self.default_form.search_for_id_mut(id) {
            found_form_pattern.form_pattern_list.push(form_pattern);
        }
    }

    /// Pushes a new multiline form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_bounds(&mut self, bounds: [Matcher; 2], form_index: Option<u16>) -> u16 {
        // Move out of array and get rid of it without copying.
        let [start, end] = bounds;
        self.last_id += 1;

        let form_pattern = if start == end {
            FormPattern {
                form_indices: vec![form_index],
                pattern: Pattern::Bound(start),
                form_pattern_list: Vec::new(),
                id: self.last_id,
            }
        } else {
            FormPattern {
                form_indices: vec![form_index],
                pattern: Pattern::Bounds(start, end),
                form_pattern_list: Vec::new(),
                id: self.last_id,
            }
        };

        self.default_form.form_pattern_list.push(form_pattern);

        self.default_form.form_pattern_list.last().unwrap().id
    }

    /// Pushes a new form onto the list.
    pub fn push_form(&mut self, style: ContentStyle, is_final: bool) {
        self.forms.push(Form { style, is_final });
    }

    pub fn forms(&self) -> &[Form] {
        self.forms.as_ref()
    }
}
