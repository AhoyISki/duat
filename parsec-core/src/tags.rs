use std::{
    cell::{Ref, RefCell, RefMut},
    ops::{Range, RangeFrom, RangeInclusive, RangeToInclusive},
    rc::{Rc, Weak},
    str,
};

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

    /// Appends a multi-line form to the stack.
    PushMlForm(u16),
    /// Removes a multi-line form from the stack. It won't always be the last one.
    PopMlForm(u16),
    /// Appends/removes a multi-line form from the stack, depending on if it was there or not.
    BoundMlForm(u16),

    /// Wraps *before* printing the character, not after.
    WrapppingChar,

    // Partially implemented:
    /// Wether the primary cursor is in this character or not
    PrimaryCursor,

    // Not Implemented:
    /// Wether a secondary cursor is in this character or not
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
        /// If the line is the beggining of a multi-line range.
        const ML_BEGIN   = 1 << 2;
        /// If the line is the end of multi-line range.
        const ML_END     = 1 << 3;
        /// If the line is in the middle of a multi-line range.
        const ML_RANGE   = 1 << 4;
        /// If the line works as either a start or an end to a multi-line range.
        const ML_BOUND   = 1 << 5;

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

// Patterns can occupy multiple lines only if they are defined as an opening and ending bound.
/// Indicates wether a pattern should be able to occupy multiple lines, and how.
#[derive(Debug)]
pub enum Pattern {
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
#[derive(Debug)]
pub struct FormPattern {
    /// The index of the form assossiated with this pattern.
    form_index: u16,
    /// What defines what the range will be.
    pattern: Pattern,
    /// Patterns that can match inside of the original match range.
    patterns: Box<FormPatternList>,
}

impl FormPattern {
    pub fn new(form_index: u16, pattern: Pattern) -> FormPattern {
        FormPattern { form_index, pattern, patterns: Box::new(FormPatternList::new()) }
    }

    pub fn push_fp(&mut self, matcher: Matcher, form_index: u16) {
        let form_pattern = FormPattern {
            form_index,
            pattern: Pattern::Word(matcher),
            patterns: Box::new(FormPatternList::new()),
        };

        self.patterns.0.push(form_pattern);
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
    pub line: usize,
    pub char_tags: CharTags,
    pub line_flags: LineFlags,
}

/// Special container for patterns associated with specific forms and subpatterns.
#[derive(Debug)]
pub struct FormPatternList(pub Vec<FormPattern>);

impl<'a> FormPatternList {
    /// Returns a new instance of `FormPatterns`
    pub fn new() -> FormPatternList {
        FormPatternList(Vec::new())
    }

    pub fn match_text_range(&self, lines: &[TextLine], range: TextRange) -> Vec<LineInfo> {
        let (start, end) = (range.start, range.end);
        let mut lines_info: Vec<LineInfo> =
            (range.lines()).map(|l| LineInfo { line: l, ..Default::default() }).collect();

        // The distance from the beginning of the real line.
        let from_zero = lines[start.line].get_line_byte_at(start.col);
        // No matter what the range is, the first iteration goes only through whole lines, since
        // we don't know if `Pattern::Word`s will be completed that start before the range.
        let start = LineBytePos { line: start.line, byte: 0, file_byte: start.byte - from_zero };

        let end_line_len = lines[end.line].text().len();
        // The distance to the end of the last line in the range.
        let to_end = end_line_len - lines[end.line].get_line_byte_at(end.col);
        let end = LineBytePos { line: end.line, byte: end_line_len, file_byte: end.byte + to_end };

        let range = LineByteRange { start, end };

        self.match_text(lines, range, lines_info.as_mut_slice());

        lines_info
    }

    /// Recursively match a pattern on a piece of text.
    ///
    /// First, it will match itself in the entire segment, secondly, it will match any subpatterns
    /// on segments of itself.
    fn match_text(&self, lines: &[TextLine], range: LineByteRange, info: &'a mut [LineInfo]) {
        let (start, end) = (range.start, range.end);
        // last line is included.
        let lines_iter = lines.iter().enumerate().take(end.line + 1).skip(start.line);

        // This number is used specifically for tree-sitter, which deals in absolute bytes.
        // The use of bytes here, instead of columns on a line, allows me to completely ignore
        // where characters begin and end, and deal only in bytes.
        let mut file_byte = start.file_byte;
        // `line_number - range_start_line == the_correct_index_in_info`.
        let range_start_line = info[0].line;

        // The ranges where we'll match subpatterns of matched patterns.
        let mut recurse_ranges = Vec::new();

        for (index, line) in lines_iter {
            let char_tags = &mut info[index - range_start_line].char_tags;

            // If the range consists of only a single line, only check the byte range.
            let (text, line_start) = if range.start.line == range.end.line {
                (str::from_utf8(&line.text().as_bytes()[start.byte..end.byte]).unwrap(), start.byte)
            // Otherwise, check from the line start, which is only different in the first line.
            } else if index == range.start.line {
                (str::from_utf8(&line.text().as_bytes()[start.byte..]).unwrap(), start.byte)
            } else if index == range.end.line {
                (str::from_utf8(&line.text().as_bytes()[..end.byte]).unwrap(), 0)
            } else {
                (line.text(), 0)
            };

            for form_pattern in &self.0 {
                // In this case, we know it'll only match a single line.
                if let Pattern::Word(Matcher::Regex(reg)) = &form_pattern.pattern {
                    let mut pattern_char_tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                    for range in reg.find_iter(text) {
                        // Adding the form changes to `char_tags`.
                        let form_start = CharTag::PushForm(form_pattern.form_index);
                        let form_end = CharTag::PopForm(form_pattern.form_index);
                        pattern_char_tags.extend_from_slice(&[
                            ((line_start + range.start()) as u32, form_start),
                            ((line_start + range.end()) as u32, form_end),
                        ]);

                        // If there are patterns that can match within this one, do a recursion.
                        if form_pattern.patterns.0.len() > 0 {
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

                            recurse_ranges.push((match_range, &form_pattern.patterns));
                        }
                    }

                    char_tags.insert_slice(pattern_char_tags.as_slice());
                // TODO: This.
                } else if let Pattern::Word(Matcher::TsCapture(_id)) = form_pattern.pattern {
                    todo!();
                } else if let Pattern::Bounds(Matcher::Regex(start), Matcher::Regex(end)) =
                    &form_pattern.pattern
                {
                    // Iterators over matches of the start and end patterns.
                    // NOTE: While each iterator will be ordered, an end pattern will not always
                    // show up after a start pattern, leaving "dangling bounds".
                    let mut start_iter = start.find_iter(text);
                    let mut end_iter = end.find_iter(text);

                    // Vectors for more efficient insertion of elements into `char_tags`.
                    let mut start_tags: SmallVec<[(u32, CharTag); 20]> = SmallVec::new();
                    let mut end_tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                    while let Some(range) = start_iter.next() {
                        // The range to check starts at 0 from the start of the line, which in
                        // the case of the first line of the original range, will not necessarily
                        // be 0.
                        let start = (range.start() + line_start) as u32;
                        // Since bounded ranges can always take on multiple lines, always insert a
                        // multi-line form.
                        start_tags.push((start, CharTag::PushMlForm(form_pattern.form_index)));

                        if form_pattern.patterns.0.len() > 0 {
                            let start = LineBytePos {
                                line: index,
                                byte: range.start() + line_start,
                                file_byte: range.start() + file_byte,
                            };
                        }
                    }

                    // At this point, it is garanteed that the matched ranges will be sorted, and it
                    // is more efficient to insert them all at once.
                    char_tags.insert_slice(start_tags.as_slice());

                    while let Some(range) = end_iter.next() {
                        let end = (range.end() + line_start) as u32;
                        end_tags.push((end, CharTag::PopMlForm(form_pattern.form_index)));

                        let end = LineBytePos {
                            line: index,
                            byte: range.end() + line_start,
                            file_byte: range.end() + file_byte,
                        };
                    }

                    char_tags.insert_slice(end_tags.as_slice());
                }
            }

            file_byte += text.len();
        }

        for (range, patterns) in recurse_ranges {
            patterns.match_text(lines, range, info);
        }
    }
}

pub struct TagManager {
    /// The forms for syntax highlighting.
    forms: Vec<Form>,

    /// The patterns associated with said forms.
    form_pattern_list: FormPatternList,
}

impl TagManager {
    /// Returns a new instance of `TagManager`
    pub fn new() -> TagManager {
        TagManager {
            forms: Vec::new(),
            form_pattern_list: FormPatternList::new(),
        }
    }

    pub fn match_text_range(&self, lines: &[TextLine], range: TextRange) -> Vec<LineInfo> {
        self.form_pattern_list.match_text_range(lines, range)
    }

    /// Pushes a new form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_fp(&mut self, matcher: Matcher, form_index: u16) -> &mut FormPattern {
        let form_pattern = FormPattern {
            form_index,
            pattern: Pattern::Word(matcher),
            patterns: Box::new(FormPatternList::new()),
        };

        self.form_pattern_list.0.push(form_pattern);

        self.form_pattern_list.0.last_mut().unwrap()
    }

    /// Pushes a new multiline form pattern.
    ///
    /// Returns a mutable reference for the purpose of placing subpatterns in the form pattern.
    pub fn push_ml_fp(&mut self, bounds: [Matcher; 2], form_index: u16) -> &mut FormPattern {
        // Move out of array and get rid of it without copying.
        let [start, end] = bounds;

        let form_pattern = if start == end {
            FormPattern {
                form_index,
                pattern: Pattern::Bound(start),
                patterns: Box::new(FormPatternList::new()),
            }
        } else {
            FormPattern {
                form_index,
                pattern: Pattern::Bounds(start, end),
                patterns: Box::new(FormPatternList::new()),
            }
        };

        self.form_pattern_list.0.push(form_pattern);

        self.form_pattern_list.0.last_mut().unwrap()
    }

    /// Pushes a new form onto the list.
    pub fn push_form(&mut self, style: ContentStyle, is_final: bool) {
        self.forms.push(Form { style, is_final });
    }

    pub fn forms(&self) -> &[Form] {
        self.forms.as_ref()
    }
}
