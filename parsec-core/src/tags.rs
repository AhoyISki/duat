use std::{slice, str};

use bitflags::bitflags;
use crossterm::style::{ContentStyle, SetAttribute};
use regex::Regex;

use crate::{action::TextRange, cursor::TextPos, file::TextLine};

// NOTE: Unlike cursor and file positions, character tags are byte indexed, not character indexed.
// The reason is that modules like `regex` and `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone, Copy, Debug)]
pub enum CharTag {
    // NOTE: Terribly concocted scenarios could partially break forms identifiers.
    // Implemented:
    /// Appends a form to the stack.
    AppendForm(u16),
    /// Removes a form from the stack. It won't always be the last one.
    RemoveForm(u16),

    /// Appends a multi-line form to the stack.
    AppendMlForm(u16),
    /// Removes a multi-line form from the stack. It won't always be the last one.
    RemoveMlForm(u16),
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
    pub struct LineFlags: u16 {
        // Implemented:
        /// If all characters in the line are ascii.
        const PURE_ASCII = 1 << 0;
        /// If there are no double/zero width characters or tabs.
        const PURE_1_COL = 1 << 1;

        // Not Implemented:
        // Wether or not the line can fold.
        const CAN_FOLD   = 1 << 2;
        /// Wether or not the line is folded.
        const IS_FOLDED  = 1 << 3;
        /// If the line is supposed to be replaced by another line when not hovered.
        const CONCEALED  = 1 << 4;
        /// If the line contains the start of a multi-line form (strings or block comments).
        const START_FORM = 1 << 5;
        /// If the line contains the end of a multi-line form (strings or block comments).
        const END_FORM   = 1 << 6;
        /// If the line contains a pattern that acts as both the start and end of a multi-line form.
        const FORM_BOUND = 1 << 7;
        /// If the line is a line wise comment.
        const IS_COMMENT = 1 << 8;
        /// If the line is line wise documentation.
        const IS_DOC     = 1 << 9;
    }
}

// This type exists solely for the purpose of allowing more efficient insertion of elements
// according to the necessity of the program.
// Since all insertions (wrappings, syntax, cursors, etc) already come sorted, it makes sense to
// create efficient insertion algorithms that take that into account.
/// A vector of `CharTags`.
#[derive(Debug)]
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
        let mut insertions = Vec::with_capacity(new_char_tags.len());

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
enum Matcher {
    /// A regular expression.
    Regex(Regex),
    /// A tree-sitter capture.
    TsCapture(usize),
}

// Patterns can occupy multiple lines only if they are defined as an opening and ending bound.
/// Indicates wether a pattern should be able to occupy multiple lines, and how.
enum MatchingSpan {
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
struct FormPattern {
    /// The index of the form assossiated with this pattern.
    form_index: u16,
    /// What defines what the range will be.
    matching_span: MatchingSpan,
    /// Patterns that can match inside of the original match range.
    patterns: Box<FormPatterns>,
}

impl FormPattern {
    pub fn new(form_index: u16, matching_span: MatchingSpan) -> FormPattern {
        FormPattern { form_index, matching_span, patterns: Box::new(FormPatterns::new()) }
    }
}

struct RecursiveMatcher {
    cur_id: u8,
}

struct FormPatterns(Vec<FormPattern>);

/// A position indexed by its byte in relation to the line, instead of column.
#[derive(Clone, Copy)]
struct LineBytePos {
    /// The line in the file.
    line: usize,
    /// The byte in relation to the line.
    byte: usize,
    /// The byte in relation to the beginning of the file.
    file_byte: usize,
}

/// A range of positions indexed by line byte, instead of column.
#[derive(Clone, Copy)]
struct LineByteRange {
    start: LineBytePos,
    end: LineBytePos,
}

impl FormPatterns {
    pub fn new() -> FormPatterns {
        FormPatterns(Vec::new())
    }

    /// Recursively match a pattern on a piece of text.
    ///
    /// First, it will match itself in the entire segment, secondly, it will match any subpatterns
    /// on segments of itself.
    fn match_text(&self, lines: &mut [TextLine], range: LineByteRange) {
        let (start, end) = (range.start, range.end);
        let lines_iter = lines.iter_mut().enumerate().take(end.line + 1).skip(start.line);
        let mut file_byte = start.file_byte;

        let mut recurse_ranges = Vec::new();

        for (index, line) in lines_iter {
            let mut char_tags = CharTags::new();

            let (text, line_start) = if range.start.line == range.end.line {
                (str::from_utf8(&line.text().as_bytes()[start.byte..end.byte]).unwrap(), start.byte)
            } else if index == range.start.line {
                (str::from_utf8(&line.text().as_bytes()[start.byte..]).unwrap(), start.byte)
            } else if index == range.end.line {
                (str::from_utf8(&line.text().as_bytes()[..end.byte]).unwrap(), 0)
            } else {
                (line.text(), 0)
            };

            for pattern in &self.0 {
                if let MatchingSpan::Word(Matcher::Regex(reg)) = &pattern.matching_span {
                    let mut pattern_char_tags = Vec::new();

                    for range in reg.find_iter(text) {
                        let form_start = CharTag::AppendForm(pattern.form_index);
                        let form_end = CharTag::AppendForm(pattern.form_index);

                        pattern_char_tags.extend_from_slice(&[
                            (range.start() as u32, form_start),
                            (range.end() as u32, form_end),
                        ]);

                        // If there are patterns that can match within this one, do a recursion.
                        if pattern.patterns.0.len() > 0 {
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
                                }
                            };
                            
							recurse_ranges.push((match_range, &pattern.patterns));
                        }
                    }

                    char_tags.insert_slice(pattern_char_tags.as_slice());
                // TODO: This.
                } else if let MatchingSpan::Word(Matcher::TsCapture(_id)) = &pattern.matching_span {

                // TODO: This as well.
                } else if let MatchingSpan::Bounds(Matcher::Regex(start), Matcher::Regex(end)) =
                    &pattern.matching_span
                {
                    let mut start_iter = start.find_iter(text);
                    let mut end_iter = end.find_iter(text);

                    let mut start_ranges = Vec::new();

                    let mut end_ranges = Vec::new();

                    while let Some(range) = start_iter.next() {
                        let start = range.start() as u32;
                        start_ranges.push((start, CharTag::AppendMlForm(pattern.form_index)));
                    }

                    char_tags.insert_slice(start_ranges.as_slice());

                    while let Some(range) = end_iter.next() {
                        let end = range.end() as u32;
                        end_ranges.push((end, CharTag::RemoveMlForm(pattern.form_index)));
                    }

                    char_tags.insert_slice(end_ranges.as_slice());
                }
            }

            line.char_tags.insert_slice(char_tags.0.as_slice());
        }

        for (range, patterns) in recurse_ranges {
            patterns.match_text(lines, range);
        }
    }
}
