use std::{
    ops::{Range, RangeFrom, RangeInclusive, RangeToInclusive},
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

        // Not Implemented:
        // Wether or not the line can fold.
        const CAN_FOLD   = 1 << 2;
        /// Wether or not the line is folded.
        const IS_FOLDED  = 1 << 3;
        /// If the line is supposed to be replaced by another line when not hovered.
        const CONCEALED  = 1 << 4;
        /// If the line contains a pattern that acts as both the start and end of a multi-line form.
        const FORM_BOUND = 1 << 5;
        /// If the line is a line wise comment.
        const IS_COMMENT = 1 << 6;
        /// If the line is line wise documentation.
        const IS_DOC     = 1 << 7;
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
    patterns: Box<FormPatterns>,
}

impl FormPattern {
    pub fn new(form_index: u16, pattern: Pattern) -> FormPattern {
        FormPattern { form_index, pattern, patterns: Box::new(FormPatterns::new()) }
    }

    pub fn push(&mut self, form_index: u16, pattern: Pattern) {
        self.patterns.0.push(FormPattern::new(form_index, pattern));
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

#[derive(Debug)]
enum MlRange<'a> {
    Bounded { range: LineByteRange, pattern: &'a FormPattern },
    LeftoverStart { start: LineBytePos, pattern: &'a FormPattern },
    LeftoverEnd { end: LineBytePos, pattern: &'a FormPattern },
}

impl<'a> MlRange<'a> {
    fn contains(&self, pos: LineBytePos) -> bool {
        match self {
            MlRange::Bounded { range: LineByteRange { start, end }, pattern } => {
                start.file_byte <= pos.file_byte && end.file_byte > pos.file_byte
            }
            MlRange::LeftoverStart { start, pattern } => start.file_byte <= pos.file_byte,
            MlRange::LeftoverEnd { end, pattern } => end.file_byte > pos.file_byte,
        }
    }

    fn contains_line(&self, line: usize) -> bool {
        match self {
            MlRange::Bounded { range, .. } => line >= range.start.line && line <= range.end.line,
            MlRange::LeftoverStart { start, .. } => line >= start.line,
            MlRange::LeftoverEnd { end, .. } => line <= end.line,
        }
    }

    fn start(&self) -> LineBytePos {
        match self {
            MlRange::Bounded { range, .. } => range.start,
            MlRange::LeftoverStart { start, .. } => *start,
            MlRange::LeftoverEnd { .. } => LineBytePos { line: 0, byte: 0, file_byte: 0 },
        }
    }

    fn start_line(&self) -> usize {
        match self {
            MlRange::Bounded { range, .. } => range.start.line,
            MlRange::LeftoverStart { start, .. } => start.line,
            MlRange::LeftoverEnd { .. } => 0,
        }
    }

    fn pattern(&self) -> &'a FormPattern {
        match self {
            &MlRange::Bounded { range, pattern } => pattern,
            &MlRange::LeftoverStart { start, pattern } => pattern,
            &MlRange::LeftoverEnd { end, pattern } => pattern,
        }
    }
}

#[derive(Debug)]
pub struct MlRangeList<'a>(Vec<MlRange<'a>>);

impl<'a> MlRangeList<'a> {
    /// Checks if a line is contained within a range of a multi-line form.
    ///
    /// Returns a range containing all the lines involved in said range that appear before the
    /// checked line. Also returns the pattern used in the range.
    pub fn wrapping_range(&self, line: usize) -> Option<(Range<usize>, &'a FormPattern)> {
        match self.0.iter().find(|r| r.contains_line(line)) {
            Some(range) => Some((range.start_line()..line, range.pattern())),
            None => None,
        }
    }

    /// Tries to match a given bound of a pattern.
    ///
    /// If the final range would occupy only a single line, it is removed.
    fn match_bound(&mut self, range: MlRange) -> Option<LineBytePos> {
        // If the range has no end, we need to look for an end to finish it.
        if let MlRange::LeftoverStart { start, pattern } = range {
            let matching_condition: &dyn Fn(&&MlRange) -> bool = &|r| -> bool {
                // We can't match a `MlRange::LeftoverStart` with another.
                !matches!(r, MlRange::LeftoverStart { .. })
                    && r.pattern().form_index == pattern.form_index
                	&& r.contains(start)
            };

            match self.0.iter().enumerate().find(|(_, r)| matching_condition(r)) {
                Some((index, range)) => {
                    if let MlRange::Bounded { range, pattern } = range {
                        let old_start = range.start;
                        let pattern = pattern.clone();

                        let end = range.end;

						// Change the start of the old `MlRange::Bounded` to the new start.
						if start.line < end.line {
                            self.0[index] =
                                MlRange::Bounded { range: LineByteRange { start, end }, pattern };

    						// Since the old start is still there, we need to change its status to
    						// `MlRange::LeftoverStart`, since the new `MlRange::Bounded` has taken
    						// its end.
                            self.0.insert(
                                index,
                                MlRange::LeftoverStart { start: old_start, pattern }
                            );
						} else {
    						// We don't need to keep track of non multi-line ranges.
    						self.0.remove(index);
						}

						// Return the position that matched with the start, for subpattern matching.
                        Some(end)
                    } else if let MlRange::LeftoverEnd { end, pattern } = range {
                        let end = *end;

						if start.line < end.line {
    						// Finish the range by transforming a `MlRange::LeftoverEnd` into a
    						// `MlRange::Bounded`.
                            self.0[index] =
                                MlRange::Bounded { range: LineByteRange { start, end }, pattern };
						} else {
    						// We don't need to keep track of non multi-line ranges.
    						self.0.remove(index);
						}

                        Some(end)
                    } else {
                        None
                    }
                }
                None => None,
            }
        // If the range has no start, we should look for one to finish it.
        } else if let MlRange::LeftoverEnd { end, pattern } = range {
            let matching_condition: &dyn Fn(&&MlRange) -> bool = &|r| -> bool {
                // We can't match a `MlRange::LeftoverEnd` with another.
                !matches!(r, MlRange::LeftoverEnd { .. })
                    && r.pattern().form_index == pattern.form_index
                	&& r.contains(end)
            };

            match self.0.iter().enumerate().find(|(_, r)| matching_condition(r)) {
                Some((index, range)) => {
                    if let MlRange::Bounded { range, pattern } = range {
                        let old_end = range.end;
                        let pattern = pattern.clone();

                        let start = range.start;

						if start.line < end.line {
    						// Change the start of the old `MlRange::Bounded` to the new start.
                            self.0[index] =
                                MlRange::Bounded { range: LineByteRange { start, end }, pattern };

    						// Since the old end is still there, we need to change its status to
    						// `MlRange::LeftoverEnd`, since the new `MlRange::Bounded` has taken
    						// its start.
                            self.0.insert(
                                index + 1,
                                MlRange::LeftoverEnd { end: old_end, pattern }
                            );
						} else {
    						// We don't need to keep track of non multi-line ranges.
    						self.0.remove(index);
						}

						// Return the position that matched with the end, for subpattern matching.
                        Some(start)
                    } else if let MlRange::LeftoverStart { start, pattern } = range {
                        let start = *start;

						if start.line < end.line {
    						// Finish the range by transforming a `MlRange::LeftoverEnd` into a
    						// `MlRange::Bounded`.
                            self.0[index] =
                                MlRange::Bounded { range: LineByteRange { start, end }, pattern };
						} else {
    						// We don't need to keep track of non multi-line ranges.
    						self.0.remove(index);
						}

                        Some(start)
                    } else {
                        None
                    }
                }
                None => None,
            }
        // This shouldn't happen.
        } else {
            None
        }
    }
}

/// Special container for
#[derive(Debug)]
pub struct FormPatterns(pub Vec<FormPattern>);

impl<'a> FormPatterns {
    /// Returns a new instance of `FormPatterns`
    pub fn new() -> FormPatterns {
        FormPatterns(Vec::new())
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

        panic!("{:#?}", self.match_text(lines, range, lines_info.as_mut_slice(), true));

        lines_info
    }

    /// Recursively match a pattern on a piece of text.
    ///
    /// First, it will match itself in the entire segment, secondly, it will match any subpatterns
    /// on segments of itself.
    fn match_text(
        &self, lines: &[TextLine], range: LineByteRange, info: &'a mut [LineInfo], is_first: bool,
    ) -> Option<MlRangeList> {
        // The vector witch will contain all multi-line ranges in the tested range.
        let mut ml_ranges = MlRangeList(Vec::new());

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

            for pattern in &self.0 {
                // In this case, we know it'll only match a single line.
                if let Pattern::Word(Matcher::Regex(reg)) = &pattern.pattern {
                    let mut pattern_char_tags: SmallVec<[(u32, CharTag); 10]> = SmallVec::new();

                    for range in reg.find_iter(text) {
                        // Adding the form changes to `char_tags`.
                        let form_start = CharTag::PushForm(pattern.form_index);
                        let form_end = CharTag::PopForm(pattern.form_index);
                        pattern_char_tags.extend_from_slice(&[
                            ((line_start + range.start()) as u32, form_start),
                            ((line_start + range.end()) as u32, form_end),
                        ]);

                        // If there are patterns that can match within this one, do a recursion.
                        if pattern.patterns.0.len() > 0 {
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

                            recurse_ranges.push((match_range, &pattern.patterns));
                        }
                    }

                    char_tags.insert_slice(pattern_char_tags.as_slice());
                // TODO: This.
                } else if let Pattern::Word(Matcher::TsCapture(_id)) = &pattern.pattern {
                } else if let Pattern::Bounds(Matcher::Regex(start), Matcher::Regex(end)) =
                    &pattern.pattern
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
                        start_tags.push((start, CharTag::PushMlForm(pattern.form_index)));

                        if pattern.patterns.0.len() > 0 {
                            let start = LineBytePos {
                                line: index,
                                byte: range.start() + line_start,
                                file_byte: range.start() + file_byte,
                            };

                            // Assume that there won't be an end to this range, for now.
                            ml_ranges.0.push(MlRange::LeftoverStart { start, pattern });
                        }
                    }

                    // At this point, it is garanteed that the matched ranges will be sorted, and it
                    // is more efficient to insert them all at once.
                    char_tags.insert_slice(start_tags.as_slice());

                    while let Some(range) = end_iter.next() {
                        let end = (range.end() + line_start) as u32;
                        end_tags.push((end, CharTag::PopMlForm(pattern.form_index)));

                        let end = LineBytePos {
                            line: index,
                            byte: range.end() + line_start,
                            file_byte: range.end() + file_byte,
                        };

                        let bound = MlRange::LeftoverEnd { end, pattern };

                        // Try to find a start to this pattern.
                        if let Some(start) = ml_ranges.match_bound(bound) {
                            // If there are subpatterns, add this range to the matching ranges.
                            if pattern.patterns.0.len() > 0 {
                                let range = LineByteRange { start, end };

                                recurse_ranges.push((range, &pattern.patterns));
                            }
                        }
                    }

                    char_tags.insert_slice(end_tags.as_slice());
                }
            }

            file_byte += text.len();
        }

        for (range, patterns) in recurse_ranges {
            patterns.match_text(lines, range, info, false);
        }

        if is_first {
            Some(ml_ranges)
        } else {
            None
        }
    }
}
