use std::{ops::RangeInclusive, str};

use bitflags::bitflags;
use crossterm::{
    cursor::SetCursorStyle,
    style::{Attributes, Color, ContentStyle, Stylize},
};
use regex::Regex;
use smallvec::SmallVec;

use crate::{
    action::TextRange,
    cursor::TextPos,
    file::TextLine,
    ui::{EndNode, Label, Ui},
};

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not character indexed.
// The reason is that modules like `regex` and `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone, Copy, PartialEq, Debug)]
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
    /// Places the main cursor.
    PrimaryCursor,
    /// Begins a selection in the file.
    SelectionStart,
    /// Ends a selection in the file.
    SelectionEnd,

    // Not Implemented:
    /// Places a secondary cursor.
    SecondaryCursor,
    /// Begins or ends a hoverable section in the file.
    HoverBound,
    /// Conceals a character with a string of text of equal lenght, permanently.
    PermanentConceal { index: u16 },
}

pub(crate) struct FormFormer {
    forms: Vec<(Form, u16)>,
}

impl FormFormer {
    pub(crate) fn new() -> Self {
        Self { forms: Vec::new() }
    }

    fn push_form(&mut self, form: Form, id: u16) -> Form {
        self.forms.push((form, id));

        self.make_form()
    }

    fn remove_form(&mut self, id: u16) -> Form {
        if let Some((index, _)) = self.forms.iter().enumerate().rfind(|(_, &(_, i))| i == id) {
            self.forms.remove(index);

            self.make_form()
        } else {
            panic!("The id {} has yet to be pushed.", id);
        }
    }

    /// Generates the form to be printed, given all the previously pushed forms in the `Form` stack.
    pub fn make_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::default(),
        };

        let mut form = Form { style, is_final: false };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final }, _) in &self.forms {
            let new_foreground = style.foreground_color;
            set_var(&mut fg_done, &mut form.style.foreground_color, &new_foreground, is_final);

            let new_background = style.background_color;
            set_var(&mut bg_done, &mut form.style.background_color, &new_background, is_final);

            let new_underline = style.underline_color;
            set_var(&mut ul_done, &mut form.style.underline_color, &new_underline, is_final);

            if !attr_done {
                form.style.attributes.extend(style.attributes);
                if is_final {
                    attr_done = true
                }
            }

            if fg_done && bg_done && ul_done && attr_done {
                break;
            }
        }

        form
    }
}

impl CharTag {
    pub(crate) fn trigger(
        &self, label: &mut impl Label, palette: &FormPalette, wrap_indent: usize,
        form_former: &mut FormFormer,
    ) -> bool {
        match self {
            CharTag::PushForm(id) => {
                label.set_form(form_former.push_form(palette.get(*id as usize), *id));
            }
            CharTag::PopForm(id) => label.set_form(form_former.remove_form(*id)),
            CharTag::WrapppingChar => {
                if label.wrap_line(wrap_indent).is_err() {
                    return false;
                }
            }
            CharTag::PrimaryCursor => label.place_primary_cursor(),
            CharTag::SecondaryCursor => label.place_secondary_cursor(),
            CharTag::SelectionStart => {
                let (form, id) = palette.main_selection;
                label.set_form(form_former.push_form(form, id));
            }
            CharTag::SelectionEnd => {
                label.set_form(form_former.remove_form(palette.main_selection.1))
            }
            _ => {}
        }

        true
    }
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
#[derive(Clone, Default)]
pub struct CharTags(Vec<(u32, CharTag)>);

impl std::fmt::Debug for CharTags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let print_vec: String = self
            .0
            .iter()
            .map(|(b, t)| match t {
                CharTag::PushForm(id) => format!("{}:PuF({})", b, id),
                CharTag::PopForm(id) => format!("{}PoF({})", b, id),
                CharTag::WrapppingChar => format!("{}:Wc", b),
                CharTag::PrimaryCursor => format!("{}:Pc", b),
                CharTag::SecondaryCursor => format!("{}:Pc", b),
                CharTag::SelectionStart => format!("{}:Ss", b),
                CharTag::SelectionEnd => format!("{}:Se", b),
                _ => panic!("{:#?}", (b, t)),
            })
            .collect::<Vec<String>>()
            .join(", ");

        f.write_fmt(format_args!("CharTags: {}", print_vec))
    }
}

impl CharTags {
    /// Creates a new instance of `CharTags`.
    pub fn new() -> CharTags {
        CharTags(Vec::new())
    }

    /// More efficient insertion method that requires no sorting.
    pub fn insert(&mut self, char_tag: (u32, CharTag)) {
        // It just finds the first element with a bigger number and puts the `char_tag` behind it.
        match self.0.iter().enumerate().find(|(_, &(c, _))| c > char_tag.0) {
            Some((pos, _)) => self.0.insert(pos, char_tag),
            None => self.0.push(char_tag),
        }
    }

    /// It's like `insert`, but it inserts "in the background". Used on multi-line ranges.
    pub fn bottom_insert(&mut self, char_tag: (u32, CharTag)) {
        match self.0.iter().enumerate().rev().find(|(_, &(c, _))| c < char_tag.0) {
            Some((pos, _)) => self.0.insert(pos + 1, char_tag),
            None => self.0.insert(0, char_tag),
        }
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
        self.0.retain(|&t| do_retain(t))
    }

    pub fn remove_first<F>(&mut self, cmp: F)
    where
        F: Fn((u32, CharTag)) -> bool,
    {
        if let Some((index, _)) = self.0.iter().enumerate().find(|&(_, &t)| cmp(t)) {
            self.0.remove(index);
        }
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

#[derive(Default, Debug, Clone, Copy)]
/// A style for text.
pub struct Form {
    pub style: ContentStyle,
    /// Wether or not the `Form`s colors and attributes should override any that come after.
    pub is_final: bool,
}

impl Form {
    pub fn new(style: ContentStyle, is_final: bool) -> Self {
        Self { style, is_final }
    }
}

#[derive(Clone, Copy)]
pub struct CursorStyle {
    /// An optional member when using application specific cursors.
    caret: Option<SetCursorStyle>,
    // NOTE: This is obligatory as a fallback for when the application can't render the
    // cursor with `caret`.
    /// To render the cursor as a form, not as an actual cursor.
    form: Form,
}

impl CursorStyle {
    pub fn new(caret: Option<SetCursorStyle>, form: Form) -> Self {
        Self { caret, form }
    }
}

/// An expandable palette of forms to be used when rendering.
#[derive(Clone)]
pub struct FormPalette {
    pub main_cursor: (CursorStyle, u16),
    pub secondary_cursors: (CursorStyle, u16),
    pub main_selection: (Form, u16),
    pub secondary_selections: (Form, u16),
    pub line_numbers: (Form, u16),
    pub main_line_number: (Form, u16),
    extra_forms: Vec<(String, Form)>,
}

impl Default for FormPalette {
    fn default() -> Self {
        let cursor_form = CursorStyle::new(
            Some(SetCursorStyle::DefaultUserShape),
            Form::new(ContentStyle::new().reverse(), false),
        );
        Self {
            line_numbers: (Form::default(), 0),
            main_line_number: (Form::default(), 1),
            main_cursor: (cursor_form, 2),
            secondary_cursors: (cursor_form, 3),
            main_selection: (Form::new(ContentStyle::new().on_dark_grey(), false), 4),
            secondary_selections: (Form::new(ContentStyle::new().on_dark_grey(), false), 5),
            extra_forms: Vec::new(),
        }
    }
}

impl FormPalette {
    /// Adds a new named `Form` to the list of user added `Form`s.
    pub fn add_form<S>(&mut self, name: S, form: Form)
    where
        S: ToString,
    {
        let name = name.to_string();
        if let None = self.extra_forms.iter().find(|(cmp, _)| *cmp == name) {
            self.extra_forms.push((name.to_string(), form));
        } else {
            panic!("The form {} is already in use!", name);
        }
    }

    // TODO: Extend this with the default forms.
    /// Returns the `Form` associated to a given name with the index for efficient access.
    pub fn get_from_name<S>(&self, name: S) -> Option<(Form, usize)>
    where
        S: ToString,
    {
        let name = name.to_string();
        self.extra_forms
            .iter()
            .enumerate()
            .find(|(_, (cmp, _))| *cmp == name)
            .map(|(index, &(_, form))| (form, index))
    }

    /// Returns a form, given an index.
    pub fn get(&self, index: usize) -> Form {
        self.extra_forms.get(index).map(|(_, form)| *form).expect("The id is not valid!")
    }
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
    fn match_iter<'a>(&'a self, text: &'a str, range: TagRange) -> MatchIter {
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
    Regex((regex::Matches<'a, 'a>, TagRange)),
    TsCapture((std::iter::Once<(usize, usize)>, TagRange)),
}

impl<'a> Iterator for MatchIter<'a> {
    type Item = TagRange;

    /// Returns an `Option<LineByteRange>` for the next element in the iterator.
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MatchIter::Regex((regs, range)) => {
                let start = range.start;
                let (line, byte, file_byte) = (start.row, start.byte, start.file_byte);

                regs.next().map(|r| {
                    let (file_start, file_end) = (file_byte + r.start(), file_byte + r.end());
                    TagRange {
                        start: TagPos { row: line, byte: byte + r.start(), file_byte: file_start },
                        end: TagPos { row: line, byte: byte + r.end(), file_byte: file_end },
                    }
                })
            }
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

impl Pattern {
    fn find(
        &self, lines: &[TextLine], range: TagRange, mut inner_count: usize,
    ) -> (SmallVec<[TagRange; 32]>, usize) {
        // An iterator over the lines in the range, it returns the truncated text of the
        // line, the line's number, and a `LineByteRange`, where the line is in the file.
        let lines_iter = lines
            .iter()
            .enumerate()
            .take(range.end.row + 1)
            .skip(range.start.row)
            .map(|(n, l)| {
                // If there's just one line, the range is truncated differently.
                if range.start.row == range.end.row {
                    (n, &l.text().as_bytes()[range.start.byte..range.end.byte], range.start.byte)
                } else if n == range.start.row {
                    (n, &l.text().as_bytes()[range.start.byte..], range.start.byte)
                } else if n == range.end.row {
                    (n, &l.text().as_bytes()[..range.end.byte], 0)
                } else {
                    (n, l.text().as_bytes(), 0)
                }
            })
            .scan(range.start.file_byte, |acc, (n, t, s)| {
                *acc += t.len();
                Some((
                    n,
                    str::from_utf8(t).unwrap(),
                    TagRange {
                        start: TagPos { row: n, byte: s, file_byte: *acc - t.len() },
                        end: TagPos { row: n, byte: s + t.len(), file_byte: *acc },
                    },
                ))
            });

        let mut matched_ranges = SmallVec::<[TagRange; 32]>::new();

        if let Pattern::Word(word) = self {
            for line in lines_iter {
                matched_ranges.extend(
                    word.match_iter(line.1, line.2)
                        .map(|r| TagRange { start: r.start, end: r.end }),
                )
            }

            (matched_ranges, 0)
        } else if let Pattern::Bounds(start, end) = self {
            let mut starts = SmallVec::<[TagPos; 32]>::new();
            let mut ends = SmallVec::<[TagPos; 32]>::new();

            // Collect every match inside the range.
            for line in lines_iter {
                starts.extend(start.match_iter(line.1, line.2).map(|r| r.start));
                ends.extend(end.match_iter(line.1, line.2).map(|r| r.end));
            }

            let mut start_iter = starts.iter();
            // If the inner count is greater than 0, start a range immediately.
            let mut current_start = if inner_count > 0 { Some(range.start) } else { None };

            let mut next_start = None;
            let mut do_iter_starts = true;

            for end in ends {
                if let Some(start) = next_start.filter(|&s| s < end) {
                    // Only update `current_start` if we have finished a range.
                    if inner_count == 0 {
                        current_start = Some(start);
                    }
                    next_start = None;

                    if !do_iter_starts {
                        inner_count += 1;
                    }
                    do_iter_starts = true;
                }

                if do_iter_starts {
                    // Iterate until we find a starting match that comes after the ending
                    // match. That might possibly be a completed range.
                    while let Some(&start) = start_iter.next() {
                        if start < end {
                            current_start = current_start.or(Some(start));
                            inner_count += 1;
                        } else {
                            next_start = Some(start);
                            do_iter_starts = false;

                            break;
                        }
                    }
                }

                // A negative `inner_count` would match inverted ranges.
                inner_count = inner_count.saturating_sub(1);

                if let (Some(start), true) = (current_start, inner_count == 0) {
                    matched_ranges.push(TagRange { start, end });

                    current_start = None;
                }
            }

            // Check for any possible remaining start matches.
            current_start = current_start.or(next_start.or(start_iter.next().copied()));

            // If we find any, match until the end of the range.
            if let Some(start) = current_start {
                matched_ranges.push(TagRange { start, end: range.end });
                inner_count += 1 + start_iter.count();
            }

            (matched_ranges, inner_count)
        } else {
            todo!()
        }
    }
}

/// An assossiation of a pattern with a spcecific form.
#[derive(Debug, Default, Clone)]
pub struct FormPattern {
    /// The index of the form assossiated with this pattern.
    form: Option<u16>,
    /// What defines what the range will be.
    pattern: Pattern,
    /// Patterns that can match inside of the original match range.
    form_matches: Vec<FormPattern>,
    /// A unique identifier for this specific `FormPattern`
    id: u16,
    /// Wether or not this `FormPattern` should allow parent patterns to match in it.
    is_exclusive: bool,
}

// The next two structs are useful in the context of syntax highlighting, since the line's byte is
// useful for regex calculation, while the file's byte is useful for tree-sitter capture, and the
// column (char index) of the line is completely useless.
/// A position indexed by its byte in relation to the line, instead of column.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct TagPos {
    /// The line in the file.
    row: usize,
    /// The byte in relation to the line.
    byte: usize,
    /// The byte in relation to the beginning of the file.
    file_byte: usize,
}

impl TagPos {
    fn from_text(line: &TextLine, pos: TextPos) -> TagPos {
        TagPos { row: pos.row, byte: line.get_line_byte_at(pos.col), file_byte: pos.byte }
    }
}

impl PartialOrd for TagPos {
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

impl Ord for TagPos {
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
struct TagRange {
    start: TagPos,
    end: TagPos,
}

impl TagRange {
    fn lines(&self) -> RangeInclusive<usize> {
        self.start.row..=self.end.row
    }
}

/// A list of ranges, specifically made with "poking" in mind.
#[derive(Debug)]
struct TagRangeList(Vec<TagRange>);

impl TagRangeList {
    // NOTE: This assumes you are matchin in ascending order of position.
    /// "Pokes" a hole in the range.
    fn poke(&mut self, poke: TagRange) {
        // Find the range that contains the first point, in order to start poking.
        if let Some((index, &range)) = self.0.iter().enumerate().find(|(_, r)| r.end > poke.start) {
            // The first range after having its end removed by the poke.
            let first_cut_range = TagRange { start: range.start, end: poke.start };

            let mut ranges_iter = self.0.iter().enumerate().skip(index);

            // Look for a range that can fit the end of the poke, and remove all ranges in between.
            while let Some((last_checked, &range)) = ranges_iter.next() {
                if range.end >= poke.end {
                    let last_cut_range = TagRange { start: poke.end, ..range };

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
    pub starting_id: u16,
    pub ending_id: u16,
}

impl FormPattern {
    /// Matches a given pattern and its subpatterns on a range of text.
    fn recur_match(
        &self, lines: &[TextLine], ranges: &mut TagRangeList, info: &mut [(LineInfo, usize)],
        last_match: LastMatch,
    ) -> (SmallVec<[TagRange; 32]>, LastMatch) {
        // The finalized vector with all ranges that were occupied by exclusive patterns.
        let mut poked_ranges = SmallVec::<[TagRange; 32]>::new();

        // This line is used for referencing `info`.
        let first_start = ranges.0[0].start;
        let last_end = ranges.0.last().unwrap().end;
        if first_start == last_end {
            return (SmallVec::new(), LastMatch::default());
        };

        // The finalized list of multi-line ranges that weren't closed off.
        let mut new_last_match = LastMatch {
            patterns: Vec::new(),
            pos: TagPos { row: last_end.row + 1, byte: 0, file_byte: last_end.file_byte + 1 },
        };

        for form_match in &self.form_matches {
            // This vector is temporary so that we don't poke the same ranges multiple times.
            let mut ranges_to_poke = SmallVec::<[TagRange; 32]>::new();

            let inner_count = if let Pattern::Bounds(_, _) = &form_match.pattern {
                if let Some(&(_, inner_count)) = last_match
                    .patterns
                    .get(0)
                    .filter(|(i, _)| i == &form_match.id && first_start >= last_match.pos)
                {
                    info[0].0.starting_id = form_match.id;
                    inner_count
                } else {
                    0
                }
            } else {
                0
            };

            for &range in ranges.0.iter() {
                let (ranges, inner_count) = form_match.pattern.find(lines, range, inner_count);

                for range in ranges {
                    for line in range.lines() {
                        let tags = &mut info[line - first_start.row].0.char_tags;

                        // The start is the starting byte in the first line, 0 otherwise.
                        if line == range.start.row {
                            if let Some(form) = form_match.form {
                                tags.insert((range.start.byte as u32, CharTag::PushForm(form)));
                            }
                        } else {
                            info[line - first_start.row].0.starting_id = form_match.id;

                            // The form here works kind of like a "background" of sorts.
                            if let Some(form) = form_match.form {
                                tags.bottom_insert((0, CharTag::PushForm(form)));
                            }
                        };

                        // Popping the form only really needs to be done at the end.
                        if line == range.end.row {
                            let end_line = &lines[line];
                            // This should guarantee that:
                            //   1: The match is ending without a proper end;
                            //   2: The match is ending at the end of a line;
                            if inner_count > 0 && range.end.byte == end_line.text().len() {
                                info[line - first_start.row].0.ending_id = form_match.id;
                            }
                            if let Some(form) = form_match.form {
                                tags.insert((range.end.byte as u32, CharTag::PopForm(form)));
                            }
                        } else {
                            // On lines other than the last, the line ends with the id.
                            info[line - first_start.row].0.ending_id = form_match.id;
                        }
                    }

                    // The count is here to ensure that the order of elements remains
                    // consistent.
                    // If the pattern is exclusive, nothing should be allowed to match on
                    // its range.
                    // Match subpatterns.
                    let mut ranges = TagRangeList(vec![range]);
                    let range_start = range.start.row - first_start.row;
                    let range_end = range.end.row - first_start.row;
                    let info = &mut info[range_start..=range_end];

                    let pattern_slice = if let Pattern::Bounds(_, _) = form_match.pattern {
                        if inner_count > 0 {
                            new_last_match.patterns.push((form_match.id, inner_count));
                        }

                        LastMatch {
                            patterns: Vec::from(last_match.patterns.get(1..).unwrap_or(&[])),
                            pos: last_end,
                        }
                    } else {
                        LastMatch::default()
                    };

                    // This inner vector represents all the places that were poked
                    // within the matched range.
                    let pokes = form_match.recur_match(lines, &mut ranges, info, pattern_slice).0;

                    // If the pattern wasn't exclusive, maybe some of its innards were?
                    if form_match.is_exclusive {
                        ranges_to_poke.push(range)
                    } else {
                        ranges_to_poke.extend(pokes);
                    }
                }
            }

            for &range_to_poke in &ranges_to_poke {
                ranges.poke(range_to_poke);
            }

            poked_ranges.extend(ranges_to_poke);
        }

        (poked_ranges, new_last_match)
    }

    fn color_text(
        &self, lines: &[TextLine], range: TagRange, end_ranges: LastMatch,
    ) -> (Vec<(LineInfo, usize)>, LastMatch) {
        let (start, end) = (range.start, range.end);

        let mut info: Vec<(LineInfo, usize)> =
            (start.row..=end.row).map(|l| (LineInfo::default(), l)).collect();

        let mut ranges = TagRangeList(vec![range]);

        // First, match according to what pattern_id was in the start of the line.
        let last_match = self.recur_match(lines, &mut ranges, &mut info, end_ranges).1;

        (info, last_match)
    }

    /// Returns a mutable reference to the `FormPattern` with the given id.
    fn search_for_id_mut(&mut self, id: u16) -> Option<&mut FormPattern> {
        if self.id == id {
            return Some(self);
        }

        for form_pattern in &mut self.form_matches {
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
#[derive(Debug, Default, Clone)]
struct LastMatch {
    /// The list of patterns that were last active and the number of recursions of each.
    patterns: Vec<(u16, usize)>,

    /// The last line that was checked.
    pos: TagPos,
}

/// The object responsible for matching `TextLine`s on `Text`.
#[derive(Clone)]
pub struct MatchManager {
    /// The forms for syntax highlighting.
    palette: FormPalette,

    // NOTE: This exists pretty much only for performance reasons. If you splice text that has no
    // pattern bounds in it, then you don't need to update the syntax highlighting of other lines,
    // (at least, not for that reason).
    /// The list of all patterns that are bounded.
    bounded_forms: Vec<Pattern>,

    /// The patterns associated with said forms.
    default: FormPattern,

    // This exists solely for pushing new `FormPatterns` into the `default_form` `FormPattern`
    // tree.
    /// The last id that was used for a `FormPattern`.
    last_id: u16,

    /// What pattern ids were used when last matching.
    last_match: LastMatch,
}

impl MatchManager {
    /// Returns a new instance of `TagManager`
    pub fn new() -> MatchManager {
        MatchManager {
            palette: FormPalette::default(),
            bounded_forms: Vec::new(),
            default: FormPattern::default(),
            last_id: 0,
            last_match: LastMatch {
                patterns: Vec::new(),
                pos: TagPos { row: 0, byte: 0, file_byte: 0 },
            },
        }
    }

    pub fn match_scroll(&mut self, lines: &[TextLine], end: TextPos) -> Vec<(LineInfo, usize)> {
        let end = TagPos::from_text(&lines[end.row], end);

        if end > self.last_match.pos {
            let range = TagRange { start: self.last_match.pos, end };

            self.generate_info(lines, range, range.end)
        } else {
            Vec::new()
        }
    }

    pub fn match_range(
        &mut self, lines: &[TextLine], range: TextRange, max_pos: TextPos,
    ) -> Vec<(LineInfo, usize)> {
        let max_pos = TagPos::from_text(&lines[max_pos.row], max_pos);

        let start = TagPos::from_text(&lines[range.start.row], range.start);
        let end = TagPos::from_text(&lines[range.end.row], range.end);
        let mut range = TagRange { start, end };

        let mut lines_iter = lines.iter().take(range.start.row).rev();
        while let Some(line) = lines_iter.next() {
            if line.info.ending_id != 0 {
                range.start.row -= 1;
                range.start.file_byte -= line.text().len();
            } else {
                break;
            }
        }

        let mut lines_iter = lines.iter().skip(range.end.row + 1);
        while let Some(line) = lines_iter.next() {
            if line.info.starting_id != 0 && range.end <= max_pos {
                range.end.row += 1;
                range.end.file_byte += line.text().len();
                range.end.byte = line.text().len();
            } else {
                break;
            }
        }

        self.generate_info(lines, range, max_pos)
    }

    fn generate_info(
        &mut self, lines: &[TextLine], range: TagRange, max_pos: TagPos,
    ) -> Vec<(LineInfo, usize)> {
        let (mut info, last_match) = self.default.color_text(lines, range, self.last_match.clone());
        self.last_match = last_match;

        if range.end.row < lines.len() - 1 {
            if info.last().unwrap().0.ending_id != lines[range.end.row + 1].info.starting_id {
                if self.last_match.pos >= max_pos {
                    return info;
                }

                let range = TagRange { start: self.last_match.pos, end: max_pos };
                let end_ranges = self.last_match.clone();

                let (new_info, end_ranges) = self.default.color_text(lines, range, end_ranges);

                self.last_match = end_ranges;

                info.extend(new_info);
            }
        }

        info
    }

    /// Pushes a new form pattern.
    ///
    /// Returns an id, for the purpose of subpattern matching.
    pub fn push_word(
        &mut self, matcher: Matcher, form_index: Option<u16>, is_exclusive: bool, id: u16,
    ) -> u16 {
        self.last_id += 1;

        let found_form_pattern = match self.default.search_for_id_mut(id) {
            Some(found_form_pattern) => found_form_pattern,
            None => todo!(),
        };

        let form_pattern = FormPattern {
            form: form_index,
            pattern: Pattern::Word(matcher),
            form_matches: Vec::new(),
            id: self.last_id,
            is_exclusive,
        };

        found_form_pattern.form_matches.push(form_pattern);

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

        let found_form_pattern = match self.default.search_for_id_mut(id) {
            Some(found_form_pattern) => found_form_pattern,
            None => todo!(),
        };

        let pattern =
            if start == end { Pattern::Bound(start) } else { Pattern::Bounds(start, end) };

        self.bounded_forms.push(pattern.clone());

        let form_pattern = FormPattern {
            form: form_index,
            pattern,
            form_matches: Vec::new(),
            id: self.last_id,
            is_exclusive,
        };

        found_form_pattern.form_matches.push(form_pattern);

        self.last_id
    }

    pub fn palette(&self) -> &FormPalette {
        &self.palette
    }
}

/// Internal method used only to shorten code in `make_form()`.
fn set_var<T>(is_set: &mut bool, var: &mut Option<T>, maybe_new: &Option<T>, is_final: bool)
where
    T: Clone,
{
    if let (Some(new_var), false) = (maybe_new, &is_set) {
        *var = Some(new_var.clone());
        if is_final {
            *is_set = true
        };
    }
}
