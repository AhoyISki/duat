use std::{
    cmp::{max, min},
    ops::RangeInclusive,
};

use crate::{cursor::TextPos, file::TextLine, output::PrintInfo};

/// A range of `chars` in the file, that is, not bytes.
#[derive(Debug, Clone, Copy)]
pub struct TextRange {
    pub start: TextPos,
    pub end: TextPos,
}

impl TextRange {
    /// Returns the lines involved in the range.
    pub fn lines(&self) -> RangeInclusive<usize> {
        self.start.line..=self.end.line
    }

    /// Returns true if the other range is contained within this one.
    pub fn contains_range(&self, other: TextRange) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}

/// A range describing a splice operation;
#[derive(Debug, Clone, Copy)]
pub struct Splice {
    /// The start of both texts.
    pub start: TextPos,
    /// The end of the added text.
    pub added_end: TextPos,
    /// The end of the taken text.
    pub taken_end: TextPos,
}

#[derive(Debug)]
pub struct Selection {
    pub cursor_pos: TextPos,
    pub anchor_pos: TextPos,
}

#[derive(Debug)]
enum Action {
    Jump(Selection),
    Change(Change),
}

/// A change in a file, empty vectors indicate a pure insertion or deletion.
#[derive(Debug)]
struct Change {
    /// The text that was added in this change.
    added_text: Vec<String>,

    /// The text that was replaced in this change.
    taken_text: Vec<String>,

    /// The splice involving the two texts.
    splice: Splice,
}

/// A moment in history, which may contain changes, or may just contain selections.
///
/// It also contains information about how to print the file, so that going back in time is less
/// jaring.
#[derive(Debug)]
pub struct Moment {
    /// Where the file was printed at the time this moment happened.
    print_info: Option<PrintInfo>,
    /// A list of actions, which may be changes, or simply selections of text.
    actions: Vec<Action>,
}

/// The history of edits, contains all moments.
#[derive(Debug)]
pub struct History {
    /// The list of moments in this file's editing history.
    pub moments: Vec<Moment>,

    current_moment: usize,
}

// Since the history gets deleted when moments prior to the current one are added, changes in file
// positions, (e.g. if lines get added before a change, moving the changed lines down the file), we
// don't need to care about changes to positions, and can splice in the same positions of the
// change. However, in the case of jumping, the new position needs to be calculated in order to
// jump to the correct location.
impl Change {
    /// Applies the change to the given text.
    fn apply(&self, lines: &Vec<TextLine>) -> Vec<String> {
        let taken_range = TextRange { start: self.splice.start, end: self.splice.taken_end };

        let edit_lines = lines[taken_range.lines()].iter().map(|l| l.text()).collect();

        extend_edit(edit_lines, self.added_text.clone(), taken_range).0
    }

    /// Undoes the change and returns the modified text.
    fn undo(&self, lines: &Vec<TextLine>) -> Vec<String> {
        let added_range = TextRange { start: self.splice.start, end: self.splice.added_end };

        let undo_lines = lines[added_range.lines()].iter().map(|l| l.text()).collect();

        extend_edit(undo_lines, self.taken_text.clone(), added_range).0
    }
}

impl History {
    pub fn new() -> History {
        History {
            moments: vec![Moment { print_info: None, actions: Vec::new() }],
            current_moment: 0,
        }
    }

    /// Tries to "merge" the change with an already existing change. If that fails, pushes a
    /// bew chage.
    ///     
    /// Here, `edit_range` is the range in the text that will be replaced by the `edit`.
    pub fn add_change<T>(
        &mut self, lines: &Vec<TextLine>, edit: Vec<T>, edit_range: TextRange,
    ) -> (Vec<String>, TextRange)
    where
        T: ToString,
    {
        let moment = self.moments.last_mut().unwrap();

        let edit: Vec<String> = edit.iter().map(|l| l.to_string()).collect();

        let edited_lines: Vec<&str> = lines[edit_range.lines()].iter().map(|l| l.text()).collect();

        let mut taken_text = if edited_lines.len() == 1 {
            let line = edited_lines.first().unwrap();

            let first_byte = get_byte(line, edit_range.start.col);
            let last_byte = get_byte(line, edit_range.end.col);

            vec![&line[first_byte..last_byte]]
        } else {
            let first_line = edited_lines.first().unwrap();
            let first_byte = get_byte(first_line, edit_range.start.col);

            let last_line = edited_lines.last().unwrap();
            let last_byte = get_byte(last_line, edit_range.end.col);

            let mut taken_text = Vec::with_capacity(edited_lines.len());

            taken_text.push(&first_line[first_byte..]);
            taken_text.extend_from_slice(&edited_lines[1..(edited_lines.len() - 1)]);
            taken_text.push(&last_line[..last_byte]);

            taken_text
        };

        // Here, `full_lines` is simply the edit with the start of the of the first line in the
        // old range, and the end of the last line in the old range. That way, we can simply splice
        // it back on to the original `Vec<TextLine>`, with no further changes necessary.
        let (full_lines, added_range) = extend_edit(edited_lines, edit.clone(), edit_range);

        // There are two scenarios in which we can append an edit to an existing change:
        // 1- The edit happens within the bounds of the change.
        // 2- The edit happens exactly before the change.
        //
        // In practice, the only difference between the two is that the second one requires that we
        // change `change.taken_text` and `change.splice_start`.
        if let Some(Action::Change(change)) = moment.actions.last_mut() {
            let change_added_range =
                TextRange { start: change.splice.start, end: change.splice.added_end };

            if change_added_range.contains_range(edit_range)
                || change.splice.start == edit_range.end
            {
                // The `edit_range` positions in relation to the change's `new_range`, so we can
                // splice the edit in.
                let start = edit_range.start - change.splice.start;
                let end = edit_range.end - change.splice.start;
                let rel_edit_range = TextRange { start, end };

                let old_added_lines = &change.added_text[rel_edit_range.lines()];
                let str_vec = old_added_lines.iter().map(|l| l.as_str()).collect();

                let (change_lines, rel_added_range) = extend_edit(str_vec, edit, rel_edit_range);

                // The absolute positions of `rel_added_range`.
                let start = rel_added_range.start + change.splice.start;
                let end = rel_added_range.end + change.splice.start;
                let added_range = TextRange { start, end };

                // If the edit happens exactly before the previous change, we need to add the taken
                // text and move `change.start`.
                if change.splice.start == edit_range.end {
                    change.taken_text[0].insert_str(0, taken_text.pop().unwrap());
                    let taken_text =
                        taken_text.iter().map(|l| l.to_string()).collect::<Vec<String>>();

                    change.taken_text.splice(0..0, taken_text);

                    change.splice.start = edit_range.start;
                }

                // If I replace text with bigger text, `change_range.end` will be bigger, If I
                // replace text with smaller text, `edit_range.end` will be bigger. This takes into
                // account edits in any range in the original `change`.
                change.splice.added_end.line += added_range.end.line - edit_range.end.line;
                if edit_range.end.line == change.splice.added_end.line {
                    change.splice.added_end.col =
                        change_lines.last().unwrap().chars().count() + change.splice.start.col;
                }

                change.added_text.splice(rel_edit_range.lines(), change_lines);

                return (full_lines, added_range);
            }
        }
        let taken_text: Vec<String> = taken_text.iter().map(|l| l.to_string()).collect();

        let change = Change {
            added_text: edit,
            taken_text,
            splice: Splice {
                start: edit_range.start,
                added_end: added_range.end,
                taken_end: edit_range.end,
            },
        };
        moment.actions.push(Action::Change(change));

        (full_lines, added_range)
    }

    /// Declares that the current moment is complete and moving to the next one.
    pub fn new_moment(&mut self, print_info: PrintInfo) {
        self.moments.last_mut().unwrap().print_info = Some(print_info);

        self.moments.push(Moment { print_info: None, actions: Vec::new() });
        self.current_moment += 1;
    }

    /// Undoes the changes of the last moment and returns a vector containing range changes.
    pub fn undo(&mut self, lines: &Vec<TextLine>) -> (Vec<(Vec<String>, Splice)>, PrintInfo) {
        let mut range_vec = Vec::new();

        // TODO: make this return an error for when we're at the first moment.
        self.current_moment = self.current_moment.saturating_sub(1);

        for action in &self.moments[self.current_moment].actions {
            if let Action::Change(change) = action {
                let undo_lines = change.undo(lines);

                range_vec.push((undo_lines, change.splice));
            }
        }

        (range_vec, self.moments[self.current_moment].print_info.unwrap())
    }

    /// Undoes the changes of the last moment and returns a vector containing range changes.
    pub fn redo(&mut self, lines: &Vec<TextLine>) -> (Vec<(Vec<String>, Splice)>, PrintInfo) {
        let mut range_vec = Vec::new();

        for action in &self.moments[self.current_moment].actions {
            if let Action::Change(change) = action {
                let redo_lines = change.apply(lines);

                range_vec.push((redo_lines, change.splice));
            }
        }
        let print_info = self.moments[self.current_moment].print_info.unwrap();

        // TODO: make this return an error for when we're at the last moment.
        self.current_moment = min(self.current_moment + 1, self.moments.len());

        (range_vec, print_info)
    }
}

// Say you have text like this:
//
// ####$###########
// ########
// ###########$##
//
// And you want to replace the text at the positions $<=x<$ with %%%%%.
// You can just take the first part of the first line, and the last part of the last line and insert
// them on %%%%%. This way, you'll get:
//
// ####%%%%%$##
//
// And your edit is complete.
/// Returns an edit with the part of the original lines appended to it.
pub fn extend_edit(
    old: Vec<&str>, mut edit: Vec<String>, range: TextRange,
) -> (Vec<String>, TextRange) {
    let start = range.start;

    let last_edit_len = edit.last().unwrap().chars().count();

    // Where the byte of `range.start` is.
    let first_line = old.first().unwrap();
    let first_byte = get_byte(first_line, range.start.col);

    // Inserting the beginning of the first original line into the edit.
    let first_edit_line = edit.first_mut().unwrap();
    first_edit_line.insert_str(0, &first_line[..first_byte]);

    // Where the byte of `range.end` is.
    let last_line = old.last().unwrap();
    let last_byte = get_byte(last_line, range.end.col);

    // Appending the end of the last original line into the edit.
    let last_edit_line = edit.last_mut().unwrap();
    last_edit_line.push_str(&last_line[last_byte..]);

    let added_range = TextRange {
        start: range.start,
        end: if edit.len() == 1 {
            TextPos { line: start.line, col: start.col + last_edit_len }
        } else {
            TextPos { line: start.line + edit.len() - 1, col: last_edit_len }
        },
    };

    // The modified edit is what should be placed in the original vector of lines.
    (edit, added_range)
}

/// Gets the byte where a character starts on a given string.
pub fn get_byte(line: &str, col: usize) -> usize {
    line.char_indices().map(|(b, _)| b).nth(col).unwrap_or(line.len())
}
