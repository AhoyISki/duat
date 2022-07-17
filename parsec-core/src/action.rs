use std::ops::RangeInclusive;

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
	pub fn contains(&self, other: TextRange) -> bool {
    	self.start <= other.start && self.end >= other.end
	}
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
    /// The range that the replaced text used to occupy.
    added_range: TextRange,
    /// The text that was replaced in this change.
    taken_text: Vec<String>,
    /// The new range that the added text occupies.
    taken_range: TextRange,
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
}

// Since the history gets deleted when moments prior to the current one are added, changes in file
// positions, (e.g. if lines get added before a change, moving the changed lines down the file), we
// don't need to care about changes to positions, and can splice in the same positions of the
// change. However, in the case of jumping, the new position needs to be calculated in order to
// jump to the correct location.
impl Change {
    /// Applies the change to the given text.
    fn apply(&self, lines: &mut Vec<TextLine>) {
        let edit_lines = lines[self.added_range.lines()].iter().map(|l| l.text()).collect();

        // Returns
        let (edit, _) = extend_edit(edit_lines, self.added_text.clone(), self.added_range);
        let edit: Vec<TextLine> = edit.iter().map(|l| TextLine::new(l)).collect();

        lines.splice(self.added_range.lines(), edit);
    }

    /// Undoes the change and returns the modified text.
    fn undo(&self, lines: &mut Vec<TextLine>) {
        let undo_lines = lines[self.taken_range.lines()].iter().map(|l| l.text()).collect();

        // Returns
        let (undo, _) = extend_edit(undo_lines, self.taken_text.clone(), self.added_range);
        let undo: Vec<TextLine> = undo.iter().map(|l| TextLine::new(l)).collect();

        lines.splice(self.added_range.lines(), undo);
    }
}

impl History {
    pub fn new() -> History {
        History { moments: vec![Moment { print_info: None, actions: Vec::new() }] }
    }

    /// Tries to "merge" the change with an already existing change. If that fails, pushes a
    /// bew chage.
    ///     
    /// Here, `edit_range` is the range in the text that will be replaced by the `edit`.
    pub fn add_change<T>(
        &mut self, lines: &mut Vec<TextLine>, edit: Vec<T>, edit_range: TextRange,
    ) -> TextRange
    where
        T: ToString,
    {
        let moment = self.moments.last_mut().unwrap();

        let mut edit: Vec<String> = edit.iter().map(|l| l.to_string()).collect();

        let edited_lines =
            lines[edit_range.lines()].iter().map(|l| l.text()).collect::<Vec<&str>>();

        let taken_text = if edited_lines.len() == 1 {
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
        let full_lines = full_lines.iter().map(|l| TextLine::new(l)).collect::<Vec<TextLine>>();

        if let Some(Action::Change(change)) = moment.actions.last_mut() {
            // When text gets appended directly to the end of the previous edit, we can merge it.
            if edit_range.start == change.added_range.end && edit_range.end == edit_range.start {
                // If the action isn't a change, appending a change to it makes no sense.
                // The first line of the edit goes into the last line of the previous edit.
                change.added_text.last_mut().unwrap().push_str(&edit[0]);
                // Subsequent lines are appended to the end of the previous edit.
                if edit.len() > 1 {
                    change.added_text.extend_from_slice(&edit[1..]);
                }
                // Only the end of the new text needs to change here.
                change.added_range.end = added_range.end;

                // The last line of `old_text` needs to be extended by the first line of the
                // removed text, since the first line involves no '\n'.
                change.taken_text.last_mut().unwrap().push_str(taken_text[0]);
                // Subsequent lines are then appended to the end of `old_text`.
                if taken_text.len() > 1 {
                    let taken_text =
                        taken_text[1..].iter().map(|l| l.to_string()).collect::<Vec<String>>();

                    change.taken_text.extend_from_slice(&taken_text);
                }

                lines.splice(edit_range.lines(), full_lines);
		        return added_range;

            // Specific case when the text new edit is within the range of the previous edit.
            // This happens, for example, when pressing backspace on text you just added.
            } else if change.added_range.contains(edit_range) {

                // The `edit_range` positions in relation to the change's `new_range`, so we can
                // splice the edit in.
                let start = edit_range.start - change.added_range.start;
                let end = edit_range.end - change.added_range.start;
                let relative_range = TextRange { start, end };

                if unsafe { crate::buffer::FOR_TEST } {
                    panic!("{:#?}", edit_range);
                }

                let old_added_lines = &change.added_text[relative_range.lines()];
                let str_vec = old_added_lines.iter().map(|l| l.as_str()).collect();

                let (change_added_text, change_range) = extend_edit(str_vec, edit, relative_range);
				change.added_text.splice(relative_range.lines(), change_added_text);

				// If I replace text with bigger text, `change_range.end` will be bigger, If I
				// replace text with smaller text, `edit_range.end` will be bigger. This takes into
				// account edits in any range in the original `change`.
                change.added_range.end += change_range.end - relative_range.end;

                lines.splice(edit_range.lines(), full_lines);
		        return added_range;
		    // In case the old change can be succesfully appended to the new edit.
		    // This happens, for example, after pressing backspace multiple times.
            } else if edit_range.end == change.added_range.start {
                edit.last_mut().unwrap().push_str(change.added_text.first().unwrap());
                if change.added_text.len() > 1 {
                    edit.extend_from_slice(&change.added_text[1..]);
                }

				change.added_text = edit;
				change.added_range.start = edit_range.start;

                lines.splice(edit_range.lines(), full_lines);
		        return added_range;
            }
        // In any other case, it's not really possible to merge the edits.
        }
        let taken_text = taken_text.iter().map(|l| l.to_string()).collect::<Vec<String>>();
        //panic!("{:#?}", added_range);

        let change = Change {
            added_text: edit,
            added_range,
            taken_range: edit_range,
            taken_text,
        };
        moment.actions.push(Action::Change(change));

        lines.splice(edit_range.lines(), full_lines);
        added_range
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
