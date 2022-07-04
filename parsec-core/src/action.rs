use crate::{
    cursor::TextPos,
    file::{TextLine, simple_splice_lines},
    output::{TextChar, PrintInfo},
    convert_to_text_chars
};

#[derive(Debug, Clone, Copy)]
pub struct TextRange {
    pub start: TextPos,
    pub end: TextPos,
}

pub struct Selection {
    pub cursor_pos: TextPos,
    pub anchor_pos: TextPos,
}

enum Action {
    Jump(Selection),
    Change(Change)
}

/// A change in a file, empty vectors indicate a pure insertion or deletion.
struct Change {
    /// The text that was added in this change.

    new_text: Vec<Vec<TextChar>>,
    /// The new range that the added text occupies.
    new_range: TextRange,
	/// The text that was replaced in this change.
    old_text: Vec<Vec<TextChar>>,
    /// The range that the replaced text used to occupy.
    old_range: TextRange,
}

/// A moment in history, which may contain changes, or may just contain selections.
///
/// It also contains information about how to print the file, so that going back in time is less
/// jaring.
struct Moment {
    /// Where the file was printed at the time this moment happened.
    print_info: Option<PrintInfo>,
    /// A list of actions, which may be changes, or simply selections of text.
    actions: Vec<Action>, 
}

/// The history of edits, contains all moments.
pub struct History {
    /// The list of moments in this file's editing history.
    moments: Vec<Moment>,
    // TODO: turn this into a vector.
    editing_pos: TextPos,
}

// Since the history gets deleted when moments prior to the current one are added, changes in file
// positions, (e.g. if lines get added before a change, moving the changed lines down the file), we
// don't need to care about changes to positions, and can splice in the same positions of the
// change. However, in the case of jumping, the new position needs to be calculated in order to
// jump to the correct location.
impl Change {
    /// Applies the change and returns the modified text..
    fn apply(&self, lines: &mut Vec<TextLine>) {
        // Replaces the text in the old range with the new text.
        simple_splice_lines(lines, self.new_text.clone(), self.old_range);
    }

	/// Undoes the change and returns the modified text.
    fn undo(&self, lines: &mut Vec<TextLine>) {
        // Replaces the text in the new range with the old text.
        simple_splice_lines(lines, self.old_text.clone(), self.new_range);
    }
}

impl History {
    pub fn new() -> History {
        History {
            moments: vec![Moment { print_info: None, actions: Vec::new() }],
            editing_pos: TextPos { line: 0, col: 0 },
        }
    }

    /// Tries to "merge" the change with an already existing change. If that fails, pushes a
    /// bew chage.
    pub fn add_change<T>(&mut self, lines: &mut Vec<TextLine>, edit: Vec<T>, range: TextRange)
        -> TextRange
    where
        T: ToString {
        let moment = self.moments.last_mut().unwrap();

        let edit: Vec<Vec<TextChar>> =
            edit.iter().map(|l| convert_to_text_chars(l.to_string())).collect();

    	let (old_text, new_range) = simple_splice_lines(lines, edit.clone(), range);

        if range.start == self.editing_pos {
            // If the action isn't a change, appending a change to it makes no sense.
            if let Some(Action::Change(change)) = moment.actions.last_mut() {
                // The change will be extended, this happens with regular typing, for example.
				change.new_text.extend_from_slice(edit.as_slice());
				// Only the end of the new text needs to change here.
				change.new_range.end = new_range.end;

				// The last line of `old_text` needs to be extended by the first line of the
				// removed text, since the first line involves no '\n'.
				change.old_text.last_mut().unwrap().extend_from_slice(old_text.get(0).unwrap());
				// The subsequent lines are then appended to the end of `old_text`.
				change.old_text.extend_from_slice(old_text.get(1..).unwrap());
				// Only the end of the range needs to change.
				change.old_range.end = range.end;
            }
        // If the position is not the same, the new change will be incompatible with the old one,
        // thus, we need to push a new change.
        } else {
            let change = Change { new_text: edit, new_range, old_text, old_range: range };
            moment.actions.push(Action::Change(change));

            self.editing_pos = new_range.end;
        }

    	new_range
    }
}

