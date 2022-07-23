//! The editor's way of editing text.
//!
//! This module contains all the operations that deal with editing a file's contents. The edits
//! happen by taking a range of lines from the original file and replacing it by a vector of lines,
//! equivalent to the original set of lines with an edit applied to it. The vast majority of the
//! time, this just involves taking the original string and placing one character on it (typing).
//!
//! This module also deals with the history system and undoing/redoing changes. The history system
//! works like this:
//!
//! Each file's `History` has a list of `Moment`s, and each `Moment` has a list of `Change`s and one
//! `PrintInfo`. `Change`s are "simple" splices that contain the original text, the text that was
//! added, their respective ending positions in the file, and a starting position.
//!
//! Whenever you undo a `Moment`, all of its splices are reversed on the file, sequentially,
//! and the file's `PrintInfo` is updated to the `Moment`'s `PrintInfo`. We change the `PrintInfo`
//! in order to send the user back to the position he was in previously, as he can just look at the
//! same place on the screen for the changes, which I think of as much less jarring.
//!
//! Undoing/redoing `Moment`s also has the effect of moving all `FileCursor`s below the splice's
//! start to a new position, or creating a new `FileCursor` to take a change into effect. This has
//! some interesting implications. Since parsec wants to be able to emulate both vim and kakoune, it
//! needs to be able to adapt to both of its history systems.
//!
//! In vim, if you type text, move around, and type more text, all in insert mode, vim would
//! consider that to be 2 `Moment`s. To fully undo the action, you would have to press `u` twice.
//! Go ahead, try it. Parsec is consistent with this, you could make a history system that considers
//! any cursor movement to be a new `Moment`, and since all `Moment`s would only have 1 `Change`,
//! multiple cursors would never happen by undoing/redoing, which is consistent with vim.
//!
//! In kakoune, if you do the same as in vim, and then undo, you will undo both actions at once, and
//! will now have two cursors. Parsec, again, can be consistent with this, you just have to put both
//! `Change`s in a single `Moment`, which is done by default.
//!
//! All this is to say that history management is an editor specific configuration. In vim, any
//! cursor movement should create a new `Moment`, in kakoune, any insertion of text is considered a
//! `Moment`, in most other text editors, a space, tab, or new line, is what creates a `Moment`.
//! Which is why `parsec-core` does not define how new moments are created.
use std::ops::RangeInclusive;

use crate::{cursor::TextPos, file::TextLine, output::PrintInfo};

/// A range of `chars` in the file, that is, not bytes.
#[derive(Debug, Clone, Copy)]
pub struct TextRange {
    pub start: TextPos,
    pub end: TextPos,
}

impl TextRange {
    /// Returns a range with all the lines involved in the edit.
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
    start: TextPos,
    /// The end of the added text.
    added_end: TextPos,
    /// The end of the taken text.
    taken_end: TextPos,
}

impl Splice {
    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn start(&self) -> TextPos {
        self.start
    }
    pub fn added_end(&self) -> TextPos {
        self.added_end
    }

    pub fn taken_end(&self) -> TextPos {
        self.taken_end
    }
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

impl Change {
    /// Applies the change to the given text.
    fn apply(&self, lines: &mut Vec<TextLine>) {
        let taken_range = TextRange { start: self.splice.start, end: self.splice.taken_end };

        let edit_lines = lines[taken_range.lines()].iter().map(|l| l.text()).collect();

        let full_lines = extend_edit(edit_lines, self.added_text.clone(), taken_range).0;
        let full_lines: Vec<TextLine> = full_lines.iter().map(|l| TextLine::new(l)).collect();

		lines.splice(taken_range.lines(), full_lines);
    }

    /// Undoes the change and returns the modified text.
    fn undo(&self, lines: &mut Vec<TextLine>) {
        let added_range = TextRange { start: self.splice.start, end: self.splice.added_end };

		// The lines where `added_range` resides.
        let undo_lines = lines[added_range.lines()].iter().map(|l| l.text()).collect();

        let full_lines = extend_edit(undo_lines, self.taken_text.clone(), added_range).0;
        let full_lines: Vec<TextLine> = full_lines.iter().map(|l| TextLine::new(l)).collect();

		lines.splice(added_range.lines(), full_lines);
    }
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
    changes: Vec<Change>,
}

/// The history of edits, contains all moments.
#[derive(Debug)]
pub struct History {
    /// The list of moments in this file's editing history.
    moments: Vec<Moment>,
    /// The currently active moment.
    current_moment: usize,

	// NOTE: Will almost definitely get rid of this.
    /// Wether or not the user has undone/redone past actions.
    traveled_in_time: bool,
}

impl History {
    pub fn new() -> History {
        History {
            moments: vec![Moment { print_info: None, changes: Vec::new() }],
            current_moment: 1,
            traveled_in_time: false,
        }
    }

    /// Tries to "merge" the change with an already existing change. If that fails, pushes a
    /// bew chage.
    ///     
    /// Here, `edit_range` is the range in the text that will be replaced by the `edit`.
    pub fn add_change<T>(
        &mut self, lines: &mut Vec<TextLine>, edit: Vec<T>, edit_range: TextRange,
    ) -> (Vec<String>, TextRange)
    where
        T: ToString,
    {
        // Cut off any actions that take place after the current one. We don't really want trees.
        unsafe { self.moments.set_len(self.current_moment) };

        // Moments cannot be expanded if you have undone actions.
        let moment = if self.traveled_in_time {
            self.current_moment += 1;
            self.traveled_in_time = false;

            self.moments.push(Moment { changes: Vec::new(), print_info: None });
            self.moments.last_mut().unwrap()
        } else {
            match self.moments.last_mut() {
                Some(moment) => moment,
                None => {
                    self.current_moment += 1;

                    self.moments.push(Moment { changes: Vec::new(), print_info: None });
                    self.moments.last_mut().unwrap()
                }
            }
        };

        let mut edit: Vec<String> = edit.iter().map(|l| l.to_string()).collect();
        // Insert a '\n' at the end of every line, skipping the last one.
        edit.iter_mut().rev().skip(1).for_each(|l| l.push('\n'));

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
        if let Some(change) = moment.changes.last_mut() {
            let change_added_range =
                TextRange { start: change.splice.start, end: change.splice.added_end };

            if change_added_range.contains_range(edit_range)
                || change.splice.start == edit_range.end
            {
                let old_splice_start = change.splice.start;

                // If the edit happens exactly before the previous change, we need to add the taken
                // text and move `change.start`.
                if change.splice.start == edit_range.end {
                    change.taken_text[0].insert_str(0, taken_text.pop().unwrap());
                    let taken_text =
                        taken_text.iter().map(|l| l.to_string()).collect::<Vec<String>>();

                    change.taken_text.splice(0..0, taken_text);

                    change.splice.start = edit_range.start;
                }

				// Since the old change doesn't necessarily start at the 0th line, we need to get
				// the relative position of `edit_range`, in order to splice correctly.
				// If the edit doesn't change the line at `change.splice.start`, its column does not
				// matter when calculating `rel_edit_range`s position.
                let mut start = edit_range.start.hor_sub(change.splice.start);
                let mut end = edit_range.end.hor_sub(change.splice.start);

				// More relative positioning.
                start.line -= change.splice.start.line;
                end.line -= change.splice.start.line;

                let rel_edit_range = TextRange { start, end };

				// If this is the case, the new splice start could be anywhere above the old one,
				// And you can't really take lines from `change.added_text` with a negative index.
				let rel_lines = if old_splice_start == edit_range.end {
    				0..=0
        		// If it is not the case, we know the edit range is contained in
        		// `change.added_range`, and as such, we can splice regularly.
				} else {
    				rel_edit_range.lines()
				};

                let old_added_lines = &change.added_text[rel_lines.clone()];
                let str_vec = old_added_lines.iter().map(|l| l.as_str()).collect();

				// Here, `edit_lines` is the original `edit`, but filled in with the text from
				// `str_vec`, the exact same way `extend_edit()` modifies a range in the file.
                let (edit_lines, mut rel_added_range) = extend_edit(str_vec, edit, rel_edit_range);
                change.added_text.splice(rel_lines, edit_lines);

                // The absolute positions of `rel_added_range`.
                // Since this is already a relative position, we add `change.splice.start.line` to
                // make it absolute.
                rel_added_range.start.line += change.splice.start.line;
                rel_added_range.end.line += change.splice.start.line;

                let start = rel_added_range.start.hor_add(change.splice.start);
                let end = rel_added_range.end.hor_add(change.splice.start);
                let added_range = TextRange { start, end };

				// If this is the case, the column of `edit_range` is taken into account.
				// Any text that is added, no matter how many lines it has, will inevitably have its
				// end in the same line as `change.added_range`, which is why we don't check.
                if change.splice.added_end.line == edit_range.end.line {
                    change.splice.added_end -= edit_range.end;
                    change.splice.added_end += added_range.end;
                // The opposite is the case here.
                } else {
                    change.splice.added_end.line -= edit_range.end.line;
                    change.splice.added_end.line += added_range.end.line;
                }

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

        moment.changes.push(change);

		if unsafe { crate::FOR_TEST } { panic!("{:#?}", self.moments) }

        (full_lines, added_range)
    }

    /// Declares that the current moment is complete and moves to the next one.
    pub fn new_moment(&mut self, print_info: PrintInfo) {
        // If the last moment in history is empty, we can keep using it.
        if !self.moments.last().unwrap().changes.is_empty() {
            self.moments.last_mut().unwrap().print_info = Some(print_info);

            self.moments.push(Moment { print_info: None, changes: Vec::new() });
            self.current_moment += 1;
        }
    }

    /// Undoes the changes of the last moment and returns a vector containing range changes.
    pub fn undo(
        &mut self, lines: &mut Vec<TextLine>,
    ) -> Option<(Vec<Splice>, Option<PrintInfo>)> {
        if self.current_moment == 0 {
            return None;
        }

        // TODO: make this return an error for when we're at the first moment.
        self.current_moment = self.current_moment.saturating_sub(1);
        self.traveled_in_time = true;

		let moment = &self.moments[self.current_moment];
        let splices = moment.changes.iter().map(|c| c.splice).collect();

        for change in moment.changes.iter().rev() {
            change.undo(lines);
        }

        Some((splices, self.moments[self.current_moment].print_info))
    }

    // TODO: Return a custom Result instead.
    /// Undoes the changes of the last moment and returns a vector containing range changes.
    pub fn redo(
        &mut self, lines: &mut Vec<TextLine>,
    ) -> Option<(Vec<Splice>, Option<PrintInfo>)> {
        // TODO: make this return an error for when we're at the last moment.
        if self.current_moment == self.moments.len() {
            return None;
        }
        self.traveled_in_time = true;

		let moment = &self.moments[self.current_moment];
        let mut splices = Vec::with_capacity(moment.changes.len());

        for change in &moment.changes {
            change.apply(lines);

            splices.push(change.splice);
        }

        self.current_moment += 1;

        Some((splices, self.moments[self.current_moment].print_info))
    }
}

// Say you have text like this:
//
// ####$###########
// ########
// ###########§##
//
// And you want to replace the text at the positions $<=x<§ with %%%%%.
// You can just take the first part of the first line, and the last part of the last line and
// insert them on %%%%%. This way, you'll get:
//
// ####%%%%%§##
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
    let edit_len = edit.len();
    let last_edit_line = edit.last_mut().unwrap();
    last_edit_line.push_str(&last_line[last_byte..]);

    let added_range = TextRange {
        start: range.start,
        end: if edit_len == 1 {
            let col = start.col + last_edit_len;
            let byte = get_byte(last_edit_line, col);
            TextPos { line: start.line, col, byte }
        } else {
            let col = last_edit_len;
            let byte = get_byte(last_edit_line, col);
            TextPos { line: start.line + edit.len() - 1, col: last_edit_len, byte }
        },
    };

    // The modified edit is what should be placed in the original vector of lines.
    (edit, added_range)
}

/// Gets the byte where a character starts on a given string.
pub fn get_byte(line: &str, col: usize) -> usize {
    line.char_indices().map(|(b, _)| b).nth(col).unwrap_or(line.len())
}
