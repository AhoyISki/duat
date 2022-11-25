//! Parsec's way of editing text.
//!
//! This module contains all the operations that deal with editing a file's contents. The edits
//! happen by taking a range of lines from the original file and replacing it by a vector of lines,
//! equivalent to the original set of lines with an edit applied to it. The vast majority of the
//! time, this just involves taking one original line and placing one character on it (typing).
//!
//! This module also deals with the history system and undoing/redoing changes. The history system
//! works like this:
//!
//! Each file's `History` has a list of `Moment`s, and each `Moment` has a list of `Change`s and
//! one `PrintInfo`. `Change`s are splices that contain the original text, the text that
//! was added, their respective ending positions in the file, and a starting position.
//!
//! Whenever you undo a `Moment`, all of its splices are reversed on the file, in reverse order
//! according to the starting position of each splice, and the file's `PrintInfo` is updated to the
//! `Moment`'s `PrintInfo`. We change the `PrintInfo` in order to send the user back to the position
//! he was in previously, as he can just look at the same place on the screen for the changes, which
//! I think of as much less jarring.
//!
//! Undoing/redoing `Moment`s also has the effect of moving all `FileCursor`s below the splice's
//! start to a new position, or creating a new `FileCursor` to take a change into effect. This has
//! some interesting implications. Since parsec wants to be able to emulate both vim and kakoune,
//! it needs to be able to adapt to both of its history systems.
//!
//! In vim, if you type text, move around, and type more text, all in insert mode, vim would
//! consider that to be 2 `Moment`s. To fully undo the action, you would have to press `u` twice.
//! Go ahead, try it. Parsec is consistent with this, you could make a history system that
//! considers any cursor movement to be a new `Moment`, and since all `Moment`s would only have 1
//! `Change`, multiple cursors would never happen by undoing/redoing, which is consistent with vim.
//!
//! In kakoune, if you do the same as in vim, and then undo, you will undo both actions at once,
//! and will now have two cursors. Parsec, again, can be consistent with this, you just have to put
//! both `Change`s in a single `Moment`, which is done by default.
//!
//! All this is to say that history management is an editor specific configuration. In vim, any
//! cursor movement should create a new `Moment`, in kakoune, any insertion of text is considered a
//! `Moment`, in most other text editors, a space, tab, new line, or movement, is what creates a
//! `Moment`, which is why `parsec-core` does not define how new moments are created.
use std::ops::RangeInclusive;

use crate::{
    cursor::{relative_add, TextPos, SpliceAdder},
    file::TextLine,
    get_byte_at_col,
    layout::file_widget::PrintInfo, log_info,
};

/// A range of `chars` in the file, that is, not bytes.
#[derive(Debug, Clone, Copy)]
pub struct TextRange {
    pub start: TextPos,
    pub end: TextPos,
}

impl TextRange {
    /// Returns a range with all the lines involved in the edit.
    pub fn lines(&self) -> RangeInclusive<usize> {
        self.start.row..=self.end.row
    }

    /// Returns true if the other range is contained within this one.
    pub fn contains_range(&self, other: TextRange) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}

/// A range describing a splice operation;
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Splice {
    /// The start of both texts.
    pub(crate) start: TextPos,
    /// The end of the taken text.
    pub(crate) taken_end: TextPos,
    /// The end of the added text.
    pub(crate) added_end: TextPos,
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

    pub fn added_range(&self) -> TextRange {
        TextRange { start: self.start, end: self.added_end }
    }

    pub fn taken_range(&self) -> TextRange {
        TextRange { start: self.start, end: self.taken_end }
    }

    pub fn calibrate(&mut self, splice: &Splice) {
        self.start.calibrate(splice);
        self.taken_end.calibrate(splice);
        self.added_end.calibrate(splice);
    }

    pub fn calibrate_on_adder(&mut self, splice_adder: &SpliceAdder) {
        for pos in [&mut self.start, &mut self.added_end, &mut self.taken_end] {
            relative_add(pos, &splice_adder);
        }
    }

    pub fn reverse(&self) -> Splice {
        Splice { added_end: self.taken_end, taken_end: self.added_end, ..*self }
    }
}

/// A change in a file, empty vectors indicate a pure insertion or deletion.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change {
    /// The splice involving the two texts.
    pub splice: Splice,

    /// The text that was added in this change.
    pub added_text: Vec<String>,

    /// The text that was replaced in this change.
    pub taken_text: Vec<String>,
}

impl Change {
    pub fn new(lines: &Vec<String>, range: TextRange, text: &Vec<TextLine>) -> Self {
        let mut end = range.start;

        end.row += lines.len() - 1;
        end.byte += lines.iter().map(|l| l.len()).sum::<usize>();
        end.col = if lines.len() == 1 {
            range.start.col + lines[0].chars().count()
        } else {
            lines.last().unwrap().chars().count()
        };

        let taken_text = get_text_in_range(text, range);
        let splice = Splice { start: range.start, taken_end: range.end, added_end: end };

        Change { added_text: lines.clone(), taken_text, splice }
    }

    /// Applies the change to the given text.
    pub fn apply(&self, lines: &mut Vec<TextLine>) {
        let taken_range = TextRange { start: self.splice.start, end: self.splice.taken_end };

        // The added lines where taken_range resides.
        let edit_lines = lines[taken_range.lines()].iter().map(|l| l.text().to_string()).collect();

        let new = extend_edit(edit_lines, self.added_text.clone(), taken_range).0;
        let new: Vec<TextLine> = new.iter().map(|l| TextLine::new(l.clone())).collect();

        lines.splice(taken_range.lines(), new);
    }

    /// Undoes the change and returns the modified text.
    pub fn undo(&self, lines: &mut Vec<TextLine>) {
        let added_range = TextRange { start: self.splice.start, end: self.splice.added_end };

        // The lines where `added_range` resides.
        let undo_lines = lines[added_range.lines()].iter().map(|l| l.text().to_string()).collect();

        let new = extend_edit(undo_lines, self.taken_text.clone(), added_range).0;
        let new: Vec<TextLine> = new.iter().map(|l| TextLine::new(l.clone())).collect();

        lines.splice(added_range.lines(), new);
    }
}

/// A moment in history, which may contain changes, or may just contain selections.
///
/// It also contains information about how to print the file, so that going back in time is less
/// jaring.
#[derive(Debug, Default)]
pub struct Moment {
    /// Where the file was printed at the time this moment happened.
    pub(crate) print_info: Option<PrintInfo>,
    /// A list of actions, which may be changes, or simply selections of text.
    pub(crate) changes: Vec<Change>,
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
    /// Returns a new instance of `History`.
    pub fn new() -> History {
        History { moments: Vec::new(), current_moment: 0, traveled_in_time: false }
    }

    /// Gets the current moment. Takes time travel into consideration.
    fn current_moment(&mut self) -> Option<&mut Moment> {
        self.moments.get_mut(self.current_moment - 1)
    }

    /// Tries to "merge" the change with an already existing change. If that fails, pushes a
    /// bew chage.
    ///     
    /// Here, `edit_range` is the range in the text that will be replaced by the `edit`.
    pub fn add_change(&mut self, change: &Change) {
        // Cut off any actions that take place after the current one. We don't really want trees.
        unsafe { self.moments.set_len(self.current_moment) };

        if let Some(moment) = self.current_moment() {
            moment.changes.push(change.clone());
        } else {
            self.new_moment(PrintInfo::default());
            self.moments.last_mut().unwrap().changes.push(change.clone());
        }
    }

    /// Declares that the current moment is complete and moves to the next one.
    pub fn new_moment(&mut self, print_info: PrintInfo) {
        // If the last moment in history is empty, we can keep using it.
        if self.current_moment().map_or(true, |m| !m.changes.is_empty()) {
            unsafe {
                self.moments.set_len(self.current_moment);
            }
            self.moments.last_mut().map(|m| m.print_info = Some(print_info));

            self.moments.push(Moment { print_info: None, changes: Vec::new() });
            self.current_moment += 1;
        }
    }

    /// Moves forwards in the timeline.
    pub fn move_forward(&mut self) -> Option<&Moment> {
        if self.current_moment == self.moments.len() {
            return None;
        } else {
            self.current_moment += 1;
            self.traveled_in_time = true;

            return Some(&self.moments[self.current_moment - 1]);
        }
    }

    /// Moves backwards in the timeline.
    pub fn move_backwards(&mut self) -> Option<&Moment> {
        if self.current_moment == 0 {
            None
        } else {
            self.current_moment -= 1;
            self.traveled_in_time = true;

            Some(&self.moments[self.current_moment])
        }
    }

    /// Sets the `PrintInfo` for the current `Moment`.
    pub fn set_print_info(&mut self, print_info: PrintInfo) {
        if let Some(moment) = self.current_moment() {
            log_info(format_args!("{:#?}", print_info));
            moment.print_info = Some(print_info);
        }
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
/// Returns an edit with the leftover original lines appended to it.
pub fn extend_edit(
    old: Vec<String>, mut edit: Vec<String>, range: TextRange,
) -> (Vec<String>, TextRange) {
    let start = range.start;

    let byte = start.byte + edit.iter().map(|l| l.len()).sum::<usize>();
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
            TextPos { row: start.row, byte, col: start.col + last_edit_len }
        } else {
            TextPos { row: start.row + edit.len() - 1, byte, col: last_edit_len }
        },
    };

    // The modified edit is what should be placed in the original vector of lines.
    (edit, added_range)
}

/// Gets the byte where a character starts on a given string.
pub fn get_byte(line: &str, col: usize) -> usize {
    line.char_indices().map(|(b, _)| b).nth(col).unwrap_or(line.len())
}

/// Returns the text in the given range of `TextLine`s.
pub fn get_text_in_range(text: &Vec<TextLine>, range: TextRange) -> Vec<String> {
    let mut lines = Vec::with_capacity(range.lines().count());
    let first_byte = get_byte_at_col(range.start.col, text[range.start.row].text()).unwrap();
    let last_byte = get_byte_at_col(range.end.col, text[range.end.row].text()).unwrap();

    if range.lines().count() == 1 {
        lines.push(text[range.start.row].text()[first_byte..last_byte].to_string());
    } else {
        lines.push(text[range.start.row].text()[first_byte..].to_string());
        for line in text.iter().take(range.end.row - 1).skip(range.start.row + 1) {
            lines.push(line.text().to_string());
        }
        lines.push(text.get(range.end.row).unwrap().text()[..last_byte].to_string());
    }

    lines
}
