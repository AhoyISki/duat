use crate::{cursor::{FileCursor, FilePos}, file::{TextLine, File}, output::OutputArea};

pub struct Selection {
    anchor: FilePos,
    cursor: FileCursor,
    lenght: usize,
}

/// What action was done to the selection:
///
/// - `Insert` places text before the selection.
/// - `Append` places text after the selection.
/// - `Delete` deletes the characters in the selection.
enum Action {
    Insert(Insertion),
    Append(Insertion),
    Delete,
}

/// A point in the editing history, used for jumping back in time and undoing.
struct HistoryPoint {
    selections: Vec<Selection>,
    actions: Vec<Vec<Action>>,
}

/// An insertion of text.
#[derive(Clone, Debug)]
pub struct Insertion {
    lines: Vec<String>,
    has_new_line: bool,
}

impl Insertion {
    pub fn new(mut text: String) -> Insertion {
        Insertion {
            lines: {
                if text.chars().nth(0).unwrap() == '\n' {
                    text.remove(0);
                }
                text.lines().map(|c| c.to_string()).collect::<Vec<String>>()
            },
            has_new_line: text.chars().nth(0).expect("empty insertion") == '\n',
        }
    }
    ////////////////////////////////
    // Getters
    pub fn lines(&self) -> &Vec<String> {
        &self.lines
    }

    pub fn has_new_line(&self) -> bool {
        self.has_new_line
    }
}

impl HistoryPoint {
    pub fn apply<T: OutputArea>(&self, file: &mut File<T>) {
        
    }
}
