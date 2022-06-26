use crate::{
    cursor::{FilePos, FileCursor},
    file::{TextLine, File},
    output::OutputArea
};

#[derive(Debug)]
pub struct Selection {
    start: FilePos,
    end: FilePos,
    content: Vec<String>,
}

impl Selection {
    /// Returns a new instance of Selection.
    pub fn new(start: FilePos, end: FilePos, lines: &Vec<TextLine>) -> Selection {
        let starting_line = lines.get(start.line).unwrap();

        let content = if start.line == end.line {
            vec![
                starting_line.text().get(start.col..end.col).unwrap()
                    .iter().map(|c| c.ch.content().clone()).collect()
            ]
        } else {
            let starting_slice = starting_line.text().get(start.col..).unwrap()
                .iter().map(|c| c.ch.content().clone()).collect();

            let ending_line = lines.get(end.line).unwrap();
            let ending_slice = ending_line.text().get(..end.col).unwrap()
                .iter().map(|c| c.ch.content().clone()).collect();

            let mut content = Vec::new();

            content.push(starting_slice);

            for line in lines.get((start.line + 1)..end.line).unwrap() {
                content.push(line.text().iter().map(|c| c.ch.content().clone()).collect());
            }

            content.push(ending_slice);

            content
        };

        Selection { start, end, content }
    }

    /// Returns a new selection, given some text. Used on insertions, for efficiency.
    pub fn from_text(start: FilePos, end: FilePos, content: Vec<String>) -> Selection {
        Selection { start, end, content }
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn start(&self) -> FilePos { self.start }

    pub fn end(&self) -> FilePos { self.end }
}

/// A point in the editing history, used for jumping back in time and undoing.
pub struct Moment {
    insertions: Vec<Selection>,
    deletions: Vec<Selection>,
    cursors: Vec<FileCursor>,
}

struct Action {
    main_selection: Selection,
    secondary_selections: Vec<Selection>
}

impl Moment {
    pub fn add_insertion(&mut self, selection: Selection) {
        if selection.start == 
        
    }

    pub fn apply<T: OutputArea>(&self, file: &mut File<T>) {
        
    }
}
