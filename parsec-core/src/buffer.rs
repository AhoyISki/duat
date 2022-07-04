use std::{
    path::PathBuf,
    fs,
    cmp::*,
};

use crate::{
    map_actions,
    impl_input_handler,
    config::{FileOptions, LineNumbers},
    input::{InputHandler, ModeList},
    output::{OutputPos, OutputArea, TextChar},
    file::{TextLine, File},
    cursor::TextPos,
    action::TextRange,
};

use crossterm::{
    event::{
        KeyEvent,
        KeyCode,
        KeyModifiers
    },
    style::{
        Stylize,
        Attribute::Reverse
    }
};

// NOTE: This struct should strive to be completely UI agnostic, i.e., it should work wether the
// app is used in a terminal or in a GUI.
pub struct Buffer<T: OutputArea> {
    /// The contents of the file
    pub file: File<T>,

    // Where exactly on the screen the origin and end of the area are placed is not important here.
    /// The area allocated to the status line.
    pub status_line_area: T,
    /// The area allocated to the line numbers.
    pub line_num_area: T,

    /// List of mapped modes for file editing.
    mappings: ModeList<Buffer<T>>,

}

impl<T: OutputArea> Buffer<T> {
    /// Returns a new instance of ContentArea
    pub fn new(mut area: T, path: PathBuf, options: FileOptions) -> Buffer<T> {
        // TODO: In the future, this will not panic!
        // TODO: Move this to a more general file.
        let file = fs::read_to_string(path).expect("file not found");

        let line_num_area;

        let mut file_handler = Buffer {
            file: {
                let lines: Vec<TextLine> =
                    file.lines().map(|l| TextLine::new(l, &options.tabs)).collect();

                let mut file_area = area.partition_y(area.height() - 2);

                let mut line_num_width = 3;
                let mut num_exp        = 10;
                    
                while lines.len() > num_exp {
                    num_exp *= 10;
                    line_num_width += 1;
                }
                line_num_area = file_area.partition_x(line_num_width);

                File::new(lines, options, file_area)
            },

            status_line_area: area,
            line_num_area,

            mappings: ModeList::new(),
        };

        map_actions! {
            file_handler: FileHandler<T>, mappings;
            "insert" => [
                // Move all cursors up.
                key: (KeyCode::Up, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut()
                            .for_each(|c| c.move_ver(-1, &h.file.lines, &h.file.options.tabs));
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors down.
                key: (KeyCode::Down, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut()
                            .for_each(|c| c.move_ver(1, &h.file.lines, &h.file.options.tabs));
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors left.
                key: (KeyCode::Left, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut()
                            .for_each(|c| c.move_hor(-1, &h.file.lines, &h.file.options.tabs));
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors right.
                key: (KeyCode::Right, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut()
                            .for_each(|c| c.move_hor(1, &h.file.lines, &h.file.options.tabs));
                        h.refresh_screen(false);
                    }
                },
                key: (KeyCode::Up, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_ver(-1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors down.
                key: (KeyCode::Down, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_ver(1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors left.
                key: (KeyCode::Left, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_hor(-1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors right.
                key: (KeyCode::Right, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_hor(1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Deletes either the character in front, or the selection.
                key: (KeyCode::Delete, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let cursor = h.file.cursors.get(h.file.main_cursor).unwrap();

                        let mut start = cursor.current();
                        let line = h.file.lines.get(start.line).unwrap();
                        
                        let end = if let Some(anchor) = cursor.anchor() {
                            let end = max(start, anchor);
                            start = min(start, anchor);
                            end
                        } else if start.col < line.text().len() {
                            TextPos { col: start.col + 1, ..start }
                        } else if start.line < h.file.lines.len() {
                            TextPos { col: 0, line: start.line + 1 }
                        } else {
                            return;
                        };

                        let range = TextRange { start, end };
                        let edit = vec![""];

						let refresh_needed = h.file.edit(edit, range);
						h.refresh_screen(refresh_needed);
                    }
                },
                // Deletes either the character behind, or the selection.
                key: (KeyCode::Backspace, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let cursor = h.file.cursors.get(h.file.main_cursor).unwrap();

                        let mut end = cursor.current();
                        
                        let start = if let Some(anchor) = cursor.anchor() {
                            let start = min(end, anchor);
                            end = max(end, anchor);
                            start
                        } else if end.col > 0 {
                            TextPos { col: end.col - 1, ..end }
                        } else if end.line > 0 {
                            let line = h.file.lines.get(end.line - 1).unwrap();
                            TextPos { col: line.text().len(), line: end.line - 1 }
                        } else {
                            return;
                        };

                        let range = TextRange { start, end };
                        let edit = vec![""];

						let refresh_needed = h.file.edit(edit, range);
						h.refresh_screen(refresh_needed);
                    }
                },
                key: (KeyCode::Tab, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

                        let edit = if h.file.options.tabs_as_spaces {
                            let tab_len = h.file.options.tabs.get_tab_len(pos.col as u16) as usize;
                            vec![" ".repeat(tab_len)]
                        } else {
                            vec!["\t".to_string()]
                        };

						let range = TextRange { start: pos, end: pos };

						let refresh_needed = h.file.edit(edit, range);
						h.refresh_screen(refresh_needed);
                    }
                },
                key: (KeyCode::Enter, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

						let range = TextRange { start: pos, end: pos };
                        let edit = vec![""; 2];

						let refresh_needed = h.file.edit(edit, range);
						h.refresh_screen(refresh_needed);
                    }
                },
                _ => {
                    |h: &mut Buffer<T>, c: char| {
                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

						let range = TextRange { start: pos, end: pos };
                        let edit = vec![c];

						h.file.edit(edit, range);
						h.refresh_screen(false);
                    }
                }
            ]
        }

        file_handler.refresh_screen(true);

        file_handler
    }

    /* TODO: Finish this function */
    /// Prints the contents of the file from line on the file.
    #[inline]
    fn refresh_screen(&mut self, force: bool) {
        self.file.print_file(force);

        // Prints the cursors
        // TODO: Add styling for the main and secondary cursors.
        for cursor in &self.file.cursors {
            self.file.area.move_cursor(cursor.pos.into());
            match self.file.get_char(cursor.current()) {
                Some(TextChar::Primary(main_char)) => {
                    if main_char.ch == '\t' {
                        let modified_x = OutputPos::from(cursor.pos).x + self.file.x_shift();
                        let tab_len = self.file.options.tabs.get_tab_len(modified_x);
                        self.file.area.print(" ".repeat(tab_len as usize).attribute(Reverse));
                    } else {
                        self.file.area.print(main_char.ch.clone().attribute(Reverse));
                    }
                }
                None => self.file.area.print(" ".yellow().attribute(Reverse)),
                _ => {}
            }
        }

        // Printing the line numbers
        // NOTE: Might move to a separate function, but idk.
        let mut pos = crate::output::OutputPos { x: 0, y: 0 };
        let top_line = self.file.top_line();

        self.line_num_area.move_cursor(pos);

        'a: for (index, line) in self.file.lines.iter().skip(top_line).enumerate() {
            let wraps = if index == 0 {
                self.file.top_wraps()..(line.wrap_cols().len() + 1)
            } else {
                0..(line.wrap_cols().len() + 1)
            };

            for _ in wraps {
                if pos.y > self.line_num_area.height() {
                    break 'a;
                }
                let width = self.line_num_area.width() as usize;

                let text = match self.file.options.line_numbers {
                    LineNumbers::Absolute => format!("{:>4}│", index + top_line),
                    LineNumbers::Relative => {
                        let main = self.file.cursors.get(self.file.main_cursor).unwrap().current();
                        format!("{:>w$}│", usize::abs_diff(index + top_line, main.line), w = width)
                    },
                    LineNumbers::Hybrid => {
                        let main = self.file.cursors.get(self.file.main_cursor).unwrap().current();
                        if index + top_line == main.line {
                            format!("{:>w$}│", index + top_line, w = width)
                        } else {
                            format!("{:>w$}│", usize::abs_diff(index + top_line, main.line), w = width)
                        }
                    },
                    _ => "".to_string(),
                };
                self.line_num_area.print(text);
                pos.y += 1;
                self.line_num_area.move_cursor(pos);
            }
        }

        for _ in pos.y..(self.line_num_area.height() + 1) {
            self.line_num_area.print("     ");
            pos.y += 1;
            self.line_num_area.move_cursor(pos);
        }

        self.file.area.flush();
    }
}

impl_input_handler!(Buffer<T>, mappings);
