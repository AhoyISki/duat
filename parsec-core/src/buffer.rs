use std::{cmp::*, fs, path::PathBuf};

use crossterm::{
    event::{KeyCode, KeyEvent, KeyModifiers},
    style::{Attribute, Attributes, Color, ContentStyle, Stylize},
};
use regex::Regex;

use crate::{
    action::TextRange,
    config::{FileOptions, LineNumbers},
    cursor::TextPos,
    file::{File, TextLine},
    impl_input_handler,
    input::{InputHandler, ModeList},
    map_actions,
    output::OutputArea,
    tags::Form,
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

    /// List of forms.
    forms: Vec<Form>,

    /// For testing ////////////////////////////////////////////////////
    reg: Regex,
}

impl<T: OutputArea> Buffer<T> {
    /// Returns a new instance of ContentArea
    pub fn new(mut area: T, path: PathBuf, options: FileOptions) -> Buffer<T> {
        // TODO: In the future, this will not panic!
        // TODO: Move this to a more general file.
        let file = fs::read_to_string(path).expect("file not found");

        let line_num_area;

        let reg = Regex::new(r"\{|\}|\(|\)|\[|\]").unwrap();

        let mut file_handler = Buffer {
            file: {
                let lines: Vec<TextLine> = file.lines().map(|l| TextLine::new(l, &reg)).collect();

                let mut file_area = area.partition_y((area.height() - 2) as u16);

                let mut line_num_width = 3;
                let mut num_exp = 10;

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

            forms: {
                let s_1 = ContentStyle::new().red();
                let s_2 = ContentStyle::new().blue();
                let s_3 = ContentStyle::new().green();

                vec![Form::new(s_1, false), Form::new(s_2, true), Form::new(s_3, false)]
            },

            reg,
        };

        map_actions! {
            file_handler: FileHandler<T>, mappings;
            "insert" => [
                // Move all cursors up.
                key: (KeyCode::Up, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            c.unset_anchor();
                            c.move_ver(-1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors down.
                key: (KeyCode::Down, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            c.unset_anchor();
                            c.move_ver(1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors left.
                key: (KeyCode::Left, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            c.unset_anchor();
                            c.move_hor(-1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Move all cursors right.
                key: (KeyCode::Right, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            c.unset_anchor();
                            c.move_hor(1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Place anchor and move all cursors up.
                key: (KeyCode::Up, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_ver(-1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Place anchor and move all cursors down.
                key: (KeyCode::Down, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_ver(1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Place anchor and move all cursors left.
                key: (KeyCode::Left, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_hor(-1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(false);
                    }
                },
                // Place anchor and move all cursors right.
                key: (KeyCode::Right, KeyModifiers::SHIFT) => {
                    |h: &mut Buffer<T>| {
                        h.file.cursors.iter_mut().for_each(|c| {
                            if let None = c.anchor() { c.set_anchor(); }
                            c.move_hor(1, &h.file.lines, &h.file.options.tabs);
                        });
                        h.refresh_screen(true);
                    }
                },
                // Deletes either the character in front, or the selection.
                key: (KeyCode::Delete, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let cursor = h.file.cursors.get(h.file.main_cursor).unwrap();

                        let current = cursor.current();
                        let line = h.file.lines.get(current.line).unwrap();

                        let (start, end) = if let Some(anchor) = cursor.anchor() {
                            (min(current, anchor), max(current, anchor))
                        } else if current.col < line.text().len() {
                            let col = current.col + 1;
                            let byte = line.get_byte_at(col);
                            (current, TextPos { col, byte, ..current })
                        } else if current.line < h.file.lines.len() {
                            let col = 0;
                            let byte = line.get_byte_at(col);
                            (current, TextPos { col, byte, line: current.line + 1 })
                        } else {
                            return;
                        };

                        let range = TextRange { start, end };
                        let edit = vec![""];

                        let refresh_needed = h.file.splice_edit(edit, range, &h.reg);
                        h.refresh_screen(refresh_needed);
                    }
                },
                // Deletes either the character behind, or the selection.
                key: (KeyCode::Backspace, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let cursor = h.file.cursors.get(h.file.main_cursor).unwrap();

                        let current = cursor.current();
                        let line = h.file.lines.get(current.line).unwrap();

                        let (start, end) = if let Some(anchor) = cursor.anchor() {
                            (min(current, anchor), max(current, anchor))
                        } else if cursor.current().col > 0 {
                            let col = current.col - 1;
                            let byte = line.get_byte_at(col);
                            (TextPos { col, byte, ..current }, current)
                        } else if current.line > 0 {
                            let line = h.file.lines.get(current.line - 1).unwrap();
                            let col = line.text().len();
                            let byte = line.get_byte_at(col);
                            (TextPos { col, byte, line: current.line - 1 }, current)
                        } else {
                            return;
                        };

                        let range = TextRange { start, end };

                        let edit = vec![""];

                        let refresh_needed = h.file.splice_edit(edit, range, &h.reg);
                        h.refresh_screen(refresh_needed);
                    }
                },
                key: (KeyCode::Tab, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

                        let edit = if h.file.options.tabs_as_spaces {
                            let tab_len = h.file.options.tabs.get_tab_len(pos.col);
                            vec![" ".repeat(tab_len)]
                        } else {
                            vec!["\t".to_string()]
                        };

                        let range = TextRange { start: pos, end: pos };

                        let refresh_needed = h.file.splice_edit(edit, range, &h.reg);
                        h.refresh_screen(refresh_needed);
                    }
                },
                key: (KeyCode::Char('m'), KeyModifiers::ALT) => {
                    |h: &mut Buffer<T>| {
                        panic!("{:#?}", h.file.history.moments)
                    }
                },
                key: (KeyCode::Char('z'), KeyModifiers::CONTROL) => {
                    |h: &mut Buffer<T>| {
                        h.file.undo(&h.reg);
                        h.refresh_screen(true);
                    }
                },
                key: (KeyCode::Char('y'), KeyModifiers::CONTROL) => {
                    |h: &mut Buffer<T>| {
                        h.file.redo(&h.reg);
                        h.refresh_screen(true);
                    }
                },
                key: (KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                    |h: &mut Buffer<T>| {
                        unsafe { crate::FOR_TEST = true }
                    }
                },
                key: (KeyCode::Enter, KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

                        let range = TextRange { start: pos, end: pos };
                        let edit = vec![""; 2];

                        let refresh_needed = h.file.splice_edit(edit, range, &h.reg);
                        h.refresh_screen(refresh_needed);
                    }
                },
                key: (KeyCode::Char(' '), KeyModifiers::NONE) => {
                    |h: &mut Buffer<T>| {
                        h.file.history.new_moment(h.file.print_info());

                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

                        let range = TextRange { start: pos, end: pos };
                        let edit = vec![' '];

                        let refresh_needed = h.file.splice_edit(edit, range, &h.reg);
                        h.refresh_screen(refresh_needed);
                    }
                },
                _ => {
                    |h: &mut Buffer<T>, c: char| {
                        let pos = h.file.cursors.get(h.file.main_cursor).unwrap().current();

                        let range = TextRange { start: pos, end: pos };
                        let edit = vec![c];

                        let refresh_needed = h.file.splice_edit(edit, range, &h.reg);
                        h.refresh_screen(refresh_needed);
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
        self.file.print_file(force, &self.forms);

        // Printing the line numbers
        // NOTE: Might move to a separate function, but idk.
        let mut pos = crate::output::OutputPos { x: 0, y: 0 };
        let top_line = self.file.top_line();

        self.line_num_area.move_cursor(pos);

        'a: for (index, line) in self.file.lines.iter().skip(top_line).enumerate() {
            let wraps = match line.wrap_iter() {
                Some(wrap_iter) => {
                    if index == 0 {
                        self.file.top_wraps()..(wrap_iter.count() + 1)
                    } else {
                        0..(wrap_iter.count() + 1)
                    }
                }
                None => 0..1,
            };

            for _ in wraps {
                if pos.y as usize > self.line_num_area.height() {
                    break 'a;
                }
                let width = self.line_num_area.width() as usize;

                let text = match self.file.options.line_numbers {
                    LineNumbers::Absolute => format!("{:>4}│", index + top_line),
                    LineNumbers::Relative => {
                        let main = self.file.cursors.get(self.file.main_cursor).unwrap().current();
                        format!("{:>w$}│", usize::abs_diff(index + top_line, main.line), w = width)
                    }
                    LineNumbers::Hybrid => {
                        let main = self.file.cursors.get(self.file.main_cursor).unwrap().current();
                        if index + top_line == main.line {
                            format!("{:>w$}│", index + top_line, w = width)
                        } else {
                            format!(
                                "{:>w$}│",
                                usize::abs_diff(index + top_line, main.line),
                                w = width
                            )
                        }
                    }
                    _ => "".to_string(),
                };
                self.line_num_area.print(text);
                pos.y += 1;
                self.line_num_area.move_cursor(pos);
            }
        }

        for _ in (pos.y as usize)..(self.line_num_area.height() + 1) {
            self.line_num_area.print("     ");
            pos.y += 1;
            self.line_num_area.move_cursor(pos);
        }

        self.file.area.flush();
    }
}

impl_input_handler!(Buffer<T>, mappings);
