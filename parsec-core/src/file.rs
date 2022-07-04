use std::cmp::max;

use crate::{
    config::{FileOptions, TabPlaces, WrapMethod},
    output::{OutputArea, OutputPos, TextChar, PrintInfo},
    action::{TextRange, History},
    cursor::CursorPos, convert_to_text_chars,
};

use crate::cursor::{FileCursor, TextPos};

// TODO: move this to a more general file.
/// A line in the text file.
pub struct TextLine {
    /// Which columns on the line should wrap around.
    wrap_cols: Vec<usize>,
    /// The text on the line.
    text: Vec<TextChar>,
    /// The line's indentation.
    indent: usize,

	/// If the line has nay multi character long graphemes.
	single_chars_only: bool,
}

impl From<&Vec<TextChar>> for TextLine {
    fn from(line: &Vec<TextChar>) -> Self {
        let mut text_line = TextLine {
            wrap_cols: Vec::new(),
            text: line.clone(),
			indent: 0,
			single_chars_only: true,
        };
        text_line.single_chars_only = !line.iter().any(
            |c| if let TextChar::Secondary(_) = c { true } else { false }
        );

		text_line
    }
}

impl TextLine {
    /// Returns a new instance of `TextLine`
    pub fn new(text: &str, tabs: &TabPlaces) -> TextLine {
        let mut file_line = TextLine {
            wrap_cols: Vec::new(),
            text: convert_to_text_chars(text.to_string()),
            indent: 0,
            single_chars_only: true,
        };

        file_line.calculate_indentation(tabs);

        file_line
    }

    /// Calculates, sets, and returns the line's indentation.
    pub fn calculate_indentation(&mut self, tabs: &TabPlaces) -> usize {
        let mut indent = 0;
        for text_char in &self.text {
            if let TextChar::Primary(tagged_char) = text_char {
                if tagged_char.ch == ' ' || tagged_char.ch == '\t' {
                    indent += tagged_char.width(indent, tabs);
                } else {
                    break;
                }
            }
        }
        self.indent = indent as usize;
        indent as usize
    }

    /// Returns the visual distance to a certain column.
    pub fn get_distance_to_col(&self, col: usize, tabs: &TabPlaces) -> u16 {
        let mut width = 0;
        for text_char in self.text.get(0..col).expect("invalid col") {
            if let TextChar::Primary(tagged_char) = text_char {
                width += tagged_char.width(width, tabs);
            }
        }
        width
    }

    /// Returns the column found at a certain visual distance from 0. Also returns any leftovers.
    ///
    /// The leftover number is positive if the width of the characters is greater (happens if the
    /// last character has a width greater than 1), and it is negative if the distance is greater
    /// (happens if there aren't enough characters to cover the distance.
    pub fn get_col_at_distance(&self, dist: u16, tabs: &TabPlaces) -> (usize, i32) {
        let (mut index, mut total_width) = (0, 0);
        let mut text_iter = self.text.iter();
        while total_width < dist {
            match text_iter.next() {
                Some(TextChar::Primary(tagged_char)) => {
                    total_width += tagged_char.width(total_width, tabs);
                    index += 1;
                }
                Some(TextChar::Secondary(_)) => index += 1,
                None => break,
            }
        }
        (index, total_width as i32 - dist as i32)
    }

    /// Parses the wrapping of a single line.
    pub fn parse_wrapping(&mut self, width: u16, options: &FileOptions) -> bool {
        let prev_wrap = self.wrap_cols.len();
        self.wrap_cols.clear();

        let mut cur_x = 0;
        let mut wrap_indentation = 0;
        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        for (index, text_char) in self.text.iter_mut().enumerate() {
            if let TextChar::Primary(main_char) = text_char {
                cur_x += main_char.width(cur_x, &options.tabs) as u16;

                if cur_x == width - wrap_indentation {
                    cur_x = 0;
                    self.wrap_cols.push(index);
                    main_char.is_wrapping = true;
                    if options.wrap_indent {
                        wrap_indentation = self.indent as u16;
                    }
                } else {
                    main_char.is_wrapping = false;
                }
            }
        }

        self.wrap_cols.len() != prev_wrap
    }

    // TODO: Make this a generic free function, and make areas do bounds checking.
    /// Prints a line in a given position, skipping `skip` characters.
    ///
    /// Returns the amount of wrapped lines that were printed.
    #[inline]
    fn print<T: OutputArea>(
        &self, area: &mut T, mut pos: OutputPos, skip: usize,
        options: &FileOptions, x_shift: u16, ) -> u16 {
        // Moves the printing cursor to the beginning of the line.
        area.move_cursor(pos);

        let mut printed_lines = 1;
        let mut col = 0;

        let (skip, leftover) = if let WrapMethod::NoWrap = options.wrap_method {
            // Knowing this code, this would seem to overwrite `top_wraps`. But since this value is
            // always 0 when wrapping is disabled, it doesn't matter.
            // The leftover here is represents the amount of characters that should not be printed,
            // for example, complex emoji may occupy multiple "empty" cells, if you print 
            self.get_col_at_distance(x_shift, &options.tabs)
        } else {
            (skip, 0)
        };

        area.print(" ".repeat(max(0, leftover) as usize));

		let text_iter =
    		self.text.iter().skip(skip).skip_while(|&l| matches!(l, TextChar::Secondary(_)));

		if let Some(&first_wrap_col) = self.wrap_cols.get(0) {
    		if skip >= first_wrap_col && options.wrap_indent {
            	area.print(" ".repeat(self.indent));
    		}
		}

        for text_char in text_iter {
            if let TextChar::Primary(main_char) = text_char {
                if let WrapMethod::NoWrap = options.wrap_method {
                    if col + main_char.width(col, &options.tabs) > area.width() {
                        break;
                    }
                }

                if main_char.ch == '\t' {
                    let tab_len = options.tabs.get_tab_len(col + x_shift);
                    area.print(" ".repeat(tab_len as usize));
                    col += tab_len;
                } else {
                    area.print(main_char.ch.clone());
                    col += main_char.width(col, &options.tabs);
                }

                if main_char.is_wrapping {
                    printed_lines += 1;
                    pos.y += 1;
                    if pos.y > area.height() {
                        break;
                    }
                    area.move_cursor(pos);
                    if options.wrap_indent && pos.x == 0 {
                        area.print(" ".repeat(self.indent));
                    }
                }
			} else if let TextChar::Secondary(ch) = text_char {
    			area.print(ch);
			}
        }

        // Erasing anything that is leftover
        let width = area.width();
        if pos.y <= area.height() {
            // NOTE: Eventually will be improved when issue #53667 on rust-lang gets closed.
            if let WrapMethod::Width = options.wrap_method {
                area.print(" ".repeat(width as usize));
            } else if col < width {
                area.print(" ".repeat(width as usize));
            }
        }

        printed_lines
    }
    
    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn wrap_cols(&self) -> &Vec<usize> {
        &self.wrap_cols
    }

    pub fn text(&self) -> &Vec<TextChar> {
        &self.text
    }

    pub fn indent(&self) -> usize {
        self.indent
    }
}

/// File text and cursors.
pub struct File<T> {
    /// The lines of the file.
    pub lines: Vec<TextLine>,

    /// Where to start printing on the file.
    print_info: PrintInfo,

    /// The area allocated to the file.
    pub area: T,

    /// The options related to files.
    pub options: FileOptions,

    /// The edtiting cursors on the file.
    pub cursors: Vec<FileCursor>,
    /// The index of the main cursor. The file "follows it".
    pub main_cursor: usize,
    
	/// The history of edits on this file.
	history: History,
}

impl<T: OutputArea> File<T> {
    /// Returns a new instance of `File<T>`, given a `Vec<FileLine>`.
    pub fn new(lines: Vec<TextLine>, options: FileOptions, area: T) -> File<T> {
        let mut file = File {
            lines,
            // TODO: Remember last session.
            print_info: PrintInfo { top_line: 0, top_wraps: 0, x_shift: 0 },
            area,
            options,
            cursors: Vec::new(),
            main_cursor: 0,
            history: History::new()
        };

        file.cursors.push(FileCursor::new(TextPos { col: 0, line: 0 }, &file));

        file.parse_wrapping();

        file
    }

    /// Returns an `Some<&StyledChar>` if it exists, and `None` if it doesn't.
    pub fn get_char(&self, pos: TextPos) -> Option<&TextChar> {
        match self.lines.get(pos.line) {
            Some(line) => {
                if line.single_chars_only || self.options.fractional_graphemes {
                    line.text.get(pos.col)
                } else {
                    let mut text_iter = line.text.iter().enumerate();
                    while let Some((index, text_char)) = text_iter.next() {
                        if index == pos.col + 1 {
                            return Some(text_char)
                        }
                    }
                    None
                }
            },
            None => None,
        }
    }

    /// Parses the wrapping for all the lines in the file.
    ///
    /// * This should only be called when the wrap_type or width change.
    pub fn parse_wrapping(&mut self) {
        match self.options.wrap_method {
            WrapMethod::Width => {
                for line in self.lines.iter_mut() {
                    line.parse_wrapping(self.area.width(), &self.options);
                }
            }
            WrapMethod::NoWrap => {
                for line in self.lines.iter_mut() {
                    line.wrap_cols.clear();
                }
            }
            _ => {}
        }
    }

    /// Updates the file's scrolling and checks if it has scrolled.
    pub fn update_print_info(&mut self) -> bool {
        let info = &mut self.print_info;
        let scrolloff = self.options.scrolloff;

        let mut cursor_pos = self.cursors.get(self.main_cursor).expect("cursor not found").pos;

        // Scroll if the cursor surpasses the soft cap imposed by `scrolloff`.
        let mut has_scrolled = false;

        // Vertical check:
        while cursor_pos.y > (self.area.height() - scrolloff.y) as i32 {
            if self.lines.get(info.top_line).unwrap().wrap_cols.len() > info.top_wraps {
                info.top_wraps += 1;
            } else {
                info.top_line += 1;
                info.top_wraps = 0;
            }
            has_scrolled = true;
            cursor_pos.y -= 1;
            self.cursors.iter_mut().for_each(|c| c.pos.y -= 1);
        }

        // Stop scrolling when the line at the top of the screen is already the first line.
        while cursor_pos.y < scrolloff.y as i32 && !(info.top_line == 0 && info.top_wraps == 0) {
            if info.top_wraps > 0 {
                info.top_wraps -= 1;
            } else {
                info.top_line -= 1;
                let top_line = self.lines.get(info.top_line).expect("invalid line");
                info.top_wraps = top_line.wrap_cols.len();
            }
            // Moves the cursors down by one, to keep them in the same place in the
            // file.
            has_scrolled = true;
            cursor_pos.y += 1;
            self.cursors.iter_mut().for_each(|c| c.pos.y += 1);
        }

        let current = self.cursors.get(self.main_cursor).expect("cursor not found").current();
        let line = self.lines.get(current.line).unwrap();

        // Horizontal check, done only when the screen can scroll horizontally:
        if let WrapMethod::NoWrap = self.options.wrap_method {
            let distance = line.get_distance_to_col(current.col, &self.options.tabs);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance >= info.x_shift + self.area.width() - scrolloff.x {
                // Shift by the amount required to keep the cursor in bounds.
                info.x_shift = distance + scrolloff.x + 1 - self.area.width();
                has_scrolled = true;
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance <= info.x_shift + scrolloff.x && info.x_shift != 0 {
                info.x_shift = distance.saturating_sub(scrolloff.x);
                has_scrolled = true;
            }
            // If the screen moves `x` forward, cursors must move `x` backward to keep them steady.
            self.cursors.iter_mut().for_each(|c| c.pos.x -= info.x_shift as i32);
        }

        has_scrolled
    }

	/// Applies a splice to the file.
    pub fn edit<S>(&mut self, edit: Vec<S>, mut range: TextRange) -> bool
    where
        S: ToString {
        let old_range = range;

		let old_lines_len = self.lines.len();

		range = self.history.add_change(&mut self.lines, edit, range);

		let mut refresh_needed = self.lines.len() != old_lines_len;

    	for line in self.lines.get_mut(range.start.line..=range.end.line).unwrap() {
        	if self.options.wrap_method.is_wrapped() {
        		refresh_needed |= line.parse_wrapping(self.area.width(), &self.options);
        	}
    		line.calculate_indentation(&self.options.tabs);
    	}

		for cursor in &mut self.cursors {
    		if cursor.current() >= old_range.end {
        		let new_pos = range.end + cursor.current() - old_range.end;
        		cursor.move_to(new_pos, &self.lines, &self.options)
    		}
		}

		refresh_needed
    }

    /// Prints the file, according to its current position.
    pub fn print_file(&mut self, force: bool) {
        // Saving the current positions to re-print the lines where cursors have been.
        let saved: Vec<(usize, CursorPos, u16)> = self
            .cursors.iter().map(|c| (c.current().line, c.pos, c.wraps())).collect();

        // Updates the information for each cursor in the file.
        self.cursors.iter_mut().for_each(|c| c.update(&self.lines, &self.options));

        // Checks if said update has caused the file to scroll.
        let has_scrolled = self.update_print_info();
        let info = self.print_info;

        // The line at the top of the screen and the amount of hidden columns.
        let skip = if info.top_wraps > 0 {
            let line = self.lines.get(info.top_line).expect("invalid line");
            *line.wrap_cols().get(info.top_wraps - 1).unwrap() + 1
        } else {
            0
        };

        // If the file has scrolled, reprint the whole screen.
        if has_scrolled || force {
            let area = &mut self.area;

            let mut line_origin = OutputPos { x: 0, y: 0 };

            // Prints the first line and updates where to print next.
            let line = self.lines.get(info.top_line).expect("invalid line");
            line_origin.y += line.print(area, line_origin, skip, &self.options, info.x_shift);

            // Prints the remaining lines
            for line in self.lines.iter().skip(info.top_line + 1) {
                if line_origin.y > area.height() {
                    break;
                }
                line_origin.y += line.print(area, line_origin, 0, &self.options, info.x_shift);
            }

            // Clears the lines where nothing has been printed.
            for _ in line_origin.y..=self.area.height() {
                self.area.move_cursor(line_origin);
                self.area.print(" ".repeat(self.area.width() as usize));
                line_origin.y += 1;
            }
        // If it hasn't, only reprint the lines where cursors have been in.
        } else {
            let mut printed_lines = Vec::new();

            for (current, prev) in self.cursors.iter().zip(saved) {
                let line = self.lines.get(prev.0).expect("invalid line");
                let mut line_origin: OutputPos = prev.1.into();

                line_origin.x = 0;
                // Printing will never happen at a negative `y`.
                line_origin.y = max(0, line_origin.y - prev.2);

                if !printed_lines.contains(&current.current().line) {
                    line_origin.y += if prev.0 == info.top_line {
                        line.print(&mut self.area, line_origin, skip, &self.options, info.x_shift)
                    } else if line_origin.y < self.area.height() {
                        line.print(&mut self.area, line_origin, 0, &self.options, info.x_shift)
                    } else {
                        0
                    };

                    printed_lines.push(prev.0);
                }
            }
        }
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn top_line(&self) -> usize {
        self.print_info.top_line
    }

    pub fn top_wraps(&self) -> usize {
        self.print_info.top_wraps
    }

    pub fn x_shift(&self) -> u16 {
        self.print_info.x_shift
    }
}

// NOTE: `new` must not be empty! It has to have at least one `String` even if it's empty.
/// Splices a `Vec<String>` into another `Vec<String>`.
///
/// Returns the changed vector, and the range that `new` occupies in `old`. This function
/// assumes that both `old` and `new` have no multi character graphemes, or that
/// `fractional_graphemes` is enabled.
pub fn simple_splice_lines(old: &mut Vec<TextLine>, new: Vec<Vec<TextChar>>, range: TextRange)
    -> (Vec<Vec<TextChar>>, TextRange) {
    let (start, end) = (range.start, range.end);
    let first_new_line = new.get(0).unwrap();

	let mut old_text = Vec::with_capacity(range.end.line + 1 - range.start.line);

	// A simple splicing is enough for splices that involve just a single line in both ends.
    if start.line == end.line && new.len() == 1 {
        let first_line = old.get_mut(start.line).unwrap();
        old_text.push(
            first_line.text.splice(start.col..end.col, first_new_line.to_owned()).collect()
        );
    } else {
        // The end of the last line will be placed at the end of insertions.
        let cutoff = old.get(end.line).unwrap().text.get(end.col..).unwrap().to_owned();

		old_text.push(cutoff.clone());

    	// Replacing the original end of the first line with the start of `new`.
        let first_line = old.get_mut(start.line).unwrap();
    	old_text.push(first_line.text.splice(start.col.., first_new_line.to_owned()).collect());

		if let Some(line_slice) = old.get((start.line + 1)..end.line) {
    		old_text.extend(line_slice.iter().map(|l| l.text().to_owned()))
		}

		old_text.push(old.get(end.line).unwrap().text().get(0..end.col).unwrap().to_owned());
        
    	// Splice aditional lines after the first one. This includes the last line in the range.
        old.splice(
            (start.line + 1)..=end.line,
            new.get(1..).unwrap().iter().map(|l| TextLine::from(l))
        );

    	// Adding the remaining text to the last line.     
    	old.get_mut(start.line + new.len() - 1).unwrap().text.extend_from_slice(&cutoff);
    };

    let new_range = TextRange {
        start: range.start,
        end: if new.len() == 1 {
            TextPos { line: start.line, col: start.col + new.get(0).unwrap().len() }
        } else {
            TextPos { line: start.line + new.len() - 1, col: new.last().unwrap().len() }
        }
    };
	
    (old_text, new_range)
}

