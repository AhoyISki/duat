use std::cmp::{max, min};

use unicode_width::UnicodeWidthChar;

use crate::cursor::{FileCursor, TextPos};
use crate::tags::LineFlags;
use crate::{
    action::{History, TextRange},
    config::{FileOptions, TabPlaces, WrapMethod},
    output::{OutputArea, OutputPos, PrintInfo},
    tags::{CharTag, CharTags},
};

// TODO: move this to a more general file.
/// A line in the text file.
pub struct TextLine {
    /// Which columns on the line should wrap around.
    char_tags: Option<Box<CharTags>>,

    /// The text on the line.
    text: String,
    line_flags: LineFlags,
}

impl TextLine {
    /// Returns a new instance of `TextLine`.
    pub fn new(text: &str) -> TextLine {
        TextLine { char_tags: None, text: String::from(text), line_flags: LineFlags::empty() }
    }

    /// Returns the line's indentation.
    pub fn indent(&self, tabs: &TabPlaces) -> usize {
        let mut indent_sum = 0;

        for ch in self.text.chars() {
            if ch == ' ' || ch == '\t' {
                indent_sum += get_char_width(ch, indent_sum, tabs);
            } else {
                break;
            }
        }

        indent_sum as usize
    }

    /// Returns the visual distance to a certain column.
    pub fn get_distance_to_col(&self, col: usize, tabs: &TabPlaces) -> usize {
        let mut width = 0;

        if self.line_flags.contains(LineFlags::PURE_1_COL) {
            width = col
        } else {
            for ch in self.text.chars().take(col) {
                width += if ch == '\t' {
                    tabs.get_tab_len(width)
                } else {
                    UnicodeWidthChar::width(ch).unwrap_or(1)
                };
            }
        }

        width
    }

    /// Returns the column found at a certain visual distance from 0. Also returns any leftovers.
    ///
    /// The leftover number is positive if the width of the characters is greater (happens if the
    /// last checked character has a width greater than 1), and it is negative if the distance is
    /// greater (happens if there aren't enough characters to cover the distance.
    pub fn get_col_at_distance(&self, min_dist: usize, tabs: &TabPlaces) -> (usize, i32) {
        let (mut byte, mut dist_sum) = (0, 0);
        let mut text_iter = self.text.chars();

        if self.line_flags.contains(LineFlags::PURE_1_COL) {
            (byte, dist_sum) = if self.line_flags.contains(LineFlags::PURE_ASCII) {
                (min(min_dist, self.text.len()), min(self.text.len() - min_dist, 0))
            } else {
                match self.text.char_indices().nth(min_dist) {
                    Some((byte, _)) => (byte, 0),
                    None => {
                        let count = self.text.chars().count();
                        (count, min_dist - count)
                    },
                }
            }
        } else {
            // NOTE: This looks really stupid.
            while let (Some(ch), true) = (text_iter.next(), dist_sum < min_dist) {
                dist_sum += if ch == '\t' {
                    tabs.get_tab_len(dist_sum)
                } else {
                    UnicodeWidthChar::width_cjk(ch).unwrap_or(1)
                };

                byte += 1;
            }
        }

        (byte, dist_sum as i32 - min_dist as i32)
    }

    /// Parses the wrapping of a single line.
    ///
    /// Returns `true` if the amount of wrapped lines has changed.
    pub fn parse_wrapping(&mut self, width: usize, options: &FileOptions) -> bool {
        let indent = self.indent(&options.tabs);

        let (char_tags, prev_len) = match self.char_tags.as_mut() {
            Some(char_tags) => {
                let prev_len = char_tags.vec().len();
                char_tags.retain(|(_, t)| !matches!(t, CharTag::WrapppingChar));

                (char_tags, prev_len)
            },
            None => {
                self.char_tags = Some(Box::from(CharTags::new()));

                (self.char_tags.as_mut().unwrap(), 0)
            },
        };

        let mut char_index = 0;
        let mut indent_wrap = 0;
        let mut additions = Vec::new();

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        if self.line_flags.contains(LineFlags::PURE_1_COL | LineFlags::PURE_ASCII) {
            char_index = width;
            while char_index < self.text.len() {
                additions.push((char_index as u32, CharTag::WrapppingChar));

                if options.wrap_indent {
                    indent_wrap = indent;
                }

                // `width` goes to the first character of the next line, so `n * width` would be
                // off by `n - 1` characters, which is why the `- 1` is there.
                char_index += width - indent_wrap;
            }
        } else {
            for (index, ch) in self.text.chars().enumerate() {
                char_index += get_char_width(ch, char_index, &options.tabs);

                if char_index > width - indent_wrap {
                    char_index = get_char_width(ch, char_index, &options.tabs);

                    additions.push((index as u32, CharTag::WrapppingChar));

                    if options.wrap_indent {
                        indent_wrap = indent;
                    }
                }
            }
        }

        char_tags.insert_slice(additions.as_slice());

        self.char_tags.as_mut().unwrap().vec().len() != prev_len
    }

    /// Returns an iterator over the wrapping columns of the line.
    pub fn wrap_iter(&self) -> Option<impl Iterator<Item = u32> + '_> {
        if let Some(tags) = self.char_tags.as_ref() {
            Some(
                tags.vec()
                    .iter()
                    .filter(|(_, t)| matches!(t, CharTag::WrapppingChar))
                    .map(|(c, _)| *c),
            )
        } else {
            None
        }
    }

	/// Returns how many characters are in the line.
	pub fn char_count(&self) -> usize {
    	if self.line_flags.contains(LineFlags::PURE_ASCII) {
        	self.text.len()
    	} else {
        	self.text.chars().count()
    	}
	}

    // NOTE: It always prints at `x = 0`, `x` in pos is treated here as an `x_shift`.
    /// Prints a line in a given position, skipping `skip` characters.
    ///
    /// Returns the amount of wrapped lines that were printed.
    #[inline]
    fn print<T>(&self, area: &mut T, shift: OutputPos, skip: usize, options: &FileOptions) -> u16
    where
        T: OutputArea,
    {
        // Moves the printing cursor to the beginning of the line.
        let mut printing_pos = OutputPos { x: 0, ..shift };
        area.move_cursor(printing_pos);

        let mut printed_lines = 1;
        let mut d_x = 0;

        let (skip, leftover) = if let WrapMethod::NoWrap = options.wrap_method {
            // Knowing this code, this would seem to overwrite `top_wraps`. But since this value is
            // always 0 when wrapping is disabled, it doesn't matter.
            // The leftover here represents the amount of characters that should not be printed,
            // for example, complex emoji may occupy several cells that should be empty, in the
            // case that part of the emoji is located before the first column.
            self.get_col_at_distance(shift.x as usize, &options.tabs)
        } else {
            (skip, 0)
        };

        area.print(" ".repeat(max(0, leftover) as usize));

        let text_iter = self.text.chars().chain(std::iter::once(' ')).enumerate().skip(skip);

        let char_width = |c, x| {
            if self.line_flags.contains(LineFlags::PURE_1_COL) {
                1
            } else {
                get_char_width(c, x, &options.tabs)
            }
        };

        if let Some(tags) = &self.char_tags {
            let mut wrap_iter = unsafe { self.wrap_iter().unwrap_unchecked() };

            // In the case where the amount of skipped characters is greater than the placement of
            // the first wrapped one, if `options.wrap_indent`, we need to indent the text
            // immediately, in order to print the text in the correct place. This will happen if
            // the top line wraps and has indentation.
            if let Some(col) = wrap_iter.next() {
                if skip >= col as usize && options.wrap_indent {
                    area.print(" ".repeat(self.indent(&options.tabs)));
                } else {
                }
            }

            let mut tags_iter = tags.vec().iter().skip_while(|(c, _)| *c < skip as u32);
            let mut current_char_tag = tags_iter.next();

            'a: for (col, ch) in text_iter {
                while let Some(&(tag_col, tag)) = current_char_tag {
                    if col == tag_col as usize {
                        current_char_tag = tags_iter.next();

                        if let CharTag::WrapppingChar = tag {
                            // If this is the first printed character of `top_line`, we don't wrap.
                            if d_x == 0 {
                                continue;
                            }

                            printed_lines += 1;
                            printing_pos.y += 1;

                            if printing_pos.y as usize > area.height() {
                                break 'a;
                            }

                            area.move_cursor(printing_pos);

                            if options.wrap_indent && shift.x == 0 {
                                (0..self.indent(&options.tabs)).for_each(|_| area.print(' '));
                            }
                        } else if let CharTag::MainCursor = tag {
                            area.style(tag);
                        }
                    } else {
                        break;
                    }
                }

                let char_width = char_width(ch, d_x);
                d_x += char_width;
                if let WrapMethod::NoWrap = options.wrap_method {
                    if d_x > area.width() {
                        break;
                    }
                }

                if ch == '\t' {
                    // `repeat()` would use string allocation (I think).
                    (0..char_width).for_each(|_| area.print(' '));
                } else {
                    area.print(ch);
                }
            }
        } else {
            for (_, ch) in text_iter {
                let char_width = char_width(ch, d_x);
                d_x += char_width;
                if d_x > area.width() {
                    break;
                }

                if ch == '\t' {
                    // `repeat()` would use string allocation (I think).
                    (0..char_width).for_each(|_| area.print(' '));
                } else {
                    area.print(ch);
                }
            }
        }

        // Erasing anything that is leftover
        let width = area.width();
        if printing_pos.y as usize <= area.height() {
            // NOTE: Eventually will be improved when issue #53667 on rust-lang gets closed.
            if let WrapMethod::Width = options.wrap_method {
                area.print(" ".repeat(width));
            } else if d_x < width {
                area.print(" ".repeat(width));
            }
        }

        printed_lines
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn text(&self) -> &str {
        &self.text.as_str()
    }
}

/// File text and cursors.
pub struct File<T> {
    /// The lines of the file.
    pub lines: Vec<TextLine>,

    /// Where on the file to start printing.
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
    // NOTE: remove pub.
    pub history: History,
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
            history: History::new(),
        };

        file.cursors.push(FileCursor::new(
            TextPos { col: 0, line: 0 },
            &file.lines,
            &file.options.tabs,
        ));

        for line in 0..file.lines.len() {
            file.update_line_info(line);
        }

        file
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
            },
            WrapMethod::NoWrap => {
                for line in self.lines.iter_mut() {
                    if let Some(tags) = &mut line.char_tags {
                        tags.retain(|(_, t)| !matches!(t, CharTag::WrapppingChar))
                    }
                }
            },
            _ => {},
        }
    }

    /// Updates the file's scrolling and checks if it has scrolled.
    pub fn update_print_info(&mut self) -> bool {
        let info = &mut self.print_info;
        let scrolloff = self.options.scrolloff;

        let main_cursor = self.cursors.get(self.main_cursor).expect("cursor not found");
        let current = main_cursor.current();
        let target = main_cursor.target();

        // Scroll if the cursor surpasses the soft cap imposed by `scrolloff`.
        let mut has_scrolled = false;

        // Vertical scroll check:
        if let WrapMethod::NoWrap = self.options.wrap_method {
            // If there is no wrapping, the check is much simpler, just check if the distance to
            // `info.top_line` is within `scrolloff.d_y` and `self.area.height() + scrolloff.d_y`,
            // If it's not, subtract the difference and add/subtract it from `info.top_line`.
            if target.line > info.top_line + self.area.height() - scrolloff.d_y {
                info.top_line += target.line + scrolloff.d_y - info.top_line - self.area.height();
                has_scrolled = true;
            } else if target.line < info.top_line + scrolloff.d_y && info.top_line != 0 {
                info.top_line -= (info.top_line + scrolloff.d_y) - target.line;
                has_scrolled = true;
            }
        } else {
            let (current_wraps, target_wraps, lines_iter) = unsafe {
                let wraps = self.lines.get_unchecked(current.line).wrap_iter().unwrap_unchecked();
                let cur = wraps.filter(|&c| c <= current.col as u32).count();

                let wraps = self.lines.get_unchecked(target.line).wrap_iter().unwrap_unchecked();
                let tar = wraps.filter(|&c| c <= target.col as u32).count();

                (cur, tar, self.lines.get_unchecked_mut(..=target.line).iter_mut())
            };

            let mut d_y = target_wraps;

            // Case where we're moving down.
            if target.line > current.line
                || (target.line == current.line && target_wraps > current_wraps)
            {
                let mut top_offset = 0;
                for (index, line) in lines_iter.enumerate().rev() {
                    // Add the vertical distance, as 1 line plus the times it wraps around.
                    // `target.line` was already added as `target_wraps`.
                    if index != target.line {
                        d_y += unsafe { line.wrap_iter().unwrap_unchecked().count() + 1 };
                    }

                    if index == info.top_line {
                        top_offset = info.top_wraps
                    };

                    // If this happens first, that means the distance between `target.line` and
                    // `info.top_line` is greater than allowed height of the cursor.
                    if d_y >= self.area.height() + top_offset - scrolloff.d_y {
                        info.top_line = index;
                        // If this equals 0, that means the distance has matched up perfectly,
                        // i.e. the distance between the new `info.top_line` is exactly what's
                        // needed for the full height. If it's greater than 0, `info.top_wraps`
                        // needs to adjust where the line actually begins to match up.
                        info.top_wraps = d_y + scrolloff.d_y - self.area.height();
                        has_scrolled = true;

                        break;
                    }

                    // If this happens first, we're in the middle of the screen, and don't need
                    // to change `info.top_line`.
                    if index == info.top_line {
                        break;
                    }
                }
            // Case where we're moving up.
            // TODO: Ignore cases where the line is at least `scrolloff.d_y` lines above
            // `info.top_line` after implementing line folding.
            } else if target.line < current.line
                || (target.line == current.line && target_wraps < current_wraps)
            {
                // Set this flag immediately in this case, because the first line that checks out
                // will definitely be `info.top_line`.
                let mut needs_new_print_info = target.line < info.top_line;
                for (index, line) in lines_iter.enumerate().rev() {
                    // Add the vertical distance, as 1 line plus the times it wraps around.
                    // `target.line` was already added as `target_wraps`.
                    if index != target.line {
                        d_y += unsafe { line.wrap_iter().unwrap_unchecked().count() + 1 };
                    };

                    if index == info.top_line {
                        // This means we ran into the top line too early, and must scroll up.
                        // `info.top_wraps` is here because the top line might be partially off
                        // screen, and we'd be "comparing" only against the shown wraps, which is
                        // incorrect
                        if d_y < scrolloff.d_y + info.top_wraps {
                            needs_new_print_info = true;
                        // If this happens, we ran into `info.top_line` while below `scrolloff.y`,
                        // this means we're in the "middle" of the screen, and don't need to scroll.
                        } else if !needs_new_print_info {
                            break;
                        }
                    }

                    // In this case, we have either passed through `info.top_line` while too close,
                    // or not passed through, so a new `info.top_line` is behind the old one.
                    if needs_new_print_info && (d_y >= scrolloff.d_y || index == 0) {
                        info.top_line = index;
                        info.top_wraps = d_y.saturating_sub(scrolloff.d_y);
                        has_scrolled = true;

                        break;
                    }
                }
            }
        }
        let current = self.cursors.get(self.main_cursor).expect("cursor not found").current();
        let line = self.lines.get(current.line).unwrap();

        // Horizontal scroll check, done only when the screen can scroll horizontally:
        if let WrapMethod::NoWrap = self.options.wrap_method {
            let distance = line.get_distance_to_col(current.col, &self.options.tabs);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance >= info.x_shift + self.area.width() - scrolloff.d_x {
                // Shift by the amount required to keep the cursor in bounds.
                info.x_shift = distance + scrolloff.d_x + 1 - self.area.width();
                has_scrolled = true;
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance <= info.x_shift + scrolloff.d_x && info.x_shift != 0 {
                info.x_shift = distance.saturating_sub(scrolloff.d_x);
                has_scrolled = true;
            }
        }

        has_scrolled
    }

    // TODO: Eventually will include syntax highlighting, hover info, etc.
    /// Updates the information for a line in the file.
    ///
    /// Returns `true` if the screen needs a full refresh.
    pub fn update_line_info(&mut self, line: usize) -> bool {
        let line = &mut self.lines[line];

        line.line_flags.set(LineFlags::PURE_ASCII, line.text.is_ascii());
        line.line_flags.set(
            LineFlags::PURE_1_COL,
            !line.text.chars().any(|c| UnicodeWidthChar::width(c).unwrap_or(1) > 1 || c == '\t'),
        );

        if !matches!(self.options.wrap_method, WrapMethod::NoWrap) {
            line.parse_wrapping(self.area.width(), &self.options)
        } else {
            false
        }
    }

    /// Applies a splice to the file.
    pub fn splice_edit<S>(&mut self, edit: Vec<S>, old_range: TextRange) -> bool
    where
        S: ToString,
    {
        let old_lines_len = self.lines.len();

        let (edits, new_range) = self.history.add_change(&mut self.lines, edit, old_range);
        let edits: Vec<TextLine> = edits.iter().map(|l| TextLine::new(l)).collect();
        self.lines.splice(old_range.lines(), edits);

        let mut full_refresh_needed = self.lines.len() != old_lines_len;

        for line in new_range.lines() {
            full_refresh_needed |= self.update_line_info(line);
        }

        for cursor in &mut self.cursors {
            let new_pos = if cursor.current().line == old_range.end.line {
                new_range.end + cursor.current() - old_range.end
            } else if cursor.current().line > old_range.end.line {
                cursor.current().move_line(new_range.end.line - old_range.end.line)
            } else {
                continue;
            };

            cursor.move_to(new_pos, &self.lines, &self.options)
        }

        full_refresh_needed
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self) {
        let (changes, print_info) = match self.history.undo(&mut self.lines) {
            Some((changes, print_info)) => (changes, print_info),
            None => return,
        };
        self.print_info = print_info.unwrap_or(self.print_info);

        for (edit, splice) in &changes {
            let edit: Vec<TextLine> = edit.iter().map(|l| TextLine::new(l)).collect();

            let added_range = TextRange { start: splice.start, end: splice.added_end };
            self.lines.splice(added_range.lines(), edit);
            for line in added_range.lines() {
                self.update_line_info(line);
            }
        }

        let mut cursor_iter = self.cursors.iter_mut();
        let mut new_cursors = Vec::new();

        for (_, splice) in changes {
            if let Some(cursor) = cursor_iter.next() {
                cursor.move_to(splice.taken_end, &self.lines, &self.options);
            } else {
                new_cursors.push(FileCursor::new(
                    splice.taken_end,
                    &self.lines,
                    &self.options.tabs,
                ));
            }
        }
        self.cursors.extend(new_cursors);
    }

    /// Re-does the last moment in history.
    pub fn redo(&mut self) {
        let (changes, print_info) = match self.history.redo(&mut self.lines) {
            Some((changes, print_info)) => (changes, print_info),
            None => return,
        };
        self.print_info = print_info.unwrap_or(self.print_info);

        for (edit, splice) in &changes {
            let edit: Vec<TextLine> = edit.iter().map(|l| TextLine::new(l)).collect();

            let taken_range = TextRange { start: splice.start, end: splice.taken_end };
            self.lines.splice(taken_range.lines(), edit);
            for line in taken_range.lines() {
                self.update_line_info(line);
            }
        }

        let mut cursor_iter = self.cursors.iter_mut();
        let mut new_cursors = Vec::new();

        for (_, splice) in changes {
            if let Some(cursor) = cursor_iter.next() {
                cursor.move_to(splice.added_end, &self.lines, &self.options);
            } else {
                new_cursors.push(FileCursor::new(
                    splice.added_end,
                    &self.lines,
                    &self.options.tabs,
                ));
            }
        }
        self.cursors.extend(new_cursors);
    }

    pub fn print_file_line(&mut self, index: usize, skip: usize, y_shift: u16) -> u16 {
        let info = self.print_info;

        let line_origin = OutputPos { x: info.x_shift as u16, y: y_shift };

        let line = self.lines.get(index).unwrap();
        line.print(&mut self.area, line_origin, skip, &self.options)
    }

    /// Prints the file, according to its current position.
    pub fn print_file(&mut self, force: bool) {
        // Saving the current cursor lines, in case the case that the whole screen doesn't need to
        // be reprinted.
        let old_cursor_lines: Vec<usize> = self.cursors.iter().map(|c| c.current().line).collect();

        // Checks if the main cursor's position change has caused the line to scroll.
        let has_scrolled = self.update_print_info();

        let current = self.cursors.get(self.main_cursor).unwrap().current();
        match &mut self.lines.get_mut(current.line).unwrap().char_tags {
            Some(tags) => tags.retain(|(_, t)| !matches!(t, CharTag::MainCursor)),
            None => {},
        }

        let target = self.cursors.get(self.main_cursor).unwrap().target();

        let char_tags = &mut self.lines.get_mut(target.line).unwrap().char_tags.as_deref_mut();
        match char_tags {
            Some(tags) => {
                tags.insert((target.col as u32, CharTag::MainCursor));
            },
            None => {
                *char_tags = Some(&mut CharTags::from(&[(target.col as u32, CharTag::MainCursor)]));
            },
        }

        // Updates the information for each cursor in the file.
        self.cursors.iter_mut().for_each(|c| c.update(&self.lines));

        let info = self.print_info;

        // The line at the top of the screen and the amount of hidden columns.
        let skip = if info.top_wraps > 0 {
            let line = self.lines.get(info.top_line).expect("invalid line");
            unsafe { line.wrap_iter().unwrap_unchecked().nth(info.top_wraps - 1).unwrap() }
        } else {
            0
        };

        // If the file has scrolled, reprint the whole screen.
        if has_scrolled || force {
            let mut line_origin = OutputPos { x: 0, y: 0 };

            // Prints the first line and updates where to print next.
            line_origin.y += self.print_file_line(info.top_line, skip as usize, line_origin.y);

            // Prints the remaining lines
            for index in (info.top_line + 1)..self.lines.len() {
                if line_origin.y as usize > self.area.height() {
                    break;
                }
                line_origin.y += self.print_file_line(index, 0, line_origin.y);
            }

            // Clears the lines where nothing has been printed.
            for _ in (line_origin.y as usize)..=self.area.height() {
                self.area.move_cursor(line_origin);
                self.area.print(" ".repeat(self.area.width() as usize));
                line_origin.y += 1;
            }
        // If it hasn't, only reprint the lines where cursors have been in.
        } else {
            let mut lines: Vec<usize> = self.cursors.iter().map(|c| c.current().line).collect();

            lines.extend(old_cursor_lines);
            lines.sort_unstable();
            lines.dedup();

            let mut height_sum = 0;
            let mut last_line_count = info.top_line;

            for index in lines {
                if index < info.top_line {
                    continue;
                } 

                let lines_iter = self.lines.get(last_line_count..index).unwrap().iter();

                height_sum += if let WrapMethod::NoWrap = self.options.wrap_method {
                    lines_iter.count() as u16
                } else {
                    lines_iter
                        .map(|l| unsafe { 1 + l.wrap_iter().unwrap_unchecked().count() as u16 })
                        .sum()
                };

                last_line_count = index;

				if index == info.top_line {
                    self.print_file_line(index, skip as usize, 0);
                } else {
                    self.print_file_line(index, 0, height_sum - info.top_wraps as u16);
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

    pub fn x_shift(&self) -> usize {
        self.print_info.x_shift
    }

    pub fn print_info(&self) -> PrintInfo {
        self.print_info
    }
}

pub fn get_char_width(ch: char, col: usize, tabs: &TabPlaces) -> usize {
    if ch == '\t' {
        tabs.get_tab_len(col)
    } else {
        UnicodeWidthChar::width(ch).unwrap_or(1)
    }
}
