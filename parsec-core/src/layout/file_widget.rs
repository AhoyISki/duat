use std::{
    cmp::{max, min},
    fs,
    ops::BitXor,
    path::PathBuf,
};

use crate::{
    action::{Change, History, Moment, TextRange},
    config::{RoState, RwState, WrapMethod},
    cursor::{Editor, Mover, SpliceAdder, TextCursor, TextPos},
    file::{update_range, Text},
    log_info, split_string_lines,
    tags::{CharTag, MatchManager},
    ui::{EndNode, Label, Ui},
};

use super::Widget;

// NOTE: The defaultness in here, when it comes to `last_main`, main cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// How many times the line at the top wraps around before being shown.
    pub top_wraps: usize,
    /// The index of the line at the top of the screen.
    pub top_line: usize,
    /// How shifted the text is to the left.
    pub x_shift: usize,
    /// The last position of the main cursor.
    pub last_main: TextPos,
}

impl PrintInfo {
    /// Scrolls the `PrintInfo` vertically by a given amount, on a given file.
    fn scroll_vertically(&mut self, mut d_y: i32, text: &Text) {
        if d_y > 0 {
            let mut lines_iter = text.lines().iter().skip(self.top_line);

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.wrap_iter().count();
                if (wrap_count + 1) as i32 > d_y {
                    self.top_wraps = d_y as usize;
                    break;
                } else {
                    self.top_line += 1;
                    d_y -= (wrap_count + 1) as i32;
                }
            }
        } else if d_y < 0 {
            let mut lines_iter = text.lines().iter().take(self.top_line).rev();

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.wrap_iter().count();
                if ((wrap_count + 1) as i32) < d_y {
                    self.top_wraps = -d_y as usize;
                    break;
                } else {
                    self.top_line -= 1;
                    d_y += (wrap_count + 1) as i32;
                }
            }
        }
    }

    fn scroll_horizontally(
        &mut self, mut d_x: i32, text: &Text, label: &impl Label, node: &EndNode<impl Ui>,
    ) {
        let mut max_d = 0;

        for index in text.printed_lines(label.height(), self) {
            let line = &text.lines()[index];
            let line_d = line.get_distance_to_col_node(line.char_count(), node);
            max_d = max(max_d, line_d);
        }

        self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    }
}

pub struct PrintedLines {
    file: RoState<Text>,
    print_info: RoState<PrintInfo>,
}

impl PrintedLines {
    /// Given an `EndNode`, figures out what lines should be printed.
    pub fn lines(&self, child_node: &EndNode<impl Ui>) -> Vec<usize> {
        let height = child_node.height();
        let (text, print_info) = (self.file.read(), self.print_info.read());
        let mut lines_iter = text.lines().iter().enumerate();
        let mut printed_lines = Vec::with_capacity(child_node.height());

        let top_line = lines_iter.nth(print_info.top_line).unwrap().1;
        let mut d_y = min(height, 1 + top_line.wrap_iter().count() - print_info.top_wraps);
        for _ in 0..d_y {
            printed_lines.push(print_info.top_line);
        }

        while let (Some((index, line)), true) = (lines_iter.next(), d_y < height) {
            let old_d_y = d_y;
            d_y = min(d_y + 1 + line.wrap_iter().count(), height);
            for _ in old_d_y..d_y {
                printed_lines.push(index);
            }
        }

        printed_lines
    }

    /// Wether or not the list of printed lines has changed.
    pub fn has_changed(&self) -> bool {
        self.file.has_changed() || self.print_info.has_changed()
    }
}

pub struct FileEditor {
    cursors: RwState<Vec<TextCursor>>,
    main_cursor: RwState<usize>,
    clearing_needed: bool,
}

impl FileEditor {
    /// Returns a new instance of `FileEditor`.
    pub fn new(cursors: RwState<Vec<TextCursor>>, main_cursor: RwState<usize>) -> Self {
        Self { cursors, main_cursor, clearing_needed: false }
    }

    /// Removes all intersecting cursors from the list, keeping only the last from the bunch.
    fn clear_intersections(&mut self) {
        let mut cursors = self.cursors.write();
        let mut last_range = cursors[0].range();
        let mut last_index = 0;
        let mut to_remove = Vec::new();

        for (index, cursor) in cursors.iter_mut().enumerate().skip(1) {
            if cursor.range().intersects(&last_range) {
                cursor.merge(&last_range);
                to_remove.push(last_index);
            }
            last_range = cursor.range();
            last_index = index;
        }

        for index in to_remove.iter().rev() {
            cursors.remove(*index);
        }
    }

    /// Edits on every cursor selection in the list.
    pub fn edit_on_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(Editor),
    {
        self.clear_intersections();
        let mut cursors = self.cursors.write();
        let mut splice_adder = SpliceAdder::default();
        cursors.iter_mut().for_each(|c| {
            c.calibrate_on_adder(&splice_adder);
            log_info!("\ncursor: {:#?}", c);
            splice_adder.reset_cols(&c.range().end);
            let cursor = Editor::new(c, &mut splice_adder);
            f(cursor);
        });
    }

    /// Alters every selection on the list.
    pub fn move_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(Mover),
    {
        let mut cursors = self.cursors.write();
        cursors.iter_mut().for_each(|c| {
            let cursor = Mover::new(c);
            f(cursor);
        });

        cursors.sort_unstable_by(|j, k| j.range().at_start_ord(&k.range()));

        drop(cursors);
        self.clearing_needed = true;
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(&mut Mover),
    {
        let mut cursors = self.cursors.write();
        let mut cursor = cursors.get_mut(index).expect("Invalid cursor index!");
        f(&mut Mover::new(&mut cursor));

        let cursor = cursors.remove(index);
        let range = cursor.range();
        let new_index = match cursors.binary_search_by(|j| j.range().at_start_ord(&range)) {
            Ok(index) => index,
            Err(index) => index,
        };
        cursors.insert(new_index, cursor);
        let mut main_cursor = self.main_cursor.write();
        if index == *main_cursor {
            *main_cursor = new_index;
        }

        drop(cursors);
        self.clearing_needed = true;
    }

    /// Alters the main cursor's selection.
    pub fn move_main<F>(&mut self, f: F)
    where
        F: FnMut(&mut Mover),
    {
        let main_cursor = *self.main_cursor.read();
        self.move_nth(f, main_cursor);
    }

    /// Alters the last cursor's selection.
    pub fn move_last<F>(&mut self, f: F)
    where
        F: FnMut(&mut Mover),
    {
        let cursors = self.cursors.read();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.move_nth(f, len - 1);
        }
    }

    /// Edits on the nth cursor's selection.
    pub fn edit_on_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(&mut Editor),
    {
        if self.clearing_needed {
            self.clear_intersections();
            self.clearing_needed = false;
        }

        let mut cursors = self.cursors.write();
        let mut cursor = cursors.get_mut(index).expect("Invalid cursor index!");
        let mut splice_adder = SpliceAdder::default();
        let mut edit_cursor = Editor::new(&mut cursor, &mut splice_adder);
        f(&mut edit_cursor);
        cursors.iter_mut().skip(index + 1).for_each(|c| c.calibrate_on_adder(&mut splice_adder));
    }

    /// Edits on the main cursor's selection.
    pub fn edit_on_main<F>(&mut self, f: F)
    where
        F: FnMut(&mut Editor),
    {
        let main_cursor = *self.main_cursor.read();
        self.edit_on_nth(f, main_cursor);
    }

    /// Edits on the last cursor's selection.
    pub fn edit_on_last<F>(&mut self, f: F)
    where
        F: FnMut(&mut Editor),
    {
        let cursors = self.cursors.read();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.edit_on_nth(f, len - 1);
        }
    }

    /// The main cursor index.
    pub fn main_cursor(&self) -> usize {
        *self.main_cursor.read()
    }

    pub fn rotate_main_forward(&mut self) {
        let mut main_cursor = self.main_cursor.write();
        *main_cursor =
            if *main_cursor == self.cursors.read().len() - 1 { 0 } else { *main_cursor + 1 }
    }

    pub fn clone_last(&mut self) {
        let mut cursors = self.cursors.write();
        let cursor = cursors.last().unwrap().clone();
        cursors.push(cursor);
    }

    pub fn push(&mut self, cursor: TextCursor) {
        self.cursors.write().push(cursor);
    }
}

/// The widget that is used to print and edit files.
pub struct FileWidget<M>
where
    M: Ui,
{
    pub(crate) text: RwState<Text>,
    print_info: RwState<PrintInfo>,
    pub(crate) main_cursor: RwState<usize>,
    pub(crate) cursors: RwState<Vec<TextCursor>>,
    pub(crate) node: EndNode<M>,
    history: History,
    do_set_print_info: bool,
}

unsafe impl<M> Send for FileWidget<M> where M: Ui {}

impl<M> Widget<M> for FileWidget<M>
where
    M: Ui,
{
    fn end_node(&self) -> &EndNode<M> {
        &self.node
    }

    fn end_node_mut(&mut self) -> &mut EndNode<M> {
        &mut self.node
    }

    fn update(&mut self) {
        if self.do_set_print_info {
            self.update_print_info();
        } else {
            self.do_set_print_info = true;
        }
        self.update();
    }

    fn needs_update(&self) -> bool {
        false
    }

    fn text(&self) -> RoState<Text> {
        RoState::from(&self.text)
    }

    fn print_info(&self) -> RoState<PrintInfo> {
        RoState::from(&self.print_info)
    }

    fn scroll_vertically(&mut self, d_y: i32) {
        self.print_info.write().scroll_vertically(d_y, &self.text.read());
    }

    fn resize(&mut self, node: &EndNode<M>) {
        for line in &mut self.text.write().lines {
            line.parse_wrapping(node);
        }
    }
}

impl<M> FileWidget<M>
where
    M: Ui,
{
    /// Returns a new instance of `FileWidget`.
    pub fn new(path: PathBuf, node: EndNode<M>, match_manager: &Option<MatchManager>) -> Self {
        // TODO: Sanitize the path further.
        let file_contents = fs::read_to_string(path).unwrap_or("".to_string());
        let text = RwState::new(Text::new(file_contents, match_manager.clone()));
        let read = text.read();
        let cursor = TextCursor::new(TextPos::default(), read.lines(), &node);
        drop(read);

        let mut file_widget = FileWidget {
            text,
            print_info: RwState::new(PrintInfo::default()),
            main_cursor: RwState::new(0),
            cursors: RwState::new(vec![cursor]),
            node,
            history: History::new(),
            do_set_print_info: true,
        };

        let mut text = file_widget.text.write();
        text.update_lines(&file_widget.node);
        drop(text);
        file_widget
    }

    /// Scrolls up or down, assuming that the lines cannot wrap.
    fn scroll_unwrapped(&mut self, target: TextPos, height: usize) {
        let info = &mut self.print_info.write();
        let scrolloff = self.node.config().scrolloff;

        if target.row > info.top_line + height - scrolloff.d_y {
            info.top_line += target.row + scrolloff.d_y - info.top_line - height;
        } else if target.row < info.top_line + scrolloff.d_y && info.top_line != 0 {
            info.top_line -= (info.top_line + scrolloff.d_y) - target.row
        }
    }

    /// Scrolls up, assuming that the lines can wrap.
    fn scroll_up(&mut self, target: TextPos, mut d_y: usize) {
        let scrolloff = self.node.config().scrolloff;
        let text = self.text.read();
        let lines_iter = text.lines().iter().take(target.row);
        let info = &mut self.print_info.write();

        // If the target line is above the top line, no matter what, a new top line is needed.
        let mut needs_new_top_line = target.row < info.top_line;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.wrap_iter().count();
            };

            if index == info.top_line {
                // This means we ran into the top line too early, and must scroll up.
                if d_y < scrolloff.d_y + info.top_wraps {
                    needs_new_top_line = true;
                // If this happens, we're in the middle of the screen, and don't need to scroll.
                } else if !needs_new_top_line {
                    break;
                }
            }

            if needs_new_top_line && (d_y >= scrolloff.d_y || index == 0) {
                info.top_line = index;
                info.top_wraps = d_y.saturating_sub(scrolloff.d_y);
                break;
            }
        }
    }

    /// Scrolls down, assuming that the lines can wrap.
    fn scroll_down(&mut self, target: TextPos, mut d_y: usize, height: usize) {
        let scrolloff = self.node.config().scrolloff;
        let text = self.text.read();
        let lines_iter = text.lines().iter().take(target.row + 1);
        let mut info = self.print_info.write();
        let mut top_offset = 0;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.wrap_iter().count();
            }

            if index == info.top_line {
                top_offset = info.top_wraps
            };

            if d_y >= height + top_offset - scrolloff.d_y {
                info.top_line = index;
                // If this equals 0, that means the distance has matched up perfectly,
                // i.e. the distance between the new `info.top_line` is exactly what's
                // needed for the full height. If it's greater than 0, `info.top_wraps`
                // needs to adjust where the line actually begins to match up.
                info.top_wraps = d_y + scrolloff.d_y - height;
                break;
            }

            // If this happens first, we're in the middle of the screen, and don't need to scroll.
            if index == info.top_line {
                break;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no folding is being used.
    fn scroll_horizontally(&mut self, target: TextPos, width: usize) {
        let mut info = self.print_info.write();
        let scrolloff = self.node.config().scrolloff;

        if let WrapMethod::NoWrap = self.node.config().wrap_method {
            let target_line = &self.text.read().lines[target.row];
            let distance = target_line.get_distance_to_col_node(target.col, &self.node);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance > info.x_shift + width - scrolloff.d_x {
                // Shift by the amount required to keep the cursor in bounds.
                info.x_shift = distance + scrolloff.d_x - width;
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance < info.x_shift + scrolloff.d_x {
                info.x_shift = distance.saturating_sub(scrolloff.d_x);
            }
        }
    }

    /// Updates the print info.
    fn update_print_info(&mut self) {
        let cursors = self.cursors.read();
        let main_cursor = &cursors.get(*self.main_cursor.read()).unwrap();
        let old = self.print_info.read().last_main;
        let new = main_cursor.caret();

        let text = self.text.read();

        let line = &text.lines()[min(old.row, text.lines().len() - 1)];
        let current_byte = line.get_line_byte_at(old.col);
        let cur_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

        let line = &text.lines()[new.row];
        let target_byte = line.get_line_byte_at(new.col);
        let old_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

        drop(text);
        drop(cursors);

        if let WrapMethod::NoWrap = self.node.config().wrap_method {
            self.scroll_unwrapped(new, self.node.height());
            self.scroll_horizontally(new, self.node.width());
        } else if new.row < old.row || (new.row == old.row && old_wraps < cur_wraps) {
            self.scroll_up(new, old_wraps);
        } else {
            self.scroll_down(new, old_wraps, self.node.height());
        }

        self.print_info.write().last_main = new;
    }

    /// Tbh, I don't remember what this is supposed to do, but it seems important.
    fn match_scroll(&mut self) {
        let text = self.text.read();
        let cursors = self.cursors.read();

        let main_cursor = cursors.get(*self.main_cursor.read()).unwrap();
        let limit_line = min(main_cursor.caret().row + self.node.height(), text.lines().len() - 1);
        let start = main_cursor.caret().translate(text.lines(), limit_line, 0);
        let target_line = &text.lines()[limit_line];
        let range = TextRange {
            start,
            end: TextPos {
                byte: start.byte + target_line.text().len(),
                col: target_line.char_count(),
                ..start
            },
        };
    }

    /// Edits the file with a cursor.
    pub fn edit(&mut self, editor: &mut Editor, edit: impl ToString) {
        self.history.start_if_needed();
        self.history.set_print_info(*self.print_info().read());
        let lines = split_string_lines(&edit.to_string());
        let mut text = self.text.write();

        let change = Change::new(&lines, editor.cursor.range(), &text.lines);
        editor.splice_adder.calibrate(&change.splice);

        text.apply_change(&change);

        editor.set_cursor_on_splice(&change.splice);
        let change_index = editor.cursor.change_index;
        let (insertion_index, change_diff) = self.history.add_change(change, change_index);
        editor.cursor.change_index = Some(insertion_index);
        editor.splice_adder.change_diff += change_diff;

        let max_line = max_line(&text, &self.print_info.read(), &self.node);
        update_range(&mut text, editor.cursor.range(), max_line, &self.node);
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self) {
        self.do_set_print_info = false;
        let mut text = self.text.write();

        let moment = match self.history.move_backwards() {
            Some(moment) => moment,
            None => return,
        };

        let mut info = self.print_info.write();
        *info = moment.print_info.unwrap_or(*info);

        let mut cursors = self.cursors.write();
        let mut cursors_iter = cursors.iter_mut();
        let mut new_cursors = Vec::new();

        let mut splice_adder = SpliceAdder::default();
        for change in &moment.changes {
            let mut splice = change.splice;

            splice.calibrate_on_adder(&splice_adder);
            splice_adder.reset_cols(&splice.added_end);

            text.undo_change(&change, &splice);

            splice_adder.calibrate(&splice.reverse());

            if let Some(cursor) = cursors_iter.next() {
                cursor.change_index = None;
                cursor.move_to_calibrated(splice.taken_end(), &text.lines, &self.node);
            } else {
                new_cursors.push(TextCursor::new(splice.taken_end(), &text.lines, &self.node));
            }

            let range = TextRange { start: splice.start(), end: splice.taken_end() };
            let max_line = max_line(&text, &info, &self.node);
            update_range(&mut text, range, max_line, &self.node);
        }

        drop(cursors);
        self.cursors.write().extend(new_cursors);
        unsafe { self.cursors.write().set_len(moment.changes.len()) };
    }

    /// Redoes the last moment in history.
    pub fn redo(&mut self) {
        self.do_set_print_info = false;
        let mut text = self.text.write();

        let moment = match self.history.move_forward() {
            Some(moment) => moment,
            None => return,
        };

        let mut info = self.print_info.write();
        *info = moment.print_info.unwrap_or(*info);

        let mut cursors = self.cursors.write();
        let mut cursors_iter = cursors.iter_mut();
        let mut new_cursors = Vec::new();

        for change in &moment.changes {
            text.apply_change(&change);

            let splice = change.splice;
            if let Some(cursor) = cursors_iter.next() {
                cursor.change_index = None;
                cursor.move_to_calibrated(splice.added_end(), &text.lines, &self.node);
            } else {
                new_cursors.push(TextCursor::new(splice.added_end(), &text.lines, &self.node));
            }

            let range = TextRange { start: splice.start(), end: splice.added_end() };
            let max_line = max_line(&text, &info, &self.node);
            update_range(&mut text, range, max_line, &self.node);
        }

        drop(cursors);
        self.cursors.write().extend(new_cursors);
        unsafe { self.cursors.write().set_len(moment.changes.len()) };
    }

    /// Removes the tags for all the cursors, used before they are expected to move.
    pub(crate) fn remove_cursor_tags(&mut self) {
        let mut text = self.text.write();

        for (index, cursor) in self.cursors.read().iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let tag = if index == *self.main_cursor.read() {
                CharTag::PrimaryCursor
            } else {
                CharTag::SecondaryCursor
            };

            let pos_list = [
                (cursor.caret(), tag),
                (start, CharTag::SelectionStart),
                (end, CharTag::SelectionEnd),
            ];

            let no_selection = if start == end { 1 } else { 3 };

            for (pos, tag) in pos_list.iter().take(no_selection) {
                if let Some(line) = text.lines.get_mut(pos.row) {
                    let byte = line.get_line_byte_at(pos.col);
                    line.info.char_tags.remove_first(|(n, t)| n as usize == byte && t == *tag);
                }
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are expected to have moved.
    pub(crate) fn add_cursor_tags(&mut self) {
        let mut text = self.text.write();

        for (index, cursor) in self.cursors.read().iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let tag = if index == *self.main_cursor.read() {
                CharTag::PrimaryCursor
            } else {
                CharTag::SecondaryCursor
            };

            let pos_list = [
                (cursor.caret(), tag),
                (start, CharTag::SelectionStart),
                (end, CharTag::SelectionEnd),
            ];

            let no_selection = if start == end { 1 } else { 3 };

            for (pos, tag) in pos_list.iter().take(no_selection) {
                if let Some(line) = text.lines.get_mut(pos.row) {
                    let byte = line.get_line_byte_at(pos.col) as u32;
                    line.info.char_tags.insert((byte, *tag));
                    log_info!("\n{:#?}", line.info.char_tags);
                }
            }
        }
    }

    /// Currently does nothing.
    fn update(&mut self) {
        self.match_scroll();
    }

    /// The list of all lines that are currently printed on the screen.
    pub fn printed_lines(&self) -> PrintedLines {
        PrintedLines {
            file: RoState::from(&self.text),
            print_info: RoState::from(&self.print_info),
        }
    }

    /// The object used to edit the file through cursors.
    pub fn file_editor(&mut self) -> FileEditor {
        FileEditor::new(self.cursors.clone(), self.main_cursor.clone())
    }

    pub fn history(&self) -> &History {
        &self.history
    }
}

// NOTE: Will definitely break once folding becomes a thing.
/// The last line that could possibly be printed.
fn max_line(text: &Text, print_info: &PrintInfo, node: &EndNode<impl Ui>) -> usize {
    min(print_info.top_line + node.height(), text.lines().len() - 1)
}
