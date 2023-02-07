use std::{
    cmp::{max, min},
    fs,
    path::{Path, PathBuf},
};

use crate::{
    action::{History, TextRange},
    config::{RwData, WrapMethod},
    cursor::{Editor, Mover, SpliceAdder, TextCursor, TextPos},
    max_line,
    tags::{CharTag, MatchManager},
    text::{update_range, Text},
    ui::{Area, EndNode, Label, Ui},
};

use super::{EditableWidget, Widget};

// NOTE: The defaultness in here, when it comes to `last_main`, main cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// How many times the row at the top wraps around before being shown.
    pub top_wraps: usize,
    /// The index of the row at the top of the screen.
    pub top_row: usize,
    /// How shifted the text is to the left.
    pub x_shift: usize,
    /// The last position of the main cursor.
    pub last_main: TextPos,
}

impl PrintInfo {
    /// Scrolls the `PrintInfo` vertically by a given amount, on a given file.
    fn scroll_vertically(&mut self, mut d_y: i32, text: &Text) {
        if d_y > 0 {
            let mut lines_iter = text.lines().iter().skip(self.top_row);

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.wrap_iter().count();
                if (wrap_count + 1) as i32 > d_y {
                    self.top_wraps = d_y as usize;
                    break;
                } else {
                    self.top_row += 1;
                    d_y -= (wrap_count + 1) as i32;
                }
            }
        } else if d_y < 0 {
            let mut lines_iter = text.lines().iter().take(self.top_row).rev();

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.wrap_iter().count();
                if ((wrap_count + 1) as i32) < d_y {
                    self.top_wraps = -d_y as usize;
                    break;
                } else {
                    self.top_row -= 1;
                    d_y += (wrap_count + 1) as i32;
                }
            }
        }
    }

    fn scroll_horizontally<U>(&mut self, d_x: i32, text: &Text, node: &EndNode<U>)
    where
        U: Ui,
    {
        let mut max_d = 0;
        let label = node.label.read();
        let config = node.config().read();

        for index in text.printed_lines(label.area().height(), self) {
            let line = &text.lines()[index];
            let line_d = line.get_distance_to_col::<U>(line.char_count(), &label, &config);
            max_d = max(max_d, line_d);
        }

        self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    }
}

/// The widget that is used to print and edit files.
pub struct FileWidget<U>
where
    U: Ui,
{
    pub(crate) end_node: RwData<EndNode<U>>,
    name: RwData<String>,
    pub(crate) text: Text,
    print_info: PrintInfo,
    pub(crate) main_cursor: usize,
    // The `Box` here is used in order to comply with `RoState` printability.
    pub(crate) cursors: Vec<TextCursor>,
    pub(crate) history: History,
    do_set_print_info: bool,
    do_add_cursor_tags: bool,
}

impl<U> FileWidget<U>
where
    U: Ui,
{
    /// Returns a new instance of `FileWidget`.
    pub fn new(
        path: &PathBuf, node: RwData<EndNode<U>>, match_manager: &Option<MatchManager>,
    ) -> Self {
        // TODO: Allow the creation of a new file.
        let file_contents = fs::read_to_string(path).expect("Failed to read the file.");
        let text = Text::new(file_contents, match_manager.clone());
        let cursor = TextCursor::new(TextPos::default(), text.lines(), &node.read());

        let mut file_widget = FileWidget {
            name: RwData::new(path.file_name().unwrap().to_string_lossy().to_string()),
            text,
            print_info: PrintInfo::default(),
            main_cursor: 0,
            cursors: vec![cursor],
            end_node: node,
            history: History::new(),
            do_set_print_info: true,
            do_add_cursor_tags: false,
        };

        file_widget.text.update_lines(&file_widget.end_node.read());
        file_widget.add_cursor_tags();
        file_widget
    }

    /// Scrolls up or down, assuming that the lines cannot wrap.
    fn scroll_unwrapped(&mut self, target: TextPos, height: usize) {
        let info = &mut self.print_info;
        let scrolloff = self.end_node.read().config().read().scrolloff;

        if target.row > info.top_row + height - scrolloff.d_y {
            self.print_info.top_row +=
                target.row + scrolloff.d_y - self.print_info.top_row - height;
        } else if target.row < info.top_row + scrolloff.d_y && info.top_row != 0 {
            info.top_row -= (info.top_row + scrolloff.d_y) - target.row
        }
    }

    /// Scrolls up, assuming that the lines can wrap.
    fn scroll_up(&mut self, target: TextPos, mut d_y: usize) {
        let scrolloff = self.end_node.read().config().read().scrolloff;
        let lines_iter = self.text.lines().iter().take(target.row);
        let info = &mut self.print_info;

        // If the target line is above the top line, no matter what, a new top line is needed.
        let mut needs_new_top_line = target.row < info.top_row;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.wrap_iter().count();
            };

            if index == info.top_row {
                // This means we ran into the top line too early, and must scroll up.
                if d_y < scrolloff.d_y + info.top_wraps {
                    needs_new_top_line = true;
                // If this happens, we're in the middle of the screen, and don't need to scroll.
                } else if !needs_new_top_line {
                    break;
                }
            }

            if needs_new_top_line && (d_y >= scrolloff.d_y || index == 0) {
                info.top_row = index;
                info.top_wraps = d_y.saturating_sub(scrolloff.d_y);
                break;
            }
        }
    }

    /// Scrolls down, assuming that the lines can wrap.
    fn scroll_down(&mut self, target: TextPos, mut d_y: usize, height: usize) {
        let scrolloff = self.end_node.read().config().read().scrolloff;
        let lines_iter = self.text.lines().iter().take(target.row + 1);
        let mut top_offset = 0;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.wrap_iter().count();
            }

            if index == self.print_info.top_row {
                top_offset = self.print_info.top_wraps
            };

            if d_y >= height + top_offset - scrolloff.d_y {
                self.print_info.top_row = index;
                // If this equals 0, that means the distance has matched up perfectly,
                // i.e. the distance between the new `info.top_line` is exactly what's
                // needed for the full height. If it's greater than 0, `info.top_wraps`
                // needs to adjust where the line actually begins to match up.
                self.print_info.top_wraps = d_y + scrolloff.d_y - height;
                break;
            }

            // If this happens first, we're in the middle of the screen, and don't need to scroll.
            if index == self.print_info.top_row {
                break;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is being used.
    fn scroll_horizontally(&mut self, target: TextPos, width: usize) {
        let node = self.end_node.read();
        let config = node.config().read();
        let label = node.label.read();

        if let WrapMethod::NoWrap = config.wrap_method {
            let target_line = &self.text.lines[target.row];
            let distance = target_line.get_distance_to_col::<U>(target.col, &label, &config);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance > self.print_info.x_shift + width - config.scrolloff.d_x {
                // Shift by the amount required to keep the cursor in bounds.
                self.print_info.x_shift = distance + config.scrolloff.d_x - width;
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance < self.print_info.x_shift + config.scrolloff.d_x {
                self.print_info.x_shift = distance.saturating_sub(config.scrolloff.d_x);
            }
        }
    }

    /// Updates the print info.
    fn update_print_info(&mut self) {
        let node = self.end_node.read();
        let wrap_method = node.config().read().wrap_method;
        let (height, width) = (node.label.read().area().height(), node.label.read().area().width());
        drop(node);

        let main_cursor = self.main_cursor();
        let old = self.print_info.last_main;
        let new = main_cursor.caret();

        let line = &self.text.lines()[min(old.row, self.text.lines().len() - 1)];
        let current_byte = line.get_line_byte_at(old.col);
        let cur_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

        let line = &self.text.lines()[new.row];
        let target_byte = line.get_line_byte_at(new.col);
        let old_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

        if let WrapMethod::NoWrap = wrap_method {
            self.scroll_unwrapped(new, height);
            self.scroll_horizontally(new, width);
        } else if new.row < old.row || (new.row == old.row && old_wraps < cur_wraps) {
            self.scroll_up(new, old_wraps);
        } else {
            self.scroll_down(new, old_wraps, height);
        }

        self.print_info.last_main = new;
    }

    /// Tbh, I don't remember what this is supposed to do, but it seems important.
    fn _match_scroll(&mut self) {
        let node = self.end_node.read();
        let label = node.label.read();

        let main_cursor = self.main_cursor();
        let limit_row =
            min(main_cursor.caret().row + label.area().height(), self.text.lines().len() - 1);
        let start = main_cursor.caret().translate(self.text.lines(), limit_row, 0);
        let target_line = &self.text.lines()[limit_row];
        let _range = TextRange {
            start,
            end: TextPos {
                byte: start.byte + target_line.text().len(),
                col: target_line.char_count(),
                ..start
            },
        };
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self) {
        let end_node = self.end_node.read();

        self.do_set_print_info = false;

        let moment = match self.history.move_backwards() {
            Some(moment) => moment,
            None => return,
        };

        self.print_info = moment.starting_print_info;

        self.cursors.clear();

        let mut splice_adder = SpliceAdder::default();
        for change in &moment.changes {
            let mut splice = change.splice;

            splice.calibrate_on_adder(&splice_adder);
            splice_adder.reset_cols(&splice.added_end);

            self.text.undo_change(&change, &splice);

            splice_adder.calibrate(&splice.reverse());

            self.cursors.push(TextCursor::new(splice.taken_end(), &self.text.lines, &end_node));

            let range = TextRange { start: splice.start(), end: splice.taken_end() };
            let max_line = max_line(&self.text, &self.print_info, &self.end_node.read());
            update_range(&mut self.text, range, max_line, &self.end_node.read());
        }
    }

    /// Redoes the last moment in history.
    pub fn redo(&mut self) {
        let end_node = self.end_node.read();

        self.do_set_print_info = false;

        let moment = match self.history.move_forward() {
            Some(moment) => moment,
            None => return,
        };

        self.print_info = moment.ending_print_info;

        self.cursors.clear();

        for change in &moment.changes {
            self.text.apply_change(&change);

            let splice = change.splice;

            self.cursors.push(TextCursor::new(splice.added_end(), &self.text.lines, &end_node));

            let range = TextRange { start: splice.start(), end: splice.added_end() };
            let max_line = max_line(&self.text, &self.print_info, &self.end_node.read());
            update_range(&mut self.text, range, max_line, &self.end_node.read());
        }
    }

    /// Removes the tags for all the cursors, used before they are expected to move.
    pub(crate) fn remove_cursor_tags(&mut self) {
        for (index, cursor) in self.cursors.iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = if index == self.main_cursor {
                (CharTag::MainCursor, CharTag::MainSelectionStart, CharTag::MainSelectionEnd)
            } else {
                (
                    CharTag::SecondaryCursor,
                    CharTag::SecondarySelectionStart,
                    CharTag::SecondarySelectionEnd,
                )
            };

            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                if let Some(line) = self.text.lines.get_mut(pos.row) {
                    let byte = line.get_line_byte_at(pos.col);
                    line.info.char_tags.remove_first(|(n, t)| n as usize == byte && t == *tag);
                }
            }
        }

        self.do_add_cursor_tags = true;
    }

    /// Adds the tags for all the cursors, used after they are expected to have moved.
    pub(crate) fn add_cursor_tags(&mut self) {
        for (index, cursor) in self.cursors.iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = if index == self.main_cursor {
                (CharTag::MainCursor, CharTag::MainSelectionStart, CharTag::MainSelectionEnd)
            } else {
                (
                    CharTag::SecondaryCursor,
                    CharTag::SecondarySelectionStart,
                    CharTag::SecondarySelectionEnd,
                )
            };

            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                if let Some(line) = self.text.lines.get_mut(pos.row) {
                    let byte = line.get_line_byte_at(pos.col) as u32;
                    line.info.char_tags.insert((byte, *tag));
                }
            }
        }
    }

    /// Returns the currently printed set of lines.
    pub fn printed_lines(&self) -> Vec<usize> {
        let end_node = self.end_node.read();
        let label = end_node.label.read();
        let height = label.area().height();
        let mut lines_iter = self.text.lines().iter().enumerate();
        let mut printed_lines = Vec::with_capacity(label.area().height());

        let top_line = lines_iter.nth(self.print_info.top_row).unwrap().1;
        let mut d_y = min(height, 1 + top_line.wrap_iter().count() - self.print_info.top_wraps);
        for _ in 0..d_y {
            printed_lines.push(self.print_info.top_row);
        }

        while let (Some((index, line)), true) = (lines_iter.next(), d_y < height) {
            let old_d_y = d_y;
            d_y = min(d_y + line.wrap_iter().count(), height);
            for _ in old_d_y..=d_y {
                printed_lines.push(index);
            }
        }

        printed_lines
    }

    // TODO: Move the history to a general placement, taking in all the files.
    /// The history associated with this file.
    pub fn history(&self) -> &History {
        &self.history
    }

    /// Ends the current moment and starts a new one.
    pub fn new_moment(&mut self) {
        self.cursors.iter_mut().for_each(|cursor| cursor.assoc_index = None);
        self.history.new_moment(self.print_info);
    }

    /// The list of `TextCursor`s on the file.
    pub fn cursors(&self) -> Vec<TextCursor> {
        self.cursors.clone()
    }

    ////////// Status line convenience functions:
    /// The main cursor of the file.
    pub fn main_cursor(&self) -> TextCursor {
        *self.cursors.get(self.main_cursor).unwrap()
    }

    /// The file's name.
    pub fn name(&self) -> String {
        self.name.read().clone()
    }

    pub fn full_path(&self) -> String {
        let mut path = std::env::current_dir().unwrap();
        path.push(Path::new(&self.name.read().as_str()));
        path.to_string_lossy().to_string()
    }

    /// The lenght of the file, in lines.
    pub fn len(&self) -> usize {
        self.text.lines().len()
    }

    pub fn node(&self) -> &RwData<EndNode<U>> {
        &self.end_node
    }

    pub fn print_info(&self) -> PrintInfo {
        self.print_info
    }
}

impl<U> Widget<U> for FileWidget<U>
where
    U: Ui,
{
    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.end_node
    }

    fn update(&mut self) {
        if self.do_set_print_info {
            self.update_print_info();
        } else {
            self.do_set_print_info = true;
        }

        let mut node = self.end_node.write();
        self.text.update_lines(&mut node);
        drop(node);
        //self.match_scroll();

        if self.do_add_cursor_tags {
            self.add_cursor_tags();
            self.do_add_cursor_tags = false
        }
    }

    fn needs_update(&self) -> bool {
        true
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print(&mut self) {
        self.text.print(&mut self.end_node.write(), self.print_info);
    }

    fn scroll_vertically(&mut self, d_y: i32) {
        self.print_info.scroll_vertically(d_y, &self.text);
    }

    fn resize(&mut self, node: &EndNode<U>) {
        for line in &mut self.text.lines {
            line.parse_wrapping::<U>(&node.label.read(), &node.config().read());
        }
    }
}

impl<U> EditableWidget<U> for FileWidget<U>
where
    U: Ui,
{
    fn editor<'a>(&'a mut self, index: usize, splice_adder: &'a mut SpliceAdder) -> Editor<U> {
        Editor::new(
            &mut self.cursors[index],
            splice_adder,
            &mut self.text,
            &self.end_node,
            None,
            None,
        )
    }

    fn mover(&mut self, index: usize) -> Mover<U> {
        Mover::new(
            &mut self.cursors[index],
            &self.text,
            &self.end_node,
            self.history.current_moment(),
        )
    }

    fn cursors(&self) -> &[TextCursor] {
        self.cursors.as_slice()
    }

    fn main_cursor_index(&self) -> usize {
        self.main_cursor
    }

    fn mut_cursors(&mut self) -> Option<&mut Vec<TextCursor>> {
        Some(&mut self.cursors)
    }

    fn mut_main_cursor_index(&mut self) -> Option<&mut usize> {
        Some(&mut self.main_cursor)
    }

    fn new_moment(&mut self) {
        self.new_moment();
    }

    fn undo(&mut self) {
        self.undo()
    }

    fn redo(&mut self) {
        self.redo()
    }
}

unsafe impl<M> Send for FileWidget<M> where M: Ui {}
