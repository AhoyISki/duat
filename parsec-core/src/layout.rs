use std::{
    cmp::{max, min},
    fmt::Write,
    fs,
    path::PathBuf,
    sync::Mutex,
    thread,
};

use crossterm::event::{self, Event, KeyCode};

use crate::{
    action::{Change, History, Splice, TextRange},
    config::{Config, LineNumbers, RoState, RwState, WrapMethod},
    cursor::{TextCursor, TextPos},
    file::{update_range, Text},
    input::{EditingScheme, FileRemapper},
    saturating_add_signed, split_string_lines,
    tags::{CharTag, MatchManager},
    ui::{Direction, EndNode, Label, MidNode, NodeManager, Split, Ui},
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// How many times the line at the top wraps around before being shown.
    pub top_wraps: usize,
    /// The index of the line at the top of the screen.
    pub top_line: usize,
    /// How shifted the text is to the left.
    pub x_shift: usize,
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

        self.x_shift = min(saturating_add_signed(self.x_shift, d_x as isize), max_d);
    }
}

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait Widget<U>: Send
where
    U: Ui,
{
    /// Returns the `ChildNode` associated with this area.
    fn end_node(&self) -> &EndNode<U>;

    fn end_node_mut(&mut self) -> &mut EndNode<U>;

    fn update(&mut self);

    fn needs_update(&self) -> bool;

    fn text(&self) -> RoState<Text>;

    /// Returns the printing information of the file.
    fn print_info(&self) -> RoState<PrintInfo> {
        RoState::new(PrintInfo::default())
    }

    /// Scrolls the text vertically by an amount.
    fn scroll_vertically(&mut self, d_y: i32) {}

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, node: &EndNode<U>) {}
}

/// An area where you can edit the text.
pub trait EditArea<M>: Widget<M>
where
    M: Ui,
{
    /// Returns a mutable reference to the text.
    fn mut_text(&mut self) -> &mut Text;

    /// Gets the cursors on the area.
    ///
    /// # Returns
    ///
    /// * A list of cursors. This includes cursors that shouldn't be printed on screen.
    /// * The index of the main cursor. Most of the time, this will be 0.
    fn cursors(&mut self) -> (&mut Vec<TextCursor>, usize);
}

pub struct PrintedLines {
    file: RoState<Text>,
    print_info: RoState<PrintInfo>,
}

impl PrintedLines {
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

    pub fn has_changed(&self) -> bool {
        self.file.has_changed() || self.print_info.has_changed()
    }
}

pub struct FileWidget<M>
where
    M: Ui,
{
    text: RwState<Text>,
    print_info: RwState<PrintInfo>,
    main_cursor: RwState<usize>,
    cursors: RwState<Vec<TextCursor>>,
    node: EndNode<M>,
    history: History,
}

unsafe impl<M> Send for FileWidget<M> where M: Ui {}

impl<M> FileWidget<M>
where
    M: Ui,
{
    fn new(path: PathBuf, node: EndNode<M>, match_manager: &Option<MatchManager>) -> Self {
        // TODO: Sanitize the path further.
        let file_contents = fs::read_to_string(path).unwrap_or("".to_string());
        let text = RwState::new(Text::new(file_contents, match_manager.clone()));
        let read = text.read();
        let cursor = TextCursor::new(TextPos::default(), read.lines(), &node);
        drop(read);

        FileWidget {
            text,
            print_info: RwState::new(PrintInfo::default()),
            main_cursor: RwState::new(0),
            cursors: RwState::new(vec![cursor]),
            node,
            history: History::new(),
        }
    }

    fn scroll_unwrapped(&mut self, target: TextPos, height: usize) {
        let info = &mut self.print_info.write();
        let scrolloff = self.node.config().scrolloff;

        if target.row > info.top_line + height - scrolloff.d_y {
            info.top_line += target.row + scrolloff.d_y - info.top_line - height;
        } else if target.row < info.top_line + scrolloff.d_y && info.top_line != 0 {
            info.top_line -= (info.top_line + scrolloff.d_y) - target.row
        }
    }

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

    fn update_print_info(&mut self) {
        let cursors = self.cursors.read();
        let main_cursor = cursors.get(*self.main_cursor.read()).unwrap();
        let prev = main_cursor.prev();
        let cur = main_cursor.cur();

        let text = self.text.read();

        let line = &text.lines()[prev.row];
        let current_byte = line.get_line_byte_at(prev.col);
        let cur_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

        let line = &text.lines()[cur.row];
        let target_byte = line.get_line_byte_at(cur.col);
        let prev_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

        let d_y = prev_wraps;

        drop(text);
        drop(cursors);

        if let WrapMethod::NoWrap = self.node.config().wrap_method {
            self.scroll_unwrapped(cur, self.node.height());
            self.scroll_horizontally(cur, self.node.width());
        } else if cur.row < prev.row || (cur.row == prev.row && prev_wraps < cur_wraps) {
            self.scroll_up(cur, d_y);
        } else {
            self.scroll_down(cur, d_y, self.node.height());
        }
    }

    fn match_scroll(&mut self) {
        let text = self.text.read();
        let cursors = self.cursors.read();

        let main_cursor = cursors.get(*self.main_cursor.read()).unwrap();
        let limit_line = min(main_cursor.cur().row + self.node.height(), text.lines().len() - 1);
        let start = main_cursor.cur().calibrated_cursor(text.lines(), limit_line, 0);
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

    pub fn edit(&mut self, edit_cursor: &mut EditCursor, edit: impl ToString) {
        let lines = split_string_lines(&edit.to_string());
        let mut text = self.text.write();

        let cur_change = Change::new(&lines, edit_cursor.0.range(), &text.lines);
        edit_cursor.1.calibrate(&cur_change.splice);

        text.apply_change(&cur_change);
        let max_line = max_line(&text, &self.print_info.read(), &self.node);
        update_range(&mut text, edit_cursor.0.range(), max_line, &self.node);

        let range = edit_cursor.0.range();
        if let Some(change) = &mut edit_cursor.0.change {
            if let Err(()) = change.try_merge(&lines, range) {
                self.history.add_change(&change);
                *change = cur_change;
            }
        } else {
            edit_cursor.0.change = Some(cur_change);
        }
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self) {
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

        let max_line = max_line(&text, &info, &self.node);
        for change in moment.changes.iter().rev() {
            text.undo_change(&change);

            let splice = change.splice;
            if let Some(cursor) = cursors_iter.next() {
                cursor.move_to_inner(splice.taken_end(), &text.lines, &self.node);
            } else {
                new_cursors.push(TextCursor::new(splice.taken_end(), &text.lines, &self.node));
            }

            let range = TextRange { start: splice.start(), end: splice.taken_end() };
            update_range(&mut text, range, max_line, &self.node);
        }

        drop(cursors);
        self.cursors.write().extend(new_cursors);
    }

    /// Redoes the last moment in history.
    pub fn redo(&mut self) {
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

        let max_line = max_line(&text, &info, &self.node);
        for change in &moment.changes {
            text.apply_change(&change);

            let splice = change.splice;
            if let Some(cursor) = cursors_iter.next() {
                cursor.move_to_inner(splice.added_end(), &text.lines, &self.node);
            } else {
                new_cursors.push(TextCursor::new(splice.added_end(), &text.lines, &self.node));
            }

            let range = TextRange { start: splice.start(), end: splice.added_end() };
            update_range(&mut text, range, max_line, &self.node);
        }

        drop(cursors);
        self.cursors.write().extend(new_cursors);
    }

    fn update(&mut self) {
        self.update_print_info();
        let mut text = self.text.write();
        let cursors = self.cursors.read();

        for (index, cursor) in self.cursors.read().iter().enumerate() {
            let prev = cursor.prev();
            let cur = cursor.cur();

            let tag = if index == *self.main_cursor.read() {
                CharTag::PrimaryCursor
            } else {
                CharTag::SecondaryCursor
            };

            if let Some(line) = text.lines.get_mut(prev.row) {
                let char_tags = &mut line.info.char_tags;
                char_tags.remove_first(|(n, t)| n as usize == prev.col && t == tag);

                let line = &mut text.lines[cur.row];
                let byte = line.get_line_byte_at(cur.col);
                line.info.char_tags.insert((byte as u32, tag));
            }
        }

        // Updates the information for each cursor in the file.
        drop(text);
        drop(cursors);
        self.cursors.write().iter_mut().for_each(|c| c.update());

        self.match_scroll();
    }

    pub fn printed_lines(&self) -> PrintedLines {
        PrintedLines {
            file: RoState::from_rw(self.text.clone()),
            print_info: RoState::from_rw(self.print_info.clone()),
        }
    }

    pub fn cursor_list(&mut self) -> FileEditor {
        FileEditor::new(self.cursors.clone(), *self.main_cursor.read())
    }
}

pub struct FileEditor {
    cursors: RwState<Vec<TextCursor>>,
    main_cursor: usize,
}

// NOTE: This is dependant on the list of cursors being sorted, if it is not, that is UB.
/// A helper, for dealing with recalibration of cursors.
#[derive(Default)]
pub struct SpliceAdder {
    pub last_line: usize,
    pub lines: usize,
    pub bytes: usize,
    pub cols: usize,
}

impl SpliceAdder {
    // NOTE: It depends on the `Splice`s own calibration by the previous state of `self`.
    /// Calibrates, given a splice.
    fn calibrate(&mut self, splice: &Splice) {
        self.lines += splice.added_end.row - splice.taken_end.row;
        self.bytes += splice.added_end.byte - splice.taken_end.byte;
        if self.last_line == splice.added_end.row {
            if splice.added_range().lines().count() == 1 {
                self.cols += splice.added_end.col - splice.taken_end.col;
            } else {
                self.cols = splice.added_end.col;
            }
        } else {
            self.cols = 0;
        }
        self.last_line = splice.added_end.row;
    }
}

pub struct EditCursor<'a>(&'a mut TextCursor, &'a mut SpliceAdder);
pub struct MoveCursor<'a>(&'a mut TextCursor);

impl<'a> MoveCursor<'a> {
    ////////// Public movement functions

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_ver(&mut self, count: i32, file: &FileWidget<impl Ui>) {
        self.0.move_ver_inner(count, file.text().read().lines(), file.end_node());
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_hor(&mut self, count: i32, file: &FileWidget<impl Ui>) {
        self.0.move_hor_inner(count, file.text().read().lines(), file.end_node());
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, pos: TextPos, file: &FileWidget<impl Ui>) {
        self.0.move_to_inner(pos, file.text().read().lines(), file.end_node());
    }
}

impl FileEditor {
    pub fn new(cursors: RwState<Vec<TextCursor>>, main_cursor: usize) -> Self {
        Self { cursors, main_cursor }
    }

    pub fn edit_on_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(EditCursor),
    {
        let mut cursors = self.cursors.write();
        let mut splice_adder = SpliceAdder::default();
        cursors.iter_mut().for_each(|c| {
            c.calibrate(&splice_adder);
            let cursor = EditCursor(c, &mut splice_adder);
            f(cursor);
        });
    }

    pub fn move_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(MoveCursor),
    {
        let mut cursors = self.cursors.write();
        cursors.iter_mut().for_each(|c| {
            let cursor = MoveCursor(c);
            f(cursor)
        });
        // TODO: Smart resort algorithm.
    }

    pub fn move_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(&mut MoveCursor),
    {
        let mut cursors = self.cursors.write();
        let mut cursor = cursors.get_mut(index).expect("Invalid cursor index!");
        f(&mut MoveCursor(&mut cursor));
    }

    pub fn move_main<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut MoveCursor),
    {
        self.move_nth(f, self.main_cursor);
    }

    pub fn move_last<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut MoveCursor),
    {
        let cursors = self.cursors.read();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.move_nth(f, len - 1);
        }
    }

    pub fn edit_on_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(&mut EditCursor),
    {
        let mut cursors = self.cursors.write();
        let mut cursor = cursors.get_mut(index).expect("Invalid cursor index!");
        let mut splice_adder = SpliceAdder::default();
        let mut edit_cursor = EditCursor(&mut cursor, &mut splice_adder);
        f(&mut edit_cursor);
        if let Some(splice) = cursor.change.as_ref().map(|a| a.splice) {
            let added_lines = splice.added_end.row - splice.taken_end.row;
            let added_bytes = splice.added_end.byte - splice.taken_end.byte;
            cursors.iter_mut().skip(index + 1).for_each(|c| c.calibrate(&splice_adder));
        }
    }

    pub fn edit_on_main<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut EditCursor),
    {
        self.edit_on_nth(f, self.main_cursor);
    }

    pub fn edit_on_last<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut EditCursor),
    {
        let cursors = self.cursors.read();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.edit_on_nth(f, len - 1);
        }
    }

    pub fn main_cursor(&self) -> usize {
        self.main_cursor
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
        self.update();
    }

    fn needs_update(&self) -> bool {
        false
    }

    fn text(&self) -> RoState<Text> {
        self.text.to_ro()
    }

    fn print_info(&self) -> RoState<PrintInfo> {
        self.print_info.to_ro()
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

pub struct StatusWidget<U>
where
    U: Ui,
{
    file_name: String,
    child_node: EndNode<U>,
}

pub struct LineNumbersWidget<U>
where
    U: Ui,
{
    node: EndNode<U>,
    printed_lines: PrintedLines,
    main_cursor: RoState<usize>,
    cursors: RoState<Vec<TextCursor>>,
    text: RwState<Text>,
}

unsafe impl<M> Send for LineNumbersWidget<M> where M: Ui {}

impl<U> LineNumbersWidget<U>
where
    U: Ui,
{
    fn new(
        file_widget: &mut FileWidget<U>, node_manager: &mut NodeManager<U>,
    ) -> (Self, MidNode<U>) {
        let mut split = 3;
        let mut num_exp = 10;
        let text = file_widget.text.write();

        while text.lines().len() > num_exp {
            num_exp *= 10;
            split += 1;
        }
        drop(text);

        let node = &mut file_widget.node;
        let (parent_node, child_node) =
            node_manager.split_end(node, Direction::Left, Split::Static(split), true);
        let printed_lines = file_widget.printed_lines();
        let main_cursor = file_widget.main_cursor.to_ro();
        let cursors = file_widget.cursors.to_ro();

        let mut line_numbers = LineNumbersWidget {
            node: child_node,
            printed_lines,
            main_cursor,
            cursors,
            text: RwState::new(Text::default()),
        };

        line_numbers.update();

        (line_numbers, parent_node)
    }
}

impl<M> Widget<M> for LineNumbersWidget<M>
where
    M: Ui,
{
    fn update(&mut self) {
        let lines = self.printed_lines.lines(&self.end_node());
        let main_line = self.cursors.read().get(*self.main_cursor.read()).unwrap().prev().row;

        // 3 is probably the average length of the numbers, in digits, plus 1 for each "\n".
        let mut line_numbers = String::with_capacity(4 * lines.len());

        match self.node.config().line_numbers {
            LineNumbers::Absolute => {
                lines.iter().for_each(|&n| write!(&mut line_numbers, "{}\n", n).unwrap());
            }
            LineNumbers::Relative => {
                lines.iter().for_each(|&n| {
                    write!(&mut line_numbers, "{}\n", usize::abs_diff(n, main_line)).unwrap()
                });
            }
            LineNumbers::Hybrid => {
                lines.iter().for_each(|&n| {
                    write!(
                        &mut line_numbers,
                        "{}\n",
                        if n != main_line { usize::abs_diff(n, main_line) } else { n }
                    )
                    .unwrap()
                });
            }
            LineNumbers::None => panic!("How the hell did you get here?"),
        }

        let mut text = self.text.write();
        *text = Text::new(line_numbers, None);
    }

    fn needs_update(&self) -> bool {
        self.printed_lines.has_changed()
    }

    fn text(&self) -> RoState<Text> {
        self.text.to_ro()
    }

    fn end_node(&self) -> &EndNode<M> {
        &self.node
    }

    fn end_node_mut(&mut self) -> &mut EndNode<M> {
        &mut self.node
    }
}

pub struct OneStatusLayout<U>
where
    U: Ui,
{
    node_manager: NodeManager<U>,
    status: StatusWidget<U>,
    widgets: Vec<Mutex<(Box<dyn Widget<U>>, WidgetPrinter<U>)>>,
    files: Vec<(FileWidget<U>, Option<MidNode<U>>, WidgetPrinter<U>)>,
    master_node: MidNode<U>,
    match_manager: MatchManager,
}

/// A form of organizing the areas on a window.
pub trait Layout<U>
where
    U: Ui,
{
    fn open_file(&mut self, path: &PathBuf);

    fn push_node_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<U> + 'static,
        C: Fn(EndNode<U>, &Self) -> P;

    fn application_loop(&mut self, key_remapper: &mut FileRemapper<impl EditingScheme>);
}

impl<U> OneStatusLayout<U>
where
    U: Ui + 'static,
{
    pub fn new(ui: U, path: &PathBuf, match_manager: MatchManager, config: Config) -> Self {
        let mut node_manager = NodeManager::new(ui);
        let mut node = node_manager.only_child(&config, "code").unwrap();

        let (master_node, child_node) =
            node_manager.split_end(&mut node, Direction::Bottom, Split::Static(1), false);

        let status = StatusWidget { child_node, file_name: String::from(path.to_str().unwrap()) };

        let mut layout = OneStatusLayout {
            node_manager,
            status,
            widgets: Vec::new(),
            files: Vec::new(),
            master_node,
            match_manager,
        };

        layout.new_file_with_node(path, node);

        layout
    }

    fn new_file_with_node(&mut self, path: &PathBuf, node: EndNode<U>) {
        let mut file =
            FileWidget::<U>::new(path.clone(), node.clone(), &Some(self.match_manager.clone()));
        let file_printer = WidgetPrinter::new(&file);

        if matches!(node.config().line_numbers, LineNumbers::None) {
            self.files.push((file, None, file_printer));
        } else {
            let (line_numbers, file_parent) =
                LineNumbersWidget::new(&mut file, &mut self.node_manager);

            let widget_printer = WidgetPrinter::new(&line_numbers);
            self.widgets.push(Mutex::new((Box::new(line_numbers), widget_printer)));

            self.files.push((file, Some(file_parent), file_printer));
        }
    }

    fn active_widget(&mut self) -> &mut FileWidget<U> {
        &mut self.files[0].0
    }
}

impl<U> Layout<U> for OneStatusLayout<U>
where
    U: Ui + 'static,
{
    fn open_file(&mut self, path: &PathBuf) {
        let (master_node, child_node) = self.node_manager.split_mid(
            &mut self.master_node,
            Direction::Right,
            Split::Ratio(0.5),
            false,
        );

        self.new_file_with_node(path, child_node);
        self.master_node = master_node;
    }

    fn push_node_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<U> + 'static,
        C: Fn(EndNode<U>, &Self) -> P,
    {
        let (master_node, end_node) =
            self.node_manager.split_mid(&mut self.master_node, direction, split, false);

        self.master_node = master_node;

        let widget = constructor(end_node.clone(), &self);
        let widget_printer = WidgetPrinter::new(&widget);
        self.widgets.push(Mutex::new((Box::new(widget), widget_printer)));
    }

    fn application_loop(&mut self, key_remapper: &mut FileRemapper<impl EditingScheme>) {
        self.node_manager.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let printer = Mutex::new(true);

        // Initial printing.
        print_files(&mut self.files);
        for mutex in &mut self.widgets {
            let mut lock = mutex.lock().unwrap();
            lock.0.update();
            lock.1.print();
        }

        // The main loop.
        thread::scope(|s_0| {
            loop {
                // TODO: Make this generalized.
                if let Event::Key(key_event) = event::read().unwrap() {
                    // NOTE: This is very much temporary.
                    if let KeyCode::Esc = key_event.code {
                        break;
                    } else {
                        key_remapper.send_key_to_file(key_event, &mut self.files[0].0);
                    }
                }

                update_files(&mut self.files);
                let printer_lock = printer.lock().unwrap();
                print_files(&mut self.files);
                drop(printer_lock);

                let widget_indices = widgets_to_update(&self.widgets);
                for index in &widget_indices {
                    let box_and_widget = &self.widgets[*index];
                    s_0.spawn(|| {
                        let mut box_and_widget = box_and_widget.lock().unwrap();
                        box_and_widget.0.update();
                        let _printer_lock = printer.lock().unwrap();
                        box_and_widget.1.print();
                    });
                }
            }
        });

        self.node_manager.shutdown();
    }
}

fn update_files(
    files: &mut Vec<(FileWidget<impl Ui>, Option<MidNode<impl Ui>>, WidgetPrinter<impl Ui>)>,
) {
    thread::scope(|s_1| {
        for file in files {
            s_1.spawn(|| file.0.update());
        }
    });
}

fn print_files(
    files: &mut Vec<(FileWidget<impl Ui>, Option<MidNode<impl Ui>>, WidgetPrinter<impl Ui>)>,
) {
    for (_, _, widget_printer) in files {
        if widget_printer.text.has_changed() {
            widget_printer.print();
        }
    }
}

fn widgets_to_update(
    widgets: &Vec<Mutex<(Box<dyn Widget<impl Ui>>, WidgetPrinter<impl Ui>)>>,
) -> Vec<usize> {
    let mut indices = Vec::new();

    for (index, widget) in widgets.iter().enumerate() {
        // If the lock is unavailable, that means the widget is being updated.
        if let Ok(lock) = widget.try_lock() {
            if lock.0.needs_update() {
                indices.push(index);
            }
        }
    }

    indices
}

fn max_line(text: &Text, print_info: &PrintInfo, node: &EndNode<impl Ui>) -> usize {
    min(print_info.top_line + node.height(), text.lines().len() - 1)
}

struct WidgetPrinter<U>
where
    U: Ui,
{
    end_node: EndNode<U>,
    text: RoState<Text>,
    print_info: RoState<PrintInfo>,
}

unsafe impl<U> Send for WidgetPrinter<U> where U: Ui {}

impl<U> WidgetPrinter<U>
where
    U: Ui,
{
    fn new(widget: &dyn Widget<U>) -> Self {
        Self {
            end_node: widget.end_node().clone(),
            text: widget.text(),
            print_info: widget.print_info(),
        }
    }

    fn print(&mut self) {
        self.text.read().print(&mut self.end_node, self.print_info.read().clone());
    }
}
