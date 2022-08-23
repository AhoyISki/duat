use std::{
    cmp::{max, min},
    fs,
    ops::RangeInclusive,
    path::PathBuf,
};

use crate::{
    action::{extend_edit, get_byte, History, TextRange},
    config::{ConfigOptions, WrapMethod},
    cursor::{get_byte_distance, TextCursor, TextPos},
    file::TextLine,
    tags::{CharTag, Form, MatchManager},
    ui::{Area, AreaManager, ChildId, ChildNode, ParentId},
};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// The index of the line at the top of the screen.
    pub top_line: usize,
    /// The number of times the top line should wrap.
    pub top_wraps: usize,
    /// The leftmost col shown on the screen.
    pub x_shift: usize,
}

impl PrintInfo {
    /// Scrolls the `PrintInfo` vertically by a given amount, on a given file.
    fn scroll_vertically(&mut self, mut d_y: i32, text: &Text) {
        let mut lines_iter = text.lines.iter();

        if d_y > 0 {
            lines_iter.skip(self.top_line);

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
            lines_iter.take(self.top_line).rev();

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

    fn scroll_horizontally(&mut self, mut d_x: i32, text: &Text, node: &ChildNode<impl Area>) {
        let mut max_d = 0;

        for (index, _) in text.printed_lines(node, self) {
            let line = text.lines[index];
            let line_d = line.get_distance_to_col(line.char_count(), &node.options().tab_places);
            max_d = max(max_d, line_d);
        }

        self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    }
}

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait PrintedArea {
    /// Returns the `ChildId` associated with this area.
    fn child_id(&self) -> ChildId;

    // The text is mutable so you can at least scroll it.
    /// Gets the text that should be printed to an area.
    fn text(&self) -> &Text;

    fn print_info(&mut self) -> &mut PrintInfo;

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, area: &dyn Area, options: &ConfigOptions);
}

/// An area where you can edit the text.
pub trait EditArea: PrintedArea {
    /// Gets the cursors on the area.
    ///
    /// # Returns
    ///
    /// * A list of cursors. This includes cursors that shouldn't be printed on screen.
    /// * The index of the main cursor. Most of the time, this will be 0.
    fn cursors(&mut self) -> (&mut Vec<TextCursor>, usize);
}

/// The text in a given area.
pub(crate) struct Text {
    lines: Vec<TextLine>,
    replacements: Vec<(Vec<TextLine>, RangeInclusive<usize>, bool)>,
    match_manager: Option<MatchManager>,
}

// TODO: Properly implement replacements.
impl Text {
    /// Prints the contents of a given area in a given `ChildNode`.
    pub fn print(&self, node: &mut ChildNode<impl Area>, forms: &[Form], print_info: &PrintInfo) {
        node.area.start_printing();

        // Print the `top_line`.
        let top_line = self.lines[print_info.top_line];
        let top_wraps = print_info.top_wraps;
        let skip = if top_wraps > 0 { top_line.wrap_iter().nth(top_wraps - 1).unwrap() } else { 0 };
        top_line.print(node, print_info.x_shift, skip as usize, node.options(), forms);

        // Prints other lines until it can't anymore.
        for line in self.lines.iter().skip(print_info.top_line) {
            if !line.print(node, print_info.x_shift, 0, node.options(), forms) {
                break;
            }
        }

        node.area.stop_printing();
    }

    /// Returns a list of all line indices that would be printed in a given node.
    ///
    /// The first number is the `TextLine`'s index, and the second is the amount of visible lines
    /// on the screen the `TextLine` would occupy.
    pub fn printed_lines(
        &self, node: &ChildNode<impl Area>, print_info: &PrintInfo,
    ) -> Vec<(usize, usize)> {
        let height_sum = print_info.top_wraps + node.area.height();
        let mut printed_lines = Vec::new();
        let mut lines_iter = self.lines.iter().enumerate();

        // List the top line.
        let top_line = lines_iter.nth(print_info.top_line).unwrap();
        let mut d_y = 1 + top_line.1.wrap_iter().count();
        printed_lines.push((top_line.0, min(d_y - print_info.top_wraps, node.area.height())));

        // List all the other lines.
        while let (Some((index, line)), true) = (lines_iter.next(), d_y < height_sum) {
            let line_count = 1 + line.wrap_iter().count();
            printed_lines.push((index, min(line_count, height_sum - d_y)));
            d_y += line_count;
        }

        printed_lines
    }

    pub(crate) fn splice(&mut self, range: TextRange, edit: impl ToString, max_line: usize) {
        let edit_lines = edit.to_string().split_inclusive('\n').map(|t| String::from(t)).collect();

        let old = self.lines[range.lines()].iter().map(|l| l.text()).collect();

        let new_lines = extend_edit(old, edit_lines, range).0;
        let new_lines: Vec<TextLine> = new_lines.iter().map(|&l| TextLine::new(l)).collect();

        self.lines.splice(range.lines(), new_lines);
    }

    pub fn splice_on_cursor(&mut self, cursor: &TextCursor, edit: impl ToString) {
        self.splice(cursor.range(), edit, self.lines.len() - 1);
    }

    pub fn lines(&self) -> &[TextLine] {
        self.lines.as_slice()
    }
}

/// A form of organizing the areas on a window.
pub trait Layout {
    fn new(area_manager: impl AreaManager) -> Self;

    fn new_file_buffer(&mut self, path: PathBuf);

    fn push_above(&mut self, area: impl PrintedArea) -> ChildId;

    fn push_below(&mut self, area: impl PrintedArea) -> ChildId;

    fn push_right(&mut self, area: impl PrintedArea) -> ChildId;

    fn push_left(&mut self, area: impl PrintedArea) -> ChildId;
}

pub struct StatusArea();

pub struct FileArea {
    text: Text,
    print_info: PrintInfo,
    main_cursor: usize,
    cursors: Vec<TextCursor>,
    options: ConfigOptions,
    child_id: ChildId,
    history: History,
}

impl FileArea {
    fn new(
        path: PathBuf, options: ConfigOptions, node: ChildNode<impl Area>,
        match_manager: Option<MatchManager>,
    ) -> FileArea {
        // TODO: Sanitize the path further.
        let file_contents = fs::read_to_string(path).unwrap_or("".to_string());
        let lines = file_contents.lines().map(|l| TextLine::new(l.to_string())).collect();

        FileArea {
            text: Text { lines, replacements: Vec::new(), match_manager },
            print_info: PrintInfo::default(),
            main_cursor: 0,
            cursors: vec![TextCursor::new(TextPos::default(), &lines, &node.options().tab_places)],
            options,
            child_id: node.id,
            history: History::new(),
        }
    }

    fn scroll_unwrapped(&mut self, current: TextPos, target: TextPos, height: usize) {
        let info = &mut self.print_info;
        let scrolloff = self.options.scrolloff;

        if target.line > info.top_line + height - scrolloff.d_y {
            info.top_line += target.line + scrolloff.d_y - info.top_line - height;
        } else if target.line < info.top_line + scrolloff.d_y && info.top_line != 0 {
            info.top_line -= (info.top_line + scrolloff.d_y) - target.line;
        }
    }

    fn scroll_up(&mut self, current: TextPos, target: TextPos, mut d_y: usize) {
        let lines_iter = self.text.lines[..=target.line].iter_mut();
        let info = &mut self.print_info;

        // If the target line is above the top line, no matter what, a new top line is needed.
        let mut needs_new_top_line = target.line < info.top_line;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.line {
                d_y += 1 + line.wrap_iter().count();
            };

            if index == info.top_line {
                // This means we ran into the top line too early, and must scroll up.
                if d_y < self.options.scrolloff.d_y + info.top_wraps {
                    needs_new_top_line = true;
                // If this happens, we're in the middle of the screen, and don't need to scroll.
                } else if !needs_new_top_line {
                    break;
                }
            }

            if needs_new_top_line && (d_y >= self.options.scrolloff.d_y || index == 0) {
                info.top_line = index;
                info.top_wraps = d_y.saturating_sub(self.options.scrolloff.d_y);
                break;
            }
        }
    }

    fn scroll_down(&mut self, current: TextPos, target: TextPos, mut d_y: usize, height: usize) {
        let lines_iter = self.text.lines.iter_mut().take(target.line);
        let mut top_offset = 0;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.line {
                d_y += 1 + line.wrap_iter().count();
            }

            if index == self.print_info.top_line {
                top_offset = self.print_info.top_wraps
            };

            if d_y >= height + top_offset - self.options.scrolloff.d_y {
                self.print_info.top_line = index;
                // If this equals 0, that means the distance has matched up perfectly,
                // i.e. the distance between the new `info.top_line` is exactly what's
                // needed for the full height. If it's greater than 0, `info.top_wraps`
                // needs to adjust where the line actually begins to match up.
                self.print_info.top_wraps = d_y + self.options.scrolloff.d_y - height;
                break;
            }

            // If this happens first, we're in the middle of the screen, and don't need to scroll.
            if index == self.print_info.top_line {
                break;
            }
        }
    }

    fn scroll_horizontally(&mut self, current: TextPos, target: TextPos, width: usize) {
        let info = &mut self.print_info;
        let scrolloff = self.options.scrolloff;

        if let WrapMethod::NoWrap = self.options.wrap_method {
            let target_line = &self.text.lines[target.line];
            let distance = target_line.get_distance_to_col(target.col, &self.options.tab_places);

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

    fn update_print_info(&mut self, node: &ChildNode<impl Area>) {
        let main_cursor = self.cursors.get(self.main_cursor).unwrap();
        let current = main_cursor.current();
        let target = main_cursor.target();

        let line = &self.text.lines[current.line];
        let current_byte = line.get_line_byte_at(current.col);
        let current_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

        let line = &self.text.lines[target.line];
        let target_byte = line.get_line_byte_at(target.col);
        let target_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

        let mut d_y = target_wraps;

        if let WrapMethod::NoWrap = node.options().wrap_method {
            self.scroll_unwrapped(current, target, node.area.height());
            self.scroll_horizontally(current, target, node.area.width());
        } else if target.line < current.line
            || (target.line == current.line && target_wraps < current_wraps)
        {
            self.scroll_up(current, target, d_y);
        } else {
            self.scroll_down(current, target, d_y, node.area.height());
        }
    }

    fn match_scroll(&mut self, node: &ChildNode<impl Area>) {
        let main_cursor = self.cursors.get(self.main_cursor).unwrap();
        let limit_line =
            min(main_cursor.target().line + node.area.height(), self.text.lines.len() - 1);
        let start = main_cursor.target().translate_to(&self.text.lines, limit_line, 0);
        let target_line = &self.text.lines[limit_line];
        let range = TextRange {
            start,
            end: TextPos {
                byte: start.byte + target_line.text().len(),
                col: target_line.char_count(),
                ..start
            },
        };
    }

    fn edit(&mut self, cursor: TextCursor, edit: impl ToString) {
        let lines = edit.to_string().split_inclusive('\n').collect();

        let (edits, range) = self.history.add_change(&mut self.text.lines, lines, cursor.range());

        let edits: Vec<TextLine> = edits.iter().map(|&l| TextLine::new(l)).collect();
        self.text.lines.splice(cursor.range().lines(), edits);

        if let Some(match_manager) = self.text.match_manager {
            match_range(&mut self.text, cursor.range(), &self.options);
        }
    }

    fn update_file(&mut self, node: &ChildNode<impl Area>) {
        self.update_print_info(node);

        let current = self.cursors.get(self.main_cursor).unwrap().current();
        let char_tags = &mut self.text.lines.get_mut(current.line).unwrap().info.char_tags;
        char_tags.retain(|(_, t)| !matches!(t, CharTag::PrimaryCursor));

        let target = self.cursors.get(self.main_cursor).unwrap().target();

        let line = &mut self.text.lines[target.line];
        let byte = line.get_line_byte_at(target.col);
        line.info.char_tags.insert((byte as u32, CharTag::PrimaryCursor));

        // Updates the information for each cursor in the file.
        self.cursors.iter_mut().for_each(|c| c.update());

        self.match_scroll(node);
    }
}

impl PrintedArea for FileArea {
    fn child_id(&self) -> ChildId {
        self.child_id
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_info(&mut self) -> &mut PrintInfo {
        &mut self.print_info
    }

    fn resize(&mut self, area: &dyn Area, options: &ConfigOptions) {
        for line in self.text.lines {
            line.parse_wrapping(options, area.width());
        }
    }
}

pub struct FileBuffer {
    path: PathBuf,
    file_area: FileArea,
}

struct OneStatusLayout {
    node: ParentId,
    status: StatusArea,
    side_areas: Vec<Box<dyn PrintedArea>>,
    files: Vec<FileBuffer>,
}

fn match_range(text: &mut Text, range: TextRange, area: &impl Area, options: &ConfigOptions) {
    if let Some(match_manager) = text.match_manager {
        let max_line_num = min(range.end.line + area.height(), text.lines.len() - 1);
        let max_line = &text.lines[max_line_num];
        let mut max_pos = range.end.translate_to(&text.lines, max_line_num, max_line.char_count());

        let start = TextPos {
            line: range.start.line,
            col: 0,
            byte: range.start.byte - get_byte(&text.lines[range.start.line].text(), range.start.col),
        };

        let len = text.lines[range.end.line].text().len();
        let end = TextPos {
            line: range.end.line,
            col: text.lines[range.end.line].char_count(),
            byte: range.end.byte - get_byte(&text.lines[range.end.line].text(), range.end.col) + len,
        };

        let range = TextRange { start, end };

        let line_infos = match_manager.match_range(text.lines.as_slice(), range, max_pos);

        for (line_info, line_num) in line_infos {
            text.lines[line_num].info = line_info;
            text.lines[line_num].update_line_info(options, area.width());
        }
    }
}
