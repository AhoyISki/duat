use std::{
    cmp::{max, min},
    fmt::Write,
    fs,
    path::PathBuf,
    sync::Mutex,
    thread,
    time::Duration,
};

use crossterm::event::{self, poll, Event, KeyCode};

use crate::{
    action::{History, TextRange},
    config::{Config, LineNumbers, RoState, RwState, WrapMethod},
    cursor::{TextCursor, TextPos},
    file::{update_range, Text, TextLine},
    saturating_add_signed,
    tags::{CharTag, MatchManager},
    ui::{Direction, EndNode, Label, MidNode, NodeManager, Split, Ui},
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
pub trait Widget<M>: Send
where
    M: Ui,
{
    /// Returns the `ChildNode` associated with this area.
    fn end_node(&self) -> &EndNode<M>;

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
    fn resize(&mut self, node: &EndNode<M>) {}
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

        if target.line > info.top_line + height - scrolloff.d_y {
            info.top_line += target.line + scrolloff.d_y - info.top_line - height;
        } else if target.line < info.top_line + scrolloff.d_y && info.top_line != 0 {
            info.top_line -= (info.top_line + scrolloff.d_y) - target.line;
        }
    }

    fn scroll_up(&mut self, target: TextPos, mut d_y: usize) {
        let scrolloff = self.node.config().scrolloff;
        let text = self.text.read();
        let lines_iter = text.lines().iter().take(target.line);
        let info = &mut self.print_info.write();

        // If the target line is above the top line, no matter what, a new top line is needed.
        let mut needs_new_top_line = target.line < info.top_line;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.line {
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

    fn scroll_down(&mut self, current: TextPos, target: TextPos, mut d_y: usize, height: usize) {
        let scrolloff = self.node.config().scrolloff;
        let text = self.text.read();
        let lines_iter = text.lines().iter().take(target.line + 1);
        let mut info = self.print_info.write();
        let mut top_offset = 0;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.line {
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
        let tab_places = &self.node.config().tab_places;

        if let WrapMethod::NoWrap = self.node.config().wrap_method {
            let target_line = &self.text.read().lines[target.line];
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
        let current = main_cursor.current();
        let target = main_cursor.target();

        let text = self.text.read();

        let line = &text.lines()[current.line];
        let current_byte = line.get_line_byte_at(current.col);
        let current_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

        let line = &text.lines()[target.line];
        let target_byte = line.get_line_byte_at(target.col);
        let target_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

        let d_y = target_wraps;

        drop(text);
        drop(cursors);

        if let WrapMethod::NoWrap = self.node.config().wrap_method {
            self.scroll_unwrapped(target, self.node.height());
            self.scroll_horizontally(target, self.node.width());
        } else if target.line < current.line
            || (target.line == current.line && target_wraps < current_wraps)
        {
            self.scroll_up(target, d_y);
        } else {
            self.scroll_down(current, target, d_y, self.node.height());
        }
    }

    fn match_scroll(&mut self) {
        let text = self.text.read();
        let cursors = self.cursors.read();

        let main_cursor = cursors.get(*self.main_cursor.read()).unwrap();
        let limit_line =
            min(main_cursor.target().line + self.node.height(), text.lines().len() - 1);
        let start = main_cursor.target().translate_to(text.lines(), limit_line, 0);
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

    fn edit(&mut self, cursor: TextCursor, edit: impl ToString) {
        let edit = edit.to_string();
        let lines = edit.split_inclusive('\n').collect();

        let mut text = self.text.write();
        let (edits, _) = self.history.add_change(&mut text.lines, lines, cursor.range());

        let edits: Vec<TextLine> = edits.iter().map(|l| TextLine::new(l.clone())).collect();
        text.lines.splice(cursor.range().lines(), edits);

        let max_line = max_line(&text, &self.print_info.read(), &self.node);
        update_range(&mut text, cursor.range(), max_line, &self.node);
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self) {
        let mut text = self.text.write();

        let (splices, print_info) = match self.history.undo(&mut text.lines) {
            Some((changes, print_info)) => (changes, print_info),
            None => return,
        };
        let mut info = self.print_info.write();
        *info = print_info.unwrap_or(*info);

        let mut cursors = self.cursors.write();
        let mut cursors_iter = cursors.iter_mut();
        let mut new_cursors = Vec::new();

        let max_line = max_line(&text, &info, &self.node);
        for splice in &splices {
            if let Some(cursor) = cursors_iter.next() {
                cursor.move_to(splice.taken_end(), &text.lines, &self.node);
            } else {
                new_cursors.push(TextCursor::new(splice.taken_end(), &text.lines, &self.node));
            }

            let range = TextRange { start: splice.start(), end: splice.taken_end() };
            update_range(&mut text, range, max_line, &self.node);
        }

        drop(cursors);
        self.cursors.write().extend(new_cursors);
    }

    /// Re-does the last moment in history.
    pub fn redo(&mut self) {
        let mut text = self.text.write();

        let (splices, print_info) = match self.history.redo(&mut text.lines) {
            Some((changes, print_info)) => (changes, print_info),
            None => return,
        };
        let mut info = self.print_info.write();
        *info = print_info.unwrap_or(*info);

        let mut cursors = self.cursors.write();
        let mut cursors_iter = cursors.iter_mut();
        let mut new_cursors = Vec::new();

        let max_line = max_line(&text, &info, &self.node);
        for splice in &splices {
            if let Some(cursor) = cursors_iter.next() {
                cursor.move_to(splice.added_end(), &text.lines, &self.node);
            } else {
                new_cursors.push(TextCursor::new(splice.added_end(), &text.lines, &self.node));
            }

            let range = TextRange { start: splice.start(), end: splice.added_end() };
            update_range(&mut text, range, max_line, &self.node);
        }

        drop(cursors);
        self.cursors.write().extend(new_cursors);
    }

    fn update_file(&mut self) {
        self.update_print_info();

        let cursors = self.cursors.read();
        let main_cursor = cursors.get(*self.main_cursor.read()).unwrap();
        let current = main_cursor.current();
        let target = main_cursor.target();

        let mut text = self.text.write();
        let char_tags = &mut text.lines.get_mut(current.line).unwrap().info.char_tags;
        char_tags.retain(|(_, t)| !matches!(t, CharTag::PrimaryCursor));

        let line = &mut text.lines[target.line];
        let byte = line.get_line_byte_at(target.col);
        line.info.char_tags.insert((byte as u32, CharTag::PrimaryCursor));

        // Updates the information for each cursor in the file.
        drop(cursors);
        self.cursors.write().iter_mut().for_each(|c| c.update());

        drop(text);

        self.match_scroll();
    }

    fn printed_lines(&self) -> PrintedLines {
        PrintedLines {
            file: RoState::from_rw(self.text.clone()),
            print_info: RoState::from_rw(self.print_info.clone()),
        }
    }
}

impl<M> Widget<M> for FileWidget<M>
where
    M: Ui,
{
    fn end_node(&self) -> &EndNode<M> {
        &self.node
    }

    fn update(&mut self) {}

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

pub struct StatusArea<U>
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
        let main_line = self.cursors.read().get(*self.main_cursor.read()).unwrap().current().line;

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
}

pub struct OneStatusLayout<U>
where
    U: Ui,
{
    node_manager: NodeManager<U>,
    status: StatusArea<U>,
    widgets: Vec<Mutex<Box<dyn Widget<U>>>>,
    files: Vec<(FileWidget<U>, Option<MidNode<U>>)>,
    master_node: Option<MidNode<U>>,
    match_manager: MatchManager,
    prints: Vec<(RoState<Text>, EndNode<U>, RoState<PrintInfo>)>,
}

/// A form of organizing the areas on a window.
pub trait Layout<M>
where
    M: Ui,
{
    fn new_file(&mut self, path: &PathBuf);

    fn push_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<M> + 'static,
        C: Fn(EndNode<M>, &Self) -> P;

    fn application_loop(&mut self);
}

impl<U> OneStatusLayout<U>
where
    U: Ui + 'static,
{
    pub fn new(
        mut node_manager: NodeManager<U>, path: &PathBuf, match_manager: MatchManager,
        config: Option<Config>,
    ) -> Self {
        let mut node = node_manager.only_child(config, Some(String::from("code"))).unwrap();

        let (master_node, child_node) =
            node_manager.split_end(&mut node, Direction::Bottom, Split::Static(1), false);

        let status = StatusArea { child_node, file_name: String::from(path.to_str().unwrap()) };

        let mut layout = OneStatusLayout {
            node_manager,
            status,
            widgets: Vec::new(),
            files: Vec::new(),
            master_node: Some(master_node),
            match_manager,
            prints: Vec::new(),
        };

        layout.new_file_with_node(path, node);

        layout
    }

    fn new_file_with_node(&mut self, path: &PathBuf, node: EndNode<U>) {
        let mut file =
            FileWidget::<U>::new(path.clone(), node.clone(), &Some(self.match_manager.clone()));

        self.prints.push((file.text().clone(), node.clone(), file.print_info().clone()));

        if matches!(node.config().line_numbers, LineNumbers::None) {
            self.files.push((file, None));
        } else {
            let (line_numbers, file_parent) =
                LineNumbersWidget::new(&mut file, &mut self.node_manager);

            self.prints.push((
                line_numbers.text(),
                line_numbers.node.clone(),
                RoState::new(PrintInfo::default()),
            ));
            self.widgets.push(Mutex::new(Box::new(line_numbers)));

            self.files.push((file, Some(file_parent)));
        }
    }
}

impl<M> Layout<M> for OneStatusLayout<M>
where
    M: Ui + 'static,
{
    fn new_file(&mut self, path: &PathBuf) {
        let master_node = &mut self.master_node.as_mut().unwrap();
        let (master_node, child_node) =
            self.node_manager.split_mid(master_node, Direction::Right, Split::Ratio(0.5), false);

        self.new_file_with_node(path, child_node);
        self.master_node = Some(master_node);
    }

    fn push_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<M> + 'static,
        C: Fn(EndNode<M>, &Self) -> P,
    {
        let master_node = &mut self.master_node.as_mut().unwrap();
        let (master_node, end_node) =
            self.node_manager.split_mid(master_node, direction, split, false);

        self.master_node = Some(master_node);

        let widget = constructor(end_node.clone(), &self);

        self.prints.push((widget.text().clone(), end_node, widget.print_info().clone()));

        self.widgets.push(Mutex::new(Box::new(widget)));
    }

    fn application_loop(&mut self) {
        let mut widgets_to_update = Vec::new();

        self.node_manager.startup();
        thread::scope(|s_0| {
            loop {
                // TODO: Make this generalized.
                if event::poll(Duration::from_millis(10)).expect("crossterm") {
                    // First, update all files simultaneously.
                    thread::scope(|s_1| {
                        for file in &mut self.files {
                            s_1.spawn(|| file.0.update());
                        }
                    });

					if let Event::Key(key_event) = event::read().unwrap() {
    					if let KeyCode::Esc = key_event.code {
        					break;
    					}
					}

                    // Second, check if any of the widgets need to be updated, given the newly
                    // updated files.
                    for (index, widget) in self.widgets.iter().enumerate() {
                        // If the lock is unavailable, that means the widget is being updated.
                        if let Ok(raw_widget) = widget.try_lock() {
                            if raw_widget.needs_update() {
                                widgets_to_update.push(index);
                            }
                        }
                    }

                    // Third, update said widgets, without blocking the loop from continuing, thus
                    // allowing a slow thread to process without blocking input and updates.
                    for index in &widgets_to_update {
                        let widget = &self.widgets[*index];
                        s_0.spawn(|| {
                            widget.lock().unwrap().update();
                        });
                    }

                    widgets_to_update.clear();
                }

                // This part should happen on every update, instantly catching up with widgets that
                // just finished updating.
                for (text, node, print_info) in &mut self.prints {
                    if text.has_changed() {
                        text.read().print(node, *print_info.read());
                    }
                }

                self.node_manager.finish_all_printing();
            }
        });

        self.node_manager.shutdown();
    }
}

fn max_line(text: &Text, print_info: &PrintInfo, node: &EndNode<impl Ui>) -> usize {
    min(print_info.top_line + node.height(), text.lines().len())
}
