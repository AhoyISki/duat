#[cfg(not(feature = "deadlock-detection"))]
use std::sync::Mutex;
use std::{
    any::Any,
    cmp::min,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::Mutex;

use super::{ActionableWidget, EditAccum, NormalWidget, Widget};
use crate::{
    config::{DownCastableData, RwData},
    history::History,
    position::{Cursor, Editor, Mover, Pos},
    text::{reader::MutTextReader, PrintInfo, Text},
    ui::{Area, EndNode, Label, NodeIndex, Ui},
};

/// The widget that is used to print and edit files.
pub struct FileWidget<U>
where
    U: Ui,
{
    pub(crate) _side_widgets: Option<(NodeIndex, Vec<NodeIndex>)>,
    identifier: String,
    text: Text<U>,
    print_info: PrintInfo,
    main_cursor: usize,
    cursors: Vec<Cursor>,
    history: History,
    readers: Vec<Box<dyn MutTextReader<U>>>,
    printed_lines: Vec<(usize, bool)>,
}

impl<U> FileWidget<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `FileWidget`.
    pub fn new(path: Option<PathBuf>) -> Widget<U> {
        // TODO: Allow the creation of a new file.
        let file_contents = path
            .as_ref()
            .map(|path| fs::read_to_string(path).expect("Failed to read the file."))
            .unwrap_or(String::from(""));

        let name = path
            .map(|path| path.file_name().unwrap().to_string_lossy().to_string())
            .unwrap_or(String::from("scratch_file"));

        let text = Text::new_rope(file_contents);
        let cursor = Cursor::default();

        Widget::Actionable(RwData::new_unsized(Arc::new(Mutex::new(FileWidget {
            _side_widgets: None,
            identifier: ["parsec-file: ", name.as_str()].join(""),
            text,
            print_info: PrintInfo::default(),
            main_cursor: 0,
            cursors: vec![cursor],
            history: History::new(),
            readers: Vec::new(),
            printed_lines: Vec::new(),
        }))))
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self, end_node: &EndNode<U>) {
        let moment = match self.history.move_backwards() {
            Some(moment) => moment,
            None => return,
        };

        self.print_info = moment.starting_print_info;

        self.cursors.clear();

        let mut chars = 0;
        for change in &moment.changes {
            self.text.undo_change(&change, chars);

            let new_caret_ch = change.taken_end().saturating_add_signed(chars);
            let pos = Pos::new(new_caret_ch, self.text.inner());
            self.cursors.push(Cursor::new(pos, &self.text.inner(), end_node));

            chars += change.taken_end() as isize - change.added_end() as isize;
        }
    }

    /// Redoes the last moment in history.
    pub fn redo(&mut self, end_node: &EndNode<U>) {
        let moment = match self.history.move_forward() {
            Some(moment) => moment,
            None => return,
        };

        self.print_info = moment.ending_print_info;

        self.cursors.clear();

        for change in &moment.changes {
            self.text.apply_change(&change);

            let new_pos = Pos::new(change.added_end(), self.text.inner());
            self.cursors.push(Cursor::new(new_pos, &self.text.inner(), &end_node));
        }
    }

    fn set_printed_lines(&mut self, end_node: &EndNode<U>) {
        let first_ch = self.print_info.first_ch;
        let wrap_method = end_node.config().wrap_method;
        let tab_places = &end_node.config().tab_places;

		// The beginning of the first line may be offscreen, which would make
		// the first line number a wrapped line.
        let mut is_wrapped = {
            let line = self.text.inner().char_to_line(first_ch);
            let line_ch = self.text.inner().line_to_char(line);
            let initial_slice = self.text.inner().slice(line_ch..first_ch);
            end_node.label.wrap_count(initial_slice, wrap_method, tab_places) > 0
        };

        let height = end_node.label.area().height();
        let slice = self.text.inner().slice(self.print_info.first_ch..);
        let mut lines_iter = slice.lines().enumerate();

        self.printed_lines.clear();
        self.printed_lines.reserve_exact(height);

        let mut accum = min(height, 1);

        while let (Some((index, line)), true) = (lines_iter.next(), accum < height) {
            let wrap_count = end_node.label.wrap_count(line, wrap_method, tab_places);
            let prev_accum = accum;
            accum = min(accum + wrap_count + 1, height);
            for _ in prev_accum..accum {
                self.printed_lines.push((index, is_wrapped));
                is_wrapped = true;
            }
            is_wrapped = false;
        }
    }

    /// Returns the currently printed set of lines.
    pub fn printed_lines(&self) -> &[(usize, bool)] {
        &self.printed_lines
    }

    // TODO: Move the history to a general placement, taking in all the
    // files.
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
    pub fn cursors(&self) -> &[Cursor] {
        self.cursors.as_slice()
    }

    /// A mutable reference to the `Text` of self.
    pub fn mut_text(&mut self) -> &mut Text<U> {
        &mut self.text
    }

    ////////// Status line convenience functions:
    /// The main cursor of the file.
    pub fn main_cursor(&self) -> Cursor {
        *self.cursors.get(self.main_cursor).unwrap()
    }

    /// The file's name.
    pub fn name(&self) -> &str {
        &self.identifier[13..]
    }

    pub fn full_path(&self) -> String {
        let mut path = std::env::current_dir().unwrap();
        path.push(Path::new(self.name()));
        path.to_string_lossy().to_string()
    }

    /// The lenght of the file, in lines.
    pub fn len_chars(&self) -> usize {
        self.text.inner().len_chars()
    }

    pub(crate) fn len_lines(&self) -> usize {
        self.text.inner().len_lines()
    }

    pub fn add_reader(&mut self, reader: Box<dyn MutTextReader<U>>) {
        self.readers.push(reader);
    }
}

impl<U> DownCastableData for FileWidget<U>
where
    U: Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<U> NormalWidget<U> for FileWidget<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        self.identifier.as_str()
    }

    fn update(&mut self, end_node: &mut EndNode<U>) {
        self.print_info.update(self.main_cursor().caret(), self.text.inner(), end_node);
        self.set_printed_lines(end_node);

        // let mut node = self.end_node.write();
        // self.text.update_lines(&mut node);
        // drop(node);
        // self.match_scroll();
    }

    fn needs_update(&self) -> bool {
        true
    }

    fn text(&self) -> &Text<U> {
        &self.text
    }

    fn scroll_vertically(&mut self, d_y: i32) {
        self.print_info.scroll_vertically(d_y, &self.text);
    }

    fn print_info(&self) -> PrintInfo {
        self.print_info
    }
}

impl<U> ActionableWidget<U> for FileWidget<U>
where
    U: Ui + 'static,
{
    fn editor<'a>(
        &'a mut self,
        index: usize,
        edit_accum: &'a mut EditAccum,
        end_node: &'a EndNode<U>,
    ) -> Editor<U> {
        Editor::new(
            &mut self.cursors[index],
            &mut self.text,
            end_node,
            edit_accum,
            Some(self.print_info),
            Some(&mut self.history),
        )
    }

    fn mover<'a>(&'a mut self, index: usize, end_node: &'a EndNode<U>) -> Mover<U> {
        Mover::new(&mut self.cursors[index], &self.text, end_node)
    }

    fn members_for_cursor_tags(&mut self) -> (&mut Text<U>, &[Cursor], usize) {
        (&mut self.text, self.cursors.as_slice(), self.main_cursor)
    }

    fn cursors(&self) -> &[Cursor] {
        self.cursors.as_slice()
    }

    fn mut_cursors(&mut self) -> Option<&mut Vec<Cursor>> {
        Some(&mut self.cursors)
    }

    fn main_cursor_index(&self) -> usize {
        self.main_cursor
    }

    fn mut_main_cursor_index(&mut self) -> Option<&mut usize> {
        Some(&mut self.main_cursor)
    }

    fn new_moment(&mut self) {
        self.new_moment();
    }

    fn undo(&mut self, end_node: &EndNode<U>) {
        self.undo(end_node)
    }

    fn redo(&mut self, end_node: &EndNode<U>) {
        self.redo(end_node)
    }
}

unsafe impl<M> Send for FileWidget<M> where M: Ui {}
