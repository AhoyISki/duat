#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{
    any::Any,
    cmp::min,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use super::{ActionableWidget, EditAccum, NormalWidget, Widget};
use crate::{
    config::{Config, DownCastableData},
    history::History,
    position::{Cursor, Editor, Mover, Pos},
    text::{reader::MutTextReader, PrintInfo, Text},
    ui::{Area, Label, Ui},
};

/// The widget that is used to print and edit files.
pub struct FileWidget<U>
where
    U: Ui,
{
    pub(crate) _side_widgets: Option<(usize, Vec<usize>)>,
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

        let mut text = Text::new_rope(file_contents);
        let cursor = Cursor::default();

        // let mut pushes_pops_you_cant_explain_that = true;
        // let lock = text.tags.get_lock();
        // for index in (0..text.len_chars()).step_by(20) {
        //     if pushes_pops_you_cant_explain_that {
        //         text.tags.insert(index, Tag::PushForm(FILE_NAME), lock);
        //     } else {
        //         text.tags.insert(index, Tag::PopForm(FILE_NAME), lock);
        //     }
        //     pushes_pops_you_cant_explain_that =
        // !pushes_pops_you_cant_explain_that
        //
        // }

        Widget::actionable(
            Arc::new(RwLock::new(FileWidget {
                _side_widgets: None,
                identifier: ["parsec-file: ", name.as_str()].join(""),
                text,
                print_info: PrintInfo::default(),
                main_cursor: 0,
                cursors: vec![cursor],
                history: History::new(),
                readers: Vec::new(),
                printed_lines: Vec::new(),
            })),
            Box::new(|| false),
        )
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self, label: &U::Label, config: &Config) {
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
            self.cursors.push(Cursor::new::<U>(pos, &self.text.inner(), label, config));

            chars += change.taken_end() as isize - change.added_end() as isize;
        }
    }

    /// Redoes the last moment in history.
    pub fn redo(&mut self, label: &U::Label, config: &Config) {
        let moment = match self.history.move_forward() {
            Some(moment) => moment,
            None => return,
        };

        self.print_info = moment.ending_print_info;

        self.cursors.clear();

        for change in &moment.changes {
            self.text.apply_change(&change);

            let new_pos = Pos::new(change.added_end(), self.text.inner());
            self.cursors.push(Cursor::new::<U>(new_pos, &self.text.inner(), label, config));
        }
    }

    fn set_printed_lines(&mut self, label: &U::Label, config: &Config) {
        let first_ch = self.print_info.first_ch;
        let wrap_method = config.wrap_method;
        let tab_places = &config.tab_places;
        let top_line = self.text.inner().char_to_line(first_ch);

        // The beginning of the first line may be offscreen, which would make
        // the first line number a wrapped line.
        let mut is_wrapped = {
            let line_ch = self.text.inner().line_to_char(top_line);
            let initial_slice = self.text.inner().slice(line_ch..first_ch);
            label.wrap_count(initial_slice, wrap_method, tab_places) > 0
        };

        let height = label.area().height();
        let slice = self.text.inner().slice(self.print_info.first_ch..);
        let mut liness = slice.lines().enumerate();

        self.printed_lines.clear();
        self.printed_lines.reserve_exact(height);

        let mut accum = 0;

        while let (Some((index, line)), true) = (liness.next(), accum <= height) {
            let line_num = index + top_line;
            let wrap_count = label.wrap_count(line, wrap_method, tab_places);
            let prev_accum = accum;
            accum = min(accum + wrap_count, height) + 1;
            for _ in prev_accum..accum {
                self.printed_lines.push((line_num, is_wrapped));
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

impl<U> NormalWidget<U> for FileWidget<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        self.identifier.as_str()
    }

    fn update(&mut self, label: &U::Label, config: &Config) {
        self.print_info
            .update::<U>(self.main_cursor().caret(), self.text.inner(), label, config);
        self.set_printed_lines(label, config);

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
    ) -> Editor<U> {
        Editor::new(
            &mut self.cursors[index],
            &mut self.text,
            edit_accum,
            Some(self.print_info),
            Some(&mut self.history),
        )
    }

    fn mover<'a>(&'a mut self, index: usize, label: &'a U::Label, config: &'a Config) -> Mover<U> {
        Mover::new(&mut self.cursors[index], &self.text, label, config)
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

    fn undo(&mut self, label: &'_ U::Label, config: &'_ Config) {
        self.undo(label, config)
    }

    fn redo(&mut self, label: &'_ U::Label, config: &'_ Config) {
        self.redo(label, config)
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

// unsafe impl<M> Send for FileWidget<M> where M: Ui {}
