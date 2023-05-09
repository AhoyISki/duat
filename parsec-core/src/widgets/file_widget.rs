//! The primary widget of Parsec, used to display files.
//!
//! The [`FileWidget<U>`] is Parsec's way of display text files. It is
//! an [`ActionableWidget`] with [`Text<U>`] containing a
//! [`ropey::Rope`] and a [`any_rope::Rope`] as its backing, unlike
//! most other widgets, that just use [`String`]s and [`Vec`]s.
//!
//! Most extensible features of Parsec have the primary purpose of
//! serving the [`FileWidget<U>`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`PrintInfo`], etc.
//!
//! [`FileWidget<U>`]s can have attached extensions called
//! [`Observer`]s, that can read the [`Text<U>`] within, and are also
//! notified of any [`Change`][crate::history::Change]s made to the
//! file.
//!
//! The [`FileWidget<U>`] also provides a list of printed lines
//! through the [`printed_lines()`][FileWidget::printed_lines()`]
//! method. This method is notably used by the
//! [`LineNumbers<U>`][crate::widgets::LineNumbers] widget, that shows
//! the numbers of the currently printed lines.
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{
    any::Any,
    cmp::min,
    fs,
    path::{Path, PathBuf},
    sync::Arc
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use super::{ActionableWidget, EditAccum, NormalWidget, Widget};
use crate::{
    data::DownCastableData,
    history::History,
    position::{Cursor, Editor, Mover, Pos},
    tags::{form::FILE_NAME, Tag},
    text::{reader::Observer, PrintCfg, Text},
    ui::{Area, Label, PrintInfo, Ui}
};

/// The widget that is used to print and edit files.
pub struct FileWidget<U>
where
    U: Ui + 'static
{
    pub(crate) _side_widgets: Option<(usize, Vec<usize>)>,
    name: String,
    text: Text<U>,
    print_info: U::PrintInfo,
    main_cursor: usize,
    cursors: Vec<Cursor>,
    history: History<U>,
    readers: Vec<Box<dyn Observer<U>>>,
    printed_lines: Vec<(usize, bool)>,
    print_cfg: PrintCfg
}

impl<U> FileWidget<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of [`FileWidget<U>`].
    pub fn new(path: Option<PathBuf>, print_cfg: PrintCfg) -> Widget<U> {
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
        let mut pushes_pops_you_cant_explain_that = true;
        let lock = text.tags.get_lock();

        for index in (0..text.len_chars()).step_by(20) {
            if pushes_pops_you_cant_explain_that {
                text.tags.insert(index, Tag::PushForm(FILE_NAME), lock);
            } else {
                text.tags.insert(index, Tag::PopForm(FILE_NAME), lock);
            }
            pushes_pops_you_cant_explain_that = !pushes_pops_you_cant_explain_that
        }

        Widget::actionable(
            Arc::new(RwLock::new(FileWidget {
                _side_widgets: None,
                name: ["parsec-file: ", name.as_str()].join(""),
                text,
                print_info: U::PrintInfo::default(),
                main_cursor: 0,
                cursors: vec![cursor],
                history: History::new(),
                readers: Vec::new(),
                printed_lines: Vec::new(),
                print_cfg
            })),
            Box::new(|| false)
        )
    }

    /// Undoes the last [`Moment<U>`][crate::history::Moment] in the
    /// [`History`].
    pub fn undo(&mut self, label: &U::Label) {
        let moment = match self.history.move_backwards() {
            Some(moment) => moment,
            None => return
        };

        self.print_info = moment.starting_print_info;

        self.cursors.clear();

        let mut chars = 0;
        for change in &moment.changes {
            self.text.undo_change(&change, chars);

            let new_caret_ch = change.taken_end().saturating_add_signed(chars);
            let pos = Pos::new(new_caret_ch, self.text.inner());
            self.cursors.push(Cursor::new::<U>(pos, &self.text, label, &self.print_cfg));

            chars += change.taken_end() as isize - change.added_end() as isize;
        }
    }

    /// Redoes the last [`Moment<U>`][crate::history::Moment] in the
    /// [`History`].
    pub fn redo(&mut self, label: &U::Label) {
        let moment = match self.history.move_forward() {
            Some(moment) => moment,
            None => return
        };

        self.print_info = moment.ending_print_info;

        self.cursors.clear();

        for change in &moment.changes {
            self.text.apply_change(&change);

            let new_pos = Pos::new(change.added_end(), self.text.inner());
            self.cursors.push(Cursor::new::<U>(new_pos, &self.text, label, &self.print_cfg));
        }
    }

    fn set_printed_lines(&mut self, label: &U::Label) {
        let first_char = self.print_info.first_char(&self.text);
        let mut line_num = self.text.char_to_line(first_char);

        // The beginning of the first line may be offscreen, which would make
        // the first line number a wrapped line.
        let mut is_wrapped = {
            let line = self.text.iter_line(line_num);
            label.wrap_count(line, &self.print_cfg, first_char) > 0
        };

        let height = label.area().height();

        self.printed_lines.clear();
        self.printed_lines.reserve_exact(height);

        let mut accum = 0;
        let lines_len = self.text.len_lines();
        while accum <= height && line_num < lines_len {
            let line = self.text.iter_line(line_num);
            let mut wrap_count = label.wrap_count(line, &self.print_cfg, usize::MAX);
            if accum == 0 {
                let line = self.text.iter_line(line_num);
                wrap_count -= label.wrap_count(line, &self.print_cfg, first_char);
            }
            let prev_accum = accum;
            accum = min(accum + wrap_count, height) + 1;
            for _ in prev_accum..accum {
                self.printed_lines.push((line_num, is_wrapped));
                is_wrapped = true;
            }
            is_wrapped = false;
            line_num += 1;
        }
    }

    /// Returns the currently printed set of lines.
    pub fn printed_lines(&self) -> &[(usize, bool)] {
        &self.printed_lines
    }

    // TODO: Move the history to a general placement, taking in all the
    // files.
    /// The [`History`] of [`Change`][crate::history::Change]s done to
    /// this file.
    pub fn history(&self) -> &History<U> {
        &self.history
    }

    /// Ends the current [`Moment<U>`][crate::history::Moment] and
    /// starts a new one.
    pub fn new_moment(&mut self) {
        self.cursors.iter_mut().for_each(|cursor| cursor.assoc_index = None);
        self.history.new_moment(self.print_info);
    }

    /// The list of [`Cursor`]s on the file.
    pub fn cursors(&self) -> &[Cursor] {
        self.cursors.as_slice()
    }

    /// A mutable reference to the [`Text<U>`] of [`self`].
    pub fn mut_text(&mut self) -> &mut Text<U> {
        &mut self.text
    }

    ////////// Status line convenience functions:
    /// The main [`Cursor`] of the file.
    pub fn main_cursor(&self) -> Cursor {
        *self.cursors.get(self.main_cursor).unwrap()
    }

    /// The file's name.
    pub fn name(&self) -> String {
        String::from(&self.name[13..])
    }

	/// The full path of the file.
    pub fn full_path(&self) -> String {
        let mut path = std::env::current_dir().unwrap();
        path.push(Path::new(self.name().as_str()));
        path.to_string_lossy().to_string()
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.inner().len_chars()
    }

	/// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.inner().len_lines()
    }

	/// Adds a new [`Observer`] to [`self`].
    pub fn add_observer(&mut self, reader: Box<dyn Observer<U>>) {
        self.readers.push(reader);
    }
}

impl<U> NormalWidget<U> for FileWidget<U>
where
    U: Ui + 'static
{
    fn update(&mut self, label: &U::Label) {
        self.print_info.scroll_to_gap(
            &self.text,
            self.main_cursor().caret(),
            label,
            &self.print_cfg
        );
        self.set_printed_lines(label);
    }

    fn text(&self) -> &Text<U> {
        &self.text
    }

    fn scroll_vertically(&mut self, _d_y: i32) {
        // self.print_info.scroll_vertically(d_y, &self.text);
    }

    fn print_info(&self) -> U::PrintInfo {
        self.print_info
    }

    fn print_cfg(&self) -> PrintCfg {
        self.print_cfg.clone()
    }
}

impl<U> ActionableWidget<U> for FileWidget<U>
where
    U: Ui + 'static
{
    fn editor<'a>(&'a mut self, index: usize, edit_accum: &'a mut EditAccum) -> Editor<U> {
        Editor::new(
            &mut self.cursors[index],
            &mut self.text,
            edit_accum,
            Some(self.print_info),
            Some(&mut self.history)
        )
    }

    fn mover<'a>(&'a mut self, index: usize, label: &'a U::Label) -> Mover<U> {
        Mover::new(&mut self.cursors[index], &self.text, label, self.print_cfg.clone())
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

    fn undo(&mut self, label: &'_ U::Label) {
        self.undo(label)
    }

    fn redo(&mut self, label: &'_ U::Label) {
        self.redo(label)
    }
}

impl<U> DownCastableData for FileWidget<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}
