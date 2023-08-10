//! The primary widget of Parsec, used to display files.
//!
//! The [`FileWidget<U>`] is Parsec's way of display text files. It is
//! an [`ActionableWidget`] with [`Text`] containing a
//! [`ropey::Rope`] and a [`any_rope::Rope`] as its backing, unlike
//! most other widgets, that just use [`String`]s and [`Vec`]s.
//!
//! Most extensible features of Parsec have the primary purpose of
//! serving the [`FileWidget<U>`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`PrintInfo`], etc.
//!
//! [`FileWidget<U>`]s can have attached extensions called
//! [`Observer`]s, that can read the [`Text`] within, and are also
//! notified of any [`Change`][crate::history::Change]s made to the
//! file.
//!
//! The [`FileWidget<U>`] also provides a list of printed lines
//! through the [`printed_lines()`][FileWidget::printed_lines()`]
//! method. This method is notably used by the
//! [`LineNumbers<U>`][crate::widgets::LineNumbers] widget, that shows
//! the numbers of the currently printed lines.
use std::{fs::File, path::PathBuf};

use super::{ActSchemeWidget, EditAccum, Widget, WidgetType};
use crate::{
    data::AsAny,
    history::History,
    position::{Cursor, Editor, Mover, Point},
    text::{IterCfg, PrintCfg, Text},
    ui::{Area, PrintInfo, Ui}
};

/// The widget that is used to print and edit files.
pub struct FileWidget<U>
where
    U: Ui + 'static
{
    path: Option<PathBuf>,
    text: Text,
    print_info: U::PrintInfo,
    main_cursor: usize,
    cursors: Vec<Cursor>,
    history: History<U>,
    cfg: PrintCfg,
    printed_lines: Vec<(usize, bool)>
}

impl<U> FileWidget<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of [`FileWidget<U>`].
    pub fn scheme(path: Option<PathBuf>, print_cfg: PrintCfg) -> WidgetType<U> {
        let contents = path.as_ref().and_then(|path| match std::fs::read_to_string(path) {
            Ok(contents) => Some(contents),
            Err(_) => None
        });

        let path = path.map(|path| {
            let file_name = path.file_name().unwrap();
            std::env::current_dir().unwrap().join(file_name)
        });

        let mut text = Text::new_rope(contents.unwrap_or(String::from("\n")));

        #[cfg(feature = "wacky-colors")]
        {
            use crate::text::{Handle, Tag};
            let mut tagger = text.tag_with(Handle::new());
            let mut pushes_pops_you_cant_explain_that = true;
            for index in (20..tagger.len_chars()).step_by(30) {
                if pushes_pops_you_cant_explain_that {
                    tagger.insert(index, Tag::ConcealStart);
                    //tagger.insert(index, Tag::ghost_from("   Ayy lmao   "));
                    tagger.insert(index, Tag::PushForm(crate::forms::SEPARATOR));
                    tagger.insert(index + 10, Tag::ConcealEnd);
                    tagger.insert(index + 11, Tag::PopForm(crate::forms::SEPARATOR));
                } else {
                    tagger.insert(index, Tag::ghost_from("   Hello World   "));
                }
                pushes_pops_you_cant_explain_that = !pushes_pops_you_cant_explain_that
            }
        }

        let cursors = vec![Cursor::default()];
        text.add_cursor_tags(&cursors, 0);

        WidgetType::scheme_input(FileWidget {
            path,
            text,
            print_info: U::PrintInfo::default(),
            main_cursor: 0,
            cursors,
            history: History::default(),
            cfg: print_cfg,
            printed_lines: Vec::new()
        })
    }

    /// Undoes the last [`Moment<U>`][crate::history::Moment] in the
    /// [`History`].
    pub fn undo(&mut self, area: &U::Area) {
        let moment = match self.history.move_backwards() {
            Some(moment) => moment,
            None => return
        };

        self.print_info = moment.starting_print_info;

        self.cursors.clear();

        let mut chars = 0;
        for change in &moment.changes {
            self.text.undo_change(change, chars);

            let new_caret_ch = change.taken_end().saturating_add_signed(chars);
            let pos = Point::new(new_caret_ch, &self.text);
            self.cursors.push(Cursor::new(pos, &self.text, area, &self.cfg));

            chars += change.taken_end() as isize - change.added_end() as isize;
        }
    }

    /// Redoes the last [`Moment<U>`][crate::history::Moment] in the
    /// [`History`].
    pub fn redo(&mut self, area: &U::Area) {
        let moment = match self.history.move_forward() {
            Some(moment) => moment,
            None => return
        };

        self.print_info = moment.ending_print_info;

        self.cursors.clear();

        for change in &moment.changes {
            self.text.apply_change(change);

            let new_pos = Point::new(change.added_end(), &self.text);
            self.cursors.push(Cursor::new(new_pos, &self.text, area, &self.cfg));
        }
    }

    pub fn write(&self) -> Result<usize, String> {
        if let Some(path) = &self.path {
            self.text.write_to(std::io::BufWriter::new(
                File::create(path).map_err(|err| err.to_string())?
            ))
        } else {
            Err(String::from("No path given to write to"))
        }
    }

    /// The number of bytes in the file.
    pub fn len_bytes(&self) -> usize {
        self.text.len_bytes()
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

    /// A mutable reference to the [`Text`] of [`self`].
    pub fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }

    ////////// Status line convenience functions:
    /// The main [`Cursor`] of the file.
    pub fn main_cursor(&self) -> Cursor {
        self.cursors.get(self.main_cursor).unwrap().clone()
    }

    /// The file's name.
    pub fn name(&self) -> Option<String> {
        self.path
            .as_ref()
            .and_then(|path| path.file_name().map(|file| file.to_string_lossy().to_string()))
    }

    /// The full path of the file.
    pub fn full_path(&self) -> String {
        self.path
            .as_ref()
            .map(|path| path.to_string_lossy().to_string())
            .unwrap_or(String::from("scratch file"))
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.len_chars()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.len_lines()
    }

    fn set_printed_lines(&mut self, area: &U::Area) {
        let start = self.print_info.first_char();

        let mut last_line_num = area
            .rev_print_iter(self.text.rev_iter_at(start), IterCfg::new(&self.cfg))
            .find_map(|((.., new_line), _)| new_line);

        self.printed_lines = area
            .print_iter(self.text.iter_at(start), IterCfg::new(&self.cfg))
            .filter_map(|((.., new_line), _)| new_line)
            .map(|line_num| {
                let wrapped = last_line_num.is_some_and(|last_line_num| last_line_num == line_num);
                last_line_num = Some(line_num);
                (line_num, wrapped)
            })
            .take(area.height())
            .collect();
    }
}

impl<U> Widget<U> for FileWidget<U>
where
    U: Ui + 'static
{
    fn update(&mut self, area: &U::Area) {
        self.print_info.scroll_to_gap(&self.text, self.main_cursor().caret(), area, &self.cfg);
        self.set_printed_lines(area);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn scroll_vertically(&mut self, _d_y: i32) {
        // self.print_info.scroll_vertically(d_y, &self.text);
    }

    fn print_info(&self) -> U::PrintInfo {
        self.print_info
    }

    fn print_cfg(&self) -> &PrintCfg {
        &self.cfg
    }
}

impl<U> ActSchemeWidget<U> for FileWidget<U>
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

    fn mover<'a>(&'a mut self, index: usize, area: &'a U::Area) -> Mover<U> {
        Mover::new(&mut self.cursors[index], &self.text, area, self.cfg.clone())
    }

    fn members_for_cursor_tags(&mut self) -> (&mut Text, &[Cursor], usize) {
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

    fn undo(&mut self, area: &'_ U::Area) {
        self.undo(area)
    }

    fn redo(&mut self, area: &'_ U::Area) {
        self.redo(area)
    }
}

impl<U> AsAny for FileWidget<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

unsafe impl<U> Send for FileWidget<U> where U: Ui {}
unsafe impl<U> Sync for FileWidget<U> where U: Ui {}
