//! The primary widget of Duat, used to display files.
//!
//! The [`FileWidget`] is Duat's way of display text files. It is
//! an [`ActionableWidget`] with [`Text`] containing a
//! [`ropey::Rope`] and a [`any_rope::Rope`] as its backing, unlike
//! most other widgets, that just use [`String`]s and [`Vec`]s.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`FileWidget`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`PrintInfo`], etc.
//!
//! [`FileWidget`]s can have attached extensions called
//! [`Observer`]s, that can read the [`Text`] within, and are also
//! notified of any [`Change`][crate::history::Change]s made to the
//! file.
//!
//! The [`FileWidget`] also provides a list of printed lines
//! through the [`printed_lines()`][FileWidget::printed_lines()`]
//! method. This method is notably used by the
//! [`LineNumbers`][crate::widgets::LineNumbers] widget, that shows
//! the numbers of the currently printed lines.
use std::{fs, path::PathBuf, sync::Arc};

use crate::{
    data::RwData,
    history::{Change, History},
    input::{Cursors, InputMethod, KeyMap},
    palette,
    text::{IterCfg, PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
    widgets::{ActiveWidget, PassiveWidget, Widget, WidgetCfg},
    Globals,
};

enum TextOp {
    NewBuffer,
    TakeText(Text, Path),
    OpenPath(PathBuf),
}

pub struct FileCfg<U>
where
    U: Ui,
{
    text_op: TextOp,
    generator: Arc<dyn Fn(File) -> Widget<U> + Send + Sync + 'static>,
    cfg: PrintCfg,
    specs: PushSpecs,
}

impl<U> FileCfg<U>
where
    U: Ui,
{
    pub(crate) fn new() -> Self {
        FileCfg {
            text_op: TextOp::NewBuffer,
            generator: Arc::new(|file| Widget::active(file, RwData::new(KeyMap::new()))),
            cfg: PrintCfg::default_for_files(),
            // Kinda arbitrary.
            specs: PushSpecs::above(),
        }
    }

    pub(crate) fn build(self) -> (Widget<U>, Box<dyn Fn() -> bool>) {
        let (text, path) = match self.text_op {
            TextOp::NewBuffer => (Text::new(String::from("\n")), Path::new_unset()),
            TextOp::TakeText(text, path) => (text, path),
            TextOp::OpenPath(path) => {
                let text = match std::fs::read_to_string(&path) {
                    Ok(contents) => Text::new(contents),
                    Err(_) => Text::new(String::from("\n")),
                };

                let full_path = {
                    let file_name = path.file_name().unwrap();
                    std::env::current_dir().unwrap().join(file_name)
                };

                (text, Path::Set(full_path))
            }
        };

        #[cfg(feature = "wacky-colors")]
        let text = {
            let mut text = text;
            use crate::{
                palette::{self, Form},
                text::{text, Marker, Tag},
            };
            let mut text = Text::new(contents.unwrap_or(String::from("\n")));

            let marker = Marker::new();
            let form1 = palette::set_form("form1lmao", Form::new().red());
            let form2 = palette::set_form("form2lmao", Form::new().on_blue());
            for i in (0..4047390).step_by(8) {
                text.insert_tag(i, Tag::PushForm(form1), marker);
                text.insert_tag(i + 4, Tag::PopForm(form1), marker);
            }

            text
        };

        let file = File {
            path,
            text,
            cfg: self.cfg,
            history: History::new(),
            printed_lines: Vec::new(),
        };

        ((self.generator)(file), Box::new(|| false))
    }

    pub(crate) fn open_path(self, path: PathBuf) -> Self {
        Self {
            text_op: TextOp::OpenPath(path),
            ..self
        }
    }

    pub(crate) fn take_from_prev(self, prev: &mut File) -> Self {
        let text = std::mem::take(&mut prev.text);
        Self {
            text_op: TextOp::TakeText(text, prev.path.clone()),
            ..self
        }
    }

    pub(crate) fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.cfg = cfg;
    }

    pub(crate) fn set_input(&mut self, input: impl InputMethod<U, Widget = File> + Clone) {
        self.generator = Arc::new(move |file| Widget::active(file, RwData::new(input.clone())));
    }

    pub(crate) fn mut_print_cfg(&mut self) -> &mut PrintCfg {
        &mut self.cfg
    }
}

impl<U> WidgetCfg<U> for FileCfg<U>
where
    U: Ui,
{
    type Widget = File;

    fn build(self, _globals: Globals<U>, _: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let specs = self.specs;
        let (widget, checker) = self.build();
        (widget, checker, specs)
    }
}

impl<U> Default for FileCfg<U>
where
    U: Ui,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<U> Clone for FileCfg<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        Self {
            text_op: TextOp::NewBuffer,
            generator: self.generator.clone(),
            cfg: self.cfg.clone(),
            specs: self.specs,
        }
    }
}

/// The widget that is used to print and edit files.
pub struct File {
    path: Path,
    text: Text,
    cfg: PrintCfg,
    history: History,
    printed_lines: Vec<(usize, bool)>,
}

impl File {
    pub fn write(&self) -> Result<usize, String> {
        if let Path::Set(path) = &self.path {
            self.text
                .write_to(std::io::BufWriter::new(
                    fs::File::create(path).map_err(|err| err.to_string())?,
                ))
                .map_err(|err| err.to_string())
        } else {
            Err(String::from(
                "The file has no associated path, and no path was given to write to",
            ))
        }
    }

    pub fn write_to(&self, path: impl AsRef<str>) -> std::io::Result<usize> {
        self.text
            .write_to(std::io::BufWriter::new(fs::File::create(path.as_ref())?))
    }

    /// The number of bytes in the file.
    pub fn len_bytes(&self) -> usize {
        self.text.len_bytes()
    }

    /// Returns the currently printed set of lines.
    pub fn printed_lines(&self) -> &[(usize, bool)] {
        &self.printed_lines
    }

    /// The file's name.
    pub fn name(&self) -> String {
        match &self.path {
            Path::Set(path) => path.file_name().unwrap().to_string_lossy().to_string(),
            Path::UnSet(id) => format!("*scratch file {id}*"),
        }
    }

    pub fn set_name(&self) -> Option<String> {
        match &self.path {
            Path::Set(path) => Some(path.file_name().unwrap().to_string_lossy().to_string()),
            Path::UnSet(_) => None,
        }
    }

    /// The full path of the file.
    pub fn full_path(&self) -> String {
        match &self.path {
            Path::Set(path) => path.to_string_lossy().to_string(),
            Path::UnSet(id) => format!("*scratch file*#{id}"),
        }
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.len_chars()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.len_lines()
    }

    pub fn new_moment(&mut self) {
        self.history.new_moment()
    }

    pub fn add_change(&mut self, change: Change, assoc_index: Option<usize>) -> (usize, isize) {
        self.history.add_change(change, assoc_index)
    }

    pub fn redo(&mut self, area: &impl Area, cursors: &mut Cursors) {
        self.history.redo(&mut self.text, area, cursors, &self.cfg)
    }

    pub fn undo(&mut self, area: &impl Area, cursors: &mut Cursors) {
        self.history.undo(&mut self.text, area, cursors, &self.cfg)
    }

    pub fn mut_text_and_history(&mut self) -> (&mut Text, &mut History) {
        (&mut self.text, &mut self.history)
    }

    pub(crate) fn cfg<U: Ui>() -> FileCfg<U> {
        FileCfg::new()
    }
}

impl<U> PassiveWidget<U> for File
where
    U: Ui,
{
    fn build(_globals: Globals<U>, _: bool) -> (Widget<U>, impl Fn() -> bool, crate::ui::PushSpecs)
    where
        Self: Sized,
    {
        let (widget, checker) = Self::cfg().build();
        (widget, checker, PushSpecs::above())
    }

    fn update(&mut self, _area: &U::Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> &PrintCfg {
        &self.cfg
    }

    fn once(_globals: crate::Globals<U>) {}

    fn print(&mut self, area: &<U as Ui>::Area) {
        let start = area.first_char();

        let mut last_line_num = area
            .rev_print_iter(self.text.rev_iter_at(start), IterCfg::new(&self.cfg))
            .find_map(|(caret, item)| caret.wrap.then_some(item.line));

        self.printed_lines.clear();
        let printed_lines = &mut self.printed_lines;

        area.print_with(
            &self.text,
            &self.cfg,
            palette::painter(),
            move |caret, item| {
                if caret.wrap {
                    let line = item.line;
                    let wrapped = last_line_num.is_some_and(|last_line_num| last_line_num == line);
                    last_line_num = Some(line);
                    printed_lines.push((line, wrapped));
                }
            },
        )
    }
}

impl<U> ActiveWidget<U> for File
where
    U: Ui,
{
    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &<U as Ui>::Area) {}

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {}
}

unsafe impl Send for File {}
unsafe impl Sync for File {}

#[derive(Clone)]
enum Path {
    Set(PathBuf),
    UnSet(usize),
}

impl Path {
    fn new_unset() -> Path {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        Path::UnSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}
