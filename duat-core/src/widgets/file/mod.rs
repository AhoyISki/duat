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
use std::{fs, io::ErrorKind, path::PathBuf, sync::Arc};

use self::read::{Reader, RevSearcher, Searcher};
use crate::{
    cache::load_cache,
    data::Context,
    forms,
    history::History,
    input::{Cursors, InputForFiles, KeyMap},
    text::{IterCfg, Point, PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
    widgets::{ActiveWidget, PassiveWidget, Widget, WidgetCfg},
};

mod read;

pub struct FileCfg<U>
where
    U: Ui,
{
    text_op: TextOp,
    builder: Arc<dyn Fn(File, Cursors) -> Widget<U> + Send + Sync + 'static>,
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
            builder: Arc::new(|file, cursors| {
                let mut input = KeyMap::new();
                InputForFiles::<U>::set_cursors(&mut input, cursors);
                Widget::active(file, input)
            }),
            cfg: PrintCfg::default_for_input(),
            // Kinda arbitrary.
            specs: PushSpecs::above(),
        }
    }

    pub(crate) fn build(self) -> (Widget<U>, Box<dyn Fn() -> bool>) {
        let (text, path) = match self.text_op {
            TextOp::NewBuffer => (Text::new(), Path::new_unset()),
            TextOp::TakeText(text, path) => (text, path),
            // TODO: Add an option for automatic path creation.
            TextOp::OpenPath(path) => match path.canonicalize() {
                Ok(path) => (Text::from_file(&path), Path::SetExists(path)),
                Err(err) if matches!(err.kind(), ErrorKind::NotFound) => {
                    if path.parent().is_some_and(std::path::Path::exists) {
                        let parent = path.with_file_name("").canonicalize().unwrap();
                        let path = parent.with_file_name(path.file_name().unwrap());
                        (Text::new(), Path::SetAbsent(path))
                    } else {
                        (Text::new(), Path::new_unset())
                    }
                }
                Err(_) => (Text::new(), Path::new_unset()),
            },
        };

        #[cfg(feature = "wack")]
        let text = {
            let mut text = text;
            use crate::{
                forms::{self, Form},
                text::{Marker, Tag},
            };

            let marker = Marker::new();
            let form1 = forms::set("form1lmao", Form::new().red());
            let form2 = forms::set("form2lmao", Form::new().undercurled().underline_blue());
            for i in (500..text.len_bytes()).step_by(500) {
                text.insert_tag(i - 490, Tag::PushForm(form1), marker);
                text.insert_tag(i - 380, Tag::PopForm(form1), marker);
                text.insert_tag(i - 310, Tag::PushForm(form2), marker);
                text.insert_tag(i - 230, Tag::PushForm(form1), marker);
                text.insert_tag(i - 160, Tag::PopForm(form2), marker);
                text.insert_tag(i - 80, Tag::PopForm(form1), marker);
            }

            text
        };

        let file = File {
            path,
            text,
            cfg: self.cfg,
            history: History::new(),
            printed_lines: Vec::new(),
            _readers: Vec::new(),
        };

        let cursors = load_cache::<Cursors>(file.path()).unwrap_or_default();
        ((self.builder)(file, cursors), Box::new(|| false))
    }

    pub(crate) fn open_path(self, path: PathBuf) -> Self {
        Self { text_op: TextOp::OpenPath(path), ..self }
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

    pub(crate) fn set_input(&mut self, input: impl InputForFiles<U> + Clone) {
        self.builder = Arc::new(move |file, cursors| {
            let mut input = input.clone();
            input.set_cursors(cursors);
            Widget::active(file, input)
        });
    }
}

impl<U> WidgetCfg<U> for FileCfg<U>
where
    U: Ui,
{
    type Widget = File;

    fn build(self, _context: Context<U>, _: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
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
            builder: self.builder.clone(),
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
    _readers: Vec<Box<dyn Reader>>,
}

impl File {
    pub fn write(&self) -> Result<usize, String> {
        if let Path::SetExists(path) = &self.path {
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

    pub fn history_mut(&mut self) -> &mut History {
        &mut self.history
    }

    pub fn text(&self) -> &Text {
        &self.text
    }
}

/// # Querying functions
///
/// These functions serve the purpose of querying information from
/// the [`File`].
impl File {
    pub fn search(&self) -> Searcher<'_> {
        Searcher::new_at(Point::default(), self.text.iter().no_ghosts().no_conceals())
    }

    pub fn search_at(&self, point: Point) -> Searcher<'_> {
        Searcher::new_at(point, self.text.iter_at(point).no_ghosts().no_conceals())
    }

    pub fn rev_search(&self) -> RevSearcher<'_> {
        RevSearcher::new_at(
            self.text.len_point(),
            self.text.rev_iter().no_ghosts().no_conceals(),
        )
    }

    pub fn rev_search_at(&self, point: Point) -> RevSearcher<'_> {
        RevSearcher::new_at(
            point,
            self.text.rev_iter_at(point).no_ghosts().no_conceals(),
        )
    }

    pub fn exists(&self) -> bool {
        self.set_path()
            .is_some_and(|p| std::fs::exists(PathBuf::from(&p)).is_ok_and(|e| e))
    }

    /// The full path of the file.
    ///
    /// If there is no set path, returns `"*scratch file*#{id}"`.
    pub fn path(&self) -> String {
        match &self.path {
            Path::SetExists(path) | Path::SetAbsent(path) => path.to_string_lossy().to_string(),
            Path::UnSet(id) => {
                let path = std::env::current_dir()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();

                format!("{path}/*scratch file*#{id}")
            }
        }
    }

    /// The full path of the file.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn set_path(&self) -> Option<String> {
        match &self.path {
            Path::SetExists(path) | Path::SetAbsent(path) => {
                Some(path.to_string_lossy().to_string())
            }
            Path::UnSet(_) => None,
        }
    }

    /// The file's name.
    ///
    /// If there is no set path, returns `"*scratch file #{id}*"`.
    pub fn name(&self) -> String {
        match &self.path {
            Path::SetExists(path) | Path::SetAbsent(path) => {
                path.file_name().unwrap().to_string_lossy().to_string()
            }
            Path::UnSet(id) => format!("*scratch file #{id}*"),
        }
    }

    /// The file's name.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn set_name(&self) -> Option<String> {
        match &self.path {
            Path::SetExists(path) | Path::SetAbsent(path) => {
                Some(path.file_name().unwrap().to_string_lossy().to_string())
            }
            Path::UnSet(_) => None,
        }
    }

    /// The number of bytes in the file.
    pub fn len_bytes(&self) -> usize {
        self.text.len_bytes()
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.len_chars()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.len_lines()
    }

    /// Returns the currently printed set of lines.
    ///
    /// These are returned as a `usize`, showing the index of the line
    /// in the file, and a `bool`, which is `true` when the line is
    /// wrapped.
    pub fn printed_lines(&self) -> &[(usize, bool)] {
        &self.printed_lines
    }
}

/// # History related functions.
///
/// These functions allow for the modification of the [`File`]'s
/// [`Text`] by navigating through a [`History`]'s changes.
/// For now, this is a linear history (i.e. modification removes all
/// future changes), but the plan is to change it to a tree at some
/// point.
impl File {
    /// Begins a new moment in history.
    ///
    /// A new moment makes it so that "undoing" or "redoing" will undo
    /// or redo all the changes in the moment. The previous moment can
    /// be undone, undoing multiple changes at once.
    pub fn add_moment(&mut self) {
        self.history.new_moment()
    }

    /// Redoes the next moment, if there is one.
    pub fn redo(&mut self, area: &impl Area, cursors: &mut Cursors) {
        self.history.redo(&mut self.text, area, &self.cfg, cursors)
    }

    /// Undoes the last moment, if there was one.
    pub fn undo(&mut self, area: &impl Area, cursors: &mut Cursors) {
        self.history.undo(&mut self.text, area, &self.cfg, cursors)
    }

    /// Returns a mutable reference to the [`Text`] and [`History`] of
    /// the [`File`].
    pub fn mut_text_and_history(&mut self) -> (&mut Text, &mut History) {
        (&mut self.text, &mut self.history)
    }
}

impl<U> PassiveWidget<U> for File
where
    U: Ui,
{
    fn build(_context: Context<U>, _: bool) -> (Widget<U>, impl Fn() -> bool, crate::ui::PushSpecs)
    where
        Self: Sized,
    {
        let (widget, checker) = FileCfg::new().build();
        (widget, checker, PushSpecs::above())
    }

    fn update(&mut self, _area: &U::Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> &PrintCfg {
        &self.cfg
    }

    fn once(_context: crate::data::Context<U>) {}

    fn print(&mut self, area: &<U as Ui>::Area) {
        let (start, _) = area.top_left();

        let mut last_line = area
            .rev_print_iter(self.text.rev_iter_at(start), IterCfg::new(&self.cfg))
            .find_map(|(caret, item)| caret.wrap.then_some(item.line()));

        self.printed_lines.clear();
        let printed_lines = &mut self.printed_lines;

        let mut has_wrapped = false;

        area.print_with(
            &self.text,
            &self.cfg,
            forms::painter(),
            move |caret, item| {
                has_wrapped |= caret.wrap;
                if has_wrapped && item.part.is_char() {
                    has_wrapped = false;
                    let line = item.line();
                    let wrapped = last_line.is_some_and(|ll| ll == line);
                    last_line = Some(line);
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
    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn text_mut_and_print_cfg(&mut self) -> (&mut Text, &PrintCfg) {
        (&mut self.text, &self.cfg)
    }
}

unsafe impl Send for File {}
unsafe impl Sync for File {}

#[derive(Clone)]
enum Path {
    SetExists(PathBuf),
    SetAbsent(PathBuf),
    UnSet(usize),
}

impl Path {
    fn new_unset() -> Path {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        Path::UnSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

enum TextOp {
    NewBuffer,
    TakeText(Text, Path),
    OpenPath(PathBuf),
}
