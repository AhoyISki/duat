//! The primary widget of Duat, used to display files.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`File`], such as multiple [`Cursor`]s, a
//! `History` system, [`Area::PrintInfo`], etc.
//!
//! The [`File`] also provides a list of printed lines through the
//! [`File::printed_lines`] method. This method is notably used by the
//! [`LineNumbers`] widget, that shows the numbers of the currently
//! printed lines.
//!
//! [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
//! [`Cursor`]: crate::mode::Cursor
use std::{
    fs,
    marker::PhantomData,
    path::{Path, PathBuf},
    sync::Arc,
};

use parking_lot::Mutex;

use self::parser::Parsers;
pub use self::parser::{FileTracker, Parser, ParserCfg};
use crate::{
    cfg::{NewLine, PrintCfg, ScrollOff, TabStops, WordChars, WrapMethod},
    context::{self, Cache, Handle},
    data::Pass,
    form::Painter,
    hook::{self, FileWritten},
    mode::{Selection, Selections},
    text::{BuilderPart, Bytes, Text, txt},
    ui::{Area, BuildInfo, PushSpecs, Ui, Widget, WidgetCfg},
};

mod parser;

/// The configuration for a new [`File`]
#[derive(Default)]
#[doc(hidden)]
pub struct FileCfg<U: Ui> {
    text_op: TextOp,
    print_cfg: PrintCfg,
    add_parsers: Option<Box<dyn FnOnce(&mut File<U>)>>,
}

impl<U: Ui> FileCfg<U> {
    /// Returns a new instance of [`FileCfg`], opening a new buffer
    pub(crate) fn new() -> Self {
        FileCfg {
            text_op: TextOp::NewBuffer,
            print_cfg: PrintCfg::default_for_input(),
            add_parsers: None,
        }
    }

    /// Adds a [`Parser`] to the [`File`]
    pub fn with_parser(mut self, parser_cfg: impl ParserCfg<U> + 'static) -> Self {
        let add_parsers = std::mem::take(&mut self.add_parsers);
        self.add_parsers = Some(Box::new(move |file| {
            if let Some(prev_add_parsers) = add_parsers {
                prev_add_parsers(file)
            }

            if let Err(err) = file.parsers.add(file, parser_cfg) {
                context::error!("{err}");
            }
        }));

        self
    }

    ////////// PrintCfg functions

    /// A mutable reference to the [`PrintCfg`]
    ///
    /// You mostly won't need this, as you can just use the other
    /// [`PrintCfg`] derived methods for the [`FileCfg`].
    pub fn print_cfg(&mut self) -> &mut PrintCfg {
        &mut self.print_cfg
    }

    /// Don't wrap when reaching the end of the area
    pub const fn dont_wrap(mut self) -> Self {
        self.print_cfg.dont_wrap();
        self
    }

    /// Wrap on the right edge of the area
    pub const fn wrap_on_edge(mut self) -> Self {
        self.print_cfg.wrap_method = WrapMethod::Edge;
        self
    }

    /// Wrap on [word] terminations
    ///
    /// [word]: word_chars
    pub const fn wrap_on_word(mut self) -> Self {
        self.print_cfg.wrap_method = WrapMethod::Edge;
        self
    }

    /// Wrap on a given distance from the left edge
    ///
    /// This can wrap beyond the screen, being a mix of [`unwrapped`]
    /// and [`edge_wrapped`].
    ///
    /// [`unwrapped`]: Self::unwrapped
    /// [`edge_wrapped`]: Self::edge_wrapped
    pub const fn wrap_at(mut self, cap: u8) -> Self {
        self.print_cfg.wrap_method = WrapMethod::Capped(cap);
        self
    }

    /// Reindent wrapped lines to the same level of indentation
    pub const fn indent_wraps(mut self, value: bool) -> Self {
        self.print_cfg.indent_wrapped = value;
        self
    }

    /// Sets the size of tabs
    pub const fn tabstop(mut self, tabstop: u8) -> Self {
        self.print_cfg.tab_stops = TabStops(tabstop);
        self
    }

    /// Sets a character to replace `'\n'`s with
    pub const fn new_line_as(mut self, char: char) -> Self {
        self.print_cfg.new_line = NewLine::AlwaysAs(char);
        self
    }

    /// Sets a character to replace `'\n'` only with trailing white
    /// space
    pub const fn trailing_new_line_as(mut self, char: char) -> Self {
        self.print_cfg.new_line = NewLine::AfterSpaceAs(char);
        self
    }

    /// Sets the horizontal and vertical scrolloff, respectively
    pub const fn scrolloff(mut self, x: u8, y: u8) -> Self {
        self.print_cfg.scrolloff = ScrollOff { x, y };
        self
    }

    /// Sets the horizontal scrolloff
    pub const fn x_scrolloff(mut self, x: u8) -> Self {
        self.print_cfg.scrolloff = ScrollOff { y: self.print_cfg.scrolloff.y, x };
        self
    }

    /// Sets the vertical scrolloff
    pub const fn y_scrolloff(mut self, y: u8) -> Self {
        self.print_cfg.scrolloff = ScrollOff { y, x: self.print_cfg.scrolloff.x };
        self
    }

    /// Sets the [`WordChars`]
    pub const fn word_chars(mut self, word_chars: WordChars) -> Self {
        self.print_cfg.word_chars = word_chars;
        self
    }

    /// Sets a forced horizontal scrolloff
    ///
    /// Without forced horizontal scrolloff, when you reach the end of
    /// a long line of text, the cursor will also reach the edge of
    /// the screen. With this enabled, Duat will keep a distance
    /// between the cursor and the edge of the screen.
    ///
    /// This is particularly useful in a situation like the
    /// [`PromptLine`] widget, in order to keep good visibility of the
    /// command.
    ///
    /// [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    pub const fn forced_horizontal_scrolloff(mut self, value: bool) -> Self {
        self.print_cfg.force_scrolloff = value;
        self
    }

    ////////// Path functions

    /// The path that the [`File`] will open with, if it was set
    pub fn path_set(&self) -> Option<String> {
        match &self.text_op {
            TextOp::TakeBuf(_, PathKind::NotSet(_), _) | TextOp::NewBuffer => None,
            TextOp::TakeBuf(_, PathKind::SetExists(path) | PathKind::SetAbsent(path), _)
            | TextOp::OpenPath(path) => Some(path.to_str()?.to_string()),
        }
    }

    /// Changes the path of this cfg
    pub(crate) fn open_path(self, path: Option<PathBuf>) -> Self {
        match path {
            Some(path) => Self { text_op: TextOp::OpenPath(path), ..self },
            None => Self { text_op: TextOp::NewBuffer, ..self },
        }
    }

    /// Takes a previous [`File`]
    pub(crate) fn take_from_prev(
        self,
        bytes: Bytes,
        pk: PathKind,
        has_unsaved_changes: bool,
    ) -> Self {
        Self {
            text_op: TextOp::TakeBuf(bytes, pk, has_unsaved_changes),
            ..self
        }
    }
}

impl<U: Ui> WidgetCfg<U> for FileCfg<U> {
    type Widget = File<U>;

    fn pushed(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
        let (text, path) = match self.text_op {
            TextOp::NewBuffer => (Text::new_with_history(), PathKind::new_unset()),
            TextOp::TakeBuf(bytes, pk, has_unsaved_changes) => match &pk {
                PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                    let selections = {
                        let cursor = Cache::new().load(path).unwrap_or_default();
                        Selections::new(cursor)
                    };
                    let text = Text::from_file(bytes, selections, path, has_unsaved_changes);
                    (text, pk)
                }
                PathKind::NotSet(_) => (
                    Text::from_bytes(bytes, Selections::new(Selection::default()), true),
                    pk,
                ),
            },
            TextOp::OpenPath(path) => {
                let canon_path = path.canonicalize();
                if let Ok(path) = &canon_path
                    && let Ok(file) = std::fs::read_to_string(path)
                {
                    let selections = {
                        let cursor = Cache::new().load(path).unwrap_or_default();
                        Selections::new(cursor)
                    };
                    let text = Text::from_file(Bytes::new(&file), selections, path, false);
                    (text, PathKind::SetExists(path.clone()))
                } else if canon_path.is_err()
                    && let Ok(mut canon_path) = path.with_file_name(".").canonicalize()
                {
                    canon_path.push(path.file_name().unwrap());
                    (Text::new_with_history(), PathKind::SetAbsent(canon_path))
                } else {
                    (Text::new_with_history(), PathKind::new_unset())
                }
            }
        };

        let mut file = File {
            path,
            text,
            cfg: Arc::new(Mutex::new(self.print_cfg)),
            printed_lines: (0..40).map(|i| (i, i == 1)).collect(),
            parsers: Parsers::default(),
            layout_order: 0,
            _ghost: PhantomData,
        };

        if let Some(add_parsers) = self.add_parsers {
            add_parsers(&mut file);
        }

        // The PushSpecs don't matter
        (file, PushSpecs::above())
    }
}

impl<U: Ui> Clone for FileCfg<U> {
    fn clone(&self) -> Self {
        Self {
            text_op: self.text_op.clone(),
            print_cfg: self.print_cfg,
            add_parsers: None,
        }
    }
}

/// The widget that is used to print and edit files
pub struct File<U: Ui> {
    path: PathKind,
    text: Text,
    printed_lines: Vec<(usize, bool)>,
    parsers: Parsers<U>,
    /// The [`PrintCfg`] of this [`File`]
    cfg: Arc<Mutex<PrintCfg>>,
    pub(crate) layout_order: usize,
    _ghost: PhantomData<U>,
}

impl<U: Ui> File<U> {
    ////////// Writing the File

    /// Writes the file to the current [`PathBuf`], if one was set
    pub fn save(&mut self) -> Result<Option<usize>, Text> {
        self.save_quit(false)
    }

    /// Saves and quits, resulting in no config reload
    pub(crate) fn save_quit(&mut self, quit: bool) -> Result<Option<usize>, Text> {
        if let PathKind::SetExists(path) | PathKind::SetAbsent(path) = &self.path {
            let path = path.clone();
            if self.text.has_unsaved_changes() {
                let bytes = self
                    .text
                    .write_to(std::io::BufWriter::new(fs::File::create(&path)?))
                    .inspect(|_| self.path = PathKind::SetExists(path.clone()))?;

                let path = path.to_string_lossy().to_string();
                hook::queue(FileWritten((path, bytes, quit)));

                Ok(Some(bytes))
            } else {
                Ok(None)
            }
        } else {
            Err(txt!("No file was set").build())
        }
    }

    /// Writes the file to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub fn save_to(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<Option<usize>> {
        self.save_quit_to(path, false)
    }

    /// Writes the file to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub(crate) fn save_quit_to(
        &self,
        path: impl AsRef<std::path::Path>,
        quit: bool,
    ) -> std::io::Result<Option<usize>> {
        if self.text.has_unsaved_changes() {
            let path = path.as_ref();
            let res = self
                .text
                .write_to(std::io::BufWriter::new(fs::File::create(path)?))
                .map(Some);

            if let Ok(Some(bytes)) = res.as_ref() {
                hook::queue(FileWritten((
                    path.to_string_lossy().to_string(),
                    *bytes,
                    quit,
                )));
            }

            res
        } else {
            Ok(None)
        }
    }

    ////////// Path querying functions

    /// The full path of the file.
    ///
    /// If there is no set path, returns `"*scratch file*#{id}"`.
    pub fn path(&self) -> String {
        self.path.path()
    }

    /// The full path of the file.
    ///
    /// Returns [`None`] if the path has not been set yet, i.e., if
    /// the file is a scratch file.
    pub fn path_set(&self) -> Option<String> {
        self.path.path_set()
    }

    /// A [`Text`] from the full path of this [`PathKind`]
    ///
    /// # Formatting
    ///
    /// If the file's `path` was set:
    ///
    /// ```text
    /// [file]{path}
    /// ```
    ///
    /// If the file's `path` was not set:
    ///
    /// ```text
    /// [file.new.scratch]*scratch file #{id}*
    /// ```
    pub fn path_txt(&self) -> Text {
        self.path_kind().path_txt()
    }

    /// The file's name.
    ///
    /// If there is no set path, returns `"*scratch file #{id}*"`.
    pub fn name(&self) -> String {
        self.path.name()
    }

    /// The file's name.
    ///
    /// Returns [`None`] if the path has not been set yet, i.e., if
    /// the file is a scratch file.
    pub fn name_set(&self) -> Option<String> {
        self.path.name_set()
    }

    /// A [`Text`] from the name of this [`PathKind`]
    ///
    /// The name of a [`File`] widget is the same as the path, but it
    /// strips away the current directory. If it can't, it will try to
    /// strip away the home directory, replacing it with `"~"`. If
    /// that also fails, it will just show the full path.
    ///
    /// # Formatting
    ///
    /// If the file's `name` was set:
    ///
    /// ```text
    /// [file]{name}
    /// ```
    ///
    /// If the file's `name` was not set:
    ///
    /// ```text
    /// [file.new.scratch]*scratch file #{id}*
    /// ```
    pub fn name_txt(&self) -> Text {
        self.path.name_txt()
    }

    /// The type of [`PathBuf`]
    ///
    /// This represents the three possible states for a [`File`]'s
    /// [`PathBuf`], as it could either represent a real [`File`], not
    /// exist, or not have been defined yet.
    pub fn path_kind(&self) -> PathKind {
        self.path.clone()
    }

    /// Returns the currently printed set of lines.
    ///
    /// These are returned as a `usize`, showing the index of the line
    /// in the file, and a `bool`, which is `true` when the line is
    /// wrapped.
    pub fn printed_lines(&self) -> &[(usize, bool)] {
        &self.printed_lines
    }

    ////////// General querying functions

    /// The [`Bytes`] of the [`File`]'s [`Text`]
    pub fn bytes(&self) -> &Bytes {
        self.text.bytes()
    }

    /// The number of bytes in the file.
    pub fn len_bytes(&self) -> usize {
        self.text.len().byte()
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> usize {
        self.text.len().char()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> usize {
        self.text.len().line()
    }

    /// The [`Selections`] that are used on the [`Text`], if they
    /// exist
    pub fn selections(&self) -> &Selections {
        self.text.selections()
    }

    /// A mutable reference to the [`Selections`], if they exist
    pub fn selections_mut(&mut self) -> &mut Selections {
        self.text.selections_mut()
    }

    /// Whether o not the [`File`] exists or not
    pub fn exists(&self) -> bool {
        self.path_set()
            .is_some_and(|p| std::fs::exists(PathBuf::from(&p)).is_ok_and(|e| e))
    }

    /// Reads from a specific [`Parser`], if it was [added]
    ///
    /// This function will block until the [`Parser`] is ready to be
    /// read. This usually implies that the [`Parser`] is done
    /// processing all the [`Change`]s up to this point. But it could
    /// also be the case that the [`Parser`] doesn't really care about
    /// [`Change`]s.
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// If you want to read in a non blocking way, see
    /// [`File::try_read_parser`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    pub fn read_parser<P: Parser<U>, Ret>(&self, read: impl FnOnce(&P) -> Ret) -> Option<Ret> {
        self.parsers.read_parser(read)
    }

    /// Tries tor read from a specific [`Parser`], if it was [added]
    ///
    /// Unlike [`File::read_parser`], this function will only be
    /// called when the [`Parser`] is ready to be read. This may not
    /// be the case if, for example, it is still processing
    /// [`Change`]s to the [`Text`]. In that case, the function will
    /// not be called and [`try_read_parser`] will return [`None`].
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    /// [`try_read_parser`]: Self::try_read_parser
    pub fn try_read_parser<P: Parser<U>, Ret>(&self, read: impl FnOnce(&P) -> Ret) -> Option<Ret> {
        self.parsers.try_read_parser(read)
    }

    /// Writes to a specific [`Parser`], if it was [added]
    ///
    /// This function will block until the [`Parser`] is ready to be
    /// written to. This usually implies that the [`Parser`] is done
    /// processing all the [`Change`]s up to this point. But it could
    /// also be the case that the [`Parser`] doesn't really care
    /// about [`Change`]s.
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// If you want to write in a non blocking way, see
    /// [`File::try_write_parser`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    pub fn write_parser<P: Parser<U>, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        self.parsers.write_parser(write)
    }

    /// Tries tor read a specific [`Parser`], if it was [added]
    ///
    /// Unlike [`File::write_parser`], this function will only be
    /// called when the [`Parser`] is ready to be written to. This may
    /// not be the case if, for example, it is still processing
    /// [`Change`]s to the [`Text`]. In that case, the function will
    /// not be called and [`try_write_parser`] will return [`None`].
    ///
    /// While this function is being called, trying to read or write
    /// to the same [`Parser`] will always return [`None`].
    ///
    /// [added]: Handle::add_parser
    /// [`Change`]: crate::text::Change
    /// [`try_write_parser`]: Self::try_write_parser
    pub fn try_write_parser<P: Parser<U>, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        self.parsers.try_write_parser(write)
    }
}

impl<U: Ui> Widget<U> for File<U> {
    type Cfg = FileCfg<U>;

    fn cfg() -> Self::Cfg {
        FileCfg::new()
    }

    fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
        let parsers = std::mem::take(&mut handle.write(pa).parsers);

        let file = handle.read(pa);
        let cfg = file.print_cfg();

        let (file, area) = handle.write_with_area(pa);

        if let Some(main) = file.text().selections().get_main() {
            area.scroll_around_point(file.text(), main.caret(), file.print_cfg());
        }

        let (start, _) = area.start_points(&file.text, cfg);
        let (end, _) = area.end_points(&file.text, cfg);

        parsers.update(pa, handle, start.byte()..end.byte());

        let file = handle.write(pa);
        file.parsers = parsers;

        file.text.update_bounds();
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.parsers.needs_update()
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn print_cfg(&self) -> PrintCfg {
        *self.cfg.lock()
    }

    fn print(&mut self, painter: Painter, area: &<U as Ui>::Area) {
        let cfg = *self.cfg.lock();
        let (start, _) = area.start_points(&self.text, cfg);

        let mut last_line = area
            .rev_print_iter(self.text.iter_rev(start), cfg)
            .find_map(|(caret, item)| caret.wrap.then_some(item.line()));

        self.printed_lines.clear();
        let printed_lines = &mut self.printed_lines;

        let mut has_wrapped = false;

        area.print_with(&mut self.text, cfg, painter, move |caret, item| {
            has_wrapped |= caret.wrap;
            if has_wrapped && item.part.is_char() {
                has_wrapped = false;
                let line = item.line();
                let wrapped = last_line.is_some_and(|ll| ll == line);
                last_line = Some(line);
                printed_lines.push((line, wrapped));
            }
        })
    }

    fn once() -> Result<(), Text> {
        Ok(())
    }
}

impl<U: Ui> Handle<File<U>, U> {
    /// Adds a [`Parser`] to react to [`Text`] [`Change`]s
    ///
    /// [`Change`]: crate::text::Change
    pub fn add_parser(&mut self, pa: &mut Pass, cfg: impl ParserCfg<U>) {
        let file = self.widget().read(pa);

        if let Err(err) = file.parsers.add(file, cfg) {
            context::error!("{err}");
        }
    }
}

/// Represents the presence or absence of a path
#[derive(Debug, Clone)]
pub enum PathKind {
    /// A [`PathBuf`] that has been defined and points to a real file
    SetExists(PathBuf),
    /// A [`PathBuf`] that has been defined but isn't a real file
    SetAbsent(PathBuf),
    /// A [`PathBuf`] that has not been defined
    ///
    /// The number within represents a specific [`File`], and when
    /// printed to, for example, the [`StatusLine`], would show up as
    /// `txt!("[file]*scratch file*#{id}")`
    ///
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    NotSet(usize),
}

impl PathKind {
    /// Returns a new unset [`PathBuf`]
    pub(crate) fn new_unset() -> PathKind {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        PathKind::NotSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    /// Returns a [`PathBuf`] if `self` is [`SetExists`] or
    /// [`SetAbsent`]
    pub fn as_path(&self) -> Option<PathBuf> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => Some(path.clone()),
            PathKind::NotSet(_) => None,
        }
    }

    /// The full path of the file.
    ///
    /// If there is no set path, returns `"*scratch file*#{id}"`.
    pub fn path(&self) -> String {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                path.to_string_lossy().to_string()
            }
            PathKind::NotSet(id) => {
                format!("*scratch file*#{id}")
            }
        }
    }

    /// The full path of the file.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn path_set(&self) -> Option<String> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                Some(path.to_string_lossy().to_string())
            }
            PathKind::NotSet(_) => None,
        }
    }

    /// The file's name.
    ///
    /// If there is no set path, returns `"*scratch file #{id}*"`.
    pub fn name(&self) -> String {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::cur_dir();
                if let Ok(path) = path.strip_prefix(cur_dir) {
                    path.to_string_lossy().to_string()
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    Path::new("~").join(path).to_string_lossy().to_string()
                } else {
                    path.to_string_lossy().to_string()
                }
            }
            PathKind::NotSet(id) => format!("*scratch file #{id}*"),
        }
    }

    /// The file's name.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn name_set(&self) -> Option<String> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::cur_dir();
                Some(if let Ok(path) = path.strip_prefix(cur_dir) {
                    path.to_string_lossy().to_string()
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    Path::new("~").join(path).to_string_lossy().to_string()
                } else {
                    path.to_string_lossy().to_string()
                })
            }
            PathKind::NotSet(_) => None,
        }
    }

    /// A [`Text`] from the full path of this [`PathKind`]
    ///
    /// # Formatting
    ///
    /// If the file's `path` was set:
    ///
    /// ```text
    /// [file]{path}
    /// ```
    ///
    /// If the file's `path` was not set:
    ///
    /// ```text
    /// [file.new.scratch]*scratch file #{id}*
    /// ```
    pub fn path_txt(&self) -> Text {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => txt!("[file]{path}").build(),
            PathKind::NotSet(id) => txt!("[file.new.scratch]*scratch file #{id}*").build(),
        }
    }

    /// A [`Text`] from the name of this [`PathKind`]
    ///
    /// The name of a [`File`] widget is the same as the path, but it
    /// strips away the current directory. If it can't, it will try to
    /// strip away the home directory, replacing it with `"~"`. If
    /// that also fails, it will just show the full path.
    ///
    /// # Formatting
    ///
    /// If the file's `name` was set:
    ///
    /// ```text
    /// [file]{name}
    /// ```
    ///
    /// If the file's `name` was not set:
    ///
    /// ```text
    /// [file.new.scratch]*scratch file #{id}*
    /// ```
    pub fn name_txt(&self) -> Text {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::cur_dir();
                if let Ok(path) = path.strip_prefix(cur_dir) {
                    txt!("[file]{path}").build()
                } else if let Some(home_dir) = dirs_next::home_dir()
                    && let Ok(path) = path.strip_prefix(home_dir)
                {
                    txt!("[file]{}", Path::new("~").join(path)).build()
                } else {
                    txt!("[file]{path}").build()
                }
            }
            PathKind::NotSet(id) => txt!("[file.new.scratch]*scratch file #{id}*").build(),
        }
    }
}

impl<P: AsRef<Path>> From<P> for PathKind {
    fn from(value: P) -> Self {
        let path = value.as_ref();
        if let Ok(true) = path.try_exists() {
            PathKind::SetExists(path.into())
        } else {
            PathKind::SetAbsent(path.into())
        }
    }
}

impl PartialEq for PathKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::SetExists(l0) | Self::SetAbsent(l0),
                Self::SetExists(r0) | Self::SetAbsent(r0),
            ) => l0 == r0,
            (Self::NotSet(l0), Self::NotSet(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for PathKind {}

impl From<PathKind> for BuilderPart {
    fn from(value: PathKind) -> Self {
        BuilderPart::Text(value.name_txt())
    }
}

/// What to do when opening the [`File`]
#[derive(Default, Clone)]
enum TextOp {
    #[default]
    NewBuffer,
    TakeBuf(Bytes, PathKind, bool),
    OpenPath(PathBuf),
}
