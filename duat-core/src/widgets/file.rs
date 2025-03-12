//! The primary widget of Duat, used to display files.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`File`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`Area::PrintInfo`], etc.
//!
//! The [`File`] also provides a list of printed lines through the
//! [`File::printed_lines`] method. This method is notably used by the
//! [`LineNumbers`] widget, that shows the numbers of the currently
//! printed lines.
//!
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [`Cursor`]: crate::mode::Cursor
use std::{fs, path::PathBuf};

use gapbuf::GapBuffer;

use crate::{
    cache::load_cache,
    cfg::{IterCfg, PrintCfg},
    context, form,
    mode::Cursors,
    text::{Text, err},
    ui::{Area, PushSpecs, Ui},
    widgets::{Widget, WidgetCfg},
};

/// The configuration for a new [`File`]
#[derive(Default, Clone)]
#[doc(hidden)]
pub struct FileCfg {
    text_op: TextOp,
    cfg: PrintCfg,
}

impl FileCfg {
    /// Returns a new instance of [`FileCfg`], opening a new buffer
    pub(crate) fn new() -> Self {
        FileCfg {
            text_op: TextOp::NewBuffer,
            cfg: PrintCfg::default_for_input(),
        }
    }

    /// Changes the path of this cfg
    pub(crate) fn open_path(self, path: PathBuf) -> Self {
        Self { text_op: TextOp::OpenPath(path), ..self }
    }

    /// Takes a previous [`File`]
    pub(crate) fn take_from_prev(
        self,
        buf: GapBuffer<u8>,
        path_kind: PathKind,
        has_unsaved_changes: bool,
    ) -> Self {
        Self {
            text_op: TextOp::TakeBuf(buf, path_kind, has_unsaved_changes),
            ..self
        }
    }

    /// Sets the [`PrintCfg`]
    pub(crate) fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.cfg = cfg;
    }
}

impl<U: Ui> WidgetCfg<U> for FileCfg {
    type Widget = File;

    fn build(self, _: bool) -> (Self::Widget, impl Fn() -> bool, PushSpecs) {
        let (text, path) = match self.text_op {
            TextOp::NewBuffer => (Text::new_with_history(), PathKind::new_unset()),
            TextOp::TakeBuf(buf, path, has_unsaved_changes) => match &path {
                PathKind::SetExists(p) | PathKind::SetAbsent(p) => {
                    let cursors = load_cache(p).unwrap_or_default();
                    (Text::from_file(buf, cursors, p, has_unsaved_changes), path)
                }
                PathKind::NotSet(_) => (Text::from_buf(buf, Some(Cursors::default()), true), path),
            },
            TextOp::OpenPath(path) => {
                let canon_path = path.canonicalize();
                if let Ok(path) = &canon_path
                    && let Ok(file) = std::fs::read_to_string(path)
                {
                    let cursors = load_cache(path).unwrap_or_default();
                    let buf = GapBuffer::from_iter(file.bytes());
                    (
                        Text::from_file(buf, cursors, path, false),
                        PathKind::SetExists(path.clone()),
                    )
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

        #[cfg(feature = "wack")]
        let text = {
            let mut text = text;
            use crate::{
                form::{self, Form},
                text::{Key, Tag, text},
            };

            let key = Key::new();
            let key2 = Key::new();
            let form1 = form::set("form1lmao", Form::red().bold());
            text.insert_tag(3, Tag::PushForm(form1), key);
            text.insert_tag(2, Tag::PushForm(form1), key);
            text.insert_tag(2, Tag::PushForm(form1), key2);

            text.insert_tag(7, Tag::PopForm(form1), key);
            text.insert_tag(8, Tag::PopForm(form1), key);
            text.insert_tag(8, Tag::PopForm(form1), key2);

            text
        };

        let file = File {
            path,
            text,
            cfg: self.cfg,
            printed_lines: (0..40).map(|i| (i, i == 1)).collect(),
            layout_ordering: 0,
        };

        // The PushSpecs don't matter
        (file, Box::new(|| false), PushSpecs::above())
    }
}

/// The widget that is used to print and edit files
pub struct File {
    path: PathKind,
    text: Text,
    cfg: PrintCfg,
    printed_lines: Vec<(usize, bool)>,
    pub(crate) layout_ordering: usize,
}

impl File {
    ////////// Writing the File

    /// Writes the file to the current [`Path`], if one was set
    ///
    /// [`Path`]: std::path::Path
    #[allow(clippy::result_large_err)]
    pub fn write(&mut self) -> Result<usize, Text> {
        if let PathKind::SetExists(path) | PathKind::SetAbsent(path) = &self.path {
            let path = path.clone();
            self.text
                .write_to(std::io::BufWriter::new(fs::File::create(&path)?))
                .inspect(|_| self.path = PathKind::SetExists(path))
                .map_err(Text::from)
        } else {
            Err(err!("No file was set"))
        }
    }

    /// Writes the file to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub fn write_to(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<usize> {
        self.text
            .write_to(std::io::BufWriter::new(fs::File::create(path)?))
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
    /// Returns [`None`] if the path has not been set yet.
    pub fn path_set(&self) -> Option<String> {
        self.path.path_set()
    }

    /// The file's name.
    ///
    /// If there is no set path, returns `"*scratch file #{id}*"`.
    pub fn name(&self) -> String {
        self.path.name()
    }

    /// The file's name.
    ///
    /// Returns [`None`] if the path has not been set yet.
    pub fn name_set(&self) -> Option<String> {
        self.path.name_set()
    }

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

    /// The [`Text`] of the [`File`]
    pub fn text(&self) -> &Text {
        &self.text
    }

    pub fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    /// The mutable [`Text`] of the [`File`]
    pub fn print_cfg(&self) -> PrintCfg {
        self.cfg
    }

    /// The [`Cursors`] that are used on the [`Text`], if they exist
    pub fn cursors(&self) -> &Cursors {
        self.text.cursors().unwrap()
    }

    /// A mutable reference to the [`Cursors`], if they exist
    pub fn cursors_mut(&mut self) -> Option<&mut Cursors> {
        self.text.cursors_mut()
    }

    /// Whether o not the [`File`] exists or not
    pub fn exists(&self) -> bool {
        self.path_set()
            .is_some_and(|p| std::fs::exists(PathBuf::from(&p)).is_ok_and(|e| e))
    }
}

impl<U: Ui> Widget<U> for File {
    type Cfg = FileCfg;

    fn cfg() -> Self::Cfg {
        FileCfg::new()
    }

    fn update(&mut self, _area: &U::Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        self.text_mut()
    }

    fn print_cfg(&self) -> PrintCfg {
        self.cfg
    }

    fn print(&mut self, area: &<U as Ui>::Area) {
        let (start, _) = area.first_points(&self.text, self.cfg);

        let mut last_line = area
            .rev_print_iter(self.text.iter_rev(start), IterCfg::new(self.cfg))
            .find_map(|(caret, item)| caret.wrap.then_some(item.line()));

        self.printed_lines.clear();
        let printed_lines = &mut self.printed_lines;

        let mut has_wrapped = false;

        area.print_with(
            &mut self.text,
            self.cfg,
            form::painter::<Self>(),
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

    fn once() -> crate::Result<(), ()> {
        Ok(())
    }
}

/// Represents the presence or absence of a path
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathKind {
    SetExists(PathBuf),
    SetAbsent(PathBuf),
    NotSet(usize),
}

impl PathKind {
    /// Returns a new unset [`Path`]
    fn new_unset() -> PathKind {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        PathKind::NotSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn path(&self) -> String {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                path.to_string_lossy().to_string()
            }
            PathKind::NotSet(id) => {
                let path = std::env::current_dir()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();

                format!("{path}/*scratch file*#{id}")
            }
        }
    }

    pub fn path_set(&self) -> Option<String> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                Some(path.to_string_lossy().to_string())
            }
            PathKind::NotSet(_) => None,
        }
    }

    pub fn name(&self) -> String {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::cur_dir();
                if let Ok(path) = path.strip_prefix(cur_dir) {
                    path.to_string_lossy().to_string()
                } else {
                    path.to_string_lossy().to_string()
                }
            }
            PathKind::NotSet(id) => format!("*scratch file #{id}*"),
        }
    }

    pub fn name_set(&self) -> Option<String> {
        match self {
            PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                let cur_dir = context::cur_dir();
                Some(if let Ok(path) = path.strip_prefix(cur_dir) {
                    path.to_string_lossy().to_string()
                } else {
                    path.to_string_lossy().to_string()
                })
            }
            PathKind::NotSet(_) => None,
        }
    }
}

/// What to do when opening the [`File`]
#[derive(Default, Clone)]
enum TextOp {
    #[default]
    NewBuffer,
    TakeBuf(GapBuffer<u8>, PathKind, bool),
    OpenPath(PathBuf),
}
