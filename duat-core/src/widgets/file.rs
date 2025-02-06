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
use std::{fs, io::ErrorKind, path::PathBuf};

use crate::{
    cfg::{IterCfg, PrintCfg},
    form,
    text::Text,
    ui::{Area, PushSpecs, Ui},
    widgets::{Widget, WidgetCfg},
};

/// The configuration for a new [`File`]
#[derive(Default, Clone)]
pub struct FileCfg {
    text_op: TextOp,
    cfg: PrintCfg,
}

impl FileCfg {
    /// Returns a new instance of [`FileCfg`], opening anew buffer
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
    pub(crate) fn take_from_prev(self, prev: &mut File) -> Self {
        let text = std::mem::take(&mut prev.text);
        Self {
            text_op: TextOp::TakeText(text, prev.path.clone()),
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
            printed_lines: Vec::new(),
        };

        // The PushSpecs don't matter
        (file, Box::new(|| false), PushSpecs::above())
    }
}

/// The widget that is used to print and edit files
pub struct File {
    path: Path,
    text: Text,
    cfg: PrintCfg,
    printed_lines: Vec<(u32, bool)>,
}

impl File {
    ////////// Writing the File

    /// Writes the file to the current [`Path`], if one was set
    ///
    /// [`Path`]: std::path::Path
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
    pub fn path_set(&self) -> Option<String> {
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
    pub fn name_set(&self) -> Option<String> {
        match &self.path {
            Path::SetExists(path) | Path::SetAbsent(path) => {
                Some(path.file_name().unwrap().to_string_lossy().to_string())
            }
            Path::UnSet(_) => None,
        }
    }

    /// Returns the currently printed set of lines.
    ///
    /// These are returned as a `usize`, showing the index of the line
    /// in the file, and a `bool`, which is `true` when the line is
    /// wrapped.
    pub fn printed_lines(&self) -> &[(u32, bool)] {
        &self.printed_lines
    }

    ////////// General querying functions

    /// The number of bytes in the file.
    pub fn len_bytes(&self) -> u32 {
        self.text.len().byte()
    }

    /// The number of [`char`]s in the file.
    pub fn len_chars(&self) -> u32 {
        self.text.len().char()
    }

    /// The number of lines in the file.
    pub fn len_lines(&self) -> u32 {
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
        &mut self.text
    }

    fn print_cfg(&self) -> PrintCfg {
        self.cfg
    }

    fn print(&mut self, area: &<U as Ui>::Area) {
        let (start, _) = area.top_left();

        let mut last_line = area
            .rev_print_iter(self.text.iter_rev(start), IterCfg::new(self.cfg))
            .find_map(|(caret, item)| caret.wrap.then_some(item.line()));

        self.printed_lines.clear();
        let printed_lines = &mut self.printed_lines;

        let mut has_wrapped = false;

        area.print_with(
            &mut self.text,
            self.cfg,
            form::painter(),
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
#[derive(Clone)]
enum Path {
    SetExists(PathBuf),
    SetAbsent(PathBuf),
    UnSet(usize),
}

impl Path {
    /// Returns a new unset [`Path`]
    fn new_unset() -> Path {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        Path::UnSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

/// What to do when opening the [`File`]
#[derive(Default, Clone)]
enum TextOp {
    #[default]
    NewBuffer,
    TakeText(Text, Path),
    OpenPath(PathBuf),
}
