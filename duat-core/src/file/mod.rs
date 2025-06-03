//! The primary widget of Duat, used to display files.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`File`], such as multiple [`Cursor`]s, a
//! [`History`] system, [`RawArea::PrintInfo`], etc.
//!
//! The [`File`] also provides a list of printed lines through the
//! [`File::printed_lines`] method. This method is notably used by the
//! [`LineNumbers`] widget, that shows the numbers of the currently
//! printed lines.
//!
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [`Cursor`]: crate::mode::Cursor
//! [`History`]: crate::text::History
use std::{fs, marker::PhantomData, path::PathBuf};

use tokio::task;

use self::reader::Readers;
pub use self::reader::{RangeList, Reader, ReaderCfg};
use crate::{
    cache::load_cache,
    cfg::PrintCfg,
    context::{self, FileHandle},
    data::{Pass, RwData},
    form,
    hook::{self, FileWritten},
    mode::Cursors,
    text::{Bytes, Text, err},
    ui::{PushSpecs, RawArea, Ui},
    widget::{Widget, WidgetCfg},
};

mod reader;

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
        bytes: Bytes,
        pk: PathKind,
        has_unsaved_changes: bool,
    ) -> Self {
        Self {
            text_op: TextOp::TakeBuf(bytes, pk, has_unsaved_changes),
            ..self
        }
    }

    /// Sets the [`PrintCfg`]
    pub(crate) fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.cfg = cfg;
    }
}

impl<U: Ui> WidgetCfg<U> for FileCfg {
    type Widget = File<U>;

    fn build(self, _: Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let (text, path) = match self.text_op {
            TextOp::NewBuffer => (Text::new_with_history(), PathKind::new_unset()),
            TextOp::TakeBuf(bytes, pk, has_unsaved_changes) => match &pk {
                PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                    let cursors = {
                        let cursor = load_cache(path).unwrap_or_default();
                        Cursors::new_with_main(cursor)
                    };
                    let text = Text::from_file(bytes, cursors, path, has_unsaved_changes);
                    (text, pk)
                }
                PathKind::NotSet(_) => {
                    (Text::from_bytes(bytes, Some(Cursors::default()), true), pk)
                }
            },
            TextOp::OpenPath(path) => {
                let canon_path = path.canonicalize();
                if let Ok(path) = &canon_path
                    && let Ok(file) = std::fs::read_to_string(path)
                {
                    let cursors = {
                        let cursor = load_cache(path).unwrap_or_default();
                        Cursors::new_with_main(cursor)
                    };
                    let text = Text::from_file(Bytes::new(&file), cursors, path, false);
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

        let file = File {
            path,
            text,
            cfg: self.cfg,
            printed_lines: (0..40).map(|i| (i, i == 1)).collect(),
            readers: Readers::default(),
            layout_order: 0,
            _ghost: PhantomData,
        };

        // The PushSpecs don't matter
        (file, PushSpecs::above())
    }
}

/// The widget that is used to print and edit files
pub struct File<U: Ui> {
    path: PathKind,
    text: Text,
    cfg: PrintCfg,
    printed_lines: Vec<(usize, bool)>,
    readers: Readers<U>,
    pub(crate) layout_order: usize,
    _ghost: PhantomData<U>,
}

impl<U: Ui> File<U> {
    ////////// Writing the File

    /// Writes the file to the current [`Path`], if one was set
    ///
    /// [`Path`]: std::path::Path
    pub fn write(&mut self) -> Result<Option<usize>, Text> {
        if let PathKind::SetExists(path) | PathKind::SetAbsent(path) = &self.path {
            let path = path.clone();
            if self.text.has_unsaved_changes() {
                let bytes = self
                    .text
                    .write_to(std::io::BufWriter::new(fs::File::create(&path)?))
                    .inspect(|_| self.path = PathKind::SetExists(path.clone()))?;

                hook::queue::<FileWritten>((path.to_string_lossy().to_string(), bytes));

                Ok(Some(bytes))
            } else {
                Ok(None)
            }
        } else {
            Err(err!("No file was set").build())
        }
    }

    /// Writes the file to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub fn write_to(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<Option<usize>> {
        if self.text.has_unsaved_changes() {
            let path = path.as_ref();
            let res = self
                .text
                .write_to(std::io::BufWriter::new(fs::File::create(path)?))
                .map(Some);

            if let Ok(Some(bytes)) = res.as_ref() {
                hook::queue::<FileWritten>((path.to_string_lossy().to_string(), *bytes));
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

    pub fn add_reader(&mut self, pa: &mut Pass, cfg: impl ReaderCfg<U>) {
        if let Err(err) = self.readers.add(pa, self.text.bytes_mut(), cfg) {
            context::notify(err);
        }
    }

    pub fn get_reader<R: Reader<U>>(&mut self) -> Option<RwData<R>> {
        self.readers.get()
    }
}

impl<U: Ui> Widget<U> for File<U> {
    type Cfg = FileCfg;

    fn cfg() -> Self::Cfg {
        FileCfg::new()
    }

    async fn update(mut pa: Pass<'_>, widget: RwData<Self>, area: &U::Area) {
        let (map, readers) = widget.read(&pa, |file| {
            (BytesDataMap(widget.clone()), file.readers.clone())
        });

        if let Some(moment) = widget.raw_write(|file| file.text.last_unprocessed_moment()) {
            task::spawn_local(async move { readers.process_changes(map, moment).await });
        }

        widget.write(&mut pa, |file| {
            if file.readers.needs_update() {
                let (start, _) = area.first_points(&file.text, file.cfg);
                let (end, _) = area.last_points(&file.text, file.cfg);

                // SAFETY: While it would be rare for this to be a problem, there is
                // none ¯\_(ツ)_/¯.
                let pa = unsafe { Pass::new() };
                file.readers
                    .update_range(pa, &mut file.text, start.byte()..end.byte());

                file.text.update_bounds();
            }
        })
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn needs_update(&self) -> bool {
        false
    }

    fn print_cfg(&self) -> PrintCfg {
        self.cfg
    }

    fn print(&mut self, area: &<U as Ui>::Area) {
        let (start, _) = area.first_points(&self.text, self.cfg);

        let mut last_line = area
            .rev_print_iter(self.text.iter_rev(start), self.cfg)
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

    fn once() -> Result<(), Text> {
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
    TakeBuf(Bytes, PathKind, bool),
    OpenPath(PathBuf),
}

#[derive(Clone)]
pub struct BytesDataMap<U: Ui>(RwData<File<U>>);

impl<U: Ui> BytesDataMap<U> {
    /// Reads the [`Bytes`] of the [`File`]'s [`Text`]
    ///
    /// This requires a [`&mut Pass`] because some reading operations
    /// on [`Bytes`] require exclusive acess.
    ///
    /// If you are looking at this method from the context of
    /// [`Reader::apply_changes`], you probably actually want to use
    /// [`BytesDataMap::read_and_write_reader`], since it is far more
    /// compatible with that usecase.
    ///
    /// # Panics
    ///
    /// Panics if there is a mutable borrow of this struct somewhere,
    /// which could happen if you use [`RwData::write_unsafe`] or
    /// [`RwData::write_unsafe_as`]
    pub fn read<Ret>(&self, _: &mut Pass, f: impl FnOnce(&mut Bytes) -> Ret) -> Ret {
        self.0.raw_write(|file| f(file.text.bytes_mut()))
    }

    /// Reads the [`Bytes`] of a [`File`], alongside a [`Reader`]
    ///
    /// This can be very convenient when you want access to these two
    /// things at once, and is completely safe, since [`File`] doesn't
    /// implement [`Reader`], the other [`RwData`] will never be
    /// [`RwData<File>`], so a double borrow could never happen.
    ///
    /// # Panics
    ///
    /// Panics if there is are any borrows of either struct elsewhere,
    /// which could happen if you use [`RwData::write_unsafe`] or
    /// [`RwData::write_unsafe_as`]
    pub fn read_and_write_reader<Ret, Rd: Reader<U>>(
        &self,
        pa: &mut Pass,
        rd: &RwData<Rd>,
        f: impl FnOnce(&mut Bytes, &mut Rd) -> Ret,
    ) -> Ret {
        self.0
            .raw_write(|file| rd.write(pa, |rd| f(file.text.bytes_mut(), rd)))
    }

    pub fn has_changed(&self) -> bool {
        self.0.has_changed()
    }
}
