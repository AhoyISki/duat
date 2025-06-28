//! The primary widget of Duat, used to display files.
//!
//! Most extensible features of Duat have the primary purpose of
//! serving the [`File`], such as multiple [`Cursor`]s, a
//! `History` system, [`RawArea::PrintInfo`], etc.
//!
//! The [`File`] also provides a list of printed lines through the
//! [`File::printed_lines`] method. This method is notably used by the
//! [`LineNumbers`] widget, that shows the numbers of the currently
//! printed lines.
//!
//! [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
//! [`Cursor`]: crate::mode::Cursor
use std::{fs, marker::PhantomData, path::PathBuf};

use self::reader::Readers;
pub use self::reader::{RangeList, Reader, ReaderCfg};
use crate::{
    cfg::PrintCfg,
    context::{self, FileHandle, Handle, load_cache},
    data::{Pass, RwData},
    form::Painter,
    hook::{self, FileWritten},
    mode::{Selection, Selections},
    text::{AsRefBytes, Bytes, RefBytes, Text, txt},
    ui::{PushSpecs, RawArea, Ui, Widget, WidgetCfg},
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

    fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let (text, path) = match self.text_op {
            TextOp::NewBuffer => (Text::new_with_history(), PathKind::new_unset()),
            TextOp::TakeBuf(bytes, pk, has_unsaved_changes) => match &pk {
                PathKind::SetExists(path) | PathKind::SetAbsent(path) => {
                    let selections = {
                        let cursor = load_cache(path).unwrap_or_default();
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
                        let cursor = load_cache(path).unwrap_or_default();
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

    /// Writes the file to the current [`PathBuf`], if one was set
    pub fn write(&mut self) -> Result<Option<usize>, Text> {
        self.write_quit(false)
    }

    pub(crate) fn write_quit(&mut self, quit: bool) -> Result<Option<usize>, Text> {
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
    pub fn write_to(&self, path: impl AsRef<std::path::Path>) -> std::io::Result<Option<usize>> {
        self.write_quit_to(path, false)
    }

    /// Writes the file to the given [`Path`]
    ///
    /// [`Path`]: std::path::Path
    pub(crate) fn write_quit_to(
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

    /// Adds a [`Reader`] to react to [`Text`] [`Change`]s
    ///
    /// [`Change`]: crate::text::Change
    pub fn add_reader(&mut self, pa: &mut Pass, cfg: impl ReaderCfg<U>) {
        if let Err(err) = self.readers.add(pa, self.text.ref_bytes(), cfg) {
            context::error!("{err}");
        }
    }

    /// Gets a [`Reader`]
    pub fn get_reader<Rd: Reader<U>>(&self) -> Option<RwData<Rd>> {
        self.readers.get()
    }
}

impl<U: Ui> Widget<U> for File<U> {
    type Cfg = FileCfg;

    fn cfg() -> Self::Cfg {
        FileCfg::new()
    }

    fn update(pa: &mut Pass, handle: Handle<Self, U>) {
        let (widget, area) = (handle.widget(), handle.area());
        let (map, readers) = widget.read(pa, |file| {
            (BytesDataMap(widget.clone()), file.readers.clone())
        });

        let moments = widget.acquire_mut(pa).text.last_unprocessed_moment();
        if let Some(moments) = moments {
            for moment in moments {
                readers.process_moment(map.clone(), moment);
            }
        }

        let mut file = widget.acquire_mut(pa);

        if let Some(main) = file.text().selections().get_main() {
            area.scroll_around_point(file.text(), main.caret(), file.print_cfg());
        }

        if file.readers.needs_update() {
            let (start, _) = area.start_points(&file.text, file.cfg);
            let (end, _) = area.end_points(&file.text, file.cfg);

            // SAFETY: I'm not passing the Pass to inner structures, so this
            // should be fine.
            let mut pa = unsafe { Pass::new() };
            readers.update_range(&mut pa, &mut file.text, start.byte()..end.byte());
        }

        file.text.update_bounds();
    }

    fn needs_update(&self) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn print_cfg(&self) -> PrintCfg {
        self.cfg
    }

    fn print(&mut self, painter: Painter, area: &<U as Ui>::Area) {
        let (start, _) = area.start_points(&self.text, self.cfg);

        let mut last_line = area
            .rev_print_iter(self.text.iter_rev(start), self.cfg)
            .find_map(|(caret, item)| caret.wrap.then_some(item.line()));

        self.printed_lines.clear();
        let printed_lines = &mut self.printed_lines;

        let mut has_wrapped = false;

        area.print_with(&mut self.text, self.cfg, painter, move |caret, item| {
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

/// Represents the presence or absence of a path
#[derive(Debug, Clone, PartialEq, Eq)]
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
    fn new_unset() -> PathKind {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static UNSET_COUNT: AtomicUsize = AtomicUsize::new(1);

        PathKind::NotSet(UNSET_COUNT.fetch_add(1, Ordering::Relaxed))
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

/// An [`RwData`] wrapper which only gives access to the [`Bytes`] of
/// a [`File`]
#[derive(Clone)]
pub struct BytesDataMap<U: Ui>(RwData<File<U>>);

impl<U: Ui> BytesDataMap<U> {
    /// Reads the [`Bytes`] of the [`File`]'s [`Text`]
    ///
    /// If you are looking at this method from the context of
    /// [`Reader::apply_changes`], you probably actually want to use
    /// [`BytesDataMap::write_with_reader`], since it is far more
    /// compatible with that usecase.
    ///
    /// # Panics
    ///
    /// Panics if there is a mutable borrow of this struct somewhere,
    /// which could happen if you use [`RwData::write_unsafe`] or
    /// [`RwData::write_unsafe_as`]
    pub fn read<Ret>(&self, pa: &Pass, f: impl FnOnce(&Bytes) -> Ret) -> Ret {
        self.0.read(pa, |file| f(file.text.bytes()))
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
    pub fn write_with_reader<Ret, Rd: Reader<U>>(
        &self,
        pa: &mut Pass,
        rd: &RwData<Rd>,
        f: impl FnOnce(&mut RefBytes, &mut Rd) -> Ret,
    ) -> Ret {
        // SAFETY: Since the other type is not a File, we can safely borrow
        // both.
        unsafe {
            self.0
                .write_unsafe(|file| rd.write(pa, |rd| f(&mut file.text.ref_bytes(), rd)))
        }
    }

    /// Gets another [`Reader`], if it was added to this [`File`]
    pub fn get_reader<Rd: Reader<U>>(&self, pa: &Pass) -> Option<RwData<Rd>> {
        let file = self.0.acquire(pa);
        file.get_reader()
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: RwData::write
    /// [`write_as`]: RwData::write_as
    /// [`read`]: RwData::read
    /// [`has_changed`]: RwData::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::ui::Widget
    pub fn has_changed(&self) -> bool {
        self.0.has_changed()
    }
}
