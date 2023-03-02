#[cfg(not(feature = "deadlock-detection"))]
use std::sync::{Mutex, MutexGuard};
use std::{
    any::{Any, TypeId},
    error::Error,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    sync::{Arc, TryLockError},
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{Mutex, MutexGuard};

use crate::{
    tags::form::FormPalette,
    text::TextLine,
    ui::{Area, Label, Ui},
};

/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    /// Wrap at the end of the screen.
    Width,
    /// Wrap at a given width.
    Capped(u16),
    /// Wrap at the end of the screen, on word boundaries.
    Word,
    /// Don't wrap at all.
    #[default]
    NoWrap,
}

// Pretty much only exists because i wanted one of these with usize as its builtin type.
#[derive(Debug, Copy, Clone)]
pub struct ScrollOff {
    pub d_y: usize,
    pub d_x: usize,
}

impl Default for ScrollOff {
    fn default() -> Self {
        ScrollOff { d_y: 5, d_x: 3 }
    }
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Debug, Clone)]
pub enum TabPlaces {
    /// The same lenght for every tab.
    Regular(usize),
    /// Varying lenghts for different tabs.
    Varied(Vec<usize>),
}

impl TabPlaces {
    /// Returns the amount of spaces that a tabulation would produce in the given column.
    pub fn spaces_on_col(&self, x: usize) -> usize {
        match self {
            TabPlaces::Regular(step) => step - (x % step),
            TabPlaces::Varied(cols) => cols.iter().find(|&s| *s > x).expect("Not enough tabs") - x,
        }
    }
}

impl Default for TabPlaces {
    fn default() -> Self {
        TabPlaces::Regular(4)
    }
}

/// Wheter to show the new line or not.
#[derive(Default, Debug, Clone, Copy)]
pub enum ShowNewLine {
    #[default]
    /// Never show new lines.
    Never,
    /// Show the given character on every new line.
    Always(char),
    /// Show the given character only when there is whitespace at end of the line.
    AfterSpace(char),
}

impl ShowNewLine {
    pub fn get_new_line_ch(&self, last_ch: char) -> char {
        match self {
            ShowNewLine::Never => ' ',
            ShowNewLine::Always(ch) => *ch,
            ShowNewLine::AfterSpace(ch) => {
                if last_ch.is_whitespace() {
                    *ch
                } else {
                    ' '
                }
            }
        }
    }
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Some standard parsec options.
#[derive(Debug, Default, Clone)]
pub struct Config {
    /// How to wrap the file.
    pub wrap_method: WrapMethod,
    /// The distance between the cursor and the edges of the screen when scrolling.
    pub scrolloff: ScrollOff,
    /// How to indent.
    pub tab_places: TabPlaces,
    /// Wether to indent wrapped lines or not.
    pub wrap_indent: bool,
    /// Wether to convert tabs to spaces.
    pub tabs_as_spaces: bool,
    /// Wether (and how) to show new lines.
    pub show_new_line: ShowNewLine,
    /// The palette of forms that will be used.
    pub palette: FormPalette,
}

impl Config {
    pub fn usable_indent<U>(&self, line: &TextLine, label: &U::Label) -> usize
    where
        U: Ui,
    {
        let indent = line.indent(self);
        if self.wrap_indent && indent < label.area().width() { indent } else { 0 }
    }
}

pub trait DownCastableData: Any {
    fn as_any(&self) -> &dyn Any;
}

/// A read-write reference to information, and can tell readers if said information has changed.
pub struct RwData<T>
where
    T: ?Sized,
{
    data: Arc<Mutex<T>>,
    updated_state: Arc<Mutex<usize>>,
    last_read_state: Mutex<usize>,
}

impl<T> RwData<T> {
    pub fn new(data: T) -> Self {
        RwData {
            data: Arc::new(Mutex::new(data)),
            updated_state: Arc::new(Mutex::new(1)),
            last_read_state: Mutex::new(1),
        }
    }
}

impl<T> RwData<T>
where
    T: ?Sized + 'static,
{
    /// Returns a new instance of [RwData<T>].
    pub fn new_unsized(data: Arc<Mutex<T>>) -> Self {
        // It's 1 here so that any `RoState`s created from this will have `has_changed()` return
        // `true` at least once, by copying the second value - 1.
        RwData { data, updated_state: Arc::new(Mutex::new(1)), last_read_state: Mutex::new(1) }
    }

    /// Reads the information.
    ///
    /// Also makes it so that [has_changed()] returns false.
    pub fn read(&self) -> RwDataReadGuard<T> {
        let updated_version = self.updated_state.lock().unwrap();
        let mut last_read_state = self.last_read_state.lock().unwrap();

        if *updated_version > *last_read_state {
            *last_read_state = *updated_version;
        }

        RwDataReadGuard(self.data.lock().unwrap())
    }

    /// Tries to read the data immediately and returns a `Result`.
    pub fn try_read(&self) -> Result<RwDataReadGuard<T>, TryLockError<MutexGuard<T>>> {
        let updated_version = self.updated_state.lock().unwrap();
        let mut last_read_state = self.last_read_state.lock().unwrap();

        if *updated_version > *last_read_state {
            *last_read_state = *updated_version;
        }

        self.data.try_lock().map(|mutex_guard| RwDataReadGuard(mutex_guard))
    }

    /// Returns a writeable reference to the state.
    ///
    /// Also makes it so that `has_changed()` on it or any of its clones returns `true`.
    pub fn write(&self) -> RwDataWriteGuard<T> {
        *self.updated_state.lock().unwrap() += 1;
        RwDataWriteGuard(self.data.lock().unwrap())
    }

    /// Wether or not it has changed since it was last read.
    pub fn has_changed(&self) -> bool {
        let last_version = self.updated_state.lock().unwrap();
        let mut current_version = self.last_read_state.lock().unwrap();
        let has_changed = *last_version > *current_version;
        *current_version = *last_version;

        has_changed
    }

    pub fn try_downcast<U>(self) -> Result<RwData<U>, RwDataCastError<T>>
    where
        U: 'static,
    {
        let RwData { data, updated_state, last_read_state } = self;
        let data = Arc::into_raw(data);
        if data.type_id() == TypeId::of::<Mutex<U>>() {
            let data = unsafe { Arc::from_raw(data.cast::<Mutex<U>>()) };
            Ok(RwData { data, updated_state, last_read_state })
        } else {
            let data = unsafe { Arc::from_raw(data) };
            Err(RwDataCastError { rw_data: RwData { data, updated_state, last_read_state } })
        }
    }
}

impl<T> Debug for RwData<T>
where
    T: ?Sized + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.data.lock().unwrap(), f)
    }
}

impl<T> Clone for RwData<T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        RwData {
            data: self.data.clone(),
            updated_state: self.updated_state.clone(),
            last_read_state: Mutex::new(*self.updated_state.lock().unwrap() - 1),
        }
    }
}

impl<D> Default for RwData<D>
where
    D: Default,
{
    fn default() -> Self {
        RwData {
            data: Arc::new(Mutex::new(D::default())),
            updated_state: Arc::new(Mutex::new(1)),
            last_read_state: Mutex::new(1),
        }
    }
}

unsafe impl<T> Sync for RwData<T> where T: ?Sized {}

/// A read-only reference to information.
pub struct RoData<T>
where
    T: ?Sized,
{
    data: Arc<Mutex<T>>,
    updated_state: Arc<Mutex<usize>>,
    last_read_state: Mutex<usize>,
}

impl<T> RoData<T>
where
    T: ?Sized + Any,
{
    /// Reads the information.
    ///
    /// Also makes it so that `has_changed()` returns false.
    pub fn read(&mut self) -> RwDataReadGuard<T> {
        let updated_version = self.updated_state.lock().unwrap();
        let mut last_read_state = self.last_read_state.lock().unwrap();

        if *updated_version > *last_read_state {
            *last_read_state = *updated_version;
        }

        RwDataReadGuard(self.data.lock().unwrap())
    }

    /// Checks if the state within has changed.
    ///
    /// If you have called `has_changed()` or `read()`, without any changes, it will return false.
    pub fn has_changed(&self) -> bool {
        let updated_version = self.updated_state.lock().unwrap();
        let mut current_version = self.last_read_state.lock().unwrap();

        if *updated_version > *current_version {
            *current_version = *updated_version;

            true
        } else {
            false
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized + DownCastableData,
{
    pub fn try_downcast<U>(self) -> Result<RoData<U>, RoDataCastError<T>>
    where
        U: 'static,
    {
        let RoData { data, updated_state, last_read_state } = self;
        if (&*data.lock().unwrap()).as_any().is::<U>() {
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<Mutex<U>>()) };
            Ok(RoData { data, updated_state, last_read_state })
        } else {
            Err(RoDataCastError { ro_data: RoData { data, updated_state, last_read_state } })
        }
    }
}

impl<T> From<&RwData<T>> for RoData<T>
where
    T: ?Sized,
{
    fn from(state: &RwData<T>) -> Self {
        RoData {
            data: state.data.clone(),
            updated_state: state.updated_state.clone(),
            last_read_state: Mutex::new(*state.updated_state.lock().unwrap() - 1),
        }
    }
}

// NOTE: Each `RoState` of a given state will have its own internal update counter.
impl<T> Clone for RoData<T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        RoData {
            data: self.data.clone(),
            updated_state: self.updated_state.clone(),
            last_read_state: Mutex::new(*self.last_read_state.lock().unwrap()),
        }
    }
}

pub struct RwDataReadGuard<'a, T>(MutexGuard<'a, T>)
where
    T: ?Sized;

impl<'a, T> Deref for RwDataReadGuard<'a, T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct RwDataWriteGuard<'a, T>(MutexGuard<'a, T>)
where
    T: ?Sized;

impl<'a, T> Deref for RwDataWriteGuard<'a, T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for RwDataWriteGuard<'a, T>
where
    T: ?Sized,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

pub struct RwDataCastError<T>
where
    T: ?Sized,
{
    rw_data: RwData<T>,
}

impl<T> RwDataCastError<T>
where
    T: ?Sized,
{
    pub fn retrieve(self) -> RwData<T> {
        self.rw_data
    }
}

impl<T> Debug for RwDataCastError<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Downcasting failed!")
    }
}

impl<T> Display for RwDataCastError<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Downcasting failed!")
    }
}

impl<T> Error for RwDataCastError<T> where T: ?Sized {}

pub struct RoDataCastError<T>
where
    T: ?Sized,
{
    ro_data: RoData<T>,
}

impl<T> RoDataCastError<T>
where
    T: ?Sized,
{
    pub fn retrieve(self) -> RoData<T> {
        self.ro_data
    }
}

impl<T> Debug for RoDataCastError<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Downcasting failed!")
    }
}

impl<T> Display for RoDataCastError<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Downcasting failed!")
    }
}

impl<T> Error for RoDataCastError<T> where T: ?Sized {}
