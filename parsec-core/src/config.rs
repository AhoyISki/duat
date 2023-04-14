//! Configuration options for parsec.

#[cfg(not(feature = "deadlock-detection"))]
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::{
    any::Any,
    error::Error,
    fmt::{Debug, Display},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, TryLockResult,
    },
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::tags::form::FormPalette;

/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    Width,
    Capped(usize),
    Word,
    #[default]
    NoWrap,
}

// Pretty much only exists because i wanted one of these with
// usizewrite its builtin type.
#[derive(Debug, Copy, Clone)]
pub struct ScrollOff {
    pub y_gap: usize,
    pub x_gap: usize,
}

impl Default for ScrollOff {
    fn default() -> Self {
        ScrollOff { y_gap: 3, x_gap: 3 }
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
    /// Returns the amount of spaces that a tabulation would produce
    /// in the given column.
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
    AlwaysAs(char),
    /// Show the given character only when there is whitespace at end
    /// of the line.
    AfterSpaceAs(char),
}

impl ShowNewLine {
    pub fn get_new_line_ch(&self, last_ch: char) -> char {
        match self {
            ShowNewLine::Never => ' ',
            ShowNewLine::AlwaysAs(ch) => *ch,
            ShowNewLine::AfterSpaceAs(ch) => {
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
    /// The distance between the cursor and the edges of the screen
    /// when scrolling.
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

pub trait DownCastableData {
    fn as_any(&self) -> &dyn Any;
}

pub trait DataHolder<T>
where
    T: Sized,
{
}

/// A read-write reference to information, and can tell readers if
/// said information has changed.
pub struct RwData<T>
where
    T: ?Sized,
{
    data: Arc<RwLock<T>>,
    updated_state: Arc<AtomicUsize>,
    last_read: AtomicUsize,
}

impl<T> RwData<T> {
    pub fn new(data: T) -> Self {
        RwData {
            data: Arc::new(RwLock::new(data)),
            updated_state: Arc::new(AtomicUsize::new(1)),
            last_read: AtomicUsize::new(1),
        }
    }
}

impl<T> RwData<T>
where
    T: ?Sized + 'static,
{
    /// Returns a new instance of [RwData<T>].
    pub fn new_unsized(data: Arc<RwLock<T>>) -> Self {
        // It's 1 here so that any `RoState`s created from this will have
        // `has_changed()` return `true` at least once, by copying the
        // second value - 1.
        RwData {
            data,
            updated_state: Arc::new(AtomicUsize::new(1)),
            last_read: AtomicUsize::new(1),
        }
    }

    /// Reads the information.
    ///
    /// Also makes it so that [has_changed()] returns false.
    pub fn read(&self) -> RwLockReadGuard<T> {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        self.last_read.store(updated_state, Ordering::Relaxed);

        self.data.read().unwrap()
    }

    /// Tries to read the data immediately and returns a `Result`.
    pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<T>> {
        self.data.try_read().map(|guard| {
            let updated_state = self.updated_state.load(Ordering::Relaxed);
            self.last_read.store(updated_state, Ordering::Relaxed);

            guard
        })
    }

    /// Returns a writeable reference to the state.
    ///
    /// Also makes it so that `has_changed()` on it or any of its
    /// clones returns `true`.
    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.updated_state.fetch_add(1, Ordering::Relaxed);
        self.data.write().unwrap()
    }

    /// Tries to return a writeable reference to the state.
    ///
    /// Also makes it so that `has_changed()` on it or any of its
    /// clones returns `true`.
    pub fn try_write(&self) -> TryLockResult<RwLockWriteGuard<T>> {
        self.data.try_write().map(|guard| {
            self.updated_state.fetch_add(1, Ordering::Relaxed);
            guard
        })
    }

    /// Wether or not it has changed since it was last read.
    pub fn has_changed(&self) -> bool {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        let last_read = self.last_read.swap(updated_state, Ordering::Relaxed);
        updated_state > last_read
    }
}

impl<T> Debug for RwData<T>
where
    T: ?Sized + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.data.read().unwrap(), f)
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
            last_read: AtomicUsize::new(self.updated_state.load(Ordering::Relaxed) - 1),
        }
    }
}

impl<T> Default for RwData<T>
where
    T: Default,
{
    fn default() -> Self {
        RwData {
            data: Arc::new(RwLock::new(T::default())),
            updated_state: Arc::new(AtomicUsize::new(1)),
            last_read: AtomicUsize::new(1),
        }
    }
}

unsafe impl<T> Sync for RwData<T> where T: ?Sized {}

/// A read-only reference to information.
pub struct RoData<T>
where
    T: ?Sized,
{
    data: Arc<RwLock<T>>,
    updated_state: Arc<AtomicUsize>,
    last_read: AtomicUsize,
}

impl<T> RoData<T>
where
    T: ?Sized + Any + 'static,
{
    /// Reads the information.
    ///
    /// Also makes it so that `has_changed()` returns false.
    pub fn read(&self) -> RwLockReadGuard<T> {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        self.last_read.store(updated_state, Ordering::Relaxed);

        self.data.read().unwrap()
    }

    /// Tries to read the data immediately and returns a `Result`.
    pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<T>> {
        self.data.try_read().map(|mutex_guard| {
            let updated_state = self.updated_state.load(Ordering::Relaxed);
            self.last_read.store(updated_state, Ordering::Relaxed);

            mutex_guard
        })
    }

    /// Checks if the state within has changed.
    ///
    /// If you have called `has_changed()` or `read()`, without any
    /// changes, it will return false.
    pub fn has_changed(&self) -> bool {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        let last_read = self.last_read.swap(updated_state, Ordering::Relaxed);
        updated_state > last_read
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
        let RoData {
            data,
            updated_state,
            last_read: last_read_state,
        } = self;
        if (&*data.read().unwrap()).as_any().is::<U>() {
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RoData {
                data,
                updated_state,
                last_read: last_read_state,
            })
        } else {
            Err(RoDataCastError {
                ro_data: RoData {
                    data,
                    updated_state,
                    last_read: last_read_state,
                },
            })
        }
    }
}

impl<T> From<&RwData<T>> for RoData<T>
where
    T: ?Sized,
{
    fn from(value: &RwData<T>) -> Self {
        RoData {
            data: value.data.clone(),
            updated_state: value.updated_state.clone(),
            last_read: AtomicUsize::new(value.updated_state.load(Ordering::Relaxed) - 1),
        }
    }
}

// NOTE: Each `RoState` of a given state will have its own internal
// update counter.
impl<T> Clone for RoData<T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        RoData {
            data: self.data.clone(),
            updated_state: self.updated_state.clone(),
            last_read: AtomicUsize::new(self.updated_state.load(Ordering::Relaxed)),
        }
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
