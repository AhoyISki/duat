//! Data types that are meant to be shared and read across Parsec.
//!
//! The data types revolve around the [`RwLock<T>`] struct from std,
//! and are adapters that may block the mutation of the inner data,
//! gor the purpose of making it available for reading to any
//! extension on Parsec.
//!
//! The first data type is [`RwData<T>`], which is a read and write
//! wrapper over information. It should mostly not be shared, being
//! used instead to write information while making sure that no other
//! part of the code is still reading it. The second data type is
//! [`RoData<T>`], or read only data. It is derived from the first
//! one, and cannot mutate the inner data.
//!
//! The most common usecase for these data types is in the
//! [`FileWidget<U>`][crate::widgets::FileWidget], where many readers
//! can peer into the [`Text<U>`][crate::text::Text] or other useful
//! information, such as the printed lines, cursors, etc.
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::{
    any::Any,
    error::Error,
    fmt::{Debug, Display},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, TryLockResult
    }
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{RwLock, RwLockReadGuard, RwLockWriteGuard};

/// A trait that's primarily used for casting widgets to a concrete
/// types.
pub trait DownCastableData {
    fn as_any(&self) -> &dyn Any;
}

/// A read-write reference to information, and can tell readers if
/// said information has changed.
pub struct RwData<T>
where
    T: ?Sized
{
    data: Arc<RwLock<T>>,
    updated_state: Arc<AtomicUsize>,
    last_read: AtomicUsize
}

impl<T> RwData<T> {
    /// Returns a new instance of a [`RwData<T>`], assuming that it is
    /// sized.
    pub fn new(data: T) -> Self {
        RwData {
            data: Arc::new(RwLock::new(data)),
            updated_state: Arc::new(AtomicUsize::new(1)),
            last_read: AtomicUsize::new(1)
        }
    }
}

impl<T> RwData<T>
where
    T: ?Sized + 'static
{
    /// Returns a new instance of [`RwData<T>`], assuming that it is
    /// unsized.
    pub fn new_unsized(data: Arc<RwLock<T>>) -> Self {
        // It's 1 here so that any `RoState`s created from this will have
        // `has_changed()` return `true` at least once, by copying the
        // second value - 1.
        RwData {
            data,
            updated_state: Arc::new(AtomicUsize::new(1)),
            last_read: AtomicUsize::new(1)
        }
    }

    /// Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    pub fn read(&self) -> RwLockReadGuard<T> {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        self.last_read.store(updated_state, Ordering::Relaxed);

        self.data.read().unwrap()
    }

    /// Non blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<T>> {
        self.data.try_read().map(|guard| {
            let updated_state = self.updated_state.load(Ordering::Relaxed);
            self.last_read.store(updated_state, Ordering::Relaxed);

            guard
        })
    }

    /// Blocking mutable reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// on it or any of its clones returns `true`.
    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.updated_state.fetch_add(1, Ordering::Relaxed);
        self.data.write().unwrap()
    }

    /// Non Blocking mutable reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// on it or any of its clones returns `true`.
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
    T: ?Sized + Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.data.read().unwrap(), f)
    }
}

impl<T> Clone for RwData<T>
where
    T: ?Sized
{
    fn clone(&self) -> Self {
        RwData {
            data: self.data.clone(),
            updated_state: self.updated_state.clone(),
            last_read: AtomicUsize::new(self.updated_state.load(Ordering::Relaxed) - 1)
        }
    }
}

impl<T> Default for RwData<T>
where
    T: Default
{
    fn default() -> Self {
        RwData {
            data: Arc::new(RwLock::new(T::default())),
            updated_state: Arc::new(AtomicUsize::new(1)),
            last_read: AtomicUsize::new(1)
        }
    }
}

impl<T> Display for RwData<T>
where
    T: Display + 'static
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.read().fmt(f)
    }
}

unsafe impl<T> Sync for RwData<T> where T: ?Sized {}

/// A read-only reference to information.
///
/// Can only be created by cloning the [`Arc<RwLock<T>>`] from a
/// [`RwData<T>`], or through cloning.
pub struct RoData<T>
where
    T: ?Sized
{
    data: Arc<RwLock<T>>,
    updated_state: Arc<AtomicUsize>,
    last_read: AtomicUsize
}

impl<T> RoData<T>
where
    T: ?Sized + Any + 'static
{
    /// Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    pub fn read(&self) -> RwLockReadGuard<T> {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        self.last_read.store(updated_state, Ordering::Relaxed);

        self.data.read().unwrap()
    }

    /// Non Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<T>> {
        self.data.try_read().map(|mutex_guard| {
            let updated_state = self.updated_state.load(Ordering::Relaxed);
            self.last_read.store(updated_state, Ordering::Relaxed);

            mutex_guard
        })
    }

    /// Wether or not it has changed since it was last read.
    pub fn has_changed(&self) -> bool {
        let updated_state = self.updated_state.load(Ordering::Relaxed);
        let last_read = self.last_read.swap(updated_state, Ordering::Relaxed);
        updated_state > last_read
    }
}

impl<T> RoData<T>
where
    T: ?Sized + DownCastableData
{
    /// Tries to downcast to a concrete type.
    pub fn try_downcast<U>(self) -> Result<RoData<U>, RoDataCastError<T>>
    where
        U: 'static
    {
        let RoData {
            data,
            updated_state,
            last_read: last_read_state
        } = self;
        if (&*data.read().unwrap()).as_any().is::<U>() {
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RoData {
                data,
                updated_state,
                last_read: last_read_state
            })
        } else {
            Err(RoDataCastError(RoData {
                data,
                updated_state,
                last_read: last_read_state
            }))
        }
    }
}

impl<T> From<&RwData<T>> for RoData<T>
where
    T: ?Sized
{
    fn from(value: &RwData<T>) -> Self {
        RoData {
            data: value.data.clone(),
            updated_state: value.updated_state.clone(),
            last_read: AtomicUsize::new(value.updated_state.load(Ordering::Relaxed) - 1)
        }
    }
}

// NOTE: Each `RoState` of a given state will have its own internal
// update counter.
impl<T> Clone for RoData<T>
where
    T: ?Sized
{
    fn clone(&self) -> Self {
        RoData {
            data: self.data.clone(),
            updated_state: self.updated_state.clone(),
            last_read: AtomicUsize::new(self.updated_state.load(Ordering::Relaxed))
        }
    }
}

impl<T> Display for RoData<T>
where
    T: Display + 'static
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.read().fmt(f)
    }
}

/// An error when trying to cast [`RoData<T>`] to [`RoData<S>`] when
/// `T != S`.
pub struct RoDataCastError<T>(RoData<T>)
where
    T: ?Sized;

impl<T> RoDataCastError<T>
where
    T: ?Sized
{
    /// Retrieve the [`RoData<T>`] back to its original state, giving
    /// up on the casting.
    pub fn retrieve(self) -> RoData<T> {
        self.0
    }
}

impl<T> Debug for RoDataCastError<T>
where
    T: ?Sized
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Downcasting failed!")
    }
}

impl<T> Display for RoDataCastError<T>
where
    T: ?Sized
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Downcasting failed!")
    }
}

impl<T> Error for RoDataCastError<T> where T: ?Sized {}
