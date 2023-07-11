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
    fmt::{Debug, Display},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, TryLockError, TryLockResult
    }
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{RwLock, RwLockReadGuard, RwLockWriteGuard};

mod private {
    use std::sync::{atomic::AtomicUsize, Arc, TryLockResult};

    pub trait Data<T>
    where
        T: ?Sized
    {
        type Deref<'a>: std::ops::Deref<Target = T>
        where
            Self: 'a;

        fn data(&self) -> Self::Deref<'_>;

        fn try_data(&self) -> TryLockResult<Self::Deref<'_>>;

        fn cur_state(&self) -> &Arc<AtomicUsize>;

        fn read_state(&self) -> &AtomicUsize;
    }
}

/// A trait that's primarily used for casting widgets to a concrete
/// types.
pub trait DownCastableData {
    fn as_any(&self) -> &dyn std::any::Any;
}

pub(crate) trait RawReadableData<T>: private::Data<T>
where
    T: ?Sized
{
    /// Blocking reference to the information.
    ///
    /// Unlike [`read()`][Self::read()], *DOES NOT* make it so
    /// [`has_changed()`][Self::has_changed()] returns `false`.
    ///
    /// This method should only be used in very specific
    /// circumstances, such as when multiple owners have nested
    /// [`RwData`]s, thus referencing the same inner [`RwData<T>`], in
    /// a way that reading from one point would interfere in the
    /// update detection of the other point.
    fn raw_read(&self) -> Self::Deref<'_> {
        self.data()
    }

    /// Non Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    ///
    /// This method is only used in the [`RoNestedData<T>`] struct, as
    /// it is important to keep the `read_state` of the inner
    /// [`RoData<T>`] intact while other clones of the
    /// [`RoNestedData<T>`] read it.
    fn raw_try_read(&self) -> TryLockResult<Self::Deref<'_>> {
        self.try_data()
    }
}

pub trait ReadableData<T>: private::Data<T>
where
    T: ?Sized
{
    /// Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    fn read(&self) -> Self::Deref<'_> {
        let cur_state = self.cur_state().load(Ordering::Acquire);
        self.read_state().store(cur_state, Ordering::Release);
        self.data()
    }

    /// Blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    fn inspect<U>(&self, f: impl FnOnce(&T) -> U) -> U {
        let cur_state = self.cur_state().load(Ordering::Acquire);
        self.read_state().store(cur_state, Ordering::Release);
        f(&self.data())
    }

    /// Non blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    fn try_read(&self) -> TryLockResult<Self::Deref<'_>> {
        self.try_data().inspect(|_| {
            let cur_state = self.cur_state().load(Ordering::Acquire);
            self.read_state().store(cur_state, Ordering::Release);
        })
    }

    /// Non blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// `false`.
    fn try_inspect<U>(&self, f: impl FnOnce(&T) -> U) -> Result<U, TryLockError<Self::Deref<'_>>> {
        self.try_data().map(|data| {
            let cur_state = self.cur_state().load(Ordering::Acquire);
            self.read_state().store(cur_state, Ordering::Release);
            f(&data)
        })
    }

    /// Wether or not it has changed since it was last read.
    fn has_changed(&self) -> bool {
        let cur_state = self.cur_state().load(Ordering::Relaxed);
        let read_state = self.read_state().swap(cur_state, Ordering::Relaxed);
        cur_state > read_state
    }

    /// Returns [`true`] if both [`RwData<T>`]s point to the same
    /// data.
    fn ptr_eq(&self, other: &impl ReadableData<T>) -> bool {
        Arc::ptr_eq(self.cur_state(), other.cur_state())
    }
}

/// A read-write reference to information, and can tell readers if
/// said information has changed.
pub struct RwData<T>
where
    T: ?Sized
{
    data: Arc<RwLock<T>>,
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize
}

impl<T> RwData<T> {
    /// Returns a new instance of a [`RwData<T>`], assuming that it is
    /// sized.
    pub fn new(data: T) -> Self {
        RwData {
            data: Arc::new(RwLock::new(data)),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
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
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
        }
    }

    /// Blocking mutable reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// on it or any of its clones returns `true`.
    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.data.write().unwrap()
    }

    /// Non Blocking mutable reference to the information.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// on it or any of its clones returns `true`.
    pub fn try_write(&self) -> TryLockResult<RwLockWriteGuard<T>> {
        self.data.try_write().map(|guard| {
            self.cur_state.fetch_add(1, Ordering::Relaxed);
            guard
        })
    }

    /// Blocking mutation of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// on it or any of its clones returns `true`.
    pub fn mutate<B>(&self, f: impl FnOnce(&mut T) -> B) -> B {
        f(&mut self.write())
    }

    /// Non blocking mutation of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// on it or any of its clones returns `true`.
    pub fn try_mutate<B>(
        &self, f: impl FnOnce(&mut T) -> B
    ) -> Result<B, TryLockError<RwLockWriteGuard<T>>> {
        let res = self.try_write();
        res.map(|mut data| f(&mut *data))
    }
}

impl<T> RwData<T>
where
    T: ?Sized + DownCastableData
{
    /// Tries to downcast to a concrete type.
    pub fn try_downcast<U>(self) -> Result<RwData<U>, DataCastErr<RwData<T>, T, U>>
    where
        U: 'static
    {
        let RwData {
            data,
            cur_state,
            read_state
        } = self;
        if (&*data.read().unwrap()).as_any().is::<U>() {
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RwData {
                data,
                cur_state,
                read_state
            })
        } else {
            Err(DataCastErr(
                RwData {
                    data,
                    cur_state,
                    read_state
                },
                std::marker::PhantomData::default(),
                std::marker::PhantomData::default()
            ))
        }
    }

    pub fn inspect_as<U, V>(&self, f: impl FnOnce(&U) -> V) -> Option<V>
    where
        U: 'static
    {
        self.inspect(|data| data.as_any().downcast_ref::<U>().map(|data| f(data)))
    }

    /// Wether or not the inner data is of type `U`.
    pub fn data_is<U>(&self) -> bool
    where
        U: 'static
    {
        let RwData { data, .. } = &self;
        data.read().unwrap().as_any().is::<U>()
    }
}

impl<T> private::Data<T> for RwData<T>
where
    T: ?Sized
{
    type Deref<'a> = RwLockReadGuard<'a, T>
    where
        Self: 'a;

    fn data(&self) -> Self::Deref<'_> {
        self.data.read().unwrap()
    }

    fn try_data(&self) -> TryLockResult<Self::Deref<'_>> {
        self.data.try_read()
    }

    fn cur_state(&self) -> &Arc<AtomicUsize> {
        &self.cur_state
    }

    fn read_state(&self) -> &AtomicUsize {
        &self.read_state
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
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1)
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
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
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

impl<T> ReadableData<T> for RwData<T> where T: ?Sized {}
impl<T> RawReadableData<T> for RwData<T> where T: ?Sized {}
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
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize
}

impl<T> RoData<T> {
    /// Returns a new instance of a [`RoData<T>`], assuming that it is
    /// sized.
    ///
    /// Since this is read only, you would usually call this
    /// function only if `T` has some sort of internal mutability, and
    /// you want only those internally mutable parts to be modifiable.
    ///
    /// If you don't need that internal mutability, I would just
    /// recommend using an [`Arc<T>`] from the `std`.
    pub fn new(data: T) -> Self {
        RoData {
            data: Arc::new(RwLock::new(data)),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized + 'static
{
    /// Returns a new instance of [`RoData<T>`], assuming that it is
    /// unsized.
    ///
    /// Since this is read only, you would usually call this
    /// function only if `T` has some sort of internal mutability, and
    /// you want only those internally mutable parts to be modifiable.
    ///
    /// If you don't need that internal mutability, I would just
    /// recommend using an [`Arc<T>`] from the `std`.
    pub fn new_unsized(data: Arc<RwLock<T>>) -> Self {
        // It's 1 here so that any `RoState`s created from this will have
        // `has_changed()` return `true` at least once, by copying the
        // second value - 1.
        RoData {
            data,
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized + DownCastableData
{
    /// Tries to downcast to a concrete type.
    pub fn try_downcast<U>(self) -> Result<RoData<U>, DataCastErr<RoData<T>, T, U>>
    where
        U: 'static
    {
        let RoData {
            data,
            cur_state,
            read_state
        } = self;
        if (&*data.read().unwrap()).as_any().is::<U>() {
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RoData {
                data,
                cur_state,
                read_state
            })
        } else {
            Err(DataCastErr(
                RoData {
                    data,
                    cur_state,
                    read_state
                },
                std::marker::PhantomData::default(),
                std::marker::PhantomData::default()
            ))
        }
    }

    pub fn data_is<U>(&self) -> bool
    where
        U: 'static
    {
        let RoData { data, .. } = &self;
        data.read().unwrap().as_any().is::<U>()
    }
}

impl<T> private::Data<T> for RoData<T>
where
    T: ?Sized
{
    type Deref<'a> = RwLockReadGuard<'a, T> where Self: 'a;

    fn data(&self) -> Self::Deref<'_> {
        self.data.read().unwrap()
    }

    fn try_data(&self) -> TryLockResult<Self::Deref<'_>> {
        self.data.try_read()
    }

    fn cur_state(&self) -> &Arc<AtomicUsize> {
        &self.cur_state
    }

    fn read_state(&self) -> &AtomicUsize {
        &self.read_state
    }
}

impl<T> ReadableData<T> for RoData<T> where T: ?Sized {}
impl<T> RawReadableData<T> for RoData<T> where T: ?Sized {}

impl<T> Default for RoData<T>
where
    T: ?Sized + Default
{
    fn default() -> Self {
        Self {
            data: Arc::new(RwLock::new(T::default())),
            cur_state: Arc::new(AtomicUsize::new(0)),
            read_state: AtomicUsize::new(0)
        }
    }
}

impl<T> Debug for RoData<T>
where
    T: ?Sized + Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&*self.data.read().unwrap(), f)
    }
}

impl<T> From<&RwData<T>> for RoData<T>
where
    T: ?Sized
{
    fn from(value: &RwData<T>) -> Self {
        RoData {
            data: value.data.clone(),
            cur_state: value.cur_state.clone(),
            read_state: AtomicUsize::new(value.cur_state.load(Ordering::Relaxed) - 1)
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
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed))
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

pub struct DataCastErr<D, T, U>(D, std::marker::PhantomData<T>, std::marker::PhantomData<U>)
where
    D: private::Data<T>,
    T: ?Sized,
    U: ?Sized;

impl<D, T, U> Debug for DataCastErr<D, T, U>
where
    D: private::Data<T>,
    T: ?Sized,
    U: ?Sized
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let orig = stringify!(self.0);
        let cast = std::any::type_name::<U>();
        f.write_fmt(format_args!("{orig}'s data cannot be coerced to {cast}."))
    }
}

/// A nested [`RoData<RoData<T>>`], synced across all readers.
///
/// An [`RoData<RoData<T>>`] is useful for allowing multiple readers
/// to the same data, while also allowing for the changing of the
/// inner [`RoData<T>`], which essentially makes this struct capable
/// of altering what data it is pointing to.
///
/// The problem with a raw [`RoData<RoData<T>>`] is that reading the
/// inner [`RoData<T>`] will automatically mean that every other owner
/// of the [`RoData<RoData<T>>`] will think that the inner data has
/// never been updated, which is false.
///
/// That's the whole point of [`RoNestedData<T>`], to allow for
/// multiple readers to catch up, not only to the inner data, but also
/// changes to the pointer itself.
pub struct RoNestedData<T>
where
    T: ?Sized
{
    data: RoData<RoData<T>>,
    read_state: AtomicUsize
}

impl<T> RoNestedData<T>
where
    T: ?Sized + 'static
{
    /// Returns a new instance of [`RoNestedData<T>`]
    ///
    /// Note that the inner [`RoData<T>`] in the [`RoData<RoData<T>>`]
    /// will never be swappable by another [`RoData<T>`] which is the
    /// whole point of this struct.
    pub fn new(data: RoData<T>) -> Self {
        let read_state = AtomicUsize::new(data.cur_state.load(Ordering::Relaxed));
        Self {
            data: RoData::new(data),
            read_state
        }
    }

    /// Blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    pub fn inspect<B>(&self, f: impl FnOnce(&T) -> B) -> B {
        let data = self.data.read();
        let cur_state = data.cur_state.load(Ordering::Relaxed);
        self.read_state.store(cur_state, Ordering::Relaxed);
        let inner_data = data.raw_read();
        f(&inner_data)
    }

    /// Non blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// `false`.
    pub fn try_inspect<B>(&self, f: impl FnOnce(&T) -> B) -> Result<B, ()> {
        self.data.try_read().map_err(|_| ()).and_then(|data| {
            data.raw_try_read().map_err(|_| ()).map(|inner_data| {
                let cur_state = data.cur_state.load(Ordering::Relaxed);
                self.read_state.store(cur_state, Ordering::Relaxed);
                f(&inner_data)
            })
        })
    }

    /// Wether or not it has changed since it was last read.
    ///
    /// Will return true if either the inner `T` in [`RoData<T>`] has
    /// changed (by mutation from some third party), or if the
    /// [`RoData<T>`] in the [`RoData<RoData<T>>`] has been swapped
    /// out for another (by changing the data that is being pointed
    /// to).
    pub fn has_changed(&self) -> bool {
        let mut has_changed = self.data.has_changed();
        let data = self.data.read();
        let cur_state = data.cur_state.load(Ordering::Relaxed);
        has_changed |= cur_state > self.read_state.load(Ordering::Relaxed);
        self.read_state.store(cur_state, Ordering::Relaxed);
        has_changed
    }
}

impl<T> Clone for RoNestedData<T>
where
    T: ?Sized + 'static
{
    fn clone(&self) -> Self {
        RoNestedData {
            data: self.data.clone(),
            read_state: AtomicUsize::new(self.data.raw_read().cur_state.load(Ordering::Relaxed))
        }
    }
}

impl<T> From<&RwData<RoData<T>>> for RoNestedData<T>
where
    T: ?Sized + 'static
{
    fn from(value: &RwData<RoData<T>>) -> Self {
        RoNestedData {
            data: RoData::from(value),
            read_state: AtomicUsize::new(value.raw_read().cur_state.load(Ordering::Relaxed))
        }
    }
}
