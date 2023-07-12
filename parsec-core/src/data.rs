//! Data types that are meant to be shared and read across Parsec.
//!
//! The data types revolve around the [`RwLock<T>`] struct from std,
//! and are adapters that may block the mutation of the inner data,
//! for the purpose of making it available for reading to any
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
//! [`FileWidget<U>`], where many readers
//! can peer into the [`Text`] or other useful
//! information, such as the printed lines, cursors, etc.
//!
//! [`FileWidget<U>`]: crate::FileWidget<U>
//! [`Text`]: crate::text::Text
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

/// This trait is meant for very specific uses where we want to access
/// `T` without notifying that we did so.
pub(crate) trait RawReadableData<T>: private::Data<T> + Send + Sync
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

/// Data that can be read, but can't necessarily be written to.
///
/// This trait encompasses 2 data types: [`RwData`] and [`RoData`],
/// which can share information in a selective way, by allowing or
/// disallowing mutation of data:
/// ```compile_fail
/// # use parsec_core::data::{ReadableData, RwData, RoData};
/// let read_write_data = RwData::new(10);
/// let shared_read_only = RoData::from(&read_write_data);
///
/// // let mut read_write = shared_read_only.write(); // Doesn`t exist!
/// let mut read_only = shared_read_only.read();
/// *read_only = 9;
/// ```
/// oth of these data types implement [`Send`], [`Sync`], and
/// [`Clone`], which means they are safe to clone and share across
/// threads (with the caveat of deadlocks, which Rust still doesn't
/// prevent!). One very prominent use for these data types is the
/// [`LineNumbers<U>`] widget, which contains an
/// [`RoData<FileWidget<U>>`] inside it, in order to retrieve
/// information about which lines of the file are currently being
/// shown.
///
/// While this trait is implemented for both types, only [`RwData`]
/// has access to write methods, such as [`write`] or [`mutate`].
///
/// [`LineNumbers<U>`]: crate::widgets::LineNumbers
/// [`RoData<FileWidget<U>>`]: crate::widgets::FileWidget
/// [`write`]: RwData::write
/// [`mutate`]: RwData::mutate
pub trait ReadableData<T>: private::Data<T>
where
    T: ?Sized
{
    /// Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed`] returns `false`.
    ///
    /// # Examples
    ///
    /// Since this is a blocking read, the thread will hault while the
    /// data is being written to:
    /// ```rust
    /// # use std::{
    /// #     thread,
    /// #     time::{Duration, Instant}
    /// # };
    /// # use parsec_core::data::{ReadableData, RoData, RwData};
    /// let read_write_data = RwData::new("☹️");
    /// let read_only_data = RoData::from(&read_write_data);
    /// let instant = Instant::now();
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut read_write = read_write_data.write();
    ///         // Supposedly long computations.
    ///         thread::sleep(Duration::from_millis(100));
    ///         *read_write = "☺️";
    ///     });
    ///
    /// 	// Just making sure that the read happens slighly after the write.
    ///     thread::sleep(Duration::from_millis(1));
    ///
    ///     let read_only = read_only_data.read();
    ///     let time_elapsed = Instant::now().duration_since(instant);
    ///     assert!(time_elapsed >= Duration::from_millis(100));
    ///     assert!(*read_only == "☺️");
    /// });
    /// ```
    /// Note that other reads will **NOT** block reading in this way,
    /// only writes:
    /// ```rust
    /// # use std::{
    /// #     thread,
    /// #     time::{Duration, Instant}
    /// # };
    /// # use parsec_core::data::{ReadableData, RoData, RwData};
    /// let read_write_data = RwData::new("☹️");
    /// let read_only_data = RoData::from(&read_write_data);
    /// let instant = Instant::now();
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let read_only = read_write_data.read();
    ///         // The thread hangs, but reading is still possible.
    ///         thread::sleep(Duration::from_millis(100));
    ///     });
    ///
    /// 	// Just making sure that this read happens slighly after the last one.
    ///     thread::sleep(Duration::from_millis(1));
    ///
    ///     let read_only = read_only_data.read();
    ///     let time_elapsed = Instant::now().duration_since(instant);
    ///     assert!(time_elapsed < Duration::from_millis(100));
    /// });
    /// ```
    /// [`has_changed`]: Self::has_changed
    fn read(&self) -> Self::Deref<'_> {
        let cur_state = self.cur_state().load(Ordering::Acquire);
        self.read_state().store(cur_state, Ordering::Release);
        self.data()
    }

    /// Blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed`] returns `false`.
    ///
    /// # Examples
    ///
    /// This method is useful if you want to scope the reading, or
    /// need to drop the reference quickly, so it can be written to.
    ///
    /// You can do this:
    /// ```rust
    /// # use parsec_core::data::{ReadableData, RoData, RwData};
    /// # fn add_to_count(count: &mut usize) {}
    /// # fn new_count() -> usize {
    /// #     0
    /// # }
    /// let count = RwData::new(31);
    /// let count_reader = RoData::from(&count);
    ///
    /// // The read write counterpart to `inspect`.
    /// count.mutate(|count| {
    ///     *count += 5;
    ///     add_to_count(count)
    /// });
    ///
    /// count_reader.inspect(|count| { /* reading operations */ });
    ///
    /// *count.write() = new_count();
    /// ```
    /// Instead of this:
    /// ```rust
    /// # use parsec_core::data::{ReadableData, RoData, RwData};
    /// # fn add_to_count(count: &mut usize) {}
    /// # fn new_count() -> usize {
    /// #     0
    /// # }
    /// let count = RwData::new(31);
    /// let count_reader = RoData::from(&count);
    ///
    /// // The read write counterpart to `inspect`.
    /// let mut count_write = count.write();
    /// *count_write += 5;
    /// add_to_count(&mut *count_write);
    /// drop(count_write);
    ///
    /// let count_read = count_reader.read();
    /// // reading operations
    /// drop(count_read);
    ///
    /// *count.write() = new_count();
    /// ```
    /// Or this:
    /// ```rust
    /// # use parsec_core::data::{ReadableData, RoData, RwData};
    /// # fn add_to_count(count: &mut usize) {}
    /// # fn new_count() -> usize {
    /// #     0
    /// # }
    /// let count = RwData::new(31);
    /// let count_reader = RoData::from(&count);
    ///
    /// // The read write counterpart to `inspect`.
    /// {
    ///     let mut count = count.write();
    ///     *count += 5;
    ///     add_to_count(&mut count)
    /// }
    ///
    /// {
    ///     let count = count.read();
    ///     // reading operations
    /// }
    ///
    /// *count.write() = new_count();
    /// ```
    /// [`has_changed`]: Self::has_changed
    fn inspect<U>(&self, f: impl FnOnce(&T) -> U) -> U {
        let cur_state = self.cur_state().load(Ordering::Acquire);
        self.read_state().store(cur_state, Ordering::Release);
        f(&self.data())
    }

    /// Non blocking reference to the information.
    ///
    /// If successful, also makes it so that [`has_changed`] returns
    /// `false`.
    ///
    /// # Examples
    ///
    /// Unlike [`read`], can fail to return a reference to the
    /// underlying data:
    /// ```rust
    /// # use std::{sync::TryLockError};
    /// # use parsec_core::data::{ReadableData, RwData};
    /// let new_data = RwData::new("hello 👋");
    ///
    /// let mut blocking_write = new_data.write();
    /// *blocking_write = "bye 👋";
    ///
    /// let try_read = new_data.try_read();
    /// assert!(matches!(try_read, Err(TryLockError::WouldBlock)));
    /// ```
    ///
    /// [`has_changed`]: Self::has_changed
    /// [`read`]: Self::read
    fn try_read(&self) -> TryLockResult<Self::Deref<'_>> {
        self.try_data().inspect(|_| {
            let cur_state = self.cur_state().load(Ordering::Acquire);
            self.read_state().store(cur_state, Ordering::Release);
        })
    }

    /// Non blocking inspection of the inner data.
    ///
    /// If successful, also makes it so that [`has_changed`] returns
    /// `false`.
    ///
    /// # Examples
    ///
    /// Unlike [`inspect`], can fail to return a reference to the
    /// underlying data:
    /// ```rust
    /// # use std::sync::TryLockError;
    /// # use parsec_core::data::{ReadableData, RwData};
    /// let new_data = RwData::new("hello 👋");
    ///
    /// let try_inspect = new_data.mutate(|blocking_mutate| {
    ///     *blocking_mutate = "bye 👋";
    ///
    ///     new_data.try_inspect(|try_inspect| *try_inspect == "bye 👋")
    /// });
    ///
    /// assert!(matches!(try_inspect, Err(TryLockError::WouldBlock)));
    /// ```
    ///
    /// [`has_changed`]: Self::has_changed
    /// [`inspect`]: Self::inspect
    fn try_inspect<U>(&self, f: impl FnOnce(&T) -> U) -> Result<U, TryLockError<Self::Deref<'_>>> {
        self.try_data().map(|data| {
            let cur_state = self.cur_state().load(Ordering::Acquire);
            self.read_state().store(cur_state, Ordering::Release);
            f(&data)
        })
    }

    /// Wether or not it has changed since it was last read.
    ///
    /// A "change" is defined as any time the methods [`write`],
    /// [`mutate`], [`try_write`], or [`try_mutate`], are called on an
    /// [`RwData`]. Once `has_changed` is called, the data will be
    /// considered unchanged since the last `has_changed` call, for
    /// that specific instance of a [`ReadableData`].
    ///
    /// When first creating a [`ReadableData`] type, `has_changed`
    /// will return `false`;
    ///
    /// # Examples
    /// ```rust
    /// use parsec_core::data::{ReadableData, RoData, RwData};
    /// let new_data = RwData::new("Initial text");
    /// assert!(!new_data.has_changed());
    ///
    /// let first_reader = RoData::from(&new_data);
    ///
    /// *new_data.write() = "Final text";
    ///
    /// let second_reader = RoData::from(&new_data);
    ///
    /// assert!(first_reader.has_changed());
    /// assert!(!second_reader.has_changed());
    /// ```
    ///
    /// [`write`]: RwData::write
    /// [`mutate`]: RwData::mutate
    /// [`try_write`]: RwData::try_write
    /// [`try_mutate`]: RwData::try_mutate
    fn has_changed(&self) -> bool {
        let cur_state = self.cur_state().load(Ordering::Relaxed);
        let read_state = self.read_state().swap(cur_state, Ordering::Relaxed);
        cur_state > read_state
    }

    /// Returns `true` if both [`ReadableData<T>`]s point to the same
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

impl<T> ReadableData<T> for RwData<T> where T: ?Sized {}
impl<T> RawReadableData<T> for RwData<T> where T: ?Sized {}

unsafe impl<T> Send for RwData<T> where T: ?Sized {}
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
            read_state: AtomicUsize::new(value.cur_state.load(Ordering::Relaxed))
        }
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

unsafe impl<T> Sync for RoData<T> where T: ?Sized {}
unsafe impl<T> Send for RoData<T> where T: ?Sized {}

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

/// A trait that's primarily used for casting [`Widget<U>`]s to a
/// concrete types.
///
/// # Examples
///
/// This can be useful if you have a [`Vec<RwData<dyn Trait>>`], and
/// want to get the concrete type of a specific item:
///
/// ```rust
/// # use std::sync::{Arc, RwLock};
/// # use parsec_core::data::{DownCastableData, RwData};
/// trait Emotion: DownCastableData {}
///
/// struct Happy;
/// impl DownCastableData for Happy {
///     fn as_any(&self) -> &dyn std::any::Any {
///         self
///     }
/// }
///
/// struct Sad;
/// impl DownCastableData for Sad {
///     fn as_any(&self) -> &dyn std::any::Any {
///         self
///     }
/// }
///
/// impl Emotion for Happy {}
/// impl Emotion for Sad {}
///
/// fn is_happy(potentially_boolean: &RwData<dyn Emotion>) -> bool {
///     potentially_boolean.data_is::<Happy>()
/// }
///
/// let happy = Arc::new(RwLock::new(Happy));
/// let sad = Arc::new(RwLock::new(Sad));
/// assert!(is_happy(&RwData::new_unsized(happy)));
/// assert!(!is_happy(&RwData::new_unsized(sad)));
/// ```
///
/// [`Widget<U>`]: crate::widgets::Widget
/// [`Vec<dyn Trait>`]: Vec
pub trait DownCastableData {
    fn as_any(&self) -> &dyn std::any::Any;
}

mod private {
    use std::sync::{atomic::AtomicUsize, Arc, TryLockResult};

    /// Private trait for the [`RwData`] and [`RoData`] structs.
    ///
    /// [`RwData`]: super::RwData
    /// [`RoData`]: super::RoData
    pub trait Data<T>
    where
        T: ?Sized
    {
        type Deref<'a>: std::ops::Deref<Target = T>
        where
            Self: 'a;

        /// The data, usually an [`Arc`]
        fn data(&self) -> Self::Deref<'_>;

        /// Attempt to get the data, that is usually an [`Arc`]
        fn try_data(&self) -> TryLockResult<Self::Deref<'_>>;

        /// The most up to date state of the data.
        fn cur_state(&self) -> &Arc<AtomicUsize>;

        /// The last read state of the data.
        fn read_state(&self) -> &AtomicUsize;
    }
}
