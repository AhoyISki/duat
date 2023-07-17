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
use std::marker::PhantomData;
#[cfg(any(not(feature = "deadlock-detection"), not(feature = "testing-features")))]
use std::sync::{atomic::Ordering, Arc, TryLockError, TryLockResult};

#[cfg(all(feature = "deadlock-detection", feature = "testing-features"))]
use no_deadlocks::{RwLock, RwLockReadGuard, RwLockWriteGuard};
pub use ro::{RoData, RoNestedData};
pub use rw::RwData;

mod ro;
mod rw;

/// This trait is meant for very specific uses where we want to access
/// `T` without notifying that we did so.
pub(crate) trait RawReadableData<T>: private::DataHolder<T>
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
/// both of these data types implement [`Send`], [`Sync`], and
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
pub trait ReadableData<T>: private::DataHolder<T>
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
    ///     // Just making sure that the read happens slighly after the write.
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
    ///     // Just making sure that this read happens slighly after the last one.
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
        let _ = self.data();
        let cur_state = self.cur_state().load(Ordering::Relaxed);
        let read_state = self.read_state().swap(cur_state, Ordering::Relaxed);
        cur_state > read_state
    }

    /// Returns `true` if both [`ReadableData<T>`]s point to the same
    /// data.
    ///
    /// # Examples
    /// ```rust
    /// # use parsec_core::data::{ReadableData, RwData};
    /// let data_1 = RwData::new(false);
    /// let data_1_clone = data_1.clone();
    ///
    /// let data_2 = RwData::new(true);
    ///
    /// assert!(data_1.ptr_eq(&data_1_clone));
    /// assert!(!data_1.ptr_eq(&data_2));
    /// ```
    fn ptr_eq(&self, other: &impl ReadableData<T>) -> bool {
        Arc::ptr_eq(self.cur_state(), other.cur_state())
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
/// # use parsec_core::data::{AsAny, RwData};
/// trait Emotion: AsAny {}
///
/// struct Happy;
/// impl AsAny for Happy {
///     fn as_any(&self) -> &dyn std::any::Any {
///         self
///     }
/// }
///
/// struct Sad;
/// impl AsAny for Sad {
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
pub trait AsAny {
    fn as_any(&self) -> &dyn std::any::Any;
}

/// An error signifying a failure in the casting of data.
///
/// This failure can happen in either [`RoData::try_downcast`] or
/// [`RwData::try_downcast`], when the type of the inner data is not
/// the correct one.
pub struct DataCastErr<Holder, Data, Cast>(Holder, PhantomData<Data>, PhantomData<Cast>)
where
    Holder: private::DataHolder<Data>,
    Data: ?Sized,
    Cast: ?Sized;

impl<Holder, Data, Cast> std::fmt::Debug for DataCastErr<Holder, Data, Cast>
where
    Holder: private::DataHolder<Data>,
    Data: ?Sized,
    Cast: ?Sized
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let orig = std::any::type_name::<Data>();
        let cast = std::any::type_name::<Cast>();
        write!(f, "{orig} cannot be coerced to {cast}.")
    }
}

pub enum DataRetrievalErr<Holder, T>
where
    Holder: private::DataHolder<T>,
    T: ?Sized
{
    WriteBlocked(PhantomData<(Holder, T)>),
    ReadBlocked(PhantomData<(Holder, T)>),
    NestedReadBlocked(PhantomData<(Holder, T)>)
}

impl<Holder, T> std::fmt::Debug for DataRetrievalErr<Holder, T>
where
    Holder: private::DataHolder<T>,
    T: ?Sized
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let holder = std::any::type_name::<Holder>();
        match self {
            DataRetrievalErr::WriteBlocked(_) => {
                write!(f, "The {holder} could not be written to at this moment.")
            }
            DataRetrievalErr::ReadBlocked(_) => {
                write!(f, "The {holder} could not be read this moment.")
            }
            DataRetrievalErr::NestedReadBlocked(_) => {
                let t = std::any::type_name::<T>();
                write!(
                    f,
                    "In the RonestedData<{t}>, the initial RoData<{holder}> could not be read at \
                     the moment."
                )
            }
        }
    }
}

mod private {
    use std::sync::{atomic::AtomicUsize, Arc, TryLockResult};

    /// Private trait for the [`RwData`] and [`RoData`] structs.
    ///
    /// [`RwData`]: super::RwData
    /// [`RoData`]: super::RoData
    pub trait DataHolder<T>
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
