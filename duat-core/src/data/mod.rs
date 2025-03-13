//! Data types that are meant to be shared and read across Duat.
//!
//! The data types revolve around the [`RwLock`] struct from
//! [`parking_lot`], and are adapters that may block the mutation of
//! the inner data, for the purpose of making it available for reading
//! to any extension on Duat.
//!
//! The first data type is [`RwData`], which is a read and write
//! wrapper over information. It should mostly not be shared, being
//! used instead to write information while making sure that no other
//! part of the code is still reading it. The second data type is
//! [`RoData`], or read only data. It is derived from the first
//! one, and cannot mutate the inner data.
//!
//! This type is used internally in order to keep track of state, like
//! widgets, input methods, etc.
//!
//! One of the main external uses for this macro is in creating
//! automatically update [`StatusLine`] fields.
//!
//! [`File`]: crate::widgets::File
//! [`Text`]: crate::text::Text
//! [`StatusLine`]: crate::widgets::StatusLine
use std::{
    any::TypeId,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use parking_lot::{Mutex, MutexGuard};

// This module is here mostly because context structs like CurFile and
// CurWidget need access to RwData internals in order to function
// properly.
/// Data about the state of Duat
pub mod context;

/// A read write shared reference to data
pub struct RwData<T: ?Sized + 'static> {
    // The `Box` is to allow for downcasting.
    pub(super) data: Arc<Mutex<T>>,
    pub(super) cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize,
    pub(super) type_id: TypeId,
}

impl<T> RwData<T> {
    /// Returns a new instance of a [`RwData`], assuming that it is
    /// sized.
    ///
    /// This has to be sized because of some Rust limitations, as you
    /// can`t really pass an unsized argument to a function (for now).
    /// If you're looking to store unsized types (`dyn Trait`,
    /// `\[Type\]`, etc) on an [`RwData`], see
    /// [`RwData::new_unsized`].
    pub fn new(data: T) -> Self {
        Self {
            data: Arc::new(Mutex::new(data)),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1),
            type_id: TypeId::of::<T>(),
        }
    }
}

impl<T: ?Sized> RwData<T> {
    /// Returns a new instance of [`RwData`], assuming that it is
    /// unsized.
    ///
    /// This method is only required if you're dealing with types that
    /// may not be [`Sized`] (`dyn Trait`, `[Type]`, etc). If the type
    /// in question is sized, use [`RwData::new`] instead.
    pub fn new_unsized<SizedT: 'static>(data: Arc<Mutex<T>>) -> Self {
        // It's 1 here so that any `RoState`s created from this will have
        // `has_changed()` return `true` at least once, by copying the
        // second value - 1.
        Self {
            data,
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1),
            type_id: TypeId::of::<SizedT>(),
        }
    }

    /// Blocking reference to the information
    ///
    /// Also makes it so that [`has_changed`] returns `false`.
    ///
    /// # Examples
    ///
    /// Since this is a blocking read, the thread will hault while the
    /// data is being written to:
    /// ```rust
    /// # use std::{thread, time::{Duration, Instant}};
    /// # use duat_core::data::{RoData, RwData};
    /// let read_write_data = RwData::new("☹️");
    /// let read_only_data = RoData::from(&read_write_data);
    /// let instant = Instant::now();
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut read_write = read_write_data.write();
    ///         // Supposedly long computations.
    ///         thread::sleep(Duration::from_millis(150));
    ///         *read_write = "☺️";
    ///     });
    ///
    ///     // Just making sure that the read happens slightly after the write.
    ///     thread::sleep(Duration::from_millis(10));
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
    /// # use std::{thread, time::{Duration, Instant}};
    /// # use duat_core::data::{RoData, RwData};
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
    ///     // Just making sure that this read happens slightly after the last one.
    ///     thread::sleep(Duration::from_millis(1));
    ///
    ///     let read_only = read_only_data.read();
    ///     let time_elapsed = Instant::now().duration_since(instant);
    ///     assert!(time_elapsed < Duration::from_millis(100));
    /// });
    /// ```
    /// [`has_changed`]: Self::has_changed
    pub fn read(&self) -> ReadDataGuard<'_, T> {
        let cur_state = self.cur_state.load(Ordering::Acquire);
        self.read_state.store(cur_state, Ordering::Release);
        ReadDataGuard(self.data.lock())
    }

    /// Non blocking reference to the information
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
    /// # use duat_core::data::RwData;
    /// let new_data = RwData::new("hello 👋");
    ///
    /// let mut blocking_write = new_data.write();
    /// *blocking_write = "bye 👋";
    ///
    /// let try_read = new_data.try_read();
    /// assert!(matches!(try_read, None));
    /// ```
    ///
    /// [`has_changed`]: Self::has_changed
    /// [`read`]: Self::read
    pub fn try_read(&self) -> Option<ReadDataGuard<'_, T>> {
        self.data.try_lock().map(|guard| {
            let cur_state = self.cur_state.load(Ordering::Acquire);
            self.read_state.store(cur_state, Ordering::Release);
            ReadDataGuard(guard)
        })
    }

    /// Whether or not it has changed since it was last read
    ///
    /// A "change" is defined as any time the methods [`write`],
    /// [`mutate`], [`try_write`], or [`try_mutate`], are called on an
    /// [`RwData`]. Once `has_changed` is called, the data will be
    /// considered unchanged since the last `has_changed` call, for
    /// that specific instance of a [`Data`].
    ///
    /// When first creating a [`Data`] type, `has_changed`
    /// will return `false`;
    ///
    /// # Examples
    /// ```rust
    /// use duat_core::data::{RoData, RwData};
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
    pub fn has_changed(&self) -> bool {
        let cur_state = self.cur_state.load(Ordering::Relaxed);
        let read_state = self.read_state.swap(cur_state, Ordering::Relaxed);
        cur_state > read_state
    }

    /// A function that returns true if the data has changed
    ///
    /// This funnction is specifically very useful when creating
    /// [`Widget`]s, as those require a `checker_fn`, and this can
    /// give you one of those with relatively minimal effort.
    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static + use<T> {
        let RwData { cur_state, read_state, .. } = self.clone();
        move || {
            let cur_state = cur_state.load(Ordering::Relaxed);
            let read_state = read_state.swap(cur_state, Ordering::Relaxed);
            cur_state > read_state
        }
    }

    /// Returns `true` if both [`Data`]s point to the same
    /// data.
    ///
    /// # Examples
    /// ```rust
    /// # use duat_core::data::{RwData};
    /// let data_1 = RwData::new(false);
    /// let data_1_clone = data_1.clone();
    ///
    /// let data_2 = RwData::new(true);
    ///
    /// assert!(data_1.ptr_eq(&data_1_clone));
    /// assert!(!data_1.ptr_eq(&data_2));
    /// ```
    pub fn ptr_eq<U: ?Sized>(&self, other: &RwData<U>) -> bool {
        Arc::as_ptr(&self.cur_state) == Arc::as_ptr(&other.cur_state)
    }

    /// Blocking mutable reference to the information
    ///
    /// Also makes it so that [`has_changed`] returns true for
    /// `self` or any of its clones, be they [`RoData`] or
    /// [`RwData`].
    ///
    /// # Safety
    ///
    /// Since this is a blocking function, you should be careful about
    /// the prevention of deadlocks, one of the few unsafe aspects of
    /// code that Rust doesn't prevent.
    ///
    /// As an example, this code will deadlock indefinitely:
    /// ```no_run
    /// # use std::{mem, thread, time::Duration};
    /// # use duat_core::data::RwData;
    /// let data_1 = RwData::new('😟');
    /// let data_2 = RwData::new('😭');
    ///
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut data_1 = data_1.write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_2 = data_2.write();
    ///         mem::swap(&mut data_1, &mut data_2);
    ///     });
    ///
    ///     scope.spawn(|| {
    ///         let mut data_2 = data_2.write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_1 = data_1.write();
    ///         mem::swap(&mut data_1, &mut data_2);
    ///     });
    /// });
    /// ```
    /// In general, try not to juggle multiple `&mut` handles to
    /// [`RwData`]s. A good way of doing that is the
    /// [`RwData::mutate`] method, which makes it very explicit when a
    /// specific handle will be dropped, mitigating the possibility of
    /// deadlocks.
    ///
    ///
    /// [`has_changed`]: Data::has_changed
    pub fn write(&self) -> WriteDataGuard<T> {
        let guard = self.data.lock();
        WriteDataGuard { guard, cur_state: &self.cur_state }
    }

    /// Non Blocking mutable reference to the information
    ///
    /// Also makes it so that [`has_changed`] returns true for
    /// `self` or any of its clones, be they [`RoData`] or
    /// [`RwData`].
    ///
    /// # Safety
    ///
    /// Unlike [`RwData::write`], this method cannot cause deadlocks,
    /// returning an [`Err`] instead.
    /// ```
    /// # use std::{mem, thread, time::Duration};
    /// # use duat_core::data::RwData;
    /// let data_1 = RwData::new('😀');
    /// let data_2 = RwData::new('😁');
    ///
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut data_1 = data_1.try_write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_2 = data_2.try_write();
    ///         if let (Some(mut data_1), Some(mut data_2)) = (data_1, data_2) {
    ///             mem::swap(&mut data_1, &mut data_2);
    ///         }
    ///     });
    ///
    ///     scope.spawn(|| {
    ///         let mut data_2 = data_2.try_write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_1 = data_1.try_write();
    ///         if let (Some(mut data_1), Some(mut data_2)) = (data_1, data_2) {
    ///             mem::swap(&mut data_1, &mut data_2);
    ///         }
    ///     });
    /// });
    ///
    /// // Two swaps will happen.
    /// assert_eq!(*data_1.read(), '😀');
    /// assert_eq!(*data_2.read(), '😁');
    /// ```
    /// The downside is that you may not want it to fail ever, in
    /// which case, you should probably use [`RwData::write`].
    ///
    /// [`has_changed`]: Data::has_changed
    pub fn try_write(&self) -> Option<WriteDataGuard<'_, T>> {
        self.data
            .try_lock()
            .map(|guard| WriteDataGuard { guard, cur_state: &self.cur_state })
    }

    /// Blocking reference to the information
    ///
    /// Unlike [`read`], *DOES NOT* make it so
    /// [`has_changed`] returns `false`.
    ///
    /// This method should only be used in very specific
    /// circumstances, such as when multiple owners have nested
    /// [`RwData`]s, thus referencing the same inner [`RwData`], in
    /// a way that reading from one point would interfere in the
    /// update detection of the other point.
    ///
    /// [`read`]: Self::read,
    /// [`has_changed`]: Self::has_changed
    pub(crate) fn raw_read(&self) -> ReadDataGuard<'_, T> {
        ReadDataGuard(self.data.lock())
    }

    /// Returns `true` if the data is of the concrete type `T`
    ///
    /// # Examples
    ///
    /// You may want this method if you're storing a list of
    /// [`RwData<dyn Trait>`], and want to know, at runtime, what type
    /// each element is:
    /// ```rust
    /// # use std::{any::Any, fmt::Display, sync::Arc};
    /// # use duat_core::data::{RwData, RwLock};
    /// let list: [RwData<dyn Display>; 3] = [
    ///     RwData::new_unsized::<String>(Arc::new(RwLock::new(String::from(
    ///         "I can show you the world",
    ///     )))),
    ///     RwData::new_unsized::<String>(Arc::new(RwLock::new(String::from(
    ///         "Shining, shimmering, splendid",
    ///     )))),
    ///     RwData::new_unsized::<char>(Arc::new(RwLock::new('🧞'))),
    /// ];
    ///
    /// assert!(list[0].data_is::<String>());
    /// assert!(list[1].data_is::<String>());
    /// assert!(list[2].data_is::<char>());
    /// ```
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn data_is<U: ?Sized + 'static>(&self) -> bool {
        self.type_id == TypeId::of::<U>()
    }

    /// Tries to downcast to a concrete type
    ///
    /// # Examples
    ///
    /// You may want this method if you're storing a list of
    /// [`RwData<dyn Trait>`], and want to know, at runtime, what type
    /// each element is:
    /// ```rust
    /// # use std::{fmt::Display, sync::Arc};
    /// # use duat_core::data::{RwData, RwLock};
    /// let list: [RwData<dyn Display>; 3] = [
    ///     RwData::new_unsized::<String>(Arc::new(RwLock::new(String::from(
    ///         "I can show you the world",
    ///     )))),
    ///     RwData::new_unsized::<String>(Arc::new(RwLock::new(String::from(
    ///         "Shining, shimmering, splendid",
    ///     )))),
    ///     RwData::new_unsized::<char>(Arc::new(RwLock::new('🧞'))),
    /// ];
    ///
    /// let maybe_char = list[2].clone().try_downcast::<char>();
    /// assert!(maybe_char.is_some());
    /// *maybe_char.unwrap().write() = '👳';
    ///
    /// let maybe_string = list[0].clone().try_downcast::<char>();
    /// assert!(maybe_string.is_none());
    /// ```
    /// If you don't need to write to the data, consider using
    /// [`RwData::inspect_as`]. If you only need to know if the type
    /// matches, consider using [`RwData::data_is`].
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn try_downcast<U: 'static>(&self) -> Option<RwData<U>> {
        if self.data_is::<U>() {
            let Self { data, cur_state, read_state, .. } = self.clone();
            let ptr = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(ptr.cast()) };
            Some(RwData {
                data,
                cur_state,
                read_state,
                type_id: self.type_id,
            })
        } else {
            None
        }
    }

    /// Blocking inspection of the inner data
    ///
    /// # Examples
    ///
    /// You may want this method if you're storing a list of
    /// [`RwData<dyn Trait>`], and want to know, at runtime, what type
    /// each element is:
    /// ```rust
    /// # use std::{any::Any, fmt::Display, sync::Arc};
    /// # use duat_core::data::{RwData, RwLock};
    /// let list: [RwData<dyn Display>; 3] = [
    ///     RwData::new_unsized::<String>(Arc::new(RwLock::new(String::from(
    ///         "I can show you the world",
    ///     )))),
    ///     RwData::new_unsized::<String>(Arc::new(RwLock::new(String::from(
    ///         "Shining, shimmering, splendid",
    ///     )))),
    ///     RwData::new_unsized::<char>(Arc::new(RwLock::new('🧞'))),
    /// ];
    ///
    /// assert!(matches!(
    ///     list[2].inspect_as::<char, usize>(|char| char.len_utf8()),
    ///     Some(4)
    /// ));
    /// assert!(matches!(
    ///     list[1].inspect_as::<char, char>(|char| char.to_ascii_uppercase()),
    ///     None
    /// ));
    /// ```
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn inspect_as<U: 'static, R>(&self, f: impl FnOnce(&U) -> R) -> Option<R> {
        (self.data_is::<U>()).then(|| {
            let ptr = Arc::as_ptr(&self.data);
            let cast = unsafe { ptr.cast::<Mutex<U>>().as_ref().unwrap() };

            self.read_state
                .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);

            f(&cast.lock())
        })
    }

    pub fn mutate_as<U: 'static, R>(&self, f: impl FnOnce(&mut U) -> R) -> Option<R> {
        (self.data_is::<U>()).then(|| {
            let ptr = Arc::as_ptr(&self.data);
            let cast = unsafe { ptr.cast::<Mutex<U>>().as_ref().unwrap() };

            self.read_state
                .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);

            let mut guard = cast.lock();
            f(&mut guard)
        })
    }

    pub(crate) fn raw_write(&self) -> MutexGuard<'_, T> {
        self.data.lock()
    }
}

impl<T: ?Sized + std::fmt::Debug> std::fmt::Debug for RwData<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.data.lock(), f)
    }
}

impl<T: ?Sized> Clone for RwData<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1),
            type_id: self.type_id,
        }
    }
}

impl<T: Default> Default for RwData<T> {
    fn default() -> Self {
        Self {
            data: Arc::new(Mutex::new(T::default())),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1),
            type_id: TypeId::of::<T>(),
        }
    }
}

pub struct ReadDataGuard<'a, T: ?Sized>(MutexGuard<'a, T>);

impl<T: ?Sized> std::ops::Deref for ReadDataGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct WriteDataGuard<'a, T: ?Sized> {
    guard: MutexGuard<'a, T>,
    cur_state: &'a Arc<AtomicUsize>,
}

impl<T: ?Sized> std::ops::Deref for WriteDataGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}

impl<T: ?Sized> std::ops::DerefMut for WriteDataGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}

impl<T: ?Sized> Drop for WriteDataGuard<'_, T> {
    fn drop(&mut self) {
        self.cur_state.fetch_add(1, Ordering::Release);
    }
}

pub struct DataMap<I: ?Sized + Send + Sync + 'static, O> {
    data: RwData<I>,
    f: Box<dyn FnMut() -> O + Send + Sync>,
}

impl<I: ?Sized + Send + Sync + 'static, O> DataMap<I, O> {
    pub fn fns(
        self,
    ) -> (
        Box<dyn FnMut() -> O + Send + Sync>,
        Box<dyn Fn() -> bool + Send + Sync>,
    ) {
        let checker = Box::new(move || self.data.has_changed());
        (self.f, checker)
    }
}

impl<I: ?Sized + Send + Sync + 'static, O: 'static> DataMap<I, O> {
    pub fn map<O2>(mut self, mut f: impl FnMut(O) -> O2 + Send + Sync + 'static) -> DataMap<I, O2> {
        DataMap {
            data: self.data,
            f: Box::new(move || f((self.f)())),
        }
    }
}

impl<I: ?Sized + Send + Sync + 'static> RwData<I> {
    pub fn map<O>(&self, mut f: impl FnMut(&I) -> O + Send + Sync + 'static) -> DataMap<I, O> {
        let data = self.clone();
        let f = move || f(&*data.read());
        DataMap { data: self.clone(), f: Box::new(f) }
    }
}
