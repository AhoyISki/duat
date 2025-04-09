//! Duat's way of sharing and updating state
//!
//! This module consists primarily of the [`RwData`] struct, which
//! holds state that can be [read] or [written to]. When it is
//! modified, other holders of a clone of [`RwData`] will know that
//! the data within has been modified.
//!
//! This is used in many places, for example, [`Widget`]s can read
//! from [`File`]s, and Duat can know when a [`File`] has been
//! altered, so these [`Widget`]s may be [updated] automatically.
//!
//! Another struct from this module is [`DataMap`]. This is
//! essentially a mapping for a [`RwData`]. It is created via
//! [`RwData::map`], and both it and [`RwData`] can be very useful in,
//! for example, a [`StatusLine`], since it will be updated
//! automatically whenever the [`RwData`] is altered.
//!
//! One thing to note however is that [`RwData`] is a type mostly used
//! by Duat itself, inside APIs like those of [`context`], so if you
//! want information to be shared across threads and you don't need
//! others to be able to reach it, you should prefer using more
//! conventional locking mechanisms, like those of [`parking_lot`],
//! which are exported by Duat.
//!
//! [read]: RwData::read
//! [written to]: RwData::write
//! [`Widget`]: crate::widgets::Widget
//! [`File`]: crate::widgets::File
//! [updated]: crate::widgets::Widget::update
//! [`Text`]: crate::text::Text
//! [`StatusLine`]: crate::widgets::StatusLine
//! [`context`]: crate::context
use std::{
    any::TypeId,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use parking_lot::{Mutex, MutexGuard};

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
    ///
    /// ```rust
    /// # use std::{thread, time::{Duration, Instant}};
    /// # use duat_core::data::RwData;
    /// let data = RwData::new("☹️");
    /// let data_clone = data.clone();
    /// let instant = Instant::now();
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut read_write = data.write();
    ///         // Supposedly long computations.
    ///         thread::sleep(Duration::from_millis(150));
    ///         *read_write = "☺️";
    ///     });
    ///
    ///     // Just making sure that the read happens slightly after the write.
    ///     thread::sleep(Duration::from_millis(10));
    ///
    ///     let read_only = data_clone.read();
    ///     let time_elapsed = Instant::now().duration_since(instant);
    ///     assert!(time_elapsed >= Duration::from_millis(100));
    ///     assert!(*read_only == "☺️");
    /// });
    /// ```
    ///
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
    ///
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
    /// A "change" is defined as any time the methods [`write`]
    /// or [`try_write`] are called on an [`RwData`]. Once
    /// `has_changed` is called, the data will be considered
    /// unchanged since the last `has_changed` call, for
    /// that specific instance of a [`RwData`].
    ///
    /// When first creating a [`RwData`] `has_changed` will return
    /// `false`, but clones of that [`RwData`] will have `has_changed`
    /// initially return `true`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use duat_core::data::RwData;
    /// let data = RwData::new("Initial text");
    /// assert!(!data.has_changed());
    ///
    /// *data.write() = "Almost final text";
    ///
    /// let data_clone1 = data.clone();
    ///
    /// assert!(data_clone1.has_changed());
    ///
    /// *data.write() = "Final text";
    ///
    /// assert!(data_clone1.has_changed());
    /// assert!(!data_clone1.has_changed());
    /// ```
    ///
    /// [`write`]: RwData::write
    /// [`try_write`]: RwData::try_write
    pub fn has_changed(&self) -> bool {
        let cur_state = self.cur_state.load(Ordering::Acquire);
        let read_state = self.read_state.swap(cur_state, Ordering::Acquire);
        cur_state > read_state
    }

    /// A function that returns true if the data has changed
    ///
    /// This is essentially a faster way of writing
    ///
    /// ```rust
    /// # use duat_core::data::RwData;
    /// let my_data = RwData::new(42);
    /// let checker = {
    ///     let my_data = my_data.clone();
    ///     move || my_data.has_changed()
    /// };
    /// ```
    ///
    /// [`Widget`]: crate::widgets::Widget
    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static + use<T> {
        let cur_state = self.cur_state.clone();
        let read_state = AtomicUsize::new(self.read_state.load(Ordering::Relaxed));
        move || {
            let cur_state = cur_state.load(Ordering::Acquire);
            let read_state = read_state.swap(cur_state, Ordering::Acquire);
            cur_state > read_state
        }
    }

    /// Returns `true` if both [`RwData`]s point to the same data
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
        Arc::ptr_eq(&self.cur_state, &other.cur_state)
    }

    /// Blocking exclusive reference to the information
    ///
    /// Also makes it so that [`has_changed`] returns true for any of
    /// the clones made from `self`, but **NOT** `self`.
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
    ///
    /// [`has_changed`]: RwData::has_changed
    pub fn write(&self) -> WriteDataGuard<T> {
        let guard = self.data.lock();
        WriteDataGuard {
            guard,
            cur_state: &self.cur_state,
            read_state: &self.read_state,
        }
    }

    /// Non Blocking mutable reference to the information
    ///
    /// Also makes it so that [`has_changed`] returns true for any of
    /// the clones made from `self`, but **NOT** `self`.
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
    /// [`has_changed`]: RwData::has_changed
    pub fn try_write(&self) -> Option<WriteDataGuard<'_, T>> {
        self.data.try_lock().map(|guard| WriteDataGuard {
            guard,
            cur_state: &self.cur_state,
            read_state: &self.read_state,
        })
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
    /// # use duat_core::{Mutex, data::RwData};
    /// let list: [RwData<dyn Display>; 3] = [
    ///     RwData::new_unsized::<String>(Arc::new(Mutex::new(String::from(
    ///         "I can show you the world",
    ///     )))),
    ///     RwData::new_unsized::<String>(Arc::new(Mutex::new(String::from(
    ///         "Shining, shimmering, splendid",
    ///     )))),
    ///     RwData::new_unsized::<char>(Arc::new(Mutex::new('🧞'))),
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
    /// # use duat_core::{Mutex, data::RwData};
    /// let list: [RwData<dyn Display>; 3] = [
    ///     RwData::new_unsized::<String>(Arc::new(Mutex::new(String::from(
    ///         "I can show you the world",
    ///     )))),
    ///     RwData::new_unsized::<String>(Arc::new(Mutex::new(String::from(
    ///         "Shining, shimmering, splendid",
    ///     )))),
    ///     RwData::new_unsized::<char>(Arc::new(Mutex::new('🧞'))),
    /// ];
    ///
    /// let maybe_char = list[2].clone().try_downcast::<char>();
    /// assert!(maybe_char.is_some());
    /// *maybe_char.unwrap().write() = '👳';
    ///
    /// let maybe_string = list[0].clone().try_downcast::<char>();
    /// assert!(maybe_string.is_none());
    /// ```
    /// If you don't need to keep a [`RwData<U>`], consider just using
    /// [`RwData::read_as`]. If you only need to know if the type
    /// matches, consider using [`RwData::data_is`].
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn try_downcast<U: 'static>(&self) -> Option<RwData<U>> {
        if self.data_is::<U>() {
            let Self { data, cur_state, read_state, .. } = self.clone();
            let ptr = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(ptr as *const Mutex<U>) };
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
    /// # use duat_core::{Mutex, data::RwData};
    /// let list: [RwData<dyn Display>; 3] = [
    ///     RwData::new_unsized::<String>(Arc::new(Mutex::new(String::from(
    ///         "I can show you the world",
    ///     )))),
    ///     RwData::new_unsized::<String>(Arc::new(Mutex::new(String::from(
    ///         "Shining, shimmering, splendid",
    ///     )))),
    ///     RwData::new_unsized::<char>(Arc::new(Mutex::new('🧞'))),
    /// ];
    ///
    /// assert!(matches!(
    ///     list[2].read_as::<char>().map(|c| c.len_utf8()),
    ///     Some(4)
    /// ));
    /// assert!(matches!(
    ///     list[1].read_as::<char>().map(|c| c.to_ascii_uppercase()),
    ///     None
    /// ));
    /// ```
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn read_as<U: 'static>(&self) -> Option<ReadDataGuard<'_, U>> {
        if !self.data_is::<U>() {
            return None;
        }

        self.read_state
            .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);
        let ptr = Arc::as_ptr(&self.data) as *const Mutex<U>;
        // SAFETY: Since this borrows this RwData, the Arc shouldn't be
        // dropped while this guard exists
        Some(ReadDataGuard(unsafe { ptr.as_ref().unwrap().lock() }))
    }

    pub fn write_as<U: 'static>(&self) -> Option<WriteDataGuard<'_, U>> {
        if !self.data_is::<U>() {
            return None;
        }

        let ptr = Arc::as_ptr(&self.data) as *const Mutex<U>;
        Some(WriteDataGuard {
            // SAFETY: Since this borrows this RwData, the Arc shouldn't
            // be dropped while this guard exists
            guard: unsafe { ptr.as_ref().unwrap().lock() },
            cur_state: &self.cur_state,
            read_state: &self.read_state,
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
    read_state: &'a AtomicUsize,
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
        let prev = self.cur_state.fetch_add(1, Ordering::Acquire);
        self.read_state.store(prev + 1, Ordering::Release)
    }
}

pub struct DataMap<I: ?Sized + Send + 'static, O> {
    data: RwData<I>,
    f: Box<dyn FnMut() -> O + Send>,
}

impl<I: ?Sized + Send + 'static, O> DataMap<I, O> {
    pub fn fns(
        self,
    ) -> (
        Box<dyn FnMut() -> O + Send>,
        Box<dyn Fn() -> bool + Send + Sync>,
    ) {
        (self.f, Box::new(self.data.checker()))
    }
}

impl<I: ?Sized + Send + 'static, O> FnOnce<()> for DataMap<I, O> {
    type Output = O;

    extern "rust-call" fn call_once(mut self, _: ()) -> Self::Output {
        (self.f)()
    }
}

impl<I: ?Sized + Send + 'static, O> FnMut<()> for DataMap<I, O> {
    extern "rust-call" fn call_mut(&mut self, _: ()) -> Self::Output {
        (self.f)()
    }
}

impl<I: ?Sized + Send + 'static> RwData<I> {
    pub fn map<O>(&self, mut f: impl FnMut(&I) -> O + Send + 'static) -> DataMap<I, O> {
        let data = self.clone();
        let f = move || f(&*data.read());
        DataMap { data: self.clone(), f: Box::new(f) }
    }
}

impl<I: ?Sized + Send + 'static, O: 'static> DataMap<I, O> {
    pub fn map<O2>(mut self, mut f: impl FnMut(O) -> O2 + Send + 'static) -> DataMap<I, O2> {
        DataMap {
            data: self.data,
            f: Box::new(move || f((self.f)())),
        }
    }
}
