use std::{
    any::TypeId,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use super::{Data, RwData, private::InnerData};
// use parking_lot::{RwLock, RwLockReadGuard};
use super::{RwLock, RwLockReadGuard};
use crate::{
    ui::Ui,
    widgets::{ActiveWidget, PassiveWidget},
};

/// A read-only reference to information.
///
/// A data type that can read, but not write, the inner data. If you
/// wish to get a read write struct, see [`RwData`]. This struct is
/// usually what you will get as a readable handle from structs
/// containing [`RwData`]. For example, when opening a new file, we
/// use a hook, called the `constructor_hook`, to do certain actions
/// to that file, like opening surroundig [`Widget`]s. This hook can
/// provide an [`RoData`], which is then queryed by the
/// widget for information about the file. This also prevents the
/// modification of data by third parties which shouldn`t be able to
/// do so.
///
/// Can only be created by cloning the [`Arc<RwLock<T>>`] from a
/// [`RwData`], or through cloning.
///
/// [`Widget`]: crate::widgets::Widget
/// [`RoData`]: RoData
pub struct RoData<T>
where
    T: ?Sized + 'static,
{
    data: Arc<RwLock<T>>,
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize,
    type_id: TypeId,
}

impl<T> RoData<T> {
    /// Returns a new instance of a [`RoData`], assuming that it is
    /// sized.
    ///
    /// This has to be sized because of some Rust limitations, as you
    /// can`t really pass an unsized argument to a function (for now).
    /// If you're looking to store unsized types (`dyn Trait`,
    /// `\[Type\]`, etc) on a [`RwData`], see [`RwData::new_unsized`].
    pub fn new(data: T) -> Self {
        Self {
            data: Arc::new(RwLock::new(data)),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1),
            type_id: TypeId::of::<T>(),
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized,
{
    /// Returns a new instance of [`RoData`], assuming that it is
    /// unsized.
    ///
    /// This method is only required if you're dealing with types that
    /// may not be [`Sized`] (`dyn Trait`, `[Type]`, etc). If the type
    /// in question is sized, use [`RwData::new`] instead.
    pub fn new_unsized<SizedT: 'static>(data: Arc<RwLock<T>>) -> Self {
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

    /// Blocking reference to the information.
    ///
    /// Also makes it so that [`has_changed`] returns `false`.
    ///
    /// # Examples
    ///
    /// Since this is a blocking read, the thread will hault while the
    /// data is being written to:
    /// ```rust
    /// # use std::{thread, time::{Duration, Instant}};
    /// # use duat_core::data::{Data, RoData, RwData};
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
    /// # use std::{thread, time::{Duration, Instant} };
    /// # use duat_core::data::{Data, RoData, RwData};
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
    pub fn read(&self) -> RwLockReadGuard<'_, T> {
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
    /// # use duat_core::data::{Data, RoData, RwData};
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
    /// # use duat_core::data::{Data, RoData, RwData};
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
    /// # use duat_core::data::{Data, RoData, RwData};
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
    pub fn inspect<U>(&self, f: impl FnOnce(&T) -> U) -> U {
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
    /// # use duat_core::data::{Data, RwData};
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
    pub fn try_read(&self) -> Option<RwLockReadGuard<'_, T>> {
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
    /// # use duat_core::data::{Data, RwData};
    /// let new_data = RwData::new("hello 👋");
    ///
    /// let try_inspect = new_data.mutate(|blocking_mutate| {
    ///     *blocking_mutate = "bye 👋";
    ///
    ///     new_data.try_inspect(|try_inspect| *try_inspect == "bye 👋")
    /// });
    ///
    /// assert!(matches!(try_inspect, None));
    /// ```
    ///
    /// [`has_changed`]: Self::has_changed
    /// [`inspect`]: Self::inspect
    pub fn try_inspect<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U> {
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
    /// that specific instance of a [`Data`].
    ///
    /// When first creating a [`Data`] type, `has_changed`
    /// will return `false`;
    ///
    /// # Examples
    /// ```rust
    /// use duat_core::data::{Data, RoData, RwData};
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
        let cur_state = self.cur_state().load(Ordering::Relaxed);
        let read_state = self.read_state().swap(cur_state, Ordering::Relaxed);
        cur_state > read_state
    }

    /// Returns `true` if both [`Data<T>`]s point to the same
    /// data.
    ///
    /// # Examples
    /// ```rust
    /// # use duat_core::data::{Data, RwData};
    /// let data_1 = RwData::new(false);
    /// let data_1_clone = data_1.clone();
    ///
    /// let data_2 = RwData::new(true);
    ///
    /// assert!(data_1.ptr_eq(&data_1_clone));
    /// assert!(!data_1.ptr_eq(&data_2));
    /// ```
    pub fn ptr_eq<U>(&self, other: &impl Data<U>) -> bool
    where
        U: ?Sized,
    {
        Arc::as_ptr(self.cur_state()) as usize == Arc::as_ptr(other.cur_state()) as usize
    }

    /// Blocking inspection of the inner data.
    ///
    /// # Examples
    ///
    /// You may want this method if you're storing a list of
    /// [`RwData`]s, and want to know, at runtime, what type
    /// each element is:
    ///
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
    ///     list[2].inspect_as::<char, bool>(|char| char.is_ascii()),
    ///     Some(false)
    /// ));
    /// assert!(matches!(
    ///     list[1].inspect_as::<char, bool>(|char| char.is_ascii()),
    ///     None
    /// ));
    /// ```
    ///
    /// [`RwData`]: RwData
    pub fn inspect_as<U: 'static, R>(&self, f: impl FnOnce(&U) -> R) -> Option<R> {
        self.data_is::<U>().then(|| {
            let ptr = Arc::as_ptr(&self.data);
            let cast = unsafe { ptr.cast::<RwLock<U>>().as_ref().unwrap() };

            self.read_state
                .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);

            f(&cast.read())
        })
    }

    pub fn data_is<U>(&self) -> bool
    where
        U: 'static,
    {
        self.type_id == TypeId::of::<U>()
    }

    /// Tries to downcast to a concrete type.
    pub fn try_downcast<U>(&self) -> Option<RoData<U>>
    where
        U: 'static,
    {
        if self.data_is::<U>() {
            let Self { data, cur_state, read_state, .. } = self.clone();
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Some(RoData {
                data,
                cur_state,
                read_state,
                type_id: TypeId::of::<U>(),
            })
        } else {
            None
        }
    }
}

impl<T> Default for RoData<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            data: Arc::new(RwLock::new(T::default())),
            cur_state: Arc::new(AtomicUsize::new(0)),
            read_state: AtomicUsize::new(0),
            type_id: TypeId::of::<T>(),
        }
    }
}

impl<T> std::fmt::Debug for RoData<T>
where
    T: ?Sized + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.data.read(), f)
    }
}

impl<T> From<&RwData<T>> for RoData<T>
where
    T: ?Sized,
{
    fn from(value: &RwData<T>) -> Self {
        RoData {
            data: value.data.clone(),
            cur_state: value.cur_state.clone(),
            read_state: AtomicUsize::new(value.cur_state.load(Ordering::Relaxed)),
            type_id: value.type_id,
        }
    }
}

impl<T> InnerData<T> for RoData<T>
where
    T: ?Sized,
{
    fn data(&self) -> RwLockReadGuard<'_, T> {
        self.data.read()
    }

    fn try_data(&self) -> Option<RwLockReadGuard<'_, T>> {
        self.data.try_read()
    }

    fn cur_state(&self) -> &Arc<AtomicUsize> {
        &self.cur_state
    }

    fn read_state(&self) -> &AtomicUsize {
        &self.read_state
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
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed)),
            type_id: self.type_id,
        }
    }
}

impl<U> RoData<dyn ActiveWidget<U>>
where
    U: Ui,
{
    pub fn to_passive(self) -> RoData<dyn PassiveWidget<U>> {
        RoData {
            data: self.data as Arc<RwLock<dyn PassiveWidget<U>>>,
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1),
            type_id: self.type_id,
        }
    }
}

impl<T: ?Sized> Data<T> for RoData<T> {
    fn to_ro(&self) -> RoData<T> {
        self.clone()
    }

    fn has_changed(&self) -> bool {
        self.has_changed()
    }
}

unsafe impl<T: ?Sized + Send> Send for RoData<T> {}
unsafe impl<T: ?Sized + Sync> Sync for RoData<T> {}
