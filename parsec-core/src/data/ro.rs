#[cfg(not(feature = "deadlock-detection"))]
use std::sync::{RwLock, RwLockReadGuard};
use std::{
    any::TypeId,
    marker::PhantomData,
    mem::MaybeUninit,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, LazyLock, TryLockError, TryLockResult,
    },
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{RwLock, RwLockReadGuard};

use super::{private, DataCastErr, DataRetrievalErr, RawReadableData, ReadableData, RwData};
use crate::{
    input::InputMethod,
    ui::Ui,
    widgets::{File, Widget},
};

/// A read-only reference to information.
///
/// A data type that can read, but not write, the inner data. If you
/// wish to get a read write struct, see [`RwData`]. This struct is
/// usually what you will get as a readable handle from structs
/// containing [`RwData`]. For example, when opening a new file, we
/// use a hook, called the `constructor_hook`, to do certain actions
/// to that file, like opening surroundig [`Widget`]s. This hook can
/// provide an [`RoData<FileWidget<U>>`], which is then queryed by the
/// widget for information about the file. This also prevents the
/// modification of data by third parties which shouldn`t be able to
/// do so.
///
/// Can only be created by cloning the [`Arc<RwLock<T>>`] from a
/// [`RwData<T>`], or through cloning.
///
/// [`Widget`]: crate::widgets::Widget
/// [`RoData<FileWidget<U>>`]: RoData
pub struct RoData<T>
where
    T: ?Sized + 'static,
{
    data: Arc<RwLock<T>>,
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize,
    concrete_type: TypeId,
}

impl<T> RoData<T> {
    /// Returns a new instance of a [`RoData<T>`], assuming that it is
    /// sized.
    ///
    /// This has to be sized because of some Rust limitations, as you
    /// can`t really pass an unsized argument to a function (for now).
    /// If you're looking to store unsized types (`dyn Trait`,
    /// `[Type]`, etc) on a [`RwData`], see [`RwData::new_unsized`].
    pub fn new(data: T) -> Self {
        Self {
            data: Arc::new(RwLock::new(data)),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1),
            concrete_type: TypeId::of::<T>(),
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized,
{
    /// Returns a new instance of [`RoData<T>`], assuming that it is
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
            concrete_type: TypeId::of::<SizedT>(),
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
    pub fn read(&self) -> RwLockReadGuard<'_, T> {
        <Self as ReadableData<T>>::read(self)
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
    pub fn inspect<U>(&self, f: impl FnOnce(&T) -> U) -> U {
        <Self as ReadableData<T>>::inspect(self, f)
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
    pub fn try_read(&self) -> TryLockResult<RwLockReadGuard<'_, T>> {
        <Self as ReadableData<T>>::try_read(self)
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
    pub fn try_inspect<U>(
        &self,
        f: impl FnOnce(&T) -> U,
    ) -> Result<U, TryLockError<RwLockReadGuard<'_, T>>> {
        <Self as ReadableData<T>>::try_inspect(self, f)
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
    pub fn has_changed(&self) -> bool {
        <Self as ReadableData<T>>::has_changed(self)
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
    pub fn ptr_eq<U>(&self, other: &impl ReadableData<U>) -> bool
    where
        U: ?Sized,
    {
        <Self as ReadableData<T>>::ptr_eq(self, other)
    }

    /// Blocking inspection of the inner data.
    ///
    /// # Examples
    ///
    /// You may want this method if you're storing a list of
    /// [`RwData<dyn Trait>`], and want to know, at runtime, what type
    /// each element is:
    /// ```rust
    /// # use std::{
    /// #     any::Any,
    /// #     fmt::Display,
    /// #     sync::{Arc, RwLock}
    /// # };
    /// # use parsec_core::data::{RwData};
    /// # struct DownCastableChar(char);
    /// # struct DownCastableString(String);
    /// # impl AsAny for DownCastableChar {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// let list: [RwData<dyn Any>; 3] = [
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableString(
    ///         String::from("I can show you the world"),
    ///     )))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableString(
    ///         String::from("Shining, shimmering, splendid"),
    ///     )))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableChar('🧞')))),
    /// ];
    ///
    /// assert!(matches!(
    ///     list[2].inspect_as::<DownCastableChar, char>(|char| char.0),
    ///     Some('🧞')
    /// ));
    /// assert!(matches!(
    ///     list[1].inspect_as::<DownCastableChar, char>(|char| char.0),
    ///     None
    /// ));
    /// ```
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn inspect_as<U: 'static, R>(&self, f: impl FnOnce(&U) -> R) -> Option<R> {
        (self.concrete_type == TypeId::of::<U>()).then(|| {
            let ptr = Arc::as_ptr(&self.data);
            let cast = unsafe { ptr.cast::<RwLock<U>>().as_ref().unwrap() };

            self.read_state
                .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);

            f(&cast.read().unwrap())
        })
    }

    pub fn data_is<U>(&self) -> bool
    where
        U: 'static,
    {
        self.concrete_type == TypeId::of::<U>()
    }

    /// Tries to downcast to a concrete type.
    pub fn try_downcast<U>(self) -> Result<RoData<U>, DataCastErr<RoData<T>, T, U>>
    where
        U: 'static,
    {
        if self.concrete_type == TypeId::of::<U>() {
            let Self {
                data,
                cur_state,
                read_state,
                ..
            } = self;
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RoData {
                data,
                cur_state,
                read_state,
                concrete_type: TypeId::of::<U>(),
            })
        } else {
            Err(DataCastErr(self, PhantomData, PhantomData))
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
            concrete_type: TypeId::of::<T>(),
        }
    }
}

impl<T> std::fmt::Debug for RoData<T>
where
    T: ?Sized + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.data.read().unwrap(), f)
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
            concrete_type: value.concrete_type,
        }
    }
}

impl<T> private::DataHolder<T> for RoData<T>
where
    T: ?Sized,
{
    fn data(&self) -> RwLockReadGuard<'_, T> {
        self.data.read().unwrap()
    }

    fn try_data(&self) -> TryLockResult<RwLockReadGuard<'_, T>> {
        self.data.try_read()
    }

    fn cur_state(&self) -> &Arc<AtomicUsize> {
        &self.cur_state
    }

    fn read_state(&self) -> &AtomicUsize {
        &self.read_state
    }
}

impl<T: ?Sized> ReadableData<T> for RoData<T> {}
impl<T: ?Sized> RawReadableData<T> for RoData<T> {}

unsafe impl<T: ?Sized + Send> Send for RoData<T> {}
unsafe impl<T: ?Sized + Sync> Sync for RoData<T> {}

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
            concrete_type: self.concrete_type,
        }
    }
}

impl<T> std::fmt::Display for RoData<T>
where
    T: std::fmt::Display + 'static,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&*self.read(), f)
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
    T: ?Sized + 'static,
{
    data: RoData<RwData<T>>,
    read_state: AtomicUsize,
}

impl<T> RoNestedData<T>
where
    T: ?Sized,
{
    /// Blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// returns `false`.
    pub fn inspect<R>(&self, f: impl FnOnce(&T) -> R) -> R {
        let data = self.data.read();
        let cur_state = data.cur_state.load(Ordering::Acquire);
        self.read_state.store(cur_state, Ordering::Release);
        let inner_data = data.raw_read();
        f(&inner_data)
    }

    /// Non blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// `false`.
    pub fn try_inspect<R>(
        &self,
        f: impl FnOnce(&T) -> R,
    ) -> Result<R, DataRetrievalErr<RoData<T>, T>> {
        self.data
            .try_read()
            .map_err(|_| DataRetrievalErr::NestedReadBlocked)
            .and_then(|data| {
                data.raw_try_read()
                    .map_err(|_| DataRetrievalErr::ReadBlocked)
                    .map(|inner_data| {
                        let cur_state = data.cur_state.load(Ordering::Acquire);
                        self.read_state.store(cur_state, Ordering::Release);
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
        let cur_state = data.cur_state.load(Ordering::Acquire);
        has_changed |= cur_state > self.read_state.load(Ordering::Acquire);
        self.read_state.store(cur_state, Ordering::Release);
        has_changed
    }
}

impl<T> Clone for RoNestedData<T>
where
    T: ?Sized + 'static,
{
    fn clone(&self) -> Self {
        RoNestedData {
            data: self.data.clone(),
            read_state: AtomicUsize::new(self.data.raw_read().cur_state.load(Ordering::Relaxed)),
        }
    }
}

impl<T> From<&RwData<RwData<T>>> for RoNestedData<T>
where
    T: ?Sized + 'static,
{
    fn from(value: &RwData<RwData<T>>) -> Self {
        RoNestedData {
            data: RoData::from(value),
            read_state: AtomicUsize::new(value.raw_read().cur_state.load(Ordering::Relaxed)),
        }
    }
}

pub struct ActiveFile {
    rw: LazyLock<RwData<MaybeUninit<(RwData<File>, RwData<dyn InputMethod>)>>>,
    ro: LazyLock<RwData<MaybeUninit<(RoData<File>, RoData<dyn InputMethod>)>>>,
}

impl ActiveFile {
    pub fn current(&self) -> FileReader {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::from(&*self.ro),
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }

    pub fn adaptive(&self) -> FileReader {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData {
                data: todo!(),
                cur_state: todo!(),
                read_state: todo!(),
                concrete_type: todo!(),
            },
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&File, &dyn InputMethod) -> R) -> R {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&file.read(), input))
    }

    pub fn inspect_as<I: InputMethod, R>(&self, f: impl FnOnce(&File, &I) -> R) -> Option<R> {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.inspect_as::<I, R>(|input| f(&file.read(), input))
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<File>, &RoData<dyn InputMethod>) -> R,
    ) -> R {
        let data = self.ro.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        f(&file, &input)
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> Option<String> {
        unsafe { self.rw.raw_read().assume_init_ref().0.raw_read().name() }
    }

    pub fn has_swapped(&self) -> bool {
        self.rw.has_changed()
    }

    pub fn file_ptr_eq<U: Ui>(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.rw.read().assume_init_ref().0) }
    }

    pub(crate) const fn new() -> Self {
        ActiveFile {
            rw: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
            ro: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub(crate) fn swap(
        &self,
        file: RwData<File>,
        input: RwData<dyn InputMethod>,
    ) -> (RwData<File>, RwData<dyn InputMethod>) {
        *self.ro.write() = MaybeUninit::new((RoData::from(&file), RoData::from(&input)));

        unsafe {
            std::mem::replace(&mut *self.rw.write(), MaybeUninit::new((file, input))).assume_init()
        }
    }

    pub(crate) fn set(&self, file: RwData<File>, input: RwData<dyn InputMethod>) {
        *self.ro.write() = MaybeUninit::new((RoData::from(&file), RoData::from(&input)));
        *self.rw.write() = MaybeUninit::new((file, input));
    }
}

pub struct FileReader {
    data: RoData<MaybeUninit<(RoData<File>, RoData<dyn InputMethod>)>>,
    file_state: AtomicUsize,
    input_state: AtomicUsize,
}

impl FileReader {
    pub fn inspect<R>(&self, f: impl FnOnce(&File, &dyn InputMethod) -> R) -> R {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state.load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state.load(Ordering::Acquire), Ordering::Release);

        input.inspect(|input| f(&file.read(), input))
    }

    pub fn inspect_as<I: InputMethod, R>(&self, f: impl FnOnce(&File, &I) -> R) -> Option<R> {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state.load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state.load(Ordering::Acquire), Ordering::Release);

        file.inspect_as::<I, R>(|input| f(&file.raw_read(), input))
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<File>, &RoData<dyn InputMethod>) -> R,
    ) -> R {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        f(&file, &input)
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> Option<String> {
        unsafe { self.data.read().assume_init_ref().0.read().name() }
    }

    pub fn has_changed(&self) -> bool {
        let mut has_changed = self.data.has_changed();
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        has_changed |= {
            let file_state = file.cur_state.load(Ordering::Acquire);

            file_state > self.file_state.swap(file_state, Ordering::Acquire)
        };

        has_changed |= {
            let input_state = input.cur_state.load(Ordering::Acquire);

            input_state > self.input_state.swap(input_state, Ordering::Acquire)
        };

        has_changed
    }
}

impl Clone for FileReader {
    fn clone(&self) -> Self {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        Self {
            data: self.data.clone(),
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }
}
