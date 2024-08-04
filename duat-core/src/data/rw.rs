use std::{
    any::TypeId,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use super::{private::InnerData, Data, RoData, RwLock, RwLockReadGuard, RwLockWriteGuard};
use crate::{
    ui::Ui,
    widgets::{ActiveWidget, PassiveWidget},
};

/// A read-write reference to information, that can tell readers if
/// said information has changed.
///
/// The reading part is done through the [`ReadableData`] trait, as
/// such, you need to import it in order to read the data of this
/// struct.
///
/// Unlike [`RoData`], this struct can read and write over itself, and
/// also implements [`Clone`] and [`Into<RoData>`], so you can have as
/// many readers and writers as you want.
///
/// All reads and writes are thread safe, allowing Duat to run many
/// different tasks at once, without letting the program hang from one
/// particularly slow task (most commonly a slow [`Widget`]).
///
/// The most common usecase for this data type are [`Widget`]s that
/// read from a [`FileWidget`], such as [`LineNumbers`], which do so
/// by holding an [`RoData<FileWidget<U>`], which was acquired from an
/// existing [`RoData<FileWidget<U>`].
/// ```rust
/// # use duat_core::{data::RoData, ui::Ui, widgets::FileWidget};
/// # struct OtherStuff;
/// struct WidgetThatReadsFromFile<U>
/// where
///     U: Ui,
/// {
///     file: RoData<FileWidget<U>>,
///     other_stuff: OtherStuff,
/// }
/// ```
/// Internally, all [`Widget`]s are stored inside [`RwData`]s, so
/// access to them is always thread safe and available (read only) to
/// everyone who wants to read them.
///
/// [`Into<RoData>`]: Into
/// [`Widget`]: crate::widgets::Widget
/// [`FileWidget`]: crate::widgets::FileWidget
/// [`LineNumbers`]: crate::widgets::LineNumbers
/// [`RoData<FileWidget<U>`]: RoData
/// [`RwData<FileWidget<U>`]: RoData
pub struct RwData<T>
where
    T: ?Sized + 'static,
{
    // The `Box` is to allow for downcasting.
    pub(super) data: Arc<RwLock<T>>,
    pub(super) cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize,
    pub(super) type_id: TypeId,
}

impl<T> RwData<T> {
    /// Returns a new instance of a [`RwData<T>`], assuming that it is
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
            type_id: TypeId::of::<T>(),
        }
    }
}

impl<T> RwData<T>
where
    T: ?Sized + 'static,
{
    /// Returns a new instance of [`RwData<T>`], assuming that it is
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
    /// # use std::{
    /// #     thread,
    /// #     time::{Duration, Instant}
    /// # };
    /// # use duat_core::data::{ReadableData, RoData, RwData};
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
    /// # use duat_core::data::{ReadableData, RoData, RwData};
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
    /// # use duat_core::data::{ReadableData, RoData, RwData};
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
    /// # use duat_core::data::{ReadableData, RoData, RwData};
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
    /// # use duat_core::data::{ReadableData, RoData, RwData};
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
    /// # use std::{sync::TryLockError};
    /// # use duat_core::data::{ReadableData, RwData};
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
    /// # use std::sync::TryLockError;
    /// # use duat_core::data::{ReadableData, RwData};
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
    /// that specific instance of a [`ReadableData`].
    ///
    /// When first creating a [`ReadableData`] type, `has_changed`
    /// will return `false`;
    ///
    /// # Examples
    /// ```rust
    /// use duat_core::data::{ReadableData, RoData, RwData};
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

    /// Returns `true` if both [`ReadableData<T>`]s point to the same
    /// data.
    ///
    /// # Examples
    /// ```rust
    /// # use duat_core::data::{ReadableData, RwData};
    /// let data_1 = RwData::new(false);
    /// let data_1_clone = data_1.clone();
    ///
    /// let data_2 = RwData::new(true);
    ///
    /// assert!(data_1.ptr_eq(&data_1_clone));
    /// assert!(!data_1.ptr_eq(&data_2));
    /// ```
    pub fn ptr_eq<U>(&self, other: &(impl Data<U> + ?Sized)) -> bool
    where
        U: ?Sized,
    {
        Arc::as_ptr(self.cur_state()) == Arc::as_ptr(other.cur_state())
    }

    /// Blocking mutable reference to the information.
    ///
    /// Also makes it so that [`has_changed`] returns true for
    /// [`self`] or any of its clones, be they [`RoData`] or
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
    /// [`has_changed`]: ReadableData::has_changed
    pub fn write(&self) -> ReadWriteGuard<T> {
        let guard = self.data.write();
        ReadWriteGuard {
            guard,
            cur_state: &self.cur_state,
        }
    }

    /// Non Blocking mutable reference to the information.
    ///
    /// Also makes it so that [`has_changed`] returns true for
    /// [`self`] or any of its clones, be they [`RoData`] or
    /// [`RwData`].
    ///
    /// # Safety
    ///
    /// Unlike [`RwData::write`], this method cannot cause deadlocks,
    /// returning an [`Err`] instead.
    /// ```
    /// # use std::{mem, thread, time::Duration};
    /// # use duat_core::data::{ReadableData, RwData};
    /// let data_1 = RwData::new('😀');
    /// let data_2 = RwData::new('😁');
    ///
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut data_1 = data_1.try_write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_2 = data_2.try_write();
    ///         if let (Ok(mut data_1), Ok(mut data_2)) = (data_1, data_2) {
    ///             mem::swap(&mut data_1, &mut data_2);
    ///         }
    ///     });
    ///
    ///     scope.spawn(|| {
    ///         let mut data_2 = data_2.try_write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_1 = data_1.try_write();
    ///         if let (Ok(mut data_1), Ok(mut data_2)) = (data_1, data_2) {
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
    /// [`has_changed`]: ReadableData::has_changed
    pub fn try_write(&self) -> Option<ReadWriteGuard<'_, T>> {
        self.data.try_write().map(|guard| ReadWriteGuard {
            guard,
            cur_state: &self.cur_state,
        })
    }

    /// Blocking mutation of the inner data.
    ///
    /// Also makes it so that [`has_changed`] returns true for
    /// [`self`] or any of its clones, be they [`RoData`] or
    /// [`RwData`].
    ///
    /// # Safety
    ///
    /// This method "technically" has the same problems as
    /// [`RwData::write`], where you can deadlock by calling it on
    /// multiple instances of [`RwData`], but it makes this much more
    /// explicit:
    /// ```no_run
    /// # use std::{mem, thread, time::Duration};
    /// # use duat_core::data::RwData;
    /// let data_1 = RwData::new('😟');
    /// let data_2 = RwData::new('😭');
    ///
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         data_1.mutate(|data_1| {
    ///             thread::sleep(Duration::from_millis(100));
    ///             let mut data_2 = data_2.write();
    ///             mem::swap(data_1, &mut *data_2);
    ///         });
    ///     });
    ///
    ///     scope.spawn(|| {
    ///         data_2.mutate(|data_2| {
    ///             thread::sleep(Duration::from_millis(100));
    ///             let mut data_1 = data_1.write();
    ///             mem::swap(&mut *data_1, data_2);
    ///         });
    ///     });
    /// });
    /// ```
    /// Generally, you should favor this method for longer functions
    /// in which the data is only needed for a short time.
    ///
    /// [`has_changed`]: ReadableData::has_changed
    pub fn mutate<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(&mut self.write().guard)
    }

    /// Non blocking mutation of the inner data.
    ///
    /// Also makes it so that [`has_changed`] returns true for
    /// [`self`] or any of its clones, be they [`RoData`] or
    /// [`RwData`].
    ///
    /// # Safety
    ///
    /// Much like [`RwData::try_write`], this also can't deadlock,
    /// failing instead. Generally, you should use this method only
    /// when you are fine with not writing the data.
    ///
    /// [`has_changed`]: ReadableData::has_changed
    pub fn try_mutate<R>(&self, f: impl FnOnce(&mut T) -> R) -> Option<R> {
        let res = self.try_write();
        res.map(|mut data| f(&mut *data))
    }

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
    pub(crate) fn raw_read(&self) -> RwLockReadGuard<'_, T> {
        self.data()
    }
}

impl<T> RwData<T>
where
    T: ?Sized + 'static,
{
    /// Returns `true` if the data is of the concrete type `T`.
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
    /// # use duat_core::data::{RwData, AsAny};
    /// # struct DownCastableChar(char);
    /// # struct DownCastableString(String);
    /// # impl AsAny for DownCastableChar {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// # impl AsAny for DownCastableString {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// let list: [RwData<dyn AsAny>; 3] = [
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableString(
    ///         String::from("I can show you the world"),
    ///     )))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableString(
    ///         String::from("Shining, shimmering, splendid"),
    ///     )))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableChar('🧞')))),
    /// ];
    ///
    /// assert!(list[0].data_is::<DownCastableString>());
    /// assert!(list[1].data_is::<DownCastableString>());
    /// assert!(list[2].data_is::<DownCastableChar>());
    /// ```
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn data_is<U>(&self) -> bool
    where
        U: 'static,
    {
        self.type_id == TypeId::of::<U>()
    }

    /// Tries to downcast to a concrete type.
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
    /// # use duat_core::data::{RwData, AsAny};
    /// # struct DownCastableChar(char);
    /// # struct DownCastableString(String);
    /// # impl AsAny for DownCastableChar {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// # impl AsAny for DownCastableString {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// let list: [RwData<dyn AsAny>; 3] = [
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableString(
    ///         String::from("I can show you the world"),
    ///     )))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableString(
    ///         String::from("Shining, shimmering, splendid"),
    ///     )))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableChar('🧞')))),
    /// ];
    ///
    /// let maybe_char = list[2].clone().try_downcast::<DownCastableChar>();
    /// assert!(maybe_char.is_ok());
    /// *maybe_char.unwrap().write() = DownCastableChar('👳');
    ///
    /// let maybe_string = list[0].clone().try_downcast::<DownCastableChar>();
    /// assert!(maybe_string.is_err());
    /// ```
    /// If you don't need to write to the data, consider using
    /// [`RwData::inspect_as`]. If you only need to know if the type
    /// matches, consider using [`RwData::data_is`].
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn try_downcast<U>(&self) -> Option<RwData<U>>
    where
        U: 'static,
    {
        if self.data_is::<U>() {
            let Self {
                data,
                cur_state,
                read_state,
                ..
            } = self.clone();
            let pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(pointer.cast::<RwLock<U>>()) };
            Some(RwData {
                data,
                cur_state,
                read_state,
                type_id: TypeId::of::<U>(),
            })
        } else {
            None
        }
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
    /// # use duat_core::data::{RwData, AsAny};
    /// # struct DownCastableChar(char);
    /// # struct DownCastableString(String);
    /// # impl AsAny for DownCastableChar {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// # impl AsAny for DownCastableString {
    /// #     fn as_any(&self) -> &dyn Any {
    /// #         self
    /// #     }
    /// # }
    /// let list: [RwData<dyn AsAny>; 3] = [
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
        (self.data_is::<U>()).then(|| {
            let ptr = Arc::as_ptr(&self.data);
            let cast = unsafe { ptr.cast::<RwLock<U>>().as_ref().unwrap() };

            self.read_state
                .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);

            f(&cast.read())
        })
    }

    pub fn mutate_as<U: 'static, R>(&self, f: impl FnOnce(&mut U) -> R) -> Option<R> {
        (self.data_is::<U>()).then(|| {
            let ptr = Arc::as_ptr(&self.data);
            let cast = unsafe { ptr.cast::<RwLock<U>>().as_ref().unwrap() };

            self.read_state
                .store(self.cur_state.load(Ordering::Acquire), Ordering::Release);

            f(&mut cast.write())
        })
    }

    pub(crate) fn raw_write(&self) -> RwLockWriteGuard<'_, T> {
        self.data.write()
    }

    pub(crate) fn inner_arc(&self) -> &Arc<RwLock<T>> {
        &self.data
    }
}

impl<T> std::fmt::Debug for RwData<T>
where
    T: ?Sized + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.data.read(), f)
    }
}

impl<T> Clone for RwData<T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1),
            type_id: self.type_id,
        }
    }
}

impl<T> Default for RwData<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            data: Arc::new(RwLock::new(T::default())),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1),
            type_id: TypeId::of::<T>(),
        }
    }
}

impl<T> InnerData<T> for RwData<T>
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

impl<U> RwData<dyn ActiveWidget<U>>
where
    U: Ui,
{
    pub fn to_passive(self) -> RwData<dyn PassiveWidget<U>> {
        RwData {
            data: self.data as Arc<RwLock<dyn PassiveWidget<U>>>,
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1),
            type_id: self.type_id,
        }
    }
}

impl<T: ?Sized> Data<T> for RwData<T> {
    fn to_ro(&self) -> super::RoData<T> {
        RoData::from(self)
    }

    fn has_changed(&self) -> bool {
        self.has_changed()
    }
}

unsafe impl<T: ?Sized + Send> Send for RwData<T> {}
unsafe impl<T: ?Sized + Sync> Sync for RwData<T> {}

pub struct ReadWriteGuard<'a, T>
where
    T: ?Sized,
{
    guard: RwLockWriteGuard<'a, T>,
    cur_state: &'a Arc<AtomicUsize>,
}

impl<'a, T> std::ops::Deref for ReadWriteGuard<'a, T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}

impl<'a, T> std::ops::DerefMut for ReadWriteGuard<'a, T>
where
    T: ?Sized,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}

impl<'a, T> Drop for ReadWriteGuard<'a, T>
where
    T: ?Sized,
{
    fn drop(&mut self) {
        self.cur_state.fetch_add(1, Ordering::Release);
    }
}
