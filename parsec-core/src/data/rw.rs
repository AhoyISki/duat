use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, TryLockError, TryLockResult
};

use super::{DataCastErr, ReadableData};

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
/// All reads and writes are thread safe, allowing Parsec to run many
/// different tasks at once, without letting the program hang from one
/// particularly slow task (most commonly a slow [`Widget`]).
///
/// The most common usecase for this data type are [`Widget`]s that
/// read from a [`FileWidget`], such as [`LineNumbers`], which do so
/// by holding an [`RoData<FileWidget<U>`], which was acquired from an
/// existing [`RoData<FileWidget<U>`].
/// ```rust
/// # use parsec_core::{data::RoData, ui::Ui, widgets::FileWidget};
/// # struct OtherStuff;
/// struct WidgetThatReadsFromFile<U>
/// where
///     U: Ui
/// {
///     file: RoData<FileWidget<U>>,
///     other_stuff: OtherStuff
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
    T: ?Sized + Send + Sync
{
    pub(super) data: Arc<RwLock<T>>,
    pub(super) cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize
}

impl<T> RwData<T>
where
    T: Send + Sync
{
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
            read_state: AtomicUsize::new(1)
        }
    }
}

impl<T> RwData<T>
where
    T: ?Sized + Send + Sync + 'static
{
    /// Returns a new instance of [`RwData<T>`], assuming that it is
    /// unsized.
    ///
    /// This method is only required if you're dealing with types that
    /// may not be [`Sized`] (`dyn Trait`, `[Type]`, etc). If the type
    /// in question is sized, use [`RwData::new`] instead.
    pub fn new_unsized(data: Arc<RwLock<T>>) -> Self {
        // It's 1 here so that any `RoState`s created from this will have
        // `has_changed()` return `true` at least once, by copying the
        // second value - 1.
        Self {
            data,
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
        }
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
    /// # use parsec_core::data::RwData;
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
    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.data.write().unwrap()
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
    /// # use parsec_core::data::{ReadableData, RwData};
    /// let data_1 = RwData::new('😀');
    /// let data_2 = RwData::new('😁');
    ///
    /// thread::scope(|scope| {
    ///     scope.spawn(|| {
    ///         let mut data_1 = data_1.try_write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_2 = data_2.try_write();
    ///         if let (Ok(mut data_1), Ok(mut data_2)) = (data_1, data_2)
    ///         {
    ///             mem::swap(&mut data_1, &mut data_2);
    ///         }
    ///     });
    ///
    ///     scope.spawn(|| {
    ///         let mut data_2 = data_2.try_write();
    ///         thread::sleep(Duration::from_millis(100));
    ///         let mut data_1 = data_1.try_write();
    ///         if let (Ok(mut data_1), Ok(mut data_2)) = (data_1, data_2)
    ///         {
    ///             mem::swap(&mut data_1, &mut data_2);
    ///         }
    ///     });
    /// });
    ///
    /// // Two swaps will happen.
    /// assert!(*data_1.read() == '😀');
    /// assert!(*data_2.read() == '😁');
    /// ```
    /// The downside is that you may not want it to fail ever, in
    /// which case, you should probably use [`RwData::write`].
    ///
    /// [`has_changed`]: ReadableData::has_changed
    pub fn try_write(&self) -> TryLockResult<RwLockWriteGuard<T>> {
        self.data.try_write().map(|guard| {
            self.cur_state.fetch_add(1, Ordering::Relaxed);
            guard
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
    /// # use parsec_core::data::RwData;
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
    pub fn mutate<B>(&self, f: impl FnOnce(&mut T) -> B) -> B {
        f(&mut self.write())
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
    pub fn try_mutate<B>(
        &self, f: impl FnOnce(&mut T) -> B
    ) -> Result<B, TryLockError<RwLockWriteGuard<T>>> {
        let res = self.try_write();
        res.map(|mut data| f(&mut *data))
    }
}

impl<T> RwData<T>
where
    T: ?Sized + Send + Sync + super::AsAny
{
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
    /// # use parsec_core::data::{RwData, AsAny};
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
    ///     RwData::new_unsized(Arc::new(RwLock::new(
    ///         DownCastableString(String::from(
    ///             "I can show you the world"
    ///         ))
    ///     ))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(
    ///         DownCastableString(String::from(
    ///             "Shining, shimmering, splendid"
    ///         ))
    ///     ))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableChar(
    ///         '🧞'
    ///     ))))
    /// ];
    ///
    /// let maybe_char =
    ///     list[2].clone().try_downcast::<DownCastableChar>();
    /// assert!(maybe_char.is_ok());
    /// *maybe_char.unwrap().write() = DownCastableChar('👳');
    ///
    /// let maybe_string =
    ///     list[0].clone().try_downcast::<DownCastableChar>();
    /// assert!(maybe_string.is_err());
    /// ```
    /// If you don't need to write to the data, consider using
    /// [`RwData::inspect_as`]. If you only need to know if the type
    /// matches, consider using [`RwData::data_is`].
    ///
    /// [`RwData<dyn Trait>`]: RwData
    pub fn try_downcast<U>(self) -> Result<RwData<U>, DataCastErr<RwData<T>, T, U>>
    where
        U: Send + Sync + 'static
    {
        let Self {
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
    /// # use parsec_core::data::{RwData, AsAny};
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
    ///     RwData::new_unsized(Arc::new(RwLock::new(
    ///         DownCastableString(String::from(
    ///             "I can show you the world"
    ///         ))
    ///     ))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(
    ///         DownCastableString(String::from(
    ///             "Shining, shimmering, splendid"
    ///         ))
    ///     ))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableChar(
    ///         '🧞'
    ///     ))))
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
    pub fn inspect_as<U, V>(&self, f: impl FnOnce(&U) -> V) -> Option<V>
    where
        U: 'static
    {
        self.inspect(|data| data.as_any().downcast_ref::<U>().map(|data| f(data)))
    }

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
    /// # use parsec_core::data::{RwData, AsAny};
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
    ///     RwData::new_unsized(Arc::new(RwLock::new(
    ///         DownCastableString(String::from(
    ///             "I can show you the world"
    ///         ))
    ///     ))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(
    ///         DownCastableString(String::from(
    ///             "Shining, shimmering, splendid"
    ///         ))
    ///     ))),
    ///     RwData::new_unsized(Arc::new(RwLock::new(DownCastableChar(
    ///         '🧞'
    ///     ))))
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
        U: 'static
    {
        let RwData { data, .. } = &self;
        data.read().unwrap().as_any().is::<U>()
    }
}

impl<T> std::fmt::Debug for RwData<T>
where
    T: ?Sized + Send + Sync + std::fmt::Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.data.read().unwrap(), f)
    }
}

impl<T> Clone for RwData<T>
where
    T: ?Sized + Send + Sync
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1)
        }
    }
}

impl<T> Default for RwData<T>
where
    T: Default + Send + Sync
{
    fn default() -> Self {
        Self {
            data: Arc::new(RwLock::new(T::default())),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
        }
    }
}

impl<T> std::fmt::Display for RwData<T>
where
    T: std::fmt::Display + Send + Sync + 'static
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.read(), f)
    }
}

impl<T> super::private::Data<T> for RwData<T>
where
    T: ?Sized + Send + Sync
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

impl<T> super::ReadableData<T> for RwData<T> where T: ?Sized + Send + Sync {}
impl<T> super::RawReadableData<T> for RwData<T> where T: ?Sized + Send + Sync {}

unsafe impl<T> Send for RwData<T> where T: ?Sized + Send + Sync {}
unsafe impl<T> Sync for RwData<T> where T: ?Sized + Send + Sync {}
