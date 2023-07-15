use std::{
    marker::PhantomData,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, RwLock, RwLockReadGuard, TryLockResult
    }
};

use super::{private, AsAny, DataCastErr, DataRetrievalErr, RawReadableData, ReadableData, RwData};

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
    T: ?Sized
{
    data: Arc<RwLock<T>>,
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize
}

impl<T> RoData<T> {
    /// Returns a new instance of a [`RoData<T>`], assuming that it is
    /// sized.
    fn new(data: T) -> Self {
        Self {
            data: Arc::new(RwLock::new(data)),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(1)
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized + AsAny
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
        if (*data.read().unwrap()).as_any().is::<U>() {
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
                std::marker::PhantomData,
                std::marker::PhantomData
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

impl<T> std::fmt::Debug for RoData<T>
where
    T: ?Sized + std::fmt::Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.data.read().unwrap(), f)
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

impl<T> private::DataHolder<T> for RoData<T>
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

unsafe impl<T> Send for RoData<T> where T: ?Sized + Send {}
unsafe impl<T> Sync for RoData<T> where T: ?Sized + Send + Sync {}

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

impl<T> std::fmt::Display for RoData<T>
where
    T: std::fmt::Display + 'static
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.read(), f)
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
    pub(crate) fn new(data: RoData<T>) -> Self {
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
        let cur_state = data.cur_state.load(Ordering::Acquire);
        self.read_state.store(cur_state, Ordering::Release);
        let inner_data = data.raw_read();
        f(&inner_data)
    }

    /// Non blocking inspection of the inner data.
    ///
    /// Also makes it so that [`has_changed()`][Self::has_changed()]
    /// `false`.
    pub fn try_inspect<B>(
        &self, f: impl FnOnce(&T) -> B
    ) -> Result<B, DataRetrievalErr<RoData<T>, T>> {
        self.data
            .try_read()
            .map_err(|_| DataRetrievalErr::NestedReadBlocked(PhantomData))
            .and_then(|data| {
                data.raw_try_read()
                    .map_err(|_| DataRetrievalErr::ReadBlocked(PhantomData))
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