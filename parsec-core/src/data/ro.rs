#[cfg(not(feature = "deadlock-detection"))]
use std::sync::{RwLock, RwLockReadGuard};
use std::{
    any::TypeId,
    marker::PhantomData,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, TryLockResult
    }
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{RwLock, RwLockReadGuard};

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
    T: ?Sized + 'static
{
    data: Arc<RwLock<T>>,
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize,
    type_id: TypeId
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
        if self.type_id == TypeId::of::<U>() {
            let Self { data, cur_state, read_state, type_id } = self;
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RoData { data, cur_state, read_state, type_id })
        } else {
            Err(DataCastErr(self, PhantomData, PhantomData))
        }
    }
}

impl<T> RoData<T>
where
    T: ?Sized
{
    pub fn data_is<U>(&self) -> bool
    where
        U: 'static
    {
        self.type_id == std::any::TypeId::of::<Arc<RwLock<U>>>()
    }
}

impl<T> Default for RoData<T>
where
    T: Default
{
    fn default() -> Self {
        Self {
            data: Arc::new(RwLock::new(T::default())),
            cur_state: Arc::new(AtomicUsize::new(0)),
            read_state: AtomicUsize::new(0),
            type_id: TypeId::of::<T>()
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
            read_state: AtomicUsize::new(value.cur_state.load(Ordering::Relaxed)),
            type_id: TypeId::of::<T>()
        }
    }
}

impl<T> private::DataHolder<T> for RoData<T>
where
    T: ?Sized
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
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed)),
            type_id: TypeId::of::<T>()
        }
    }
}

impl<T> std::fmt::Display for RoData<T>
where
    T: std::fmt::Display + 'static
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
    T: ?Sized + 'static
{
    data: RoData<RwData<T>>,
    read_state: AtomicUsize
}

impl<T> RoNestedData<T>
where
    T: ?Sized
{
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
        self.data.try_read().map_err(|_| DataRetrievalErr::NestedReadBlocked(PhantomData)).and_then(
            |data| {
                data.raw_try_read().map_err(|_| DataRetrievalErr::ReadBlocked(PhantomData)).map(
                    |inner_data| {
                        let cur_state = data.cur_state.load(Ordering::Acquire);
                        self.read_state.store(cur_state, Ordering::Release);
                        f(&inner_data)
                    }
                )
            }
        )
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

impl<T> From<&RwData<RwData<T>>> for RoNestedData<T>
where
    T: ?Sized + 'static
{
    fn from(value: &RwData<RwData<T>>) -> Self {
        RoNestedData {
            data: RoData::from(value),
            read_state: AtomicUsize::new(value.raw_read().cur_state.load(Ordering::Relaxed))
        }
    }
}
