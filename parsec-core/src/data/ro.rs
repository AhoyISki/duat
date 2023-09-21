#[cfg(not(feature = "deadlock-detection"))]
use std::sync::{RwLock, RwLockReadGuard};
use std::{
    any::TypeId,
    marker::PhantomData,
    mem::MaybeUninit,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, LazyLock, TryLockResult,
    },
};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::{RwLock, RwLockReadGuard};

use super::{private, DataCastErr, DataRetrievalErr, RawReadableData, ReadableData, RwData};
use crate::{
    input::InputMethod,
    ui::Ui,
    widgets::{FileWidget, Widget},
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
    type_id: TypeId,
}

impl<T> RoData<T>
where
    T: ?Sized,
{
    /// Tries to downcast to a concrete type.
    pub fn try_downcast<U>(self) -> Result<RoData<U>, DataCastErr<RoData<T>, T, U>>
    where
        U: 'static,
    {
        if self.type_id == TypeId::of::<U>() {
            let Self {
                data,
                cur_state,
                read_state,
                type_id,
            } = self;
            let raw_data_pointer = Arc::into_raw(data);
            let data = unsafe { Arc::from_raw(raw_data_pointer.cast::<RwLock<U>>()) };
            Ok(RoData {
                data,
                cur_state,
                read_state,
                type_id,
            })
        } else {
            Err(DataCastErr(self, PhantomData, PhantomData))
        }
    }

    pub fn data_is<U>(&self) -> bool
    where
        U: 'static,
    {
        self.type_id == std::any::TypeId::of::<Arc<RwLock<U>>>()
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
            type_id: TypeId::of::<T>(),
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

impl<T> ReadableData<T> for RoData<T> where T: ?Sized {}
impl<T> RawReadableData<T> for RoData<T> where T: ?Sized {}

unsafe impl<T> Send for RoData<T> where T: ?Sized + Send {}
unsafe impl<T> Sync for RoData<T> where T: ?Sized + Send + Sync {}

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
            type_id: TypeId::of::<T>(),
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
        &self,
        f: impl FnOnce(&T) -> B,
    ) -> Result<B, DataRetrievalErr<RoData<T>, T>> {
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
    data: LazyLock<RwData<MaybeUninit<(RwData<FileWidget>, RwData<dyn InputMethod>)>>>,
    file_state: AtomicUsize,
    input_state: AtomicUsize,
}

impl ActiveFile {
    pub fn current(&self) -> FileReader {
        let data = self.data.raw_read();
        let (file, input) = unsafe { data.assume_init_ref().clone() };

        FileReader { file, input }
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&FileWidget, &dyn InputMethod) -> R) -> R {
        let data = self.data.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&file.read(), input))
    }

    pub fn inspect_as<I: InputMethod, R>(&self, f: impl FnOnce(&FileWidget, &I) -> R) -> Option<R> {
        let data = self.data.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.inspect_as::<I, R>(|input| f(&file.read(), input))
    }

    pub fn mutate_as<I: InputMethod, R>(
        &self,
        f: impl FnOnce(&mut FileWidget, &mut I) -> R,
    ) -> Option<R> {
        let data = self.data.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.mutate_as::<I, R>(|input| f(&mut file.write(), input))
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> Option<String> {
        unsafe { self.data.raw_read().assume_init_ref().0.raw_read().name() }
    }

    pub fn has_changed(&self) -> bool {
        let data = self.data.raw_read();
        let (mut has_changed, (file, input)) = unsafe {
            let has_changed = self.data.has_changed();
            (has_changed, data.assume_init_ref())
        };

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

    pub fn has_swapped(&self) -> bool {
        self.data.has_changed()
    }

    pub fn file_ptr_eq<U: Ui>(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.data.read().assume_init_ref().0) }
    }

    pub(crate) const fn new() -> Self {
        ActiveFile {
            data: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
            file_state: AtomicUsize::new(0),
            input_state: AtomicUsize::new(0),
        }
    }

    pub(crate) fn swap(
        &self,
        file: RwData<FileWidget>,
        input: RwData<dyn InputMethod>,
    ) -> (RwData<FileWidget>, RwData<dyn InputMethod>) {
        self.file_state
            .store(file.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
        self.input_state
            .store(input.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);

        unsafe {
            std::mem::replace(&mut *self.data.write(), MaybeUninit::new((file, input)))
                .assume_init()
        }
    }

    pub(crate) fn set(&self, file: RwData<FileWidget>, input: RwData<dyn InputMethod>) {
        *self.data.write() = MaybeUninit::new((file, input));
    }
}

#[derive(Clone)]
pub struct FileReader {
    file: RwData<FileWidget>,
    input: RwData<dyn InputMethod>,
}

impl FileReader {
    pub fn read(
        &self,
    ) -> (
        RwLockReadGuard<'_, FileWidget>,
        RwLockReadGuard<'_, dyn InputMethod>,
    ) {
        let input = self.input.read();

        (self.file.read(), input)
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&FileWidget, &dyn InputMethod) -> R) -> R {
        self.input.inspect(|input| f(&self.file.read(), input))
    }

    pub fn inspect_as<I: InputMethod, R>(&self, f: impl FnOnce(&FileWidget, &I) -> R) -> Option<R> {
        self.input
            .inspect_as::<I, R>(|input| f(&self.file.read(), input))
    }

    pub fn mutate_as<I: InputMethod, R>(&self, f: impl FnOnce(&FileWidget, &I) -> R) -> Option<R> {
        self.input
            .mutate_as::<I, R>(|input| f(&self.file.read(), input))
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> Option<String> {
        self.file.read().name()
    }

    pub fn has_changed(&self) -> bool {
        self.file.has_changed() || self.input.has_changed()
    }
}
