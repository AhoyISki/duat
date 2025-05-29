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
    self,
    any::TypeId,
    cell::{Cell, RefCell},
    marker::PhantomData,
    rc::Rc,
};

#[derive(Debug)]
pub struct RwData<T: ?Sized> {
    value: Rc<RefCell<T>>,
    cur_state: Rc<Cell<usize>>,
    read_state: Cell<usize>,
    ty: TypeId,
    no_update: bool,
}

impl<T: 'static> RwData<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Rc::new(RefCell::new(value)),
            ty: TypeId::of::<T>(),
            cur_state: Rc::new(Cell::new(1)),
            read_state: Cell::new(0),
            no_update: false,
        }
    }
}

impl<T: ?Sized> RwData<T> {
    /// Returns an unsized [`RwData`], such as [`RwData<dyn Trait>`]
    ///
    /// # Safety
    ///
    /// There is a type argument `SizedT` which _must_ be the exact
    /// type you are passing to this constructor, i.e., if you are
    /// creating an [`RwData<dyn Display>`] from a [`String`], you'd
    /// do this:
    ///
    /// ```rust
    /// # use std::{cell::RefCell, fmt::Display rc::Rc};
    /// # use duat_core::data::RwData;
    /// let rw_data: RwData<dyn Display> = unsafe {
    ///     RwData::new_unsized::<String>(Rc::new(RefCell::new("testing".to_string()))
    /// };
    /// ```
    pub unsafe fn new_unsized<SizedT: 'static>(value: Rc<RefCell<T>>) -> Self {
        Self {
            value,
            ty: TypeId::of::<SizedT>(),
            cur_state: Rc::new(Cell::new(1)),
            read_state: Cell::new(0),
            no_update: false,
        }
    }

    #[track_caller]
    pub fn read<Ret>(&self, _dk: &DataKey, f: impl FnOnce(&T) -> Ret) -> Ret {
        let ret = f(&*self.value.borrow());
        self.read_state.set(self.cur_state.get());
        ret
    }

    #[track_caller]
    pub fn read_as<Ret, U: 'static>(
        &self,
        _dk: &DataKey,
        f: impl FnOnce(&U) -> Ret,
    ) -> Option<Ret> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Rc::as_ptr(&self.value);
        let value = unsafe { (ptr as *const RefCell<U>).as_ref().unwrap() };
        let ret = f(&*value.borrow());
        self.read_state.set(self.cur_state.get());
        Some(ret)
    }

    #[track_caller]
    pub unsafe fn read_unsafe<Ret>(&self, f: impl FnOnce(&T) -> Ret) -> Ret {
        let ret = f(&*self.value.borrow());
        self.read_state.set(self.cur_state.get());
        ret
    }

    #[track_caller]
    pub unsafe fn read_unsafe_as<Ret, U: 'static>(&self, f: impl FnOnce(&U) -> Ret) -> Option<Ret> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Rc::as_ptr(&self.value);
        let value = unsafe { (ptr as *const RefCell<U>).as_ref().unwrap() };
        let ret = f(&*value.borrow());
        self.read_state.set(self.cur_state.get());
        Some(ret)
    }

    #[track_caller]
    pub(crate) fn raw_read<Ret>(&self, f: impl FnOnce(&T) -> Ret) -> Ret {
        f(&*self.value.borrow_mut())
    }

    pub(crate) fn acquire(&self) -> std::cell::Ref<'_, T> {
        self.read_state.set(self.cur_state.get());
        self.value.borrow()
    }

    #[track_caller]
    pub fn write<Ret>(&self, _dk: &mut DataKey, f: impl FnOnce(&mut T) -> Ret) -> Ret {
        let ret = f(&mut *self.value.borrow_mut());
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        ret
    }

    #[track_caller]
    pub fn write_as<Ret, U: 'static>(
        &self,
        _dk: &mut DataKey,
        f: impl FnOnce(&mut U) -> Ret,
    ) -> Option<Ret> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Rc::as_ptr(&self.value);
        let value = unsafe { (ptr as *const RefCell<U>).as_ref().unwrap() };

        let ret = f(&mut *value.borrow_mut());
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        Some(ret)
    }

    #[track_caller]
    pub unsafe fn write_unsafe<Ret>(&self, f: impl FnOnce(&mut T) -> Ret) -> Ret {
        let ret = f(&mut *self.value.borrow_mut());
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        ret
    }

    #[track_caller]
    pub unsafe fn write_unsafe_as<Ret, U: 'static>(
        &self,
        f: impl FnOnce(&mut U) -> Ret,
    ) -> Option<Ret> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Rc::as_ptr(&self.value);
        let value = unsafe { (ptr as *const RefCell<U>).as_ref().unwrap() };

        let ret = f(&mut *value.borrow_mut());
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        Some(ret)
    }

    #[track_caller]
    pub(crate) fn raw_write<Ret>(&self, f: impl FnOnce(&mut T) -> Ret) -> Ret {
        f(&mut *self.value.borrow_mut())
    }

    pub(crate) fn acquire_mut(&self) -> std::cell::RefMut<'_, T> {
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        self.value.borrow_mut()
    }

    pub fn map<Ret: 'static>(&self, map: impl FnMut(&T) -> Ret + 'static) -> DataMap<T, Ret> {
        let RwData { value, cur_state, read_state, .. } = self.clone();
        let data = RwData {
            value,
            cur_state,
            read_state,
            ty: TypeId::of::<T>(),
            no_update: self.no_update,
        };

        DataMap { data, map: Rc::new(RefCell::new(map)) }
    }

    pub fn ptr_eq<U: ?Sized>(&self, other: &RwData<U>) -> bool {
        self.value.as_ptr().addr() == other.value.as_ptr().addr()
    }

    pub fn type_id(&self) -> TypeId {
        self.ty
    }

    pub fn data_is<U: 'static>(&self) -> bool {
        self.ty == TypeId::of::<U>()
    }

    pub fn try_downcast<U: 'static>(&self) -> Option<RwData<U>> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Rc::into_raw(self.value.clone());
        let value = unsafe { Rc::from_raw(ptr as *const RefCell<U>) };
        Some(RwData {
            value,
            cur_state: self.cur_state.clone(),
            read_state: Cell::new(self.cur_state.get()),
            ty: TypeId::of::<U>(),
            no_update: self.no_update,
        })
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Some types like [`Text`], and traits like [`Widget`] offer
    /// [`has_changed`](crate::widgets::Widget::has_changed) methods,
    /// you should try to determine what parts to look for changes.
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: Self::write
    /// [`write_as`]: Self::write_as
    /// [`read`]: Self::read
    /// [`has_changed`]: Self::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::widgets::Widget
    pub fn has_changed(&self) -> bool {
        self.read_state.get() < self.cur_state.get()
    }

    /// Simulates a [`read`] without actually reading
    ///
    /// This is useful if you want to tell Duat that you don't want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`DataKey`] available to [`read`] the value.
    ///
    /// [`read`]: Self::read
    /// [`has_changed`]: Self::has_changed
    pub fn declare_as_read(&self) {
        self.read_state.set(self.cur_state.get());
    }

    pub(crate) fn read_raw<Ret>(&self, f: impl FnOnce(&T) -> Ret) -> Ret {
        f(&*self.value.borrow())
    }
}

impl<T: ?Sized + 'static> RwData<T> {}

impl<T: ?Sized> Clone for RwData<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            ty: self.ty,
            cur_state: self.cur_state.clone(),
            read_state: Cell::new(self.cur_state.get()),
            no_update: self.no_update,
        }
    }
}

impl<T: Default + 'static> Default for RwData<T> {
    fn default() -> Self {
        Self {
            value: Rc::default(),
            cur_state: Rc::new(Cell::new(1)),
            read_state: Cell::default(),
            ty: TypeId::of::<T>(),
            no_update: false,
        }
    }
}

pub struct DataMap<I: ?Sized + 'static, O: 'static> {
    data: RwData<I>,
    map: Rc<RefCell<dyn FnMut(&I) -> O>>,
}

impl<I: ?Sized + 'static, O> DataMap<I, O> {
    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Some types like [`Text`], and traits like [`Widget`] offer
    /// [`has_changed`](crate::widgets::Widget::has_changed) methods,
    /// you should try to determine what parts to look for changes.
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: RwData::write
    /// [`write_as`]: RwData::write_as
    /// [`read`]: RwData::read
    /// [`has_changed`]: RwData::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::widgets::Widget
    pub fn has_changed(&self) -> bool {
        self.data.has_changed()
    }
}

impl<I: ?Sized + 'static, O> Clone for DataMap<I, O> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            map: self.map.clone(),
        }
    }
}

impl<I: ?Sized + 'static, O: 'static> DataMap<I, O> {
    pub fn map<O2>(&self, mut f: impl FnMut(O) -> O2 + 'static) -> DataMap<I, O2> {
        let data_map = self.clone();
        data_map
            .data
            .clone()
            .map(move |input| f(data_map.map.borrow_mut()(input)))
    }
}

impl<I: ?Sized + 'static, O: 'static> FnOnce<(&DataKey<'_>,)> for DataMap<I, O> {
    type Output = O;

    extern "rust-call" fn call_once(self, (key,): (&DataKey,)) -> Self::Output {
        self.data.read(key, |input| self.map.borrow_mut()(input))
    }
}

impl<I: ?Sized + 'static, O: 'static> FnMut<(&DataKey<'_>,)> for DataMap<I, O> {
    extern "rust-call" fn call_mut(&mut self, (key,): (&DataKey,)) -> Self::Output {
        self.data.read(key, |input| self.map.borrow_mut()(input))
    }
}

/// A key for reading/writing to [`RwData`]
///
/// This key is necessary in order to prevent breakage of the number
/// one rule of Rust: any number of shared references, or one
/// exclusive reference.
///
/// When you call [`RwData::read`], any call to [`RwData::write`] may
/// end up breaking this rule, and vice-versa, which is why this
/// struct is necessary.
///
/// One downside of this approach is that it is even more restrictive
/// than Rust's rule of thumb, since that one is enforced on
/// individual instances, while this one is enforced on all
/// [`RwData`]s. This (as far as i know) cannot be circumvented, as a
/// more advanced compile time checker (that distinguishes
/// [`RwData<T>`]s of different `T`s, for example) does not seem
/// feasible without the use of unfinished features, which I am not
/// willing to use.
///
/// Do note that you can still ignore this if you want, by use of
/// [`read_unsafe`] and [`write_unsafe`], but this might cause a panic
/// in Duat because of the [`RefCell`]s under the hood.
///
/// [`read_unsafe`]: RwData::read_unsafe
/// [`write_unsafe`]: RwData::write_unsafe
pub struct DataKey<'a>(PhantomData<&'a Rc<RefCell<()>>>);

impl DataKey<'_> {
    /// Returns a new instance of [`DataKey`]
    ///
    /// Be careful when using this!
    pub(crate) unsafe fn new() -> Self {
        DataKey(PhantomData)
    }
}
