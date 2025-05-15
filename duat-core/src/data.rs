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
    any::{Any, TypeId},
    cell::{Cell, RefCell},
    marker::PhantomData,
    ptr::DynMetadata,
    rc::Rc,
};

pub struct RwData2<T: ?Sized> {
    value: Rc<RefCell<T>>,
    cur_state: Rc<Cell<usize>>,
    read_state: Cell<usize>,
    ty: TypeId,
    no_update: bool,
}

impl<T: 'static> RwData2<T> {
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

impl<T: ?Sized> RwData2<T> {
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
    pub fn read<Ret>(&self, f: impl FnOnce(&T) -> Ret) -> Ret {
        let ret = f(&*self.value.borrow());
        self.read_state.set(self.cur_state.get());
        ret
    }

    #[track_caller]
    pub fn read_as<Ret, U: 'static>(&self, f: impl FnOnce(&U) -> Ret) -> Option<Ret> {
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
    pub fn write<Ret>(&self, f: impl FnOnce(&mut T) -> Ret) -> Ret {
        let ret = f(&mut *self.value.borrow_mut());
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        ret
    }

    #[track_caller]
    pub fn write_as<Ret, U: 'static>(&self, f: impl FnOnce(&mut U) -> Ret) -> Option<Ret> {
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

    pub fn map<Ret>(&self, f: impl FnMut(&T) -> Ret + 'static) -> DataMap<T, Ret> {
        let RwData2 { value, cur_state, read_state, .. } = self.clone();
        let data = RwData2 {
            value,
            cur_state,
            read_state,
            ty: TypeId::of::<T>(),
            no_update: self.no_update,
        };

        // SAFETY: std::mem::transmute just reinterprets the bytes, so in
        // theory, I should be able re-reinterpret the bytes from the other
        // end.
        let masked_rc = unsafe {
            let rc: Rc<RefCell<dyn FnMut(&T) -> Ret>> = Rc::new(RefCell::new(f));

            std::mem::transmute(Some(rc))
        };

        DataMap { data, masked_rc, _ghost: PhantomData }
    }

    pub(crate) fn acquire_mut(&self) -> std::cell::RefMut<'_, T> {
        self.cur_state.set(self.cur_state.get() + 1);
        self.read_state.set(self.cur_state.get());
        self.value.borrow_mut()
    }

    pub fn ptr_eq<U: ?Sized>(&self, other: &RwData2<U>) -> bool {
        self.value.as_ptr().addr() == other.value.as_ptr().addr()
    }

    pub fn type_id(&self) -> TypeId {
        self.ty
    }

    pub fn data_is<U: 'static>(&self) -> bool {
        self.ty == TypeId::of::<U>()
    }

    pub fn try_downcast<U: 'static>(&self) -> Option<RwData2<U>> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Rc::into_raw(self.value.clone());
        let value = unsafe { Rc::from_raw(ptr as *const RefCell<U>) };
        Some(RwData2 {
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

    pub(crate) fn read_raw<Ret>(&self, f: impl FnOnce(&T) -> Ret) -> Ret {
        f(&*self.value.borrow())
    }
}

impl<T: ?Sized + 'static> RwData2<T> {}

impl<T: ?Sized> Clone for RwData2<T> {
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

impl<T: Default + 'static> Default for RwData2<T> {
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

pub struct DataMap<I: ?Sized + 'static, O> {
    data: RwData2<I>,
    masked_rc: Option<Rc<RefCell<dyn Any>>>,
    _ghost: PhantomData<fn(O)>,
}

impl<I: ?Sized + 'static, O> DataMap<I, O> {
    /// Reads the output value, mapping it to something
    #[track_caller]
    pub fn read<Ret>(&mut self, f: impl FnOnce(O) -> Ret) -> Ret {
        self.data.read(|input| {
            // SAFETY: This is its real type, the other type is just a mask
            let real_rc: Rc<RefCell<dyn FnMut(&I) -> O>> =
                unsafe { std::mem::transmute(self.masked_rc.take().unwrap()) };

            let ret = f(real_rc.borrow_mut()(input));

            // SAFETY: Again, this is the masked type
            self.masked_rc = Some(unsafe { std::mem::transmute(real_rc) });

            ret
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
    /// [`write`]: RwData2::write
    /// [`write_as`]: RwData2::write_as
    /// [`read`]: RwData2::read
    /// [`has_changed`]: RwData2::has_changed
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
            masked_rc: self.masked_rc.clone(),
            _ghost: PhantomData,
        }
    }
}

impl<I: ?Sized + 'static, O: 'static> DataMap<I, O> {
    pub fn map<O2>(mut self, mut f: impl FnMut(O) -> O2 + 'static) -> DataMap<I, O2> {
        self.data.clone().map(move |input| {
            // SAFETY: This is its real type, the other type is just a mask
            let real_rc: Rc<RefCell<dyn FnMut(&I) -> O>> =
                unsafe { std::mem::transmute(self.masked_rc.take().unwrap()) };

            let ret = f(real_rc.borrow_mut()(input));

            // SAFETY: Again, this is the masked type
            self.masked_rc = Some(unsafe { std::mem::transmute(real_rc) });

            ret
        })
    }
}

impl<I: ?Sized + 'static, O: 'static> FnOnce<()> for DataMap<I, O> {
    type Output = O;

    extern "rust-call" fn call_once(mut self, _: ()) -> Self::Output {
        // SAFETY: This is its real type, the other type is just a mask
        let real_rc: Rc<RefCell<dyn FnMut(&I) -> O>> =
            unsafe { std::mem::transmute(self.masked_rc.take().unwrap()) };

        self.data.read(&mut *real_rc.borrow_mut())
    }
}

impl<I: ?Sized + 'static, O: 'static> FnMut<()> for DataMap<I, O> {
    extern "rust-call" fn call_mut(&mut self, _: ()) -> Self::Output {
        // SAFETY: This is its real type, the other type is just a mask
        let real_rc: Rc<RefCell<dyn FnMut(&I) -> O>> =
            unsafe { std::mem::transmute(self.masked_rc.take().unwrap()) };

        let ret = self.data.read(&mut *real_rc.borrow_mut());

        // SAFETY: Again, this is the masked type
        self.masked_rc = Some(unsafe { std::mem::transmute(real_rc) });

        ret
    }
}

impl<I: ?Sized + 'static, O> Drop for DataMap<I, O> {
    fn drop(&mut self) {
        // SAFETY: This is its real type, the other type is just a mask
        let real_rc: Rc<RefCell<dyn FnMut(&I) -> O>> =
            unsafe { std::mem::transmute(self.masked_rc.take()) };

        // We have to drop this rc specifically, since it contains all of the
        // proper information fro dropping the inner function.
        drop(real_rc)
    }
}
