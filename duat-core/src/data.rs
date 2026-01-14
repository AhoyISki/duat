//! Duat's way of sharing and updating state
//!
//! This module consists primarily of the [`RwData`] struct, which
//! holds state that can be [read] or [written to]. When it is
//! modified, other holders of a clone of [`RwData`] will know that
//! the data within has been modified.
//!
//! This is used in many places, for example, [`Widget`]s can read
//! from [`Buffer`]s, and Duat can know when a [`Buffer`] has been
//! altered, so these [`Widget`]s may be [updated] automatically.
//!
//! Another struct from this module is [`DataMap`]. This is
//! essentially a mapping for a [`RwData`]. It is created via
//! [`RwData::map`], and both it and [`RwData`] can be very useful in,
//! for example, a [`StatusLine`], since it will be updated
//! automatically whenever the [`RwData`] is altered.
//!
//! One thing to note is that these structs are only meant to exist
//! and be used in the main thread of execution in Duat. In fact, it
//! should be impossible to acquire them outside of this main thread
//! without use of unsafe code. If it is still possible, report that
//! as a bug.
//!
//! The reason why these structs should only be valid in the main
//! thread is because, internally, they use non [`Send`]/[`Sync`]
//! structs, specifically [`RefCell`] and [`UnsafeCell`].
//! These are often considered "crutches" by a lot of the Rust
//! community, but in an environment where most of the code is
//! supposed to be able to access most of the state, it is impossible
//! to go without using them.
//!
//! The use of [`UnsafeCell`] internally also makes the [`read`] and
//! [`write`] operations _basically_ 0 cost,
//!
//! [read]: RwData::read
//! [written to]: RwData::write
//! [`Widget`]: crate::ui::Widget
//! [`Buffer`]: crate::buffer::Buffer
//! [updated]: crate::ui::Widget::update
//! [`Text`]: crate::text::Text
//! [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
//! [`context`]: crate::context
//! [`Mutex`]: std::sync::Mutex
//! [`read`]: RwData::read
//! [`write`]: RwData::write
use std::{
    self,
    any::TypeId,
    cell::{RefCell, UnsafeCell},
    marker::PhantomData,
    sync::{
        Arc, LazyLock, Mutex,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    time::Duration,
};

use crate::ui::Widget;

/// A container for shared read/write state
///
/// This is the struct used internally (and externally) to allow for
/// massively shareable state in duat's API. Its main purpose is to
/// hold all of the [`Widget`]s in Duat, making them available for
/// usage from any function with access to a [`Pass`]. However, it can
/// also be used to hold any other type, and also has the ability to
/// notify when changes have taken place.
///
/// # [`Pass`]es
///
/// The `Pass` is a sort of "key" for accessing the value within an
/// `RwData`, its purpose is to maintain Rust's number one rule,
/// i.e. one exclusive reference or multiple shared references
/// (mutability XOR aliasing), and that is done by borrowing the
/// `Pass` mutably or non mutably. That comes with some limitations
/// on how they can be used, mostly the fact that you must mutably
/// borrow all `RwData`s that will be used [_at the same time_] in
/// order to get multiple mutable references at once.
///
/// The use of a `Pass` for reading/writing to `RwData`s confers
/// various benefits:
///
/// - Aside from updating an internal update counter, it is truly
///   _zero cost_, unlike in the case of a [`Mutex`] or [`RefCell`],
///   which have to do checks at runtime. This happens because the
///   `Pass` has zero size, i.e. it gets removed at compile time.
/// - You can't run into deadlocks like you would be able to when
///   using `Mutex`es. Neither can you run into panics from
///   reborrowing, like with `RefCell`.
/// - You don't have to drop a `Guard` type (like [`MutexGuard`]) in
///   order to reborrow from the `RwData` since borrowing gives you a
///   first class `&` or `&mut`, which are much easier to work with.
/// - You can do sub borrowings, like `&mut data.write(pa).field`,
///   given the `&mut` borrow.
///
/// However, there are also a few disadvantages:
///
/// - Sometimes, mutably borrowing multiple things in a single
///   function _can_ be a challenge, although that is mostly mitigated
///   by [`Pass::write_many`].
/// - You _cannot_ access the data in a `RwData` from threads other
///   than the main one, since the `Pass` is only accessible from it.
///   This isn't _really_ a disadvantage, since it simplifies thought
///   patterns and eases reasoning about the current state of things.
///
/// [`Mutex`]: std::sync::Mutex
/// [`MutexGuard`]: std::sync::MutexGuard
/// [parser]: crate::buffer::BufferTracker
/// [`Text`]: crate::text::Text
/// [_at the same time_]: Pass::write_many
#[derive(Debug)]
pub struct RwData<T: ?Sized> {
    value: Arc<UnsafeCell<T>>,
    cur_state: Arc<AtomicUsize>,
    read_state: Arc<AtomicUsize>,
    ty: TypeId,
}

impl<T: 'static> RwData<T> {
    /// Returns a new [`RwData<T>`]
    ///
    /// Note that this is only for sized types. For unsized types, the
    /// process is a little more convoluted, and you need to use
    /// [`RwData::new_unsized`].
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(UnsafeCell::new(value)),
            ty: TypeId::of::<T>(),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: Arc::new(AtomicUsize::new(0)),
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
    /// # duat_core::doc_duat!(duat);
    /// use std::{cell::UnsafeCell, fmt::Display, sync::Arc};
    ///
    /// use duat::{data::RwData, prelude::*};
    /// let rw_data: RwData<dyn Display> =
    ///     unsafe { RwData::new_unsized::<String>(Arc::new(UnsafeCell::new("test".to_string()))) };
    /// ```
    ///
    /// This ensures that methods such as [`read_as`] and [`write_as`]
    /// will correctly identify such [`RwData<dyn Display>`] as a
    /// [`String`].
    ///
    /// [`read_as`]: Self::read_as
    /// [`write_as`]: Self::write_as
    #[doc(hidden)]
    pub unsafe fn new_unsized<SizedT: 'static>(value: Arc<UnsafeCell<T>>) -> Self {
        Self {
            value,
            ty: TypeId::of::<SizedT>(),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: Arc::new(AtomicUsize::new(0)),
        }
    }

    ////////// Reading functions

    /// Reads the value within using a [`Pass`]
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    pub fn read<'p>(&'p self, _: &'p Pass) -> &'p T {
        self.read_state
            .store(self.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
        // SAFETY: If one were to try and write to this value, this reference
        // would instantly become invalid, and trying to read from it again
        // would cause a compile error due to a Pass borrowing conflict.
        unsafe { &*self.value.get() }
    }

    /// Reads the value within as `U` using a [`Pass`]
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    pub fn read_as<'p, U: 'static>(&'p self, _: &'p Pass) -> Option<&'p U> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        self.read_state
            .store(self.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);

        let ptr = Arc::as_ptr(&self.value) as *const UnsafeCell<U>;

        // SAFETY: Same as above, but also, the TypeId in the Handle
        // "guarantees" that this is the correct type.
        Some(unsafe { &*(&*ptr).get() })
    }

    /// Simulates a [`read`] without actually reading
    ///
    /// This is useful if you want to tell Duat that you don't want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`Pass`] available to [`read`] the value.
    ///
    /// [`read`]: Self::read
    /// [`has_changed`]: Self::has_changed
    pub fn declare_as_read(&self) {
        self.read_state
            .store(self.cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
    }

    ////////// Writing functions

    /// Writes to the value within using a [`Pass`]
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    pub fn write<'p>(&'p self, _: &'p mut Pass) -> &'p mut T {
        let prev = self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.read_state.store(prev + 1, Ordering::Relaxed);
        // SAFETY: Again, the mutable reference to the Pass ensures that this
        // is the only _valid_ mutable reference, if another reference,
        // created prior to this one, were to be reused, that would be a
        // compile error.
        unsafe { &mut *self.value.get() }
    }

    /// Writes to the value within as `U` using a [`Pass`]
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    pub fn write_as<'p, U: 'static>(&'p self, _: &'p mut Pass) -> Option<&'p mut U> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let prev = self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.read_state.store(prev + 1, Ordering::Relaxed);

        let ptr = Arc::as_ptr(&self.value) as *const UnsafeCell<U>;

        // SAFETY: Same as above, but also, the TypeId in the Handle
        // "guarantees" that this is the correct type.
        Some(unsafe { &mut *(&*ptr).get() })
    }

    /// Writes to the value _and_ internal [`RwData`]-like structs
    ///
    /// This method takes a function that borrows a [`WriteableTuple`]
    /// from `self`, letting you write to `self` and the data in the
    /// tuple (or single element) at the same time.
    ///
    /// This is _really_ useful in a scenario where, for example, your
    /// [`Handle<W>`] for some widget `W` holds a [`Handle<Buffer>`],
    /// and you wish to access both at the same time, while writing to
    /// the former:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::prelude::*;
    ///
    /// struct MyWidget {
    ///     text: Text,
    ///     buf: Handle,
    /// }
    ///
    /// impl Widget for MyWidget {
    ///     fn update(pa: &mut Pass, handle: &Handle<Self>) {
    ///         let (wid, buf) = handle.write_then(pa, |wid| &wid.buf);
    ///         // Updating the widget and reading/writing from the Buffer at the same time.
    ///         // ...
    ///     }
    ///     // ..
    ///     # fn text(&self) -> &Text { &self.text }
    ///     # fn text_mut(&mut self) -> TextMut { self.text.as_mut() }
    ///     # fn needs_update(&self, pa: &Pass) -> bool { false }
    /// }
    /// ```
    ///
    /// You can also return tuples from the function, allowing for
    /// access to up to twelve different [`RwData`]-like structs:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::prelude::*;
    ///
    /// struct MyWidget {
    ///     text: Text,
    ///     buf1: Handle,
    ///     buf2: Handle,
    /// }
    ///
    /// impl Widget for MyWidget {
    ///     fn update(pa: &mut Pass, handle: &Handle<Self>) {
    ///         let (wid, (b1, b2)) = handle.write_then(pa, |wid| (&wid.buf1, &wid.buf2));
    ///         // ...
    ///     }
    ///     // ..
    ///     # fn text(&self) -> &Text { &self.text }
    ///     # fn text_mut(&mut self) -> TextMut { self.text.as_mut() }
    ///     # fn needs_update(&self, pa: &Pass) -> bool { false }
    /// }
    /// ```
    ///
    /// # Panics
    ///
    /// This function will panic if any of the elements of the tuple
    /// point to the same data as any other element or `self`, see
    /// [`Pass::write_many`] for more information.
    ///
    /// [`Handle<W>`]: crate::context::Handle
    /// [`Handle<Buffer>`]: crate::context::Handle
    #[track_caller]
    #[allow(static_mut_refs)]
    pub fn write_then<'p, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p self,
        pa: &'p mut Pass,
        tup_fn: impl FnOnce(&'p T) -> Tup,
    ) -> (&'p mut T, Tup::Return) {
        let tup = tup_fn(self.read(pa));
        if tup
            .state_ptrs()
            .into_iter()
            .any(|ptr| ptr == CurStatePtr(&self.cur_state))
        {
            panic!("Tried writing to the same data multiple times at the same time");
        }

        /// SAFETY: The ptrs are already verifiably not pointing to
        /// the data of self.
        static PASS: Pass = unsafe { Pass::new() };

        let tup_ret = unsafe { (&raw const PASS as *mut Pass).as_mut() }
            .unwrap()
            .write_many(tup);

        let value = self.write(unsafe { (&raw const PASS as *mut Pass).as_mut() }.unwrap());

        (value, tup_ret)
    }

    /// Simulates a [`write`] without actually writing
    ///
    /// This is useful if you want to tell Duat that you want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`Pass`] available to [`write`] the value with.
    ///
    /// [`write`]: Self::write
    /// [`has_changed`]: Self::has_changed
    pub fn declare_written(&self) {
        let prev = self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.read_state.store(prev + 1, Ordering::Relaxed);
    }

    /// Takes the value within, replacing it with the default
    pub fn take(&self, pa: &mut Pass) -> T
    where
        T: Default,
    {
        std::mem::take(self.write(pa))
    }

    ////////// Mapping of the inner value

    /// Maps the value to another value with a function
    ///
    /// This function will return a struct that acts like a "read
    /// only" version of [`RwData`], which also maps the value to
    /// a return type.
    pub fn map<Ret: 'static>(&self, map: impl FnMut(&T) -> Ret + 'static) -> DataMap<T, Ret> {
        let RwData { value, cur_state, .. } = self.clone();
        let data = RwData {
            value,
            cur_state,
            read_state: Arc::new(AtomicUsize::new(self.cur_state.load(Ordering::Relaxed))),
            ty: TypeId::of::<T>(),
        };

        DataMap { data, map: Arc::new(RefCell::new(map)) }
    }

    /// Maps the value to another value with a mutating function
    ///
    /// This is useful if you want to repeat a function over and over
    /// again in order to get a new different result, whilst mutating
    /// the data within.
    pub fn map_mut<Ret: 'static>(
        &self,
        map: impl FnMut(&mut T) -> Ret + 'static,
    ) -> MutDataMap<T, Ret> {
        let RwData { value, cur_state, .. } = self.clone();
        let data = RwData {
            value,
            cur_state,
            read_state: Arc::new(AtomicUsize::new(self.cur_state.load(Ordering::Relaxed))),
            ty: TypeId::of::<T>(),
        };

        MutDataMap { data, map: Arc::new(RefCell::new(map)) }
    }

    /// Attempts to downcast an [`RwData`] to a concrete type
    ///
    /// Returns [`Some(RwData<U>)`] if the value within is of type
    /// `U`. For unsized types, `U` is the type parameter
    /// passed when calling [`RwData::new_unsized`].
    ///
    /// [`Some(RwData<U>)`]: Some
    pub fn try_downcast<U: 'static>(&self) -> Option<RwData<U>> {
        if TypeId::of::<U>() != self.ty {
            return None;
        }

        let ptr = Arc::into_raw(self.value.clone());
        // SAFETY: TypeId argument "guarantees" this
        let value = unsafe { Arc::from_raw(ptr as *const UnsafeCell<U>) };
        Some(RwData {
            value,
            cur_state: self.cur_state.clone(),
            read_state: Arc::new(AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1)),
            ty: TypeId::of::<U>(),
        })
    }

    ////////// Querying functions

    /// Wether this [`RwData`] and another point to the same value
    pub fn ptr_eq<U: ?Sized>(&self, other: &RwData<U>) -> bool {
        Arc::ptr_eq(&self.cur_state, &other.cur_state)
    }

    /// The [`TypeId`] of the concrete type within
    pub fn type_id(&self) -> TypeId {
        self.ty
    }

    /// Wether the concrete [`TypeId`] matches that of `U`
    pub fn is<U: 'static>(&self) -> bool {
        self.ty == TypeId::of::<U>()
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or `write`
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Some types like [`Text`], and traits like [`Widget`] offer
    /// [`has_changed`](crate::ui::Widget::needs_update) methods,
    /// you should try to determine what parts to look for changes.
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: Self::write
    /// [`write_as`]: Self::write_as
    /// [`read`]: Self::read
    /// [`has_changed`]: Self::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::ui::Widget
    pub fn has_changed(&self) -> bool {
        self.read_state.load(Ordering::Relaxed) < self.cur_state.load(Ordering::Relaxed)
    }

    /// A function that checks if the data has been updated
    ///
    /// Do note that this function will check for the specific
    /// [`RwData`] that was used in its creation, so if you call
    /// [`read`] on that specific [`RwData`] for example, this
    /// function will start returning `false`.
    ///
    /// [`read`]: Self::read
    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static {
        let (read, cur) = (self.read_state.clone(), self.cur_state.clone());
        move || read.load(Ordering::Relaxed) < cur.load(Ordering::Relaxed)
    }
}

impl<W: Widget> RwData<W> {
    /// Downcasts [`RwData<impl Widget>`] to [`RwData<dyn Widget>`]
    pub fn to_dyn_widget(&self) -> RwData<dyn Widget> {
        let ptr = Arc::into_raw(self.value.clone());
        // SAFETY: Implements Widget
        let value = unsafe { Arc::from_raw(ptr as *const UnsafeCell<dyn Widget>) };
        RwData {
            value,
            cur_state: self.cur_state.clone(),
            read_state: Arc::new(AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1)),
            ty: self.ty,
        }
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<T: ?Sized> Send for RwData<T> {}
unsafe impl<T: ?Sized> Sync for RwData<T> {}

impl<T: ?Sized> Clone for RwData<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            ty: self.ty,
            cur_state: self.cur_state.clone(),
            read_state: Arc::new(AtomicUsize::new(self.cur_state.load(Ordering::Relaxed) - 1)),
        }
    }
}

impl<T: Default + 'static> Default for RwData<T> {
    fn default() -> Self {
        Self {
            value: Arc::default(),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: Arc::new(AtomicUsize::new(0)),
            ty: TypeId::of::<T>(),
        }
    }
}

/// A mapping of an [`RwData`]
pub struct DataMap<I: ?Sized + 'static, O: 'static> {
    data: RwData<I>,
    map: Arc<RefCell<dyn FnMut(&I) -> O>>,
}

impl<I: ?Sized, O> DataMap<I, O> {
    /// Call this `DataMap`'s mapping function, returning the output
    pub fn call(&self, pa: &Pass) -> O {
        self.map.borrow_mut()(self.data.read(pa))
    }

    /// Maps the value within, works just like [`RwData::map`]
    pub fn map<O2>(self, mut f: impl FnMut(O) -> O2 + 'static) -> DataMap<I, O2> {
        self.data.map(move |input| f(self.map.borrow_mut()(input)))
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Some types like [`Text`], and traits like [`Widget`] offer
    /// [`needs_update`] methods, you should try to determine what
    /// parts to look for changes.
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: RwData::write
    /// [`write_as`]: RwData::write_as
    /// [`read`]: RwData::read
    /// [`has_changed`]: RwData::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::ui::Widget
    /// [`needs_update`]: crate::ui::Widget::needs_update
    pub fn has_changed(&self) -> bool {
        self.data.has_changed()
    }

    /// A function that checks if the data has been updated
    ///
    /// Do note that this function will check for the specific
    /// [`RwData`] that was used in its creation, so if you call
    /// [`read`] on that specific [`RwData`] for example, this
    /// function will start returning `false`.
    ///
    /// [`read`]: RwData::read
    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static {
        self.data.checker()
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<I: ?Sized + 'static, O: 'static> Send for DataMap<I, O> {}
unsafe impl<I: ?Sized + 'static, O: 'static> Sync for DataMap<I, O> {}

/// A mutable mapping of an [`RwData`]
///
/// This works very similarly to the [`DataMap`], except the function
/// is allowed to mutate the data, so it takes a `&mut Pass` instead
/// of a regular `&Pass`.
pub struct MutDataMap<I: ?Sized + 'static, O: 'static> {
    data: RwData<I>,
    map: Arc<RefCell<dyn FnMut(&mut I) -> O>>,
}

impl<I: ?Sized, O> MutDataMap<I, O> {
    /// Call this `DataMap`'s mapping function, returning the output
    pub fn call(&self, pa: &mut Pass) -> O {
        self.map.borrow_mut()(self.data.write(pa))
    }

    /// Maps the value within, works just like [`RwData::map`]
    pub fn map<O2>(self, mut f: impl FnMut(O) -> O2 + 'static) -> MutDataMap<I, O2> {
        self.data
            .map_mut(move |input| f(self.map.borrow_mut()(input)))
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Some types like [`Text`], and traits like [`Widget`] offer
    /// [`needs_update`] methods, you should try to determine what
    /// parts to look for changes.
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: RwData::write
    /// [`write_as`]: RwData::write_as
    /// [`read`]: RwData::read
    /// [`has_changed`]: RwData::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::ui::Widget
    /// [`needs_update`]: crate::ui::Widget::needs_update
    pub fn has_changed(&self) -> bool {
        self.data.has_changed()
    }

    /// A function that checks if the data has been updated
    ///
    /// Do note that this function will check for the specific
    /// [`RwData`] that was used in its creation, so if you call
    /// [`read`] on that specific [`RwData`] for example, this
    /// function will start returning `false`.
    ///
    /// [`read`]: RwData::read
    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static {
        self.data.checker()
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<I: ?Sized + 'static, O: 'static> Send for MutDataMap<I, O> {}
unsafe impl<I: ?Sized + 'static, O: 'static> Sync for MutDataMap<I, O> {}

/// A struct used for asynchronously mutating [`RwData`]s without a
/// [`Pass`]
///
/// This works by wrapping the `RwData` and collecting every mutating
/// function inside a separate [`Mutex`]. Whenever you access the
/// `Data`, the changes are applied to it.
///
/// With this in mind, one limitation of this type is that every
/// access must make use of a `&mut Pass`, with the exception of
/// [`BulkDataWriter::try_read`], which returns `Some` only when there
/// have been no changes to the `Data`.
#[derive(Default)]
pub struct BulkDataWriter<Data: Default + 'static> {
    actions: LazyLock<Arc<Mutex<Vec<Box<dyn FnOnce(&mut Data) + Send + 'static>>>>>,
    data: LazyLock<RwData<Data>>,
}

impl<Data: Default + 'static> BulkDataWriter<Data> {
    /// Returns a new `BulkDataWriter`
    ///
    /// Considering the fact that this struct is almost exclusively
    /// used in `static` variables, I have decided to make its
    /// constructor `const`, to facilitate its usage.
    #[allow(clippy::new_without_default)]
    pub const fn new() -> Self {
        Self {
            actions: LazyLock::new(|| Arc::new(Mutex::new(Vec::new()))),
            data: LazyLock::new(|| RwData::new(Data::default())),
        }
    }

    /// Adds a mutating function to the list of functions to call upon
    /// accessing the `Data`
    ///
    /// This is useful for allowing mutation from any thread, and
    /// without needing [`Pass`]es. `duat-core` makes extensive use of
    /// this function in order to provide pleasant to use APIs.
    pub fn mutate(&self, f: impl FnOnce(&mut Data) + Send + 'static) {
        self.actions.lock().unwrap().push(Box::new(f));
    }

    /// Accesses the `Data`, calling all added actions
    ///
    /// This function will call all actions that were sent by the
    /// [`BulkDataWriter::mutate`] function in order to write to the
    /// `Data` asynchronously.
    pub fn write<'p>(&'p self, pa: &'p mut Pass) -> &'p mut Data {
        let data = self.data.write(pa);
        for action in self.actions.lock().unwrap().drain(..) {
            action(data);
        }
        data
    }

    /// Attempts to read the `Data`
    ///
    /// This function will return [`None`] if there are pending
    /// actions that need to happen before reading/writing. You should
    /// almost always prefer calling [`BulkDataWriter::write`]
    /// instead.
    pub fn try_read<'p>(&'p self, pa: &'p Pass) -> Option<&'p Data> {
        self.actions
            .lock()
            .unwrap()
            .is_empty()
            .then(|| self.data.read(pa))
    }

    /// Maps the value to another value with a mutating function
    ///
    /// This will apply the delayed updting of
    /// [`BulkDataWriter::write`] every time the mapping is called, so
    /// the value always stays up to date.
    pub fn map_mut<Ret: 'static>(
        &self,
        mut map: impl FnMut(&mut Data) -> Ret + 'static,
    ) -> MutDataMap<Data, Ret> {
        let actions = self.actions.clone();
        self.data.map_mut(move |data| {
            for action in actions.lock().unwrap().drain(..) {
                action(data);
            }
            map(data)
        })
    }
}

/// A checking struct that periodically returns `true`
pub struct PeriodicChecker(Arc<AtomicBool>);

impl PeriodicChecker {
    /// Returns a new [`PeriodicChecker`]
    pub fn new(duration: Duration) -> Self {
        let has_elapsed = Arc::new(AtomicBool::new(false));
        std::thread::spawn({
            let has_elapsed = has_elapsed.clone();
            move || {
                while !crate::context::will_reload_or_quit() {
                    std::thread::sleep(duration);
                    has_elapsed.store(true, Ordering::Relaxed);
                }
            }
        });

        Self(has_elapsed)
    }

    /// Checks if the requested [`Duration`] has elapsed
    pub fn check(&self) -> bool {
        self.0.fetch_and(false, Ordering::Relaxed)
    }
}

impl Default for PeriodicChecker {
    fn default() -> Self {
        Self::new(Duration::from_secs(1))
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
pub struct Pass(PhantomData<()>);

impl Pass {
    /// Returns a new instance of [`Pass`]
    ///
    /// Be careful when using this!
    pub(crate) const unsafe fn new() -> Self {
        Pass(PhantomData)
    }

    /// Writes to many [`RwData`]-like structs at once
    ///
    /// This function accepts tuples (or a single element) of
    /// references to types that implement the [`WriteableData`]
    /// trait, which is one of the following:
    ///
    /// - [`RwData`]: Duat's regular smart pointer.
    /// - [`BulkDataWriter`]: A pointer to lazyly updated data.
    /// - [`Handle`]: A handle for a [`Widget`]
    /// - [`RwArea`]: A handle for a `Widget`'s [`Area`]
    ///
    /// Here's an example, which writes to two `RwData`s at the same
    /// time as a `Handle`:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::{data::RwData, prelude::*};
    /// setup_duat!(setup);
    ///
    /// fn setup() {
    ///     let (num1, num2) = (RwData::new(0), RwData::new(0));
    ///     hook::add::<BufferOpened>(move |pa, handle: &Handle| {
    ///         let (num1, num2, buf) = pa.write_many((&num1, &num2, handle));
    ///         // Rest of the function writes to all of them at the same time.
    ///     });
    /// }
    /// ```
    ///
    /// This allows for much more flexibility when writing to global
    /// state, which should hopefully lead to more streamlined
    /// functions
    ///
    /// # Panics
    ///
    /// This function will panic if any of the elements of the tuple
    /// point to the same data as any other element, for example, with
    /// the earlier code snippet:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::{data::RwData, prelude::*};
    /// setup_duat!(setup);
    ///
    /// fn setup() {
    ///     let num1 = RwData::new(0);
    ///     // num2 is now a clone of num1
    ///     let num2 = num1.clone();
    ///     hook::add::<BufferOpened>(move |pa, handle: &Handle| {
    ///         let (num1, num2, buf) = pa.write_many((&num1, &num2, handle));
    ///         // Rest of the function writes to all of them at the same time.
    ///     });
    /// }
    /// ```
    ///
    /// Since `num1` and `num2` point to the same data, you'd be
    /// getting two `&mut i32` for the same variable, which violates
    /// rust's "mutability xor aliasing" rule. This is why this will
    /// panic. If you want a non-panicking version of this function,
    /// check out [`Pass::try_write_many`], which returns a [`Result`]
    /// instead.
    ///
    /// [`Handle`]: crate::context::Handle
    /// [`RwArea`]: crate::ui::RwArea
    /// [`Area`]: crate::ui::Area
    #[track_caller]
    pub fn write_many<'p, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p mut self,
        tup: Tup,
    ) -> Tup::Return {
        if let Some(ret) = tup.write_all(self) {
            ret
        } else {
            panic!("Tried writing to the same data multiple times");
        }
    }

    /// Tries writing to many [`RwData`]-like structs at once
    ///
    /// This function accepts tuples (or a single element) of
    /// references to types that implement the [`WriteableData`]
    /// trait, which is one of the following:
    ///
    /// - [`RwData`]: Duat's regular smart pointer.
    /// - [`BulkDataWriter`]: A pointer to lazyly updated data.
    /// - [`Handle`]: A handle for a [`Widget`]
    /// - [`RwArea`]: A handle for a `Widget`'s [`Area`]
    ///
    /// This function works exactly like [`Pass::write_many`],
    /// however, instead of panicking, this function returns a
    /// [`Result`], returning an [`Err`] if any of the tuple's
    /// elements point to the same data as any of the other elements.
    ///
    /// [`Handle`]: crate::context::Handle
    /// [`RwArea`]: crate::ui::RwArea
    /// [`Area`]: crate::ui::Area
    pub fn try_write_many<'p, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p mut self,
        tup: Tup,
    ) -> Result<Tup::Return, crate::text::Text> {
        tup.write_all(self)
            .ok_or_else(|| crate::text::txt!("Tried writing to the same data multiple times"))
    }
}

/// A tuple of [`WriteableData`], used for writing to many things at
/// once
#[doc(hidden)]
pub trait WriteableTuple<'p, _Dummy> {
    type Return;

    #[doc(hidden)]
    fn write_all(self, pa: &'p mut Pass) -> Option<Self::Return>;

    #[doc(hidden)]
    fn state_ptrs(&self) -> impl IntoIterator<Item = CurStatePtr<'_>>;
}

macro_rules! implWriteableTuple {
    ($(($tup:ident, $dummy:ident)),+) => {
        #[allow(non_snake_case)]
        impl<'p, $($tup),+, $($dummy),+> WriteableTuple<'p, ($(&mut $dummy),+)> for ($($tup),+)
        where
            $($tup: WriteableTuple<'p, $dummy>),+
        {
            type Return = ($($tup::Return),+);

            fn write_all(self, _: &'p mut Pass) -> Option<Self::Return> {
                if self.state_ptrs().into_iter().enumerate().any(|(lhs, i)| {
                    self.state_ptrs()
                        .into_iter()
                        .enumerate()
                        .any(|(rhs, j)| lhs == rhs && i != j)
                }) {
                    return None;
                }

                let ($($tup),+) = self;

				/// SAFETY: The ptrs are already verifiably not pointing to the same data.
                static PASS: Pass = unsafe { Pass::new() };

                Some(($(
                    $tup.write_all(
                        unsafe { (&raw const PASS as *mut Pass).as_mut() }.unwrap()
                    )
                    .unwrap()
                ),+))
            }

            fn state_ptrs(&self) -> impl IntoIterator<Item = CurStatePtr<'_>> {
                let ($($tup),+) = self;

                implWriteableTuple!(@chain $($tup),+)
            }
        }
    };

    (@chain $tup:ident $(, $rest:ident)*) => {
        $tup.state_ptrs().into_iter().chain(implWriteableTuple!(@chain $($rest),*))
    };
    (@chain ) => { [] };
}

impl<'p, Data, T> WriteableTuple<'p, (&mut T,)> for &'p Data
where
    Data: WriteableData<'p, T>,
    T: ?Sized + 'p,
{
    type Return = &'p mut T;

    fn write_all(self, pa: &'p mut Pass) -> Option<Self::Return> {
        Some(self.write_one_of_many(pa))
    }

    fn state_ptrs(&self) -> impl IntoIterator<Item = CurStatePtr<'_>> {
        [self.cur_state_ptr()]
    }
}

implWriteableTuple!((D0, T0), (D1, T1));
implWriteableTuple!((D0, T0), (D1, T1), (D2, T2));
implWriteableTuple!((D0, T0), (D1, T1), (D2, T2), (D3, T3));
implWriteableTuple!((D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4));
implWriteableTuple!((D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5));
#[rustfmt::skip]
implWriteableTuple!((D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5), (D6, T6));
#[rustfmt::skip]
implWriteableTuple!((D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5), (D6, T6), (D7, T7));
#[rustfmt::skip]
implWriteableTuple!(
    (D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5), (D6, T6), (D7, T7) , (D8, T8)
);
#[rustfmt::skip]
implWriteableTuple!(
    (D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5), (D6, T6), (D7, T7) , (D8, T8),
    (D9, T9)
);
#[rustfmt::skip]
implWriteableTuple!(
    (D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5), (D6, T6), (D7, T7) , (D8, T8),
    (D9, T9), (D10, T10)
);
#[rustfmt::skip]
implWriteableTuple!(
    (D0, T0), (D1, T1), (D2, T2), (D3, T3), (D4, T4), (D5, T5), (D6, T6), (D7, T7) , (D8, T8),
    (D9, T9), (D10, T10), (D11, T11)
);

impl<'p, const N: usize, Tup, Dummy> WriteableTuple<'p, [Dummy; N]> for [Tup; N]
where
    Tup: WriteableTuple<'p, Dummy> + 'p,
{
    type Return = [Tup::Return; N];

    fn write_all(self, _: &'p mut Pass) -> Option<Self::Return> {
        if self.state_ptrs().into_iter().enumerate().any(|(lhs, i)| {
            self.state_ptrs()
                .into_iter()
                .enumerate()
                .any(|(rhs, j)| lhs == rhs && i != j)
        }) {
            return None;
        }

        static PASS: Pass = unsafe { Pass::new() };

        Some(self.map(|tup| {
            let pa = &raw const PASS as *mut Pass;
            tup.write_all(unsafe { pa.as_mut() }.unwrap()).unwrap()
        }))
    }

    fn state_ptrs(&self) -> impl IntoIterator<Item = CurStatePtr<'_>> {
        self.iter().flat_map(|tup| tup.state_ptrs())
    }
}

/// A trait for writing to multiple [`RwData`]-like structs at once
#[doc(hidden)]
pub trait WriteableData<'p, T: ?Sized + 'p>: InnerWriteableData {
    /// Just like [`RwData::write`]
    #[doc(hidden)]
    fn write_one_of_many(&'p self, pa: &'p mut Pass) -> &'p mut T;

    /// A pointer for [`Pass::try_write_many`]
    #[doc(hidden)]
    fn cur_state_ptr(&self) -> CurStatePtr<'_>;
}

impl<'p, T: ?Sized + 'p> WriteableData<'p, T> for RwData<T> {
    fn write_one_of_many(&'p self, pa: &'p mut Pass) -> &'p mut T {
        self.write(pa)
    }

    fn cur_state_ptr(&self) -> CurStatePtr<'_> {
        CurStatePtr(&self.cur_state)
    }
}

impl<'p, T: Default> WriteableData<'p, T> for BulkDataWriter<T> {
    fn write_one_of_many(&'p self, pa: &'p mut Pass) -> &'p mut T {
        self.write(pa)
    }

    fn cur_state_ptr(&self) -> CurStatePtr<'_> {
        CurStatePtr(&self.data.cur_state)
    }
}

impl<'p, W: Widget> WriteableData<'p, W> for crate::context::Handle<W> {
    fn write_one_of_many(&'p self, pa: &'p mut Pass) -> &'p mut W {
        self.write(pa)
    }

    fn cur_state_ptr(&self) -> CurStatePtr<'_> {
        CurStatePtr(&self.widget().cur_state)
    }
}

impl<'p> WriteableData<'p, crate::ui::Area> for crate::ui::RwArea {
    fn write_one_of_many(&'p self, pa: &'p mut Pass) -> &'p mut crate::ui::Area {
        self.write(pa)
    }

    fn cur_state_ptr(&self) -> CurStatePtr<'_> {
        CurStatePtr(&self.0.cur_state)
    }
}

/// To prevent outside implementations
trait InnerWriteableData {}
impl<T: ?Sized> InnerWriteableData for RwData<T> {}
impl<T: Default> InnerWriteableData for BulkDataWriter<T> {}
impl<W: Widget> InnerWriteableData for crate::context::Handle<W> {}
impl InnerWriteableData for crate::ui::RwArea {}

/// A struct for comparison when calling [`Pass::write_many`]
#[doc(hidden)]
#[derive(Clone, Copy)]
pub struct CurStatePtr<'p>(&'p Arc<AtomicUsize>);

impl std::cmp::PartialEq for CurStatePtr<'_> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(self.0, other.0)
    }
}
