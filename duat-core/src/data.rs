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
//! One thing to note is that these structs are only meant to exist
//! and be used in the main thread of execution in Duat. In fact, it
//! should be impossible to acquire them outside of this main thread
//! without use of unsafe code. If it is still possible, report that
//! as a bug.
//!
//! The reason why these structs should only be valid in the main
//! thread is because, internally, they use non [`Send`]/[`Sync`]
//! structs, specifically [`Arc`] and [`RefCell`]. These are often
//! considered "crutches" by a lot of the Rust community, but in an
//! environment where most of the code is supposed to be able to
//! access most of the state, it is impossible to go without using
//! them.
//!
//! As well as not having the problem of deadlocks that [`Mutex`]es
//! would have, [`Arc<RefCell>`] is way faster to clone and lock than
//! its [`Sync`] equivalents [`Arc<Mutex>`] or [`Arc<RwLock>`].
//!
//! [read]: RwData::read
//! [written to]: RwData::write
//! [`Widget`]: crate::ui::Widget
//! [`File`]: crate::file::File
//! [updated]: crate::ui::Widget::update
//! [`Text`]: crate::text::Text
//! [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
//! [`context`]: crate::context
//! [`Mutex`]: std::sync::Mutex
//! [`Arc<Mutex>`]: std::sync::Arc
//! [`Arc<RwLock>`]: std::sync::Arc
use std::{
    self,
    any::TypeId,
    cell::{RefCell, UnsafeCell},
    marker::PhantomData,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    time::Duration,
};

use crate::ui::{Ui, Widget};

/// A container for shared read/write state
///
/// This is the struct used internally (and externally) to allow for
/// massively shareable state in duat's API. Its main purpose is to
/// hold all of the [`Widget`]s in Duat, making them available for
/// usage from any function with access to a [`Pass`].
///
/// # [`Pass`]es
///
/// The [`Pass`] is a sort of "key" for accessing the value within an
/// [`RwData`], it's purpose is to maintain Rust's number one rule,
/// i.e. one exclusive reference or multiple shared references, and
/// that is done by borrowing the [`Pass`] mutably or non mutably.
/// That comes with some limitations, of course, mainly that you can't
/// really mutate two [`RwData`]s at the same time, even if it is
/// known that they don't point to the same data.
///
/// There are some common exceptions to this, where Duat provides some
/// safe way to do that when it is known that the two types are not
/// the same.
///
/// # Not [`Send`]/[`Sync`]
///
/// Internally, the [`RwData`] makes use of an [`Arc<RefCell>`]. The
/// usage of an [`Arc<RefCell>`] over an [`Arc<Mutex>`] is, i've
/// assumed, a necessary evil in order to preserve the aforementioned
/// rule. But the lack of [`Send`]/[`Sync`] does confer the [`RwData`]
/// some advantages:
///
/// * Deadlocks are impossible, being replaced by much easier to debug
///   panics.
/// * The order in which data is accessed doesn't matter, unlike with
///   [`Mutex`]es.
/// * Performance of unlocking and cloning should generally be better,
///   since no atomic operations are done (I actually managed to
///   observe this, where in my rudimentary benchmarks against neovim,
///   the [`Arc<Mutex>`] version was very frequently losing to a
///   comparable neovim build.
///
/// However, I admit that there are also some drawbacks, the most
/// notable being the difficulty of reading or writing to [`Text`]
/// from outside of the main thread. But for the most common usecase
/// where that will be needed ([`Parser`]s), a [`Send`]/[`Sync`]
/// solution will be provided soon.
///
/// [`Arc<Mutex>`]: std::sync::Arc
/// [`Mutex`]: std::sync::Mutex
/// [`Parser`]: crate::file::Parser
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
    /// use std::{cell::RefCell, fmt::Display, rc::Arc};
    ///
    /// use duat_core::{data::RwData, prelude::*};
    /// let rw_data: RwData<dyn Display> =
    ///     unsafe { RwData::new_unsized::<String>(Arc::new(RefCell::new("testing".to_string()))) };
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
    pub fn read<'a>(&'a self, _: &'a Pass) -> &'a T {
        update_read_state(&self.read_state, &self.cur_state);
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
    pub fn read_as<'a, U: 'static>(&'a self, _: &'a Pass) -> Option<&'a U> {
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

    /// Reads the data without updating its read state, might be
    /// removed, idk
    pub(crate) fn read_raw<'a>(&'a self, _: &'a Pass) -> &'a T {
        // SAFETY: Same as Self::read
        unsafe { &*self.value.get() }
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
    pub fn write<'a>(&'a self, _: &'a mut Pass) -> &'a mut T {
        update_cur_state(&self.read_state, &self.cur_state);
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
    pub fn write_as<'a, U: 'static>(&'a self, _: &'a mut Pass) -> Option<&'a mut U> {
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

    /// Simulates a [`write`] without actually writing
    ///
    /// This is useful if you want to tell Duat that you want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`Pass`] available to [`write`] the value with.
    ///
    /// [`write`]: Self::write
    /// [`has_changed`]: Self::has_changed
    pub fn declare_written(&self) {
        update_cur_state(&self.read_state, &self.cur_state);
    }

    ////////// Mapping of the inner value

    /// Maps the value to another value with a function
    ///
    /// This function will return a struct that acts like a "read
    /// only" version of [`RwData`], which also maps the value to
    /// a return type.
    pub fn map<Ret: 'static>(
        &self,
        _: &Pass,
        map: impl FnMut(&T) -> Ret + 'static,
    ) -> DataMap<T, Ret> {
        let RwData { value, cur_state, read_state, .. } = self.clone();
        let data = RwData {
            value,
            cur_state,
            read_state,
            ty: TypeId::of::<T>(),
        };

        DataMap { data, map: Arc::new(RefCell::new(map)) }
    }

    /// Attempts to downcast an [`RwData`] to a concrete type
    ///
    /// Returns [`Some(RwData<U>)`] if the value within was of type
    /// `U`, i.e., for unsized types, `U` was the type parameter
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
        Arc::as_ptr(&self.value).addr() == Arc::as_ptr(&other.value).addr()
    }

    /// The [`TypeId`] of the concrete type within
    pub fn type_id(&self) -> TypeId {
        self.ty
    }

    /// Wether the concrete [`TypeId`] matches that of `U`
    pub fn data_is<U: 'static>(&self) -> bool {
        self.ty == TypeId::of::<U>()
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
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
        let (cur, read) = (self.cur_state.clone(), self.read_state.clone());
        move || read.load(Ordering::Relaxed) < cur.load(Ordering::Relaxed)
    }
}

impl<W> RwData<W> {
    /// Downcasts [`RwData<impl Widget<U>`] to [`RwData<dyn
    /// Widget<U>>`]
    pub fn to_dyn_widget<U: Ui>(&self) -> RwData<dyn Widget<U>>
    where
        W: Widget<U>,
    {
        let ptr = Arc::into_raw(self.value.clone());
        // SAFETY: Implements Widget<U>
        let value = unsafe { Arc::from_raw(ptr as *const UnsafeCell<dyn Widget<U>>) };
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
unsafe impl<T: ?Sized + 'static> Send for RwData<T> {}
unsafe impl<T: ?Sized + 'static> Sync for RwData<T> {}

impl<T: ?Sized + 'static> RwData<T> {}

impl<T: ?Sized> Clone for RwData<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            ty: self.ty,
            cur_state: self.cur_state.clone(),
            read_state: Arc::new(AtomicUsize::new(self.cur_state.load(Ordering::Relaxed))),
        }
    }
}

impl<T: Default + 'static> Default for RwData<T> {
    fn default() -> Self {
        Self {
            value: Arc::default(),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: Arc::default(),
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
    /// Maps the value within, works just like [`RwData::map`]
    pub fn map<O2>(&self, pa: &Pass, mut f: impl FnMut(O) -> O2 + 'static) -> DataMap<I, O2> {
        let data_map = self.clone();
        data_map
            .data
            .clone()
            .map(pa, move |input| f(data_map.map.borrow_mut()(input)))
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

impl<I: ?Sized + 'static, O> Clone for DataMap<I, O> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            map: self.map.clone(),
        }
    }
}

impl<I: ?Sized + 'static, O: 'static> DataMap<I, O> {}

impl<I: ?Sized + 'static, O: 'static> FnOnce<(&Pass,)> for DataMap<I, O> {
    type Output = O;

    extern "rust-call" fn call_once(self, (key,): (&Pass,)) -> Self::Output {
        self.map.borrow_mut()(self.data.read(key))
    }
}

impl<I: ?Sized + 'static, O: 'static> FnMut<(&Pass,)> for DataMap<I, O> {
    extern "rust-call" fn call_mut(&mut self, (key,): (&Pass,)) -> Self::Output {
        self.map.borrow_mut()(self.data.read(key))
    }
}

impl<I: ?Sized + 'static, O: 'static> Fn<(&Pass,)> for DataMap<I, O> {
    extern "rust-call" fn call(&self, (key,): (&Pass,)) -> Self::Output {
        self.map.borrow_mut()(self.data.read(key))
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
pub struct Pass(PhantomData<()>);

impl Pass {
    /// Returns a new instance of [`Pass`]
    ///
    /// Be careful when using this!
    pub(crate) unsafe fn new() -> Self {
        Pass(PhantomData)
    }
}

fn update_read_state(read_state: &Arc<AtomicUsize>, cur_state: &Arc<AtomicUsize>) {
    read_state.store(cur_state.load(Ordering::Relaxed), Ordering::Relaxed);
}

fn update_cur_state(read_state: &Arc<AtomicUsize>, cur_state: &Arc<AtomicUsize>) {
    let prev = cur_state.fetch_add(1, Ordering::Relaxed);
    read_state.store(prev + 1, Ordering::Relaxed);
}
