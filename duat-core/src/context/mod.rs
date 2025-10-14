//! Access to widgets and other other parts of the state of Duat
//!
//! This module lets you access and mutate some things:
//!
//! # Files
use std::any::TypeId;

pub use self::{cache::*, global::*, handles::*, log::*};
use crate::{
    data::{Pass, RwData},
    file::File,
    ui::{Area, Node, Widget},
};

mod cache;
mod handles;
mod log;

mod global {
    use std::{
        any::Any,
        path::PathBuf,
        sync::{
            LazyLock, Mutex, OnceLock,
            atomic::{AtomicBool, Ordering},
            mpsc,
        },
    };

    use super::{CurWidget, DynFile};
    use crate::{
        context::Handle,
        data::{DataMap, Pass, RwData},
        file::File,
        session::DuatEvent,
        text::Text,
        ui::Windows,
    };

    static WINDOWS: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static MODE_NAME: LazyLock<RwData<&str>> = LazyLock::new(RwData::default);

    static WILL_RELOAD_OR_QUIT: AtomicBool = AtomicBool::new(false);
    static CUR_DIR: OnceLock<Mutex<PathBuf>> = OnceLock::new();
    static SENDER: OnceLock<mpsc::Sender<DuatEvent>> = OnceLock::new();

    /// Queues a function to be done on the main thread with a
    /// [`Pass`]
    ///
    /// You can use this whenever you don't have access to a [`Pass`],
    /// in order to execute an action on the main thread, gaining
    /// access to Duat's global state within that function.
    pub fn queue(f: impl FnOnce(&mut Pass) + Send + 'static) {
        sender()
            .send(DuatEvent::QueuedFunction(Box::new(f)))
            .unwrap();
    }

    ////////// Internal setters meant to be called once

    /// Attempts to set the current [`Handle`]
    ///
    /// Fails if said [`Handle`] was already deleted.
    pub(crate) fn set_current_node(pa: &mut Pass, node: crate::ui::Node) {
        if let Err(err) = windows().set_current_node(pa, node) {
            super::warn!("{err}");
        }
    }

    /// Sets the [`Window`]s for Duat
    pub(crate) fn set_windows(windows: Windows) {
        WINDOWS
            .set(Box::leak(Box::new(windows)))
            .expect("Setup ran twice")
    }

    /// Orders to quit Duat
    pub(crate) fn order_reload_or_quit() {
        WILL_RELOAD_OR_QUIT.store(true, Ordering::Relaxed);
    }

    /// Sets the sender for [`DuatEvent`]s
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn set_sender(sender: mpsc::Sender<DuatEvent>) {
        SENDER.set(sender).expect("setup ran twice");
    }

    ////////// Widget Handle getters

    /// Returns a "fixed" [`Handle`] for the currently active [`File`]
    ///
    /// This [`Handle`] will always point to the same [`File`],
    /// even when it is not active. If you want a [`Handle`] that
    /// always points to the current [`File`], see [`dyn_file`]
    ///
    /// [`File`]: crate::file::File
    pub fn cur_file(pa: &Pass) -> Handle<File> {
        windows().current_file(pa).read(pa).clone()
    }

    /// Returns a "dynamic" [`Handle`] for the active [`File`]
    ///
    /// This [`Handle`] will change to point to the current [`File`],
    /// whenever the user swicthes which [`File`] is active. If you
    /// want a [`Handle`] that will stay on the current [`File`], see
    /// [`cur_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn dyn_file(pa: &Pass) -> DynFile {
        let dyn_file = windows().current_file(pa);
        let cur_file = RwData::new(dyn_file.read(pa).clone());
        DynFile { dyn_file, cur_file }
    }

    /// Returns a [`Handle`] for a [`File`] with the given name
    ///
    /// [`File`]: crate::file::File
    pub fn file_named(pa: &Pass, name: impl ToString) -> Result<Handle<File>, Text> {
        let (.., handle) = windows().named_file_entry(pa, &name.to_string())?;

        Ok(handle)
    }

    /// The [`CurWidget`]
    pub(crate) fn cur_widget(pa: &Pass) -> CurWidget {
        CurWidget(windows().current_widget(pa))
    }

    ////////// Other getters

    /// The [`Window`]s of Duat, must be used on main thread
    pub(crate) fn windows() -> &'static Windows {
        WINDOWS.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// The index of the currently active window
    pub fn cur_window(pa: &Pass) -> usize {
        windows().current_window(pa)
    }

    /// The name of the current [`Mode`]
    ///
    /// This uses a [`DataMap`] in order to prevent mutation of said
    /// name.
    ///
    /// [`Mode`]: crate::mode::Mode
    pub fn mode_name(pa: &Pass) -> DataMap<&'static str, &'static str> {
        MODE_NAME.map(pa, |name| *name)
    }

    // pub(crate) in order to keep just the DataMap one public
    pub(crate) fn raw_mode_name() -> RwData<&'static str> {
        MODE_NAME.clone()
    }

    /// The current directory
    pub fn cur_dir() -> PathBuf {
        CUR_DIR
            .get_or_init(|| Mutex::new(std::env::current_dir().unwrap()))
            .lock()
            .unwrap()
            .clone()
    }

    /// A [`mpsc::Sender`] for [`DuatEvent`]s in the main loop
    pub(crate) fn sender() -> mpsc::Sender<DuatEvent> {
        SENDER.get().unwrap().clone()
    }

    ////////// Functions for synchronization

    /// Returns `true` if Duat is about to reload
    pub fn will_reload_or_quit() -> bool {
        WILL_RELOAD_OR_QUIT.load(Ordering::Relaxed)
    }
}

/// A "dynamic" [`Handle`] wrapper for [`File`]s
///
/// This [`Handle`] wrapper will always point to the presently active
/// [`File`]. It can also detect when that [`File`] has been changed
/// or when another [`File`] becomes the active [`File`].
///
/// [`read`]: DynFile::read
/// [`write`]: DynFile::write
pub struct DynFile {
    dyn_file: RwData<Handle<File>>,
    cur_file: RwData<Handle<File>>,
}

impl DynFile {
    /// Wether the [`File`] pointed to has changed or swapped with
    /// another
    pub fn has_changed(&self, pa: &Pass) -> bool {
        if self.cur_file.has_changed() {
            true
        } else {
            self.dyn_file.read(pa).has_changed(pa)
        }
    }

    /// Swaps the [`DynFile`] to the currently active [`File`]
    pub fn swap_to_current(&mut self) {
        // SAFETY: Since this struct uses deep Cloning, no mutable
        // references to the RwData exist.
        let pa = unsafe { &mut Pass::new() };
        if self.cur_file.has_changed() {
            *self.dyn_file.write(pa) = self.cur_file.read(pa).clone();
        }
    }

    /// Reads the presently active [`File`]
    pub fn read<'a>(&'a mut self, pa: &'a Pass) -> &'a File {
        self.dyn_file.read(pa).read(pa)
    }

    /// The [`Handle<File>`] currently being pointed to
    pub fn handle(&self) -> &Handle<File> {
        // SAFETY: Since this struct uses deep Cloning, no mutable
        // references to the RwData exist.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };
        self.dyn_file.read(INTERNAL_PASS)
    }

    /// Simulates a [`read`] without actually reading
    ///
    /// This is useful if you want to tell Duat that you don't want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`Pass`] available to [`read`] the value.
    ///
    /// This assumes that you don't care about the active [`File`]
    /// possibly being swapped.
    ///
    /// [`read`]: Self::read
    /// [`has_changed`]: Self::has_changed
    pub fn declare_as_read(&self) {
        // SAFETY: Since this struct uses deep Cloning, no mutable
        // references to the RwData exist.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };
        self.dyn_file.read(INTERNAL_PASS).declare_as_read();
        self.cur_file.declare_as_read();
    }

    ////////// Writing functions

    /// Reads the presently active [`File`]
    pub fn write<'a>(&'a self, pa: &'a mut Pass) -> &'a mut File {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        self.dyn_file.read(INTERNAL_PASS).write(pa)
    }

    /// Writes to the [`File`] and [`Area`], making use of a
    /// [`Pass`]
    ///
    /// [`Area`]: crate::ui::Ui::Area
    pub fn write_with_area<'a>(&'a self, pa: &'a mut Pass) -> (&'a mut File, &'a dyn Area) {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        self.dyn_file.read(INTERNAL_PASS).write_with_area(pa)
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
        self.dyn_file.declare_written();
    }
}

impl Clone for DynFile {
    /// Returns a _deep cloned_ duplicate of the value
    ///
    /// In this case, what this means is that the clone and `self`
    /// will have different internal pointers for the current
    /// [`File`]. So if, for example, you call
    /// [`DynFile::swap_to_current`] on `self`, that will switch
    /// `self` to point to the current `File`, but the same will not
    /// be done in the clone.
    fn clone(&self) -> Self {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        Self {
            dyn_file: RwData::new(self.dyn_file.read(INTERNAL_PASS).clone()),
            cur_file: self.cur_file.clone(),
        }
    }
}

/// The current [`Widget`]
pub(crate) struct CurWidget(RwData<Node>);

impl CurWidget {
    /// The [`Widget`]'s [`TypeId`]
    pub fn type_id(&self, pa: &Pass) -> TypeId {
        self.0.read(pa).widget().type_id()
    }

    /// Reads the [`Widget`] and its [`Area`](crate::ui::Area)
    pub fn _read<R>(&self, pa: &Pass, f: impl FnOnce(&dyn Widget, &dyn Area) -> R) -> R {
        let node = self.0.read(pa);
        f(node.handle().read(pa), node.area().read(pa))
    }

    /// Reads the [`Widget`] as `W` and its
    /// [`Area`](crate::ui::Area)
    pub fn _read_as<W: Widget, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&W, &dyn Area) -> R,
    ) -> Option<R> {
        let node = self.0.read(pa);
        Some(f(node.read_as(pa)?, node.area().read(pa)))
    }

    /// Mutates the [`RwData<dyn Widget<U>>`], its
    /// [`Area`](crate::ui::Area), and related [`Widget`]s
    pub(crate) fn mutate_data<R>(&self, pa: &Pass, f: impl FnOnce(&Handle<dyn Widget>) -> R) -> R {
        f(self.0.read(pa).handle())
    }

    /// Mutates the [`RwData<dyn Widget<U>>`] as `W`, its
    /// [`Area`](crate::ui::Area), and related [`Widget`]s
    pub(crate) fn mutate_data_as<W: Widget, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&Handle<W>) -> R,
    ) -> Option<R> {
        let inner = self.0.read(pa);
        Some(f(&inner.try_downcast()?))
    }

    /// The inner [`Node`]
    pub(crate) fn node(&self, pa: &Pass) -> Node {
        self.0.read(pa).clone()
    }
}
