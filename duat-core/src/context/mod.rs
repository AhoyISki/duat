//! Access to widgets and other other parts of the state of Duat
//!
//! This module lets you access and mutate some things:
//!
//! # Buffers
use std::{
    any::TypeId,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
};

use crossterm::event::KeyEvent;

pub use self::{cache::*, global::*, handles::*, log::*};
use crate::{
    buffer::Buffer,
    data::{Pass, RwData},
    session::{DuatEvent, UiMouseEvent},
    ui::{Area, Node, Widget},
};

mod cache;
mod handles;
mod log;

mod global {
    use std::{
        path::PathBuf,
        sync::{
            LazyLock, Mutex, OnceLock,
            atomic::{AtomicBool, AtomicUsize, Ordering},
            mpsc,
        },
    };

    use super::{CurWidgetNode, DynBuffer};
    use crate::{
        context::{DuatReceiver, DuatSender, Handle},
        data::{DataMap, Pass, RwData},
        session::DuatEvent,
        text::Text,
        ui::{Widget, Window, Windows},
    };

    static WINDOWS: OnceLock<&Windows> = OnceLock::new();
    static MODE_NAME: LazyLock<RwData<&str>> = LazyLock::new(RwData::default);

    static WILL_RELOAD_OR_QUIT: AtomicBool = AtomicBool::new(false);
    static CUR_DIR: OnceLock<Mutex<PathBuf>> = OnceLock::new();
    static SENDER: OnceLock<DuatSender> = OnceLock::new();
    static NEW_EVENT_COUNT: OnceLock<&'static AtomicUsize> = OnceLock::new();

    /// Queues a function to be done on the main thread with a
    /// [`Pass`]
    ///
    /// You can use this whenever you don't have access to a `Pass`,
    /// in order to execute an action on the main thread, gaining
    /// access to Duat's global state within that function.
    ///
    /// Note that, since this can be called from any thread, it needs
    /// to be [`Send`] and `'static`.
    pub fn queue(f: impl FnOnce(&mut Pass) + Send + 'static) {
        sender().send(DuatEvent::QueuedFunction(Box::new(f)));
    }

    ////////// Internal setters meant to be called internally

    /// Attempts to set the current [`Handle`]
    ///
    /// Fails if said [`Handle`] was already deleted.
    #[track_caller]
    pub(crate) fn set_current_node(pa: &mut Pass, node: crate::ui::Node) {
        if let Err(err) = windows().set_current_node(pa, node) {
            super::warn!("{err}");
        }
    }

    /// Sets the [`Window`]s for Duat
    pub(crate) fn set_windows(windows: Windows) {
        if WINDOWS.set(Box::leak(Box::new(windows))).is_err() {
            panic!("Setup ran twice");
        }
    }

    /// Orders to quit Duat
    pub(crate) fn order_reload_or_quit() {
        WILL_RELOAD_OR_QUIT.store(true, Ordering::SeqCst);
    }

    /// Wether Duat has received new events that need to be handled
    ///
    /// Events can be anything, from a [key press], a [refocus], or
    /// even a [queued function].
    ///
    /// You can use this function to set up "timeouts". That is, if
    /// the thing you're trying to do is on the main thread and is
    /// taking way too long, you can check for this function and stop
    /// what you're doing if there's any new events that Duat hasn't
    /// handled.
    ///
    /// [key press]: crate::mode::KeyEvent
    /// [refocus]: crate::hook::FocusedOnDuat
    /// [queued function]: queue
    pub fn has_unhandled_events() -> bool {
        NEW_EVENT_COUNT.get().unwrap().load(Ordering::SeqCst) > 0
    }

    /// A channel for [`DuatEvent`]s
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn duat_channel() -> (DuatSender, DuatReceiver) {
        static NEW_EVENT_COUNT: AtomicUsize = AtomicUsize::new(0);

        let (sender, receiver) = mpsc::channel();
        (
            DuatSender(sender, &NEW_EVENT_COUNT),
            DuatReceiver(receiver, &NEW_EVENT_COUNT),
        )
    }

    /// Sets the sender for [`DuatEvent`]s
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn set_sender(sender: DuatSender) {
        NEW_EVENT_COUNT.set(sender.1).expect("setup ran twice");
        SENDER.set(sender).expect("setup ran twice");
    }

    ////////// Widget Handle getters

    /// Returns a "fixed" [`Handle`] for the currently active
    /// [`Buffer`]
    ///
    /// This `Handle` will always point to the same `Buffer`,
    /// even when it is not active. If you want a `Handle` that
    /// always points to the current Buffer, see dyn_buffer
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn current_buffer(pa: &Pass) -> Handle {
        windows().current_buffer(pa).read(pa).clone()
    }

    /// Returns a "dynamic" [`Handle`] for the active [`Buffer`]
    ///
    /// This `Handle` will change to point to the current `Buffer`,
    /// whenever the user swicthes which `Buffer` is active. If you
    /// want a `Handle` that will stay on the current `Buffer`, see
    /// [`current_buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn dynamic_buffer(pa: &Pass) -> DynBuffer {
        let dyn_buffer = windows().current_buffer(pa).clone();
        let cur_buffer = RwData::new(dyn_buffer.read(pa).clone());
        DynBuffer {
            cur_buffer: dyn_buffer,
            saved_buffer: cur_buffer,
        }
    }

    /// Returns a [`Handle`] for a [`Buffer`] with the given name
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn get_buffer(pa: &Pass, name: impl ToString) -> Result<Handle, Text> {
        let (.., handle) = windows().named_buffer_entry(pa, &name.to_string())?;

        Ok(handle)
    }

    /// Returns the current active [`Handle`]
    ///
    /// Unlike [`current_buffer`], this function will return a
    /// [`Handle<dyn Widget>`], which means it could be any
    /// [`Widget`], not just a [`Buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn current_widget(pa: &Pass) -> &Handle<dyn Widget> {
        windows().current_widget(pa).read(pa).handle()
    }

    /// The [`CurWidgetNode`]
    pub(crate) fn current_widget_node(pa: &Pass) -> CurWidgetNode {
        CurWidgetNode(windows().current_widget(pa).clone())
    }

    ////////// Other getters

    /// The [`Window`]s of Duat
    ///
    /// This struct gives you reading access to every [`Window`] in
    /// Duat with [`Windows::get`], you also get access to every
    /// [`Handle<dyn Widget>`], including every [`Handle<Buffer>`],
    /// through the [`Windows::handles`], [`Window::handles`] and
    /// [`Window::buffers`] function
    pub fn windows() -> &'static Windows {
        WINDOWS.get().unwrap()
    }

    /// The current [`Window`]
    ///
    /// You can iterate through all [`Handle<Buffer>`]s and
    /// [`Handle<dyn Widget>`] with [`Window::buffers`] and
    /// [`Window::handles`] respectively.
    ///
    /// If you wish to access other [`Window`]s, you can use
    /// `context::windows().get(pa, n)` to get the `n`th `Window`.
    /// The current window number can be found with
    /// [`context::current_win_index`]
    ///
    /// [`context::current_win_index`]: current_win_index
    pub fn current_window(pa: &Pass) -> &Window {
        let win = current_win_index(pa);
        WINDOWS.get().unwrap().get(pa, win).unwrap()
    }

    /// The index of the currently active window
    pub fn current_win_index(pa: &Pass) -> usize {
        windows().current_window(pa)
    }

    /// The name of the current [`Mode`]
    ///
    /// This uses a [`DataMap`] in order to prevent mutation of said
    /// name.
    ///
    /// [`Mode`]: crate::mode::Mode
    pub fn mode_name() -> DataMap<&'static str, &'static str> {
        MODE_NAME.map(|name| *name)
    }

    // pub(crate) in order to keep just the DataMap one public
    pub(crate) fn raw_mode_name() -> RwData<&'static str> {
        MODE_NAME.clone()
    }

    /// The current directory
    pub fn current_dir() -> PathBuf {
        CUR_DIR
            .get_or_init(|| Mutex::new(std::env::current_dir().unwrap()))
            .lock()
            .unwrap()
            .clone()
    }

    /// A [`mpsc::Sender`] for [`DuatEvent`]s in the main loop
    pub(crate) fn sender() -> DuatSender {
        SENDER.get().unwrap().clone()
    }

    ////////// Functions for synchronization

    /// Returns `true` if Duat is about to reload
    pub fn will_reload_or_quit() -> bool {
        WILL_RELOAD_OR_QUIT.load(Ordering::Relaxed)
    }
}

/// A sender of [`DuatEvent`]s
#[doc(hidden)]
#[derive(Clone, Debug)]
pub struct DuatSender(mpsc::Sender<DuatEvent>, &'static AtomicUsize);

impl DuatSender {
    /// Sends a [`KeyEvent`]
    pub fn send_key(&self, key: KeyEvent) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::KeyEventSent(key)).unwrap();
    }

    /// Sends a [`MouseEvent`]
    pub fn send_mouse(&self, mouse: UiMouseEvent) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::MouseEventSent(mouse)).unwrap();
    }

    /// Sends a notice that the app has resized
    pub fn send_resize(&self) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::Resized).unwrap();
    }

    /// Triggers the [`FocusedOnDuat`] [`hook`]
    ///
    /// [`FocusedOnDuat`]: crate::hook::FocusedOnDuat
    /// [`hook`]: crate::hook
    pub fn send_focused(&self) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::FocusedOnDuat).unwrap();
    }

    /// Triggers the [`UnfocusedFromDuat`] [`hook`]
    ///
    /// [`UnfocusedFromDuat`]: crate::hook::UnfocusedFromDuat
    /// [`hook`]: crate::hook
    pub fn send_unfocused(&self) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::UnfocusedFromDuat).unwrap();
    }

    /// Informs `duat-core` that a reload was successful
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn send_reload_succeeded(&self) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::ReloadSucceeded).unwrap();
    }

    /// Informs `duat-core` that a reload failed
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn send_reload_failed(&self) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(DuatEvent::ReloadFailed).unwrap();
    }

    /// Sends any [`DuatEvent`]
    pub(crate) fn send(&self, event: DuatEvent) {
        self.1.fetch_add(1, Ordering::Relaxed);
        self.0.send(event).unwrap();
    }
}

/// A receiver for [`DuatEvent`]s
#[doc(hidden)]
pub struct DuatReceiver(mpsc::Receiver<DuatEvent>, &'static AtomicUsize);

impl DuatReceiver {
    pub(crate) fn recv_timeout(&self, timeout: std::time::Duration) -> Option<DuatEvent> {
        self.0
            .recv_timeout(timeout)
            .inspect(|_| _ = self.1.fetch_sub(1, Ordering::Relaxed))
            .ok()
    }
}

/// A "dynamic" [`Handle`] wrapper for [`Buffer`]s
///
/// This `Handle` wrapper will always point to the presently active
/// `Buffer`. It can also detect when that `Buffer` has been changed
/// or when another `Buffer` becomes the active `Buffer`.
pub struct DynBuffer {
    cur_buffer: RwData<Handle>,
    saved_buffer: RwData<Handle>,
}

impl DynBuffer {
    /// Wether the [`Buffer`] pointed to has changed or swapped with
    /// another
    pub fn has_changed(&self, pa: &Pass) -> bool {
        if self.cur_buffer.has_changed() {
            true
        } else {
            self.saved_buffer.read(pa).has_changed(pa)
        }
    }

    /// Swaps the [`DynBuffer`] to the currently active [`Buffer`]
    pub fn swap_to_current(&mut self) {
        // SAFETY: Since this struct uses deep Cloning, no mutable
        // references to the RwData exist.
        let pa = unsafe { &mut Pass::new() };
        if self.cur_buffer.has_changed() {
            *self.saved_buffer.write(pa) = self.cur_buffer.read(pa).clone();
        }
    }

    /// Reads the presently active [`Buffer`]
    pub fn read<'a>(&'a mut self, pa: &'a Pass) -> &'a Buffer {
        self.saved_buffer.read(pa).read(pa)
    }

    /// The [`Handle<Buffer>`] currently being pointed to
    pub fn handle(&self) -> &Handle {
        // SAFETY: Since this struct uses deep Cloning, no mutable
        // references to the RwData exist.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };
        self.saved_buffer.read(INTERNAL_PASS)
    }

    /// Simulates a [`read`] without actually reading
    ///
    /// This is useful if you want to tell Duat that you don't want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`Pass`] available to [`read`] the value.
    ///
    /// This assumes that you don't care about the active [`Buffer`]
    /// possibly being swapped.
    ///
    /// [`read`]: Self::read
    /// [`has_changed`]: Self::has_changed
    pub fn declare_as_read(&self) {
        // SAFETY: Since this struct uses deep Cloning, no mutable
        // references to the RwData exist.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };
        self.cur_buffer.declare_as_read();
        self.saved_buffer.read(INTERNAL_PASS).declare_as_read();
    }

    ////////// Writing functions

    /// Reads the presently active [`Buffer`]
    pub fn write<'a>(&'a self, pa: &'a mut Pass) -> &'a mut Buffer {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        self.saved_buffer.read(INTERNAL_PASS).write(pa)
    }

    /// Writes to the [`Buffer`] and [`Area`], making use of a
    /// [`Pass`]
    ///
    /// [`Area`]: crate::ui::Area
    pub fn write_with_area<'a>(&'a self, pa: &'a mut Pass) -> (&'a mut Buffer, &'a mut Area) {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        self.saved_buffer.read(INTERNAL_PASS).write_with_area(pa)
    }

    /// Simulates a [`write`] without actually writing
    ///
    /// This is useful if you want to tell Duat that you want
    /// [`has_changed`] to return `true`, but you don't have a
    /// [`Pass`] available to `write` the value with.
    ///
    /// [`write`]: Self::write
    /// [`has_changed`]: Self::has_changed
    pub fn declare_written(&self) {
        // SAFETY: Since this struct uses deep Cloning, no other references to
        // the RwData exist.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };
        self.cur_buffer.read(INTERNAL_PASS).declare_written();
    }
}

impl Clone for DynBuffer {
    /// Returns a _deep cloned_ duplicate of the value
    ///
    /// In this case, what this means is that the clone and `self`
    /// will have different internal pointers for the current
    /// [`Buffer`]. So if, for example, you call
    /// [`DynBuffer::swap_to_current`] on `self`, that will switch
    /// `self` to point to the current `Buffer`, but the same will not
    /// be done in the clone.
    fn clone(&self) -> Self {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        Self {
            cur_buffer: RwData::new(self.cur_buffer.read(INTERNAL_PASS).clone()),
            saved_buffer: self.saved_buffer.clone(),
        }
    }
}

/// The current [`Widget`]
pub(crate) struct CurWidgetNode(RwData<Node>);

impl CurWidgetNode {
    /// The [`Widget`]'s [`TypeId`]
    pub fn type_id(&self, pa: &Pass) -> TypeId {
        self.0.read(pa).widget().type_id()
    }

    /// Reads the [`Widget`] and its [`Area`]
    pub fn _read<R>(&self, pa: &Pass, f: impl FnOnce(&dyn Widget, &Area) -> R) -> R {
        let node = self.0.read(pa);
        f(node.handle().read(pa), node.area().read(pa))
    }

    /// Reads the [`Widget`] as `W` and its
    /// [`Area`]
    pub fn _read_as<W: Widget, R>(&self, pa: &Pass, f: impl FnOnce(&W, &Area) -> R) -> Option<R> {
        let node = self.0.read(pa);
        Some(f(node.read_as(pa)?, node.area().read(pa)))
    }

    /// Mutates the [`RwData<dyn Widget>`], its
    /// [`Area`], and related [`Widget`]s
    pub(crate) fn mutate_data<R>(&self, pa: &Pass, f: impl FnOnce(&Handle<dyn Widget>) -> R) -> R {
        f(self.0.read(pa).handle())
    }

    /// Mutates the [`RwData<dyn Widget>`] as `W`, its
    /// [`Area`], and related [`Widget`]s
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
