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
    text::{Text, txt},
    ui::{Node, Ui, Widget},
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
            atomic::{AtomicBool, AtomicUsize, Ordering},
            mpsc,
        },
    };

    use super::{CurFile, CurWidget, DynFile};
    use crate::{
        context::Handle,
        data::{DataMap, Pass, RwData},
        file::File,
        session::DuatEvent,
        text::{Text, txt},
        ui::{GetAreaId, Node, Ui, Widget, Windows},
    };

    static CUR_FILE: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static CUR_WIDGET: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static WINDOWS: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static MODE_NAME: LazyLock<RwData<&str>> = LazyLock::new(RwData::default);

    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);
    static WILL_RELOAD_OR_QUIT: AtomicBool = AtomicBool::new(false);
    static CUR_DIR: OnceLock<Mutex<PathBuf>> = OnceLock::new();
    static SENDER: OnceLock<mpsc::Sender<DuatEvent>> = OnceLock::new();

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

    /// Returns a [`Handle`] for a [`File`] with the given name
    ///
    /// [`File`]: crate::file::File
    pub fn file_named<U: Ui>(pa: &Pass, name: impl ToString) -> Result<Handle<File<U>, U>, Text> {
        let (.., handle) = windows::<U>().named_file_entry(pa, &name.to_string())?;

        Ok(handle)
    }

    /// Returns a "fixed" [`Handle`] for the currently active [`File`]
    ///
    /// This [`Handle`] will always point to the same [`File`],
    /// even when it is not active. If you want a [`Handle`] that
    /// always points to the current [`File`], see [`dyn_file`]
    ///
    /// [`File`]: crate::file::File
    pub fn fixed_file<U: Ui>(pa: &Pass) -> Result<Handle<File<U>, U>, Text> {
        cur_file(pa).fixed(pa)
    }

    /// Returns a "dynamic" [`Handle`] for the active [`File`]
    ///
    /// This [`Handle`] will change to point to the current [`File`],
    /// whenever the user swicthes which [`File`] is active. If you
    /// want a [`Handle`] that will stay on the current [`File`], see
    /// [`fixed_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn dyn_file<U: Ui>(pa: &Pass) -> Result<DynFile<U>, Text> {
        cur_file(pa).dynamic(pa)
    }

    /// Gets the [`Widget`] from a [`U::Area`]
    ///
    /// # Note
    ///
    /// This can fail if the [`Widget`] was closed.
    ///
    /// [`U::Area`]: Ui::Area
    pub fn get_widget<W: Widget<U>, U: Ui>(
        pa: &Pass,
        area: &impl GetAreaId,
    ) -> Result<Handle<W, U>, Text> {
        let node = windows::<U>()
            .entries(pa)
            .find(|(.., node)| node.area_id() == area.area_id());
        if let Some((.., node)) = node {
            if let Some(handle) = node.try_downcast() {
                Ok(handle)
            } else {
                Err(txt!(
                    "The widget with the given U::Area is not [a]{}",
                    crate::utils::duat_name::<W>()
                )
                .build())
            }
        } else {
            Err(txt!("No widget found that matches").build())
        }
    }

    /// The index of the currently active window
    pub fn cur_window() -> usize {
        CUR_WINDOW.load(Ordering::Relaxed)
    }

    /// Sets the value of the currently active window
    pub(crate) fn set_cur_window(new: usize) {
        CUR_WINDOW.store(new, Ordering::Relaxed);
    }

    /// The current directory
    pub fn cur_dir() -> PathBuf {
        CUR_DIR
            .get_or_init(|| Mutex::new(std::env::current_dir().unwrap()))
            .lock()
            .unwrap()
            .clone()
    }

    /// Returns `true` if Duat is about to reload
    pub fn will_reload_or_quit() -> bool {
        WILL_RELOAD_OR_QUIT.load(Ordering::Relaxed)
    }

    /// A [`mpsc::Sender`] for [`DuatEvent`]s in the main loop
    pub(crate) fn sender() -> mpsc::Sender<DuatEvent> {
        SENDER.get().unwrap().clone()
    }

    /// Sets the [`CurWidget`] and [`CurFile`], if needed
    pub(crate) fn set_cur<U: Ui>(
        pa: &mut Pass,
        file: Option<Handle<File<U>, U>>,
        node: Node<U>,
    ) -> Option<(Handle<File<U>, U>, Node<U>)> {
        let old = file.and_then(|new| cur_file(pa).0.write(pa).replace(new));

        old.zip(inner_cur_widget().0.write(pa).replace(node))
    }

    /// The [`CurWidget`]
    pub(crate) fn cur_widget<U: Ui>(pa: &Pass) -> Result<&'static CurWidget<U>, Text> {
        let cur_widget = inner_cur_widget();
        if cur_widget.0.read(pa).is_none() {
            Err(txt!("No widget yet").build())
        } else {
            Ok(cur_widget)
        }
    }

    /// Sets the [`Window`]s for Duat
    pub(crate) fn set_windows<U: Ui>(windows: Windows<U>) {
        WINDOWS
            .set(Box::leak(Box::new(windows)))
            .expect("Setup ran twice")
    }

    /// The [`Window`]s of Duat, must be used on main thread
    pub(crate) fn windows<U: Ui>() -> &'static Windows<U> {
        WINDOWS.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// Orders to quit Duat
    pub(crate) fn order_reload_or_quit() {
        WILL_RELOAD_OR_QUIT.store(true, Ordering::Relaxed);
    }

    /// The inner [`CurFile`]
    fn cur_file<U: Ui>(_: &Pass) -> &'static CurFile<U> {
        CUR_FILE.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// The inner [`CurWidget`]
    fn inner_cur_widget<U: Ui>() -> &'static CurWidget<U> {
        CUR_WIDGET.get().unwrap().downcast_ref().expect("1 Ui only")
    }

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

    /// Sets us static variables that were created by leaking memory
    ///
    /// ONLY MEANT TO BE USED BY THE pre_setup FUNCTION
    ///
    /// # Safety
    ///
    /// This sets the `WINDOWS` constant, so this should only run on
    /// the main thread.
    #[doc(hidden)]
    pub fn setup_context<U: Ui>(
        cur_file: &'static CurFile<U>,
        cur_widget: &'static CurWidget<U>,
        cur_window: usize,
    ) {
        CUR_FILE.set(cur_file).expect("setup ran twice");
        CUR_WIDGET.set(cur_widget).expect("setup ran twice");
        CUR_WINDOW.store(cur_window, Ordering::Relaxed);
    }

    /// Sets the sender for [`DuatEvent`]s
    ///
    /// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
    #[doc(hidden)]
    pub fn set_sender(sender: mpsc::Sender<DuatEvent>) {
        SENDER.set(sender).expect("setup ran twice");
    }
}

impl<U: Ui> Default for CurFile<U> {
    fn default() -> Self {
        Self::new()
    }
}

/// A [`Handle`] that dynamically points to the active [`File`]
#[derive(Clone)]
pub struct CurFile<U: Ui>(RwData<Option<Handle<File<U>, U>>>);

impl<U: Ui> CurFile<U> {
    /// Returns a new [`CurFile`]
    #[doc(hidden)]
    pub fn new() -> Self {
        Self(RwData::new(None))
    }

    /// Returns a new "fixed" [`Handle<File>`]
    ///
    /// This can fail if there is no [`File`] open yet, which can
    /// happen right as Duat is starting up, such as in a
    /// [`WidgetCreated<File>`] hook.
    ///
    /// [`WidgetCreated<File>`]: crate::hook::WidgetCreated
    fn fixed(&self, pa: &Pass) -> Result<Handle<File<U>, U>, Text> {
        self.0
            .read(pa)
            .clone()
            .ok_or_else(|| txt!("No file yet").build())
    }

    /// Returns a new "dynamic" "[`Handle<File>`]", in the form of
    /// [`DynFile`]
    ///
    /// This can fail if there is no [`File`] open yet, which can
    /// happen right as Duat is starting up, such as in a
    /// [`WidgetCreated<File>`] hook.
    ///
    /// [`WidgetCreated<File>`]: crate::hook::WidgetCreated
    fn dynamic(&self, pa: &Pass) -> Result<DynFile<U>, Text> {
        Ok(DynFile {
            file: RwData::new(self.0.read(pa).clone().ok_or_else(|| txt!("No file yet"))?),
            cur_file: self.clone(),
        })
    }

    /// Wether the current [`File`] has been swapped
    pub fn has_swapped(&self) -> bool {
        self.0.has_changed()
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
pub struct DynFile<U: Ui> {
    file: RwData<Handle<File<U>, U>>,
    cur_file: CurFile<U>,
}

impl<U: Ui> DynFile<U> {
    /// Wether the [`File`] pointed to has changed or swapped with
    /// another
    pub fn has_changed(&self, pa: &Pass) -> bool {
        if self.cur_file.has_swapped() {
            true
        } else {
            self.file.read(pa).has_changed()
        }
    }

    /// Swaps the [`DynFile`] to the currently active [`File`]
    pub fn swap_to_current(&mut self) {
        // SAFETY: Since this struct doesn't implement Clone, mutable access
        // ensures that nothing is holding a reference to the RwData.
        let pa = unsafe { &mut Pass::new() };
        if self.cur_file.has_swapped() {
            *self.file.write(pa) = self.cur_file.fixed(pa).unwrap();
        }
    }

    /// Reads the presently active [`File`]
    pub fn read<'a>(&'a mut self, pa: &'a Pass) -> &'a File<U> {
        self.file.read(pa).read(pa)
    }

    /// The [`Handle<File>`] currently being pointed to
    pub fn handle(&self) -> &Handle<File<U>, U> {
        // SAFETY: Since this struct doesn't implement Clone, no mutable
        // references to the RwData exist.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };
        self.file.read(INTERNAL_PASS)
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
        self.file.declare_as_read();
        self.cur_file.0.declare_as_read();
    }

    ////////// Writing functions

    /// Reads the presently active [`File`]
    pub fn write<'a>(&'a self, pa: &'a mut Pass) -> &'a mut File<U> {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        self.file.read(INTERNAL_PASS).write(pa)
    }

    /// Writes to the [`File`] and [`Area`], making use of a
    /// [`Pass`]
    ///
    /// [`Area`]: crate::ui::Ui::Area
    pub fn write_with_area<'a>(&'a self, pa: &'a mut Pass) -> (&'a mut File<U>, &'a U::Area) {
        // SAFETY: Because I already got a &mut Pass, the RwData can't be
        // accessed anyways.
        static INTERNAL_PASS: &Pass = unsafe { &Pass::new() };

        self.file.read(INTERNAL_PASS).write_with_area(pa)
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
        self.file.declare_written();
    }
}

/// The current [`Widget`]
#[doc(hidden)]
pub struct CurWidget<U: Ui>(RwData<Option<Node<U>>>);

impl<U: Ui> CurWidget<U> {
    /// Returns a new [`CurWidget`]
    pub fn new() -> Self {
        Self(RwData::new(None))
    }

    /// The [`Widget`]'s [`TypeId`]
    pub fn type_id(&self, pa: &Pass) -> TypeId {
        self.0.read(pa).as_ref().unwrap().widget().type_id()
    }

    /// Reads the [`Widget`] and its [`Area`](crate::ui::Area)
    pub fn read<R>(&self, pa: &Pass, f: impl FnOnce(&dyn Widget<U>, &U::Area) -> R) -> R {
        let inner = self.0.read(pa);
        let node = inner.as_ref().unwrap();
        f(node.handle().read(pa), node.area(pa))
    }

    /// Reads the [`Widget`] as `W` and its
    /// [`Area`](crate::ui::Area)
    pub fn read_as<W: Widget<U>, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&W, &U::Area) -> R,
    ) -> Option<R> {
        let inner = self.0.read(pa);
        let handle = inner.as_ref().unwrap();
        Some(f(handle.read_as(pa)?, handle.area(pa)))
    }

    /// Mutates the [`RwData<dyn Widget<U>>`], its
    /// [`Area`](crate::ui::Area), and related [`Widget`]s
    pub(crate) fn mutate_data<R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&Handle<dyn Widget<U>, U>) -> R,
    ) -> R {
        let inner = self.0.read(pa);
        f(inner.as_ref().unwrap().handle())
    }

    /// Mutates the [`RwData<dyn Widget<U>>`] as `W`, its
    /// [`Area`](crate::ui::Area), and related [`Widget`]s
    pub(crate) fn mutate_data_as<W: Widget<U>, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&Handle<W, U>) -> R,
    ) -> Option<R> {
        let inner = self.0.read(pa);
        Some(f(&inner.as_ref().unwrap().handle().try_downcast()?))
    }

    /// The inner [`Node`]
    pub(crate) fn node(&self, pa: &Pass) -> Node<U> {
        self.0.read(pa).clone().unwrap()
    }
}

impl<U: Ui> Default for CurWidget<U> {
    fn default() -> Self {
        Self::new()
    }
}
