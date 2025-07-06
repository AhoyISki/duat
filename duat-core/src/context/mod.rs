//! Access to widgets and other other parts of the state of Duat
//!
//! This module lets you access and mutate some things:
//!
//! # Files
use std::{any::TypeId, cell::Cell, rc::Rc};

pub use self::{cache::*, global::*, handles::*, log::*};
use crate::{
    data::{Pass, RwData},
    file::File,
    ui::{Node, Related, Ui, Widget},
};

mod cache;
mod handles;
mod log;

mod global {
    use std::{
        any::Any,
        cell::RefCell,
        path::PathBuf,
        sync::{
            LazyLock, Mutex, OnceLock,
            atomic::{AtomicBool, AtomicUsize, Ordering},
            mpsc,
        },
    };

    use super::{CurFile, CurWidget, FileHandle, FileParts};
    use crate::{
        context::Handle,
        data::{DataMap, Pass, RwData},
        main_thread_only::MainThreadOnly,
        text::{Text, txt},
        ui::{DuatEvent, Node, Ui, Widget, Window},
    };

    static MAIN_THREAD_ID: OnceLock<std::thread::ThreadId> = OnceLock::new();
    static CUR_FILE: MainThreadOnly<OnceLock<&'static dyn Any>> =
        MainThreadOnly::new(OnceLock::new());
    static CUR_WIDGET: MainThreadOnly<OnceLock<&'static dyn Any>> =
        MainThreadOnly::new(OnceLock::new());
    static WINDOWS: MainThreadOnly<OnceLock<&'static dyn Any>> =
        MainThreadOnly::new(OnceLock::new());
    static MODE_NAME: LazyLock<MainThreadOnly<RwData<&'static str>>> =
        LazyLock::new(MainThreadOnly::default);

    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);
    static WILL_RELOAD_OR_QUIT: AtomicBool = AtomicBool::new(false);
    static CUR_DIR: OnceLock<Mutex<PathBuf>> = OnceLock::new();
    static SENDER: OnceLock<&mpsc::Sender<DuatEvent>> = OnceLock::new();

    /// The name of the current [`Mode`]
    ///
    /// This uses a [`DataMap`] in order to prevent mutation of said
    /// name.
    ///
    /// [`Mode`]: crate::mode::Mode
    pub fn mode_name() -> DataMap<&'static str, &'static str> {
        assert_is_on_main_thread();
        unsafe { MODE_NAME.get() }.map(|name| *name)
    }

    // pub(crate) in order to keep just the DataMap one public
    pub(crate) fn raw_mode_name() -> RwData<&'static str> {
        assert_is_on_main_thread();
        unsafe { MODE_NAME.get() }.clone()
    }

    /// Returns a [`FileHandle`] for a [`File`] with the given name
    ///
    /// [`File`]: crate::file::File
    pub fn file_named<U: Ui>(pa: &Pass, name: impl ToString) -> Result<FileHandle<U>, Text> {
        let windows = windows::<U>().borrow();
        let name = name.to_string();
        let (.., node) = crate::file_entry(pa, &windows, &name)?;

        let (widget, area, mask, related) = node.parts();
        let file = widget.try_downcast().unwrap();

        let handle = Handle::from_parts(file, area.clone(), mask.clone());

        Ok(FileHandle::from_parts(
            Some((handle, related.clone().unwrap_or_default())),
            cur_file(pa).unwrap().0.clone(),
        ))
    }

    /// Returns a "fixed" [`FileHandle`] for the currently active
    /// [`File`]
    ///
    /// This [`FileHandle`] will always point to the same [`File`],
    /// even when it is not active. If you want a [`FileHandle`] that
    /// always points to the current [`File`], see [`dyn_file`]
    ///
    /// [`File`]: crate::file::File
    pub fn fixed_file<U: Ui>(pa: &Pass) -> Result<FileHandle<U>, Text> {
        Ok(cur_file(pa)?.fixed(pa))
    }

    /// Returns a "dynamic" [`FileHandle`] for the active [`File`]
    ///
    /// This [`FileHandle`] will change to point to the current
    /// [`File`], whenever the user swicthes which [`File`] is active.
    /// If you want a [`FileHandle`] that will stay on the current
    /// [`File`], see [`fixed_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn dyn_file<U: Ui>(pa: &Pass) -> Result<FileHandle<U>, Text> {
        Ok(cur_file(pa)?.dynamic())
    }

    /// Gets the [`Widget`] from a [`U::Area`]
    ///
    /// [`U::Area`]: Ui::Area
    pub fn get_widget<W: Widget<U>, U: Ui>(_: &Pass, area: &U::Area) -> Result<Handle<W, U>, Text> {
        let windows = windows::<U>().borrow();

        let node = windows
            .iter()
            .flat_map(Window::nodes)
            .find(|node| node.area() == area);
        if let Some(node) = node {
            let (widget, area, mask, _) = node.parts();
            if let Some(widget) = widget.try_downcast() {
                Ok(Handle::from_parts(widget, area.clone(), mask.clone()))
            } else {
                Err(txt!(
                    "The widget with the given U::Area is not [a]{}",
                    crate::duat_name::<W>()
                )
                .build())
            }
        } else {
            Err(txt!("No widget found that matches the given U::Area").build())
        }
    }

    /// The index of the currently active window.
    pub fn cur_window() -> usize {
        CUR_WINDOW.load(Ordering::Relaxed)
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
    pub(crate) fn sender() -> &'static mpsc::Sender<DuatEvent> {
        SENDER.get().unwrap()
    }

    /// Sets the [`CurWidget`] and [`CurFile`], if needed
    pub(crate) fn set_cur<U: Ui>(
        pa: &mut Pass,
        parts: Option<FileParts<U>>,
        node: Node<U>,
    ) -> Option<(FileParts<U>, Node<U>)> {
        let old = parts.and_then(|new| inner_cur_file().0.write(pa, |old| old.replace(new)));

        old.zip(inner_cur_widget().0.write(pa, |cw| cw.replace(node)))
    }

    /// The [`CurWidget`]
    pub(crate) fn cur_widget<U: Ui>(pa: &Pass) -> Result<&'static CurWidget<U>, Text> {
        let cur_widget = inner_cur_widget();
        if cur_widget.0.read(pa, |cw| cw.is_none()) {
            Err(txt!("No widget yet").build())
        } else {
            Ok(cur_widget)
        }
    }

    /// Sets the [`Window`]s for Duat
    pub(crate) fn set_windows<U: Ui>(wins: Vec<Window<U>>) -> &'static AtomicUsize {
        *windows().borrow_mut() = wins;
        &CUR_WINDOW
    }

    /// The [`Window`]s of Duat, must be used on main thread
    pub(crate) fn windows<U: Ui>() -> &'static RefCell<Vec<Window<U>>> {
        assert_is_on_main_thread();
        let windows = unsafe { WINDOWS.get() };
        windows.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// The [`CurFile`], must be used on main thread
    pub(crate) fn inner_cur_file<U: Ui>() -> &'static CurFile<U> {
        assert_is_on_main_thread();
        let cur_file = unsafe { CUR_FILE.get() };
        cur_file.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// Orders to quit Duat
    pub(crate) fn order_reload_or_quit() {
        WILL_RELOAD_OR_QUIT.store(true, Ordering::Relaxed);
    }

    /// The inner [`CurFile`]
    fn cur_file<U: Ui>(pa: &Pass) -> Result<&'static CurFile<U>, Text> {
        let cur_file = inner_cur_file();
        if cur_file.0.read(pa, |f| f.is_none()) {
            return Err(txt!("No file yet").build());
        }
        Ok(cur_file)
    }

    /// The inner [`CurWidget`]
    fn inner_cur_widget<U: Ui>() -> &'static CurWidget<U> {
        let cur_widget = unsafe { CUR_WIDGET.get() };
        cur_widget.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// Asserts that the current thread is the main one
    ///
    /// This is important because many of the objects in Duat are
    /// meant to be initialized only once and are also not [`Send`],
    /// so the free functions that return said objects should not be
    /// allowed outside of the main thread of execution.
    ///
    /// # Panics
    ///
    /// Panics if not on the main thread of execution.
    pub fn assert_is_on_main_thread() {
        assert_eq!(
            std::thread::current().id(),
            *MAIN_THREAD_ID.get().unwrap(),
            "Not on main thread"
        );
    }

    /// Sets us static variables that were created by leaking memory
    #[doc(hidden)]
    pub unsafe fn setup_context<U: Ui>(
        cur_file: &'static CurFile<U>,
        cur_widget: &'static CurWidget<U>,
        cur_window: usize,
        windows: &'static RefCell<Vec<Window<U>>>,
        sender: &'static mpsc::Sender<DuatEvent>,
    ) {
        MAIN_THREAD_ID
            .set(std::thread::current().id())
            .expect("setup ran twice");

        unsafe {
            CUR_FILE.get().set(cur_file).expect("setup ran twice");
            CUR_WIDGET.get().set(cur_widget).expect("setup ran twice");
            WINDOWS.get().set(windows).expect("setup ran twice");
        }

        CUR_WINDOW.store(cur_window, Ordering::Relaxed);
        SENDER.set(sender).expect("setup ran twice");
    }
}

impl<U: Ui> Default for CurFile<U> {
    fn default() -> Self {
        Self::new()
    }
}

/// The current file
#[doc(hidden)]
#[derive(Clone)]
pub struct CurFile<U: Ui>(RwData<Option<FileParts<U>>>);

impl<U: Ui> CurFile<U> {
    /// Returns a new [`CurFile`]
    #[doc(hidden)]
    pub fn new() -> Self {
        Self(RwData::new(None))
    }

    /// Returns a new "fixed" [`FileHandle`]
    fn fixed(&self, pa: &Pass) -> FileHandle<U> {
        FileHandle::from_parts(self.0.read(pa, |parts| parts.clone()), self.0.clone())
    }

    /// Returns a new "dynamic" [`FileHandle`]
    fn dynamic(&self) -> FileHandle<U> {
        FileHandle::from_parts(None, self.0.clone())
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
        self.0
            .read(pa, |node| node.as_ref().unwrap().widget().type_id())
    }

    /// Reads the [`Widget`] and its [`Area`](crate::ui::RawArea)
    pub fn read<R>(&self, pa: &Pass, f: impl FnOnce(&dyn Widget<U>, &U::Area) -> R) -> R {
        self.0.read(pa, |node| {
            let (widget, area, ..) = node.as_ref().unwrap().parts();
            widget.read(pa, |widget| f(widget, area))
        })
    }

    /// Reads the [`Widget`] as `W` and its
    /// [`Area`](crate::ui::RawArea)
    pub fn read_as<W: Widget<U>, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&W, &U::Area) -> R,
    ) -> Option<R> {
        self.0.read(pa, |node| {
            let (widget, area, ..) = node.as_ref().unwrap().parts();
            widget.read_as(pa, |widget| f(widget, area))
        })
    }

    /// Mutates the [`RwData<dyn Widget<U>>`], its
    /// [`Area`](crate::ui::RawArea), and related [`Widget`]s
    pub(crate) unsafe fn mutate_data<R>(
        &self,
        f: impl FnOnce(&RwData<dyn Widget<U>>, &U::Area, &Related<U>) -> R,
    ) -> R {
        unsafe {
            self.0.read_unsafe(|node| {
                let (widget, area, _, related) = node.as_ref().unwrap().parts();
                f(widget, area, related)
            })
        }
    }

    /// Mutates the [`RwData<dyn Widget<U>>`] as `W`, its
    /// [`Area`](crate::ui::RawArea), and related [`Widget`]s
    pub(crate) unsafe fn mutate_data_as<W: Widget<U>, R>(
        &self,
        f: impl FnOnce(&RwData<W>, &U::Area, &Rc<Cell<&'static str>>, &Related<U>) -> R,
    ) -> Option<R> {
        unsafe {
            self.0.read_unsafe(|node| {
                let (widget, area, mask, related) = node.as_ref().unwrap().parts();
                Some(f(&widget.try_downcast()?, area, mask, related))
            })
        }
    }

    /// The inner [`Node`]
    pub(crate) fn node(&self, pa: &Pass) -> Node<U> {
        self.0.read(pa, |node| node.clone().unwrap())
    }
}

impl<U: Ui> Default for CurWidget<U> {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) type FileParts<U> = (Handle<File<U>, U>, RwData<Vec<Node<U>>>);
