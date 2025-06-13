//! Access to widgets and other other parts of the state of Duat
//!
//! This module lets you access and mutate some things:
//!
//! # Files
use std::{
    any::TypeId,
    sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

pub use self::global::*;
use crate::{
    data::{Pass, RwData},
    file::File,
    text::{Cursorless, Text},
    ui::{RawArea, Ui},
    widget::{Node, Related, Widget},
};

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

    use super::{CurFile, CurWidget, FileHandle, FileParts, Logs};
    use crate::{
        data::{DataMap, Pass, RwData},
        main_thread_only::MainThreadOnly,
        text::{Text, err},
        ui::{DuatEvent, Ui, Window},
        widget::Node,
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

    static LOGS: LazyLock<Logs> = LazyLock::new(Logs::new);
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

        let (widget, area, related) = node.parts();
        let file = widget.try_downcast().unwrap();

        Ok(FileHandle {
            fixed: Some((file, area.clone(), related.clone().unwrap_or_default())),
            current: cur_file(pa).unwrap().0.clone(),
        })
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

    /// Notifications for duat
    ///
    /// This is a mutable, shareable, [`Send`]/[`Sync`] list of
    /// notifications in the form of [`Text`]s, you can read this,
    /// send new notifications, and check for updates, just like with
    /// [`RwData`], except in this case, you don't need [`Pass`]es, so
    /// there might be changes to make this API safer in the future.
    pub fn logs() -> Logs {
        LOGS.clone()
    }

    /// Sends a notification to Duat
    pub fn notify(msg: impl Into<Text>) {
        LOGS.push(msg)
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
        let old = parts.and_then(|new| inner_cur_file().0.write(&mut *pa, |old| old.replace(new)));

        old.zip(inner_cur_widget().0.write(&mut *pa, |cw| cw.replace(node)))
    }

    /// The [`CurWidget`]
    pub(crate) fn cur_widget<U: Ui>(pa: &Pass) -> Result<&'static CurWidget<U>, Text> {
        let cur_widget = inner_cur_widget();
        if cur_widget.0.read(pa, |cw| cw.is_none()) {
            Err(err!("No widget yet").build())
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
            return Err(err!("No file yet").build());
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
        FileHandle {
            fixed: self.0.read(pa, |parts| parts.clone()),
            current: self.0.clone(),
        }
    }

    /// Returns a new "dynamic" [`FileHandle`]
    fn dynamic(&self) -> FileHandle<U> {
        FileHandle { fixed: None, current: self.0.clone() }
    }

    pub(crate) fn _get_related_widget<W: 'static>(
        &mut self,
        pa: &Pass,
    ) -> Option<(RwData<W>, U::Area)> {
        self.0.read(pa, |parts| {
            let (file, area, related) = parts.as_ref().unwrap();
            if file.data_is::<W>() {
                Some((file.try_downcast().unwrap(), area.clone()))
            } else {
                related.read(pa, |related| {
                    related.iter().find_map(|node| {
                        let (widget, area, _) = node.parts();
                        widget.try_downcast().map(|data| (data, area.clone()))
                    })
                })
            }
        })
    }
}

impl<U: Ui> Default for CurFile<U> {
    fn default() -> Self {
        Self::new()
    }
}

/// A handle to a [`File`] widget
///
/// This handle acts much like an [`RwData<File>`], but it also
/// includes an [`Area`] that can be acted upon alongside the
/// [`File`].
///
/// This is the only way you are supposed to read information about
/// the [`File`], in order to display it on [`Widget`]s, create
/// [`Text`]s, and do all sorts of things. You can, of course, also
/// modify a [`File`] from within this struct, but you should be
/// careful to prevent infinite loops, where you modify a [`File`], it
/// gets updated, and then you modify it again after noticing that it
/// has changed.
///
/// [`Area`]: crate::ui::RawArea
#[derive(Clone)]
pub struct FileHandle<U: Ui> {
    fixed: Option<FileParts<U>>,
    current: RwData<Option<FileParts<U>>>,
}

impl<U: Ui> FileHandle<U> {
    /// Reads from the [`File`] and the [`Area`] using a [`Pass`]
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    ///
    /// # Panics
    ///
    /// Panics if there is a mutable borrow of this struct somewhere,
    /// which could happen if you use [`RwData::write_unsafe`] or
    /// [`RwData::write_unsafe_as`] from some other place
    ///
    /// [`Area`]: crate::ui::RawArea
    pub fn read<Ret>(&self, pa: &Pass, f: impl FnOnce(&File<U>, &U::Area) -> Ret) -> Ret {
        if let Some((file, area, _)) = self.fixed.as_ref() {
            file.read(pa, |file| f(file, area))
        } else {
            self.current.read(pa, |parts| {
                let (file, area, _) = parts.as_ref().unwrap();
                file.read(pa, |file| f(file, area))
            })
        }
    }

    /// Writes to the [`File`] and [`Area`] within using a [`Pass`]
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    ///
    /// # Panics
    ///
    /// Panics if there is any type of borrow of this struct
    /// somewhere, which could happen if you use
    /// [`RwData::read_unsafe`] or [`RwData::write_unsafe`], for
    /// example.
    ///
    /// [`Area`]: crate::ui::RawArea
    pub fn write<Ret>(&self, pa: &mut Pass, f: impl FnOnce(&mut File<U>, &U::Area) -> Ret) -> Ret {
        let update = move |file: &RwData<File<U>>, area: &U::Area| {
            file.write(pa, |file| {
                let cfg = file.print_cfg();
                file.text_mut().remove_cursors(area, cfg);

                let ret = f(file, area);

                if let Some(main) = file.cursors().get_main() {
                    area.scroll_around_point(file.text(), main.caret(), cfg);
                }
                file.text_mut().add_cursors(area, cfg);

                ret
            })
        };

        if let Some((file, area, _)) = self.fixed.as_ref() {
            update(file, area)
        } else {
            // SAFETY: Since the update closure only uses a write method, the
            // Pass becomes unusable for other purposes, making it impossible
            // to make further borrows, asserting that there is no other borrow
            // for self.current.
            unsafe {
                self.current.read_unsafe(|parts| {
                    let (file, area, _) = parts.as_ref().unwrap();
                    update(file, area)
                })
            }
        }
    }

    /// Reads a [`Widget`] related to this [`File`], alongside its
    /// [`Area`], with a [`Pass`]
    ///
    /// A related [`Widget`] is one that was pushed to this [`File`]
    /// during the [`OnFileOpen`] [hook].
    ///
    /// [`Area`]: crate::ui::Area
    /// [`OnFileOpen`]: crate::hook::OnFileOpen
    /// [hook]: crate::hook
    pub fn read_related<W: 'static, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&W, &U::Area) -> R,
    ) -> Option<R> {
        let read = |(file, area, related): &FileParts<U>| {
            if file.data_is::<W>() {
                file.read_as(pa, |w| f(w, area))
            } else {
                related.read(pa, |related| {
                    related
                        .iter()
                        .find(|node| node.data_is::<W>())
                        .and_then(|node| node.widget().read_as(pa, |w| f(w, node.area())))
                })
            }
        };

        if let Some(parts) = self.fixed.as_ref() {
            read(parts)
        } else {
            self.current.read(pa, |parts| read(parts.as_ref().unwrap()))
        }
    }

    /// Gets the [`RwData`] and [`Area`] of a related widget, with a
    /// [`Pass`]
    ///
    /// A related [`Widget`] is one that was pushed to this [`File`]
    /// during the [`OnFileOpen`] [hook].
    ///
    /// [`Area`]: crate::ui::Area
    /// [`OnFileOpen`]: crate::hook::OnFileOpen
    /// [hook]: crate::hook
    pub fn get_related_widget<W: 'static>(&self, pa: &Pass) -> Option<(RwData<W>, U::Area)> {
        let get_related = |(file, area, related): &FileParts<U>| {
            if file.data_is::<W>() {
                Some((file.try_downcast().unwrap(), area.clone()))
            } else {
                related.read(pa, |related| {
                    related.iter().find_map(|node| {
                        let (widget, area, _) = node.parts();
                        widget.try_downcast().map(|data| (data, area.clone()))
                    })
                })
            }
        };

        if let Some(parts) = self.fixed.as_ref() {
            get_related(parts)
        } else {
            self.current
                .read(pa, |parts| get_related(parts.as_ref().unwrap()))
        }
    }

    /// Writes to the related widgets
    pub(crate) fn write_related_widgets(&self, pa: &mut Pass, f: impl FnOnce(&mut Vec<Node<U>>)) {
        if let Some((.., related)) = self.fixed.as_ref() {
            related.write(pa, f)
        } else {
            // SAFETY: Same situation as the write method
            unsafe {
                self.current
                    .read_unsafe(|parts| parts.as_ref().unwrap().2.write(pa, f))
            }
        }
    }

    ////////// Querying functions

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
    /// [`Widget`]: crate::widget::Widget
    /// [`needs_update`]: crate::widget::Widget::needs_update
    pub fn has_changed(&self) -> bool {
        if let Some((file, area, _)) = self.fixed.as_ref() {
            file.has_changed() || area.has_changed()
        } else {
            self.current.has_changed()
                || self.current.read_raw(|parts| {
                    let (file, area, _) = parts.as_ref().unwrap();
                    file.has_changed() || area.has_changed()
                })
        }
    }

    /// Wether the [`File`] within has swapped to another
    ///
    /// This can only happen when this is a
    pub fn has_swapped(&self) -> bool {
        let has_changed = self.current.has_changed();
        self.current.declare_as_read();
        has_changed
    }

    /// Wether the [`RwData`] within and another point to the same
    /// value
    pub fn ptr_eq<T: ?Sized>(&self, pa: &Pass, other: &RwData<T>) -> bool {
        if let Some((file, ..)) = self.fixed.as_ref() {
            file.ptr_eq(other)
        } else {
            self.current
                .read(pa, |parts| parts.as_ref().unwrap().0.ptr_eq(other))
        }
    }
}

/// The notifications sent to Duat.
///
/// This can include command results, failed mappings, recompilation
/// messages, and any other thing that you want to [push] to be
/// notified.
///
/// [push]: Logs::push
pub struct Logs {
    list: &'static Mutex<Vec<&'static Log>>,
    cutoffs: &'static Mutex<Vec<usize>>,
    cur_state: Arc<AtomicUsize>,
    read_state: AtomicUsize,
}

impl Clone for Logs {
    fn clone(&self) -> Self {
        Self {
            list: self.list,
            cutoffs: self.cutoffs,
            cur_state: self.cur_state.clone(),
            read_state: AtomicUsize::new(self.cur_state.load(Ordering::Relaxed)),
        }
    }
}

impl Logs {
    /// Creates a new [`Logs`]
    fn new() -> Self {
        Self {
            list: Box::leak(Box::default()),
            cutoffs: Box::leak(Box::default()),
            cur_state: Arc::new(AtomicUsize::new(1)),
            read_state: AtomicUsize::new(0),
        }
    }

    /// Returns an owned valued of a [`SliceIndex`]
    ///
    /// - `&'static Log` for `usize`;
    /// - [`Vec<&'static Log>`] for `impl RangeBounds<usize>`;
    ///
    /// [`SliceIndex`]: std::slice::SliceIndex
    pub fn get<I>(&self, i: I) -> Option<<I::Output as ToOwned>::Owned>
    where
        I: std::slice::SliceIndex<[&'static Log]>,
        I::Output: ToOwned,
    {
        self.list.lock().unwrap().get(i).map(ToOwned::to_owned)
    }

    /// Returns the last [`Log`]
    pub fn last(&self) -> Option<&'static Log> {
        self.list.lock().unwrap().last().copied()
    }

    /// Wether there are new notifications or not
    pub fn has_changed(&self) -> bool {
        self.cur_state.load(Ordering::Relaxed) > self.read_state.load(Ordering::Relaxed)
    }

    /// Pushes a new [notification] to Duat
    ///
    /// This could be any type that implements [`Into<Text>`]. If it
    /// is a [`Text`] however, in order to implement [`Send`] +
    /// [`Sync`], the [`Text`] will have its history and [`Reader`]s
    /// removed.
    ///
    /// [notification]: Text
    /// [`Reader`]: crate::file::Reader
    pub fn push(&self, text: impl Into<Text>) {
        self.cur_state.fetch_add(1, Ordering::Relaxed);
        self.list.lock().unwrap().push(Box::leak(Box::new(Log::Text(
            Into::<Text>::into(text).without_cursors(),
        ))))
    }

    /// Pushes a command's [`Text`] result
    pub(crate) fn push_cmd_result(&self, cmd: String, was_success: bool, result: Text) {
        self.cur_state.fetch_add(1, Ordering::Relaxed);

        let log = Log::CmdResult(cmd, was_success, result.without_cursors());
        self.list.lock().unwrap().push(Box::leak(Box::new(log)))
    }
}

/// A message that was sent to Duat
pub enum Log {
    /// Regular [`Text`], but [without cursors]
    ///
    /// [without cursors]: Cursorless
    Text(Cursorless),
    /// The result of a command
    CmdResult(String, bool, Cursorless),
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
    pub fn type_id(&self) -> TypeId {
        self.0.type_id()
    }

    /// Reads the [`Widget`] and its [`Area`](crate::ui::RawArea)
    pub fn read<R>(&self, pa: &Pass, f: impl FnOnce(&dyn Widget<U>, &U::Area) -> R) -> R {
        self.0.read(pa, |node| {
            let (widget, area, _) = node.as_ref().unwrap().parts();
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
            let (widget, area, _) = node.as_ref().unwrap().parts();
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
                let (widget, area, related) = node.as_ref().unwrap().parts();
                f(widget, area, related)
            })
        }
    }

    /// Mutates the [`RwData<dyn Widget<U>>`] as `W`, its
    /// [`Area`](crate::ui::RawArea), and related [`Widget`]s
    pub(crate) unsafe fn mutate_data_as<W: Widget<U>, R>(
        &self,
        f: impl FnOnce(&RwData<W>, &U::Area, &Related<U>) -> R,
    ) -> Option<R> {
        unsafe {
            self.0.read_unsafe(|node| {
                let (widget, area, related) = node.as_ref().unwrap().parts();
                Some(f(&widget.try_downcast()?, area, related))
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

pub(crate) type FileParts<U> = (RwData<File<U>>, <U as Ui>::Area, RwData<Vec<Node<U>>>);
