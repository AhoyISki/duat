//! Access to widgets and other other parts of the state of Duat
//!
//! This module lets you access and mutate some things:
//!
//! # Files
use std::{any::TypeId, sync::Arc};

use parking_lot::Mutex;

pub use self::global::*;
use crate::{
    data::{ReadDataGuard, RwData, WriteDataGuard},
    mode::Cursors,
    text::Text,
    ui::{Area, Ui},
    widgets::{File, Node, Related, Widget},
};

mod global {
    use std::{
        any::Any,
        path::PathBuf,
        sync::{
            LazyLock, OnceLock,
            atomic::{AtomicBool, AtomicUsize, Ordering},
        },
    };

    use parking_lot::{Mutex, RwLock};

    use super::{CurFile, CurWidget, DynamicFile, FileParts, FixedFile, Notifications};
    use crate::{
        data::RwData,
        duat_name,
        mode::Regular,
        text::{Text, err},
        ui::{Ui, Window},
        widgets::Node,
    };

    static MODE_NAME: LazyLock<RwData<&str>> =
        LazyLock::new(|| RwData::new(duat_name::<Regular>()));
    static CUR_FILE: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static CUR_WIDGET: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);
    static WINDOWS: OnceLock<&(dyn Any + Send + Sync)> = OnceLock::new();
    static NOTIFICATIONS: LazyLock<RwData<Vec<Text>>> = LazyLock::new(RwData::default);
    static WILL_RELOAD_OR_QUIT: AtomicBool = AtomicBool::new(false);
    static CUR_DIR: OnceLock<Mutex<PathBuf>> = OnceLock::new();

    pub fn mode_name() -> &'static RwData<&'static str> {
        &MODE_NAME
    }

    pub fn file_named<U: Ui>(name: impl ToString) -> Result<FixedFile<U>, Text> {
        let windows = windows::<U>().read();
        let name = name.to_string();
        let (.., node) = crate::file_entry(&windows, &name)?;

        let (widget, area, related) = node.parts();
        let file = widget.try_downcast().unwrap();

        Ok(FixedFile((
            file,
            area.clone(),
            related.as_ref().unwrap().clone(),
        )))
    }

    pub fn fixed_file<U: Ui>() -> Result<FixedFile<U>, Text> {
        Ok(cur_file()?.fixed_file())
    }

    pub fn dyn_file<U: Ui>() -> Result<DynamicFile<U>, Text> {
        Ok(cur_file()?.dyn_file())
    }

    pub fn cur_window() -> usize {
        CUR_WINDOW.load(Ordering::Relaxed)
    }

    pub fn cur_dir() -> PathBuf {
        CUR_DIR
            .get_or_init(|| Mutex::new(std::env::current_dir().unwrap()))
            .lock()
            .clone()
    }

    pub fn notifications() -> Notifications {
        Notifications(NOTIFICATIONS.clone())
    }

    pub fn notify(msg: Text) {
        NOTIFICATIONS.write().push(msg)
    }

    /// Returns `true` if Duat is about to reload
    pub fn will_reload_or_quit() -> bool {
        WILL_RELOAD_OR_QUIT.load(Ordering::Relaxed)
    }

    pub(crate) fn set_cur<U: Ui>(
        parts: Option<FileParts<U>>,
        node: Node<U>,
    ) -> Option<(FileParts<U>, Node<U>)> {
        let prev = parts.and_then(|p| inner_cur_file().0.write().replace(p));

        prev.zip(inner_cur_widget().0.write().replace(node))
    }

    pub(crate) fn cur_widget<U: Ui>() -> Result<&'static CurWidget<U>, Text> {
        let cur_widget = inner_cur_widget();
        cur_widget.0.read().as_ref().ok_or(err!("No widget yet"))?;
        Ok(cur_widget)
    }

    pub(crate) fn set_windows<U: Ui>(wins: Vec<Window<U>>) -> &'static AtomicUsize {
        *windows().write() = wins;
        &CUR_WINDOW
    }

    pub(crate) fn windows<U: Ui>() -> &'static RwLock<Vec<Window<U>>> {
        WINDOWS.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    pub(crate) fn inner_cur_file<U: Ui>() -> &'static CurFile<U> {
        CUR_FILE.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    /// Orders to quit Duat
    pub(crate) fn order_reload_or_quit() {
        WILL_RELOAD_OR_QUIT.store(true, Ordering::Relaxed);
        while crate::thread::still_running() {
            std::thread::sleep(std::time::Duration::from_micros(500));
        }
    }

    fn cur_file<U: Ui>() -> Result<&'static CurFile<U>, Text> {
        let cur_file = inner_cur_file();
        cur_file.0.read().as_ref().ok_or(err!("No file yet"))?;
        Ok(cur_file)
    }

    fn inner_cur_widget<U: Ui>() -> &'static CurWidget<U> {
        CUR_WIDGET.get().unwrap().downcast_ref().expect("1 Ui only")
    }

    #[doc(hidden)]
    pub fn setup_non_statics<U: Ui>(
        cur_file: &'static CurFile<U>,
        cur_widget: &'static CurWidget<U>,
        cur_window: usize,
        windows: &'static RwLock<Vec<Window<U>>>,
    ) {
        CUR_FILE.set(cur_file).expect("setup ran twice");
        CUR_WIDGET.set(cur_widget).expect("setup ran twice");
        CUR_WINDOW.store(cur_window, Ordering::Relaxed);
        WINDOWS.set(windows).expect("setup ran twice");
    }
}

pub struct CurFile<U: Ui>(RwData<Option<FileParts<U>>>);

impl<U: Ui> CurFile<U> {
    pub fn new() -> Self {
        Self(RwData::new(None))
    }

    pub fn fixed_file(&self) -> FixedFile<U> {
        let parts = self.0.raw_read();
        let (file, area, related) = parts.clone().unwrap();

        FixedFile((file, area, related))
    }

    pub fn dyn_file(&self) -> DynamicFile<U> {
        let dyn_parts = self.0.clone();
        // Here, I do regular read, so the dyn_parts checker doesn't keep
        // returning true for all eternity.
        let (file, area, related) = {
            let parts = dyn_parts.read();
            parts.clone().unwrap()
        };
        let checker = file.checker();

        DynamicFile {
            parts: (file, area, related),
            dyn_parts,
            checker: Arc::new(Mutex::new(Box::new(checker))),
        }
    }

    pub(crate) fn mutate_related_widget<W: Widget<U>, R>(
        &self,
        f: impl FnOnce(&mut W, &U::Area) -> R,
    ) -> Option<R> {
        let f = move |widget: &mut W, area| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(area, cfg);

            let ret = f(widget, area);

            let cfg = widget.print_cfg();

            if let Some(main) = widget.cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
            }
            widget.text_mut().add_cursors(area, cfg);
            widget.update(area);
            widget.print(area);

            ret
        };

        let data = self.0.raw_read();
        let (file, area, rel) = data.as_ref().unwrap();

        let rel = rel.read();
        if file.data_is::<W>() {
            file.write_as().map(|mut w| f(&mut w, area))
        } else {
            rel.iter()
                .find(|node| node.data_is::<W>())
                .and_then(|node| {
                    let (widget, area, _) = node.parts();
                    widget.write_as().map(|mut w| f(&mut w, area))
                })
        }
    }
}

impl<U: Ui> Default for CurFile<U> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct FixedFile<U: Ui>(FileParts<U>);

impl<U: Ui> FixedFile<U> {
    pub fn read(&mut self) -> (ReadDataGuard<File>, &U::Area) {
        let (file, area, _) = &self.0;
        (file.read(), area)
    }

    pub fn write(&mut self) -> (WriteFileGuard<U>, &U::Area) {
        let (file, area, _) = &self.0;
        let mut file = file.write();
        let cfg = file.print_cfg();
        file.text_mut().remove_cursors(area, cfg);

        let guard = WriteFileGuard { file, area };
        (guard, area)
    }

    pub fn has_changed(&self) -> bool {
        self.0.0.has_changed()
    }

    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static + use<U> {
        self.0.0.checker()
    }

    // NOTE: Doesn't return result, since it is expected that widgets can
    // only be created after the file exists.
    pub fn file_ptr_eq(&self, other: &Node<U>) -> bool {
        other.ptr_eq(&self.0.0)
    }

    pub(crate) fn get_related_widget<W>(&mut self) -> Option<(RwData<W>, U::Area)> {
        let (.., related) = &self.0;
        let related = related.read();
        related.iter().find_map(|node| {
            let (widget, area, _) = node.parts();
            widget.try_downcast().map(|data| (data, area.clone()))
        })
    }

    pub(crate) fn inspect_related<W: 'static, R>(&mut self, f: impl FnOnce(&W) -> R) -> Option<R> {
        let (file, _, related) = &self.0;

        if file.data_is::<W>() {
            file.read_as().map(|w| f(&w))
        } else {
            let related = related.read();
            related
                .iter()
                .find(|node| node.data_is::<W>())
                .and_then(|w| w.read_as().map(|w| f(&w)))
        }
    }

    pub(crate) fn related_widgets(&self) -> &RwData<Vec<Node<U>>> {
        &self.0.2
    }
}

pub struct DynamicFile<U: Ui> {
    parts: FileParts<U>,
    dyn_parts: RwData<Option<FileParts<U>>>,
    checker: Arc<Mutex<Box<dyn Fn() -> bool + Send + Sync + 'static>>>,
}

impl<U: Ui> DynamicFile<U> {
    pub fn read(&mut self) -> (ReadDataGuard<File>, &U::Area) {
        if self.dyn_parts.has_changed() {
            let (file, area, related) = self.dyn_parts.read().clone().unwrap();
            *self.checker.lock() = Box::new(file.checker());
            self.parts = (file, area, related);
        }

        let (file, area, _) = &self.parts;
        (file.read(), area)
    }

    pub fn write(&mut self) -> (WriteFileGuard<U>, &U::Area) {
        if self.dyn_parts.has_changed() {
            let (file, area, related) = self.dyn_parts.read().clone().unwrap();
            *self.checker.lock() = Box::new(file.checker());
            self.parts = (file, area, related);
        }

        let (file, area, _) = &self.parts;
        let mut file = file.write();
        let cfg = file.print_cfg();
        file.text_mut().remove_cursors(area, cfg);

        let guard = WriteFileGuard { file, area };
        (guard, area)
    }

    /// Wether the [`File`] within was switched to another
    pub fn has_swapped(&self) -> bool {
        self.dyn_parts.has_changed()
    }

    pub fn has_changed(&self) -> bool {
        let has_swapped = self.dyn_parts.has_changed();
        let (file, ..) = &self.parts;

        file.has_changed() || has_swapped
    }

    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + 'static + use<U> {
        let dyn_checker = self.dyn_parts.checker();
        let checker = self.checker.clone();
        move || checker.lock()() || { dyn_checker() }
    }

    // NOTE: Doesn't return result, since it is expected that widgets can
    // only be created after the file exists.
    pub fn file_ptr_eq(&self, other: &Node<U>) -> bool {
        other.ptr_eq(&self.dyn_parts.raw_read().as_ref().unwrap().0)
    }

    pub(crate) fn inspect_related<T: 'static, R>(&mut self, f: impl FnOnce(&T) -> R) -> Option<R> {
        if self.dyn_parts.has_changed() {
            let (file, area, related) = self.dyn_parts.read().clone().unwrap();
            *self.checker.lock() = Box::new(file.checker());
            self.parts = (file, area, related);
        }
        let (file, _, related) = &self.parts;

        if file.data_is::<T>() {
            file.read_as().map(|w| f(&w))
        } else {
            let related = related.read();
            related
                .iter()
                .find(|node| node.data_is::<T>())
                .and_then(|w| w.read_as().map(|w| f(&w)))
        }
    }
}

impl<U: Ui> Clone for DynamicFile<U> {
    fn clone(&self) -> Self {
        let (file, area, related) = self.parts.clone();
        let checker = file.checker();
        Self {
            parts: (file, area, related),
            dyn_parts: self.dyn_parts.clone(),
            checker: Arc::new(Mutex::new(Box::new(checker))),
        }
    }
}

pub struct WriteFileGuard<'a, U: Ui> {
    file: WriteDataGuard<'a, File>,
    area: &'a U::Area,
}

impl<U: Ui> std::ops::Deref for WriteFileGuard<'_, U> {
    type Target = File;

    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

impl<U: Ui> std::ops::DerefMut for WriteFileGuard<'_, U> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.file
    }
}

// TODO: Make this not update the RwData if the File hasn't changed
impl<U: Ui> Drop for WriteFileGuard<'_, U> {
    fn drop(&mut self) {
        let cfg = self.file.print_cfg();

        if let Some(main) = self.file.cursors().get_main() {
            self.area
                .scroll_around_point(self.file.text(), main.caret(), cfg);
        }
        self.file.text_mut().add_cursors(self.area, cfg);

        <File as Widget<U>>::update(&mut self.file, self.area);
        <File as Widget<U>>::print(&mut self.file, self.area);
    }
}

/// The notifications sent to Duat.
///
/// This can include command results, failed mappings, recompilation
/// messages, and any other thing that you want to [push] to be
/// notified.
///
/// [push]: Notifications::push
#[derive(Clone)]
pub struct Notifications(RwData<Vec<Text>>);

impl Notifications {
    /// Reads the notifications that were sent to Duat
    pub fn read(&mut self) -> ReadDataGuard<Vec<Text>> {
        self.0.read()
    }

	/// Wether there are new notifications or not
    pub fn has_changed(&self) -> bool {
        self.0.has_changed()
    }

    /// Checker for new notifications
    pub fn checker(&self) -> impl Fn() -> bool + Send + Sync + use<> {
        self.0.checker()
    }

    /// Pushes a new [notification] to Duat
    ///
    /// [notification]: Text
    pub fn push(&mut self, text: Text) {
        self.0.write().push(text)
    }
}

#[doc(hidden)]
pub struct CurWidget<U: Ui>(RwData<Option<Node<U>>>);

impl<U: Ui> CurWidget<U> {
    pub fn new() -> Self {
        Self(RwData::new(None))
    }

    pub fn type_id(&self) -> TypeId {
        self.0.type_id
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&dyn Widget<U>, &U::Area) -> R) -> R {
        let data = self.0.raw_read();
        let (widget, area, _) = data.as_ref().unwrap().parts();
        let widget = widget.read();

        f(&*widget, area)
    }

    pub fn inspect_as<W: Widget<U>, R>(&self, f: impl FnOnce(&W, &U::Area) -> R) -> Option<R> {
        let data = self.0.raw_read();
        let (widget, area, _) = data.as_ref().unwrap().parts();

        widget.read_as().map(|widget| f(&widget, area))
    }

    pub(crate) fn mutate_data<R>(
        &self,
        f: impl FnOnce(&RwData<dyn Widget<U>>, &U::Area, &Related<U>) -> R,
    ) -> R {
        let data = self.0.read();
        let (widget, area, related) = data.as_ref().unwrap().parts();

        f(widget, area, related)
    }

    pub(crate) fn mutate_data_as<W: Widget<U>, R>(
        &self,
        f: impl FnOnce(&RwData<W>, &U::Area, &Related<U>) -> R,
    ) -> Option<R> {
        let data = self.0.read();
        let (widget, area, related) = data.as_ref().unwrap().parts();

        Some(f(&widget.try_downcast::<W>()?, area, related))
    }

    pub(crate) fn node(&self) -> Node<U> {
        self.0.read().as_ref().unwrap().clone()
    }
}

impl<U: Ui> Default for CurWidget<U> {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) type FileParts<U> = (RwData<File>, <U as Ui>::Area, RwData<Vec<Node<U>>>);
