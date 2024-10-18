use std::sync::{
    LazyLock,
    atomic::{AtomicUsize, Ordering},
};

pub use self::global::*;
use super::{RoData, RwData, private::InnerData};
use crate::{
    input::InputMethod,
    ui::{Area, Ui},
    widgets::{ActiveWidget, File, PassiveWidget, RelatedWidgets, Widget},
};

mod global {
    use std::{
        any::Any,
        sync::{
            LazyLock, OnceLock,
            atomic::{AtomicUsize, Ordering},
        },
    };

    use super::{CurFile, CurWidget, FileParts, FileReader};
    use crate::{
        Error, Result,
        data::RwData,
        text::Text,
        ui::{Ui, Window},
        widgets::{CommandLine, CommandLineMode, File, Widget},
    };

    static CUR_FILE: OnceLock<&'static (dyn Any + Send + Sync)> = OnceLock::new();
    static CUR_WIDGET: OnceLock<&'static (dyn Any + Send + Sync)> = OnceLock::new();
    static CUR_WINDOW: AtomicUsize = AtomicUsize::new(0);
    static WINDOWS: OnceLock<&'static (dyn Any + Send + Sync)> = OnceLock::new();
    static NOTIFICATIONS: LazyLock<RwData<Text>> = LazyLock::new(RwData::default);

    pub fn fixed_reader<U: Ui>() -> Result<FileReader<U>, File> {
        Ok(cur_file()?.fixed_reader())
    }

    pub fn dyn_reader<U: Ui>() -> Result<FileReader<U>, File> {
        Ok(cur_file()?.dyn_reader())
    }

    pub fn cur_file<U: Ui>() -> Result<&'static CurFile<U>, File> {
        let cur_file = inner_cur_file();
        cur_file.0.read().as_ref().ok_or(Error::NoFileYet)?;
        Ok(cur_file)
    }

    pub fn cur_widget<U: Ui>() -> Result<&'static CurWidget<U>, File> {
        let cur_widget = inner_cur_widget();
        cur_widget.0.read().as_ref().ok_or(Error::NoWidgetYet)?;
        Ok(cur_widget)
    }

    pub fn cur_window() -> usize {
        CUR_WINDOW.load(Ordering::Relaxed)
    }

    pub fn set_cmd_mode<M: CommandLineMode<U>, U: Ui>() {
        inner_cur_file::<U>()
            .mutate_related_widget::<CommandLine<U>, ()>(|w, _| w.write().set_mode::<M>())
            .unwrap_or_else(|| {
                let windows = windows::<U>().read();
                let w = cur_window();
                let cur_window = &windows[w];

                let mut widgets = {
                    let previous = windows[..w].iter().flat_map(Window::widgets);
                    let following = windows[(w + 1)..].iter().flat_map(Window::widgets);
                    cur_window.widgets().chain(previous).chain(following)
                };

                if let Some(cmd_line) = widgets.find_map(|(w, _)| {
                    w.data_is::<CommandLine<U>>()
                        .then(|| w.downcast::<CommandLine<U>>().unwrap())
                }) {
                    cmd_line.write().set_mode::<M>()
                }
            })
    }

    pub fn notifications() -> &'static RwData<Text> {
        &NOTIFICATIONS
    }

    pub fn notify(msg: Text) {
        *NOTIFICATIONS.write() = msg
    }

    pub fn setup<U: Ui>(
        cur_file: &'static CurFile<U>,
        cur_widget: &'static CurWidget<U>,
        cur_window: usize,
        windows: &'static RwData<Vec<Window<U>>>,
    ) {
        CUR_FILE.set(cur_file).expect("setup ran twice");
        CUR_WIDGET.set(cur_widget).expect("setup ran twice");
        CUR_WINDOW.store(cur_window, Ordering::Relaxed);
        WINDOWS.set(windows).expect("setup ran twice");
    }

    pub(crate) fn set_cur<U: Ui>(parts: FileParts<U>, widget: Widget<U>) {
        let area = parts.1.clone();
        *inner_cur_file().0.write() = Some(parts);
        *inner_cur_widget().0.write() = Some((widget, area));
    }

    pub(crate) fn set_windows<U: Ui>(
        win: Vec<Window<U>>,
    ) -> (&'static RwData<Vec<Window<U>>>, &'static AtomicUsize) {
        let windows = windows();
        *windows.write() = win;
        (windows, &CUR_WINDOW)
    }

    pub(crate) fn windows<U: Ui>() -> &'static RwData<Vec<Window<U>>> {
        WINDOWS
            .get()
            .unwrap()
            .downcast_ref::<RwData<Vec<Window<U>>>>()
            .expect("You are using more than one UI!!! Stop!")
    }

    pub(crate) fn inner_cur_file<U: Ui>() -> &'static CurFile<U> {
        CUR_FILE
            .get()
            .unwrap()
            .downcast_ref::<CurFile<U>>()
            .expect("You are using more than one UI!!! Stop!")
    }

    pub(crate) fn inner_cur_widget<U: Ui>() -> &'static CurWidget<U> {
        CUR_WIDGET
            .get()
            .unwrap()
            .downcast_ref::<CurWidget<U>>()
            .expect("You are using more than one UI!!! Stop!")
    }
}

pub struct CurFile<U>(LazyLock<RwData<Option<FileParts<U>>>>)
where
    U: Ui;

impl<U> CurFile<U>
where
    U: Ui,
{
    pub const fn new() -> Self {
        Self(LazyLock::new(|| RwData::new(None)))
    }

    pub fn fixed_reader(&self) -> FileReader<U> {
        let data = self.0.raw_read();
        let (file, area, input, related) = data.clone().unwrap();
        let file_state = AtomicUsize::new(file.cur_state().load(Ordering::Relaxed));
        let input_state = AtomicUsize::new(input.cur_state().load(Ordering::Relaxed));

        FileReader {
            data: RoData::new(Some((file, area, input, related))),
            file_state,
            input_state,
        }
    }

    pub fn dyn_reader(&self) -> FileReader<U> {
        let data = self.0.raw_read();
        let (file, _, input, _) = data.clone().unwrap();

        FileReader {
            data: RoData::from(&*self.0),
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&File, &U::Area, &dyn InputMethod<U>) -> R) -> R {
        let data = self.0.raw_read();
        let (file, area, input, _) = data.as_ref().unwrap();
        input.inspect(|input| f(&file.read(), area, input))
    }

    pub fn inspect_as<I: InputMethod<U>, R>(
        &self,
        f: impl FnOnce(&File, &U::Area, &I) -> R,
    ) -> Option<R> {
        let data = self.0.raw_read();
        let (file, area, input, _) = data.as_ref().unwrap();

        input.inspect_as::<I, R>(|input| f(&file.read(), area, input))
    }

    /// The name of the active [`File`]'s file.
    pub fn name(&self) -> String {
        self.0.raw_read().as_ref().unwrap().0.read().name()
    }

    /// The name of the active [`File`]'s file.
    pub fn path(&self) -> String {
        self.0.raw_read().as_ref().unwrap().0.read().path()
    }

    // NOTE: Doesn't return result, since it is expected that widgets can
    // only be created after the file exists.
    pub fn file_ptr_eq(&self, other: &Widget<U>) -> bool {
        other.ptr_eq(&self.0.read().as_ref().unwrap().0)
    }

    pub(crate) fn mutate_related<T: 'static, R>(
        &self,
        f: impl FnOnce(&RwData<T>) -> R,
    ) -> Option<R> {
        let data = self.0.raw_read();
        let (file, _, input, rel) = data.as_ref().unwrap();

        file.try_downcast()
            .or_else(|| input.try_downcast())
            .or_else(|| {
                let rel = rel.read();
                rel.iter().find_map(|(widget, ..)| widget.try_downcast())
            })
            .map(|rel| f(&rel))
    }

    pub(crate) fn mutate_data<R>(
        &self,
        f: impl FnOnce(&RwData<File>, &U::Area, &RwData<dyn InputMethod<U>>) -> R,
    ) -> R {
        let data = self.0.raw_read();
        let (file, area, input, _) = data.as_ref().unwrap();

        input.inspect(|input| {
            if let Some(cursors) = input.cursors() {
                <File as ActiveWidget<U>>::text_mut(&mut file.write()).remove_cursor_tags(cursors);
            }
        });

        let ret = f(file, area, input);

        let input = input.read();
        let mut file = file.write();
        if let Some(cursors) = input.cursors() {
            <File as ActiveWidget<U>>::text_mut(&mut file).add_cursor_tags(cursors);

            area.scroll_around_point(
                file.text(),
                cursors.main().caret(),
                <File as PassiveWidget<U>>::print_cfg(&file),
            );
        }

        <File as PassiveWidget<U>>::update(&mut file, area);
        <File as PassiveWidget<U>>::print(&mut file, area);

        ret
    }

    pub(crate) fn mutate_related_widget<W: 'static, R>(
        &self,
        f: impl FnOnce(&RwData<W>, &U::Area) -> R,
    ) -> Option<R> {
        let data = self.0.raw_read();
        let (file, area, _, rel) = data.as_ref().unwrap();
        let rel = rel.read();

        file.try_downcast()
            .map(|w| (w, area))
            .or_else(|| {
                rel.iter()
                    .find_map(|(widget, area, _)| widget.try_downcast().zip(Some(area)))
            })
            .map(|(widget, area)| f(&widget, area))
    }

    pub(crate) fn swap(&self, parts: FileParts<U>) -> FileParts<U> {
        std::mem::replace(self.0.write().as_mut().unwrap(), parts)
    }

    pub(crate) fn set(&self, parts: FileParts<U>) {
        *self.0.write() = Some(parts);
    }

    pub(crate) fn get_related_widget(
        &self,
        name: &str,
    ) -> Option<(RwData<dyn PassiveWidget<U>>, U::Area)> {
        let data = self.0.write();
        let (.., related) = data.as_ref().unwrap();
        let related = related.read();

        related.iter().find_map(|(widget, area, w_name)| {
            (*w_name == name).then_some((widget.clone(), area.clone()))
        })
    }

    pub(crate) fn add_related_widget(
        &self,
        new: (RwData<dyn PassiveWidget<U>>, U::Area, &'static str),
    ) {
        let data = self.0.write();
        let (.., related) = data.as_ref().unwrap();
        let mut related = related.write();

        related.push(new)
    }
}

impl<U> Default for CurFile<U>
where
    U: Ui,
{
    fn default() -> Self {
        Self::new()
    }
}

pub struct FileReader<U>
where
    U: Ui,
{
    data: RoData<Option<FileParts<U>>>,
    file_state: AtomicUsize,
    input_state: AtomicUsize,
}

impl<U> FileReader<U>
where
    U: Ui,
{
    pub fn inspect<R>(&self, f: impl FnOnce(&File, &U::Area, &dyn InputMethod<U>) -> R) -> R {
        let data = self.data.read();
        let (file, area, input, _) = data.as_ref().unwrap();

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        input.inspect(|input| f(&file.read(), area, input))
    }

    pub fn inspect_input<I: InputMethod<U>, R>(
        &self,
        f: impl FnOnce(&File, &U::Area, &I) -> R,
    ) -> Option<R> {
        let data = self.data.read();
        let (file, area, input, _) = data.as_ref().unwrap();

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        file.inspect_as::<I, R>(|input| f(&file.raw_read(), area, input))
    }

    pub fn inspect_related<T: 'static, R>(&self, f: impl FnOnce(&T) -> R) -> Option<R> {
        let data = self.data.read();
        let (file, _, input, related) = data.as_ref().unwrap();

        if file.data_is::<T>() {
            file.inspect_as(f)
        } else if input.data_is::<T>() {
            input.inspect_as(f)
        } else {
            let related = related.read();
            related
                .iter()
                .find_map(|(widget, ..)| widget.data_is::<T>().then_some(widget))
                .and_then(|widget| widget.inspect_as(f))
        }
    }

    pub fn inspect_file_and<T: 'static, R>(&self, f: impl FnOnce(&File, &T) -> R) -> Option<R> {
        let data = self.data.read();
        let (file, _, input, related) = data.as_ref().unwrap();

        if input.data_is::<T>() {
            input.inspect_as(|input| f(&file.read(), input))
        } else {
            let related = related.read();
            related
                .iter()
                .find_map(|(widget, ..)| widget.data_is::<T>().then_some(widget))
                .and_then(|widget| widget.inspect_as(|widget| f(&file.read(), widget)))
        }
    }

    /// The name of the active [`File`]'s file.
    pub fn name(&self) -> String {
        self.data.read().as_ref().unwrap().0.read().name()
    }

    pub fn has_swapped(&self) -> bool {
        self.data.has_changed()
    }

    pub fn has_changed(&self) -> bool {
        let mut has_changed = self.data.has_changed();
        let data = self.data.read();
        let (file, _, input, _) = data.as_ref().unwrap();

        has_changed |= {
            let file_state = file.cur_state().load(Ordering::Acquire);

            file_state > self.file_state.swap(file_state, Ordering::Acquire)
        };

        has_changed |= {
            let input_state = input.cur_state().load(Ordering::Acquire);

            input_state > self.input_state.swap(input_state, Ordering::Acquire)
        };

        has_changed
    }
}

impl<U> Clone for FileReader<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        let (file, _, input, _) = self.data.read().clone().unwrap();

        Self {
            data: self.data.clone(),
            file_state: AtomicUsize::new(file.cur_state().load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state().load(Ordering::Relaxed)),
        }
    }
}

pub struct CurWidget<U>(LazyLock<RwData<Option<(Widget<U>, U::Area)>>>)
where
    U: Ui;

impl<U> CurWidget<U>
where
    U: Ui,
{
    pub const fn new() -> Self {
        Self(LazyLock::new(|| RwData::new(None)))
    }

    pub fn type_name_is(&self, name: &'static str) -> bool {
        let data = self.0.raw_read();
        let (widget, _) = data.as_ref().unwrap();
        widget.type_name() == name
    }

    pub fn inspect<R>(
        &self,
        f: impl FnOnce(&dyn ActiveWidget<U>, &U::Area, &dyn InputMethod<U>) -> R,
    ) -> R {
        let data = self.0.raw_read();
        let (widget, area) = data.as_ref().unwrap();
        let (widget, input) = widget.as_active().unwrap();
        input.inspect(|input| f(&*widget.read(), area, input))
    }

    pub fn inspect_widget_as<W, R>(
        &self,
        f: impl FnOnce(&W, &U::Area, &dyn InputMethod<U>) -> R,
    ) -> Option<R>
    where
        W: ActiveWidget<U>,
    {
        let data = self.0.raw_read();
        let (widget, area) = data.as_ref().unwrap();
        let (widget, input) = widget.as_active().unwrap();
        let input = input.read();
        widget.inspect_as::<W, R>(|widget| f(widget, area, &*input))
    }

    pub fn inspect_as<W, I, R>(&self, f: impl FnOnce(&W, &U::Area, &I) -> R) -> Option<R>
    where
        W: ActiveWidget<U>,
        I: InputMethod<U>,
    {
        let data = self.0.raw_read();
        let (widget, area) = data.as_ref().unwrap();
        let (widget, input) = widget.as_active().unwrap();

        input
            .inspect_as::<I, Option<R>>(|input| {
                widget.inspect_as::<W, R>(|widget| f(widget, area, input))
            })
            .flatten()
    }

    pub fn widget_ptr_eq(&self, other: &Widget<U>) -> bool {
        let data = self.0.raw_read();
        let (widget, _) = data.as_ref().unwrap();
        let (widget, _) = widget.as_active().unwrap();

        other.ptr_eq(widget)
    }

    pub(crate) fn mutate_as<T: 'static, R>(&self, mut f: impl FnMut(&RwData<T>) -> R) -> Option<R> {
        let data = self.0.read();
        let (widget, _) = data.as_ref().unwrap();
        let (widget, input) = widget.as_active().unwrap();

        widget
            .try_downcast()
            .or_else(|| input.try_downcast())
            .map(|t| f(&t))
    }

    pub(crate) fn set(&self, widget: Widget<U>, area: U::Area) {
        *self.0.write() = Some((widget, area.clone()));
    }
}

impl<U> Default for CurWidget<U>
where
    U: Ui,
{
    fn default() -> Self {
        Self::new()
    }
}

type FileParts<U> = (
    RwData<File>,
    <U as Ui>::Area,
    RwData<dyn InputMethod<U>>,
    RelatedWidgets<U>,
);
