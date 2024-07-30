use std::{
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        LazyLock,
    },
    thread::JoinHandle,
};

use super::{private::InnerData, RoData, RwData};
use crate::{
    commands::Commands, input::InputMethod, log_info, ui::Ui, widgets::{ActiveWidget, File, PassiveWidget, RelatedWidgets, Widget}, Error, Result
};

pub struct Context<U>
where
    U: Ui,
{
    pub commands: &'static Commands<U>,
    cur_file: &'static CurFile<U>,
    cur_widget: &'static CurWidget<U>,
    handles: &'static AtomicUsize,
    has_ended: &'static AtomicBool,
}

impl<U> Clone for Context<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        *self
    }
}
impl<U> Copy for Context<U> where U: Ui {}

impl<U> Context<U>
where
    U: Ui,
{
    pub const fn new(
        current_file: &'static CurFile<U>,
        current_widget: &'static CurWidget<U>,
        commands: &'static Commands<U>,
        handles: &'static AtomicUsize,
        has_ended: &'static AtomicBool,
    ) -> Self {
        Self {
            cur_file: current_file,
            cur_widget: current_widget,
            commands,
            handles,
            has_ended,
        }
    }

    pub fn spawn<R: Send + 'static>(
        &self,
        f: impl FnOnce() -> R + Send + 'static,
    ) -> JoinHandle<R> {
        self.handles.fetch_add(1, Ordering::Relaxed);
        std::thread::spawn(|| {
            let ret = f();
            self.handles.fetch_sub(1, Ordering::Relaxed);
            ret
        })
    }

    pub fn has_ended(&self) -> bool {
        self.has_ended.load(Ordering::Relaxed)
    }

    pub fn fixed_reader(&self) -> Result<FileReader<U>, File> {
        self.cur_file.0.read().as_ref().ok_or(Error::NoFileYet)?;
        Ok(self.cur_file.fixed_reader())
    }

    pub fn dyn_reader(&self) -> Result<FileReader<U>, File> {
        self.cur_file.0.read().as_ref().ok_or(Error::NoFileYet)?;
        Ok(self.cur_file.dyn_reader())
    }

    pub fn cur_file(self) -> Result<&'static CurFile<U>, File> {
        self.cur_file.0.read().as_ref().ok_or(Error::NoFileYet)?;
        Ok(self.cur_file)
    }

    pub fn cur_widget(self) -> Result<&'static CurWidget<U>, File> {
        // `cur_widget doesn't exist iff cur_file doesn't exist`.
        self.cur_file.0.read().as_ref().ok_or(Error::NoWidgetYet)?;
        Ok(self.cur_widget)
    }

    pub(crate) fn threads_are_running(&self) -> bool {
        self.handles.load(Ordering::Relaxed) > 0
    }

    pub(crate) fn end(&self) {
        self.has_ended.store(true, Ordering::Relaxed);
    }

    pub(crate) fn set(&self, parts: FileParts<U>, widget: Widget<U>) {
        let area = parts.1.clone();
        *self.cur_file.0.write() = Some(parts);
        *self.cur_widget.0.write() = Some((widget, area));
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

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> String {
        self.0.raw_read().as_ref().unwrap().0.read().name()
    }

    // NOTE: Doesn't return result, since it is expected that widgets can
    // only be created after the file exists.
    pub fn file_ptr_eq(&self, other: &Widget<U>) -> bool {
        other.ptr_eq(&self.0.read().as_ref().unwrap().0)
    }

    pub(crate) fn mutate_related<T: 'static, R>(&self, f: impl FnOnce(&mut T) -> R) -> Option<R> {
        let data = self.0.raw_read();
        let (file, _, input, related) = data.as_ref().unwrap();

        if file.data_is::<T>() {
            file.mutate_as(f)
        } else if input.data_is::<T>() {
            input.mutate_as(f)
        } else {
            let related = related.read();
            related
                .iter()
                .find(|(widget, ..)| widget.data_is::<T>())
                .and_then(|(widget, ..)| widget.mutate_as(f))
        }
    }

    pub(crate) fn mutate_related_widget<W: 'static, R>(
        &self,
        f: impl FnOnce(&mut W, &mut U::Area) -> R,
    ) -> Option<R> {
        let data = self.0.raw_read();
        let (.., related) = data.as_ref().unwrap();
        let mut related = related.write();

        related
            .iter_mut()
            .find(|(widget, ..)| widget.data_is::<W>())
            .and_then(|(widget, area, _)| widget.mutate_as::<W, R>(|widget| f(widget, area)))
    }

    pub(crate) fn swap(&self, parts: FileParts<U>) -> FileParts<U> {
        std::mem::replace(self.0.write().as_mut().unwrap(), parts)
    }

    pub(crate) fn set(&self, parts: FileParts<U>) {
        *self.0.write() = Some(parts);
    }

    pub(crate) fn get_related_widget(
        &self,
        type_name: &str,
    ) -> Option<(RwData<dyn PassiveWidget<U>>, U::Area)> {
        let data = self.0.write();
        let (.., related) = data.as_ref().unwrap();
        let related = related.read();

        related.iter().find_map(|(widget, area, cmp)| {
            (*cmp == type_name).then(|| (widget.clone(), area.clone()))
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

    /// The name of the active [`FileWidget`]'s file.
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

    pub(crate) fn mutate_as<T: 'static, R>(&self, mut f: impl FnMut(&mut T) -> R) -> Option<R> {
        let data = self.0.read();
        let (widget, _) = data.as_ref().unwrap();
        let (widget, input) = widget.as_active().unwrap();

        widget
            .mutate_as::<T, R>(&mut f)
            .or_else(|| input.mutate_as::<T, R>(f))
    }

    pub(crate) fn set(&self, widget: Widget<U>, area: U::Area) {
        *self.0.write() = Some((widget, area.clone()));
    }

    pub(crate) fn mutate_data<R>(
        &self,
        f: impl FnOnce(&RwData<dyn ActiveWidget<U>>, &U::Area, &RwData<dyn InputMethod<U>>) -> R,
    ) -> R {
        let data = self.0.read();
        let (widget, area) = data.as_ref().unwrap();
        let (widget, input) = widget.as_active().unwrap();

        f(widget, area, input)
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
