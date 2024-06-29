use std::{
    mem::MaybeUninit,
    sync::{
        atomic::{AtomicUsize, Ordering},
        LazyLock,
    },
};

use super::{private::InnerData, RoData, RwData};
use crate::{
    input::InputMethod,
    ui::Ui,
    widgets::{ActiveWidget, File, PassiveWidget, RelatedWidgets, Widget},
};

pub struct CurrentFile<U>
where
    U: Ui,
{
    cur: LazyLock<RwData<MaybeUninit<FileParts<U>>>>,
}

impl<U> CurrentFile<U>
where
    U: Ui,
{
    pub const fn new() -> Self {
        Self {
            cur: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub fn constant(&self) -> FileReader<U> {
        let data = self.cur.raw_read();
        let (file, area, input, related) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::new(MaybeUninit::new((
                file.clone(),
                area.clone(),
                input.clone(),
                related.clone(),
            ))),
            file_state: AtomicUsize::new(file.cur_state().load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state().load(Ordering::Relaxed)),
        }
    }

    pub fn adaptive(&self) -> FileReader<U> {
        let data = self.cur.raw_read();
        let (file, _, input, _) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::from(&*self.cur),
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&File, &U::Area, &dyn InputMethod<U>) -> R) -> R {
        let data = self.cur.raw_read();
        let (file, area, input, _) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&file.read(), area, input))
    }

    pub fn inspect_as<I: InputMethod<U>, R>(
        &self,
        f: impl FnOnce(&File, &U::Area, &I) -> R,
    ) -> Option<R> {
        let data = self.cur.raw_read();
        let (file, area, input, _) = unsafe { data.assume_init_ref() };

        input.inspect_as::<I, R>(|input| f(&file.read(), area, input))
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> String {
        unsafe { self.cur.raw_read().assume_init_ref().0.raw_read().name() }
    }

    pub fn file_ptr_eq(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.cur.read().assume_init_ref().0) }
    }

    pub(crate) fn mutate_related<T: 'static, R>(&self, f: impl FnOnce(&mut T) -> R) -> Option<R> {
        let data = self.cur.raw_read();
        let (file, _, input, related) = unsafe { data.assume_init_ref() };

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
        let data = self.cur.raw_read();
        let (.., related) = unsafe { data.assume_init_ref() };
        let mut related = related.write();

        related
            .iter_mut()
            .find(|(widget, ..)| widget.data_is::<W>())
            .and_then(|(widget, area, _)| widget.mutate_as::<W, R>(|widget| f(widget, area)))
    }

    pub(crate) fn swap(&self, parts: FileParts<U>) -> FileParts<U> {
        unsafe { std::mem::replace(&mut *self.cur.write(), MaybeUninit::new(parts)).assume_init() }
    }

    pub(crate) fn set(&self, parts: FileParts<U>) {
        *self.cur.write() = MaybeUninit::new(parts);
    }

    pub(crate) fn get_related_widget(
        &self,
        type_name: &str,
    ) -> Option<(RwData<dyn PassiveWidget<U>>, U::Area)> {
        let mut data = self.cur.write();
        let (.., related) = unsafe { data.assume_init_mut() };
        let related = related.read();

        related.iter().find_map(|(widget, area, cmp)| {
            (*cmp == type_name).then(|| (widget.clone(), area.clone()))
        })
    }

    pub(crate) fn add_related_widget(
        &self,
        new: (RwData<dyn PassiveWidget<U>>, U::Area, &'static str),
    ) {
        let mut data = self.cur.write();
        let (.., related) = unsafe { data.assume_init_mut() };
        let mut related = related.write();

        related.push(new)
    }
}

impl<U> Default for CurrentFile<U>
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
    data: RoData<MaybeUninit<FileParts<U>>>,
    file_state: AtomicUsize,
    input_state: AtomicUsize,
}

impl<U> FileReader<U>
where
    U: Ui,
{
    pub fn inspect<R>(&self, f: impl FnOnce(&File, &U::Area, &dyn InputMethod<U>) -> R) -> R {
        let data = self.data.read();
        let (file, area, input, _) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        input.inspect(|input| f(&file.read(), area, input))
    }

    pub fn inspect_sized<I: InputMethod<U>, R>(
        &self,
        f: impl FnOnce(&File, &U::Area, &I) -> R,
    ) -> Option<R> {
        let data = self.data.read();
        let (file, area, input, _) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        file.inspect_as::<I, R>(|input| f(&file.raw_read(), area, input))
    }

    pub fn inspect_related<T: 'static, R>(&self, f: impl FnOnce(&T) -> R) -> Option<R> {
        let data = self.data.read();
        let (file, _, input, related) = unsafe { data.assume_init_ref() };

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
        let (file, _, input, related) = unsafe { data.assume_init_ref() };

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
        unsafe { self.data.read().assume_init_ref().0.read().name() }
    }

    pub fn has_swapped(&self) -> bool {
        self.data.has_changed()
    }

    pub fn has_changed(&self) -> bool {
        let mut has_changed = self.data.has_changed();
        let data = self.data.read();
        let (file, _, input, _) = unsafe { data.assume_init_ref() };

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
        let data = self.data.read();
        let (file, _, input, _) = unsafe { data.assume_init_ref() };

        Self {
            data: self.data.clone(),
            file_state: AtomicUsize::new(file.cur_state().load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state().load(Ordering::Relaxed)),
        }
    }
}

pub struct CurrentWidget<U>
where
    U: Ui,
{
    cur: LazyLock<RwData<MaybeUninit<(Widget<U>, U::Area)>>>,
}

impl<U> CurrentWidget<U>
where
    U: Ui,
{
    pub const fn new() -> Self {
        Self {
            cur: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub fn type_name_is(&self, name: &'static str) -> bool {
        let data = self.cur.raw_read();
        let (widget, _) = unsafe { data.assume_init_ref() };

        widget.type_name() == name
    }

    pub fn inspect<R>(
        &self,
        f: impl FnOnce(&dyn ActiveWidget<U>, &U::Area, &dyn InputMethod<U>) -> R,
    ) -> R {
        let data = self.cur.raw_read();
        let (widget, area) = unsafe { data.assume_init_ref() };
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
        let data = self.cur.raw_read();
        let (widget, area) = unsafe { data.assume_init_ref() };
        let (widget, input) = widget.as_active().unwrap();

        let input = input.read();
        widget.inspect_as::<W, R>(|widget| f(widget, area, &*input))
    }

    pub fn inspect_as<W, I, R>(
        &self,
        f: impl FnOnce(&W, &U::Area, &dyn InputMethod<U>) -> R,
    ) -> Option<R>
    where
        W: ActiveWidget<U>,
        I: InputMethod<U>,
    {
        let data = self.cur.raw_read();
        let (widget, area) = unsafe { data.assume_init_ref() };
        let (widget, input) = widget.as_active().unwrap();

        input
            .inspect_as::<I, Option<R>>(|input| {
                widget.inspect_as::<W, R>(|widget| f(widget, area, input))
            })
            .flatten()
    }

    pub fn widget_ptr_eq(&self, other: &Widget<U>) -> bool {
        let data = self.cur.raw_read();
        let (widget, _) = unsafe { data.assume_init_ref() };
        let (widget, _) = widget.as_active().unwrap();

        other.ptr_eq(widget)
    }

    pub(crate) fn mutate_as<T: 'static, R>(&self, mut f: impl FnMut(&mut T) -> R) -> Option<R> {
        let data = self.cur.read();
        let (widget, _) = unsafe { data.assume_init_ref() };
        let (widget, input) = widget.as_active().unwrap();

        widget
            .mutate_as::<T, R>(&mut f)
            .or_else(|| input.mutate_as::<T, R>(f))
    }

    pub(crate) fn set(&self, widget: Widget<U>, area: U::Area) {
        *self.cur.write() = MaybeUninit::new((widget, area.clone()));
    }

    pub(crate) fn mutate_data<R>(
        &self,
        f: impl FnOnce(&RwData<dyn ActiveWidget<U>>, &U::Area, &RwData<dyn InputMethod<U>>) -> R,
    ) -> R {
        let data = self.cur.read();
        let (widget, area) = unsafe { data.assume_init_ref() };
        let (widget, input) = widget.as_active().unwrap();

        f(widget, area, input)
    }
}

impl<U> Default for CurrentWidget<U>
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
