use std::{
    mem::MaybeUninit,
    sync::{
        atomic::{AtomicUsize, Ordering},
        LazyLock, RwLock,
    },
};

use super::{private::InnerData, RoData, RwData};
use crate::{
    input::InputMethod,
    ui::{Area, Ui},
    widgets::{ActiveWidget, File, PassiveWidget, Widget},
};

pub struct CurrentFile<U>
where
    U: Ui,
{
    rw: LazyLock<RwData<MaybeUninit<(RwData<File<U>>, U::Area, RwData<dyn InputMethod<U>>)>>>,
    ro: LazyLock<RwData<MaybeUninit<(RoData<File<U>>, U::Area, RoData<dyn InputMethod<U>>)>>>,
}

impl<U> CurrentFile<U>
where
    U: Ui,
{
    pub fn constant(&self) -> FileReader<U> {
        let data = self.ro.raw_read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::new(MaybeUninit::new((
                file.clone(),
                area.clone(),
                input.clone(),
            ))),
            file_state: AtomicUsize::new(file.cur_state().load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state().load(Ordering::Relaxed)),
        }
    }

    pub fn adaptive(&self) -> FileReader<U> {
        let data = self.rw.raw_read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::from(&*self.ro),
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&File<U>, &U::Area, &dyn InputMethod<U>) -> R) -> R {
        let data = self.rw.raw_read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&file.read(), area, input))
    }

    pub fn inspect_as<I: InputMethod<U>, R>(
        &self,
        f: impl FnOnce(&File<U>, &U::Area, &I) -> R,
    ) -> Option<R> {
        let data = self.rw.raw_read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        input.inspect_as::<I, R>(|input| f(&file.read(), area, input))
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<File<U>>, &U::Area, &RoData<dyn InputMethod<U>>) -> R,
    ) -> R {
        let data = self.ro.raw_read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        f(file, area, input)
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> String {
        unsafe { self.rw.raw_read().assume_init_ref().0.raw_read().name() }
    }

    pub fn file_ptr_eq(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.rw.read().assume_init_ref().0) }
    }

    pub(crate) fn mutate_related<T: 'static, R>(
        &self,
        mut f: impl FnMut(&mut T) -> R,
    ) -> Option<R> {
        let data = self.rw.raw_read();
        let (file, _, input) = unsafe { data.assume_init_ref() };
        file.mutate_as::<T, R>(&mut f)
            .or_else(|| {
                let file = file.read();
                file.mutate_related(&mut f)
            })
            .or_else(|| input.mutate_as(&mut f))
    }

    pub(crate) fn mutate_related_widget<W: PassiveWidget<U>, R>(
        &self,
        mut f: impl FnMut(&mut W, &mut U::Area) -> R,
    ) -> Option<R> {
        let data = self.rw.raw_read();
        let (file, area, _) = unsafe { data.assume_init_ref() };
        let mut file = file.write();
        file.mutate_related_widget::<W, R>(&mut f)
    }

    pub(crate) fn get_related_widget(
        &self,
        type_name: &str,
    ) -> Option<(RwData<dyn PassiveWidget<U>>, U::Area)> {
        let data = self.rw.raw_read();
        let (file, ..) = unsafe { data.assume_init_ref() };

        let file = file.read();
        file.get_related_widget(type_name)
    }

    pub(crate) fn mutate<R>(
        &self,
        f: impl FnOnce(&mut File<U>, &mut U::Area, &mut dyn InputMethod<U>) -> R,
    ) -> R {
        let mut data = self.rw.write();
        let (file, area, input) = unsafe { data.assume_init_mut() };

        input.mutate(|input| f(&mut file.write(), area, input))
    }

    pub(crate) const fn new() -> Self {
        Self {
            rw: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
            ro: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub(crate) fn swap(
        &self,
        file: RwData<File<U>>,
        area: U::Area,
        input: RwData<dyn InputMethod<U>>,
    ) -> (RwData<File<U>>, U::Area, RwData<dyn InputMethod<U>>) {
        *self.ro.write() =
            MaybeUninit::new((RoData::from(&file), area.clone(), RoData::from(&input)));

        unsafe {
            std::mem::replace(&mut *self.rw.write(), MaybeUninit::new((file, area, input)))
                .assume_init()
        }
    }

    pub(crate) fn set(
        &self,
        file: RwData<File<U>>,
        area: U::Area,
        input: RwData<dyn InputMethod<U>>,
    ) {
        *self.ro.write() = MaybeUninit::new((RoData::from(&file), area.clone(), RoData::from(&input)));
        *self.rw.write() = MaybeUninit::new((file, area, input));
    }
}

pub struct FileReader<U>
where
    U: Ui,
{
    data: RoData<MaybeUninit<(RoData<File<U>>, U::Area, RoData<dyn InputMethod<U>>)>>,
    file_state: AtomicUsize,
    input_state: AtomicUsize,
}

impl<U> FileReader<U>
where
    U: Ui,
{
    pub fn inspect<R>(&self, f: impl FnOnce(&File<U>, &U::Area, &dyn InputMethod<U>) -> R) -> R {
        let data = self.data.read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        input.inspect(|input| f(&file.read(), area, input))
    }

    pub fn inspect_as<I: InputMethod<U>, R>(
        &self,
        f: impl FnOnce(&File<U>, &U::Area, &I) -> R,
    ) -> Option<R> {
        let data = self.data.read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        file.inspect_as::<I, R>(|input| f(&file.raw_read(), area, input))
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<File<U>>, &U::Area, &RoData<dyn InputMethod<U>>) -> R,
    ) -> R {
        let data = self.data.read();
        let (file, area, input) = unsafe { data.assume_init_ref() };

        f(file, area, input)
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
        let (file, _, input) = unsafe { data.assume_init_ref() };

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
        let (file, _, input) = unsafe { data.assume_init_ref() };

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
    name: RwLock<&'static str>,
    rw: LazyLock<RwData<MaybeUninit<RwWidget<U>>>>,
    ro: LazyLock<RwData<MaybeUninit<RoWidget<U>>>>,
}

impl<U> CurrentWidget<U>
where
    U: Ui,
{
    pub fn has_name(&self, name: &'static str) -> bool {
        *self.name.read().unwrap() == name
    }

    pub fn inspect<R>(
        &self,
        f: impl FnOnce(&dyn ActiveWidget<U>, &U::Area, &dyn InputMethod<U>) -> R,
    ) -> R {
        let data = self.rw.raw_read();
        let (widget, area, input) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&*widget.read(), area, input))
    }

    pub fn inspect_widget_as<W, R>(
        &self,
        f: impl FnOnce(&W, &U::Area, &dyn InputMethod<U>) -> R,
    ) -> Option<R>
    where
        W: ActiveWidget<U>,
    {
        let data = self.rw.raw_read();
        let (widget, area, input) = unsafe { data.assume_init_ref() };

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
        let data = self.rw.raw_read();
        let (widget, area, input) = unsafe { data.assume_init_ref() };

        input
            .inspect_as::<I, Option<R>>(|input| {
                widget.inspect_as::<W, R>(|widget| f(widget, area, input))
            })
            .flatten()
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<dyn ActiveWidget<U>>, &U::Area, &RoData<dyn InputMethod<U>>) -> R,
    ) -> R {
        let data = self.ro.raw_read();
        let (widget, area, input) = unsafe { data.assume_init_ref() };

        f(widget, area, input)
    }

    pub fn widget_ptr_eq(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.rw.read().assume_init_ref().0) }
    }

    pub(crate) fn mutate_as<T: 'static, R>(&self, mut f: impl FnMut(&mut T) -> R) -> Option<R> {
        let data = self.rw.read();
        let (widget, _, input) = unsafe { data.assume_init_ref() };
        widget
            .mutate_as::<T, R>(&mut f)
            .or_else(|| input.mutate_as::<T, R>(f))
    }

    pub(crate) const fn new() -> Self {
        Self {
            name: RwLock::new("none"),
            rw: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
            ro: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub(crate) fn set(
        &self,
        name: &'static str,
        widget: RwData<dyn ActiveWidget<U>>,
        area: U::Area,
        input: RwData<dyn InputMethod<U>>,
    ) {
        *self.name.write().unwrap() = name;
        *self.ro.write() =
            MaybeUninit::new((RoData::from(&widget), area.clone(), RoData::from(&input)));
        *self.rw.write() = MaybeUninit::new((widget, area, input));
    }
}

pub type RoWidget<U: Ui> = (
    RoData<dyn ActiveWidget<U>>,
    U::Area,
    RoData<dyn InputMethod<U>>,
);

pub type RwWidget<U: Ui> = (
    RwData<dyn ActiveWidget<U>>,
    U::Area,
    RwData<dyn InputMethod<U>>,
);
