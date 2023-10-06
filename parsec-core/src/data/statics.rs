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
    ui::{Area, Ui},
    widgets::{ActiveWidget, File, PassiveWidget, Widget},
};

pub struct CurrentFile {
    rw: LazyLock<RwData<MaybeUninit<(RwData<File>, RwData<dyn InputMethod>)>>>,
    ro: LazyLock<RwData<MaybeUninit<(RoData<File>, RoData<dyn InputMethod>)>>>,
}

impl CurrentFile {
    pub fn constant(&self) -> FileReader {
        let data = self.ro.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::new(MaybeUninit::new((file.clone(), input.clone()))),
            file_state: AtomicUsize::new(file.cur_state().load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state().load(Ordering::Relaxed)),
        }
    }

    pub fn adaptive(&self) -> FileReader {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        FileReader {
            data: RoData::from(&*self.ro),
            file_state: AtomicUsize::new(file.cur_state.load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state.load(Ordering::Relaxed)),
        }
    }

    pub fn inspect<R>(&self, f: impl FnOnce(&File, &dyn InputMethod) -> R) -> R {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&file.read(), input))
    }

    pub fn inspect_as<I: InputMethod, R>(&self, f: impl FnOnce(&File, &I) -> R) -> Option<R> {
        let data = self.rw.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.inspect_as::<I, R>(|input| f(&file.read(), input))
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<File>, &RoData<dyn InputMethod>) -> R,
    ) -> R {
        let data = self.ro.raw_read();
        let (file, input) = unsafe { data.assume_init_ref() };

        f(file, input)
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> Option<String> {
        unsafe { self.rw.raw_read().assume_init_ref().0.raw_read().name() }
    }

    pub fn file_ptr_eq<U: Ui>(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.rw.read().assume_init_ref().0) }
    }

    pub(crate) fn mutate_related_widget<W: PassiveWidget, R>(
        &self,
        mut f: impl FnMut(&mut W, &mut dyn Area) -> R,
    ) -> Option<R> {
        let data = self.rw.raw_read();
        let (file, _) = unsafe { data.assume_init_ref() };
        let mut file = file.write();
        file.mutate_related_widget::<W, R>(&mut f)
    }

    pub(crate) fn get_related_widget(&self, type_name: &str) -> Option<RwData<dyn PassiveWidget>> {
        let data = self.rw.raw_read();
        let (file, _) = unsafe { data.assume_init_ref() };

		let file = file.read();
        file.get_related_widget(type_name)
    }

    pub(crate) fn mutate<R>(&self, f: impl FnOnce(&mut File, &mut dyn InputMethod) -> R) -> R {
        let data = self.rw.write();
        let (file, input) = unsafe { data.assume_init_ref() };

        input.mutate(|input| f(&mut file.write(), input))
    }

    pub(crate) const fn new() -> Self {
        Self {
            rw: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
            ro: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub(crate) fn swap(
        &self,
        file: RwData<File>,
        input: RwData<dyn InputMethod>,
    ) -> (RwData<File>, RwData<dyn InputMethod>) {
        *self.ro.write() = MaybeUninit::new((RoData::from(&file), RoData::from(&input)));

        unsafe {
            std::mem::replace(&mut *self.rw.write(), MaybeUninit::new((file, input))).assume_init()
        }
    }

    pub(crate) fn set(&self, file: RwData<File>, input: RwData<dyn InputMethod>) {
        *self.ro.write() = MaybeUninit::new((RoData::from(&file), RoData::from(&input)));
        *self.rw.write() = MaybeUninit::new((file, input));
    }
}

pub struct FileReader {
    data: RoData<MaybeUninit<(RoData<File>, RoData<dyn InputMethod>)>>,
    file_state: AtomicUsize,
    input_state: AtomicUsize,
}

impl FileReader {
    pub fn inspect<R>(&self, f: impl FnOnce(&File, &dyn InputMethod) -> R) -> R {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        input.inspect(|input| f(&file.read(), input))
    }

    pub fn inspect_as<I: InputMethod, R>(&self, f: impl FnOnce(&File, &I) -> R) -> Option<R> {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        self.file_state
            .store(file.cur_state().load(Ordering::Acquire), Ordering::Release);
        self.input_state
            .store(input.cur_state().load(Ordering::Acquire), Ordering::Release);

        file.inspect_as::<I, R>(|input| f(&file.raw_read(), input))
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<File>, &RoData<dyn InputMethod>) -> R,
    ) -> R {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        f(file, input)
    }

    /// The name of the active [`FileWidget`]'s file.
    pub fn name(&self) -> Option<String> {
        unsafe { self.data.read().assume_init_ref().0.read().name() }
    }

    pub fn has_swapped(&self) -> bool {
        self.data.has_changed()
    }

    pub fn has_changed(&self) -> bool {
        let mut has_changed = self.data.has_changed();
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

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

impl Clone for FileReader {
    fn clone(&self) -> Self {
        let data = self.data.read();
        let (file, input) = unsafe { data.assume_init_ref() };

        Self {
            data: self.data.clone(),
            file_state: AtomicUsize::new(file.cur_state().load(Ordering::Relaxed)),
            input_state: AtomicUsize::new(input.cur_state().load(Ordering::Relaxed)),
        }
    }
}

pub struct CurrentWidget {
    rw: LazyLock<RwData<MaybeUninit<(RwData<dyn ActiveWidget>, RwData<dyn InputMethod>)>>>,
    ro: LazyLock<RwData<MaybeUninit<(RoData<dyn ActiveWidget>, RoData<dyn InputMethod>)>>>,
}

impl CurrentWidget {
    pub fn inspect<R>(&self, f: impl FnOnce(&dyn ActiveWidget, &dyn InputMethod) -> R) -> R {
        let data = self.rw.raw_read();
        let (widget, input) = unsafe { data.assume_init_ref() };

        input.inspect(|input| f(&*widget.read(), input))
    }

    pub fn inspect_input_as<I, R>(&self, f: impl FnOnce(&dyn ActiveWidget, &I) -> R) -> Option<R>
    where
        I: InputMethod,
    {
        let data = self.rw.raw_read();
        let (widget, input) = unsafe { data.assume_init_ref() };

        input.inspect_as::<I, R>(|input| f(&*widget.read(), input))
    }

    pub fn inspect_widget_as<W, R>(&self, f: impl FnOnce(&W, &dyn InputMethod) -> R) -> Option<R>
    where
        W: ActiveWidget,
    {
        let data = self.rw.raw_read();
        let (widget, input) = unsafe { data.assume_init_ref() };

        let input = input.read();
        widget.inspect_as::<W, R>(|widget| f(widget, &*input))
    }

    pub fn inspect_as<W, I, R>(&self, f: impl FnOnce(&W, &dyn InputMethod) -> R) -> Option<R>
    where
        W: ActiveWidget,
        I: InputMethod,
    {
        let data = self.rw.raw_read();
        let (widget, input) = unsafe { data.assume_init_ref() };

        input
            .inspect_as::<I, Option<R>>(|input| {
                widget.inspect_as::<W, R>(|widget| f(widget, input))
            })
            .flatten()
    }

    pub fn inspect_data<R>(
        &self,
        f: impl FnOnce(&RoData<dyn ActiveWidget>, &RoData<dyn InputMethod>) -> R,
    ) -> R {
        let data = self.ro.raw_read();
        let (widget, input) = unsafe { data.assume_init_ref() };

        f(widget, input)
    }

    pub fn widget_ptr_eq<U: Ui>(&self, other: &Widget<U>) -> bool {
        unsafe { other.ptr_eq(&self.rw.read().assume_init_ref().0) }
    }

    pub(crate) const fn new() -> Self {
        Self {
            rw: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
            ro: LazyLock::new(|| RwData::new(MaybeUninit::uninit())),
        }
    }

    pub(crate) fn set(&self, widget: RwData<dyn ActiveWidget>, input: RwData<dyn InputMethod>) {
        *self.ro.write() = MaybeUninit::new((RoData::from(&widget), RoData::from(&input)));
        *self.rw.write() = MaybeUninit::new((widget, input));
    }
}
