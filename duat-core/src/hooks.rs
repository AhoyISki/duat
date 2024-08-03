use std::{any::TypeId, collections::HashMap, marker::PhantomData, sync::LazyLock};

pub use global::*;
use parking_lot::RwLock;

use crate::{
    data::RwData,
    ui::{FileBuilder, Ui, WindowBuilder},
    widgets::ActiveWidget,
};

mod global {
    use super::{Hookable, Hooks};

    static HOOKS: Hooks = Hooks::new();

    pub fn add<H: Hookable>(f: impl for<'a, 'b> FnMut(&H::Args<'a>) + Send + Sync + 'static) {
        HOOKS.add::<H>("", f)
    }

    pub fn add_grouped<H: Hookable>(
        group: &'static str,
        f: impl for<'a, 'b> FnMut(&H::Args<'a>) + Send + Sync + 'static,
    ) {
        HOOKS.add::<H>(group, f)
    }

    pub fn remove_group(group: &'static str) {
        HOOKS.remove(group)
    }

    pub fn trigger<H: Hookable>(args: H::Args<'_>) {
        HOOKS.trigger::<H>(args)
    }

    pub fn group_exists(group: &'static str) -> bool {
        HOOKS.group_exists(group)
    }
}

pub trait Hookable: Sized + 'static {
    type Args<'args>;
}

trait HookHolder: Send + Sync {
    fn remove_group(&mut self, group: &'static str);

    fn group_exists(&self, group: &'static str) -> bool;
}

struct HooksOf<H>(RwLock<Vec<Hook<H>>>)
where
    H: Hookable;

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove_group(&mut self, group: &'static str) {
        self.0.write().extract_if(|(g, _)| *g == group).last();
    }

    fn group_exists(&self, group: &'static str) -> bool {
        self.0.read().iter().any(|(g, _)| *g == group)
    }
}

struct Hooks(LazyLock<RwLock<HashMap<TypeId, Box<dyn HookHolder>>>>);

impl Hooks {
    const fn new() -> Self {
        Hooks(LazyLock::new(|| RwLock::new(HashMap::new())))
    }

    fn add<H: Hookable>(
        &self,
        group: &'static str,
        f: impl for<'a> FnMut(&H::Args<'a>) + Send + Sync + 'static,
    ) {
        let mut map = self.0.write();

        if let Some(holder) = map.get_mut(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&mut **holder as *mut dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_mut().unwrap()
            };

            hooks_of.0.write().push((group, Box::new(f)))
        } else {
            let hooks_of = HooksOf::<H>(RwLock::new(vec![(group, Box::new(f))]));

            map.insert(TypeId::of::<H>(), Box::new(hooks_of));
        }
    }

    fn remove(&self, group: &'static str) {
        let mut map = self.0.write();
        for holder in map.iter_mut() {
            holder.1.remove_group(group)
        }
    }

    fn trigger<H: Hookable>(&self, args: H::Args<'_>) {
        let map = self.0.read();

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            for (_, f) in &mut *hooks_of.0.write() {
                f(&args)
            }
        }
    }

    fn group_exists(&self, group: &'static str) -> bool {
        let map = self.0.read();
        map.iter().any(|(_, holder)| holder.group_exists(group))
    }
}

pub struct OnFileOpen<U>(PhantomData<U>)
where
    U: Ui;

impl<U> Hookable for OnFileOpen<U>
where
    U: Ui,
{
    type Args<'a> = &'a mut FileBuilder<'a, U>;
}

pub struct OnWindowOpen<U>(PhantomData<U>)
where
    U: Ui;

impl<U> Hookable for OnWindowOpen<U>
where
    U: Ui,
{
    type Args<'a> = WindowBuilder<'a, U>;
}

pub struct FocusedOn<W, U>(PhantomData<(W, U)>)
where
    W: ActiveWidget<U>,
    U: Ui;

impl<W, U> Hookable for FocusedOn<W, U>
where
    W: ActiveWidget<U>,
    U: Ui,
{
    type Args<'args> = &'args RwData<W>;
}

pub struct UnfocusedFrom<W, U>(PhantomData<(W, U)>)
where
    W: ActiveWidget<U>,
    U: Ui;

impl<W, U> Hookable for UnfocusedFrom<W, U>
where
    W: ActiveWidget<U>,
    U: Ui,
{
    type Args<'args> = &'args RwData<W>;
}

type Hook<H> = (
    &'static str,
    Box<dyn for<'a, 'b> FnMut(&<H as Hookable>::Args<'a>) + Send + Sync>,
);
