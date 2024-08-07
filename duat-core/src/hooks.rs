use std::{
    any::TypeId,
    collections::HashMap,
    marker::PhantomData,
    sync::{atomic::AtomicUsize, LazyLock},
};

pub use global::*;
use parking_lot::RwLock;

use crate::{
    data::RwData,
    ui::{FileBuilder, Ui, WindowBuilder},
    widgets::ActiveWidget,
};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

mod global {
    use super::{Hookable, Hooks, COUNTER};

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
        COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        HOOKS.group_exists(group)
    }
}

pub trait Hookable: Sized + 'static {
    type Args<'args>;
}

trait HookHolder: Send + Sync {
    fn remove_group(&mut self, group: &str);
}

struct HooksOf<H>(RwLock<Vec<Hook<H>>>)
where
    H: Hookable;

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove_group(&mut self, group: &str) {
        let mut hooks = None;
        while hooks.is_none() {
            hooks = self.0.try_write();
        }
        hooks.unwrap().extract_if(|(g, _)| *g == group).last();
    }
}

struct Hooks {
    types: LazyLock<RwLock<HashMap<TypeId, Box<dyn HookHolder>>>>,
    groups: LazyLock<RwLock<Vec<&'static str>>>,
}

impl Hooks {
    const fn new() -> Self {
        Hooks {
            types: LazyLock::new(RwLock::default),
            groups: LazyLock::new(RwLock::default),
        }
    }

    fn add<H: Hookable>(
        &self,
        group: &'static str,
        f: impl for<'a> FnMut(&H::Args<'a>) + Send + Sync + 'static,
    ) {
        let mut map = self.types.write();

        if !group.is_empty() {
            let mut groups = self.groups.write();
            if !groups.contains(&group) {
                groups.push(group)
            }
        }

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

    fn remove(&'static self, group: &'static str) {
        crate::thread::spawn(move || {
            self.groups.write().extract_if(|g| *g == group).next();
            let mut map = self.types.write();
            for holder in map.iter_mut() {
                holder.1.remove_group(group)
            }
        });
    }

    fn trigger<H: Hookable>(&self, args: H::Args<'_>) {
        let map = self.types.read();

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

    fn group_exists(&self, group: &str) -> bool {
        self.groups.read().contains(&group)
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
