use std::{any::TypeId, collections::HashMap, marker::PhantomData, sync::LazyLock};

pub use global::*;
use parking_lot::RwLock;

use crate::ui::{FileBuilder, Ui, WindowBuilder};

mod global {
    use super::{Hookable, Hooks};

    static HOOKS: Hooks = Hooks::new();

    pub fn add<H: Hookable>(
        f: impl for<'a, 'b> FnMut(&'b mut H::Args<'a>) + Send + Sync + 'static,
    ) {
        HOOKS.add_hook::<H>("", f)
    }

    pub fn add_grouped<H: Hookable>(
        group: &'static str,
        f: impl for<'a, 'b> FnMut(&'b mut H::Args<'a>) + Send + Sync + 'static,
    ) {
        HOOKS.add_hook::<H>(group, f)
    }

    pub fn remove_group(group: &'static str) {
        HOOKS.remove(group)
    }

    pub fn trigger<H: Hookable>(args: &mut H::Args<'_>) {
        HOOKS.activate::<H>(args)
    }
}

pub trait Hookable: Sized + 'static {
    type Args<'args>;
}

trait HookHolder: Send + Sync {
    fn remove_group(&mut self, group: &'static str);
}

struct HooksOf<H>(RwLock<Vec<Hook<H>>>)
where
    H: Hookable;

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove_group(&mut self, group: &'static str) {
        self.0.write().extract_if(|(cmp, _)| *cmp == group).last();
    }
}

struct Hooks(LazyLock<RwLock<HashMap<TypeId, Box<dyn HookHolder>>>>);

impl Hooks {
    const fn new() -> Self {
        Hooks(LazyLock::new(|| RwLock::new(HashMap::new())))
    }

    fn add_hook<H: Hookable>(
        &self,
        group: &'static str,
        f: impl for<'a, 'b> FnMut(&'b mut H::Args<'a>) + Send + Sync + 'static,
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

    fn activate<H: Hookable>(&self, args: &mut H::Args<'_>) {
        let map = self.0.read();

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            for (_, f) in &mut *hooks_of.0.write() {
                f(args)
            }
        }
    }
}

pub struct OnFileOpen<U>(PhantomData<U>)
where
    U: Ui;

impl<U> Hookable for OnFileOpen<U>
where
    U: Ui,
{
    type Args<'a> = FileBuilder<'a, U>;
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

type Hook<H> = (
    &'static str,
    Box<dyn for<'a, 'b> FnMut(&'b mut <H as Hookable>::Args<'a>) + Send + Sync>,
);
