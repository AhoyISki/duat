//! Utilities for hooks in Duat
//!
//! In Duat, hooks are handled through the [`Hookable`] trait. This
//! trait contains the [`Hookable::Args`] associated type. By
//! implementing this trait, you allow an end user to hook executions
//! whenever said [`Hookable`] is triggered:
//!
//! ```rust
//! # use duat_core::{
//! #     hooks::{self, *},
//! #     ui::Ui,
//! #     widgets::{File, LineNumbers, PassiveWidget},
//! # };
//! # fn test<U: Ui>() {
//! hooks::add::<OnFileOpen<U>>(|builder| {
//!     builder.push(LineNumbers::cfg());
//! });
//! # }
//! ```
//!
//! The hook above is an example of a specialized use. [`OnFileOpen`]
//! lets you push widgets around a [`File`] whenever one is opened. In
//! the case above, I've pushed the [`LineNumbers`] widget to the
//! [`File`].
//!
//! Currently, these are the existing hooks in `duat-core`:
//!
//! - [`SessionStarted`], happening after the initial setup is done.
//! - [`OnFileOpen`], which lets you push widgets around a [`File`].
//! - [`OnWindowOpen`], which lets you push widgets around the window.
//! - [`FocusedOn`] lets you act on a [widget] whenever it is focused.
//! - [`UnfocusedFrom`] lets you act on a [widget] when unfocused.
//! - [`KeySent`] lets you act on a [dyn ActiveWidget], depending on
//!   the [key] sent to it.
//! - [`KeySentTo`], unlike [`KeySent`], lets you act on a specific
//!   [widget], given a [key].
//!
//! # A note on execution
//!
//! In order to prevent [deadlocks], Duat executes hooks and
//! [commands] asynchronously in the order that they are sent. For
//! hooks, this means that if you [`trigger`] a [`Hookable`], close
//! actions following said triggering will probably happen before the
//! [`trigger`] is over:
//!
//! ```rust
//! # use duat_core::hooks::{self, *};
//! struct CustomHook;
//! impl Hookable for CustomHook {
//!     type Args = usize;
//! }
//!
//! let arg = 42;
//! hooks::trigger::<CustomHook>(arg);
//! println!("This will probably print before the trigger is over.");
//! ```
//!
//! [`File`]: crate::widgets::File
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [widget]: ActiveWidget
//! [dyn ActiveWidget]: ActiveWidget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::commands
use std::{any::TypeId, collections::HashMap, marker::PhantomData, sync::LazyLock};

use parking_lot::{Mutex, RwLock};

pub use self::global::*;
use crate::{
    data::RwData,
    input::{InputMethod, KeyEvent},
    ui::{FileBuilder, Ui, WindowBuilder},
    widgets::ActiveWidget,
};

pub struct SessionStarted<U: Ui>(PhantomData<U>);

impl<U: Ui> Hookable for SessionStarted<U> {
    type Args = ();
}

/// Triggers whenever a [`File`] is opened
///
/// # Arguments
///
/// - The file [builder], which can be used to push widgets to the
///   file, and to eachother.
///
/// [`File`]: crate::widgets::File
/// [builder]: crate::ui::FileBuilder
pub struct OnFileOpen<U: Ui>(PhantomData<U>);

impl<U: Ui> Hookable for OnFileOpen<U> {
    type Args = FileBuilder<U>;
}

/// Triggers whenever a new window is opened
///
/// # Arguments
///
/// - The window [builder], which can be used to push widgets to the
///   edges of the window, surrounding the inner file region.
///
/// [builder]: crate::ui::WindowBuilder
pub struct OnWindowOpen<U: Ui>(PhantomData<U>);

impl<U: Ui> Hookable for OnWindowOpen<U> {
    type Args = WindowBuilder<U>;
}

/// Triggers whenever the given [`widget`] is focused
///
/// # Arguments
///
/// - The widget itself.
///
/// [`widget`]: crate::widgets::ActiveWidget
pub struct FocusedOn<W, U>(PhantomData<(W, U)>)
where
    W: ActiveWidget<U>,
    U: Ui;

impl<W, U> Hookable for FocusedOn<W, U>
where
    W: ActiveWidget<U>,
    U: Ui,
{
    type Args = (RwData<W>, RwData<dyn InputMethod<U>>, U::Area);

    fn post_hook(args: &Self::Args) {
        let (widget, input, area) = args;
        let input = input.read();
        let mut widget = widget.write();

        if let Some(cursors) = input.cursors() {
            widget.text_mut().add_cursor_tags(cursors);
        }

        widget.update(area);
        widget.print(area);
    }
}

/// Triggers whenever the given [`widget`] is focused
///
/// # Arguments
///
/// - The widget itself.
///
/// [`widget`]: crate::widgets::ActiveWidget
pub struct UnfocusedFrom<W, U>(PhantomData<(W, U)>)
where
    W: ActiveWidget<U>,
    U: Ui;

impl<W, U> Hookable for UnfocusedFrom<W, U>
where
    W: ActiveWidget<U>,
    U: Ui,
{
    type Args = (RwData<W>, RwData<dyn InputMethod<U>>, U::Area);

    fn post_hook(args: &Self::Args) {
        let (widget, input, area) = args;
        let input = input.read();
        let mut widget = widget.write();

        if let Some(cursors) = input.cursors() {
            widget.text_mut().add_cursor_tags(cursors);
        }

        widget.update(area);
        widget.print(area);
    }
}

/// Triggers whenever a [key] is sent
///
/// # Arguments
///
/// - The [key] sent.
/// - An [`RwData<dyn ActiveWidget<U>>`] for the widget active when
///   the key was sent.
///
/// [key]: KeyEvent
pub struct KeySent<U: Ui>(PhantomData<U>);

impl<U: Ui> Hookable for KeySent<U> {
    type Args = (KeyEvent, RwData<dyn ActiveWidget<U>>);
}

/// Triggers whenever a [key] is sent to `W`
///
/// # Arguments
///
/// - The [key] sent.
/// - An [`RwData<W>`] for the widget active when the key was sent.
///
/// [key]: KeyEvent
pub struct KeySentTo<W, U>(PhantomData<(&'static W, U)>)
where
    W: ActiveWidget<U> + ?Sized,
    U: Ui;

impl<W, U> Hookable for KeySentTo<W, U>
where
    W: ActiveWidget<U> + ?Sized,
    U: Ui,
{
    type Args = (KeyEvent, RwData<W>);
}

/// Hook functions
mod global {
    use super::{Hookable, Hooks};

    static HOOKS: Hooks = Hooks::new();

    /// Adds a [hook]
    ///
    /// This hook is ungrouped, that is, it cannot be removed. If you
    /// want a hook that is removable, see [`hooks::add_grouped`].
    ///
    /// [hook]: Hookable
    /// [`hooks::add_grouped`]: add_grouped
    pub fn add<H: Hookable>(f: impl FnMut(&H::Args) + Send + 'static) {
        crate::thread::queue(move || HOOKS.add::<H>("", f))
    }

    /// Adds a grouped [hook]
    ///
    /// A grouped hook is one that, along with others on the same
    /// group, can be removed by [`hooks::remove_group`]. If you do
    /// not need/want this feature, take a look at [`hooks::add`]
    ///
    /// [hook]: Hookable
    /// [`hooks::remove_group`]: remove_group
    /// [`hooks::add`]: add
    pub fn add_grouped<H: Hookable>(group: &'static str, f: impl FnMut(&H::Args) + Send + 'static) {
        crate::thread::queue(move || HOOKS.add::<H>(group, f))
    }

    /// Removes a [hook] group
    ///
    /// By removing the group, this function will remove all hooks
    /// added via [`hooks::add_grouped`] with the same group.
    ///
    /// [hook]: Hookable
    /// [`hooks::add_grouped`]: add_grouped
    pub fn remove_group(group: &'static str) {
        crate::thread::queue(move || HOOKS.remove(group));
    }

    /// Triggers a hooks for a [`Hookable`] struct
    ///
    /// When you trigger a hook, all hooks added via [`hooks::add`] or
    /// [`hooks::add_grouped`] for said [`Hookable`] struct will
    /// be called.
    ///
    /// [hook]: Hookable
    /// [`hooks::add`]: add
    /// [`hooks::add_grouped`]: add_grouped
    pub fn trigger<H: Hookable>(args: H::Args) {
        crate::thread::queue(move || HOOKS.trigger::<H>(args));
    }

    pub(crate) fn trigger_now<H: Hookable>(args: H::Args) {
        HOOKS.trigger::<H>(args)
    }

    /// Checks if a give group exists
    ///
    /// Returns `true` if said group was added via
    /// [`hooks::add_grouped`], and no [`hooks::remove_group`]
    /// followed these additions
    ///
    /// [`hooks::add_grouped`]: add_grouped
    /// [`hooks::remove_group`]: remove_group
    pub fn group_exists(group: &'static str) -> bool {
        HOOKS.group_exists(group)
    }
}

/// A hookable struct, for hooks taking [`Hookable::Args`]
///
/// Through this trait, Duat allows for custom hookable structs. With
/// these structs, plugin creators can create their own custom hooks,
/// and trigger them via [`hooks::trigger`].
///
/// This further empowers an end user to customize the behaviour of
/// Duat in the configuration crate.
///
/// [`hooks::trigger`]: trigger
pub trait Hookable: Sized + 'static {
    type Args: Send + 'static;

    /// A function to be trigggered after all hooks are done.
    #[allow(unused)]
    fn post_hook(args: &Self::Args) {}
}

/// An intermediary trait, meant for group removal
trait HookHolder: Send + Sync {
    /// Remove the given group from hooks of this holder
    fn remove_group(&mut self, group: &str);
}

/// An intermediary struct, meant to hold the hooks of a [`Hookable`]
struct HooksOf<H>(Mutex<Vec<Hook<H>>>)
where
    H: Hookable;

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove_group(&mut self, group: &str) {
        let mut hooks = None;
        while hooks.is_none() {
            hooks = self.0.try_lock();
        }
        hooks.unwrap().extract_if(|(g, _)| *g == group).last();
    }
}

/// Where all hooks of Duat are stored
struct Hooks {
    types: LazyLock<RwLock<HashMap<TypeId, Box<dyn HookHolder>>>>,
    groups: LazyLock<RwLock<Vec<&'static str>>>,
}

impl Hooks {
    /// Returns a new instance of [`Hooks`]
    const fn new() -> Self {
        Hooks {
            types: LazyLock::new(RwLock::default),
            groups: LazyLock::new(RwLock::default),
        }
    }

    /// Adds a hook for a [`Hookable`]
    fn add<H: Hookable>(
        &self,
        group: &'static str,
        f: impl for<'a> FnMut(&'a H::Args) + Send + 'static,
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

            hooks_of.0.lock().push((group, Box::new(f)))
        } else {
            let hooks_of = HooksOf::<H>(Mutex::new(vec![(group, Box::new(f))]));

            map.insert(TypeId::of::<H>(), Box::new(hooks_of));
        }
    }

    /// Removes hooks with said group
    fn remove(&'static self, group: &'static str) {
        self.groups.write().extract_if(|g| *g == group).next();
        let mut map = self.types.write();
        for holder in map.iter_mut() {
            holder.1.remove_group(group)
        }
    }

    /// Triggers hooks with args of the [`Hookable`]
    fn trigger<H: Hookable>(&self, args: H::Args) {
        let map = self.types.read();

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            for (_, f) in &mut *hooks_of.0.lock() {
                f(&args)
            }
        }

        H::post_hook(&args);
    }

    /// Checks if a hook group exists
    fn group_exists(&self, group: &str) -> bool {
        self.groups.read().contains(&group)
    }
}

type Hook<H> = (
    &'static str,
    Box<dyn for<'a> FnMut(&'a <H as Hookable>::Args) + Send + 'static>,
);
