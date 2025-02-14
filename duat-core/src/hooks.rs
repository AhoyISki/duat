//! Utilities for hooks in Duat
//!
//! In Duat, hooks are handled through the [`Hookable`] trait. This
//! trait contains the [`Hookable::Args`] associated type. By
//! implementing this trait, you allow an end user to hook executions
//! whenever said [`Hookable`] is triggered:
//!
//! ```rust
//! # use duat_core::{hooks::{self, *}, ui::Ui, widgets::{File, LineNumbers, Widget}};
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
//! - [`ConfigLoaded`] triggers after loading the config crate.
//! - [`ConfigUnloaded`] triggers after unloading the config crate.
//! - [`ExitedDuat`] triggers after Duat has exited.
//! - [`OnFileOpen`], which lets you push widgets around a [`File`].
//! - [`OnWindowOpen`], which lets you push widgets around the window.
//! - [`FocusedOn`] lets you act on a [widget] when focused.
//! - [`UnfocusedFrom`] lets you act on a [widget] when unfocused.
//! - [`KeySent`] lets you act on a [dyn Widget], given a[key].
//! - [`KeySentTo`] lets you act on a given [widget], given a [key].
//! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
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
//! [widget]: Widget
//! [dyn Widget]: Widget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::cmd
use std::{any::TypeId, collections::HashMap, marker::PhantomData, sync::LazyLock};

use parking_lot::{Mutex, RwLock};

pub use self::global::*;
use crate::{
    data::RwData,
    form::{Form, FormId},
    mode::{Cursors, KeyEvent},
    ui::{Area, FileBuilder, Ui, WindowBuilder},
    widgets::Widget,
};

/// [`Hookable`]: Triggers when Duat opens or reloads
///
/// This trigger will also happen after a few other initial setups of
/// Duat.
///
/// There are no arguments
pub struct ConfigLoaded;

impl Hookable for ConfigLoaded {
    type Args = ();
}

/// [`Hookable`]: Triggers when Duat closes or has to reload
///
/// There are no arguments
pub struct ConfigUnloaded;

impl Hookable for ConfigUnloaded {
    type Args = ();
}

/// [`Hookable`]: Triggers when Duat closes
///
/// There are no arguments
pub struct ExitedDuat;

impl Hookable for ExitedDuat {
    type Args = ();
}

/// [`Hookable`]: Triggers when a [`File`] is opened
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

/// [`Hookable`]: Triggers when a new window is opened
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

/// [`Hookable`]: Triggers when the [`Widget`] is focused
///
/// # Arguments
///
/// - The widget itself.
/// - Its [area].
/// - Its [`Cursors`]
///
/// [`Widget`]: crate::widgets::Widget
/// [area]: crate::ui::Area
pub struct FocusedOn<W: Widget<U>, U: Ui>(PhantomData<(W, U)>);

impl<W: Widget<U>, U: Ui> Hookable for FocusedOn<W, U> {
    type Args = (RwData<W>, U::Area, RwData<Cursors>);

    fn post_hook(args: &Self::Args) {
        let (widget, area, cursors) = args;
        let cursors = cursors.read();
        let mut widget = widget.write();
        let cfg = widget.print_cfg();

        widget.text_mut().add_cursors(&cursors, area, cfg);
        if let Some(main) = cursors.get_main() {
            area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
        }

        widget.update(area);
        widget.print(area);
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is unfocused
///
/// # Arguments
///
/// - The widget itself.
/// - Its [area].
/// - Its [`Cursors`]
///
/// [`Widget`]: crate::widgets::Widget
/// [area]: crate::ui::Area
pub struct UnfocusedFrom<W: Widget<U>, U: Ui>(PhantomData<(W, U)>);

impl<W: Widget<U>, U: Ui> Hookable for UnfocusedFrom<W, U> {
    type Args = (RwData<W>, U::Area, RwData<Cursors>);

    fn post_hook(args: &Self::Args) {
        let (widget, area, cursors) = args;
        let cursors = cursors.read();
        let mut widget = widget.write();
        let cfg = widget.print_cfg();

        widget.text_mut().add_cursors(&cursors, area, cfg);
        if let Some(main) = cursors.get_main() {
            area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
        }

        widget.update(area);
        widget.print(area);
    }
}

/// [`Hookable`]: Triggers when the [`Mode`] is changed
///
/// # Arguments
///
/// - The previous mode.
/// - The current mode.
///
/// # Note
///
/// You should try to avoid more than one [`Mode`] with the same name.
/// This can happen if you're using two structs with the same name,
/// but from different crates.
///
/// [`Mode`]: crate::mode::Mode
pub struct ModeSwitched;

impl Hookable for ModeSwitched {
    type Args = (&'static str, &'static str);
}

/// [`Hookable`]: Triggers whenever a [key] is sent
///
/// # Arguments
///
/// - The [key] sent.
/// - An [`RwData<dyn Widget<U>>`] for the widget.
///
/// [key]: KeyEvent
pub struct KeySent<U: Ui>(PhantomData<U>);

impl<U: Ui> Hookable for KeySent<U> {
    type Args = (KeyEvent, RwData<dyn Widget<U>>);
}

/// [`Hookable`]: Triggers whenever a [key] is sent to the [`Widget`]
///
/// # Arguments
///
/// - The [key] sent.
/// - An [`RwData<W>`] for the widget.
///
/// [key]: KeyEvent
pub struct KeySentTo<W: Widget<U>, U: Ui>(PhantomData<(&'static W, U)>);

impl<W: Widget<U>, U: Ui> Hookable for KeySentTo<W, U> {
    type Args = (KeyEvent, RwData<W>);
}

/// [`Hookable`]: Triggers whenever a [`Form`] is set
///
/// This can be a creation or alteration of a [`Form`].
/// If the [`Form`] is a reference to another, the reference's
/// [`Form`] will be returned instead.
///
/// # Arguments
///
/// - The [`Form`]'s name.
/// - Its [`FormId`].
/// - Its new value.
pub struct FormSet;

impl Hookable for FormSet {
    type Args = (&'static str, FormId, Form);
}

/// [`Hookable`]: Triggers whenever a [`ColorScheme`] is set
///
/// Since [`Form`]s are set asynchronously, this may happen before the
/// [`ColorScheme`] is done with its changes.
///
/// # Arguments
///
/// - The name of the [`ColorScheme`]
///
/// [`ColorScheme`]: crate::form::ColorScheme
pub struct ColorSchemeSet;

impl Hookable for ColorSchemeSet {
    type Args = &'static str;
}

/// Hook functions
mod global {
    use super::{Hookable, Hooks};

    static HOOKS: Hooks = Hooks::new();

    pub fn address() {
        //crate::log_file!("{:p}", &HOOKS);
    }

    /// Adds a [hook]
    ///
    /// This hook is ungrouped, that is, it cannot be removed. If you
    /// want a hook that is removable, see [`hooks::add_grouped`].
    ///
    /// [hook]: Hookable
    /// [`hooks::add_grouped`]: add_grouped
    pub fn add<H: Hookable>(f: impl FnMut(&H::Args) + Send + 'static) {
        //crate::log_file!("{:p}", &HOOKS);
        crate::thread::queue(move || HOOKS.add::<H>("", f))
    }

    /// Adds a grouped [hook]
    ///
    /// A grouped hook is one that, along with others on the same
    /// group, can be removed by [`hooks::remove`]. If you do
    /// not need/want this feature, take a look at [`hooks::add`]
    ///
    /// [hook]: Hookable
    /// [`hooks::remove`]: remove
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
    pub fn remove(group: &'static str) {
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
    /// [`hooks::add_grouped`], and no [`hooks::remove`]
    /// followed these additions
    ///
    /// [`hooks::add_grouped`]: add_grouped
    /// [`hooks::remove`]: remove
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
    fn remove(&mut self, group: &str);
}

/// An intermediary struct, meant to hold the hooks of a [`Hookable`]
struct HooksOf<H: Hookable>(Mutex<Vec<Hook<H>>>);

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove(&mut self, group: &str) {
        let mut hooks = None;
        while hooks.is_none() {
            hooks = self.0.try_lock();
        }
        hooks.unwrap().extract_if(.., |(g, _)| *g == group).last();
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
        self.groups.write().extract_if(.., |g| *g == group).next();
        let mut map = self.types.write();
        for holder in map.iter_mut() {
            holder.1.remove(group)
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
