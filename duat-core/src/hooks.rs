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
//! - [`ModeSwitched`] triggers when you change [`Mode`].
//! - [`ModeSetTo`] lets you act on a [`Mode`] after switching.
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
//!     type Args<'a> = usize;
//!     type PreArgs = usize;
//!
//!     fn trigger_hooks<'b>(
//!         pre_args: Self::PreArgs,
//!         hooks: impl Iterator<Item = &'b mut HookFn<Self>>,
//!     ) {
//!         for hook in hooks {
//!             hook(pre_args)
//!         }
//!     }
//! }
//!
//! let arg = 42;
//! hooks::trigger::<CustomHook>(arg);
//! println!("This will probably print before the trigger is over.");
//! ```
//!
//! The above example ilustrates how hooks are implemented in Duat,
//! that is, you are responsible for actually calling the functions.
//! This is roughly how hooks should be defined:
//!
//! ```rust
//! # fn args_from_pre_args(usize: &usize) -> usize { *usize }
//! # use duat_core::hooks::{self, *};
//! struct NewHook;
//! impl Hookable for NewHook {
//!     // Some type derived from Self::PreArgs
//!     type Args<'a> = usize;
//!     // Some Send + 'static type
//!     type PreArgs = usize;
//!
//!     fn trigger_hooks<'b>(
//!         pre_args: Self::PreArgs,
//!         hooks: impl Iterator<Item = &'b mut HookFn<Self>>,
//!     ) {
//!         // Preprocessing before creating Self::Args
//!         // ...
//!         let args = args_from_pre_args(&pre_args);
//!         // Preprocessing before triggering hook functions
//!         // ...
//!         for hook in hooks {
//!             hook(args)
//!         }
//!         // Postprocessing
//!         // ...
//!     }
//! }
//! ```
//!
//! One example of where this ends up being very useful is in
//! [`Widget`] related hooks, since it allows Duat to pass just [`&mut
//! Widget`] instead of [`RwData<Widget>`] as a parameter, and it also
//! lets Duat immediately print the widget as soon as the hooks are
//! done altering it.
//!
//! [`File`]: crate::widgets::File
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [widget]: Widget
//! [dyn Widget]: Widget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::cmd
//! [`Mode`]: crate::mode::Mode
//! [`&mut Widget`]: Widget
use std::{any::TypeId, collections::HashMap, marker::PhantomData, sync::LazyLock};

use parking_lot::{Mutex, RwLock};

pub use self::global::*;
use crate::{
    data::RwData,
    form::{Form, FormId},
    mode::{Cursors, KeyEvent, Mode},
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
    type Args<'a> = ();
    type PreArgs = ();

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args)
        }
    }
}

/// [`Hookable`]: Triggers when Duat closes or has to reload
///
/// There are no arguments
pub struct ConfigUnloaded;

impl Hookable for ConfigUnloaded {
    type Args<'a> = ();
    type PreArgs = ();

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args)
        }
    }
}

/// [`Hookable`]: Triggers when Duat closes
///
/// There are no arguments
pub struct ExitedDuat;

impl Hookable for ExitedDuat {
    type Args<'a> = ();
    type PreArgs = ();

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args)
        }
    }
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
    type Args<'a> = &'a mut FileBuilder<U>;
    type PreArgs = FileBuilder<U>;

    fn trigger_hooks<'b>(_: Self::PreArgs, _: impl Iterator<Item = Hook<'b, Self>>) {
        unreachable!("This hook is not meant to be triggered asynchronously")
    }

    fn trigger_hooks_now<'b>(
        mut pre_args: Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) {
        for hook in hooks {
            hook(&mut pre_args)
        }
    }
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
    type Args<'a> = &'a mut WindowBuilder<U>;
    type PreArgs = WindowBuilder<U>;

    fn trigger_hooks<'b>(_: Self::PreArgs, _: impl Iterator<Item = Hook<'b, Self>>) {
        unreachable!("This hook is not meant to be triggered asynchronously")
    }

    fn trigger_hooks_now<'b>(
        mut pre_args: Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) {
        for hook in hooks {
            hook(&mut pre_args)
        }
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is focused
///
/// # Arguments
///
/// - The widget itself.
/// - Its [area].
///
/// [`Widget`]: crate::widgets::Widget
/// [area]: crate::ui::Area
pub struct FocusedOn<W: Widget<U>, U: Ui>(PhantomData<(W, U)>);

impl<W: Widget<U>, U: Ui> Hookable for FocusedOn<W, U> {
    type Args<'a> = (&'a mut W, &'a U::Area);
    type PreArgs = (RwData<W>, U::Area);

    fn trigger_hooks<'b>(
        (widget, area): Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) {
        let mut widget = widget.write();
        let cfg = widget.print_cfg();
        widget.text_mut().remove_cursors(&area, cfg);
        widget.on_focus(&area);

        for hook in hooks {
            hook((&mut *widget, &area));
        }

        widget.text_mut().add_cursors(&area, cfg);
        if let Some(main) = widget.cursors().and_then(Cursors::get_main) {
            area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
        }

        widget.update(&area);
        widget.print(&area);
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is unfocused
///
/// # Arguments
///
/// - The widget itself.
/// - Its [area].
///
/// [`Widget`]: crate::widgets::Widget
/// [area]: crate::ui::Area
pub struct UnfocusedFrom<W: Widget<U>, U: Ui>(PhantomData<(W, U)>);

impl<W: Widget<U>, U: Ui> Hookable for UnfocusedFrom<W, U> {
    type Args<'a> = (&'a mut W, &'a U::Area);
    type PreArgs = (RwData<W>, U::Area);

    fn trigger_hooks<'b>(
        (widget, area): Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) {
        let mut widget = widget.write();
        let cfg = widget.print_cfg();
        widget.text_mut().remove_cursors(&area, cfg);
        widget.on_unfocus(&area);

        for hook in hooks {
            hook((&mut *widget, &area));
        }

        widget.text_mut().add_cursors(&area, cfg);
        if let Some(main) = widget.cursors().and_then(Cursors::get_main) {
            area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
        }

        widget.update(&area);
        widget.print(&area);
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
    type Args<'a> = (&'a str, &'a str);
    type PreArgs = (&'static str, &'static str);

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args)
        }
    }
}

/// [`Hookable`]: Lets you modify a [`Mode`] as it is set
///
/// # Arguments
///
/// - The new mode.
/// - Its widget.
///
/// This hook is very useful if you want to, for example, set
/// different options upon switching to modes, depending on things
/// like the language of a [`File`].
///
/// [`File`]: crate::widgets::File
pub struct ModeSetTo<M: Mode<U>, U: Ui>(PhantomData<(M, U)>);

impl<M: Mode<U>, U: Ui> Hookable for ModeSetTo<M, U> {
    type Args<'a> = (M, &'a M::Widget, &'a U::Area);
    type PreArgs = (M, RwData<M::Widget>, U::Area);
    type Return = M;

    fn trigger_hooks<'b>(_: Self::PreArgs, _: impl Iterator<Item = Hook<'b, Self>>) {
        unreachable!("This hook is not meant to be triggered asynchronously")
    }

    fn trigger_hooks_now<'b>(
        (mut mode, widget, area): Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) -> Self::Return {
        let widget = widget.read();

        for hook in hooks {
            mode = hook((mode, &widget, &area));
        }

        mode
    }
}

/// [`Hookable`]: Triggers whenever a [key] is sent
///
/// # Arguments
///
/// - The [key] sent.
///
/// [key]: KeyEvent
pub struct KeySent;

impl Hookable for KeySent {
    type Args<'a> = KeyEvent;
    type PreArgs = KeyEvent;

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args);
        }
    }
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
    type Args<'b> = (KeyEvent, &'b mut W, &'b U::Area);
    type PreArgs = (KeyEvent, RwData<W>, U::Area);

    fn trigger_hooks<'b>(_: Self::PreArgs, _: impl Iterator<Item = Hook<'b, Self>>) {
        unreachable!("This hook is not meant to be triggered asynchronously")
    }

    fn trigger_hooks_now<'b>(
        (key, widget, area): Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) {
        let mut widget = widget.write();
        for hook in hooks {
            hook((key, &mut *widget, &area));
        }
    }
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
    type Args<'b> = Self::PreArgs;
    type PreArgs = (&'static str, FormId, Form);

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args)
        }
    }
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
    type Args<'b> = Self::PreArgs;
    type PreArgs = &'static str;

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>) {
        for hook in hooks {
            hook(pre_args)
        }
    }
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
    pub fn add<H: Hookable>(f: impl for<'a> FnMut(H::Args<'a>) -> H::Return + Send + 'static) {
        crate::thread::spawn(move || HOOKS.add::<H>("", f));
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
    pub fn add_grouped<H: Hookable>(
        group: &'static str,
        f: impl for<'a> FnMut(H::Args<'a>) -> H::Return + Send + 'static,
    ) {
        crate::thread::spawn(move || HOOKS.add::<H>(group, f));
    }

    /// Removes a [hook] group
    ///
    /// By removing the group, this function will remove all hooks
    /// added via [`hooks::add_grouped`] with the same group.
    ///
    /// [hook]: Hookable
    /// [`hooks::add_grouped`]: add_grouped
    pub fn remove(group: &'static str) {
        crate::thread::spawn(move || HOOKS.remove(group));
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
    pub fn trigger<H: Hookable>(args: H::PreArgs) {
        crate::thread::spawn(move || {
            HOOKS.trigger::<H>(args);
        });
    }

    pub(crate) fn trigger_now<H: Hookable>(args: H::PreArgs) -> H::Return {
        HOOKS.trigger_now::<H>(args)
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
    type PreArgs: Send;
    type Args<'a>;
    /// This type is useful if, for example, you want to pass a value
    /// by moving it to Args, while returning and reusing said value.
    type Return = ();

    fn trigger_hooks<'b>(pre_args: Self::PreArgs, hooks: impl Iterator<Item = Hook<'b, Self>>);

    /// This function is only for internal use, it will not be used if
    /// you implement it.
    #[allow(unused_variables)]
    #[doc(hidden)]
    fn trigger_hooks_now<'b>(
        pre_args: Self::PreArgs,
        hooks: impl Iterator<Item = Hook<'b, Self>>,
    ) -> Self::Return {
        unreachable!("This Hookable is not meant to be called immediately");
    }
}

/// An intermediary trait, meant for group removal
trait HookHolder: Send + Sync {
    /// Remove the given group from hooks of this holder
    fn remove(&mut self, group: &str);
}

/// An intermediary struct, meant to hold the hooks of a [`Hookable`]
struct HooksOf<H: Hookable>(Mutex<Vec<(&'static str, InnerHookFn<H>)>>);

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove(&mut self, group: &str) {
        let mut hooks = None;
        while hooks.is_none() {
            hooks = self.0.try_lock();
        }
        hooks.unwrap().retain(|(g, _)| *g != group);
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
        f: impl for<'a> FnMut(H::Args<'a>) -> H::Return + Send + 'static,
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
        self.groups.write().retain(|g| *g != group);
        let mut map = self.types.write();
        for holder in map.iter_mut() {
            holder.1.remove(group)
        }
    }

    /// Triggers hooks with args of the [`Hookable`]
    fn trigger<H: Hookable>(&self, pre_args: H::PreArgs) {
        let map = self.types.read();

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            let mut hooks = hooks_of.0.lock();
            H::trigger_hooks(pre_args, hooks.iter_mut().map(|(_, f)| Hook(f)));
        } else {
            H::trigger_hooks(pre_args, std::iter::empty());
        }
    }

    /// Triggers hooks, returning [`H::Return`]
    ///
    /// [`H::Return`]: Hookable::Return
    fn trigger_now<H: Hookable>(&self, pre_args: H::PreArgs) -> H::Return {
        let map = self.types.read();

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            let mut hooks = hooks_of.0.lock();
            H::trigger_hooks_now(pre_args, hooks.iter_mut().map(|(_, f)| Hook(f)))
        } else {
            H::trigger_hooks_now(pre_args, std::iter::empty())
        }
    }

    /// Checks if a hook group exists
    fn group_exists(&self, group: &str) -> bool {
        self.groups.read().contains(&group)
    }
}

/// A function to be called when a [`Hookable`] is triggered
///
/// This function implements [`FnOnce`], as it is only meant to be
/// called once per [`trigger`] call.
struct Hook<'b, H: Hookable>(&'b mut InnerHookFn<H>);

impl<'b, H: Hookable> std::ops::FnOnce<(H::Args<'_>,)> for Hook<'b, H> {
    type Output = H::Return;

    extern "rust-call" fn call_once(self, (args,): (H::Args<'_>,)) -> Self::Output {
        (self.0)(args)
    }
}

pub type InnerHookFn<H> =
    Box<dyn for<'a> FnMut(<H as Hookable>::Args<'a>) -> <H as Hookable>::Return + Send + 'static>;
