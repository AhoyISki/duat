//! Utilities for hooks in Duat
//!
//! In Duat, hooks are handled through the [`Hookable`] trait. This
//! trait contains the [`Hookable::Args`] associated type. By
//! implementing this trait, you allow an end user to hook executions
//! whenever said [`Hookable`] is triggered:
//!
//! ```rust
//! # use duat_core::{hook::{self, *}, ui::Ui, widgets::{File, LineNumbers, Widget}};
//! # fn test<U: Ui>() {
//! hook::add::<OnFileOpen<U>>(|builder| {
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
//! - [`KeysSent`] lets you act on a [dyn Widget], given a[key].
//! - [`KeysSentTo`] lets you act on a given [widget], given a [key].
//! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
//! - [`ModeSwitched`] triggers when you change [`Mode`].
//! - [`ModeSetTo`] lets you act on a [`Mode`] after switching.
//! - [`SearchPerformed`] triggers after a search is performed.
//! - [`SearchUpdated`] triggers after a search updates.
//! - [`FileWritten`] triggers after the [`File`] is written.
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
//! # use duat_core::hook::{self, *};
//! struct CustomHook;
//! impl Hookable for CustomHook {
//!     type Args<'a> = usize;
//!     type PreArgs = usize;
//!
//!     fn trigger<'b>(
//!         pre_args: Self::PreArgs,
//!         hooks: impl Iterator<Item = Hook<'b, Self>>,
//!     ) {
//!         for hook in hooks {
//!             hook(pre_args)
//!         }
//!     }
//! }
//!
//! let arg = 42;
//! hook::trigger::<CustomHook>(arg);
//! println!("This will probably print before the trigger is over.");
//! ```
//!
//! The above example ilustrates how hooks are implemented in Duat,
//! that is, you are responsible for actually calling the functions.
//! This is roughly how hooks should be defined:
//!
//! ```rust
//! # fn args_from_pre_args(usize: &usize) -> usize { *usize }
//! # use duat_core::hook::{self, *};
//! struct NewHook;
//! impl Hookable for NewHook {
//!     // Some type derived from Self::PreArgs
//!     type Args<'a> = usize;
//!     // Some Send + 'static type
//!     type PreArgs = usize;
//!
//!     fn trigger<'b>(
//!         pre_args: Self::PreArgs,
//!         hooks: impl Iterator<Item = Hook<'b, Self>>,
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
//! [`File`]: crate::file::File
//! [`LineNumbers`]: crate::widgets::LineNumbers
//! [widget]: Widget
//! [dyn Widget]: Widget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::cmd
//! [`Mode`]: crate::mode::Mode
//! [`&mut Widget`]: Widget
use std::{any::TypeId, cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};

pub use self::global::*;
use crate::{
    data::{Pass, RwData},
    form::{Form, FormId},
    mode::{Cursors, KeyEvent, Mode},
    ui::{FileBuilder, RawArea, Ui, WindowBuilder},
    widget::Widget,
};

/// Hook functions
mod global {
    use std::sync::LazyLock;

    use super::{Hookable, InnerHooks};
    use crate::{context, data::Pass, main_thread_only::MainThreadOnly, ui::DuatEvent};

    static HOOKS: LazyLock<MainThreadOnly<InnerHooks>> = LazyLock::new(MainThreadOnly::default);

    /// Adds a [hook]
    ///
    /// This hook is ungrouped, that is, it cannot be removed. If you
    /// want a hook that is removable, see [`hook::add_grouped`].
    ///
    /// [hook]: Hookable
    /// [`hook::add_grouped`]: add_grouped
    #[inline(never)]
    pub fn add<H: Hookable>(f: impl FnMut(Pass, H::Args<'_>) -> H::Output + 'static) {
        context::assert_is_on_main_thread();
        unsafe { HOOKS.get() }.add::<H>("", Box::new(f));
    }

    /// Adds a grouped [hook]
    ///
    /// A grouped hook is one that, along with others on the same
    /// group, can be removed by [`hook::remove`]. If you do
    /// not need/want this feature, take a look at [`hook::add`]
    ///
    /// [hook]: Hookable
    /// [`hook::remove`]: remove
    /// [`hook::add`]: add
    #[inline(never)]
    pub fn add_grouped<H: Hookable>(
        group: &'static str,
        f: impl FnMut(Pass, H::Args<'_>) -> H::Output + 'static,
    ) {
        context::assert_is_on_main_thread();
        unsafe { HOOKS.get() }.add::<H>(group, Box::new(f));
    }

    /// Removes a [hook] group
    ///
    /// By removing the group, this function will remove all hooks
    /// added via [`hook::add_grouped`] with the same group.
    ///
    /// [hook]: Hookable
    /// [`hook::add_grouped`]: add_grouped
    pub fn remove(group: &'static str) {
        context::assert_is_on_main_thread();
        unsafe { HOOKS.get() }.remove(group);
    }

    /// Triggers a hooks for a [`Hookable`] struct
    ///
    /// When you trigger a hook, all hooks added via [`hook::add`] or
    /// [`hook::add_grouped`] for said [`Hookable`] struct will
    /// be called.
    ///
    /// [hook]: Hookable
    /// [`hook::add`]: add
    /// [`hook::add_grouped`]: add_grouped
    #[inline(never)]
    pub fn trigger<H: Hookable>(pa: &mut Pass, args: H::Input) -> H::Output {
        context::assert_is_on_main_thread();
        unsafe { HOOKS.get().trigger::<H>(pa, args) }
    }

    /// Queues a [`Hookable`]'s execution
    ///
    /// You should use this if you are not in an async environment or
    /// on the main thread, and are thus unable to call [`trigger`].
    /// The notable difference between this function and [`trigger`]
    /// is that it doesn't return [`H::Output`], since the triggering
    /// of this hook will happen outside of the calling function.
    ///
    /// Most of the time, this doesn't really matter, as only a few
    /// types really need to recover [`H::Output`], so you should be
    /// able to call this from pretty much anywhere.
    ///
    /// [`H::Output`]: Hookable::Output
    pub fn queue<H>(args: H::Input)
    where
        H: Hookable,
        H::Input: Send + 'static,
    {
        let sender = crate::context::sender();
        sender
            .send(DuatEvent::QueuedFunction(Box::new(|mut pa| {
                // SAFETY: There is a Pass argument
                unsafe { HOOKS.get() }.trigger::<H>(&mut pa, args);
            })))
            .unwrap();
    }

    /// Checks if a give group exists
    ///
    /// Returns `true` if said group was added via
    /// [`hook::add_grouped`], and no [`hook::remove`]
    /// followed these additions
    ///
    /// [`hook::add_grouped`]: add_grouped
    /// [`hook::remove`]: remove
    pub fn group_exists(group: &'static str) -> bool {
        context::assert_is_on_main_thread();
        unsafe { HOOKS.get() }.group_exists(group)
    }
}

/// [`Hookable`]: Triggers when Duat opens or reloads
///
/// This trigger will also happen after a few other initial setups of
/// Duat.
///
/// There are no arguments
pub struct ConfigLoaded;

impl Hookable for ConfigLoaded {
    type Args<'a> = ();
    type Input = ();

    fn trigger(mut pa: Pass<'_>, input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, input)
        }
    }
}

/// [`Hookable`]: Triggers when Duat closes or has to reload
///
/// There are no arguments
pub struct ConfigUnloaded;

impl Hookable for ConfigUnloaded {
    type Args<'a> = ();
    type Input = ();

    fn trigger(mut pa: Pass<'_>, input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, input)
        }
    }
}

/// [`Hookable`]: Triggers when Duat closes
///
/// There are no arguments
pub struct ExitedDuat;

impl Hookable for ExitedDuat {
    type Args<'a> = ();
    type Input = ();

    fn trigger(mut pa: Pass<'_>, input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, input)
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
/// [`File`]: crate::file::File
/// [builder]: crate::ui::FileBuilder
pub struct OnFileOpen<U: Ui>(PhantomData<U>);

impl<U: Ui> Hookable for OnFileOpen<U> {
    type Args<'a> = &'a mut FileBuilder<U>;
    type Input = FileBuilder<U>;

    fn trigger(mut pa: Pass<'_>, mut input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, &mut input)
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
    type Input = WindowBuilder<U>;

    fn trigger(mut pa: Pass<'_>, mut input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, &mut input)
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
    type Args<'a> = (&'a RwData<W>, &'a U::Area);
    type Input = (RwData<W>, U::Area);

    fn trigger(mut pa: Pass<'_>, (widget, area): Self::Input, hooks: Hooks<Self>) {
        widget.write(&mut pa, |widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(&area, cfg);
        });

        for hook in hooks {
            hook(&mut pa, (&widget, &area));
        }

        Widget::on_focus(pa, widget.clone(), &area);

        // SAFETY: Since the last Pass was consumed, we can create a new
        // one.
        widget.write(&mut unsafe { Pass::new() }, |widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().add_cursors(&area, cfg);
            if let Some(main) = widget.text().cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
            }
        });
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
    type Args<'a> = (&'a RwData<W>, &'a U::Area);
    type Input = (RwData<W>, U::Area);

    fn trigger(mut pa: Pass<'_>, (widget, area): Self::Input, hooks: Hooks<Self>) {
        widget.write(&mut pa, |widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(&area, cfg);
        });

        for hook in hooks {
            hook(&mut pa, (&widget, &area));
        }

        Widget::on_unfocus(pa, widget.clone(), &area);

        // SAFETY: Since the last Pass was consumed, we can create a new
        // one.
        widget.write(&mut unsafe { Pass::new() }, |widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().add_cursors(&area, cfg);
            if let Some(main) = widget.text().cursors().and_then(Cursors::get_main) {
                area.scroll_around_point(widget.text(), main.caret(), widget.print_cfg());
            }
        });
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
    type Input = (&'static str, &'static str);

    fn trigger(mut pa: Pass<'_>, input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, input)
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
/// [`File`]: crate::file::File
pub struct ModeSetTo<M: Mode<U>, U: Ui>(PhantomData<(M, U)>);

impl<M: Mode<U>, U: Ui> Hookable for ModeSetTo<M, U> {
    type Args<'a> = (M, &'a RwData<M::Widget>, &'a U::Area);
    type Input = (M, RwData<M::Widget>, U::Area);
    type Output = M;

    fn trigger(
        mut pa: Pass<'_>,
        (mut mode, widget, area): Self::Input,
        hooks: Hooks<Self>,
    ) -> Self::Output {
        for hook in hooks {
            mode = hook(&mut pa, (mode, &widget, &area));
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
pub struct KeysSent;

impl Hookable for KeysSent {
    type Args<'a> = &'a [KeyEvent];
    type Input = Vec<KeyEvent>;

    fn trigger(mut pa: Pass<'_>, input: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, &input);
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
pub struct KeysSentTo<W: Widget<U>, U: Ui>(PhantomData<(&'static W, U)>);

impl<W: Widget<U>, U: Ui> Hookable for KeysSentTo<W, U> {
    type Args<'b> = (&'b [KeyEvent], &'b RwData<W>, &'b U::Area);
    type Input = (Vec<KeyEvent>, RwData<W>, U::Area);

    fn trigger(mut pa: Pass<'_>, (keys, widget, area): Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, (&keys, &widget, &area));
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
    type Args<'b> = Self::Input;
    type Input = (&'static str, FormId, Form);

    fn trigger(mut pa: Pass<'_>, pre_args: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, pre_args)
        }
    }
}

/// [`Hookable`]: Triggers when a [`ColorScheme`] is set
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
    type Args<'b> = Self::Input;
    type Input = &'static str;

    fn trigger(mut pa: Pass<'_>, pre_args: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, pre_args)
        }
    }
}

/// [`Hookable`]: Triggers when a [search] is performed
///
/// Will not be triggered on empty searches.
///
/// # Arguments
///
/// - The searched regex pattern
///
/// [search]: crate::mode::IncSearch
pub struct SearchPerformed;

impl Hookable for SearchPerformed {
    type Args<'a> = &'a str;
    type Input = String;

    fn trigger(mut pa: Pass<'_>, pre_args: Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, &pre_args)
        }
    }
}

/// [`Hookable`]: Triggers when a [search] is updated
///
/// Will not be triggered if the previous and current patterns are the
/// same.
///
/// # Arguments
///
/// - The previous regex pattern
/// - The current regex pattern
///
/// [search]: crate::mode::IncSearch
pub struct SearchUpdated;

impl Hookable for SearchUpdated {
    type Args<'a> = (&'a str, &'a str);
    type Input = (String, String);
    type Output = ();

    fn trigger(mut pa: Pass<'_>, (prev, cur): Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, (&prev, &cur))
        }
    }
}

/// [`Hookable`]: Triggers after [`File::write`] or [`File::write_to`]
///
/// Only triggers if the file was actually updated.
///
/// # Arguments
///
/// - The path of the file
/// - The number of bytes written to said file
///
/// [`File::write`]: crate::file::File::write
/// [`File::write_to`]: crate::file::File::write_to
pub struct FileWritten;

impl Hookable for FileWritten {
    type Args<'a> = (&'a str, usize);
    type Input = (String, usize);

    fn trigger(mut pa: Pass<'_>, (file, bytes): Self::Input, hooks: Hooks<Self>) {
        for hook in hooks {
            hook(&mut pa, (&file, bytes));
        }
    }
}

/// A hookable struct, for hooks taking [`Hookable::Args`]
///
/// Through this trait, Duat allows for custom hookable structs. With
/// these structs, plugin creators can create their own custom hooks,
/// and trigger them via [`hook::trigger`].
///
/// This further empowers an end user to customize the behaviour of
/// Duat in the configuration crate.
///
/// [`hook::trigger`]: trigger
pub trait Hookable: Sized + 'static {
    /// The arguments that should be passed to each [`Hook`]
    type Args<'a>;
    /// What to pass to the [`Hookable::trigger`] function
    type Input;
    /// The output of triggering all [`Hook`]s. Mostly never used
    type Output = ();

    /// Trigger all [`Hook`]s
    ///
    /// This is done this way in order to allow the [`Hookable`] more
    /// fine grained control over the activation of individual
    /// [`Hook`]s. It also permits the transformation of
    /// [`Hookable::Input`] into [`Hookable::Args`], which would
    /// otherwise just be a simple reference, which may not be the
    /// best type in each scenario.
    fn trigger(pa: Pass<'_>, input: Self::Input, hooks: Hooks<Self>) -> Self::Output;
}

/// Where all hooks of Duat are stored
#[derive(Default, Clone)]
struct InnerHooks {
    types: Rc<RefCell<HashMap<TypeId, Rc<dyn HookHolder>>>>,
    groups: Rc<RefCell<Vec<&'static str>>>,
}

impl InnerHooks {
    /// Adds a hook for a [`Hookable`]
    fn add<H: Hookable>(
        &self,
        group: &'static str,
        f: Box<dyn FnMut(Pass, H::Args<'_>) -> H::Output + 'static>,
    ) {
        let mut map = self.types.borrow_mut();

        if !group.is_empty() {
            let mut groups = self.groups.borrow_mut();
            if !groups.contains(&group) {
                groups.push(group)
            }
        }

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            let mut hooks = hooks_of.0.borrow_mut();
            hooks.push((group, Box::leak(Box::new(RefCell::new(f)))));
        } else {
            let hooks_of = HooksOf::<H>(RefCell::new(vec![(
                group,
                Box::leak(Box::new(RefCell::new(f))),
            )]));

            map.insert(TypeId::of::<H>(), Rc::new(hooks_of));
        }
    }

    /// Removes hooks with said group
    fn remove(&self, group: &'static str) {
        self.groups.borrow_mut().retain(|g| *g != group);
        let map = self.types.borrow();
        for holder in map.iter() {
            holder.1.remove(group)
        }
    }

    /// Triggers hooks with args of the [`Hookable`]
    #[define_opaque(Hooks)]
    fn trigger<H: Hookable>(&self, _: &mut Pass, pre_args: H::Input) -> H::Output {
        let holder = self.types.borrow().get(&TypeId::of::<H>()).cloned();

        // SAFETY: There is a &mut Pass argument.
        let pa = unsafe { Pass::new() };

        if let Some(holder) = holder {
            let holder = holder.clone();
            // SAFETY: HooksOf<H> is the only type that this HookHolder could be.
            let hooks_of = unsafe {
                let ptr = (&*holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            let hooks = hooks_of.0.borrow_mut().clone();
            let hooks: Hooks<H> = hooks.into_iter().map(Hook::new);
            H::trigger(pa, pre_args, hooks)
        } else {
            H::trigger(pa, pre_args, Vec::new().into_iter().map(Hook::new))
        }
    }

    /// Checks if a hook group exists
    fn group_exists(&self, group: &str) -> bool {
        self.groups.borrow().contains(&group)
    }
}

/// An intermediary trait, meant for group removal
trait HookHolder {
    /// Remove the given group from hooks of this holder
    fn remove(&self, group: &str);
}

/// An intermediary struct, meant to hold the hooks of a [`Hookable`]
struct HooksOf<H: Hookable>(RefCell<Vec<(&'static str, InnerHookFn<H>)>>);

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove(&self, group: &str) {
        let mut hooks = self.0.borrow_mut();
        hooks.retain(|(g, _)| *g != group);
    }
}

/// A function to be called when a [`Hookable`] is triggered
///
/// This function implements [`FnOnce`], as it is only meant to be
/// called once per [`trigger`] call.
pub struct Hook<H: Hookable>(InnerHookFn<H>);

impl<H: Hookable> Hook<H> {
    fn new((_, f): (&str, InnerHookFn<H>)) -> Self {
        Self(f)
    }
}

impl<H: Hookable> std::ops::FnOnce<(&mut Pass<'_>, H::Args<'_>)> for Hook<H> {
    type Output = H::Output;

    extern "rust-call" fn call_once(self, (_, args): (&mut Pass, H::Args<'_>)) -> Self::Output {
        // SAFETY: Since this function requires an exclusive reference to a
        // Pass, I can freely create new Passs.
        let pa = unsafe { Pass::new() };
        self.0.borrow_mut()(pa, args)
    }
}

type InnerHookFn<H> = &'static RefCell<
    (dyn FnMut(Pass, <H as Hookable>::Args<'_>) -> <H as Hookable>::Output + 'static),
>;
/// An [`Iterator`] over all of the [`Hook`]s that a [`Hookable`] must
/// process
pub type Hooks<H: Hookable> = impl Iterator<Item = Hook<H>>;
