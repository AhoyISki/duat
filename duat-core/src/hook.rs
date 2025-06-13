//! Utilities for hooks in Duat
//!
//! In Duat, hooks are handled through the [`Hookable`] trait. This
//! trait contains the [`Hookable::Input`] associated type, which is
//! what should be passed to hooks on the specific [`Hookable`]. By
//! implementing this trait, you allow an end user to hook executions
//! whenever said [`Hookable`] is triggered:
//!
//! ```rust
//! # struct LineNumbers<U: Ui>(std::marker::PhantomData<U>);
//! # impl<U: Ui> Widget<U> for LineNumbers<U> {
//! #     type Cfg = LineNumbersOptions<U>;
//! #     fn update(_: Pass, _: RwData<Self>, _: &<U as Ui>::Area) {}
//! #     fn needs_update(&self) -> bool { todo!(); }
//! #     fn cfg() -> Self::Cfg { todo!() }
//! #     fn text(&self) -> &Text { todo!(); }
//! #     fn text_mut(&mut self) -> &mut Text { todo!(); }
//! #     fn once() -> Result<(), Text> { Ok(()) }
//! # }
//! # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
//! # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
//! #     type Widget = LineNumbers<U>;
//! #     fn build(self, _: Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
//! #         todo!();
//! #     }
//! # }
//! use duat_core::prelude::*;
//!
//! fn test_with_ui<U: Ui>() {
//!     hook::add::<OnFileOpen<U>>(|mut pa, builder| {
//!         // `LineNumbers` comes from duat-utils
//!         builder.push(&mut pa, LineNumbers::cfg());
//!     });
//! }
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
//! - [`FileWritten`] triggers after the [`File`] is written.
//! - [`SearchPerformed`] (from duat-utils) triggers after a search is
//!   performed.
//! - [`SearchUpdated`]  (from duat-utils)triggers after a search
//!   updates.
//!
//! # Basic makeout
//!
//! When a hook is added, it can take arguments
//!
//!
//! ```rust
//! use duat_core::prelude::*;
//! struct CustomHook(usize);
//! impl Hookable for CustomHook {
//!     type Input<'h> = usize;
//!
//!     fn get_input(&mut self) -> Self::Input<'_> {
//!         self.0
//!     }
//! }
//!
//! fn runtime_function_that_triggers_hook(mut pa: Pass) {
//!     let arg = 42;
//!     hook::trigger(&mut pa, CustomHook(arg));
//! }
//! ```
//!
//! The above example ilustrates how hooks are implemented in Duat.
//! You essentially pass a struct wich will hold the arguments that
//! will be passed as input to the hooks. The [`Hookable::Input`]
//! argument makes it so you can have more convenient parameters for
//! hooks, like `(usize, &'h str)`, for example.
//!
//! [`Hookable`]s also have the [`Output`] type, which is set to `()`
//! by default, because it is mostly unnecessary. But it can be used
//! to, for example, make the builder pattern work through hooks:
//!
//! ```rust
//! use duat_core::prelude::*;
//! pub struct MyBuilder(bool, usize);
//!
//! impl MyBuilder {
//!     pub fn set_true(mut self) -> Self {
//!         self.0 = true;
//!         self
//!     }
//!
//!     pub fn set_num(mut self, num: usize) -> Self {
//!         self.1 = num;
//!         self
//!     }
//!
//!     pub fn consume(self) {
//!         todo!();
//!     }
//! }
//!
//! struct MyBuilderCreated(Option<MyBuilder>);
//! impl Hookable for MyBuilderCreated {
//!     type Input<'h> = MyBuilder;
//!     type Output = MyBuilder;
//!
//!     fn get_input(&mut self) -> Self::Input<'_> {
//!         self.0.take().unwrap()
//!     }
//!
//!     fn return_output(&mut self, output: Self::Output) {
//!         self.0 = Some(output)
//!     }
//! }
//!
//! fn runtime_function_that_triggers_hook(mut pa: Pass) {
//!     let builder = MyBuilder(false, 0);
//!
//!     let mut hookable =
//!         hook::trigger(&mut pa, MyBuilderCreated(Some(builder)));
//!
//!     let builder = hookable.0.take().unwrap();
//!     builder.consume();
//! }
//! ```
//!
//! This is, for example, the pattern that [`ModeSetTo`] follows.
//!
//! [`File`]: crate::file::File
//! [`LineNumbers`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
//! [widget]: Widget
//! [dyn Widget]: Widget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::cmd
//! [`Mode`]: crate::mode::Mode
//! [`&mut Widget`]: Widget
//! [`Output`]: Hookable::Output
//! [`SearchPerformed`]: https://docs.rs/duat-utils/latest/duat_utils/hooks/struct.SearchPerformed.html
//! [`SearchUpdated`]: https://docs.rs/duat-utils/latest/duat_utils/hooks/struct.SearchUpdated.html
use std::{any::TypeId, cell::RefCell, collections::HashMap, rc::Rc};

pub use self::global::*;
use crate::{
    data::{Pass, RwData},
    form::{Form, FormId},
    mode::{KeyEvent, Mode},
    ui::{FileBuilder, Ui, WindowBuilder},
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
    pub fn add<H: Hookable>(f: impl FnMut(Pass, H::Input<'_>) -> H::Output + 'static) {
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
        f: impl FnMut(Pass, H::Input<'_>) -> H::Output + 'static,
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
    pub fn trigger<H: Hookable>(pa: &mut Pass, hookable: H) -> H {
        unsafe { HOOKS.get().trigger(pa, hookable) }
    }

    /// Queues a [`Hookable`]'s execution
    ///
    /// You should use this if you are not on the main thread of
    /// execution, and are thus unable to call [`trigger`].
    /// The notable difference between this function and [`trigger`]
    /// is that it doesn't return the [`Hookable`], since the
    /// triggering of the hooks will happen outside of the calling
    /// function.
    ///
    /// Most of the time, this doesn't really matter, as in only a few
    /// cases do you actually need to recover the [`Hookable`], so you
    /// should be able to call this from pretty much anywhere.
    pub fn queue(hookable: impl Hookable + Send) {
        let sender = crate::context::sender();
        sender
            .send(DuatEvent::QueuedFunction(Box::new(move |mut pa| {
                // SAFETY: There is a Pass argument
                unsafe { HOOKS.get() }.trigger(&mut pa, hookable);
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
pub struct ConfigLoaded(pub(crate) ());

impl Hookable for ConfigLoaded {
    type Input<'h> = ();

    fn get_input(&mut self) -> Self::Input<'_> {}
}

/// [`Hookable`]: Triggers when Duat closes or has to reload
///
/// There are no arguments
pub struct ConfigUnloaded(pub(crate) ());

impl Hookable for ConfigUnloaded {
    type Input<'h> = ();

    fn get_input(&mut self) -> Self::Input<'_> {}
}

/// [`Hookable`]: Triggers when Duat closes
///
/// There are no arguments
pub struct ExitedDuat(pub(crate) ());

impl Hookable for ExitedDuat {
    type Input<'h> = ();

    fn get_input(&mut self) -> Self::Input<'_> {}
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
pub struct OnFileOpen<U: Ui>(pub(crate) FileBuilder<U>);

impl<U: Ui> Hookable for OnFileOpen<U> {
    type Input<'h> = &'h mut FileBuilder<U>;

    fn get_input(&mut self) -> Self::Input<'_> {
        &mut self.0
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
pub struct OnWindowOpen<U: Ui>(pub(crate) WindowBuilder<U>);

impl<U: Ui> Hookable for OnWindowOpen<U> {
    type Input<'h> = &'h mut WindowBuilder<U>;

    fn get_input(&mut self) -> Self::Input<'_> {
        &mut self.0
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is focused
///
/// # Arguments
///
/// - The widget itself.
/// - Its [area].
///
/// [`Widget`]: crate::widget::Widget
/// [area]: crate::ui::Area
pub struct FocusedOn<W: Widget<U>, U: Ui>(pub(crate) (RwData<W>, U::Area));

impl<W: Widget<U>, U: Ui> Hookable for FocusedOn<W, U> {
    type Input<'h> = (&'h RwData<W>, &'h U::Area);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, &self.0.1)
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is unfocused
///
/// # Arguments
///
/// - The widget itself.
/// - Its [area].
///
/// [`Widget`]: crate::widget::Widget
/// [area]: crate::ui::Area
pub struct UnfocusedFrom<W: Widget<U>, U: Ui>(pub(crate) (RwData<W>, U::Area));

impl<W: Widget<U>, U: Ui> Hookable for UnfocusedFrom<W, U> {
    type Input<'h> = (&'h RwData<W>, &'h U::Area);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, &self.0.1)
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
pub struct ModeSwitched(pub(crate) (&'static str, &'static str));

impl Hookable for ModeSwitched {
    type Input<'h> = (&'static str, &'static str);

    fn get_input(&mut self) -> Self::Input<'_> {
        self.0
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
pub struct ModeSetTo<M: Mode<U>, U: Ui>(pub(crate) (Option<M>, RwData<M::Widget>, U::Area));

impl<M: Mode<U>, U: Ui> Hookable for ModeSetTo<M, U> {
    type Input<'h> = (M, &'h RwData<M::Widget>, &'h U::Area);
    type Output = M;

    fn get_input(&mut self) -> Self::Input<'_> {
        (self.0.0.take().unwrap(), &self.0.1, &self.0.2)
    }

    fn return_output(&mut self, output: Self::Output) {
        self.0.0 = Some(output)
    }
}

/// [`Hookable`]: Triggers whenever a [key] is sent
///
/// # Arguments
///
/// - The [key] sent.
///
/// [key]: KeyEvent
pub struct KeysSent(pub(crate) Vec<KeyEvent>);

impl Hookable for KeysSent {
    type Input<'h> = &'h [KeyEvent];

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
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
pub struct KeysSentTo<W: Widget<U>, U: Ui>(pub(crate) (Vec<KeyEvent>, RwData<W>, U::Area));

impl<W: Widget<U>, U: Ui> Hookable for KeysSentTo<W, U> {
    type Input<'h> = (&'h [KeyEvent], &'h RwData<W>, &'h U::Area);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, &self.0.1, &self.0.2)
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
pub struct FormSet(pub(crate) (&'static str, FormId, Form));

impl Hookable for FormSet {
    type Input<'h> = (&'static str, FormId, Form);

    fn get_input(&mut self) -> Self::Input<'_> {
        (self.0.0, self.0.1, self.0.2)
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
pub struct ColorSchemeSet(pub(crate) &'static str);

impl Hookable for ColorSchemeSet {
    type Input<'h> = &'static str;

    fn get_input(&mut self) -> Self::Input<'_> {
        self.0
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
pub struct FileWritten(pub(crate) (String, usize));

impl Hookable for FileWritten {
    type Input<'i> = (&'i str, usize);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, self.0.1)
    }
}

/// A hookable struct, for hooks taking [`Hookable::Input`]
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
    /// The arguments that are passed to each hook.
    type Input<'h>;
    /// The output of triggering hooks. Mostly never used
    ///
    /// This value is never returned when calling [`hook::trigger`],
    /// instead, through the [`Hookable::return_output`] function, you
    /// are supposed to store it in [`Self`], and then you can access
    /// it after the [`hook::trigger`] call, if it supposed to be
    /// something like the builder pattern.
    ///
    /// [`hook::trigger`]: global::trigger
    /// [`Self`]: Hookable
    type Output = ();

    /// How to get the arguments from the [`Hookable`]
    fn get_input(&mut self) -> Self::Input<'_>;

    /// When a [`Hookable`] has an [`Output`], you can define how it
    /// takes it back
    ///
    /// One example of how this can be useful is if your [`Hookable`]
    /// is using a builder pattern definition for the [`Output`], like
    /// the [`ModeSetTo`] [`Hookable`].
    ///
    /// [`Output`]: Hookable::Output
    #[allow(unused_variables)]
    fn return_output(&mut self, output: Self::Output) {}
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
        f: Box<dyn FnMut(Pass, H::Input<'_>) -> H::Output + 'static>,
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
    fn trigger<H: Hookable>(&self, _: &mut Pass, mut hookable: H) -> H {
        let holder = self.types.borrow().get(&TypeId::of::<H>()).cloned();

        let Some(holder) = holder else {
            return hookable;
        };

        let holder = holder.clone();
        // SAFETY: HooksOf<H> is the only type that this HookHolder could be.
        let hooks_of = unsafe {
            let ptr = (&*holder as *const dyn HookHolder).cast::<HooksOf<H>>();
            ptr.as_ref().unwrap()
        };

        let hooks = hooks_of.0.borrow_mut();
        for (_, hook) in hooks.iter() {
            // SAFETY: There is a &mut Pass argument.
            let pa = unsafe { Pass::new() };
            let input = hookable.get_input();
            let output = hook.borrow_mut()(pa, input);
            hookable.return_output(output);
        }

        hookable
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

type InnerHookFn<H> =
    &'static RefCell<(dyn FnMut(Pass, <H as Hookable>::Input<'_>) -> <H as Hookable>::Output)>;
