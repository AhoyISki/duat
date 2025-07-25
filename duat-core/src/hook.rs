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
//! #     fn update(_: &mut Pass, _: Handle<Self, U>) {}
//! #     fn needs_update(&self) -> bool { todo!(); }
//! #     fn cfg() -> Self::Cfg { todo!() }
//! #     fn text(&self) -> &Text { todo!(); }
//! #     fn text_mut(&mut self) -> &mut Text { todo!(); }
//! #     fn once() -> Result<(), Text> { Ok(()) }
//! # }
//! # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
//! # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
//! #     type Widget = LineNumbers<U>;
//! #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
//! #         todo!();
//! #     }
//! # }
//! use duat_core::{hook::OnFileOpen, prelude::*};
//!
//! fn test_with_ui<U: Ui>() {
//!     hook::add::<OnFileOpen<U>, U>(|pa, builder| {
//!         // `LineNumbers` comes from duat-utils
//!         builder.push(pa, LineNumbers::cfg());
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
//! - [`WidgetCreated`] triggers when a [`Widget`]'s [cfg] is created,
//!   letting you change it, [`Widget`] can be used as its [alias]
//! - [`OnFileOpen`], which lets you push widgets around a [`File`].
//! - [`OnWindowOpen`], which lets you push widgets around the window.
//! - [`OnFileClose`], which triggers on every file upon closing Duat.
//! - [`OnFileReload`], which triggers on every file upon reloading
//!   Duat.
//! - [`FocusedOn`] lets you act on a [widget] when focused.
//! - [`UnfocusedFrom`] lets you act on a [widget] when unfocused.
//! - [`KeysSent`] lets you act on a [dyn Widget], given a[key].
//! - [`KeysSentTo`] lets you act on a given [widget], given a [key].
//! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
//! - [`ModeSwitched`] triggers when you change [`Mode`].
//! - [`ModeCreated`] lets you act on a [`Mode`] after switching.
//! - [`FileWritten`] triggers after the [`File`] is written.
//! - [`SearchPerformed`] (from `duat-utils`) triggers after a search
//!   is performed.
//! - [`SearchUpdated`] (from `duat-utils`) triggers after a search
//!   updates.
//!
//! # Basic makeout
//!
//! When a hook is added, it can take arguments
//!
//!
//! ```rust
//! use duat_core::{hook::Hookable, prelude::*};
//! struct CustomHook(usize);
//! impl Hookable for CustomHook {
//!     type Input<'h> = usize;
//!
//!     fn get_input(&mut self) -> Self::Input<'_> {
//!         self.0
//!     }
//! }
//!
//! fn runtime_function_that_triggers_hook(pa: &mut Pass) {
//!     let arg = 42;
//!     hook::trigger(pa, CustomHook(arg));
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
//! use duat_core::{hook::Hookable, prelude::*};
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
//!     fn take_output_back(&mut self, output: Self::Output) {
//!         self.0 = Some(output)
//!     }
//! }
//!
//! fn runtime_function_that_triggers_hook(pa: &mut Pass) {
//!     let builder = MyBuilder(false, 0);
//!
//!     let mut hookable = hook::trigger(pa, MyBuilderCreated(Some(builder)));
//!
//!     let builder = hookable.0.take().unwrap();
//!     builder.consume();
//! }
//! ```
//!
//! This is, for example, the pattern that [`ModeCreated`] follows.
//!
//! [alias]: HookAlias
//! [cfg]: crate::ui::Widget::Cfg
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
use std::{any::TypeId, cell::RefCell, collections::HashMap, marker::PhantomData, sync::Mutex};

pub use self::global::*;
use crate::{
    context::{Cache, FileHandle, Handle},
    data::Pass,
    file::File,
    form::{Form, FormId},
    mode::{KeyEvent, Mode},
    ui::{FileBuilder, Ui, Widget, WindowBuilder},
};

/// Hook functions
mod global {
    use std::sync::LazyLock;

    use super::{HookAlias, Hookable, InnerHooks};
    use crate::{
        data::Pass,
        hook::HookDummy,
        ui::{DuatEvent, Ui},
    };

    static HOOKS: LazyLock<InnerHooks> = LazyLock::new(InnerHooks::default);

    /// Adds a [hook]
    ///
    /// This hook is ungrouped, that is, it cannot be removed. If you
    /// want a hook that is removable, see [`hook::add_grouped`]. If
    /// you don't have access to a [`Ui`] argument for some reason,
    /// see [`hook::add_no_alias`].
    ///
    /// [hook]: Hookable
    /// [`hook::add_grouped`]: add_grouped
    /// [`hook::add_no_alias`]: add_no_alias
    #[inline(never)]
    pub fn add<H: HookAlias<U, impl HookDummy>, U: Ui>(
        f: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + Send + 'static,
    ) {
        HOOKS.add::<H::Hookable>("", Box::new(f));
    }

    /// Adds a [hook], without accepting aliases
    ///
    /// Use this if you want to add a hook, but have no access to
    /// [`Ui`] parameter.
    ///
    /// [hook]: Hookable
    #[doc(hidden)]
    pub fn add_no_alias<H: Hookable>(
        f: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + Send + 'static,
    ) {
        HOOKS.add::<H>("", Box::new(f));
    }

    /// Adds a grouped [hook]
    ///
    /// A grouped hook is one that, along with others on the same
    /// group, can be removed by [`hook::remove`]. If you do
    /// not need/want this feature, take a look at [`hook::add`]. If
    /// you don't have access to a [`Ui`] argument for some reason,
    /// see [`hook::add_grouped_no_alias`].
    ///
    /// [hook]: Hookable
    /// [`hook::remove`]: remove
    /// [`hook::add`]: add
    /// [`hook::add_grouped_no_alias`]: add_grouped_no_alias
    #[inline(never)]
    pub fn add_grouped<H: HookAlias<U, impl HookDummy>, U: Ui>(
        group: &'static str,
        f: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + Send + 'static,
    ) {
        HOOKS.add::<H::Hookable>(group, Box::new(f));
    }

    /// Adds a grouped [hook], without accepting aliases
    ///
    /// Use this if you want to add a hook, but have no access to
    /// [`Ui`] parameter.
    ///
    /// [hook]: Hookable
    #[doc(hidden)]
    pub fn add_grouped_no_alias<H: Hookable>(
        f: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + Send + 'static,
    ) {
        HOOKS.add::<H>("", Box::new(f));
    }

    /// Removes a [hook] group
    ///
    /// By removing the group, this function will remove all hooks
    /// added via [`hook::add_grouped`] with the same group.
    ///
    /// [hook]: Hookable
    /// [`hook::add_grouped`]: add_grouped
    pub fn remove(group: &'static str) {
        HOOKS.remove(group);
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
    pub fn trigger<H: Hookable>(pa: &mut Pass, hookable: H) -> H {
        HOOKS.trigger(pa, hookable)
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
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                trigger(pa, hookable);
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
        HOOKS.group_exists(group)
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

/// [`Hookable`]: Triggers when Duat is refocused
///
/// # Arguments
///
/// There are no arguments
pub struct FocusedOnDuat(pub(crate) ());

impl Hookable for FocusedOnDuat {
    type Input<'h> = ();

    fn get_input(&mut self) -> Self::Input<'_> {}
}

/// [`Hookable`]: Triggers when Duat is unfocused
///
/// # Arguments
///
/// There are no arguments
pub struct UnfocusedFromDuat(pub(crate) ());

impl Hookable for UnfocusedFromDuat {
    type Input<'h> = ();

    fn get_input(&mut self) -> Self::Input<'_> {}
}

/// [`Hookable`]: Triggers when a [`Widget`]'s [cfg] is created
///
/// # Arguments
///
/// - The [`WidgetCfg`] in question.
/// - The [`FileHandle`] to which it was pushed, if there was one.
///
/// # Aliases
///
/// Since every [`Widget`] implements the [`HookAlias`] trait, instead
/// of writing this in the config crate:
///
/// ```rust
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<WidgetCreated<LineNumbers<Ui>>>(|pa, (ln, handle)| ln.rel_abs());
/// }
/// ```
///
/// You can just write this:
///
/// ```rust
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<LineNumbers<Ui>>(|pa, (ln, handle)| ln.rel_abs());
/// }
/// ```
///
/// Do note that this [hook] doesn't let you change the layout of the
/// pushed [`Widget`]s, only their configuration. So if one of these
/// changes changed the [direction] where the [`Widget`] was pushed,
/// for example, the layout could get messed up.
///
/// If you want to change the layout of the [`Widget`]s in a controled
/// fashion, look at [`OnFileOpen`] and [`OnWindowOpen`].
///
/// [cfg]: crate::ui::Widget::Cfg
/// [`WidgetCfg`]: crate::ui::WidgetCfg
/// [hook]: self
/// [direction]: crate::ui::PushSpecs
pub struct WidgetCreated<W: Widget<U>, U: Ui>(pub(crate) (Option<W::Cfg>, Option<FileHandle<U>>));

impl<W: Widget<U>, U: Ui> Hookable for WidgetCreated<W, U> {
    type Input<'h> = (W::Cfg, Option<&'h FileHandle<U>>);
    type Output = W::Cfg;

    fn get_input(&mut self) -> Self::Input<'_> {
        (self.0.0.take().unwrap(), self.0.1.as_ref())
    }

    fn take_output_back(&mut self, output: Self::Output) {
        self.0.0 = Some(output)
    }
}

/// [`Hookable`]: Triggers when a [`File`] is opened
///
/// # Arguments
///
/// - The file [builder], which can be used to push widgets to the
///   file, and to eachother.
///
/// This is a rather "advanced" [hook], since it lets you change the
/// layout of [`Widget`]s around the screen. If you don't need all
/// that power, you can check out [`WidgetCreated`], which is a more
/// straightforward form of changing [`Widget`]s, and doesn't
/// interfere with the default hooks of `"FileWidgets"` and
/// `"WindowWidgets"`, preset by Duat.
///
/// [builder]: crate::ui::FileBuilder
/// [hook]: self
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
/// This is a rather "advanced" [hook], since it lets you change the
/// layout of [`Widget`]s around the screen. If you don't need all
/// that power, you can check out [`WidgetCreated`], which is a more
/// straightforward form of changing [`Widget`]s, and doesn't
/// interfere with the default hooks of `"FileWidgets"` and
/// `"WindowWidgets"`, preset by Duat.
///
/// [builder]: crate::ui::WindowBuilder
/// [hook]: self
pub struct OnWindowOpen<U: Ui>(pub(crate) WindowBuilder<U>);

impl<U: Ui> Hookable for OnWindowOpen<U> {
    type Input<'h> = &'h mut WindowBuilder<U>;

    fn get_input(&mut self) -> Self::Input<'_> {
        &mut self.0
    }
}

/// [`Hookable`]: Triggers before closing a [`File`]
///
/// # Arguments
///
/// - The [`File`]'s [`Handle`].
/// - A [`Cache`]. This can be used in order to decide wether or not
///   some things will be reloaded on the next opening of Duat.
///
/// This will not trigger upon reloading Duat. For that, see
/// [`OnFileClose`].
pub struct OnFileClose<U: Ui>(pub(crate) (Handle<File<U>, U>, Cache));

impl<U: Ui> Hookable for OnFileClose<U> {
    type Input<'h> = &'h (Handle<File<U>, U>, Cache);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers before reloading a [`File`]
///
/// # Arguments
///
/// - The [`File`]'s [`Handle`].
/// - A [`Cache`]. This can be used in order to decide wether or not
///   some things will be reloaded on the next opening of Duat.
///
/// This will not trigger upon closing Duat. For that, see
/// [`OnFileClose`].
pub struct OnFileReload<U: Ui>(pub(crate) (Handle<File<U>, U>, Cache));

impl<U: Ui> Hookable for OnFileReload<U> {
    type Input<'h> = &'h (Handle<File<U>, U>, Cache);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is focused
///
/// # Arguments
///
/// - A [`Handle<dyn Widget>`] for newly focused [`Widget`]
/// - A [`Handle<W>`] for the unfocused [`Widget`]
pub struct FocusedOn<W: Widget<U>, U: Ui>(pub(crate) (Handle<dyn Widget<U>, U>, Handle<W, U>));

impl<W: Widget<U>, U: Ui> Hookable for FocusedOn<W, U> {
    type Input<'h> = &'h (Handle<dyn Widget<U>, U>, Handle<W, U>);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is unfocused
///
/// # Arguments
///
/// - A [`Handle<W>`] for newly focused [`Widget`]
/// - A [`Handle<dyn Widget>`] for the unfocused [`Widget`]
pub struct UnfocusedFrom<W: Widget<U>, U: Ui>(pub(crate) (Handle<W, U>, Handle<dyn Widget<U>, U>));

impl<W: Widget<U>, U: Ui> Hookable for UnfocusedFrom<W, U> {
    type Input<'h> = &'h (Handle<W, U>, Handle<dyn Widget<U>, U>);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when the [`Mode`] is changed
///
/// # Arguments
///
/// - The previous mode.
/// - The current mode.
///
/// # Aliases
///
/// Since every [`Mode`] implements the [`HookAlias`] trait, given a
/// `duat_kak` plugin imported as `kak`, instead of writing this:
///
/// ```rust
/// # mod kak {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone)]
/// #     pub struct Normal;
/// #     impl<U: Ui> Mode<U> for Normal {
/// #         type Widget = File<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use kak::Normal;
///
/// fn setup() {
///     hook::add::<ModeCreated<Normal>>(|pa, (normal, handle)| normal);
/// }
/// ```
///
/// You can just write this:
///
/// ```rust
/// # mod kak {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone)]
/// #     pub struct Normal;
/// #     impl<U: Ui> Mode<U> for Normal {
/// #         type Widget = File<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use kak::Normal;
///
/// fn setup() {
///     hook::add::<Normal>(|pa, (normal, handle)| normal);
/// }
/// ```
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
pub struct ModeCreated<M: Mode<U>, U: Ui>(pub(crate) (Option<M>, Handle<M::Widget, U>));

impl<M: Mode<U>, U: Ui> Hookable for ModeCreated<M, U> {
    type Input<'h> = (M, &'h Handle<M::Widget, U>);
    type Output = M;

    fn get_input(&mut self) -> Self::Input<'_> {
        (self.0.0.take().unwrap(), &self.0.1)
    }

    fn take_output_back(&mut self, output: Self::Output) {
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
/// - An [`Handle<W>`] for the widget.
///
/// [key]: KeyEvent
pub struct KeysSentTo<W: Widget<U>, U: Ui>(pub(crate) (Vec<KeyEvent>, Handle<W, U>));

impl<W: Widget<U>, U: Ui> Hookable for KeysSentTo<W, U> {
    type Input<'h> = (&'h [KeyEvent], &'h Handle<W, U>);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, &self.0.1)
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
/// - Wether Duat is in the process of quitting (happens when calling
///   the `wq` or `waq` commands)
///
/// [`File::write`]: crate::file::File::write
/// [`File::write_to`]: crate::file::File::write_to
pub struct FileWritten(pub(crate) (String, usize, bool));

impl Hookable for FileWritten {
    type Input<'h> = (&'h str, usize, bool);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, self.0.1, self.0.2)
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
pub trait Hookable<_H: HookDummy = NormalHook>: Sized + 'static {
    /// The arguments that are passed to each hook.
    type Input<'h>;
    /// The output of triggering hooks. Mostly never used
    ///
    /// This value is never returned when calling [`hook::trigger`],
    /// instead, through the [`Hookable::take_output_back`] function,
    /// you are supposed to store it in [`Self`], and then you can
    /// access it after the [`hook::trigger`] call, if it supposed
    /// to be something like the builder pattern.
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
    /// the [`ModeCreated`] [`Hookable`].
    ///
    /// [`Output`]: Hookable::Output
    #[allow(unused_variables)]
    fn take_output_back(&mut self, output: Self::Output) {}
}

/// Where all hooks of Duat are stored
#[derive(Clone, Copy)]
struct InnerHooks {
    types: &'static Mutex<HashMap<TypeId, Box<dyn HookHolder>>>,
    groups: &'static Mutex<Vec<&'static str>>,
}

impl InnerHooks {
    /// Adds a hook for a [`Hookable`]
    fn add<H: Hookable>(
        &self,
        group: &'static str,
        f: Box<dyn FnMut(&mut Pass, H::Input<'_>) -> H::Output + 'static>,
    ) {
        let mut map = self.types.lock().unwrap();

        if !group.is_empty() {
            let mut groups = self.groups.lock().unwrap();
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

            map.insert(TypeId::of::<H>(), Box::new(hooks_of));
        }
    }

    /// Removes hooks with said group
    fn remove(&self, group: &'static str) {
        self.groups.lock().unwrap().retain(|g| *g != group);
        let map = self.types.lock().unwrap();
        for holder in map.iter() {
            holder.1.remove(group)
        }
    }

    /// Triggers hooks with args of the [`Hookable`]
    fn trigger<H: Hookable>(&self, pa: &mut Pass, mut hookable: H) -> H {
        let holder = self.types.lock().unwrap().remove(&TypeId::of::<H>());

        let Some(holder) = holder else {
            return hookable;
        };

        // SAFETY: HooksOf<H> is the only type that this HookHolder could be.
        let hooks_of = unsafe {
            let ptr = Box::into_raw(holder) as *mut HooksOf<H>;
            Box::from_raw(ptr)
        };

        for (_, hook) in hooks_of.0.borrow_mut().iter() {
            let input = hookable.get_input();
            let output = hook.borrow_mut()(pa, input);
            hookable.take_output_back(output);
        }

        self.types
            .lock()
            .unwrap()
            .insert(TypeId::of::<H>(), unsafe {
                Box::from_raw(Box::into_raw(hooks_of) as *mut (dyn HookHolder))
            });

        hookable
    }

    /// Checks if a hook group exists
    fn group_exists(&self, group: &str) -> bool {
        self.groups.lock().unwrap().contains(&group)
    }
}

impl Default for InnerHooks {
    fn default() -> Self {
        Self {
            types: Box::leak(Box::default()),
            groups: Box::leak(Box::default()),
        }
    }
}

unsafe impl Send for InnerHooks {}
unsafe impl Sync for InnerHooks {}

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
    &'static RefCell<(dyn FnMut(&mut Pass, <H as Hookable>::Input<'_>) -> <H as Hookable>::Output)>;

/// An alias for a [`Hookable`]
///
/// This trait is not normally meant to be implemented manually,
/// instead, it is automatically derived for every [`Hookable`].
///
/// You can use this if you want to use something that is not
/// [`Hookable`] as a [`Hookable`] alias that gets translated to an
/// actually [`Hookable`] type via [`HookAlias::Hookable`]. An example
/// of where this is used is with [`Widget`]s and [`Mode`]s:
///
/// ```rust
/// use duat_core::{
///     hook::{HookAlias, HookDummy, Hookable},
///     prelude::*,
/// };
///
/// pub struct CreatedStruct<T: 'static>(Option<T>);
///
/// impl<T: 'static> Hookable for CreatedStruct<T> {
///     type Input<'h> = T;
///     type Output = T;
///
///     fn get_input(&mut self) -> Self::Input<'_> {
///         self.0.take().unwrap()
///     }
///
///     fn take_output_back(&mut self, output: Self::Output) {
///         self.0 = Some(output);
///     }
/// }
///
/// struct MyStructWithAVeryLongName;
///
/// // In the user's config crate:
/// # {
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     // This is way too long
///     hook::add::<CreatedStruct<MyStructWithAVeryLongName>>(|pa, arg| arg);
/// }
/// # }
/// ```
///
/// In this case, you can do this instead:
///
/// ```rust
/// use duat_core::{
///     hook::{HookAlias, HookDummy, Hookable},
///     prelude::*,
/// };
///
/// pub struct CreatedStruct<T: 'static>(Option<T>);
///
/// impl<T: 'static> Hookable for CreatedStruct<T> {
///     type Input<'h> = T;
///     type Output = T;
///
///     fn get_input(&mut self) -> Self::Input<'_> {
///         self.0.take().unwrap()
///     }
///
///     fn take_output_back(&mut self, output: Self::Output) {
///         self.0 = Some(output);
///     }
/// }
///
/// struct MyStructWithAVeryLongName;
///
/// struct MyDummy;
///
/// #[doc(hidden)]
/// impl HookDummy for MyDummy {}
///
/// impl<U: Ui> HookAlias<U, MyDummy> for MyStructWithAVeryLongName {
///     type Hookable = CreatedStruct<MyStructWithAVeryLongName>;
///     type Input<'h> = <Self::Hookable as Hookable>::Input<'h>;
///     type Output = <Self::Hookable as Hookable>::Output;
/// }
///
/// // In the user's config crate:
/// # {
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     // Much better
///     hook::add::<MyStructWithAVeryLongName>(|pa, arg| arg);
/// }
/// # }
/// ```
pub trait HookAlias<U: Ui, D: HookDummy = NormalHook> {
    /// Just a shorthand for less boilerplate in the function
    /// definition
    type Input<'h>;
    /// Just a shorthand for less boilerplate in the function
    /// definition
    type Output;

    /// The actual [`Hookable`] that this [`HookAlias`] is supposed to
    /// map to
    type Hookable: for<'h> Hookable<Input<'h> = Self::Input<'h>, Output = Self::Output>;
}

impl<H: Hookable, U: Ui> HookAlias<U> for H {
    type Hookable = Self;
    type Input<'h> = H::Input<'h>;
    type Output = H::Output;
}

impl<W: Widget<U>, U: Ui> HookAlias<U, WidgetCreatedDummy<U>> for W {
    type Hookable = WidgetCreated<W, U>;
    type Input<'h> = <WidgetCreated<W, U> as Hookable>::Input<'h>;
    type Output = <WidgetCreated<W, U> as Hookable>::Output;
}

impl<M: Mode<U>, U: Ui> HookAlias<U, ModeCreatedDummy<U>> for M {
    type Hookable = ModeCreated<M, U>;
    type Input<'h> = <ModeCreated<M, U> as Hookable>::Input<'h>;
    type Output = <ModeCreated<M, U> as Hookable>::Output;
}

/// Use this trait if you want to make specialized hooks
///
/// This trait in particular doesn't really serve any purposes other
/// than allowing for specialization resolution. It is recommended
/// that you use `#[doc(hidden)]` for any type implementing this
/// trait.
///
/// ```rust
/// use duat_core::{
///     hook::{HookAlias, HookDummy, Hookable},
///     prelude::*,
/// };
///
/// pub struct CreatedStruct<T: 'static>(Option<T>);
///
/// impl<T: 'static> Hookable for CreatedStruct<T> {
///     type Input<'h> = T;
///     type Output = T;
///
///     fn get_input(&mut self) -> Self::Input<'_> {
///         self.0.take().unwrap()
///     }
///
///     fn take_output_back(&mut self, output: Self::Output) {
///         self.0 = Some(output);
///     }
/// }
///
/// struct MyStructWithAVeryLongName;
///
/// // In the user's config crate:
/// # {
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     // This is way too long
///     hook::add::<CreatedStruct<MyStructWithAVeryLongName>>(|pa, arg| arg);
/// }
/// # }
/// ```
///
/// In this case, you can do this instead:
///
/// ```rust
/// use duat_core::{
///     hook::{HookAlias, HookDummy, Hookable},
///     prelude::*,
/// };
///
/// pub struct CreatedStruct<T: 'static>(Option<T>);
///
/// impl<T: 'static> Hookable for CreatedStruct<T> {
///     type Input<'h> = T;
///     type Output = T;
///
///     fn get_input(&mut self) -> Self::Input<'_> {
///         self.0.take().unwrap()
///     }
///
///     fn take_output_back(&mut self, output: Self::Output) {
///         self.0 = Some(output);
///     }
/// }
///
/// struct MyStructWithAVeryLongName;
///
/// struct MyDummy;
///
/// #[doc(hidden)]
/// impl HookDummy for MyDummy {}
///
/// impl<U: Ui> HookAlias<U, MyDummy> for MyStructWithAVeryLongName {
///     type Hookable = CreatedStruct<MyStructWithAVeryLongName>;
///     type Input<'h> = <Self::Hookable as Hookable>::Input<'h>;
///     type Output = <Self::Hookable as Hookable>::Output;
/// }
///
/// // In the user's config crate:
/// # {
/// # use duat_core::doc_duat as duat;
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     // Much better
///     hook::add::<MyStructWithAVeryLongName>(|pa, arg| arg);
/// }
/// # }
/// ```
pub trait HookDummy {}

/// For specialization purposes
#[doc(hidden)]
pub struct NormalHook;

impl HookDummy for NormalHook {}

/// For specialization purposes
#[doc(hidden)]
pub struct WidgetCreatedDummy<U>(PhantomData<U>);

impl<U> HookDummy for WidgetCreatedDummy<U> {}

/// For specialization purposes
#[doc(hidden)]
pub struct ModeCreatedDummy<U>(PhantomData<U>);

impl<U> HookDummy for ModeCreatedDummy<U> {}
