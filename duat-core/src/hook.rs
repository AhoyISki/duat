//! Utilities for hooks in Duat
//!
//! In Duat, hooks are handled through the [`Hookable`] trait. This
//! trait contains the [`Hookable::Input`] associated type, which is
//! what should be passed to hooks on the specific [`Hookable`]. By
//! implementing this trait, you allow an end user to hook executions
//! whenever said [`Hookable`] is triggered:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     hook::add::<File>(|pa: &mut Pass, (cfg, builder)| {
//!         // `LineNumbers` comes from duat-utils
//!         builder.push(LineNumbers::cfg());
//!
//!         if let Some("lisp") = cfg.filetype() {
//!             cfg.dont_wrap()
//!         } else {
//!             cfg
//!         }
//!     });
//! }
//! ```
//!
//! The hook above is triggered whenever a [`File`] widget is opened.
//! Like every other hook, it gives you access to the global state via
//! the [`Pass`], and this one also gets you a [`UiBuilder`] and a
//! [`Widget::Cfg`] argument. The [`UiBuilder`] lets you push new
//! [`Widget`]s around the [`File`], while the [`Widget::Cfg`]
//! argument lets you modify a [`Widget`] before it gets added in. You
//! can call this hook with any [`Widget`], not just the [`File`].
//!
//! This is just one of many built-in [`Hookable`]s. Currently, these
//! are the existing hooks in `duat-core`, but you can also make your
//! own:
//!
//! - [`ConfigLoaded`] triggers after loading the config crate.
//! - [`ConfigUnloaded`] triggers after unloading the config crate.
//! - [`ExitedDuat`] triggers after Duat has exited.
//! - [`FocusedOnDuat`] triggers when Duat gains focus.
//! - [`UnfocusedFromDuat`] triggers when Duat loses focus.
//! - [`WidgetCreated`] triggers when a [`Widget`]'s [cfg] is created,
//!   letting you change it, [`Widget`] can be used as its [alias]
//! - [`WindowCreated`], which lets you push widgets around the
//!   window.
//! - [`FileWritten`] triggers after the [`File`] is written.
//! - [`FileClosed`] triggers on every file upon closing Duat.
//! - [`FileReloaded`] triggers on every file upon reloading Duat.
//! - [`FocusedOn`] lets you act on a [widget] when focused.
//! - [`UnfocusedFrom`] lets you act on a [widget] when unfocused.
//! - [`KeysSent`] lets you act on a [dyn Widget], given a [key].
//! - [`KeysSentTo`] lets you act on a given [widget], given a [key].
//! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
//! - [`ModeSwitched`] triggers when you change [`Mode`].
//! - [`ModeCreated`] lets you act on a [`Mode`] after switching.
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
    context::{Cache, Handle},
    data::Pass,
    file::File,
    form::{Form, FormId},
    mode::{KeyEvent, Mode},
    text::Text,
    ui::{Ui, UiBuilder, Widget},
};

/// Hook functions
mod global {
    use std::sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    };

    use super::{HookAlias, HookDummy, Hookable, InnerGroupId, InnerHooks};
    use crate::{data::Pass, session::DuatEvent, text::Text, ui::Ui};

    static HOOKS: LazyLock<InnerHooks> = LazyLock::new(InnerHooks::default);

    /// A [`GroupId`] that can be used in order to remove hooks
    ///
    /// When [adding grouped hooks], you can either use strings or a
    /// [`GroupId`]. You should use strings for publicly removable
    /// hooks, and [`GroupId`]s for privately removable hooks:
    ///
    /// [adding grouped hooks]: add_grouped
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct GroupId(usize);

    impl GroupId {
        /// Returns a new [`GroupId`]
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            static HOOK_GROUPS: AtomicUsize = AtomicUsize::new(0);
            Self(HOOK_GROUPS.fetch_add(1, Ordering::Relaxed))
        }

        /// Remove all hooks that belong to this [`GroupId`]
        ///
        /// This can be used in order to have hooks remove themselves,
        /// for example.
        pub fn remove(self) {
            remove(self)
        }
    }

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
        f: impl FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static,
    ) {
        HOOKS.add::<H::Hookable>(None, Box::new(f));
    }

    /// Adds a grouped [hook]
    ///
    /// The group can either be a type that implements
    /// [`std::fmt::Display`], like [`String`] or [`&str`], or it can
    /// be a [`GroupId`], which is a dedicated structure for anonymous
    /// hook grouping.
    ///
    /// As a plugin writer, when you use a string as the hook group,
    /// you are allowing an end user to remove that hook group. If you
    /// use a [`GroupId`], only you can remove it, since only you have
    /// access to the [`GroupId`]. Which one you choose depends on
    /// those two options.
    ///
    /// Alternatively, if you don't have a use for removing hooks, you
    /// can just call [`hook::add`] in order to add them without a
    /// group. In addition, if you don't have access to a [`Ui`]
    /// argument (inside of a static definition, for example), you can
    /// call [`hook::add_grouped_no_alias`], which doesn't require a
    /// [`Ui`] argument.
    ///
    /// [hook]: Hookable
    /// [`hook::remove`]: remove
    /// [`hook::add`]: add
    /// [`hook::add_grouped_no_alias`]: add_grouped_no_alias
    /// [`&str`]: str
    #[inline(never)]
    pub fn add_grouped<H: HookAlias<U, impl HookDummy>, U: Ui>(
        group: impl Into<InnerGroupId>,
        f: impl FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static,
    ) {
        HOOKS.add::<H::Hookable>(Some(group.into()), Box::new(f));
    }

    /// Adds a [hook] to be executed only once
    ///
    /// This hook will only trigger once, being removed after the
    /// fact. Alternatively, this can also be achieved by making
    /// use of a [`GroupId`], like so:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     let group_id = hook::GroupId::new();
    ///     hook::add_grouped::<WindowCreated>(group_id, move |_, builder| {
    ///         builder.push(status!("Main Window").above());
    ///         group_id.remove();
    ///     });
    /// }
    /// ```
    ///
    /// This hook should only be triggered on the first opened window,
    /// after which it self destructs via [`GroupId::remove`]. This
    /// method also allows for any number of triggers to this hook, or
    /// for a condition to cause its removal, unlike
    /// [`hook::add_once`].
    ///
    /// [hook]: Hookable
    /// [`hook::add_once`]: add_once
    pub fn add_once<H: HookAlias<U, impl HookDummy>, U: Ui>(
        mut f: impl FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static,
    ) {
        let group_id = GroupId::new();
        HOOKS.add::<H::Hookable>(
            Some(group_id.into()),
            Box::new(move |pa, input| {
                let ret = f(pa, input);
                group_id.remove();
                ret
            }),
        );
    }

    /// Adds a [hook], without accepting aliases
    ///
    /// Use this if you want to add a hook, but have no access to
    /// [`Ui`] parameter, like when declaring static variables on
    /// plugins.
    ///
    /// [hook]: Hookable
    #[doc(hidden)]
    pub fn add_no_alias<H: Hookable>(
        f: impl FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static,
    ) {
        HOOKS.add::<H>(None, Box::new(f));
    }

    /// Adds a grouped [hook], without accepting aliases
    ///
    /// Use this if you want to add a hook, but have no access to
    /// [`Ui`] parameter, like when declaring static variables on
    /// plugins.
    ///
    /// [hook]: Hookable
    #[doc(hidden)]
    pub fn add_grouped_no_alias<H: Hookable>(
        group: impl Into<InnerGroupId>,
        f: impl FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static,
    ) {
        HOOKS.add::<H>(Some(group.into()), Box::new(f));
    }

    /// Removes a [hook] group
    ///
    /// The hook can either be a string type, or a [`GroupId`].
    ///
    /// By removing the group, this function will remove all hooks
    /// added via [`hook::add_grouped`] with the same group.
    ///
    /// [hook]: Hookable
    /// [`hook::add_grouped`]: add_grouped
    pub fn remove(group: impl Into<InnerGroupId>) {
        HOOKS.remove(group.into());
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
    /// The hook can either be a string type, or a [`GroupId`].
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

/// A group can either be created through a name or through a
/// [`GroupId`]
#[derive(Clone, Debug, PartialEq, Eq)]
#[doc(hidden)]
pub enum InnerGroupId {
    Numbered(GroupId),
    Named(String),
}

impl From<GroupId> for InnerGroupId {
    fn from(value: GroupId) -> Self {
        Self::Numbered(value)
    }
}

impl<S: ToString> From<S> for InnerGroupId {
    fn from(value: S) -> Self {
        Self::Named(value.to_string())
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
/// - A [`UiBuilder`], which lets you push [`Widget`]s around this
///   one.
///
/// # Aliases
///
/// Since every [`Widget`] implements the [`HookAlias`] trait, instead
/// of writing this in the config crate:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<WidgetCreated<LineNumbers<Ui>>>(|pa, (ln, _)| ln.rel_abs());
/// }
/// ```
///
/// You can just write this:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<LineNumbers<Ui>>(|_, (ln, _)| ln.rel_abs());
/// }
/// ```
///
/// # Changing the layout
///
/// Assuming you are using `duat-term`, you could make it so every
/// [`LineNumbers`] comes with a [`VertRule`] on the right, like this:
///
/// ```rust
/// # use duat_core::prelude::{BuildInfo, PushSpecs};
/// # pub struct VertRule(Text);
/// # impl Widget<Ui> for VertRule {
/// #     type Cfg = VertRuleCfg;
/// #     fn update(_: &mut Pass, _: &Handle<Self>) {}
/// #     fn needs_update(&self, _: &Pass) -> bool { false }
/// #     fn cfg() -> Self::Cfg { VertRuleCfg }
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text { &mut self.0 }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # pub struct VertRuleCfg;
/// # impl VertRuleCfg {
/// #     pub fn on_the_right(self) -> Self { self }
/// # }
/// # impl WidgetCfg<Ui> for VertRuleCfg {
/// #     type Widget = VertRule;
/// #     fn build(self, _: &mut Pass, _: BuildInfo<Ui>) -> (Self::Widget, PushSpecs) {
/// #         (VertRule(Text::new()), PushSpecs::left())
/// #     }
/// # }
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<LineNumbers<Ui>>(|_, (ln, builder)| {
///         builder.push(VertRule::cfg().on_the_right());
///         ln
///     });
/// }
/// ```
///
/// Now, every time a [`LineNumbers`]s [`Widget`] is inserted in Duat,
/// a [`VertRule`] will be pushed on the right of it. You could even
/// further add a [hook] on [`VertRule`], that would push further
/// [`Widget`]s if you wanted to.
///
/// [cfg]: crate::ui::Widget::Cfg
/// [`WidgetCfg`]: crate::ui::WidgetCfg
/// [hook]: self
/// [direction]: crate::ui::PushSpecs
/// [`LineNumbers`]: https://docs.rs/duat_utils/latest/duat-utils/widgets/struct.LineNumbers.html
/// [`VertRule`]: https://docs.rs/duat_term/latest/duat-term/struct.VertRule.html
pub struct WidgetCreated<W: Widget<U>, U: Ui>(pub(crate) Handle<W, U>);

impl<W: Widget<U>, U: Ui> Hookable for WidgetCreated<W, U> {
    type Input<'h> = &'h Handle<W, U>;

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
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
/// [builder]: crate::ui::UiBuilder
/// [hook]: self
pub struct WindowCreated<U: Ui>(pub(crate) UiBuilder<U>);

impl<U: Ui> Hookable for WindowCreated<U> {
    type Input<'h> = &'h mut UiBuilder<U>;

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
/// [`FileClosed`].
pub struct FileClosed<U: Ui>(pub(crate) (Handle<File<U>, U>, Cache));

impl<U: Ui> Hookable for FileClosed<U> {
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
/// [`FileClosed`].
pub struct FileReloaded<U: Ui>(pub(crate) (Handle<File<U>, U>, Cache));

impl<U: Ui> Hookable for FileReloaded<U> {
    type Input<'h> = &'h (Handle<File<U>, U>, Cache);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is focused
///
/// # Arguments
///
/// - The [`Handle<dyn Widget>`] for the unfocused [`Widget`]
/// - The [`Handle<W>`] for the newly focused [`Widget`]
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
/// - The [`Handle<W>`] for the unfocused [`Widget`]
/// - The [`Handle<dyn Widget>`] for the newly focused [`Widget`]
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
/// # mod duat_kak {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone)]
/// #     pub struct Normal;
/// #     impl<U: Ui> Mode<U> for Normal {
/// #         type Widget = File<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use duat_kak::Normal;
///
/// fn setup() {
///     hook::add::<ModeCreated<Normal>>(|pa, (normal, handle)| normal);
/// }
/// ```
///
/// You can just write this:
///
/// ```rust
/// # mod duat_kak {
/// #     use duat_core::prelude::*;
/// #     #[derive(Clone)]
/// #     pub struct Normal;
/// #     impl<U: Ui> Mode<U> for Normal {
/// #         type Widget = File<U>;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {}
/// #     }
/// # }
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use duat_kak::Normal;
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
pub struct ModeCreated<M: Mode<U>, U: Ui>(pub(crate) (M, Handle<M::Widget, U>));

impl<M: Mode<U>, U: Ui> Hookable for ModeCreated<M, U> {
    type Input<'h> = (&'h mut M, &'h Handle<M::Widget, U>);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&mut self.0.0, &self.0.1)
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
pub struct KeysSentTo<M: Mode<U>, U: Ui>(pub(crate) (Vec<KeyEvent>, Handle<M::Widget, U>));

impl<M: Mode<U>, U: Ui> Hookable for KeysSentTo<M, U> {
    type Input<'h> = (&'h [KeyEvent], &'h Handle<M::Widget, U>);

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

/// [`Hookable`]: Triggers after [`File::save`] or [`File::save_to`]
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
/// [`File::save`]: crate::file::File::save
/// [`File::save_to`]: crate::file::File::save_to
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
    /// How to get the arguments from the [`Hookable`]
    fn get_input(&mut self) -> Self::Input<'_>;
}

/// Where all hooks of Duat are stored
#[derive(Clone, Copy)]
struct InnerHooks {
    types: &'static Mutex<HashMap<TypeId, Box<dyn HookHolder>>>,
    groups: &'static Mutex<Vec<InnerGroupId>>,
}

impl InnerHooks {
    /// Adds a hook for a [`Hookable`]
    fn add<H: Hookable>(
        &self,
        group_id: Option<InnerGroupId>,
        f: Box<dyn FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + 'static>,
    ) {
        let mut map = self.types.lock().unwrap();

        if let Some(group_id) = group_id.clone() {
            let mut groups = self.groups.lock().unwrap();
            if !groups.contains(&group_id) {
                groups.push(group_id)
            }
        }

        if let Some(holder) = map.get(&TypeId::of::<H>()) {
            let hooks_of = unsafe {
                let ptr = (&**holder as *const dyn HookHolder).cast::<HooksOf<H>>();
                ptr.as_ref().unwrap()
            };

            let mut hooks = hooks_of.0.borrow_mut();
            hooks.push((group_id, Box::leak(Box::new(RefCell::new(f)))));
        } else {
            let hooks_of = HooksOf::<H>(RefCell::new(vec![(
                group_id,
                Box::leak(Box::new(RefCell::new(f))),
            )]));

            map.insert(TypeId::of::<H>(), Box::new(hooks_of));
        }
    }

    /// Removes hooks with said group
    fn remove(&self, group_id: InnerGroupId) {
        self.groups.lock().unwrap().retain(|g| *g != group_id);
        let map = self.types.lock().unwrap();
        for holder in map.iter() {
            holder.1.remove(&group_id)
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

        for (group, hook) in hooks_of.0.borrow_mut().iter() {
            let input = hookable.get_input();
            if let Err(err) = hook.borrow_mut()(pa, input) {
                if let Some(InnerGroupId::Named(group)) = group {
                    crate::context::error!(target: group, "{err}");
                } else {
                    crate::context::error!(target: crate::utils::duat_name::<H>(), "{err}");
                }
            }
        }

        self.types
            .lock()
            .unwrap()
            .insert(TypeId::of::<H>(), unsafe {
                Box::from_raw(Box::into_raw(hooks_of) as *mut dyn HookHolder)
            });

        hookable
    }

    /// Checks if a hook group exists
    fn group_exists(&self, group: impl Into<InnerGroupId>) -> bool {
        self.groups.lock().unwrap().contains(&group.into())
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
    fn remove(&self, group_id: &InnerGroupId);
}

/// An intermediary struct, meant to hold the hooks of a [`Hookable`]
struct HooksOf<H: Hookable>(RefCell<Vec<(Option<InnerGroupId>, InnerHookFn<H>)>>);

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove(&self, group_id: &InnerGroupId) {
        let mut hooks = self.0.borrow_mut();
        hooks.retain(|(g, _)| g.as_ref().is_some_and(|g| g != group_id));
    }
}

type InnerHookFn<H> =
    &'static RefCell<dyn FnMut(&mut Pass, <H as Hookable>::Input<'_>) -> Result<(), Text>>;

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
/// # duat_core::doc_duat!(duat);
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
/// # duat_core::doc_duat!(duat);
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
    /// The actual [`Hookable`] that this [`HookAlias`] is supposed to
    /// map to
    type Hookable: for<'h> Hookable<Input<'h> = Self::Input<'h>>;
}

impl<H: Hookable, U: Ui> HookAlias<U> for H {
    type Hookable = Self;
    type Input<'h> = H::Input<'h>;
}

impl<W: Widget<U>, U: Ui> HookAlias<U, WidgetCreatedDummy<U>> for W {
    type Hookable = WidgetCreated<W, U>;
    type Input<'h> = <WidgetCreated<W, U> as Hookable>::Input<'h>;
}

impl<M: Mode<U>, U: Ui> HookAlias<U, ModeCreatedDummy<U>> for M {
    type Hookable = ModeCreated<M, U>;
    type Input<'h> = <ModeCreated<M, U> as Hookable>::Input<'h>;
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
/// # duat_core::doc_duat!(duat);
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
/// # duat_core::doc_duat!(duat);
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
