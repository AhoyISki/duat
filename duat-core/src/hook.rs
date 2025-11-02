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
//!     hook::add::<Buffer>(|pa: &mut Pass, handle: &Handle| {
//!         let buffer = handle.write(pa);
//!         if let Some("lisp") = buffer.filetype() {
//!             buffer.opts.dont_wrap = true;
//!         }
//!         Ok(())
//!     });
//! }
//! ```
//!
//! The hook above is triggered whenever a [`Buffer`] widget is
//! opened. Like every other hook, it gives you access to the global
//! state via the [`Pass`]. Additionally, like most hooks, it gives
//! you a relevant argument, in this case, a [`Handle<Buffer>`], which
//! you can modify to your liking.
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
//! - [`WidgetCreated`] triggers when a [`Widget`] is created, letting
//!   you change it, [`Widget`] can be used as its [alias]
//! - [`WindowCreated`], triggers when a [`Window`] is created,
//!   letting you change it.
//! - [`BufferWritten`] triggers after the [`Buffer`] is written.
//! - [`BufferClosed`] triggers on every buffer upon closing Duat.
//! - [`BufferReloaded`] triggers on every buffer upon reloading Duat.
//! - [`FocusedOn`] triggers when a [widget] is focused.
//! - [`UnfocusedFrom`] triggers when a [widget] is unfocused.
//! - [`FocusChanged`] is like [`FocusedOn`], but on [dyn `Widget`]s.
//! - [`KeysSent`] lets you act on a [dyn `Widget`], given a [key].
//! - [`KeysSentTo`] lets you act on a given [widget], given a [key].
//! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
//! - [`ModeSwitched`] triggers when you change [`Mode`].
//! - [`ModeCreated`] lets you act on a [`Mode`] after switching.
//! - [`SearchPerformed`] (from `duat`) triggers after a search is
//!   performed.
//! - [`SearchUpdated`] (from `duat`) triggers after a search updates.
//!
//! # Basic makeout
//!
//! When a hook is added, it can take arguments
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! use duat::prelude::*;
//!
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
//! Additionally, you may also trigger hooks "remotely". That is, if
//! you don't have acces to a [`Pass`] (due to not being on the main
//! thread or for some other reason), you may call [`hook::queue`]
//! rathe than [`hook::trigger`], in order to queue the hook to be
//! executed on the main thread:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! # use duat::prelude::*;
//! # struct CustomHook(usize);
//! # impl Hookable for CustomHook {
//! #     type Input<'h> = usize;
//! #     fn get_input(&mut self) -> Self::Input<'_> { self.0 }
//! # }
//! fn on_a_thread_far_far_away() {
//!     let arg = 42;
//!     hook::queue(CustomHook(arg));
//! }
//! ```
//!
//! The main difference (apart from the asynchronous execution) is
//! that [`hook::trigger`] _returns_ the hook to you, so you can
//! retrieve its internal values. This can be useful if, for example,
//! you wish to create a hook for configuring things:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! use duat::prelude::*;
//!
//! #[derive(Default)]
//! struct MyConfig {
//!     pub value_1: usize,
//!     pub value_2: Option<f32>,
//! }
//!
//! struct MyConfigCreated(MyConfig);
//!
//! impl Hookable for MyConfigCreated {
//!     type Input<'h> = &'h mut MyConfig;
//!
//!     fn get_input(&mut self) -> Self::Input<'_> {
//!         &mut self.0
//!     }
//! }
//!
//! fn create_my_config(pa: &mut Pass) -> MyConfig {
//!     let my_config = MyConfig::default();
//!     let MyConfigCreated(my_config) = hook::trigger(pa, MyConfigCreated(my_config));
//!     my_config
//! }
//! ```
//!
//! This way, the user can configure `MyConfig` by calling
//! [`hook::add`]:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! # #[derive(Default)]
//! # struct MyConfig {
//! #     pub value_1: usize,
//! #     pub value_2: Option<f32>,
//! # }
//! # struct MyConfigCreated(MyConfig);
//! # impl Hookable for MyConfigCreated {
//! #     type Input<'h> = &'h mut MyConfig;
//! #     fn get_input(&mut self) -> Self::Input<'_> { &mut self.0 }
//! # }
//! use duat::prelude::*;
//! setup_duat!(setup);
//!
//! fn setup() {
//!     hook::add::<MyConfigCreated>(|pa, my_config| {
//!         my_config.value_1 = 3;
//!         Ok(())
//!     });
//! }
//! ```
//!
//! [`Buffer`]: crate::buffer::Buffer
//! [alias]: HookAlias
//! [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
//! [widget]: Widget
//! [dyn `Widget`]: Widget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::cmd
//! [`Mode`]: crate::mode::Mode
//! [`&mut Widget`]: Widget
//! [`hook::queue`]: queue
//! [`hook::trigger`]: trigger
//! [`hook::add`]: add
//! [`SearchPerformed`]: https://docs.rs/duat/latest/duat/hooks/struct.SearchPerformed.html
//! [`SearchUpdated`]: https://docs.rs/duat/latest/duat/hooks/struct.SearchUpdated.html
use std::{any::TypeId, cell::RefCell, collections::HashMap, sync::Mutex};

pub use self::global::*;
use crate::{
    context::{Cache, Handle},
    data::Pass,
    form::{Form, FormId},
    mode::{KeyEvent, Mode},
    text::Text,
    ui::{Widget, Window},
};

/// Hook functions
mod global {
    use std::sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    };

    use super::{HookAlias, Hookable, InnerGroupId, InnerHooks};
    use crate::{data::Pass, session::DuatEvent, text::Text};

    static HOOKS: LazyLock<InnerHooks> = LazyLock::new(InnerHooks::default);

    /// A [`GroupId`] that can be used in order to remove hooks
    ///
    /// When [adding grouped hooks], you can either use strings or a
    /// [`GroupId`]. You should use strings for publicly removable
    /// hooks, and [`GroupId`]s for privately removable hooks:
    ///
    /// [adding grouped hooks]: HookBuilder::grouped
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

    /// A struct used in order to specify more options for [hook]s
    ///
    /// You can set three options currently:
    ///
    /// - [`HookBuilder::grouped`]: Groups this hook with others,
    ///   allowing them to all be [removed] at once.
    /// - [`HookBuilder::filter`]: Filters when this hook should be
    ///   called, by giving a struct for which  `H` implements
    ///   [`PartialEq`]. An example is the [`FocusedOn`] hook, which
    ///   accepts [`Handle`]s and [`Handle`] pairs.
    /// - [`HookBuilder::once`]: Calls the hook only once.
    ///
    /// [hook]: crate::hook
    /// [`FocusedOn`]: super::FocusedOn
    /// [`Handle`]: crate::context::Handle
    /// [removed]: remove
    pub struct HookBuilder<H: Hookable> {
        callback:
            Option<Box<dyn FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static>>,
        group: Option<InnerGroupId>,
        filter: Option<Box<dyn Fn(&H) -> bool + Send>>,
        once: bool,
    }

    impl<H: Hookable> HookBuilder<H> {
        /// Add a group to this hook
        ///
        /// This makes it so you can call [`hook::remove`] in order to
        /// remove this hook as well as every other hook added to the
        /// same group.
        ///
        /// There are two types of group, a private [`GroupId`] and
        /// [`impl ToString`] types, which can be removed by an end
        /// user.
        ///
        /// [`impl ToString`]: ToString
        /// [`hook::remove`]: super::remove
        pub fn grouped(mut self, group: impl Into<InnerGroupId>) -> Self {
            self.group = Some(group.into());
            self
        }

        /// Calls this hook only once
        ///
        /// This makes it so the hook will only be called once. Note
        /// that, if the hook is removed via [`hook::remove`] before
        /// being called, then it never will be.
        ///
        /// [`hook::remove`]: super::remove
        pub fn once(mut self) -> Self {
            self.once = true;
            self
        }

        /// Filter when this hook will be called
        ///
        /// This is mostly for convenience's sake, since you _could_
        /// just add a check inside of the callback itself.
        ///
        /// This is useful if for example, you want to trigger a hook
        /// on only some specific [`Handle`], or some [`Buffer`],
        /// things of the sort.
        ///
        /// [`Handle`]: crate::context::Handle
        /// [`Buffer`]: crate::buffer::Buffer
        pub fn filter<T: Send + 'static>(mut self, filter: T) -> Self
        where
            H: PartialEq<T>,
        {
            self.filter = Some(Box::new(move |hookable| *hookable == filter));
            self
        }
    }

    impl<H: Hookable> Drop for HookBuilder<H> {
        fn drop(&mut self) {
            HOOKS.add::<H>(
                self.callback.take().unwrap(),
                self.group.take(),
                self.filter.take(),
                self.once,
            )
        }
    }

    /// Adds a hook, which will be called whenever the [`Hookable`] is
    /// triggered
    ///
    /// [`hook::add`] will return a [`HookBuilder`], which is a struct
    /// that can be used to further modify the behaviour of the hook,
    /// and will add said hook when [`Drop`]ped.
    ///
    /// For example, [`HookBuilder::grouped`] will group this hook
    /// with others of the same group, while [`HookBuilder::once`]
    /// will make it so the hook is only called one time.
    ///
    /// [`hook::add`]: add
    #[inline(never)]
    pub fn add<H: HookAlias<impl std::any::Any>>(
        f: impl FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + Send + 'static,
    ) -> HookBuilder<H::Hookable> {
        HookBuilder {
            callback: Some(Box::new(f)),
            group: None,
            once: false,
            filter: None,
        }
    }

    /// Removes a [hook] group
    ///
    /// The hook can either be a string type, or a [`GroupId`].
    ///
    /// [hook]: Hookable
    pub fn remove(group: impl Into<InnerGroupId>) {
        HOOKS.remove(group.into());
    }

    /// Triggers a hooks for a [`Hookable`] struct
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
    /// [`HookBuilder::grouped`], and no [`hook::remove`]
    /// followed these additions
    ///
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

/// [`Hookable`]: Triggers when a [`Widget`] is created
///
/// # Arguments
///
/// - The [`Handle<W>`] of said `Widget`.
///
/// # Aliases
///
/// Since every `Widget` implements the `HookAlias` trait, instead
/// of writing this in the config crate:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<WidgetCreated<LineNumbers>>(|pa, ln| Ok(ln.write(pa).relative = true));
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
///     hook::add::<LineNumbers>(|pa, ln| Ok(ln.write(pa).relative = true));
/// }
/// ```
///
/// # Changing the layout
///
/// Assuming you are using `duat-term`, you could make it so every
/// [`LineNumbers`] comes with a [`VertRule`] on the right, like this:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     hook::add::<LineNumbers>(|pa, handle| {
///         VertRule::builder().on_the_right().push_on(pa, handle);
///         Ok(())
///     });
/// }
/// ```
///
/// Now, every time a [`LineNumbers`]s `Widget` is inserted in Duat,
/// a [`VertRule`] will be pushed on the right of it. You could even
/// further add a [hook] on `VertRule`, that would push further
/// `Widget`s if you wanted to.
///
/// [hook]: self
/// [direction]: crate::ui::PushSpecs
/// [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
/// [`VertRule`]: https://docs.rs/duat_term/latest/duat-term/struct.VertRule.html
pub struct WidgetCreated<W: Widget>(pub(crate) Handle<W>);

impl<W: Widget> Hookable for WidgetCreated<W> {
    type Input<'h> = &'h Handle<W>;

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when a new window is opened
///
/// # Arguments
///
/// - The [`Window`] that was created
///
/// One of the main reasons to use this [hook] is to push new
/// [`Widget`]s to a `Window`. That is, you can push [outer
/// `Widget`s] or [inner `Widget`s], just like with
/// [`Handle`]s.
///
/// Here's how that works: `Window`s are divided into two main
/// regions, the inner "[`Buffer`] region", and the outer "master
/// region". This means that, on every `Window`, you'll have a
/// collection of `Buffer`s in the middle, with their satellite
/// `Widget`s, and various `Widget`s on the outer rims of the
/// `Window`, not necessarily associated with any single `Buffer`.
///
/// As an example, this is how the default layout of Duat is layed
/// out:
///
/// ```text
/// ╭┄┄┬┄┄┄┄┄┄┄┄┬┄┄┬┄┄┄┄┄┄┄┄┬───────╮
/// ┊  │        │  │        ┊       │
/// ┊LN│        │LN│        ┊       │
/// ┊  │ Buffer │  │ Buffer ┊       │
/// ┊VR│        │VR│        ┊LogBook│
/// ┊  │        │  │        ┊       │
/// ├┄┄┴┄┄┄┄┄┄┄┄┴┄┄┴┄┄┄┄┄┄┄┄┤       │
/// │     FooterWidgets     │       │
/// ╰───────────────────────┴───────╯
/// ```
///
/// In this configuration, you can see the delineation between the
/// "`Buffer` region" (surrounded by dotted lines) and the "master
/// region", where:
///
/// - For each [`Buffer`], we are adding a [`LineNumbers`] (LN) and a
///   [`VertRule`] (VR) `Widget`s. Each of these is related to a
///   specific `Buffer`, and if that `Buffer` moves around, they will
///   follow.
///
/// - On the outer edges, we have a [`FooterWidgets`], which includes
///   a [`StatusLine`], [`PromptLine`] and [`Notifications`], as well
///   as a [`LogBook`] on the side, which is hidden by default. These
///   [`Widget`]s are not related to any `Buffer`, and will not move
///   around or be removed, unless directly.
///
/// So the distinction here is that, if you call
/// [`Window::push_inner`], you will be pushing [`Widget`]s _around_
/// the "`Buffer` region", but _not_ within it. If you want to push to
/// specific [`Buffer`]s, you should look at
/// [`Handle::push_inner_widget`] and [`Handle::push_outer_widget`].
///
/// On the other hand, by calling [`Window::push_outer`], you will be
/// pushing [`Widget`]s around the "master region", so they will go on
/// the edges of the screen.
///
/// [hook]: self
/// [outer `Widget`s]: Window::push_outer
/// [inner `Widget`s]: Window::push_inner
/// [`Buffer`]: crate::buffer::Buffer
/// [`LineNumbers`]: https://docs.rs/duat/duat/latest/widgets/struct.LineNumbers.html
/// [`VertRule`]: https://docs.rs/duat/duat/latest/widgets/struct.VertRule.html
/// [`FooterWidgets`]: https://docs.rs/duat/duat/latest/widgets/struct.FooterWidgets.html
/// [`StatusLine`]: https://docs.rs/duat/duat/latest/widgets/struct.StatusLine.html
/// [`PromptLine`]: https://docs.rs/duat/duat/latest/widgets/struct.PromptLine.html
/// [`Notifications`]: https://docs.rs/duat/duat/latest/widgets/struct.Notifications.html
/// [`LogBook`]: https://docs.rs/duat/duat/latest/widgets/struct.LogBook.html
pub struct WindowCreated(pub(crate) Window);

impl Hookable for WindowCreated {
    type Input<'h> = &'h mut Window;

    fn get_input(&mut self) -> Self::Input<'_> {
        &mut self.0
    }
}

/// [`Hookable`]: Triggers before closing a [`Buffer`]
///
/// # Arguments
///
/// - The [`Buffer`]'s [`Handle`].
/// - A [`Cache`]. This can be used in order to decide wether or not
///   some things will be reloaded on the next opening of Duat.
///
/// This will not trigger upon reloading Duat. For that, see
/// [`BufferClosed`].
///
/// [`Buffer`]: crate::buffer::Buffer
pub struct BufferClosed(pub(crate) (Handle, Cache));

impl Hookable for BufferClosed {
    type Input<'h> = &'h (Handle, Cache);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers before reloading a [`Buffer`]
///
/// # Arguments
///
/// - The [`Buffer`]'s [`Handle`].
/// - A [`Cache`]. This can be used in order to decide wether or not
///   some things will be reloaded on the next opening of Duat.
///
/// This will not trigger upon closing Duat. For that, see
/// [`BufferClosed`].
///
/// [`Buffer`]: crate::buffer::Buffer
pub struct BufferReloaded(pub(crate) (Handle, Cache));

impl Hookable for BufferReloaded {
    type Input<'h> = &'h (Handle, Cache);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is focused
///
/// # Arguments
///
/// - The [`Handle<dyn Widget>`] for the unfocused `Widget`.
/// - The [`Handle<W>`] for the newly focused `Widget`.
///
/// # Filters
///
/// This `Hookable` can be filtered in two ways
///
/// - By a focused [`Handle<_>`].
/// - By a `(Handle<_>, Handle<_>)` pair.
pub struct FocusedOn<W: Widget>(pub(crate) (Handle<dyn Widget>, Handle<W>));

impl<W: Widget> Hookable for FocusedOn<W> {
    type Input<'h> = &'h (Handle<dyn Widget>, Handle<W>);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

impl<W1: Widget, W2: Widget + ?Sized> PartialEq<Handle<W2>> for FocusedOn<W1> {
    fn eq(&self, other: &Handle<W2>) -> bool {
        self.0.1 == *other
    }
}

impl<W1: Widget, W2: Widget + ?Sized, W3: Widget + ?Sized> PartialEq<(Handle<W2>, Handle<W3>)>
    for FocusedOn<W1>
{
    fn eq(&self, other: &(Handle<W2>, Handle<W3>)) -> bool {
        self.0.0 == other.0 && self.0.1 == other.1
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is unfocused
///
/// # Arguments
///
/// - The [`Handle<W>`] for the unfocused `Widget`
/// - The [`Handle<dyn Widget>`] for the newly focused `Widget`
pub struct UnfocusedFrom<W: Widget>(pub(crate) (Handle<W>, Handle<dyn Widget>));

impl<W: Widget> Hookable for UnfocusedFrom<W> {
    type Input<'h> = &'h (Handle<W>, Handle<dyn Widget>);

    fn get_input(&mut self) -> Self::Input<'_> {
        &self.0
    }
}

impl<W1: Widget, W2: Widget + ?Sized> PartialEq<Handle<W2>> for UnfocusedFrom<W1> {
    fn eq(&self, other: &Handle<W2>) -> bool {
        self.0.0 == *other
    }
}

impl<W1: Widget, W2: Widget + ?Sized, W3: Widget + ?Sized> PartialEq<(Handle<W2>, Handle<W3>)>
    for UnfocusedFrom<W1>
{
    fn eq(&self, other: &(Handle<W2>, Handle<W3>)) -> bool {
        self.0.0 == other.0 && self.0.1 == other.1
    }
}

/// [`Hookable`]: Triggers when focus changes between two [`Widget`]s
///
/// # Arguments
///
/// - The [`Handle<dyn Widget>`] for the unfocused `Widget`
/// - The [`Handle<dyn Widget>`] for the newly focused `Widget`
///
/// This `Hookable` is triggered _before_ [`FocusedOn`] and
/// [`UnfocusedFrom`] are triggered.
pub struct FocusChanged(pub(crate) (Handle<dyn Widget>, Handle<dyn Widget>));

impl Hookable for FocusChanged {
    type Input<'h> = &'h (Handle<dyn Widget>, Handle<dyn Widget>);

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
/// like the language of a [`Buffer`].
///
/// # Aliases
///
/// Since every `Mode` implements the `HookAlias` trait, given a
/// `duat_kak` plugin imported as `kak`, instead of writing this:
///
/// ```rust
/// # mod duat_kak {
/// #     use duat_core::{buffer::Buffer, context::Handle, data::Pass, mode::{Mode, KeyEvent}};
/// #     #[derive(Clone)]
/// #     pub struct Normal {
/// #         pub indent_on_capital_i: bool
/// #     }
/// #     impl Mode for Normal {
/// #         type Widget = Buffer;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget>) {}
/// #     }
/// # }
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use duat_kak::Normal;
///
/// fn setup() {
///     hook::add::<ModeCreated<Normal>>(|pa, (normal, handle)|
///         Ok(normal.indent_on_capital_i = true)
///     );
/// }
/// ```
///
/// You can just write this:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # mod duat_kak {
/// #     use duat_core::{buffer::Buffer, context::Handle, data::Pass, mode::{Mode, KeyEvent}};
/// #     #[derive(Clone)]
/// #     pub struct Normal {
/// #         pub indent_on_capital_i: bool
/// #     }
/// #     impl Mode for Normal {
/// #         type Widget = Buffer;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget>) {}
/// #     }
/// # }
/// setup_duat!(setup);
/// use duat::prelude::*;
/// use duat_kak::Normal;
///
/// fn setup() {
///     hook::add::<Normal>(|pa, (normal, handle)|
///         Ok(normal.indent_on_capital_i = true)
///     );
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
/// [`Buffer`]: crate::buffer::Buffer
pub struct ModeCreated<M: Mode>(pub(crate) (M, Handle<M::Widget>));

impl<M: Mode> Hookable for ModeCreated<M> {
    type Input<'h> = (&'h mut M, &'h Handle<M::Widget>);

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
pub struct KeysSentTo<M: Mode>(pub(crate) (Vec<KeyEvent>, Handle<M::Widget>));

impl<M: Mode> Hookable for KeysSentTo<M> {
    type Input<'h> = (&'h [KeyEvent], &'h Handle<M::Widget>);

    fn get_input(&mut self) -> Self::Input<'_> {
        (&self.0.0, &self.0.1)
    }
}

/// [`Hookable`]: Triggers whenever a [`Form`] is set
///
/// This can be a creation or alteration of a `Form`.
/// If the `Form` is a reference to another, the reference's
/// `Form` will be returned instead.
///
/// # Arguments
///
/// - The `Form`'s name.
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
/// `ColorScheme` is done with its changes.
///
/// # Arguments
///
/// - The name of the `ColorScheme`
///
/// [`ColorScheme`]: crate::form::ColorScheme
pub struct ColorSchemeSet(pub(crate) &'static str);

impl Hookable for ColorSchemeSet {
    type Input<'h> = &'static str;

    fn get_input(&mut self) -> Self::Input<'_> {
        self.0
    }
}

/// [`Hookable`]: Triggers after [`Buffer::save`] or [`Buffer::save_to`]
///
/// Only triggers if the buffer was actually updated.
///
/// # Arguments
///
/// - The path of the buffer
/// - The number of bytes written to said buffer
/// - Wether Duat is in the process of quitting (happens when calling
///   the `wq` or `waq` commands)
///
/// [`Buffer::save`]: crate::buffer::Buffer::save
/// [`Buffer::save_to`]: crate::buffer::Buffer::save_to
pub struct BufferWritten(pub(crate) (String, usize, bool));

impl Hookable for BufferWritten {
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
pub trait Hookable: Sized + 'static {
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
        callback: Box<dyn FnMut(&mut Pass, H::Input<'_>) -> Result<(), Text> + 'static>,
        group: Option<InnerGroupId>,
        filter: Option<Box<dyn Fn(&H) -> bool + Send + 'static>>,
        once: bool,
    ) {
        let mut map = self.types.lock().unwrap();

        if let Some(group_id) = group.clone() {
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
            hooks.push(Hook {
                callback: Box::leak(Box::new(RefCell::new(callback))),
                group,
                filter,
                once,
            });
        } else {
            let hooks_of = HooksOf::<H>(RefCell::new(vec![Hook {
                callback: Box::leak(Box::new(RefCell::new(callback))),
                group,
                filter,
                once,
            }]));

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

        hooks_of.0.borrow_mut().retain_mut(|hook| {
            if let Some(filter) = hook.filter.as_ref()
                && !filter(&hookable)
            {
                return true;
            }

            let input = hookable.get_input();
            if let Err(err) = hook.callback.borrow_mut()(pa, input) {
                if let Some(InnerGroupId::Named(group)) = hook.group.as_ref() {
                    crate::context::error!(target: group, "{err}");
                } else {
                    crate::context::error!(target: crate::utils::duat_name::<H>(), "{err}");
                }
            }

            !hook.once
        });

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
struct HooksOf<H: Hookable>(RefCell<Vec<Hook<H>>>);

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove(&self, group_id: &InnerGroupId) {
        let mut hooks = self.0.borrow_mut();
        hooks.retain(|hook| hook.group.as_ref().is_none_or(|g| g != group_id));
    }
}

struct Hook<H: Hookable> {
    callback:
        &'static RefCell<dyn FnMut(&mut Pass, <H as Hookable>::Input<'_>) -> Result<(), Text>>,
    group: Option<InnerGroupId>,
    filter: Option<Box<dyn Fn(&H) -> bool + Send + 'static>>,
    once: bool,
}

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
/// # duat_core::doc_duat!(duat);
/// use duat::prelude::*;
///
/// pub struct CreatedStruct<T: 'static>(T);
///
/// impl<T: 'static> Hookable for CreatedStruct<T> {
///     type Input<'h> = &'h mut T;
///
///     fn get_input(&mut self) -> Self::Input<'_> {
///         &mut self.0
///     }
/// }
///
/// struct MyStructWithAVeryLongName {
///     pub option_1: bool,
/// };
///
/// // In the user's config crate:
/// # {
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     // This is way too long
///     hook::add::<CreatedStruct<MyStructWithAVeryLongName>>(|_, arg| Ok(arg.option_1 = true));
/// }
/// # }
/// ```
///
/// In this case, you can do this instead:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use duat::prelude::*;
/// use hook::HookAlias;
///
/// pub struct CreatedStruct<T: 'static>(T);
///
/// impl<T: 'static> Hookable for CreatedStruct<T> {
///     type Input<'h> = &'h mut T;
///
///     fn get_input(&mut self) -> Self::Input<'_> {
///         &mut self.0
///     }
/// }
///
/// struct MyStructWithAVeryLongName {
///     pub option_1: bool,
/// };
///
/// struct MyDummy;
///
/// impl HookAlias<MyDummy> for MyStructWithAVeryLongName {
///     type Hookable = CreatedStruct<MyStructWithAVeryLongName>;
///     type Input<'h> = <Self::Hookable as Hookable>::Input<'h>;
/// }
///
/// // In the user's config crate:
/// # {
/// setup_duat!(setup);
/// use duat::prelude::*;
///
/// fn setup() {
///     // Much better
///     hook::add::<MyStructWithAVeryLongName>(|_, arg| Ok(arg.option_1 = true));
/// }
/// # }
/// ```
pub trait HookAlias<D: std::any::Any = NormalHook> {
    /// Just a shorthand for less boilerplate in the function
    /// definition
    type Input<'h>;
    /// The actual [`Hookable`] that this [`HookAlias`] is supposed to
    /// map to
    type Hookable: for<'h> Hookable<Input<'h> = Self::Input<'h>>;
}

impl<H: Hookable> HookAlias for H {
    type Hookable = Self;
    type Input<'h> = H::Input<'h>;
}

impl<W: Widget> HookAlias<WidgetCreatedDummy> for W {
    type Hookable = WidgetCreated<W>;
    type Input<'h> = <WidgetCreated<W> as Hookable>::Input<'h>;
}

impl<M: Mode> HookAlias<ModeCreatedDummy> for M {
    type Hookable = ModeCreated<M>;
    type Input<'h> = <ModeCreated<M> as Hookable>::Input<'h>;
}

/// For specialization purposes
#[doc(hidden)]
pub struct NormalHook;

/// For specialization purposes
#[doc(hidden)]
pub struct WidgetCreatedDummy;

/// For specialization purposes
#[doc(hidden)]
pub struct ModeCreatedDummy;
