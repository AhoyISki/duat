//! Utilities for hooks in Duat.
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
//!     hook::add::<BufferOpened>(|pa: &mut Pass, handle: &Handle| {
//!         let buffer = handle.write(pa);
//!         if let Some("lisp") = buffer.filetype() {
//!             buffer.opts.wrap_lines = true;
//!         }
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
//! - [`BufferOpened`] is an alias for [`WidgetOpened<Buffer>`].
//! - [`BufferSaved`] triggers after the [`Buffer`] is written.
//! - [`BufferClosed`] triggers on every buffer upon closing Duat.
//! - [`BufferUnloaded`] triggers on every buffer upon reloading Duat.
//! - [`BufferUpdated`] triggers whenever a buffer changes.
//! - [`BufferPrinted`] triggers after a buffer has been printed.
//! - [`BufferSwitched`] triggers when switching the active buffer.
//! - [`ConfigLoaded`] triggers after loading the config crate.
//! - [`ConfigUnloaded`] triggers after unloading the config crate.
//! - [`ExitedDuat`] triggers after Duat has exited.
//! - [`FocusedOnDuat`] triggers when Duat gains focus.
//! - [`UnfocusedFromDuat`] triggers when Duat loses focus.
//! - [`WidgetOpened`] triggers when a [`Widget`] is opened.
//! - [`WindowOpened`] triggers when a [`Window`] is created.
//! - [`FocusedOn`] triggers when a [widget] is focused.
//! - [`UnfocusedFrom`] triggers when a [widget] is unfocused.
//! - [`FocusChanged`] is like [`FocusedOn`], but on [dyn `Widget`]s.
//! - [`KeySent`] triggers when a keys are sent.
//! - [`KeySentTo`] same, but on a specific [widget].
//! - [`KeyTyped`] triggers when keys are _typed_, not _sent_.
//! - [`OnMouseEvent`] triggers with mouse events.
//! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
//! - [`ModeSwitched`] triggers when you change [`Mode`].
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
//!     fn get_input<'h>(&'h mut self, pa: &mut Pass) -> Self::Input<'h> {
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
//! Sometimes, you may need to trigger hooks "remotely", that is, in a
//! place wher you don't have acces to a [`Pass`] (due to not being on
//! the main thread or for some other reason), you can make use of
//! [`context::queue`] in order to queue the hook to be executed on
//! the main thread:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! # use duat::prelude::*;
//! # struct CustomHook(usize);
//! # impl Hookable for CustomHook {
//! #     type Input<'h> = usize;
//! #     fn get_input<'h>(&'h mut self, pa: &mut Pass) -> Self::Input<'h> { self.0 }
//! # }
//! fn on_a_thread_far_far_away() {
//!     let arg = 42;
//!     context::queue(move |pa| _ = hook::trigger(pa, CustomHook(arg)));
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
//!     fn get_input<'h>(&'h mut self, pa: &mut Pass) -> Self::Input<'h> {
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
//! #     fn get_input<'h>(&'h mut self, pa: &mut Pass) -> Self::Input<'h> { &mut self.0 }
//! # }
//! use duat::prelude::*;
//! setup_duat!(setup);
//!
//! fn setup() {
//!     hook::add::<MyConfigCreated>(|pa, my_config| my_config.value_1 = 3);
//! }
//! ```
//!
//! [`Buffer`]: crate::buffer::Buffer
//! [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
//! [widget]: Widget
//! [dyn `Widget`]: Widget
//! [key]: KeyEvent
//! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
//! [commands]: crate::cmd
//! [`Mode`]: crate::mode::Mode
//! [`&mut Widget`]: Widget
//! [`context::queue`]: crate::context::queue
//! [`hook::trigger`]: trigger
//! [`hook::add`]: add
//! [`SearchPerformed`]: https://docs.rs/duat/latest/duat/hooks/struct.SearchPerformed.html
//! [`SearchUpdated`]: https://docs.rs/duat/latest/duat/hooks/struct.SearchUpdated.html
use std::{any::TypeId, cell::RefCell, collections::HashMap, sync::Mutex};

use crossterm::event::MouseEventKind;

pub use self::global::*;
use crate::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    form::{Form, FormId},
    mode::{KeyEvent, Mode, MouseEvent},
    ui::{Widget, Window},
    utils::catch_panic,
};

/// Hook functions.
mod global {
    use std::sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    };

    use super::{Hookable, InnerGroupId, InnerHooks};
    use crate::{data::Pass, hook::Callback};

    static HOOKS: LazyLock<InnerHooks> = LazyLock::new(InnerHooks::default);

    /// A [`GroupId`] that can be used in order to remove hooks.
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

        /// Remove all hooks that belong to this [`GroupId`].
        ///
        /// This can be used in order to have hooks remove themselves,
        /// for example.
        pub fn remove(self) {
            remove(self)
        }

        /// Returns `true` if this `GroupId` has any hooks.
        pub fn has_hooks(self) -> bool {
            group_exists(self)
        }
    }

    /// A struct used in order to specify more options for [hook]s.
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
        callback: Option<Callback<H>>,
        group: Option<InnerGroupId>,
        filter: Option<Box<dyn Fn(&H) -> bool + Send>>,
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

        /// Filter when this hook will be called.
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
            HOOKS.add(
                self.callback.take().unwrap(),
                self.group.take(),
                self.filter.take(),
            )
        }
    }

    /// Adds a hook, which will be called whenever the [`Hookable`] is
    /// triggered.
    ///
    /// [`hook::add`] will return a [`HookBuilder`], which is a struct
    /// that can be used to further modify the behaviour of the hook,
    /// and will add said hook when [`Drop`]ped.
    ///
    /// You can call [`HookBuilder::grouped`], which will group this
    /// hook with others of the same group, allowing for
    /// [`hook::remove`] to remove many hooks a once.
    ///
    /// You can also call [`HookBuilder::filter`], which lets you
    /// filter when your function is actually called, based on the
    /// input arguments of `H`.
    ///
    /// If you want to call a function only once, check out
    /// [`hook::add_once`], which takes an [`FnOnce`] as input, rather
    /// than an [`FnMut`].
    ///
    /// [`hook::add`]: add
    /// [`hook::add_once`]: add_once
    /// [`hook::remove`]: remove
    #[inline(never)]
    pub fn add<H: Hookable>(
        f: impl FnMut(&mut Pass, H::Input<'_>) + Send + 'static,
    ) -> HookBuilder<H> {
        HookBuilder {
            callback: Some(Callback::FnMut(Box::new(f))),
            group: None,
            filter: None,
        }
    }

    /// Adds a hook, which will be called once when the [`Hookable`]
    /// is triggered.
    ///
    /// This is in contrast to [`hook::add`], whose function is called
    /// every time the `Hookable` is triggered, the function passed to
    /// `add_once` will only be triggered one time.
    ///
    /// [`hook::add`] will return a [`HookBuilder`], which is a struct
    /// that can be used to further modify the behaviour of the hook,
    /// and will add said hook when [`Drop`]ped.
    ///
    /// You can call [`HookBuilder::grouped`], which will group this
    /// hook with others of the same group, allowing for
    /// [`hook::remove`] to remove many hooks a once.
    ///
    /// You can also call [`HookBuilder::filter`], which lets you
    /// filter when your function is actually called, based on the
    /// input arguments of `H`. This is especially useful with
    /// `add_once`, since it prevents the function from being called
    /// once with the wrong arguments (e.g., it was meant for a
    /// specific [`Buffer`], but the trigger happened first on another).
    ///
    /// [`hook::add`]: add
    /// [`hook::remove`]: remove
    /// [`Buffer`]: crate::buffer::Buffer
    #[inline(never)]
    pub fn add_once<H: Hookable>(
        f: impl FnOnce(&mut Pass, H::Input<'_>) + Send + 'static,
    ) -> HookBuilder<H> {
        HookBuilder {
            callback: Some(Callback::FnOnce(Some(Box::new(f)))),
            group: None,
            filter: None,
        }
    }

    /// Removes a [hook] group.
    ///
    /// The hook can either be a string type, or a [`GroupId`].
    ///
    /// [hook]: Hookable
    pub fn remove(group: impl Into<InnerGroupId>) {
        HOOKS.remove(group.into());
    }

    /// Triggers a hooks for a [`Hookable`] struct.
    pub fn trigger<H: Hookable>(pa: &mut Pass, hookable: H) -> H {
        HOOKS.trigger(pa, hookable)
    }

    /// Checks if a give group exists.
    ///
    /// The hook can either be a string type, or a [`GroupId`].
    ///
    /// Returns `true` if said group was added via
    /// [`HookBuilder::grouped`], and no [`hook::remove`]
    /// followed these additions
    ///
    /// [`hook::remove`]: remove
    pub fn group_exists(group: impl Into<InnerGroupId>) -> bool {
        HOOKS.group_exists(group.into())
    }
}

/// A group can either be created through a name or through a
/// [`GroupId`].
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

/// [`Hookable`]: Triggers when Duat opens or reloads.
///
/// This trigger will also happen after a few other initial setups of
/// Duat.
///
/// There are no arguments
pub struct ConfigLoaded(pub(crate) ());

impl Hookable for ConfigLoaded {
    type Input<'h> = ();

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {}
}

/// [`Hookable`]: Triggers when Duat closes or has to reload.
///
/// There are no arguments
pub struct ConfigUnloaded(pub(crate) ());

impl Hookable for ConfigUnloaded {
    type Input<'h> = ();

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {}
}

/// [`Hookable`]: Triggers when Duat closes.
///
/// There are no arguments
pub struct ExitedDuat(pub(crate) ());

impl Hookable for ExitedDuat {
    type Input<'h> = ();

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {}
}

/// [`Hookable`]: Triggers when Duat is refocused.
///
/// # Arguments
///
/// There are no arguments
pub struct FocusedOnDuat(pub(crate) ());

impl Hookable for FocusedOnDuat {
    type Input<'h> = ();

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {}
}

/// [`Hookable`]: Triggers when Duat is unfocused.
///
/// # Arguments
///
/// There are no arguments
pub struct UnfocusedFromDuat(pub(crate) ());

impl Hookable for UnfocusedFromDuat {
    type Input<'h> = ();

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {}
}

/// [`Hookable`]: Triggers when a [`Widget`] is created.
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
///     hook::add::<WidgetOpened<LineNumbers>>(|pa, ln| ln.write(pa).relative = true);
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
///     hook::add::<WidgetOpened<LineNumbers>>(|pa, ln| ln.write(pa).relative = true);
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
///     hook::add::<WidgetOpened<LineNumbers>>(|pa, handle| {
///         VertRule::builder().on_the_right().push_on(pa, handle);
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
pub struct WidgetOpened<W: Widget>(pub(crate) Handle<W>);

impl<W: Widget> Hookable for WidgetOpened<W> {
    type Input<'h> = &'h Handle<W>;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}

/// An alias for [`WidgetOpened<Buffer>`].
pub type BufferOpened = WidgetOpened<Buffer>;

/// [`Hookable`]: Triggers when a new window is opened.
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
/// [`LineNumbers`]: https://docs.rs/duat/duat/latest/widgets/struct.LineNumbers.html
/// [`VertRule`]: https://docs.rs/duat/duat/latest/widgets/struct.VertRule.html
/// [`FooterWidgets`]: https://docs.rs/duat/duat/latest/widgets/struct.FooterWidgets.html
/// [`StatusLine`]: https://docs.rs/duat/duat/latest/widgets/struct.StatusLine.html
/// [`PromptLine`]: https://docs.rs/duat/duat/latest/widgets/struct.PromptLine.html
/// [`Notifications`]: https://docs.rs/duat/duat/latest/widgets/struct.Notifications.html
/// [`LogBook`]: https://docs.rs/duat/duat/latest/widgets/struct.LogBook.html
pub struct WindowOpened(pub(crate) Window);

impl Hookable for WindowOpened {
    type Input<'h> = &'h mut Window;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &mut self.0
    }
}

/// [`Hookable`]: Triggers before closing a [`Buffer`].
///
/// # Arguments
///
/// - The [`Buffer`]'s [`Handle`].
pub struct BufferClosed(pub(crate) Handle);

impl Hookable for BufferClosed {
    type Input<'h> = &'h Handle;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}

impl PartialEq<Handle> for BufferClosed {
    fn eq(&self, other: &Handle) -> bool {
        self.0 == *other
    }
}

/// [`Hookable`]: Triggers before unloading a [`Buffer`].
///
/// # Arguments
///
/// - The [`Buffer`]'s [`Handle`].
///
/// This will not trigger upon closing Duat. For that, see
/// [`BufferClosed`].
pub struct BufferUnloaded(pub(crate) Handle);

impl Hookable for BufferUnloaded {
    type Input<'h> = &'h Handle;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}

impl PartialEq<Handle> for BufferUnloaded {
    fn eq(&self, other: &Handle) -> bool {
        self.0 == *other
    }
}

/// [`Hookable`]: Triggers when a [`Buffer`] updates.
///
/// This is triggered after a batch of writing calls to the `Buffer`,
/// once per frame. This can happen after typing a key, calling a
/// command, triggering hooks, or any other action with access to a
/// [`Pass`], which could be used to write to the `Buffer`.
///
/// Think of this is as a "last pass" on the `Buffer`, right before
/// printing, where it can be adjusted given the modifications to it,
/// like [`Change`]s and such.
///
/// As an example, here's a hook that will highlight every non ascii
/// character:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use duat::{
///     prelude::*,
///     text::{Bytes, Tags},
/// };
///
/// static TRACKER: BufferTracker = BufferTracker::new();
///
/// fn setup() {
///     let tagger = Tagger::new();
///     let tag = form::id_of!("non_ascii_char").to_tag(50);
///
///     let hl_non_ascii = move |tags: &mut Tags, bytes: &Bytes, range: Range<Point>| {
///         for (b, char) in bytes[range.clone()].char_indices() {
///             let b = b + range.start.byte();
///             if !char.is_ascii() {
///                 tags.insert(tagger, b..b + char.len_utf8(), tag);
///             }
///         }
///     };
///
///     hook::add::<BufferOpened>(move |pa, handle| {
///         TRACKER.register_buffer(handle.write(pa));
///
///         let mut parts = handle.text_parts(pa);
///         let range = Point::default()..parts.bytes.len();
///         hl_non_ascii(&mut parts.tags, parts.bytes, range);
///     });
///
///     hook::add::<BufferUpdated>(move |pa, handle| {
///         let mut parts = TRACKER.parts(handle.write(pa)).unwrap();
///
///         for change in parts.changes {
///             parts.tags.remove(tagger, change.added_range());
///             hl_non_ascii(&mut parts.tags, parts.bytes, change.added_range())
///         }
///     });
/// }
/// ```
///
/// The [`BufferTracker`] will keep track of each registered
/// [`Buffer`], telling you about every new [`Change`] that took place
/// since the last call to [`BufferTracker::parts`]. The
/// `BufferTracker::parts` function works much like [`Text::parts`],
/// by separating the [`Bytes`], [`Tags`] and [`Selections`], letting
/// you modify the tags, without permitting further edits to the
/// `Text`.
///
/// This is a nice way to automatically keep track of the changes, and
/// it will work even if the function isn't called frequently.
///
/// # Arguments
///
/// - The [`Buffer`]'s [`Handle`]
///
/// [`Area`]: crate::ui::Area
/// [`Buffer`]: crate::buffer::Buffer
/// [`PrintOpts`]: crate::opts::PrintOpts
/// [`Change`]: crate::buffer::Change
/// [`Cursor`]: crate::mode::Cursor
/// [`Tag`]: crate::text::Tag
/// [`Bytes`]: crate::text::Bytes
/// [`Tags`]: crate::text::Tags
/// [`Selections`]: crate::mode::Selections
/// [`Text`]: crate::text::Text
/// [`Text::parts`]: crate::text::Text::parts
/// [`Text::replace_range`]: crate::text::Text::replace_range
/// [`BufferTracker`]: crate::buffer::BufferTracker
/// [`BufferTracker::parts`]: crate::buffer::BufferTracker::parts
pub struct BufferUpdated(pub(crate) Handle);

impl Hookable for BufferUpdated {
    type Input<'h> = &'h Handle;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}

impl PartialEq<Handle> for BufferUpdated {
    fn eq(&self, other: &Handle) -> bool {
        self.0 == *other
    }
}

/// [`Hookable`]: Triggers after a [`Buffer`] is printed.
///
/// The primary purpose of this `Widget` is to do cleanup on temporary
/// changes made during a `BufferUpdated` triggering.
///
/// One example of this is with the default `BufferOpts`, which allow
/// you to hightlight the current cursor line. Since this makes use of
/// disruptive `Tag`s, it is best to do this only during the printing
/// process, then get rid of said tags.
///
/// # Warning
///
/// Any changes done to the [`Buffer`] or [`Area`] from this hook will
/// _not_ be checked in order for a reprint. This is to avoid
/// repeatedly printing over and over again.
///
/// [`Area`]: crate::ui::Area
pub struct BufferPrinted(pub(crate) Handle);

impl Hookable for BufferPrinted {
    type Input<'h> = &'h Handle;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}

/// [`Hookable`]: Triggers whenever the active [`Buffer`] changes.
///
/// # Arguments
///
/// - The former `Buffer`'s [`Handle`]
/// - The current `Buffer`'s [`Handle`]
///
/// [`Buffer`]: crate::buffer::Buffer
pub struct BufferSwitched(pub(crate) (Handle, Handle));

impl Hookable for BufferSwitched {
    type Input<'h> = (&'h Handle, &'h Handle);

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        (&self.0.0, &self.0.1)
    }
}

/// [`Hookable`]: Triggers when the [`Widget`] is focused.
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

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
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

/// [`Hookable`]: Triggers when the [`Widget`] is unfocused.
///
/// # Arguments
///
/// - The [`Handle<W>`] for the unfocused `Widget`
/// - The [`Handle<dyn Widget>`] for the newly focused `Widget`
pub struct UnfocusedFrom<W: Widget>(pub(crate) (Handle<W>, Handle<dyn Widget>));

impl<W: Widget> Hookable for UnfocusedFrom<W> {
    type Input<'h> = &'h (Handle<W>, Handle<dyn Widget>);

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
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

/// [`Hookable`]: Triggers when focus changes between two [`Widget`]s.
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

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}

/// [`Hookable`]: Triggers when the [`Mode`] is changed.
///
/// # Arguments
///
/// - The previous mode.
/// - The current mode.
pub struct ModeSwitched(pub(crate) (&'static str, &'static str));

impl Hookable for ModeSwitched {
    type Input<'h> = (&'static str, &'static str);

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        self.0
    }
}

/// [`Hookable`]: Triggers whenever a [key] is sent.
///
/// [`KeyEvent`]s are "sent" when you type [unmapped] keys _or_ with
/// the keys that were mapped, this is in contrast with [`KeyTyped`],
/// which triggers when you type or when calling [`mode::type_keys`].
/// For example, if `jk` is mapped to `<Esc>`, [`KeyTyped`] will
/// trigger once for `j` and once for `k`, while [`KeySent`] will
/// trigger once for `<Esc>`.
///
/// # Arguments
///
/// - The sent [key].
///
/// [key]: KeyEvent
/// [unmapped]: crate::mode::map
/// [`mode::type_keys`]: crate::mode::type_keys
pub struct KeySent(pub(crate) KeyEvent);

impl Hookable for KeySent {
    type Input<'h> = KeyEvent;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        self.0
    }
}

/// [`Hookable`]: Triggers whenever a [key] is sent to the [`Widget`].
///
/// # Arguments
///
/// - The sent [key].
/// - An [`Handle<W>`] for the widget.
///
/// [key]: KeyEvent
pub struct KeySentTo<M: Mode>(pub(crate) (KeyEvent, Handle<M::Widget>));

impl<M: Mode> Hookable for KeySentTo<M> {
    type Input<'h> = (KeyEvent, &'h Handle<M::Widget>);

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        (self.0.0, &self.0.1)
    }
}

/// [`Hookable`]: Triggers whenever a [key] is typed.
///
/// [`KeyEvent`]s are "typed" when typing keys _or_ when calling the
/// [`mode::type_keys`] function, this is in contrast with
/// [`KeySent`], which triggers when you type [unmapped] keys or with
/// the remapped keys. For example, if `jk` is mapped to `<Esc>`,
/// [`KeyTyped`] will trigger once for `j` and once for `k`, while
/// [`KeySent`] will trigger once for `<Esc>`.
///
/// # Arguments
///
/// - The typed [key].
///
/// [key]: KeyEvent
/// [unmapped]: crate::mode::map
/// [`mode::type_keys`]: crate::mode::type_keys
pub struct KeyTyped(pub(crate) KeyEvent);

impl Hookable for KeyTyped {
    type Input<'h> = KeyEvent;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        self.0
    }
}

impl PartialEq<KeyEvent> for KeyTyped {
    fn eq(&self, other: &KeyEvent) -> bool {
        self.0 == *other
    }
}

/// [`Hookable`]: Triggers on every [`MouseEvent`].
///
/// # Arguments
///
/// - The [`Handle<dyn Widget>`] under the mouse.
/// - The [`MouseEvent`] itself.
pub struct OnMouseEvent(pub(crate) (Handle<dyn Widget>, MouseEvent));

impl Hookable for OnMouseEvent {
    type Input<'h> = (&'h Handle<dyn Widget>, MouseEvent);

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        (&self.0.0, self.0.1)
    }
}

impl PartialEq<MouseEvent> for OnMouseEvent {
    fn eq(&self, other: &MouseEvent) -> bool {
        self.0.1 == *other
    }
}

impl PartialEq<MouseEventKind> for OnMouseEvent {
    fn eq(&self, other: &MouseEventKind) -> bool {
        self.0.1.kind == *other
    }
}

impl<W: Widget> PartialEq<Handle<W>> for OnMouseEvent {
    fn eq(&self, other: &Handle<W>) -> bool {
        self.0.0.ptr_eq(other.widget())
    }
}

/// [`Hookable`]: Triggers whenever a [`Form`] is set.
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

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        (self.0.0, self.0.1, self.0.2)
    }
}

/// [`Hookable`]: Triggers when a [`ColorScheme`] is set.
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

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        self.0
    }
}

/// [`Hookable`]: Triggers after [`Handle::save`] or [`Handle::save_to`].
///
/// Only triggers if the buffer was actually updated.
///
/// # Arguments
///
/// - The [`Handle`] of said [`Buffer`]
/// - Wether the `Buffer` will be closed (happens when calling the
///   `wq` or `waq` commands)
pub struct BufferSaved(pub(crate) (Handle, bool));

impl Hookable for BufferSaved {
    type Input<'h> = (&'h Handle, bool);

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        (&self.0.0, self.0.1)
    }
}

/// A hookable struct, for hooks taking [`Hookable::Input`].
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
    /// How to get the arguments from the [`Hookable`].
    ///
    /// This function is triggered once on every call that was added
    /// via [`hook::add`]. So if three hooks were added to
    /// [`BufferSaved`], for example, [`BufferSaved::get_input`]
    /// will be called three times, once before each hook.
    ///
    /// The vast majority of the time, this function is just a
    /// "getter", as it should take a copy, clone, or reference to the
    /// input type, which should be owned by the `Hookable`. For
    /// example, here's the definition of the [`KeyTyped`] hook:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::{hook::Hookable, mode::KeyEvent, prelude::*};
    ///
    /// struct KeyTyped(pub(crate) KeyEvent);
    ///
    /// impl Hookable for KeyTyped {
    ///     type Input<'h> = KeyEvent;
    ///
    ///     fn get_input<'h>(&'h mut self, pa: &mut Pass) -> Self::Input<'h> {
    ///         self.0
    ///     }
    /// }
    /// ```
    ///
    /// However, given the `&mut self` and `&mut Pass`, you can also
    /// do "inter hook mutations", in order to prepare for future hook
    /// calls. An example of this is on [`BufferUpdated`].
    ///
    /// Note that the [`Pass`] here is purely for internal use, you
    /// are not allowed to return something that borrows from it, as
    /// the borrow checker will prevent you.
    ///
    /// [`hook::add`]: add
    fn get_input<'h>(&'h mut self, pa: &mut Pass) -> Self::Input<'h>;
}

/// Where all hooks of Duat are stored.
#[derive(Clone, Copy)]
struct InnerHooks {
    types: &'static Mutex<HashMap<TypeId, Box<dyn HookHolder>>>,
    groups: &'static Mutex<Vec<InnerGroupId>>,
}

impl InnerHooks {
    /// Adds a hook for a [`Hookable`]
    fn add<H: Hookable>(
        &self,
        callback: Callback<H>,
        group: Option<InnerGroupId>,
        filter: Option<Box<dyn Fn(&H) -> bool + Send + 'static>>,
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
            hooks.push(Hook { callback, group, filter });
        } else {
            let hooks_of = HooksOf::<H>(RefCell::new(vec![Hook { callback, group, filter }]));

            map.insert(TypeId::of::<H>(), Box::new(hooks_of));
        }
    }

    /// Removes hooks with said group.
    fn remove(&self, group_id: InnerGroupId) {
        self.groups.lock().unwrap().retain(|g| *g != group_id);
        let map = self.types.lock().unwrap();
        for holder in map.iter() {
            holder.1.remove(&group_id)
        }
    }

    /// Triggers hooks with args of the [`Hookable`].
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

            let input = hookable.get_input(pa);

            match &mut hook.callback {
                Callback::FnMut(fn_mut) => {
                    catch_panic(|| fn_mut(pa, input));
                    true
                }
                Callback::FnOnce(fn_once) => {
                    catch_panic(|| fn_once.take().unwrap()(pa, input));
                    false
                }
            }
        });

        let mut types = self.types.lock().unwrap();
        if let Some(new_holder) = types.remove(&TypeId::of::<H>()) {
            let new_hooks_of = unsafe {
                let ptr = Box::into_raw(new_holder) as *mut HooksOf<H>;
                Box::from_raw(ptr)
            };

            hooks_of
                .0
                .borrow_mut()
                .extend(new_hooks_of.0.borrow_mut().drain(..));

            types.insert(TypeId::of::<H>(), unsafe {
                Box::from_raw(Box::into_raw(hooks_of) as *mut dyn HookHolder)
            });
        } else {
            types.insert(TypeId::of::<H>(), unsafe {
                Box::from_raw(Box::into_raw(hooks_of) as *mut dyn HookHolder)
            });
        }

        hookable
    }

    /// Checks if a hook group exists.
    fn group_exists(&self, group: InnerGroupId) -> bool {
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

/// An intermediary trait, meant for group removal.
trait HookHolder {
    /// Remove the given group from hooks of this holder
    fn remove(&self, group_id: &InnerGroupId);
}

/// An intermediary struct, meant to hold the hooks of a [`Hookable`].
struct HooksOf<H: Hookable>(RefCell<Vec<Hook<H>>>);

impl<H: Hookable> HookHolder for HooksOf<H> {
    fn remove(&self, group_id: &InnerGroupId) {
        let mut hooks = self.0.borrow_mut();
        hooks.retain(|hook| hook.group.as_ref().is_none_or(|g| g != group_id));
    }
}

struct Hook<H: Hookable> {
    callback: Callback<H>,
    group: Option<InnerGroupId>,
    filter: Option<Box<dyn Fn(&H) -> bool + Send + 'static>>,
}

enum Callback<H: Hookable> {
    FnMut(Box<dyn FnMut(&mut Pass, <H as Hookable>::Input<'_>)>),
    FnOnce(Option<Box<dyn FnOnce(&mut Pass, <H as Hookable>::Input<'_>)>>),
}
