//! APIs for the construction of [`Widget`]s
//!
//! This module has the [`Widget`] trait, which is a region on the
//! window containing a [`Text`], and may be modified by user mode
//! (but not necessarily).
//!
//! These widgets will be used in a few circumstances:
//!
//! - Pushed to [`Handle`]s via [`Handle::push_outer_widget`] or
//!   [`Handle::push_inner_widget`].
//! - Pushed to the [`Window`]'s edges of a window via
//!   [`Window::push_inner`] and [`Window::push_outer`].
//! - Spawned on [`Handle`]s via [`Handle::spawn_widget`].
//! - Spawned on [`Text`] via the [`SpawnTag`] [tag].
//!
//! They can be pushed to all 4 sides of other widgets through the
//! use of [`PushSpecs`]. Or they can be spawned with
//! [`DynSpawnSpecs`]. Each of these structs determine the specifics
//! of where the [`Widget`] will be spawned, as well as how its
//! [`Area`] should adapt to changes in the layout.
//!
//! For example, if you spawn a [`Widget`] on [`Text`] via the
//! [`SpawnTag`], then any movements and modifications on said `Text`
//! will also move the `Widget` around.
//!
//! The only [`Widget`] that is defined in `duat-core` is the
//! [`Buffer`]. It is the quitessential `Widget` for a text editor,
//! being the part that is modified by user input.
//!
//! [`Buffer`]: crate::buffer::Buffer
//! [`Window`]: super::Window
//! [`Window::push_inner`]: super::Window::push_inner
//! [`Window::push_outer`]: super::Window::push_outer
//! [`SpawnTag`]: crate::text::SpawnTag
//! [tag]: crate::text::Tag
//! [`PushSpecs`]: super::PushSpecs
//! [`DynSpawnSpecs`]: super::DynSpawnSpecs
//! [`Area`]: super::Area
use std::sync::{Arc, Mutex, atomic::Ordering};

use crate::{
    context::Handle,
    data::{Pass, RwData},
    form,
    hook::{self, FocusedOn, OnMouseEvent, UnfocusedFrom},
    mode::MouseEvent,
    opts::PrintOpts,
    session::UiMouseEvent,
    text::{Text, TextMut},
    ui::{PrintInfo, RwArea},
    utils::catch_panic,
};

/// An area where [`Text`] will be printed to the screen
///
/// Most widgets are supposed to be passive widgets, that simply show
/// information about the current state of Duat. In order to
/// show that information, widgets make use of [`Text`], which can
/// show stylized text, buttons, and all sorts of other stuff. (For
/// widgets that react to input, see the documentation for[`Mode`]).
///
/// For a demonstration on how to create a widget, I will create a
/// widget that shows the uptime, in seconds, for Duat.
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use std::time::Duration;
///
/// use duat::{data::PeriodicChecker, prelude::*};
///
/// struct UpTime(Text, PeriodicChecker);
///
/// impl UpTime {
///     fn new() -> Self {
///         Self(
///             Text::default(),
///             PeriodicChecker::new(Duration::from_secs(1)),
///         )
///     }
/// }
/// ```
///
/// In order to be a proper widget, it must have a [`Text`] to
/// display. The [`PeriodicChecker`] will be explained later. Next, I
/// implement `Widget` on `UpTime`:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use std::time::Duration;
///
/// use duat::{data::PeriodicChecker, prelude::*};
///
/// struct UpTime(Text, PeriodicChecker);
///
/// impl UpTime {
///     fn new() -> Self {
///         Self(
///             Text::default(),
///             PeriodicChecker::new(Duration::from_secs(1)),
///         )
///     }
/// }
///
/// impl Widget for UpTime {
///     fn update(pa: &mut Pass, handle: &Handle<Self>) {
///         todo!()
///     }
///
///     fn needs_update(&self, pa: &Pass) -> bool {
///         todo!();
///     }
///
///     fn text(&self) -> &Text {
///         &self.0
///     }
///
///     fn text_mut(&mut self) -> TextMut<'_> {
///         self.0.as_mut()
///     }
/// }
/// ```
///
/// The [`Widget::update`] funcion is responsible for updating the
/// [`Text`] of the `Widget` on every frame. However, it is only
/// called if [`Widget::needs_update`] returns `true`. Note that this
/// isn't the only way to update `Widget`s, since in any place where
/// you have global access throught the [`Pass`] (like [hooks],
/// [commands], etc.), you can update any [`Handle`] for any `Widget`.
///
/// There are two other `Widget` functions:
/// [`Widget::on_focus`] and [`Widget::on_unfocus`], which are called
/// when a [`Mode`] is set, and the `Widget` is focused or unfocused.
/// For this example, since there are no `Mode`s, these will not be
/// used.
///
/// Next, I will finish implementing the `Widget` trait.
///
/// First of all, there needs to be a starting [`Instant`] to compare
/// with the current moment in time. The correct moment to do that
/// would be right as the `setup` function is called. This can be done
/// safely with a [`OnceLock`]:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use std::{sync::OnceLock, time::Instant};
///
/// use duat::prelude::*;
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// fn setup() {
///     START_TIME.set(Instant::now()).unwrap();
/// }
/// ```
///
/// However, exposing that to end users is rather poor UX, so you
/// should make use of [`Plugin`]s instead:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use std::{sync::OnceLock, time::Instant};
///
/// use duat::prelude::*;
/// struct UpTimePlugin;
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// impl Plugin for UpTimePlugin {
///     fn plug(self, plugins: &Plugins) {
///         START_TIME.set(Instant::now()).unwrap();
///     }
/// }
/// ```
///
/// Next, I'm going to implement the [`update`] function:
///
/// ```rust
/// # use std::{sync::OnceLock, time::Instant};
/// # duat_core::doc_duat!(duat);
/// use duat::{data::PeriodicChecker, prelude::*};
/// # struct UpTime(Text, PeriodicChecker);
///
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// impl Widget for UpTime {
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> TextMut<'_> { self.0.as_mut() }
/// #     fn needs_update(&self, pa: &Pass) -> bool { todo!(); }
///     // ...
///     fn update(pa: &mut Pass, handle: &Handle<Self>) {
///         let start = START_TIME.get().unwrap();
///         let elapsed = start.elapsed();
///         let mins = elapsed.as_secs() / 60;
///         let secs = elapsed.as_secs() % 60;
///
///         handle.write(pa).0 = txt!("[uptime.mins]{mins}m [uptime.secs]{secs}s");
///     }
/// }
/// ```
///
/// This should format the [`Text`] via [`txt!`] to show how many
/// minutes and seconds have passed. However, I'm using the
/// `uptime.mins` and `updime.secs` [`Form`]s, which aren't set to
/// anything, so they'll just display normally colored text.
///
/// To solve that, just add more statements to the plugin:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use std::{sync::OnceLock, time::Instant};
/// use duat::prelude::*;
///
/// struct UpTimePlugin;
///
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// impl Plugin for UpTimePlugin {
///     fn plug(self, plugins: &Plugins) {
///         START_TIME.set(Instant::now()).unwrap();
///         form::set_weak("uptime", Form::green());
///     }
/// }
/// ```
///
/// Note the [`form::set_weak`]. This function "weakly" sets the
/// [`Form`], that is, it sets it _only_ if it wasn't set before via
/// [`form::set`]. This is useful since the order in which the plugin
/// is added and the `Form` is set by the end user doesn't end up
/// mattering.
///
/// Note also that I set `uptime`, rather than `uptime.mins` or
/// `uptime.secs`. Due to `Form` inheritance, any form with a `.` in
/// it will inherit from the parent, unless explicitly set to
/// something else. this inheritance follows even when the parent
/// changes. That is, if the user sets the `uptime` form to something
/// else, `uptime.mins` and `uptime.secs` will also be set to that.
///
/// Now, I'm going to implement the [`needs_update`] function, that's
/// where the [`PeriodicChecker`] comes in to play:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use std::{sync::OnceLock, time::Instant};
/// use duat::{data::PeriodicChecker, prelude::*};
///
/// // This was set during the `setup` function
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// struct UpTime(Text, PeriodicChecker);
///
/// impl Widget for UpTime {
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> TextMut<'_> { self.0.as_mut() }
/// #     fn update(pa: &mut Pass, handle: &Handle<Self>) { }
///     fn needs_update(&self, pa: &Pass) -> bool {
///         // Returns `true` once per second
///         self.1.check()
///     }
/// }
/// ```
///
/// The [`needs_update`] function is executed on every frame, however,
/// it should only return `true` every second, which is when the
/// [`update`] function will be called, updating the `Widget`.
///
/// Now, all that is left to do is placing the `Widget` on screen. To
/// do that, I will make use of a [hook] to place them on the bottom
/// of the [`Window`], right below the [`Buffer`]s:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use std::{
///     sync::OnceLock,
///     time::{Duration, Instant},
/// };
///
/// use duat::{data::PeriodicChecker, prelude::*};
///
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// struct UpTimePlugin;
///
/// impl Plugin for UpTimePlugin {
///     fn plug(self, plugins: &Plugins) {
///         START_TIME.set(Instant::now()).unwrap();
///         form::set_weak("uptime", Form::green());
///
///         hook::add::<WindowOpened>(|pa, window| {
///             let specs = ui::PushSpecs {
///                 side: ui::Side::Below,
///                 height: Some(1.0),
///                 ..Default::default()
///             };
///             window.push_inner(pa, UpTime::new(), specs);
///         });
///     }
/// }
///
/// struct UpTime(Text, PeriodicChecker);
///
/// impl UpTime {
///     fn new() -> Self {
///         Self(
///             Text::default(),
///             PeriodicChecker::new(Duration::from_secs(1)),
///         )
///     }
/// }
///
/// impl Widget for UpTime {
///     fn update(pa: &mut Pass, handle: &Handle<Self>) {
///         let start = START_TIME.get().unwrap();
///         let elapsed = start.elapsed();
///         let mins = elapsed.as_secs() / 60;
///         let secs = elapsed.as_secs() % 60;
///
///         handle.write(pa).0 = txt!("[uptime.mins]{mins}m [uptime.secs]{secs}s");
///     }
///
///     fn needs_update(&self, pa: &Pass) -> bool {
///         self.1.check()
///     }
///
///     fn text(&self) -> &Text {
///         &self.0
///     }
///
///     fn text_mut(&mut self) -> TextMut<'_> {
///         self.0.as_mut()
///     }
/// }
/// ```
///
/// Here, I'm adding a [hook] to push this widget to the bottom of the
/// [`Window`], right as said [`Window`] is opened. By using
/// [`Window::push_inner`], the `Widget` will be placed below the
/// central [`Buffer`]s region, but above other `Widget`s that were
/// pushed to the bottom. If I wanted the `Widget` on the edges of the
/// screen, I could use [`Window::push_outer` instead.
///
/// [`Mode`]: crate::mode::Mode
/// [`PeriodicChecker`]: crate::data::PeriodicChecker
/// [`WidgetOpened`]: crate::hook::WidgetOpened
/// [`WindowOpened`]: crate::hook::WindowOpened
/// [hooks]: crate::hook
/// [commands]: crate::cmd
/// [`PhantomData<U>`]: std::marker::PhantomData
/// [`Instant`]: std::time::Instant
/// [`ConfigLoaded`]: crate::hook::ConfigLoaded
/// [`update`]: Widget::update
/// [`needs_update`]: Widget::needs_update
/// [`Form`]: crate::form::Form
/// [`form::set_weak*`]: crate::form::set_weak
/// [`txt!`]: crate::text::txt
/// [`Plugin`]: crate::Plugin
/// [`Buffer`]: crate::buffer::Buffer
/// [`PushSpecs`]: super::PushSpecs
/// [`Window`]: super::Window
/// [`Window::push_inner`]: super::Window::push_inner
/// [`Window::push_outer`]: super::Window::push_outer
/// [`OnceLock`]: std::sync::OnceLock
#[allow(unused)]
pub trait Widget: Send + 'static {
    ////////// Stateful functions

    /// Updates the widget alongside its [`RwArea`] in the [`Handle`]
    ///
    /// This function will be triggered when Duat deems that a change
    /// has happened to this [`Widget`], which is when
    /// [`Widget::needs_update`] returns `true`.
    ///
    /// It can also happen if [`RwData<Self>::has_changed`] or
    /// [`RwData::has_changed`] return `true`. This can happen
    /// from many places, like [hooks], [commands], editing by
    /// [`Mode`]s, etc.
    ///
    /// Importantly, [`update`] should be able to handle any number of
    /// changes that have taken place in this [`Widget`], so you
    /// should avoid depending on state which may become
    /// desynchronized.
    ///
    /// When implementing this, you are free to remove the `where`
    /// clause.
    ///
    /// [hooks]: crate::hook
    /// [commands]: crate::cmd
    /// [`Mode`]: crate::mode::Mode
    /// [`update`]: Widget::update
    fn update(pa: &mut Pass, handle: &Handle<Self>)
    where
        Self: Sized;

    /// Actions to do whenever this [`Widget`] is focused
    ///
    /// When implementing this, you are free to remove the `where`
    /// clause.
    fn on_focus(pa: &mut Pass, handle: &Handle<Self>)
    where
        Self: Sized,
    {
    }

    /// Actions to do whenever this [`Widget`] is unfocused
    ///
    /// When implementing this, you are free to remove the `where`
    /// clause.
    fn on_unfocus(pa: &mut Pass, handle: &Handle<Self>)
    where
        Self: Sized,
    {
    }

    /// How to handle a [`MouseEvent`]
    ///
    /// Normally, nothing will be done, with the exception of button
    /// [`Tag`]s which are triggered normally.
    ///
    /// [`Tag`]: crate::text::Tag
    fn on_mouse_event(pa: &mut Pass, handle: &Handle<Self>, event: MouseEvent)
    where
        Self: Sized,
    {
    }

    /// Tells Duat that this [`Widget`] should be updated
    ///
    /// Determining wether a [`Widget`] should be updated, for a good
    /// chunk of them, will require some code like this:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::prelude::*;
    ///
    /// struct MyWidget(Handle<Buffer>);
    ///
    /// impl Widget for MyWidget {
    /// #   fn update(_: &mut Pass, handle: &Handle<Self>) { todo!() }
    /// #   fn text(&self) -> &Text { todo!() }
    /// #   fn text_mut(&mut self) -> TextMut<'_> { todo!() }
    ///     // ...
    ///     fn needs_update(&self, pa: &Pass) -> bool {
    ///         self.0.has_changed(pa)
    ///     }
    /// }
    /// ```
    ///
    /// In this case, `MyWidget` is telling Duat that it should be
    /// updated whenever the buffer in the [`Handle<Buffer>`] gets
    /// changed.
    ///
    /// One interesting use case of this function is the
    /// [`StatusLine`], which can be altered if any of its parts
    /// get changed, some of them depend on a [`Handle<Buffer>`],
    /// but a lot of others depend on checking other [`data`] types in
    /// order to figure out if an update is needed.
    ///
    /// [`StatusLine`]: https://docs.rs/duat-core/latest/duat/widgets/struct.StatusLine.html
    /// [`data`]: crate::data
    fn needs_update(&self, pa: &Pass) -> bool;

    /// The text that this widget prints out
    fn text(&self) -> &Text;

    /// A mutable reference to the [`Text`] that is printed
    fn text_mut(&mut self) -> TextMut<'_>;

    /// The [configuration] for how to print [`Text`]
    ///
    /// The default configuration, used when `print_opts` is not
    /// implemented,can be found at [`PrintOpts::new`].
    ///
    /// [configuration]: PrintOpts
    fn get_print_opts(&self) -> PrintOpts {
        PrintOpts::new()
    }
}

/// Elements related to the [`Widget`]s
#[derive(Clone)]
pub(crate) struct Node {
    handle: Handle<dyn Widget>,
    update: Arc<dyn Fn(&mut Pass) + Send + Sync>,
    print: Arc<dyn Fn(&mut Pass) + Send + Sync>,
    on_focus: Arc<dyn Fn(&mut Pass, Handle<dyn Widget>) + Send + Sync>,
    on_unfocus: Arc<dyn Fn(&mut Pass, Handle<dyn Widget>) + Send + Sync>,
    on_mouse_event: Arc<dyn Fn(&mut Pass, UiMouseEvent) + Send + Sync>,
}

impl Node {
    /// Returns a new `Node`
    pub(crate) fn new<W: Widget>(
        widget: RwData<W>,
        area: RwArea,
        master: Option<Handle<dyn Widget>>,
    ) -> Self {
        Self::from_handle(Handle::new(widget, area, Arc::new(Mutex::new("")), master))
    }

    /// Returns a `Node` from an existing [`Handle`]
    pub(crate) fn from_handle<W: Widget>(handle: Handle<W>) -> Self {
        Self {
            handle: handle.to_dyn(),
            update: Arc::new({
                let handle = handle.clone();
                move |pa| _ = catch_panic(|| W::update(pa, &handle))
            }),
            print: Arc::new({
                let handle = handle.clone();
                move |pa| {
                    let painter =
                        form::painter_with_widget_and_mask::<W>(*handle.mask().lock().unwrap());
                    handle
                        .area
                        .print(pa, handle.text(pa), handle.opts(pa), painter)
                }
            }),
            on_focus: Arc::new({
                let handle = handle.clone();
                move |pa, old| {
                    hook::trigger(pa, FocusedOn((old, handle.clone())));
                    catch_panic(|| W::on_focus(pa, &handle));
                }
            }),
            on_unfocus: Arc::new({
                let handle = handle.clone();
                move |pa, new| {
                    hook::trigger(pa, UnfocusedFrom((handle.clone(), new)));
                    catch_panic(|| W::on_unfocus(pa, &handle));
                }
            }),
            on_mouse_event: Arc::new({
                let dyn_handle = handle.to_dyn();
                let handle = handle.clone();
                move |pa, event| {
                    let opts = handle.opts(pa);
                    let text = handle.text(pa);
                    let event = MouseEvent {
                        points: handle.area().points_at_coord(pa, text, event.coord, opts),
                        coord: event.coord,
                        kind: event.kind,
                        modifiers: event.modifiers,
                    };

                    catch_panic(|| {
                        hook::trigger(pa, OnMouseEvent((dyn_handle.clone(), event)));
                        W::on_mouse_event(pa, &handle, event);
                    });
                }
            }),
        }
    }

    ////////// Reading and parts acquisition

    pub(crate) fn read_as<'a, W: Widget>(&'a self, pa: &'a Pass) -> Option<&'a W> {
        self.handle.read_as(pa)
    }

    /// The [`Widget`] of this [`Node`]
    pub(crate) fn widget(&self) -> &RwData<dyn Widget> {
        self.handle.widget()
    }

    /// The [`Ui::Area`] of this [`Widget`]
    pub(crate) fn area(&self) -> &RwArea {
        self.handle.area()
    }

    /// Returns the downcast ref of this [`Widget`].
    pub(crate) fn try_downcast<W: Widget>(&self) -> Option<Handle<W>> {
        self.handle.try_downcast()
    }

    /// The "parts" of this [`Node`]
    pub(crate) fn handle(&self) -> &Handle<dyn Widget> {
        &self.handle
    }

    ////////// Querying functions

    /// Wether the value within is `W`
    pub(crate) fn data_is<W: 'static>(&self) -> bool {
        self.handle.widget().is::<W>()
    }

    /// Wether this and [`RwData`] point to the same value
    pub(crate) fn ptr_eq<W: ?Sized>(&self, other: &RwData<W>) -> bool {
        self.handle.ptr_eq(other)
    }

    /// Wether this [`Widget`] needs to be updated
    pub(crate) fn needs_update(&self, pa: &Pass) -> bool {
        self.handle.update_requested.load(Ordering::Relaxed)
            || self.handle.widget().has_changed()
            || self.handle.area.has_changed(pa)
            || self.handle.read(pa).needs_update(pa)
    }

    ////////// Eventful functions

    /// Updates and prints this [`Node`]
    pub(crate) fn update_and_print(&self, pa: &mut Pass, win: usize) {
        self.handle.update_requested.store(false, Ordering::Relaxed);
        if self.handle().is_closed(pa) {
            return;
        }

        (self.update)(pa);

        crate::context::windows().cleanup_despawned(pa);
        if self.handle().is_closed(pa) {
            return;
        }

        let print_info = self.handle.area().get_print_info(pa);
        let (widget, area) = self.handle.write_with_area(pa);
        let opts = widget.get_print_opts();
        widget.text_mut().add_selection_tags(area, opts);

        if print_info != PrintInfo::default() {
            widget.text_mut().update_bounds();
        }

        let widgets_to_spawn = self.handle.text_mut(pa).get_widget_spawns();
        for spawn in widgets_to_spawn {
            spawn(pa, win, self.handle.clone());
        }

        (self.print)(pa);

        self.handle.text_mut(pa).remove_selection_tags();
    }

    /// What to do when focusing
    pub(crate) fn on_focus(&self, pa: &mut Pass, old: Handle<dyn Widget>) {
        self.handle.area().set_as_active(pa);
        (self.on_focus)(pa, old)
    }

    /// What to do when unfocusing
    pub(crate) fn on_unfocus(&self, pa: &mut Pass, new: Handle<dyn Widget>) {
        (self.on_unfocus)(pa, new)
    }

    pub(crate) fn on_mouse_event(&self, pa: &mut Pass, mouse_event: UiMouseEvent) {
        (self.on_mouse_event)(pa, mouse_event);
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("handle", &self.handle)
            .finish_non_exhaustive()
    }
}
