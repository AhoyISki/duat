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
    hook::{self, BufferPrinted, FocusedOn, OnMouseEvent, UnfocusedFrom},
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
/// [`Mode`]: crate::mode::Mode
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
    fn print_opts(&self) -> PrintOpts {
        PrintOpts::new()
    }
}

/// Elements related to the [`Widget`]s
#[derive(Clone)]
pub(crate) struct Node {
    handle: Handle<dyn Widget>,
    update: Arc<dyn Fn(&mut Pass) + Send + Sync>,
    print: Arc<dyn Fn(&mut Pass, &Handle<dyn Widget>) + Send + Sync>,
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
                let buf_handle = handle.try_downcast();

                move |pa, orig_handle| {
                    let painter =
                        form::painter_with_widget_and_mask::<W>(*handle.mask().lock().unwrap());

                    handle
                        .area
                        .print(pa, handle.text(pa), handle.opts(pa), painter);

                    if let Some(buf_handle) = buf_handle.clone() {
                        hook::trigger(pa, BufferPrinted(buf_handle));
                        orig_handle.declare_as_read();
                        orig_handle.area().0.declare_as_read();
                    }
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
        let (widget, _) = self.handle.write_with_area(pa);

        if print_info != PrintInfo::default() {
            widget.text_mut().update_bounds();
        }

        let widgets_to_spawn = widget.text_mut().get_widget_spawns();
        for spawn in widgets_to_spawn {
            spawn(pa, win, self.handle.clone());
        }

        (self.print)(pa, &self.handle);
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
