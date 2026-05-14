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
//! - Spawned on [`Text`] via the [`Spawn`] [tag].
//!
//! They can be pushed to all 4 sides of other widgets through the
//! use of [`PushSpecs`]. Or they can be spawned with
//! [`DynSpawnSpecs`]. Each of these structs determine the specifics
//! of where the [`Widget`] will be spawned, as well as how its
//! [`Area`] should adapt to changes in the layout.
//!
//! For example, if you spawn a [`Widget`] on [`Text`] via the
//! [`Spawn`], then any movements and modifications on said `Text`
//! will also move the `Widget` around.
//!
//! The only [`Widget`] that is defined in `duat-core` is the
//! [`Buffer`]. It is the quitessential `Widget` for a text editor,
//! being the part that is modified by user input.
//!
//! [`Window`]: super::Window
//! [`Window::push_inner`]: super::Window::push_inner
//! [`Window::push_outer`]: super::Window::push_outer
//! [`Spawn`]: crate::text::Spawn
//! [tag]: crate::text::Tag
//! [`PushSpecs`]: super::PushSpecs
//! [`DynSpawnSpecs`]: super::DynSpawnSpecs
//! [`Area`]: super::Area
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

use crate::{
    buffer::Buffer,
    context::Handle,
    data::{Pass, RwData},
    form,
    hook::{self, BufferClosed, OnMouseEvent, WidgetClosed},
    mode::{ToggleEvent, TwoPointsPlace},
    opts::PrintOpts,
    session::UiMouseEvent,
    text::{Text, TextId, TextMut, TextVersion},
    ui::{PrintInfo, RwArea, SpawnId},
};

/// An area where [`Text`] will be printed to the screen.
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
    /// The [`Text`] that this widget prints out.
    ///
    /// Note here the presence of the [`Pass`] argument. This means
    /// that this could actually be any `Text` in duat, that is
    /// accessible through the [`Pass`].
    ///
    /// Sometimes, it is annoying to access the `Text` of a widget
    /// because you don't have the `Pass` available at that moment. In
    /// those scenarios, the recommendation is that you implement an
    /// inherent `Self::text` function, indicating that access to the
    /// `Text` is not reliant on access to global state.
    fn text<'p>(widget: &'p RwData<Self>, pa: &'p Pass) -> &'p Text
    where
        Self: Sized;

    /// A mutable reference to the [`Text`] that is printed.
    ///
    /// Note here the presence of the [`Pass`] argument. This means
    /// that this could actually be any `Text` in duat, that is
    /// accessible through the [`Pass`].
    ///
    /// Sometimes, it is annoying to access the `Text` of a widget
    /// because you don't have the `Pass` available at that moment. In
    /// those scenarios, the recommendation is that you implement an
    /// inherent `Self::text_mut` function, indicating that access to
    /// the `Text` is not reliant on access to global state.
    fn text_mut<'p>(widget: &'p RwData<Self>, pa: &'p mut Pass) -> TextMut<'p>
    where
        Self: Sized;

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
    print: Arc<dyn Fn(&mut Pass) + Send + Sync>,
    on_mouse_event: Arc<dyn Fn(&mut Pass, UiMouseEvent) + Send + Sync>,
    on_close: fn(&mut Pass, &Handle<dyn Widget>),
    last_printed: RwData<Option<(TextVersion, TextId)>>,
}

impl Node {
    /// Returns a new `Node`
    pub(crate) fn new<W: Widget>(
        widget: RwData<W>,
        area: RwArea,
        master: Option<Handle<dyn Widget>>,
        is_closed: Arc<AtomicBool>,
        spawn_id: Option<SpawnId>,
    ) -> Self {
        Self::from_handle(Handle::new(widget, area, master, is_closed, spawn_id))
    }

    /// Returns a `Node` from an existing [`Handle`]
    pub(crate) fn from_handle<W: Widget>(handle: Handle<W>) -> Self {
        Self {
            handle: handle.to_dyn(),
            print: if let Some(buffer) = handle.get_as::<Buffer>() {
                let handle = handle.clone();

                Arc::new(move |pa| {
                    Buffer::update(pa, &buffer);

                    handle.area.print(
                        pa,
                        handle.text(pa),
                        handle.opts(pa),
                        form::painter_with_widget::<W>(),
                    );
                })
            } else {
                let handle = handle.clone();
                Arc::new(move |pa| {
                    handle.area.print(
                        pa,
                        handle.text(pa),
                        handle.opts(pa),
                        form::painter_with_widget::<W>(),
                    );
                })
            },
            on_mouse_event: Arc::new({
                let handle = handle.clone();
                let dyn_handle = handle.to_dyn();
                move |pa, event| {
                    let opts = handle.opts(pa);
                    let text = handle.text(pa);

                    let points = handle.area().points_at_coord(pa, text, event.coord, opts);

                    hook::trigger(
                        pa,
                        OnMouseEvent {
                            handle: dyn_handle.clone(),
                            points,
                            coord: event.coord,
                            kind: event.kind,
                            modifiers: event.modifiers,
                        },
                    );
                    hook::trigger(
                        pa,
                        OnMouseEvent {
                            handle: handle.clone(),
                            points,
                            coord: event.coord,
                            kind: event.kind,
                            modifiers: event.modifiers,
                        },
                    );

                    if let Some(TwoPointsPlace::Within(points)) = points {
                        let event = ToggleEvent {
                            handle: &dyn_handle,
                            points,
                            coord: event.coord,
                            kind: event.kind,
                            modifiers: event.modifiers,
                        };
                        let toggles = handle.text(pa).toggles_surrounding(points.real);

                        crate::utils::catch_panic(|| {
                            for (range, toggle_fn) in toggles {
                                toggle_fn.lock().unwrap()(pa, event, range);
                            }
                        });
                    }
                }
            }),
            on_close: if handle.widget().is::<Buffer>() {
                |pa, handle| _ = hook::trigger(pa, BufferClosed((handle.get_as().unwrap(), false)))
            } else {
                |pa, handle| _ = hook::trigger(pa, WidgetClosed(handle.get_as::<W>().unwrap()))
            },
            last_printed: RwData::new(None),
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
        self.handle.get_as()
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
        if let Some(last_printed) = self.last_printed.read(pa) {
            let text = self.handle.text(pa);
            let new = (text.version(), text.id());
            *last_printed != new
                || self.handle.update_requested.load(Ordering::Relaxed)
                || self.handle.widget().has_changed()
                || self.handle.area.has_changed(pa)
        } else {
            true
        }
    }

    ////////// Eventful functions

    /// Updates and prints this [`Node`]
    pub(crate) fn print(&self, pa: &mut Pass, win: usize) {
        self.handle.update_requested.store(false, Ordering::Relaxed);

        crate::context::windows().cleanup_despawned(pa);
        if self.handle().is_closed() {
            return;
        }

        let print_info = self.handle.area().get_print_info(pa);
        let mut text = self.handle.text_mut(pa);

        if print_info != PrintInfo::default() {
            text.update_bounds();
        }

        let widgets_to_spawn = text.get_widget_spawns();
        for (_, spawn) in widgets_to_spawn {
            spawn(pa, win, self.handle.clone());
        }

        (self.print)(pa);

        self.handle.declare_as_read();
        self.handle.area().0.declare_as_read();

        let text = self.handle.text(pa);
        let new_version_printed = (text.version(), text.id());
        *self.last_printed.write(pa) = Some(new_version_printed);
    }

    pub(crate) fn on_mouse_event(&self, pa: &mut Pass, mouse_event: UiMouseEvent) {
        (self.on_mouse_event)(pa, mouse_event);
    }

    pub(crate) fn on_close(&self, pa: &mut Pass) {
        self.handle.declare_closed();
        (self.on_close)(pa, &self.handle)
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("handle", &self.handle)
            .finish_non_exhaustive()
    }
}
