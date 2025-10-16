//! APIs for the construction of widgets, and a few common ones.
//!
//! This module has the [`Widget`] trait, which is a region on the
//! window containing a [`Text`], and may be modified by user mode
//! (but not necessarily).
//!
//! These widgets will be used in two circumstances:
//!
//! - Being pushed to [`Widget`]s via the [`WidgetCreated`] [`hook`];
//! - Being pushed to the outer edges of a window via
//!   [`WindowCreated`];
//!
//! They can be pushed to all 4 sides of other widgets through the
//! use of [`PushSpecs`]. When pushing widgets, you can also include
//! [`Constraint`]s in order to get a specific size on the screen for
//! the widget.
//!
//! ```rust
//! # use duat_core::ui::PushSpecs;
//! let specs = PushSpecs::left().hor_min(10.0).ver_len(2.0);
//! ```
//!
//! When pushing a widget with these `specs` to another widget, Duat
//! will put it on the left, and _try_ to give it a minimum width of
//! `10.0`, and a height of `2.0`.
//!
//! This module also describes a [`WidgetCfg`], which is used in
//! widget construction.
//!
//! [`Buffer`]: crate::buffer::Buffer
//! [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
//! [`LineNumbers`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
//! [`StatusLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
//! [`WidgetCreated`]: crate::hook::WidgetCreated
//! [`WindowCreated`]: crate::hook::WindowCreated
//! [`Constraint`]: crate::ui::Constraint
use std::sync::{Arc, Mutex};

use crate::{
    cfg::PrintCfg,
    context::Handle,
    data::{Pass, RwData},
    form::{self, Painter},
    hook::{self, FocusedOn, UnfocusedFrom},
    text::Text,
    ui::{Area, PrintInfo},
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
/// # use duat_core::{data::PeriodicChecker, text::Text};
/// struct UpTime(Text, PeriodicChecker);
/// ```
///
/// In order to be a proper widget, it must have a [`Text`] to
/// display. The [`PeriodicChecker`] will be explained later. Next, I
/// implement [`Widget`] on `UpTime`:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{data::PeriodicChecker, prelude::*};
/// # struct UpTime(Text, PeriodicChecker);
/// # struct UpTimeCfg;
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg {
/// #     type Widget = UpTime;
/// #     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) { todo!() }
/// # }
/// impl<U: Ui> Widget<U> for UpTime {
///     type Cfg = UpTimeCfg;
///
///     fn cfg() -> Self::Cfg {
///         UpTimeCfg
///     }
///     // more methods remain below
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text { &mut self.0 }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, handle: &Handle<Self, U>) {}
/// #     fn needs_update(&self, pa: &Pass) -> bool { todo!(); }
/// }
/// ```
///
/// Notice the `UpTimeCfg` defined as the `Widget::Cfg` for `UpTime`.
/// [`WidgetCfg`]s  exist to let users push [`Widget`]s to [`Buffer`]s
/// and the window through the [`WidgetCreated`] and [`WindowCreated`]
/// [hooks]. It lets users configure widgets through methods defined
/// by the widget author.
///
/// ```rust
/// # use std::{sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{data::PeriodicChecker, prelude::*};
/// # struct UpTime(Text, PeriodicChecker);
/// struct UpTimeCfg;
///
/// impl<U: Ui> WidgetCfg<U> for UpTimeCfg {
///     type Widget = UpTime;
///
///     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
///         let checker = PeriodicChecker::new(std::time::Duration::from_secs(1));
///         let widget = UpTime(Text::new(), checker);
///         let specs = PushSpecs::below().ver_len(1.0);
///
///         (widget, specs)
///     }
/// }
/// # impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg;
/// #     fn cfg() -> Self::Cfg { UpTimeCfg }
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text{ &mut self.0 }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, handle: &Handle<Self, U>) {}
/// #     fn needs_update(&self, pa: &Pass) -> bool { todo!(); }
/// # }
/// ```
///
/// The [`build`] method should return 2 objects:
///
/// * The widget itself.
/// * [How] to push the widget around. This happens in an inside-out
///   fashion.
///
/// Now, there are some other methods from [`Widget`] that need
/// to be implemented for this to work. First of all, there needs to
/// be a starting [`Instant`] to compare with the current moment in
/// time.
///
/// The best time to do something like this is after Duat is done with
/// initial setup. This happens when the [`ConfigLoaded`] hook is
/// triggered.
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// setup_duat!(setup);
/// use std::{sync::OnceLock, time::Instant};
///
/// use duat::prelude::*;
///
/// fn setup() {
///     static START_TIME: OnceLock<Instant> = OnceLock::new();
///     hook::add::<ConfigLoaded>(|_, _| START_TIME.set(Instant::now()).unwrap());
/// }
/// ```
///
/// This should be added to the `setup` function in the `config`
/// crate. Obviously, requiring that the end user adds a [hook] for
/// your [`Widget`] to work is poor UX design, so this should be
/// placed inside of a [`Plugin`] instead.
///
/// Next I'm going to implement two other [`Widget`] functions:
/// [`once`] and [`update`]. The [`once`] function will do things that
/// should only happen once, even if multiple of a given [`Widget`]
/// are spawned. The [`update`] function is where the [`Text`] should
/// be updated:
///
/// ```rust
/// # use std::{sync::OnceLock, time::Instant};
/// # use duat_core::{prelude::*, data::PeriodicChecker};
/// # struct UpTime(Text, PeriodicChecker);
/// # struct UpTimeCfg;
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg {
/// #     type Widget = UpTime;
/// #     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (UpTime, PushSpecs) { todo!() }
/// # }
/// // This was set during the `setup` function
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
/// impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg;
/// #     fn cfg() -> Self::Cfg { UpTimeCfg }
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text { &mut self.0 }
/// #     fn needs_update(&self, pa: &Pass) -> bool { todo!(); }
///     // ...
///     fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
///         let start = START_TIME.get().unwrap();
///         let elapsed = start.elapsed();
///         let mins = elapsed.as_secs() / 60;
///         let secs = elapsed.as_secs() % 60;
///
///         handle.write(pa).0 = txt!("[uptime.mins]{mins}m [uptime.secs]{secs}s").build();
///     }
///
///     fn once() -> Result<(), Text> {
///         form::set_weak("uptime.mins", Form::new().green());
///         form::set_weak("uptime.secs", Form::new().green());
///         Ok(())
///     }
/// }
/// ```
///
/// In the [`once`] function, I am setting the `"UpTime"` [`Form`],
/// which is going to be used on the `UpTime`'s [`Text`]. Finally, the
/// only thing that remains to be done is a function to check for
/// updates: [`Widget::needs_update`]. That's where the
/// [`PeriodicChecker`] comes in:
///
/// ```rust
/// # use std::{sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{data::PeriodicChecker, prelude::*};
/// // This was set during the `setup` function
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// struct UpTime(Text, PeriodicChecker);
///
/// impl<U: Ui> Widget<U> for UpTime {
///     type Cfg = UpTimeCfg;
///
///     fn needs_update(&self, pa: &Pass) -> bool {
///         // Returns `true` once per second
///         self.1.check()
///     }
///
///     fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
///         let start = START_TIME.get().unwrap();
///         let elapsed = start.elapsed();
///         let mins = elapsed.as_secs() / 60;
///         let secs = elapsed.as_secs() % 60;
///
///         handle.write(pa).0 = txt!("[uptime.mins]{mins}m [uptime.secs]{secs}s").build();
///     }
///
///     fn cfg() -> Self::Cfg {
///         UpTimeCfg
///     }
///
///     // Some methods used in Duat
///     fn text(&self) -> &Text {
///         &self.0
///     }
///
///     fn text_mut(&mut self) -> &mut Text {
///         &mut self.0
///     }
///
///     fn once() -> Result<(), Text> {
///         form::set_weak("uptime.mins", Form::new().green());
///         form::set_weak("uptime.secs", Form::new().green());
///         Ok(())
///     }
/// }
///
/// struct UpTimeCfg;
///
/// impl<U: Ui> WidgetCfg<U> for UpTimeCfg {
///     type Widget = UpTime;
///
///     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (UpTime, PushSpecs) {
///         // You could imagine how a method on `UpTimeCfg` could
///         // change the periodicity
///         let checker = PeriodicChecker::new(Duration::from_secs(1));
///         let widget = UpTime(Text::new(), checker);
///         let specs = PushSpecs::below().ver_len(1.0);
///
///         (widget, specs)
///     }
/// }
/// ```
///
/// [`Mode`]: crate::mode::Mode
/// [`cfg`]: Widget::cfg
/// [`build`]: WidgetCfg::build
/// [How]: PushSpecs
/// [`PeriodicChecker`]: crate::data::PeriodicChecker
/// [`WidgetCreated`]: crate::hook::WidgetCreated
/// [`WindowCreated`]: crate::hook::WindowCreated
/// [hooks]: crate::hook
/// [`PhantomData<U>`]: std::marker::PhantomData
/// [`Instant`]: std::time::Instant
/// [`ConfigLoaded`]: crate::hook::ConfigLoaded
/// [`once`]: Widget::once
/// [`update`]: Widget::update
/// [`Form`]: crate::form::Form
/// [`form::set_weak*`]: crate::form::set_weak
/// [`txt!`]: crate::text::txt
/// [`Plugin`]: crate::Plugin
/// [`Buffer`]: crate::buffer::Buffer
pub trait Widget: Send + 'static {
    ////////// Stateful functions

    /// Updates the widget, allowing the modification of its
    /// [`Area`]
    ///
    /// This function will be triggered when Duat deems that a change
    /// has happened to this [`Widget`], which is when
    /// [`RwData<Self>::has_changed`] returns `true`. This can happen
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
    #[allow(unused)]
    fn update(pa: &mut Pass, handle: &Handle<Self>)
    where
        Self: Sized;

    /// Actions to do whenever this [`Widget`] is focused
    ///
    /// When implementing this, you are free to remove the `where`
    /// clause.
    #[allow(unused)]
    fn on_focus(pa: &mut Pass, handle: &Handle<Self>)
    where
        Self: Sized,
    {
    }

    /// Actions to do whenever this [`Widget`] is unfocused
    ///
    /// When implementing this, you are free to remove the `where`
    /// clause.
    #[allow(unused)]
    fn on_unfocus(pa: &mut Pass, handle: &Handle<Self>)
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
    /// # use duat_core::prelude::*;
    /// # struct Cfg;
    /// # impl<U: Ui> WidgetCfg<U> for Cfg {
    /// #     type Widget = MyWidget<U>;
    /// #     fn build(self, _: &mut Pass, _: BuildInfo<U>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// struct MyWidget<U: Ui>(Handle<Buffer<U>, U>);
    ///
    /// impl<U: Ui> Widget<U> for MyWidget<U> {
    /// #   type Cfg = Cfg;
    /// #   fn cfg() -> Self::Cfg { todo!() }
    /// #   fn update(_: &mut Pass, handle: &Handle<Self, U>) { todo!() }
    /// #   fn text(&self) -> &Text { todo!() }
    /// #   fn text_mut(&mut self) -> &mut Text { todo!() }
    /// #   fn once() -> Result<(), Text> { todo!() }
    ///     // ...
    ///     fn needs_update(&self, pa: &Pass) -> bool {
    ///         self.0.has_changed()
    ///     }
    /// }
    /// ```
    ///
    /// In this case, `MyWidget` is telling Duat that it should be
    /// updated whenever the file in the [`Handle<Buffer>`] gets
    /// changed.
    ///
    /// One exception to this is the [`StatusLine`], which can be
    /// altered if any of its parts get changed, some of them depend
    /// on a [`Handle<Buffer>`], but a lot of others depend on checking
    /// functions which might need to be triggered.
    ///
    /// [`StatusLine`]: https://docs.rs/duat-core/latest/duat_utils/widgets/struct.StatusLine.html
    fn needs_update(&self, pa: &Pass) -> bool;

    /// The text that this widget prints out
    fn text(&self) -> &Text;

    /// A mutable reference to the [`Text`] that is printed
    fn text_mut(&mut self) -> &mut Text;

    /// The [configuration] for how to print [`Text`]
    ///
    /// The default configuration, used when `print_cfg` is not
    /// implemented,can be found at [`PrintCfg::new`].
    ///
    /// [configuration]: PrintCfg
    fn get_print_cfg(&self) -> PrintCfg {
        PrintCfg::new()
    }

    /// Prints the widget
    ///
    /// Very rarely shouuld you actually implement this method, one
    /// example of where this is actually implemented is in
    /// [`Buffer::print`], where [`Area::print_with`] is called in
    /// order to simultaneously update the list of lines numbers,
    /// for widgets like [`LineNumbers`] to read.
    ///
    /// [`LineNumbers`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    /// [`Buffer::print`]: crate::buffer::Buffer::print
    fn print(&self, pa: &Pass, painter: Painter, area: &Area) {
        let cfg = self.get_print_cfg();
        area.print(pa, self.text(), cfg, painter)
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
}

impl Node {
    /// Returns a new `Node`
    pub(crate) fn new<W: Widget>(widget: RwData<W>, area: Area) -> Self {
        Self::from_handle(Handle::new(widget, area, Arc::new(Mutex::new(""))))
    }

    /// Returns a `Node` from an existing [`Handle`]
    pub(crate) fn from_handle<W: Widget>(handle: Handle<W>) -> Self {
        Self {
            handle: handle.to_dyn(),
            update: Arc::new({
                let handle = handle.clone();
                move |pa| W::update(pa, &handle)
            }),
            print: Arc::new({
                let handle = handle.clone();
                move |pa| {
                    let painter = form::painter_with_mask::<W>(*handle.mask().lock().unwrap());
                    W::print(handle.read(pa), pa, painter, handle.area());
                }
            }),
            on_focus: Arc::new({
                let handle = handle.clone();
                move |pa, old| {
                    hook::trigger(pa, FocusedOn((old, handle.clone())));
                    W::on_focus(pa, &handle);
                }
            }),
            on_unfocus: Arc::new({
                let handle = handle.clone();
                move |pa, new| {
                    hook::trigger(pa, UnfocusedFrom((handle.clone(), new)));
                    W::on_unfocus(pa, &handle);
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
    pub(crate) fn area(&self) -> &Area {
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
        self.handle.widget().data_is::<W>()
    }

    /// Wether this and [`RwData`] point to the same value
    pub(crate) fn ptr_eq<W: ?Sized>(&self, other: &RwData<W>) -> bool {
        self.handle.ptr_eq(other)
    }

    /// Wether this [`Widget`] needs to be updated
    pub(crate) fn needs_update(&self, pa: &Pass) -> bool {
        self.handle.has_changed(pa) || self.handle.read(pa).needs_update(pa)
    }

    ////////// Eventful functions

    /// Updates and prints this [`Node`]
    pub(crate) fn update_and_print(&self, pa: &mut Pass, win: usize) {
        if self.handle().is_closed(pa) {
            return;
        }

        (self.update)(pa);

        let print_info = self.handle.area().get_print_info(pa);
        let (widget, area) = self.handle.write_with_area(pa);
        let cfg = widget.get_print_cfg();
        widget.text_mut().add_selections(area, cfg);

        if print_info != PrintInfo::default() {
            widget.text_mut().update_bounds();
        }

        let widgets_to_spawn = self.handle.text_mut(pa).get_widget_spawns();
        for spawn in widgets_to_spawn {
            spawn(pa, win);
        }
        (self.print)(pa);

        self.handle.text_mut(pa).remove_selections();
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
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("handle", &self.handle)
            .finish_non_exhaustive()
    }
}
