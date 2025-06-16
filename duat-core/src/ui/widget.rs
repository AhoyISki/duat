//! APIs for the construction of widgets, and a few common ones.
//!
//! This module has the [`Widget`] trait, which is a region on the
//! window containing a [`Text`], and may be modified by user mode
//! (but not necessarily).
//!
//! With the exception of the [`File`], these widgets will show up in
//! one of three contexts:
//!
//! - Being pushed to a [`File`] via the hook [`OnFileOpen`];
//! - Being pushed to the outer edges via [`OnWindowOpen`];
//! - Being pushed to popup widgets via `OnPopupOpen` (TODO);
//!
//! These widgets can be pushed to all 4 sides of other widgets,
//! through the use of [`PushSpecs`]. When pushing widgets, you can
//! also include [`Constraint`] in order to get a specific size on the
//! screen for the widget.
//!
//! ```rust
//! # use duat_core::ui::PushSpecs;
//! let specs = PushSpecs::left().with_hor_min(10.0).with_ver_len(2.0);
//! ```
//!
//! When pushing a widget with these `specs` to another widget, Duat
//! will put it on the left, and _try_ to give it a minimum width of
//! `10.0`, and a height of `2.0`.
//!
//! The module also provides 4 native widgets, [`File`] and
//! [`PromptLine`], which can receive user mode, and [`StatusLine`]
//! and [`LineNumbers`] which are not supposed to.
//!
//! These 4 widgets are supposed to be universal, not needing a
//! specific [`Ui`] implementation to work. In contrast, you can
//! create widgets for specific [`Ui`]s. As an example, the
//! [`duat-term`] crate, which is a terminal [`Ui`] implementation for
//! Duat, defines the [`VertRule`] widget, which is a separator that
//! only makes sense in the context of a terminal.
//!
//! This module also describes a [`WidgetCfg`], which is used in
//! widget construction.
//!
//! [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
//! [`LineNumbers`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
//! [`StatusLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
//! [`OnFileOpen`]: crate::hook::OnFileOpen
//! [`OnWindowOpen`]: crate::hook::OnWindowOpen
//! [`Constraint`]: crate::ui::Constraint
use std::{any::TypeId, cell::Cell, rc::Rc};

use crate::{
    cfg::PrintCfg,
    context::{FileHandle, FileParts, Handle},
    data::{Pass, RwData},
    file::File,
    form::{self, Painter},
    hook::{self, FocusedOn, UnfocusedFrom},
    text::Text,
    ui::{PushSpecs, RawArea, Ui},
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
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (UpTime, PushSpecs) { todo!() }
/// # }
/// impl<U: Ui> Widget<U> for UpTime {
///     type Cfg = UpTimeCfg<U>;
///     fn cfg() -> Self::Cfg {
///         UpTimeCfg(PhantomData)
///     }
///     // more methods remain below
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text { &mut self.0 }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, handle: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// }
/// ```
///
/// Notice the `UpTimeCfg` defined as the `Widget::Cfg` for `UpTime`.
/// [`WidgetCfg`]s  exist to let users push [`Widget`]s to [`File`]s
/// and the window through the [`OnFileOpen`] and [`OnWindowOpen`]
/// [hooks]. It lets users configure widgets through methods defined
/// by the widget author.
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{data::PeriodicChecker, prelude::*};
/// # struct UpTime(Text, PeriodicChecker);
/// struct UpTimeCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
///     type Widget = UpTime;
///
///     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (UpTime, PushSpecs) {
///         let checker = PeriodicChecker::new(std::time::Duration::from_secs(1));
///         let widget = UpTime(Text::new(), checker);
///         let specs = PushSpecs::below().with_ver_len(1.0);
///
///         (widget, specs)
///     }
/// }
/// # impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg { UpTimeCfg(PhantomData) }
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text{ &mut self.0 }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// #     fn update(_: &mut Pass, handle: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// # }
/// ```
///
/// The [`build`] method should return 3 objects:
///
/// * The widget itself.
/// * [How] to push the widget into the [`File`]/window.
///
/// Also, note that `UpTimeCfg` includes a [`PhantomData<U>`]. This is
/// done so that the end user does not need to specify a [`Ui`] when
/// using [`WidgetCfg`]s.
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
/// # use std::{sync::OnceLock, time::Instant};
/// # use duat_core::{hook::{self, ConfigLoaded}, ui::Ui};
/// # fn test<U: Ui>() {
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
/// hook::add::<ConfigLoaded>(|_, _| START_TIME.set(Instant::now()).unwrap());
/// # }
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
/// # use std::{marker::PhantomData, sync::OnceLock, time::Instant};
/// # use duat_core::{prelude::*, data::PeriodicChecker};
/// # struct UpTime(Text, PeriodicChecker);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (UpTime, PushSpecs) { todo!() }
/// # }
/// // This was set during the `setup` function
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
/// impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg { UpTimeCfg(PhantomData) }
/// #     fn text(&self) -> &Text { &self.0 }
/// #     fn text_mut(&mut self) -> &mut Text { &mut self.0 }
/// #     fn needs_update(&self) -> bool { todo!(); }
///     // ...
///     fn update(pa: &mut Pass, handle: Handle<Self, U>) {
///         let start = START_TIME.get().unwrap();
///         let elapsed = start.elapsed();
///         let mins = elapsed.as_secs() / 60;
///         let secs = elapsed.as_secs() % 60;
///
///         handle.widget().replace_text::<U>(
///             pa,
///             txt!("[uptime.mins]{mins}m [uptime.secs]{secs}s")
///         );
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
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{data::PeriodicChecker, prelude::*};
/// // This was set during the `setup` function
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// struct UpTime(Text, PeriodicChecker);
///
/// impl<U: Ui> Widget<U> for UpTime {
///     type Cfg = UpTimeCfg<U>;
///
///     fn needs_update(&self) -> bool {
///         // Returns `true` once per second
///         self.1.check()
///     }
///
///     fn update(pa: &mut Pass, handle: Handle<Self, U>) {
///         let start = START_TIME.get().unwrap();
///         let elapsed = start.elapsed();
///         let mins = elapsed.as_secs() / 60;
///         let secs = elapsed.as_secs() % 60;
///
///         handle
///             .widget()
///             .replace_text::<U>(pa, txt!("[uptime.mins]{mins}m [uptime.secs]{secs}s"));
///     }
///
///     fn cfg() -> Self::Cfg {
///         UpTimeCfg(PhantomData)
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
/// struct UpTimeCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
///     type Widget = UpTime;
///
///     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (UpTime, PushSpecs) {
///         // You could imagine how a method on `UpTimeCfg` could
///         // change the periodicity
///         let checker = PeriodicChecker::new(Duration::from_secs(1));
///         let widget = UpTime(Text::new(), checker);
///         let specs = PushSpecs::below().with_ver_len(1.0);
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
/// [`OnFileOpen`]: crate::hook::OnFileOpen
/// [`OnWindowOpen`]: crate::hook::OnWindowOpen\
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
pub trait Widget<U: Ui>: 'static {
    /// The configuration type
    type Cfg: WidgetCfg<U, Widget = Self>
    where
        Self: Sized;

    ////////// Stateful functions

    /// Updates the widget, allowing the modification of its
    /// [`RawArea`]
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
    /// [hooks]: crate::hook
    /// [commands]: crate::cmd
    /// [`Mode`]: crate::mode::Mode
    /// [`update`]: Widget::update
    #[allow(unused)]
    fn update(pa: &mut Pass, handle: Handle<Self, U>)
    where
        Self: Sized;

    /// Actions to do whenever this [`Widget`] is focused
    #[allow(unused)]
    fn on_focus(pa: &mut Pass, handle: Handle<Self, U>)
    where
        Self: Sized,
    {
    }

    /// Actions to do whenever this [`Widget`] is unfocused
    #[allow(unused)]
    fn on_unfocus(pa: &mut Pass, handle: Handle<Self, U>)
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
    /// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// struct MyWidget<U: Ui>(FileHandle<U>);
    ///
    /// impl<U: Ui> Widget<U> for MyWidget<U> {
    /// #   type Cfg = Cfg;
    /// #   fn cfg() -> Self::Cfg { todo!() }
    /// #   fn update(_: &mut Pass, handle: Handle<Self, U>) { todo!() }
    /// #   fn text(&self) -> &Text { todo!() }
    /// #   fn text_mut(&mut self) -> &mut Text { todo!() }
    /// #   fn once() -> Result<(), Text> { todo!() }
    ///     // ...
    ///     fn needs_update(&self) -> bool {
    ///         self.0.has_changed()
    ///     }
    /// }
    /// ```
    ///
    /// In this case, `MyWidget` is telling Duat that it should be
    /// updated whenever the file in the [`FileHandle`] gets changed.
    ///
    /// One exception to this is the [`StatusLine`], which can be
    /// altered if any of its parts get changed, some of them depend
    /// on a [`FileHandle`], but a lot of others depend on checking
    /// functions which might need to be triggered.
    ///
    /// [`FileHandle`]: crate::context::FileHandle
    /// [`StatusLine`]: https://docs.rs/duat-core/latest/duat_utils/widgets/struct.StatusLine.html
    fn needs_update(&self) -> bool;

    /// Returns a [`WidgetCfg`], for use in layout construction
    ///
    /// This function exists primarily so the [`WidgetCfg`]s
    /// themselves don't need to be in scope. You will want to use
    /// these in [hooks] like [`OnFileOpen`]:
    ///
    /// ```rust
    /// # use duat_core::{hook::OnFileOpen, prelude::*};
    /// # struct LineNumbers<U: Ui>(std::marker::PhantomData<U>);
    /// # impl<U: Ui> Widget<U> for LineNumbers<U> {
    /// #     type Cfg = LineNumbersOptions<U>;
    /// #     fn update(_: &mut Pass, handle: Handle<Self, U>) { todo!() }
    /// #     fn needs_update(&self) -> bool { todo!(); }
    /// #     fn cfg() -> Self::Cfg { todo!() }
    /// #     fn text(&self) -> &Text { todo!(); }
    /// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
    /// #     fn once() -> Result<(), Text> { Ok(()) }
    /// # }
    /// # struct LineNumbersOptions<U>(std::marker::PhantomData<U>);
    /// # impl<U> LineNumbersOptions<U> {
    /// #     pub fn align_right(self) -> Self { todo!(); }
    /// #     pub fn align_main_left(self) -> Self { todo!(); }
    /// #     pub fn on_the_right(self) -> Self { todo!(); }
    /// # }
    /// # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
    /// #     type Widget = LineNumbers<U>;
    /// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
    /// #         todo!();
    /// #     }
    /// # }
    /// # fn test<U: Ui>() {
    /// hook::remove("FileWidgets");
    /// hook::add::<OnFileOpen<U>>(|pa, builder| {
    ///     // Screw it, LineNumbers on both sides.
    ///     builder.push(pa, LineNumbers::cfg());
    ///     builder.push(pa, LineNumbers::cfg().on_the_right().align_right());
    /// });
    /// # }
    /// ```
    ///
    /// [hooks]: crate::hook
    /// [`OnFileOpen`]: crate::hook::OnFileOpen
    fn cfg() -> Self::Cfg
    where
        Self: Sized;

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
    fn print_cfg(&self) -> PrintCfg {
        PrintCfg::new()
    }

    /// Prints the widget
    ///
    /// Very rarely shouuld you actually implement this method, one
    /// example of where this is actually implemented is in
    /// [`File::print`], where [`RawArea::print_with`] is called in
    /// order to simultaneously update the list of lines numbers,
    /// for widgets like [`LineNumbers`] to read.
    ///
    /// [`LineNumbers`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    fn print(&mut self, painter: Painter, area: &U::Area) {
        let cfg = self.print_cfg();
        area.print(self.text_mut(), cfg, painter)
    }

    /// Actions taken when this widget opens for the first time
    ///
    /// Examples of things that should go in here are [`form`]
    /// functions, [hooks], [commands] you want executed only once
    ///
    /// [hooks]: crate::hook
    /// [commands]: crate::cmd
    fn once() -> Result<(), Text>
    where
        Self: Sized;
}

/// A configuration struct for a [`Widget`]
///
/// This configuration is used to make adjustments on how a widget
/// will be added to a file or a window. These adjustments are
/// primarily configurations for the widget itself, and to what
/// direction it will be pushed:
///
/// ```rust
/// # use duat_core::{hook::OnFileOpen, prelude::*};
/// # struct LineNumbers<U: Ui> {
/// #     _ghost: std::marker::PhantomData<U>,
/// # }
/// # impl<U: Ui> Widget<U> for LineNumbers<U> {
/// #     type Cfg = LineNumbersOptions<U>;
/// #     fn update(_: &mut Pass, handle: Handle<Self, U>) {}
/// #     fn needs_update(&self) -> bool { todo!(); }
/// #     fn cfg() -> Self::Cfg { todo!(); }
/// #     fn text(&self) -> &Text { todo!(); }
/// #     fn text_mut(&mut self) -> &mut Text { todo!(); }
/// #     fn once() -> Result<(), Text> { Ok(()) }
/// # }
/// # struct LineNumbersOptions<U> {
/// #     _ghost: std::marker::PhantomData<U>,
/// # }
/// # impl<U> LineNumbersOptions<U> {
/// #     pub fn align_right(self) -> Self { todo!(); }
/// #     pub fn align_main_left(self) -> Self { todo!(); }
/// #     pub fn on_the_right(self) -> Self { todo!(); }
/// # }
/// # impl<U: Ui> WidgetCfg<U> for LineNumbersOptions<U> {
/// #     type Widget = LineNumbers<U>;
/// #     fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
/// #         todo!();
/// #     }
/// # }
/// # fn test<U: Ui>() {
/// hook::add::<OnFileOpen<U>>(|pa, builder| {
///     // Change pushing direction to the right.
///     let cfg = LineNumbers::cfg().on_the_right();
///     // Changes to where the numbers will be placed.
///     let cfg = cfg.align_right().align_main_left();
///
///     builder.push(pa, cfg);
/// });
/// # }
/// ```
pub trait WidgetCfg<U: Ui>: Sized {
    /// The [`Widget`] that will be created by this [`WidgetCfg`]
    type Widget: Widget<U, Cfg = Self>;

    /// Builds the [`Widget`] alongside [`PushSpecs`]
    ///
    /// The [`PushSpecs`] are determined by the [`WidgetCfg`] itself,
    /// and the end user is meant to change it by public facing
    /// functions in the [`WidgetCfg`]. This is to prevent nonsensical
    /// [`Widget`] pushing, like [`LineNumbers`] on the bottom of a
    /// [`File`], for example.
    ///
    /// [`LineNumbers`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    fn build(self, pa: &mut Pass, handle: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs);
}

/// Elements related to the [`Widget`]s
#[derive(Clone)]
pub(crate) struct Node<U: Ui> {
    widget: RwData<dyn Widget<U>>,
    area: U::Area,
    mask: Rc<Cell<&'static str>>,
    related_widgets: Related<U>,
    update: fn(&Self, &mut Pass),
    print: fn(&Self, &mut Pass),
    on_focus: fn(&Self, &mut Pass),
    on_unfocus: fn(&Self, &mut Pass),
}

impl<U: Ui> Node<U> {
    /// Returns a new [`Node`]
    pub(crate) fn new<W: Widget<U>>(widget: RwData<dyn Widget<U>>, area: U::Area) -> Self {
        let related_widgets = (TypeId::of::<W>() == TypeId::of::<File<U>>()).then(RwData::default);

        Self {
            widget,
            area,
            mask: Rc::new(Cell::new("")),
            related_widgets,
            update: Self::update_fn::<W>,
            print: Self::print_fn::<W>,
            on_focus: Self::on_focus_fn::<W>,
            on_unfocus: Self::on_unfocus_fn::<W>,
        }
    }

    ////////// Reading and parts acquisition

    pub(crate) fn read_as<W: 'static, Ret>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&W) -> Ret,
    ) -> Option<Ret> {
        self.widget.clone().read_as(pa, f)
    }

    /// The [`Widget`] of this [`Node`]
    pub(crate) fn widget(&self) -> &RwData<dyn Widget<U>> {
        &self.widget
    }

    pub(crate) fn area(&self) -> &U::Area {
        &self.area
    }

    /// Returns the downcast ref of this [`Widget`].
    pub(crate) fn try_downcast<W: 'static>(&self) -> Option<RwData<W>> {
        self.widget.try_downcast()
    }

    pub(crate) fn parts(
        &self,
    ) -> (
        &RwData<dyn Widget<U>>,
        &<U as Ui>::Area,
        &Rc<Cell<&'static str>>,
        &Related<U>,
    ) {
        (&self.widget, &self.area, &self.mask, &self.related_widgets)
    }

    pub(crate) fn as_file(&self) -> Option<FileParts<U>> {
        self.widget.try_downcast().map(|file: RwData<File<U>>| {
            (
                Handle::from_parts(file, self.area.clone(), self.mask.clone()),
                self.related_widgets.clone().unwrap(),
            )
        })
    }

    pub(crate) fn related_widgets(&self) -> Option<&RwData<Vec<Node<U>>>> {
        self.related_widgets.as_ref()
    }

    ////////// Querying functions

    /// Wether the value within is `W`
    pub(crate) fn data_is<W: 'static>(&self) -> bool {
        self.widget.data_is::<W>()
    }

    /// Wether this and [`RwData`] point to the same value
    pub(crate) fn ptr_eq<W: ?Sized>(&self, other: &RwData<W>) -> bool {
        self.widget.ptr_eq(other)
    }

    /// Wether this [`Widget`] needs to be updated
    pub(crate) fn needs_update(&self, pa: &Pass) -> bool {
        self.area.has_changed()
            || self.widget.has_changed()
            || self.widget.read(pa, |w| w.needs_update())
    }

    ////////// Eventful functions

    /// Updates and prints this [`Node`]
    pub(crate) fn update_and_print(&self, pa: &mut Pass) {
        (self.update)(self, pa);

        {
            let mut widget = self.widget.acquire_mut(pa);
            let cfg = widget.print_cfg();
            widget.text_mut().add_selections(&self.area, cfg);

            if self.area.print_info() != <U::Area as RawArea>::PrintInfo::default() {
                widget.text_mut().update_bounds();
            }
        };

        (self.print)(self, pa);

        let mut widget = self.widget.acquire_mut(pa);
        let cfg = widget.print_cfg();
        widget.text_mut().remove_selections(&self.area, cfg);
    }

    /// What to do when focusing
    pub(crate) fn on_focus(&self, pa: &mut Pass) {
        self.area.set_as_active();
        (self.on_focus)(self, pa)
    }

    /// What to do when unfocusing
    pub(crate) fn on_unfocus(&self, pa: &mut Pass) {
        (self.on_unfocus)(self, pa)
    }

    /// Static dispatch inner update function
    fn update_fn<W: Widget<U>>(&self, pa: &mut Pass) {
        let widget = self.widget.try_downcast::<W>().unwrap();
        let handle = Handle::from_parts(widget, self.area.clone(), self.mask.clone());
        Widget::update(pa, handle);
    }

    /// Static dispatch inner print function
    fn print_fn<W: Widget<U>>(&self, pa: &mut Pass) {
        let painter = form::painter_with_mask::<W>(self.mask.get());
        let mut widget = self.widget.acquire_mut(pa);

        widget.print(painter, &self.area);
    }

    /// Static dispatch inner update on_focus
    fn on_focus_fn<W: Widget<U>>(&self, pa: &mut Pass) {
        self.area.set_as_active();
        let widget: RwData<W> = self.widget.try_downcast().unwrap();

        let handle = Handle::from_parts(widget, self.area.clone(), self.mask.clone());
        hook::trigger(pa, FocusedOn(handle.clone()));

        Widget::on_focus(pa, handle);
    }

    /// Static dispatch inner update on_unfocus
    fn on_unfocus_fn<W: Widget<U>>(&self, pa: &mut Pass) {
        let widget: RwData<W> = self.widget.try_downcast().unwrap();

        let handle = Handle::from_parts(widget, self.area.clone(), self.mask.clone());
        hook::trigger(pa, UnfocusedFrom(handle.clone()));

        Widget::on_unfocus(pa, handle);
    }
}

impl<U: Ui> PartialEq for Node<U> {
    fn eq(&self, other: &Self) -> bool {
        self.widget.ptr_eq(&other.widget)
    }
}

impl<U: Ui> Eq for Node<U> {}

/// [`Widget`]s related to another [`Widget`]
pub(crate) type Related<U> = Option<RwData<Vec<Node<U>>>>;
