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
//! [`CmdLine`], which can receive user mode, and [`StatusLine`]
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
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
//! [`OnFileOpen`]: crate::hooks::OnFileOpen
//! [`OnWindowOpen`]: crate::hooks::OnWindowOpen
//! [`Constraint`]: crate::ui::Constraint
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

pub use self::{
    command_line::{CmdLine, CmdLineCfg, CmdLineMode, IncSearch, RunCommands, ShowNotifications},
    file::{File, FileCfg, PathKind},
    line_numbers::{LineNum, LineNumbers, LineNumbersCfg},
    status_line::{State, StatusLine, StatusLineCfg, common, status},
};
use crate::{
    cfg::PrintCfg,
    context::FileParts,
    data::{Data, RwData},
    form,
    hooks::{self, FocusedOn, UnfocusedFrom},
    mode::Cursors,
    text::Text,
    ui::{Area, PushSpecs, Ui},
};

mod command_line;
mod file;
mod line_numbers;
mod status_line;

/// An area where [`Text`] will be printed to the screen
///
/// Most widgets are supposed to be passive widgets, that simply show
/// information about the current state of Duat. If you want to see
/// how to create a widget that takes in mode, see [`Mode`]
///
/// In order to show that information, widgets make use of [`Text`],
/// which can show stylized text, buttons, and all sorts of other
/// stuff.
///
/// For a demonstration on how to create a widget, I will create a
/// widget that shows the uptime, in seconds, for Duat.
///
/// ```rust
/// # use duat_core::text::Text;
/// struct UpTime(Text);
/// ```
///
/// In order to be a proper widget, it must have a [`Text`] to
/// display. Next, I must implement [`Widget`]:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     hooks, periodic_checker, text::Text, ui::{PushSpecs, Ui},
/// #     widgets::{Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(self,_: bool) -> (Self::Widget, impl Fn() -> bool + 'static, PushSpecs) {
/// #         (UpTime(Text::new()), || false, PushSpecs::below())
/// #     }
/// # }
/// impl<U: Ui> Widget<U> for UpTime {
///     type Cfg = UpTimeCfg<U>;
///
///     fn cfg() -> Self::Cfg {
///         UpTimeCfg(PhantomData)
///     }
///     // ...
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
/// #     fn text_mut(&mut self) -> &mut Text {
/// #         &mut self.0
/// #     }
/// #     fn once() -> Result<(), duat_core::Error<()>> {
/// #         Ok(())
/// #     }
/// }
/// ```
///
/// Note the [`Cfg`](Widget::Cfg) type, and the [`cfg`] method.
/// These exist to give the user the ability to modify the widgets
/// before they are pushed. The `Cfg` type, which implements
/// [`WidgetCfg`] is the thing that will actually construct the
/// widget. Let's look at `UpTimeCfg`:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     hooks, periodic_checker, text::Text, ui::{PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// struct UpTimeCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
///     type Widget = UpTime;
///
///     fn build(self, on_file: bool) -> (UpTime, impl Fn() -> bool + 'static, PushSpecs) {
///         let widget = UpTime(Text::new());
///         let checker = periodic_checker(Duration::new(1, 0));
///         let specs = PushSpecs::below().with_ver_len(1.0);
///
///         (widget, checker, specs)
///     }
/// }
/// # impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         UpTimeCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
/// #     fn text_mut(&mut self) -> &mut Text{
/// #         &mut self.0
/// #     }
/// #     fn once() -> Result<(), duat_core::Error<()>> {
/// #         Ok(())
/// #     }
/// # }
/// ```
///
/// The [`build`] method should return 3 objects:
///
/// * The widget itself.
/// * A checker function that tells Duat when to update the widget.
/// * [How] to push the widget into the [`File`]/window.
///
/// In this case, [`periodic_checker`] returns a function that returns
/// `true` every `duration` that passes.
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
/// # use duat_core::{hooks::{self, ConfigLoaded}, ui::Ui};
/// # fn test<U: Ui>() {
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
/// hooks::add::<ConfigLoaded>(|_| START_TIME.set(Instant::now()).unwrap());
/// # }
/// ```
///
/// I could put this code inside the [`cfg`] method, however, by
/// doing so, it will be called every time this widget is added to the
/// ui.
///
/// Instead, I'll put it in [`Widget::once`]. This function is
/// only triggered once, no matter how many times the widget is added
/// to the ui:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     form::{self, Form}, hooks::{self, ConfigLoaded}, periodic_checker,
/// #     text::Text, ui::{PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(self, on_file: bool) -> (UpTime, impl Fn() -> bool + 'static, PushSpecs) {
/// #         (UpTime(Text::new()), || false, PushSpecs::below())
/// #     }
/// # }
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         UpTimeCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
/// #     fn text_mut(&mut self) -> &mut Text {
/// #         &mut self.0
/// #     }
///     // ...
///     fn once() -> Result<(), duat_core::Error<()>> {
///         hooks::add::<ConfigLoaded>(|_| {
///             START_TIME.set(Instant::now()).unwrap()
///         });
///         form::set_weak("UpTime", Form::cyan());
///         Ok(())
///     }
/// }
/// ```
///
/// I also added the `"UpTime"` [`Form`], which will be used by the
/// widget when it is updated. When adding form, you should use the
/// [`form::set_weak*`] functions, in order to not interfere with
/// the configuration crate.
///
/// Next, I need to implement the [`update`] method, which will simply
/// format the [`Text`] into a readable format:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     hooks, periodic_checker, text::{Text, text}, ui::{PushSpecs, Ui},
/// #     widgets::{Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(self, on_file: bool) -> (UpTime, impl Fn() -> bool + 'static, PushSpecs) {
/// #         (UpTime(Text::new()), || false, PushSpecs::below())
/// #     }
/// # }
/// # static START_TIME: OnceLock<Instant> = OnceLock::new();
/// impl<U: Ui> Widget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         UpTimeCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
/// #     fn text_mut(&mut self) -> &mut Text {
/// #         &mut self.0
/// #     }
///     // ...
///     fn update(&mut self, _area: &U::Area) {
///         let Some(start) = START_TIME.get() else {
///             return;
///         };
///         let duration = start.elapsed();
///         let mins = duration.as_secs() / 60;
///         let secs = duration.as_secs() % 60;
///         self.0 = text!([UpTime] mins "m " secs "s");
///     }
///     // ...
///     fn once() -> Result<(), duat_core::Error<()>> {
///         Ok(())
///     }
/// }
/// ```
///
/// [`Mode`]: crate::mode::Mode
/// [`cfg`]: Widget::cfg
/// [`build`]: WidgetCfg::build
/// [How]: PushSpecs
/// [`periodic_checker`]: crate::periodic_checker
/// [`PhantomData<U>`]: std::marker::PhantomData
/// [`Instant`]: std::time::Instant
/// [`ConfigLoaded`]: crate::hooks::ConfigLoaded
/// [`update`]: Widget::update
/// [`Form`]: crate::form::Form
/// [`form::set_weak*`]: crate::form::set_weak
/// [`text!`]: crate::text::text
pub trait Widget<U>: Send + Sync + 'static
where
    U: Ui,
{
    /// The configuration type
    type Cfg: WidgetCfg<U, Widget = Self>
    where
        Self: Sized;

    /// Returns a [`WidgetCfg`], for use in layout construction
    ///
    /// This function exists primarily so the [`WidgetCfg`]s
    /// themselves don't need to be in scope. You will want to use
    /// these in [hooks] like [`OnFileOpen`]:
    ///
    /// ```rust
    /// # use duat_core::{
    /// #     hooks::{self, OnFileOpen},
    /// #     ui::{FileBuilder, Ui},
    /// #     widgets::{File, LineNumbers, Widget, common::selections_fmt, status},
    /// # };
    /// # fn test<U: Ui>() {
    /// hooks::remove("FileWidgets");
    /// hooks::add::<OnFileOpen<U>>(|builder| {
    ///     // Screw it, LineNumbers on both sides.
    ///     builder.push(LineNumbers::cfg());
    ///     builder.push(LineNumbers::cfg().on_the_right().align_right());
    /// });
    /// # }
    /// ```
    ///
    /// [`OnFileOpen`]: crate::hooks::OnFileOpen
    fn cfg() -> Self::Cfg
    where
        Self: Sized;

    /// Updates the widget, allowing the modification of its [`Area`]
    ///
    /// There are a few contexts in which this function is triggered:
    ///
    /// * A key was sent to the widget, if it is an [`Widget`]
    /// * It was modified externally by something like [`IncSearch`]
    /// * The window was resized, so all widgets must be reprinted
    ///
    /// In this function, the text should be updated to match its new
    /// conditions, or request changes to its [`Area`]. As an example,
    /// the [`LineNumbers`] widget asks for [more or less width],
    /// depending on the number of lines in the file, in order
    /// to show an appropriate number of digits.
    ///
    /// [`Session`]: crate::session::Session
    /// [more or less width]: Area::constrain_hor
    fn update(&mut self, _area: &U::Area) {}

    /// The text that this widget prints out
    fn text(&self) -> &Text;

    /// A mutable reference to the [`Text`] that is printed
    fn text_mut(&mut self) -> &mut Text;

    /// The [`Cursors`] that are used on the [`Text`], if they exist
    fn cursors(&self) -> Option<&Cursors> {
        self.text().cursors()
    }

    /// A mutable reference to the [`Cursors`], if they exist
    fn cursors_mut(&mut self) -> Option<&mut Cursors> {
        self.text_mut().cursors_mut()
    }

    /// Actions to do whenever this [`Widget`] is focused.
    #[allow(unused)]
    fn on_focus(&mut self, area: &U::Area) {}

    /// Actions to do whenever this [`Widget`] is unfocused.
    #[allow(unused)]
    fn on_unfocus(&mut self, area: &U::Area) {}

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
    /// [`File::print`], where [`Area::print_with`] is called in order
    /// to simultaneously update the list of lines numbers, for
    /// widgets like [`LineNumbers`] to read.
    fn print(&mut self, area: &U::Area) {
        let cfg = self.print_cfg();
        area.print(self.text_mut(), cfg, form::painter())
    }

    /// Actions taken when this widget opens for the first time
    ///
    /// Examples of things that should go in here are [`form`]
    /// functions, [hooks], [commands] you want executed only once
    ///
    /// [commands]: crate::cmd
    fn once() -> Result<(), crate::Error<()>>
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
/// # use duat_core::{
/// #     hooks::{self, OnFileOpen},
/// #     ui::Ui,
/// #     widgets::{LineNumbers, Widget},
/// # };
/// # fn test<U: Ui>() {
/// hooks::add::<OnFileOpen<U>>(|builder| {
///     // Change pushing direction to the right.
///     let cfg = LineNumbers::cfg().on_the_right();
///     // Changes to where the numbers will be placed.
///     let cfg = cfg.align_right().align_main_left();
///
///     builder.push(cfg);
/// });
/// # }
/// ```
pub trait WidgetCfg<U>: Sized
where
    U: Ui,
{
    type Widget: Widget<U>;

    fn build(self, on_file: bool) -> (Self::Widget, impl CheckerFn, PushSpecs);
}

// Elements related to the [`Widget`]s
pub struct Node<U: Ui> {
    widget: RwData<dyn Widget<U>>,
    area: U::Area,

    checker: Arc<dyn Fn() -> bool + Send + Sync>,
    busy_updating: Arc<AtomicBool>,

    related_widgets: Option<RwData<Vec<Node<U>>>>,
    on_focus: fn(&Node<U>),
    on_unfocus: fn(&Node<U>),
}

impl<U: Ui> Node<U> {
    pub fn new<W: Widget<U>>(
        widget: RwData<dyn Widget<U>>,
        area: U::Area,
        checker: impl CheckerFn,
    ) -> Self {
        let related_widgets = widget.data_is::<File>().then(RwData::default);

        Self {
            widget,
            area,

            checker: Arc::new(checker),
            busy_updating: Arc::new(AtomicBool::new(false)),

            related_widgets,
            on_focus: Self::on_focus_fn::<W>,
            on_unfocus: Self::on_unfocus_fn::<W>,
        }
    }

    pub fn widget(&self) -> &RwData<dyn Widget<U>> {
        &self.widget
    }

    /// Returns the downcast ref of this [`Widget`].
    pub fn try_downcast<W>(&self) -> Option<RwData<W>> {
        self.widget.try_downcast()
    }

    pub fn data_is<W: 'static>(&self) -> bool {
        self.widget.data_is::<W>()
    }

    pub fn update_and_print(&self) {
        self.busy_updating.store(true, Ordering::Release);

        let mut widget = self.widget.raw_write();
        widget.update(&self.area);
        widget.print(&self.area);

        self.busy_updating.store(false, Ordering::Release);
    }

    pub fn inspect_as<W: 'static, B>(&self, f: impl FnOnce(&W) -> B) -> Option<B> {
        self.widget.inspect_as(f)
    }

    pub fn ptr_eq<W, D>(&self, other: &D) -> bool
    where
        W: ?Sized,
        D: Data<W> + ?Sized,
    {
        self.widget.ptr_eq(other)
    }

    pub fn needs_update(&self) -> bool {
        if !self.busy_updating.load(Ordering::Acquire) {
            (self.checker)() || self.area.has_changed()
        } else {
            false
        }
    }

    pub(crate) fn update(&self) {
        self.widget.raw_write().update(&self.area)
    }

    pub(crate) fn parts(&self) -> (&RwData<dyn Widget<U>>, &U::Area) {
        (&self.widget, &self.area)
    }

    pub(crate) fn as_file(&self) -> Option<FileParts<U>> {
        self.widget.try_downcast().map(|file| {
            (
                file,
                self.area.clone(),
                self.related_widgets.clone().unwrap(),
            )
        })
    }

    pub(crate) fn on_focus(&self) {
        self.area.set_as_active();
        (self.on_focus)(self)
    }

    pub(crate) fn on_unfocus(&self) {
        (self.on_unfocus)(self)
    }

    pub(crate) fn raw_inspect<B>(&self, f: impl FnOnce(&dyn Widget<U>) -> B) -> B {
        let widget = self.widget.raw_read();
        f(&*widget)
    }

    pub(crate) fn area(&self) -> &U::Area {
        &self.area
    }

    pub(crate) fn related_widgets(&self) -> Option<&RwData<Vec<Node<U>>>> {
        self.related_widgets.as_ref()
    }

    fn on_focus_fn<W: Widget<U>>(&self) {
        self.widget.mutate(|widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(&self.area, cfg);
            widget.on_focus(&self.area);
        });

        self.area.set_as_active();
        let widget = self.widget.try_downcast().unwrap();

        hooks::trigger::<FocusedOn<W, U>>((widget, self.area.clone()));
    }

    fn on_unfocus_fn<W: Widget<U>>(&self) {
        self.widget.mutate(|widget| {
            let cfg = widget.print_cfg();
            widget.text_mut().remove_cursors(&self.area, cfg);
            widget.on_unfocus(&self.area);
        });

        let widget = self.widget.try_downcast().unwrap();

        hooks::trigger::<UnfocusedFrom<W, U>>((widget, self.area.clone()));
    }
}

impl<U: Ui> Clone for Node<U> {
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            area: self.area.clone(),
            checker: self.checker.clone(),
            busy_updating: self.busy_updating.clone(),
            related_widgets: self.related_widgets.clone(),
            on_focus: self.on_focus,
            on_unfocus: self.on_unfocus,
        }
    }
}

pub trait CheckerFn = Fn() -> bool + 'static + Send + Sync;
