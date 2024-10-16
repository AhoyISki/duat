//! APIs for the construction of widgets, and a few common ones.
//!
//! This module declares two traits for widgets, [`PassiveWidget`]s
//! and [`ActiveWidget`]s. [`PassiveWidget`]s simply show information,
//! and cannot receive input or be focused. [`ActiveWidget`]s can be
//! modified by an external [`InputMethod`], thus they react to user
//! input whenever they are in focus.
//!
//! These widgets will be used in one of three contexts:
//!
//! - Being pushed to a file via the hook [`OnFileOpen`];
//! - Being pushed to the outer edges via [`OnWindowOpen`];
//! - Being pushed to popup widgets via [`OnPopupOpen`];
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
//! The module also provides 4 native widgets, [`StatusLine`] and
//! [`LineNumbers`], which are [`PassiveWidget`]s, and
//! [`File`] and [`CommandLine`] which are [`ActiveWidget`]s.
//!
//! These 4 widgets are supposed to be universal, not needing a
//! specific [`Ui`] implementation to work. In contrast, you can
//! create widgets for specific [`Ui`]s. As an example, the
//! [`duat-term`] crate, which is a terminal [`Ui`] implementation for
//! Duat, defines the [`VertRule`] widget, which is a separator that
//! only makes sense in the context of a terminal.
//!
//! This module also describes a [`WidgetCfg`], which is an optional
//! struct for widgets that details how they can be customized.
//!
//! [`duat-term`]: https://docs.rs/duat-term/latest/duat_term/
//! [`VertRule`]: https://docs.rs/duat-term/latest/duat_term/struct.VertRule.html
//! [`OnFileOpen`]: crate::hooks::OnFileOpen
//! [`OnWindowOpen`]: crate::hooks::OnWindowOpen
//! [`Constraint`]: crate::ui::Constraint
use std::{any::TypeId, sync::Arc};

use crossterm::event::KeyEvent;

pub use self::{
    command_line::{
        CommandLine, CommandLineCfg, CommandLineMode, IncSearch, RunCommands, ShowNotifications,
    },
    file::{File, FileCfg},
    line_numbers::{LineNumbers, LineNumbersCfg},
    status_line::{State, StatusLine, StatusLineCfg, common, status},
};
use crate::{
    data::{Context, Data, RwData, RwLock},
    duat_name, forms,
    hooks::{self, FocusedOn, KeySent, KeySentTo, UnfocusedFrom},
    input::InputMethod,
    text::{PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
};

mod command_line;
mod file;
mod line_numbers;
mod status_line;

/// An area where [`Text`] will be printed to the screen
///
/// Most widgets are supposed to be passive widgets, that simply show
/// information about the current state of Duat.
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
/// display. Next, I must implement [`PassiveWidget`]:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     data::Context, hooks, periodic_checker, text::Text, ui::{PushSpecs, Ui},
/// #     widgets::{PassiveWidget, Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(
/// #         self,
/// #         _: Context<U>,
/// #         _: bool,
/// #     ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
/// #         let widget = Widget::passive(UpTime(Text::new()));
/// #         (widget, || false, PushSpecs::below())
/// #     }
/// # }
/// impl<U: Ui> PassiveWidget<U> for UpTime {
///     type Cfg = UpTimeCfg<U>;
///
///     fn cfg() -> Self::Cfg {
///         UpTimeCfg(PhantomData)
///     }
///     // ...
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
/// #     fn once(context: Context<U>) {}
/// }
/// ```
///
/// Note the [`Cfg`](PassiveWidget::Cfg) type, and the [`cfg`] method.
/// These exist to give the user the ability to modify the widgets
/// before they are pushed. The `Cfg` type, which implements
/// [`WidgetCfg`] is the thing that will actually construct the
/// widget. Let's look at `UpTimeCfg`:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     data::Context, hooks, periodic_checker, text::Text,
/// #     ui::{PushSpecs, Ui}, widgets::{PassiveWidget, Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// struct UpTimeCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
///     type Widget = UpTime;
///
///     fn build(
///         self,
///         context: Context<U>,
///         on_file: bool,
///     ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
///         let widget = UpTime(Text::new());
///         let checker = periodic_checker(Duration::new(1, 0));
///         let specs = PushSpecs::below().with_ver_len(1.0);
///
///         (Widget::passive(widget), checker, specs)
///     }
/// }
/// # impl<U: Ui> PassiveWidget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         UpTimeCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
/// #     fn once(context: Context<U>) {}
/// # }
/// ```
///
/// The [`build`] method should return 3 objects:
///
/// * The widget itself, can either be [passive] or [active].
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
/// Now, there are some other methods from [`PassiveWidget`] that need
/// to be implemented for this to work. First of all, there needs to
/// be a starting [`Instant`] to compare with the current moment in
/// time.
///
/// The best time to do something like this is after Duat is done with
/// initial setup. This happens when the [`SessionStarted`] hook is
/// triggered.
///
/// ```rust
/// # use std::{sync::OnceLock, time::Instant};
/// # use duat_core::{hooks::{self, SessionStarted}, ui::Ui};
/// # fn test<U: Ui>() {
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
/// hooks::add::<SessionStarted<U>>(|_context| {
///     START_TIME.set(Instant::now()).unwrap();
/// });
/// # }
/// ```
///
/// I could put this code inside the [`cfg`] method, however, by
/// doing so, it will be called every time this widget is added to the
/// ui.
///
/// Instead, I'll put it in [`PassiveWidget::once`]. This function is
/// only triggered once, no matter how many times the widget is added
/// to the ui:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     data::Context, forms::{self, Form}, hooks::{self, SessionStarted}, periodic_checker,
/// #     text::Text, ui::{PushSpecs, Ui}, widgets::{PassiveWidget, Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(
/// #         self,
/// #         context: Context<U>,
/// #         on_file: bool,
/// #     ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
/// #         let widget = Widget::passive(UpTime(Text::new()));
/// #         (widget, || false, PushSpecs::below())
/// #     }
/// # }
/// static START_TIME: OnceLock<Instant> = OnceLock::new();
///
/// impl<U: Ui> PassiveWidget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         UpTimeCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.0
/// #     }
///     // ...
///     fn once(context: Context<U>) {
///         hooks::add::<SessionStarted<U>>(|_context| {
///             START_TIME.set(Instant::now()).unwrap();
///         });
///         forms::set_weak("UpTime", Form::cyan());
///     }
/// }
/// ```
///
/// I also added the `"UpTime"` [`Form`], which will be used by the
/// widget when it is updated. When adding forms, you should use the
/// [`forms::set_weak*`] functions, in order to not interfere with
/// the configuration crate.
///
/// Next, I need to implement the [`update`] method, which will simply
/// format the [`Text`] into a readable format:
///
/// ```rust
/// # use std::{marker::PhantomData, sync::OnceLock, time::{Duration, Instant}};
/// # use duat_core::{
/// #     data::Context, hooks, periodic_checker, text::{Text, text}, ui::{PushSpecs, Ui},
/// #     widgets::{PassiveWidget, Widget, WidgetCfg},
/// # };
/// # struct UpTime(Text);
/// # struct UpTimeCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for UpTimeCfg<U> {
/// #     type Widget = UpTime;
/// #     fn build(
/// #         self,
/// #         context: Context<U>,
/// #         on_file: bool,
/// #     ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
/// #         let widget = Widget::passive(UpTime(Text::new()));
/// #         (widget, || false, PushSpecs::below())
/// #     }
/// # }
/// # static START_TIME: OnceLock<Instant> = OnceLock::new();
/// impl<U: Ui> PassiveWidget<U> for UpTime {
/// #     type Cfg = UpTimeCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         UpTimeCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.0
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
/// #   fn once(context: Context<U>) {}
/// }
/// ```
///
/// [`cfg`]: PassiveWidget::cfg
/// [passive]: Widget::passive
/// [active]: Widget::active
/// [`build`]: WidgetCfg::build
/// [How]: PushSpecs
/// [`periodic_checker`]: crate::periodic_checker
/// [`PhantomData<U>`]: std::marker::PhantomData
/// [`Instant`]: std::time::Instant
/// [`SessionStarted`]: crate::hooks::SessionStarted
/// [`update`]: PassiveWidget::update
/// [`Form`]: crate::forms::Form
/// [`forms::set_weak*`]: crate::forms::set_weak
/// [`text!`]: crate::text::text
pub trait PassiveWidget<U>: Send + Sync + 'static
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
    /// #     widgets::{File, LineNumbers, PassiveWidget, common::selections_fmt, status},
    /// # };
    /// # fn test<U: Ui>() {
    /// hooks::remove_group("FileWidgets");
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
    /// * A key was sent to the widget, if it is an [`ActiveWidget`]
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
        area.print(self.text(), self.print_cfg(), forms::painter())
    }

    /// Actions taken when this widget opens for the first time
    ///
    /// Examples of things that should go in here are [`forms`]
    /// functions, [hooks], [commands] you want executed only once
    ///
    /// [commands]: crate::commands
    fn once(context: Context<U>)
    where
        Self: Sized;
}

/// A configuration struct for a [passive] or an [active] widget
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
/// #     widgets::{LineNumbers, PassiveWidget},
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
///
/// [passive]: PassiveWidget
/// [active]: ActiveWidget
pub trait WidgetCfg<U>: Sized
where
    U: Ui,
{
    type Widget: PassiveWidget<U>;

    fn build(
        self,
        context: Context<U>,
        on_file: bool,
    ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs);
}

/// A widget that can be modified by input
///
/// Here is how you can create an [`ActiveWidget`]. First, create the
/// struct that will become said widget, see [`PassiveWidget`] for
/// what you need:
///
/// ```rust
/// # use duat_core::text::Text;
/// #[derive(Default)]
/// struct Menu {
///     text: Text,
///     selected_entry: usize,
///     active_etry: Option<usize>,
/// }
/// ```
/// In this widget, I will create a menu whose entries can be selected
/// by an [`InputMethod`].
///
/// Let's say that said menu has five entries, and one of them can be
/// active at a time:
///
/// ```rust
/// # #![feature(let_chains)]
/// # use duat_core::text::{Text, text, AlignCenter};
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// impl Menu {
///     pub fn shift_selection(&mut self, shift: i32) {
///         let selected = self.selected_entry as i32 + shift;
///         self.selected_entry = if selected < 0 {
///             4
///         } else if selected > 4 {
///             0
///         } else {
///             selected as usize
///         };
///     }
///
///     pub fn toggle(&mut self) {
///         self.active_entry = match self.active_entry {
///             Some(entry) if entry == self.selected_entry => None,
///             Some(_) | None => Some(self.selected_entry),
///         };
///     }
///
///     fn build_text(&mut self) {
///         let mut builder = Text::builder();
///         text!(builder, AlignCenter);
///
///         for i in 0..5 {
///             if let Some(active) = self.active_entry
///                 && active == i
///             {
///                 if self.selected_entry == i {
///                     text!(builder, [MenuSelActive])
///                 } else {
///                     text!(builder, [MenuActive])
///                 }
///             } else if self.selected_entry == i {
///                 text!(builder, [MenuSelected]);
///             } else {
///                 text!(builder, [MenuInactive]);
///             }
///
///             text!(builder, "Entry " i);
///         }
///
///         self.text = builder.finish();
///     }
/// }
/// ```
///
/// By making `shift_selection` and `toggle` `pub`, I can allow an end
/// user to create their own [`InputMethod`] for this widget.
///
/// Let's say that I have created an [`InputMethod`] `MenuInput` for
/// the `Menu`. This input method is actually the one that is
/// documented on the documentation entry for [`InputMethod`], you can
/// check it out next, to see how that was handled.
///
/// Now I'll implement [`PassiveWidget`]:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::{
/// #     data::{Context, RwData}, input::{InputMethod, KeyEvent}, forms::{self, Form},
/// #     text::{text, Text}, ui::{PushSpecs, Ui},
/// #     widgets::{ActiveWidget, PassiveWidget, Widget, WidgetCfg},
/// # };
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # impl Menu {
/// #     fn build_text(&mut self) {
/// #         todo!();
/// #     }
/// # }
/// # #[derive(Default)]
/// # struct MenuInput;
/// # impl<U: Ui> InputMethod<U> for MenuInput {
/// #     type Widget = Menu;
/// #     fn send_key(&mut self, _: KeyEvent, _: &RwData<Menu>, _: &U::Area, _: Context<U>) {
/// #         todo!();
/// #     }
/// # }
/// struct MenuCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
///     type Widget = Menu;
///
///     fn build(
///         self,
///         context: Context<U>,
///         on_file: bool,
///     ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
///         let checker = || false;
///
///         let mut widget = Menu::default();
///         widget.build_text();
///
///         let input = MenuInput::default();
///         let specs = PushSpecs::left().with_hor_len(10.0).with_ver_len(5.0);
///
///         (Widget::active(widget, input), checker, specs)
///     }
/// }
///
/// impl<U: Ui> PassiveWidget<U> for Menu {
///     type Cfg = MenuCfg<U>;
///
///     fn cfg() -> Self::Cfg {
///         MenuCfg(PhantomData)
///     }
///
///     fn text(&self) -> &Text {
///         &self.text
///     }
///
///     fn once(_context: Context<U>) {
///         forms::set_weak("MenuInactive", "Inactive");
///         forms::set_weak("MenuSelected", "Inactive");
///         forms::set_weak("MenuActive", Form::blue());
///         forms::set_weak("MenuSelActive", Form::blue());
///     }
/// }
/// # impl<U: Ui> ActiveWidget<U> for Menu {
/// #     fn text_mut(&mut self) -> &mut Text {
/// #         &mut self.text
/// #     }
/// # }
/// ```
///
/// We can use `let checker = || false` here, since [`ActiveWidget`]s
/// get automatically updated whenever they are focused and a key is
/// sent.
///
/// Now, all that is needed is an implementation of [`ActiveWidget`]:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::{
/// #     data::{Context, RwData}, input::{InputMethod, KeyEvent}, forms::{self, Form},
/// #     text::{text, Text}, ui::{PushSpecs, Ui},
/// #     widgets::{ActiveWidget, PassiveWidget, Widget, WidgetCfg},
/// # };
/// # #[derive(Default)]
/// # struct Menu {
/// #     text: Text,
/// #     selected_entry: usize,
/// #     active_entry: Option<usize>,
/// # }
/// # struct MenuCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for MenuCfg<U> {
/// #     type Widget = Menu;
/// #     fn build(
/// #         self,
/// #         context: Context<U>,
/// #         on_file: bool,
/// #     ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs) {
/// #         (Widget::passive(Menu::default()), || false, PushSpecs::left())
/// #     }
/// # }
/// # impl<U: Ui> PassiveWidget<U> for Menu {
/// #     type Cfg = MenuCfg<U>;
/// #     fn cfg() -> Self::Cfg {
/// #         MenuCfg(PhantomData)
/// #     }
/// #     fn text(&self) -> &Text {
/// #         &self.text
/// #     }
/// #     fn once(_context: Context<U>) {}
/// # }
/// impl<U: Ui> ActiveWidget<U> for Menu {
///     fn text_mut(&mut self) -> &mut Text {
///         &mut self.text
///     }
///
///     fn on_focus(&mut self, _area: &U::Area) {
///         forms::set_weak("MenuInactive", "Default");
///         forms::set_weak("MenuSelected", Form::new().on_grey());
///         forms::set_weak("MenuSelActive", Form::blue().on_grey());
///     }
///
///     fn on_unfocus(&mut self, _area: &U::Area) {
///         forms::set_weak("MenuInactive", "Inactive");
///         forms::set_weak("MenuSelected", "Inactive");
///         forms::set_weak("MenuSelActive", Form::blue());
///     }
/// }
/// ```
///
/// Notice that [`ActiveWidget`]s have [`on_focus`] and [`on_unfocus`]
/// methods, so you can make something happen whenever your widget
/// becomes focused or unfocused. These methods also provide you with
/// the [`Area`], so you can do things like [resizing] it.
///
/// In this case, I chose to replace the [`Form`]s with "inactive"
/// variants, to visually show when the widget is not active.
///
/// Do also note that [`on_focus`] and [`on_unfocus`] are optional
/// methods.
///
/// [`Cursor`]: crate::input::Cursor
/// [`print`]: PassiveWidget::print
/// [`on_focus`]: ActiveWidget::on_focus
/// [`on_unfocus`]: ActiveWidget::on_unfocus
/// [resizing]: Area::constrain_ver
/// [`Form`]: crate::forms::Form
pub trait ActiveWidget<U>: PassiveWidget<U>
where
    U: Ui,
{
    /// Returns the [`&mut Text`] that is printed
    ///
    /// [`&mut Text`]: Text
    fn text_mut(&mut self) -> &mut Text;

    /// Actions to do whenever this [`ActionableWidget`] is focused.
    fn on_focus(&mut self, _area: &U::Area) {}

    /// Actions to do whenever this [`ActionableWidget`] is unfocused.
    fn on_unfocus(&mut self, _area: &U::Area) {}
}

#[allow(private_interfaces)]
trait ActiveHolder<U>: Send + Sync
where
    U: Ui,
{
    // General widget methods
    fn passive_widget(&self) -> &RwData<dyn PassiveWidget<U>>;

    /// Updates the widget, allowing the modification of its
    /// [`Area`][Ui::Area].
    ///
    /// This function will be called when Duat determines that the
    /// [`WidgetNode`]
    ///
    /// [`Session`]: crate::session::Session
    fn update_and_print(&self, area: &U::Area);

    fn update(&self, area: &U::Area);

    fn type_name(&self) -> &'static str;

    // Active widget methods
    fn active_widget(&self) -> &RwData<dyn ActiveWidget<U>>;

    fn input(&self) -> &RwData<dyn InputMethod<U>>;

    fn send_key(&self, key: KeyEvent, area: &U::Area, context: Context<U>);

    fn on_focus(&self, area: &U::Area);

    fn on_unfocus(&self, area: &U::Area);

    fn related_widgets(&self) -> Option<RelatedWidgets<U>>;
}

struct InnerActiveWidget<W, I, U>
where
    W: ActiveWidget<U>,
    I: InputMethod<U, Widget = W>,
    U: Ui,
{
    widget: RwData<W>,
    dyn_active: RwData<dyn ActiveWidget<U>>,
    dyn_passive: RwData<dyn PassiveWidget<U>>,
    input: RwData<I>,
    dyn_input: RwData<dyn InputMethod<U>>,
    related: Option<RelatedWidgets<U>>,
}

impl<W, I, U> ActiveHolder<U> for InnerActiveWidget<W, I, U>
where
    W: ActiveWidget<U>,
    I: InputMethod<U, Widget = W>,
    U: Ui,
{
    fn passive_widget(&self) -> &RwData<dyn PassiveWidget<U>> {
        &self.dyn_passive
    }

    fn update_and_print(&self, area: &U::Area) {
        let mut widget = self.widget.raw_write();
        widget.update(area);
        widget.print(area);
    }

    fn update(&self, area: &<U as Ui>::Area) {
        self.widget.raw_write().update(area)
    }

    fn type_name(&self) -> &'static str {
        duat_name::<W>()
    }

    fn active_widget(&self) -> &RwData<dyn ActiveWidget<U>> {
        &self.dyn_active
    }

    fn input(&self) -> &RwData<dyn InputMethod<U>> {
        &self.dyn_input
    }

    fn send_key(&self, key: KeyEvent, area: &<U as Ui>::Area, context: Context<U>) {
        let mut input = self.input.write();

        if let Some(cursors) = input.cursors() {
            self.widget.write().text_mut().remove_cursor_tags(cursors);
        }

        hooks::trigger::<KeySent<U>>((key, self.dyn_active.clone()));
        hooks::trigger::<KeySentTo<W, U>>((key, self.widget.clone()));

        input.send_key(key, &self.widget, area, context);

        let mut widget = self.widget.write();

        if let Some(cursors) = input.cursors() {
            widget.text_mut().add_cursor_tags(cursors);

            area.scroll_around_point(widget.text(), cursors.main().caret(), widget.print_cfg());
        }

        widget.update(area);
        widget.print(area);
    }

    fn on_focus(&self, area: &<U as Ui>::Area) {
        self.input.inspect(|input| {
            if let Some(cursors) = input.cursors() {
                self.widget.write().text_mut().remove_cursor_tags(cursors);
            }
        });

        self.input.mutate(|input| input.on_focus(area));
        self.widget.mutate(|widget| widget.on_focus(area));

        hooks::trigger::<FocusedOn<W, U>>((
            self.widget.clone(),
            self.dyn_input.clone(),
            area.clone(),
        ));
    }

    fn on_unfocus(&self, area: &<U as Ui>::Area) {
        self.input.inspect(|input| {
            if let Some(cursors) = input.cursors() {
                self.widget.write().text_mut().remove_cursor_tags(cursors);
            }
        });

        self.input.mutate(|input| input.on_unfocus(area));
        self.widget.mutate(|widget| widget.on_unfocus(area));

        hooks::trigger::<UnfocusedFrom<W, U>>((
            self.widget.clone(),
            self.dyn_input.clone(),
            area.clone(),
        ));
    }

    fn related_widgets(&self) -> Option<RelatedWidgets<U>> {
        self.related.clone()
    }
}

#[allow(private_interfaces)]
pub enum Widget<U>
where
    U: Ui,
{
    Passive(RwData<dyn PassiveWidget<U>>, &'static str),
    Active(Arc<dyn ActiveHolder<U>>),
}

impl<U> Clone for Widget<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        match self {
            Self::Passive(widget, name) => Self::Passive(widget.clone(), name),
            Self::Active(widget) => Self::Active(widget.clone()),
        }
    }
}

impl<U> Widget<U>
where
    U: Ui,
{
    pub fn passive<W>(widget: W) -> Self
    where
        W: PassiveWidget<U>,
    {
        Widget::Passive(
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget))),
            duat_name::<W>(),
        )
    }

    pub fn active<W, I>(widget: W, input: I) -> Self
    where
        W: ActiveWidget<U>,
        I: InputMethod<U, Widget = W>,
    {
        let dyn_active: RwData<dyn ActiveWidget<U>> =
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget)));
        let dyn_passive = dyn_active.clone().to_passive();

        let dyn_input: RwData<dyn InputMethod<U>> =
            RwData::new_unsized::<I>(Arc::new(RwLock::new(input)));
        let input = dyn_input.try_downcast::<I>().unwrap();

        if let Some(file) = dyn_active.try_downcast::<File>()
            && let Some(cursors) = input.read().cursors()
        {
            ActiveWidget::<U>::text_mut(&mut *file.write()).add_cursor_tags(cursors)
        }

        let inner = InnerActiveWidget {
            widget: dyn_active.clone().try_downcast::<W>().unwrap(),
            dyn_active,
            dyn_passive,
            input,
            dyn_input,
            related: if TypeId::of::<W>() == TypeId::of::<File>() {
                Some(RwData::new(Vec::new()))
            } else {
                None
            },
        };

        Widget::Active(Arc::new(inner))
    }

    pub fn update_and_print(&self, area: &U::Area) {
        match self {
            Widget::Passive(widget, _) => {
                let mut widget = widget.raw_write();
                widget.update(area);
                widget.print(area);
            }
            Widget::Active(holder) => {
                holder.update_and_print(area);
            }
        }
    }

    /// Returns the downcast ref of this [`Widget`].
    pub fn downcast<W>(&self) -> Option<RwData<W>>
    where
        W: PassiveWidget<U>,
    {
        match self {
            Widget::Passive(widget, _) => widget.clone().try_downcast::<W>(),
            Widget::Active(holder) => holder.active_widget().clone().try_downcast::<W>(),
        }
    }

    pub fn data_is<W>(&self) -> bool
    where
        W: 'static,
    {
        match self {
            Widget::Passive(widget, _) => widget.data_is::<W>(),
            Widget::Active(holder) => holder.active_widget().data_is::<W>(),
        }
    }

    pub fn inspect_as<W, B>(&self, f: impl FnOnce(&W) -> B) -> Option<B>
    where
        W: PassiveWidget<U>,
    {
        match self {
            Widget::Passive(widget, _) => widget.inspect_as::<W, B>(f),
            Widget::Active(holder) => holder.active_widget().inspect_as::<W, B>(f),
        }
    }

    pub fn as_passive(&self) -> &RwData<dyn PassiveWidget<U>> {
        match self {
            Widget::Passive(widget, _) => widget,
            Widget::Active(holder) => holder.passive_widget(),
        }
    }

    pub fn as_active(&self) -> Option<(&RwData<dyn ActiveWidget<U>>, &RwData<dyn InputMethod<U>>)> {
        match self {
            Widget::Active(holder) => Some((holder.active_widget(), holder.input())),
            _ => None,
        }
    }

    pub fn input(&self) -> Option<&RwData<dyn InputMethod<U>>> {
        match self {
            Widget::Passive(..) => None,
            Widget::Active(holder) => Some(holder.input()),
        }
    }

    pub fn ptr_eq<W, D>(&self, other: &D) -> bool
    where
        W: ?Sized,
        D: Data<W> + ?Sized,
    {
        match self {
            Widget::Passive(widget, _) => widget.ptr_eq(other),
            Widget::Active(holder) => holder.active_widget().ptr_eq(other),
        }
    }

    pub fn update(&self, area: &U::Area) {
        match self {
            Widget::Passive(widget, _) => widget.raw_write().update(area),
            Widget::Active(holder) => holder.update(area),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Widget::Passive(_, name) => name,
            Widget::Active(holder) => holder.type_name(),
        }
    }

    pub(crate) fn on_focus(&self, area: &U::Area) {
        match self {
            Widget::Passive(..) => {}
            Widget::Active(holder) => holder.on_focus(area),
        }
    }

    pub(crate) fn on_unfocus(&self, area: &U::Area) {
        match self {
            Widget::Passive(..) => {}
            Widget::Active(holder) => holder.on_unfocus(area),
        }
    }

    pub(crate) fn send_key(&self, key: KeyEvent, area: &U::Area, context: Context<U>) {
        match self {
            Widget::Passive(..) => unreachable!("Sending keys to passive widgets is impossible"),
            Widget::Active(holder) => holder.send_key(key, area, context),
        }
    }

    pub(crate) fn raw_inspect<B>(&self, f: impl FnOnce(&dyn PassiveWidget<U>) -> B) -> B {
        match self {
            Widget::Passive(widget, _) => f(&*widget.raw_read()),
            Widget::Active(holder) => f(&*holder.active_widget().raw_read()),
        }
    }

    pub(crate) fn related_widgets(&self) -> Option<RelatedWidgets<U>> {
        match self {
            Widget::Passive(..) => None,
            Widget::Active(holder) => holder.related_widgets(),
        }
    }
}

pub type RelatedWidgets<U> =
    RwData<Vec<(RwData<dyn PassiveWidget<U>>, <U as Ui>::Area, &'static str)>>;
