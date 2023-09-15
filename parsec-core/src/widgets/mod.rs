//! APIs for the construction of widgets, and a few common ones.
//!
//! This module describes two types of widget, [`NormalWidget`]s and
//! [`ActionableWidget`]s. [`NormalWidget`]s simply show information,
//! and cannot receive input or be focused. [`ActionableWidget`] is a
//! superset of [`NormalWidget`], capable of receiving input,
//! focusing, unfocusing, and showing cursors.
//!
//! The module also provides 4 native widgets, [`StatusLine<U>`] and
//! [`LineNumbers<U>`], which are [`NormalWidget`]s, and
//! [`FileWidget<U>`] and [`CommandLine<U>`] which are
//! [`ActionableWidget`]s.
//!
//! These widgets are supposed to be universal, not needing a specific
//! [`Ui`] implementation to work. As an example, the
//! [`parsec-term`](https://docs.rs/parsec-term) crate, which is a ui
//! implementation for Parsec, defines "rule" widgets, which are
//! separators that only really make sense in the context of a
//! terminal.
mod command_line;
mod file_widget;
mod line_numbers;
mod status_line;

use std::sync::Arc;
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;

use crossterm::event::KeyEvent;
#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

pub use self::{
    command_line::{CommandLine, CommandLineCfg},
    file_widget::{FileWidget, FileWidgetCfg},
    line_numbers::{LineNumbers, LineNumbersCfg, ITER_COUNT},
    status_line::{file_parts, StatusLine, StatusPart},
};
use crate::{
    data::{AsAny, ReadableData, RwData},
    input::InputMethod,
    text::{PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
    Controler, PALETTE,
};

/// An area where text will be printed to the screen.
pub trait PassiveWidget: AsAny + Send + Sync + 'static {
    fn build<U>(controler: &Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
        Self: Sized;

    /// Updates the widget, allowing the modification of its
    /// [`Area`][Ui::Area].
    ///
    /// This function will be called when Parsec determines that the
    /// [`WidgetNode`]
    ///
    /// [`Session<U>`]: crate::session::Session
    fn update(&mut self, area: &impl Area)
    where
        Self: Sized;

    /// The text that this widget prints out.
    fn text(&self) -> &Text;

    fn print_cfg(&self) -> &PrintCfg {
        use std::sync::LazyLock;
        static CFG: LazyLock<PrintCfg> = LazyLock::new(PrintCfg::default);

        &CFG
    }

    fn print(&mut self, area: &impl Area)
    where
        Self: Sized,
    {
        area.print(self.text(), self.print_cfg(), &PALETTE)
    }
}

pub trait ActiveWidgetCfg: Sized + Clone {
    type Widget: ActiveWidget;
    type WithInput<NewI>: ActiveWidgetCfg
    where
        NewI: InputMethod<Widget = Self::Widget> + Clone;

    fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui;

    fn with_input<NewI>(self, input: NewI) -> Self::WithInput<NewI>
    where
        NewI: InputMethod<Widget = Self::Widget> + Clone;
}

/// A widget that can receive input and show [`Cursor`]s.
pub trait ActiveWidget: PassiveWidget {
    type Config: ActiveWidgetCfg<Widget = Self>
    where
        Self: Sized;

    /// A mutable reference to the [`Text`] printed by this cursor.
    fn mut_text(&mut self) -> &mut Text;

    fn config() -> Self::Config
    where
        Self: Sized;

    /// Actions to do whenever this [`ActionableWidget`] is focused.
    fn on_focus(&mut self, _area: &impl Area)
    where
        Self: Sized,
    {
    }

    /// Actions to do whenever this [`ActionableWidget`] is unfocused.
    fn on_unfocus(&mut self, _area: &impl Area)
    where
        Self: Sized,
    {
    }
}

trait WidgetHolder<U>: Send + Sync
where
    U: Ui,
{
    /// Updates the widget, allowing the modification of its
    /// [`Area`][Ui::Area].
    ///
    /// This function will be called when Parsec determines that the
    /// [`WidgetNode`]
    ///
    /// [`Session<U>`]: crate::session::Session
    fn update_and_print(&self, area: &U::Area);

    fn update(&self, area: &U::Area);
}

#[allow(private_interfaces)]
trait PassiveWidgetHolder<U>: WidgetHolder<U>
where
    U: Ui,
{
    fn passive_widget(&self) -> &RwData<dyn PassiveWidget>;
}

#[allow(private_interfaces)]
trait ActiveWidgetHolder<U>: WidgetHolder<U>
where
    U: Ui,
{
    fn active_widget(&self) -> &RwData<dyn ActiveWidget>;

    fn input(&self) -> &RwData<dyn InputMethod>;

    fn send_key(&self, key: KeyEvent, area: &U::Area, controler: &Controler<U>);

    fn on_focus(&self, area: &U::Area);

    fn on_unfocus(&self, area: &U::Area);
}

#[derive(Clone)]
struct InnerPassiveWidget<W>
where
    W: PassiveWidget,
{
    widget: RwData<W>,
    dyn_widget: RwData<dyn PassiveWidget>,
}

impl<U, W> WidgetHolder<U> for InnerPassiveWidget<W>
where
    U: Ui,
    W: PassiveWidget,
{
    fn update_and_print(&self, area: &<U as Ui>::Area) {
        let mut widget = self.widget.raw_write();
        widget.update(area);
        widget.print(area);
    }

    fn update(&self, area: &<U as Ui>::Area) {
        self.widget.write().update(area);
    }
}

impl<U, W> PassiveWidgetHolder<U> for InnerPassiveWidget<W>
where
    U: Ui,
    W: PassiveWidget,
{
    fn passive_widget(&self) -> &RwData<dyn PassiveWidget> {
        &self.dyn_widget
    }
}

#[derive(Clone)]
struct InnerActiveWidget<W, I>
where
    W: ActiveWidget,
    I: InputMethod<Widget = W>,
{
    widget: RwData<W>,
    dyn_widget: RwData<dyn ActiveWidget>,
    input: RwData<I>,
    dyn_input: RwData<dyn InputMethod>,
}

impl<U, W, I> WidgetHolder<U> for InnerActiveWidget<W, I>
where
    U: Ui,
    W: ActiveWidget,
    I: InputMethod<Widget = W>,
{
    fn update_and_print(&self, area: &U::Area) {
        let mut widget = self.widget.raw_write();
        widget.update(area);
        widget.print(area);
    }

    fn update(&self, area: &<U as Ui>::Area) {
        self.widget.write().update(area)
    }
}

impl<U, W, I> ActiveWidgetHolder<U> for InnerActiveWidget<W, I>
where
    U: Ui,
    W: ActiveWidget,
    I: InputMethod<Widget = W>,
{
    fn active_widget(&self) -> &RwData<dyn ActiveWidget> {
        &self.dyn_widget
    }

    fn input(&self) -> &RwData<dyn InputMethod> {
        &self.dyn_input
    }

    fn send_key(&self, key: KeyEvent, area: &U::Area, controler: &Controler<U>) {
        let mut input = self.input.write();

        if let Some(cursors) = input.cursors() {
            self.widget.write().mut_text().remove_cursor_tags(cursors);
        }

        input.send_key(key, &self.widget, area, controler);

        if let Some(cursors) = input.cursors() {
            let mut widget = self.widget.write();
            widget.mut_text().add_cursor_tags(cursors);

            area.scroll_around_point(widget.text(), cursors.main().caret(), widget.print_cfg());

            widget.update(area);
            widget.print(area);
        }
    }

    fn on_focus(&self, area: &<U as Ui>::Area) {
        self.widget.write().on_focus(area)
    }

    fn on_unfocus(&self, area: &<U as Ui>::Area) {
        self.widget.write().on_unfocus(area)
    }
}

pub enum Widget<U>
where
    U: Ui,
{
    Passive(Box<dyn PassiveWidgetHolder<U>>),
    Active(Box<dyn ActiveWidgetHolder<U>>),
}

impl<U> Widget<U>
where
    U: Ui,
{
    pub fn passive<W>(widget: W) -> Self
    where
        W: PassiveWidget,
    {
        let dyn_widget: RwData<dyn PassiveWidget> =
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget)));

        let inner_widget = InnerPassiveWidget {
            widget: dyn_widget.clone().try_downcast::<W>().unwrap(),
            dyn_widget,
        };

        Widget::Passive(Box::new(inner_widget))
    }

    pub fn active<W, I>(widget: W, input: RwData<I>) -> Self
    where
        W: ActiveWidget,
        I: InputMethod<Widget = W>,
    {
        let dyn_widget: RwData<dyn ActiveWidget> =
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget)));

        let input_data = input.inner_arc().clone() as Arc<RwLock<dyn InputMethod>>;

        let inner_widget = InnerActiveWidget {
            widget: dyn_widget.clone().try_downcast::<W>().unwrap(),
            dyn_widget,
            input,
            dyn_input: RwData::new_unsized::<I>(input_data),
        };

        Widget::Active(Box::new(inner_widget))
    }

    pub fn update_and_print(&self, area: &U::Area) {
        match self {
            Widget::Passive(inner) => {
                inner.update_and_print(area);
            }
            Widget::Active(inner) => {
                inner.update_and_print(area);
            }
        }
    }

    /// Returns the downcast ref of this [`Widget<U>`].
    pub fn downcast_ref<W>(&self) -> Option<RwData<W>>
    where
        W: PassiveWidget,
    {
        match self {
            Widget::Passive(inner) => inner.passive_widget().clone().try_downcast::<W>().ok(),
            Widget::Active(inner) => inner.active_widget().clone().try_downcast::<W>().ok(),
        }
    }

    pub fn data_is<W>(&self) -> bool
    where
        W: 'static,
    {
        match self {
            Widget::Passive(inner) => inner.passive_widget().data_is::<W>(),
            Widget::Active(inner) => inner.active_widget().data_is::<W>(),
        }
    }

    pub fn inspect_as<W, B>(&self, f: impl FnOnce(&W) -> B) -> Option<B>
    where
        W: PassiveWidget,
    {
        match self {
            Widget::Passive(inner) => inner.passive_widget().inspect_as::<W, B>(f),
            Widget::Active(inner) => inner.active_widget().inspect_as::<W, B>(f),
        }
    }

    pub fn as_active(&self) -> Option<&RwData<dyn ActiveWidget>> {
        match self {
            Widget::Active(inner) => Some(inner.active_widget()),
            _ => None,
        }
    }

    pub fn input(&self) -> Option<&RwData<dyn InputMethod>> {
        match self {
            Widget::Active(inner) => Some(inner.input()),
            Widget::Passive(_) => None,
        }
    }

    pub fn ptr_eq<R, W>(&self, other: &R) -> bool
    where
        R: ReadableData<W>,
        W: ?Sized,
    {
        match self {
            Widget::Passive(inner) => inner.passive_widget().ptr_eq(other),
            Widget::Active(inner) => inner.active_widget().ptr_eq(other),
        }
    }

    pub fn update(&self, area: &U::Area) {
        match self {
            Widget::Active(inner) => inner.update(area),
            Widget::Passive(inner) => inner.update(area),
        }
    }

    pub(crate) fn on_focus(&self, area: &U::Area) {
        match self {
            Widget::Active(inner) => inner.on_focus(area),
            Widget::Passive(_) => {}
        }
    }

    pub(crate) fn on_unfocus(&self, area: &U::Area) {
        match self {
            Widget::Active(inner) => inner.on_unfocus(area),
            Widget::Passive(_) => {}
        }
    }

    pub(crate) fn send_key(&self, key: KeyEvent, area: &U::Area, controler: &Controler<U>) {
        match self {
            Widget::Active(inner) => inner.send_key(key, area, controler),
            Widget::Passive(_) => {}
        }
    }

    pub(crate) fn raw_inspect<B>(&self, f: impl FnOnce(&dyn PassiveWidget) -> B) -> B {
        match self {
            Widget::Passive(inner) => f(&*inner.passive_widget().read()),
            Widget::Active(inner) => f(&*inner.active_widget().read()),
        }
    }
}
