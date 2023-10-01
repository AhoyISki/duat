//! APIs for the construction of widgets, and a few common ones.
//!
//! This module describes two types of widget, [`NormalWidget`]s and
//! [`ActionableWidget`]s. [`NormalWidget`]s simply show information,
//! and cannot receive input or be focused. [`ActionableWidget`] is a
//! superset of [`NormalWidget`], capable of receiving input,
//! focusing, unfocusing, and showing cursors.
//!
//! The module also provides 4 native widgets, [`StatusLine`] and
//! [`LineNumbers`], which are [`NormalWidget`]s, and
//! [`FileWidget`] and [`CommandLine`] which are
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
    file_widget::{File, FileCfg},
    line_numbers::{LineNumbers, LineNumbersCfg},
    status_line::{file_parts, status_cfg, DynInput, StatusLine, StatusLineCfg},
};
use crate::{
    data::{Data, RwData},
    input::InputMethod,
    text::{PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
    PALETTE,
};

/// An area where text will be printed to the screen.
pub trait PassiveWidget: Send + Sync + 'static {
    fn build<U>() -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
        Self: Sized;

    /// Updates the widget, allowing the modification of its
    /// [`Area`][Ui::Area].
    ///
    /// This function will be called when Parsec determines that the
    /// [`WidgetNode`]
    ///
    /// [`Session`]: crate::session::Session
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

#[allow(refining_impl_trait)]
pub trait ActiveWidgetCfg: Sized + Clone {
    type Widget: ActiveWidget;
    type WithInput<NewI>: ActiveWidgetCfg
    where
        NewI: InputMethod<Widget = Self::Widget> + Clone;

    fn builder<U: Ui>(self) -> impl FnOnce() -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs);

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

    fn cfg() -> Self::Config
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
    /// [`Session`]: crate::session::Session
    fn update_and_print(&self, area: &U::Area);

    fn update(&self, area: &U::Area);

    fn widget_type(&self) -> &'static str;
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

    fn send_key(&self, key: KeyEvent, area: &U::Area);

    fn on_focus(&self, area: &U::Area);

    fn on_unfocus(&self, area: &U::Area);
}

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

    fn widget_type(&self) -> &'static str {
        stringify!(W)
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

impl<W> Clone for InnerPassiveWidget<W>
where
    W: PassiveWidget,
{
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            dyn_widget: self.dyn_widget.clone(),
        }
    }
}

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

    fn widget_type(&self) -> &'static str {
        stringify!(W)
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

    fn send_key(&self, key: KeyEvent, area: &U::Area) {
        let mut input = self.input.write();

        if let Some(cursors) = input.cursors() {
            self.widget.write().mut_text().remove_cursor_tags(cursors);
        }

        input.send_key(key, &self.widget, area);

        if let Some(cursors) = input.cursors() {
            let mut widget = self.widget.write();
            widget.mut_text().add_cursor_tags(cursors);

            area.scroll_around_point(widget.text(), cursors.main().caret(), widget.print_cfg());

            widget.update(area);
            widget.print(area);
        }
    }

    fn on_focus(&self, area: &<U as Ui>::Area) {
        self.input.mutate(|input| input.on_focus(area));
        self.widget.mutate(|widget| widget.on_focus(area));
    }

    fn on_unfocus(&self, area: &<U as Ui>::Area) {
        self.input.mutate(|input| input.on_unfocus(area));
        self.widget.mutate(|widget| widget.on_unfocus(area));
    }
}

impl<W: Clone, I: Clone> Clone for InnerActiveWidget<W, I>
where
    W: ActiveWidget,
    I: InputMethod<Widget = W>,
{
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            dyn_widget: self.dyn_widget.clone(),
            input: self.input.clone(),
            dyn_input: self.dyn_input.clone(),
        }
    }
}

#[allow(private_interfaces)]
pub enum Widget<U>
where
    U: Ui,
{
    Passive(Arc<dyn PassiveWidgetHolder<U>>),
    Active(Arc<dyn ActiveWidgetHolder<U>>),
}

impl<U> Clone for Widget<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        match self {
            Self::Passive(widget) => Self::Passive(widget.clone()),
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
        W: PassiveWidget,
    {
        let dyn_widget: RwData<dyn PassiveWidget> =
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget)));

        let holder_widget = InnerPassiveWidget {
            widget: dyn_widget.clone().try_downcast::<W>().unwrap(),
            dyn_widget,
        };

        Widget::Passive(Arc::new(holder_widget))
    }

    pub fn active<W, I>(widget: W, input: RwData<I>) -> Self
    where
        W: ActiveWidget,
        I: InputMethod<Widget = W>,
    {
        let dyn_widget: RwData<dyn ActiveWidget> =
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget)));

        let input_data = input.inner_arc().clone() as Arc<RwLock<dyn InputMethod>>;

        let holder_widget = InnerActiveWidget {
            widget: dyn_widget.clone().try_downcast::<W>().unwrap(),
            dyn_widget,
            input,
            dyn_input: RwData::new_unsized::<I>(input_data),
        };

        Widget::Active(Arc::new(holder_widget))
    }

    pub fn update_and_print(&self, area: &U::Area) {
        match self {
            Widget::Passive(holder) => {
                holder.update_and_print(area);
            }
            Widget::Active(holder) => {
                holder.update_and_print(area);
            }
        }
    }

    /// Returns the downcast ref of this [`Widget`].
    pub fn downcast<W>(&self) -> Option<RwData<W>>
    where
        W: PassiveWidget,
    {
        match self {
            Widget::Passive(holder) => holder.passive_widget().clone().try_downcast::<W>().ok(),
            Widget::Active(holder) => holder.active_widget().clone().try_downcast::<W>().ok(),
        }
    }

    pub fn data_is<W>(&self) -> bool
    where
        W: 'static,
    {
        match self {
            Widget::Passive(holder) => holder.passive_widget().data_is::<W>(),
            Widget::Active(holder) => holder.active_widget().data_is::<W>(),
        }
    }

    pub fn inspect_as<W, B>(&self, f: impl FnOnce(&W) -> B) -> Option<B>
    where
        W: PassiveWidget,
    {
        match self {
            Widget::Passive(holder) => holder.passive_widget().inspect_as::<W, B>(f),
            Widget::Active(holder) => holder.active_widget().inspect_as::<W, B>(f),
        }
    }

    pub fn as_active(&self) -> Option<(&RwData<dyn ActiveWidget>, &RwData<dyn InputMethod>)> {
        match self {
            Widget::Active(holder) => Some((holder.active_widget(), holder.input())),
            _ => None,
        }
    }

    pub fn input(&self) -> Option<&RwData<dyn InputMethod>> {
        match self {
            Widget::Active(holder) => Some(holder.input()),
            Widget::Passive(_) => None,
        }
    }

    pub fn ptr_eq<W, D>(&self, other: &D) -> bool
    where
        W: ?Sized,
        D: Data<W>,
    {
        match self {
            Widget::Passive(holder) => holder.passive_widget().ptr_eq(other),
            Widget::Active(holder) => holder.active_widget().ptr_eq(other),
        }
    }

    pub fn update(&self, area: &U::Area) {
        match self {
            Widget::Active(holder) => holder.update(area),
            Widget::Passive(holder) => holder.update(area),
        }
    }

    pub fn widget_type(&self) -> &'static str {
        match self {
            Widget::Passive(holder) => holder.widget_type(),
            Widget::Active(holder) => holder.widget_type(),
        }
    }

    pub(crate) fn on_focus(&self, area: &U::Area) {
        match self {
            Widget::Active(holder) => holder.on_focus(area),
            Widget::Passive(_) => {}
        }
    }

    pub(crate) fn on_unfocus(&self, area: &U::Area) {
        match self {
            Widget::Active(holder) => holder.on_unfocus(area),
            Widget::Passive(_) => {}
        }
    }

    pub(crate) fn send_key(&self, key: KeyEvent, area: &U::Area) {
        match self {
            Widget::Active(holder) => holder.send_key(key, area),
            Widget::Passive(_) => {}
        }
    }

    pub(crate) fn raw_inspect<B>(&self, f: impl FnOnce(&dyn PassiveWidget) -> B) -> B {
        match self {
            Widget::Passive(holder) => f(&*holder.passive_widget().read()),
            Widget::Active(holder) => f(&*holder.active_widget().read()),
        }
    }
}
