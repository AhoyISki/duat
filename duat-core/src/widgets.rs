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
//! [`duat-term`](https://docs.rs/duat-term) crate, which is a ui
//! implementation for Duat, defines "rule" widgets, which are
//! separators that only really make sense in the context of a
//! terminal.
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{
    any::TypeId,
    collections::HashMap,
    sync::{Arc, LazyLock},
};

use crossterm::event::KeyEvent;
#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

pub use crate::file::{File, FileCfg};
use crate::{
    data::{Data, RwData},
    input::InputMethod,
    palette,
    text::{PrintCfg, Text},
    ui::{Area, PushSpecs, Ui},
    Globals,
};

/// An area where text will be printed to the screen.
pub trait PassiveWidget<U>: Send + Sync + 'static
where
    U: Ui,
{
    fn build(
        globals: Globals<U>,
        on_file: bool,
    ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs)
    where
        Self: Sized;

    /// Updates the widget, allowing the modification of its
    /// [`Area`][Ui::Area].
    ///
    /// This function will be called when Duat determines that the
    /// [`WidgetNode`]
    ///
    /// [`Session`]: crate::session::Session
    fn update(&mut self, area: &U::Area);

    /// The text that this widget prints out.
    fn text(&self) -> &Text;

    fn print_cfg(&self) -> &PrintCfg {
        static CFG: LazyLock<PrintCfg> = LazyLock::new(PrintCfg::default);

        &CFG
    }

    fn print(&mut self, area: &U::Area) {
        area.print(self.text(), self.print_cfg(), palette::painter())
    }

    fn once(globals: Globals<U>)
    where
        Self: Sized;

    fn name() -> &'static str
    where
        Self: Sized,
    {
        static NAMES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(|| RwLock::new(HashMap::new()));
        let mut names = NAMES.write().unwrap();
        let type_id = TypeId::of::<Self>();

        if let Some(name) = names.get(&type_id) {
            name
        } else {
            let verbose = std::any::type_name::<Self>();
            let mut name = String::new();

            for path in verbose.split_inclusive(['<', '>']) {
                for segment in path.split("::") {
                    if segment.chars().any(|char| char.is_ascii_uppercase()) {
                        name.push_str(segment);
                    }
                }
            }

            names.insert(type_id, name.leak());
            names.get(&type_id).unwrap()
        }
    }
}

#[allow(refining_impl_trait)]
pub trait WidgetCfg<U>: Sized
where
    U: Ui,
{
    type Widget: PassiveWidget<U>;
    fn build(
        self,
        globals: Globals<U>,
        on_file: bool,
    ) -> (Widget<U>, impl Fn() -> bool + 'static, PushSpecs);
}

/// A widget that can receive input and show [`Cursor`]s.
pub trait ActiveWidget<U>: PassiveWidget<U>
where
    U: Ui,
{
    /// A mutable reference to the [`Text`] printed by this cursor.
    fn mut_text(&mut self) -> &mut Text;

    /// Actions to do whenever this [`ActionableWidget`] is focused.
    fn on_focus(&mut self, _area: &U::Area);

    /// Actions to do whenever this [`ActionableWidget`] is unfocused.
    fn on_unfocus(&mut self, _area: &U::Area);
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

    fn send_key(&self, key: KeyEvent, area: &U::Area, globals: Globals<U>);

    fn on_focus(&self, area: &U::Area);

    fn on_unfocus(&self, area: &U::Area);
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
        W::name()
    }

    fn active_widget(&self) -> &RwData<dyn ActiveWidget<U>> {
        &self.dyn_active
    }

    fn input(&self) -> &RwData<dyn InputMethod<U>> {
        &self.dyn_input
    }

    fn send_key(&self, key: KeyEvent, area: &U::Area, globals: Globals<U>) {
        let mut input = self.input.write();

        if let Some(cursors) = input.cursors() {
            self.widget.write().mut_text().remove_cursor_tags(cursors);
        }

        input.send_key(key, &self.widget, area, globals);

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

impl<W, I, U> Clone for InnerActiveWidget<W, I, U>
where
    W: ActiveWidget<U>,
    I: InputMethod<U, Widget = W>,
    U: Ui,
{
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            dyn_active: self.dyn_active.clone(),
            dyn_passive: self.dyn_passive.clone(),
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
            W::name(),
        )
    }

    pub fn active<W, I>(widget: W, input: RwData<I>) -> Self
    where
        W: ActiveWidget<U>,
        I: InputMethod<U, Widget = W>,
    {
        let dyn_active: RwData<dyn ActiveWidget<U>> =
            RwData::new_unsized::<W>(Arc::new(RwLock::new(widget)));
        let dyn_passive = dyn_active.clone().to_passive();

        let input_data = input.inner_arc().clone() as Arc<RwLock<dyn InputMethod<U>>>;

        let inner = InnerActiveWidget {
            widget: dyn_active.clone().try_downcast::<W>().unwrap(),
            dyn_active,
            dyn_passive,
            input,
            dyn_input: RwData::new_unsized::<I>(input_data),
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
            Widget::Passive(widget, _) => widget.clone().try_downcast::<W>().ok(),
            Widget::Active(holder) => holder.active_widget().clone().try_downcast::<W>().ok(),
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

    pub(crate) fn send_key(&self, key: KeyEvent, area: &U::Area, globals: Globals<U>) {
        match self {
            Widget::Passive(..) => unreachable!("Sending keys to passive widgets is impossible"),
            Widget::Active(holder) => holder.send_key(key, area, globals),
        }
    }

    pub(crate) fn raw_inspect<B>(&self, f: impl FnOnce(&dyn PassiveWidget<U>) -> B) -> B {
        match self {
            Widget::Passive(widget, _) => f(&*widget.raw_read()),
            Widget::Active(holder) => f(&*holder.active_widget().raw_read()),
        }
    }
}
