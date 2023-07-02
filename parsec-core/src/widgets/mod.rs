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

#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{cmp::Ordering, ops::Range, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use crate::{
    data::{DownCastableData, RawReadableData, ReadableData, RoData, RwData},
    position::{Cursor, Editor, Mover},
    tags::form::FormPalette,
    text::{PrintCfg, Text},
    ui::Ui
};

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait Widget<U>: DownCastableData + 'static
where
    U: Ui + 'static
{
    /// Updates the widget, allowing the modification of its
    /// [`Area`][Ui::Area].
    ///
    /// This function will be called when Parsec determines that the
    /// [`WidgetNode`]
    ///
    /// [`Session<U>`]: crate::session::Session
    fn update(&mut self, area: &mut U::Area);

    /// The text that this widget prints out.
    fn text(&self) -> &Text;

    /// Scrolls the text vertically by an amount.
    fn scroll_vertically(&mut self, _d_y: i32) {}

    fn print_info(&self) -> U::PrintInfo {
        U::PrintInfo::default()
    }

    fn print_cfg(&self) -> PrintCfg {
        PrintCfg::default()
    }

    fn input_taker(&mut self) -> Option<InputTaker<U>> {
        None
    }
}

pub enum InputTaker<'a, U>
where
    U: Ui
{
    Scheme(&'a mut dyn SchemeWidget<U>),
    Direct(&'a mut dyn DirectWidget<U>)
}

impl<'a, U> InputTaker<'a, U>
where
    U: Ui
{
	pub fn on_focus(&self, area: &U::Area) {
    	match self {
        InputTaker::Scheme(widget) => widget.on_focus(area),
        InputTaker::Direct(widget) => widget.on_focus(area),
    }
	}

	pub fn on_unfocus(&self, area: &U::Area) {
    	match self {
        InputTaker::Scheme(widget) => widget.on_unfocus(area),
        InputTaker::Direct(widget) => widget.on_unfocus(area),
    }
	}
}

/// A widget that can receive input and show [`Cursor`]s.
pub trait SchemeWidget<U>: Widget<U>
where
    U: Ui + 'static
{
    /// Returns an [`Editor<U>`], which uses a cursor to input text to
    /// [`self`].
    fn editor<'a>(&'a mut self, index: usize, edit_accum: &'a mut EditAccum) -> Editor<U>;

    /// Returns a [`Mover<U>`], which can move a cursor's position
    /// over [`self`].
    fn mover<'a>(&'a mut self, index: usize, area: &'a U::Area) -> Mover<U>;

    /// This is used specifically to remove and add the [`Cursor`]
    /// [`Tag`][crate::tags::Tag]s to [`self`]
    fn members_for_cursor_tags(&mut self) -> (&mut Text, &[Cursor], usize);

    /// The list of active [`Cursor`]s on [`self`].
    fn cursors(&self) -> &[Cursor];

    /// A mutable list of active [`Cursor`]s
    ///
    /// As this is an [`Option`], the widget may or may not pass this
    /// reference.
    fn mut_cursors(&mut self) -> Option<&mut Vec<Cursor>>;

    /// The index of the main cursor of [`self`].
    fn main_cursor_index(&self) -> usize;

    /// A mutable reference to the main cursor index of [`self`].
    ///
    /// As this is an [`Option`], the widget may or may not pass this
    /// reference.
    fn mut_main_cursor_index(&mut self) -> Option<&mut usize>;

    /// Starts a new [`Moment`][crate::history::Moment].
    ///
    /// Will panic by default, assuming that the [`ActionableWidget`]
    /// does not have a [`History`][crate::history::History].
    fn new_moment(&mut self) {
        panic!("This ActionableWidget does not have a History of its own.")
    }

    /// Undoes the last [`Moment`][crate::history::Moment].
    ///
    /// Will panic by default, assuming that the [`ActionableWidget`]
    /// does not have a [`History`][crate::history::History].
    fn undo(&mut self, _area: &U::Area) {
        panic!("This ActionableWidget does not have a History of its own.")
    }

    /// Redoes the last [`Moment`][crate::history::Moment].
    ///
    /// Will panic by default, assuming that the [`ActionableWidget`]
    /// does not have a [`History`][crate::history::History].
    fn redo(&mut self, _area: &U::Area) {
        panic!("This ActionableWidget does not have a History of its own.")
    }

    /// Actions to do whenever this [`ActionableWidget`] is focused.
    fn on_focus(&mut self, _area: &U::Area) {}

    /// Actions to do whenever this [`ActionableWidget`] is unfocused.
    fn on_unfocus(&mut self, _area: &U::Area) {}
}

pub trait DirectWidget<U>
where
    U: Ui
{
    /// Actions to do whenever this [`ActionableWidget`] is focused.
    fn on_focus(&mut self, _area: &U::Area) {}

    /// Actions to do whenever this [`ActionableWidget`] is unfocused.
    fn on_unfocus(&mut self, _area: &U::Area) {}
}

/// An enum for handling the 2 types of widget.
#[derive(Clone)]
enum InnerWidget<U>
where
    U: Ui
{
    Normal(RwData<dyn Widget<U>>),
    Actionable(RwData<dyn SchemeWidget<U>>)
}

impl<U> InnerWidget<U>
where
    U: Ui
{
    pub fn update(&self, area: &mut U::Area) {
        match self {
            InnerWidget::Normal(widget) => {
                widget.write().update(area);
            }
            InnerWidget::Actionable(widget) => {
                widget.write().update(area);
            }
        }
    }
}

/// A full representation of a widget.
///
/// This includes information describing wether or not the
/// [`WidgetNode<U>`] is "slow", and a function that determines if the
/// [`WidgetNode<U>`] needs to be updated.
pub struct WidgetNode<U>
where
    U: Ui
{
    inner: InnerWidget<U>,
    is_slow: bool,
    needs_update: Box<dyn Fn() -> bool>
}

impl<U> WidgetNode<U>
where
    U: Ui + 'static
{
    /// Returns a [`NormalWidget`] [`WidgetNode<U>`].
    pub fn normal(widget: impl Widget<U>, f: impl Fn() -> bool + 'static) -> WidgetNode<U> {
        WidgetNode {
            inner: InnerWidget::Normal(RwData::new_unsized(Arc::new(RwLock::new(widget)))),
            is_slow: false,
            needs_update: Box::new(f)
        }
    }

    /// Returns an [`ActionableWidget`] [`WidgetNode<U>`].
    pub fn actionable(
        widget: impl SchemeWidget<U>, f: impl Fn() -> bool + 'static
    ) -> WidgetNode<U> {
        WidgetNode {
            inner: InnerWidget::Actionable(RwData::new_unsized(Arc::new(RwLock::new(widget)))),
            is_slow: false,
            needs_update: Box::new(f)
        }
    }

    /// Returns a slow [`NormalWidget`] [`WidgetNode<U>`].
    ///
    /// Slow [`WidgetNode<U>`]s get updated asynchronously, as to not
    /// slow down the execution of Parsec.
    pub fn slow_normal(widget: impl Widget<U>, f: impl Fn() -> bool + 'static) -> WidgetNode<U> {
        // assert!(updaters.len() > 0, "Without any updaters, this widget can
        // never update");
        WidgetNode {
            inner: InnerWidget::Normal(RwData::new_unsized(Arc::new(RwLock::new(widget)))),
            is_slow: true,
            needs_update: Box::new(f)
        }
    }

    /// Returns a slow [`ActionableWidget`] [`WidgetNode<U>`].
    ///
    /// Slow [`WidgetNode<U>`]s get updated asynchronously, as to not
    /// slow down the execution of Parsec.
    pub fn slow_actionable(
        widget: impl SchemeWidget<U>, f: impl Fn() -> bool + 'static
    ) -> WidgetNode<U> {
        WidgetNode {
            inner: InnerWidget::Actionable(RwData::new_unsized(Arc::new(RwLock::new(widget)))),
            is_slow: true,
            needs_update: Box::new(f)
        }
    }

    /// Updates the [`WidgetNode<U>`].
    pub(crate) fn update(&self, area: &mut U::Area) {
        self.inner.update(area);
    }

    /// Prints the [`WidgetNode<U>`] to its designated
    /// [`Area`][crate::ui::Area].
    pub(crate) fn print(&self, area: &mut U::Area, palette: &FormPalette) {
        match &self.inner {
            InnerWidget::Normal(widget) => {
                let widget = widget.read();
                let print_info = widget.print_info();
                let print_cfg = widget.print_cfg();
                widget.text().print::<U>(area, print_info, print_cfg, palette);
            }
            InnerWidget::Actionable(widget) => {
                let widget = widget.read();
                let print_info = widget.print_info();
                let print_cfg = widget.print_cfg();
                widget.text().print::<U>(area, print_info, print_cfg, palette);
            }
        }
    }

    /// Wether or not the [`WidgetNode<U>`] needs to be updated.
    pub(crate) fn needs_update(&self) -> bool {
        match &self.inner {
            InnerWidget::Normal(_) => (self.needs_update)(),
            InnerWidget::Actionable(widget) => widget.has_changed() || (self.needs_update)()
        }
    }

    /// Raw inspection of the inner [`NormalWidget<U>`].
    ///
    /// This method should only be used in very specific
    /// circumstances, such as when multiple owners have nested
    /// [`RwData`]s, thus referencing the same inner [`RwData<T>`], in
    /// a way that reading from one point would interfere in the
    /// update detection of the other point.
    pub(crate) fn raw_inspect<B>(&self, f: impl FnOnce(&dyn Widget<U>) -> B) -> B {
        match &self.inner {
            InnerWidget::Normal(widget) => f(&*widget.raw_read()),
            InnerWidget::Actionable(widget) => f(&*widget.raw_read() as &dyn Widget<U>)
        }
    }

    pub(crate) fn as_actionable(&self) -> Option<&RwData<dyn SchemeWidget<U>>> {
        match &self.inner {
            InnerWidget::Normal(_) => None,
            InnerWidget::Actionable(widget) => Some(&widget)
        }
    }

    /// Tries to downcast the [`InnerWidget<U>`]s
    /// [`dyn NormalWidget`][NormalWidget] to a specific type.
    pub fn try_downcast<W>(&self) -> Option<RoData<W>>
    where
        W: Widget<U> + 'static
    {
        match &self.inner {
            InnerWidget::Normal(widget) => {
                let widget = RoData::from(widget);
                widget.try_downcast::<W>().ok()
            }
            InnerWidget::Actionable(widget) => {
                let widget = RoData::from(widget);
                widget.try_downcast::<W>().ok()
            }
        }
    }

    /// Wether or not the [`WidgetNode<U>`] is slow.
    ///
    /// Slow [`WidgetNode<U>`]s get updated asynchronously, as to not
    /// slow down the execution of Parsec.
    pub fn is_slow(&self) -> bool {
        self.is_slow
    }
}

unsafe impl<U> Sync for WidgetNode<U> where U: Ui {}

/// An accumulator used specifically for editing with [`Editor<U>`]s.
#[derive(Default)]
pub struct EditAccum {
    pub chars: isize,
    pub changes: isize
}

/// A struct used by [`InputMethod`][crate::input::InputScheme]s to
/// edit [`Text`].
pub struct WidgetActor<'a, U, AW>
where
    U: Ui + 'static,
    AW: SchemeWidget<U> + ?Sized
{
    clearing_needed: bool,
    widget: &'a RwData<AW>,
    area: &'a U::Area
}

impl<'a, U, Aw> WidgetActor<'a, U, Aw>
where
    U: Ui,
    Aw: SchemeWidget<U> + ?Sized + 'static
{
    /// Returns a new instace of [`WidgetActor<U, AW>`].
    pub(crate) fn new(actionable: &'a RwData<Aw>, area: &'a U::Area) -> Self {
        WidgetActor {
            clearing_needed: false,
            widget: actionable,
            area
        }
    }

    /// Removes all intersecting [`Cursor`]s from the list, keeping
    /// only the last from the bunch.
    fn clear_intersections(&mut self) {
        let mut widget = self.widget.write();
        let Some(cursors) = widget.mut_cursors() else {
            return
        };

        let (mut start, mut end) = cursors[0].pos_range();
        let mut last_index = 0;
        let mut to_remove = Vec::new();

        for (index, cursor) in cursors.iter_mut().enumerate().skip(1) {
            if cursor.try_merge(start, end).is_ok() {
                to_remove.push(last_index);
            }
            (start, end) = cursor.pos_range();
            last_index = index;
        }

        for index in to_remove.iter().rev() {
            cursors.remove(*index);
        }
    }

    /// Edits on every cursor selection in the list.
    pub fn edit_on_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(Editor<U>)
    {
        self.clear_intersections();
        let mut widget = self.widget.write();
        let mut edit_accum = EditAccum::default();
        let cursors = widget.cursors();

        for index in 0..cursors.len() {
            let editor = widget.editor(index, &mut edit_accum);
            f(editor);
        }
    }

    /// Alters every selection on the list.
    pub fn move_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(Mover<U>)
    {
        let mut widget = self.widget.write();
        for index in 0..widget.cursors().len() {
            let mover = widget.mover(index, self.area);
            f(mover);
        }

        // TODO: Figure out a better way to sort.
        widget.mut_cursors().map(|cursors| {
            cursors.sort_unstable_by(|j, k| at_start_ord(&j.range(), &k.range()));
        });
        self.clearing_needed = true;
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(Mover<U>)
    {
        let mut widget = self.widget.write();
        let mover = widget.mover(index, self.area);
        f(mover);

        if let Some(cursors) = widget.mut_cursors() {
            let cursor = cursors.remove(index);
            let range = cursor.range();
            let new_index = match cursors.binary_search_by(|j| at_start_ord(&j.range(), &range)) {
                Ok(index) => index,
                Err(index) => index
            };
            cursors.insert(new_index, cursor);

            if let Some(main_cursor) = widget.mut_main_cursor_index() {
                if index == *main_cursor {
                    *main_cursor = new_index;
                }
            }
        };

        self.clearing_needed = true;
    }

    /// Alters the main cursor's selection.
    pub fn move_main<F>(&mut self, f: F)
    where
        F: FnMut(Mover<U>)
    {
        let main_index = self.widget.read().main_cursor_index();
        self.move_nth(f, main_index);
    }

    /// Alters the last cursor's selection.
    pub fn move_last<F>(&mut self, f: F)
    where
        F: FnMut(Mover<U>)
    {
        let len = self.cursors_len();
        if len > 0 {
            self.move_nth(f, len - 1);
        }
    }

    /// Edits on the nth cursor's selection.
    pub fn edit_on_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(Editor<U>)
    {
        let mut widget = self.widget.write();
        assert!(index < widget.cursors().len(), "Index {index} out of bounds.");
        if self.clearing_needed {
            self.clear_intersections();
            self.clearing_needed = false;
        }

        let mut edit_accum = EditAccum::default();
        let editor = widget.editor(index, &mut edit_accum);
        f(editor);

        for index in (index + 1)..(widget.cursors().len() - 1) {
            // A bit hacky, but the creation of an `Editor` automatically
            // calibrates the cursor's position.
            widget.editor(index, &mut edit_accum);
        }
    }

    /// Edits on the main cursor's selection.
    pub fn edit_on_main<F>(&mut self, f: F)
    where
        F: FnMut(Editor<U>)
    {
        let main_cursor = self.widget.read().main_cursor_index();
        self.edit_on_nth(f, main_cursor);
    }

    /// Edits on the last cursor's selection.
    pub fn edit_on_last<F>(&mut self, f: F)
    where
        F: FnMut(Editor<U>)
    {
        let len = self.cursors_len();
        if len > 0 {
            self.edit_on_nth(f, len - 1);
        }
    }

    /// The main cursor index.
    pub fn main_cursor_index(&self) -> usize {
        self.widget.read().main_cursor_index()
    }

    /// Rotates the main cursor index forward.
    pub fn rotate_main_forward(&mut self) {
        let cursors_len = self.cursors_len();
        if cursors_len == 0 {
            return;
        }

        self.widget.write().mut_main_cursor_index().map(|main_index| {
            *main_index = if *main_index == cursors_len - 1 {
                0
            } else {
                *main_index + 1
            }
        });
    }

    /// Rotates the main cursor index backwards.
    pub fn rotate_main_backwards(&mut self) {
        let cursors_len = self.cursors_len();
        if cursors_len == 0 {
            return;
        }

        self.widget.write().mut_main_cursor_index().map(|main_index| {
            *main_index = if *main_index == 0 {
                cursors_len - 1
            } else {
                *main_index - 1
            }
        });
    }

    /// The amount of active [`Cursor`]s in the [`Text`].
    pub fn cursors_len(&self) -> usize {
        self.widget.read().cursors().len()
    }

    /// Starts a new [`Moment`][crate::history::Moment].
    pub fn new_moment(&mut self) {
        self.widget.write().new_moment();
    }

    /// Undoes the last [`Moment`][crate::history::Moment].
    pub fn undo(&mut self) {
        self.widget.write().undo(self.area);
    }

    /// Redoes the last [`Moment`][crate::history::Moment].
    pub fn redo(&mut self) {
        self.widget.write().redo(self.area);
    }

    pub fn main_cursor(&self) -> Cursor {
        self.widget.read().cursors()[self.main_cursor_index()]
    }

    pub fn nth_cursor(&self, index: usize) -> Option<Cursor> {
        self.widget.read().cursors().get(index).copied()
    }
}

/// Comparets the `left` and `right` [`Range`]s, returning an
/// [`Ordering`], based on the intersection at the start.
fn at_start_ord(left: &Range<usize>, right: &Range<usize>) -> Ordering {
    if left.end > right.start && right.start > left.start {
        std::cmp::Ordering::Equal
    } else if left.start > right.end {
        std::cmp::Ordering::Greater
    } else {
        std::cmp::Ordering::Less
    }
}

/// A list of [`RoData<T>`]s to check.
///
/// If any of them return `true`, the [`WidgetNode<U>`] must be
/// updated.
#[macro_export]
macro_rules! updaters {
    (@ro_data) => {};

    (@ro_data $updaters:ident) => {};

    (@ro_data $updaters:ident, $updater:expr $(, $items:tt)*) => {
        $updaters.push(Box::new($updater));

        updaters!(@ro_data $updaters $(, $items)*);
    };

    () => {
        compile_error!("Without anything to check, a widget cannot be updated!");
    };

    ($($items:tt),*) => {
        {
            let mut updaters = Vec::new();

            updaters!(@ro_data updaters, $($items)*);

            move || updaters.iter().any(|data| data.has_changed())
        }
    }
}

pub use command_line::CommandLine;
pub use file_widget::FileWidget;
pub use line_numbers::{Align, LineNumbers, LineNumbersCfg};
pub use status_line::{file_parts, status_parts, StatusLine, StatusPart};
