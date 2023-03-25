pub mod command_line;
pub mod file_widget;
pub mod line_numbers;
pub mod status_line;

use std::{
    any::Any,
    cmp::Ordering,
    ops::{Deref, Range},
    sync::{Arc, Mutex, MutexGuard},
};

use self::command_line::CommandList;
use crate::{
    config::{DownCastableData, RoData, RwData},
    position::{Cursor, Editor, Mover},
    text::{PrintInfo, Text},
    ui::{EndNode, Ui},
};

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait NormalWidget<U>: Send + DownCastableData
where
    U: Ui + Any + 'static,
{
    /// Returns an identifier for this `Widget`. They may not be
    /// unique.
    fn identifier(&self) -> &str;

    /// Updates the widget.
    fn update(&mut self, end_node: &mut EndNode<U>);

    /// Wether or not the widget needs to be updated.
    fn needs_update(&self) -> bool;

    /// The text that this widget prints out.
    fn text(&self) -> &Text<U>;

    /// Scrolls the text vertically by an amount.
    fn scroll_vertically(&mut self, _d_y: i32) {}

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, _end_node: &EndNode<U>) {}

    /// If the `Widget` implements `Commandable`. Should return
    /// `Some(widget)`
    fn command_list(&mut self) -> Option<CommandList> {
        None
    }

    fn editable(&mut self) -> Option<&mut dyn ActionableWidget<U>> {
        None
    }

    fn print_info(&self) -> PrintInfo {
        PrintInfo::default()
    }
}

pub trait ActionableWidget<U>: NormalWidget<U>
where
    U: Ui + 'static,
{
    fn editor<'a>(
        &'a mut self,
        index: usize,
        edit_accum: &'a mut EditAccum,
        end_node: &'a EndNode<U>,
    ) -> Editor<U>;

    fn mover<'a>(&'a mut self, index: usize, end_node: &'a EndNode<U>) -> Mover<U>;

    fn members_for_cursor_tags(&mut self) -> (&mut Text<U>, &[Cursor], usize);

    fn cursors(&self) -> &[Cursor];

    fn mut_cursors(&mut self) -> Option<&mut Vec<Cursor>>;

    fn main_cursor_index(&self) -> usize;

    fn mut_main_cursor_index(&mut self) -> Option<&mut usize>;

    fn new_moment(&mut self) {
        panic!("This implementation of Editable does not have a History of its own.")
    }

    fn undo(&mut self, _end_node: &EndNode<U>) {
        panic!("This implementation of Editable does not have a History of its own.")
    }

    fn redo(&mut self, _end_node: &EndNode<U>) {
        panic!("This implementation of Editable does not have a History of its own.")
    }

    fn on_focus(&mut self, _end_node: &mut EndNode<U>) {}

    fn on_unfocus(&mut self, _end_node: &mut EndNode<U>) {}

    fn still_valid(&self) -> bool {
        true
    }
}

pub struct RoMutexGuard<'a, T>(MutexGuard<'a, T>)
where
    T: ?Sized + 'a;

impl<T> Deref for RoMutexGuard<'_, T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct RoMutex<T>(Arc<Mutex<T>>)
where
    T: ?Sized;

impl<T> From<Arc<Mutex<T>>> for RoMutex<T>
where
    T: ?Sized,
{
    fn from(value: Arc<Mutex<T>>) -> Self {
        RoMutex(value.clone())
    }
}

enum InnerWidget<U>
where
    U: Ui + ?Sized,
{
    Normal(RwData<dyn NormalWidget<U>>),
    Actionable(RwData<dyn ActionableWidget<U>>),
}

pub struct Widget<U>
where
    U: Ui + ?Sized,
{
    inner: InnerWidget<U>,
    updaters: Vec<Box<dyn Fn() -> bool>>,
}

impl<U> Widget<U>
where
    U: Ui + 'static,
{
    pub fn normal(
        widget: Arc<Mutex<dyn NormalWidget<U>>>,
        updaters: Vec<Box<dyn Fn() -> bool>>,
    ) -> Widget<U> {
        // assert!(updaters.len() > 0, "Without any updaters, this widget can
        // never update");
        Widget {
            inner: InnerWidget::Normal(RwData::new_unsized(widget)),
            updaters,
        }
    }
    pub fn actionable(
        widget: Arc<Mutex<dyn ActionableWidget<U>>>,
        updaters: Vec<Box<dyn Fn() -> bool>>,
    ) -> Widget<U> {
        Widget {
            inner: InnerWidget::Actionable(RwData::new_unsized(widget)),
            updaters,
        }
    }

    pub(crate) fn update(&self, end_node: &mut EndNode<U>) {
        match &self.inner {
            InnerWidget::Normal(widget) => {
                let mut widget = widget.write();
                widget.update(end_node);
            }
            InnerWidget::Actionable(widget, ..) => {
                let mut widget = widget.write();
                widget.update(end_node);
            }
        }
    }

    pub(crate) fn print(&self, end_node: &mut EndNode<U>) {
        match &self.inner {
            InnerWidget::Normal(widget) => {
                let widget = widget.read();
                widget.text().print(end_node, widget.print_info());
            }
            InnerWidget::Actionable(widget, ..) => {
                let widget = widget.read();
                widget.text().print(end_node, widget.print_info());
            }
        }
    }

    pub(crate) fn identifier(&self) -> String {
        match &self.inner {
            InnerWidget::Normal(widget) => widget.read().identifier().to_string(),
            InnerWidget::Actionable(widget, ..) => widget.read().identifier().to_string(),
        }
    }

    pub fn needs_update(&self) -> bool {
        match &self.inner {
            InnerWidget::Normal(_) => self.updaters.iter().any(|updater| updater()),
            InnerWidget::Actionable(actionable) => {
                actionable.has_changed() || self.updaters.iter().any(|updater| updater())
            }
        }
    }

    pub fn get_actionable(&self) -> Option<&RwData<dyn ActionableWidget<U>>> {
        match &self.inner {
            InnerWidget::Normal(_) => None,
            InnerWidget::Actionable(widget) => Some(&widget),
        }
    }
}

unsafe impl<U> Sync for Widget<U> where U: Ui {}

#[derive(Default)]
pub struct EditAccum {
    pub chars: isize,
    pub changes: isize,
}

pub struct WidgetActor<'a, U, E>
where
    U: Ui + 'static,
    E: ActionableWidget<U> + ?Sized,
{
    clearing_needed: bool,
    actionable: &'a mut E,
    end_node: &'a EndNode<U>,
}

impl<'a, U, E> WidgetActor<'a, U, E>
where
    U: Ui,
    E: ActionableWidget<U> + ?Sized,
{
    /// Returns a new instace of `WidgetActor<U, E>`.
    pub(crate) fn new(actionable: &'a mut E, end_node: &'a EndNode<U>) -> Self {
        WidgetActor {
            clearing_needed: false,
            actionable,
            end_node,
        }
    }

    /// Removes all intersecting cursors from the list, keeping only
    /// the last from the bunch.
    fn clear_intersections(&mut self) {
        let Some(cursors) = self.actionable.mut_cursors() else {
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
        F: FnMut(Editor<U>),
    {
        self.clear_intersections();
        let mut edit_accum = EditAccum::default();
        let cursors = self.actionable.cursors();

        for index in 0..cursors.len() {
            let editor = self.actionable.editor(index, &mut edit_accum, self.end_node);
            f(editor);
        }
    }

    /// Alters every selection on the list.
    pub fn move_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(Mover<U>),
    {
        for index in 0..self.actionable.cursors().len() {
            let mover = self.actionable.mover(index, self.end_node);
            f(mover);
        }

        // TODO: Figure out a better way to sort.
        self.actionable.mut_cursors().map(|cursors| {
            cursors.sort_unstable_by(|j, k| at_start_ord(&j.range(), &k.range()));
        });
        self.clearing_needed = true;
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(Mover<U>),
    {
        let mover = self.actionable.mover(index, self.end_node);
        f(mover);

        if let Some(cursors) = self.actionable.mut_cursors() {
            let cursor = cursors.remove(index);
            let range = cursor.range();
            let new_index = match cursors.binary_search_by(|j| at_start_ord(&j.range(), &range)) {
                Ok(index) => index,
                Err(index) => index,
            };
            cursors.insert(new_index, cursor);

            if let Some(main_cursor) = self.actionable.mut_main_cursor_index() {
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
        F: FnMut(Mover<U>),
    {
        let main_index = self.actionable.main_cursor_index();
        self.move_nth(f, main_index);
    }

    /// Alters the last cursor's selection.
    pub fn move_last<F>(&mut self, f: F)
    where
        F: FnMut(Mover<U>),
    {
        let cursors = &self.actionable.cursors();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.move_nth(f, len - 1);
        }
    }

    /// Edits on the nth cursor's selection.
    pub fn edit_on_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(Editor<U>),
    {
        if self.clearing_needed {
            self.clear_intersections();
            self.clearing_needed = false;
        }

        let mut edit_accum = EditAccum::default();
        let editor = self.actionable.editor(index, &mut edit_accum, self.end_node);
        f(editor);

        let mut new_cursors = Vec::from(&self.actionable.cursors()[(index + 1)..]);
        for cursor in &mut new_cursors {
            cursor.calibrate_on_accum(&edit_accum, self.actionable.text().inner());
        }

        self.actionable
            .mut_cursors()
            .unwrap()
            .splice((index + 1).., new_cursors.into_iter());
    }

    /// Edits on the main cursor's selection.
    pub fn edit_on_main<F>(&mut self, f: F)
    where
        F: FnMut(Editor<U>),
    {
        let main_cursor = self.actionable.main_cursor_index();
        self.edit_on_nth(f, main_cursor);
    }

    /// Edits on the last cursor's selection.
    pub fn edit_on_last<F>(&mut self, f: F)
    where
        F: FnMut(Editor<U>),
    {
        let cursors = &self.actionable.cursors();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.edit_on_nth(f, len - 1);
        }
    }

    /// The main cursor index.
    pub fn main_cursor_index(&self) -> usize {
        self.actionable.main_cursor_index()
    }

    pub fn rotate_main_forward(&mut self) {
        let cursors_len = self.cursors_len();
        if cursors_len == 0 {
            return;
        }

        self.actionable.mut_main_cursor_index().map(|main_index| {
            *main_index = if *main_index == cursors_len - 1 {
                0
            } else {
                *main_index + 1
            }
        });
    }

    pub fn rotate_main_backwards(&mut self) {
        let cursors_len = self.cursors_len();
        if cursors_len == 0 {
            return;
        }

        self.actionable.mut_main_cursor_index().map(|main_index| {
            *main_index = if *main_index == 0 {
                cursors_len - 1
            } else {
                *main_index - 1
            }
        });
    }

    pub fn cursors_len(&self) -> usize {
        self.actionable.cursors().len()
    }

    pub fn new_moment(&mut self) {
        self.actionable.new_moment();
    }

    pub fn undo(&mut self) {
        self.actionable.undo(self.end_node);
    }

    pub fn redo(&mut self) {
        self.actionable.redo(self.end_node);
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
