pub mod command_line;
pub mod file_widget;
pub mod line_numbers;
pub mod status_line;

use std::{
    ops::Deref,
    sync::{Arc, Mutex, MutexGuard},
};

use self::command_line::CommandList;
use crate::{
    config::RwData,
    cursor::{Editor, Mover, SpliceAdder, TextCursor},
    text::{PrintInfo, Text},
    ui::{EndNode, Ui},
};

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait NormalWidget<U>: Send
where
    U: Ui + 'static,
{
    /// Returns an identifier for this `Widget`. They may not be unique.
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

    /// If the `Widget` implements `Commandable`. Should return `Some(widget)`
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
        &'a mut self, index: usize, splice_adder: &'a mut SpliceAdder, end_node: &'a EndNode<U>,
    ) -> Editor<U>;

    fn mover<'a>(&'a mut self, index: usize, end_node: &'a EndNode<U>) -> Mover<U>;

    fn members_for_cursor_tags(&mut self) -> (&mut Text<U>, &[TextCursor], usize);

    fn cursors(&self) -> &[TextCursor];

    fn mut_cursors(&mut self) -> Option<&mut Vec<TextCursor>>;

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

pub enum Widget<U>
where
    U: Ui + ?Sized,
{
    Normal(RwData<dyn NormalWidget<U>>),
    Actionable(RwData<dyn ActionableWidget<U>>),
}

impl<U> Widget<U>
where
    U: Ui + 'static,
{
    pub(crate) fn update(&self, end_node: &mut EndNode<U>) -> bool {
        match self {
            Widget::Normal(widget) => {
                let mut widget = widget.write();
                if widget.needs_update() {
                    widget.update(end_node);
                    true
                } else {
                    false
                }
            }
            Widget::Actionable(widget) => {
                let mut widget = widget.write();
                if widget.needs_update() {
                    widget.update(end_node);
                    true
                } else {
                    false
                }
            }
        }
    }

    pub(crate) fn print(&self, end_node: &mut EndNode<U>) {
        match self {
            Widget::Normal(widget) => {
                let widget = widget.read();
                widget.text().print(end_node, widget.print_info());
            }
            Widget::Actionable(widget) => {
                let widget = widget.read();
                widget.text().print(end_node, widget.print_info());
            }
        }
    }

    pub(crate) fn identifier(&self) -> String {
        match self {
            Widget::Normal(widget) => widget.read().identifier().to_string(),
            Widget::Actionable(widget) => widget.read().identifier().to_string(),
        }
    }
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
        WidgetActor { clearing_needed: false, actionable, end_node }
    }

    /// Removes all intersecting cursors from the list, keeping only the last from the bunch.
    fn clear_intersections(&mut self) {
        let Some(cursors) = self.actionable.mut_cursors() else {
            return
        };

        let mut last_range = cursors[0].range();
        let mut last_index = 0;
        let mut to_remove = Vec::new();

        for (index, cursor) in cursors.iter_mut().enumerate().skip(1) {
            if cursor.range().intersects(&last_range) {
                cursor.merge(&last_range);
                to_remove.push(last_index);
            }
            last_range = cursor.range();
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
        let mut splice_adder = SpliceAdder::default();
        let cursors = self.actionable.cursors();

        for index in 0..cursors.len() {
            let mut editor = self.actionable.editor(index, &mut splice_adder, self.end_node);
            editor.calibrate_on_adder();
            editor.reset_cols();
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
            cursors.sort_unstable_by(|j, k| j.range().at_start_ord(&k.range()));
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
            let new_index = match cursors.binary_search_by(|j| j.range().at_start_ord(&range)) {
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

        let mut splice_adder = SpliceAdder::default();
        let editor = self.actionable.editor(index, &mut splice_adder, self.end_node);
        f(editor);

        if let Some(cursors) = self.actionable.mut_cursors() {
            for cursor in cursors.iter_mut().skip(index + 1) {
                cursor.calibrate_on_adder(&mut splice_adder);
            }
        }
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
            *main_index = if *main_index == cursors_len - 1 { 0 } else { *main_index + 1 }
        });
    }

    pub fn rotate_main_backwards(&mut self) {
        let cursors_len = self.cursors_len();
        if cursors_len == 0 {
            return;
        }

        self.actionable.mut_main_cursor_index().map(|main_index| {
            *main_index = if *main_index == 0 { cursors_len - 1 } else { *main_index - 1 }
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
