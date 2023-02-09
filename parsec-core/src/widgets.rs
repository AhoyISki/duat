pub mod command_line;
pub mod file_widget;
pub mod line_numbers;
pub mod status_line;

use std::{
    marker::PhantomData,
    sync::{Arc, Mutex},
};

use crate::{
    config::RwData,
    cursor::{Editor, Mover, SpliceAdder, TextCursor},
    text::Text,
    ui::{EndNode, MidNode, Ui},
};

use self::{command_line::CommandList, file_widget::FileWidget};

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait NormalWidget<U>: Send
where
    U: Ui,
{
    /// Returns an identifier for this `Widget`. They may not be unique.
    fn identifier(&self) -> String;

    /// Returns the `EndNode` associated with this area.
    fn end_node(&self) -> &RwData<EndNode<U>>;

    /// Returns a mutable reference to the `EndNode` associated with this area.
    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>>;

    /// Updates the widget.
    fn update(&mut self);

    /// Wether or not the widget needs to be updated.
    fn needs_update(&self) -> bool;

    /// The text that this widget prints out.
    fn text(&self) -> &Text;

    /// Prints the contents of this `Widget`.
    fn print(&mut self);

    /// Scrolls the text vertically by an amount.
    fn scroll_vertically(&mut self, d_y: i32) {}

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, node: &EndNode<U>) {}

    /// If the `Widget` implements `Commandable`. Should return `Some(widget)`
    fn command_list(&mut self) -> Option<CommandList> {
        None
    }

    fn editable(&mut self) -> Option<&mut dyn EditableWidget<U>> {
        None
    }
}

pub trait EditableWidget<U>: NormalWidget<U>
where
    U: Ui,
{
    fn editor<'a>(&'a mut self, index: usize, splice_adder: &'a mut SpliceAdder) -> Editor<U>;

    fn mover(&mut self, index: usize) -> Mover<U>;

    fn cursors(&self) -> &[TextCursor];

    fn mut_cursors(&mut self) -> Option<&mut Vec<TextCursor>> {
        None
    }

    fn main_cursor_index(&self) -> usize;

    fn mut_main_cursor_index(&mut self) -> Option<&mut usize> {
        None
    }

    fn new_moment(&mut self) {
        panic!("This implementation of Editable does not have a History of its own.")
    }

    fn undo(&mut self) {
        panic!("This implementation of Editable does not have a History of its own.")
    }

    fn redo(&mut self) {
        panic!("This implementation of Editable does not have a History of its own.")
    }

    fn update_pre_keys(&mut self);
}

pub enum Widget<U>
where
    U: Ui,
{
    Normal(Arc<Mutex<dyn NormalWidget<U>>>),
    Editable(Arc<Mutex<dyn EditableWidget<U>>>),
}

impl<U> Clone for Widget<U>
where
    U: Ui,
{
    fn clone(&self) -> Self {
        match self {
            Widget::Normal(widget) => Widget::Normal(widget.clone()),
            Widget::Editable(widget) => Widget::Editable(widget.clone()),
        }
    }
}

impl<U> Widget<U>
where
    U: Ui,
{
    pub(crate) fn identifier(&self) -> String {
        match self {
            Widget::Normal(widget) => widget.lock().unwrap().identifier(),
            Widget::Editable(widget) => widget.lock().unwrap().identifier(),
        }
    }

    pub(crate) fn update(&self) {
        match self {
            Widget::Normal(widget) => widget.lock().unwrap().update(),
            Widget::Editable(widget) => widget.lock().unwrap().update(),
        }
    }

    pub(crate) fn needs_update(&self) -> bool {
        match self {
            Widget::Normal(widget) => {
                widget.try_lock().map(|widget| widget.needs_update()).unwrap_or(false)
            }
            Widget::Editable(widget) => {
                widget.try_lock().map(|widget| widget.needs_update()).unwrap_or(false)
            }
        }
    }

    pub(crate) fn resize_requested(&self) -> bool {
        match self {
            Widget::Normal(widget) => widget.lock().unwrap().end_node().read().resize_requested(),
            Widget::Editable(widget) => widget.lock().unwrap().end_node().read().resize_requested(),
        }
    }

    pub(crate) fn print(&self) {
        match self {
            Widget::Normal(widget) => widget.try_lock().unwrap().print(),
            Widget::Editable(widget) => widget.lock().unwrap().print(),
        }
    }

    pub(crate) fn try_set_size(&self) -> Result<(), ()> {
        match self {
            Widget::Normal(widget) => {
                if let Ok(mut widget) = widget.try_lock() {
                    widget.end_node_mut().write().try_set_size()
                } else {
                    Err(())
                }
            }
            Widget::Editable(widget) => {
                if let Ok(mut widget) = widget.try_lock() {
                    widget.end_node_mut().write().try_set_size()
                } else {
                    Err(())
                }
            }
        }
    }

    fn try_to_editable(&self) -> Option<Arc<Mutex<dyn EditableWidget<U>>>> {
        match self {
            Widget::Normal(_) => None,
            Widget::Editable(widget) => Some(widget.clone()),
        }
    }
}

pub struct WidgetActor<'a, U, E>
where
    U: Ui,
    E: EditableWidget<U> + ?Sized,
{
    clearing_needed: bool,
    editable: &'a mut E,
    _ghost: PhantomData<U>,
}

impl<'a, U, E> WidgetActor<'a, U, E>
where
    U: Ui,
    E: EditableWidget<U> + ?Sized,
{
    /// Removes all intersecting cursors from the list, keeping only the last from the bunch.
    fn clear_intersections(&mut self) {
        let Some(cursors) = self.editable.mut_cursors() else {
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
        for index in 0..self.editable.cursors().len() {
            let mut editor = self.editable.editor(index, &mut splice_adder);
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
        for index in 0..self.editable.cursors().len() {
            let mover = self.editable.mover(index);
            f(mover);
        }

        // TODO: Figure out a better way to sort.
        self.editable
            .mut_cursors()
            .map(|cursors| cursors.sort_unstable_by(|j, k| j.range().at_start_ord(&k.range())));
        self.clearing_needed = true;
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(Mover<U>),
    {
        let mover = self.editable.mover(index);
        f(mover);

        let Some(cursors) = self.editable.mut_cursors() else {
            return;
        };
        let cursor = cursors.remove(index);
        let range = cursor.range();
        let new_index = match cursors.binary_search_by(|j| j.range().at_start_ord(&range)) {
            Ok(index) => index,
            Err(index) => index,
        };
        cursors.insert(new_index, cursor);
        drop(cursors);

        if let Some(main_cursor) = self.editable.mut_main_cursor_index() {
            if index == *main_cursor {
                *main_cursor = new_index;
            }
        }

        self.clearing_needed = true;
    }

    /// Alters the main cursor's selection.
    pub fn move_main<F>(&mut self, f: F)
    where
        F: FnMut(Mover<U>),
    {
        let main_cursor = self.editable.main_cursor_index();
        self.move_nth(f, main_cursor);
    }

    /// Alters the last cursor's selection.
    pub fn move_last<F>(&mut self, f: F)
    where
        F: FnMut(Mover<U>),
    {
        let cursors = &self.editable.cursors();
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
        let editor = self.editable.editor(index, &mut splice_adder);
        f(editor);
        if let Some(cursors) = self.editable.mut_cursors() {
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
        let main_cursor = self.editable.main_cursor_index();
        self.edit_on_nth(f, main_cursor);
    }

    /// Edits on the last cursor's selection.
    pub fn edit_on_last<F>(&mut self, f: F)
    where
        F: FnMut(Editor<U>),
    {
        let cursors = &self.editable.cursors();
        if !cursors.is_empty() {
            let len = cursors.len();
            drop(cursors);
            self.edit_on_nth(f, len - 1);
        }
    }

    /// The main cursor index.
    pub fn main_cursor_index(&self) -> usize {
        self.editable.main_cursor_index()
    }

    pub fn rotate_main_forward(&mut self) {
        let cursors_len = self.editable.cursors().len();
        let Some(cursor) = self.editable.mut_main_cursor_index() else {
            return;
        };

        *cursor = if *cursor == cursors_len - 1 { 0 } else { *cursor + 1 }
    }

    pub fn cursors_len(&self) -> usize {
        self.editable.cursors().len()
    }

    pub fn new_moment(&mut self) {
        self.editable.new_moment();
    }

    pub fn undo(&mut self) {
        self.editable.undo();
    }

    pub fn redo(&mut self) {
        self.editable.redo();
    }
}

impl<'a, U, E> From<&'a mut E> for WidgetActor<'a, U, E>
where
    U: Ui,
    E: EditableWidget<U> + ?Sized,
{
    fn from(value: &'a mut E) -> Self {
        WidgetActor { editable: value, clearing_needed: false, _ghost: PhantomData::default() }
    }
}

#[derive(Clone)]
pub enum TargetWidget {
    File(String),
    FirstLocal(String),
    LastLocal(String),
    FirstGlobal(String),
    LastGlobal(String),
    Absolute(String, usize),
}

impl TargetWidget {
    pub(crate) fn find_file<U>(
        &self,
        files: &Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>, Vec<(Widget<U>, usize)>)>,
    ) -> Option<usize>
    where
        U: Ui,
    {
        let TargetWidget::File(name) = self else {
            return None;
        };

        files
            .iter()
            .enumerate()
            .find(|(_, (file, _, _))| file.read().name() == *name)
            .map(|(index, _)| index)
    }

    pub(crate) fn find_editable<U>(
        &self, widgets: &Vec<(Widget<U>, usize)>,
    ) -> Option<Arc<Mutex<dyn EditableWidget<U>>>>
    where
        U: Ui,
    {
        let mut widgets = widgets.iter();

        let result = match self {
            TargetWidget::File(_) => None,
            TargetWidget::FirstLocal(_) => todo!(),
            TargetWidget::LastLocal(_) => todo!(),
            TargetWidget::FirstGlobal(identifier) => {
                widgets.find(|(widget, _)| widget.identifier() == *identifier)
            }
            TargetWidget::LastGlobal(identifier) => {
                widgets.rev().find(|(widget, _)| widget.identifier() == *identifier)
            }
            TargetWidget::Absolute(identifier, index) => {
                widgets.find(|(widget, cmp)| widget.identifier() == *identifier && cmp == index)
            }
        };

        result.map(|(widget, _)| widget.try_to_editable()).flatten()
    }
}
