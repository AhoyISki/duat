pub mod command_line;
pub mod file_widget;
pub mod line_numbers;
pub mod status_line;

use std::{
    marker::PhantomData,
    path::PathBuf,
    sync::{Arc, Mutex, RwLockWriteGuard},
    thread,
    time::Duration,
};

use crossterm::event::{self, Event, KeyCode};

use crate::{
    config::{Config, RoData, RwData},
    cursor::{Editor, Mover, SpliceAdder, TextCursor},
    input::{KeyRemapper, InputScheme},
    tags::{form::FormPalette, MatchManager},
    text::Text,
    ui::{Direction, EndNode, MidNode, Node, NodeManager, Split, Ui},
};

use self::{
    command_line::{Command, CommandList},
    file_widget::FileWidget,
    status_line::StatusLine,
};

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait Widget<U>: Send
where
    U: Ui,
{
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

    fn print(&mut self);

    /// Scrolls the text vertically by an amount.
    fn scroll_vertically(&mut self, d_y: i32) {}

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, node: &EndNode<U>) {}

    /// If the `Widget` implements `Commandable`. Should return `Some(widget)`
    fn command_list(&mut self) -> Option<CommandList> {
        None
    }
}

pub type WidgetFormer<U> = dyn Fn(
    RwData<EndNode<U>>,
    &mut NodeManager<U>,
    RwData<FileWidget<U>>,
) -> Arc<Mutex<dyn Widget<U>>>;

pub struct Session<U>
where
    U: Ui,
{
    node_manager: NodeManager<U>,
    pub status: StatusLine<U>,
    widgets: Vec<Arc<Mutex<dyn Widget<U>>>>,
    files: Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>)>,
    future_file_widgets: Vec<(Box<WidgetFormer<U>>, Direction, Split)>,
    master_node: RwData<MidNode<U>>,
    all_files_parent: Node<U>,
    match_manager: MatchManager,
    session_control: RwData<SessionControl>,
    global_commands: CommandList,
    active_widget: ActiveWidget<U>,
}

impl<U> Session<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new(
        ui: U, match_manager: MatchManager, config: Config, palette: FormPalette,
        direction: Direction, split: Split,
    ) -> Self {
        let mut node_manager = NodeManager::new(ui);
        let mut node = node_manager.only_child(config, palette, "code").unwrap();

        let (master_node, end_node) = node_manager.split_end(&mut node, direction, split, false);

        let status = StatusLine::new(end_node, &mut node_manager);

        let session_control = RwData::new(SessionControl::default());
        let mut command_list = CommandList::default();
        for command in session_commands::<U>(session_control.clone()) {
            command_list.try_add(Box::new(command)).unwrap();
        }

        let session = Session {
            node_manager,
            status,
            widgets: Vec::new(),
            files: Vec::new(),
            future_file_widgets: Vec::new(),
            master_node,
            all_files_parent: Node::EndNode(node),
            match_manager,
            session_control: RwData::new(SessionControl::default()),
            global_commands: command_list,
            active_widget: ActiveWidget::File(0),
        };

        session
    }

    /// Creates or opens a new file in a given node.
    fn new_file_with_node(&mut self, path: &PathBuf, mut node: RwData<EndNode<U>>) {
        let file = FileWidget::<U>::new(path, node.clone(), &Some(self.match_manager.clone()));
        let (file, mut file_parent) = (RwData::new(file), None);

        for (constructor, direction, split) in &self.future_file_widgets {
            let (mid_node, end_node) = match &mut file_parent {
                None => self.node_manager.split_end(&mut node, *direction, *split, false),
                Some(parent) => self.node_manager.split_mid(parent, *direction, *split, false),
            };

            self.widgets.push(constructor(end_node, &mut self.node_manager, file.clone()));
            file_parent = Some(mid_node);
        }

        self.files.push((file, file_parent));
    }

    pub fn active_file_mut(&mut self) -> RwData<FileWidget<U>> {
        self.files[0].0.clone()
    }

    pub fn active_file(&self) -> RoData<FileWidget<U>> {
        RoData::from(&self.files[0].0)
    }

    pub fn open_arg_files(&mut self) {
        for file in std::env::args().skip(1) {
            self.open_file(&PathBuf::from(file))
        }
        self.master_node.write().resize_children().unwrap();
    }

    pub fn open_file(&mut self, path: &PathBuf) {
        match &self.all_files_parent {
            // If it is an `EndNode`, no file has been opened, or a file was opened without any
            // widgets attached.
            Node::EndNode(node) => {
                if let Some(file) = self.files.get_mut(0) {
                    let (all_files_parent, end_node) = self.node_manager.split_end(
                        &mut file.0.write().end_node_mut(),
                        Direction::Right,
                        Split::Static(50),
                        false,
                    );
                    self.new_file_with_node(path, end_node);
                    self.all_files_parent = Node::MidNode(all_files_parent);
                } else {
                    self.new_file_with_node(path, node.clone());
                    if let Some(node) = &self.files.last().as_ref().unwrap().1 {
                        self.all_files_parent = Node::MidNode(node.clone());
                    }
                }
            }
            Node::MidNode(node) => {
                let (all_files_parent, end_node) = self.node_manager.split_mid(
                    &mut node.clone(),
                    Direction::Right,
                    Split::Static(50),
                    false,
                );
                self.new_file_with_node(path, end_node);
                self.all_files_parent = Node::MidNode(all_files_parent);
            }
        }

        self.status.set_file(RoData::from(&self.files.last().unwrap().0));
    }

    pub fn push_node_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<U> + 'static,
        C: Fn(RwData<EndNode<U>>, &mut NodeManager<U>) -> P,
    {
        let (master_node, end_node) =
            self.node_manager.split_mid(&mut self.master_node, direction, split, false);

        self.master_node = master_node;

        let widget = constructor(end_node.clone(), &mut self.node_manager);

        self.widgets.push(Arc::new(Mutex::new(widget)));
    }

    pub fn push_node_to_file(
        &mut self, constructor: Box<WidgetFormer<U>>, direction: Direction, split: Split,
    ) {
        self.future_file_widgets.push((Box::new(constructor), direction, split));
    }

    pub fn application_loop(&mut self, key_remapper: &mut KeyRemapper<impl InputScheme>) {
        self.node_manager.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let printer = Mutex::new(true);

        // Initial printing.
        self.status.update();
        self.status.print();
        print_files(&mut self.files);
        for widget in &self.widgets {
            let mut widget = widget.lock().unwrap();
            widget.update();
            widget.print();
        }

        let resize_requested = Mutex::new(true);
        let mut iteration = 0;

        // The main loop.
        thread::scope(|s_0| {
            loop {
                if let Ok(mut resize_requested) = resize_requested.try_lock() {
                    if *resize_requested {
                        *resize_requested = false;
                        drop(resize_requested);
                        for widget in self.widgets.iter() {
                            if let Ok(mut widget) = widget.try_lock() {
                                widget.end_node_mut().write().try_set_size().unwrap();
                            }
                        }
                        let _printer_lock = printer.lock().unwrap();
                        for widget in &self.widgets {
                            if let Ok(mut widget) = widget.try_lock() {
                                widget.print();
                            }
                        }
                        print_files(&mut self.files);
                    }
                }

                if let Ok(true) = event::poll(Duration::from_micros(100)) {
                    if let Event::Key(key_event) = event::read().unwrap() {
                        match &mut self.active_widget {
                            ActiveWidget::File(index) => {
                                // NOTE: Temporary.
                                if let KeyCode::Esc = key_event.code {
                                    break;
                                } else {
                                    let mut file = self.files[*index].0.write();
                                    file.update_pre_keys();
                                    key_remapper.send_key_to_editable(key_event, &mut *file);
                                }
                            }
                            ActiveWidget::Other(editable) => {
                                let mut editable = editable.lock().unwrap();
                                editable.update_pre_keys();
                                key_remapper.send_key_to_editable(key_event, &mut *editable);
                            }
                        }
                    }
                } else {
                    continue;
                }

                self.status.update();
                let printer_lock = printer.lock().unwrap();
                self.status.print();
                print_files(&mut self.files);
                drop(printer_lock);
                iteration = 0;

                let widget_indices = widgets_to_update(&self.widgets);
                for index in &widget_indices {
                    let widget = &self.widgets[*index];
                    s_0.spawn(|| {
                        let mut widget = widget.lock().unwrap();
                        widget.update();
                        let widget_resized = widget.end_node().read().resize_requested();
                        let mut resize_requested = resize_requested.lock().unwrap();
                        *resize_requested |= widget_resized;
                        if !widget_resized {
                            drop(resize_requested);
                            let _printer_lock = printer.lock().unwrap();
                            widget.print();
                        }
                    });
                }
            }
        });

        self.node_manager.shutdown();
    }

    fn files(&self) -> Vec<RoData<FileWidget<U>>> {
        self.files.iter().map(|(f, ..)| RoData::from(f)).collect()
    }
}

#[derive(Default)]
pub struct SessionControl {
    should_quit: bool,
    files_to_open: Option<Vec<PathBuf>>,
    go_to_command_line: bool,
}

pub trait EditableWidget<U>: Widget<U>
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

enum ActiveWidget<U>
where
    U: Ui,
{
    File(usize),
    Other(Arc<Mutex<dyn EditableWidget<U>>>),
}

/// Prints all the files.
fn print_files<U>(printer: &mut Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>)>)
where
    U: Ui,
{
    for (file_widget, _) in printer.iter_mut() {
        let mut file_widget = file_widget.write();
        file_widget.update();
        file_widget.print();
    }
}

/// List of widgets that need to be updated.
fn widgets_to_update<U>(widgets: &Vec<Arc<Mutex<dyn Widget<U>>>>) -> Vec<usize>
where
    U: Ui,
{
    let mut indices = Vec::new();

    for (index, widget) in widgets.iter().enumerate() {
        // If the lock is unavailable, that means the widget is being updated.
        if let Ok(lock) = widget.try_lock() {
            if lock.needs_update() {
                indices.push(index);
            }
        }
    }

    indices
}

pub fn session_commands<U>(session: RwData<SessionControl>) -> Vec<Command<SessionControl>>
where
    U: Ui,
{
    let quit_callers = vec![String::from("quit"), String::from("q")];
    let quit_command = Command::new(
        Box::new(|session: &mut SessionControl, _, _| {
            session.should_quit = true;
            Ok(None)
        }),
        quit_callers,
        session.clone(),
    );

    let open_file_callers = vec![String::from("edit"), String::from("e")];
    let open_file_command = Command::new(
        Box::new(|session: &mut SessionControl, _, files| {
            session.files_to_open =
                Some(files.into_iter().map(|file| PathBuf::from(file)).collect());
            Ok(None)
        }),
        open_file_callers,
        session.clone(),
    );

    vec![quit_command, open_file_command]
}
