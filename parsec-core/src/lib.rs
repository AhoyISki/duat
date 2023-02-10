pub mod action;
pub mod config;
pub mod cursor;
pub mod input;
pub mod tags;
pub mod text;
pub mod ui;
pub mod widgets;

use std::{
    cmp::min,
    path::PathBuf,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use config::{Config, RoData, RwData};
use crossterm::event::{self, Event};
use cursor::TextPos;
use input::{InputScheme, KeyRemapper};
use tags::{form::FormPalette, MatchManager};
use text::{PrintInfo, Text};
use ui::{Area, Direction, EndNode, Label, MidNode, Node, NodeManager, Split, Ui};
use widgets::{
    command_line::{Command, CommandList},
    file_widget::FileWidget,
    status_line::StatusLine,
    ActionableWidget, NormalWidget, TargetWidget, Widget,
};

pub type WidgetFormer<U> =
    dyn Fn(RwData<EndNode<U>>, &mut NodeManager<U>, RwData<FileWidget<U>>) -> Widget<U>;

pub struct Session<U>
where
    U: Ui,
{
    node_manager: NodeManager<U>,
    pub status: StatusLine<U>,
    widgets: Vec<(Widget<U>, usize)>,
    files: Vec<RwData<FileWidget<U>>>,
    future_file_widgets: Vec<(Box<WidgetFormer<U>>, Direction, Split)>,
    master_node: RwData<MidNode<U>>,
    all_files_parent: Node<U>,
    match_manager: MatchManager,
    control: RwData<SessionControl<U>>,
    global_commands: RwData<CommandList>,
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

        let control = RwData::new(SessionControl::<U>::default());
        let mut command_list = CommandList::default();
        for command in session_commands::<U>(control.clone()) {
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
            control,
            global_commands: RwData::new(command_list),
        };

        session
    }

    /// Creates or opens a new file in a given node.
    fn new_file_with_node(&mut self, path: &PathBuf, mut node: RwData<EndNode<U>>) {
        let file = FileWidget::<U>::new(path, node.clone(), &Some(self.match_manager.clone()));
        let mut file = RwData::new(file);
        let rw_file = file.clone();

        for (constructor, direction, split) in &self.future_file_widgets {
            let mut file_lock = file.write();
            let (mid_node, end_node) = match file_lock.mid_node_mut() {
                None => self.node_manager.split_end(&mut node, *direction, *split, false),
                Some(parent) => self.node_manager.split_mid(parent, *direction, *split, false),
            };
            drop(file_lock);

            let widget = constructor(end_node, &mut self.node_manager, rw_file.clone());
            let index = self.get_next_index(&widget);
            self.widgets.push((widget.clone(), index));

            let mut file = file.write();

            file.side_widgets.push((widget, index));
            *file.mid_node_mut() = Some(mid_node);
            file.end_node_mut().write().is_active = true;

        }

		let mut file_lock = file.write();
        let (text, cursors, main_index) = file_lock.members_for_cursor_tags();
        text.add_cursor_tags(cursors, main_index);
        drop(file_lock);

        self.files.push(file);
    }

    /// Returns the next index for a given `Widget` type.
    fn get_next_index(&self, widget: &Widget<U>) -> usize {
        let identifier = widget.identifier();
        let mut new_index = 0;

        for (widget, index) in self.widgets.iter().rev() {
            if widget.identifier() == identifier {
                new_index = index + 1;
                break;
            }
        }
        new_index
    }

    pub fn active_file(&self) -> RoData<FileWidget<U>> {
        RoData::from(&self.files[0])
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
                        &mut file.write().end_node_mut(),
                        Direction::Right,
                        Split::Static(50),
                        false,
                    );
                    self.new_file_with_node(path, end_node);
                    self.all_files_parent = Node::MidNode(all_files_parent);
                } else {
                    self.new_file_with_node(path, node.clone());
                    if let Some(node) = &self.files.last().as_ref().unwrap().read().mid_node() {
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

        self.status.set_file(RoData::from(self.files.last().unwrap()));
        self.control.write().files_len += 1;
    }

    pub fn push_widget_to_edge<C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        C: Fn(RwData<EndNode<U>>, &mut Session<U>) -> Widget<U>,
    {
        let (master_node, end_node) =
            self.node_manager.split_mid(&mut self.master_node, direction, split, false);

        self.master_node = master_node;

        let widget = constructor(end_node.clone(), self);
        let index = self.get_next_index(&widget);
        self.widgets.push((widget, index));
    }

    pub fn push_widget_to_file(
        &mut self, constructor: Box<WidgetFormer<U>>, direction: Direction, split: Split,
    ) {
        self.future_file_widgets.push((Box::new(constructor), direction, split));
    }

    /// The full loop. A break in this loop means quitting Parsec.
    pub fn main_loop<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        self.node_manager.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let resize_requested = Mutex::new(true);

        // The main loop.
        loop {
            // Initial printing.
            self.status.update();
            print_widget(&mut self.status);
            print_files(&mut self.files);
            for (widget, _) in &mut self.widgets {
                widget.update();
                widget.print();
            }

            self.session_loop(&resize_requested, key_remapper);

            if self.control.read().should_quit {
                break;
            }
        }

        self.node_manager.shutdown();
    }

    /// The primary application loop, executed while no breaking commands have been sent to
    /// `SessionControl`.
    fn session_loop<I>(&mut self, resize_requested: &Mutex<bool>, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        thread::scope(|s_0| loop {
            resize_widgets(resize_requested, &self.widgets, &mut self.files);

            let mut control = self.control.write();
            control.switch_to_target(&mut self.files, &self.widgets);
            if control.break_loop {
                break;
            }
            drop(control);

            if let Ok(true) = event::poll(Duration::from_micros(100)) {
                let active_file = &mut self.files[self.control.read().active_file];
                send_event(key_remapper, &mut self.control, active_file);
            } else {
                continue;
            }

            self.status.update();
            print_widget(&mut self.status);
            print_files(&mut self.files);

            let widget_indices = widgets_to_update(&self.widgets);
            for index in &widget_indices {
                let (widget, _) = &self.widgets[*index];
                s_0.spawn(|| {
                    widget.update();
                    if !widget.resize_requested() {
                        widget.print();
                    } else {
                        *resize_requested.lock().unwrap() = true;
                    }
                });
            }
        });
    }

    /// The list of commands that are considered global, as oposed to local to a file.
    pub fn global_commands(&self) -> RwData<CommandList> {
        self.global_commands.clone()
    }
}

pub struct SessionControl<U>
where
    U: Ui,
{
    should_quit: bool,
    files_to_open: Vec<PathBuf>,
    target_widget: Option<TargetWidget>,
    files_len: usize,
    active_file: usize,
    active_widget: Option<Arc<Mutex<dyn ActionableWidget<U>>>>,
    break_loop: bool,
}

impl<U> Default for SessionControl<U>
where
    U: Ui,
{
    fn default() -> Self {
        SessionControl {
            should_quit: false,
            files_to_open: Vec::new(),
            target_widget: None,
            files_len: 0,
            active_file: 0,
            active_widget: None,
            break_loop: false,
        }
    }
}

impl<U> SessionControl<U>
where
    U: Ui,
{
    /// Returns to the active file from any widget, including the file itself.
    pub fn return_to_file(&mut self) {
        self.target_widget = Some(TargetWidget::Absolute(String::from("file"), self.active_file));
    }

    /// Switches the input to another `Widget`.
    pub fn switch_widget(&mut self, target_widget: TargetWidget) {
        self.target_widget = Some(target_widget);
    }

    /// Quits Parsec.
    pub fn quit(&mut self) {
        self.break_loop = true;
        self.should_quit = true;
    }

    /// The creation index of the currently active file.
    pub fn active_file(&self) -> usize {
        self.active_file
    }

    /// The number of opened files.
    pub fn files_len(&self) -> usize {
        self.files_len
    }

    fn unfocus_old(&mut self, active_file: &mut RwData<FileWidget<U>>) {
        if let Some(widget) = &mut self.active_widget {
            let mut widget = widget.lock().unwrap();
            widget.end_node_mut().write().is_active = false;
            widget.on_unfocus();
            print_widget(&mut *widget);
        } else {
            let mut active_file = active_file.write();
            active_file.end_node_mut().write().is_active = false;
            active_file.on_unfocus();
            print_widget(&mut *active_file);
        }
    }

    /// Switches the active widget to `self.target_widget`.
    fn switch_to_target(
        &mut self, files: &mut Vec<RwData<FileWidget<U>>>, widgets: &Vec<(Widget<U>, usize)>,
    ) {
        if let Some(target) = self.target_widget.take() {
            let Some(file) = target.find_file(&files) else {
                let mut new_widget = target.find_editable(widgets);
                if let Some(widget) = new_widget.take() {
                    self.unfocus_old(&mut files[self.active_file]);

                    let mut widget_lock = widget.lock().unwrap();
                    widget_lock.on_focus();
                    widget_lock.end_node_mut().write().is_active = true;
                    print_widget(&mut *widget_lock);
                    drop(widget_lock);
                    self.active_widget = Some(widget);
                }

                return;
            };

            self.unfocus_old(&mut files[self.active_file]);
            self.active_file = file;
            let mut file = files[self.active_file].write();
            file.end_node_mut().write().is_active = true;
            print_widget(&mut *file);
            self.active_widget = None;
        }
    }
}

pub fn session_commands<U>(session: RwData<SessionControl<U>>) -> Vec<Command<SessionControl<U>>>
where
    U: Ui,
{
    let quit_callers = vec![String::from("quit"), String::from("q")];
    let quit_command = Command::new(
        Box::new(|session: &mut SessionControl<U>, _, _| {
            session.break_loop = true;
            session.should_quit = true;
            Ok(None)
        }),
        quit_callers,
        session.clone(),
    );

    let open_file_callers = vec![String::from("edit"), String::from("e")];
    let open_file_command = Command::new(
        Box::new(|session: &mut SessionControl<U>, _, files| {
            session.files_to_open = files.into_iter().map(|file| PathBuf::from(file)).collect();
            Ok(None)
        }),
        open_file_callers,
        session.clone(),
    );

    vec![quit_command, open_file_command]
}

fn print_widget<U, W>(widget: &mut W)
where
    U: Ui,
    W: NormalWidget<U> + ?Sized,
{
    let (text, end_node, print_info) = widget.members_for_printing();
    text.print(&mut end_node.write(), print_info);
}

/// Prints all the files.
fn print_files<U>(files: &mut Vec<RwData<FileWidget<U>>>)
where
    U: Ui,
{
    for file_widget in files.iter_mut() {
        let mut file_widget = file_widget.write();
        file_widget.update();
        print_widget(&mut *file_widget);
    }
}

/// List of widgets that need to be updated.
fn widgets_to_update<U>(widgets: &Vec<(Widget<U>, usize)>) -> Vec<usize>
where
    U: Ui,
{
    let mut indices = Vec::new();

    for (index, (widget, _)) in widgets.iter().enumerate() {
        // If the lock is unavailable, that means the widget is being updated.
        if widget.needs_update() {
            indices.push(index);
        }
    }

    indices
}

fn resize_widgets<U>(
    resize_requested: &Mutex<bool>, widgets: &Vec<(Widget<U>, usize)>,
    files: &mut Vec<RwData<FileWidget<U>>>,
) where
    U: Ui,
{
    if let Ok(mut resize_requested) = resize_requested.try_lock() {
        if *resize_requested {
            *resize_requested = false;
            drop(resize_requested);
            widgets.iter().for_each(|(widget, _)| widget.try_set_size().unwrap());
            widgets.iter().for_each(|(widget, _)| widget.print());
            print_files(files);
        }
    }
}

fn send_event<U, I>(
    key_remapper: &mut KeyRemapper<I>, control: &mut RwData<SessionControl<U>>,
    active_file: &mut RwData<FileWidget<U>>,
) where
    U: Ui,
    I: InputScheme,
{
    if let Event::Key(key_event) = event::read().unwrap() {
        let mut control = control.write();
        if let Some(widget) = control.active_widget.take() {
            let mut widget_lock = widget.lock().unwrap();
            let (text, cursors, main_index) = widget_lock.members_for_cursor_tags();
            text.remove_cursor_tags(cursors, main_index);

            key_remapper.send_key_to_actionable(key_event, &mut *widget_lock, &mut control);

            let (text, cursors, main_index) = widget_lock.members_for_cursor_tags();
            text.add_cursor_tags(cursors, main_index);

            // If the widget is no longer valid, return to the file.
            if widget_lock.still_valid() {
                drop(widget_lock);
                control.active_widget = Some(widget);
            }
        } else {
            let mut file = active_file.write();
            let (text, cursors, main_index) = file.members_for_cursor_tags();
            text.remove_cursor_tags(cursors, main_index);

            key_remapper.send_key_to_actionable(key_event, &mut *file, &mut control);

            let (text, cursors, main_index) = file.members_for_cursor_tags();
            text.add_cursor_tags(cursors, main_index);
        }
    }
}

/// Given a position (which is assumed to be on the line), will return the position at its start.
pub fn get_line_start(pos: TextPos, line: &String) -> TextPos {
    TextPos { byte: pos.byte - line.char_indices().take(pos.col).count(), col: 0, row: pos.row }
}

/// Creates a vector of `&str`s from a `String`, making sure to keep at least one empty
/// string at the end, in case of an empty, or `\n` terminated string.
fn split_string_lines(string: &String) -> Vec<String> {
    if string.is_empty() {
        vec![String::from("")]
    } else {
        let mut lines: Vec<String> = string.split_inclusive('\n').map(|s| s.to_string()).collect();
        if string.ends_with('\n') {
            lines.push(String::from(""));
        }
        lines
    }
}

/// Gets the line-byte at a given col in a string.
pub fn get_byte_at_col(col: usize, text: &String) -> usize {
    text.char_indices().nth(col).map(|c| c.0).unwrap_or(text.len())
}

/// An empty list of `String`s, representing an empty edit/file.
pub fn empty_edit() -> Vec<String> {
    vec![String::from("")]
}

// NOTE: Will definitely break once folding becomes a thing.
/// The last line that could possibly be printed.
pub fn max_line(text: &Text, print_info: &PrintInfo, node: &EndNode<impl Ui>) -> usize {
    min(print_info.top_row + node.label.read().area().height(), text.lines().len() - 1)
}

//////////// Useful for testing.
pub static mut FOR_TEST: usize = 0;

/// Internal macro used to log information.
#[macro_export]
macro_rules! log_info {
    ($($text:tt)*) => {
        {
            use std::{fs, io::Write};
            let mut log = fs::OpenOptions::new().append(true).open("log").unwrap();
            log.write_fmt(format_args!($($text)*)).unwrap();
        }
    }
}
