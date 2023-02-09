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
use text::Text;
use ui::{Area, Direction, EndNode, Label, MidNode, Node, NodeManager, Split, Ui};
use widgets::{
    command_line::{Command, CommandList},
    file_widget::{FileWidget, PrintInfo},
    status_line::StatusLine,
    EditableWidget, NormalWidget, TargetWidget, Widget,
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
    files: Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>, Vec<(Widget<U>, usize)>)>,
    future_file_widgets: Vec<(Box<WidgetFormer<U>>, Direction, Split)>,
    master_node: RwData<MidNode<U>>,
    all_files_parent: Node<U>,
    match_manager: MatchManager,
    session_control: RwData<SessionControl>,
    global_commands: CommandList,
    active_file: usize,
    active_widget: Option<Arc<Mutex<dyn EditableWidget<U>>>>,
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
            active_file: 0,
            active_widget: None,
        };

        session
    }

    /// Creates or opens a new file in a given node.
    fn new_file_with_node(&mut self, path: &PathBuf, mut node: RwData<EndNode<U>>) {
        let file = FileWidget::<U>::new(path, node.clone(), &Some(self.match_manager.clone()));
        let (file, mut file_parent) = (RwData::new(file), None);

        let mut widgets = Vec::new();
        for (constructor, direction, split) in &self.future_file_widgets {
            let (mid_node, end_node) = match &mut file_parent {
                None => self.node_manager.split_end(&mut node, *direction, *split, false),
                Some(parent) => self.node_manager.split_mid(parent, *direction, *split, false),
            };

            let mut widget = constructor(end_node, &mut self.node_manager, file.clone());
            let index = self.get_next_index(&widget);
            widgets.push((widget.clone(), index));
            self.widgets.push((widget, index));
            file_parent = Some(mid_node);
        }

        self.files.push((file, file_parent, widgets));
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

    pub fn push_widget_to_edge<C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        C: Fn(RwData<EndNode<U>>, &mut NodeManager<U>) -> Widget<U>,
    {
        let (master_node, end_node) =
            self.node_manager.split_mid(&mut self.master_node, direction, split, false);

        self.master_node = master_node;

        let mut widget = constructor(end_node.clone(), &mut self.node_manager);
        let index = self.get_next_index(&widget);
        self.widgets.push((widget, index));
    }

    pub fn push_widget_to_file(
        &mut self, constructor: Box<WidgetFormer<U>>, direction: Direction, split: Split,
    ) {
        self.future_file_widgets.push((Box::new(constructor), direction, split));
    }

    pub fn application_loop<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        self.node_manager.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let printer = Mutex::new(true);

        // Initial printing.
        self.status.update();
        self.status.print();
        print_files(&mut self.files);
        for (widget, _) in &mut self.widgets {
            widget.update();
            widget.print();
        }

        let resize_requested = Mutex::new(true);
        let mut iteration = 0;

        // The main loop.
        thread::scope(|s_0| {
            loop {
                resize_widgets(&resize_requested, &printer, &self.widgets, &mut self.files);

                if let Ok(true) = event::poll(Duration::from_micros(100)) {
                    if let Event::Key(key_event) = event::read().unwrap() {
                        if let Some(widget) = &mut self.active_widget {
                            let mut widget = widget.lock().unwrap();
                            widget.update_pre_keys();
                            key_remapper.send_key_to_editable(key_event, &mut *widget);
                            // NOTE: Temporary.
                        } else {
                            let mut file = self.files[self.active_file].0.write();
                            file.update_pre_keys();
                            key_remapper.send_key_to_editable(key_event, &mut *file);
                        }
                    }
                } else {
                    continue;
                }

                let session_control = self.session_control.read();
                if session_control.should_quit {
                    break;
                } else if let Some(target) = &session_control.target_widget {
                    if let Some(file) = target.find_file(&self.files) {
                        self.active_file = file;
                    } else {
                        self.active_widget = target.find_editable(&self.widgets);
                    }
                }

                self.status.update();
                let printer_lock = printer.lock().unwrap();
                self.status.print();
                print_files(&mut self.files);
                drop(printer_lock);
                iteration = 0;

                let widget_indices = widgets_to_update(&self.widgets);
                for index in &widget_indices {
                    let (widget, _) = &self.widgets[*index];
                    s_0.spawn(|| {
                        widget.update();
                        if !widget.resize_requested() {
                            let _printer_lock = printer.lock().unwrap();
                            widget.print();
                        } else {
                            *resize_requested.lock().unwrap() = true;
                        }
                    });
                }
            }
        });

        self.node_manager.shutdown();
    }
}

#[derive(Default)]
pub struct SessionControl {
    should_quit: bool,
    files_to_open: Option<Vec<PathBuf>>,
    target_widget: Option<TargetWidget>,
}

impl SessionControl {
    /// Switches the input to another `Widget`.
    pub fn switch_widget(&mut self, target_widget: &TargetWidget) {
        self.target_widget = Some(target_widget.clone());
    }

    /// Quits Parsec.
    pub fn quit(&mut self) {
        self.should_quit = true;
    }
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

/// Prints all the files.
fn print_files<U>(
    files: &mut Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>, Vec<(Widget<U>, usize)>)>,
) where
    U: Ui,
{
    for (file_widget, ..) in files.iter_mut() {
        let mut file_widget = file_widget.write();
        file_widget.update();
        file_widget.print();
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
    resize_requested: &Mutex<bool>, printer: &Mutex<bool>, widgets: &Vec<(Widget<U>, usize)>,
    files: &mut Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>, Vec<(Widget<U>, usize)>)>,
) where
    U: Ui,
{
    if let Ok(mut resize_requested) = resize_requested.try_lock() {
        if *resize_requested {
            *resize_requested = false;
            drop(resize_requested);
            widgets.iter().for_each(|(widget, _)| widget.try_set_size().unwrap());
            let _printer_lock = printer.lock().unwrap();
            widgets.iter().for_each(|(widget, _)| widget.print());
            print_files(files);
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
