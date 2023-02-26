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

use config::{Config, RwData};
use crossterm::event::{self, Event, KeyEvent};
use cursor::TextPos;
use input::{InputScheme, KeyRemapper};
use text::{PrintInfo, Text};
use ui::{ActionableWidgets, Area, EndNode, Label, ModNode, Side, Split, Ui, Window};
use widgets::{
    command_line::{Command, CommandList},
    file_widget::FileWidget,
    ActionableWidget, Widget,
};

pub struct Session<U>
where
    U: Ui,
{
    window: Window<U>,
    pub constructor_hook: Box<dyn Fn(ModNode<U>)>,
    control: RwData<SessionControl>,
    global_commands: RwData<CommandList>,
}

impl<U> Session<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new<C>(ui: U, config: Config, constructor_hook: Box<C>) -> Self
    where
        C: Fn(ModNode<U>) + 'static,
    {
        let file = std::env::args().nth(2);
        let file_widget = FileWidget::new(file.as_ref().map(|file| PathBuf::from(file)));
        let file_name = file_widget.identifier();

        let node_manager = Window::new(ui, file_widget, config, &constructor_hook);
        let (active_widget, _) = node_manager.actionable_widgets().next().unwrap();

        let control = SessionControl::new(file_name, active_widget.lock().unwrap().identifier());
        let control = RwData::new(control);

        let mut command_list = CommandList::default();
        for command in session_commands(control.clone()) {
            command_list.try_add(Box::new(command)).unwrap();
        }

        let session = Session {
            window: node_manager,
            constructor_hook,
            control,
            global_commands: RwData::new(command_list),
        };

        session
    }

    pub fn open_arg_files(&mut self) {
        for file in std::env::args().skip(1) {
            self.open_file(PathBuf::from(file))
        }
    }

    pub fn open_file(&mut self, path: PathBuf) {
        let file_widget = FileWidget::new(Some(path));
        let (side, split) = (Side::Right, Split::Minimum(40));
        self.window.push_to_files(file_widget, side, split, false, &self.constructor_hook);
    }

    pub fn push_widget_to_edge(&mut self, widget: Widget<U>, side: Side, split: Split) {
        let (master_node, end_node) = self.window.push_to_master(widget, side, split, false);
    }

    /// The full loop. A break in this loop means quitting Parsec.
    pub fn main_loop<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        self.window.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let resize_requested = Mutex::new(true);

        // The main loop.
        loop {
            // Initial printing.
            for (widget, end_node) in self.window.widgets() {
                let mut end_node = end_node.lock().unwrap();
                if widget.update(&mut end_node) {
                    widget.print(&mut end_node);
                }
            }

            self.session_loop(&resize_requested, key_remapper);

            if self.control.read().should_quit {
                break;
            }
        }

        self.window.shutdown();
    }

    /// The primary application loop, executed while no breaking commands have been sent to
    /// `SessionControl`.
    fn session_loop<I>(&mut self, resize_requested: &Mutex<bool>, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        thread::scope(|s_0| {
            loop {
                self.window.print_if_layout_changed();

                let mut control = self.control.write();
                control.try_switch_to_target(&self.window);
                if control.break_loop {
                    break;
                }
                drop(control);

                if let Ok(true) = event::poll(Duration::from_millis(5)) {
                    send_event(key_remapper, &mut self.control, &self.window);
                } else {
                    continue;
                }

                for (widget, end_node) in self.window.widgets() {
                    s_0.spawn(|| {
                        let mut end_node = end_node.lock().unwrap();
                        if widget.update(&mut end_node) {
                            widget.print(&mut end_node);
                        }
                    });
                }
            }
        });
    }

    /// The list of commands that are considered global, as oposed to local to a file.
    pub fn global_commands(&self) -> RwData<CommandList> {
        self.global_commands.clone()
    }
}

pub struct SessionControl {
    files_to_open: Vec<PathBuf>,
    target_widget: Option<String>,
    active_file: String,
    active_widget: String,
    break_loop: bool,
    should_quit: bool,
}

impl SessionControl {
    fn new(active_file: impl ToString, active_widget: impl ToString) -> Self {
        SessionControl {
            files_to_open: Vec::new(),
            target_widget: None,
            active_file: active_file.to_string(),
            active_widget: active_widget.to_string(),
            break_loop: false,
            should_quit: false,
        }
    }

    /// Returns to the active file from any widget, including the file itself.
    pub fn return_to_file(&mut self) {
        self.target_widget = Some(self.active_file.clone());
    }

    /// Switches the input to another `Widget`.
    pub fn switch_widget(&mut self, target_widget: String) {
        self.target_widget = Some(target_widget);
    }

    /// Quits Parsec.
    pub fn quit(&mut self) {
        self.break_loop = true;
        self.should_quit = true;
    }

    /// Switches the active widget to `self.target_widget`.
    fn try_switch_to_target<U>(&mut self, mut window: &Window<U>)
    where
        U: Ui,
    {
        let Some(identifier) = self.target_widget.take() else {
            return;
        };

        let (widget, _) = window
            .actionable_widgets()
            .find(|(widget, _)| widget.lock().unwrap().identifier() == self.active_widget.as_str())
            .unwrap();
        let mut widget = widget.lock().unwrap();
        widget.on_unfocus();

        let (widget, _) = window
            .actionable_widgets()
            .find(|(widget, _)| widget.lock().unwrap().identifier() == identifier.as_str())
            .unwrap();
        let mut widget = widget.lock().unwrap();
        widget.on_focus();

        if identifier.starts_with("parsec-file: ") {
            self.active_file = identifier;
        }
    }
}

pub fn session_commands(session: RwData<SessionControl>) -> Vec<Command<SessionControl>> {
    let quit_callers = vec![String::from("quit"), String::from("q")];
    let quit_command = Command::new(
        Box::new(|session: &mut SessionControl, _, _| {
            session.break_loop = true;
            session.should_quit = true;
            Ok(None)
        }),
        quit_callers,
        session.clone(),
    );

    let open_file_callers = vec![String::from("edit"), String::from("e")];
    let open_file_command = Command::new(
        Box::new(|session: &mut SessionControl, _, files| {
            session.files_to_open = files.into_iter().map(|file| PathBuf::from(file)).collect();
            Ok(None)
        }),
        open_file_callers,
        session.clone(),
    );

    vec![quit_command, open_file_command]
}

/// Sends an event to the `Widget` determined by `SessionControl`.
fn send_event<U, I>(
    key_remapper: &mut KeyRemapper<I>, control: &mut RwData<SessionControl>, window: &Window<U>,
) where
    U: Ui,
    I: InputScheme,
{
    if let Event::Key(key_event) = event::read().unwrap() {
        let mut control = control.write();
        let actionable_widget = window.actionable_widgets().find(|(widget, _)| {
            if let Ok(widget) = widget.try_lock() {
                widget.identifier() == control.active_widget
            } else {
                false
            }
        });

        let Some((widget, end_node)) = actionable_widget else {
            return;
        };

        let mut widget = widget.lock().unwrap();
        let end_node = end_node.lock().unwrap();
        blink_cursors_and_send_key(&mut *widget, &end_node, &mut control, key_event, key_remapper);
        // If the widget is no longer valid, return to the file.
        if !widget.still_valid() {
            control.target_widget = Some(control.active_file.clone());
        }
    }
}

/// Removes the cursors, sends an event, and adds them again.
fn blink_cursors_and_send_key<U, W, I>(
    widget: &mut W, end_node: &EndNode<U>, control: &mut SessionControl, key_event: KeyEvent,
    key_remapper: &mut KeyRemapper<I>,
) where
    U: Ui,
    W: ActionableWidget<U> + ?Sized,
    I: InputScheme,
{
    let (text, cursors, main_index) = widget.members_for_cursor_tags();
    text.remove_cursor_tags(cursors, main_index);

    key_remapper.send_key_to_actionable(key_event, &mut *widget, end_node, control);

    let (text, cursors, main_index) = widget.members_for_cursor_tags();
    text.add_cursor_tags(cursors, main_index);
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
pub fn max_line<U>(text: &Text<U>, print_info: &PrintInfo, node: &EndNode<U>) -> usize
where
    U: Ui,
{
    min(print_info.top_row + node.label.area().height(), text.lines().len() - 1)
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
