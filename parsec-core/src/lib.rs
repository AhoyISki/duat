pub mod config;
pub mod history;
pub mod input;
pub mod position;
pub mod tags;
pub mod text;
pub mod ui;
pub mod widgets;

use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use config::{Config, RoData, RwData};
use crossterm::event::{self, Event, KeyEvent};
use input::{InputScheme, KeyRemapper};
use position::Pos;
use ui::{EndNode, ModNode, NodeIndex, Side, Split, Ui, Window};
use widgets::{
    command_line::{Command, CommandList},
    file_widget::FileWidget,
    ActionableWidget, Widget,
};

pub struct Session<U>
where
    U: Ui,
{
    windows: Vec<Window<U>>,
    active_window: usize,
    pub constructor_hook: Box<dyn Fn(ModNode<U>, RoData<FileWidget<U>>)>,
    session_manager: RwData<SessionManager>,
    global_commands: RwData<CommandList>,
}

impl<U> Session<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new(
        ui: U, config: Config, constructor_hook: Box<dyn Fn(ModNode<U>, RoData<FileWidget<U>>)>,
    ) -> Self {
        let file = std::env::args().nth(1);
        let file_widget = FileWidget::new(file.as_ref().map(|file| PathBuf::from(file)));
        let file_name = file_widget.identifier();

        let session_manager = RwData::new(SessionManager::new(&file_name, &file_name));

        let mut command_list = CommandList::default();
        for command in session_commands(session_manager.clone()) {
            command_list.try_add(Box::new(command)).unwrap();
        }

        let window =
            Window::new(ui, file_widget, config, &mut session_manager.write(), &constructor_hook);

        let mut session = Session {
            windows: vec![window],
            active_window: 0,
            constructor_hook,
            session_manager,
            global_commands: RwData::new(command_list),
        };

        session.open_arg_files();

        session
    }

    fn mut_active_window(&mut self) -> &mut Window<U> {
        self.windows.get_mut(self.active_window).unwrap()
    }

    fn active_window(&self) -> &Window<U> {
        self.windows.get(self.active_window).unwrap()
    }

    fn open_arg_files(&mut self) {
        for file in std::env::args().skip(2) {
            self.open_file(PathBuf::from(file))
        }
    }

    pub fn open_file(&mut self, path: PathBuf) {
        let file_widget = FileWidget::new(Some(path));
        let (side, split) = (Side::Right, Split::Min(40));
        self.windows[self.active_window].push_file(
            file_widget,
            side,
            split,
            false,
            &mut self.session_manager.write(),
            &self.constructor_hook,
        );
    }

    pub fn push_widget_to_edge<C>(
        &mut self, constructor: C, side: Side, split: Split,
    ) -> (NodeIndex, Option<NodeIndex>)
    where
        C: Fn(&Session<U>) -> Widget<U>,
    {
        let widget = (constructor)(self);
        self.mut_active_window().push_to_master(widget, side, split, false)
    }

    /// Start the application, initiating a read/response loop.
    pub fn start_parsec<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        self.mut_active_window().startup();

        // The main loop.
        loop {
            for (widget, end_node) in self.active_window().widgets() {
                let mut end_node = end_node.write();
                if widget.update(&mut end_node) {
                    widget.print(&mut end_node);
                }
            }

            self.session_loop(key_remapper);

            if self.session_manager.read().should_quit || true{
                break;
            }
        }

        self.mut_active_window().shutdown();
    }

    /// The primary application loop, executed while no breaking commands have been sent to
    /// `SessionControl`.
    fn session_loop<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme,
    {
        thread::scope(|s_0| {
            loop {
                self.active_window().print_if_layout_changed();

                let mut session_manager = self.session_manager.write();
                if session_manager.break_loop {
                    session_manager.break_loop = false;
                    break;
                }

                if let Ok(true) = event::poll(Duration::from_millis(5)) {
                    let active_window = &self.windows[self.active_window];
                    send_event(key_remapper, &mut session_manager, active_window);
                } else {
                    continue;
                }

                for (widget, end_node) in self.windows[self.active_window].widgets() {
                    s_0.spawn(|| {
                        let mut end_node = end_node.write();
                        if widget.update(&mut end_node) {
                            widget.print(&mut end_node);
                        }
                    });
                }
            }
        });
    }

    /// The list of commands that are considered global, as oposed to local to a file.
    fn global_commands(&self) -> RwData<CommandList> {
        self.global_commands.clone()
    }
}

pub struct SessionManager {
    files_to_open: Vec<PathBuf>,
    active_file: String,
    active_widget: String,
    break_loop: bool,
    should_quit: bool,
}

impl SessionManager {
    fn new(active_file: impl ToString, active_widget: impl ToString) -> Self {
        SessionManager {
            files_to_open: Vec::new(),
            active_file: active_file.to_string(),
            active_widget: active_widget.to_string(),
            break_loop: false,
            should_quit: false,
        }
    }
}

pub struct Controls<'a, U>
where
    U: Ui,
{
    session_manager: &'a mut SessionManager,
    window: &'a Window<U>,
}

impl<'a, U> Controls<'a, U>
where
    U: Ui + 'static,
{
    /// Quits Parsec.
    pub fn quit(&mut self) {
        self.session_manager.should_quit = true;
        self.session_manager.break_loop = true;
    }

    /// Switches to a `Widget<U>` with the given identifier.
    pub fn switch_to_widget(&mut self, target: impl AsRef<str>) -> Result<(), ()> {
        let (widget, _, end_node) = self
            .window
            .actionable_widgets()
            .find(|(widget, ..)| widget.read().identifier() == target.as_ref())
            .ok_or(())?;
        let mut widget = widget.write();
        widget.on_focus(&mut end_node.write());

        let (widget, _, end_node) = self
            .window
            .actionable_widgets()
            .find(|(widget, ..)| {
                widget.read().identifier() == self.session_manager.active_widget.as_str()
            })
            .ok_or(())?;

        let mut widget = widget.write();
        widget.on_unfocus(&mut end_node.write());

        if &target.as_ref()[..13] == "parsec-file: " {
            self.session_manager.active_file = target.as_ref().to_string();
        }

        self.session_manager.active_widget = target.as_ref().to_string();

        Ok(())
    }

    /// Switches to the next `Widget<U>` that contains a `FileWidget<U>`.
    pub fn next_file(&mut self) -> Result<(), ()> {
        if self.window.files().count() < 2 {
            Err(())
        } else {
            let widget = self
                .window
                .files()
                .cycle()
                .skip_while(|identifier| identifier.as_str() != self.session_manager.active_file)
                .nth(1)
                .ok_or(())?;

            self.switch_to_widget(widget)
        }
    }

    /// Switches to the previous `Widget<U>` that contains a `FileWidget<U>`.
    pub fn prev_file(&mut self) -> Result<(), ()> {
        if self.window.files().count() < 2 {
            Err(())
        } else {
            let widget = self
                .window
                .files()
                .rev()
                .cycle()
                .skip_while(|identifier| identifier.as_str() != self.session_manager.active_file)
                .nth(1)
                .ok_or(())?;

            self.switch_to_widget(widget)
        }
    }

    /// The identifier of the active file.
    pub fn active_file(&self) -> &str {
        self.session_manager.active_file.as_str()
    }

    /// The identifier of the active widget.
    pub fn active_widget(&self) -> &str {
        self.session_manager.active_widget.as_str()
    }

    pub fn return_to_file(&mut self) -> Result<(), ()> {
        self.switch_to_widget(self.session_manager.active_file.clone())
    }
}

fn session_commands(session: RwData<SessionManager>) -> Vec<Command<SessionManager>> {
    let quit_callers = vec![String::from("quit"), String::from("q")];
    let quit_command = Command::new(
        Box::new(|session: &mut SessionManager, _, _| {
            session.break_loop = true;
            session.should_quit = true;
            Ok(None)
        }),
        quit_callers,
        session.clone(),
    );

    let open_file_callers = vec![String::from("edit"), String::from("e")];
    let open_file_command = Command::new(
        Box::new(|session: &mut SessionManager, _, files| {
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
    key_remapper: &mut KeyRemapper<I>, session_manager: &mut SessionManager, window: &Window<U>,
) where
    U: Ui + 'static,
    I: InputScheme,
{
    if let Event::Key(key_event) = event::read().unwrap() {
        let actionable_widget = window
            .actionable_widgets()
            .find(|(_, identifier, _)| *identifier == session_manager.active_widget.as_str());

        let Some((widget, _, end_node)) = actionable_widget else {
            return;
        };

        let end_node = end_node.read();
        let mut widget = widget.write();

        let controls = Controls { session_manager: &mut *session_manager, window };
        blink_cursors_and_send_key(&mut *widget, &end_node, controls, key_event, key_remapper);
        // If the widget is no longer valid, return to the file.
        if !widget.still_valid() {
            session_manager.active_widget = session_manager.active_file.clone();
        }
    }
}

/// Removes the cursors, sends an event, and adds them again.
fn blink_cursors_and_send_key<U, W, I>(
    widget: &mut W, end_node: &EndNode<U>, controls: Controls<U>, key_event: KeyEvent,
    key_remapper: &mut KeyRemapper<I>,
) where
    U: Ui + 'static,
    W: ActionableWidget<U> + ?Sized,
    I: InputScheme,
{
    let (text, cursors, main_index) = widget.members_for_cursor_tags();
    text.remove_cursor_tags(cursors, main_index);

    key_remapper.send_key_to_actionable(key_event, &mut *widget, end_node, controls);

    let (text, cursors, main_index) = widget.members_for_cursor_tags();
    text.add_cursor_tags(cursors, main_index);
}

pub fn identifier_matches<U>(
    widget: &Arc<Mutex<dyn ActionableWidget<U>>>, identifier: impl AsRef<str>,
) -> bool
where
    U: Ui + 'static,
{
    widget.lock().unwrap().identifier() == identifier.as_ref()
}

/// Given a position (which is assumed to be on the line), will return the position at its start.
pub fn get_line_start(pos: Pos, line: &String) -> Pos {
    Pos { byte: pos.byte - line.char_indices().take(pos.col).count(), col: 0, row: pos.row }
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

//////////// Useful for testing.
#[doc(hidden)]
pub static mut FOR_TEST: usize = 0;

/// Internal macro used to log information.
#[macro_export]
#[doc(hidden)]
macro_rules! log_info {
    ($($text:tt)*) => {{
        use std::{fs, io::Write};
        let mut log = fs::OpenOptions::new().append(true).open("log").unwrap();
        log.write_fmt(format_args!($($text)*)).unwrap();
    }};
}
