#![feature(drain_filter, result_option_inspect)]

pub mod data;
pub mod history;
pub mod input;
pub mod position;
pub mod tags;
pub mod text;
pub mod ui;
pub mod widgets;

use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc
    },
    thread,
    time::Duration
};

use crossterm::event::{self, Event, KeyEvent};
use data::{RoData, RwData};
use input::{InputScheme, KeyRemapper};
use tags::form::FormPalette;
use text::PrintCfg;
use ui::{ModNode, ParsecWindow, PushSpecs, Side, Split, Ui};
use widgets::{
    command_line::{Command, CommandError, Commands},
    file_widget::FileWidget,
    ActionableWidget, Widget
};

pub struct Session<U>
where
    U: Ui
{
    ui: U,
    windows: Vec<ParsecWindow<U>>,
    active_window: usize,
    pub constructor_hook: Box<dyn FnMut(ModNode<U>, RoData<FileWidget<U>>)>,
    manager: RwData<Manager>,
    print_cfg: RwData<PrintCfg>
}

impl<U> Session<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new(
        mut ui: U, print_cfg: PrintCfg, palette: FormPalette,
        mut constructor_hook: Box<dyn FnMut(ModNode<U>, RoData<FileWidget<U>>)>
    ) -> Self {
        let file = std::env::args().nth(1);
        let file_widget =
            FileWidget::<U>::new(file.as_ref().map(|file| PathBuf::from(file)), print_cfg.clone());

        let manager = RwData::new(Manager::new(0, 0, palette));

        let window =
            ParsecWindow::new(&mut ui, file_widget, &mut manager.write(), &mut constructor_hook);

        let mut session = Session {
            ui,
            windows: vec![window],
            active_window: 0,
            constructor_hook,
            manager,
            print_cfg: RwData::new(print_cfg)
        };

        session.open_arg_files();

        session
    }

    fn mut_active_window(&mut self) -> &mut ParsecWindow<U> {
        self.windows.get_mut(self.active_window).unwrap()
    }

    fn active_window(&self) -> &ParsecWindow<U> {
        self.windows.get(self.active_window).unwrap()
    }

    fn open_arg_files(&mut self) {
        for file in std::env::args().skip(2) {
            self.open_file(PathBuf::from(file))
        }
    }

    pub fn open_file(&mut self, path: PathBuf) {
        let file_widget = FileWidget::new(Some(path), self.print_cfg.read().clone());
        let push_specs = PushSpecs {
            side: Side::Right,
            split: Split::Min(40)
        };
        self.windows[self.active_window].push_file(
            file_widget,
            push_specs,
            &mut self.constructor_hook,
            &mut self.manager.write()
        );
    }

    pub fn push_widget_to_edge<C>(
        &mut self, constructor: C, push_specs: PushSpecs
    ) -> (usize, Option<usize>)
    where
        C: Fn(&Session<U>) -> Widget<U>
    {
        let widget = (constructor)(self);
        self.mut_active_window().push_to_master(widget, push_specs)
    }

    /// Start the application, initiating a read/response loop.
    pub fn start_parsec<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme
    {
        self.ui.startup();

        // The main loop.
        loop {
            let session_manager = self.manager.read();
            let palette = &session_manager.palette;
            for (widget, mut label) in self.active_window().widgets() {
                widget.update(&mut label);
                widget.print(&mut label, &palette);
            }
            drop(palette);
            drop(session_manager);

            self.session_loop(key_remapper);

            if self.manager.read().should_quit.load(Ordering::Acquire) || true {
                break;
            }
        }

        self.ui.shutdown();
    }

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme
    {
        let palette = self.manager.read().palette.clone();
        thread::scope(|scope| {
            loop {
                self.active_window().print_if_layout_changed(&palette);

                let mut session_manager = self.manager.write();
                if session_manager.break_loop.load(Ordering::Acquire) {
                    session_manager.break_loop.store(false, Ordering::Release);
                    break;
                }

                for (widget, mut label) in self.windows[self.active_window].widgets() {
                    if widget.needs_update() {
                        if widget.is_slow() {
                            let palette = &palette;
                            scope.spawn(move || {
                                widget.update(&mut label);
                                widget.print(&mut label, palette);
                            });
                        } else {
                            widget.update(&mut label);
                            widget.print(&mut label, &palette);
                        }
                    }
                }

                if let Ok(true) = event::poll(Duration::from_millis(10)) {
                    let active_window = &self.windows[self.active_window];
                    send_event(key_remapper, &mut session_manager, active_window, &palette);
                } else {
                    continue;
                }
            }
        });
    }
}

/// A general manager for Parsec, that can be called upon by certain
/// structs
pub struct Manager {
    commands: RwData<Commands>,
    files_to_open: RwData<Vec<PathBuf>>,
    anchor_file: usize,
    active_widget: usize,
    break_loop: Arc<AtomicBool>,
    should_quit: Arc<AtomicBool>,
    pub palette: FormPalette
}

impl Manager {
    fn new(anchor_file: usize, active_widget: usize, palette: FormPalette) -> Self {
        let manager = Manager {
            commands: RwData::new(Commands::default()),
            files_to_open: RwData::new(Vec::new()),
            anchor_file,
            active_widget,
            break_loop: Arc::new(AtomicBool::from(false)),
            should_quit: Arc::new(AtomicBool::from(false)),
            palette
        };

        let break_loop = manager.break_loop.clone();
        let should_quit = manager.should_quit.clone();
        let quit = Command::new(
            move |_, _| {
                break_loop.swap(true, Ordering::Release);
                should_quit.swap(true, Ordering::Release);
                Ok(None)
            },
            vec![String::from("quit"), String::from("q")]
        );

        let break_loop = manager.break_loop.clone();
        let files_to_open = manager.files_to_open.clone();
        let open_files = Command::new(
            move |_, files| {
                break_loop.swap(true, Ordering::Release);
                *files_to_open.write() = files.iter().map(|file| PathBuf::from(file)).collect();
                Ok(None)
            },
            vec![String::from("edit"), String::from("e")]
        );

        manager.commands.mutate(|commands| {
            commands.try_add(quit).unwrap();
            commands.try_add(open_files).unwrap();
        });

        manager
    }

    /// A thread safe, read-write [`Commands`], meant to be used
    /// globaly.
    pub fn commands(&self) -> RwData<Commands> {
        self.commands.clone()
    }
}

unsafe impl Send for Manager {}
unsafe impl Sync for Manager {}

// TODO: Local and global widgets.
pub struct Controls<'a, U>
where
    U: Ui
{
    manager: &'a mut Manager,
    window: &'a ParsecWindow<U>
}

impl<'a, U> Controls<'a, U>
where
    U: Ui + 'static
{
    /// Quits Parsec.
    pub fn quit(&mut self) {
        self.manager.should_quit.swap(true, Ordering::Release);
        self.manager.break_loop.swap(true, Ordering::Release);
    }

    /// Switches to the [`FileWidget<U>`] with the given name.
    pub fn switch_to_file(&mut self, target: impl AsRef<str>) -> Result<(), ()> {
        let target = target.as_ref();
        let (file_index, (widget_index, _)) = self
            .window
            .file_names()
            .enumerate()
            .find(|(_, (_, name))| name == target)
            .ok_or(())?;

        self.manager.anchor_file = file_index;

        self.switch_to_widget_index(widget_index)
    }

    /// Switches to the next [`FileWidget<U>`].
    pub fn next_file(&mut self) -> Result<(), ()> {
        if self.window.file_names().count() < 2 {
            Err(())
        } else {
            let (file_index, (widget_index, _)) = self
                .window
                .file_names()
                .enumerate()
                .cycle()
                .skip(self.manager.anchor_file + 1)
                .next()
                .ok_or(())?;

            self.manager.anchor_file = file_index;

            self.switch_to_widget_index(widget_index)
        }
    }

    /// Switches to the previous [`FileWidget<U>`].
    pub fn prev_file(&mut self) -> Result<(), ()> {
        if self.window.file_names().count() < 2 {
            Err(())
        } else {
            let (file_index, (widget_index, _)) = self
                .window
                .file_names()
                .enumerate()
                .take(self.manager.anchor_file.wrapping_sub(1))
                .last()
                .ok_or(())?;

            self.manager.anchor_file = file_index;

            self.switch_to_widget_index(widget_index)
        }
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to_widget<Aw>(&mut self) -> Result<(), ()>
    where
        Aw: ActionableWidget<U>
    {
        let index = self
            .window
            .actionable_widgets()
            .position(|(widget, _)| widget.data_is::<Aw>())
            .ok_or(())?;

        self.switch_to_widget_index(index)
    }

    fn switch_to_widget_index(&mut self, index: usize) -> Result<(), ()> {
        let (widget, label) = self.window.actionable_widgets().nth(index).ok_or(())?;

        let mut widget = widget.write();
        widget.on_focus(&label);
        drop(widget);

        let active_index = self.manager.active_widget;
        let (widget, label) = self.window.actionable_widgets().nth(active_index).ok_or(())?;

        let mut widget = widget.write();
        widget.on_unfocus(&label);

        self.manager.active_widget = index;

        Ok(())
    }

    /// The name of the active [`FileWidget<U>`].
    pub fn active_file(&self) -> String {
        self.window.file_names().nth(self.manager.anchor_file).unwrap().1
    }

    pub fn return_to_file(&mut self) -> Result<(), ()> {
        let (widget_index, _) = self.window.file_names().nth(self.manager.anchor_file).ok_or(())?;

        self.switch_to_widget_index(widget_index)
    }

    pub fn run_cmd(&mut self, cmd: impl ToString) -> Result<Option<String>, CommandError> {
        let cmd = cmd.to_string();
        let mut commands = self.manager.commands.write();
        commands.try_parse(cmd)
    }
}

/// Sends an event to the `Widget` determined by `SessionControl`.
fn send_event<U, I>(
    key_remapper: &mut KeyRemapper<I>, session_manager: &mut Manager, window: &ParsecWindow<U>,
    palette: &FormPalette
) where
    U: Ui + 'static,
    I: InputScheme
{
    if let Event::Key(key_event) = event::read().unwrap() {
        let actionable_widget = window.actionable_widgets().nth(session_manager.active_widget);

        let Some((widget, mut label)) = actionable_widget else {
            return;
        };

        let controls = Controls {
            manager: &mut *session_manager,
            window
        };

        blink_cursors_and_send_key(&widget, &mut label, controls, key_event, key_remapper, palette);

        // If the widget is no longer valid, return to the file.
        if !widget.read().still_valid() {
            session_manager.active_widget = session_manager.anchor_file.clone();
        }
    }
}

/// Removes the cursors, sends an event, and adds them again.
fn blink_cursors_and_send_key<U, AW, I>(
    widget: &RwData<AW>, label: &mut U::Label, controls: Controls<U>, key_event: KeyEvent,
    key_remapper: &mut KeyRemapper<I>, palette: &FormPalette
) where
    U: Ui + 'static,
    AW: ActionableWidget<U> + ?Sized + 'static,
    I: InputScheme
{
    let mut widget_lock = widget.write();
    let (text, cursors, _) = widget_lock.members_for_cursor_tags();
    text.remove_cursor_tags(cursors);
    drop(widget_lock);

    key_remapper.send_key_to_actionable(key_event, widget, label, controls);

    let mut widget_lock = widget.write();
    let (text, cursors, main_index) = widget_lock.members_for_cursor_tags();
    text.add_cursor_tags(cursors, main_index);
    drop((text, cursors, main_index));

    widget_lock.update(label);
    widget_lock
        .text()
        .print(label, widget_lock.print_info(), widget_lock.print_cfg(), palette);
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
