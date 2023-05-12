#![feature(drain_filter, result_option_inspect, trait_upcasting)]

pub mod commands;
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
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc
    },
    thread,
    time::Duration
};

use commands::{Command, CommandErr, Commands};
use crossterm::event::{self, Event, KeyEvent};
use data::{RoData, RoNestedData, RwData};
use input::{InputScheme, KeyRemapper};
use tags::form::FormPalette;
use text::PrintCfg;
use ui::{activate_hook, ModNode, ParsecWindow, PushSpecs, RoWindows, Side, Split, Ui};
use widgets::{file_widget::FileWidget, ActionableWidget, Widget};

pub struct Session<U>
where
    U: Ui
{
    ui: U,
    pub constructor_hook: Box<dyn FnMut(ModNode<U>, RoData<FileWidget<U>>)>,
    manager: Manager<U>,
    print_cfg: RwData<PrintCfg>
}

impl<U> Session<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new(
        mut ui: U, print_cfg: PrintCfg, palette: FormPalette,
        mut constructor_hook: impl FnMut(ModNode<U>, RoData<FileWidget<U>>) + 'static
    ) -> Self {
        let file = std::env::args().nth(1).as_ref().map(|file| PathBuf::from(file));
        let file = FileWidget::<U>::new(file, print_cfg.clone());

        let window = ParsecWindow::new(&mut ui, file, Some(0));
        let mut manager = Manager::new(window, 0, 0, palette);
        manager.commands.write().context = Some(0);
        activate_hook(&mut manager, 0, &mut constructor_hook);
        manager.commands.write().context = None;

        let mut session = Session {
            ui,
            constructor_hook: Box::new(constructor_hook),
            manager,
            print_cfg: RwData::new(print_cfg)
        };

        session.open_arg_files();

        session
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
        let context = self.manager.windows.inspect(|windows| {
            Some(windows.iter().map(|window| window.file_names().count()).sum::<usize>())
        });

        let (new_area, _) = self.manager.windows.mutate(|windows| {
            let active_window = &mut windows[self.manager.active_window];
            active_window.push_file(file_widget, push_specs, context)
        });

        let context = self.manager.commands.mutate(|commands| {
            let old_context = commands.context;
            commands.context = context;
            old_context
        });

        activate_hook(&mut self.manager, new_area, &mut self.constructor_hook);
        self.manager.commands.write().context = context;
    }

    pub fn push_widget_to_edge(
        &mut self, constructor: impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U>,
        push_specs: PushSpecs
    ) -> (usize, Option<usize>) {
        let widget = (constructor)(&self.manager, push_specs);
        self.manager.windows.write()[self.manager.active_window].push_to_master(widget, push_specs)
    }

    /// Start the application, initiating a read/response loop.
    pub fn start_parsec<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: InputScheme
    {
        self.ui.startup();

        // The main loop.
        loop {
            let palette = &self.manager.palette;
            for (widget, mut label, _) in
                self.manager.windows.read()[self.manager.active_window].widgets()
            {
                widget.update(&mut label);
                widget.print(&mut label, &palette);
            }

            self.session_loop(key_remapper);

            let mut files = std::mem::take(&mut *self.manager.files_to_open.write());
            for file in files.drain(..) {
                self.open_file(file);
            }

            if self.manager.should_quit.load(Ordering::Acquire) {
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
        let palette = &self.manager.palette;
        let manager = &self.manager;
        let windows = self.manager.windows.read();
        thread::scope(|scope| {
            loop {
                let active_window = &windows[self.manager.active_window];
                active_window.print_if_layout_changed(palette);

                if manager.break_loop.load(Ordering::Relaxed) {
                    manager.break_loop.store(false, Ordering::Relaxed);
                    break;
                }

                for (widget, mut label, _) in active_window.widgets() {
                    if widget.needs_update() {
                        if widget.is_slow() {
                            let palette = &palette;
                            scope.spawn(move || {
                                widget.update(&mut label);
                                widget.print(&mut label, palette);
                            });
                        } else {
                            widget.update(&mut label);
                            widget.print(&mut label, palette);
                        }
                    }
                }

                if let Ok(true) = event::poll(Duration::from_millis(10)) {
                    send_event(key_remapper, &manager, &palette);
                } else {
                    continue;
                }
            }
        });
    }

    /// Returns the [`RwData<Manager>`].
    pub fn manager(&self) -> &Manager<U> {
        &self.manager
    }
}

/// A general manager for Parsec, that can be called upon by certain
/// structs
pub struct Manager<U>
where
    U: Ui
{
    windows: RwData<Vec<ParsecWindow<U>>>,
    active_window: usize,
    commands: RwData<Commands>,
    files_to_open: RwData<Vec<PathBuf>>,
    anchor_file: Arc<AtomicUsize>,
    active_file: RwData<RoData<FileWidget<U>>>,
    active_widget: Arc<AtomicUsize>,
    break_loop: Arc<AtomicBool>,
    should_quit: Arc<AtomicBool>,
    pub palette: FormPalette
}

impl<U> Manager<U>
where
    U: Ui
{
    /// Returns a new instance of [`Manager`].
    fn new(
        window: ParsecWindow<U>, anchor_file: usize, active_widget: usize, palette: FormPalette
    ) -> Self {
        let active_file = window
            .actionable_widgets()
            .find_map(|(widget, ..)| widget.clone().try_downcast::<FileWidget<U>>().ok())
            .unwrap();

        let manager = Manager {
            windows: RwData::new(vec![window]),
            active_window: 0,
            commands: Commands::new_rw_data(),
            files_to_open: RwData::new(Vec::new()),
            anchor_file: Arc::new(AtomicUsize::new(anchor_file)),
            active_file: RwData::new(RoData::from(&active_file)),
            active_widget: Arc::new(AtomicUsize::new(active_widget)),
            break_loop: Arc::new(AtomicBool::from(false)),
            should_quit: Arc::new(AtomicBool::from(false)),
            palette
        };

        let break_loop = manager.break_loop.clone();
        let should_quit = manager.should_quit.clone();
        let quit = Command::new(vec!["quit", "q"], move |_, _| {
            break_loop.store(true, Ordering::Release);
            should_quit.store(true, Ordering::Release);
            Ok(None)
        });

        let break_loop = manager.break_loop.clone();
        let files_to_open = manager.files_to_open.clone();
        let open_files = Command::new(vec!["edit", "e"], move |_, files| {
            break_loop.store(true, Ordering::Release);
            *files_to_open.write() = files.map(|file| PathBuf::from(file)).collect();
            Ok(None)
        });

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

    pub fn windows(&self) -> RoWindows<U> {
        RoWindows::new(RoData::from(&self.windows))
    }

    pub fn active_file(&self) -> RoNestedData<FileWidget<U>> {
        RoNestedData::from(&self.active_file)
    }
}

unsafe impl<U> Send for Manager<U> where U: Ui {}
unsafe impl<U> Sync for Manager<U> where U: Ui {}

// TODO: Local and global widgets.
pub struct Controls<'a, U>
where
    U: Ui
{
    manager: &'a Manager<U>,
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

        self.manager.anchor_file.store(file_index, Ordering::Release);

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
                .skip(self.manager.anchor_file.load(Ordering::Acquire) + 1)
                .next()
                .ok_or(())?;

            self.manager.anchor_file.store(file_index, Ordering::Release);

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
                .take(self.manager.anchor_file.load(Ordering::Acquire).wrapping_sub(1))
                .last()
                .ok_or(())?;

            self.manager.anchor_file.store(file_index, Ordering::Release);

            self.switch_to_widget_index(widget_index)
        }
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to_widget<Aw>(&mut self) -> Result<(), ()>
    where
        Aw: ActionableWidget<U>
    {
        let cur_context = self.manager.commands.read().context;
        let index = self
            .window
            .actionable_widgets()
            .position(|(widget, _, context)| {
                widget.data_is::<Aw>() && (context.is_none() || context == cur_context)
            })
            .ok_or(())?;

        self.switch_to_widget_index(index)
    }

    fn switch_to_widget_index(&mut self, index: usize) -> Result<(), ()> {
        let (widget, label, context) = self.window.actionable_widgets().nth(index).ok_or(())?;
        widget.write().on_focus(&label);
        if let Some(file) = widget.clone().try_downcast::<FileWidget<U>>().ok() {
            *self.manager.active_file.write() = RoData::from(&file);
        }

        self.manager.commands.write().context = context;

        let active_index = self.manager.active_widget.load(Ordering::Acquire);
		log_info!("\nactive: {active_index}, switch: {index}");
        let (widget, label, _) = self.window.actionable_widgets().nth(active_index).ok_or(())?;
        widget.write().on_unfocus(&label);

        self.manager.active_widget.store(index, Ordering::Release);

        Ok(())
    }

    /// The name of the active [`FileWidget<U>`].
    pub fn active_file(&self) -> String {
        let index = self.manager.anchor_file.load(Ordering::Acquire);
        self.window.file_names().nth(index).unwrap().1
    }

    pub fn return_to_file(&mut self) -> Result<(), ()> {
        let index = self.manager.anchor_file.load(Ordering::Acquire);
        let (widget_index, _) = self.window.file_names().nth(index).ok_or(())?;

        self.switch_to_widget_index(widget_index)
    }

    pub fn run_cmd(&mut self, cmd: impl ToString) -> Result<Option<String>, CommandErr> {
        self.manager.commands.read().try_exec(cmd.to_string())
    }
}

/// Sends an event to the `Widget` determined by `SessionControl`.
fn send_event<U, I>(key_remapper: &mut KeyRemapper<I>, manager: &Manager<U>, palette: &FormPalette)
where
    U: Ui + 'static,
    I: InputScheme
{
    if let Event::Key(key_event) = event::read().unwrap() {
        let windows = manager.windows.read();
        let window = &windows[manager.active_window];

        let index = manager.active_widget.load(Ordering::Acquire);
        let actionable_widget = window.actionable_widgets().nth(index);

        let Some((widget, mut label, _)) = actionable_widget else {
            return;
        };

        let controls = Controls {
            manager: &*manager,
            window
        };

        blink_cursors_and_send_key(&widget, &mut label, controls, key_event, key_remapper, palette);
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
