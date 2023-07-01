#![feature(drain_filter, result_option_inspect, trait_upcasting, let_chains)]

use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc
    }
};

use commands::{Command, CommandErr, Commands};
use data::{RoData, RoNestedData, RwData};
use tags::form::FormPalette;
use ui::{Area, ParsecWindow, RoWindows, Ui};
use widgets::{ActionableWidget, FileWidget};

pub mod commands;
pub mod data;
pub mod history;
pub mod input;
pub mod position;
pub mod session;
pub mod tags;
pub mod text;
pub mod ui;
pub mod widgets;

static BREAK_LOOP: AtomicBool = AtomicBool::new(false);
static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

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
    active_file: RwData<RoData<FileWidget<U>>>,
    active_widget: Arc<AtomicUsize>,
    pub palette: FormPalette
}

impl<U> Manager<U>
where
    U: Ui
{
    /// Returns a new instance of [`Manager`].
    fn new(window: ParsecWindow<U>, active_widget: usize, palette: FormPalette) -> Self {
        let active_file = window
            .actionable_widgets()
            .find_map(|(widget, ..)| widget.clone().try_downcast::<FileWidget<U>>().ok())
            .unwrap();

        let manager = Manager {
            windows: RwData::new(vec![window]),
            active_window: 0,
            commands: Commands::new_rw_data(),
            files_to_open: RwData::new(Vec::new()),
            active_file: RwData::new(RoData::from(&active_file)),
            active_widget: Arc::new(AtomicUsize::new(active_widget)),
            palette
        };

        let quit = Command::new(vec!["quit", "q"], move |_, _| {
            BREAK_LOOP.store(true, Ordering::Release);
            SHOULD_QUIT.store(true, Ordering::Release);
            Ok(None)
        });

        let files_to_open = manager.files_to_open.clone();
        let open_files = Command::new(vec!["edit", "e"], move |_, files| {
            BREAK_LOOP.store(true, Ordering::Release);
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

    pub fn dynamic_active_file(&self) -> RoNestedData<FileWidget<U>> {
        RoNestedData::from(&self.active_file)
    }

    pub fn active_file(&self) -> RoData<FileWidget<U>> {
        self.active_file.read().clone()
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
        BREAK_LOOP.store(true, Ordering::Release);
        SHOULD_QUIT.store(true, Ordering::Release);
    }

    /// Switches to the [`FileWidget<U>`] with the given name.
    pub fn switch_to_file(&mut self, target: impl AsRef<str>) -> Result<(), ()> {
        let target = target.as_ref();
        let (widget_index, _) =
            self.window.file_names().find(|(_, name)| name == target).ok_or(())?;

        self.switch_to_widget_index(widget_index)
    }

    /// Switches to the next [`FileWidget<U>`].
    pub fn next_file(&mut self) -> Result<(), ()> {
        if self.window.file_names().count() < 2 {
            Err(())
        } else {
            let cur_name = self.manager.active_file.inspect(|file| file.read().name());
            let (widget_index, _) = self
                .window
                .file_names()
                .cycle()
                .skip_while(|(_, name)| *name != cur_name)
                .nth(1)
                .ok_or(())?;

            self.switch_to_widget_index(widget_index)
        }
    }

    /// Switches to the previous [`FileWidget<U>`].
    pub fn prev_file(&mut self) -> Result<(), ()> {
        if self.window.file_names().count() < 2 {
            Err(())
        } else {
            let cur_name = self.manager.active_file.inspect(|file| file.read().name());
            let (widget_index, _) = self
                .window
                .file_names()
                .take_while(|(_, name)| *name != cur_name)
                .last()
                .ok_or(())?;

            self.switch_to_widget_index(widget_index)
        }
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to_widget<Aw>(&mut self) -> Result<(), ()>
    where
        Aw: ActionableWidget<U>
    {
        let cur_file_id = self.manager.commands.read().file_id;
        let index = self
            .window
            .actionable_widgets()
            .position(|(widget, _, file_id)| {
                widget.data_is::<Aw>() && (file_id.is_none() || cur_file_id == file_id)
            })
            .ok_or(())?;

        self.switch_to_widget_index(index)
    }

    fn switch_to_widget_index(&mut self, index: usize) -> Result<(), ()> {
        let (widget, mut area, file_id) = self.window.actionable_widgets().nth(index).ok_or(())?;
        area.set_as_active();
        widget.write().on_focus(&area);
        if let Some(file) = widget.clone().try_downcast::<FileWidget<U>>().ok() {
            *self.manager.active_file.write() = RoData::from(&file);
        }

        self.manager.commands.write().file_id = file_id;

        let active_index = self.manager.active_widget.load(Ordering::Acquire);
        let (widget, area, _) = self.window.actionable_widgets().nth(active_index).ok_or(())?;
        widget.write().on_unfocus(&area);

        self.manager.active_widget.store(index, Ordering::Release);

        Ok(())
    }

    /// The name of the active [`FileWidget<U>`].
    pub fn active_file_name(&self) -> String {
        self.manager.active_file.inspect(|file| file.read().name())
    }

    pub fn return_to_file(&mut self) -> Result<(), ()> {
        let cur_name = self.manager.active_file.inspect(|file| file.read().name());
        let (widget_index, _) =
            self.window.file_names().find(|(_, name)| *name == cur_name).ok_or(())?;

        self.switch_to_widget_index(widget_index)
    }

    pub fn run_cmd(&mut self, cmd: impl ToString) -> Result<Option<String>, CommandErr> {
        self.manager.commands.read().try_exec(cmd.to_string())
    }
}

/// A convenience macro to join any number of variables that can
/// be turned into `String`s.
///
/// # Examples
///
/// ```
/// # use parsec_core::join;
/// let my_text = join!["number: ", 21, ", floating: ", 3.14];
/// assert!(my_text == String::from("number: 21, floating: 3.14"));
/// ```
#[macro_export]
macro_rules! join {
    () => { String::from("") };

    ($($var:expr),+ $(,)?) => {
        [$($var.to_string()),+].join("")
    }
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
