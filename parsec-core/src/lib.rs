#![feature(drain_filter, result_option_inspect, trait_upcasting, let_chains)]

use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        RwLock
    }
};

use commands::{Command, CommandErr, Commands};
use data::{ReadableData, RoData, RoNestedData, RwData};
use tags::form::FormPalette;
use ui::{Area, Node, ParsecWindow, RoWindows, Ui};
use widgets::{FileWidget, SchemeWidget, Widget};

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
pub struct Controler<U>
where
    U: Ui
{
    windows: RwData<Vec<ParsecWindow<U>>>,
    active_window: usize,
    commands: RwData<Commands>,
    files_to_open: RwData<Vec<PathBuf>>,
    active_file: RwData<RoData<FileWidget<U>>>,
    active_widget: RwLock<RwData<dyn Widget<U>>>,
    pub palette: FormPalette
}

impl<U> Controler<U>
where
    U: Ui
{
    /// Returns a new instance of [`Controler`].
    fn new(window: ParsecWindow<U>, palette: FormPalette) -> Self {
        // NOTE: For now, we're picking the first file as active.
        let (widget, ..) =
            window.widgets().find(|(widget, ..)| widget.data_is::<FileWidget<U>>()).unwrap();

        let file = widget.clone().try_downcast::<FileWidget<U>>().unwrap();

        let manager = Self {
            windows: RwData::new(vec![window]),
            active_window: 0,
            commands: Commands::new_rw_data(),
            files_to_open: RwData::new(Vec::new()),
            active_file: RwData::new(RoData::from(&file)),
            active_widget: RwLock::new(widget.clone()),
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

    /// Changes `self.active_widget`, given the target
    /// [`ActionableWidget<U>`], its [`U::Area`][Ui::Area], and a
    /// `file_id`.
    fn inner_change_to(
        &self, widget: RwData<dyn Widget<U>>, area: &U::Area, file_id: Option<usize>
    ) -> Result<(), ()> {
        area.set_as_active();

        if let Some(file) = widget.clone().try_downcast::<FileWidget<U>>().ok() {
            *self.active_file.write() = RoData::from(&file);
        }

        let (old_widget, old_area, _) = self
            .inspect_active_window(|window| {
                window
                    .widgets()
                    .find(|(widget, ..)| widget.ptr_eq(&*self.active_widget.read().unwrap()))
                    .map(|(widget, area, file_id)| (widget.clone(), area, file_id))
            })
            .ok_or(())?;

        // Order matters here, since `on_unfocus` could rely on the
        // `Commands`'s prior `file_id`.
        old_widget.write().input_taker().unwrap().on_unfocus(&old_area);
        self.commands.write().file_id = file_id;
        widget.write().input_taker().unwrap().on_focus(&area);

        *self.active_widget.write().unwrap() = widget;

        Ok(())
    }

    /// Inspects the currently active window.
    fn inspect_active_window<T>(&self, f: impl FnOnce(&ParsecWindow<U>) -> T) -> T {
        self.windows.inspect(|windows| f(&windows[self.active_window]))
    }
}

/// # Querying Functions
///
/// These functions do not trigger any internal mutability within the
/// [`Controler`], and only serve to return information to the caller.
impl<U> Controler<U>
where
    U: Ui
{
    /// The name of the active [`FileWidget<U>`].
    ///
    /// Note that, even while a non [`FileWidget<U>`] is active, this
    /// will return the name of the last [`FileWidget<U>`] to be
    /// active.
    pub fn active_file_name(&self) -> String {
        self.active_file.inspect(|file| file.read().name())
    }

    /// A thread safe, read-write [`Commands`], meant to be used
    /// globaly.
    pub fn commands(&self) -> RwData<Commands> {
        self.commands.clone()
    }

    /// A read only list of [`ParsecWindow<U>`]s.
    pub fn windows(&self) -> RoWindows<U> {
        RoWindows::new(RoData::from(&self.windows))
    }

    /// Returns a special reference that always points to the active
    /// [`FileWidget<U>`] of the [`Controler`].
    ///
    /// One place where this function shines is in the
    /// [`StatusLine<U>`], where the [`parts_global_fn`] and
    /// [`default_global_fn`] methods generate a status line
    /// with information that is always pertaining to the active
    /// file, allowing the end user to have only a single status
    /// line for everything.
    ///
    /// If you wish for a reference only to the file that is active
    /// only at that specific moment, see [`active_file`]
    ///
    /// [`StatusLine<U>`]: crate::widgets::StatusLine
    /// [`parts_global_fn`]: crate::widgets::StatusLine::parts_global_fn
    /// [`default_global_fn`]: crate::widgets::StatusLine::default_global_fn
    /// [`active_file`]: Self::active_file
    pub fn dynamic_active_file(&self) -> RoNestedData<FileWidget<U>> {
        RoNestedData::from(&self.active_file)
    }

    /// A read only reference to the currently active
    /// [`FileWidget<U>`].
    ///
    /// If you wish for a reference that updates itself, such that it
    /// always points to the active file of the [`Controler`], see
    /// [`dynamic_active_file`]
    ///
    /// [`dynamic_active_file`]: Self::dynamic_active_file
    pub fn active_file(&self) -> RoData<FileWidget<U>> {
        self.active_file.read().clone()
    }
}

/// # Modification Functions
///
/// These functions are used to alter the state of Parsec through the
/// internal mutability pattern.
impl<U> Controler<U>
where
    U: Ui
{
    pub fn return_to_file(&self) -> Result<(), ()> {
        let cur_name = self.active_file.inspect(|file| file.read().name());
        self.switch_to_file(cur_name)
    }

    pub fn run_cmd(&mut self, cmd: impl ToString) -> Result<Option<String>, CommandErr> {
        self.commands.read().try_exec(cmd.to_string())
    }

    /// Quits Parsec.
    pub fn quit(&self) {
        BREAK_LOOP.store(true, Ordering::Release);
        SHOULD_QUIT.store(true, Ordering::Release);
    }

    /// Switches to the [`FileWidget<U>`] with the given name.
    pub fn switch_to_file(&self, target: impl AsRef<str>) -> Result<(), ()> {
        let name = target.as_ref();
        let (widget, area, file_id) = self.inspect_active_window(|window| {
            window
                .widgets()
                .find(|(widget, ..)| {
                    widget
                        .inspect_as::<FileWidget<U>, bool>(|file| file.name() == name)
                        .is_some_and(|name_equals| name_equals)
                })
                .map(|(widget, area, file_id)| (widget.clone(), area, file_id))
                .ok_or(())
        })?;

        self.inner_change_to(widget, area, file_id)
    }

    /// Switches to the next [`FileWidget<U>`].
    pub fn next_file(&self) -> Result<(), ()> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(());
        }

        let cur_name = self.active_file.inspect(|file| file.read().name());
        let (widget, area, file_id) = self
            .inspect_active_window(|window| {
                window
                    .widgets()
                    .cycle()
                    .filter(|(widget, ..)| widget.data_is::<FileWidget<U>>())
                    .skip_while(|(widget, ..)| {
                        widget
                            .inspect_as::<FileWidget<U>, bool>(|file| file.name() != cur_name)
                            .unwrap()
                    })
                    .nth(1)
                    .map(|(widget, area, file_id)| (widget.clone(), area, file_id))
            })
            .ok_or(())?;

        self.inner_change_to(widget.clone(), area, file_id)
    }

    /// Switches to the previous [`FileWidget<U>`].
    pub fn prev_file(&self) -> Result<(), ()> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(());
        }
        let cur_name = self.active_file.inspect(|file| file.read().name());
        let (widget, area, file_id) = self
            .inspect_active_window(|window| {
                window
                    .widgets()
                    .filter(|(widget, ..)| widget.data_is::<FileWidget<U>>())
                    .take_while(|(widget, ..)| {
                        widget
                            .inspect_as::<FileWidget<U>, bool>(|file| file.name() != cur_name)
                            .unwrap()
                    })
                    .last()
                    .map(|(widget, area, file_id)| (widget.clone(), area, file_id))
            })
            .ok_or(())?;

        self.inner_change_to(widget, area, file_id)
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to<Aw>(&self) -> Result<(), ()>
    where
        Aw: SchemeWidget<U>
    {
        let cur_file_id = self.commands.read().file_id;
        let (widget, area, file_id) = self
            .inspect_active_window(|window| {
                window
                    .widgets()
                    .find(|(widget, _, file_id)| {
                        widget.data_is::<Aw>() && (file_id.is_none() || cur_file_id == *file_id)
                    })
                    .map(|(widget, area, file_id)| (widget.clone(), area, file_id))
            })
            .ok_or(())?;

        self.inner_change_to(widget, area, file_id)
    }
}

unsafe impl<U> Send for Controler<U> where U: Ui {}
unsafe impl<U> Sync for Controler<U> where U: Ui {}

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
