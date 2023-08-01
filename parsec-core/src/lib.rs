#![feature(
    extract_if,
    result_option_inspect,
    trait_upcasting,
    let_chains,
    option_zip,
    iter_advance_by,
    return_position_impl_trait_in_trait
)]
#![allow(
    clippy::arc_with_non_send_sync,
    clippy::type_complexity,
    clippy::vec_init_then_push,
    clippy::while_let_on_iterator
)]

use std::{
    marker::PhantomData,
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex
    }
};

use commands::{Command, CommandErr, Commands};
use data::{ReadableData, RoData, RoNestedData, RwData};
use forms::FormPalette;
use ui::{Area, FileId, RoWindows, Ui, Window};
use widgets::{ActSchemeWidget, FileWidget};

pub mod commands;
pub mod data;
pub mod forms;
pub mod history;
pub mod input;
pub mod position;
pub mod session;
pub mod text;
pub mod ui;
pub mod widgets;

static BREAK_LOOP: AtomicBool = AtomicBool::new(false);
static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);
static CMD_FILE_ID: Mutex<Option<FileId>> = Mutex::new(None);

pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();

/// A general manager for Parsec, that can be called upon by certain
/// structs
pub struct Controler<U>
where
    U: Ui
{
    windows: RwData<Vec<Window<U>>>,
    active_window: usize,
    commands: RwData<Commands>,
    files_to_open: RwData<Vec<PathBuf>>,
    active_file: RwData<RoData<FileWidget<U>>>,
    active_widget: RwData<RwData<dyn ActSchemeWidget<U>>>,
    pub palette: FormPalette
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
        self.active_file
            .inspect(|file| file.read().name().unwrap_or(String::from("*scratch file*")))
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
    pub fn return_to_file(&self) -> Result<(), WidgetSwitchErr<FileWidget<U>, U>> {
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .find(|(widget_type, ..)| widget_type.ptr_eq(&*self.active_file.read()))
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_scheme_input().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::NotFound(PhantomData))?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id)
            )
        })
    }

    pub fn run_cmd(&self, cmd: impl ToString) -> Result<Option<String>, CommandErr> {
        self.commands.read().try_exec(cmd.to_string())
    }

    /// Quits Parsec.
    pub fn quit(&self) {
        BREAK_LOOP.store(true, Ordering::Release);
        SHOULD_QUIT.store(true, Ordering::Release);
    }

    /// Switches to the [`FileWidget<U>`] with the given name.
    pub fn switch_to_file(
        &self, target: impl AsRef<str>
    ) -> Result<(), WidgetSwitchErr<FileWidget<U>, U>> {
        let name = target.as_ref();
        let windows = self.windows.read();
        let (widget, area, file_id) = windows
            .iter()
            .flat_map(|window| window.widgets())
            .find(|(widget_type, ..)| {
                widget_type
                    .inspect_as::<FileWidget<U>, bool>(|file| {
                        file.name().is_some_and(|cmp| cmp == name)
                    })
                    .unwrap_or(false)
            })
            .map(|(widget_type, area, file_id)| {
                (widget_type.as_scheme_input().unwrap().clone(), area, file_id)
            })
            .ok_or(WidgetSwitchErr::FileNotFound(String::from(name), PhantomData))?;

        switch_widget(&windows, &self.active_widget, &self.active_file, (widget, area, file_id))
    }

    /// Switches to the next [`FileWidget<U>`].
    pub fn next_file(&self) -> Result<(), WidgetSwitchErr<FileWidget<U>, U>> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(WidgetSwitchErr::NoOthersExist(PhantomData));
        }

        let cur_name = self.active_file.inspect(|file| file.read().name());
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .cycle()
                .filter(|(widget, ..)| widget.data_is::<FileWidget<U>>())
                .skip_while(|(widget_type, ..)| {
                    widget_type
                        .inspect_as::<FileWidget<U>, bool>(|file| file.name() != cur_name)
                        .unwrap_or(false)
                })
                .nth(1)
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_scheme_input().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::NotFound(PhantomData))?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id)
            )
        })
    }

    /// Switches to the previous [`FileWidget<U>`].
    pub fn prev_file(&self) -> Result<(), WidgetSwitchErr<FileWidget<U>, U>> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(WidgetSwitchErr::NoOthersExist(PhantomData));
        }

        let cur_name = self.active_file.inspect(|file| file.read().name());
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .filter(|(widget_type, ..)| widget_type.data_is::<FileWidget<U>>())
                .take_while(|(widget_type, ..)| {
                    widget_type
                        .inspect_as::<FileWidget<U>, bool>(|file| file.name() != cur_name)
                        .unwrap_or(false)
                })
                .last()
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_scheme_input().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::NotFound(PhantomData))?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id)
            )
        })
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to<Sw>(&self) -> Result<(), WidgetSwitchErr<Sw, U>>
    where
        Sw: ActSchemeWidget<U>
    {
        let cur_file_id = *CMD_FILE_ID.lock().unwrap();
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .find(|(widget, _, file_id)| {
                    widget.data_is::<Sw>() && (file_id.is_none() || cur_file_id == *file_id)
                })
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_scheme_input().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::NotFound(PhantomData))?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id)
            )
        })
    }
}

impl<U> Controler<U>
where
    U: Ui
{
    /// Returns a new instance of [`Controler`].
    fn new(window: Window<U>, palette: FormPalette) -> Self {
        // NOTE: For now, we're picking the first file as active.
        let (widget, ..) =
            window.widgets().find(|(widget, ..)| widget.data_is::<FileWidget<U>>()).unwrap();

        let file = widget.downcast_ref::<FileWidget<U>>().unwrap();
        let widget = widget.as_scheme_input().unwrap().clone();

        let active_file = RwData::new(RoData::from(&file));
        let controler = Self {
            windows: RwData::new(vec![window]),
            active_window: 0,
            commands: Commands::new_rw_data(),
            files_to_open: RwData::new(Vec::new()),
            active_file,
            active_widget: RwData::new(widget),
            palette
        };

        let quit = Command::new(["quit", "q"], move |_, _| {
            BREAK_LOOP.store(true, Ordering::Release);
            SHOULD_QUIT.store(true, Ordering::Release);
            Ok(None)
        });

        let edit = {
            let windows = RoData::from(&controler.windows);
            let files_to_open = controler.files_to_open.clone();
            let active_widget = controler.active_widget.clone();
            let active_file = controler.active_file.clone();
            edit_cmd(windows, files_to_open, active_widget, active_file)
        };

        let write = {
            let active_file = controler.active_file.clone();
            let windows = RoData::from(&controler.windows);
            Command::new(["write", "w"], move |_, args| {
                if let Some(name) = args.next() {
                    windows
                        .read()
                        .iter()
                        .flat_map(|window| window.widgets())
                        .find_map(|(widget, ..)| {
                            type Ret = Option<Result<Option<String>, String>>;
                            widget.inspect_as::<FileWidget<U>, Ret>(|file| {
                                file.name().is_some_and(|cmp| cmp == name).then(|| {
                                    file.write()
                                        .map(|bytes| Some(format!("Wrote {bytes} to {name}")))
                                })
                            })
                        })
                        .flatten()
                        .unwrap_or(Err(String::from("File not found")))
                } else {
                    let active_file = active_file.read();
                    let file = active_file.read();
                    if let Some(name) = file.name() {
                        file.write().map(|bytes| Some(format!("Wrote {bytes} bytes to {name}")))
                    } else {
                        Err(String::from("File does not have a name, and no name was given."))
                    }
                }
            })
        };

        controler.commands.mutate(|commands| {
            commands.try_add(quit).unwrap();
            commands.try_add(edit).unwrap();
            commands.try_add(write).unwrap();
        });

        controler
    }

    /// Inspects the currently active window.
    fn inspect_active_window<T>(&self, f: impl FnOnce(&Window<U>) -> T) -> T {
        self.windows.inspect(|windows| f(&windows[self.active_window]))
    }

    fn mutate_active_window<T>(&self, f: impl FnOnce(&mut Window<U>) -> T) -> T {
        self.windows.mutate(|windows| f(&mut windows[self.active_window]))
    }
}

unsafe impl<U> Send for Controler<U> where U: Ui {}
unsafe impl<U> Sync for Controler<U> where U: Ui {}

pub enum WidgetSwitchErr<Sw, U>
where
    Sw: ActSchemeWidget<U>,
    U: Ui
{
    NotFound(PhantomData<(Sw, U)>),
    FileNotFound(String, PhantomData<(Sw, U)>),
    AlreadyOnIt(PhantomData<(Sw, U)>),
    NoOthersExist(PhantomData<(Sw, U)>),
    CouldNotUnfocus(PhantomData<(Sw, U)>)
}

impl<Sw, U> std::fmt::Display for WidgetSwitchErr<Sw, U>
where
    Sw: ActSchemeWidget<U>,
    U: Ui
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let widget = std::any::type_name::<Sw>();
        match self {
            WidgetSwitchErr::NotFound(_) => write!(f, "No {widget} exists."),
            WidgetSwitchErr::FileNotFound(file, _) => {
                write!(f, "The file \"{file}\" is not currently open")
            }
            WidgetSwitchErr::AlreadyOnIt(_) => {
                write!(f, "You want to switch to {widget}, but are already on it.")
            }
            WidgetSwitchErr::NoOthersExist(_) => {
                write!(f, "You seem to want to switch to the next {widget}, but no others exist.")
            }
            WidgetSwitchErr::CouldNotUnfocus(_) => {
                write!(
                    f,
                    "The previously active widget, prior to the switch atempt, could not be \
                     unfocused."
                )
            }
        }
    }
}

/// Changes `self.active_widget`, given the target
/// [`ActionableWidget<U>`], its [`U::Area`][Ui::Area], and a
/// `file_id`.
fn switch_widget<Sw, U>(
    windows: &[Window<U>], active_widget: &RwData<RwData<dyn ActSchemeWidget<U>>>,
    active_file: &RwData<RoData<FileWidget<U>>>,
    target: (RwData<dyn ActSchemeWidget<U>>, &U::Area, Option<FileId>)
) -> Result<(), WidgetSwitchErr<Sw, U>>
where
    Sw: ActSchemeWidget<U>,
    U: Ui
{
    target.1.set_as_active();

    if let Ok(file) = target.0.clone().try_downcast::<FileWidget<U>>() {
        *active_file.write() = RoData::from(&file);
    }

    let prior_widget = std::mem::replace(&mut *active_widget.write(), target.0);
    windows
        .iter()
        .flat_map(|window| window.widgets())
        .find(|(widget_type, ..)| widget_type.ptr_eq(&prior_widget))
        .map(|(widget_type, area, _)| {
            widget_type.as_scheme_input().unwrap().write().on_unfocus(area);
        })
        .ok_or(WidgetSwitchErr::CouldNotUnfocus(PhantomData))?;

    // Order matters here, since `on_unfocus` could rely on the
    // `Commands`'s prior `file_id`.
    *CMD_FILE_ID.lock().unwrap() = target.2;
    active_widget.read().write().on_focus(target.1);

    Ok(())
}

fn edit_cmd<U>(
    windows: RoData<Vec<Window<U>>>, files_to_open: RwData<Vec<PathBuf>>,
    active_widget: RwData<RwData<dyn ActSchemeWidget<U>>>,
    active_file: RwData<RoData<FileWidget<U>>>
) -> Command
where
    U: Ui
{
    Command::new(["edit", "e"], move |_, args| {
        let Some(file) = args.next() else {
            return Err(String::from("No arguments supplied"));
        };

        let path = PathBuf::from(file);
        let name =
            path.file_name().ok_or(String::from("No file in path"))?.to_string_lossy().to_string();

        let windows = windows.read();
        let Some(target) = windows
            .iter()
            .flat_map(|window| window.widgets())
            .find(|(widget_type, ..)| {
                widget_type
                    .inspect_as::<FileWidget<U>, bool>(|file| {
                        file.name().is_some_and(|cmp| cmp == name)
                    })
                    .unwrap_or(false)
            })
            .map(|(widget_type, area, file_id)| {
                let ret = (widget_type.as_scheme_input().unwrap().clone(), area, file_id);
                ret
            })
        else {
            BREAK_LOOP.store(true, Ordering::Release);
            files_to_open.write().push(path);
            return Ok(Some(format!("Created {file}")));
        };

        switch_widget::<FileWidget<U>, U>(&windows, &active_widget, &active_file, target)
            .map(|_| Some(format!("Switched to {name}")))
            .map_err(|err: WidgetSwitchErr<FileWidget<U>, U>| err.to_string())
    })
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

#[macro_export]
macro_rules! status_parts {
    () => { Vec::new() };

    (@process $parts:expr; $part:expr $(,)?) => {
        $parts.push(StatusPart::from($part));
    };

    (@process $parts:expr; $part:expr, $($remainder:tt)*) => {
        $parts.push(StatusPart::from($part));

        status_parts!(@process $parts; $($remainder)*);
    };

    ($($text:tt)*) => {
        {
            use $crate::widgets::StatusPart;
            let mut parts = Vec::new();

            status_parts!(@process parts; $($text)*);

            parts
        }
    };
}

/// Internal macro used to log information.
#[macro_export]
macro_rules! log_info {
    ($($text:tt)*) => {{
        use std::{fs, io::Write, time::Instant};
        let mut log = fs::OpenOptions::new().append(true).open("log").unwrap();
        let mut text = format!($($text)*);
        if text.lines().count() > 1 {
            let chars = text.char_indices().filter_map(|(pos, char)| (char == '\n').then_some(pos));
            let nl_indices: Vec<usize> = chars.collect();
            for index in nl_indices {
                text.insert_str(index + 1, "  ");
            }

            let duration = Instant::now().duration_since(*$crate::DEBUG_TIME_START.get().unwrap());
            write!(log, "\nat {:.4?}:\n  {text}", duration).unwrap();
        } else {
            let duration = Instant::now().duration_since(*$crate::DEBUG_TIME_START.get().unwrap());
            write!(log, "\nat {:.4?}: {text}", duration).unwrap();
        }
    }};
}
