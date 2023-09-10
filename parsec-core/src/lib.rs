#![feature(
    lazy_cell,
    extract_if,
    result_option_inspect,
    trait_upcasting,
    let_chains,
    option_zip,
    iter_advance_by,
    control_flow_enum,
    return_position_impl_trait_in_trait,
    decl_macro,
    generic_const_exprs
)]
#![allow(
    clippy::arc_with_non_send_sync,
    clippy::type_complexity,
    clippy::vec_init_then_push,
    clippy::while_let_on_iterator,
    incomplete_features,
    refining_impl_trait
)]

use std::{
    marker::PhantomData,
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

use commands::{Command, CommandErr, Commands};
use data::{ReadableData, RoData, RoNestedData, RwData};
use forms::FormPalette;
use input::InputMethod;
use ui::{Area, FileId, RoWindows, Ui, Window};
use widgets::{ActiveWidget, FileWidget};

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
    U: Ui,
{
    windows: RwData<Vec<Window<U>>>,
    active_window: usize,
    active_file: RwData<RwData<FileWidget>>,
    active_widget: RwData<RwData<dyn ActiveWidget>>,
    active_input: RwData<RwData<dyn InputMethod>>,
    commands: RwData<Commands>,
    files_to_open: RwData<Vec<PathBuf>>,
    pub palette: FormPalette,
}

/// # Querying Functions
///
/// These functions do not trigger any internal mutability within the
/// [`Controler`], and only serve to return information to the caller.
impl<U> Controler<U>
where
    U: Ui,
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
    /// [`FileWidget`] of the [`Controler`].
    ///
    /// One place where this function shines is in the
    /// [`StatusLine`], which can show information pertaining to the
    /// currently active file.
    ///
    /// If you wish for a version of this method that always points to the same
    /// file, but has the same return type, see [`active_file`]. If
    /// this restriction is not a requirement, use [`current_file`] instead.
    ///
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`active_file`]: Self::active_file()
    pub fn dyn_active_file(&self) -> RoNestedData<FileWidget> {
        RoNestedData::from(&self.active_file)
    }

    /// Same as [`dynamic_active_file`], but always points to the file the was
    /// active at the moment of the function call.
    ///
    /// Use this if you want an alternative for [`dynamic_active_file`] that has
    /// the same data type. If that is not your need, use [`current_file`]
    /// instead, since it is more efficient in its reading.
    ///
    /// [`dyn_active_file`]: Self::dynamic_active_file
    /// [`current_file`]: Self::current_file()
    pub fn active_file(&self) -> RoNestedData<FileWidget> {
        RoNestedData::from(&RwData::new(self.active_file.read().clone()))
    }

    /// A [`RoData`] that points to the currently active file.
    ///
    /// This reference will always point to the same file, if you want a
    /// reference that changes where it is pointing, such that it always points
    /// to the active file at that moment, see [`dyn_active_file`].
    ///
    /// [`dyn_active_file`]: Self::dyn_active_file
    pub fn current_file(&self) -> RoData<FileWidget> {
        RoData::from(&*self.active_file.read())
    }

    /// Returns a special reference that always points to the active
    /// [`InputMethod`] of the [`Controler`].
    ///
    /// If you wish for a version of this method that always points to the same
    /// input, but has the same return type, see [`active_input`]. If
    /// this restriction is not a requirement, use [`current_input`] instead.
    ///
    /// [`active_input`]: Self::active_input()
    /// [`current_input`]: Self::current_input
    pub fn dyn_active_input(&self) -> RoNestedData<dyn InputMethod> {
        RoNestedData::from(&RwData::new(self.active_input.read().clone()))
    }

    /// Same as [`dynamic_active_input`], but always points to the input the was
    /// active at the moment of the function call.
    ///
    /// Use this if you want an alternative for [`dyn_active_input`] that
    /// has the same data type. If that is not your need, use
    /// [`current_input`] instead, since it is more efficient in its reading.
    ///
    /// [`dyn_active_input`]: Self::dynamic_active_input
    /// [`current_input`]: Self::current_input()
    pub fn active_input(&self) -> RoNestedData<dyn InputMethod> {
        RoNestedData::from(&self.active_input)
    }

    /// A [`RoData`] that points to the currently active input.
    ///
    /// This reference will always point to the same file, if you want a
    /// reference that changes where it is pointing, such that it always points
    /// to the active file at that moment, see [`dyn_active_input`].
    ///
    /// [`dyn_active_input`]: Self::dyn_active_input
    pub fn current_input(&self) -> RoData<dyn InputMethod> {
        RoData::from(&*self.active_input.read())
    }
}

/// # Modification Functions
///
/// These functions are used to alter the state of Parsec through the
/// internal mutability pattern.
impl<U> Controler<U>
where
    U: Ui,
{
    pub fn return_to_file(&self) -> Result<(), WidgetSwitchErr<FileWidget>> {
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .find(|(widget_type, ..)| widget_type.ptr_eq(&*self.active_file.read()))
                .map(|(widget, area, file_id)| (widget.as_active().unwrap().clone(), area, file_id))
                .ok_or(WidgetSwitchErr::NotFound)?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id),
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
        &self,
        target: impl AsRef<str>,
    ) -> Result<(), WidgetSwitchErr<FileWidget>> {
        let name = target.as_ref();
        let windows = self.windows.read();
        let (widget, area, file_id) = windows
            .iter()
            .flat_map(|window| window.widgets())
            .find(|(widget_type, ..)| {
                widget_type
                    .inspect_as::<FileWidget, bool>(|file| {
                        file.name().is_some_and(|cmp| cmp == name)
                    })
                    .unwrap_or(false)
            })
            .map(|(widget_type, area, file_id)| {
                (widget_type.as_active().unwrap().clone(), area, file_id)
            })
            .ok_or(WidgetSwitchErr::FileNotFound(
                String::from(name),
                PhantomData,
            ))?;

        switch_widget(
            &windows,
            &self.active_widget,
            &self.active_file,
            (widget, area, file_id),
        )
    }

    /// Switches to the next [`FileWidget<U>`].
    pub fn next_file(&self) -> Result<(), WidgetSwitchErr<FileWidget>> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(WidgetSwitchErr::NoOthersExist);
        }

        let cur_name = self.active_file.inspect(|file| file.read().name());
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .cycle()
                .filter(|(widget, ..)| widget.data_is::<FileWidget>())
                .skip_while(|(widget_type, ..)| {
                    widget_type
                        .inspect_as::<FileWidget, bool>(|file| file.name() != cur_name)
                        .unwrap_or(false)
                })
                .nth(1)
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_active().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::NotFound)?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id),
            )
        })
    }

    /// Switches to the previous [`FileWidget<U>`].
    pub fn prev_file(&self) -> Result<(), WidgetSwitchErr<FileWidget>> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(WidgetSwitchErr::NoOthersExist);
        }

        let cur_name = self.active_file.inspect(|file| file.read().name());
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .filter(|(widget_type, ..)| widget_type.data_is::<FileWidget>())
                .take_while(|(widget_type, ..)| {
                    widget_type
                        .inspect_as::<FileWidget, bool>(|file| file.name() != cur_name)
                        .unwrap_or(false)
                })
                .last()
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_active().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::NotFound)?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id),
            )
        })
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to<W>(&self) -> Result<(), WidgetSwitchErr<W>>
    where
        W: ActiveWidget,
    {
        let cur_file_id = *CMD_FILE_ID.lock().unwrap();
        self.inspect_active_window(|window| {
            let (widget, area, file_id) = window
                .widgets()
                .find(|(widget, _, file_id)| {
                    widget.data_is::<W>() && (file_id.is_none() || cur_file_id == *file_id)
                })
                .map(|(widget_type, area, file_id)| {
                    (widget_type.as_active().unwrap().clone(), area, file_id)
                })
                .ok_or(WidgetSwitchErr::<W>::NotFound)?;

            switch_widget(
                &self.windows.read(),
                &self.active_widget,
                &self.active_file,
                (widget, area, file_id),
            )
        })
    }

    /// Returns a new instance of [`Controler`].
    fn new(
        window: Window<U>,
        palette: FormPalette,
        active_widget: RwData<dyn ActiveWidget>,
        active_input: RwData<dyn InputMethod>,
    ) -> Self {
        let active_file = active_widget.clone().try_downcast::<FileWidget>().unwrap();
        let controler = Self {
            windows: RwData::new(vec![window]),
            active_window: 0,
            active_file: RwData::new(active_file),
            active_widget: RwData::new(active_widget),
            active_input: RwData::new(active_input),
            commands: Commands::new_rw_data(),
            files_to_open: RwData::new(Vec::new()),
            palette,
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
                            widget.inspect_as::<FileWidget, Ret>(|file| {
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
                        file.write()
                            .map(|bytes| Some(format!("Wrote {bytes} bytes to {name}")))
                    } else {
                        Err(String::from(
                            "File does not have a name, and no name was given.",
                        ))
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
        self.windows
            .inspect(|windows| f(&windows[self.active_window]))
    }

    fn mutate_active_window<T>(&self, f: impl FnOnce(&mut Window<U>) -> T) -> T {
        self.windows
            .mutate(|windows| f(&mut windows[self.active_window]))
    }
}

unsafe impl<U> Send for Controler<U> where U: Ui {}
unsafe impl<U> Sync for Controler<U> where U: Ui {}

pub enum WidgetSwitchErr<W>
where
    W: ActiveWidget,
{
    NotFound,
    FileNotFound(String, PhantomData<W>),
    AlreadyOnIt,
    NoOthersExist,
    CouldNotUnfocus,
}

impl<W> std::fmt::Display for WidgetSwitchErr<W>
where
    W: ActiveWidget,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let widget = std::any::type_name::<W>();
        match self {
            WidgetSwitchErr::NotFound => write!(f, "No {widget} exists."),
            WidgetSwitchErr::FileNotFound(file, _) => {
                write!(f, "The file \"{file}\" is not currently open")
            }
            WidgetSwitchErr::AlreadyOnIt => {
                write!(f, "You want to switch to {widget}, but are already on it.")
            }
            WidgetSwitchErr::NoOthersExist => {
                write!(
                    f,
                    "You seem to want to switch to the next {widget}, but no others exist."
                )
            }
            WidgetSwitchErr::CouldNotUnfocus => {
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
fn switch_widget<U, W>(
    windows: &[Window<U>],
    active_widget: &RwData<RwData<dyn ActiveWidget>>,
    active_file: &RwData<RwData<FileWidget>>,
    target: (RwData<dyn ActiveWidget>, &U::Area, Option<FileId>),
) -> Result<(), WidgetSwitchErr<W>>
where
    W: ActiveWidget,
    U: Ui,
{
    target.1.set_as_active();

    if let Ok(file) = target.0.clone().try_downcast::<FileWidget>() {
        *active_file.write() = file;
    }

    let prior_widget = std::mem::replace(&mut *active_widget.write(), target.0.clone());
    windows
        .iter()
        .flat_map(|window| window.widgets())
        .find(|(widget, ..)| widget.ptr_eq(&prior_widget))
        .inspect(|(widget, area, _)| (*widget).on_unfocus(area))
        .ok_or(WidgetSwitchErr::CouldNotUnfocus)?;

    // Order matters here, since `on_unfocus` could rely on the
    // `Commands`'s prior `file_id`.
    *CMD_FILE_ID.lock().unwrap() = target.2;
    windows
        .iter()
        .flat_map(|window| window.widgets())
        .find(|(widget, ..)| widget.ptr_eq(&*active_widget.read()))
        .inspect(|(widget, area, _)| (*widget).on_focus(area))
        .ok_or(WidgetSwitchErr::CouldNotUnfocus)?;

    Ok(())
}

fn edit_cmd<U>(
    windows: RoData<Vec<Window<U>>>,
    files_to_open: RwData<Vec<PathBuf>>,
    active_widget: RwData<RwData<dyn ActiveWidget>>,
    active_file: RwData<RwData<FileWidget>>,
) -> Command
where
    U: Ui,
{
    Command::new(["edit", "e"], move |_, args| {
        let Some(file) = args.next() else {
            return Err(String::from("No arguments supplied"));
        };

        let path = PathBuf::from(file);
        let name = path
            .file_name()
            .ok_or(String::from("No file in path"))?
            .to_string_lossy()
            .to_string();

        let windows = windows.read();
        let Some(target) = windows
            .iter()
            .flat_map(|window| window.widgets())
            .find(|(widget_type, ..)| {
                widget_type
                    .inspect_as::<FileWidget, bool>(|file| {
                        file.name().is_some_and(|cmp| cmp == name)
                    })
                    .unwrap_or(false)
            })
            .map(|(widget_type, area, file_id)| {
                let ret = (widget_type.as_active().unwrap().clone(), area, file_id);
                ret
            })
        else {
            BREAK_LOOP.store(true, Ordering::Release);
            files_to_open.write().push(path);
            return Ok(Some(format!("Created {file}")));
        };

        switch_widget::<U, FileWidget>(&windows, &active_widget, &active_file, target)
            .map(|_| Some(format!("Switched to {name}")))
            .map_err(|err: WidgetSwitchErr<FileWidget>| err.to_string())
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
            for index in nl_indices.iter().rev() {
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
