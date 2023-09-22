#![allow(incomplete_features, clippy::type_complexity)]
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
    generic_const_exprs,
    step_trait,
    type_alias_impl_trait,
    impl_trait_in_assoc_type,
    min_specialization
)]

use std::{
    marker::PhantomData,
    path::PathBuf,
    sync::atomic::{AtomicBool, Ordering},
};

use commands::{Command, Commands};
use data::{ActiveFile, RoData, RwData};
use forms::FormPalette;
use input::InputMethod;
use ui::{Area, RoWindows, Ui, Window};
use widgets::{ActiveWidget, File};

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

// Debugging objects.
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();

// Internal control objects.
static BREAK_LOOP: AtomicBool = AtomicBool::new(false);
static SHOULD_QUIT: AtomicBool = AtomicBool::new(false);

pub static PALETTE: FormPalette = FormPalette::new();
pub static COMMANDS: Commands = Commands::new();
pub static ACTIVE_FILE: ActiveFile = ActiveFile::new();

/// A general manager for Parsec, that can be called upon by certain
/// structs
pub struct Controler<U>
where
    U: Ui,
{
    windows: RwData<Vec<Window<U>>>,
    active_window: usize,
    active_widget: RwData<RwData<dyn ActiveWidget>>,
    files_to_open: RwData<Vec<PathBuf>>,
}

/// # Querying Functions
///
/// These functions do not trigger any internal mutability within the
/// [`Controler`], and only serve to return information to the caller.
impl<U> Controler<U>
where
    U: Ui,
{
    /// A read only list of [`ParsecWindow<U>`]s.
    pub fn windows(&self) -> RoWindows<U> {
        RoWindows::new(RoData::from(&self.windows))
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
    pub fn return_to_file(&self) -> Result<(), WidgetSwitchErr<File>> {
        self.inspect_active_window(|window| {
            let (widget, area) = window
                .widgets()
                .find(|(widget, ..)| ACTIVE_FILE.file_ptr_eq(widget))
                .map(|(widget, area)| (widget.as_active().unwrap(), area))
                .ok_or(WidgetSwitchErr::NotFound)?;

            switch_widget(&self.windows.read(), &self.active_widget, (widget, area))
        })
    }

    /// Quits Parsec.
    pub fn quit(&self) {
        BREAK_LOOP.store(true, Ordering::Release);
        SHOULD_QUIT.store(true, Ordering::Release);
    }

    /// Switches to the [`FileWidget<U>`] with the given name.
    pub fn switch_to_file(&self, target: impl AsRef<str>) -> Result<(), WidgetSwitchErr<File>> {
        let name = target.as_ref();
        let windows = self.windows.read();
        let (widget, area) = windows
            .iter()
            .flat_map(|window| window.widgets())
            .find(|(widget, ..)| {
                widget
                    .inspect_as::<File, bool>(|file| file.name().is_some_and(|cmp| cmp == name))
                    .unwrap_or(false)
            })
            .map(|(widget, area)| (widget.as_active().unwrap(), area))
            .ok_or(WidgetSwitchErr::FileNotFound(
                String::from(name),
                PhantomData,
            ))?;

        switch_widget(&windows, &self.active_widget, (widget, area))
    }

    /// Switches to the next [`FileWidget<U>`].
    pub fn next_file(&self) -> Result<(), WidgetSwitchErr<File>> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(WidgetSwitchErr::NoOthersExist);
        }

        let cur_name = ACTIVE_FILE.name();
        self.inspect_active_window(|window| {
            let (widget, area) = window
                .widgets()
                .cycle()
                .filter(|(widget, ..)| widget.data_is::<File>())
                .skip_while(|(widget, ..)| {
                    widget
                        .inspect_as::<File, bool>(|file| file.name() != cur_name)
                        .unwrap_or(false)
                })
                .nth(1)
                .map(|(widget, area)| (widget.as_active().unwrap(), area))
                .ok_or(WidgetSwitchErr::NotFound)?;

            switch_widget(&self.windows.read(), &self.active_widget, (widget, area))
        })
    }

    /// Switches to the previous [`FileWidget<U>`].
    pub fn prev_file(&self) -> Result<(), WidgetSwitchErr<File>> {
        if self.inspect_active_window(|window| window.file_names().count() < 2) {
            return Err(WidgetSwitchErr::NoOthersExist);
        }

        let cur_name = ACTIVE_FILE.name();
        self.inspect_active_window(|window| {
            let (widget, area) = window
                .widgets()
                .filter(|(widget, ..)| widget.data_is::<File>())
                .take_while(|(widget, ..)| {
                    widget
                        .inspect_as::<File, bool>(|file| file.name() != cur_name)
                        .unwrap_or(false)
                })
                .last()
                .map(|(widget, area)| (widget.as_active().unwrap(), area))
                .ok_or(WidgetSwitchErr::NotFound)?;

            switch_widget(&self.windows.read(), &self.active_widget, (widget, area))
        })
    }

    /// Switches to an [`ActionableWidget<U>`] of type `Aw`.
    pub fn switch_to<W>(&self) -> Result<(), WidgetSwitchErr<W>>
    where
        W: ActiveWidget,
    {
        self.inspect_active_window(|window| {
            let (widget, area) = window
                .widgets()
                .find(|(widget, _)| {
                    // TODO: Add data checking.
                    widget.data_is::<W>() // && (file_id.is_none() || cur_file_id == *file_id)
                })
                .map(|(widget, area)| (widget.as_active().unwrap(), area))
                .ok_or(WidgetSwitchErr::<W>::NotFound)?;

            switch_widget(&self.windows.read(), &self.active_widget, (widget, area))
        })
    }

    /// Returns a new instance of [`Controler`].
    fn new(window: Window<U>, active_widget: RwData<dyn ActiveWidget>) -> Self {
        let controler = Self {
            windows: RwData::new(vec![window]),
            active_window: 0,
            active_widget: RwData::new(active_widget),
            files_to_open: RwData::new(Vec::new()),
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
            edit_cmd(windows, files_to_open, active_widget)
        };

        let write = Command::new(["write", "w"], move |_, args| {
            let paths: Vec<&str> = args.collect();
            if paths.is_empty() {
                ACTIVE_FILE.inspect(|file, _| {
                    if let Some(name) = file.name() {
                        file.write()
                            .map(|bytes| Some(format!("Wrote {bytes} bytes to {name}")))
                    } else {
                        Err(String::from("Give the file a name, to write it with"))
                    }
                })
            } else {
                ACTIVE_FILE.inspect(|file, _| {
                    let mut bytes = 0;
                    for path in &paths {
                        bytes = file.write_to(path)?;
                    }

                    Ok(Some(format!("Wrote {bytes} to {}", paths.join(", "))))
                })
            }
        });

        COMMANDS.try_add(quit).unwrap();
        COMMANDS.try_add(edit).unwrap();
        COMMANDS.try_add(write).unwrap();

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
    target: (
        (&RwData<dyn ActiveWidget>, &RwData<dyn InputMethod>),
        &U::Area,
    ),
) -> Result<(), WidgetSwitchErr<W>>
where
    W: ActiveWidget,
    U: Ui,
{
    let ((widget, input), area) = target;

    area.set_as_active();

    if let Ok(file) = widget.clone().try_downcast::<File>() {
        ACTIVE_FILE.swap(file, input.clone());
    }

    let prior_widget = std::mem::replace(&mut *active_widget.write(), widget.clone());
    windows
        .iter()
        .flat_map(|window| window.widgets())
        .find(|(widget, ..)| widget.ptr_eq(&prior_widget))
        .inspect(|(widget, area)| (*widget).on_unfocus(area))
        .ok_or(WidgetSwitchErr::CouldNotUnfocus)?;

    // Order matters here, since `on_unfocus` could rely on the
    // `Commands`'s prior `file_id`.
    windows
        .iter()
        .flat_map(|window| window.widgets())
        .find(|(widget, ..)| widget.ptr_eq(&*active_widget.read()))
        .inspect(|(widget, area)| (*widget).on_focus(area))
        .ok_or(WidgetSwitchErr::CouldNotUnfocus)?;

    Ok(())
}

fn edit_cmd<U>(
    windows: RoData<Vec<Window<U>>>,
    files_to_open: RwData<Vec<PathBuf>>,
    active_widget: RwData<RwData<dyn ActiveWidget>>,
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
            .find(|(widget, ..)| {
                widget
                    .inspect_as::<File, bool>(|file| file.name().is_some_and(|cmp| cmp == name))
                    .unwrap_or(false)
            })
            .map(|(widget, area)| (widget.as_active().unwrap(), area))
        else {
            BREAK_LOOP.store(true, Ordering::Release);
            files_to_open.write().push(path);
            return Ok(Some(format!("Created {file}")));
        };

        switch_widget::<U, File>(&windows, &active_widget, target)
            .map(|_| Some(format!("Switched to {name}")))
            .map_err(|err| WidgetSwitchErr::to_string(&err))
    })
}

/// Internal macro used to log information.
pub macro log_info {
    ($($text:tt)*) => {
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
    }
}
