#![doc = include_str!("README.md")]
#![feature(
    extract_if,
    iter_advance_by,
    iter_intersperse,
    trait_upcasting,
    let_chains,
    decl_macro,
    step_trait,
    type_alias_impl_trait,
    if_let_guard,
    closure_lifetime_binder
)]

use std::{
    any::{TypeId, type_name},
    collections::HashMap,
    marker::PhantomData,
    sync::{
        Arc, LazyLock, Mutex, Once,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use parking_lot::RwLock;
use serde::{Deserialize, Serialize};

pub use self::{commands::has_ended, data::context};
use self::{
    text::{Text, err, hint},
    ui::Ui,
};

pub mod cache;
pub mod commands;
pub mod data;
pub mod forms;
pub mod history;
pub mod hooks;
pub mod input;
pub mod session;
pub mod text;
pub mod ui;
pub mod widgets;

/// A plugin for Duat
///
/// A plugin is something that can be invoked in the configuration
/// crate for Duat, and can apply a multitude of effects upon its
/// creation.
pub trait Plugin<U: Ui>: 'static {
    /// A cacheable struct for your plugin
    ///
    /// If you want data to be stored between executions, you can
    /// store it in this struct, and it will be returned to the
    /// plugin when Duat is executed in the future. If you don't
    /// need this feature, you can set `type Cache = ();`
    type Cache: Default + Serialize + Deserialize<'static> + 'static
    where
        Self: Sized;

    /// Returns a new instance from an old cache
    ///
    /// If this is the first time the plugin was loaded, it will
    /// receive [`Cache::default()`] as the argument.
    ///
    /// ```rust
    /// # use duat_core::{
    /// #     commands,
    /// #     hooks::{self, *},
    /// #     input::{key, KeyCode},
    /// #     ui::Ui,
    /// #     widgets::File,
    /// #     Plugin,
    /// # };
    /// struct AutoSaver;
    /// impl<U: Ui> Plugin<U> for AutoSaver {
    ///     type Cache = ();
    ///
    ///     fn new(cache: Self::Cache) -> Self {
    ///         hooks::add::<KeySentTo<File, U>>(move |&(key, _)| {
    ///             // Assuming that we are leaving an insert mode
    ///             if let key!(KeyCode::Esc) = key {
    ///                 commands::run("write").unwrap();
    ///             }
    ///         });
    ///         AutoSaver
    ///     }
    /// }
    /// ```
    ///
    /// [`Cache::default()`]: Default::default
    /// [forms]: forms::Form
    /// [`File`]: widgets::File
    fn new(cache: Self::Cache) -> Self
    where
        Self: Sized;
}

pub mod thread {
    use std::{
        sync::{
            LazyLock,
            atomic::{AtomicUsize, Ordering},
            mpsc,
        },
        thread::JoinHandle,
        time::Duration,
    };

    use parking_lot::{Mutex, Once};

    /// Duat's [`JoinHandle`]s
    static HANDLES: AtomicUsize = AtomicUsize::new(0);

    /// Spawns a new thread, returning a [`JoinHandle`] for it.
    ///
    /// Use this function instead of [`std::thread::spawn`].
    ///
    /// The threads from this function work in the same way that
    /// threads from [`std::thread::spawn`] work, but it has
    /// synchronicity with Duat, and makes sure that the
    /// application won't exit or reload the configuration before
    /// all spawned threads have stopped.
    pub fn spawn<R: Send + 'static>(f: impl FnOnce() -> R + Send + 'static) -> JoinHandle<R> {
        HANDLES.fetch_add(1, Ordering::Relaxed);
        std::thread::spawn(|| {
            let ret = f();
            HANDLES.fetch_sub(1, Ordering::Relaxed);
            ret
        })
    }

    /// Queues an action
    ///
    /// All queued actions will be done in the sequence that they came
    /// in, sequentially in a single thread.
    pub(crate) fn queue<R>(f: impl FnOnce() -> R + Send + 'static) {
        static LOOP: Once = Once::new();
        static ACTIONS: LazyLock<(
            mpsc::Sender<Box<dyn FnOnce() + Send>>,
            Mutex<mpsc::Receiver<Box<dyn FnOnce() + Send>>>,
        )> = LazyLock::new(|| {
            let (sender, receiver) = mpsc::channel();
            (sender, Mutex::new(receiver))
        });

        let (sender, receiver) = &*ACTIONS;

        if still_running() {
            sender
                .send(Box::new(move || {
                    f();
                }))
                .unwrap();
        }

        LOOP.call_once(|| {
            spawn(|| {
                let recv = receiver.lock();
                while !crate::has_ended() {
                    if let Ok(f) = recv.recv_timeout(Duration::from_millis(50)) {
                        f();
                    }
                }
            });
        });
    }

    /// Returns true if there are any threads still running
    pub(crate) fn still_running() -> bool {
        HANDLES.load(Ordering::Relaxed) > 0
    }
}

/// A checker that returns `true` every `duration`
///
/// This is primarily used within [`WidgetCfg::build`], where a
/// `checker` must be returned in order to update the widget.
///
/// [`WidgetCfg::build`]: crate::widgets::WidgetCfg::build
pub fn periodic_checker(duration: Duration) -> impl Fn() -> bool {
    let check = Arc::new(AtomicBool::new(false));
    crate::thread::spawn({
        let check = check.clone();
        move || {
            while !crate::has_ended() {
                std::thread::sleep(duration);
                check.store(true, Ordering::Release);
            }
        }
    });

    move || check.fetch_and(false, Ordering::Acquire)
}

/// An error that can be displayed as [`Text`] in Duat
pub trait DuatError {
    fn into_text(self) -> Text;
}

/// Error for failures in Duat
#[derive(Clone)]
pub enum Error<E> {
    /// An alias wasn't just a single word
    AliasNotSingleWord(String),
    /// The caller for a command already pertains to another
    CallerAlreadyExists(String),
    /// No commands have the given caller as one of their own
    CallerNotFound(String),
    /// The command failed internally
    CommandFailed(Text),
    /// There was no caller and no arguments
    Empty,
    /// The [`Ui`] still hasn't created the first file
    ///
    /// [`Ui`]: ui::Ui
    NoFileYet,
    /// Since the [`Ui`] has no file, widgets can't relate to it
    ///
    /// [`Ui`]: ui::Ui
    NoFileForRelated,
    /// The [`Ui`] still hasn't created the first widget (a file)
    ///
    /// [`Ui`]: ui::Ui
    NoWidgetYet,
    /// The checked widget is not of the type given
    WidgetIsNot,
    /// The checked input is not of the type given
    InputIsNot(PhantomData<E>),
    /// The [`Layout`] does not allow for anothe file to open
    ///
    /// [`Layout`]: ui::Layout
    LayoutDisallowsFile,
}

impl<E1> Error<E1> {
    /// Converts [`Error<E1>`] to [`Error<E2>`]
    #[doc(hidden)]
    pub fn into_other_type<E2>(self) -> Error<E2> {
        match self {
            Self::AliasNotSingleWord(caller) => Error::AliasNotSingleWord(caller),
            Self::CallerAlreadyExists(caller) => Error::CallerAlreadyExists(caller),
            Self::CallerNotFound(caller) => Error::CallerNotFound(caller),
            Self::CommandFailed(failure) => Error::CommandFailed(failure),
            Self::Empty => Error::Empty,
            Self::NoFileYet => Error::NoFileYet,
            Self::NoFileForRelated => Error::NoFileForRelated,
            Self::NoWidgetYet => Error::NoWidgetYet,
            Self::WidgetIsNot => Error::WidgetIsNot,
            Self::InputIsNot(_) => Error::InputIsNot(PhantomData),
            Self::LayoutDisallowsFile => Error::LayoutDisallowsFile,
        }
    }
}

impl<E> DuatError for Error<E> {
    /// Turns the [`Error`] into formatted [`Text`]
    fn into_text(self) -> Text {
        let early = hint!(
            "Try this after " [*a] "OnUiStart" []
            ", maybe by using hooks::add::<OnUiStart>"
        );

        match self {
            Self::AliasNotSingleWord(caller) => err!(
                "The caller " [*a] caller [] " is not a single word."
            ),
            Self::CallerAlreadyExists(caller) => err!(
                "The caller " [*a] caller [] " already exists."
            ),
            Self::CallerNotFound(caller) => err!("The caller " [*a] caller [] " was not found."),
            Self::CommandFailed(failure) => failure,
            Self::Empty => err!("The command is empty."),
            Self::NoFileYet => err!("There is no file yet. " early),
            Self::NoFileForRelated => err!(
                "There is no file for a related " [*a] { type_name::<E>() } [] " to exist. " early
            ),
            Self::NoWidgetYet => err!("There can be no widget yet. " early),
            Self::WidgetIsNot => err!(
                "The widget is not " [*a] { type_name::<E>() } [] ". " early
            ),
            Self::InputIsNot(..) => err!(
                "This file's input is not " [*a] { type_name::<E>() } [] ". " early
            ),
            Self::LayoutDisallowsFile => err!(
                "The " [*a] "Layout" [] " disallows the addition of more files."
            ),
        }
    }
}

impl<E> std::fmt::Debug for Error<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_tuple(match self {
            Self::AliasNotSingleWord(_) => "AliasNotSingleWord",
            Self::CallerAlreadyExists(_) => "CallerAlreadyExists",
            Self::CallerNotFound(_) => "CallerNotFound",
            Self::CommandFailed(_) => "CommandFailed",
            Self::Empty => "Empty ",
            Self::NoFileYet => "NoFileYet ",
            Self::NoFileForRelated => "NoFileForRelated ",
            Self::NoWidgetYet => "NoWidgetYet ",
            Self::WidgetIsNot => "WidgetIsNot ",
            Self::InputIsNot(_) => "InputIsNot",
            Self::LayoutDisallowsFile => "LayoutDisallowsFile",
        });

        match self {
            Self::AliasNotSingleWord(str)
            | Self::CallerAlreadyExists(str)
            | Self::CallerNotFound(str) => debug.field(&str),
            Self::CommandFailed(text) => debug.field(&text),
            Self::Empty
            | Self::NoFileYet
            | Self::NoFileForRelated
            | Self::NoWidgetYet
            | Self::WidgetIsNot
            | Self::InputIsNot(_)
            | Self::LayoutDisallowsFile => &mut debug,
        }
        .finish()
    }
}

pub type Result<T, E> = std::result::Result<T, Error<E>>;

/// Takes a type and generates an appropriate name for it
///
/// Use this function if you need a name of a type to be
/// referrable by string, such as by commands or by the
/// user.
pub fn duat_name<T>() -> &'static str
where
    T: ?Sized + 'static,
{
    static NAMES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> = LazyLock::new(RwLock::default);
    let mut names = NAMES.write();
    let type_id = TypeId::of::<T>();

    if let Some(name) = names.get(&type_id) {
        name
    } else {
        let verbose = std::any::type_name::<T>();
        let mut name = String::new();

        for path in verbose.split_inclusive(['<', '>', ',', ' ']) {
            for segment in path.split("::") {
                let is_type = segment.chars().any(|c| c.is_uppercase());
                let is_punct = segment.chars().all(|c| !c.is_alphanumeric());
                let is_dyn = segment.starts_with("dyn");
                if is_type || is_punct || is_dyn {
                    name.push_str(segment);
                }
            }
        }

        names.insert(type_id, name.leak());
        names.get(&type_id).unwrap()
    }
}

/// Returns the source crate of a given type
///
/// This is primarily used on the [`cache`] module.
pub fn src_crate<T>() -> &'static str
where
    T: ?Sized + 'static,
{
    static CRATES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
        LazyLock::new(|| RwLock::new(HashMap::new()));
    let mut crates = CRATES.write();
    let type_id = TypeId::of::<T>();

    if let Some(src_crate) = crates.get(&type_id) {
        src_crate
    } else {
        let src_crate = std::any::type_name::<T>()
            .split([' ', ':'])
            .find(|w| *w != "dyn")
            .unwrap();

        crates.insert(type_id, src_crate);
        crates.get(&type_id).unwrap()
    }
}

// Debugging objects.
#[doc(hidden)]
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
#[doc(hidden)]
pub static HOOK: Once = Once::new();
#[doc(hidden)]
pub static LOG: LazyLock<Mutex<String>> = LazyLock::new(|| Mutex::new(String::new()));

/// Internal macro used to log information
#[doc(hidden)]
pub macro log_info($($text:tt)*) {{
    #[cfg(not(debug_assertions))] {
    	compile_error!("You are not supposed to use log_info on release profiles!");
    }

    use std::{fmt::Write, time::Instant};

    use crate::{HOOK, LOG};

    let mut text = format!($($text)*);

    HOOK.call_once(|| {
        let old_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            old_hook(info);
            println!("Logs:");
            println!("{}\n", LOG.lock().unwrap());
        }));
    });

    if let Some(start) = $crate::DEBUG_TIME_START.get() {
        if text.lines().count() > 1 {
            let chars = text.char_indices().filter_map(|(pos, char)| (char == '\n').then_some(pos));
            let nl_indices: Vec<usize> = chars.collect();
            for index in nl_indices.iter().rev() {
                text.insert_str(index + 1, "  ");
            }

            let duration = Instant::now().duration_since(*start);
            write!(LOG.lock().unwrap(), "\nat {:.4?}:\n  {text}", duration).unwrap();
        } else {
            let duration = Instant::now().duration_since(*start);
            write!(LOG.lock().unwrap(), "\nat {:.4?}: {text}", duration).unwrap();
        }
    } else {
        write!(LOG.lock().unwrap(), "\n{text}").unwrap();
    }
}}
