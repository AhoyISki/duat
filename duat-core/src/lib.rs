#![feature(
    extract_if,
    iter_intersperse,
    iter_order_by,
    trait_upcasting,
    let_chains,
    control_flow_enum,
    decl_macro,
    step_trait,
    type_alias_impl_trait,
    result_flattening,
    is_none_or,
    if_let_guard
)]
#![doc = include_str!("../README.md")]

use std::{
    any::{type_name, TypeId},
    collections::HashMap,
    marker::PhantomData,
    sync::{LazyLock, Mutex, Once},
};

use parking_lot::RwLock;
use text::{err, hint, Text};

pub mod cache;
pub mod commands;
pub mod data;
pub mod history;
pub mod hooks;
pub mod input;
pub mod palette;
pub mod session;
pub mod text;
pub mod ui;
pub mod widgets;

pub trait DuatError {
    fn into_text(self) -> Text;
}

/// Error for failures in Duat
#[derive(Clone)]
pub enum Error<E> {
    /// # Command related errors:

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

    /// # Context related errors:

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

    /// # Layout related errors:

    /// The [`Layout`] does not allow for anothe file to open
    ///
    /// [`Layout`]: ui::Layout
    LayoutDisallowsFile,

    /// # Cache related errors:

    /// The [cache] was not found in the string
    ///
    /// [cache]: cache::CacheAble
    CacheNotFound,

    /// The [cache] was not parsed properly
    ///
    /// [cache]: cache::CacheAble
    CacheNotParsed,
}

impl<E> Error<E> {
    pub fn into_other_type<T>(self) -> Error<T> {
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
            Self::CacheNotFound => Error::CacheNotFound,
            Self::CacheNotParsed => Error::CacheNotParsed,
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
            Self::CacheNotFound => err!(
                "The cache for " [*a] { type_name::<E>() } [] " was incomplete."
            ),
            Self::CacheNotParsed => err!(
                "The cache for " [*a] { type_name::<E>() } [] " could not be parsed."
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
            Self::CacheNotFound => "CacheNotFound",
            Self::CacheNotParsed => "CacheNotParsed",
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
            | Self::LayoutDisallowsFile
            | Self::CacheNotFound
            | Self::CacheNotParsed => &mut debug,
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
    static NAMES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
        LazyLock::new(|| RwLock::new(HashMap::new()));
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

pub mod thread {
    use std::{
        sync::atomic::{AtomicUsize, Ordering},
        thread::JoinHandle,
    };

    static HANDLES: AtomicUsize = AtomicUsize::new(0);

    /// Spawns a new thread, returning a [`JoinHandle`] for it.
    ///
    /// Use this function instead of [`std::thread::spawn`].
    ///
    /// The threads from this function workin the same way that
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

    pub(crate) fn still_running() -> bool {
        HANDLES.load(Ordering::Relaxed) > 0
    }
}

// Debugging objects.
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
pub static HOOK: Once = Once::new();
pub static LOG: LazyLock<Mutex<String>> = LazyLock::new(|| Mutex::new(String::new()));

/// Internal macro used to log information.
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
