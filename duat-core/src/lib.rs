//! # duat-core
//!
//! The core of Duat, this crate is meant to be used only for the
//! creation of plugins for Duat.
//!
//! # Quick Start
//!
//! The capabilities of `duat-core` are largely the same as the those
//! of Duat, however, the main difference is the multi [`Ui`] APIs of
//! this crate. In it, the public functions and types are defined in
//! terms of `U: Ui`,  which means that they can work on various
//! different interfaces:
//!
//! ```rust
//! # use duat_core::{
//! #     data::RwData, mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, Mode, key},
//! #     ui::Ui, widgets::File,
//! # };
//! #[derive(Default, Clone)]
//! struct FindSeq(Option<char>);
//!
//! impl<U: Ui> Mode<U> for FindSeq {
//!     type Widget = File;
//!
//!     fn send_key(
//!         &mut self,
//!         key: KeyEvent,
//!         widget: &RwData<Self::Widget>,
//!         area: &<U as Ui>::Area,
//!         cursors: &mut Cursors,
//!     ) {
//!         use KeyCode::*;
//!         let mut helper = EditHelper::new(widget, area, cursors);
//!
//!         // Make sure that the typed key is a character.
//!         let key!(Char(c)) = key else {
//!             mode::reset();
//!             return;
//!         };
//!         // Checking if a character was already sent.
//!         let Some(first) = self.0 else {
//!             self.0 = Some(c);
//!             return;
//!         };
//!
//!         helper.move_each(|mut m| {
//!             let pat: String = [first, c].iter().collect();
//!             let matched = m.search_fwd(pat, None).next();
//!             if let Some((p0, p1)) = matched {
//!                 m.move_to(p0);
//!                 m.set_anchor();
//!                 m.move_to(p1);
//!                 if m.is_incl() {
//!                     m.move_hor(-1)
//!                 }
//!             }
//!         });
//!
//!         mode::reset();
//!     }
//! }
//! ```
//!
//! In this example, I have created a [`Mode`] for [`File`]s. This
//! mode is (I think) popular within Vim circles. It's like the `f`
//! key in Vim, but it lets you look for a sequence of 2 characters,
//! instead of just one.
//!
//! What's great about it is that it will work no matter what editing
//! model the user is using. It could be Vim inspired, Kakoune
//! inspired, Emacs inspired, doesn't matter. All the user has to do
//! to use this mode is this:
//!
//! ```rust
//! # struct Normal;
//! # #[derive(Default, Clone)]
//! # struct FindSeq;
//! # fn map<M>(take: &str, give: &impl std::any::Any) {}
//! # // I fake it here because this function is from duat, not duat-core
//! map::<Normal>("<C-s>", &FindSeq::default());
//! ```
//!
//! And now, whenever the usert types `Control S` in `Normal` mode,
//! the mode will switch to `FindSeq`. You could replace `Normal` with
//! any other mode, from any other editing model, and this would still
//! work.
//!
//! Of course, this is most useful for plugins, for your own
//! configuration, you should probably just rely on [`map`] to
//! accomplish the same thing.
//!
//! Okay, but that was a relatively simple example, here's a more
//! advanced example, which makes use of more of Duat's features.
//!
//! This is a copy of [EasyMotion], a plugin for
//! Vim/Neovim/Kakoune/Emacs that lets you skip around the screen with
//! at most 2 keypresses.
//!
//! In order to emulate it, we use [ghost text] and [concealment]:
//!
//! ```rust
//! # use duat_core::{
//! #     data::RwData, mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, Mode, key},
//! #     text::{Key, Point, Tag, text}, ui::{Area, Ui}, widgets::File,
//! # };
//! #[derive(Clone)]
//! pub struct EasyMotion {
//!     is_line: bool,
//!     key: Key,
//!     points: Vec<(Point, Point)>,
//!     seq: String,
//! }
//!
//! impl EasyMotion {
//!     pub fn word() -> Self {
//!         Self {
//!             is_line: false,
//!             key: Key::new(),
//!             points: Vec::new(),
//!             seq: String::new(),
//!         }
//!     }
//!
//!     pub fn line() -> Self {
//!         Self {
//!             is_line: true,
//!             key: Key::new(),
//!             points: Vec::new(),
//!             seq: String::new(),
//!         }
//!     }
//! }
//!
//! impl<U: Ui> Mode<U> for EasyMotion {
//!     type Widget = File;
//!
//!     fn on_switch(
//!         &mut self,
//!         widget: &RwData<Self::Widget>,
//!         area: &<U as Ui>::Area,
//!         _cursors: &mut Cursors,
//!     ) {
//!         let mut widget = widget.write();
//!         let cfg = widget.print_cfg();
//!         let text = widget.text_mut();
//!
//!         let regex = match self.is_line {
//!             true => "[^\n\\s][^\n]+",
//!             false => "[^\n\\s]+",
//!         };
//!         let start = area.first_point(text, cfg);
//!         let end = area.last_point(text, cfg);
//!         self.points = text.search_fwd(regex, start, Some(end)).unwrap().collect();
//!
//!         let seqs = key_seqs(self.points.len());
//!
//!         for (seq, (p1, _)) in seqs.iter().zip(&self.points) {
//!             let ghost = text!([EasyMotionWord] seq);
//!
//!             text.insert_tag(p1.byte(), Tag::GhostText(ghost), self.key);
//!             text.insert_tag(p1.byte(), Tag::StartConceal, self.key);
//!             let seq_end = p1.byte() + seq.chars().count() as u32;
//!             text.insert_tag(seq_end, Tag::EndConceal, self.key);
//!         }
//!     }
//!
//!     fn send_key(
//!         &mut self,
//!         key: KeyEvent,
//!         widget: &RwData<Self::Widget>,
//!         area: &<U as Ui>::Area,
//!         cursors: &mut Cursors,
//!     ) {
//!         let char = match key {
//!             key!(KeyCode::Char(c)) => c,
//!             // Return a char that will never match.
//!             _ => '❌'
//!         };
//!         self.seq.push(char);
//!
//!         let mut helper = EditHelper::new(widget, area, cursors);
//!         helper.remove_extra_cursors();
//!
//!         let seqs = key_seqs(self.points.len());
//!         for (seq, &(p1, p2)) in seqs.iter().zip(&self.points) {
//!             if *seq == self.seq {
//!                 helper.move_main(|mut m| {
//!                     m.move_to(p1);
//!                     m.set_anchor();
//!                     m.move_to(p2);
//!                 });
//!                 mode::reset();
//!             } else if seq.starts_with(&self.seq) {
//!                 continue;
//!             }
//!
//!             helper.remove_tags_on(p1.byte(), self.key);
//!             helper.remove_tags_on(p1.byte() + seq.len() as u32, self.key);
//!         }
//!
//!         if self.seq.chars().count() == 2 || !LETTERS.contains(char) {
//!             mode::reset();
//!         }
//!     }
//! }
//!
//! fn key_seqs(len: usize) -> Vec<String> {
//!     let double = len / LETTERS.len();
//!
//!     let mut seqs = Vec::new();
//!     seqs.extend(LETTERS.chars().skip(double).map(char::into));
//!
//!     let chars = LETTERS.chars().take(double);
//!     seqs.extend(chars.flat_map(|c1| LETTERS.chars().map(move |c2| format!("{c1}{c2}"))));
//!
//!     seqs
//! }
//!
//! static LETTERS: &str = "abcdefghijklmnopqrstuvwxyz";
//! ```
//! All that this plugin is doing is:
//!
//! - Search on the screen for words/lines;
//! - In the beginning of said words/lines, add a [`Tag::GhostText`];
//! - Also add a [`Tag::StartConceal`] and a [`Tag::EndConceal`];
//! - Then, just match the typed keys and [remove] tags accordingly;
//! - [Move] to the matched sequence, if it exists;
//!
//! Now, in order to use this mode, it's the exact same thing as
//! `FindSeq`:
//!
//! ```rust
//! # struct Normal;
//! #[derive(Clone)]
//! # pub struct EasyMotion;
//! # impl EasyMotion {
//! #     pub fn word() -> Self {
//! #         Self
//! #     }
//! #     pub fn line() -> Self {
//! #         Self
//! #     }
//! # }
//! # fn map<M>(take: &str, give: &impl std::any::Any) {}
//! # // I fake it here because this function is from duat, not duat-core
//! map::<Normal>("<CA-w>", &EasyMotion::word());
//! map::<Normal>("<CA-l>", &EasyMotion::line());
//! ```
//!
//! [`Mode`]: crate::mode::Mode
//! [`File`]: crate::widgets::File
//! [`map`]: https://docs.rs/duat/0.2.0/duat/prelude/fn.map.html
//! [EasyMotion]: https://github.com/easymotion/vim-easymotion
//! [ghost text]: crate::text::Tag::GhostText
//! [concealment]: crate::text::Tag::StartConceal
//! [`Tag::GhostText`]: crate::text::Tag::GhostText
//! [`Tag::StartConceal`]: crate::text::Tag::StartConceal
//! [`Tag::EndConceal`]: crate::text::Tag::EndConceal
//! [remove]: crate::text::Text::remove_tags_on
//! [Move]: crate::mode::Mover::move_to
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
    closure_lifetime_binder,
    trait_alias,
    exact_size_is_empty
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
use ui::Window;
use widgets::{File, Node, Widget};

pub use self::{cmd::has_ended, data::context};
use self::{
    text::{Text, err, hint},
    ui::Ui,
};

pub mod cache;
pub mod cfg;
pub mod cmd;
pub mod data;
pub mod form;
pub mod hooks;
pub mod mode;
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
    /// #     cmd, hooks::{self, *}, mode::{key, KeyCode}, ui::Ui, widgets::File, Plugin,
    /// # };
    /// struct AutoSaver;
    /// impl<U: Ui> Plugin<U> for AutoSaver {
    ///     type Cache = ();
    ///
    ///     fn new(cache: Self::Cache) -> Self {
    ///         hooks::add::<KeySentTo<File, U>>(move |&(key, _)| {
    ///             // Assuming that we are leaving an insert mode
    ///             if let key!(KeyCode::Esc) = key {
    ///                 cmd::run("write").unwrap();
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
    };

    use parking_lot::{Mutex, Once};

    /// Duat's [`JoinHandle`]s
    static HANDLES: AtomicUsize = AtomicUsize::new(0);
    static ACTIONS: LazyLock<(mpsc::Sender<SentHook>, Mutex<mpsc::Receiver<SentHook>>)> =
        LazyLock::new(|| {
            let (sender, receiver) = mpsc::channel();
            (sender, Mutex::new(receiver))
        });

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
        let (sender, receiver) = &*ACTIONS;

        LOOP.call_once(|| {
            spawn(|| {
                let receiver = receiver.lock();
                while let Ok(SentHook::Fn(f)) = receiver.recv() {
                    f();
                }
            });
        });

        if !crate::has_ended() {
            sender
                .send(SentHook::Fn(Box::new(move || {
                    f();
                })))
                .unwrap();
        }
    }

    /// Returns true if there are any threads still running
    pub(crate) fn still_running() -> bool {
        HANDLES.load(Ordering::Relaxed) > 0
    }

    /// Stops the thread running the queue
    pub(crate) fn quit_queue() {
        let (sender, _) = &*ACTIONS;
        sender.send(SentHook::Quit).unwrap()
    }

    enum SentHook {
        Fn(Box<dyn FnOnce() + Send>),
        Quit,
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
    /// The [`Layout`] does not allow for another file to open
    ///
    /// [`Layout`]: ui::Layout
    LayoutDisallowsFile(PhantomData<E>),
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
            Self::LayoutDisallowsFile(_) => Error::LayoutDisallowsFile(PhantomData),
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
            Self::LayoutDisallowsFile(_) => err!(
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
            Self::LayoutDisallowsFile(_) => "LayoutDisallowsFile",
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
            | Self::LayoutDisallowsFile(_) => &mut debug,
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

/// Binary searching by key taking into account the index
fn binary_search_by_key_and_index<T, K>(
    vec: &[T],
    key: K,
    f: impl Fn(usize, &T) -> K,
) -> std::result::Result<usize, usize>
where
    K: PartialEq + Eq + PartialOrd + Ord,
{
    let mut size = vec.len();
    let mut left = 0;
    let mut right = size;

    while left < right {
        let mid = left + size / 2;

        let k = f(mid, &vec[mid]);

        match k.cmp(&key) {
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Equal => return Ok(mid),
            std::cmp::Ordering::Greater => right = mid,
        }

        size = right - left;
    }

    Err(left)
}

// Internal functions used for widget switching

/// An entry for a file with the given name
fn file_entry<'a, U: Ui>(
    windows: &'a [Window<U>],
    name: &str,
) -> std::result::Result<(usize, &'a Node<U>), Text> {
    windows
        .iter()
        .enumerate()
        .flat_map(window_index_widget)
        .find(|(_, node)| {
            matches!(
                node.inspect_as::<File, bool>(|file| file.name() == name),
                Some(true)
            )
        })
        .ok_or_else(|| err!("File with name " [*a] name [] " not found."))
}

/// An entry for a widget of a specific type
fn widget_entry<W: Widget<U>, U: Ui>(
    windows: &[Window<U>],
    w: usize,
) -> std::result::Result<(usize, &Node<U>), Text> {
    let cur_file = context::cur_file::<U>().unwrap();

    if let Some(node) = cur_file.get_related_widget::<W>() {
        windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
            .find(|(_, n)| n.ptr_eq(node.widget()))
    } else {
        iter_around(windows, w, 0).find(|(_, node)| node.data_is::<W>())
    }
    .ok_or(err!("No widget of type " [*a] { type_name::<W>() } [] " found."))
}

/// Iterator over a group of windows, that returns the window's index
fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl DoubleEndedIterator<Item = (usize, &Node<U>)> {
    window.nodes().map(move |entry| (index, entry))
}

/// Iterates around a specific widget, going forwards
fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, &Node<U>)> + '_ {
    let prev_len: usize = windows.iter().take(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .skip(window)
        .flat_map(window_index_widget)
        .skip(widget + 1)
        .chain(
            windows
                .iter()
                .enumerate()
                .take(window + 1)
                .flat_map(window_index_widget)
                .take(prev_len + widget),
        )
}

/// Iterates around a specific widget, going backwards
fn iter_around_rev<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, &Node<U>)> {
    let next_len: usize = windows.iter().skip(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .rev()
        .skip(windows.len() - window)
        .flat_map(move |(i, win)| {
            window_index_widget((i, win))
                .rev()
                .skip(win.len_widgets() - widget)
        })
        .chain(
            windows
                .iter()
                .enumerate()
                .rev()
                .take(windows.len() - window)
                .flat_map(move |(i, win)| window_index_widget((i, win)).rev())
                .take(next_len - (widget + 1)),
        )
}

// Debugging objects.
#[doc(hidden)]
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
#[doc(hidden)]
pub static HOOK: Once = Once::new();
#[doc(hidden)]
pub static LOG: LazyLock<Mutex<String>> = LazyLock::new(|| Mutex::new(String::new()));

/// Log information to be shown when panicking
#[doc(hidden)]
pub macro log_panic($($text:tt)*) {{
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

    if let Some(start) = $crate::DEBUG_TIME_START.get()
        && text != "" {
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

/// Log information to a log file
#[doc(hidden)]
pub macro log_file($($text:tt)*) {{
    #[cfg(not(debug_assertions))] {
    	compile_error!("You are not supposed to use log_info on release profiles!");
    }

    let mut file = std::io::BufWriter::new(
        std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("log")
            .unwrap(),
    );

    use std::{io::Write, time::Instant};

    let mut text = format!($($text)*);

    if let Some(start) = $crate::DEBUG_TIME_START.get()
        && text != "" {
        if text.lines().count() > 1 {
            let chars = text.char_indices().filter_map(|(pos, char)| (char == '\n').then_some(pos));
            let nl_indices: Vec<usize> = chars.collect();
            for index in nl_indices.iter().rev() {
                text.insert_str(index + 1, "  ");
            }

            let duration = Instant::now().duration_since(*start);
            write!(file, "\nat {:.4?}:\n  {text}", duration).unwrap();
        } else {
            let duration = Instant::now().duration_since(*start);
            write!(file, "\nat {:.4?}: {text}", duration).unwrap();
        }
    } else {
        write!(file, "\n{text}").unwrap();
    }
}}
