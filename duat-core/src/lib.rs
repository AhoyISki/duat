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
//! #     mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, Mode, key}, ui::Ui, widgets::File,
//! # };
//! #[derive(Default, Clone)]
//! struct FindSeq(Option<char>);
//!
//! impl<U: Ui> Mode<U> for FindSeq {
//!     type Widget = File;
//!
//!     fn send_key(&mut self, key: KeyEvent, file: &mut File, area: &U::Area) {
//!         use KeyCode::*;
//!         let mut helper = EditHelper::new(file, area);
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
//!         helper.move_many(.., |mut m| {
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
//! #     mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, Mode, key},
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
//!     fn on_switch(&mut self, file: &mut File, area: &<U as Ui>::Area) {
//!         let cfg = file.print_cfg();
//!         let text = file.text_mut();
//!
//!         let regex = match self.is_line {
//!             true => "[^\n\\s][^\n]+",
//!             false => "[^\n\\s]+",
//!         };
//!         let (start, _) = area.first_points(text, cfg);
//!         let (end, _) = area.last_points(text, cfg);
//!         self.points = text.search_fwd(regex, (start, end)).unwrap().collect();
//!
//!         let seqs = key_seqs(self.points.len());
//!
//!         for (seq, (p1, _)) in seqs.iter().zip(&self.points) {
//!             let ghost = text!([EasyMotionWord] seq);
//!
//!             text.insert_tag(p1.byte(), Tag::GhostText(ghost), self.key);
//!             text.insert_tag(p1.byte(), Tag::StartConceal, self.key);
//!             let seq_end = p1.byte() + seq.chars().count() ;
//!             text.insert_tag(seq_end, Tag::EndConceal, self.key);
//!         }
//!     }
//!
//!     fn send_key(&mut self, key: KeyEvent, file: &mut File, area: &U::Area) {
//!         let char = match key {
//!             key!(KeyCode::Char(c)) => c,
//!             // Return a char that will never match.
//!             _ => '❌'
//!         };
//!         self.seq.push(char);
//!
//!         let mut helper = EditHelper::new(file, area);
//!         helper.cursors_mut().remove_extras();
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
//!             helper.text_mut().remove_tags(p1.byte(), self.key);
//!             helper.text_mut().remove_tags(p1.byte() + seq.len(), self.key);
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
//! [remove]: crate::text::Text::remove_tags
//! [Move]: crate::mode::Mover::move_to
#![feature(
    let_chains,
    decl_macro,
    step_trait,
    type_alias_impl_trait,
    trait_alias,
    debug_closure_helpers,
    box_as_ptr
)]
#![allow(clippy::single_range_in_vec_init)]

use std::{
    any::{TypeId, type_name},
    collections::HashMap,
    marker::PhantomData,
    sync::{
        Arc, LazyLock, Once,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

#[allow(unused_imports)]
use dirs_next::cache_dir;
pub use parking_lot::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use ui::Window;
use widgets::{File, Node, Widget};

use self::{
    text::{Text, err, hint},
    ui::Ui,
};

pub mod cache;
pub mod cfg;
pub mod cmd;
pub mod context;
pub mod data;
pub mod form;
pub mod hooks;
pub mod mode;
pub mod session;
pub mod status;
pub mod text;
pub mod ui;
pub mod widgets;

pub mod prelude {
    //! The prelude of Duat
    pub use crate::{
        Error, cmd,
        data::{self, RwData},
        form,
        text::{Builder, Text, err, hint, ok, text},
        ui, widgets,
    };
}

/// A plugin for Duat
///
/// Plugins must follow the builder pattern, and can be specific to
/// certain [`Ui`]s. Generally, plugins should do all the setup
/// necessary for their function when [`Plugin::plug`] is called.
///
/// [`Plugin`] will usually be [plugged] by a `macro` in the Duat
/// config crate. This macro requires that the [`Plugin`] be
/// compatible with the [`Ui`]. And this can cause some inconvenience
/// for the end user. For example, say we have a plugin like this:
///
/// ```rust
/// # use duat_core::{Plugin, ui::Ui};
/// struct MyPlugin;
///
/// impl<U: Ui> Plugin<U> for MyPlugin {
///     fn new() -> Self {
///         MyPlugin
///     }
///
///     fn plug(self) {
///         //..
///     }
/// }
///
/// impl MyPlugin {
///     pub fn modify(self) -> Self {
///         //..
/// #       self
///     }
/// }
/// ```
///
/// In the config crate, the user would have to add the plugin in a
/// really awkward way:
///
/// ```rust
/// # use duat_core::Plugin;
/// # macro_rules! plug {
/// #     ($($plug:expr),+) => {};
/// # }
/// # struct MyPlugin;
/// # impl<U: duat_core::ui::Ui> duat_core::Plugin<U> for MyPlugin {
/// #     fn new() -> Self {
/// #         MyPlugin
/// #     }
/// #     fn plug(self) {}
/// # }
/// # fn test<Ui: duat_core::ui::Ui>() {
/// plug!(<MyPlugin as Plugin<Ui>>::new().modify());
/// # }
/// ```
///
/// To prevent that, just add a [`Ui`] [`PhantomData`] parameter:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::{Plugin, ui::Ui};
/// struct MyPlugin<U>(PhantomData<U>);
///
/// impl<U: Ui> Plugin<U> for MyPlugin<U> {
///     fn new() -> Self {
///         MyPlugin(PhantomData)
///     }
///
///     fn plug(self) {
///         //..
///     }
/// }
///
/// impl<U> MyPlugin<U> {
///     pub fn modify(self) -> Self {
///         //..
/// #       self
///     }
/// }
/// ```
/// And now the plugin can be plugged much more normally:
///
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::Plugin;
/// # macro_rules! plug {
/// #     ($($plug:expr),+) => {};
/// # }
/// # struct MyPlugin<U>(PhantomData<U>);
/// # impl<U: duat_core::ui::Ui> duat_core::Plugin<U> for MyPlugin<U> {
/// #     fn new() -> Self {
/// #         MyPlugin(PhantomData)
/// #     }
/// #     fn plug(self) {}
/// # }
/// # impl<U> MyPlugin<U> {
/// #     pub fn modify(self) -> Self {
/// #         self
/// #     }
/// # }
/// # fn test<Ui: duat_core::ui::Ui>() {
/// plug!(MyPlugin::new().modify());
/// # }
/// ```
/// [plugged]: Plugin::plug
pub trait Plugin<U: Ui>: Sized {
    /// Returns a builder pattern instance of this [`Plugin`]
    fn new() -> Self;

    /// Sets up the [`Plugin`]
    fn plug(self);
}

pub mod thread {
    //! Multithreading for Duat
    //!
    //! The main rationale behind multithreading in Duat is not so
    //! much the performance gains, but more to allow for multi
    //! tasking, as some plugins (like an LSP) may block for a while,
    //! which would be frustrating for end users.
    //!
    //! The functions in this module differ from [`std::thread`] in
    //! that they synchronize with Duat, telling the application when
    //! there are no more threads running, so Duat can safely quit or
    //! reload.
    use std::{
        sync::atomic::{AtomicUsize, Ordering},
        thread::JoinHandle,
    };

    /// Duat's [`JoinHandle`]s
    pub static HANDLES: AtomicUsize = AtomicUsize::new(0);
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

    /// Returns true if there are any threads still running
    pub(crate) fn still_running() -> bool {
        HANDLES.load(Ordering::Relaxed) > 0
    }
}

pub mod clipboard {
    //! Clipboard interaction for Duat
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::OnceLock;

    pub use arboard::Clipboard;
    use parking_lot::Mutex;

    static CLIPB: OnceLock<&'static Mutex<Clipboard>> = OnceLock::new();

    /// Gets a [`String`] from the clipboard
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    ///
    /// Or if there is no clipboard i guess
    pub fn get_text() -> Option<String> {
        CLIPB.get().unwrap().lock().get_text().ok()
    }

    /// Sets a [`String`] to the clipboard
    pub fn set_text(text: impl std::fmt::Display) {
        let clipb = CLIPB.get().unwrap();
        clipb.lock().set_text(text.to_string()).unwrap();
    }

    pub(crate) fn set_clipboard(clipb: &'static Mutex<Clipboard>) {
        CLIPB.set(clipb).map_err(|_| {}).expect("Setup ran twice");
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
            while !crate::context::will_reload_or_quit() {
                std::thread::sleep(duration);
                check.store(true, Ordering::Release);
            }
        }
    });

    move || check.fetch_and(false, Ordering::Acquire)
}

/// An error that can be displayed as [`Text`] in Duat
pub trait DuatError {
    fn into_text(self) -> Box<Text>;
}

/// Error for failures in Duat
#[derive(Clone)]
pub enum Error<E> {
    /// The caller for a command already pertains to another
    CallerAlreadyExists(String),
    /// No commands have the given caller as one of their own
    CallerNotFound(String),
    /// The command failed internally
    CommandFailed(Box<Text>),
    /// There was no caller and no arguments
    Empty,
    /// Arguments could not be parsed correctly
    FailedParsing(Box<Text>),
    /// The [`Layout`] does not allow for another file to open
    ///
    /// [`Layout`]: ui::Layout
    LayoutDisallowsFile(PhantomData<E>),
    /// The [`Ui`] still hasn't created the first file
    ///
    /// [`Ui`]: ui::Ui
    NoFileYet,
    /// The [`File`] was not found
    FileNotFound(String),
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
}

impl<E1> Error<E1> {
    /// Converts [`Error<E1>`] to [`Error<E2>`]
    #[doc(hidden)]
    pub fn into_other_type<E2>(self) -> Error<E2> {
        match self {
            Self::CallerAlreadyExists(caller) => Error::CallerAlreadyExists(caller),
            Self::CallerNotFound(caller) => Error::CallerNotFound(caller),
            Self::CommandFailed(failure) => Error::CommandFailed(failure),
            Self::Empty => Error::Empty,
            Self::FailedParsing(failure) => Error::FailedParsing(failure),
            Self::NoFileYet => Error::NoFileYet,
            Self::FileNotFound(name) => Error::FileNotFound(name),
            Self::NoFileForRelated => Error::NoFileForRelated,
            Self::NoWidgetYet => Error::NoWidgetYet,
            Self::WidgetIsNot => Error::WidgetIsNot,
            Self::LayoutDisallowsFile(_) => Error::LayoutDisallowsFile(PhantomData),
        }
    }
}

impl<E> DuatError for Error<E> {
    /// Turns the [`Error`] into formatted [`Text`]
    fn into_text(self) -> Box<Text> {
        let early = hint!(
            "Try this after " [*a] "OnUiStart" []
            ", maybe by using hooks::add::<OnUiStart>"
        );

        match self {
            Self::CallerAlreadyExists(caller) => Box::new(err!(
                "The caller " [*a] caller [] " already exists."
            )),
            Self::CallerNotFound(caller) => {
                Box::new(err!("The caller " [*a] caller [] " was not found."))
            }
            Self::CommandFailed(failure) => failure,
            Self::Empty => Box::new(err!("The command is empty.")),
            Self::FailedParsing(failure) => failure,
            Self::NoFileYet => Box::new(err!("There is no file yet. " early)),
            Self::FileNotFound(name) => Box::new(err!("No file named " [*a] name [] " open")),
            Self::NoFileForRelated => Box::new(err!(
                "There is no file for a related " [*a] { type_name::<E>() } [] " to exist. " early
            )),
            Self::NoWidgetYet => Box::new(err!("There can be no widget yet. " early)),
            Self::WidgetIsNot => Box::new(err!(
                "The widget is not " [*a] { type_name::<E>() } [] ". " early
            )),
            Self::LayoutDisallowsFile(_) => Box::new(err!(
                "The " [*a] "Layout" [] " disallows the addition of more files."
            )),
        }
    }
}

impl<E> std::fmt::Debug for Error<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_tuple(match self {
            Self::CallerAlreadyExists(_) => "CallerAlreadyExists",
            Self::CallerNotFound(_) => "CallerNotFound",
            Self::CommandFailed(_) => "CommandFailed",
            Self::Empty => "Empty ",
            Self::FailedParsing(_) => "FailedParsing",
            Self::NoFileYet => "NoFileYet ",
            Self::FileNotFound(_) => "FileNotFound ",
            Self::NoFileForRelated => "NoFileForRelated ",
            Self::NoWidgetYet => "NoWidgetYet ",
            Self::WidgetIsNot => "WidgetIsNot ",
            Self::LayoutDisallowsFile(_) => "LayoutDisallowsFile",
        });

        match self {
            Self::CallerAlreadyExists(str) | Self::CallerNotFound(str) => debug.field(&str),
            Self::CommandFailed(text) | Self::FailedParsing(text) => debug.field(&text),
            Self::FileNotFound(name) => debug.field(&name),
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
///
/// # NOTE
///
/// Any `<Ui>` or `Ui, ` type arguments will be removed from the final
/// result, since Duat is supposed to have only one [`Ui`] in use.
pub fn duat_name<T: ?Sized + 'static>() -> &'static str {
    fn duat_name_inner(type_id: TypeId, type_name: &str) -> &'static str {
        static NAMES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(RwLock::default);
        let mut names = NAMES.write();

        if let Some(name) = names.get(&type_id) {
            name
        } else {
            let mut name = String::new();

            for path in type_name.split_inclusive(['<', '>', ',', ' ']) {
                for segment in path.split("::") {
                    let is_type = segment.chars().any(|c| c.is_uppercase());
                    let is_punct = segment.chars().all(|c| !c.is_alphanumeric());
                    let is_dyn = segment.starts_with("dyn");
                    if is_type || is_punct || is_dyn {
                        name.push_str(segment);
                    }
                }
            }

            while let Some((i, len)) = None
                .or_else(|| name.find("<Ui>").map(|i| (i, "<Ui>".len())))
                .or_else(|| name.find("Ui, ").map(|i| (i, "Ui, ".len())))
                .or_else(|| name.find("::<Ui>").map(|i| (i, "::<Ui>".len())))
            {
                unsafe {
                    name.as_mut_vec().splice(i..(i + len), []);
                }
            }

            names.insert(type_id, name.leak());
            names.get(&type_id).unwrap()
        }
    }

    duat_name_inner(TypeId::of::<T>(), std::any::type_name::<T>())
}

/// Returns the source crate of a given type
///
/// This is primarily used on the [`cache`] module.
pub fn src_crate<T: ?Sized + 'static>() -> &'static str {
    fn src_crate_inner(type_id: TypeId, type_name: &'static str) -> &'static str {
        static CRATES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(|| RwLock::new(HashMap::new()));
        let mut crates = CRATES.write();

        if let Some(src_crate) = crates.get(&type_id) {
            src_crate
        } else {
            let src_crate = type_name.split([' ', ':']).find(|w| *w != "dyn").unwrap();

            crates.insert(type_id, src_crate);
            crates.get(&type_id).unwrap()
        }
    }

    src_crate_inner(TypeId::of::<T>(), std::any::type_name::<T>())
}

/// Convenience function for the bounds of a range
fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => (*end + 1).min(max),
        std::ops::Bound::Excluded(end) => (*end).min(max),
        std::ops::Bound::Unbounded => max,
    };

    (start, end)
}

/// An entry for a file with the given name
#[allow(clippy::result_large_err)]
fn file_entry<'a, U: Ui>(
    windows: &'a [Window<U>],
    name: &str,
) -> std::result::Result<(usize, usize, &'a Node<U>), Text> {
    windows
        .iter()
        .enumerate()
        .flat_map(window_index_widget)
        .find(|(.., node)| matches!(node.inspect_as(|f: &File| f.name() == name), Some(true)))
        .ok_or_else(|| err!("File with name " [*a] name [] " not found."))
}

/// An entry for a widget of a specific type
#[allow(clippy::result_large_err)]
fn widget_entry<W: Widget<U>, U: Ui>(
    windows: &[Window<U>],
    w: usize,
) -> std::result::Result<(usize, usize, &Node<U>), Text> {
    let mut ff = context::fixed_file::<U>().unwrap();

    if let Some((widget, _)) = ff.get_related_widget::<W>() {
        windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
            .find(|(.., n)| n.ptr_eq(&widget))
    } else {
        iter_around(windows, w, 0).find(|(.., node)| node.data_is::<W>())
    }
    .ok_or(err!("No widget of type " [*a] { type_name::<W>() } [] " found."))
}

/// Iterator over a group of windows, that returns the window's index
fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl ExactSizeIterator<Item = (usize, usize, &Node<U>)> + DoubleEndedIterator {
    window
        .nodes()
        .enumerate()
        .map(move |(i, entry)| (index, i, entry))
}

/// Iterates around a specific widget, going forwards
fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, usize, &Node<U>)> + '_ {
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
) -> impl Iterator<Item = (usize, usize, &Node<U>)> {
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
        compile_error!("You are not supposed to use log_panic on release profiles!");
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
        compile_error!("You are not supposed to use log_file on release profiles!");
    }

    if let Some(cache) = cache_dir() {
        let mut file = std::io::BufWriter::new(
            std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(cache.join("duat/log"))
                .unwrap(),
        );

        use std::{io::Write, time::Instant};

        let mut text = format!($($text)*);

        if let Some(start) = $crate::DEBUG_TIME_START.get()
            && text != "" {
            if text.lines().count() > 1 {
                let chars = text
                    .char_indices()
                    .filter_map(|(pos, char)| (char == '\n').then_some(pos));
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
    }
}}
