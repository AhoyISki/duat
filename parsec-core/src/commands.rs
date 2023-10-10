//! Creation and execution of commands.
//!
//! Commands in Parsec work through the use of functions that don't
//! require references (unless they're `'static`) and return results
//! which may contain a [`Text`] to be displayed, if successful, and
//! *must* contain an error [`Text`] to be displayed if they fail.
//!
//! Commands act on two parameters. which will be provided when ran:
//! [`Flags`] and [`Args`].
//!
//! [`Flags`] will contain a list of all flags that were passed to the
//! command. These flags follow the UNIX conventions, that is `"-"`
//! starts a short cluster, `"--"` starts a single, larger flag, and
//! `"--"` followed by nothing means that the remaining arguments are
//! not flags. Here's an example:
//!
//! `"my-command --flag1 --flag2 -short -- --not-flag more-args"`
//!
//! `"--not-flag"` would not be treated as a flag, being instead
//! treated as an argument in conjunction with `"more-args"`.
//!
//! [`Args`] is merely an iterator over the remaining arguments, which
//! are given as `&str`s to be consumed.
//!
//! Here's a simple example of how one would add a command:
//!
//! ```rust
//! # use parsec_core::commands::{self, Flags, Args};
//! # use std::sync::{
//! #     atomic::{AtomicBool, Ordering},
//! #     Arc
//! # };
//! #
//! // Any of these callers will work for running the command.
//! let callers = ["my-command", "mc"];
//!
//! // `commands::add` create a new globally avaliable command.
//! let result = commands::add(callers, move |_flags, _args| {
//!     unimplemented!();
//! });
//!
//! // Adding a command can fail if a command with the same
//! // name already exists.
//! // In this case, the command is brand new, so no the
//! // Result is `Ok`.
//! assert!(result.is_ok());
//! ```
//!
//! In this case, a command has been added that can be called with
//! both `"my-command"` and `"mc"`.
//!
//! Here's a simple command that makes use of [`Flags`]:
//!
//! ```rust
//! # use parsec_core::commands;
//! # use std::sync::{
//! #     atomic::{AtomicU32, Ordering},
//! #     Arc
//! # };
//! #
//! let expression = Arc::new(AtomicU32::default());
//! let callers = ["my-command", "mc"];
//! let my_command = {
//!     let expression = expression.clone();
//!     commands::add(callers, move |flags, _args| {
//!         // `Flags::long` checks for `--` flags
//!         if flags.long("happy") {
//!             expression.store('😁' as u32, Ordering::Relaxed)
//!         // `Flags::short` checks for `-` flags
//!         // They can check for any valid unicode character.
//!         } else if flags.short("🤯") {
//!             expression.store('🤯' as u32, Ordering::Relaxed)
//!         } else if flags.long("sad") {
//!             expression.store('😢' as u32, Ordering::Relaxed)
//!         } else {
//!             expression.store('😶' as u32, Ordering::Relaxed)
//!         }
//!         Ok(None)
//!     })
//! };
//!
//! // The order of flags doesn't matter.
//! commands::run("mc --sad -🤯 unused-1 unused-2").unwrap();
//!
//! let num = expression.load(Ordering::Relaxed);
//! assert_eq!(char::from_u32(num), Some('🤯'))
//! ```
//!
//! To run commands, simply call [`commands::run`]:
//!
//! ```rust
//! # use parsec_core::{
//! #     commands,
//! #     session::SessionCfg,
//! #     text::{PrintCfg, text},
//! #     ui::{FileBuilder, Ui},
//! #     widgets::{CommandLine, status},
//! # };
//! # fn test_fn<U: Ui>(ui: U) {
//! let session = SessionCfg::new(ui)
//!     .with_file_fn(|builder: &mut FileBuilder<U>, _file| {
//!         // `commands::run` might return an `Ok(Some(Text))`,
//!         // hence the double unwrap.
//!         let output = commands::run("lol").unwrap().unwrap();
//!         let status = status!("Output of \"lol\": " output);
//!
//!         builder.push(status.build());
//!     });
//!
//! let callers = ["lol", "lmao"];
//! commands::add(callers, |_flags, _args| {
//!     Ok(Some(text!("😜")))
//! }).unwrap();
//! # }
//! ```
//!
//! In the above example, we are creating a new [`SessionCfg`], which
//! will be used to start Parsec. in it, we're changing the
//! "`file_fn`", a file constructor that, among other things, will
//! attach widgets to files that are opened.
//!
//! In that "`file_fn`", the command `"lol"` is being ran. Notice that
//! the command doesn't exist at the time the closure was declared.
//! But since this closure will only be ran after the [`Session`] has
//! started, as long as the command was added before that point,
//! everything will work just fine.
//!
//! Here's an example that makes use of the arguments passed to the
//! command.
//!
//! ```rust
//! # use std::path::PathBuf;
//! # use parsec_core::{commands, text::text};
//! commands::add(["copy", "cp"], move |flags, args| {
//!     // If there is a next argument, next will return `Ok(arg)`.
//!     // If there isn't it will return `Err(Text)`.
//!     // If you needed an argument but got none, you should
//!     // return the given error with the `?` operator.
//!     let source = args.next()?;
//!     // You can return custom error messages in order to improve
//!     // the feedback of failures when running the command.
//!     let target_1 = args.next_or(text!("No target given."))?;
//!     // You can also parse the arguments of the caller to any
//!     // type that implements `FromStr`:
//!     let target_2 = args.next_as::<PathBuf>()?;
//!
//!     // This is optional, if you feel like your command shouldn't
//!     // allow for more args than are required, you can call this.
//!     args.ended()?;
//!
//!     if flags.long("link") {
//!         unimplemented!("Logic for linking files.");
//!     } else {
//!         unimplemented!("Logic for copying files.");
//!     }
//!
//!     // The return message (if there is one) is in the form of
//!     // a `Text`, so it is recommended that you use the
//!     // `parsec_core::text::text` macro to facilitate the
//!     // creation of that message.
//!     Ok(Some(text!(
//!         "Copied from " [AccentOk] source []
//!         " to " [AccentOk] target_1 [] "."
//!     )))
//! });
//! ```
//! The returned result from a command should make use of 4 specific
//! forms: `"CommandOk"`, `"AccentOk"`, `"CommandErr"` and
//! `"AccentErr"`. When errors are displayed, the `"Default"` [`Form`]
//! gets mapped to `"CommandOk"` if the result is [`Ok`], and to
//! `"CommandErr"` if the result is [`Err`]. . This formatting of
//! result messages allows for more expressive feedback while still
//! letting the end user configure their appearance.
//!
//! In the previous command, we handled a static number of arguments.
//! But we can also easily handle arguments as an "iterator of
//! results".
//!
//! ```rust
//! # use parsec_core::{commands, text::text};
//! commands::add(["write", "w"], move |_flags, mut args| {
//!     let mut count = 0;
//!     while let Ok(arg) = args.next() {
//!         count += 1;
//!         unimplemented!("Logic for writing to the files.");
//!     }
//!
//!     Ok(Some(text!(
//!         "Wrote to " [AccentOk] count [] " files successfully."
//!     )))
//! });
//! ```
//!
//! Do keep in mind that since args returns
//!
//! [`SessionCfg`]: crate::session::SessionCfg
//! [`Session`]: crate::session::Session
//! [`commands::run`]: crate::commands::run
//! [`Form`]: crate::forms::Form
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::atomic::Ordering;
use std::{
    any::TypeId,
    iter::Peekable,
    str::{FromStr, SplitWhitespace},
};

use self::inner::{Commands, InnerFlags};
use crate::{
    data::RwData,
    text::{text, Text},
    ui::{Area, Ui, Window},
    widgets::{ActiveWidget, PassiveWidget},
    BREAK_LOOP, SHOULD_QUIT,
};

static COMMANDS: Commands = Commands::new();

/// The standard result for [`commands`] operations.
///
/// [`commands`]: super
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct Args<'a> {
    count: usize,
    args: Peekable<SplitWhitespace<'a>>,
}

impl<'a> Args<'a> {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err(match self.count {
                0 => text!("Received " [AccentErr] 0 [] " arguments."),
                num => text!(
                    "Expected " [AccentErr] { num + 1 } []
                    "arguments, received " [AccentErr] num [] " instead."
                ),
            }),
        }
    }

    pub fn next_as<F: FromStr>(&mut self) -> std::result::Result<F, Text> {
        let arg = self.next()?;
        arg.parse().map_err(|_| {
            text!(
                "Couldn't convert " [AccentErr] arg []
                " to " [AccentErr] { std::any::type_name::<F>() } [] "."
            )
        })
    }

    pub fn next_or(&mut self, text: Text) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err(text),
        }
    }

    pub fn ended(&mut self) -> std::result::Result<(), Text> {
        match self.args.next() {
            Some(_) => Err(text!(
                "Expected " [AccentErr] { self.count } []
                "arguments, received " [AccentErr] { self.count + 1 } [] " instead."
            )),
            None => Ok(()),
        }
    }

    pub fn collect<B: FromIterator<&'a str> + 'static>(&mut self) -> B {
        let args: Vec<&str> = (&mut self.args).collect();

        if TypeId::of::<B>() == TypeId::of::<String>() {
            B::from_iter(args.into_iter().intersperse(" "))
        } else {
            B::from_iter(args)
        }
    }
}

/// A struct representing flags passed down to [`Command`]s when
/// running them.
///
/// There are 2 types of flag, the `short` and `long` flags.
///
/// `short` flags represent singular characters passed after a
/// single `'-'` character, they can show up in multiple
/// places, and should represent an incremental addition of
/// features to a command.
///
/// `long` flags are words that come after any `"--"` sequence,
/// and should represent more verbose, but more readable
/// versions of `short` flags.
///
/// # Examples
///
/// Both `short` and `long` flags can only be counted once, no
/// matter how many times they show up:
///
/// ```rust
/// # use parsec_core::commands::{split_flags, Flags};
/// let command = "my-command --foo --bar -abcde --foo --baz -abfgh arg1";
/// let mut command_args = command.split_whitespace().skip(1);
/// let (flags, args) = split_flags(command_args);
///
/// assert!(flags.short == String::from("abcdefgh"));
/// assert!(flags.long == vec!["foo", "bar", "baz"]);
/// ```
///
/// If you have any arguments that start with `'-'` or `"--"`, but
/// are not supposed to be flags, you can insert an empty
/// `"--"` after the flags, in order to distinguish them.
///
/// ```rust
/// # use parsec_core::commands::{split_flags, Flags};
/// let command =
///     "my-command --foo --bar -abcde -- --not-a-flag -also-not-flags";
/// let mut command_args = command.split_whitespace().skip(1);
/// let (flags, args) = split_flags(command_args);
///
/// assert!(flags.short == String::from("abcde"));
/// assert!(flags.long == vec!["foo", "bar"]);
/// ```
#[derive(Clone, Copy)]
pub struct Flags<'a, 'b>(&'a InnerFlags<'b>);

impl<'a, 'b> Flags<'a, 'b> {
    /// Checks if all of the [`char`]s in the `short` passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::{split_flags, Flags};
    /// let command = "run -abcdefgh -ablk args -wz";
    /// let mut command_args = command.split_whitespace().skip(1);
    /// let (flags, args) = split_flags(command_args);
    ///
    /// assert!(flags.short("k"));
    /// assert!(!flags.short("w"));
    /// ```
    pub fn short(&self, short: impl AsRef<str>) -> bool {
        self.0.short(short)
    }

    /// Returns `true` if the `long` flag was passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::{split_flags, Flags};
    /// let command = "run --foo --bar args -baz";
    /// let mut command_args = command.split_whitespace().skip(1);
    /// let (flags, args) = split_flags(command_args);
    ///
    /// assert!(flags.long("foo"));
    /// assert!(!flags.long("baz"));
    /// ```
    pub fn long(&self, flag: impl AsRef<str>) -> bool {
        self.0.long(flag)
    }

    /// Returns `true` if no flags have been passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::{split_flags, Flags};
    /// let command = "run arg1 --foo --bar arg2 -baz";
    /// let mut command_args = command.split_whitespace().skip(1);
    /// let (flags, args) = split_flags(command_args);
    ///
    /// assert!(flags.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Adds a command to the global list of commands.
///
/// This command cannot take any arguments beyond the [`Flags`]
/// and [`Args`], so any mutation of state must be done
/// through captured variables, usually in the form of
/// [`RwData<T>`]s.
///
/// # Examples
///
/// ```rust
/// # use parsec_core::{
/// #     commands,
/// #     data::RwData,
/// #     text::{text, Text},
/// #     widgets::status
/// # };
/// // Shared state, which will be displayed in a `StatusLine`.
/// let var = RwData::new(35);
///
/// commands::add(["set-var"], {
///     // A clone is necessary, in order to have one copy of `var`
///     // in the closure, while the other is in the `StatusLine`.
///     let var = var.clone();
///     move |_flags, args| {
///         let value: usize = args
///             .next()
///             .ok_or(text!("No value given."))?
///             .parse()
///             .map_err(Text::from)?;
///
///         *var.write() = value;
///
///         Ok(None)
///     }
/// });
///
/// // A `StatusLineCfg` that can be used to create a `StatusLine`.
/// let status_cfg = status!("The value is currently " var);
/// ```
///
/// In the above example, we created a variable that can be
/// modified by the command `"set-var"`, and then sent it to a
/// [`StatusLineCfg`], so that it could be displayed in a
/// [`StatusLine`]. Note that the use of an [`RwData<usize>`]/
/// [`RoData<usize>`] means that the [`StatusLine`] will
/// be updated automatically, whenever the command is ran.
///
/// [`StatusLineCfg`]: crate::widgets::StatusLineCfg
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`RoData<usize>`]: crate::data::RoData
pub fn add(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    COMMANDS.add(callers, f)
}

/// ```rust
/// # use parsec_core::{
/// #     commands,
/// #     data::RwData,
/// #     input::{InputMethod, MultiCursorEditor},
/// #     text::text,
/// #     widgets::File,
/// # };
/// #[derive(Debug)]
/// enum Mode {
///     Normal,
///     Insert,
///     Prompt,
///     Visual,
/// }
///
/// struct ModalEditor {
///     mode: Mode,
/// }
///
/// impl InputMethod for ModalEditor {
///     // Implementation details.
/// # type Widget = File
/// # where
/// #     Self: Sized;
///
/// # fn send_key(
/// #     &mut self,
/// #     key: crossterm::event::KeyEvent,
/// #     widget: &RwData<Self::Widget>,
/// #     area: &impl parsec_core::ui::Area,
/// # ) where
/// #     Self: Sized,
/// # {
/// #     todo!()
/// # }
/// }
///
/// commands::add_for_current::<ModalEditor>(
///     ["set-mode"],
///     |modal, flags, args| {
///         let mode = args.next().ok_or(text!("No mode given"))?;
///
///         match mode {
///             "normal" | "Normal" => modal.mode = Mode::Normal,
///             "insert" | "Insert" => modal.mode = Mode::Insert,
///             "prompt" | "Prompt" => modal.mode = Mode::Prompt,
///             "visual" | "Visual" => modal.mode = Mode::Visual,
///             mode => {
///                 return Err(text!(
///                     "Mode" [AccentErr] mode []
///                     "is not a valid mode"
///                 ));
///             }
///         }
///
///         let mode = format!("{:?}", modal.mode);
///
///         Ok(Some(text!("Mode was set to " [AccentOk] mode [] ".")))
///     }
/// )
/// .unwrap();
/// ```
pub fn add_for_current<T: 'static>(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(&mut T, Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    COMMANDS.add_for_current(callers, f)
}

/// Adds a command that can mutate a widget of the given type,
/// along with its associated [`dyn Area`].
///
/// This command will look for the given [`PassiveWidget`] in the
/// following order:
///
/// 1. Any instance that is "related" to the currently active
///    [`File`], that is, any widgets that were added during the
///    [`Session`]'s "`file_fn`".
/// 2. Other widgets in the currently active window, related or not to
///    any given [`File`].
/// 3. Any instance of the [`PassiveWidget`] that is found in other
///    windows, looking first at windows ahead.
///
/// Keep in mind that the search is deterministic, that is, if
/// there are multiple instances of the widget that fit the
/// same category, only one of them will ever be used.
///
/// This search algorithm allows a more versatile configuration of
/// widgets, for example, one may have a [`CommandLine`] per
/// [`File`], or one singular [`CommandLine`] that acts upon
/// all files in the window, and both would respond correctly
/// to the `"set-prompt"` command.
///
/// # Examples
///
/// In this example, we create a simple `Timer` widget, along with
/// some control commands.
///
/// ```rust
/// // Required feature for widgets.
/// #![feature(return_position_impl_trait_in_trait)]
/// # use std::{
/// #    sync::{
/// #        atomic::{AtomicBool, Ordering},
/// #        Arc,
/// #    },
/// #    time::Instant,
/// # };
/// # use parsec_core::{
/// #    commands,
/// #    palette::{self, Form},
/// #    text::{text, Text, AlignCenter},
/// #    ui::{Area, PushSpecs, Ui},
/// #    widgets::{PassiveWidget, Widget},
/// # };
/// # pub struct Timer {
/// #    text: Text,
/// #    instant: Instant,
/// #    running: Arc<AtomicBool>,
/// # }
/// impl PassiveWidget for Timer {
///     fn build<U: Ui>() -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
///         let timer = Self {
///             text: text!(AlignCenter [Counter] "0ms"),
///             instant: Instant::now(),
///             // No need to use an `RwData`, since
///             // `RwData::has_changed` is never called.
///             running: Arc::new(AtomicBool::new(false)),
///         };
///
///         // The checker should tell the `Timer` to update only
///         // if `running` is `true`.
///         let checker = {
///             // Clone any variables before moving them to
///             // the `checker`.
///             let running = timer.running.clone();
///             move || running.load(Ordering::Relaxed)
///         };
///
///         let specs = PushSpecs::below().with_lenght(1.0);
///
///         (Widget::passive(timer), checker, specs)
///     }
///
///     fn update(&mut self, _area: &impl Area) {
///         if self.running.load(Ordering::Relaxed) {
///             let duration = self.instant.elapsed();
///             let duration = format!("{:.3?}", duration);
///             self.text = text!(
///                 AlignCenter [Counter] duration [] "elapsed"
///             );
///         }
///     }
///     
///
///     fn text(&self) -> &Text {
///         &self.text
///     }
///
///     // The `once` function of a `PassiveWidget` is only called
///     // when that widget is first created.
///     // It is generally useful to add commands and set forms
///     // in the `palette`.
///     fn once() {
///         // `palette::set_weak_form` will only set that form if
///         // it doesn't already exist.
///         // That means that a user of the widget will be able to
///         // control that form by changing it before or after this
///         // widget is pushed.
///         palette::set_weak_form("Counter", Form::new().green());
///
///         commands::add_for_widget::<Timer>(
///             ["play"],
///             |timer, _area, _flags, _args| {
///                 timer.running.store(true, Ordering::Relaxed);
///
///                 Ok(None)
///             })
///             .unwrap();
///
///         commands::add_for_widget::<Timer>(
///             ["pause"],
///             |timer, _area, _flags, _args| {
///                 timer.running.store(false, Ordering::Relaxed);
///
///                 Ok(None)
///             })
///             .unwrap();
///
///         commands::add_for_widget::<Timer>(
///             ["pause"],
///             |timer, _area, _flags, _args| {
///                 timer.instant = Instant::now();
///
///                 Ok(None)
///             })
///             .unwrap();
///     }
/// }
/// ```
///
/// [`dyn Area`]: crate::ui::Area
/// [`File`]: crate::widgets::File
/// [`Session`]: crate::session::Session
/// [`CommandLine`]: crate::widgets::CommandLine
pub fn add_for_widget<Widget: PassiveWidget>(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(&mut Widget, &dyn Area, Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    COMMANDS.add_for_widget(callers, f)
}

/// Runs a full command, with a caller, [`Flags`], and [`Args`].
///
/// When running the command, the ordering of flags does not
/// matter, as long as they are placed before the arguments to the
/// command.
///
/// # Examples
///
/// ```rust
/// # use parsec_core::{
/// #     commands::{self, Result},
/// #     text::Text,
/// # };
/// # fn test() -> Result<Option<Text>> {
/// commands::run("set-prompt new-prompt")
/// # }
/// ```
///
/// In this case we're running a command that will affect the most
/// relevant [`CommandLine`]. See [`commands::add_for_widget`] for
/// more information.
///
/// [`CommandLine`]: crate::widgets::CommandLine
/// [`commands::add_for_widget`]: crate::commands::add_for_widget
pub fn run(command: impl ToString) -> Result<Option<Text>> {
    COMMANDS.run(command)
}

mod doc_code {}

/// Canonical way to quit Parsec.
///
/// By calling the quit command, all threads will finish their
/// tasks, and then Parsec will execute a program closing
/// function, as defined by the [`Ui`].
pub fn quit() {
    BREAK_LOOP.store(true, Ordering::Release);
    SHOULD_QUIT.store(true, Ordering::Release);
}

pub fn switch_to<W: ActiveWidget>() -> Result<Option<Text>> {
    COMMANDS.run(format!("switch-to {}", W::type_name()))
}

pub fn buffer(file: impl AsRef<str>) -> Result<Option<Text>> {
    COMMANDS.run(format!("buffer {}", file.as_ref()))
}

pub fn next_file() -> Result<Option<Text>> {
    COMMANDS.run("next-file")
}

pub fn prev_file() -> Result<Option<Text>> {
    COMMANDS.run("prev-file")
}

pub fn return_to_file() -> Result<Option<Text>> {
    COMMANDS.run("return-to-file")
}

/// Tries to alias a `caller` to an existing `command`.
///
/// Returns an [`Err`] if the `caller` is already a caller for
/// another command, or if `command` is not a real caller to an
/// exisiting [`Command`].
pub fn alias(alias: impl ToString, command: impl ToString) -> Result<Option<Text>> {
    COMMANDS.try_alias(alias, command)
}

/// Adds a widget getter to the globally accessible [`Commands`].
pub(crate) fn add_widget_getter<U: Ui>(getter: RwData<Vec<Window<U>>>) {
    COMMANDS.add_widget_getter(getter);
}

impl std::error::Error for Error {}

pub type CmdResult = std::result::Result<Option<Text>, Text>;

/// An failure in executing or adding a command.
#[derive(Debug)]
pub enum Error {
    NotSingleWord(String),
    AlreadyExists(String),
    CallerNotFound(String),
    Failed(Text),
    WrongArgCount(usize, usize),
    Empty,
}

impl Error {
    fn into_text(self) -> Text {
        match self {
            Error::NotSingleWord(caller) => text!(
                "The caller " [AccentErr] caller [] "is not a single word."
            ),
            Error::AlreadyExists(caller) => text!(
                "The caller " [AccentErr] caller [] "already exists."
            ),
            Error::CallerNotFound(caller) => text!(
                "The caller " [AccentErr] caller [] "was not found."
            ),
            Error::Failed(failure) => failure,
            Error::WrongArgCount(needed, got) => text!(
                "Wrong argument count, needed " [AccentErr] needed []
                " and got " [AccentErr] got [] "."
            ),
            Error::Empty => text!("The command is empty."),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NotSingleWord(caller) => {
                write!(f, "The caller \"{caller}\" is not a single word")
            }
            Error::AlreadyExists(caller) => {
                write!(f, "The caller \"{caller}\" already exists.")
            }
            Error::CallerNotFound(caller) => {
                write!(f, "The caller \"{caller}\" was not found.")
            }
            Error::Failed(failure) => f.write_str(&failure.chars().collect::<String>()),
            Error::WrongArgCount(needed, got) => {
                write!(f, "Wrong argument count, needed {needed} and got {got}.")
            }
            Error::Empty => f.write_str("The command is empty"),
        }
    }
}

mod inner {
    use std::{
        collections::HashMap,
        mem::MaybeUninit,
        sync::{Arc, LazyLock, RwLock},
    };

    use super::{Args, CmdResult, Error, Flags, Result};
    use crate::{
        data::{Data, RwData},
        text::{text, Text},
        ui::{Area, Ui, Window},
        widgets::PassiveWidget,
        CURRENT_FILE, CURRENT_WIDGET,
    };

    /// A struct representing flags passed down to [`Command`]s when
    /// running them.
    pub struct InnerFlags<'a> {
        short: String,
        long: Vec<&'a str>,
    }

    impl<'a> InnerFlags<'a> {
        /// Checks if all of the [`char`]s in the `short` passed.
        pub fn short(&self, short: impl AsRef<str>) -> bool {
            let mut all_chars = true;
            for char in short.as_ref().chars() {
                all_chars &= self.short.contains(char);
            }
            all_chars
        }

        /// Returns `true` if the `long` flag was passed.
        pub fn long(&self, flag: impl AsRef<str>) -> bool {
            self.long.contains(&flag.as_ref())
        }

        /// Returns `true` if no flags have been passed.
        pub fn is_empty(&self) -> bool {
            self.short.is_empty() && self.long.is_empty()
        }
    }

    /// A function that can be called by name.
    #[derive(Clone)]
    struct Command {
        f: RwData<dyn FnMut(Flags, Args) -> CmdResult>,
        callers: Arc<[String]>,
    }

    impl Command {
        /// Returns a new instance of [`Command`].
        fn new<F>(callers: impl IntoIterator<Item = impl ToString>, f: F) -> Self
        where
            F: FnMut(Flags, Args) -> CmdResult + 'static,
        {
            let callers: Arc<[String]> = callers
                .into_iter()
                .map(|caller| caller.to_string())
                .collect();

            if let Some(caller) = callers
                .iter()
                .find(|caller| caller.split_whitespace().count() != 1)
            {
                panic!("Command caller \"{caller}\" contains more than one word.");
            }
            Self {
                f: RwData::new_unsized::<F>(Arc::new(RwLock::new(f))),
                callers,
            }
        }

        /// Executes the inner function if the `caller` matches any of
        /// the callers in [`self`].
        fn try_exec(&self, flags: Flags, args: Args<'_>) -> Result<Option<Text>> {
            (self.f.write())(flags, args).map_err(Error::Failed)
        }

        /// The list of callers that will trigger this command.
        fn callers(&self) -> &[String] {
            &self.callers
        }
    }

    unsafe impl Send for Command {}
    unsafe impl Sync for Command {}

    /// A list of [`Command`]s.
    pub struct Commands {
        inner: LazyLock<RwData<InnerCommands>>,
        widget_getter: RwLock<MaybeUninit<RwData<dyn WidgetGetter>>>,
    }

    impl Commands {
        /// Returns a new instance of [`Commands`].
        pub const fn new() -> Self {
            Self {
                inner: LazyLock::new(|| {
                    let inner = RwData::new(InnerCommands {
                        list: Vec::new(),
                        aliases: HashMap::new(),
                    });

                    let alias = {
                        let inner = inner.clone();
                        Command::new(["alias"], move |flags, mut args| {
                            if !flags.is_empty() {
                                Err(text!(
                                    "An alias cannot take any flags, try moving them after the \
                                     command, like \"alias my-alias my-caller --foo --bar\", \
                                     instead of \"alias --foo --bar my-alias my-caller\""
                                ))
                            } else {
                                let alias = args.next()?.to_string();
                                let args: String = args.collect();

                                inner
                                    .write()
                                    .try_alias(alias, args)
                                    .map_err(Error::into_text)
                            }
                        })
                    };

                    let quit = {
                        Command::new(["quit", "q"], move |_, _| {
                            super::quit();
                            Ok(None)
                        })
                    };

                    inner.write().try_add(quit).unwrap();
                    inner.write().try_add(alias).unwrap();

                    inner
                }),
                widget_getter: RwLock::new(MaybeUninit::uninit()),
            }
        }

        /// Runs the given command.
        pub fn run(&self, args: impl ToString) -> Result<Option<Text>> {
            let args = args.to_string();
            let mut args = args.split_whitespace();
            let caller = args.next().ok_or(Error::Empty)?;

            let (call, command) = self.inner.inspect(|inner| {
                let args: String = args.intersperse(" ").collect();

                let (args, caller) = if let Some(command) = inner.aliases.get(caller) {
                    let (caller, aliased_args) = command;

                    let args: String = aliased_args
                        .split_whitespace()
                        .chain(args.split_whitespace())
                        .collect();

                    (args, caller.clone())
                } else {
                    (args, caller.to_string())
                };

                if let Some(command) = inner
                    .list
                    .iter()
                    .find(|command| command.callers.contains(&caller))
                {
                    Ok((args, command.clone()))
                } else {
                    Err(Error::CallerNotFound(caller))
                }
            })?;

            let (flags, args) = split_flags(call.split_whitespace());

            Ok(command.try_exec(Flags(&flags), args).unwrap())
        }

        /// Add a new command to the [`self`].
        pub fn add(
            &self,
            callers: impl IntoIterator<Item = impl ToString>,
            f: impl FnMut(Flags, Args) -> CmdResult + 'static,
        ) -> Result<()> {
            let command = Command::new(callers, f);
            self.inner.write().try_add(command)
        }

        /// Adds a command that will try to affect the currently
        /// active widget or file.
        pub fn add_for_current<T: 'static>(
            &self,
            callers: impl IntoIterator<Item = impl ToString>,
            mut f: impl FnMut(&mut T, Flags, Args) -> CmdResult + 'static,
        ) -> Result<()> {
            let command = Command::new(callers, move |flags, args| {
                let result =
                    CURRENT_FILE.mutate_related::<T, CmdResult>(|t| f(t, flags, args.clone()));

                result
                    .or_else(|| {
                        CURRENT_WIDGET.mutate_as::<T, CmdResult>(|t| f(t, flags, args.clone()))
                    })
                    .transpose()?
                    .ok_or_else(|| {
                        text!(
                            "The current file has no related structs of type {}"
                            { std::any::type_name::<T>() }
                        )
                    })
            });

            self.inner.write().try_add(command)
        }

        /// Adds a command that will look for a given widget, and then
        /// mutate it and its area.
        pub fn add_for_widget<W: PassiveWidget>(
            &self,
            callers: impl IntoIterator<Item = impl ToString>,
            mut f: impl FnMut(&mut W, &dyn Area, Flags, Args) -> CmdResult + 'static,
        ) -> Result<()> {
            let widget_getter =
                unsafe { self.widget_getter.read().unwrap().assume_init_ref().clone() };

            let command = Command::new(callers, move |flags, args| {
                CURRENT_FILE
                    .mutate_related_widget::<W, CmdResult>(|widget, area| {
                        f(widget, area, flags, args.clone())
                    })
                    .unwrap_or_else(|| {
                        let widget_getter = widget_getter.read();
                        CURRENT_WIDGET.inspect_data(|widget, _| {
                            let widget = widget.clone().to_passive();
                            if let Some((w, a)) =
                                widget_getter.get_from_name(W::type_name(), &widget)
                            {
                                w.mutate_as::<W, CmdResult>(|w| f(w, a, flags, args))
                                    .unwrap()
                            } else {
                                let name = W::type_name();
                                Err(text!("No widget of type " [AccentErr] name [] " found"))
                            }
                        })
                    })
            });

            self.inner.write().try_add(command)
        }

        /// Adds a [`WidgetGetter`] to [`self`].
        pub fn add_widget_getter<U: Ui>(&self, getter: RwData<Vec<Window<U>>>) {
            let inner_arc = getter.inner_arc().clone() as Arc<RwLock<dyn WidgetGetter>>;
            let getter = RwData::new_unsized::<Window<U>>(inner_arc);
            let mut lock = self.widget_getter.write().unwrap();
            *lock = MaybeUninit::new(getter)
        }

        pub fn try_alias(
            &self,
            alias: impl ToString,
            command: impl ToString,
        ) -> Result<Option<Text>> {
            self.inner.write().try_alias(alias, command)
        }
    }

    struct InnerCommands {
        list: Vec<Command>,
        aliases: HashMap<String, (String, String)>,
    }

    impl InnerCommands {
        /// Tries to add the given [`Command`] to the list.
        fn try_add(&mut self, command: Command) -> Result<()> {
            let mut new_callers = command.callers().iter();

            let commands = self.list.iter();
            for caller in commands.flat_map(|cmd| cmd.callers().iter()) {
                if new_callers.any(|new_caller| new_caller == caller) {
                    return Err(Error::AlreadyExists(caller.clone()));
                }
            }

            self.list.push(command);

            Ok(())
        }

        /// Tries to alias a full command (caller, flags, and
        /// arguments) to an alias.
        fn try_alias(
            &mut self,
            alias: impl ToString,
            command: impl ToString,
        ) -> Result<Option<Text>> {
            let alias = alias.to_string();
            let command = command.to_string();
            let mut command = command.split_whitespace();

            if alias.split_whitespace().count() != 1 {
                return Err(Error::NotSingleWord(alias));
            }
            let caller = command.next().ok_or(Error::Empty)?.to_string();

            let mut callers = self.list.iter().flat_map(|cmd| cmd.callers.iter());

            if callers.any(|name| *name == caller) {
                let args = command.intersperse(" ").collect::<String>();
                match self
                    .aliases
                    .insert(alias.clone(), (caller.clone(), args.clone()))
                {
                    Some((prev_caller, prev_args)) => Ok(Some(text!(
                        "Aliased " [AccentOk] alias []
                        " from " [AccentOk] prev_caller " " prev_args []
                        " to " [AccentOk] caller " " args [] "."
                    ))),
                    None => Ok(Some(text!(
                         "Aliased " [AccentOk] alias []
                         " to " [AccentOk] caller " " args [] "."
                    ))),
                }
            } else {
                Err(Error::CallerNotFound(caller))
            }
        }
    }

    trait WidgetGetter: Send + Sync {
        fn get_from_name(
            &self,
            type_id: &'static str,
            arc: &dyn Data<dyn PassiveWidget>,
        ) -> Option<(&RwData<dyn PassiveWidget>, &dyn Area)>;
    }

    impl<U> WidgetGetter for Vec<Window<U>>
    where
        U: Ui,
    {
        fn get_from_name(
            &self,
            type_name: &'static str,
            widget: &dyn Data<dyn PassiveWidget>,
        ) -> Option<(&RwData<dyn PassiveWidget>, &dyn Area)> {
            let window = self
                .iter()
                .position(|w| w.widgets().any(|(cmp, _)| cmp.ptr_eq(widget)))
                .unwrap();

            let on_window = self[window].widgets();
            let previous = self.iter().take(window).flat_map(|w| w.widgets());
            let following = self.iter().skip(window + 1).flat_map(|w| w.widgets());

            on_window
                .chain(following)
                .chain(previous)
                .find(|(w, _)| w.type_name() == type_name)
                .map(|(w, a)| (w.as_passive(), a as &dyn Area))
        }
    }

    /// Takes the [`Flags`] from an [`Iterator`] of `args`.
    fn split_flags(args: std::str::SplitWhitespace<'_>) -> (InnerFlags<'_>, Args<'_>) {
        let mut short = String::new();
        let mut long = Vec::new();

        let mut args = args.peekable();
        while let Some(arg) = args.peek() {
            if let Some(flag_arg) = arg.strip_prefix("--") {
                if !flag_arg.is_empty() {
                    args.next();
                    if !long.contains(&flag_arg) {
                        long.push(flag_arg)
                    }
                } else {
                    args.next();
                    break;
                }
            } else if let Some(short_arg) = arg.strip_prefix('-') {
                args.next();
                for char in short_arg.chars() {
                    if !short.contains(char) {
                        short.push(char)
                    }
                }
            } else {
                break;
            }
        }

        (InnerFlags { short, long }, Args { count: 0, args })
    }
}
