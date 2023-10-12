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
//!         builder.push_cfg(status);
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
//! commands::add(["copy", "cp"], move |flags, mut args| {
//!     // If there is a next argument, next will return `Ok(arg)`.
//!     // If there isn't it will return `Err(Text)`.
//!     // If you needed an argument but got none, you should
//!     // return the given error with the `?` operator.
//!     let source = args.next()?;
//!     // You can return custom error messages in order to improve
//!     // the feedback of failures when running the command.
//!     let target_1 = args.next_else(text!("No target given."))?;
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

pub use self::inner::split_flags_and_args;
use self::inner::{Commands, InnerFlags};
use crate::{
    data::RwData,
    text::{text, Text},
    ui::{Area, Ui, Window},
    widgets::{ActiveWidget, PassiveWidget},
    BREAK_LOOP, SHOULD_QUIT,
};

mod inner;

static COMMANDS: Commands = Commands::new();

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
///     move |_flags, mut args| {
///         // You can easily parse arguments, and an appropriate
///         // error will be returned if the parsing fails.
///         let value: usize = args.next_as()?;
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
///     |modal, flags, mut args| {
///         let mode = args.next_else(text!("No mode given"))?;
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

impl std::error::Error for Error {}

pub type CmdResult = std::result::Result<Option<Text>, Text>;

/// The standard result for [`commands`] operations.
///
/// [`commands`]: super
pub type Result<T> = std::result::Result<T, Error>;

/// The non flag arguments that were passed to the caller.
///
/// The first argument not prefixed with a "`-`" or a "`--`" will turn
/// all remaining arguments into non flag arguments, even if they have
/// those prefixes.
///
/// # Examples
///
/// ```rust
/// # use parsec_core::commands::{split_flags_and_args};
/// let call = "command --foo -bar notflag --foo --baz -abfgh";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("bar"));
/// assert!(flags.long("foo"));
/// assert_eq!(args.collect::<Vec<&str>>(), vec![
///     "notflag", "--foo", "--baz", "-abfgh"
/// ]);
/// ```
///
/// You can also make that happen by introducing an empty "`--`"
/// argument:
///
/// ```rust
/// # use parsec_core::commands::{split_flags_and_args};
/// let call = "command --foo -bar -- --foo --baz -abfgh";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("bar"));
/// assert!(flags.long("foo"));
/// assert_eq!(args.collect::<Vec<&str>>(), vec![
///     "--foo", "--baz", "-abfgh"
/// ]);
/// ```
#[derive(Clone)]
pub struct Args<'a> {
    count: usize,
    expected: Option<usize>,
    args: Peekable<SplitWhitespace<'a>>,
}

impl<'a> Args<'a> {
    /// Returns the next argument, if there is one.
    ///
    /// Since this method is supposed to be used inside of a command,
    /// it returns an error that can easily be returned by `?`,
    /// exiting the function with an appropriate error message,
    /// formated to be shown somewhere in the editor.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "run away i'll kill you 👹";
    /// let (flags, mut args) = split_flags_and_args(call);
    /// args.next();
    /// args.next();
    /// args.next();
    /// args.next();
    ///
    /// let ogre = args.next();
    /// assert_eq!(ogre, Ok("👹"));
    ///
    /// let error = args.next();
    /// let error_msg = text!(
    ///     "Expected at least " [AccentErr] 6 []
    ///     " arguments, got " [AccentErr] 5 [] "."
    /// );
    /// assert_eq!(error, Err(error_msg));
    /// ```
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err({
                let expected = match self.expected {
                    Some(expected) => text!([AccentErr] expected),
                    None => text!("at least " [AccentErr] { self.count + 1 }),
                };
                let (args, received) = match self.count {
                    0 => (" arguments", text!([AccentErr] "none")),
                    1 => (" argument", text!([AccentErr] 1)),
                    count => (" arguments", text!([AccentErr] count)),
                };

                text!("Expected " expected [] args ", got " received [] ".")
            }),
        }
    }

    /// Attempts to parse the next argument, if there is one.
    ///
    /// This method will return an [`Err`] in two different ways,
    /// either there is no next argument, in which it defers to the
    /// error message of [`Args::next`], or the parsing fails, then it
    /// returns a custom built error message for that type.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "int-and-float 42 non-float-arg";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let int = args.next_as::<usize>();
    /// assert_eq!(int, Ok(42));
    ///
    /// let float = args.next_as::<f32>();
    /// let error_msg = text!(
    ///     "Couldn't convert " [AccentErr] "non-float-arg" []
    ///     " to " [AccentErr] "f32" [] "."
    /// );
    /// assert_eq!(float, Err(error_msg));
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn next_as<F: FromStr>(&mut self) -> std::result::Result<F, Text> {
        let arg = self.next()?;
        arg.parse().map_err(|_| {
            text!(
                "Couldn't convert " [AccentErr] arg []
                " to " [AccentErr] { std::any::type_name::<F>() } [] "."
            )
        })
    }

    /// Returns the next argument, if there is one, otherwise, returns
    /// a custom error message.
    ///
    /// This method will replace the usual "not enough arguments"
    /// error message from the [`Args::next`] method by a [`Text`]
    /// provided by the user itself, usually with the [`text`] macro.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "expects-2-and-file arg-1 not-quite";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let first = args.next();
    /// assert_eq!(first, Ok("arg-1"));
    ///
    /// let second = args.next();
    /// assert_eq!(second, Ok("not-quite"));
    ///
    /// let msg = text!("I expected a " [Wack] "file" [] ", damnit!");
    /// let float = args.next_else(msg.clone());
    /// assert_eq!(float, Err(msg));
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn next_else(&mut self, text: Text) -> std::result::Result<&str, Text> {
        match self.args.next() {
            Some(arg) => {
                self.count += 1;
                Ok(arg)
            }
            None => Err(text),
        }
    }

    /// Optional function to return an error message in case there are
    /// more arguments than expected.
    ///
    /// This is an optional function, in case you want to complain if
    /// the user passes too many arguments. Of course, you could just
    /// ignore them.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "just-1-arg,man arg-1 too-many wayy tooo many";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let first = args.next();
    /// assert_eq!(first, Ok("arg-1"));
    ///
    /// let error = args.ended();
    /// let msg = text!(
    ///     "Expected " [AccentErr] 1 []
    ///     " argument, received " [AccentErr] 5 [] " instead."
    /// );
    /// assert_eq!(error, Err(msg));
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn ended(&mut self) -> std::result::Result<(), Text> {
        match self.args.clone().count() {
            0 => Ok(()),
            count => Err({
                let args = match self.count == 1 {
                    true => " argument",
                    false => " arguments",
                };
                text!(
                    "Expected " [AccentErr] { self.count } [] args
                    ", received " [AccentErr] { self.count + count } [] " instead."
                )
            }),
        }
    }

    /// Collects the remaining arguments.
    ///
    /// This is similar to any [`Iterator::collect`], but it will
    /// collect differently depending on what struct is being used.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "runner arg1 arg2 arg3 arg4";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// let vector: Vec<&str> = args.clone().collect();
    /// assert_eq!(vector, vec!["arg1", "arg2", "arg3", "arg4"]);
    ///
    /// // In strings, the arguments are joined by a " ".
    /// let string: String = args.collect();
    /// assert_eq!(&string, "arg1 arg2 arg3 arg4");
    /// ```
    ///
    /// [`Args::next`]: Args::next
    pub fn collect<B: FromIterator<&'a str> + 'static>(&mut self) -> B {
        let args: Vec<&str> = (&mut self.args).collect();

        if TypeId::of::<B>() == TypeId::of::<String>() {
            B::from_iter(args.into_iter().intersperse(" "))
        } else {
            B::from_iter(args)
        }
    }

    /// Sets an expected value for the number of arguments.
    ///
    /// This will change the default [`Args::next`] error message, so
    /// that it shows how many arguments were actually expected.
    ///
    /// The reason why this method is here, instead of the command's
    /// creator being able to set a specified number of arguments per
    /// command when creating the given command, is because the number
    /// of arguments to any given command may vary, depending on the
    /// specifics of said command's implementation.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{commands::{split_flags_and_args}, text::text};
    /// let call = "expects-5 arg1 arg2 ";
    /// let (flags, mut args) = split_flags_and_args(call);
    /// args.set_expected(5);
    /// args.next();
    /// args.next();
    ///
    /// let error = args.next();
    /// let error_msg = text!(
    ///     "Expected " [AccentErr] 5 []
    ///     " arguments, got " [AccentErr] 2 [] "."
    /// );
    /// assert_eq!(error, Err(error_msg));
    /// ```
    pub fn set_expected(&mut self, expected: usize) {
        self.expected = Some(expected);
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
/// # use parsec_core::commands::{split_flags_and_args};
/// let call = "my-command --foo -abcde --foo --bar -abfgh arg1";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("abcdefgh"));
/// assert!(flags.long("foo") && flags.long("bar"));
/// assert_eq!(args.collect::<Vec<&str>>(), vec!["arg1"]);
/// ```
///
/// If you have any arguments that start with `'-'` or `"--"`, but
/// are not supposed to be flags, you can insert an empty
/// `"--"` after the flags, in order to distinguish them.
///
/// ```rust
/// # use parsec_core::commands::{split_flags_and_args};
/// let call = "command --foo --bar -abcde -- --!flag -also-not";
/// let (flags, mut args) = split_flags_and_args(call);
///
/// assert!(flags.short("abcde"));
/// assert!(flags.long("foo") && flags.long("bar"));
/// assert_eq!(args.collect::<String>(), "--!flag -also-not")
/// ```
#[derive(Clone, Copy)]
pub struct Flags<'a, 'b>(&'a InnerFlags<'b>);

impl<'a, 'b> Flags<'a, 'b> {
    /// Checks if all of the [`char`]s in the `short` passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::split_flags_and_args;
    /// let call = "run -abcdefgh -ablk args -wz";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// assert!(flags.short("k"));
    /// assert!(!flags.short("w"));
    /// assert_eq!(args.collect::<Vec<&str>>(), vec!["args", "-wz"]);
    /// ```
    pub fn short(&self, short: impl AsRef<str>) -> bool {
        self.0.short(short)
    }

    /// Returns `true` if the `long` flag was passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::split_flags_and_args;
    /// let call = "run --foo --bar args --baz";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// assert!(flags.long("foo"));
    /// assert!(!flags.long("baz"));
    /// assert_eq!(&args.collect::<String>(), "args --baz");
    /// ```
    pub fn long(&self, flag: impl AsRef<str>) -> bool {
        self.0.long(flag)
    }

    /// Returns `true` if no flags have been passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::split_flags_and_args;
    /// let call = "run arg1 --foo --bar arg2 -baz";
    /// let (flags, mut args) = split_flags_and_args(call);
    ///
    /// assert!(flags.is_empty());
    /// assert_eq!(args.collect::<Vec<&str>>(), vec![
    ///     "arg1", "--foo", "--bar", "arg2", "-baz"
    /// ]);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// An error relating to commands in general.
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

/// Adds a widget getter to the globally accessible [`Commands`].
pub(crate) fn add_widget_getter<U: Ui>(getter: RwData<Vec<Window<U>>>) {
    COMMANDS.add_widget_getter(getter);
}
