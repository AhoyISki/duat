//! Creation and execution of [`Command`]s.
//!
//! The [`Command`] struct, in Parsec, is function that's supposed to
//! be ran from any part of the program asynchronously, being able to
//! mutate data through reference counting and internal mutability.
//!
//! [`Command`]s must act on 2 specific parameters, [`Flags`], and
//! arguments coming from an [`Iterator<Item = &str>`].
//!
//! The [`Flags`] struct contains both `unit` (e.g `"--force"`,
//! `"--read"`, etc), and `blob` (e.g. `"-abcde"`, `"-amongus"`, etc)
//! flags inside of it, and they stop being considered after the first
//! empty `"--"` sequence, for example, in the command:
//!
//! `"my-command --flag1 --flag2 -blob -- --not-flag more-args"`
//!
//! `"--not-flag"` would not be treated as a flag, being instead
//! treated as an argument in conjunction with `"more-args"`. This
//! interface is heavily inspired by commands in UNIX like operating
//! systems.
//!
//! The [`Command`]'s function will have the type
//! `
//! Box<dyn FnMut(&Flags, &mut dyn Iterator<Item = &str>) ->
//! Result<Option<String>, String>> `
//! which seems complex, but in most cases, a [`Command`] will just be
//! created from a closure like so:
//!
//! ```rust
//! # use parsec_core::commands::{Command, Flags};
//! # use std::sync::{
//! #     atomic::{AtomicBool, Ordering},
//! #     Arc
//! # };
//! #
//! let internally_mutable = Arc::new(AtomicBool::default());
//! let callers = vec!["my-command", "mc"];
//! let my_command = Command::new(
//!     callers,
//!     move |flags: &Flags, args: &mut dyn Iterator<Item = &str>| {
//!         todo!();
//!     },
//! );
//! ```
//!
//! In this case, a [`Command`] is created that can be called with
//! both `"my-command"` and `"mc"`.
//!
//! Here's a simple command that uses the [`Flags`] struct:
//!
//! ```rust
//! # use parsec_core::commands::{Command, Flags};
//! # use std::sync::{
//! #     atomic::{AtomicU32, Ordering},
//! #     Arc
//! # };
//! #
//! let expression = Arc::new(AtomicU32::default());
//! let callers = vec!["my-command", "mc"];
//! let my_command = Command::new(callers, move |flags, _args| {
//!     if flags.unit("happy") {
//!         expression.store('😁' as u32, Ordering::Relaxed)
//!     } else if flags.unit("sad") {
//!         expression.store('😢' as u32, Ordering::Relaxed)
//!     } else {
//!         expression.store('😶' as u32, Ordering::Relaxed)
//!     }
//!     Ok(None)
//! });
//! ```
//!
//! The calling of commands is done through the [`Commands`] struct,
//! shared around with an [`RwData<Commands>`].
//!
//! This struct is found in the [`Session<U>`], either
//! through the `constructor_hook` via [`ModNode::commands`], or
//! via [`Session::commands`], from an existing [`Session<U>`]
//! instance.
//!
//! ```rust
//! # use parsec_core::{
//! #     data::RwData,
//! #     session::Session,
//! #     tags::form::FormPalette,
//! #     text::PrintCfg,
//! #     ui::{Constraint, ModNode, PushSpecs, Ui},
//! #     widgets::CommandLine,
//! #     commands::{Command, Commands}
//! # };
//! # fn test_fn<U>(ui: U, print_cfg: PrintCfg, palette: FormPalette)
//! # where
//! #     U: Ui
//! # {
//! let constructor_hook = move |mod_node: ModNode<U>, file| {
//!     let commands = mod_node.commands();
//!     commands.write().try_exec("lol");
//!
//!     let specs = PushSpecs::above(Constraint::Length(1.0));
//!     mod_node.push(CommandLine::default_fn(), specs);
//! };
//!
//! let session = Session::new(ui, print_cfg, palette, constructor_hook);
//!
//! let my_callers = vec!["lol", "lmao"];
//! let lol_cmd = Command::new(my_callers, |_flags, _args| {
//!     Ok(Some(String::from("😜")))
//! });
//!
//! session.commands().write().try_add(lol_cmd).unwrap();
//! # }
//! ```
//!
//! The [`Commands`] struct, in this snippet, is chronologically first
//! accessed through
//!
//! ```rust
//! # use parsec_core::{commands::Command, session::Session, ui::Ui};
//! # fn test_fn<U>(session: Session<U>, lol_cmd: Command)
//! # where
//! #     U: Ui
//! # {
//! session.commands().write().try_add(lol_cmd);
//! # }
//! ```
//!
//! In this line, the [`Command`] `lol_cmd` is added to the list
//! of commands, with 2 callers, simply returning a `"😜"`, which
//! can be chained to other [`Command`]s or used in some other way.
//!
//! It is then accessed by the `constructor_hook`, where the `"lol"`
//! caller is called upon, executing the `lol_cmd`. Also in the
//! `constructor_hook`, a [`CommandLine<U>`] is pushed below the
//! [`FileWidget<U>`]. The [`Controler<U>`] parameter is
//! then copied internally to the [`CommandLine<U>`], giving it access
//! to the [`Commands`] struct, allowing it to take user input to run
//! commands.
//!
//! Here's an example that makes use of the arguments of a command:
//!
//! ```rust
//! # use parsec_core::{
//! #     commands::{Command, Commands},
//! #     data::{ReadableData, RwData}
//! # };
//! # fn test_fn(commands: &mut Commands) {
//! let my_var = RwData::new(String::new());
//! let my_var_clone = my_var.clone();
//!
//! let write = Command::new(["write", "w"], move |flags, args| {
//!     let mut count = 0;
//!     for file in args {
//!         count += 1;
//!         todo!()
//!         // Logic for writing to the files.
//!     }
//!
//!     Ok(Some(format!("Wrote to {count} files successfully.")))
//! });
//! # }
//! ```
//!
//! [`Controler<U>`]: crate::Controler
//! [`Session<U>`]: crate::session::Session
//! [`Session::commands`]: crate::session::Session::commands
//! [`ModNode<U>`]: crate::ui::ModNode
//! [`ModNode::commands`]: crate::ui::ModNode::commands
//! [`CommandLine<U>`]: crate::widgets::CommandLine
//! [`FileWidget<U>`]: crate::widgets::FileWidget
//! [`Iterator<Item = &str>`]: std::iter::Iterator
#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{collections::HashMap, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use crate::{
    data::{ReadableData, RwData},
    ui::FileId,
};

/// A struct representing flags passed down to [`Command`]s when
/// running them.
///
/// There are 2 types of flag, the `blob` and `unit` flags.
///
/// `blob` flags represent singular characters passed after a single
/// `'-'` character, they can show up in multiple places, and should
/// represent an incremental addition of features to a command.
///
/// `unit` flags are words that come after any `"--"` sequence, and
/// should represent more verbose, but more readable versions of
/// `blob` flags.
///
/// # Examples
///
/// Both `blob` and `unit` flags can only be counted once, no matter
/// how many times they show up:
///
/// ```rust
/// # use parsec_core::commands::{split_flags, Flags};
/// let command = "my-command --foo --bar -abcde --foo --baz -abfgh arg1";
/// let mut command_args = command.split_whitespace().skip(1);
/// let (flags, args) = split_flags(command_args);
///
/// assert!(flags.blob == String::from("abcdefgh"));
/// assert!(flags.units == vec!["foo", "bar", "baz"]);
/// ```
///
/// If you have any arguments that start with `'-'` or `"--"`, but are
/// not supposed to be flags, you can insert an empty `"--"` after the
/// flags, in order to distinguish them.
///
/// ```rust
/// # use parsec_core::commands::{split_flags, Flags};
/// let command =
///     "my-command --foo --bar -abcde -- --not-a-flag -also-not-flags";
/// let mut command_args = command.split_whitespace().skip(1);
/// let (flags, args) = split_flags(command_args);
///
/// assert!(flags.blob == String::from("abcde"));
/// assert!(flags.units == vec!["foo", "bar"]);
/// ```
pub struct Flags<'a> {
    pub blob: String,
    pub units: Vec<&'a str>,
}

impl<'a> Flags<'a> {
    /// Checks if all of the [`char`]s in the `blob` passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::{split_flags, Flags};
    /// let command = "run -abcdefgh -ablk args -wz";
    /// let mut command_args = command.split_whitespace().skip(1);
    /// let (flags, args) = split_flags(command_args);
    ///
    /// assert!(flags.blob("k"));
    /// assert!(!flags.blob("w"));
    /// ```
    pub fn blob(&self, blob: impl AsRef<str>) -> bool {
        let mut all_chars = true;
        for char in blob.as_ref().chars() {
            all_chars &= self.blob.contains(char);
        }
        all_chars
    }

    /// Returns `true` if the `unit` flag was passed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::commands::{split_flags, Flags};
    /// let command = "run --foo --bar args -baz";
    /// let mut command_args = command.split_whitespace().skip(1);
    /// let (flags, args) = split_flags(command_args);
    ///
    /// assert!(flags.unit("foo"));
    /// assert!(!flags.unit("baz"));
    /// ```
    pub fn unit(&self, flag: impl AsRef<str>) -> bool {
        self.units.contains(&flag.as_ref())
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
        self.blob.is_empty() && self.units.is_empty()
    }
}

/// A function to be used on hooks or in the [`CommandLine<U>`].
///
/// The [`Command`] takes in two vectors of [`String`]s, the first
/// represents the `flags` passed on to the [`Command`], while the
/// second is a list of `arguments`.
///
/// # Examples
///
/// When creating a [`Command`], one should prioritize the reduction
/// of "data" that the [`Command`] uses when triggering.
///
/// As an example, let's say you're creating a [`Widget<U>`],
/// as seen on the `struct` below.
///
/// ```rust
/// # use parsec_core::{text::Text, ui::Ui};
/// # use std::sync::{atomic::AtomicBool, Arc};
/// struct MyWidget {
///     text: Text,
///     other_field: String,
///     relevant_field: Arc<AtomicBool>,
/// }
/// ```
///
/// In this case, assuming that you will add the [`Command`]s through
/// the closure, capturing one [`RwData<MyWidget<U>>`]. You should do
/// this:
///
/// ```rust
/// # use parsec_core::{
/// #     commands::{Command, CommandErr, Commands, Flags},
/// #     data::{RwData, ReadableData},
/// #     text::Text,
/// #     ui::Ui
/// # };
/// # use std::sync::{
/// #     atomic::{AtomicBool, Ordering},
/// #     Arc
/// # };
/// #
/// # struct MyWidget {
/// #     text: Text,
/// #     other_field: String,
/// #     relevant_field: Arc<AtomicBool>
/// # }
/// #
/// # fn command_function(
/// #     relevant_field: bool, flags: &Flags,
/// #     args: &mut dyn Iterator<Item = &str>
/// # ) -> Result<Option<String>, String> {
/// #     todo!();
/// # }
/// #
/// fn add_commands(
///     widget: RwData<MyWidget>,
///     commands: &mut Commands,
/// ) -> Result<(), CommandErr> {
///     // Cloning the `relevant_field`, the only important part of
///     // `MyWidget` for the command.
///     let relevant_field = widget.read().relevant_field.clone();
///     let callers = vec!["my-function-caller"];
///     let command = Command::new(callers, move |flags, args| {
///         command_function(
///             relevant_field.load(Ordering::Relaxed),
///             flags,
///             args,
///         )
///     });
///     commands.try_add(command)
/// }
/// ```
///
/// Instead of this:
///
/// ```rust
/// # use parsec_core::{
/// #     commands::{Command, CommandErr, Commands, Flags},
/// #     data::{RwData, ReadableData},
/// #     text::Text,
/// #     ui::Ui
/// # };
/// # use std::sync::{
/// #     atomic::{AtomicBool, Ordering},
/// #     Arc
/// # };
/// #
/// # struct MyWidget {
/// #     text: Text,
/// #     other_field: String,
/// #     relevant_field: Arc<AtomicBool>
/// # }
/// #
/// # fn command_function(
/// #     relevant_field: bool, flags: &Flags,
/// #     args: &mut dyn Iterator<Item = &str>
/// # ) -> Result<Option<String>, String> {
/// #     todo!();
/// # }
/// #
/// fn add_commands(
///     widget: RwData<MyWidget>,
///     commands: &mut Commands,
/// ) -> Result<(), CommandErr> {
///     let callers = vec!["my-function-caller"];
///     let command = Command::new(callers, move |flags, args| {
///         // Here, the whole widget got moved, which is bad, for
///         // thread safety reasons.
///         let relevant_field =
///             widget.read().relevant_field.load(Ordering::Relaxed);
///         command_function(relevant_field, flags, args)
///     });
///     commands.try_add(command)
/// }
/// ```
///
/// In the second version, the whole `widget` variable gets moved into
/// the closure. The problem with this is that this specific
/// [`RwData`] will get used very often, and by
/// running the [`Command`], you may cause a deadlock, which is really
/// annoying to diagnose.
///
/// As an example, there is the
/// [`CommandLine<U>`] widget. If its
/// [`Command`]s moved an entire
/// [`RwData<CommandLine<U>>`] to the
/// closures, every single one of them, when triggered through the
/// widget, would result in a deadlock, since they're writing to an
/// [`RwData`] that was already being written
/// to.
///
/// [`Controler<U>`]: crate::Controler
/// [`RwData<MyWidget<U>>`]: crate::data::RwData
/// [`Session<U>`]: crate::session::Session
/// [`Session::commands`]: crate::session::Session::commands
/// [`ModNode<U>`]: crate::ui::ModNode
/// [`ModNode::commands`]: crate::ui::ModNode::commands
/// [`CommandLine<U>`]: crate::widgets::CommandLine
/// [`FileWidget<U>`]: crate::widgets::FileWidget
/// [`Widget<U>`]: crate::widgets::Widget
pub struct Command {
    /// A closure to trigger when any of the `callers` are called.
    ///
    /// # Arguments
    ///
    /// - 1: A [`&Flags`][Flags] containing the flags that have been passed to
    ///   the function.
    /// - 2: An [`&mut dyn Iterator<Item = &str>`][Iterator] representing the
    ///   arguments to be read by the function.
    ///
    /// # Returns
    ///
    /// A [`Result<Option<String>, String>`]. [`Ok(Some(String))`] is
    /// an outcome that could be used to chain multiple commands.
    /// The [`String`] in [`Err(String)`] is used to tell the user
    /// what went wrong while running the command, and possibly to
    /// show a message somewhere on Parsec.
    f: RwData<dyn FnMut(&Flags, &mut dyn Iterator<Item = &str>) -> Result<Option<String>, String>>,
    /// A list of [`String`]s that act as callers for [`self`].
    callers: Vec<String>,
    file_id: Option<FileId>,
}

impl Command {
    /// Returns a new instance of [`Command`].
    ///
    /// # Arguments
    ///
    /// - `callers`: A list of names to call this function by. In other editors,
    ///   you would see things like `["edit", "e"]`, or `["write-quit", "wq"]`,
    ///   and this is no different.
    /// - `f`: The closure to call when running the command. In order for the
    ///   closure to actually do anything, you would want it to capture shared,
    ///   multi-threaded objects (e.g. [`Arc`]s, [`RwData`]s, e.t.c.).
    /// ```rust
    /// # use parsec_core::{
    /// #     commands::{Command},
    /// #     data::{RwData, ReadableData},
    /// # };
    /// # use std::sync::atomic::AtomicBool;
    /// let my_var = RwData::new(AtomicBool::new(false));
    /// let clone_of_my_var = my_var.clone();
    /// let callers = ["foo", "bar"];
    /// let command = Command::new(callers, move |flags, args| {
    ///     let read_var = my_var.read();
    ///     todo!();
    /// });
    /// ```
    ///
    ///   This way, `my_var` will be accessible, both to the command,
    ///   and to any reader that is interested in its value.
    ///
    /// # Panics
    ///
    /// This function will panic if any of the callers contain more
    /// than one word, as commands are only allowed to be one word
    /// long.
    pub fn new<F>(callers: impl IntoIterator<Item = impl ToString>, f: F) -> Self
    where
        F: FnMut(&Flags, &mut dyn Iterator<Item = &str>) -> Result<Option<String>, String>
            + 'static,
    {
        let callers = callers
            .into_iter()
            .map(|caller| caller.to_string())
            .collect::<Vec<String>>();
        if let Some(caller) = callers
            .iter()
            .find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{caller}\" contains more than one word.");
        }
        Self {
            f: RwData::new_unsized::<F>(Arc::new(RwLock::new(f))),
            callers,
            file_id: None,
        }
    }

    /// Returns a new instance of [`Command`].
    ///
    /// # Arguments
    ///
    /// - `callers`: A list of names to call this function by. In other editors,
    ///   you would see things like `["edit", "e"]`, or `["write-quit", "wq"]`,
    ///   and this is no different.
    /// - `f`: The closure to call when running the command. In order for the
    ///   closure to actually do anything, you would want it to capture shared,
    ///   multi-threaded objects (e.g. [`Arc`]s, [`RwData`]s, e.t.c.).
    /// ```rust
    /// # use parsec_core::{
    /// #     commands::{Command},
    /// #     data::{RwData, ReadableData},
    /// # };
    /// # use std::sync::atomic::AtomicBool;
    /// let my_var = RwData::new(AtomicBool::new(false));
    /// let clone_of_my_var = my_var.clone();
    /// let callers = ["foo", "bar"];
    /// let command = Command::new(callers, move |flags, args| {
    ///     let read_var = my_var.read();
    ///     todo!();
    /// });
    /// ```
    ///
    ///   This way, `my_var` will be accessible, both to the command,
    ///   and to any reader that is interested in its value.
    ///
    /// # Panics
    ///
    /// This function will panic if any of the callers contain more
    /// than one word, as commands are only allowed to be one word
    /// long.
    pub fn new_local<F>(callers: impl IntoIterator<Item = impl ToString>, f: F) -> Self
    where
        F: FnMut(&Flags, &mut dyn Iterator<Item = &str>) -> Result<Option<String>, String>
            + 'static,
    {
        let callers = callers
            .into_iter()
            .map(|caller| caller.to_string())
            .collect::<Vec<String>>();
        if let Some(caller) = callers
            .iter()
            .find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{caller}\" contains more than one word.");
        }
        Self {
            f: RwData::new_unsized::<F>(Arc::new(RwLock::new(f))),
            callers,
            file_id: *crate::CMD_FILE_ID.lock().unwrap(),
        }
    }

    /// Executes the inner function if the `caller` matches any of the
    /// callers in [`self`].
    fn try_exec<'a>(
        &self,
        caller: &str,
        flags: &Flags,
        args: &mut impl Iterator<Item = &'a str>,
    ) -> Result<Option<String>, CommandErr> {
        if self.callers.iter().any(|name| name == caller) {
            (self.f.write())(flags, args).map_err(CommandErr::Failed)
        } else {
            Err(CommandErr::NotFound(String::from(caller)))
        }
    }

    /// The list of callers that will trigger this command.
    fn callers(&self) -> &[String] {
        &self.callers
    }
}

unsafe impl Send for Command {}
unsafe impl Sync for Command {}

/// A list of [`Command`]s, meant to be used in a
/// [`CommandLine<U>`].
///
/// [`Command`]s can be added through the [`Commands::try_add`]
/// function, they can have multiple callers, and return
/// [`Result<Option<String>, String>`]
///
/// ```rust
/// # use parsec_core::{
/// #     commands::{Command, Commands},
/// #     data::RwData
/// # };
/// #
/// # fn test_fn(commands: &mut Commands) {
/// let callers = vec!["run-cmd"];
/// let command = Command::new(callers, |flags, _| {
///     if flags.unit("success") {
///         Ok(Some(String::from("✅")))
///     } else if flags.unit("failure") {
///         Err(String::from("❎"))
///     } else {
///         Ok(None)
///     }
/// });
/// commands.try_add(command).unwrap();
/// # }
/// ```
///
/// This is a [`Command`] that analyzes the [`Flags`] that were
/// passed, checking for the `"success"` and `"failure"` flags.
///
/// By default, one command will always be available, that
/// being the `"alias"` command This is its calling syntax:
///
/// `
/// "alias command-alias command-caller --flag1 --flag2 -blobs --flag1
/// -abc args" `
///
/// This call will alias the `"alias-name"` keyword to the
/// `"aliased-command"` caller. If the caller doesn't exist, the
/// `"alias"` [`Command`] will return an [`Err`].
///
/// When running the `"command-alias"`, this alias will also pass
/// the following [`Flags`]:
///
/// ```rust
/// # use parsec_core::commands::Flags;
/// # fn test_fn<'a>() -> Flags<'a> {
/// Flags {
///     blob: String::from("blosac"),
///     units: vec!["flag1", "flag2"],
/// }
/// # }
/// ```
///
/// And will also pass `"args"` as an argument for the
/// `"command-caller"` command.
///
/// [`CommandLine<U>`]: crate::widgets::CommandLine
/// [`Commands::try_add`]: Commands::try_add
pub struct Commands {
    list: Vec<Command>,
    aliases: RwData<HashMap<String, String>>,
}

impl Commands {
    /// Parses the `command`, dividing it into a caller, flags, and
    /// args, and then tries to run it.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{
    /// #     commands::{Command, Commands},
    /// #     data::{ReadableData, RwData}
    /// # };
    /// # fn test_fn(commands: &mut Commands) {
    /// let my_var = RwData::new(String::new());
    /// let my_var_clone = my_var.clone();
    ///
    /// let command = Command::new(["foo", "bar", "baz"], move |_flags, args| {
    ///     if let Some(arg) = args.next() {
    ///         *my_var_clone.write() = String::from(arg);
    ///     } else {
    ///         *my_var_clone.write() = String::from("😿");
    ///     }
    ///
    ///     Ok(None)
    /// });
    ///
    /// commands.try_add(command).unwrap();
    ///
    /// assert!(&*my_var.read() == "");
    ///
    /// commands.try_exec("foo hello-pardner!");
    /// assert!(&*my_var.read() == "hello-pardner!");
    ///
    /// commands.try_exec("bar --my-flag-here -my-blobs-here arrrgh!");
    /// assert!(&*my_var.read() == "arrrgh!");
    ///
    /// commands.try_exec("baz --args=none-lol");
    /// assert!(&*my_var.read() == "😿");
    /// # }
    /// ```
    pub fn try_exec(&self, command: impl ToString) -> Result<Option<String>, CommandErr> {
        let command = command.to_string();
        let caller = command
            .split_whitespace()
            .next()
            .ok_or(CommandErr::Empty)?
            .to_string();

        let command = self
            .aliases
            .inspect(|aliases| aliases.get(&caller).cloned().unwrap_or(command));
        let mut command = command.split_whitespace();
        let caller = command.next().unwrap();

        let (flags, mut args) = split_flags(command);

        let cur_file_id = *crate::CMD_FILE_ID.lock().unwrap();
        for cmd in &self.list {
            if cmd
                .file_id
                .zip_with(cur_file_id, |rhs, lhs| rhs == lhs)
                .unwrap_or(true)
            {
                let result = cmd.try_exec(caller, &flags, &mut args);
                let Err(CommandErr::NotFound(_)) = result else {
                    return result;
                };
            }
        }

        Err(CommandErr::NotFound(String::from(caller)))
    }

    /// Tries to add a new [`Command`].
    ///
    /// Returns an [`Err`] if any of the callers of the [`Command`]
    /// are already callers for other commands.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{
    /// #     commands::{Command, Commands},
    /// #     data::{ReadableData, RwData}
    /// # };
    /// # fn test_fn(commands: &mut Commands) {
    /// let capitalize = Command::new(["capitalize", "cap"], |_flags, args| {
    ///     Ok(Some(
    ///         args.map(|arg| arg.to_string().to_uppercase())
    ///             .collect::<String>(),
    ///     ))
    /// });
    ///
    /// assert!(commands.try_add(capitalize).is_ok());
    ///
    /// let capitals = Command::new(["capitals", "cap"], |_flags, args| {
    ///     if let Some("USA!!1!1!") = args.next() {
    ///         Ok(Some(String::from("Washington")))
    ///     } else {
    ///         Ok(Some(String::from("idk")))
    ///     }
    /// });
    ///
    /// let result = format!("{:?}", commands.try_add(capitals));
    /// assert!(result == "The caller \"cap\" already exists.");
    /// # }
    /// ```
    pub fn try_add(&mut self, command: Command) -> Result<(), CommandErr> {
        let mut new_callers = command.callers().iter();
        let cur_file_id = *crate::CMD_FILE_ID.lock().unwrap();

        let commands = self.list.iter();
        for (caller, file_id) in
            commands.flat_map(|cmd| cmd.callers().iter().map(|caller| (caller, cmd.file_id)))
        {
            if new_callers.any(|new_caller| new_caller == caller)
                && file_id
                    .zip_with(cur_file_id, |rhs, lhs| rhs == lhs)
                    .unwrap_or(true)
            {
                return Err(CommandErr::AlreadyExists(caller.clone()));
            }
        }

        self.list.push(command);

        Ok(())
    }

    /// Returns a new instance of [`RwData<Commands>`].
    ///
    /// This function is primarily used in the creation of a
    /// [`Controler`], for bookkeeping of the available
    /// [`Command`]s in an accessible place.
    ///
    /// It also has the purpose of preloading [`self`] with the
    /// `"alias"` command, allowing the end user to alias callers
    /// into full commands.
    ///
    /// [`Controler`]: crate::Controler
    /// [`RwData<Commands>`]: crate::data::RwData
    pub(crate) fn new_rw_data() -> RwData<Self> {
        let commands = RwData::new(Commands {
            list: Vec::new(),
            aliases: RwData::new(HashMap::new()),
        });

        let alias = {
            let commands = commands.clone();
            Command::new(vec!["alias"], move |flags, args| {
                if !flags.is_empty() {
                    Err(String::from(
                        "An alias cannot take any flags, try moving them after the command, like \
                         \"alias my-alias my-caller --foo --bar\", instead of \"alias --foo --bar \
                         my-alias my-caller\"",
                    ))
                } else {
                    let alias = args.next().ok_or(String::from("No alias supplied"))?;
                    commands
                        .read()
                        .try_alias(alias, args)
                        .map_err(|err| err.to_string())
                }
            })
        };

        commands.write().try_add(alias).unwrap();

        commands
    }

    /// Tries to alias a `caller` to an existing `command`.
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// exisiting [`Command`].
    fn try_alias<'a>(
        &self,
        alias: impl ToString,
        command: impl IntoIterator<Item = &'a str>,
    ) -> Result<Option<String>, CommandErr> {
        let alias = alias.to_string();
        let mut command = command.into_iter();

        if alias.split_whitespace().count() != 1 {
            return Err(CommandErr::NotSingleWord(alias));
        }
        let caller = String::from(command.next().ok_or(CommandErr::Empty)?);

        let mut callers = self.list.iter().flat_map(|cmd| cmd.callers.iter());

        if callers.any(|name| *name == caller) {
            let mut aliases = self.aliases.write();
            Ok(aliases.insert(alias, caller + command.collect::<String>().as_str()))
        } else {
            Err(CommandErr::NotFound(caller))
        }
    }
}

/// An failure in executing or adding a [`Command`].
#[derive(Debug)]
pub enum CommandErr {
    NotSingleWord(String),
    AlreadyExists(String),
    NotFound(String),
    Failed(String),
    Empty,
}

impl std::fmt::Display for CommandErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandErr::NotSingleWord(caller) => {
                write!(f, "The caller \"{caller}\" is not a single word")
            }
            CommandErr::AlreadyExists(caller) => {
                write!(f, "The caller \"{caller}\" already exists.")
            }
            CommandErr::NotFound(caller) => {
                write!(f, "The caller \"{caller}\" was not found.")
            }
            CommandErr::Failed(failure) => f.write_str(failure),
            CommandErr::Empty => f.write_str("No caller supplied."),
        }
    }
}

impl std::error::Error for CommandErr {}

/// Takes the [`Flags`] from an [`Iterator`] of `args`.
///
/// # Examples
///
/// ```rust
/// # use parsec_core::commands::{split_flags, Flags};
/// let command = "my-command --foo --bar -abcde --foo --baz -abfgh arg1";
/// let mut command_args = command.split_whitespace().skip(1);
/// let (flags, args) = split_flags(command_args);
///
/// assert!(flags.blob == String::from("abcdefgh"));
/// assert!(flags.units == vec!["foo", "bar", "baz"]);
/// ```
///
/// If you have any arguments that start with `'-'` or `"--"`, but are
/// not supposed to be flags, you can insert an empty `"--"` after the
/// flags, in order to distinguish them.
///
/// ```rust
/// # use parsec_core::commands::{split_flags, Flags};
/// let command =
///     "my-command --foo --bar -abcde -- --not-a-flag -also-not-flags";
/// let mut command_args = command.split_whitespace().skip(1);
/// let (flags, args) = split_flags(command_args);
///
/// assert!(flags.blob == String::from("abcde"));
/// assert!(flags.units == vec!["foo", "bar"]);
/// ```
pub fn split_flags<'a>(
    args: impl Iterator<Item = &'a str>,
) -> (Flags<'a>, impl Iterator<Item = &'a str>) {
    let mut blob = String::new();
    let mut units = Vec::new();

    let mut args = args.peekable();
    while let Some(arg) = args.peek() {
        if let Some(flag_arg) = arg.strip_prefix("--") {
            if !flag_arg.is_empty() {
                if !units.contains(&flag_arg) {
                    units.push(flag_arg)
                }
                args.next();
            } else {
                break;
            }
        } else if let Some(blob_arg) = arg.strip_prefix('-') {
            for char in blob_arg.chars() {
                if !blob.contains(char) {
                    blob.push(char)
                }
            }
            args.next();
        } else {
            break;
        }
    }

    (Flags { blob, units }, args)
}
