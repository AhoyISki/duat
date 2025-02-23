//! Creation and execution of commands.
//!
//! Commands on Duat are bits of code that can be executed on the
//! [`CmdLine`] widget. They can also be invoked from other parts of
//! the code, but their use is mostly intended for runtime calls.
//!
//! They are executed asynchronously in order to prevent deadlocks in
//! Duat's internal systems.
//!
//! # Running commands
//!
//! ```rust
//! # use duat::cmd;
//! cmd::run("colorscheme solarized");
//! ```
//!
//! The code above runs the `colorscheme` command. In this case, if
//! the command succeds or fails, no notification will be shown, if
//! you want notifications, you should use [`cmd::run_notify`]:
//!
//! ```rust
//! # use duat::cmd;
//! cmd::run_notify("set-form --flag -abc rgb 255 0 0 hsl 1");
//! ```
//!
//! The `set-form` command above will fail, since the hsl [`Color`]
//! [`Parameter`] was not completely matched, missing the saturation
//! and lightness arguments. It also shows two flag arguments, word
//! flags (`"--flag"`) and blob flags (`"-abc"`).
//!
//! # Adding commands
//!
//! Commands are added through the [`add!`] and [`add_for!`] macros.
//! The first one is used when you just want to interpret some
//! [`Flags`] and [`Args`]. The second one lets you modify [`Widget`]s
//! directly, modifying the most relevant one to the current [`File`].
//!
//! These macros will take two arguments, the first one is a list of
//! callers for that command, e.g. `["quit", "q"]` for the `quit`
//! command. Note that a regular [`&str`] argument is also accepted.
//!
//! The second argument is a _rust-like_ closure with a variable
//! number of arguments. The first argument is always the [`Flags`] of
//! the command. Subsequent arguments are of any type that implements
//! [`Parameter`]. These [`Parameter`] arguments are not "a word
//! each", for example, the [`Color`] parameter can take up to 4
//! arguments to be processed:
//!
//! ```rust
//! # use duat::prelude::{
//! #     ok, form::{self, Form}, cmd::{self, Args, FormName, Flags}
//! # };
//! # use std::sync::{atomic::{AtomicBool, Ordering}, Arc};
//! let callers = ["unset-form", "uf"];
//! // A `Vec<T>` parameter will try to collect all
//! // remaining arguments as `T` in a list.
//! let result = cmd::add!(callers, |_flags, forms: Vec<FormName>| {
//!     for form in forms.iter() {
//!         form::set("form", Form::new());
//!     }
//!     // You can return a success message, but must
//!     // return an error message.
//!     // For those, you should use the `ok!` and `err!`
//!     // macros.
//!     Ok(Some(ok!("Unset " [*a] { forms.len() } [] " forms")))
//! });
//!
//! // Adding a command can fail if a command with the same
//! // name already exists.
//! assert!(result.is_ok());
//! ```
//!
//! In the command above, you'll notice that I used the [`ok!`] macro,
//! which returns [`Ok(Some({Text}))`]. This macro is used when you
//! want to return a notification saying that the command succeeded.
//! Its counterpart is the [`err!`] macro, which just returns a
//! [`Text`], like [`text!`]. This macro represents failure in the
//! execution of the command, and must be used.
//!
//! Here's a simple command that makes use of [`Flags`]:
//!
//! ```rust
//! # use duat::cmd;
//! # use std::sync::{atomic::{AtomicU32, Ordering}, Arc};
//! let expression = Arc::new(AtomicU32::default());
//! let my_command = {
//!     let expression = expression.clone();
//!     cmd::add!("mood", move |flags| {
//!         // `Flags::long` checks for `--` flags
//!         if flags.word("happy") {
//!             expression.store('😁' as u32, Ordering::Relaxed)
//!         // `Flags::short` checks for `-` flags
//!         // They can check for any valid unicode character.
//!         } else if flags.blob("🤯") {
//!             expression.store('🤯' as u32, Ordering::Relaxed)
//!         } else if flags.word("sad") {
//!             expression.store('😢' as u32, Ordering::Relaxed)
//!         } else {
//!             expression.store('😶' as u32, Ordering::Relaxed)
//!         }
//!         Ok(None)
//!     })
//! };
//!
//! cmd::run("mood --sad -🤯");
//! // Passing more arguments than needed results in
//! // an error, so the command is never executed.
//! cmd::run_notify("mood --happy extra args not allowed");
//!
//! // Enough time for no async shenanigans.
//! std::thread::sleep(std::time::Duration::new(1, 0));
//!
//! let num = expression.load(Ordering::Relaxed);
//! assert_eq!(char::from_u32(num), Some('🤯'))
//! ```
//!
//! There are other [`Parameter`]s in [`cmd`] that can be used on a
//! variety of things:
//!
//! ```rust
//! # use duat::prelude::{cmd, err, ok};
//! cmd::add!("pip", |flags, args: cmd::Remainder| {
//!     match std::process::Command::new("pip").spawn() {
//!         Ok(child) => match child.wait_with_output() {
//!             Ok(ok) => Ok(Some(ok!({
//!                 String::from_utf8_lossy(&ok.stdout).into_owned()
//!             }))),
//!             Err(err) => Err(err!(err)),
//!         },
//!         Err(err) => Err(err!(err)),
//!     }
//! });
//! ```
//!
//! The other type of command that Duat supports is one that also acts
//! on a [`Widget`]:
//!
//! ```rust
//! # use duat::{cmd, widgets::{LineNumbers, LineNum}};
//! cmd::add_for!("toggle-relative", |ln: LineNumbers, _, _| {
//!     let mut cfg = ln.get_cfg();
//!     cfg.num_rel = match cfg.num_rel {
//!         LineNum::Abs => LineNum::RelAbs,
//!         LineNum::Rel | LineNum::RelAbs => LineNum::Abs,
//!     };
//!     ln.reconfigure(cfg);
//!     Ok(None)
//! });
//! ```
//!
//! [`CmdLine`]: crate::widgets::CmdLine
//! [`cmd::run_notify`]: run_notify
//! [`Color`]: crate::form::Color
//! [`Widget`]: crate::widgets::Widget
//! [`File`]: crate::widgets::File
//! [`&str`]: str
//! [`cmd`]: self
//! [`ok!`]: crate::prelude::ok
//! [`Ok(Some({Text}))`]: Ok
//! [`err!`]: crate::prelude::err
//! [`Text`]: crate::prelude::Text
//! [`text!`]: crate::prelude::text
//! [`Form`]: crate::form::Form
pub use duat_core::cmd::{
    Args, Between, ColorSchemeArg, F32PercentOfU8, Flags, FormName, Parameter, Remainder, add,
    add_for, run, run_notify,
};

use crate::Ui;

pub type FileBuffer = duat_core::cmd::FileBuffer<Ui>;
pub type OtherFileBuffer = duat_core::cmd::OtherFileBuffer<Ui>;
