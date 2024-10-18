//! Creation and execution of commands.
//!
//! Commands in Duat work through the use of functions that don't
//! require references (unless they're `'static`) and return results
//! which may contain a [`Text`] to be displayed, if successful, and
//! *must* contain an error [`Text`] to be displayed if they fail.
//!
//! Commands act on two parameters. which will be provided when ran:
//! [`Flags`] and [`Args`].
//!
//! [`Flags`] will contain a list of all flags that were passed to the
//! command. These flags follow the UNIX conventions, so `"-"` will
//! pass blob flags, `"--"` starts a single, larger flag, and
//! `"--"` followed by nothing means that the remaining arguments are
//! not flags. Here's an example:
//!
//! `"my-command --flag1 --flag2 -short -- --not-flag more-args"`
//!
//! [`Args`] is merely an iterator over the remaining arguments, which
//! are given as `&str`s to be consumed.
//!
//! Here's a simple example of how one would add a command:
//!
//! ```rust
//! # use crate::prelude::commands::{self, Flags, Args};
//! # use std::sync::{
//! #     atomic::{AtomicBool, Ordering},
//! #     Arc
//! # };
//! #
//! // Any of these callers will work for running the command.
//! let callers = ["my-command", "mc"];
//!
//! // commands::add create a new globally avaliable command.
//! let result = commands::add(callers, move |_flags, _args| {
//!     unimplemented!();
//! });
//!
//! // Adding a command can fail if a command with the same
//! // name already exists.
//! assert!(result.is_ok());
//! ```
//!
//! To run that command, simply do the following:
//!
//! ```rust
//! # use crate::prelude::commands;
//! commands::run("my-command");
//! ```
//!
//! Here's a simple command that makes use of [`Flags`]:
//!
//! ```rust
//! # use duat_core::commands;
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
//! // flags and args are included within the string.
//! commands::run("mc --sad -🤯 unused-1 unused-2").unwrap();
//!
//! let num = expression.load(Ordering::Relaxed);
//! assert_eq!(char::from_u32(num), Some('🤯'))
//! ```
//!
//! Here's an example that makes use of the arguments passed to the
//! command.
//!
//! ```rust
//! # use std::path::PathBuf;
//! # use crate::prelude::{commands, text};
//! commands::add(["copy", "cp"], move |flags, mut args| {
//!     // The `?` is very handy to get a specific number of args.
//!     let source = args.next()?;
//!     // You can also return custom error messages.
//!     let target_1 = args.next_else(text!([CommandErr] "No target given."))?;
//!     // And also parse the arguments.
//!     let target_2 = args.next_as::<PathBuf>()?;
//!
//!     // If you want to error out on too many commands.
//!     args.ended()?;
//!
//!     if flags.long("link") {
//!         unimplemented!("Logic for linking files.");
//!     } else {
//!         unimplemented!("Logic for copying files.");
//!     }
//!
//!     // You can return a success message, but this is optional.
//!     Ok(Some(text!(
//!         "Copied from " [AccentOk] source []
//!         " to " [AccentOk] target_1 [] "."
//!     )))
//! });
//! ```
//!
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
//! # use crate::prelude::{commands, text};
//! commands::add(["write", "w"], move |_flags, mut args| {
//!     let mut count = 0;
//!     while let Ok(arg) = args.next() {
//!         count += 1;
//!         /* Logic for writing to files */
//!     }
//!
//!     Ok(Some(text!(
//!         "Wrote to " [AccentOk] count [] " files successfully."
//!     )))
//! });
//! ```
//!
//! [`Form`]: crate::forms::Form

pub use duat_core::commands::{Args, Flags};
use duat_core::{
    commands::{self, CmdResult},
    data::RwData,
    text::Text,
    ui,
    widgets::{CommandLineMode, PassiveWidget},
};

use crate::{Ui, setup::CONTEXT};

/// Runs a full command, with a caller, [`Flags`], and [`Args`].
///
/// When running the command, the ordering of flags does not
/// matter, as long as they are placed before the arguments to the
/// command.
///
/// # Examples
///
/// ```rust
/// # use duat_core::{
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
/// [`commands::add_for_widget`]: add_for_widget
#[inline(never)]
pub fn run(call: impl std::fmt::Display) -> Result<Option<Text>> {
    commands::run(call)
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
/// # use duat_core::{
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
/// [`StatusLine`]. Note that the use of an [`RwData`]/
/// [`RoData`] means that the [`StatusLine`] will
/// be updated automatically, whenever the command is ran.
///
/// [`StatusLineCfg`]: crate::widgets::StatusLineCfg
/// [`StatusLine`]: crate::widgets::StatusLine
/// [`RwData`]: duat_core::data::RwData
/// [`RoData`]: duat_core::data::RoData
#[inline(never)]
pub fn add(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    commands::add(callers, f)
}

/// Adds a command to an object "related" to the current [`File`]
///
/// This object can be one of three things, a [`PassiveWidget`],
/// an [`InputMethod`], or the [`File`]. When the
/// command is ran, Duat will look at the currently active
/// file for any instance of an [`RwData`] it can find.
///
/// # Examples
///
/// ```rust
/// # use duat_core::{
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
///     /* Implementation details. */
/// # type Widget = File
/// # where
/// #     Self: Sized;
///
/// # fn send_key(
/// #     &mut self,
/// #     key: crossterm::event::KeyEvent,
/// #     widget: &RwData<Self::Widget>,
/// #     area: &impl duat_core::ui::Area,
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
///
/// [`File`]: crate::widgets::File
/// [`InputMethod`]: crate::input::InputMethod
/// [`Session`]: crate::session::Session
/// [`RwData`]: crate::prelude::data::RwData
#[inline(never)]
pub fn add_for_current<T: 'static>(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(&RwData<T>, Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    commands::add_for_current::<T, Ui>(callers, f)
}

/// Adds a command that can mutate a widget of the given type,
/// along with its associated [`dyn Area`].
///
/// This command will look for the [`PassiveWidget`] in the
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
/// This ordering means that, wether your [`CommandLine`] is
/// attached to a [`File`], or to edges of the window, it will
/// respond to `"set-prompt"`.
///
/// Note that the search will only look for the first instance of
/// said widget to be found.
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
/// # use duat_core::{
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
///         // The checker determines when updates happen.
///         let checker = {
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
///         if COMMANDS.running.load(Ordering::Relaxed) {
///             let duration = COMMANDS.instant.elapsed();
///             let duration = format!("{:.3?}", duration);
///             COMMANDS.text = text!(
///                 AlignCenter [Counter] duration [] "elapsed"
///             );
///         }
///     }
///     
///
///     fn text() -> &Text {
///         &COMMANDS.text
///     }
///
///     // The `once` function of a `PassiveWidget` is only called
///     // when that widget is first created.
///     // It is useful to add commands and set forms.
///     fn once() {
///         // "weak" forms are only created if they don't exist.
///         // If writing a plugin, always use weak forms.
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
///             ["reset"],
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
/// [`Session`]: duat_core::session::Session
/// [`dyn Area`]: duat_core::ui::Area
/// [`File`]: crate::widgets::File
/// [`CommandLine`]: crate::widgets::CommandLine
#[inline(never)]
pub fn add_for_widget<W: PassiveWidget<Ui>>(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(&RwData<W>, &<Ui as ui::Ui>::Area, Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    commands::add_for_widget::<W, Ui>(callers, f)
}

/// Sets the mode of the [`CommandLine`]
///
/// [`CommandLine`]: crate::widgets::CommandLine
pub fn set_mode<M: CommandLineMode<Ui>>() {
    CONTEXT.set_cmd_mode::<M>()
}

type Result<T> = duat_core::Result<T, ()>;
