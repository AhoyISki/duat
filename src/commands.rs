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
//! # use duat::prelude::commands::{self, Flags, Args};
//! # use std::sync::{atomic::{AtomicBool, Ordering}, Arc};
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
//! # use duat::prelude::commands;
//! commands::run("my-command");
//! ```
//!
//! Here's a simple command that makes use of [`Flags`]:
//!
//! ```rust
//! # use duat::commands;
//! # use std::sync::{atomic::{AtomicU32, Ordering}, Arc};
//! let expression = Arc::new(AtomicU32::default());
//! let callers = ["my-command", "mc"];
//! let my_command = {
//!     let expression = expression.clone();
//!     commands::add(callers, move |flags, _args| {
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
//! # use duat::prelude::{commands, err, ok};
//! commands::add(["copy", "cp"], move |flags, mut args| {
//!     // The `?` is very handy to get a specific number of args.
//!     let source = args.next()?;
//!     // You can also return custom error messages.
//!     let target_1 = args.next_else(err!([*a] "No target given."))?;
//!     // And also parse the arguments.
//!     let target_2: PathBuf = args.next_as()?;
//!
//!     // If you want to error out on too many commands.
//!     args.ended()?;
//!
//!     if flags.word("link") {
//!         unimplemented!("Logic for linking files.");
//!     } else {
//!         unimplemented!("Logic for copying files.");
//!     }
//!
//!     // You can return a success message, but this is optional.
//!     ok!("Copied from " [*a] source [] " to " [*a] target_1 [] ".")
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
//! # use duat::prelude::{commands, ok};
//! commands::add(["write", "w"], move |_flags, mut args| {
//!     let mut count = 0;
//!     while let Ok(arg) = args.next() {
//!         count += 1;
//!         /* Logic for writing to files */
//!     }
//!
//!     ok!("Wrote to " [*a] count [] " files successfully.")
//! });
//! ```
//!
//! [`Form`]: crate::forms::Form

pub use duat_core::commands::{Args, Flags, reset_mode};
use duat_core::{
    commands::{self, CmdResult},
    input::Cursors,
    text::Text,
    widgets::Widget,
};

use crate::{Area, Ui};

/// Runs a full command, with a caller, [`Flags`], and [`Args`].
///
/// When running the command, the ordering of flags does not
/// matter, as long as they are placed before the arguments to the
/// command.
///
/// # Examples
///
/// ```rust
/// # use duat_core::{commands::{self, Result}, text::Text};
/// # fn test() -> Result<Option<Text>> {
/// commands::run("set-prompt new-prompt")
/// # }
/// ```
///
/// In this case we're running a command that will affect the most
/// relevant [`CommandLine`]. See [`commands::add_for`] for
/// more information.
///
/// [`CommandLine`]: crate::widgets::CommandLine
/// [`commands::add_for`]: add_for
#[inline(never)]
pub fn run(call: impl std::fmt::Display) -> Result<Option<Text>> {
    commands::run(call)
}

/// Adds a command to the global list of commands.
///
/// This command cannot take any arguments beyond the [`Flags`]
/// and [`Args`], so any mutation of state must be done
/// through captured variables, usually in the form of
/// [`RwData`]s.
///
/// # Examples
///
/// ```rust
/// # use duat::prelude::{commands, data::RwData, status, StatusLineCfg};
/// # fn test() -> StatusLineCfg {
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
/// status_cfg
/// # }
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

/// Adds a command that can mutate a widget of the given type,
/// along with its associated [`dyn Area`].
///
/// This command will look for the [`Widget`] in the
/// following order:
///
/// 1. Any widget directly attached to the current file.
/// 2. One other instance in the active window.
/// 3. Instances in other windows.
///
/// Keep in mind that this command will always execute on the
/// first widget found.
///
/// This search algorithm allows a more versatile configuration of
/// widgets, for example, one may have a [`CommandLine`] per
/// [`File`], or one singular [`CommandLine`] that acts upon
/// all files in the window, and both would respond correctly
/// to the `"set-prompt"` command.
///
/// # Examples
///
/// In this example, we'll create a simple `Timer` widget:
///
/// ```rust
/// // Required feature for widgets.
/// # use std::{
/// #     sync::{atomic::{AtomicBool, Ordering}, Arc}, marker::PhantomData,
/// #     time::{Duration, Instant}
/// # };
/// # use duat_core::{
/// #     commands, forms::{self, Form}, text::{text, Text, AlignCenter},
/// #     ui::{Area, PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
/// # };
/// struct TimerCfg<U>(PhantomData<U>);
///
/// impl<U: Ui> WidgetCfg<U> for TimerCfg<U> {
///     type Widget = Timer;
///
///     fn build(self, _is_file: bool) -> (Timer, impl Fn() -> bool, PushSpecs) {
///         let widget = Timer {
///             text: text!(AlignCenter [Counter] 0 [] "ms"),
///             instant: Instant::now(),
///             // No need to use an `RwData`, since
///             // `RwData::has_changed` is never called.
///             running: Arc::new(AtomicBool::new(false)),
///         };
///
///         let checker = {
///             let p = duat_core::periodic_checker(Duration::from_millis(100));
///             let running = widget.running.clone();
///             move || p() && running.load(Ordering::Relaxed)
///         };
///
///         let specs = PushSpecs::below().with_ver_len(1.0);
///
///         (widget, checker, specs)
///     }
/// }
///
/// struct Timer {
///     text: Text,
///     instant: Instant,
///     running: Arc<AtomicBool>,
/// }
///
/// impl<U: Ui> Widget<U> for Timer {
///     type Cfg = TimerCfg<U>;
///
///     fn cfg() -> Self::Cfg {
///         TimerCfg(PhantomData)
///     }
///     
///     fn update(&mut self, _area: &U::Area) {
///         if self.running.load(Ordering::Relaxed) {
///             let duration = self.instant.elapsed();
///             let time = format!("{:.3?}", duration);
///             self.text = text!(AlignCenter [Counter] time [] "ms");
///         }
///     }
///     // ...
/// #    fn text(&self) -> &Text {
/// #        &self.text
/// #    }
/// #    fn text_mut(&mut self) -> &mut Text {
/// #        &mut self.text
/// #    }
/// #    fn once() {}
/// }
/// ```
///
/// Next, we'll add three commands for this widget, "`play`",
/// "`pause`" and "`reset`". The best place to add them is in the
/// [`once`] function of [`Widget`]s
///
/// ```rust
/// // Required feature for widgets.
/// # use std::{
/// #     sync::{atomic::{AtomicBool, Ordering}, Arc}, marker::PhantomData,
/// #     time::{Duration, Instant}
/// # };
/// # use duat_core::{
/// #     commands, forms::{self, Form}, text::{text, Text, AlignCenter},
/// #     ui::{Area, PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
/// # };
/// # struct TimerCfg<U>(PhantomData<U>);
/// # impl<U: Ui> WidgetCfg<U> for TimerCfg<U> {
/// #     type Widget = Timer;
/// #     fn build(self, _is_file: bool) -> (Timer, impl Fn() -> bool, PushSpecs) {
/// #         let widget = Timer {
/// #             text: text!(AlignCenter [Counter] 0 [] "ms"),
/// #             instant: Instant::now(),
/// #             running: Arc::new(AtomicBool::new(false)),
/// #         };
/// #         (widget, || false, PushSpecs::below().with_ver_len(1.0))
/// #     }
/// # }
/// # struct Timer {
/// #     text: Text,
/// #     instant: Instant,
/// #     running: Arc<AtomicBool>,
/// # }
/// impl<U: Ui> Widget<U> for Timer {
/// #    type Cfg = TimerCfg<U>;
/// #    fn cfg() -> Self::Cfg {
/// #        TimerCfg(PhantomData)
/// #    }
/// #    fn update(&mut self, _area: &U::Area) {
/// #    }
/// #    fn text(&self) -> &Text {
/// #        &self.text
/// #    }
/// #    fn text_mut(&mut self) -> &mut Text {
/// #        &mut self.text
/// #    }
///     // ...
///     fn once() {
///         forms::set_weak("Counter", Form::green());
///
///         commands::add_for::<Timer, U>(
///             ["play"],
///             |timer, _area, _cursors, _flags, _args| {
///                 timer.running.store(true, Ordering::Relaxed);
///
///                 Ok(None)
///             })
///             .unwrap();
///
///         commands::add_for::<Timer, U>(
///             ["pause"],
///             |timer, _, _, _, _| {
///                 timer.running.store(false, Ordering::Relaxed);
///
///                 Ok(None)
///             })
///             .unwrap();
///
///         commands::add_for::<Timer, U>(
///             ["reset"],
///             |timer, _, _, _, _| {
///                 timer.instant = Instant::now();
///
///                 Ok(None)
///             })
///             .unwrap();
///     }
/// }
/// ```
///
/// I also added a [`Form`] in the [`once`] function. You should
/// use [`forms::set_weak`] instead of [`forms::set`], as to not
/// interfere with the user configuration.
///
/// [`dyn Area`]: duat_core::ui::Area
/// [`File`]: crate::widgets::File
/// [`Session`]: crate::session::Session
/// [`CommandLine`]: crate::widgets::CommandLine
/// [`once`]: Widget::once
/// [`Form`]: crate::forms::Form
/// [`forms::set`]: crate::forms::set
/// [`forms::set_weak`]: duat_core::forms::set_weak
#[inline(never)]
pub fn add_for<W: Widget<Ui>>(
    callers: impl IntoIterator<Item = impl ToString>,
    f: impl FnMut(&mut W, &Area, &mut Cursors, Flags, Args) -> CmdResult + 'static,
) -> Result<()> {
    commands::add_for(callers, f)
}

type Result<T> = duat_core::Result<T, ()>;
