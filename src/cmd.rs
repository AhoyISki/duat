//! Creation and execution of cmd.
//!
//! Commands on Duat are bits of code that can be executed on the
//! [`CmdLine`] widget. They can also be invoked from other parts of
//! the code, but their use is mostly intended for runtime calls.
//!
//! ```rust
//! # use duat::cmd;
//! cmd::run("set-colorscheme solarized");
//! ```
//!
//! ```rust
//! # use duat::prelude::cmd::{self, Flags, Args};
//! # use std::sync::{atomic::{AtomicBool, Ordering}, Arc};
//! // Any of these callers will work for running the command.
//! let callers = ["my-command", "mc"];
//!
//! // cmd::add create a new globally available command.
//! let result = cmd::add!(callers, move |_flags| {
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
//! # use duat::prelude::cmd;
//! cmd::run("my-command");
//! ```
//!
//! Here's a simple command that makes use of [`Flags`]:
//!
//! ```rust
//! # use duat::cmd;
//! # use std::sync::{atomic::{AtomicU32, Ordering}, Arc};
//! let expression = Arc::new(AtomicU32::default());
//! let callers = ["my-command", "mc"];
//! let my_command = {
//!     let expression = expression.clone();
//!     cmd::add!(callers, move |flags| {
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
//! assert!(cmd::run("mc --sad -🤯").is_ok());
//! // Passing more arguments than needed results in an error
//! assert!(cmd::run("mc --happy extra args not allowed").is_err());
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
//! # use duat::prelude::{cmd, err, ok};
//! cmd::add(["copy", "cp"], move |flags, mut args| {
//!     // The `?` is very handy to get a specific number of args.
//!     let source = args.next()?;
//!     // You can also return custom error messages.
//!     let target_1 = args.next_else(err!([*a] "No target given."))?;
//!     // And also parse the arguments.
//!     let target_2: PathBuf = args.next_as()?;
//!
//!     // If you want to error out on too many cmd.
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
//! form: `"CommandOk"`, `"AccentOk"`, `"CommandErr"` and
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
//! # use duat::prelude::{cmd, ok};
//! cmd::add(["write", "w"], move |_flags, mut args| {
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
//! [`CmdLine`]: crate::widgets::CmdLine
//! [`Form`]: crate::form::Form
pub use duat_core::cmd::{
    Args, Between, ColorSchemeArg, F32PercentOfU8, Flags, FormName, Parameter, Remainder, add, run,
};

use crate::Ui;

pub type FileBuffer = duat_core::cmd::FileBuffer<Ui>;
pub type OtherFileBuffer = duat_core::cmd::OtherFileBuffer<Ui>;

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
/// widgets, for example, one may have a [`CmdLine`] per
/// [`File`], or one singular [`CmdLine`] that acts upon
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
/// #     cmd, form::{self, Form}, text::{text, Text, AlignCenter},
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
/// Next, we'll add three cmd for this widget, "`play`",
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
/// #     cmd, form::{self, Form}, text::{text, Text, AlignCenter},
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
///         form::set_weak("Counter", Form::green());
///
///         cmd::add_for::<Timer, U>(
///             ["play"],
///             |timer, _area, _cursors, _flags, _args| {
///                 timer.running.store(true, Ordering::Relaxed);
///
///                 Ok(None)
///             })
///             .unwrap();
///
///         cmd::add_for::<Timer, U>(
///             ["pause"],
///             |timer, _, _, _, _| {
///                 timer.running.store(false, Ordering::Relaxed);
///
///                 Ok(None)
///             })
///             .unwrap();
///
///         cmd::add_for::<Timer, U>(
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
/// use [`form::set_weak`] instead of [`form::set`], as to not
/// interfere with the user configuration.
///
/// [`dyn Area`]: duat_core::ui::Area
/// [`File`]: crate::widgets::File
/// [`Session`]: crate::session::Session
/// [`CmdLine`]: crate::widgets::CmdLine
/// [`once`]: Widget::once
/// [`Form`]: crate::form::Form
/// [`form::set`]: crate::form::set
/// [`form::set_weak`]: duat_core::form::set_weak
pub macro add_for($($tokens:tt)+) {{
    $crate::prelude::duat_core::cmd::add_for!(Ui, $($tokens)+)
}}
