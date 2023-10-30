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
//! # use duat_core::commands::{self, Flags, Args};
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
//! # use duat_core::{
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
//! will be used to start Duat. in it, we're changing the
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
//! # use duat_core::{commands, text::text};
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
//!     // `duat_core::text::text` macro to facilitate the
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
//! # use duat_core::{commands, text::text};
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
use std::{
    collections::HashMap,
    fmt::Display,
    mem::MaybeUninit,
    sync::{Arc, LazyLock, RwLock},
};

pub use self::parameters::{split_flags_and_args, Args, Flags};
use crate::{
    data::{CurrentFile, CurrentWidget, Data, RwData},
    text::{text, Text},
    ui::{Ui, Window},
    widgets::{ActiveWidget, PassiveWidget},
    BreakReason, BREAK,
};

mod parameters;

/// A list of [`Command`]s.
pub struct Commands<U>
where
    U: Ui,
{
    inner: LazyLock<RwData<InnerCommands>>,
    windows: RwLock<MaybeUninit<RwData<Vec<Window<U>>>>>,
    current_file: &'static CurrentFile<U>,
    current_widget: &'static CurrentWidget<U>,
}

impl<U> Commands<U>
where
    U: Ui,
{
    /// Returns a new instance of [`Commands`].
    pub const fn new(
        current_file: &'static CurrentFile<U>,
        current_widget: &'static CurrentWidget<U>,
    ) -> Self {
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
                                 command, like \"alias my-alias my-caller --foo --bar\", instead \
                                 of \"alias --foo --bar my-alias my-caller\""
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

                let quit = Command::new(["quit", "q"], move |_, _| {
                    BREAK.store(BreakReason::ToQuitDuat);
                    Ok(None)
                });

                inner.write().try_add(quit).unwrap();
                inner.write().try_add(alias).unwrap();

                inner
            }),
            windows: RwLock::new(MaybeUninit::uninit()),
            current_file,
            current_widget,
        }
    }

    /// Canonical way to quit Duat.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Duat will execute a program closing
    /// function, as defined by the [`Ui`].
    pub fn quit(&self) {
        self.run("quit").unwrap();
    }

    /// Switches to the given [`ActiveWidget`].
    ///
    /// The widget will be chosen in the following order:
    ///
    /// 1. Any instance that is "related" to the currently active
    ///    [`File`], that is, any widgets that were added during the
    ///    [`Session`]'s "`file_fn`".
    /// 2. Other widgets in the currently active window, related or
    ///    not to any given [`File`].
    /// 3. Any instance of the [`PassiveWidget`] that is found in
    ///    other windows, looking first at windows ahead.
    ///
    /// [`File`]: crate::widgets::File
    /// [`Session`]: crate::session::Session
    pub fn switch_to<W: ActiveWidget<U>>(&self) -> Result<Option<Text>> {
        self.run(format!("switch-to {}", W::name()))
    }

    /// Switches to/opens a [`File`] with the given name.
    ///
    /// If you wish to specifically switch to files that are already
    /// open, use [`commands::buffer`].
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::buffer`]: crate::commands::buffer
    pub fn edit(&self, file: impl Display) -> Result<Option<Text>> {
        self.run(format!("edit {}", file))
    }

    /// Switches to a [`File`] with the given name.
    ///
    /// If there is no file open with that name, does nothing. Use
    /// [`commands::edit`] if you wish to open files.
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::edit`]: crate::commands::edit
    pub fn buffer(&self, file: impl Display) -> Result<Option<Text>> {
        self.run(format!("buffer {}", file))
    }

    /// Switches to the next [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`commands::next_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::next_global_file`]: crate::commands::next_global_file
    pub fn next_file(&self) -> Result<Option<Text>> {
        self.run("next-file")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`commands::prev_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::prev_global_file`]: crate::commands::prev_global_file
    pub fn prev_file(&self) -> Result<Option<Text>> {
        self.run("prev-file")
    }

    /// Switches to the next [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`commands::next_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::next_file`]: crate::commands::next_file
    pub fn next_global_file(&self) -> Result<Option<Text>> {
        self.run("next-file --global")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`commands::prev_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::prev_file`]: crate::commands::prev_file
    pub fn prev_global_file(&self) -> Result<Option<Text>> {
        self.run("prev-file --global")
    }

    /// If not in a [`File`], switches to the last active [`File`].
    ///
    /// This is useful if the currently active widget is not a file
    /// (e.g. [`CommandLine`], a file tree, etc), and you want to
    /// return to the file seamlessly.
    ///
    /// [`File`]: crate::widgets::File
    /// [`CommandLine`]: crate::widgets::CommandLine
    pub fn return_to_file(&self) -> Result<Option<Text>> {
        self.run("return-to-file")
    }

    /// Tries to alias a `caller` to an existing `command`.
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// exisiting [`Command`].
    pub fn alias(&self, alias: impl ToString, command: impl ToString) -> Result<Option<Text>> {
        self.inner.write().try_alias(alias, command)
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
    /// [`commands::add_for_widget`]: crate::commands::add_for_widget
    pub fn run(&self, call: impl Display) -> Result<Option<Text>> {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(Error::Empty)?.to_string();

        let (command, call) = self.inner.inspect(|inner| {
            if let Some(command) = inner.aliases.get(&caller) {
                let (command, call) = command;
                let mut call = call.clone() + " ";
                call.extend(args);

                Ok((command.clone(), call))
            } else {
                let command = inner
                    .list
                    .iter()
                    .find(|cmd| cmd.callers().contains(&caller))
                    .ok_or(Error::CallerNotFound(caller))?;

                Ok((command.clone(), call.clone()))
            }
        })?;

        let (flags, args) = split_flags_and_args(&call);

        Ok(command.try_exec(Flags::new(&flags), args).unwrap())
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
    /// [`StatusLine`]. Note that the use of an [`RwData<usize>`]/
    /// [`RoData<usize>`] means that the [`StatusLine`] will
    /// be updated automatically, whenever the command is ran.
    ///
    /// [`StatusLineCfg`]: crate::widgets::StatusLineCfg
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`RoData<usize>`]: crate::data::RoData
    pub fn add(
        &self,
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let command = Command::new(callers, f);
        self.inner.write().try_add(command)
    }

    /// Adds a command to an object "related" to the current [`File`]
    ///
    /// This object can be one of three things, a [`PassiveWidget`],
    /// an [`InputMethod`], or the [`File`] itself. When the
    /// command is ran, Duat will look at the currently active
    /// file for any instance of an [`RwData<Thing>`] it can find.
    /// Those will either be the file itself, or will be added in
    /// the [`Session`]'s "file_fn".
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
    ///     // Implementation details.
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
    pub fn add_for_current<T: 'static>(
        &'static self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&mut T, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let command = Command::new(callers, move |flags, args| {
            let result = self
                .current_file
                .mutate_related::<T, CmdResult>(|t| f(t, flags, args.clone()));

            result
                .or_else(|| {
                    self.current_widget
                        .mutate_as::<T, CmdResult>(|t| f(t, flags, args.clone()))
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

    /// Adds a command that can mutate a widget of the given type,
    /// along with its associated [`dyn Area`].
    ///
    /// This command will look for the [`PassiveWidget`] in the
    /// following order:
    ///
    /// 1. Any instance that is "related" to the currently active
    ///    [`File`], that is, any widgets that were added during the
    ///    [`Session`]'s "`file_fn`".
    /// 2. Other widgets in the currently active window, related or
    ///    not to any given [`File`].
    /// 3. Any instance of the [`PassiveWidget`] that is found in
    ///    other windows, looking first at windows ahead.
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
    pub fn add_for_widget<W: PassiveWidget<U>>(
        &'static self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&mut W, &U::Area, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let windows = unsafe { self.windows.read().unwrap().assume_init_ref().clone() };

        let command = Command::new(callers, move |flags, args| {
            self.current_file
                .mutate_related_widget::<W, CmdResult>(|widget, area| {
                    f(widget, area, flags, args.clone())
                })
                .unwrap_or_else(|| {
                    let windows = windows.read();
                    self.current_widget.inspect_data(|widget, _, _| {
                        let widget = widget.clone().to_passive();
                        if let Some((w, a)) = get_from_name(&windows, W::name(), &widget) {
                            w.mutate_as::<W, CmdResult>(|w| f(w, a, flags, args))
                                .unwrap()
                        } else {
                            let name = W::name();
                            Err(text!("No widget of type " [AccentErr] name [] " found"))
                        }
                    })
                })
        });

        self.inner.write().try_add(command)
    }

    /// Adds a [`WidgetGetter`] to [`self`].
    pub(crate) fn add_windows(&self, windows: RwData<Vec<Window<U>>>) {
        let mut lock = self.windows.write().unwrap();
        *lock = MaybeUninit::new(windows)
    }
}

/// The standard error that should be returned when [`run`]ning
/// commands.
///
/// This error _must_ include an error message in case of failure. It
/// may also include a success message, but that is not required.
pub type CmdResult = std::result::Result<Option<Text>, Text>;

/// An error relating to commands in general.
#[derive(Debug)]
pub enum Error {
    /// An alias wasn't just a single word.
    AliasNotSingleWord(String),
    /// The caller for a command already pertains to another.
    CallerAlreadyExists(String),
    /// No commands have the given caller as one of their own.
    CallerNotFound(String),
    /// The command failed internally.
    CommandFailed(Text),
    /// There was no caller and no arguments.
    Empty,
}

impl Error {
    /// Turns the [`Error`] into formatted [`Text`]
    fn into_text(self) -> Text {
        match self {
            Error::AliasNotSingleWord(caller) => text!(
                "The caller " [AccentErr] caller [] "is not a single word."
            ),
            Error::CallerAlreadyExists(caller) => text!(
                "The caller " [AccentErr] caller [] "already exists."
            ),
            Error::CallerNotFound(caller) => text!(
                "The caller " [AccentErr] caller [] "was not found."
            ),
            Error::CommandFailed(failure) => failure,
            Error::Empty => text!("The command is empty."),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::AliasNotSingleWord(caller) => {
                write!(f, "The caller \"{caller}\" is not a single word")
            }
            Error::CallerAlreadyExists(caller) => {
                write!(f, "The caller \"{caller}\" already exists.")
            }
            Error::CallerNotFound(caller) => {
                write!(f, "The caller \"{caller}\" was not found.")
            }
            Error::CommandFailed(failure) => f.write_str(&failure.chars().collect::<String>()),
            Error::Empty => f.write_str("The command is empty"),
        }
    }
}

impl std::error::Error for Error {}

/// The standard result for [`commands`] operations.
///
/// [`commands`]: super
pub type Result<T> = std::result::Result<T, Error>;

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
        (self.f.write())(flags, args).map_err(Error::CommandFailed)
    }

    /// The list of callers that will trigger this command.
    fn callers(&self) -> &[String] {
        &self.callers
    }
}

unsafe impl Send for Command {}
unsafe impl Sync for Command {}
struct InnerCommands {
    list: Vec<Command>,
    aliases: HashMap<String, (Command, String)>,
}

impl InnerCommands {
    /// Tries to add the given [`Command`] to the list.
    fn try_add(&mut self, command: Command) -> Result<()> {
        let mut new_callers = command.callers().iter();

        let commands = self.list.iter();
        for caller in commands.flat_map(|cmd| cmd.callers().iter()) {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(Error::CallerAlreadyExists(caller.clone()));
            }
        }

        self.list.push(command);

        Ok(())
    }

    /// Tries to alias a full command (caller, flags, and
    /// arguments) to an alias.
    fn try_alias(&mut self, alias: impl ToString, call: impl ToString) -> Result<Option<Text>> {
        let alias = alias.to_string();
        if alias.split_whitespace().count() != 1 {
            return Err(Error::AliasNotSingleWord(alias));
        }

        let call = call.to_string();
        let caller = call
            .split_whitespace()
            .next()
            .ok_or(Error::Empty)?
            .to_string();

        let mut cmds = self.list.iter();

        if let Some(command) = cmds.find(|cmd| cmd.callers().contains(&caller)) {
            let entry = (command.clone(), call.clone());
            match self.aliases.insert(alias.clone(), entry) {
                Some((_, prev_call)) => Ok(Some(text!(
                    "Aliased " [AccentOk] alias []
                    " from " [AccentOk] prev_call []
                    " to " [AccentOk] call [] "."
                ))),
                None => Ok(Some(text!(
                     "Aliased " [AccentOk] alias []
                     " to " [AccentOk] call [] "."
                ))),
            }
        } else {
            Err(Error::CallerNotFound(caller))
        }
    }
}

fn get_from_name<'a, U: Ui>(
    windows: &'a [Window<U>],
    type_name: &'static str,
    widget: &impl Data<dyn PassiveWidget<U>>,
) -> Option<(&'a RwData<dyn PassiveWidget<U>>, &'a U::Area)> {
    let window = windows
        .iter()
        .position(|w| w.widgets().any(|(cmp, _)| cmp.ptr_eq(widget)))
        .unwrap();

    let on_window = windows[window].widgets();
    let previous = windows.iter().take(window).flat_map(|w| w.widgets());
    let following = windows.iter().skip(window + 1).flat_map(|w| w.widgets());

    on_window
        .chain(following)
        .chain(previous)
        .find(|(w, _)| w.type_name() == type_name)
        .map(|(w, a)| (w.as_passive(), a))
}
