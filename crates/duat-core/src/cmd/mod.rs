//! Creation and execution of commands.
//!
//! Commands on Duat are bits of code that can be executed on the
//! [`PromptLine`] widget. They can also be invoked from other parts
//! of the code, but their use is mostly intended for user calls.
//!
//! # Running commands
//!
//! There are two environments where you'd run commands. When you have
//! a [`Pass`] available, and when you don't.
//!
//! ## When you have a [`Pass`]
//!
//! If you have a `Pass` available, it means you are in the main
//! thread of execution, and can safely execute commands. The
//! advantage of using a `Pass` is that you can retrieve the value
//! returned by the command:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::*;
//! fn main_thread_function(pa: &mut Pass) {
//!     let result = cmd::call(pa, "colorscheme solarized");
//!     if result.is_ok() {
//!         context::info!("[a]Awesome!");
//!     }
//! }
//! ```
//!
//! The code above runs the `colorscheme` command. In this case, if
//! the command succeds or fails, no notification will be shown, if
//! you want notifications, you should use [`cmd::call_notify`], and
//! the return value would still be acquired.
//!
//! ## When you don't have a [`Pass`]
//!
//! you may not have a `Pass` if, for example, you are not on the
//! main thread of execution. In this case, there is [`cmd::queue`],
//! which queues up a call to be executed later.
//! This means that you can't retrieve the return value, since it will
//! be executed asynchronously:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::*;
//! fn on_a_thread_far_far_away() {
//!     cmd::queue("set-form --flag -abc punctuation.delimiter rgb 255 0 0 hsl 1");
//! }
//! ```
//!
//! The `set-form` command above will fail, since the hsl [`Color`]
//! [`Parameter`] was not completely matched, missing the saturation
//! and lightness arguments. It also shows two unused flag arguments,
//! word flags (`"--flag"`) and blob flags (`"-abc"`), which will also
//! cause the command to fail. Even failing, no notification will be
//! sent, because I called [`queue`], if you want notifications, call
//! [`queue_notify`].
//!
//! If you want to "react" to the result of a queued call, you can use
//! the [`cmd::queue_and`] function, which lets you also send a
//! function that takes [`CmdResult`] as parameter:
//!
//! ```rust
//! # mod duat { pub mod prelude { pub use duat_core::cmd; } }
//! # use std::sync::atomic::{AtomicUsize, Ordering};
//! use duat::prelude::*;
//!
//! static FAILURE_COUNT: AtomicUsize = AtomicUsize::new(0);
//!
//! fn on_a_thread_far_far_away() {
//!     cmd::queue_and(
//!         "set-form --flag -abc punctuation.delimiter rgb 255 0 0 hsl 1",
//!         |res| {
//!             if res.is_err() {
//!                 FAILURE_COUNT.fetch_add(1, Ordering::Relaxed);
//!             }
//!         },
//!     );
//! }
//! ```
//!
//! Note that, because this function might be sent to another thread,
//! it must be [`Send + 'static`].
//!
//! # Adding commands
//!
//! Commands are added through the [`add`] function. This function
//! takes a caller in the form of `&str` and a function to call.
//!
//! This function should have the following arguments:
//!
//! - One `&mut Pass`: This _must_ be annotated.
//! - Up to 12 [`Parameter`] arguments. These will provide automatic
//!   checking on the `PromptLine`.
//!
//! If the second argument is a closure, all of the arguments _MUST_
//! have type annotations *INCLUDING the `&mut Pass`*, otherwise type
//! inference won't work. If this function fails to compile, it is
//! most likely because you passed a closure with missing argument
//! type annotations.
//!
//! Most Rust [`std`] types (that would make sense) are implemented as
//! [`Parameter`]s, so you can place [`String`]s, [`f32`]s, [`bool`]s,
//! and all sorts of other types as `Parameter`s for your command.
//!
//! Types like [`Vec`] and [`Option`] are also implemented as
//! [`Parameter`]s, so you can have a list of `Parameter`s or an
//! optional `Parameter`.
//!
//! ```rust
//! # fn test() {
//! # duat_core::doc_duat!(duat);
//! use duat::prelude::*;
//!
//! let result = cmd::add("unset-form", |pa: &mut Pass, forms: Vec<cmd::FormName>| {
//!     for form in forms.iter() {
//!         form::set("form", Form::new());
//!     }
//!     // You can return a success message, but must
//!     // return an error message if there is a problem.
//!     // For those, you should use the `txt!` macro.
//!     Ok(Some(txt!("Unset [a]{}[] forms", forms.len())))
//! });
//! // You can also alias commands:
//! cmd::alias("uf", "unset-form");
//! # }
//! ```
//!
//! In the command above, you'll notice that [`Ok`] values return
//! [`Option<Text>`]. This is because you may not care about
//! announcing that the command succedeed. For the [`Err`] variant,
//! however, the return value is just [`Text`], because you should say
//! what went wrong in the command. Most of the time, this happens
//! because of something out of your control, like a buffer not
//! existing. In these cases, the `?` is enough to return an
//! appropriate `Text`.
//!
//! There are other builtin types of [`Parameter`]s in `cmd` that
//! can be used on a variety of things. For example, the [`Remainder`]
//! `Parameter` is one that just takes the remaining arguments and
//! collects them into a single [`String`].
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! # let (tx, rx) = duat_core::context::duat_channel();
//! # duat_core::context::set_sender(tx);
//! # duat_core::form::set_initial(duat_core::form::get_initial());
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! cmd::add("pip", |_: &mut Pass, args: cmd::Remainder| {
//!     let child = std::process::Command::new("pip").spawn()?;
//!     let res = child.wait_with_output()?;
//!
//!     Ok(Some(txt!("{res:?}")))
//! });
//! ```
//!
//! [`PromptLine`]: https://docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
//! [`cmd::call_notify`]: call_notify
//! [`cmd::queue`]: queue
//! [`cmd::queue_and`]: queue_and
//! [`Send + 'static`]: Send
//! [`Color`]: crate::form::Color
//! [`txt!`]: crate::text::txt
//! [`Ok(Some({Text}))`]: Ok
//! [`Form`]: crate::form::Form
//! [`Handle`]: crate::context::Handle
use std::{
    collections::HashMap,
    fmt::Display,
    ops::Range,
    sync::{Arc, Mutex},
};

pub use self::{global::*, parameters::*};
use crate::{
    data::Pass,
    form::FormId,
    text::{Text, txt},
    utils::catch_panic,
};

mod default;
mod parameters;

mod global {
    use std::{
        any::TypeId,
        ops::Range,
        sync::{Arc, Mutex},
    };

    use super::{CmdResult, Commands};
    use crate::data::BulkDataWriter;
    #[doc(inline)]
    use crate::{cmd::CmdFn, context, data::Pass, form::FormId, session::DuatEvent, text::Text};

    static COMMANDS: BulkDataWriter<Commands> = BulkDataWriter::new();

    /// A builder for a command
    ///
    /// This struct is created by [`cmd::add`], and when it is
    /// dropped, the command gets added.
    ///
    /// [`cmd::add`]: add
    pub struct CmdBuilder {
        command: Option<super::Command>,
        param_n: usize,
    }

    impl CmdBuilder {
        /// Adds documentation to this command
        pub fn doc(mut self, short: Text, long: Option<Text>) -> Self {
            let command = self.command.as_mut().unwrap();
            command.doc.short = Some(Arc::new(short));
            command.doc.long = long.map(Arc::new);
            self
        }

        /// Adds documentation for the next [`Parameter`] of the
        /// command
        ///
        /// You have to give a short description, and you may give a
        /// long description and a name.
        ///
        /// The short and long descriptions should not present the
        /// same information, the long description merely adds
        /// additional context.
        ///
        /// The `name` argument will rename the argument to something
        /// else. If it is excluded, then the default name (provided
        /// by [`Parameter::arg_name`] will be used.
        ///
        /// [`Parameter`]: super::Parameter
        /// [`Parameter::arg_name`]: super::Parameter::arg_name
        #[track_caller]
        pub fn doc_param(mut self, short: Text, long: Option<Text>, name: Option<Text>) -> Self {
            assert!(
                !self.command.as_ref().unwrap().doc.params.is_empty(),
                "Command has no parameters to be documented"
            );

            assert!(
                self.param_n < self.command.as_ref().unwrap().doc.params.len(),
                "Too many docs for the number of Parameters",
            );

            let param = {
                let params = &mut self.command.as_mut().unwrap().doc.params;
                &mut Arc::get_mut(params).unwrap()[self.param_n]
            };

            param.short = Some(short);
            param.long = long;
            if let Some(name) = name {
                param.arg_name = name;
            }

            self.param_n += 1;

            self
        }
    }

    impl Drop for CmdBuilder {
        fn drop(&mut self) {
            let command = self.command.take().unwrap();
            COMMANDS.mutate(move |cmds| cmds.add(command));
        }
    }

    /// The description for a [`Parameter`], which is used by Duat's
    /// commands
    ///
    /// [`Parameter`]: super::Parameter
    #[derive(Debug, Clone)]
    pub struct ParamDoc {
        /// The name for the parameter
        ///
        /// This will be used when formatting documentation for the
        /// command, like `<path>` in `buffer <path>`.
        pub arg_name: Text,
        /// "Short" documentation for the `Parameter`
        ///
        /// This should be a short line of few words, meant for
        /// possibly being displayed in a list.
        ///
        /// Note that this is _not_ a short version of
        /// [`ParamDoc::long`]. So they shouldn't present the same
        /// information.
        pub short: Option<Text>,
        /// "Long" documentation for a command
        ///
        /// This should add more details to the [`Text`] of
        /// [`ParamDoc::short`]. This description is meant to be shown
        /// when more information is required _on top_ of
        /// [`ParamDoc::short`], so they shouldn't present duplicate
        /// information.
        pub long: Option<Text>,
    }

    /// Adds a command to Duat
    ///
    /// Anyone will be able to execute this command, and it can take
    /// any number of [`Parameter`] arguments. These arguments will be
    /// automatically matched and checked before the command starts
    /// execution, providing feedback to the user as he is typing the
    /// commands.
    ///
    /// # Examples
    ///
    /// In the config crate:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     let var = data::RwData::new(35);
    ///
    ///     let var_clone = var.clone();
    ///     cmd::add("set-var", move |pa: &mut Pass, value: usize| {
    ///         *var_clone.write(pa) = value;
    ///         Ok(None)
    ///     });
    ///
    ///     hook::add::<WindowOpened>(move |mut pa, window| {
    ///         status!("The value is currently {var}")
    ///             .above()
    ///             .push_on(pa, window);
    ///     });
    /// }
    /// ```
    ///
    /// Since `var` is an [`RwData`], it will be updated
    /// automatically in the [`StatusLine`]
    ///
    /// [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
    /// [`RwData`]: crate::data::RwData
    /// [`Parameter`]: super::Parameter
    pub fn add<Cmd: CmdFn<impl std::any::Any>>(caller: &str, mut cmd: Cmd) -> CmdBuilder {
        CmdBuilder {
            command: Some(super::Command::new(
                caller.to_string(),
                Arc::new(Mutex::new(move |pa: &mut Pass, args: super::Args| {
                    cmd.call(pa, args)
                })),
                Cmd::check_args,
                Cmd::param_arg_names(),
            )),
            param_n: 0,
        }
    }

    /// Canonical way to quit Duat.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Duat will execute a program closing
    /// function, as defined by the [Ui].
    ///
    /// [Ui]: crate::ui::traits::RawUi
    pub fn quit() {
        queue("quit");
    }

    /// Switches to/opens a [`Buffer`] with the given name.
    ///
    /// If you wish to specifically switch to buffers that are already
    /// open, use [`buffer`].
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn edit(pa: &mut Pass, buffer: impl std::fmt::Display) -> CmdResult {
        call(pa, format!("edit {buffer}"))
    }

    /// Switches to a [`Buffer`] with the given name.
    ///
    /// If there is no buffer open with that name, does nothing. Use
    /// [`edit`] if you wish to open buffers.
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn buffer(pa: &mut Pass, buffer: impl std::fmt::Display) -> CmdResult {
        call(pa, format!("buffer {buffer}"))
    }

    /// Switches to the next [`Buffer`].
    ///
    /// This function will only look at buffers that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`next_global_buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn next_buffer(pa: &mut Pass) -> CmdResult {
        call(pa, "next-buffer")
    }

    /// Switches to the previous [`Buffer`].
    ///
    /// This function will only look at buffers that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`prev_global_buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn prev_buffer(pa: &mut Pass) -> CmdResult {
        call(pa, "prev-buffer")
    }

    /// Switches to the next [`Buffer`].
    ///
    /// This function will look for buffers in all windows. If you
    /// want to limit the search to just the current window, use
    /// [`next_buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn next_global_buffer(pa: &mut Pass) -> CmdResult {
        call(pa, "next-buffer --global")
    }

    /// Switches to the previous [`Buffer`].
    ///
    /// This function will look for buffers in all windows. If you
    /// want to limit the search to just the current window, use
    /// [`prev_buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn prev_global_buffer(pa: &mut Pass) -> CmdResult {
        call(pa, "prev-buffer --global")
    }

    /// Tries to alias a `caller` to an existing `command`.
    pub fn alias(alias: impl ToString, command: impl ToString) {
        let alias = alias.to_string();
        let command = command.to_string();
        COMMANDS.mutate(move |cmds| context::logs().push_cmd_result(cmds.alias(alias, command)))
    }

    /// Runs a full command synchronously, with a [`Pass`].
    ///
    /// If you call commands through this function, you will be able
    /// to retrieve their return values,  but because of the [`Pass`],
    /// this function can only be used on the main thread of
    /// execution. If you want to call commands from other threads,
    /// see [`cmd::queue`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn main_thread_function(pa: &mut Pass) {
    ///     cmd::call(pa, "set-prompt new-prompt");
    /// }
    /// ```
    ///
    /// In this case we're running a command that will affect the most
    /// relevant [`PromptLine`], and no notifications are sent. That's
    /// because I used [`call`]. If you want notifications, see
    /// [`cmd::call_notify`].
    ///
    /// [`cmd::queue`]: queue
    /// [`cmd::call_notify`]: call_notify
    /// [`PromptLine`]: https://docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
    pub fn call(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        COMMANDS.write(pa).get_cmd(call).and_then(|cmd| cmd(pa))
    }

    /// Like [`call`], but notifies the result
    #[allow(unused_must_use)]
    pub fn call_notify(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        let result = COMMANDS
            .write(pa)
            .get_cmd(call.to_string())
            .and_then(|cmd| cmd(pa));
        context::logs().push_cmd_result(result.clone());

        result
    }

    /// Queues a command call
    ///
    /// You should only use this if you're not in the main thread of
    /// execution, or if you don't have a [`Pass`] for some other
    /// reason, like you are in the middle of accessing an [`RwData`]
    /// or something like that.
    ///
    /// Since this function will run outside of the current scope, its
    /// [`Result`] will not be returned.
    ///
    /// [`RwData`]: crate::data::RwData
    pub fn queue(call: impl std::fmt::Display) {
        let call = call.to_string();
        crate::context::sender().send(DuatEvent::QueuedFunction(Box::new(move |pa| {
            let _ = COMMANDS.write(pa).get_cmd(call).and_then(|cmd| cmd(pa));
        })));
    }

    /// Like [`queue`], but notifies the result
    pub fn queue_notify(call: impl std::fmt::Display) {
        let call = call.to_string();
        crate::context::sender().send(DuatEvent::QueuedFunction(Box::new(move |pa| {
            context::logs()
                .push_cmd_result(COMMANDS.write(pa).get_cmd(call).and_then(|cmd| cmd(pa)));
        })))
    }

    /// Like [`queue`], but acts on the [`Result`]
    pub fn queue_and(call: impl std::fmt::Display, map: impl FnOnce(CmdResult) + Send + 'static) {
        let call = call.to_string();
        crate::context::sender().send(DuatEvent::QueuedFunction(Box::new(move |pa| {
            map(COMMANDS.write(pa).get_cmd(call).and_then(|cmd| cmd(pa)));
        })))
    }

    /// Like [`queue_and`], but also notifies the [`Result`]
    pub fn queue_notify_and(
        call: impl std::fmt::Display,
        map: impl FnOnce(CmdResult) + Send + 'static,
    ) {
        let call = call.to_string();
        crate::context::sender().send(DuatEvent::QueuedFunction(Box::new(move |pa| {
            let result = COMMANDS.write(pa).get_cmd(call).and_then(|cmd| cmd(pa));
            context::logs().push_cmd_result(result.clone());

            map(result)
        })));
    }

    /// Check if the arguments for a given `caller` are correct
    pub fn check_args(
        pa: &mut Pass,
        call: &str,
    ) -> Option<(
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    )> {
        COMMANDS.write(pa).check_args(call).map(|ca| ca(pa))
    }

    /// The [`TypeId`] of the last [`Parameter`]s that were being
    /// parsed
    ///
    /// This is a list that may have multiple `Parameter`s because
    /// some of those might only optionally take arguments, which
    /// means that you'd be parsing both the optionally parseable
    /// `Parameter` as well as the following ones.
    ///
    /// Returns [`None`] if the caller couldn't be found, or if the
    /// call was an empty string.
    ///
    /// # Note
    ///
    /// You can pass in only a prefix of the full call in order to get
    /// the parameter completion for the `n`th argument, as opposed to
    /// just the last one. This is useful if you want to get the
    /// `TypeId`s being parsed at a cursor's position, which may be in
    /// the middle of the string as opposed to the end.
    ///
    /// [`Parameter`]: super::Parameter
    pub fn last_parsed_parameters(pa: &mut Pass, call: &str) -> Option<Vec<TypeId>> {
        Some(COMMANDS.write(pa).args_after_check(call)?(pa).last_parsed())
    }

    /// The description for a Duat command, which can be executed in
    /// the `PromptLine`
    #[derive(Clone)]
    pub struct CmdDoc {
        /// The caller for the command
        pub caller: Arc<str>,
        /// "Short" documentation for a command
        ///
        /// This should be a short line of few words, meant for
        /// possibly being displayed in a list.
        ///
        /// Note that this is _not_ a short version of
        /// [`CmdDoc::long`]. So they shouldn't present the same
        /// information.
        pub short: Option<Arc<Text>>,
        /// "Long" documentation for a command
        ///
        /// This should add more details to the [`Text`] of
        /// [`CmdDoc::short`]. This description is meant to be shown
        /// when more information is required _on top_ of
        /// [`CmdDoc::short`], so they shouldn't present the same
        /// information.
        ///
        /// Note also that you _can_ add documentation about the
        /// parameters, however, you should prioritize calling
        /// [`CmdBuilder::doc_param`] for that purpose instead.
        pub long: Option<Arc<Text>>,
        /// Documentation about the command's parameters
        pub params: Arc<[ParamDoc]>,
    }

    /// The description for a Duat alias, which can be executed in
    /// the `PromptLine`, aliasing to a proper command
    #[derive(Clone)]
    pub struct AliasDescription {
        /// The caller for the alias
        pub caller: Arc<str>,
        /// What the caller gets replaced by
        pub replacement: Arc<str>,
        /// The description of the original command
        pub cmd: CmdDoc,
    }

    /// Description of a Duat command or alias
    #[derive(Clone)]
    pub enum Description {
        /// The description of a command.
        Command(CmdDoc),
        /// The description of an alias for a command.
        Alias(AliasDescription),
    }

    impl Description {
        /// The caller for the command/alias
        pub fn caller(&self) -> &str {
            match self {
                Description::Command(cmd_description) => &cmd_description.caller,
                Description::Alias(alias_description) => &alias_description.caller,
            }
        }
    }

    /// A list of descriptions for all commands in Duat
    ///
    /// This list does not have any inherent sorting, with the
    /// exception that aliases are listed after commands.
    pub fn cmd_list(pa: &mut Pass) -> Vec<Description> {
        let commands = COMMANDS.write(pa);
        commands
            .list
            .iter()
            .map(|cmd| Description::Command(cmd.doc.clone()))
            .chain(commands.aliases.iter().map(|(caller, (cmd, replacement))| {
                Description::Alias(AliasDescription {
                    caller: caller.clone(),
                    replacement: replacement.clone(),
                    cmd: cmd.doc.clone(),
                })
            }))
            .collect()
    }
}

/// A list of commands.
///
/// This list contains all of the commands that have been
/// added to Duat, as well as info on the current [`Buffer`],
/// [widget] and all of the [windows].
///
/// [`Buffer`]: crate::buffer::Buffer
/// [widget]: crate::ui::Widget
/// [windows]: crate::ui::Window
struct Commands {
    list: Vec<Command>,
    aliases: HashMap<Arc<str>, (Command, Arc<str>)>,
}

impl Commands {
    /// Aliases a command to a specific word
    fn alias(&mut self, alias: String, call: String) -> CmdResult {
        if alias.split_whitespace().count() != 1 {
            return Err(txt!("Alias [a]{alias}[] is not a single word"));
        }

        let caller = call
            .split_whitespace()
            .next()
            .ok_or(txt!("The command is empty"))?
            .to_string();

        let mut cmds = self.list.iter();

        if let Some(command) = cmds.find(|cmd| cmd.doc.caller.as_ref() == caller) {
            let entry = (command.clone(), Arc::from(call));
            self.aliases.insert(Arc::from(alias), entry);
            Ok(None)
        } else {
            Err(txt!("The caller [a]{caller}[] was not found"))
        }
    }

    /// Runs a command from a call
    fn get_cmd(
        &self,
        call: impl Display,
    ) -> Result<impl for<'a> FnOnce(&'a mut Pass) -> CmdResult + 'static, Text> {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(txt!("The command is empty"))?.to_string();

        let (command, call) = {
            if let Some(command) = self.aliases.get(caller.as_str()) {
                let (alias, aliased_call) = command;
                let mut aliased_call = aliased_call.to_string() + " ";
                aliased_call.push_str(call.strip_prefix(&caller).unwrap());

                (alias.clone(), aliased_call)
            } else {
                let command = self
                    .list
                    .iter()
                    .find(|cmd| cmd.doc.caller.as_ref() == caller)
                    .ok_or(txt!("[a]{caller}[]: No such command"))?;

                (command.clone(), call.clone())
            }
        };

        let silent = call.len() > call.trim_start().len();
        let cmd = command.cmd.clone();
        Ok(move |pa: &mut Pass| {
            let args = Args::new(&call);

            match catch_panic(move || cmd.lock().unwrap()(pa, args)) {
                Some(result) => result
                    .map(|ok| ok.filter(|_| !silent))
                    .map_err(|err| txt!("[a]{caller}[]: {err}")),
                None => Err(txt!("[a]{caller}[]: Command panicked")),
            }
        })
    }

    /// Adds a command to the list of commands
    fn add(&mut self, command: Command) {
        self.list.retain(|cmd| cmd.doc.caller != command.doc.caller);
        self.list.push(command);
    }

    /// Gets the parameter checker for a command, if it exists
    fn check_args<'a>(&self, call: &'a str) -> Option<impl FnOnce(&Pass) -> CheckedArgs + 'a> {
        let mut args = call.split_whitespace();
        let caller = args.next()?.to_string();

        let check_args = if let Some((command, _)) = self.aliases.get(caller.as_str()) {
            command.check_args
        } else {
            let command = self
                .list
                .iter()
                .find(|cmd| cmd.doc.caller.as_ref() == caller)?;
            command.check_args
        };

        Some(move |pa: &Pass| check_args(pa, &mut Args::new(call)))
    }

    /// Gets the last parsed [`Parameter`] of a call
    fn args_after_check<'a>(&self, call: &'a str) -> Option<impl FnOnce(&Pass) -> Args<'a> + 'a> {
        let mut args = call.split_whitespace();
        let caller = args.next()?.to_string();

        let check_args = if let Some((command, _)) = self.aliases.get(caller.as_str()) {
            command.check_args
        } else {
            let command = self
                .list
                .iter()
                .find(|cmd| cmd.doc.caller.as_ref() == caller)?;
            command.check_args
        };

        Some(move |pa: &Pass| {
            let mut args = Args::new(call);
            check_args(pa, &mut args);
            args
        })
    }
}

impl Default for Commands {
    fn default() -> Self {
        default::add_defalt_commands();
        Self {
            list: Default::default(),
            aliases: Default::default(),
        }
    }
}

/// The standard error that should be returned when [`call`]ing
/// commands.
///
/// This error _must_ include an error message in case of failure. It
/// may also include a success message, but that is not required.
///
/// [`call`]: global::call
pub type CmdResult = Result<Option<Text>, Text>;

/// A function that can be called by name.
#[derive(Clone)]
struct Command {
    cmd: InnerCmdFn,
    check_args: CheckerFn,
    doc: CmdDoc,
}

impl Command {
    /// Returns a new instance of command.
    fn new(
        caller: String,
        cmd: InnerCmdFn,
        check_args: CheckerFn,
        param_arg_names: Vec<Text>,
    ) -> Self {
        if caller.split_whitespace().count() != 1 {
            panic!("Command caller \"{caller}\" contains more than one word");
        }
        Self {
            cmd,
            check_args,
            doc: CmdDoc {
                caller: caller.into(),
                short: None,
                long: None,
                params: param_arg_names
                    .into_iter()
                    .map(|arg_name| ParamDoc { arg_name, short: None, long: None })
                    .collect(),
            },
        }
    }
}

/// A list of names to call a command with
pub trait Caller<'a>: Sized {
    /// An [`Iterator`] over said callers
    fn into_callers(self) -> impl Iterator<Item = &'a str>;
}

impl<'a> Caller<'a> for &'a str {
    fn into_callers(self) -> impl Iterator<Item = &'a str> {
        [self].into_iter()
    }
}

impl<'a> Caller<'a> for &'a [&'a str] {
    fn into_callers(self) -> impl Iterator<Item = &'a str> {
        self.iter().cloned()
    }
}

impl<'a, const N: usize> Caller<'a> for [&'a str; N] {
    fn into_callers(self) -> impl Iterator<Item = &'a str> {
        self.into_iter()
    }
}

type InnerCmdFn = Arc<Mutex<dyn FnMut(&mut Pass, Args) -> CmdResult + Send + 'static>>;
type CheckerFn = fn(&Pass, &mut Args) -> CheckedArgs;
type CheckedArgs = (
    Vec<(Range<usize>, Option<FormId>)>,
    Option<(Range<usize>, Text)>,
);

trait CmdFn<Arguments>: Send + 'static {
    fn call(&mut self, pa: &mut Pass, args: Args) -> CmdResult;

    fn check_args(
        pa: &Pass,
        args: &mut Args,
    ) -> (
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    );

    fn param_arg_names() -> Vec<Text>;
}

impl<F: FnMut(&mut Pass) -> CmdResult + Send + 'static> CmdFn<()> for F {
    fn call(&mut self, pa: &mut Pass, _: Args) -> CmdResult {
        self(pa)
    }

    fn check_args(
        _: &Pass,
        args: &mut Args,
    ) -> (
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    ) {
        if let Some(start) = args.next_start() {
            let err = txt!("Too many arguments");
            return (Vec::new(), Some((start..args.param_range().end, err)));
        }

        (Vec::new(), None)
    }

    fn param_arg_names() -> Vec<Text> {
        Vec::new()
    }
}

macro_rules! implCmdFn {
    ($($param:ident),+) => {
        impl<$($param),+, F> CmdFn<($($param,)+)> for F
        where
            $($param: Parameter,)+
            F: FnMut(&mut Pass, $($param),+) -> CmdResult + Send + 'static
        {
            #[allow(non_snake_case)]
            fn call(&mut self, pa: &mut Pass, mut args: Args) -> CmdResult {
                $(
                    let ($param, _) = $param::from_args(pa, &mut args)?;
                )+

                self(pa, $($param),+)
            }

            fn check_args(
                pa: &Pass,
                args: &mut Args,
            ) -> (
                Vec<(Range<usize>, Option<FormId>)>,
                Option<(Range<usize>, Text)>,
            ) {
                let mut ok_ranges = Vec::new();

				$(
                    let start = args.next_start();
                    args.use_completions_for::<$param>();
                    let result = args.next_as_with_form::<$param>(pa);
                    match result {
                        Ok((_, form)) => {
                            if let Some(start) = start.filter(|s| args.param_range().end > *s) {
                                ok_ranges.push((start..args.param_range().end, form));
                            }
                        }
                        Err(err) => match start.filter(|s| args.param_range().end > *s) {
                            Some(_) => return (ok_ranges, Some((args.param_range(), err))),
                            None => return (ok_ranges, None)
                        }
                    }
				)+

                if let Some(start) = args.next_start() {
                    let err = txt!("Too many arguments");
                    return (ok_ranges, Some((start..args.param_range().end, err)));
                }

                (ok_ranges, None)
            }

            fn param_arg_names() -> Vec<Text> {
                vec![$($param::arg_name()),+]
            }
        }
    }
}

implCmdFn!(P0);
implCmdFn!(P0, P1);
implCmdFn!(P0, P1, P2);
implCmdFn!(P0, P1, P2, P3);
implCmdFn!(P0, P1, P2, P3, P4);
implCmdFn!(P0, P1, P2, P3, P4, P5);
implCmdFn!(P0, P1, P2, P3, P4, P5, P6);
implCmdFn!(P0, P1, P2, P3, P4, P5, P6, P7);
implCmdFn!(P0, P1, P2, P3, P4, P5, P6, P7, P8);
implCmdFn!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9);
implCmdFn!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
implCmdFn!(P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
