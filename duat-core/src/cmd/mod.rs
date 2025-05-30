//! Creation and execution of commands.
//!
//! Commands on Duat are bits of code that can be executed on the
//! [`PromptLine`] widget. They can also be invoked from other parts
//! of the code, but their use is mostly intended for runtime calls.
//!
//! They are executed asynchronously in order to prevent deadlocks in
//! Duat's internal systems.
//!
//! # Running commands
//!
//! ```rust
//! # use duat_core::cmd;
//! cmd::run("colorscheme solarized");
//! ```
//!
//! The code above runs the `colorscheme` command. In this case, if
//! the command succeds or fails, no notification will be shown, if
//! you want notifications, you should use [`cmd::run_notify`]:
//!
//! ```rust
//! # use duat_core::cmd;
//! cmd::run_notify(
//!     "set-form --flag -abc punctuation.delimiter rgb 255 0 0 hsl 1",
//! );
//! ```
//!
//! [`cmd::run_notify`] is what is used by Duat when running commands
//! in the [`PromptLine`], but you can silence notifications by
//! including leading whitespace:
//!
//! ```rust
//! # use duat_core::{cmd, context, text::Text};
//! cmd::run_notify(" set-form Default.StatusLine #000000 #ffffff");
//! assert_eq!(*context::notifications().read(), Vec::<Text>::new());
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
//! The first one is used if you only wish to interpret some [`Args`].
//! The second one lets you modify a specific [`Widget`] and its
//! [`Area`], allongside said [`Args`].
//!
//! These macros will take two arguments, the first one is a list of
//! callers for that command, e.g. `["quit", "q"]` for the `quit`
//! command. Note that a regular [`&str`] argument is also accepted.
//!
//! The second argument is a _rust-like_ closure with a variable
//! number of arguments. Each argument must implement the
//! [`Parameter`] trait, and its type must be explicit, in order for
//! Duat to automatically interpret user input as a specific type.
//!
//! Most Rust [`std`] types (that would make sense) are implemented as
//! [`Parameter`]s, so you can place [`String`]s, [`f32`]s, [`bool`]s,
//! and all sorts of other types as [`Parameter`]s for your command.
//!
//! Types like [`Vec`] and [`Option`] are also implemented as
//! [`Parameter`]s, so you can have a list of [`Parameter`]s or an
//! optional [`Parameter`].
//!
//! ```rust
//! # use duat_core::prelude::{
//! #     ok, form::{self, Form}, cmd::{self, Args, FormName, Flags}
//! # };
//! # use std::sync::{atomic::{AtomicBool, Ordering}, Arc};
//! let callers = ["unset-form", "uf"];
//! // A `Vec<T>` parameter will try to collect all
//! // remaining arguments as `T` in a list.
//! let result = cmd::add!(callers, |forms: Vec<FormName>| {
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
//! In the command above, you'll notice that I used the [`ok!`] macro.
//! This macro is used when you want to return a notification saying
//! that the command succeeded. Its counterpart is the [`err!`] macro.
//! It represents failure in the execution of the command, and must be
//! used.
//!
//! Duat commands also offer flags in the form of the [`Flags`]
//! [`Parameter`]. These can make use of word flags (`--full-words`)
//! and blob flags (`-singlechar`):
//!
//! ```rust
//! # use duat_core::cmd;
//! # use std::sync::{atomic::{AtomicU32, Ordering}, Arc};
//! let expression = Arc::new(AtomicU32::default());
//! let my_command = {
//!     let expression = expression.clone();
//!     cmd::add!("mood", move |flags: cmd::Flags| {
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
//! There are other builtin types of [`Parameter`]s in [`cmd`] that
//! can be used on a variety of things. For example, the [`Remainder`]
//! [`Parameter`] is one that just takes the remaining arguments and
//! collects them into a single [`String`].
//!
//! ```rust
//! # use duat_core::prelude::{cmd, err, ok};
//! cmd::add!("pip", |args: cmd::Remainder| {
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
//! on a [`Widget`] and its [`Area`]. Both of these [`Parameter`]s
//! need to be included and type anotated:
//!
//! ```rust
//! # use duat_core::{cmd, widgets::{LineNumbers, LineNum}};
//! # fn test<U: duat_core::ui::Ui>() {
//! cmd::add_for!("toggle-relative", |ln: LineNumbers<U>, _: U::Area| {
//!     let opts = ln.options_mut();
//!     opts.num_rel = match opts.num_rel {
//!         LineNum::Abs => LineNum::RelAbs,
//!         LineNum::Rel | LineNum::RelAbs => LineNum::Abs,
//!     };
//!     Ok(None)
//! });
//! # }
//! ```
//!
//! [`PromptLine`]: crate::widgets::PromptLine
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
//! [`Area`]: crate::ui::Ui::Area
use std::{
    collections::HashMap,
    fmt::Display,
    ops::Range,
    sync::{Arc, LazyLock},
    time::Instant,
};

use crossterm::style::Color;

pub use self::{
    global::*,
    parameters::{
        Args, Between, ColorSchemeArg, F32PercentOfU8, FileBuffer, Flags, FormName,
        OtherFileBuffer, Parameter, PossibleFile, Remainder, args_iter, get_args,
    },
};
use crate::{
    context,
    data::RwData,
    file_entry, iter_around, iter_around_rev,
    mode::{self},
    session::sender,
    text::{Text, err, hint, ok},
    ui::{DuatEvent, Ui, Window},
    widget_entry,
    widgets::{File, Widget},
};

mod parameters;

pub(crate) fn add_session_commands<U: Ui>() -> Result<(), Text> {
    add!("alias", |flags: Flags, alias: &str, command: Remainder| {
        if !flags.is_empty() {
            Err(err!("An alias cannot take any flags"))
        } else {
            Ok(crate::cmd::alias(alias, command)?)
        }
    })?;

    add!(["quit", "q"], |name: Option<FileBuffer<U>>| {
        let cur_name = context::fixed_file::<U>()?.read().0.name();
        let name = name.unwrap_or(&cur_name);

        let windows = context::windows::<U>().read();
        let (win, wid, file) = file_entry(&windows, name).unwrap();

        let has_unsaved_changes = {
            let file = file.read_as::<File>().unwrap();
            file.text().has_unsaved_changes() && file.exists()
        };
        if has_unsaved_changes {
            return Err(err!("[a]{name}[] has unsaved changes"));
        }

        // If we are on the current File, switch to the next one.
        if name == cur_name {
            let Some(next_name) = iter_around::<U>(&windows, win, wid)
                .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
            else {
                sender().send(DuatEvent::Quit).unwrap();
                return Ok(None);
            };

            // If I send the switch signal first, and the Window is deleted, I
            // will have the synchronously change the current window number
            // without affecting anything else.
            mode::reset_switch_to::<U>(&next_name, true);
        }

        sender()
            .send(DuatEvent::CloseFile(name.to_string()))
            .unwrap();
        Ok(Some(ok!("Closed [a]{name}")))
    })?;

    add!(["quit!", "q!"], |name: Option<FileBuffer<U>>| {
        let cur_name = context::fixed_file::<U>()?.read().0.name();
        let name = name.unwrap_or(&cur_name);

        // Should wait here until I'm out of `session_loop`
        let windows = context::windows::<U>().read();
        let (win, wid, file) = file_entry(&windows, name).unwrap();

        if name == cur_name {
            let Some(next_name) = iter_around::<U>(&windows, win, wid)
                .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
            else {
                sender().send(DuatEvent::Quit).unwrap();
                return Ok(None);
            };
            mode::reset_switch_to::<U>(&next_name, true);
        }

        sender()
            .send(DuatEvent::CloseFile(name.to_string()))
            .unwrap();
        Ok(Some(ok!("Closed [a]{name}")))
    })?;

    add!(["quit-all", "qa"], || {
        let windows = context::windows::<U>().read();
        let unwritten = windows
            .iter()
            .flat_map(Window::file_nodes)
            .filter(|(node, _)| {
                let file = node.read_as::<File>().unwrap();
                file.exists() && file.text().has_unsaved_changes()
            })
            .count();

        if unwritten == 0 {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else if unwritten == 1 {
            Err(err!("There is [a]1[] unsaved file"))
        } else {
            Err(err!("There are [a]{unwritten}[] unsaved files"))
        }
    })?;

    add!(["quit-all!", "qa!"], || {
        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    })?;

    add!(["write", "w"], |path: Option<PossibleFile>| {
        let mut ff = context::fixed_file::<U>()?;
        let (mut file, _) = ff.write();

        let (bytes, name) = if let Some(path) = path {
            (file.write_to(&path)?, path)
        } else if let Some(name) = file.name_set() {
            (file.write()?, std::path::PathBuf::from(name))
        } else {
            return Err(err!("File has no name path to write to"));
        };

        match bytes {
            Some(bytes) => Ok(Some(ok!("Wrote [a]{bytes}[] bytes to [File]{name}"))),
            None => Ok(Some(ok!("Nothing to be written"))),
        }
    })?;

    add!(["write-quit", "wq"], |path: Option<PossibleFile>| {
        let mut ff = context::fixed_file::<U>()?;
        let (bytes, name) = {
            let (mut file, area) = ff.write();
            let bytes = if let Some(path) = path {
                file.write_to(&path)?
            } else {
                file.write()?
            };
            (bytes, file.name())
        };

        // Should wait here until I'm out of `session_loop`
        let windows = context::windows::<U>().read();
        let w = context::cur_window();

        let (win, wid, file) = file_entry(&windows, &name).unwrap();

        let Some(next_name) = iter_around::<U>(&windows, win, wid)
            .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
        else {
            sender().send(DuatEvent::Quit).unwrap();
            return Ok(None);
        };

        mode::reset_switch_to::<U>(&next_name, true);

        sender().send(DuatEvent::CloseFile(name.clone())).unwrap();
        match bytes {
            Some(bytes) => Ok(Some(ok!(
                "Wrote [a]{bytes}[] bytes to [File]{name}[], then closed it"
            ))),
            None => Ok(Some(ok!("No changes in [File]{name}[], so just closed it"))),
        }
    })?;

    add!(["write-all", "wa"], || {
        let windows = context::windows::<U>().read();

        let mut written = 0;
        let file_count = windows
            .iter()
            .flat_map(Window::file_nodes)
            .filter(|(node, _)| {
                node.widget()
                    .read_as::<File>()
                    .is_some_and(|f| f.path_set().is_some())
            })
            .inspect(|(node, _)| {
                written += node.widget().write_as::<File>().unwrap().write().is_ok() as usize;
            })
            .count();

        if written == file_count {
            Ok(Some(ok!("Wrote to [a]{written}[] files")))
        } else {
            let unwritten = file_count - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(err!("Failed to write to [a]{unwritten}[] file{plural}"))
        }
    })?;

    add!(["write-all-quit", "waq"], || {
        let windows = context::windows::<U>().read();

        let mut written = 0;
        let file_count = windows
            .iter()
            .flat_map(Window::file_nodes)
            .filter(|(node, _)| {
                node.widget()
                    .read_as::<File>()
                    .is_some_and(|f| f.path_set().is_some())
            })
            .inspect(|(node, _)| {
                written += node.widget().write_as::<File>().unwrap().write().is_ok() as usize;
            })
            .count();

        if written == file_count {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else {
            let unwritten = file_count - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(err!("Failed to write to [a]{unwritten}[] file{plural}"))
        }
    })?;

    add!(["write-all-quit!", "waq!"], || {
        let windows = context::windows::<U>().read();

        windows
            .iter()
            .flat_map(Window::file_nodes)
            .filter_map(|(node, _)| node.widget().write_as::<File>())
            .for_each(|mut file| {
                let _ = file.write();
            });

        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    })?;

    add!(["reload"], || {
        let Some(toml_path) = crate::crate_dir()
            .map(|config_dir| config_dir.join("Cargo.toml"))
            .filter(|path| path.try_exists().is_ok_and(|exists| exists))
        else {
            return Err(err!("Cargo.toml was not found"));
        };

        let mut cargo = std::process::Command::new("cargo");
        cargo.stdin(std::process::Stdio::null());
        cargo.stdout(std::process::Stdio::null());
        cargo.stderr(std::process::Stdio::null());
        cargo.args([
            "build",
            "--quiet",
            "--manifest-path",
            toml_path.to_str().unwrap(),
        ]);
        if !cfg!(debug_assertions) {
            cargo.arg("--release");
        };

        match cargo.spawn() {
            Ok(child) => {
                sender()
                    .send(DuatEvent::ReloadStarted(Instant::now()))
                    .unwrap();
                std::thread::spawn(|| {
                    match child.wait_with_output() {
                        Ok(out) => {
                            if out.status.code() != Some(0) {
                                context::notify(err!("Couldn't compile config crate"));
                            }
                        }
                        Err(err) => context::notify(err!("cargo failed: [a]{err}")),
                    };
                });
                Ok(Some(hint!("Started config recompilation")))
            }
            Err(err) => Err(err!("Failed to start cargo: [a]{err}")),
        }
    })?;

    add!(["edit", "e"], |path: PossibleFile| {
        let windows = context::windows::<U>().read();

        let name = if let Ok(path) = path.strip_prefix(context::cur_dir()) {
            path.to_string_lossy().to_string()
        } else {
            path.to_string_lossy().to_string()
        };

        if file_entry(&windows, &name).is_err() {
            sender().send(DuatEvent::OpenFile(name.clone())).unwrap();
            return Ok(Some(ok!("Opened [a]{name}")));
        }

        mode::reset_switch_to::<U>(name.clone(), true);
        Ok(Some(ok!("Switched to [a]{name}")))
    })?;

    add!(["open", "o"], |path: PossibleFile| {
        let windows = context::windows::<U>().read();

        let name = if let Ok(path) = path.strip_prefix(context::cur_dir()) {
            path.to_string_lossy().to_string()
        } else {
            path.to_string_lossy().to_string()
        };

        let Ok((win, wid, node)) = file_entry(&windows, &name) else {
            sender().send(DuatEvent::OpenWindow(name.clone())).unwrap();
            return Ok(Some(ok!("Opened [a]{name}[] on new window")));
        };

        if windows[win].file_nodes().len() == 1 {
            mode::reset_switch_to::<U>(name.clone(), true);
            Ok(Some(ok!("Switched to [a]{name}")))
        } else {
            sender().send(DuatEvent::OpenWindow(name.clone())).unwrap();
            Ok(Some(ok!("Moved [a]{name}[] to a new window")))
        }
    })?;

    add!(["buffer", "b"], |name: OtherFileBuffer<U>| {
        mode::reset_switch_to::<U>(&name, true);
        Ok(Some(ok!("Switched to [a]{name}")))
    })?;

    add!("next-file", |flags: Flags| {
        let windows = context::windows().read();
        let ff = context::fixed_file()?;
        let win = context::cur_window();

        let wid = windows[win]
            .nodes()
            .position(|node| ff.file_ptr_eq(node))
            .unwrap();

        let name = if flags.word("global") {
            iter_around::<U>(&windows, win, wid)
                .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
                .ok_or_else(|| err!("There are no other open files"))?
        } else {
            let slice = &windows[win..=win];
            iter_around(slice, 0, wid)
                .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
                .ok_or_else(|| err!("There are no other files open in this window"))?
        };

        mode::reset_switch_to::<U>(&name, true);
        Ok(Some(ok!("Switched to [a]{name}")))
    })?;

    add!("prev-file", |flags: Flags| {
        let windows = context::windows().read();
        let file = context::fixed_file()?;
        let w = context::cur_window();

        let widget_i = windows[w]
            .nodes()
            .position(|node| file.file_ptr_eq(node))
            .unwrap();

        let name = if flags.word("global") {
            iter_around_rev::<U>(&windows, w, widget_i)
                .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
                .ok_or_else(|| err!("There are no other open files"))?
        } else {
            let slice = &windows[w..=w];
            iter_around_rev(slice, 0, widget_i)
                .find_map(|(.., node)| node.read_as::<File>().map(|f| f.name()))
                .ok_or_else(|| err!("There are no other files open in this window"))?
        };

        mode::reset_switch_to::<U>(&name, true);

        Ok(Some(ok!("Switched to [a]{name}")))
    })?;

    add!("swap", |lhs: FileBuffer<U>, rhs: Option<FileBuffer<U>>| {
        let rhs = if let Some(rhs) = rhs {
            rhs.to_string()
        } else {
            context::fixed_file::<U>()?.read().0.name()
        };
        sender()
            .send(DuatEvent::SwapFiles(lhs.to_string(), rhs.clone()))
            .unwrap();

        Ok(Some(ok!("Swapped [a]{lhs}[] and [a]{rhs}")))
    })?;

    add!("colorscheme", |scheme: ColorSchemeArg| {
        crate::form::set_colorscheme(scheme);
        Ok(Some(ok!("Set colorscheme to [a]{scheme}[]")))
    })?;

    add!(
        "set-form",
        |name: FormName, colors: Between<0, 3, Color>| {
            let mut form = crate::form::Form::new();
            form.style.foreground_color = colors.first().cloned();
            form.style.background_color = colors.get(1).cloned();
            form.style.underline_color = colors.get(2).cloned();
            crate::form::set(name, form);

            Ok(Some(ok!("Set [a]{name}[] to a new Form")))
        }
    )?;

    Ok(())
}

mod global {
    use std::{ops::Range, sync::Arc};

    use super::{Args, CmdResult, Commands, Parameter};
    use crate::{text::Text, ui::Ui, widgets::Widget};

    static COMMANDS: Commands = Commands::new();

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
    /// ```rust
    /// # use duat_core::{cmd, data::RwData, hooks::{self, OnWindowOpen}, ui::Ui, widgets::status};
    /// # fn test<U: Ui>() {
    /// let var = RwData::new(35);
    ///
    /// let var_clone = var.clone();
    /// cmd::add!("set-var", move |value: usize| {
    ///     *var_clone.write() = value;
    ///     Ok(None)
    /// });
    ///
    /// hooks::add::<OnWindowOpen<U>>(move |builder| {
    ///     let var = var.clone();
    ///     builder.push(status!("The value is currently " var));
    /// });
    /// # }
    /// ```
    ///
    /// Since `var` is an [`RwData`], it will be updated
    /// automatically in the [`StatusLine`]
    ///
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`RwData`]: crate::data::RwData
    pub macro add {
        ($callers:expr, $($mv:ident)? |$($arg:tt: $t:ty),+| $f:block) => {{
            #[allow(unused_variables, unused_mut)]
            let cmd = $($mv)? |mut args: Args| -> CmdResult {
                $(
                    let $arg: <$t as Parameter>::Returns = <$t as Parameter>::new(&mut args)?;
                )*

                if let Ok(arg) = args.next() {
                    return Err($crate::text::err!("Too many arguments"));
                }

                $f
            };

            #[allow(unused_variables, unused_mut)]
            let check_args = |mut args: Args| -> (Vec<Range<usize>>, Option<(Range<usize>, Text)>) {
                let mut ok_ranges = Vec::new();

                $(
                    let start = args.next_start();
                    match args.next_as::<$t>() {
                        Ok(_) => if let Some(start) = start
                            .filter(|s| args.param_range().end > *s)
                        {
                            ok_ranges.push(start..args.param_range().end);
                        }
                        Err(err) => return (ok_ranges, Some((args.param_range(), err)))
                    }
                )*

                let start = args.next_start();
                if let (Ok(_), Some(start)) = (args.next_as::<super::Remainder>(), start) {
                    let err = $crate::text::err!("Too many arguments");
                    return (ok_ranges, Some((start..args.param_range().end, err)))
                }

                (ok_ranges, None)
            };

            add_inner($callers, cmd, check_args)
    	}},

        ($callers:expr, $($mv:ident)? || $f:block) => {{
            #[allow(unused_variables, unused_mut)]
            let cmd = $($mv)? |mut args: Args| -> CmdResult {
                if let Ok(arg) = args.next() {
                    return Err($crate::text::err!("Too many arguments"));
                }

                $f
            };

            #[allow(unused_variables, unused_mut)]
            let check_args = |mut args: Args| -> (Vec<Range<usize>>, Option<(Range<usize>, Text)>) {
                let start = args.next_start();
                if let (Ok(_), Some(start)) = (args.next_as::<super::Remainder>(), start) {
                    let err = $crate::text::err!("Too many arguments");
                    return (Vec::new(), Some((start..args.param_range().end, err)))
                }

                (Vec::new(), None)
            };

            add_inner($callers, cmd, check_args)
    	}}
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
    /// widgets, for example, one may have a [`PromptLine`] per
    /// [`File`], or one singular [`PromptLine`] that acts upon
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
    /// #     fn once() -> Result<(), Text> {
    /// #         Ok(())
    /// #     }
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
    ///     fn once() -> Result<(), Text> {
    ///         form::set_weak("Counter", Form::green());
    ///
    ///         cmd::add_for!("play", |timer: Timer, _: U::Area| {
    ///             timer.running.store(true, Ordering::Relaxed);
    ///             Ok(None)
    ///         })?;
    ///
    ///         cmd::add_for!("pause", |timer: Timer, _: U::Area| {
    ///             timer.running.store(false, Ordering::Relaxed);
    ///             Ok(None)
    ///         })?;
    ///
    ///         cmd::add_for!("reset", |timer: Timer, _: U::Area| {
    ///             timer.instant = Instant::now();
    ///             Ok(None)
    ///         })
    ///     }
    /// }
    /// ```
    ///
    /// I also added a [`Form`] in the [`once`] function. You should
    /// use [`form::set_weak`] instead of [`form::set`], as to not
    /// interfere with the user configuration.
    ///
    /// [`dyn Area`]: crate::ui::Area
    /// [`File`]: crate::widgets::File
    /// [`Session`]: crate::session::Session
    /// [`PromptLine`]: crate::widgets::PromptLine
    /// [`once`]: Widget::once
    /// [`Form`]: crate::form::Form
    /// [`form::set`]: crate::form::set
    /// [`form::set_weak`]: crate::form::set_weak
    pub macro add_for($callers:expr, $($mv:ident)? |
        $widget:tt: $w_ty:ty, $area:tt: $a_ty:ty $(, $arg:tt: $t:ty)* $(,)?
    | $f:block) {{
        #[allow(unused_variables, unused_mut)]
        let cmd = $($mv)? |
            $widget: &mut $w_ty,
            $area: &$a_ty,
            mut args: Args
        | -> CmdResult {
            $(
                let $arg: <$t as Parameter>::Returns = <$t as Parameter>::new(&mut args)?;
            )*

            if let Ok(arg) = args.next() {
                return Err($crate::text::err!("Too many arguments"));
            }

            $f
        };

        #[allow(unused_variables, unused_mut)]
        let check_args = |mut args: Args| -> (Vec<Range<usize>>, Option<(Range<usize>, Text)>) {
            let mut ok_ranges = Vec::new();

            $(
                let start = args.next_start();
                match args.next_as::<$t>() {
                    Ok(_) => if let Some(start) = start
                        .filter(|s| args.param_range().end > *s)
                    {
                        ok_ranges.push(start..args.param_range().end);
                    }
                    Err(err) => return (ok_ranges, Some((args.param_range(), err)))
                }
            )*

            let start = args.next_start();
            if let (Ok(_), Some(start)) = (args.next_as::<super::Remainder>(), start) {
                let err = $crate::text::err!("Too many arguments");
                return (ok_ranges, Some((start..args.param_range().end, err)))
            }

            (ok_ranges, None)
        };

        add_for_inner::<$w_ty, <$a_ty as $crate::ui::RawArea>::Ui>($callers, cmd, check_args)
    }}

    /// Canonical way to quit Duat.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Duat will execute a program closing
    /// function, as defined by the [`Ui`].
    pub fn quit() {
        COMMANDS.run("quit").unwrap();
    }

    /// Switches to/opens a [`File`] with the given name.
    ///
    /// If you wish to specifically switch to files that are already
    /// open, use [`buffer`].
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    pub fn edit(file: impl std::fmt::Display) -> Result<Option<Text>, Text> {
        COMMANDS.run(format!("edit {file}"))
    }

    /// Switches to a [`File`] with the given name.
    ///
    /// If there is no file open with that name, does nothing. Use
    /// [`edit`] if you wish to open files.
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    pub fn buffer(file: impl std::fmt::Display) -> Result<Option<Text>, Text> {
        COMMANDS.run(format!("buffer {file}"))
    }

    /// Switches to the next [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`next_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn next_file() -> Result<Option<Text>, Text> {
        COMMANDS.run("next-file")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`prev_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn prev_file() -> Result<Option<Text>, Text> {
        COMMANDS.run("prev-file")
    }

    /// Switches to the next [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`next_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn next_global_file() -> Result<Option<Text>, Text> {
        COMMANDS.run("next-file --global")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`prev_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn prev_global_file() -> Result<Option<Text>, Text> {
        COMMANDS.run("prev-file --global")
    }

    /// Tries to alias a `caller` to an existing `command`.
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// existing command.
    pub fn alias(alias: impl ToString, command: impl ToString) -> Result<Option<Text>, Text> {
        COMMANDS.alias(alias, command)
    }

    /// A command executed via [`run`] or [`run_notify`]
    ///
    /// Since the command will only be executed once [`CmdRunner`] is
    /// dropped, I would suggest that you shouldn't store it in a
    /// variable.
    pub struct CmdRunner {
        cmd: Option<Box<dyn FnOnce() -> Result<Option<Text>, Text> + Send + Sync + 'static>>,
        map: Vec<Box<dyn FnOnce(&Option<Text>) + Send + Sync + 'static>>,
        map_err: Vec<Box<dyn FnOnce(&Text) + Send + Sync + 'static>>,
    }

    impl CmdRunner {
        /// Maps the [`Ok`] result of this commmand.
        ///
        /// This is a convenient way of executing functions after a
        /// command finished executing successfully.
        pub fn map(mut self, f: impl FnOnce(&Option<Text>) + Send + Sync + 'static) -> Self {
            self.map.push(Box::new(f));
            self
        }

        /// Maps the [`Err`] result of this commmand.
        ///
        /// This is a convenient way to add fallbacks in case the
        /// command fails for some reason or another.
        pub fn map_err(mut self, f: impl FnOnce(&Text) + Send + Sync + 'static) -> Self {
            self.map_err.push(Box::new(f));
            self
        }
    }

    impl Drop for CmdRunner {
        fn drop(&mut self) {
            let cmd = self.cmd.take().unwrap();
            let map = std::mem::take(&mut self.map);
            let map_err = std::mem::take(&mut self.map_err);
            crate::thread::spawn(move || match cmd() {
                Ok(ok) => map.into_iter().for_each(|f| f(&ok)),
                Err(err) => map_err.into_iter().for_each(|f| f(&err)),
            });
        }
    }

    /// Runs a full command, with a caller and [`Args`].
    ///
    /// When running the command, the ordering of [`Flags`] does not
    /// matter, as long as they are placed before the arguments to the
    /// command.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::cmd;
    /// cmd::run("set-prompt new-prompt");
    /// ```
    ///
    /// In this case we're running a command that will affect the most
    /// relevant [`PromptLine`]. See [`add_for`] for
    /// more information.
    ///
    /// [`PromptLine`]: crate::widgets::PromptLine
    /// [`Flags`]: super::Flags
    pub fn run(call: impl std::fmt::Display) -> CmdRunner {
        let call = call.to_string();
        CmdRunner {
            cmd: Some(Box::new(move || COMMANDS.run(call))),
            map: Vec::new(),
            map_err: Vec::new(),
        }
    }

    /// Like [`run`], but notifies the result
    pub fn run_notify(call: impl std::fmt::Display) -> CmdRunner {
        let call = call.to_string();
        CmdRunner {
            cmd: Some(Box::new(move || COMMANDS.run_notify(call))),
            map: Vec::new(),
            map_err: Vec::new(),
        }
    }

    /// Don't call this function, use [`cmd::add`] instead
    ///
    /// [`cmd::add`]: add
    #[doc(hidden)]
    pub fn add_inner<'a>(
        callers: impl super::Caller<'a>,
        cmd: impl super::CmdFn,
        check_args: impl super::CheckerFn,
    ) -> Result<(), Text> {
        let callers: Vec<String> = callers.into_callers().map(str::to_string).collect();
        COMMANDS.add(callers, Box::new(cmd), Arc::new(check_args))
    }

    /// Don't call this function, use [`cmd::add_for`] instead
    ///
    /// [`cmd::add_for`]: add_for
    #[doc(hidden)]
    pub fn add_for_inner<'a, W: Widget<U>, U: Ui>(
        callers: impl super::Caller<'a>,
        cmd: impl super::ArgCmdFn<W, U>,
        check_args: impl super::CheckerFn,
    ) -> Result<(), Text> {
        let callers: Vec<String> = callers.into_callers().map(str::to_string).collect();
        COMMANDS.add_for(callers, cmd, Arc::new(check_args))
    }

    /// Check if the arguments for a given `caller` are correct
    pub fn check_args(caller: &str) -> Option<(Vec<Range<usize>>, Option<(Range<usize>, Text)>)> {
        COMMANDS.check_args(caller)
    }
}

/// A list of commands.
///
/// This list contains all of the commands that have been
/// added to Duat, as well as info on the current [`File`],
/// [widget] and all of the [windows].
///
/// [`File`]: crate::widgets::File
/// [widget]: crate::widgets::ActiveWidget
/// [windows]: crate::ui::Window
struct Commands(LazyLock<RwData<InnerCommands>>);

impl Commands {
    /// Returns a new instance of [`Commands`].
    #[doc(hidden)]
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            RwData::new(InnerCommands {
                list: Vec::new(),
                aliases: HashMap::new(),
            })
        }))
    }

    /// Aliases a command to a specific word
    fn alias(&self, alias: impl ToString, command: impl ToString) -> Result<Option<Text>, Text> {
        self.0
            .write()
            .try_alias(alias.to_string(), command.to_string())
    }

    /// Runs a command from a call
    fn run(&self, call: impl Display) -> Result<Option<Text>, Text> {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(err!("The command is empty"))?.to_string();

        let (command, call) = {
            let inner = self.0.read();
            if let Some(command) = inner.aliases.get(&caller) {
                let (command, call) = command;
                let mut call = call.clone() + " ";
                call.extend(args);

                (command.clone(), call)
            } else {
                let command = inner
                    .list
                    .iter()
                    .find(|cmd| cmd.callers().contains(&caller))
                    .ok_or(err!("The caller [a]{caller}[] was not found"))?;

                (command.clone(), call.clone())
            }
        };

        let args = get_args(&call);

        if let (_, Some((_, err))) = (command.check_args)(args.clone()) {
            return Err(err);
        }

        let silent = call.len() > call.trim_start().len();
        command.try_exec(args).map(|ok| ok.filter(|_| !silent))
    }

    /// Runs a command and notifies its result
    fn run_notify(&self, call: impl Display) -> Result<Option<Text>, Text> {
        let ret = self.run(call);
        match ret.as_ref() {
            Ok(Some(ok)) => context::notify(ok.clone()),
            Err(err) => context::notify(err.clone()),
            _ => {}
        }
        ret
    }

    /// Adds a command to the list of commands
    fn add(
        &self,
        callers: Vec<String>,
        cmd: Box<dyn CmdFn>,
        check_args: Arc<dyn CheckerFn>,
    ) -> Result<(), Text> {
        let command = Command::new(callers, cmd, check_args);
        self.0.write().try_add(command)
    }

    /// Adds a command for a widget of type `W`
    fn add_for<W: Widget<U>, U: Ui>(
        &'static self,
        callers: Vec<String>,
        mut cmd: impl ArgCmdFn<W, U>,
        check_args: Arc<dyn CheckerFn>,
    ) -> Result<(), Text> {
        let cmd = Box::new(move |args: Args| {
            let cur_file = context::inner_cur_file::<U>();
            cur_file
                .mutate_related_widget::<W, CmdResult>(|widget, area| {
                    cmd(widget, area, args.clone())
                })
                .unwrap_or_else(|| {
                    let windows = context::windows::<U>().read();
                    let w = context::cur_window();

                    if windows.is_empty() {
                        return Err(err!(
                            "Widget command executed before the [a]Ui[] was initiated, try \
                             executing after [a]OnUiStart[]"
                        ));
                    }

                    let (.., node) = widget_entry::<W, U>(&windows, w)?;
                    let (w, a, _) = node.parts();
                    cmd(&mut w.write_as().unwrap(), a, args)
                })
        });

        let command = Command::new(callers, cmd, check_args);
        self.0.write().try_add(command)
    }

    /// Gets the parameter checker for a command, if it exists
    fn check_args(&self, call: &str) -> Option<(Vec<Range<usize>>, Option<(Range<usize>, Text)>)> {
        let mut args = call.split_whitespace();
        let caller = args.next()?.to_string();

        let inner = self.0.read();
        if let Some((command, _)) = inner.aliases.get(&caller) {
            Some((command.check_args)(get_args(call)))
        } else {
            let command = inner
                .list
                .iter()
                .find(|cmd| cmd.callers().contains(&caller))?;

            Some((command.check_args)(get_args(call)))
        }
    }
}

/// The standard error that should be returned when [`run`]ning
/// commands.
///
/// This error _must_ include an error message in case of failure. It
/// may also include a success message, but that is not required.
pub type CmdResult = std::result::Result<Option<Text>, Text>;

/// A function that can be called by name.
#[derive(Clone)]
struct Command {
    callers: Arc<[String]>,
    cmd: RwData<Box<dyn CmdFn>>,
    check_args: Arc<dyn CheckerFn>,
}

impl Command {
    /// Returns a new instance of command.
    fn new(callers: Vec<String>, cmd: Box<dyn CmdFn>, check_args: Arc<dyn CheckerFn>) -> Self {
        if let Some(caller) = callers
            .iter()
            .find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{caller}\" contains more than one word");
        }
        Self {
            cmd: RwData::new(cmd),
            check_args,
            callers: callers.into(),
        }
    }

    /// Executes the inner function if the `caller` matches any of
    /// the callers in [`self`].
    fn try_exec(&self, args: Args<'_>) -> Result<Option<Text>, Text> {
        (self.cmd.write())(args)
    }

    /// The list of callers that will trigger this command.
    fn callers(&self) -> &[String] {
        &self.callers
    }
}

struct InnerCommands {
    list: Vec<Command>,
    aliases: HashMap<String, (Command, String)>,
}

impl InnerCommands {
    /// Tries to add the given command to the list.
    fn try_add(&mut self, command: Command) -> Result<(), Text> {
        let mut new_callers = command.callers().iter();

        let commands = self.list.iter();
        for caller in commands.flat_map(|cmd| cmd.callers().iter()) {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(err!("The caller [a]{caller}[] already exists"));
            }
        }

        self.list.push(command);

        Ok(())
    }

    /// Tries to alias a full command (caller, flags, and
    /// arguments) to an alias.
    fn try_alias(&mut self, alias: String, call: String) -> Result<Option<Text>, Text> {
        if alias.split_whitespace().count() != 1 {
            return Err(err!("Alias [a]{alias}[] is not a single word"));
        }

        let caller = call
            .split_whitespace()
            .next()
            .ok_or(err!("The command is empty"))?
            .to_string();

        let mut cmds = self.list.iter();

        if let Some(command) = cmds.find(|cmd| cmd.callers().contains(&caller)) {
            let entry = (command.clone(), call.clone());
            Ok(Some(match self.aliases.insert(alias.clone(), entry) {
                Some((_, prev_call)) => {
                    ok!("Aliased [a]{alias}[] from [a]{prev_call}[] to [a]{call}")
                }
                None => ok!("Aliased [a]{alias}[] to [a]{call}"),
            }))
        } else {
            Err(err!("The caller [a]{caller}[] was not found"))
        }
    }
}

pub trait Caller<'a>: Sized {
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

trait CmdFn = FnMut(Args) -> CmdResult + 'static + Send + Sync;
trait ArgCmdFn<W, U: Ui> = FnMut(&mut W, &U::Area, Args) -> CmdResult + 'static + Send + Sync;
trait CheckerFn =
    Fn(Args) -> (Vec<Range<usize>>, Option<(Range<usize>, Text)>) + 'static + Send + Sync;
