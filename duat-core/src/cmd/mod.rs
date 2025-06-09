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
//! [`PromptLine`]: crate::widget::PromptLine
//! [`cmd::run_notify`]: run_notify
//! [`Color`]: crate::form::Color
//! [`Widget`]: crate::widget::Widget
//! [`File`]: crate::file::File
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
        Args, Between, Buffer, ColorSchemeArg, F32PercentOfU8, Flags, FormName, OtherFileBuffer,
        Parameter, PossibleFile, Remainder, args_iter, get_args,
    },
};
use crate::{
    context,
    context::sender,
    data::{Pass, RwData},
    file::File,
    file_entry, iter_around, iter_around_rev, mode,
    text::{Text, err, hint, ok},
    ui::{DuatEvent, Ui},
};

mod parameters;

pub(crate) fn add_session_commands<U: Ui>() -> Result<(), Text> {
    add!("alias", |pa,
                   flags: Flags,
                   alias: &str,
                   command: Remainder| {
        if !flags.is_empty() {
            Err(err!("An alias cannot take any flags").build())
        } else {
            crate::cmd::alias(&mut pa, alias, command)
        }
    })?;

    add!(["quit", "q"], |pa, name: Option<Buffer<U>>| {
        let cur_name = context::fixed_file::<U>(&pa)?.read(&pa, |file, _| file.name());
        let name = name.unwrap_or(&cur_name);

        let windows = context::windows::<U>().borrow();
        let (win, wid, file) = file_entry(&pa, &windows, name).unwrap();

        let has_unsaved_changes = file
            .read_as(&pa, |f: &File<U>| {
                f.text().has_unsaved_changes() && f.exists()
            })
            .unwrap();
        if has_unsaved_changes {
            return Err(err!("[a]{name}[] has unsaved changes").build());
        }

        // If we are on the current File, switch to the next one.
        if name == cur_name {
            let Some(next_name) = iter_around::<U>(&windows, win, wid)
                .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
            else {
                sender().send(DuatEvent::Quit).unwrap();
                return Ok(None);
            };

            // If I send the switch signal first, and the Window is deleted, I
            // will have the synchronously change the current window number
            // without affecting anything else.
            mode::reset_switch_to::<U>(&pa, &next_name, true);
        }

        sender()
            .send(DuatEvent::CloseFile(name.to_string()))
            .unwrap();
        Ok(Some(ok!("Closed [a]{name}").build()))
    })?;

    add!(["quit!", "q!"], |pa, name: Option<Buffer<U>>| {
        let cur_name = context::fixed_file::<U>(&pa)?.read(&pa, |file, _| file.name());
        let name = name.unwrap_or(&cur_name);

        // Should wait here until I'm out of `session_loop`
        let windows = context::windows::<U>().borrow();
        let (win, wid, file) = file_entry(&pa, &windows, name).unwrap();

        if name == cur_name {
            let Some(next_name) = iter_around::<U>(&windows, win, wid)
                .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
            else {
                sender().send(DuatEvent::Quit).unwrap();
                return Ok(None);
            };
            mode::reset_switch_to::<U>(&pa, &next_name, true);
        }

        sender()
            .send(DuatEvent::CloseFile(name.to_string()))
            .unwrap();
        Ok(Some(ok!("Closed [a]{name}").build()))
    })?;

    add!(["quit-all", "qa"], |pa| {
        let windows = context::windows::<U>().borrow();
        let unwritten = windows
            .iter()
            .flat_map(|w| w.file_nodes(&pa))
            .filter(|(node, _)| {
                node.read_as(&pa, |f: &File<U>| {
                    f.text().has_unsaved_changes() && f.exists()
                })
                .unwrap()
            })
            .count();

        if unwritten == 0 {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else if unwritten == 1 {
            Err(err!("There is [a]1[] unsaved file").build())
        } else {
            Err(err!("There are [a]{unwritten}[] unsaved files").build())
        }
    })?;

    add!(["quit-all!", "qa!"], |_dk| {
        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    })?;

    add!(["write", "w"], |pa, path: Option<PossibleFile>| {
        context::fixed_file::<U>(&pa)?.write(&mut pa, |file, _| {
            let (bytes, name) = if let Some(path) = path {
                (file.write_to(&path)?, path)
            } else if let Some(name) = file.name_set() {
                (file.write()?, std::path::PathBuf::from(name))
            } else {
                return Err(err!("File has no name path to write to").build());
            };

            match bytes {
                Some(bytes) => Ok(Some(
                    ok!("Wrote [a]{bytes}[] bytes to [File]{name}").build(),
                )),
                None => Ok(Some(ok!("Nothing to be written").build())),
            }
        })
    })?;

    add!(["write-quit", "wq"], |pa, path: Option<PossibleFile>| {
        let mut ff = context::fixed_file::<U>(&pa)?;
        let (bytes, name) = ff.write(&mut pa, |file, _| {
            let bytes = if let Some(path) = path {
                file.write_to(&path)?
            } else {
                file.write()?
            };
            Result::<_, Text>::Ok((bytes, file.name()))
        })?;

        // Should wait here until I'm out of `session_loop`
        let windows = context::windows::<U>().borrow();
        let w = context::cur_window();

        let (win, wid, file) = file_entry(&pa, &windows, &name).unwrap();

        let Some(next_name) = iter_around::<U>(&windows, win, wid)
            .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
        else {
            sender().send(DuatEvent::Quit).unwrap();
            return Ok(None);
        };

        mode::reset_switch_to::<U>(&pa, &next_name, true);

        sender().send(DuatEvent::CloseFile(name.clone())).unwrap();
        match bytes {
            Some(bytes) => Ok(Some(
                ok!("Wrote [a]{bytes}[] bytes to [File]{name}[], then closed it").build(),
            )),
            None => Ok(Some(
                ok!("No changes in [File]{name}[], so just closed it").build(),
            )),
        }
    })?;

    add!(["write-all", "wa"], |pa| {
        let windows = context::windows::<U>().borrow();

        let mut written = 0;
        let file_count = windows
            .iter()
            .flat_map(|w| w.file_nodes(&pa))
            .filter(|(node, _)| {
                node.widget()
                    .read_as(&pa, |f: &File<U>| f.path_set().is_some())
                    == Some(true)
            })
            .inspect(|(node, _)| {
                // SAFETY: It is known that this function does not have any inner
                // RwData.
                written += unsafe {
                    node.widget()
                        .write_unsafe_as(|f: &mut File<U>| f.write().is_ok())
                        .unwrap() as usize
                };
            })
            .count();

        if written == file_count {
            Ok(Some(ok!("Wrote to [a]{written}[] files").build()))
        } else {
            let unwritten = file_count - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(err!("Failed to write to [a]{unwritten}[] file{plural}").build())
        }
    })?;

    add!(["write-all-quit", "waq"], |pa| {
        let windows = context::windows::<U>().borrow();

        let mut written = 0;
        let file_count = windows
            .iter()
            .flat_map(|w| w.file_nodes(&pa))
            .filter(|(node, _)| {
                node.widget()
                    .read_as(&pa, |f: &File<U>| f.path_set().is_some())
                    == Some(true)
            })
            .inspect(|(node, _)| {
                // SAFETY: It is known that this function does not have any inner
                // RwData.
                written += unsafe {
                    node.widget()
                        .write_unsafe_as(|f: &mut File<U>| f.write().is_ok())
                        .unwrap() as usize
                };
            })
            .count();

        if written == file_count {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else {
            let unwritten = file_count - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(err!("Failed to write to [a]{unwritten}[] file{plural}").build())
        }
    })?;

    add!(["write-all-quit!", "waq!"], |pa| {
        let windows = context::windows::<U>().borrow();

        for (node, _) in windows.iter().flat_map(|w| w.file_nodes(&pa)) {
            // SAFETY: It is known that this function does not have any inner
            // RwData.
            unsafe {
                node.widget().write_unsafe_as(|f: &mut File<U>| f.write());
            }
        }

        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    })?;

    add!(["reload"], |_dk| {
        let Some(toml_path) = crate::crate_dir()
            .map(|config_dir| config_dir.join("Cargo.toml"))
            .filter(|path| path.try_exists().is_ok_and(|exists| exists))
        else {
            return Err(err!("Cargo.toml was not found").build());
        };

        let mut cargo = std::process::Command::new("cargo");
        cargo.stdin(std::process::Stdio::null());
        cargo.stdout(std::process::Stdio::null());
        cargo.stderr(std::process::Stdio::null());
        cargo.args([
            "build",
            "--quiet",
            "--release",
            "--manifest-path",
            toml_path.to_str().unwrap(),
        ]);
        //if !cfg!(debug_assertions) {
        //    cargo.arg("--release");
        //};

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
                Ok(Some(hint!("Started config recompilation").build()))
            }
            Err(err) => Err(err!("Failed to start cargo: [a]{err}").build()),
        }
    })?;

    add!(["edit", "e"], |pa, path: PossibleFile| {
        let windows = context::windows::<U>().borrow();

        let name = if let Ok(path) = path.strip_prefix(context::cur_dir()) {
            path.to_string_lossy().to_string()
        } else {
            path.to_string_lossy().to_string()
        };

        if file_entry(&pa, &windows, &name).is_err() {
            sender().send(DuatEvent::OpenFile(name.clone())).unwrap();
            return Ok(Some(ok!("Opened [a]{name}").build()));
        }

        mode::reset_switch_to::<U>(&pa, name.clone(), true);
        Ok(Some(ok!("Switched to [a]{name}").build()))
    })?;

    add!(["open", "o"], |pa, path: PossibleFile| {
        let windows = context::windows::<U>().borrow();

        let name = if let Ok(path) = path.strip_prefix(context::cur_dir()) {
            path.to_string_lossy().to_string()
        } else {
            path.to_string_lossy().to_string()
        };

        let Ok((win, wid, node)) = file_entry(&pa, &windows, &name) else {
            sender().send(DuatEvent::OpenWindow(name.clone())).unwrap();
            return Ok(Some(ok!("Opened [a]{name}[] on new window").build()));
        };

        if windows[win].file_nodes(&pa).len() == 1 {
            mode::reset_switch_to::<U>(&pa, name.clone(), true);
            Ok(Some(ok!("Switched to [a]{name}").build()))
        } else {
            sender().send(DuatEvent::OpenWindow(name.clone())).unwrap();
            Ok(Some(ok!("Moved [a]{name}[] to a new window").build()))
        }
    })?;

    add!(["buffer", "b"], |pa, name: OtherFileBuffer<U>| {
        mode::reset_switch_to::<U>(&pa, &name, true);
        Ok(Some(ok!("Switched to [a]{name}").build()))
    })?;

    add!("next-file", |pa, flags: Flags| {
        let windows = context::windows().borrow();
        let ff = context::fixed_file::<U>(&pa)?;
        let win = context::cur_window();

        let wid = windows[win]
            .nodes()
            .position(|node| ff.ptr_eq(&pa, node.widget()))
            .unwrap();

        let name = if flags.word("global") {
            iter_around::<U>(&windows, win, wid)
                .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
                .ok_or_else(|| err!("There are no other open files"))?
        } else {
            let slice = &windows[win..=win];
            iter_around(slice, 0, wid)
                .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
                .ok_or_else(|| err!("There are no other files open in this window"))?
        };

        mode::reset_switch_to::<U>(&pa, &name, true);
        Ok(Some(ok!("Switched to [a]{name}").build()))
    })?;

    add!("prev-file", |pa, flags: Flags| {
        let windows = context::windows().borrow();
        let ff = context::fixed_file::<U>(&pa)?;
        let w = context::cur_window();

        let widget_i = windows[w]
            .nodes()
            .position(|node| ff.ptr_eq(&pa, node.widget()))
            .unwrap();

        let name = if flags.word("global") {
            iter_around_rev::<U>(&windows, w, widget_i)
                .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
                .ok_or_else(|| err!("There are no other open files"))?
        } else {
            let slice = &windows[w..=w];
            iter_around_rev(slice, 0, widget_i)
                .find_map(|(.., node)| node.read_as(&pa, |f: &File<U>| f.name()))
                .ok_or_else(|| err!("There are no other files open in this window"))?
        };

        mode::reset_switch_to::<U>(&pa, &name, true);

        Ok(Some(ok!("Switched to [a]{name}").build()))
    })?;

    add!("swap", |pa, lhs: Buffer<U>, rhs: Option<Buffer<U>>| {
        let rhs = if let Some(rhs) = rhs {
            rhs.to_string()
        } else {
            context::fixed_file::<U>(&pa)?.read(&pa, |file, _| file.name())
        };
        sender()
            .send(DuatEvent::SwapFiles(lhs.to_string(), rhs.clone()))
            .unwrap();

        Ok(Some(ok!("Swapped [a]{lhs}[] and [a]{rhs}").build()))
    })?;

    add!("colorscheme", |_dk, scheme: ColorSchemeArg| {
        crate::form::set_colorscheme(scheme);
        Ok(Some(ok!("Set colorscheme to [a]{scheme}[]").build()))
    })?;

    add!(
        "set-form",
        |_dk, name: FormName, colors: Between<0, 3, Color>| {
            let mut form = crate::form::Form::new();
            form.style.foreground_color = colors.first().cloned();
            form.style.background_color = colors.get(1).cloned();
            form.style.underline_color = colors.get(2).cloned();
            crate::form::set(name, form);

            Ok(Some(ok!("Set [a]{name}[] to a new Form").build()))
        }
    )?;

    Ok(())
}

mod global {
    use std::{cell::RefCell, ops::Range, rc::Rc};

    use super::{Args, CheckerFn, CmdFn, CmdResult, Commands, Parameter};
    use crate::{
        context,
        data::{Pass, RwData},
        main_thread_only::MainThreadOnly,
        text::Text,
        ui::DuatEvent,
    };

    static COMMANDS: MainThreadOnly<Commands> = MainThreadOnly::new(Commands::new());

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
    /// [`StatusLine`]: crate::widget::StatusLine
    /// [`RwData`]: crate::data::RwData
    pub macro add(
        $callers:expr, |$pa:ident $(: Pass)? $(, $arg:tt: $t:ty)* $(,)?| $f:block
    ) {{
        #[allow(unused_variables, unused_mut)]
        let cmd = move |pa: $crate::data::Pass, mut args: Args| -> CmdResult {
            $(
                let $arg: <$t as Parameter>::Returns = <$t as Parameter>::new(&pa, &mut args)?;
            )*

            if let Ok(arg) = args.next() {
                return Err($crate::text::err!("Too many arguments").build());
            }

            let mut $pa = pa;

            $f
        };

        #[allow(unused_variables, unused_mut)]
        let check_args = |pa: &$crate::data::Pass, mut args: Args| {
            let mut ok_ranges = Vec::new();

            $(
                let start = args.next_start();
                match args.next_as::<$t>(pa) {
                    Ok(_) => if let Some(start) = start
                        .filter(|s| args.param_range().end > *s)
                    {
                        ok_ranges.push(start..args.param_range().end);
                    }
                    Err(err) => return (ok_ranges, Some((args.param_range(), err)))
                }
            )*

            let start = args.next_start();
            if let (Ok(_), Some(start)) = (args.next_as::<super::Remainder>(pa), start) {
                let err = $crate::text::err!("Too many arguments").build();
                return (ok_ranges, Some((start..args.param_range().end, err)))
            }

            (ok_ranges, None)
        };

        let callers: Vec<String> = $callers.into_callers().map(str::to_string).collect();
        // SAFETY: This type will never actually be queried
        let cmd: CmdFn = unsafe { RwData::new_unsized::<()>(Rc::new(RefCell::new(cmd))) };

        add_inner(callers, cmd, check_args)
    }}

    /// Canonical way to quit Duat.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Duat will execute a program closing
    /// function, as defined by the [`Ui`].
    pub fn quit() {
        queue("quit");
    }

    /// Switches to/opens a [`File`] with the given name.
    ///
    /// If you wish to specifically switch to files that are already
    /// open, use [`buffer`].
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::file::File
    pub fn edit(pa: &mut Pass, file: impl std::fmt::Display) -> CmdResult {
        call(pa, format!("edit {file}"))
    }

    /// Switches to a [`File`] with the given name.
    ///
    /// If there is no file open with that name, does nothing. Use
    /// [`edit`] if you wish to open files.
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::file::File
    pub fn buffer(pa: &mut Pass, file: impl std::fmt::Display) -> CmdResult {
        call(pa, format!("buffer {file}"))
    }

    /// Switches to the next [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`next_global_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn next_file(pa: &mut Pass) -> CmdResult {
        call(pa, "next-file")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`prev_global_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn prev_file(pa: &mut Pass) -> CmdResult {
        call(pa, "prev-file")
    }

    /// Switches to the next [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`next_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn next_global_file(pa: &mut Pass) -> CmdResult {
        call(pa, "next-file --global")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`prev_file`].
    ///
    /// [`File`]: crate::file::File
    pub fn prev_global_file(pa: &mut Pass) -> CmdResult {
        call(pa, "prev-file --global")
    }

    /// Tries to alias a `caller` to an existing `command`.
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// existing command.
    pub fn alias(pa: &mut Pass, alias: impl ToString, command: impl ToString) -> CmdResult {
        // SAFETY: Function has a Pass argument.
        unsafe { COMMANDS.get() }.alias(pa, alias, command)
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
    /// [`PromptLine`]: crate::widget::PromptLine
    /// [`Flags`]: super::Flags
    pub fn call(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        // SAFETY: Function has a Pass argument.
        unsafe { COMMANDS.get() }.run(pa, call)
    }

    /// Like [`call`], but notifies the result
    pub fn call_notify(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        // SAFETY: Function has a Pass argument.
        let result = unsafe { COMMANDS.get() }.run(pa, call);
        if let Ok(Some(result)) | Err(result) = result.clone() {
            context::notify(result);
        }
        result
    }

    /// Queues a command call
    ///
    /// You should use this if you are not in an async environment or
    /// on the main thread, and are thus unable to use the [`call`]
    /// function. Do note that this will not happen in sequence with
    /// the rest of your code.
    ///
    /// Since this function will run outside of the current scope, its
    /// [`Result`] will not be returned.
    pub fn queue(call: impl std::fmt::Display) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |mut pa| {
                // SAFETY: Closure has Pass argument.
                let _ = unsafe { COMMANDS.get() }.run(&mut pa, call);
            })))
            .unwrap();
    }

    /// Like [`queue`], but notifies the result
    pub fn queue_notify(call: impl std::fmt::Display) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |mut pa| {
                if let Ok(Some(res)) | Err(res) = unsafe { COMMANDS.get() }.run(&mut pa, call) {
                    context::notify(res);
                }
            })))
            .unwrap()
    }

    /// Like [`queue`], but acts on the [`Result`]
    pub fn queue_and(call: impl std::fmt::Display, map: impl FnOnce(CmdResult) + Send + 'static) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |mut pa| {
                // SAFETY: Function has a Pass argument.
                map(unsafe { COMMANDS.get() }.run(&mut pa, call));
            })))
            .unwrap()
    }

    /// Don't call this function, use [`cmd::add`] instead
    ///
    /// [`cmd::add`]: add
    #[doc(hidden)]
    pub fn add_inner(callers: Vec<String>, cmd: CmdFn, check_args: CheckerFn) -> Result<(), Text> {
        // SAFETY: There is no way to obtain an external RwData of Commands,
        // so you can modify it from anywhere in the main thread.
        let mut pa = unsafe { Pass::new() };
        context::assert_is_on_main_thread();
        unsafe { COMMANDS.get() }.add(&mut pa, callers, cmd, check_args)
    }

    /// Check if the arguments for a given `caller` are correct
    pub fn check_args(caller: &str) -> Option<(Vec<Range<usize>>, Option<(Range<usize>, Text)>)> {
        // SAFETY: There is no way to obtain an external RwData of Commands,
        // so you can modify it from anywhere in the main thread.
        let pa = unsafe { Pass::new() };
        context::assert_is_on_main_thread();
        unsafe { COMMANDS.get() }.check_args(&pa, caller)
    }
}

/// A list of commands.
///
/// This list contains all of the commands that have been
/// added to Duat, as well as info on the current [`File`],
/// [widget] and all of the [windows].
///
/// [`File`]: crate::file::File
/// [widget]: crate::widget::Widget
/// [windows]: crate::ui::Window
struct Commands(LazyLock<RwData<InnerCommands>>);

impl Commands {
    /// Returns a new instance of [`Commands`].
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            RwData::new(InnerCommands {
                list: Vec::new(),
                aliases: HashMap::new(),
            })
        }))
    }

    /// Aliases a command to a specific word
    fn alias(&self, pa: &mut Pass, alias: impl ToString, command: impl ToString) -> CmdResult {
        self.0.write(pa, |inner| {
            inner.try_alias(alias.to_string(), command.to_string())
        })
    }

    /// Runs a command from a call
    fn run(&self, _: &mut Pass, call: impl Display) -> CmdResult {
        // SAFETY: &mut Pass is an argument.
        let pa = unsafe { Pass::new() };

        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(err!("The command is empty"))?.to_string();

        let (command, call) = unsafe {
            self.0.read_unsafe(|inner| {
                Result::<_, Text>::Ok(if let Some(command) = inner.aliases.get(&caller) {
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
                })
            })?
        };

        let args = get_args(&call);

        if let (_, Some((_, err))) = (command.check_args)(&pa, args.clone()) {
            return Err(err);
        }

        let silent = call.len() > call.trim_start().len();
        command.cmd.acquire_mut(&mut unsafe { Pass::new() })(pa, args)
            .map(|ok| ok.filter(|_| !silent))
    }

    /// Adds a command to the list of commands
    fn add(
        &self,
        pa: &mut Pass,
        callers: Vec<String>,
        cmd: CmdFn,
        check_args: CheckerFn,
    ) -> Result<(), Text> {
        let cmd = Command::new(callers, cmd, check_args);
        self.0.write(pa, |c| c.try_add(cmd))
    }

    // fn add_for<W: Widget<U>, U: Ui>(
    //     &self,
    //     callers: Vec<String>,
    //     mut cmd: impl FnMut(&mut W, &U::Area, Args) -> CmdResult +
    // 'static,     check_args: CheckerFn,
    // ) -> Result<(), Text> {
    //     let f = move |args: Args| {
    //         let mut cur_file = context::inner_cur_file::<U>().clone();
    //         if let Some((widget, area)) =
    // cur_file.get_related_widget::<W>() {             cmd(&mut
    // *widget.acquire_mut(), &area, args)         } else {
    //             let windows = context::windows::<U>().borrow();
    //             let w = context::cur_window();

    //             if windows.is_empty() {
    //                 return Err(err!(
    //                     "Widget command executed before the [a]Ui[] was
    // initiated, try executing \                      after
    // [a]OnUiStart[]"                 )
    //                 .build());
    //             }

    //             let node = match widget_entry::<W, U>(&windows, w) {
    //                 Ok((.., node)) => node,
    //                 Err(err) => return Err(err),
    //             };
    //             let (w, a, _) = node.parts();
    //             let widget = w.try_downcast().unwrap();
    //             let area = a.clone();

    //             cmd(&mut *widget.acquire_mut(), &area, args)
    //         }
    //     };

    //     let command = Command::new(
    //         callers,
    //         // SAFETY: This type will never actually be queried
    //         unsafe {
    // RwData::new_unsized::<()>(Rc::new(RefCell::new(f))) },
    //         check_args,
    //     );
    //     self.0.write(|inner| inner.try_add(command))
    // }

    /// Gets the parameter checker for a command, if it exists
    fn check_args(
        &self,
        pa: &Pass,
        call: &str,
    ) -> Option<(Vec<Range<usize>>, Option<(Range<usize>, Text)>)> {
        let mut args = call.split_whitespace();
        let caller = args.next()?.to_string();

        self.0.read(pa, |inner| {
            if let Some((command, _)) = inner.aliases.get(&caller) {
                Some((command.check_args)(pa, get_args(call)))
            } else {
                let command = inner
                    .list
                    .iter()
                    .find(|cmd| cmd.callers().contains(&caller))?;

                Some((command.check_args)(pa, get_args(call)))
            }
        })
    }
}

/// The standard error that should be returned when [`run`]ning
/// commands.
///
/// This error _must_ include an error message in case of failure. It
/// may also include a success message, but that is not required.
pub type CmdResult = Result<Option<Text>, Text>;

/// A function that can be called by name.
#[derive(Clone)]
struct Command {
    callers: Arc<[String]>,
    cmd: CmdFn,
    check_args: CheckerFn,
}

impl Command {
    /// Returns a new instance of command.
    fn new(callers: Vec<String>, cmd: CmdFn, check_args: CheckerFn) -> Self {
        if let Some(caller) = callers
            .iter()
            .find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{caller}\" contains more than one word");
        }
        Self { cmd, check_args, callers: callers.into() }
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
                return Err(err!("The caller [a]{caller}[] already exists").build());
            }
        }

        self.list.push(command);

        Ok(())
    }

    /// Tries to alias a full command (caller, flags, and
    /// arguments) to an alias.
    fn try_alias(&mut self, alias: String, call: String) -> Result<Option<Text>, Text> {
        if alias.split_whitespace().count() != 1 {
            return Err(err!("Alias [a]{alias}[] is not a single word").build());
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
                    ok!("Aliased [a]{alias}[] from [a]{prev_call}[] to [a]{call}").build()
                }
                None => ok!("Aliased [a]{alias}[] to [a]{call}").build(),
            }))
        } else {
            Err(err!("The caller [a]{caller}[] was not found").build())
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

type CmdFn = RwData<dyn FnMut(Pass, Args) -> CmdResult + 'static>;
type CheckerFn = fn(&Pass, Args) -> (Vec<Range<usize>>, Option<(Range<usize>, Text)>);
