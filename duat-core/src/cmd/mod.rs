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
//! and lightness arguments. It also shows two flag arguments, word
//! flags (`"--flag"`) and blob flags (`"-abc"`). Even failing, no
//! notification will be sent, because I called [`queue`], if you want
//! notifications, call [`queue_notify`].
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
//! cmd::alias("uf", "unset-form").unwrap();
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
    sync::{LazyLock, Mutex},
};

use crossterm::style::Color;

pub use self::{global::*, parameters::*};
use crate::{
    buffer::{Buffer, PathKind},
    context::{self, Handle, sender},
    data::{Pass, RwData},
    form::FormId,
    mode,
    session::DuatEvent,
    text::{Text, txt},
    ui::{Node, Widget},
    utils::catch_panic,
};

mod parameters;

/// Adds all the usual session commands
pub(crate) fn add_session_commands() {
    use crate::cmd::alias;

    add(
        "alias",
        |_: &mut Pass, alias: String, command: Remainder| crate::cmd::alias(alias, command.0),
    );

    add("write", |pa: &mut Pass, path: Option<ValidFilePath>| {
        let handle = context::current_buffer(pa).clone();
        let buffer = handle.write(pa);

        let (bytes, name) = if let Some(path) = path {
            (buffer.save_to(&path.0)?, path.0)
        } else if let Some(name) = buffer.name_set() {
            (buffer.save()?, std::path::PathBuf::from(name))
        } else {
            return Err(txt!("Buffer has no name path to write to"));
        };

        match bytes {
            Some(bytes) => Ok(Some(txt!("Wrote [a]{bytes}[] bytes to [buffer]{name}"))),
            None => Ok(Some(txt!("Nothing to be written"))),
        }
    });
    alias("w", "write").unwrap();

    add(
        "write-quit",
        |pa: &mut Pass, path: Option<ValidFilePath>| {
            let handle = context::current_buffer(pa).clone();

            let (bytes, name) = {
                let buffer = handle.write(pa);
                let bytes = if let Some(path) = path {
                    buffer.save_quit_to(path.0, true)?
                } else {
                    buffer.save_quit(true)?
                };
                (bytes, buffer.name())
            };

            context::windows().close(pa, &handle)?;

            match bytes {
                Some(bytes) => Ok(Some(txt!(
                    "Closed [buffer]{name}[], writing [a]{bytes}[] bytes"
                ))),
                None => Ok(Some(txt!("Closed [buffer]{name}[]"))),
            }
        },
    );
    alias("wq", "write-quit").unwrap();

    add("write-all", |pa: &mut Pass| {
        let windows = context::windows();

        let mut written = 0;
        let handles: Vec<_> = windows
            .buffers(pa)
            .filter(|handle| handle.read(pa).path_set().is_some())
            .collect();

        for handle in &handles {
            written += handle.write(pa).save().is_ok() as usize;
        }

        if written == handles.len() {
            Ok(Some(txt!("Wrote to [a]{written}[] buffers")))
        } else {
            let unwritten = handles.len() - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(txt!("Failed to write to [a]{unwritten}[] buffer{plural}"))
        }
    });
    alias("wa", "write-all").unwrap();

    add("write-all-quit", |pa: &mut Pass| {
        let windows = context::windows();

        let mut written = 0;
        let handles: Vec<_> = windows
            .buffers(pa)
            .filter(|handle| handle.read(pa).path_set().is_some())
            .collect();
        for handle in &handles {
            written += handle.write(pa).save_quit(true).is_ok() as usize;
        }

        if written == handles.len() {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else {
            let unwritten = handles.len() - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(txt!("Failed to write to [a]{unwritten}[] buffer{plural}"))
        }
    });
    alias("waq", "write-all-quit").unwrap();

    add("write-all-quit!", |pa: &mut Pass| {
        let handles: Vec<_> = context::windows().buffers(pa).collect();

        for handle in handles {
            let _ = handle.write(pa).save_quit(true);
        }

        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    });
    alias("waq!", "write-all-quit!").unwrap();

    add("quit", |pa: &mut Pass, handle: Option<Handle>| {
        let handle = match handle {
            Some(handle) => handle,
            None => context::current_buffer(pa).clone(),
        };

        let buffer = handle.read(pa);
        if buffer.text().has_unsaved_changes() && buffer.exists() {
            return Err(txt!("{} has unsaved changes", buffer.name()));
        }

        context::windows().close(pa, &handle)?;

        Ok(Some(txt!("Closed [buffer]{}", handle.read(pa).name())))
    });
    alias("q", "quit").unwrap();

    add("quit!", |pa: &mut Pass, handle: Option<Handle>| {
        let handle = match handle {
            Some(handle) => handle,
            None => context::current_buffer(pa).clone(),
        };

        context::windows().close(pa, &handle)?;

        Ok(Some(txt!("Forcefully closed {}", handle.read(pa).name())))
    });
    alias("q!", "quit!").unwrap();

    add("quit-all", |pa: &mut Pass| {
        let windows = context::windows();
        let unwritten = windows
            .buffers(pa)
            .filter(|handle| {
                let buffer = handle.read(pa);
                buffer.text().has_unsaved_changes() && buffer.exists()
            })
            .count();

        if unwritten == 0 {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else if unwritten == 1 {
            Err(txt!("There is [a]1[] unsaved buffer"))
        } else {
            Err(txt!("There are [a]{unwritten}[] unsaved buffers"))
        }
    });
    alias("qa", "quit-all").unwrap();

    add("quit-all!", |_: &mut Pass| {
        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    });
    alias("qa!", "quit-all!").unwrap();

    add(
        "reload",
        |_: &mut Pass, opts: ReloadOptions, profile: Option<String>| {
            sender()
                .send(DuatEvent::RequestReload(crate::session::ReloadEvent {
                    clean: opts.clean,
                    update: opts.update,
                    profile: profile.unwrap_or(crate::utils::profile().to_string()),
                }))
                .unwrap();

            // This has to be done on Windows, since you can't remove
            // loaded dlls. Thus, we need to quit the curent
            // configuration first, and then we can start compiling the
            // new version of the config crate.
            #[cfg(target_os = "windows")]
            sender().send(DuatEvent::ReloadSucceeded).unwrap();

            Ok(None)
        },
    );

    add("edit", |pa: &mut Pass, arg: PathOrBufferOrCfg| {
        let windows = context::windows();

        let pk = match arg {
            PathOrBufferOrCfg::Cfg => {
                PathKind::from(crate::utils::crate_dir()?.join("src").join("lib.rs"))
            }
            PathOrBufferOrCfg::CfgManifest => {
                PathKind::from(crate::utils::crate_dir()?.join("Cargo.toml"))
            }
            PathOrBufferOrCfg::Path(path) => PathKind::from(path),
            PathOrBufferOrCfg::Buffer(handle) => {
                mode::reset_to(handle.to_dyn());
                return Ok(Some(txt!("Switched to {}", handle.read(pa).name())));
            }
        };

        let buffer = Buffer::new(pk.as_path(), *crate::session::FILE_CFG.get().unwrap());
        let handle = windows.new_buffer(pa, buffer);
        context::set_current_node(pa, handle);

        Ok(Some(txt!("Opened {pk}")))
    });
    alias("e", "edit").unwrap().unwrap();

    add("open", |pa: &mut Pass, arg: PathOrBufferOrCfg| {
        let windows = context::windows();

        let (pk, msg) = match arg {
            PathOrBufferOrCfg::Cfg => (
                PathKind::from(crate::utils::crate_dir()?.join("src").join("lib.rs")),
                None,
            ),
            PathOrBufferOrCfg::CfgManifest => (
                PathKind::from(crate::utils::crate_dir()?.join("Cargo.toml")),
                None,
            ),
            PathOrBufferOrCfg::Path(path) => (PathKind::from(path), None),
            PathOrBufferOrCfg::Buffer(handle) => {
                let pk = handle.read(pa).path_kind();
                let (win, ..) = windows.buffer_entry(pa, pk.clone()).unwrap();
                if windows.get(pa, win).unwrap().buffers(pa).len() == 1 {
                    (pk.clone(), Some(txt!("Switched to {pk}")))
                } else {
                    (pk.clone(), Some(txt!("Moved {pk} to a new window")))
                }
            }
        };

        let file_cfg = *crate::session::FILE_CFG.get().unwrap();
        windows.open_or_move_to_new_window(pa, pk.clone(), file_cfg);

        Ok(msg.or_else(|| Some(txt!("Opened {pk} on new window"))))
    });
    alias("o", "open").unwrap();

    add("buffer", |pa: &mut Pass, handle: OtherBuffer| {
        mode::reset_to(handle.to_dyn());
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    });
    alias("b", "buffer").unwrap();

    add("next-buffer", |pa: &mut Pass, scope: Scope| {
        let windows = context::windows();
        let handle = context::current_buffer(pa);
        let win = context::current_win_index(pa);

        let wid = windows
            .get(pa, win)
            .unwrap()
            .nodes(pa)
            .position(|node| handle.ptr_eq(node.widget()))
            .unwrap_or_else(|| panic!("{}, {win}", handle.read(pa).name()));

        let handle = if scope == Scope::Global {
            windows
                .iter_around(pa, win, wid)
                .find_map(as_buffer_handle)
                .ok_or_else(|| txt!("There are no other open buffers"))?
        } else {
            windows
                .iter_around(pa, win, wid)
                .filter(|(lhs, ..)| *lhs == win)
                .find_map(as_buffer_handle)
                .ok_or_else(|| txt!("There are no other buffers open in this window"))?
        };

        mode::reset_to(handle.to_dyn());
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    });

    add("prev-buffer", |pa: &mut Pass, scope: Scope| {
        let windows = context::windows();
        let handle = context::current_buffer(pa);
        let win = context::current_win_index(pa);

        let wid = windows
            .get(pa, win)
            .unwrap()
            .nodes(pa)
            .position(|node| handle.ptr_eq(node.widget()))
            .unwrap();

        let handle = if scope == Scope::Global {
            windows
                .iter_around_rev(pa, win, wid)
                .find_map(as_buffer_handle)
                .ok_or_else(|| txt!("There are no other open buffers"))?
        } else {
            windows
                .iter_around(pa, win, wid)
                .filter(|(lhs, ..)| *lhs == win)
                .find_map(as_buffer_handle)
                .ok_or_else(|| txt!("There are no other buffers open in this window"))?
        };

        mode::reset_to(handle.to_dyn());
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    });

    add("last-buffer", |pa: &mut Pass| {
        let handle = context::windows().last_buffer(pa)?;
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    });

    add("swap", |pa: &mut Pass, lhs: Handle, rhs: Option<Handle>| {
        let rhs = rhs.unwrap_or_else(|| context::current_buffer(pa).clone());

        context::windows().swap(pa, &lhs.to_dyn(), &rhs.to_dyn())?;

        Ok(Some(txt!(
            "Swapped {} and {}",
            lhs.read(pa).name(),
            rhs.read(pa).name()
        )))
    });

    add("colorscheme", |_: &mut Pass, scheme: ColorSchemeArg| {
        crate::form::set_colorscheme(&scheme);
        Ok(Some(txt!("Set colorscheme to [a]{}[]", scheme.0)))
    });

    add(
        "set-form",
        |_: &mut Pass, name: FormName, colors: Between<0, 3, Color>| {
            let mut form = crate::form::Form::new();
            form.style.foreground_color = colors.first().cloned();
            form.style.background_color = colors.get(1).cloned();
            form.style.underline_color = colors.get(2).cloned();
            crate::form::set(&name.0, form);

            Ok(Some(txt!("Set [a]{}[] to a new Form", name.0)))
        },
    );
}

mod global {
    use std::{cell::UnsafeCell, ops::Range, sync::Arc};

    use super::{CmdResult, Commands};
    #[doc(inline)]
    use crate::{
        cmd::CmdFn,
        context,
        data::{Pass, RwData},
        form::FormId,
        session::DuatEvent,
        text::Text,
    };

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
    ///     hook::add::<WindowCreated>(move |mut pa, window| {
    ///         // status! macro is from duat.
    ///         status!("The value is currently {var}")
    ///             .above()
    ///             .push_on(pa, window);
    ///         Ok(())
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
    pub fn add<Cmd: CmdFn<impl std::any::Any>>(caller: &str, mut cmd: Cmd) {
        COMMANDS.add(super::Command::new(
            caller.to_string(),
            // SAFETY: The type of this RwData doesn't matter, as it is never checked.
            unsafe {
                RwData::new_unsized::<Cmd>(Arc::new(UnsafeCell::new(
                    move |pa: &mut Pass, args: super::Args| cmd.call(pa, args),
                )))
            },
            Cmd::check_args,
        ));
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
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// existing command.
    pub fn alias(alias: impl ToString, command: impl ToString) -> CmdResult {
        COMMANDS.alias(alias, command)
    }

    /// Runs a full command synchronously, with a [`Pass`].
    ///
    /// If you call commands through this function, you will be able
    /// to retrieve their return values,  but because of the [`Pass`],
    /// this function can only be used on the main thread of
    /// execution. If you want to call commands from other threads,
    /// see [`cmd::queue`].
    ///
    /// When running the command, the ordering of [`Flags`] does not
    /// matter, as long as they are placed before the arguments to the
    /// command.
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
    /// [`Flags`]: super::Flags
    pub fn call(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        COMMANDS.run(pa, call)
    }

    /// Like [`call`], but notifies the result
    #[allow(unused_must_use)]
    pub fn call_notify(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        let result = COMMANDS.run(pa, call.to_string());
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
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                // SAFETY: Closure has Pass argument.
                let _ = COMMANDS.run(pa, call);
            })))
            .unwrap();
    }

    /// Like [`queue`], but notifies the result
    pub fn queue_notify(call: impl std::fmt::Display) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                context::logs().push_cmd_result(COMMANDS.run(pa, call.clone()).clone());
            })))
            .unwrap()
    }

    /// Like [`queue`], but acts on the [`Result`]
    pub fn queue_and(call: impl std::fmt::Display, map: impl FnOnce(CmdResult) + Send + 'static) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                map(COMMANDS.run(pa, call));
            })))
            .unwrap()
    }

    /// Like [`queue_and`], but also notifies the [`Result`]
    pub fn queue_notify_and(
        call: impl std::fmt::Display,
        map: impl FnOnce(CmdResult) + Send + 'static,
    ) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                let result = COMMANDS.run(pa, call.clone());
                context::logs().push_cmd_result(result.clone());

                map(result)
            })))
            .unwrap()
    }

    /// Check if the arguments for a given `caller` are correct
    pub fn check_args(
        pa: &Pass,
        caller: &str,
    ) -> Option<(
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    )> {
        COMMANDS.check_args(pa, caller)
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
struct Commands(LazyLock<Mutex<InnerCommands>>);

impl Commands {
    /// Returns a new instance of [`Commands`].
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            Mutex::new(InnerCommands {
                list: Vec::new(),
                aliases: HashMap::new(),
            })
        }))
    }

    /// Aliases a command to a specific word
    fn alias(&self, alias: impl ToString, command: impl ToString) -> CmdResult {
        self.0
            .lock()
            .unwrap()
            .try_alias(alias.to_string(), command.to_string())
    }

    /// Runs a command from a call
    fn run(&self, pa: &mut Pass, call: impl Display) -> CmdResult {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(txt!("The command is empty"))?.to_string();

        let inner = self.0.lock().unwrap();

        let (command, call) = {
            if let Some(command) = inner.aliases.get(&caller) {
                let (command, call) = command;
                let mut call = call.clone() + " ";
                call.extend(args);

                (command.clone(), call)
            } else {
                let command = inner
                    .list
                    .iter()
                    .find(|cmd| cmd.caller() == caller)
                    .ok_or(txt!("[a]{caller}[]: No such command"))?;

                (command.clone(), call.clone())
            }
        };

        let args = Args::new(&call);

        match catch_panic(|| (command.check_args)(pa, args.clone())) {
            Some((_, Some((_, err)))) => return Err(txt!("[a]{caller}[]: {err}")),
            Some(_) => {}
            None => return Err(txt!("[a]{caller}[]: Argument parsing panicked")),
        }

        let silent = call.len() > call.trim_start().len();
        match catch_panic(|| command.cmd.write(&mut unsafe { Pass::new() })(pa, args)) {
            Some(result) => result
                .map(|ok| ok.filter(|_| !silent))
                .map_err(|err| txt!("[a]{caller}[]: {err}")),
            None => Err(txt!("[a]{caller}[]: Command panicked")),
        }
    }

    /// Adds a command to the list of commands
    fn add(&self, command: Command) {
        self.0.lock().unwrap().add(command)
    }

    /// Gets the parameter checker for a command, if it exists
    fn check_args(
        &self,
        pa: &Pass,
        call: &str,
    ) -> Option<(
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    )> {
        let mut args = call.split_whitespace();
        let caller = args.next()?.to_string();

        let inner = self.0.lock().unwrap();
        if let Some((command, _)) = inner.aliases.get(&caller) {
            Some((command.check_args)(pa, Args::new(call)))
        } else {
            let command = inner.list.iter().find(|cmd| cmd.caller() == caller)?;

            Some((command.check_args)(pa, Args::new(call)))
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
    caller: String,
    cmd: InnerCmdFn,
    check_args: CheckerFn,
}

impl Command {
    /// Returns a new instance of command.
    fn new(caller: String, cmd: InnerCmdFn, check_args: CheckerFn) -> Self {
        if caller.split_whitespace().count() != 1 {
            panic!("Command caller \"{caller}\" contains more than one word");
        }
        Self { cmd, check_args, caller }
    }

    /// The caller for this command
    fn caller(&self) -> &str {
        &self.caller
    }
}

struct InnerCommands {
    list: Vec<Command>,
    aliases: HashMap<String, (Command, String)>,
}

impl InnerCommands {
    /// Adds a command to the list
    ///
    /// Overrides previous commands with the same name.
    fn add(&mut self, command: Command) {
        self.list.retain(|cmd| cmd.caller != command.caller());
        self.list.push(command);
    }

    /// Tries to alias a full command (caller, flags, and
    /// arguments) to an alias.
    fn try_alias(&mut self, alias: String, call: String) -> Result<Option<Text>, Text> {
        if alias.split_whitespace().count() != 1 {
            return Err(txt!("Alias [a]{alias}[] is not a single word"));
        }

        let caller = call
            .split_whitespace()
            .next()
            .ok_or(txt!("The command is empty"))?
            .to_string();

        let mut cmds = self.list.iter();

        if let Some(command) = cmds.find(|cmd| cmd.caller().contains(&caller)) {
            let entry = (command.clone(), call.clone());
            Ok(Some(match self.aliases.insert(alias.clone(), entry) {
                Some((_, prev_call)) => {
                    txt!("Aliased [a]{alias}[] from [a]{prev_call}[] to [a]{call}")
                }
                None => txt!("Aliased [a]{alias}[] to [a]{call}"),
            }))
        } else {
            Err(txt!("The caller [a]{caller}[] was not found"))
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

/// Inner function for Commands
#[doc(hidden)]
pub type InnerCmdFn = RwData<dyn FnMut(&mut Pass, Args) -> CmdResult + Send + 'static>;

/// Inner checking function
#[doc(hidden)]
pub type CheckerFn = fn(
    &Pass,
    Args,
) -> (
    Vec<(Range<usize>, Option<FormId>)>,
    Option<(Range<usize>, Text)>,
);

pub(crate) fn as_buffer_handle((.., node): (usize, &Node)) -> Option<Handle> {
    node.try_downcast()
}

trait CmdFn<Arguments>: Send + 'static {
    fn call(&mut self, pa: &mut Pass, args: Args) -> CmdResult;

    fn check_args(
        pa: &Pass,
        args: Args,
    ) -> (
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    );
}

impl<F: FnMut(&mut Pass) -> CmdResult + Send + 'static> CmdFn<()> for F {
    fn call(&mut self, pa: &mut Pass, _: Args) -> CmdResult {
        self(pa)
    }

    fn check_args(
        pa: &Pass,
        mut args: Args,
    ) -> (
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    ) {
        let start = args.next_start();
        if let (Ok(_), Some(start)) = (args.next_as::<Remainder>(pa), start) {
            let err = txt!("Too many arguments");
            return (Vec::new(), Some((start..args.param_range().end, err)));
        }

        (Vec::new(), None)
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
                    let ($param, _) = $param::new(pa, &mut args)?;
                )+

                self(pa, $($param),+)
            }

            fn check_args(
                pa: &Pass,
                mut args: Args,
            ) -> (
                Vec<(Range<usize>, Option<FormId>)>,
                Option<(Range<usize>, Text)>,
            ) {
                let mut ok_ranges = Vec::new();

				$(
                    let start = args.next_start();
                    let result = $param::new(pa, &mut args);
                    match result {
                        Ok((_, form)) => {
                            if let Some(start) = start.filter(|s| args.param_range().end > *s) {
                                ok_ranges.push((start..args.param_range().end, form));
                            }
                        }
                        Err(err) => return (ok_ranges, Some((args.param_range(), err))),
                    }
				)+

                let start = args.next_start();
                if let (Ok(_), Some(start)) = (args.next_as::<Remainder>(pa), start) {
                    let err = txt!("Too many arguments");
                    return (ok_ranges, Some((start..args.param_range().end, err)));
                }

                (ok_ranges, None)
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
