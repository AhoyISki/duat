//! Creation and execution of commands.
//!
//! Commands on Duat are bits of code that can be executed on the
//! [`PromptLine`] widget. They can also be invoked from other parts
//! of the code, but their use is mostly intended for runtime calls.
//!
//! # Running commands
//!
//! There are two environments where you'd run commands. When you have
//! a [`Pass`] available, and when you don't.
//!
//! ## When you have a [`Pass`]
//!
//! If you have a [`Pass`] available, it means you are in the main
//! thread of execution, and can safely execute commands. The
//! advantage of using a [`Pass`] is that you can retrieve the value
//! returned by the command:
//!
//! ```rust
//! use duat_core::prelude::*;
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
//! you may not have a [`Pass`] if for example, you are not on the
//! main thread of execution. In this case, there is [`cmd::queue`],
//! which, as the name implies, queues up a call to be executed later.
//! This means that you can't retrieve the return value, since it will
//! be executed asynchronously:
//!
//! ```rust
//! use duat_core::prelude::*;
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
//! # use std::sync::atomic::{AtomicUsize, Ordering};
//! use duat_core::prelude::*;
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
//! Commands are added through the [`add!`] macro. This macro takes in
//! two arguments. The first argument is a list of callers, for
//! example `["quit", "q"]`, or just a caller, like `"reload"`.
//!
//! The second argument is a _rust-like_ "closure" that receives a
//! variable number of [`Parameter`]s as arguments, alongside a
//! [`Pass`]. Inside of this "closure", you will have access to the
//! ful breatdh of duat's shareable state (just like any other time
//! you have a [`Pass`]).
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
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     let callers = ["unset-form", "uf"];
//!     // A `Vec<T>` parameter will try to collect all
//!     // remaining arguments as `T` in a list.
//!     let result = cmd::add!(callers, |pa: &mut Pass, forms: Vec<cmd::FormName>| {
//!         for form in forms.iter() {
//!             form::set("form", Form::new());
//!         }
//!         // You can return a success message, but must
//!         // return an error message.
//!         // For those, you should use the `txt!` macro.
//!         Ok(Some(txt!("Unset [a]{}[] forms", forms.len()).build()))
//!     });
//! }
//! ```
//!
//! In the command above, you'll notice that [`Ok`] values return
//! [`Option<Text>`]. This is because you may not care about
//! announcing that the command succedeed. For the [`Err`] variant,
//! however, the return value is just [`Text`], because you should say
//! what went wrong in the command. Most of the time, this happens
//! because of something out of your control, like a file not
//! existing. In these cases, the `?` is enough to return an
//! appropriate [`Text`].
//!
//! Duat commands also offer flags in the form of the [`Flags`]
//! [`Parameter`]. These can make use of word flags (`--full-words`)
//! and blob flags (`-singlechar`):
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use std::sync::atomic::{AtomicU32, Ordering};
//!
//! use duat::prelude::*;
//!
//! static EXPRESSION: AtomicU32 = AtomicU32::new('a' as u32);
//!
//! // Imagine this is the config crate `setup` function
//! fn setup() {
//!     cmd::add!("mood", |_pa, flags: cmd::Flags| {
//!         // `Flags::long` checks for `--` flags
//!         if flags.word("happy") {
//!             EXPRESSION.store('😁' as u32, Ordering::Relaxed)
//!         // `Flags::short` checks for `-` flags
//!         // They can check for any valid unicode character.
//!         } else if flags.blob("🤯") {
//!             EXPRESSION.store('🤯' as u32, Ordering::Relaxed)
//!         } else if flags.word("sad") {
//!             EXPRESSION.store('😢' as u32, Ordering::Relaxed)
//!         } else {
//!             EXPRESSION.store('😶' as u32, Ordering::Relaxed)
//!         }
//!         Ok(None)
//!     });
//! }
//!
//! fn main_thread_function(pa: &mut Pass) {
//!     let _ = cmd::call(pa, "mood --sad -🤯");
//!     // Passing more arguments than needed results in
//!     // an error, so the command is never executed.
//!     let _ = cmd::call_notify(pa, "mood --happy extra args not allowed");
//!
//!     let num = EXPRESSION.load(Ordering::Relaxed);
//!     assert_eq!(char::from_u32(num), Some('🤯'))
//! }
//! ```
//!
//! There are other builtin types of [`Parameter`]s in [`cmd`] that
//! can be used on a variety of things. For example, the [`Remainder`]
//! [`Parameter`] is one that just takes the remaining arguments and
//! collects them into a single [`String`].
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     cmd::add!("pip", |_pa, args: cmd::Remainder| {
//!         let child = std::process::Command::new("pip").spawn()?;
//!         let res = child.wait_with_output()?;
//!
//!         Ok(Some(txt!("{res:?}").build()))
//!     });
//! }
//! ```
//!
//! You can also act on [`Widget`]s, with the [`Handles`]
//! [`Parameter`]:
//!
//! ```rust
//! # duat_core::doc_duat!(duat);
//! setup_duat!(setup);
//! use duat::prelude::{file::File, *};
//!
//! fn setup() {
//!     cmd::add!(
//!         "set-mask",
//!         |pa, mask: String, handles: cmd::Handles<File<Ui>>| {
//!             handles.on_flags(pa, |pa, handle| {
//!                 handle.set_mask(mask.clone().leak());
//!             });
//!
//!             Ok(None)
//!         }
//!     );
//! }
//! ```
//!
//! In this case, [`Handles<File<Ui>>`] will grab every single
//! [`Handle<File<Ui>>`] open at the moment, and lets you act on them.
//! The function [`Handles::on_flags`] will act based on the [`Flags`]
//! that were passed, i.e. if `"--global"` was passed, [`on_flags`]
//! will act on every [`Handle`] of a a [`File`]. You can also ignore
//! the [`Flags`] by calling something like [`Handles::on_window`], or
//! [`Handles::on_current`].
//!
//! [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
//! [`cmd::call_notify`]: call_notify
//! [`cmd::queue`]: queue
//! [`cmd::queue_and`]: queue_and
//! [`Send + 'static`]: Send
//! [`Color`]: crate::form::Color
//! [`Widget`]: crate::ui::Widget
//! [`File`]: crate::file::File
//! [`&str`]: str
//! [`cmd`]: self
//! [`txt!`]: crate::prelude::txt
//! [`Ok(Some({Text}))`]: Ok
//! [`Form`]: crate::form::Form
//! [`Area`]: crate::ui::Ui::Area
//! [`Handle<File<Ui>>`]: crate::context::Handle
//! [`on_flags`]: Handles::on_flags
//! [`Handle`]: crate::context::Handle
use std::{
    collections::HashMap,
    fmt::Display,
    ops::Range,
    sync::{Arc, LazyLock},
};

use crossterm::style::Color;

pub use self::{global::*, parameters::*};
use crate::{
    context::{self, sender},
    data::{Pass, RwData},
    file::{File, PathKind},
    form::FormId,
    mode,
    session::DuatEvent,
    text::{Text, txt},
    ui::{Node, Ui, Widget},
};

mod parameters;

pub(crate) fn add_session_commands<U: Ui>() {
    add!("alias", |pa,
                   flags: Flags,
                   alias: &str,
                   command: Remainder| {
        if !flags.is_empty() {
            Err(txt!("An alias cannot take any flags").build())
        } else {
            crate::cmd::alias(pa, alias, command)
        }
    });

    add!(["quit", "q"], |pa, handle: Option<Buffer<U>>| {
        let handle = match handle {
            Some(handle) => handle,
            None => context::fixed_file::<U>(pa)?,
        };

        let file = handle.read(pa);
        let pk = file.path_kind();
        if file.text().has_unsaved_changes() && file.exists() {
            return Err(txt!("{} has unsaved changes", pk.clone()).build());
        }

        sender().send(DuatEvent::CloseFile(pk.clone())).unwrap();
        Ok(Some(txt!("Closed {pk}").build()))
    });

    add!(["quit!", "q!"], |pa, handle: Option<Buffer<U>>| {
        let handle = match handle {
            Some(handle) => handle,
            None => context::fixed_file::<U>(pa)?,
        };

        let pk = handle.read(pa).path_kind();
        sender().send(DuatEvent::CloseFile(pk.clone())).unwrap();
        Ok(Some(txt!("Closed {pk}").build()))
    });

    add!(["quit-all", "qa"], |pa| {
        let windows = context::windows::<U>();
        let unwritten = windows
            .file_handles(pa)
            .filter(|handle| {
                let file = handle.read(pa);
                file.text().has_unsaved_changes() && file.exists()
            })
            .count();

        if unwritten == 0 {
            sender().send(DuatEvent::Quit).unwrap();
            Ok(None)
        } else if unwritten == 1 {
            Err(txt!("There is [a]1[] unsaved file").build())
        } else {
            Err(txt!("There are [a]{unwritten}[] unsaved files").build())
        }
    });

    add!(["quit-all!", "qa!"], |_pa| {
        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    });

    add!(["write", "w"], |pa, path: Option<ValidFile<U>>| {
        let handle = context::fixed_file::<U>(pa)?;
        let file = handle.write(pa);

        let (bytes, name) = if let Some(path) = path {
            (file.save_to(&path)?, path)
        } else if let Some(name) = file.name_set() {
            (file.save()?, std::path::PathBuf::from(name))
        } else {
            return Err(txt!("File has no name path to write to").build());
        };

        match bytes {
            Some(bytes) => Ok(Some(
                txt!("Wrote [a]{bytes}[] bytes to [file]{name}").build(),
            )),
            None => Ok(Some(txt!("Nothing to be written").build())),
        }
    });

    add!(["write-quit", "wq"], |pa, path: Option<ValidFile<U>>| {
        let handle = context::fixed_file::<U>(pa)?;

        let (bytes, name) = {
            let file = handle.write(pa);
            let bytes = if let Some(path) = path {
                file.save_quit_to(path, true)?
            } else {
                file.save_quit(true)?
            };
            (bytes, file.name())
        };

        sender()
            .send(DuatEvent::CloseFile(handle.read(pa).path_kind()))
            .unwrap();
        match bytes {
            Some(bytes) => Ok(Some(
                txt!("Closed [file]{name}[], writing [a]{bytes}[] bytes").build(),
            )),
            None => Ok(Some(txt!("Closed [file]{name}[]").build())),
        }
    });

    add!(["write-all", "wa"], |pa| {
        let windows = context::windows::<U>();

        let mut written = 0;
        let handles: Vec<_> = windows
            .file_handles(pa)
            .filter(|handle| handle.read(pa).path_set().is_some())
            .collect();

        for handle in &handles {
            written += handle.write(pa).save().is_ok() as usize;
        }

        if written == handles.len() {
            Ok(Some(txt!("Wrote to [a]{written}[] files").build()))
        } else {
            let unwritten = handles.len() - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(txt!("Failed to write to [a]{unwritten}[] file{plural}").build())
        }
    });

    add!(["write-all-quit", "waq"], |pa| {
        let windows = context::windows::<U>();

        let mut written = 0;
        let handles: Vec<_> = windows
            .file_handles(pa)
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
            Err(txt!("Failed to write to [a]{unwritten}[] file{plural}").build())
        }
    });

    add!(["write-all-quit!", "waq!"], |pa| {
        let handles: Vec<_> = context::windows::<U>().file_handles(pa).collect();

        for handle in handles {
            let _ = handle.write(pa).save_quit(true);
        }

        sender().send(DuatEvent::Quit).unwrap();
        Ok(None)
    });

    add!(["reload"], |_pa, flags: Flags, profile: Option<String>| {
        sender()
            .send(DuatEvent::RequestReload(crate::session::ReloadEvent {
                clean: flags.word("clean"),
                update: flags.word("update"),
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
    });

    add!(["edit", "e"], |pa, arg: FileOrBufferOrCfg<U>| {
        let pk = match arg {
            FileOrBufferOrCfg::Cfg => {
                PathKind::from(crate::utils::crate_dir()?.join("src").join("lib.rs"))
            }
            FileOrBufferOrCfg::CfgManifest => {
                PathKind::from(crate::utils::crate_dir()?.join("Cargo.toml"))
            }
            FileOrBufferOrCfg::File(path) => PathKind::from(path),
            FileOrBufferOrCfg::Buffer(handle) => {
                let pk = handle.read(pa).path_kind();
                mode::reset_to_file::<U>(pk.clone(), true);
                return Ok(Some(txt!("Switched to {pk}").build()));
            }
        };

        sender().send(DuatEvent::OpenFile(pk.clone())).unwrap();
        return Ok(Some(txt!("Opened {pk}").build()));
    });

    add!(["open", "o"], |pa, arg: FileOrBufferOrCfg<U>| {
        let windows = context::windows::<U>();

        let pk = match arg {
            FileOrBufferOrCfg::Cfg => {
                PathKind::from(crate::utils::crate_dir()?.join("src").join("lib.rs"))
            }
            FileOrBufferOrCfg::CfgManifest => {
                PathKind::from(crate::utils::crate_dir()?.join("Cargo.toml"))
            }
            FileOrBufferOrCfg::File(path) => PathKind::from(path),
            FileOrBufferOrCfg::Buffer(handle) => {
                let pk = handle.read(pa).path_kind();
                let (win, ..) = windows.file_entry(pa, pk.clone()).unwrap();
                if windows.get(pa, win).unwrap().file_handles(pa).len() == 1 {
                    mode::reset_to_file::<U>(pk.clone(), true);
                    return Ok(Some(txt!("Switched to {pk}").build()));
                } else {
                    sender().send(DuatEvent::OpenWindow(pk.clone())).unwrap();
                    return Ok(Some(txt!("Moved {pk} to a new window").build()));
                }
            }
        };

        sender().send(DuatEvent::OpenWindow(pk.clone())).unwrap();
        return Ok(Some(txt!("Opened {pk} on new window").build()));
    });

    add!(["buffer", "b"], |pa, handle: OtherBuffer<U>| {
        let pk = handle.read(pa).path_kind();
        mode::reset_to_file::<U>(pk.clone(), true);
        Ok(Some(txt!("Switched to [a]{pk}").build()))
    });

    add!("next-file", |pa, flags: Flags| {
        let windows = context::windows::<U>();
        let handle = context::fixed_file::<U>(pa)?;
        let win = context::cur_window();

        let wid = windows
            .get(pa, win)
            .unwrap()
            .nodes()
            .position(|node| handle.ptr_eq(node.widget()))
            .unwrap();

        let pk = if flags.word("global") {
            windows
                .iter_around(pa, win, wid)
                .find_map(get_pk(pa))
                .ok_or_else(|| txt!("There are no other open files"))?
        } else {
            windows
                .iter_around(pa, win, wid)
                .filter(|(lhs, ..)| *lhs == win)
                .find_map(get_pk(pa))
                .ok_or_else(|| txt!("There are no other files open in this window"))?
        };

        mode::reset_to_file::<U>(pk.clone(), true);
        Ok(Some(txt!("Switched to {pk}").build()))
    });

    add!("prev-file", |pa, flags: Flags| {
        let windows = context::windows::<U>();
        let handle = context::fixed_file::<U>(pa)?;
        let win = context::cur_window();

        let wid = windows
            .get(pa, win)
            .unwrap()
            .nodes()
            .position(|node| handle.ptr_eq(node.widget()))
            .unwrap();

        let pk = if flags.word("global") {
            windows
                .iter_around_rev(pa, win, wid)
                .find_map(get_pk(pa))
                .ok_or_else(|| txt!("There are no other open files"))?
        } else {
            windows
                .iter_around(pa, win, wid)
                .filter(|(lhs, ..)| *lhs == win)
                .find_map(get_pk(pa))
                .ok_or_else(|| txt!("There are no other files open in this window"))?
        };

        mode::reset_to_file::<U>(pk.clone(), true);

        Ok(Some(txt!("Switched to {pk}").build()))
    });

    add!("swap", |pa, lhs: Buffer<U>, rhs: Option<Buffer<U>>| {
        let lhs = lhs.read(pa).path_kind();
        let rhs = match rhs {
            Some(rhs) => rhs.read(pa).path_kind(),
            None => context::fixed_file::<U>(pa)?.read(pa).path_kind(),
        };
        sender()
            .send(DuatEvent::SwapFiles(lhs.clone(), rhs.clone()))
            .unwrap();

        Ok(Some(txt!("Swapped {lhs} and {rhs}").build()))
    });

    add!("colorscheme", |_pa, scheme: ColorSchemeArg| {
        crate::form::set_colorscheme(scheme);
        Ok(Some(txt!("Set colorscheme to [a]{scheme}[]").build()))
    });

    add!(
        "set-form",
        |_pa, name: FormName, colors: Between<0, 3, Color>| {
            let mut form = crate::form::Form::new();
            form.style.foreground_color = colors.first().cloned();
            form.style.background_color = colors.get(1).cloned();
            form.style.underline_color = colors.get(2).cloned();
            crate::form::set(name, form);

            Ok(Some(txt!("Set [a]{name}[] to a new Form").build()))
        }
    );
}

mod global {
    use std::ops::Range;

    use super::{CheckerFn, CmdFn, CmdResult, Commands};
    use crate::{
        context, data::Pass, form::FormId, main_thread_only::MainThreadOnly, session::DuatEvent,
        text::Text,
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
    ///     cmd::add!("set-var", |pa: &mut Pass, value: usize| {
    ///         *var_clone.write(pa) = value;
    ///         Ok(None)
    ///     });
    ///
    ///     hook::add::<WindowCreated>(move |mut pa, builder| {
    ///         // status! macro is from duat-utils.
    ///         builder.push(status!("The value is currently {var}").above());
    ///     });
    /// }
    /// ```
    ///
    /// Since `var` is an [`RwData`], it will be updated
    /// automatically in the [`StatusLine`]
    ///
    /// [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
    /// [`RwData`]: crate::data::RwData
    /// [`Parameter`]: super::Parameter
    pub macro add($callers:expr, |$pa:ident $(: &mut Pass)? $(, $arg:tt: $t:ty)* $(,)?| $f:block) {{
        use std::{sync::Arc, cell::UnsafeCell};
        #[allow(unused_imports)]
        use $crate::{
            data::{Pass, RwData},
            cmd::{Args, Caller, CmdFn, CmdResult, Parameter, Remainder, add_inner}
        };

        #[allow(unused_variables, unused_mut)]
        let cmd = move |pa: &mut Pass, mut args: Args| -> CmdResult {
            $(
                let ($arg, form): (<$t as Parameter>::Returns, _) =
                    <$t as Parameter>::new(pa, &mut args)?;
            )*

            if let Ok(arg) = args.next() {
                return Err($crate::text::txt!("Too many arguments").build());
            }

            let mut $pa = pa;

            $f
        };

        #[allow(unused_variables, unused_mut)]
        let check_args = |pa: &Pass, mut args: Args| {
            let mut ok_ranges = Vec::new();

            $(
                let start = args.next_start();
                let result = <$t as Parameter>::new(pa, &mut args);
                match result {
                    Ok((_, form)) => if let Some(start) = start
                        .filter(|s| args.param_range().end > *s)
                    {
                        ok_ranges.push((start..args.param_range().end, form));
                    }
                    Err(err) => return (ok_ranges, Some((args.param_range(), err)))
                }
            )*

            let start = args.next_start();
            if let (Ok(_), Some(start)) = (args.next_as::<Remainder>(pa), start) {
                let err = $crate::text::txt!("Too many arguments").build();
                return (ok_ranges, Some((start..args.param_range().end, err)))
            }

            (ok_ranges, None)
        };

        let callers: Vec<String> = $callers.into_callers().map(str::to_string).collect();
        // SAFETY: This type will never actually be queried
        let cmd: CmdFn = unsafe { RwData::new_unsized::<()>(Arc::new(UnsafeCell::new(cmd))) };

        add_inner(callers, cmd, check_args)
    }}

    /// Canonical way to quit Duat.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Duat will execute a program closing
    /// function, as defined by the [`Ui`].
    ///
    /// [`Ui`]: crate::ui::Ui
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
    /// use duat_core::prelude::*;
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
    /// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    /// [`Flags`]: super::Flags
    pub fn call(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        // SAFETY: Function has a Pass argument.
        unsafe { COMMANDS.get() }.run(pa, call)
    }

    /// Like [`call`], but notifies the result
    pub fn call_notify(pa: &mut Pass, call: impl std::fmt::Display) -> CmdResult {
        // SAFETY: Function has a Pass argument.
        let result = unsafe { COMMANDS.get() }.run(pa, call.to_string());
        let call = call.to_string();
        let cmd = call.split(' ').find(|w| !w.is_empty()).unwrap();
        context::logs().push_cmd_result(cmd.to_string(), result.clone());

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
                let _ = unsafe { COMMANDS.get() }.run(pa, call);
            })))
            .unwrap();
    }

    /// Like [`queue`], but notifies the result
    pub fn queue_notify(call: impl std::fmt::Display) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                let result = unsafe { COMMANDS.get() }.run(pa, call.clone());
                let call = call;
                let cmd = call.split(' ').find(|w| !w.is_empty()).unwrap();
                context::logs().push_cmd_result(cmd.to_string(), result.clone());
            })))
            .unwrap()
    }

    /// Like [`queue`], but acts on the [`Result`]
    pub fn queue_and(call: impl std::fmt::Display, map: impl FnOnce(CmdResult) + Send + 'static) {
        let call = call.to_string();
        crate::context::sender()
            .send(DuatEvent::QueuedFunction(Box::new(move |pa| {
                // SAFETY: Function has a Pass argument.
                map(unsafe { COMMANDS.get() }.run(pa, call));
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
                // SAFETY: Function has a Pass argument.
                let result = unsafe { COMMANDS.get() }.run(pa, call.clone());
                let call = call;
                let cmd = call.split(' ').find(|w| !w.is_empty()).unwrap();
                context::logs().push_cmd_result(cmd.to_string(), result.clone());

                map(result)
            })))
            .unwrap()
    }

    /// Don't call this function, use [`cmd::add`] instead
    ///
    /// [`cmd::add`]: add
    #[doc(hidden)]
    pub fn add_inner(callers: Vec<String>, cmd: CmdFn, check_args: CheckerFn) {
        // SAFETY: There is no way to obtain an external RwData of Commands,
        // so you can modify it from anywhere in the main thread.
        let mut pa = unsafe { Pass::new() };
        unsafe { COMMANDS.get() }.add(&mut pa, callers, cmd, check_args)
    }

    /// Check if the arguments for a given `caller` are correct
    pub fn check_args(
        pa: &Pass,
        caller: &str,
    ) -> Option<(
        Vec<(Range<usize>, Option<FormId>)>,
        Option<(Range<usize>, Text)>,
    )> {
        // SAFETY: There is a Pass argument
        unsafe { COMMANDS.get() }.check_args(pa, caller)
    }
}

/// A list of commands.
///
/// This list contains all of the commands that have been
/// added to Duat, as well as info on the current [`File`],
/// [widget] and all of the [windows].
///
/// [`File`]: crate::file::File
/// [widget]: crate::ui::Widget
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
        self.0
            .write(pa)
            .try_alias(alias.to_string(), command.to_string())
    }

    /// Runs a command from a call
    fn run(&self, pa: &mut Pass, call: impl Display) -> CmdResult {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(txt!("The command is empty"))?.to_string();

        let inner = self.0.read(pa);

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
                    .find(|cmd| cmd.callers().contains(&caller))
                    .ok_or(txt!("No such command"))?;

                (command.clone(), call.clone())
            }
        };

        let args = get_args(&call);

        if let (_, Some((_, err))) = (command.check_args)(pa, args.clone()) {
            return Err(err);
        }

        let silent = call.len() > call.trim_start().len();
        command.cmd.write(&mut unsafe { Pass::new() })(pa, args).map(|ok| ok.filter(|_| !silent))
    }

    /// Adds a command to the list of commands
    fn add(&self, pa: &mut Pass, callers: Vec<String>, cmd: CmdFn, check_args: CheckerFn) {
        let cmd = Command::new(callers, cmd, check_args);
        self.0.write(pa).add(cmd)
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

        let inner = self.0.read(pa);
        if let Some((command, _)) = inner.aliases.get(&caller) {
            Some((command.check_args)(pa, get_args(call)))
        } else {
            let command = inner
                .list
                .iter()
                .find(|cmd| cmd.callers().contains(&caller))?;

            Some((command.check_args)(pa, get_args(call)))
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
    /// Adds a command to the list
    ///
    /// Overrides previous commands with the same name.
    fn add(&mut self, command: Command) {
        let mut new_callers = command.callers().iter();

        self.list
            .retain(|cmd| new_callers.all(|caller| !cmd.callers.contains(caller)));

        self.list.push(command);
    }

    /// Tries to alias a full command (caller, flags, and
    /// arguments) to an alias.
    fn try_alias(&mut self, alias: String, call: String) -> Result<Option<Text>, Text> {
        if alias.split_whitespace().count() != 1 {
            return Err(txt!("Alias [a]{alias}[] is not a single word").build());
        }

        let caller = call
            .split_whitespace()
            .next()
            .ok_or(txt!("The command is empty"))?
            .to_string();

        let mut cmds = self.list.iter();

        if let Some(command) = cmds.find(|cmd| cmd.callers().contains(&caller)) {
            let entry = (command.clone(), call.clone());
            Ok(Some(match self.aliases.insert(alias.clone(), entry) {
                Some((_, prev_call)) => {
                    txt!("Aliased [a]{alias}[] from [a]{prev_call}[] to [a]{call}").build()
                }
                None => txt!("Aliased [a]{alias}[] to [a]{call}").build(),
            }))
        } else {
            Err(txt!("The caller [a]{caller}[] was not found").build())
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
pub type CmdFn = RwData<dyn FnMut(&mut Pass, Args) -> CmdResult + Send + 'static>;

/// Inner checking function
#[doc(hidden)]
pub type CheckerFn = fn(
    &Pass,
    Args,
) -> (
    Vec<(Range<usize>, Option<FormId>)>,
    Option<(Range<usize>, Text)>,
);

pub(crate) fn get_pk<U: Ui>(pa: &Pass) -> impl Fn((usize, usize, &Node<U>)) -> Option<PathKind> {
    |(.., node)| node.read_as(pa).map(|f: &File<U>| f.path_kind())
}
