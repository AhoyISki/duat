use crossterm::style::Color;

use crate::{
    buffer::{Buffer, PathKind},
    cmd::{
        Between, ColorSchemeArg, Existing, FormName, OtherBuffer, PathOrBufferOrCfg, ReloadOptions,
        Remainder, Scope, ValidFilePath, add, alias,
    },
    context::{self, Handle, sender},
    data::Pass,
    mode,
    session::{self, DuatEvent},
    txt,
    ui::Node,
};

pub fn add_defalt_commands() {
    add(
        "alias",
        |_: &mut Pass, alias: String, command: Remainder| {
            crate::cmd::alias(alias, command.0);
            Ok(None)
        },
    )
    .doc(
        txt!("Create an alias for a command"),
        Some(txt!(
            "The alias [a]must[] be a single word (no spaces), and the command can include \
             arguments"
        )),
    )
    .doc_param(
        txt!("Alias name"),
        Some(txt!("[a]Must[] be a single word")),
        Some(txt!("[param]alias")),
    )
    .doc_param(
        txt!("Aliased command"),
        Some(txt!("Can include arguments")),
        Some(txt!("[param]command")),
    );

    add("write", |pa: &mut Pass, path: Option<ValidFilePath>| {
        let handle = context::current_buffer(pa);

        let (has_written, name) = if let Some(path) = path {
            (handle.save_to(pa, &path.0)?, path.0)
        } else if let Some(name) = handle.read(pa).name_set() {
            (handle.save(pa)?, std::path::PathBuf::from(name))
        } else {
            return Err(txt!("Buffer has no name path to write to"));
        };

        match has_written {
            true => Ok(Some(txt!(
                "Wrote [a]{}[] bytes to [buffer]{name}",
                handle.text(pa).len()
            ))),
            false => Ok(Some(txt!("Nothing to be written"))),
        }
    })
    .doc(
        txt!("Saves the [a]Buffer[] in storage"),
        Some(txt!("By default, will write to the [a]Buffer[]'s path")),
    )
    .doc_param(
        txt!("[a]Optional[] path to write to"),
        Some(txt!(
            "If the [a]Buffer[] has no path ([a]scratch buffer[]), then this argument [a]must[] \
             be included"
        )),
        None,
    );
    alias("w", "write");

    add(
        "write-quit",
        |pa: &mut Pass, path: Option<ValidFilePath>| {
            let handle = context::current_buffer(pa);

            let (has_written, name) = {
                let bytes = if let Some(path) = path {
                    handle.save_quit_to(pa, path.0, true)?
                } else {
                    handle.save_quit(pa, true)?
                };
                (bytes, handle.read(pa).name())
            };

            context::windows().close(pa, &handle)?;

            match has_written {
                true => Ok(Some(txt!(
                    "Closed [buffer]{name}[], writing [a]{}[] bytes",
                    handle.text(pa).len()
                ))),
                false => Ok(Some(txt!("Closed [buffer]{name}[]"))),
            }
        },
    )
    .doc(
        txt!("Save the [a]Buffer[] in storage and closes it"),
        Some(txt!(
            "By default, will write to the [a]Buffer[]'s path. If this is the last open \
             [a]Buffer[], also quit Duat"
        )),
    )
    .doc_param(
        txt!("[a]Optional[] path to write to"),
        Some(txt!(
            "If the [a]Buffer[] has no path (a [a]scratch buffer[]), then this argument [a]must[] \
             be included"
        )),
        None,
    );
    alias("wq", "write-quit");

    add("write-all", |pa: &mut Pass| {
        let windows = context::windows();

        let mut written = 0;
        let handles: Vec<_> = windows
            .buffers(pa)
            .into_iter()
            .filter(|handle| handle.read(pa).path_set().is_some())
            .collect();

        for handle in &handles {
            written += handle.save(pa).is_ok() as usize;
        }

        if written == handles.len() {
            Ok(Some(txt!("Wrote to [a]{written}[] buffers")))
        } else {
            let unwritten = handles.len() - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(txt!("Failed to write to [a]{unwritten}[] buffer{plural}"))
        }
    })
    .doc(txt!("Writes to all [a]Buffer[]s"), None);
    alias("wa", "write-all");

    add("write-all-quit", |pa: &mut Pass| {
        let windows = context::windows();

        let mut written = 0;
        let handles: Vec<_> = windows
            .buffers(pa)
            .into_iter()
            .filter(|handle| handle.read(pa).path_set().is_some())
            .collect();
        for handle in &handles {
            written += handle.save_quit(pa, true).is_ok() as usize;
        }

        if written == handles.len() {
            sender().send(DuatEvent::Quit);
            Ok(None)
        } else {
            let unwritten = handles.len() - written;
            let plural = if unwritten == 1 { "" } else { "s" };
            Err(txt!("Failed to write to [a]{unwritten}[] buffer{plural}"))
        }
    })
    .doc(
        txt!("Save all [a]Buffer[]s in storage, then quit"),
        Some(txt!(
            "If any [a]Buffer[] fails to be saved, won't quit Duat"
        )),
    );
    alias("waq", "write-all-quit");

    add("write-all-quit!", |pa: &mut Pass| {
        for handle in context::windows().buffers(pa) {
            let _ = handle.save_quit(pa, true);
        }

        sender().send(DuatEvent::Quit);
        Ok(None)
    })
    .doc(
        txt!("Save all [a]Buffer[]s in storage, then [a]forcibly[] quit"),
        Some(txt!(
            "[a]WARNING[]: This command will ignore any failed attempts to write"
        )),
    );
    alias("waq!", "write-all-quit!");

    add("quit", |pa: &mut Pass, handle: Option<Handle>| {
        let handle = match handle {
            Some(handle) => handle,
            None => context::current_buffer(pa),
        };

        let buffer = handle.read(pa);
        if buffer.text().has_unsaved_changes() && buffer.exists() {
            return Err(txt!("{} has unsaved changes", buffer.name()));
        }

        context::windows().close(pa, &handle)?;

        Ok(Some(txt!("Closed [buffer]{}", handle.read(pa).name())))
    })
    .doc(
        txt!("Close a [a]Buffer[]"),
        Some(txt!(
            "This will fail if the [a]Buffer[] has unsaved changes. If it is the last open \
             [a]Buffer[], also quit Duat"
        )),
    )
    .doc_param(
        txt!("Optional [a]Buffer[] to quit, instead of the current one"),
        None,
        None,
    );
    alias("q", "quit");

    add("quit!", |pa: &mut Pass, handle: Option<Handle>| {
        let handle = match handle {
            Some(handle) => handle,
            None => context::current_buffer(pa),
        };

        context::windows().close(pa, &handle)?;

        Ok(Some(txt!("Forcibly closed {}", handle.read(pa).name())))
    })
    .doc(
        txt!("[a]Forcibly[] close a [a]Buffer[]"),
        Some(txt!("If it is the last open [a]Buffer[], also quit Duat")),
    )
    .doc_param(
        txt!("Optional [a]Buffer[] to quit, instead of the current one"),
        None,
        None,
    );
    alias("q!", "quit!");

    add("quit-all", |pa: &mut Pass| {
        let windows = context::windows();
        let unwritten = windows
            .buffers(pa)
            .into_iter()
            .filter(|handle| {
                let buffer = handle.read(pa);
                buffer.text().has_unsaved_changes() && buffer.exists()
            })
            .count();

        if unwritten == 0 {
            sender().send(DuatEvent::Quit);
            Ok(None)
        } else if unwritten == 1 {
            Err(txt!("There is [a]1[] unsaved buffer"))
        } else {
            Err(txt!("There are [a]{unwritten}[] unsaved buffers"))
        }
    })
    .doc(
        txt!("Close all [a]Buffer[]s, then quit Duat"),
        Some(txt!(
            "This will fail if the [a]Buffer[] has unsaved changes"
        )),
    );
    alias("qa", "quit-all");

    add("quit-all!", |_: &mut Pass| {
        sender().send(DuatEvent::Quit);
        Ok(None)
    })
    .doc(
        txt!("[a]Forcibly[] close all [a]Buffer[]s, then quit Duat"),
        Some(txt!(
            "[a]WARNING[]: This command will ignore any unsaved changes"
        )),
    );
    alias("qa!", "quit-all!");

    add(
        "reload",
        |_: &mut Pass, opts: ReloadOptions, profile: Option<String>| {
            sender().send(DuatEvent::RequestReload(session::ipc::ReloadRequest {
                clean: opts.clean,
                update: opts.update,
                profile: profile.unwrap_or(crate::utils::profile().to_string()),
            }));

            Ok(None)
        },
    )
    .doc(
        txt!("Reload the configuration crate"),
        Some(txt!(
            "This will call [a]cargo build[] on your chosen configuration path, then will \
             automatically reload the compiled binary"
        )),
    )
    .doc_param(
        txt!("clean files and/or update dependencies"),
        Some(txt!(
            "[param.flag]--clean[] will call [a]cargo clean[] as well as the remove the \
             [a]cache[] and [a]local[] directories of Duat, [param.flag]--update[] will call \
             [a]cargo update[]"
        )),
        None,
    )
    .doc_param(
        txt!("Optional [a]profile[]"),
        None,
        Some(txt!("[param]profile[param.punctuation]?")),
    );

    add(
        "edit",
        |pa: &mut Pass, _: Existing, arg: PathOrBufferOrCfg| {
            let windows = context::windows();

            let pk = match arg {
                PathOrBufferOrCfg::Cfg => {
                    PathKind::from(crate::utils::crate_dir()?.join("src").join("main.rs"))
                }
                PathOrBufferOrCfg::CfgManifest => {
                    PathKind::from(crate::utils::crate_dir()?.join("Cargo.toml"))
                }
                PathOrBufferOrCfg::Path(path) => PathKind::from(path),
                PathOrBufferOrCfg::Buffer(handle) => {
                    mode::reset_to(pa, &handle);
                    return Ok(Some(txt!("Switched to {}", handle.read(pa).name())));
                }
            };

            let buffer = if let Some(buffer) = context::buffers(pa)
                .into_iter()
                .find(|buffer| buffer.read(pa).path_kind() == pk)
            {
                buffer.to_dyn()
            } else {
                let buffer = Buffer::new(pk.as_path(), *session::BUFFER_OPTS.get().unwrap());
                windows.new_buffer(pa, buffer).handle().clone()
            };

            mode::reset_to(pa, &buffer);

            Ok(Some(txt!("Opened {pk}")))
        },
    )
    .doc(
        txt!("Switch to a [a]Buffer[] or open it on the same window"),
        Some(txt!(
            "When opening a new [a]Buffer[], will open it in the current window"
        )),
    )
    .doc_param(txt!("Fail if the file doesn't exist"), None, None)
    .doc_param(
        txt!("Which [a]Buffer[] to open or switch to"),
        Some(txt!(
            "This accepts one of 4 types: an open [a]Buffer[]'s name, a valid file path, or the \
             one of the flags [param.flag]--cfg[] or [param.flag]--cfg-manifest[], which open \
             configuration crate files"
        )),
        None,
    );
    alias("e", "edit");

    add(
        "open",
        |pa: &mut Pass, _: Existing, arg: PathOrBufferOrCfg| {
            let windows = context::windows();

            let (pk, msg) = match arg {
                PathOrBufferOrCfg::Cfg => (
                    PathKind::from(crate::utils::crate_dir()?.join("src").join("main.rs")),
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

            let buffer_opts = *session::BUFFER_OPTS.get().unwrap();
            windows.open_or_move_to_new_window(pa, pk.clone(), buffer_opts);

            Ok(msg.or_else(|| Some(txt!("Opened {pk} on new window"))))
        },
    )
    .doc(
        txt!("Switch to a [a]Buffer[] or open it on another window"),
        None,
    )
    .doc_param(txt!("Fail if the file doesn't exist"), None, None)
    .doc_param(
        txt!("Which [a]Buffer[] to open or switch to"),
        Some(txt!(
            "This accepts one of 4 types: an open [a]Buffer[]'s name, a valid file path, or the \
             one of the flags [param.flag]--cfg[] or [param.flag]--cfg-manifest[], which open \
             configuration crate files"
        )),
        None,
    );
    alias("o", "open");

    add("buffer", |pa: &mut Pass, handle: OtherBuffer| {
        mode::reset_to(pa, &handle);
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    })
    .doc(txt!("Switch to an open [a]Buffer[]"), None)
    .doc_param(txt!("The name of an open [a]Buffer"), None, None);
    alias("b", "buffer");

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

        mode::reset_to(pa, &handle);
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    })
    .doc(
        txt!("Switch to the next [a]Buffer[]"),
        Some(txt!(
            "The switching is based on the [a]Layout[]. By default, this will move it clockwise"
        )),
    )
    .doc_param(txt!("Also consider other windows"), None, None);

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

        mode::reset_to(pa, &handle);
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    })
    .doc(
        txt!("Switch to the previous [a]Buffer[]"),
        Some(txt!(
            "The switching is based on the [a]Layout[]. By default, this will move it \
             anticlockwise"
        )),
    )
    .doc_param(txt!("Also consider other windows"), None, None);

    add("last-switched-buffer", |pa: &mut Pass| {
        let handle = context::windows().last_switched_buffer(pa)?;
        Ok(Some(txt!("Switched to [buffer]{}", handle.read(pa).name())))
    })
    .doc(
        txt!("Switch to the last switched [a]Buffer[]"),
        Some(txt!(
            "Not to be confused with [caller.info]prev-buffer[], this command switches to the \
             [a]Buffer[] that was active before the current one."
        )),
    );

    add("swap", |pa: &mut Pass, lhs: Handle, rhs: Option<Handle>| {
        let rhs = rhs.unwrap_or_else(|| context::current_buffer(pa));

        context::windows().swap(pa, &lhs.to_dyn(), &rhs.to_dyn())?;

        Ok(Some(txt!(
            "Swapped {} and {}",
            lhs.read(pa).name(),
            rhs.read(pa).name()
        )))
    })
    .doc(
        txt!("Swap the positions of two [a]Buffer[]s"),
        Some(txt!(
            "If only one parameter is given, then will swap that [a]Buffer[] with the current one"
        )),
    )
    .doc_param(txt!("The [a]Buffer[] to swap with"), None, None)
    .doc_param(
        txt!("Optional, switch with this [a]Buffer[] instead of the current one"),
        None,
        None,
    );

    add("colorscheme", |_: &mut Pass, scheme: ColorSchemeArg| {
        crate::form::set_colorscheme(&scheme);
        Ok(Some(txt!("Set colorscheme to [a]{}[]", scheme.0)))
    })
    .doc(txt!("Change the active colorscheme"), None)
    .doc_param(txt!("The name of an existing colorscheme"), None, None);

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
    )
    .doc(
        txt!("Change a [a]Form"),
        Some(txt!(
            "This [a]Form[] must already exist, since setting a [a]Form[] that isn't in use is \
             kind of pointless at runtime"
        )),
    )
    .doc_param(txt!("The name of an existing [a]Form"), None, None)
    .doc_param(
        txt!("Up to three colors"),
        Some(txt!(
            "The first color will be used for the foreground, the second for the background, and \
             the third for the underline. The colors can be in a hex format ([a]#123456[]) or hsl \
             (hsl 1 2 3) format. For hsl, the parameters can be integers from [a]0[] to [a]255[] \
             or percentages between [a]0%[] and [a]100%[].

                 If there are no parameters, then this will be set to the [a]Default[] form"
        )),
        None,
    );
}

pub fn as_buffer_handle((.., node): (usize, &Node)) -> Option<Handle> {
    node.try_downcast()
}
