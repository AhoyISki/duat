use std::{
    any::TypeId,
    path::PathBuf,
    sync::{
        Arc, LazyLock,
        atomic::{AtomicBool, Ordering},
        mpsc,
    },
};

use crossterm::event::KeyEvent;
use parking_lot::Mutex;

use super::{file_entry, widget_entry};
use crate::{
    commands::{self, iter_around, iter_around_rev},
    context, duat_name,
    hooks::{self, ModeSwitched},
    input::Mode,
    text::{Text, err, ok},
    ui::{Event, Ui, Window},
    widgets::{CmdLineMode, CommandLine, File, Node},
};

static SEND_KEY: LazyLock<Mutex<Box<dyn FnMut(KeyEvent) + Send + Sync>>> =
    LazyLock::new(|| Mutex::new(Box::new(|_| {})));
static RESET_MODE: LazyLock<Mutex<Arc<dyn Fn() + Send + Sync>>> =
    LazyLock::new(|| Mutex::new(Arc::new(|| {})));
static SET_MODE: Mutex<Option<Box<dyn FnOnce() + Send + Sync>>> = Mutex::new(None);
static HAS_ENDED: AtomicBool = AtomicBool::new(false);

/// Returns `true` if Duat must quit/reload
///
/// You should use this function in order to check if loops inside
/// of threads should break.
pub fn has_ended() -> bool {
    HAS_ENDED.load(Ordering::Relaxed)
}

pub fn was_mode_set() -> Option<Box<dyn FnOnce() + Send + Sync>> {
    SET_MODE.lock().take()
}

pub fn set_default_mode<M: Mode<U>, U: Ui>(mode: M) {
    *RESET_MODE.lock() = Arc::new(move || set_mode_fn::<M, U>(mode.clone()));
    let mut set_mode = SET_MODE.lock();
    let prev = set_mode.take();
    *set_mode = Some(Box::new(move || {
        if let Some(f) = prev {
            f()
        }
        RESET_MODE.lock()()
    }));
}

pub fn reset_mode() {
    *SET_MODE.lock() = Some(Box::new(|| RESET_MODE.lock()()))
}

pub fn set_mode<U: Ui>(mode: impl Mode<U>) {
    let mut set_mode = SET_MODE.lock();
    let prev = set_mode.take();
    *set_mode = Some(Box::new(move || {
        if let Some(f) = prev {
            f()
        }
        set_mode_fn(mode)
    }));
}

pub fn set_cmd_mode<U: Ui>(mode: impl CmdLineMode<U>) {
    let Ok(cur_file) = context::cur_file::<U>() else {
        return;
    };

    if let Some(node) = cur_file.get_related_widget::<CommandLine<U>>() {
        node.try_downcast::<CommandLine<U>>()
            .unwrap()
            .write()
            .set_mode(mode);
    } else {
        let windows = context::windows::<U>().read();
        let w = context::cur_window();
        let cur_window = &windows[w];

        let mut widgets = {
            let previous = windows[..w].iter().flat_map(Window::nodes);
            let following = windows[(w + 1)..].iter().flat_map(Window::nodes);
            cur_window.nodes().chain(previous).chain(following)
        };

        if let Some(cmd_line) = widgets.find_map(|node| {
            node.data_is::<CommandLine<U>>()
                .then(|| node.try_downcast::<CommandLine<U>>().unwrap())
        }) {
            cmd_line.write().set_mode(mode)
        }
    }
}

/// Switches to the file with the given name
pub fn switch_to_file<U: Ui>(name: impl std::fmt::Display) {
    let windows = context::windows::<U>().read();
    let name = name.to_string();
    match file_entry(&windows, &name) {
        Ok((_, node)) => {
            let node = node.clone();
            *SET_MODE.lock() = Some(Box::new(move || {
                switch_widget(node);
                RESET_MODE.lock().clone()()
            }));
        }
        Err(err) => context::notify(err),
    }
}

pub(crate) fn send_key(key: KeyEvent) {
    SEND_KEY.lock()(key);
}

/// Ends duat, either for reloading the config, or quitting
pub(crate) fn end_session() {
    HAS_ENDED.store(true, Ordering::Relaxed);
    while crate::thread::still_running() {
        std::thread::sleep(std::time::Duration::from_micros(500));
    }
}

pub(crate) fn switch_widget<U: Ui>(node: Node<U>) {
    if let Ok(widget) = context::cur_widget::<U>() {
        widget.node().on_unfocus();
    }

    context::set_cur(node.as_file(), node.clone());

    node.on_focus();
}

pub(crate) fn add_session_commands<U: Ui>(tx: mpsc::Sender<Event>) -> crate::Result<(), ()> {
    commands::add(["quit", "q"], {
        let tx = tx.clone();

        move |_flags, _args| {
            tx.send(Event::Quit).unwrap();
            Ok(None)
        }
    })?;

    commands::add(["write", "w"], move |_flags, mut args| {
        let file = context::cur_file::<U>()?;

        let paths = {
            let mut paths = Vec::new();

            while let Ok(arg) = args.next() {
                paths.push(arg.to_string());
            }

            paths
        };

        if paths.is_empty() {
            file.inspect(|file, _, _| {
                if let Some(name) = file.set_name() {
                    let bytes = file.write()?;
                    ok!("Wrote " [*a] bytes [] " bytes to " [*a] name [] ".")
                } else {
                    Err(err!("Give the file a name, to write it with"))
                }
            })
        } else {
            file.inspect(|file, _, _| {
                let mut bytes = 0;
                for path in &paths {
                    bytes = file.write_to(path)?;
                }

                let files_text = {
                    let mut builder = Text::builder();
                    ok!(builder, [*a] { &paths[0] });

                    for path in paths.iter().skip(1).take(paths.len() - 1) {
                        ok!(builder, [] ", " [*a] path)
                    }

                    if paths.len() > 1 {
                        ok!(builder, [] " and " [*a] { paths.last().unwrap() })
                    }

                    builder.finish()
                };

                ok!("Wrote " [*a] bytes [] " bytes to " files_text [] ".")
            })
        }
    })?;

    commands::add(["edit", "e"], {
        let windows = context::windows::<U>();

        move |_, mut args| {
            let windows = windows.read();
            let file = args.next_else(err!("No path supplied."))?;

            let path = PathBuf::from(file);
            let name = path
                .file_name()
                .ok_or(err!("No file in path"))?
                .to_string_lossy()
                .to_string();

            if !windows.iter().flat_map(Window::nodes).any(|node| {
                matches!(
                    node.inspect_as::<File, bool>(|f| f.name() == name),
                    Some(true)
                )
            }) {
                tx.send(Event::OpenFile(path)).unwrap();
                return ok!("Opened " [*a] file [] ".");
            }

            commands::switch_to_file::<U>(&name);
            ok!("Switched to " [*a] name [] ".")
        }
    })?;

    commands::add(["buffer", "b"], move |_, mut args| {
        let path: PathBuf = args.next_as()?;
        let name = path
            .file_name()
            .ok_or(err!("No file in path"))?
            .to_string_lossy()
            .to_string();

        commands::switch_to_file::<U>(&name);
        ok!("Switched to " [*a] name [] ".")
    })?;

    commands::add(["next-file"], {
        let windows = context::windows();

        move |flags, _| {
            let file = context::cur_file()?;
            let read_windows = windows.read();
            let w = context::cur_window();

            let widget_index = read_windows[w]
                .nodes()
                .position(|node| file.file_ptr_eq(node))
                .unwrap();

            let name = if flags.word("global") {
                iter_around::<U>(&read_windows, w, widget_index)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other open files."))?
            } else {
                let slice = &read_windows[w..=w];
                iter_around(slice, 0, widget_index)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other files open in this window."))?
            };

            commands::switch_to_file::<U>(&name);
            ok!("Switched to " [*a] name [] ".")
        }
    })?;

    commands::add(["prev-file"], {
        let windows = context::windows();

        move |flags, _| {
            let file = context::cur_file()?;
            let windows = windows.read();
            let w = context::cur_window();

            let widget_i = windows[w]
                .nodes()
                .position(|node| file.file_ptr_eq(node))
                .unwrap();

            let name = if flags.word("global") {
                iter_around_rev::<U>(&windows, w, widget_i)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other open files."))?
            } else {
                let slice = &windows[w..=w];
                iter_around_rev(slice, 0, widget_i)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other files open in this window."))?
            };

            commands::switch_to_file::<U>(&name);

            ok!("Switched to " [*a] name [] ".")
        }
    })?;

    Ok(())
}

fn send_key_fn<U: Ui>(mode: &mut impl Mode<U>, key: KeyEvent) {
    let Ok(widget) = context::cur_widget::<U>() else {
        return;
    };

    widget.mutate_data_as(|widget, area, cursors| {
        let mut c = cursors.write();
        *c = mode.send_key(key, widget, area, c.take())
    });
}

fn set_mode_fn<M: Mode<U>, U: Ui>(mut mode: M) {
    // If we are on the correct widget, no switch is needed.
    if context::cur_widget::<U>().unwrap().type_id() != TypeId::of::<M::Widget>() {
        let windows = context::windows().read();
        let w = context::cur_window();
        let entry = if TypeId::of::<M::Widget>() == TypeId::of::<File>() {
            let name = context::cur_file::<U>().unwrap().name();
            file_entry(&windows, &name)
        } else {
            widget_entry::<M::Widget, U>(&windows, w)
        };

        match entry {
            Ok((_, node)) => switch_widget(node.clone()),
            Err(err) => {
                context::notify(err);
                return;
            }
        };
    }

    context::mode_name().mutate(|mode| {
        let new_mode = duat_name::<M>();
        hooks::trigger::<ModeSwitched>((mode, new_mode));
        *mode = new_mode;
    });

    *SEND_KEY.lock() = Box::new(move |key| send_key_fn::<U>(&mut mode, key));
}
