use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use duat::process::PersistentChild;
use duat_core::{
    clipboard::ClipboardFns,
    context,
    notify::{NotifyFns, WatcherCallback},
    process::ProcessFns,
};
use interrupt_read::Interruptor;
use notify::{
    Event, RecommendedWatcher,
    RecursiveMode::{NonRecursive, Recursive},
    Watcher,
};

#[cfg(not(target_os = "android"))]
pub fn get_clipboard_fns() -> ClipboardFns {
    use std::sync::{Mutex, OnceLock};

    enum ClipboardType {
        Platform(arboard::Clipboard),
        Local(Option<String>),
    }

    static CLIPBOARD: OnceLock<Mutex<ClipboardType>> = OnceLock::new();

    _ = CLIPBOARD.set(Mutex::new(match arboard::Clipboard::new() {
        Ok(clipb) => ClipboardType::Platform(clipb),
        Err(err) => {
            context::error!("{err}");
            ClipboardType::Local(None)
        }
    }));

    duat_core::clipboard::ClipboardFns {
        get_text: || match &mut *CLIPBOARD.get().unwrap().lock().unwrap() {
            ClipboardType::Platform(clipb) => clipb.get_text().ok(),
            ClipboardType::Local(text) => text.clone(),
        },
        set_text: |text| match &mut *CLIPBOARD.get().unwrap().lock().unwrap() {
            ClipboardType::Platform(clipb) => clipb.set_text(text).unwrap(),
            ClipboardType::Local(old) => *old = Some(text),
        },
    }
}
#[cfg(target_os = "android")]
fn get_clipboard() -> ClipboardFns {
    duat_core::clipboard::ClipboardFns {
        get_text: || {
            android_clipboard::get_text()
                .map_err(|err| context::error!("{err}"))
                .ok()
        },
        set_text: |text| {
            if let Err(err) = android_clipboard::set_text(text.to_string()) {
                context::error!("{err}");
            }
        },
    }
}

pub fn get_notify_fns() -> NotifyFns {
    use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

    use notify::{Error, ErrorKind};

    static WATCHER_COUNT: AtomicUsize = AtomicUsize::new(0);
    static WATCHERS: LazyLock<Mutex<HashMap<usize, RecommendedWatcher>>> =
        LazyLock::new(Mutex::default);

    fn convert<T>(result: Result<T, Error>) -> std::io::Result<T> {
        match result {
            Ok(ok) => Ok(ok),
            Err(err) => Err(match err.kind {
                ErrorKind::Generic(err) => std::io::Error::other(err),
                ErrorKind::Io(err) => err,
                ErrorKind::PathNotFound => std::io::Error::new(std::io::ErrorKind::NotFound, err),
                ErrorKind::WatchNotFound => {
                    std::io::Error::other("Attempted to remove path that wasn't being watched.")
                }
                ErrorKind::MaxFilesWatch => {
                    std::io::Error::other("Reached maximum number of files to watch.")
                }
                ErrorKind::InvalidConfig(_) => unreachable!(),
            }),
        }
    }

    /// A wrapper to implement the [`notify::EventHandler`] trait.
    struct WatcherCallbackWrapper(WatcherCallback);

    impl notify::EventHandler for WatcherCallbackWrapper {
        fn handle_event(&mut self, result: notify::Result<Event>) {
            self.0.call(convert(result));
        }
    }

    NotifyFns {
        spawn_watcher: |callback| {
            let watcher = convert(notify::recommended_watcher(WatcherCallbackWrapper(
                callback,
            )))?;

            let id = WATCHER_COUNT.fetch_add(1, Relaxed);
            WATCHERS.lock().unwrap().insert(id, watcher);
            Ok(id)
        },
        watch_path: |id, path| {
            let mut watchers = WATCHERS.lock().unwrap();
            convert(watchers.get_mut(&id).unwrap().watch(path, NonRecursive))
        },
        watch_path_recursive: |id, path| {
            let mut watchers = WATCHERS.lock().unwrap();
            convert(watchers.get_mut(&id).unwrap().watch(path, Recursive))
        },
        unwatch_path: |id, path| {
            let mut watchers = WATCHERS.lock().unwrap();
            convert(watchers.get_mut(&id).unwrap().unwatch(path))
        },
        unwatch_all: |id| _ = WATCHERS.lock().unwrap().remove(&id),
        remove_all_watchers: || WATCHERS.lock().unwrap().clear(),
    }
}

pub fn get_process_fns() -> ProcessFns {
    use std::io::BufWriter;

    static INTERRUPTORS: Mutex<Vec<Interruptor>> = Mutex::new(Vec::new());
    static READER_THREADS: Mutex<Vec<Box<dyn Fn() -> bool + Send>>> = Mutex::new(Vec::new());

    ProcessFns {
        spawn: |command| {
            let mut child = command.spawn()?;
            let mut interruptors = INTERRUPTORS.lock().unwrap();
            let mut reader_threads = READER_THREADS.lock().unwrap();

            let stdin = child.stdin.take().map(BufWriter::new);
            let stdout = child.stdout.take().map(|stdout| {
                let (stdout, interruptor) = interrupt_read::pair(stdout);
                interruptors.push(interruptor);
                reader_threads.push(Box::new(stdout.is_reading_fn()));
                stdout
            });
            let stderr = child.stderr.take().map(|stderr| {
                let (stderr, interruptor) = interrupt_read::pair(stderr);
                interruptors.push(interruptor);
                reader_threads.push(Box::new(stderr.is_reading_fn()));
                stderr
            });

            Ok(PersistentChild { child, stdin, stdout, stderr })
        },
        get_child: |key| PROCESSES.lock().unwrap().remove(&key),
        store_child: |key, child| PROCESSES.lock().unwrap().insert(key, child),
        interrupt_all: || {
            INTERRUPTORS
                .lock()
                .unwrap()
                .retain(|interruptor| interruptor.interrupt().is_ok())
        },
        reader_thread_count: || {
            let mut count = 0;
            READER_THREADS.lock().unwrap().retain(|is_reading| {
                let is_reading = is_reading();
                count += is_reading as usize;
                is_reading
            });
            count
        },
    }
}

/// Kills all remaining processes.
pub fn kill_remaining_processes() {
    for (_, mut child) in PROCESSES.lock().unwrap().drain() {
        _ = child.child.kill();
    }
}

static PROCESSES: LazyLock<Mutex<HashMap<String, PersistentChild>>> = LazyLock::new(Mutex::default);
