//! The session of Duat
//!
//! **FOR USE IN THE DUAT EXECUTABLE ONLY**
//!
//! This module defines the [`Session`] struct, which is used to run
//! the whole session of duat, controling everything that is not
//! related to printing or receiving input. This includes interpreting
//! input, updating every widget, updating parsers, mapping keys, etc.
use std::{
    any::TypeId,
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock, mpsc},
    time::Duration,
};

use crossterm::event::{KeyEvent, KeyModifiers, MouseEventKind};

use crate::{
    MetaFunctions, Plugins,
    buffer::{Buffer, BufferOpts, History, PathKind},
    cmd,
    context::{self, DuatReceiver, cache, sender},
    data::Pass,
    form,
    hook::{
        self, BufferClosed, BufferUnloaded, ConfigLoaded, ConfigUnloaded, ExitedDuat,
        FocusedOnDuat, UnfocusedFromDuat,
    },
    mode::{self, Selection, Selections},
    text::{StrsBuf, TwoPoints},
    ui::{
        Coord, Ui, Windows,
        layout::{Layout, MasterOnLeft},
    },
    utils::catch_panic,
};

pub(crate) static BUFFER_OPTS: OnceLock<BufferOpts> = OnceLock::new();

/// A message sent from the parent process.
#[derive(Debug, bincode::Decode, bincode::Encode)]
pub enum MsgFromParent {
    /// The initial state of Duat, including buffers and long lasting
    /// structs.
    InitialState {
        buffers: Vec<Vec<ReloadedBuffer>>,
        structs: HashMap<String, Vec<u8>>,
    },
    /// Content from the clipboard.
    ///
    /// This can be [`None`] because it may fail to be retrieved, or
    /// because there were no changes to it.
    ClipboardContent(Option<String>),
    /// The result of a reload request or event.
    ///
    /// This should show up either after a reload is requested by the
    /// child process, or the parent process decides to start
    /// reloading because of changes to the crate dir.
    ReloadResult(Result<(), String>),
}

/// A message sent from the child process.
#[derive(Debug, bincode::Decode, bincode::Encode)]
pub enum MsgFromChild {
    /// The final state, after ending the child process.
    ///
    /// This represents a successful exit from the child process, and
    /// if the `buffers` field is empty, it means we are quitting Duat
    /// as well.
    FinalState {
        buffers: Vec<Vec<ReloadedBuffer>>,
        structs: HashMap<String, Vec<u8>>,
    },
    /// Spawn a new long lasting process.
    ///
    /// This process will be spawned by the parent executor, so it
    /// owns it instead of the child.
    ///
    /// IPC between the child and the spawned process will be done
    /// through local sockets from the [`interprocess`] crate.
    SpawnProcess {
        path: String,
        args: Vec<String>,
        env: Vec<(String, String)>,
        identifier: String,
    },
    /// Kill a previously spawned long lasting process.
    KillProcess { identifier: String },
    /// Request an external update to the clipboard.
    RequestClipboard,
    /// Manually set the content of the clipboard.
    ///
    /// This is so other processes can read from the clipboard.
    UpdateClipboard(String),
    /// Request a reload.
    ///
    /// This will tell the parent process to start compiling the crate
    /// again.
    RequestReload,
    /// A panic occurred.
    ///
    /// As opposed to [`MsgFromChild::FinalState`], this represents a
    /// scenario where duat panicked and thus couldn't exit
    /// succesfully.
    Panicked(String),
}

/// Configuration for a session of Duat
#[doc(hidden)]
pub struct SessionCfg {
    layout: Box<Mutex<dyn Layout>>,
}

impl SessionCfg {
    pub fn new(meta_functions: &'static MetaFunctions, buffer_opts: BufferOpts) -> Self {
        log::set_logger(Box::leak(Box::new(context::logs()))).unwrap();

        crate::clipboard::set_clipboard(&meta_functions.clipboard_fns);
        crate::notify::set_notify_fns(&meta_functions.notify_fns);
        crate::process::set_process_fns(&meta_functions.process_fns);
        crate::storage::set_storage_fns(&meta_functions.storage_fns);
        BUFFER_OPTS.set(buffer_opts).unwrap();

        SessionCfg {
            layout: Box::new(Mutex::new(MasterOnLeft)),
        }
    }

    pub fn build(
        self,
        ui: Ui,
        buffers: Vec<Vec<ReloadedBuffer>>,
        already_plugged: Vec<TypeId>,
    ) -> Session {
        let plugins = Plugins::_new();
        // SAFETY: The only externally available function for Plugins is to
        // add more plugins, accessing the plugging functions happens only on
        // this thread.
        while let Some((plug, ty)) = {
            plugins
                .0
                .lock()
                .unwrap()
                .iter_mut()
                .find_map(|(f, ty)| f.take().zip(Some(*ty)))
        } {
            if !already_plugged.contains(&ty) {
                catch_panic(|| plug(plugins));
            }
        }

        // SAFETY: This function is only called from the main thread in
        // ../src/setup.rs, and from there, there are no other active
        // Passs, so this is fine.
        let pa = unsafe { &mut Pass::new() };

        cmd::add_session_commands();

        let session = Session { ui };

        let mut layout = Some(self.layout);

        for mut rel_buffers in buffers.into_iter().map(|rf| rf.into_iter()) {
            let opts = *BUFFER_OPTS.get().unwrap();
            let (buffer, is_active) = rel_buffers.next().unwrap().into_buffer(opts, 0);

            if let Some(layout) = layout.take() {
                Windows::initialize(pa, buffer, layout, ui);
            } else {
                let node = context::windows().new_window(pa, buffer);
                if is_active {
                    context::set_current_node(pa, node);
                }
            }

            for (i, reloaded_buffer) in rel_buffers.enumerate() {
                let opts = *BUFFER_OPTS.get().unwrap();
                let layout_order = i + 1;
                let (buffer, is_active) = reloaded_buffer.into_buffer(opts, layout_order);
                let node = context::windows().new_buffer(pa, buffer);
                if is_active {
                    context::set_current_node(pa, node);
                }
            }
        }

        session
    }
}

/// A session of Duat
#[doc(hidden)]
pub struct Session {
    ui: Ui,
}

impl Session {
    /// Creates a new `Session` for Duat.
    ///
    /// This should be done before creating the [`Ui`], but after initial setup,
    #[inline(never)]
    pub fn start(setup: fn() -> (Ui, Vec<TypeId>, BufferOpts)) {
        let mut args = std::env::args();
        let socket_dir = PathBuf::from(args.next().unwrap());
        let config_profile = args.next().unwrap();
        let config_dir = args.next().unwrap();

        crate::utils::set_crate_profile_and_dir(
            config_profile,
            (!config_dir.is_empty()).then_some(config_dir),
        );

        let socket_name = if interprocess::

        match catch_panic(|| self.inner_start(&duat_rx, reload_tx.as_ref())) {
            Some(result) => Some(result.map(|ret| (ret, duat_rx))),
            None => {
                let pa = unsafe { &mut Pass::new() };
                for handle in context::windows().buffers(pa) {
                    _ = handle.save(pa);
                }

                None
            }
        }
    }

    /// Real start, wrapped on a `catch_unwind`
    fn inner_start(
        self,
        duat_rx: &DuatReceiver,
        reload_tx: Option<&mpsc::Sender<ReloadEvent>>,
    ) -> Result<Vec<Vec<ReloadedBuffer>>, String> {
        fn get_windows_nodes(pa: &Pass) -> Vec<Vec<crate::ui::Node>> {
            context::windows()
                .iter(pa)
                .map(|window| window.nodes(pa).cloned().collect())
                .collect()
        }

        form::set_sender(sender());

        // SAFETY: No Passes exists at this point in time.
        let pa = unsafe { &mut Pass::new() };

        hook::trigger(pa, ConfigLoaded(()));

        if mode::reset::<Buffer>(pa).is_none() {
            unreachable!("Somebody forgot to set a default mode, I'm looking at you, duat!");
        };

        let mut reload_countdown = 1;
        let mut unload_instant = None;
        let mut reload_requested = false;
        let mut reprint_screen = false;

        self.ui.flush_layout();

        let mut print_screen = {
            let mut last_win = context::current_win_index(pa);
            let mut last_win_len = context::windows().len(pa);
            let mut windows_nodes = get_windows_nodes(pa);

            let correct_window_nodes = |pa: &mut Pass, windows_nodes: &mut Vec<_>| {
                // Additional Widgets may have been created in the meantime.
                // DDOS vulnerable I guess.
                while let Some(new_additions) = context::windows().get_additions(pa) {
                    self.ui.flush_layout();

                    let cur_win = context::current_win_index(pa);
                    for (_, node) in new_additions.iter().filter(|(win, _)| *win == cur_win) {
                        node.update_and_print(pa, cur_win);
                    }

                    *windows_nodes = get_windows_nodes(pa);
                }
            };

            move |pa: &mut Pass, force: bool| {
                context::windows().cleanup_despawned(pa);
                correct_window_nodes(pa, &mut windows_nodes);

                let cur_win = context::current_win_index(pa);
                let cur_win_len = context::windows().len(pa);

                // When exiting Duat, this will return `None`.
                let Some(window) = windows_nodes.get(cur_win) else {
                    return;
                };

                let mut printed_at_least_one = false;

                for node in window {
                    let windows_changed = cur_win != last_win || cur_win_len != last_win_len;
                    if force || windows_changed || node.needs_update(pa) {
                        node.update_and_print(pa, last_win);
                        printed_at_least_one = true;
                    }
                }

                correct_window_nodes(pa, &mut windows_nodes);

                if printed_at_least_one {
                    self.ui.print()
                }

                last_win = cur_win;
                last_win_len = cur_win_len;
            }
        };

        print_screen(pa, true);

        loop {
            if let Some(event) = duat_rx.recv_timeout(Duration::from_millis(10)) {
                match event {
                    DuatEvent::KeyEventSent(key_event) => {
                        mode::send_key_event(pa, key_event);
                        if mode::keys_were_sent(pa) {
                            continue;
                        }
                    }
                    DuatEvent::MouseEventSent(mouse_event) => {
                        context::current_window(pa)
                            .clone()
                            .send_mouse_event(pa, mouse_event);
                    }
                    DuatEvent::KeyEventsSent(keys) => {
                        for key in keys {
                            mode::send_key_event(pa, key)
                        }
                        if mode::keys_were_sent(pa) {
                            continue;
                        }
                    }
                    DuatEvent::QueuedFunction(f) => _ = catch_panic(|| f(pa)),
                    DuatEvent::Resized | DuatEvent::FormChange => {
                        reprint_screen = true;
                        continue;
                    }
                    DuatEvent::FocusedOnDuat => {
                        hook::trigger(pa, FocusedOnDuat(()));
                    }
                    DuatEvent::UnfocusedFromDuat => {
                        hook::trigger(pa, UnfocusedFromDuat(()));
                    }
                    DuatEvent::RequestReload(reload) => match (&reload_tx, reload_requested) {
                        (Some(reload_tx), false) => {
                            reload_tx.send(reload).unwrap();
                            reload_requested = true;
                        }
                        (Some(_), true) => context::warn!("Waiting for previous reload"),
                        (None, _) => {
                            context::error!("Loaded default config, so reloading is not available")
                        }
                    },
                    DuatEvent::ReloadSucceeded => {
                        let already_called = unload_instant.is_some();
                        let instant = unload_instant.get_or_insert_with(std::time::Instant::now);
                        if !already_called {
                            context::declare_will_unload();

                            for handle in context::windows().buffers(pa) {
                                hook::trigger(pa, BufferUnloaded(handle));
                            }

                            crate::process::interrupt_all();
                            hook::trigger(pa, ConfigUnloaded(()));
                            crate::notify::remove_all_watchers();
                        }

                        let mut thread_count = 0;
                        let mut threads = || {
                            thread_count = thread_amount::thread_amount().unwrap().get()
                                - crate::process::reader_thread_count();
                            thread_count
                        };

                        #[cfg(not(target_vendor = "apple"))]
                        const NORMAL_THREAD_COUNT: usize = 7;
                        #[cfg(target_vendor = "apple")]
                        const NORMAL_THREAD_COUNT: usize = 8;

                        if threads() <= NORMAL_THREAD_COUNT
                            || !already_called && {
                                std::thread::sleep(Duration::from_millis(10));
                                threads() <= NORMAL_THREAD_COUNT
                            }
                        {
                            let ui = self.ui;
                            let buffers = self.take_buffers(pa);
                            ui.unload();
                            return Ok(buffers);
                        } else if instant.elapsed() > Duration::from_secs(reload_countdown) {
                            if reload_countdown == 5 {
                                for handle in context::buffers(pa) {
                                    _ = handle.save(pa);
                                }

                                self.ui.unload();
                                return Err(format!(
                                    "Failed to reload because there were {} threads too many, so \
                                     saved and exited",
                                    thread_count - 7
                                ));
                            }
                            context::warn!(
                                "Reloading because there are {} extra threads open",
                                thread_count - 7
                            );
                            reload_countdown += 1;
                        }
                        context::sender().send_reload_succeeded();
                    }
                    DuatEvent::ReloadFailed => reload_requested = false,
                    DuatEvent::Quit => {
                        context::declare_will_unload();
                        context::declare_will_quit();

                        for handle in context::windows().buffers(pa) {
                            hook::trigger(pa, BufferUnloaded(handle.clone()));
                            hook::trigger(pa, BufferClosed(handle));
                        }

                        crate::process::interrupt_all();
                        hook::trigger(pa, ConfigUnloaded(()));
                        hook::trigger(pa, ExitedDuat(()));

                        self.ui.unload();
                        return Ok(Vec::new());
                    }
                }
            }

            print_screen(pa, reprint_screen);
            reprint_screen = false;
        }
    }

    fn take_buffers(self, pa: &mut Pass) -> Vec<Vec<ReloadedBuffer>> {
        let buffers =
            context::windows()
                .entries(pa)
                .fold(Vec::new(), |mut file_handles, (win, node)| {
                    if win >= file_handles.len() {
                        file_handles.push(Vec::new());
                    }

                    if let Some(handle) = node.try_downcast::<Buffer>() {
                        file_handles.last_mut().unwrap().push(handle)
                    }

                    file_handles
                });

        buffers
            .into_iter()
            .map(|buffers| {
                buffers
                    .into_iter()
                    .map(|handle| {
                        let (buffer, area) = handle.write_with_area(pa);
                        ReloadedBuffer::from_buffer(buffer, area.is_active())
                    })
                    .collect()
            })
            .collect()
    }
}

/// Where exactly did the [`TwoPoints`] for a given [`Coord`] match
///
/// It can be either an exact match, that is, the mouse was in the
/// position of the `TwoPoints`, or it can be on the same line as the
/// `TwoPoints` at the end of the line.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TwoPointsPlace {
    /// The mouse was on top of the character that matched.
    Within(TwoPoints),
    /// The mouse was on the same line as the character.
    AheadOf(TwoPoints),
}

impl TwoPointsPlace {
    /// The [`TwoPoints`] that were interacted with
    pub fn points(&self) -> TwoPoints {
        match self {
            TwoPointsPlace::Within(points) | TwoPointsPlace::AheadOf(points) => *points,
        }
    }
}

/// A mouse event sent by the [`Ui`], doesn't include [`Text`]
/// positioning
///
/// [`Text`]: crate::text::Text
#[derive(Debug, Clone, Copy)]
pub struct UiMouseEvent {
    /// Thee coordinate on screen where the mouse was.
    pub coord: Coord,
    /// What the mouse did.
    pub kind: MouseEventKind,
    /// Modifiers that were pressed during this mouse event.
    pub modifiers: KeyModifiers,
}

/// An event that Duat must handle
#[doc(hidden)]
pub(crate) enum DuatEvent {
    /// A [`KeyEvent`] was typed
    KeyEventSent(KeyEvent),
    /// A [`MouseEvent`] was sent
    MouseEventSent(UiMouseEvent),
    /// Multiple [`KeyEvent`]s were sent
    KeyEventsSent(Vec<KeyEvent>),
    /// A function was queued
    QueuedFunction(Box<dyn FnOnce(&mut Pass) + Send>),
    /// The Screen has resized
    Resized,
    /// A [`Form`] was altered, which one it is, doesn't matter
    ///
    /// [`Form`]: crate::form::Form
    FormChange,
    /// Focused on Duat
    FocusedOnDuat,
    /// Unfocused from Duat
    UnfocusedFromDuat,
    /// Request a reload of the configuration to the executable
    RequestReload(ReloadEvent),
    /// A reloading attempt succeeded
    ReloadSucceeded,
    /// A reloading attempt failed
    ReloadFailed,
    /// Quit Duat
    Quit,
}

/// The parts that compose a [`Buffer`] widget
///
/// **FOR USE BY THE DUAT EXECUTABLE ONLY**
#[doc(hidden)]
#[derive(Debug, bincode::Decode, bincode::Encode)]
pub struct ReloadedBuffer {
    buf: StrsBuf,
    selections: Selections,
    history: History,
    path_kind: PathKind,
    is_active: bool,
}

impl ReloadedBuffer {
    /// Creates a new `ReloadedBuffer` from parts gathered from
    /// arguments
    ///
    /// **MEANT TO BE USED BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub fn by_args(path: Option<PathBuf>, is_active: bool) -> Result<Self, std::io::Error> {
        let (buf, selections, path_kind) = if let Some(path) = path {
            let canon_path = path.canonicalize();
            if let Ok(path) = &canon_path
                && let Ok(buffer) = std::fs::read_to_string(path)
            {
                let selections = {
                    let selection = cache::load(path).unwrap_or_default();
                    Selections::new(selection)
                };
                (
                    StrsBuf::new(buffer),
                    selections,
                    PathKind::SetExists(path.clone()),
                )
            } else if canon_path.is_err()
                && let Ok(mut canon_path) = path.with_file_name(".").canonicalize()
            {
                canon_path.push(path.file_name().unwrap());
                (
                    StrsBuf::new("".to_string()),
                    Selections::new(Selection::default()),
                    PathKind::SetAbsent(canon_path),
                )
            } else {
                (
                    StrsBuf::new("".to_string()),
                    Selections::new(Selection::default()),
                    PathKind::new_unset(),
                )
            }
        } else {
            (
                StrsBuf::new("".to_string()),
                Selections::new(Selection::default()),
                PathKind::new_unset(),
            )
        };

        Ok(Self {
            buf,
            selections,
            history: History::default(),
            path_kind,
            is_active,
        })
    }

    /// Creates a new `ReloadedBuffer` from an already loaded
    /// [`Buffer`].
    pub fn from_buffer(buffer: &mut Buffer, is_active: bool) -> Self {
        let (buf, selections, history) = buffer.take_reload_parts();
        Self {
            buf,
            selections,
            history,
            path_kind: buffer.path_kind(),
            is_active,
        }
    }

    /// Transforms this struct into a new [`Buffer`] and wether or not
    /// it's active.
    pub fn into_buffer(self, opts: BufferOpts, layout_order: usize) -> (Buffer, bool) {
        let Self { buf, selections, history, path_kind, .. } = self;
        (
            Buffer::from_raw_parts(buf, selections, history, path_kind, opts, layout_order),
            self.is_active,
        )
    }
}

/// How Duat should reload
///
/// **FOR USE IN THE DUAT EXECUTABLE ONLY**
#[doc(hidden)]
pub struct ReloadEvent {
    pub clean: bool,
    pub update: bool,
    pub profile: String,
}

fn get_sockets(socket_dir: &Path) -> [interprocess::local_socket::Stream; 2] {
    use interprocess::local_socket::{GenericFilePath, GenericNamespaced, prelude::*};

    let [recv, send] = if GenericNamespaced::is_supported() {
        [
        socket_dir.join("0").to_string_lossy().to_ns_name().unwrap(),
        socket_dir.join("1").to_string_lossy().to_ns_name().unwrap()
        ]
    } else {
        [
        socket_dir.join("0").to_fs_name().unwrap(),
        socket_dir.join("1").to_fs_name().unwrap(),
        ]
    };

    let recv_stream = 
}
