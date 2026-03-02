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
    path::PathBuf,
    sync::{Mutex, OnceLock},
    time::Duration,
};

use crossterm::event::{KeyEvent, KeyModifiers, MouseEventKind};

pub use crate::session::ipc::{MsgFromChild, MsgFromParent, ReloadRequest};
use crate::{
    Plugins,
    buffer::{Buffer, BufferOpts, History, PathKind},
    context::{self, cache},
    data::Pass,
    hook::{
        self, BufferClosed, BufferUnloaded, ConfigLoaded, ConfigUnloaded, FocusedOnDuat,
        UnfocusedFromDuat,
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

/// Starts running duat.
#[doc(hidden)]
#[inline(never)]
pub fn start(setup: fn() -> (Ui, Vec<TypeId>, BufferOpts)) {
    log::set_logger(Box::leak(Box::new(context::logs()))).unwrap();

    let mut args = std::env::args();
    let socket_dir = PathBuf::from(args.next().unwrap());
    let is_first_time = args.next().unwrap().parse().unwrap();
    let config_profile = args.next().unwrap();
    let config_dir = args.next().unwrap();

    crate::utils::set_crate_profile_and_dir(
        config_profile,
        (!config_dir.is_empty()).then_some(config_dir),
    );

    let (mut ipc_tx, mut ipc_rx) = ipc::channel(&socket_dir);

    if catch_panic(|| {
        let MsgFromParent::InitialState { buffers, structs } = ipc_rx.recv() else {
            panic!("Initial message should've been `InitialState`");
        };

        let (ui, already_plugged, buffer_opts) = setup();
        BUFFER_OPTS.set(buffer_opts).unwrap();
        load_remaining_plugins(already_plugged);

        // SAFETY: this is the first time this is called.
        let pa = unsafe { &mut Pass::new() };

        let layout = Box::new(Mutex::new(MasterOnLeft));
        setup_buffers(pa, buffers, ui, layout);

        let buffers = main_loop(ui, &mut ipc_tx, is_first_time);

        let result = ipc_tx.send(if buffers.is_empty() {
            MsgFromChild::FinalState { buffers, structs: HashMap::new() }
        } else {
            let structs = crate::storage::get_structs();
            MsgFromChild::FinalState { buffers, structs }
        });
    })
    .is_none()
    {
        let pa = unsafe { &mut Pass::new() };
        for handle in context::windows().buffers(pa) {
            _ = handle.save(pa);
        }
    }
}

/// Real start, wrapped on a `catch_unwind`
fn main_loop(ui: Ui, ipc_tx: &mut ipc::IpcSender, is_first_time: bool) -> Vec<Vec<ReloadedBuffer>> {
    fn get_windows_nodes(pa: &Pass) -> Vec<Vec<crate::ui::Node>> {
        context::windows()
            .iter(pa)
            .map(|window| window.nodes(pa).cloned().collect())
            .collect()
    }

    let duat_rx = context::receiver();

    // SAFETY: No Passes exists at this point in time.
    let pa = unsafe { &mut Pass::new() };

    hook::trigger(pa, ConfigLoaded(is_first_time));

    if mode::reset::<Buffer>(pa).is_none() {
        unreachable!("Somebody forgot to set a default mode, I'm looking at you, duat!");
    };

    let mut reload_requested = false;
    let mut reprint_screen = false;

    ui.flush_layout();

    let mut print_screen = {
        let mut last_win = context::current_win_index(pa);
        let mut last_win_len = context::windows().len(pa);
        let mut windows_nodes = get_windows_nodes(pa);

        let correct_window_nodes = |pa: &mut Pass, windows_nodes: &mut Vec<_>| {
            // Additional Widgets may have been created in the meantime.
            // DDOS vulnerable I guess.
            while let Some(new_additions) = context::windows().get_additions(pa) {
                ui.flush_layout();

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
                ui.print()
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
                DuatEvent::RequestReload(request) => match reload_requested {
                    false => {
                        ipc_tx.send(MsgFromChild::RequestReload(request));
                        reload_requested = true;
                    }
                    true => context::warn!("Waiting for previous reload"),
                },
                DuatEvent::ReloadSucceeded => {
                    context::declare_will_unload();

                    for handle in context::windows().buffers(pa) {
                        hook::trigger(pa, BufferUnloaded(handle));
                    }

                    hook::trigger(pa, ConfigUnloaded(false));

                    ui.unload();
                    return take_buffers(pa);
                }
                DuatEvent::ReloadFailed => reload_requested = false,
                DuatEvent::Quit => {
                    context::declare_will_unload();
                    context::declare_will_quit();

                    for handle in context::windows().buffers(pa) {
                        hook::trigger(pa, BufferUnloaded(handle.clone()));
                        hook::trigger(pa, BufferClosed(handle));
                    }

                    hook::trigger(pa, ConfigUnloaded(true));

                    ui.unload();
                    return Vec::new();
                }
            }
        }

        print_screen(pa, reprint_screen);
        reprint_screen = false;
    }
}

fn take_buffers(pa: &mut Pass) -> Vec<Vec<ReloadedBuffer>> {
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
    RequestReload(ipc::ReloadRequest),
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

fn load_remaining_plugins(already_plugged: Vec<TypeId>) {
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
}

fn setup_buffers(
    pa: &mut Pass,
    buffers: Vec<Vec<ReloadedBuffer>>,
    ui: Ui,
    layout: Box<Mutex<dyn Layout>>,
) {
    let mut layout = Some(layout);

    for mut window_buffers in buffers.into_iter().map(|rb| rb.into_iter()) {
        let opts = *BUFFER_OPTS.get().unwrap();
        let (buffer, is_active) = window_buffers.next().unwrap().into_buffer(opts, 0);

        if let Some(layout) = layout.take() {
            Windows::initialize(pa, buffer, layout, ui);
        } else {
            let node = context::windows().new_window(pa, buffer);
            if is_active {
                context::set_current_node(pa, node);
            }
        }

        for (i, reloaded_buffer) in window_buffers.enumerate() {
            let opts = *BUFFER_OPTS.get().unwrap();
            let layout_order = i + 1;
            let (buffer, is_active) = reloaded_buffer.into_buffer(opts, layout_order);
            let node = context::windows().new_buffer(pa, buffer);
            if is_active {
                context::set_current_node(pa, node);
            }
        }
    }
}

mod ipc {
    /// A message sent from the parent process.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub enum MsgFromParent {
        /// The initial state of Duat, including buffers and long
        /// lasting structs.
        InitialState {
            buffers: Vec<Vec<ReloadedBuffer>>,
            structs: HashMap<String, Vec<u8>>,
        },
        /// Content from the clipboard.
        ///
        /// This can be [`None`] because it may fail to be retrieved,
        /// or because there were no changes to it.
        ClipboardContent(Option<String>),
        /// The result of a reload request or event.
        ///
        /// This should show up either after a reload is requested by
        /// the child process, or the parent process decides
        /// to start reloading because of changes to the crate
        /// dir.
        ReloadResult(Result<(), String>),
        /// The result of trying to spawn a process.
        // This will become an `std::io::RawOsError` once that feature is stabilized.
        SpawnResult(Result<String, i32>),
    }

    /// A message sent from the child process.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub enum MsgFromChild {
        /// The final state, after ending the child process.
        ///
        /// This represents a successful exit from the child process,
        /// and if the `buffers` field is empty, it means we
        /// are quitting Duat as well.
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
        SpawnProcess(PersistentCommandRequest),
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
        /// This will tell the parent process to start compiling the
        /// crate again.
        RequestReload(ReloadRequest),
        /// A panic occurred.
        ///
        /// As opposed to [`MsgFromChild::FinalState`], this
        /// represents a scenario where duat panicked and thus
        /// couldn't exit succesfully.
        Panicked(String),
    }

    /// How Duat should reload
    #[doc(hidden)]
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub struct ReloadRequest {
        pub clean: bool,
        pub update: bool,
        pub profile: String,
    }

    use std::{collections::HashMap, io::BufReader, path::Path};

    use bincode::{config, decode_from_std_read, encode_into_std_write};
    use interprocess::local_socket::{GenericFilePath, GenericNamespaced, prelude::*};

    use crate::{process::PersistentCommandRequest, session::ReloadedBuffer};

    /// Connect to a socket-based ipc channel with the parent process.
    pub fn channel(socket_dir: &Path) -> (IpcSender, IpcReceiver) {
        let (send, recv) = (socket_dir.join("0"), socket_dir.join("1"));
        let [send, recv] = if GenericNamespaced::is_supported() {
            [
                send.to_string_lossy()
                    .to_string()
                    .to_ns_name::<GenericNamespaced>()
                    .unwrap(),
                recv.to_string_lossy()
                    .to_string()
                    .to_ns_name::<GenericNamespaced>()
                    .unwrap(),
            ]
        } else {
            [
                send.to_fs_name::<GenericFilePath>().unwrap(),
                recv.to_fs_name::<GenericFilePath>().unwrap(),
            ]
        };

        let ipc_rx = IpcReceiver(BufReader::new(LocalSocketStream::connect(recv).unwrap()));
        let ipc_tx = IpcSender(LocalSocketStream::connect(send).unwrap());

        (ipc_tx, ipc_rx)
    }

    pub struct IpcSender(LocalSocketStream);

    impl IpcSender {
        /// Send a message from the child process.
        #[track_caller]
        pub fn send(&mut self, msg: MsgFromChild) {
            if let Err(err) = encode_into_std_write(msg, &mut self.0, config::standard()) {
                panic!("{err}");
            }
        }
    }

    pub struct IpcReceiver(BufReader<LocalSocketStream>);

    impl IpcReceiver {
        /// Send a message from the child process.
        #[track_caller]
        pub fn recv(&mut self) -> MsgFromParent {
            match decode_from_std_read(&mut self.0, config::standard()) {
                Ok(msg) => msg,
                Err(err) => panic!("{err}"),
            }
        }
    }
}
