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
    session::ipc::{InitialState, MsgFromChild},
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

    let mut args = std::env::args().skip(1);
    let socket_dir = PathBuf::from(args.next().unwrap());
    let is_first_time: bool = args.next().unwrap().parse().unwrap();
    let config_profile = args.next().unwrap();
    let config_dir = args.next().unwrap();

    crate::utils::set_crate_profile_and_dir(
        config_profile.clone(),
        (config_dir != "--").then_some(config_dir),
    );

    ipc::initialize_main_channel(&socket_dir);
    println!("initialized ipc channel");

    if catch_panic(|| {
        let InitialState { buffers, structs, clipb, reload_start } = ipc::recv_init();
        println!("received initial state");

        crate::storage::set_structs(structs);
        if let Some(clipboard) = clipb {
            crate::clipboard::set(clipboard);
        }

        let (ui, already_plugged, buffer_opts) = setup();
        BUFFER_OPTS.set(buffer_opts).unwrap();
        load_remaining_plugins(already_plugged);

        // SAFETY: this is the first time this is called.
        let pa = unsafe { &mut Pass::new() };

        let layout = Box::new(Mutex::new(MasterOnLeft));
        setup_buffers(pa, buffers, ui, layout);

        if let Some(reload_start) = reload_start {
            let time = reload_start.elapsed().unwrap();
            context::info!("[a]{config_profile}[] reloaded in [a]{time:?}");
        } else if !is_first_time {
            context::info!("[a]{config_profile}[] reloaded");
        }

        let buffers = main_loop(ui, is_first_time);

        ipc::send(if buffers.is_empty() {
            let structs = HashMap::new();
            MsgFromChild::FinalState(ipc::FinalState { buffers, structs })
        } else {
            let structs = crate::storage::get_structs();
            MsgFromChild::FinalState(ipc::FinalState { buffers, structs })
        });
    })
    .is_none()
    {
        // SAFETY: All other Passes have been destroyed at this point.
        let pa = unsafe { &mut Pass::new() };
        for handle in context::windows().buffers(pa) {
            _ = handle.save(pa);
        }
    }
}

/// Real start, wrapped on a `catch_unwind`
fn main_loop(ui: Ui, is_first_time: bool) -> Vec<Vec<ReloadedBuffer>> {
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
                        ipc::send(MsgFromChild::RequestReload(request));
                        reload_requested = true;
                    }
                    true => context::warn!("Waiting for previous reload"),
                },
                DuatEvent::ReloadResult(Ok(())) => {
                    context::declare_will_unload();

                    for handle in context::windows().buffers(pa) {
                        hook::trigger(pa, BufferUnloaded(handle));
                    }

                    hook::trigger(pa, ConfigUnloaded(false));

                    ui.unload();
                    return take_buffers(pa);
                }
                DuatEvent::ReloadResult(Err(err)) => {
                    reload_requested = false;
                    context::error!("{err}");
                }
                DuatEvent::Quit => {
                    context::declare_will_unload();

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

/// Where exactly did the [`TwoPoints`] for a given [`Coord`] match.
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
    /// The [`TwoPoints`] that were interacted with.
    pub fn points(&self) -> TwoPoints {
        match self {
            TwoPointsPlace::Within(points) | TwoPointsPlace::AheadOf(points) => *points,
        }
    }
}

/// A mouse event sent by the [`Ui`], doesn't include [`Text`]
/// positioning.
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

/// An event that Duat must handle.
pub(crate) enum DuatEvent {
    /// A [`KeyEvent`] was typed.
    KeyEventSent(KeyEvent),
    /// A [`MouseEvent`] was sent.
    MouseEventSent(UiMouseEvent),
    /// Multiple [`KeyEvent`]s were sent.
    KeyEventsSent(Vec<KeyEvent>),
    /// A function was queued.
    QueuedFunction(Box<dyn FnOnce(&mut Pass) + Send>),
    /// The Screen has resized.
    Resized,
    /// A [`Form`] was altered, which one it is, doesn't matter.
    ///
    /// [`Form`]: crate::form::Form
    FormChange,
    /// Focused on Duat.
    FocusedOnDuat,
    /// Unfocused from Duat.
    UnfocusedFromDuat,
    /// Request a reload of the configuration to the executable.
    RequestReload(ipc::ReloadRequest),
    /// The result of a reloading event.
    ReloadResult(Result<(), String>),
    /// Quit Duat.
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

#[doc(hidden)]
pub mod ipc {
    //! Everything related to IPC.
    //!
    //! This includes the interprocess communication that needs to
    //! take place between the duat executor and the duat config, as
    //! well as the communication of [`PersistentChild`]ren, since
    //! those are spawned in the parent, and communication would have
    //! to go through them first.
    //!
    //! [`PersistentChild`]: crate::process::PersistentChild
    use std::{
        collections::HashMap,
        io::{BufReader, Chain, Cursor, Read},
        path::{Path, PathBuf},
        sync::{LazyLock, Mutex, OnceLock, mpsc},
        time::SystemTime,
    };

    use bincode::{config, decode_from_std_read, encode_into_std_write};
    use interprocess::local_socket::{GenericFilePath, GenericNamespaced, Name, prelude::*};

    use crate::{
        context,
        process::PersistentCommandRequest,
        session::{DuatEvent, ReloadedBuffer},
        storage::MaybeTypedValues,
    };

    /// A message sent from the parent process.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub enum MsgFromParent {
        /// The initial state of Duat, including buffers and long
        /// lasting structs.
        InitialState(InitialState),
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
        // The i32 will become an `std::io::RawOsError` once that feature is stabilized.
        SpawnResult(Result<String, i32>),
        /// The result of trying to kill a process.
        // The i32 will become an `std::io::RawOsError` once that feature is stabilized.
        KillResult(Result<(), i32>),
    }

    /// A message sent from the child process.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub enum MsgFromChild {
        /// The final state, after ending the child process.
        ///
        /// This represents a successful exit from the child process,
        /// and if the `buffers` field is empty, it means we
        /// are quitting Duat as well.
        FinalState(FinalState),
        /// Spawn a new long lasting process.
        ///
        /// This process will be spawned by the parent executor, so it
        /// owns it instead of the child.
        ///
        /// IPC between the child and the spawned process will be done
        /// through local sockets from the [`interprocess`] crate.
        SpawnProcess(PersistentCommandRequest),
        /// Kill a previously spawned long lasting process.
        KillProcess(String),
        /// Request that the parent executor stop writing to a
        /// process.
        InterruptWrites(String),
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

    /// The initial state of the duat child process.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub struct InitialState {
        pub buffers: Vec<Vec<ReloadedBuffer>>,
        pub structs: HashMap<String, MaybeTypedValues>,
        pub clipb: Option<String>,
        pub reload_start: Option<SystemTime>,
    }

    /// The final state of the duat child process.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub struct FinalState {
        pub buffers: Vec<Vec<ReloadedBuffer>>,
        pub structs: HashMap<String, MaybeTypedValues>,
    }

    /// A request for reloading duat.
    #[derive(Debug, bincode::Decode, bincode::Encode)]
    pub struct ReloadRequest {
        pub clean: bool,
        pub update: bool,
        pub profile: String,
    }

    static SOCKET_DIR: OnceLock<&Path> = OnceLock::new();
    static CHILD_OUTPUT: OnceLock<Mutex<LocalSocketStream>> = OnceLock::new();
    static CLIPB_CHANNEL: LazyLock<Channel<Option<String>>> = Channel::lazy();
    static SPAWN_CHANNEL: LazyLock<Channel<Result<String, i32>>> = Channel::lazy();
    static KILL_CHANNEL: LazyLock<Channel<Result<(), i32>>> = Channel::lazy();
    static INIT_CHANNEL: LazyLock<Channel<InitialState>> = Channel::lazy();

    /// Send a message from the child process.
    #[track_caller]
    pub(crate) fn send(msg: MsgFromChild) {
        let mut tx = CHILD_OUTPUT.get().unwrap().lock().unwrap();
        if let Err(err) = encode_into_std_write(msg, &mut *tx, config::standard()) {
            panic!("{err}");
        }
    }

    /// Receive the [`InitialState`] event.
    ///
    /// This should be done as Duat is starting.
    pub(crate) fn recv_init() -> InitialState {
        INIT_CHANNEL.rx.lock().unwrap().recv().unwrap()
    }

    /// Receive the next clipboard event.
    pub(crate) fn recv_clipboard() -> Option<String> {
        CLIPB_CHANNEL.rx.lock().unwrap().recv().unwrap()
    }

    /// Receive the next spawn event.
    pub(crate) fn recv_spawn() -> Result<String, i32> {
        SPAWN_CHANNEL.rx.lock().unwrap().recv().unwrap()
    }

    /// Receive the next [`Child`] kill event.
    ///
    /// [`Child`]: std::process::Child
    pub(crate) fn recv_kill() -> Result<(), i32> {
        KILL_CHANNEL.rx.lock().unwrap().recv().unwrap()
    }

    /// Connect to a socket-based ipc channel with the parent process.
    pub fn initialize_main_channel(socket_dir: &Path) {
        let child_input = get_name(socket_dir.join("0"));
        let child_output = get_name(socket_dir.join("1"));

        let mut child_input = BufReader::new(LocalSocketStream::connect(child_input).unwrap());
        let child_output = LocalSocketStream::connect(child_output).unwrap();

        std::thread::spawn(move || {
            while let Ok(msg) = decode_from_std_read(&mut child_input, config::standard()) {
                match msg {
                    MsgFromParent::InitialState(state) => INIT_CHANNEL.tx.send(state).unwrap(),
                    MsgFromParent::ClipboardContent(content) => {
                        crate::log_to_file!("received content");
                        CLIPB_CHANNEL.tx.send(content).unwrap()
                    }
                    MsgFromParent::ReloadResult(result) => {
                        context::sender().send(DuatEvent::ReloadResult(result));
                    }
                    MsgFromParent::SpawnResult(result) => SPAWN_CHANNEL.tx.send(result).unwrap(),
                    MsgFromParent::KillResult(result) => KILL_CHANNEL.tx.send(result).unwrap(),
                }
            }
        });

        SOCKET_DIR.set(socket_dir.to_path_buf().leak()).unwrap();
        CHILD_OUTPUT.set(Mutex::new(child_output)).ok().unwrap();
    }

    /// Connect to a [`PersistentChild`] channel.
    ///
    /// [`PersistentChild`]: crate::process::PersistentChild
    pub(crate) fn connect_process_channel(
        identifier: &str,
        stdout_bytes: Vec<u8>,
        stderr_bytes: Vec<u8>,
    ) -> std::io::Result<(LocalSocketStream, [IpcReader; 2])> {
        let socket_dir = SOCKET_DIR.get().unwrap().join("proc").join(identifier);

        let stdin = LocalSocketStream::connect(get_name(socket_dir.join("0")))?;
        let stdout = BufReader::new(
            Cursor::new(stdout_bytes)
                .chain(LocalSocketStream::connect(get_name(socket_dir.join("1")))?),
        );
        let stderr = BufReader::new(
            Cursor::new(stderr_bytes)
                .chain(LocalSocketStream::connect(get_name(socket_dir.join("2")))?),
        );

        Ok((stdin, [stdout, stderr]))
    }

    /// Get the name of a [`LocalSocketStream`]
    fn get_name(path: PathBuf) -> Name<'static> {
        if GenericNamespaced::is_supported() {
            path.to_string_lossy()
                .to_string()
                .to_ns_name::<GenericNamespaced>()
                .unwrap()
        } else {
            path.to_fs_name::<GenericFilePath>().unwrap()
        }
    }

    /// A simple channel to send stuff over.
    struct Channel<T> {
        tx: mpsc::Sender<T>,
        rx: Mutex<mpsc::Receiver<T>>,
    }

    impl<T> Channel<T> {
        /// Returns a new [`LazyLock<Channel>`]
        const fn lazy() -> LazyLock<Self> {
            LazyLock::new(|| {
                let (tx, rx) = mpsc::channel();
                Self { tx, rx: Mutex::new(rx) }
            })
        }
    }

    /// A stream of bytes, including possibly missed ones.
    pub type IpcReader = BufReader<Chain<Cursor<Vec<u8>>, LocalSocketStream>>;
}
