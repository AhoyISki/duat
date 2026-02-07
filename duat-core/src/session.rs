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
    path::PathBuf,
    sync::{Mutex, OnceLock, mpsc},
    time::Duration,
};

use crossterm::event::{KeyEvent, KeyModifiers, MouseEventKind};

use crate::{
    Plugins,
    buffer::{Buffer, BufferOpts},
    clipboard::Clipboard,
    cmd,
    context::{self, DuatReceiver, sender},
    data::Pass,
    form,
    hook::{
        self, BufferClosed, BufferUnloaded, ConfigLoaded, ConfigUnloaded, ExitedDuat,
        FocusedOnDuat, UnfocusedFromDuat,
    },
    mode::{self},
    text::TwoPoints,
    ui::{
        Coord, Ui, Windows,
        layout::{Layout, MasterOnLeft},
    },
    utils::catch_panic,
};

pub(crate) static BUFFER_OPTS: OnceLock<BufferOpts> = OnceLock::new();

/// Configuration for a session of Duat
#[doc(hidden)]
pub struct SessionCfg {
    layout: Box<Mutex<dyn Layout>>,
}

impl SessionCfg {
    pub fn new(clipb: &'static Clipboard, buffer_opts: BufferOpts) -> Self {
        crate::clipboard::set_clipboard(clipb);
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
        crate::buffer::BufferId::set_min(buffers.iter().flatten().map(|rb| rb.buffer.buffer_id()));
        ui.setup_default_print_info();

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
            let ReloadedBuffer { mut buffer, is_active } = rel_buffers.next().unwrap();
            buffer.opts = *BUFFER_OPTS.get().unwrap();

            if let Some(layout) = layout.take() {
                Windows::initialize(pa, buffer, layout, ui);
            } else {
                let node = context::windows().new_window(pa, buffer);
                if is_active {
                    context::set_current_node(pa, node);
                }
            }

            for ReloadedBuffer { mut buffer, is_active } in rel_buffers {
                buffer.opts = *BUFFER_OPTS.get().unwrap();
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
    /// Start the application, initiating a read/response loop.
    ///
    /// If it returns `Some(list, _)`, where `list` is an empty
    /// vector, it means Duat must quit. If the vector is not empty,
    /// each inner `Vec` represents a different window.
    ///
    /// This will return [`None`] if Duat panicked midway through.
    pub fn start(
        self,
        duat_rx: DuatReceiver,
        reload_tx: Option<mpsc::Sender<ReloadEvent>>,
    ) -> Option<(Vec<Vec<ReloadedBuffer>>, DuatReceiver)> {
        match catch_panic(|| self.inner_start(&duat_rx, reload_tx.as_ref())) {
            Some(ret) => Some((ret, duat_rx)),
            None => {
                let pa = unsafe { &mut Pass::new() };
                for handle in context::windows().buffers(pa) {
                    let _ = handle.save(pa);
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
    ) -> Vec<Vec<ReloadedBuffer>> {
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
                            hook::trigger(pa, ConfigUnloaded(()));

                            for handle in context::windows().buffers(pa) {
                                hook::trigger(pa, BufferUnloaded(handle));
                            }
                        }

                        if thread_amount::thread_amount().unwrap().get() <= 7 {
                            context::logs().clear();
                            let ui = self.ui;
                            let buffers = self.take_files(pa);
                            ui.unload();
                            return buffers;
                        } else if instant.elapsed() > Duration::from_secs(5) {
                            self.ui.unload();
                            return Vec::new();
                        } else {
                            context::sender().send_reload_succeeded();
                        }
                    }
                    DuatEvent::ReloadFailed => reload_requested = false,
                    DuatEvent::Quit => {
                        hook::trigger(pa, ConfigUnloaded(()));

                        for handle in context::windows().buffers(pa) {
                            hook::trigger(pa, BufferUnloaded(handle.clone()));
                            hook::trigger(pa, BufferClosed(handle));
                        }

                        hook::trigger(pa, ExitedDuat(()));

                        self.ui.unload();
                        return Vec::new();
                    }
                }
            }

            print_screen(pa, reprint_screen);
            reprint_screen = false;
        }
    }

    fn take_files(self, pa: &mut Pass) -> Vec<Vec<ReloadedBuffer>> {
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
                        let buffer = buffer.prepare_for_reloading();
                        let is_active = area.is_active();

                        ReloadedBuffer { buffer, is_active }
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
pub struct ReloadedBuffer {
    buffer: Buffer,
    is_active: bool,
}

impl ReloadedBuffer {
    /// Creates a new `ReloadedBuffer` from parts gathered from
    /// arguments
    ///
    /// **MEANT TO BE USED BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub fn by_args(path: Option<PathBuf>, is_active: bool) -> Result<Self, std::io::Error> {
        Ok(Self {
            buffer: Buffer::new(path, BufferOpts::default()),
            is_active,
        })
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
