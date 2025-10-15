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
    sync::{
        Mutex, OnceLock,
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    time::Duration,
};

use crossterm::event::KeyEvent;

use crate::{
    Plugins,
    cfg::PrintCfg,
    clipboard::Clipboard,
    cmd,
    context::{self, Cache, sender},
    data::Pass,
    file::File,
    form,
    hook::{
        self, ConfigLoaded, ConfigUnloaded, ExitedDuat, FileClosed, FileReloaded, FocusedOnDuat,
        UnfocusedFromDuat,
    },
    mode,
    ui::{
        Ui, Windows,
        layout::{Layout, MasterOnLeft},
    },
};

pub(crate) static FILE_CFG: OnceLock<PrintCfg> = OnceLock::new();

/// Configuration for a session of Duat
#[doc(hidden)]
pub struct SessionCfg {
    layout: Box<Mutex<dyn Layout>>,
}

impl SessionCfg {
    pub fn new(clipb: &'static Mutex<Clipboard>, file_cfg: PrintCfg) -> Self {
        crate::clipboard::set_clipboard(clipb);
        FILE_CFG.set(file_cfg).unwrap();

        SessionCfg {
            layout: Box::new(Mutex::new(MasterOnLeft)),
        }
    }

    pub fn build(
        self,
        ui: Ui,
        files: Vec<Vec<ReloadedFile>>,
        already_plugged: Vec<TypeId>,
    ) -> Session {
        ui.setup_default_print_info();

        let plugins = Plugins::_new();
        // SAFETY: The only externally available function for Plugins is to
        // add more plugins, accessing the plugging functions happens only on
        // this thread.
        while let Some((plug, ty)) = {
            unsafe { plugins.0.get() }
                .lock()
                .iter_mut()
                .find_map(|(f, ty)| f.take().zip(Some(*ty)))
        } {
            if !already_plugged.contains(&ty) {
                plug(plugins);
            }
        }

        // SAFETY: This function is only called from the main thread in
        // ../src/setup.rs, and from there, there are no other active
        // Passs, so this is fine.
        let pa = unsafe { &mut Pass::new() };

        cmd::add_session_commands();

        let session = Session { ui };

        let mut layout = Some(self.layout);

        for mut rel_files in files.into_iter().map(|rf| rf.into_iter()) {
            let ReloadedFile { mut file, is_active } = rel_files.next().unwrap();
            *file.cfg() = *FILE_CFG.get().unwrap();

            if let Some(layout) = layout.take() {
                Windows::initialize(pa, file, layout, ui);
            } else {
                let node = context::windows().new_window(pa, file);
                if is_active {
                    context::set_current_node(pa, node);
                }
            }

            for ReloadedFile { mut file, is_active } in rel_files {
                *file.cfg() = *FILE_CFG.get().unwrap();
                let node = context::windows().new_file(pa, file);
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
    pub fn start(
        self,
        duat_rx: mpsc::Receiver<DuatEvent>,
        spawn_count: &'static AtomicUsize,
        reload_tx: Option<mpsc::Sender<ReloadEvent>>,
    ) -> (Vec<Vec<ReloadedFile>>, mpsc::Receiver<DuatEvent>) {
        fn get_windows_nodes(pa: &Pass) -> Vec<Vec<crate::ui::Node>> {
            context::windows()
                .windows(pa)
                .map(|window| window.nodes().cloned().collect())
                .collect()
        }

        form::set_sender(DuatSender::new(sender()));

        // SAFETY: No Passes exists at this point in time.
        let pa = unsafe { &mut Pass::new() };

        hook::trigger(pa, ConfigLoaded(()));

        let Some(mode_fn) = mode::take_set_mode_fn(pa) else {
            unreachable!("Somebody forgot to set a default mode, I'm looking at you `duat`!");
        };
        mode_fn(pa);

        let mut reload_requested = false;
        let mut reprint_screen = false;

        self.ui.flush_layout();

        let mut print_screen = {
            let mut last_win = context::cur_window(pa);
            let mut windows_nodes = get_windows_nodes(pa);

            move |pa: &mut Pass, force: bool| {
                context::windows().cleanup_despawned(pa);
                let cur_win = context::cur_window(pa);

                let mut printed_at_least_one = false;
                for node in windows_nodes.get(last_win).unwrap() {
                    if force || cur_win != last_win || node.needs_update(pa) {
                        node.update_and_print(pa, last_win);
                        printed_at_least_one = true;
                    }
                }

                // Additional Widgets may have been created in the meantime.
                // DDOS vulnerable i guess.
                while let Some(new_additions) = context::windows().get_additions(pa) {
                    self.ui.flush_layout();

                    let cur_win = context::cur_window(pa);
                    for (_, node) in new_additions.iter().filter(|(win, _)| *win == cur_win) {
                        node.update_and_print(pa, cur_win);
                    }

                    windows_nodes = get_windows_nodes(pa);
                }

                if printed_at_least_one {
                    self.ui.print()
                }

                last_win = cur_win;
            }
        };

        print_screen(pa, true);

        loop {
            if let Some(mode_fn) = mode::take_set_mode_fn(pa) {
                mode_fn(pa);
            }

            if let Ok(event) = duat_rx.recv_timeout(Duration::from_millis(10)) {
                match event {
                    DuatEvent::KeySent(key) => {
                        mode::send_key(pa, key);
                        if mode::keys_were_sent(pa) {
                            continue;
                        }
                    }
                    DuatEvent::KeysSent(keys) => {
                        for key in keys {
                            mode::send_key(pa, key)
                        }
                        if mode::keys_were_sent(pa) {
                            continue;
                        }
                    }
                    DuatEvent::QueuedFunction(f) => f(pa),
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
                        hook::trigger(pa, ConfigUnloaded(()));
                        context::order_reload_or_quit();
                        wait_for_threads_to_end(spawn_count);

                        let handles: Vec<_> = context::windows().file_handles(pa).collect();
                        for handle in handles {
                            hook::trigger(pa, FileReloaded((handle, Cache::new())));
                        }

                        let ui = self.ui;
                        let files = self.take_files(pa);
                        ui.unload();
                        return (files, duat_rx);
                    }
                    DuatEvent::ReloadFailed => reload_requested = false,
                    DuatEvent::Quit => {
                        hook::trigger(pa, ConfigUnloaded(()));
                        hook::trigger(pa, ExitedDuat(()));
                        context::order_reload_or_quit();
                        wait_for_threads_to_end(spawn_count);

                        let handles: Vec<_> = context::windows().file_handles(pa).collect();
                        for handle in handles {
                            hook::trigger(pa, FileClosed((handle, Cache::new())));
                        }

                        self.ui.unload();
                        return (Vec::new(), duat_rx);
                    }
                }
            }

            print_screen(pa, reprint_screen);
            reprint_screen = false;
        }
    }

    fn take_files(self, pa: &mut Pass) -> Vec<Vec<ReloadedFile>> {
        let files =
            context::windows()
                .entries(pa)
                .fold(Vec::new(), |mut file_handles, (win, _, node)| {
                    if win >= file_handles.len() {
                        file_handles.push(Vec::new());
                    }

                    if let Some(handle) = node.try_downcast::<File>() {
                        file_handles.last_mut().unwrap().push(handle)
                    }

                    file_handles
                });

        files
            .into_iter()
            .map(|files| {
                files
                    .into_iter()
                    .map(|handle| {
                        let (file, area) = handle.write_with_area(pa);
                        let file = file.prepare_for_reloading();
                        let is_active = area.is_active();

                        ReloadedFile { file, is_active }
                    })
                    .collect()
            })
            .collect()
    }
}

/// An event that Duat must handle
#[doc(hidden)]
pub enum DuatEvent {
    /// A [`KeyEvent`] was typed
    KeySent(KeyEvent),
    /// Multiple [`KeyEvent`]s were sent
    KeysSent(Vec<KeyEvent>),
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

/// A sender of [`DuatEvent`]s
pub struct DuatSender(mpsc::Sender<DuatEvent>);

impl DuatSender {
    /// Returns a new [`DuatSender`]
    pub fn new(sender: mpsc::Sender<DuatEvent>) -> Self {
        Self(sender)
    }

    /// Sends a [`KeyEvent`]
    pub fn send_key(&self, key: KeyEvent) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::KeySent(key))
    }

    /// Sends a notice that the app has resized
    pub fn send_resize(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::Resized)
    }

    /// Triggers the [`FocusedOnDuat`] [`hook`]
    ///
    /// [`FocusedOnDuat`]: crate::hook::FocusedOnDuat
    /// [`hook`]: crate::hook
    pub fn send_focused(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::FocusedOnDuat)
    }

    /// Triggers the [`UnfocusedFromDuat`] [`hook`]
    ///
    /// [`UnfocusedFromDuat`]: crate::hook::UnfocusedFromDuat
    /// [`hook`]: crate::hook
    pub fn send_unfocused(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::UnfocusedFromDuat)
    }

    /// Sends a notice that a [`Form`] has changed
    ///
    /// [`Form`]: crate::form::Form
    pub(crate) fn send_form_changed(&self) -> Result<(), mpsc::SendError<DuatEvent>> {
        self.0.send(DuatEvent::FormChange)
    }
}

/// The parts that compose a [`File`] widget
///
/// **FOR USE BY THE DUAT EXECUTABLE ONLY**
#[doc(hidden)]
pub struct ReloadedFile {
    file: File,
    is_active: bool,
}

impl ReloadedFile {
    /// Creates a new [`FileParts`] from parts gathered from arguments
    ///
    /// **MEANT TO BE USED BY THE DUAT EXECUTABLE ONLY**
    #[doc(hidden)]
    pub fn by_args(path: Option<PathBuf>, is_active: bool) -> Result<Self, std::io::Error> {
        Ok(Self {
            file: File::new(path, PrintCfg::default_for_input()),
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

fn wait_for_threads_to_end(spawn_count: &'static AtomicUsize) {
    let mut count = spawn_count.load(Ordering::Relaxed);
    while count > 0 {
        std::thread::sleep(std::time::Duration::from_millis(10));
        count = spawn_count.load(Ordering::Relaxed);
    }
}
