use std::{
    path::PathBuf,
    sync::{Mutex, mpsc},
    time::{Duration, Instant},
};

use crate::{
    cfg::PrintCfg,
    clipboard::Clipboard,
    cmd,
    context::{self, Cache, sender},
    data::Pass,
    file::{File, FileCfg, PathKind},
    form,
    hook::{
        self, ConfigLoaded, ConfigUnloaded, ExitedDuat, FocusedOnDuat, OnFileClose, OnFileReload,
        UnfocusedFromDuat,
    },
    mode,
    text::Bytes,
    ui::{
        Area, DuatEvent, Sender, Ui, Widget, Windows,
        layout::{Layout, MasterOnLeft},
    },
};

#[doc(hidden)]
pub struct SessionCfg<U: Ui> {
    file_cfg: FileCfg,
    layout_fn: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
}

impl<U: Ui> SessionCfg<U> {
    pub fn new(clipb: &'static Mutex<Clipboard>) -> Self {
        crate::clipboard::set_clipboard(clipb);

        SessionCfg {
            file_cfg: FileCfg::new(),
            layout_fn: Box::new(|| Box::new(MasterOnLeft)),
        }
    }

    pub fn session_from_args(self, ms: &'static U::MetaStatics) -> Session<U> {
        // SAFETY: This function is only called from the main thread in
        // ../src/setup.rs, and from there, there are no other active
        // Passs, so this is fine.
        let pa = unsafe { &mut Pass::new() };

        context::set_windows(Windows::<U>::new());

        cmd::add_session_commands::<U>();

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let file_cfg = if let Some(name) = first {
            self.file_cfg.clone().open_path(name)
        } else {
            self.file_cfg.clone()
        };

        let node = context::windows().new_window(pa, ms, file_cfg, (self.layout_fn)());

        let session = Session {
            ms,
            file_cfg: self.file_cfg,
            layout_fn: self.layout_fn,
        };

        context::set_cur(pa, node.try_downcast(), node.clone());

        for file in args {
            session.open_file_from_path(pa, context::cur_window(), PathBuf::from(file));
        }

        session
    }

    pub fn session_from_prev(
        self,
        ms: &'static U::MetaStatics,
        prev: Vec<Vec<FileRet>>,
    ) -> Session<U> {
        cmd::add_session_commands::<U>();
        // SAFETY: This function is only called from the main thread in
        // ../src/setup.rs, and from there, there are no other active
        // Passs, so this is fine.
        let pa = unsafe { &mut Pass::new() };

        context::set_windows::<U>(Windows::new());

        let file_cfg = self.file_cfg.clone();
        let inherited_cfgs = prev.into_iter().enumerate().map(|(i, cfgs)| {
            let cfgs = cfgs.into_iter().map(|file_ret| {
                let bytes = file_ret.bytes;
                let pk = file_ret.path_kind;
                let unsaved = file_ret.has_unsaved_changes;
                let file_cfg = file_cfg.clone().take_from_prev(bytes, pk, unsaved);
                (file_cfg, file_ret.is_active)
            });
            (i, cfgs)
        });

        let mut session = Session {
            ms,
            file_cfg: self.file_cfg,
            layout_fn: self.layout_fn,
        };

        let mut hasnt_set_cur = true;
        for (win, mut cfgs) in inherited_cfgs {
            let (file_cfg, is_active) = cfgs.next().unwrap();

            let node = context::windows().new_window(pa, ms, file_cfg, (session.layout_fn)());

            if is_active || hasnt_set_cur {
                context::set_cur(pa, node.try_downcast(), node.clone());
                if win != context::cur_window() {
                    context::set_cur_window(win);
                    U::switch_window(session.ms, win);
                }
                hasnt_set_cur = false;
            }

            for (file_cfg, is_active) in cfgs {
                session.open_file_from_cfg(pa, file_cfg, is_active, win);
            }
        }

        session
    }

    #[doc(hidden)]
    pub fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.file_cfg.set_print_cfg(cfg);
    }
}

pub struct Session<U: Ui> {
    ms: &'static U::MetaStatics,
    file_cfg: FileCfg,
    layout_fn: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
}

impl<U: Ui> Session<U> {
    /// Start the application, initiating a read/response loop.
    pub fn start(
        self,
        duat_rx: mpsc::Receiver<DuatEvent>,
    ) -> (
        Vec<Vec<FileRet>>,
        mpsc::Receiver<DuatEvent>,
        Option<Instant>,
    ) {
        form::set_sender(Sender::new(sender()));

        // SAFETY: No Passes exists at this point in time.
        let pa = unsafe { &mut Pass::new() };
        // SAFETY: Windows won't be accessible via &mut Pass by the end user,
        // so I should be able to use a shared Pass in order to allow easier
        // use here, whilst using the other &mut Pass for mutation operations.
        let wins_pa = unsafe { &Pass::new() };

        hook::trigger(pa, ConfigLoaded(()));

        let Some(mode_fn) = mode::take_set_mode_fn(pa) else {
            unreachable!("Somebody forgot to set a default mode, I'm looking at you `duat`!");
        };
        mode_fn(pa);

        {
            let win = context::cur_window();
            let window = context::windows::<U>().get(wins_pa, win).unwrap();
            for node in window.nodes() {
                node.update_and_print(pa);
            }
        }

        U::flush_layout(self.ms);

        let mut reload_instant = None;
        let mut reprint_screen = false;

        loop {
            if let Some(mode_fn) = mode::take_set_mode_fn(pa) {
                mode_fn(pa);
            }

            if let Ok(event) = duat_rx.recv_timeout(Duration::from_millis(50)) {
                match event {
                    DuatEvent::Tagger(key) => mode::send_key(pa, key),
                    DuatEvent::QueuedFunction(f) => f(pa),
                    DuatEvent::Resized | DuatEvent::FormChange => {
                        reprint_screen = true;
                        continue;
                    }
                    DuatEvent::OpenFile(name) => {
                        self.open_file_from_path(pa, context::cur_window(), PathBuf::from(&name));
                        mode::reset_to_file::<U>(name, false);
                    }
                    DuatEvent::CloseFile(name) => self.close_file(pa, name),
                    DuatEvent::SwapFiles(lhs, rhs) => self.swap_files(pa, lhs, rhs),
                    DuatEvent::OpenWindow(name) => self.open_window_with(pa, name),
                    DuatEvent::SwitchWindow(win) => {
                        reprint_screen = true;
                        context::set_cur_window(win);
                        U::switch_window(self.ms, win);
                    }
                    DuatEvent::FocusedOnDuat => {
                        hook::trigger(pa, FocusedOnDuat(()));
                    }
                    DuatEvent::UnfocusedFromDuat => {
                        hook::trigger(pa, UnfocusedFromDuat(()));
                    }
                    DuatEvent::ReloadStarted(instant) => reload_instant = Some(instant),
                    DuatEvent::ReloadConfig => {
                        hook::trigger(pa, ConfigUnloaded(()));
                        context::order_reload_or_quit();
                        wait_for_threads_to_despawn();

                        for handle in context::windows::<U>().file_handles(wins_pa) {
                            hook::trigger(pa, OnFileReload((handle, Cache::new())));
                        }

                        let ms = self.ms;
                        let files = self.take_files(pa);
                        U::unload(ms);
                        return (files, duat_rx, reload_instant);
                    }
                    DuatEvent::Quit => {
                        hook::trigger(pa, ConfigUnloaded(()));
                        hook::trigger(pa, ExitedDuat(()));
                        context::order_reload_or_quit();
                        wait_for_threads_to_despawn();

                        for handle in context::windows::<U>().file_handles(wins_pa) {
                            hook::trigger(pa, OnFileClose((handle, Cache::new())));
                        }

                        return (Vec::new(), duat_rx, None);
                    }
                }
            } else if reprint_screen {
                let win = context::cur_window();
                let window = context::windows::<U>().get(wins_pa, win).unwrap();
                for node in window.nodes() {
                    node.update_and_print(pa);
                }
                continue;
            }

            let win = context::cur_window();
            let window = context::windows::<U>().get(wins_pa, win).unwrap();
            for node in window.nodes() {
                if node.needs_update(pa) {
                    node.update_and_print(pa);
                }
            }
        }
    }

    fn take_files(self, pa: &mut Pass) -> Vec<Vec<FileRet>> {
        let files = context::windows::<U>().entries(pa).fold(
            Vec::new(),
            |mut file_handles, (win, _, node)| {
                if win >= file_handles.len() {
                    file_handles.push(Vec::new());
                }

                if let Some(handle) = node.try_downcast::<File<U>>() {
                    file_handles.last_mut().unwrap().push(handle)
                }

                file_handles
            },
        );

        files
            .into_iter()
            .map(|files| {
                let files = files.into_iter().map(|handle| {
                    let (file, area) = handle.write_with_area(pa);

                    let text = std::mem::take(file.text_mut());
                    let has_unsaved_changes = text.has_unsaved_changes();
                    let bytes = text.take_bytes();
                    let pk = file.path_kind();
                    let is_active = area.is_active();

                    FileRet::new(bytes, pk, is_active, has_unsaved_changes)
                });
                files.collect()
            })
            .collect()
    }

    fn open_file_from_cfg(
        &mut self,
        pa: &mut Pass,
        file_cfg: FileCfg,
        is_active: bool,
        win: usize,
    ) {
        match context::windows::<U>().new_file(pa, win, file_cfg) {
            Ok(node) => {
                if is_active {
                    context::set_cur(pa, node.try_downcast(), node.clone());
                    if context::cur_window() != win {
                        context::set_cur_window(win);
                        U::switch_window(self.ms, win);
                    }
                }
            }
            Err(err) => context::error!("{err}"),
        }
    }

    fn open_file_from_path(&self, pa: &mut Pass, win: usize, path: PathBuf) {
        if let Err(err) =
            context::windows::<U>().new_file(pa, win, self.file_cfg.clone().open_path(path))
        {
            context::error!("{err}")
        }
    }
}

// Loop functions
impl<U: Ui> Session<U> {
    fn close_file(&self, pa: &mut Pass, name: String) {
        context::windows::<U>().close_file(pa, &name, self.ms);
    }

    fn swap_files(&self, pa: &mut Pass, lhs_name: String, rhs_name: String) {
        context::windows::<U>().swap_files(pa, &lhs_name, &rhs_name, self.ms);
    }

    fn open_window_with(&self, pa: &mut Pass, name: String) {
        context::windows::<U>().open_or_move_to_new_window(
            pa,
            &name,
            self.ms,
            (self.layout_fn)(),
            self.file_cfg.clone(),
        );
    }
}

fn wait_for_threads_to_despawn() {
    loop {
        if let Some(count) = thread_count::thread_count() {
            if count.get() > 5 {
                std::thread::sleep(std::time::Duration::from_millis(10))
            } else {
                break;
            }
        } else {
            // Precautionary cushioning for when thread_amount is not supported.
            std::thread::sleep(std::time::Duration::from_secs(1));
            break;
        }
    }
}

pub struct FileRet {
    bytes: Bytes,
    path_kind: PathKind,
    is_active: bool,
    has_unsaved_changes: bool,
}

impl FileRet {
    fn new(bytes: Bytes, path_kind: PathKind, is_active: bool, has_unsaved_changes: bool) -> Self {
        Self {
            bytes,
            path_kind,
            is_active,
            has_unsaved_changes,
        }
    }
}
