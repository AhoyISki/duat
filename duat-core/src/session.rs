use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    time::Duration,
};

use arboard::Clipboard;
use gapbuf::GapBuffer;
use parking_lot::Mutex;

use crate::{
    cache::{delete_cache, store_cache},
    cfg::PrintCfg,
    cmd, context, form,
    hooks::{self, ConfigLoaded, ConfigUnloaded, ExitedDuat, OnFileOpen, OnWindowOpen},
    mode,
    ui::{
        Area, DuatEvent, FileBuilder, Layout, MasterOnLeft, Sender, Ui, UiEvent, Window,
        WindowBuilder,
    },
    widgets::{File, FileCfg, Node, PathKind, WidgetCfg},
};

#[doc(hidden)]
pub struct SessionCfg<U: Ui> {
    file_cfg: FileCfg,
    layout: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
}

impl<U: Ui> SessionCfg<U> {
    pub fn new(clipb: &'static Mutex<Clipboard>) -> Self {
        crate::clipboard::set_clipboard(clipb);
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);

        SessionCfg {
            file_cfg: FileCfg::new(),
            layout: Box::new(|| Box::new(MasterOnLeft)),
        }
    }

    pub fn session_from_args(
        self,
        ms: &'static U::MetaStatics,
        tx: mpsc::Sender<DuatEvent>,
    ) -> Session<U> {
        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker, _) = if let Some(path) = first {
            <FileCfg as WidgetCfg<U>>::build(self.file_cfg.clone().open_path(path), false)
        } else {
            self.file_cfg.clone().build(false)
        };

        let (window, node) = Window::new(ms, widget, checker, (self.layout)());
        let cur_window = context::set_windows(vec![window]);

        let mut session = Session {
            ms,
            cur_window,
            file_cfg: self.file_cfg,
            tx,
        };

        context::set_cur(node.as_file(), node.clone());
        cmd::add_session_commands::<U>(session.tx.clone()).unwrap();

        // Open and process files.
        let builder = FileBuilder::new(node, context::cur_window());
        hooks::trigger_now::<OnFileOpen<U>>(builder);

        args.for_each(|file| session.open_file(PathBuf::from(file)));

        // Build the window's widgets.
        let builder = WindowBuilder::new(0);
        hooks::trigger_now::<OnWindowOpen<U>>(builder);

        session
    }

    pub fn session_from_prev(
        self,
        ms: &'static U::MetaStatics,
        prev: Vec<FileRet>,
        duat_tx: mpsc::Sender<DuatEvent>,
    ) -> Session<U> {
        let mut inherited_cfgs = Vec::new();
        for (buf, path_kind, is_active) in prev {
            let file_cfg = self.file_cfg.clone().take_from_prev(buf, path_kind);
            inherited_cfgs.push((file_cfg, is_active))
        }

        let Some((file_cfg, _)) = inherited_cfgs.pop() else {
            unreachable!("There should've been at least one file.")
        };

        let (widget, checker, _) = <FileCfg as WidgetCfg<U>>::build(file_cfg, false);

        let (window, node) = Window::new(ms, widget, checker, (self.layout)());
        let cur_window = context::set_windows(vec![window]);

        let mut session = Session {
            ms,
            cur_window,
            file_cfg: self.file_cfg,
            tx: duat_tx,
        };

        context::set_cur(node.as_file(), node.clone());
        cmd::add_session_commands::<U>(session.tx.clone()).unwrap();

        // Open and process files.
        let builder = FileBuilder::new(node, context::cur_window());
        hooks::trigger_now::<OnFileOpen<U>>(builder);

        for (file_cfg, is_active) in inherited_cfgs {
            session.open_file_from_cfg(file_cfg, is_active);
        }

        // Build the window's widgets.
        let builder = WindowBuilder::new(0);
        hooks::trigger_now::<OnWindowOpen<U>>(builder);

        session
    }

    #[doc(hidden)]
    pub fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.file_cfg.set_print_cfg(cfg);
    }
}

pub struct Session<U: Ui> {
    ms: &'static U::MetaStatics,
    cur_window: &'static AtomicUsize,
    file_cfg: FileCfg,
    tx: mpsc::Sender<DuatEvent>,
}

impl<U: Ui> Session<U> {
    pub fn open_file(&mut self, path: PathBuf) {
        let windows = context::windows::<U>();
        let pushed = windows.mutate(|windows| {
            let cur_window = self.cur_window.load(Ordering::Relaxed);
            let (file, checker, _) =
                <FileCfg as WidgetCfg<U>>::build(self.file_cfg.clone().open_path(path), false);
            windows[cur_window].push_file(file, checker)
        });

        match pushed {
            Ok((node, _)) => {
                let builder = FileBuilder::new(node, context::cur_window());
                hooks::trigger_now::<OnFileOpen<U>>(builder);
            }
            Err(err) => context::notify(err.into()),
        }
    }

    /// Start the application, initiating a read/response loop.
    pub fn start(
        mut self,
        duat_rx: mpsc::Receiver<DuatEvent>,
        ui_tx: mpsc::Sender<UiEvent>,
    ) -> (Vec<FileRet>, mpsc::Receiver<DuatEvent>) {
        hooks::trigger::<ConfigLoaded>(());
        form::set_sender(Sender::new(self.tx.clone()));

        // This loop is very useful when trying to find deadlocks.
        #[cfg(feature = "deadlocks")]
        crate::thread::spawn(|| {
            use std::io::Write;

            let mut file = std::io::BufWriter::new(
                std::fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open("deadlocks")
                    .unwrap(),
            );

            crate::log_file!("deadlocks are active!");

            loop {
                std::thread::sleep(std::time::Duration::new(2, 0));
                let deadlocks = parking_lot::deadlock::check_deadlock();
                crate::log_file!("{} deadlocks detected", deadlocks.len());
                for (i, threads) in deadlocks.iter().enumerate() {
                    crate::log_file!("Deadlock #{}", i);
                    for t in threads {
                        crate::log_file!("Thread Id {:#?}", t.thread_id());
                        crate::log_file!("{:#?}", t.backtrace());
                    }
                }
                if context::will_reload_or_quit() {
                    break;
                }
            }
        });

        ui_tx.send(UiEvent::Start).unwrap();

        // The main loop.
        loop {
            U::flush_layout(self.ms);

            let cur_window = self.cur_window.load(Ordering::Relaxed);
            context::windows::<U>().inspect(|windows| {
                for node in windows[cur_window].nodes() {
                    node.update_and_print();
                }
            });

            let reason_to_break = self.session_loop(&duat_rx);

            match reason_to_break {
                BreakTo::QuitDuat => {
                    hooks::trigger::<ConfigUnloaded>(());
                    hooks::trigger::<ExitedDuat>(());
                    crate::thread::quit_queue();
                    self.save_cache(true);
                    context::order_reload_or_quit();
                    ui_tx.send(UiEvent::Quit).unwrap();
                    break (Vec::new(), duat_rx);
                }
                BreakTo::ReloadConfig => {
                    hooks::trigger::<ConfigUnloaded>(());
                    U::unload(self.ms);
                    crate::thread::quit_queue();
                    self.save_cache(false);
                    let files = self.take_files();
                    context::order_reload_or_quit();
                    ui_tx.send(UiEvent::Reload).unwrap();
                    break (files, duat_rx);
                }
                BreakTo::OpenFile(file) => self.open_file(file),
                BreakTo::CloseWidgets(path_kind) => {
                    context::windows::<U>().mutate(|windows| {
                        for window in windows.iter_mut() {
                            window.remove_file(path_kind.clone());
                        }
                    });
                }
            }
        }
    }

    /// The primary application loop, executed while no breaking
    /// functions have been called
    fn session_loop(&mut self, duat_rx: &mpsc::Receiver<DuatEvent>) -> BreakTo {
        let w = self.cur_window;
        let windows = context::windows::<U>().read();

        std::thread::scope(|s| {
            loop {
                let cur_window = &windows[w.load(Ordering::Relaxed)];

                if let Some(set_mode) = mode::was_set() {
                    set_mode();
                }

                if let Ok(event) = duat_rx.recv_timeout(Duration::from_millis(500)) {
                    match event {
                        DuatEvent::Key(key) => mode::send_key(key),
                        DuatEvent::Resize | DuatEvent::FormChange => {
                            for node in cur_window.nodes() {
                                s.spawn(||
                                node.update_and_print());
                            }
                            continue;
                        }
                        DuatEvent::MetaMsg(msg) => context::notify(msg),
                        DuatEvent::ReloadConfig => break BreakTo::ReloadConfig,
                        DuatEvent::OpenFile(file) => break BreakTo::OpenFile(file),
                        DuatEvent::CloseFile(path_kind) => {
                            break BreakTo::CloseWidgets(path_kind);
                        }
                        DuatEvent::Quit => break BreakTo::QuitDuat,
                    }
                }

                for node in cur_window.nodes() {
                    if node.needs_update() {
                        s.spawn(|| node.update_and_print());
                    }
                }
            }
        })
    }

    fn save_cache(&self, is_quitting_duat: bool) {
        let windows = context::windows::<U>().read();
        for (file, area, _) in windows
            .iter()
            .flat_map(Window::nodes)
            .filter_map(|node| node.as_file())
        {
            let mut file = file.write();
            let path = file.path();

            if is_quitting_duat && (!file.exists() || file.text().has_unsaved_changes()) {
                delete_cache(&path);
                return;
            }
            if let Some(cache) = area.cache() {
                store_cache(&path, cache);
            }

            let cursors = file.cursors_mut().unwrap();
            if !cursors.is_empty() {
                if is_quitting_duat {
                    cursors.remove_extras();
                }
                store_cache(path, std::mem::take(&mut *cursors));
            }
        }
    }

    fn take_files(self) -> Vec<FileRet> {
        let windows = context::windows::<U>().read();

        windows
            .iter()
            .flat_map(Window::nodes)
            .filter_map(|node| {
                node.try_downcast::<File>().map(|file| {
                    let mut file = file.write();
                    let text = std::mem::take(file.text_mut());
                    let buf = text.take_buf();
                    let kind = file.path_kind();
                    (buf, kind, node.area().is_active())
                })
            })
            .collect()
    }

    fn open_file_from_cfg(&mut self, file_cfg: FileCfg, is_active: bool) {
        let pushed = context::windows().mutate(|windows| {
            let cur_window = self.cur_window.load(Ordering::Relaxed);

            let (widget, checker, _) = <FileCfg as WidgetCfg<U>>::build(file_cfg, false);

            let result = windows[cur_window].push_file(widget, checker);

            if let Ok((node, _)) = &result
                && is_active
            {
                context::set_cur(node.as_file(), node.clone());
            }

            result
        });

        match pushed {
            Ok((node, _)) => {
                let builder = FileBuilder::new(node, context::cur_window());
                hooks::trigger_now::<OnFileOpen<U>>(builder);
            }
            Err(err) => context::notify(err.into()),
        }
    }
}

enum BreakTo {
    ReloadConfig,
    OpenFile(PathBuf),
    CloseWidgets(PathKind),
    QuitDuat,
}

pub type FileRet = (GapBuffer<u8>, PathKind, bool);
