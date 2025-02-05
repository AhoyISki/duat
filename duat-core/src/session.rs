use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    time::Duration,
};

use crate::{
    cache::{delete_cache, store_cache},
    cfg::PrintCfg,
    cmd, context,
    data::RwData,
    hooks::{self, OnFileOpen, OnWindowOpen, SessionStarted},
    mode,
    ui::{Area, Event, FileBuilder, Layout, MasterOnLeft, Sender, Ui, Window, WindowBuilder},
    widgets::{File, FileCfg, Node, Widget, WidgetCfg},
};

#[doc(hidden)]
pub struct SessionCfg<U: Ui> {
    ui: U,
    file_cfg: FileCfg,
    layout: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
}

impl<U: Ui> SessionCfg<U> {
    pub fn new(ui: U) -> Self {
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);

        SessionCfg {
            ui,
            file_cfg: FileCfg::new(),
            layout: Box::new(|| Box::new(MasterOnLeft)),
        }
    }

    pub fn session_from_args(mut self, tx: mpsc::Sender<Event>) -> Session<U> {
        self.ui.open();

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker, _) = if let Some(path) = first {
            <FileCfg as WidgetCfg<U>>::build(self.file_cfg.clone().open_path(path), false)
        } else {
            self.file_cfg.clone().build(false)
        };

        let (window, node) = Window::new(&mut self.ui, widget, checker, (self.layout)());
        let cur_window = context::set_windows(vec![window]);

        let mut session = Session {
            ui: self.ui,
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
        mut self,
        prev: Vec<(RwData<File>, bool)>,
        tx: mpsc::Sender<Event>,
    ) -> Session<U> {
        let mut inherited_cfgs = Vec::new();
        for (file, is_active) in prev {
            let mut file = file.write();
            let file_cfg = self.file_cfg.clone().take_from_prev(&mut file);
            inherited_cfgs.push((file_cfg, is_active))
        }

        let Some((file_cfg, _)) = inherited_cfgs.pop() else {
            unreachable!("There should've been at least one file.")
        };

        let (widget, checker, _) = <FileCfg as WidgetCfg<U>>::build(file_cfg, false);

        let (window, node) = Window::new(&mut self.ui, widget, checker, (self.layout)());
        let cur_window = context::set_windows(vec![window]);

        let mut session = Session {
            ui: self.ui,
            cur_window,
            file_cfg: self.file_cfg,
            tx,
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
    ui: U,
    cur_window: &'static AtomicUsize,
    file_cfg: FileCfg,
    tx: mpsc::Sender<Event>,
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
    pub fn start(mut self, rx: mpsc::Receiver<Event>) -> Vec<(RwData<File>, bool)> {
        hooks::trigger::<SessionStarted<U>>(());

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

            loop {
                std::thread::sleep(std::time::Duration::new(2, 0));
                let deadlocks = parking_lot::deadlock::check_deadlock();
                writeln!(file, "{} deadlocks detected", deadlocks.len()).unwrap();
                for (i, threads) in deadlocks.iter().enumerate() {
                    writeln!(file, "Deadlock #{}", i).unwrap();
                    for t in threads {
                        writeln!(file, "Thread Id {:#?}", t.thread_id()).unwrap();
                        writeln!(file, "{:#?}", t.backtrace()).unwrap();
                    }
                }
            }
        });

        self.ui.flush_layout();
        self.ui.start(Sender::new(self.tx.clone()));
        crate::form::set_sender(Sender::new(self.tx.clone()));

        // The main loop.
        loop {
            let cur_window = self.cur_window.load(Ordering::Relaxed);
            context::windows::<U>().inspect(|windows| {
                while windows
                    .iter()
                    .flat_map(Window::nodes)
                    .any(Node::needs_update)
                {
                    for node in windows.iter().flat_map(Window::nodes) {
                        node.update();
                    }
                }

                for node in windows[cur_window].nodes() {
                    node.update_and_print();
                }
            });

            let reason_to_break = self.session_loop(&rx);

            match reason_to_break {
                BreakTo::QuitDuat => {
                    crate::thread::quit_queue();
                    cmd::end_session();
                    self.save_cache(true);
                    self.ui.close();

                    break Vec::new();
                }
                BreakTo::ReloadConfig => {
                    crate::thread::quit_queue();
                    cmd::end_session();
                    self.save_cache(false);
                    self.ui.end();

                    break self.reload_config();
                }
                BreakTo::OpenFile(file) => self.open_file(file),
            }
        }
    }

    /// The primary application loop, executed while no breaking
    /// functions have been called
    fn session_loop(&mut self, rx: &mpsc::Receiver<Event>) -> BreakTo {
        let w = self.cur_window;
        let windows = context::windows::<U>().read();

        std::thread::scope(|s| {
            loop {
                let cur_window = &windows[w.load(Ordering::Relaxed)];

                if let Some(set_mode) = mode::was_set() {
                    set_mode();
                }

                if let Ok(event) = rx.recv_timeout(Duration::from_millis(50)) {
                    match event {
                        Event::Key(key) => mode::send_key(key),
                        Event::Resize | Event::FormChange => {
                            for node in cur_window.nodes() {
                                s.spawn(|| node.update_and_print());
                            }
                            crate::REPRINTING_SCREEN.store(false, Ordering::Release);
                            continue;
                        }
                        Event::ReloadConfig => break BreakTo::ReloadConfig,
                        Event::Quit => break BreakTo::QuitDuat,
                        Event::OpenFile(file) => break BreakTo::OpenFile(file),
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
        for (file, area, cursors, _) in windows
            .iter()
            .flat_map(Window::nodes)
            .filter_map(|node| node.as_file())
        {
            let file = file.read();

            if is_quitting_duat && !file.exists() {
                delete_cache(file.path());
                return;
            }
            if let Some(cache) = area.cache() {
                store_cache(file.path(), cache);
            }

            let mut cursors = cursors.write();
            if !cursors.is_empty() {
                if is_quitting_duat {
                    cursors.remove_extras();
                }
                store_cache(file.path(), std::mem::take(&mut *cursors));
            }
        }
    }

    fn reload_config(self) -> Vec<(RwData<File>, bool)> {
        let windows = context::windows::<U>().read();

        windows
            .iter()
            .flat_map(Window::nodes)
            .filter_map(|node| {
                node.try_downcast::<File>().map(|file| {
                    Widget::<U>::text_mut(&mut *file.write()).clear_tags();
                    (file, node.area().is_active())
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
    QuitDuat,
}

unsafe impl<U: Ui> Send for SessionCfg<U> {}
unsafe impl<U: Ui> Sync for SessionCfg<U> {}

unsafe impl<U: Ui> Send for Session<U> {}
unsafe impl<U: Ui> Sync for Session<U> {}
