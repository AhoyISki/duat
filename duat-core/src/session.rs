use std::{
    path::PathBuf,
    sync::{
        OnceLock,
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    time::Duration,
};

use arboard::Clipboard;
use gapbuf::GapBuffer;
use parking_lot::Mutex;

use crate::{
    cache::{delete_cache, delete_cache_for, store_cache},
    cfg::PrintCfg,
    cmd, context, file_entry, form,
    hooks::{self, ConfigLoaded, ConfigUnloaded, ExitedDuat, OnFileOpen, OnWindowOpen},
    mode,
    ui::{
        Area, DuatEvent, DuatPermission, FileBuilder, Layout, MasterOnLeft, Sender, Ui, UiEvent,
        Window, WindowBuilder,
    },
    widgets::{File, FileCfg, Node, PathKind, WidgetCfg},
};

static DUAT_SENDER: OnceLock<&mpsc::Sender<DuatEvent>> = OnceLock::new();

pub(crate) fn sender() -> &'static mpsc::Sender<DuatEvent> {
    DUAT_SENDER.get().unwrap()
}

#[doc(hidden)]
pub struct SessionCfg<U: Ui> {
    file_cfg: FileCfg,
    layout_fn: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
}

impl<U: Ui> SessionCfg<U> {
    pub fn new(clipb: &'static Mutex<Clipboard>) -> Self {
        crate::clipboard::set_clipboard(clipb);
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);

        SessionCfg {
            file_cfg: FileCfg::new(),
            layout_fn: Box::new(|| Box::new(MasterOnLeft)),
        }
    }

    pub fn session_from_args(
        self,
        ms: &'static U::MetaStatics,
        duat_tx: &'static mpsc::Sender<DuatEvent>,
    ) -> Session<U> {
        DUAT_SENDER.set(duat_tx).unwrap();

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker, _) = if let Some(name) = first {
            <FileCfg as WidgetCfg<U>>::build(self.file_cfg.clone().open_path(name), false)
        } else {
            self.file_cfg.clone().build(false)
        };

        let (window, node) = Window::new(ms, widget, checker, (self.layout_fn)());
        let cur_window = context::set_windows(vec![window]);

        let mut session = Session {
            ms,
            cur_window,
            file_cfg: self.file_cfg,
            layout_fn: self.layout_fn,
        };

        context::set_cur(node.as_file(), node.clone());
        cmd::add_session_commands::<U>().unwrap();

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
        prev: Vec<Vec<FileRet>>,
        duat_tx: &'static mpsc::Sender<DuatEvent>,
    ) -> Session<U> {
        DUAT_SENDER.set(duat_tx).unwrap();
        let cur_window = context::set_windows::<U>(Vec::new());

        cmd::add_session_commands::<U>().unwrap();

        let file_cfg = self.file_cfg.clone();
        let inherited_cfgs = prev.into_iter().enumerate().map(|(i, cfgs)| {
            let cfgs = cfgs.into_iter().map(|file_ret| {
                let buf = file_ret.buf;
                let pk = file_ret.path_kind;
                let unsaved = file_ret.has_unsaved_changes;
                let file_cfg = file_cfg.clone().take_from_prev(buf, pk, unsaved);
                (file_cfg, file_ret.is_active)
            });
            (i, cfgs)
        });

        let mut session = Session {
            ms,
            cur_window,
            file_cfg: self.file_cfg,
            layout_fn: self.layout_fn,
        };

        for (win, mut cfgs) in inherited_cfgs {
            let (file_cfg, is_active) = cfgs.next().unwrap();
            let (widget, checker, _) = <FileCfg as WidgetCfg<U>>::build(file_cfg, false);

            let (window, node) = Window::new(ms, widget, checker, (session.layout_fn)());
            context::windows::<U>().write().push(window);

            if is_active {
                context::set_cur(node.as_file(), node.clone());
                if win != context::cur_window() {
                    session.cur_window.store(win, Ordering::Relaxed);
                    U::switch_window(session.ms, win);
                }
            }

            let builder = FileBuilder::new(node, context::cur_window());
            hooks::trigger_now::<OnFileOpen<U>>(builder);

            for (file_cfg, is_active) in cfgs {
                session.open_file_from_cfg(file_cfg, is_active, win);
            }

            // Build the window's widgets.
            let builder = WindowBuilder::new(win);
            hooks::trigger_now::<OnWindowOpen<U>>(builder);
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
    cur_window: &'static AtomicUsize,
    file_cfg: FileCfg,
    layout_fn: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
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
    ) -> (Vec<Vec<FileRet>>, mpsc::Receiver<DuatEvent>) {
        hooks::trigger::<ConfigLoaded>(());
        form::set_sender(Sender::new(sender()));

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

        // The main loop.
        loop {
            U::flush_layout(self.ms);

            let win = self.cur_window.load(Ordering::Relaxed);
            context::windows::<U>().inspect(|windows| {
                for node in windows[win].nodes() {
                    node.update_and_print();
                }
            });

            ui_tx.send(UiEvent::ResumePrinting).unwrap();

            let break_to = self.session_loop(&duat_rx);

            ui_tx.send(UiEvent::PausePrinting).unwrap();

            for break_to in break_to {
                match break_to {
                    BreakTo::QuitDuat => {
                        hooks::trigger::<ConfigUnloaded>(());
                        hooks::trigger::<ExitedDuat>(());
                        crate::thread::quit_queue();
                        context::order_reload_or_quit();
                        self.save_cache(true);
                        ui_tx.send(UiEvent::Quit).unwrap();
                        return (Vec::new(), duat_rx);
                    }
                    BreakTo::ReloadConfig => {
                        hooks::trigger::<ConfigUnloaded>(());
                        crate::thread::quit_queue();
                        context::order_reload_or_quit();
                        self.save_cache(false);
                        let ms = self.ms;
                        let files = self.take_files();
                        U::unload(ms);
                        return (files, duat_rx);
                    }
                    BreakTo::OpenFile(name) => {
                        self.open_file(PathBuf::from(&name));
                        mode::reset_switch_to::<U>(name, false);
                    }
                    BreakTo::CloseFile(name) => self.close_file(name),
                    BreakTo::SwapFiles(lhs_name, rhs_name) => self.swap_files(lhs_name, rhs_name),
                    BreakTo::OpenWindow(name) => self.open_window_with(name),
                    BreakTo::SwitchWindow(win) => {
                        self.cur_window.store(win, Ordering::Relaxed);
                        U::switch_window(self.ms, win);
                    }
                }
            }
        }
    }

    /// The primary application loop, executed while no breaking
    /// functions have been called
    fn session_loop(&mut self, duat_rx: &mpsc::Receiver<DuatEvent>) -> Vec<BreakTo> {
        let win = self.cur_window.load(Ordering::Relaxed);
        let windows = context::windows::<U>().read();
        let mut reasons = Vec::new();

        std::thread::scope(|s| {
            loop {
                let cur_window = &windows[win];

                if let Some(set_mode) = mode::was_set() {
                    set_mode();
                }

                if let Ok(event) = duat_rx.recv_timeout(Duration::from_millis(15)) {
                    match event {
                        DuatEvent::Key(key) => mode::send_key(key),
                        DuatEvent::Resize | DuatEvent::FormChange => {
                            for node in cur_window.nodes() {
                                s.spawn(|| node.update_and_print());
                            }
                            continue;
                        }
                        DuatEvent::MetaMsg(msg) => context::notify(msg),
                        DuatEvent::ReloadConfig => reasons.push(BreakTo::ReloadConfig),
                        DuatEvent::OpenFile(name) => reasons.push(BreakTo::OpenFile(name)),
                        DuatEvent::CloseFile(name) => reasons.push(BreakTo::CloseFile(name)),
                        DuatEvent::SwapFiles(lhs, rhs) => {
                            reasons.push(BreakTo::SwapFiles(lhs, rhs))
                        }
                        DuatEvent::OpenWindow(name) => reasons.push(BreakTo::OpenWindow(name)),
                        DuatEvent::SwitchWindow(win) => reasons.push(BreakTo::SwitchWindow(win)),
                        DuatEvent::Quit => reasons.push(BreakTo::QuitDuat),
                    }
                } else if !reasons.is_empty() {
                    break reasons;
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
            file.text_mut().new_moment();

            if is_quitting_duat {
                delete_cache_for::<crate::text::History>(&path);
                if !file.exists() || file.text().has_unsaved_changes() {
                    delete_cache(&path);
                    return;
                }
            } else if let Some(history) = file.text().history() {
                store_cache(&path, history.clone());
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

    fn take_files(self) -> Vec<Vec<FileRet>> {
        context::windows::<U>()
            .read()
            .iter()
            .map(|w| {
                let files = w.nodes().filter_map(|node| {
                    node.try_downcast::<File>().map(|file| {
                        let mut file = file.write();
                        let text = std::mem::take(file.text_mut());
                        let has_unsaved_changes = text.has_unsaved_changes();
                        let buf = text.take_buf();
                        let pk = file.path_kind();
                        let is_active = node.area().is_active();
                        crate::log_file!("{has_unsaved_changes}");
                        FileRet::new(buf, pk, is_active, has_unsaved_changes)
                    })
                });
                files.collect()
            })
            .collect()
    }

    fn open_file_from_cfg(&mut self, file_cfg: FileCfg, is_active: bool, win: usize) {
        let pushed = context::windows().mutate(|windows| {
            let (widget, checker, _) = <FileCfg as WidgetCfg<U>>::build(file_cfg, false);

            let result = windows[win].push_file(widget, checker);

            if let Ok((node, _)) = &result
                && is_active
            {
                context::set_cur(node.as_file(), node.clone());
                if context::cur_window() != win {
                    self.cur_window.store(win, Ordering::Relaxed);
                    U::switch_window(self.ms, win);
                }
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

// BreakTo functions.
impl<U: Ui> Session<U> {
    fn close_file(&self, name: String) {
        let (win, lhs, nodes) = context::windows::<U>().inspect(|windows| {
            let (lhs_win, _, lhs) = file_entry(windows, &name).unwrap();
            let lhs = lhs.clone();

            let ordering = lhs.inspect_as(|f: &File| f.layout_ordering).unwrap();

            let nodes: Vec<Node<U>> = windows[lhs_win].file_nodes()[(ordering + 1)..]
                .iter()
                .map(|(n, _)| (*n).clone())
                .collect();

            (lhs_win, lhs, nodes)
        });

        for rhs in nodes {
            swap([win, win], [&lhs, &rhs]);
        }

        let mut windows = context::windows::<U>().write();
        windows[win].remove_file(&name);
        if windows[win].file_names().is_empty() {
            windows.remove(win);
            U::remove_window(self.ms, win);
            let cur_win = context::cur_window();
            if cur_win > win {
                self.cur_window.store(cur_win - 1, Ordering::Relaxed);
            }
        }
    }

    fn swap_files(&self, lhs_name: String, rhs_name: String) {
        let (wins, [lhs_node, rhs_node]) = context::windows::<U>().inspect(|windows| {
            let (lhs_win, _, lhs_node) = file_entry(windows, &lhs_name).unwrap();
            let (rhs_win, _, rhs_node) = file_entry(windows, &rhs_name).unwrap();
            let lhs_node = lhs_node.clone();
            let rhs_node = rhs_node.clone();
            ([lhs_win, rhs_win], [lhs_node, rhs_node])
        });

        swap(wins, [&lhs_node, &rhs_node]);

        let name = context::cur_file::<U>().unwrap().name();
        if wins[0] != wins[1] {
            if name == lhs_name {
                self.cur_window.store(wins[1], Ordering::Relaxed);
                U::switch_window(self.ms, wins[1]);
            } else if name == rhs_name {
                self.cur_window.store(wins[0], Ordering::Relaxed);
                U::switch_window(self.ms, wins[0]);
            }
        }
    }

    fn open_window_with(&self, name: String) {
        let mut windows = context::windows::<U>().write();
        let new_win = windows.len();

        if let Ok((win, .., node)) = file_entry(&windows, &name) {
            // Take the nodes in the original Window
            let node = node.clone();
            node.widget()
                .mutate_as(|f: &mut File| f.layout_ordering = 0);
            let nodes = windows[win].take_file_and_related_nodes(&node);
            let layout = (self.layout_fn)();

            // Create a new Window Swapping the new root with files_area
            let new_root = U::new_root(self.ms, <U::Area as Area>::Cache::default());
            node.area().swap(&new_root, DuatPermission::new());
            let window = Window::<U>::from_raw(node.area().clone(), nodes, layout);
            windows.push(window);

            // Swap the Files ahead of the swapped new_root
            let ordering = node.inspect_as(|f: &File| f.layout_ordering).unwrap();

            for (node, _) in &windows[win].file_nodes()[ordering..] {
                new_root.swap(node.area(), DuatPermission::new());
            }
            drop(windows);

            // Delete the new_root, which should be the last "File" in the
            // list of the original Window.
            new_root.delete(DuatPermission::new());
        } else {
            let (widget, checker, _) = <FileCfg as WidgetCfg<U>>::build(
                self.file_cfg.clone().open_path(PathBuf::from(name.clone())),
                false,
            );

            let (window, node) = Window::new(self.ms, widget, checker, (self.layout_fn)());
            windows.push(window);
            drop(windows);

            // Open and process files.
            let builder = FileBuilder::new(node, new_win);
            hooks::trigger_now::<OnFileOpen<U>>(builder);
        }

        let builder = WindowBuilder::new(new_win);
        hooks::trigger_now::<OnWindowOpen<U>>(builder);

        if context::cur_file::<U>().unwrap().name() != name {
            mode::reset_switch_to::<U>(name, false);
        }

        self.cur_window.store(new_win, Ordering::Relaxed);
        U::switch_window(self.ms, new_win);
    }
}

fn swap<U: Ui>([lhs_w, rhs_w]: [usize; 2], [lhs, rhs]: [&Node<U>; 2]) {
    let rhs_ordering = rhs
        .widget()
        .inspect_as::<File, usize>(|f| f.layout_ordering)
        .unwrap();
    let lhs_ordering = lhs
        .widget()
        .mutate_as::<File, usize>(|f| std::mem::replace(&mut f.layout_ordering, rhs_ordering))
        .unwrap();
    rhs.widget()
        .mutate_as::<File, ()>(|f| f.layout_ordering = lhs_ordering);

    let mut windows = context::windows::<U>().write();

    let lhs_nodes = windows[lhs_w].take_file_and_related_nodes(lhs);
    windows[rhs_w].insert_file_nodes(rhs_ordering, lhs_nodes);

    let rhs_nodes = windows[rhs_w].take_file_and_related_nodes(rhs);
    windows[lhs_w].insert_file_nodes(lhs_ordering, rhs_nodes);

    lhs.area().swap(rhs.area(), DuatPermission::new());
}

enum BreakTo {
    ReloadConfig,
    OpenFile(String),
    CloseFile(String),
    SwapFiles(String, String),
    OpenWindow(String),
    SwitchWindow(usize),
    QuitDuat,
}

pub struct FileRet {
    buf: GapBuffer<u8>,
    path_kind: PathKind,
    is_active: bool,
    has_unsaved_changes: bool,
}

impl FileRet {
    fn new(
        buf: GapBuffer<u8>,
        path_kind: PathKind,
        is_active: bool,
        has_unsaved_changes: bool,
    ) -> Self {
        Self {
            buf,
            path_kind,
            is_active,
            has_unsaved_changes,
        }
    }
}
