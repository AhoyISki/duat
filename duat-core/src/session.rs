use std::{
    path::PathBuf,
    sync::{
        Mutex,
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    time::{Duration, Instant},
};

use crate::{
    cfg::PrintCfg,
    clipboard::Clipboard,
    cmd,
    context::{self, delete_cache, delete_cache_for, sender, store_cache},
    data::Pass,
    file::{File, FileCfg, PathKind},
    file_entry, form,
    hook::{self, ConfigLoaded, ConfigUnloaded, ExitedDuat, OnFileOpen, OnWindowOpen},
    mode,
    text::Bytes,
    ui::{
        DuatEvent, FileBuilder, MutArea, Node, RawArea, Sender, Ui, Widget, WidgetCfg, Window,
        WindowBuilder,
        layout::{FileId, Layout, MasterOnLeft},
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
        let mut pa = unsafe { Pass::new() };

        cmd::add_session_commands::<U>().unwrap();

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, _) = if let Some(name) = first {
            self.file_cfg.clone().open_path(name).build(&mut pa, None)
        } else {
            self.file_cfg.clone().build(&mut pa, None)
        };

        let (window, node) = Window::new(&mut pa, ms, widget, (self.layout_fn)());
        let cur_window = context::set_windows(vec![window]);

        let session = Session {
            ms,
            cur_window,
            file_cfg: self.file_cfg,
            layout_fn: self.layout_fn,
        };

        context::set_cur(&mut pa, node.as_file(), node.clone());

        // Open and process files.
        let builder = FileBuilder::new(&mut pa, node, context::cur_window());
        hook::trigger(&mut pa, OnFileOpen(builder));

        for file in args {
            session.open_file(&mut pa, PathBuf::from(file));
        }

        // Build the window's widgets.
        let builder = WindowBuilder::<U>::new(0);
        hook::trigger(&mut pa, OnWindowOpen(builder));

        session
    }

    pub fn session_from_prev(
        self,
        ms: &'static U::MetaStatics,
        prev: Vec<Vec<FileRet>>,
    ) -> Session<U> {
        cmd::add_session_commands::<U>().unwrap();
        // SAFETY: This function is only called from the main thread in
        // ../src/setup.rs, and from there, there are no other active
        // Passs, so this is fine.
        let mut pa = unsafe { Pass::new() };

        let cur_window = context::set_windows::<U>(Vec::new());

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
            cur_window,
            file_cfg: self.file_cfg,
            layout_fn: self.layout_fn,
        };

        let mut hasnt_set_cur = true;
        for (win, mut cfgs) in inherited_cfgs {
            let (file_cfg, is_active) = cfgs.next().unwrap();
            let (widget, _) = file_cfg.build(&mut pa, None);

            // SAFETY: Last Pass was just consumed.
            let mut pa = unsafe { Pass::new() };

            let (window, node) = Window::new(&mut pa, ms, widget, (session.layout_fn)());
            context::windows::<U>().borrow_mut().push(window);

            if is_active || hasnt_set_cur {
                context::set_cur(&mut pa, node.as_file(), node.clone());
                if win != context::cur_window() {
                    session.cur_window.store(win, Ordering::Relaxed);
                    U::switch_window(session.ms, win);
                }
                hasnt_set_cur = false;
            }

            let builder = FileBuilder::new(&mut pa, node, context::cur_window());
            hook::trigger(&mut pa, OnFileOpen(builder));

            for (file_cfg, is_active) in cfgs {
                session.open_file_from_cfg(&mut pa, file_cfg, is_active, win);
            }

            // Build the window's widgets.
            let builder = WindowBuilder::<U>::new(win);
            hook::trigger(&mut pa, OnWindowOpen(builder));
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
    fn open_file(&self, pa: &mut Pass, path: PathBuf) {
        let windows = context::windows::<U>();
        let pushed = {
            let mut windows = windows.borrow_mut();
            let cur_window = self.cur_window.load(Ordering::Relaxed);

            let (file, _) =
                <FileCfg as WidgetCfg<U>>::build(self.file_cfg.clone().open_path(path), pa, None);

            windows[cur_window].push_file(pa, file)
        };

        match pushed {
            Ok((node, _)) => {
                let builder = FileBuilder::new(pa, node, context::cur_window());
                hook::trigger(pa, OnFileOpen(builder));
            }
            Err(err) => context::error!("{err}"),
        }
    }

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
        let mut pa = unsafe { Pass::new() };

        hook::trigger(&mut pa, ConfigLoaded(()));

        let Some(mode_fn) = mode::take_set_mode_fn() else {
            unreachable!("Somebody forgot to set a default mode, I'm looking at you `duat`!");
        };
        mode_fn(&mut pa);

        {
            let win = self.cur_window.load(Ordering::Relaxed);
            let windows = context::windows::<U>().borrow();
            for node in windows[win].nodes() {
                node.update_and_print(&mut pa);
            }
        }

        U::flush_layout(self.ms);

        let mut reload_instant = None;
        let mut reprint_screen = false;

        loop {
            if let Ok(event) = duat_rx.recv_timeout(Duration::from_millis(50)) {
                match event {
                    DuatEvent::Tagger(key) => {
                        mode::send_key(&mut pa, key);
                    }
                    DuatEvent::QueuedFunction(f) => {
                        f(&mut pa);
                    }
                    DuatEvent::Resize | DuatEvent::FormChange => {
                        reprint_screen = true;
                        continue;
                    }
                    DuatEvent::OpenFile(name) => {
                        self.open_file(&mut pa, PathBuf::from(&name));
                        mode::reset_to_file::<U>(&pa, name, false);
                    }
                    DuatEvent::CloseFile(name) => self.close_file(&mut pa, name),
                    DuatEvent::SwapFiles(lhs, rhs) => self.swap_files(&mut pa, lhs, rhs),
                    DuatEvent::OpenWindow(name) => self.open_window_with(&mut pa, name),
                    DuatEvent::SwitchWindow(win) => {
                        self.cur_window.store(win, Ordering::SeqCst);
                        U::switch_window(self.ms, win);
                    }
                    DuatEvent::ReloadStarted(instant) => reload_instant = Some(instant),
                    DuatEvent::ReloadConfig => {
                        hook::trigger(&mut pa, ConfigUnloaded(()));
                        context::order_reload_or_quit();
                        wait_for_threads_to_despawn();

                        self.save_cache(&mut pa, false);
                        let ms = self.ms;
                        let files = self.take_files(&mut pa);
                        U::unload(ms);
                        return (files, duat_rx, reload_instant);
                    }
                    DuatEvent::Quit => {
                        hook::trigger(&mut pa, ConfigUnloaded(()));
                        hook::trigger(&mut pa, ExitedDuat(()));
                        context::order_reload_or_quit();
                        wait_for_threads_to_despawn();

                        self.save_cache(&mut pa, true);
                        return (Vec::new(), duat_rx, None);
                    }
                }
            } else if reprint_screen {
                let windows = context::windows::<U>().borrow();

                let win = self.cur_window.load(Ordering::SeqCst);
                reprint_screen = false;
                for node in windows[win].nodes() {
                    node.update_and_print(&mut pa);
                }

                continue;
            }

            let windows = context::windows::<U>().borrow();
            let win = self.cur_window.load(Ordering::SeqCst);
            for node in windows[win].nodes() {
                if node.needs_update(&pa) {
                    node.update_and_print(&mut pa);
                }
            }
        }
    }

    fn save_cache(&self, pa: &mut Pass, is_quitting_duat: bool) {
        let windows = context::windows::<U>().borrow_mut();
        for (handle, _) in windows
            .iter()
            .flat_map(Window::nodes)
            .filter_map(|node| node.as_file())
        {
            handle.write(pa, |file, area| {
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

                if let Some(main) = file.selections_mut().get_main() {
                    store_cache(path, main.clone());
                }
            });
        }
    }

    fn take_files(self, pa: &mut Pass) -> Vec<Vec<FileRet>> {
        context::windows::<U>()
            .borrow()
            .iter()
            .map(|w| {
                let files = w.nodes().filter_map(|node| {
                    node.try_downcast::<File<U>>().map(|file| {
                        file.write(pa, |file| {
                            let text = std::mem::take(file.text_mut());
                            let has_unsaved_changes = text.has_unsaved_changes();
                            let bytes = text.take_bytes();
                            let pk = file.path_kind();
                            let is_active = node.area().is_active();
                            FileRet::new(bytes, pk, is_active, has_unsaved_changes)
                        })
                    })
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
        let pushed = {
            let mut windows = context::windows::<U>().borrow_mut();
            let (widget, _) = file_cfg.build(pa, None);

            let result = windows[win].push_file(pa, widget);

            if let Ok((node, _)) = &result
                && is_active
            {
                context::set_cur(pa, node.as_file(), node.clone());
                if context::cur_window() != win {
                    self.cur_window.store(win, Ordering::Relaxed);
                    U::switch_window(self.ms, win);
                }
            }

            result
        };

        match pushed {
            Ok((node, _)) => {
                let builder = FileBuilder::new(pa, node, context::cur_window());
                hook::trigger(pa, OnFileOpen(builder));
            }
            Err(err) => context::error!("{err}"),
        }
    }
}

// Loop functions
impl<U: Ui> Session<U> {
    fn close_file(&self, pa: &mut Pass, name: String) {
        let mut windows = context::windows::<U>().borrow_mut();
        let (win, lhs, nodes) = {
            let (lhs_win, _, lhs) = file_entry(pa, &windows, &name).unwrap();
            let lhs = lhs.clone();

            let lo = lhs.read_as(pa, |f: &File<U>| f.layout_order).unwrap();

            let nodes: Vec<Node<U>> = windows[lhs_win]
                .nodes()
                .filter(|n| n.data_is::<File<U>>())
                .skip(lo + 1)
                .cloned()
                .collect();

            (lhs_win, lhs, nodes)
        };

        for rhs in nodes {
            swap(pa, &mut windows, [win, win], [&lhs, &rhs]);
        }

        windows[win].remove_file(pa, &name);
        if windows[win].file_names(pa).is_empty() {
            windows.remove(win);
            U::remove_window(self.ms, win);
            let cur_win = context::cur_window();
            if cur_win > win {
                self.cur_window.store(cur_win - 1, Ordering::Relaxed);
            }
        }
    }

    fn swap_files(&self, pa: &mut Pass, lhs_name: String, rhs_name: String) {
        let mut windows = context::windows::<U>().borrow_mut();
        let (wins, [lhs_node, rhs_node]) = {
            let (lhs_win, _, lhs_node) = file_entry(pa, &windows, &lhs_name).unwrap();
            let (rhs_win, _, rhs_node) = file_entry(pa, &windows, &rhs_name).unwrap();
            let lhs_node = lhs_node.clone();
            let rhs_node = rhs_node.clone();
            ([lhs_win, rhs_win], [lhs_node, rhs_node])
        };

        swap(pa, &mut windows, wins, [&lhs_node, &rhs_node]);
        drop(windows);

        let name = context::fixed_file::<U>(pa)
            .unwrap()
            .read(pa, |file, _| file.name());
        if wins[0] != wins[1]
            && let Some(win) = [lhs_name, rhs_name].into_iter().position(|n| n == name)
        {
            self.cur_window.store(win, Ordering::Relaxed);
            U::switch_window(self.ms, win);
        }
    }

    #[allow(clippy::await_holding_refcell_ref)]
    fn open_window_with(&self, pa: &mut Pass, name: String) {
        // Holding of the RefCell
        let mut windows = context::windows::<U>().borrow_mut();
        let new_win = windows.len();

        if let Ok((win, .., node)) = file_entry(pa, &windows, &name) {
            // Take the nodes in the original Window
            let node = node.clone();
            node.widget()
                .write_as(pa, |f: &mut File<U>| f.layout_order = 0);
            let nodes = windows[win].take_file_and_related_nodes(pa, &node);
            let layout = (self.layout_fn)();

            // Create a new Window Swapping the new root with files_area
            let new_root = U::new_root(self.ms, <U::Area as RawArea>::Cache::default());
            U::Area::swap(MutArea(node.area()), &new_root);
            let window = Window::<U>::from_raw(node.area().clone(), nodes, layout);
            windows.push(window);

            // Swap the Files ahead of the swapped new_root
            let lo = node.read_as(pa, |f: &File<U>| f.layout_order).unwrap();

            for (_, FileId(area)) in &windows[win].file_nodes(pa)[lo..] {
                MutArea(&new_root).swap(area);
            }
            // RefCell dropped here, before any .await
            drop(windows);

            // Delete the new_root, which should be the last "File" in the
            // list of the original Window.
            MutArea(&new_root).delete();
        } else {
            let (widget, _) = self
                .file_cfg
                .clone()
                .open_path(PathBuf::from(name.clone()))
                .build(pa, None);

            let (window, node) = Window::new(pa, self.ms, widget, (self.layout_fn)());
            windows.push(window);
            // RefCell dropped here, before any .await
            drop(windows);

            // Open and process files.
            let builder = FileBuilder::new(pa, node, new_win);
            hook::trigger(pa, OnFileOpen(builder));
        }

        let builder = WindowBuilder::<U>::new(new_win);
        hook::trigger(pa, OnWindowOpen(builder));

        if context::fixed_file::<U>(pa)
            .unwrap()
            .read(pa, |file, _| file.name())
            != name
        {
            mode::reset_to_file::<U>(pa, name, false);
        }

        self.cur_window.store(new_win, Ordering::Relaxed);
        U::switch_window(self.ms, new_win);
    }
}

fn swap<U: Ui>(
    pa: &mut Pass,
    windows: &mut [Window<U>],
    [lhs_w, rhs_w]: [usize; 2],
    [lhs, rhs]: [&Node<U>; 2],
) {
    let rhs_lo = rhs
        .widget()
        .read_as(pa, |f: &File<U>| f.layout_order)
        .unwrap();
    let lhs_lo = lhs
        .widget()
        .write_as(pa, |f: &mut File<U>| {
            std::mem::replace(&mut f.layout_order, rhs_lo)
        })
        .unwrap();

    rhs.widget()
        .write_as(pa, |f: &mut File<U>| f.layout_order = lhs_lo);

    let lhs_nodes = windows[lhs_w].take_file_and_related_nodes(pa, lhs);
    windows[rhs_w].insert_file_nodes(pa, rhs_lo, lhs_nodes);

    let rhs_nodes = windows[rhs_w].take_file_and_related_nodes(pa, rhs);
    windows[lhs_w].insert_file_nodes(pa, lhs_lo, rhs_nodes);

    MutArea(lhs.area()).swap(rhs.area());
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
