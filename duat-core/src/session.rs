use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        mpsc,
    },
    time::Duration,
};

use crate::{
    cache::{delete_cache, load_cache, store_cache},
    data::{Context, RwData},
    hooks::{self, OnWindowOpen},
    input::InputForFiles,
    text::{err, ok, text, PrintCfg, Text},
    ui::{
        build_file, Area, Event, Layout, MasterOnLeft, Node, PushSpecs, Sender, Ui, Window,
        WindowBuilder,
    },
    widgets::{ActiveWidget, File, FileCfg, Widget},
    Plugin,
};

#[doc(hidden)]
pub struct SessionCfg<U>
where
    U: Ui,
{
    ui: RwData<U>,
    file_cfg: FileCfg<U>,
    context: Context<U>,
    layout: Box<dyn Fn() -> Box<dyn Layout<U> + 'static>>,
    plugins: Vec<Box<dyn Plugin<U>>>,
}

impl<U> SessionCfg<U>
where
    U: Ui,
{
    pub fn new(ui: RwData<U>, context: Context<U>) -> Self {
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);

        SessionCfg {
            ui,
            file_cfg: FileCfg::new(),
            context,
            layout: Box::new(|| Box::new(MasterOnLeft)),
            plugins: Vec::new(),
        }
    }

    pub fn session_from_args(self, tx: mpsc::Sender<Event>) -> Session<U> {
        let mut ui = self.ui.write();
        ui.open();

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker) = if let Some(path) = first {
            self.file_cfg.clone().open_path(path).build()
        } else {
            self.file_cfg.clone().build()
        };

        let (window, area) = Window::new(&mut *ui, widget.clone(), checker, (self.layout)());
        let (windows, cur_window) = self.context.set_windows(vec![window]);

        let mut session = Session {
            ui: self.ui.clone(),
            windows,
            cur_window,
            file_cfg: self.file_cfg,
            context: self.context,
            tx,
        };

        session.set_active_file(widget, &area);

        add_session_commands(&session, self.context, session.tx.clone());

        // Open and process files.
        build_file(session.windows, area, self.context);
        args.for_each(|file| session.open_file(PathBuf::from(file)));

        // Build the window's widgets.
        let builder = WindowBuilder::new(session.windows, 0, self.context);
        hooks::trigger::<OnWindowOpen<U>>(builder);

        session
    }

    pub fn session_from_prev(
        self,
        prev: Vec<(RwData<File>, bool)>,
        tx: mpsc::Sender<Event>,
    ) -> Session<U> {
        let mut ui = self.ui.write();

        let mut inherited_cfgs = Vec::new();
        for (file, is_active) in prev {
            let mut file = file.write();
            let file_cfg = self.file_cfg.clone().take_from_prev(&mut file);
            inherited_cfgs.push((file_cfg, is_active))
        }

        let Some((file_cfg, _)) = inherited_cfgs.pop() else {
            unreachable!("There should've been at least one file.")
        };

        let (widget, checker) = file_cfg.build();

        let (window, area) = Window::new(&mut *ui, widget.clone(), checker, (self.layout)());
        let (windows, cur_window) = self.context.set_windows(vec![window]);

        let mut session = Session {
            ui: self.ui.clone(),
            windows,
            cur_window,
            file_cfg: self.file_cfg,
            context: self.context,
            tx,
        };

        session.set_active_file(widget, &area);

        add_session_commands(&session, self.context, session.tx.clone());

        // Open and process files..
        build_file(session.windows, area, self.context);

        for (file_cfg, is_active) in inherited_cfgs {
            session.open_file_from_cfg(file_cfg, is_active);
        }

        // Build the window's widgets.
        let builder = WindowBuilder::new(session.windows, 0, self.context);
        hooks::trigger::<OnWindowOpen<U>>(builder);

        session
    }

    #[doc(hidden)]
    pub fn set_input(&mut self, input: impl InputForFiles<U> + Clone) {
        self.file_cfg.set_input(input);
    }

    #[doc(hidden)]
    pub fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.file_cfg.set_print_cfg(cfg);
    }

    #[doc(hidden)]
    pub fn load_plugin<P>(&mut self, context: Context<U>)
    where
        P: Plugin<U>,
    {
        let cache = load_cache::<P::Cache>("").unwrap_or_default();
        let plugin = P::new(cache, context);
        self.plugins.push(Box::new(plugin));
    }

    pub fn load_plugin_then<P>(&mut self, context: Context<U>, f: impl FnOnce(&mut P))
    where
        P: Plugin<U>,
    {
        let cache = load_cache::<P::Cache>("").unwrap_or_default();
        let mut plugin = P::new(cache, context);
        f(&mut plugin);
        self.plugins.push(Box::new(plugin));
    }
}

pub struct Session<U>
where
    U: Ui,
{
    ui: RwData<U>,
    windows: &'static RwData<Vec<Window<U>>>,
    cur_window: &'static AtomicUsize,
    file_cfg: FileCfg<U>,
    context: Context<U>,
    tx: mpsc::Sender<Event>,
}

impl<U> Session<U>
where
    U: Ui + 'static,
{
    pub fn open_file(&mut self, path: PathBuf) {
        let pushed = self.windows.mutate(|windows| {
            let cur_window = self.cur_window.load(Ordering::Relaxed);
            let (file, checker) = self.file_cfg.clone().open_path(path).build();
            windows[cur_window].push_file(file, checker)
        });

        match pushed {
            Ok((area, _)) => build_file(self.windows, area, self.context),
            Err(err) => self.context.notify(err.into()),
        }
    }

    pub fn push_widget<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let cur_window = self.cur_window.load(Ordering::Relaxed);
        self.windows.write()[cur_window].push_to_master(widget, checker, specs)
    }

    pub fn push_widget_to<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
        area: &U::Area,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let cur_window = self.cur_window.load(Ordering::Relaxed);
        self.windows.write()[cur_window].push(widget, area, checker, specs, false)
    }

    pub fn group_widget_with<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
        area: &U::Area,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let cur_window = self.cur_window.load(Ordering::Relaxed);
        self.windows.write()[cur_window].push(widget, area, checker, specs, true)
    }

    /// Start the application, initiating a read/response loop.
    pub fn start(mut self, rx: mpsc::Receiver<Event>) -> Vec<(RwData<File>, bool)> {
        // This loop is very useful when trying to find deadlocks.
        #[cfg(feature = "deadlocks")]
        crate::thread::spawn(|| {
            use std::io::Write;

            loop {
                std::thread::sleep(std::time::Duration::from_secs(2));
                let mut file = std::io::BufWriter::new(
                    std::fs::OpenOptions::new()
                        .append(true)
                        .create(true)
                        .open("deadlocks")
                        .unwrap(),
                );

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

        self.ui.mutate(|ui| {
            ui.flush_layout();
            ui.start(Sender::new(self.tx.clone()), self.context);
        });

        // The main loop.
        loop {
            let cur_window = self.cur_window.load(Ordering::Relaxed);
            self.windows.inspect(|windows| {
                while windows
                    .iter()
                    .flat_map(Window::nodes)
                    .any(Node::needs_update)
                {
                    for (widget, area) in windows.iter().flat_map(Window::widgets) {
                        widget.update(area);
                    }
                }

                for (widget, area) in windows[cur_window].widgets() {
                    widget.update_and_print(area);
                }
            });

            let reason_to_break = self.session_loop(&rx);

            match reason_to_break {
                BreakTo::QuitDuat => {
                    self.ui.write().close();
                    self.context.end_duat();
                    self.save_cache(true);

                    break Vec::new();
                }
                BreakTo::ReloadConfig => {
                    self.save_cache(false);

                    break self.reload_config();
                }
                BreakTo::OpenFile(file) => self.open_file(file),
            }
        }
    }

    fn save_cache(&self, is_quitting_duat: bool) {
        let windows = self.windows.read();
        for (widget, area) in windows.iter().flat_map(Window::widgets) {
            widget.inspect_as::<File, ()>(|file| {
                if is_quitting_duat && !file.exists() {
                    delete_cache(file.path());
                    return;
                }
                if let Some(cache) = area.cache() {
                    store_cache(file.path(), cache);
                }

                let input = widget.input().unwrap().read();

                let mut cursors = input.cursors().unwrap().clone();
                if is_quitting_duat {
                    cursors.remove_extras();
                }
                store_cache(file.path(), cursors);
            });
        }
    }

    fn reload_config(self) -> Vec<(RwData<File>, bool)> {
        self.ui.write().end();
        self.context.end_duat();
        while crate::thread::still_running() {
            std::thread::sleep(Duration::from_micros(500));
        }
        let windows = self.windows.read();
        windows
            .iter()
            .flat_map(Window::widgets)
            .filter_map(|(widget, area)| {
                widget.downcast::<File>().map(|file| {
                    ActiveWidget::<U>::text_mut(&mut *file.write()).clear_tags();
                    (file, area.is_active())
                })
            })
            .collect()
    }

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop(&mut self, rx: &mpsc::Receiver<Event>) -> BreakTo {
        let cur_window = self.cur_window.load(Ordering::Relaxed);
        let windows = self.windows.read();

        std::thread::scope(|s| {
            loop {
                let active_window = &windows[cur_window];

                if let Ok(event) = rx.recv_timeout(Duration::from_millis(10)) {
                    match event {
                        Event::Key(key) => {
                            active_window.send_key(key, self.context);
                        }
                        Event::Resize | Event::FormChange => {
                            for node in active_window.nodes() {
                                s.spawn(|| node.update_and_print());
                            }
                            continue;
                        }
                        Event::ReloadConfig => break BreakTo::ReloadConfig,
                        Event::Quit => break BreakTo::QuitDuat,
                        Event::OpenFile(file) => break BreakTo::OpenFile(file),
                    }
                }

                for node in active_window.nodes() {
                    if node.needs_update() {
                        s.spawn(|| node.update_and_print());
                    }
                }
            }
        })
    }

    fn open_file_from_cfg(&mut self, file_cfg: FileCfg<U>, is_active: bool) {
        let pushed = self.windows.mutate(|windows| {
            let cur_window = self.cur_window.load(Ordering::Relaxed);

            let (widget, checker) = file_cfg.build();

            let pushed = windows[cur_window].push_file(widget.clone(), checker);

            if let Ok((area, _)) = &pushed
                && is_active
            {
                self.set_active_file(widget, area);
            }

            pushed
        });

        match pushed {
            Ok((area, _)) => build_file(self.windows, area, self.context),
            Err(err) => self.context.notify(err.into()),
        }
    }

    fn set_active_file(&self, widget: Widget<U>, area: &U::Area) {
        let Some((_, file, input)) = widget.as_active().and_then(|(active, input)| {
            let file = active.clone().try_downcast::<File>()?;
            Some((active, file, input))
        }) else {
            return;
        };
        self.context.set_cur(
            (
                file,
                area.clone(),
                input.clone(),
                widget.related_widgets().unwrap(),
            ),
            widget,
        );
    }
}

enum BreakTo {
    ReloadConfig,
    OpenFile(PathBuf),
    QuitDuat,
}

fn add_session_commands<U>(session: &Session<U>, context: Context<U>, tx: mpsc::Sender<Event>)
where
    U: Ui,
{
    context
        .add_cmd(["quit", "q"], {
            let tx = tx.clone();

            move |_flags, _args| {
                tx.send(Event::Quit).unwrap();
                Ok(None)
            }
        })
        .unwrap();

    context
        .add_cmd(["write", "w"], move |_flags, mut args| {
            let file = context.cur_file()?;

            let paths = {
                let mut paths = Vec::new();

                while let Ok(arg) = args.next() {
                    paths.push(arg.to_string());
                }

                paths
            };

            if paths.is_empty() {
                file.inspect(|file, _, _| {
                    if let Some(name) = file.set_name() {
                        let bytes = file.write()?;
                        ok!("Wrote " [*a] bytes [] " bytes to " [*a] name [] ".")
                    } else {
                        Err(err!("Give the file a name, to write it with"))
                    }
                })
            } else {
                file.inspect(|file, _, _| {
                    let mut bytes = 0;
                    for path in &paths {
                        bytes = file.write_to(path)?;
                    }

                    let files_text = {
                        let mut builder = Text::builder();
                        ok!(builder, [*a] { &paths[0] });

                        for path in paths.iter().skip(1).take(paths.len() - 1) {
                            ok!(builder, [] ", " [*a] path)
                        }

                        if paths.len() > 1 {
                            ok!(builder, [] " and " [*a] { paths.last().unwrap() })
                        }

                        builder.finish()
                    };

                    ok!("Wrote " [*a] bytes [] " bytes to " files_text [] ".")
                })
            }
        })
        .unwrap();

    context
        .add_cmd(["edit", "e"], {
            let windows = session.windows.clone();

            move |_, mut args| {
                let file = args.next_else(err!("No path supplied."))?;

                let path = PathBuf::from(file);
                let name = path
                    .file_name()
                    .ok_or(err!("No file in path"))?
                    .to_string_lossy()
                    .to_string();

                let read_windows = windows.read();
                let Some((window_index, entry)) = read_windows
                    .iter()
                    .enumerate()
                    .flat_map(window_index_widget)
                    .find(|(_, (widget, ..))| {
                        widget
                            .inspect_as::<File, bool>(|file| {
                                file.set_name().is_some_and(|cmp| cmp == name)
                            })
                            .unwrap_or(false)
                    })
                else {
                    tx.send(Event::OpenFile(path)).unwrap();
                    return ok!("Created " [*a] file [] ".");
                };

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                });

                ok!("Switched to " [*a] { file_name(&entry) } [] ".")
            }
        })
        .unwrap();

    context
        .add_cmd(["buffer", "b"], {
            let windows = session.windows.clone();

            move |_, mut args| {
                let path: PathBuf = args.next_as()?;
                let name = path
                    .file_name()
                    .ok_or(err!("No file in path"))?
                    .to_string_lossy()
                    .to_string();

                let read_windows = windows.read();
                let (window_index, entry) = read_windows
                    .iter()
                    .enumerate()
                    .flat_map(window_index_widget)
                    .find(|(_, (widget, ..))| {
                        widget
                            .inspect_as::<File, bool>(|file| {
                                file.set_name().is_some_and(|cmp| cmp == name)
                            })
                            .unwrap_or(false)
                    })
                    .ok_or(err!("No open files named " [*a] name [] "."))?;

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                });

                ok!("Switched to " [*a] { file_name(&entry) } [] ".")
            }
        })
        .unwrap();

    context
        .add_cmd(["next-file"], {
            let windows = session.windows.clone();
            let cur_window = session.cur_window;

            move |flags, _| {
                let file = context.cur_file()?;
                let read_windows = windows.read();
                let window_index = cur_window.load(Ordering::Acquire);

                let widget_index = read_windows[window_index]
                    .widgets()
                    .position(|(widget, _)| file.file_ptr_eq(widget))
                    .unwrap();

                let (new_window, entry) = if flags.word("global") {
                    iter_around::<U>(&read_windows, window_index, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .ok_or_else(|| err!("There are no other open files."))?
                } else {
                    let slice = &read_windows[window_index..=window_index];
                    let (_, entry) = iter_around(slice, 0, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .ok_or_else(|| err!("There are no other files open in this window."))?;

                    (window_index, entry)
                };

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let cur_window = cur_window;
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                    cur_window.store(new_window, Ordering::Release);
                });

                ok!("Switched to " [*a] { file_name(&entry) } [] ".")
            }
        })
        .unwrap();

    context
        .add_cmd(["prev-file"], {
            let windows = session.windows.clone();
            let cur_window = session.cur_window;

            move |flags, _| {
                let file = context.cur_file()?;
                let read_windows = windows.read();
                let window_index = cur_window.load(Ordering::Acquire);

                let widget_index = read_windows[window_index]
                    .widgets()
                    .position(|(widget, _)| file.file_ptr_eq(widget))
                    .unwrap();

                let (new_window, entry) = if flags.word("global") {
                    iter_around_rev::<U>(&read_windows, window_index, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .ok_or_else(|| err!("There are no other open files."))?
                } else {
                    let slice = &read_windows[window_index..=window_index];
                    let (_, entry) = iter_around_rev(slice, 0, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .ok_or_else(|| err!("There are no other files open in this window."))?;

                    (window_index, entry)
                };

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                    cur_window.store(new_window, Ordering::Release);
                });

                ok!("Switched to " [*a] { file_name(&entry) } [] ".")
            }
        })
        .unwrap();

    context
        .add_cmd(["return-to-file"], {
            let windows = session.windows.clone();
            let cur_window = session.cur_window;

            move |_, _| {
                let file = context.cur_file()?;
                let read_windows = windows.read();
                let window_i = cur_window.load(Ordering::Acquire);

                let (new_window, entry) = read_windows
                    .iter()
                    .enumerate()
                    .flat_map(window_index_widget)
                    .find(|(_, (widget, _))| file.file_ptr_eq(widget))
                    .unwrap();

                crate::thread::queue({
                    let (widget, area) = (entry.0.clone(), entry.1.clone());
                    let windows = windows.clone();
                    move || {
                        switch_widget(&(widget, area), &windows.read(), window_i, context);
                        cur_window.store(new_window, Ordering::Release);
                    }
                });

                Ok(Some(
                    text!("Returned to " [*a] { file_name(&entry) } [] "."),
                ))
            }
        })
        .unwrap();
}

fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl DoubleEndedIterator<Item = (usize, (&Widget<U>, &U::Area))> {
    window.widgets().map(move |widget| (index, widget))
}

pub(crate) fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, (&Widget<U>, &U::Area))> {
    let prev_len: usize = windows.iter().take(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .skip(window)
        .flat_map(window_index_widget)
        .skip(widget + 1)
        .chain(
            windows
                .iter()
                .enumerate()
                .take(window + 1)
                .flat_map(window_index_widget)
                .take(prev_len + widget),
        )
}

pub(crate) fn iter_around_rev<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, (&Widget<U>, &U::Area))> {
    let next_len: usize = windows.iter().skip(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .rev()
        .skip(windows.len() - window)
        .flat_map(move |(i, win)| {
            window_index_widget((i, win))
                .rev()
                .skip(win.len_widgets() - widget)
        })
        .chain(
            windows
                .iter()
                .enumerate()
                .rev()
                .take(windows.len() - window)
                .flat_map(move |(i, win)| window_index_widget((i, win)).rev())
                .take(next_len - (widget + 1)),
        )
}

pub(crate) fn switch_widget<U: Ui>(
    entry: &(Widget<U>, U::Area),
    windows: &[Window<U>],
    window: usize,
    context: Context<U>,
) {
    if let Some((widget, area)) = windows[window]
        .widgets()
        .find(|(widget, _)| context.cur_widget().unwrap().widget_ptr_eq(widget))
    {
        widget.on_unfocus(area);
        widget.update_and_print(area);
    }

    let (widget, area) = entry;

    context
        .cur_widget()
        .unwrap()
        .set(widget.clone(), area.clone());

    let (active, input) = widget.as_active().unwrap();
    if let Some(file) = active.try_downcast::<File>() {
        context.cur_file().unwrap().set((
            file,
            area.clone(),
            input.clone(),
            widget.related_widgets().unwrap(),
        ));
    }

    area.set_as_active();
    widget.on_focus(area);
    if entry.0.as_active().unwrap().0.data_is::<File>() {
        BACK_TO_FILE.store(true, Ordering::Relaxed)
    };
    widget.update_and_print(area);
}

pub static BACK_TO_FILE: AtomicBool = AtomicBool::new(false);

fn file_name<U: Ui>((widget, _): &(&Widget<U>, &U::Area)) -> String {
    widget.inspect_as::<File, String>(File::name).unwrap()
}

unsafe impl<U: Ui> Send for SessionCfg<U> {}
unsafe impl<U: Ui> Sync for SessionCfg<U> {}

unsafe impl<U: Ui> Send for Session<U> {}
unsafe impl<U: Ui> Sync for Session<U> {}
