use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc, Arc,
    },
    time::Duration,
};

use crate::{
    data::{Context, RwData},
    hooks::{self, OnWindowOpen},
    input::InputMethod,
    text::{err, ok, text, PrintCfg, Text},
    ui::{build_file, Area, Event, Node, PushSpecs, Sender, Ui, Window, WindowBuilder},
    widgets::{ActiveWidget, File, FileCfg, Widget},
};

#[doc(hidden)]
pub struct SessionCfg<U>
where
    U: Ui,
{
    ui: U,
    file_cfg: FileCfg<U>,
    context: Context<U>,
}

impl<U> SessionCfg<U>
where
    U: Ui,
{
    pub fn new(ui: U, context: Context<U>) -> Self {
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);

        SessionCfg {
            ui,
            file_cfg: FileCfg::new(),
            context,
        }
    }

    pub fn session_from_args(mut self, tx: mpsc::Sender<Event>) -> Session<U> {
        self.ui.open();
        self.ui.start(Sender::new(tx.clone()), self.context);

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker) = if let Some(path) = first {
            self.file_cfg.clone().open_path(path).build()
        } else {
            self.file_cfg.clone().build()
        };

        let (window, area) = Window::new(&mut self.ui, widget.clone(), checker);
        self.context.commands.add_windows(vec![window]);

        let mut session = Session {
            ui: self.ui,
            windows: self.context.commands.get_windows(),
            current_window: Arc::new(AtomicUsize::new(0)),
            file_cfg: self.file_cfg,
            context: self.context,
            tx,
        };

        session.set_active_file(widget, &area);

        add_session_commands(&session, self.context, session.tx.clone());

        // Open and process files.
        build_file(&mut session.windows.write()[0], area, self.context);
        args.for_each(|file| session.open_file(PathBuf::from(file)));

        // Build the window's widgets.
        session.windows.mutate(|windows| {
            let builder = WindowBuilder::new(&mut windows[0], self.context);
            hooks::trigger::<OnWindowOpen<U>>(builder);
        });

        session
    }

    pub fn session_from_prev(
        mut self,
        prev_files: Vec<(RwData<File>, bool)>,
        tx: mpsc::Sender<Event>,
    ) -> Session<U> {
        self.ui.start(Sender::new(tx.clone()), self.context);

        let mut inherited_cfgs = Vec::new();
        for (file, is_active) in prev_files {
            let mut file = file.write();
            let file_cfg = self.file_cfg.clone().take_from_prev(&mut file);
            inherited_cfgs.push((file_cfg, is_active))
        }

        let Some((file_cfg, _)) = inherited_cfgs.pop() else {
            unreachable!("There should've been at least one file.")
        };

        let (widget, checker) = file_cfg.build();

        let (window, area) = Window::new(&mut self.ui, widget.clone(), checker);
        self.context.commands.add_windows(vec![window]);

        let mut session = Session {
            ui: self.ui,
            windows: self.context.commands.get_windows(),
            current_window: Arc::new(AtomicUsize::new(0)),
            file_cfg: self.file_cfg,
            context: self.context,
            tx,
        };

        session.set_active_file(widget, &area);

        add_session_commands(&session, self.context, session.tx.clone());

        // Open and process files..
        build_file(&mut session.windows.write()[0], area, self.context);

        for (file_cfg, is_active) in inherited_cfgs {
            session.open_file_from_cfg(file_cfg, is_active);
        }

        // Build the window's widgets.
        session.windows.mutate(|windows| {
            let builder = WindowBuilder::new(&mut windows[0], self.context);
            hooks::trigger::<OnWindowOpen<U>>(builder);
        });

        session
    }

    pub fn set_input(&mut self, input: impl InputMethod<U, Widget = File> + Clone) {
        self.file_cfg.set_input(input);
    }

    pub fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.file_cfg.set_print_cfg(cfg);
    }

    pub fn mut_print_cfg(&mut self) -> &mut PrintCfg {
        self.file_cfg.mut_print_cfg()
    }
}

pub struct Session<U>
where
    U: Ui,
{
    ui: U,
    windows: RwData<Vec<Window<U>>>,
    current_window: Arc<AtomicUsize>,
    file_cfg: FileCfg<U>,
    context: Context<U>,
    tx: mpsc::Sender<Event>,
}

impl<U> Session<U>
where
    U: Ui + 'static,
{
    pub fn open_file(&mut self, path: PathBuf) {
        let mut windows = self.windows.write();
        let current_window = self.current_window.load(Ordering::Relaxed);

        let (file, checker) = self.file_cfg.clone().open_path(path).build();

        let (area, _) = windows[current_window].push_file(file, checker, PushSpecs::below());

        build_file(&mut windows[current_window], area, self.context);
    }

    pub fn push_widget<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let current_window = self.current_window.load(Ordering::Relaxed);
        self.windows.write()[current_window].push_to_master(widget, checker, specs)
    }

    pub fn push_widget_to<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
        area: &U::Area,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let current_window = self.current_window.load(Ordering::Relaxed);
        self.windows.write()[current_window].push(widget, area, checker, specs, false)
    }

    pub fn group_widget_with<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
        area: &U::Area,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        let current_window = self.current_window.load(Ordering::Relaxed);
        self.windows.write()[current_window].push(widget, area, checker, specs, true)
    }

    /// Start the application, initiating a read/response loop.
    pub fn start(mut self, rx: mpsc::Receiver<Event>) -> Vec<(RwData<File>, bool)> {
        // The main loop.
        loop {
            let current_window = self.current_window.load(Ordering::Relaxed);
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

                for (widget, area) in windows[current_window].widgets() {
                    widget.update_and_print(area);
                }
            });

            let reason_to_break = self.session_loop(&rx);

            match reason_to_break {
                BreakTo::QuitDuat => {
                    self.ui.close();
                    self.context.end_duat();

                    break Vec::new();
                }
                BreakTo::ReloadConfig => {
                    break self.reload_config();
                }
                _ => {}
            }
        }
    }

    fn reload_config(mut self) -> Vec<(RwData<File>, bool)> {
        self.ui.end();
        self.context.end_duat();
        while self.context.threads_are_running() {
            std::thread::sleep(Duration::from_micros(500));
        }
        let windows = self.windows.read();
        windows
            .iter()
            .flat_map(Window::widgets)
            .filter_map(|(widget, area)| {
                widget
                    .downcast::<File>()
                    .inspect(|file| {
                        // Remove the cursors, so that on the next
                        // reload, there aren't a bunch of leftover
                        // Tags.
                        if let Some(input) = widget.input()
                            && let Some(cursors) = input.read().cursors()
                        {
                            ActiveWidget::<U>::text_mut(&mut *file.write())
                                .remove_cursor_tags(cursors);
                        }
                    })
                    .zip(Some(area.is_active()))
            })
            .collect()
    }

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop(&mut self, rx: &mpsc::Receiver<Event>) -> BreakTo {
        let current_window = self.current_window.load(Ordering::Relaxed);
        let windows = self.windows.read();

        loop {
            let active_window = &windows[current_window];

            if let Ok(event) = rx.recv_timeout(Duration::from_millis(50)) {
                match event {
                    Event::Key(key) => {
                        active_window.send_key(key, self.context);
                    }
                    Event::Resize | Event::FormChange => {
                        for node in active_window.nodes() {
                            node.update_and_print();
                        }
                        continue;
                    }
                    Event::ReloadConfig => break BreakTo::ReloadConfig,
                    Event::Quit => break BreakTo::QuitDuat,
                    Event::OpenFiles => break BreakTo::OpenFiles,
                }
            }

            for node in active_window.nodes() {
                if node.needs_update() {
                    node.update_and_print();
                }
            }
        }
    }

    fn open_file_from_cfg(&mut self, file_cfg: FileCfg<U>, is_active: bool) {
        let mut windows = self.windows.write();
        let current_window = self.current_window.load(Ordering::Relaxed);

        let (widget, checker) = file_cfg.build();

        let (area, _) =
            windows[current_window].push_file(widget.clone(), checker, PushSpecs::below());

        if is_active {
            self.set_active_file(widget, &area);
        }

        build_file(&mut windows[current_window], area, self.context);
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
    OpenFiles,
    QuitDuat,
}

fn add_session_commands<U>(session: &Session<U>, context: Context<U>, tx: mpsc::Sender<Event>)
where
    U: Ui,
{
    context
        .commands
        .add(["quit", "q"], {
            let tx = tx.clone();

            move |_flags, _args| {
                tx.send(Event::Quit).unwrap();
                Ok(None)
            }
        })
        .unwrap();

    context
        .commands
        .add(["write", "w"], move |_flags, mut args| {
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
                    if let Some(name) = file.name_set() {
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
        .commands
        .add(["edit", "e"], {
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
                                file.name_set().is_some_and(|cmp| cmp == name)
                            })
                            .unwrap_or(false)
                    })
                else {
                    tx.send(Event::OpenFiles).unwrap();
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
        .commands
        .add(["buffer", "b"], {
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
                let (window_index, entry) = read_windows
                    .iter()
                    .enumerate()
                    .flat_map(window_index_widget)
                    .find(|(_, (widget, ..))| {
                        widget
                            .inspect_as::<File, bool>(|file| {
                                file.name_set().is_some_and(|cmp| cmp == name)
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
        .commands
        .add(["switch-to"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |_, mut args| {
                let file = context.cur_file()?;
                let ty = args.next_else(err!("No widget supplied."))?;

                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let (window, entry) = if let Some((widget, _)) = file.get_related_widget(ty) {
                    read_windows
                        .iter()
                        .enumerate()
                        .flat_map(|(i, window)| window.widgets().map(move |entry| (i, entry)))
                        .find(|(_, (cmp, _))| cmp.ptr_eq(&widget))
                } else {
                    iter_around(&read_windows, window_index, 0)
                        .filter(|(_, (widget, _))| widget.as_active().is_some())
                        .find(|(_, (widget, _))| widget.type_name() == ty)
                }
                .ok_or(err!("No widget of type " [*a] ty [] " found."))?;

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let current_window = current_window.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                    current_window.store(window, Ordering::Release);
                });

                ok!("Switched to " [*a] ty [] ".")
            }
        })
        .unwrap();

    context
        .commands
        .add(["next-file"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |flags, _| {
                let file = context.cur_file()?;
                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let widget_index = read_windows[window_index]
                    .widgets()
                    .position(|(widget, _)| file.file_ptr_eq(widget))
                    .unwrap();

                let (new_window, entry) = if flags.word("global") {
                    iter_around(&read_windows, window_index, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .unwrap()
                } else {
                    let slice = &read_windows[window_index..=window_index];
                    let (_, entry) = iter_around(slice, 0, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .unwrap();

                    (window_index, entry)
                };

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let current_window = current_window.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                    current_window.store(new_window, Ordering::Release);
                });

                ok!("Switched to " [*a] { file_name(&entry) } [] ".")
            }
        })
        .unwrap();

    context
        .commands
        .add(["prev-file"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |flags, _| {
                let file = context.cur_file()?;
                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let widget_index = read_windows[window_index]
                    .widgets()
                    .position(|(widget, _)| file.file_ptr_eq(widget))
                    .unwrap();

                let (new_window, entry) = if flags.word("global") {
                    iter_around_rev(&read_windows, window_index, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .unwrap()
                } else {
                    let slice = &read_windows[window_index..=window_index];
                    let (_, entry) = iter_around_rev(slice, 0, widget_index)
                        .find(|(_, (widget, _))| widget.data_is::<File>())
                        .unwrap();

                    (window_index, entry)
                };

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let current_window = current_window.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                    current_window.store(new_window, Ordering::Release);
                });

                ok!("Switched to " [*a] { file_name(&entry) } [] ".")
            }
        })
        .unwrap();

    context
        .commands
        .add(["return-to-file"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |_, _| {
                let file = context.cur_file()?;
                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let (new_window, entry) = read_windows
                    .iter()
                    .enumerate()
                    .flat_map(window_index_widget)
                    .find(|(_, (widget, _))| file.file_ptr_eq(widget))
                    .unwrap();

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let current_window = current_window.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, context);
                    current_window.store(new_window, Ordering::Release);
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

fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, (&Widget<U>, &U::Area))> {
    let prev_len: usize = windows
        .iter()
        .take(window + 1)
        .map(Window::<U>::len_widgets)
        .sum();

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
                .take(prev_len + widget + 1),
        )
}

fn iter_around_rev<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, (&Widget<U>, &U::Area))> {
    let next_len: usize = windows.iter().skip(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .rev()
        .skip(windows.len() - 1 - window)
        .flat_map(move |(index, window)| {
            window_index_widget((index, window))
                .rev()
                .skip(window.len_widgets() - widget)
        })
        .chain(
            windows
                .iter()
                .enumerate()
                .rev()
                .take(windows.len() - window)
                .flat_map(move |(index, window)| window_index_widget((index, window)).rev())
                .take(next_len - widget),
        )
}

fn switch_widget<U: Ui>(
    entry: &(Widget<U>, U::Area),
    windows: &[Window<U>],
    window: usize,
    context: Context<U>,
) {
    let (widget, area) = entry;

    if let Some((widget, area)) = windows[window]
        .widgets()
        .find(|(widget, _)| context.cur_widget().unwrap().widget_ptr_eq(widget))
    {
        widget.on_unfocus(area);
        widget.update_and_print(area);
    }

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
    widget.update_and_print(area);
}

fn file_name<U: Ui>((widget, _): &(&Widget<U>, &U::Area)) -> String {
    widget.inspect_as::<File, String>(File::name).unwrap()
}

unsafe impl<U: Ui> Send for SessionCfg<U> {}
unsafe impl<U: Ui> Sync for SessionCfg<U> {}

unsafe impl<U: Ui> Send for Session<U> {}
unsafe impl<U: Ui> Sync for Session<U> {}
