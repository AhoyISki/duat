use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc::{self, TryRecvError},
        Arc,
    },
    time::Duration,
};

use crate::{
    data::RwData,
    input::InputMethod,
    text::{text, PrintCfg, Text},
    ui::{
        build_file, Area, Event, FileBuilder, Node, PushSpecs, Sender, Ui, Window, WindowBuilder,
    },
    widgets::{File, FileCfg, Widget},
    Globals,
};

#[allow(clippy::type_complexity)]
pub struct SessionCfg<U>
where
    U: Ui,
{
    ui: U,
    file_cfg: FileCfg<U>,
    file_fn: Box<dyn FnMut(&mut FileBuilder<U>)>,
    window_fn: Box<dyn FnMut(&mut WindowBuilder<U>)>,
    globals: Globals<U>,
}

impl<U> SessionCfg<U>
where
    U: Ui,
{
    pub fn session_from_args(mut self, tx: mpsc::Sender<Event>) -> Session<U> {
        self.ui.startup();

        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker) = if let Some(path) = first {
            self.file_cfg.clone().open_path(path).build()
        } else {
            self.file_cfg.clone().build()
        };

        let (window, area) = Window::new(&mut self.ui, widget.clone(), checker);

        let mut session = Session {
            ui: self.ui,
            windows: RwData::new(vec![window]),
            current_window: Arc::new(AtomicUsize::new(0)),
            file_cfg: self.file_cfg,
            file_fn: self.file_fn,
            window_fn: self.window_fn,
            globals: self.globals,
            tx,
        };

        session.set_active_file(widget, &area);

        self.globals.commands.add_windows(session.windows.clone());
        add_session_commands(&session, self.globals, session.tx.clone());

        // Open and process files..
        build_file(
            &mut session.windows.write()[0],
            area,
            &mut session.file_fn,
            self.globals,
        );
        args.for_each(|file| session.open_file(PathBuf::from(file)));

        // Build the window's widgets.
        session.windows.mutate(|windows| {
            let mut builder = WindowBuilder::new(&mut windows[0], self.globals);
            (session.window_fn)(&mut builder);
        });

        session
    }

    pub fn session_from_prev(
        mut self,
        prev_files: Vec<(RwData<File>, bool)>,
        tx: mpsc::Sender<Event>,
    ) -> Session<U> {
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

        let mut session = Session {
            ui: self.ui,
            windows: RwData::new(vec![window]),
            current_window: Arc::new(AtomicUsize::new(0)),
            file_cfg: self.file_cfg,
            file_fn: self.file_fn,
            window_fn: self.window_fn,
            globals: self.globals,
            tx,
        };

        session.set_active_file(widget, &area);

        self.globals.commands.add_windows(session.windows.clone());
        add_session_commands(&session, self.globals, session.tx.clone());

        // Open and process files..
        build_file(
            &mut session.windows.write()[0],
            area,
            &mut session.file_fn,
            self.globals,
        );

        for (file_cfg, is_active) in inherited_cfgs {
            session.open_file_from_cfg(file_cfg, is_active);
        }

        // Build the window's widgets.
        session.windows.mutate(|windows| {
            let mut builder = WindowBuilder::new(&mut windows[0], self.globals);
            (session.window_fn)(&mut builder);
        });

        session
    }

    pub fn set_input(&mut self, input: impl InputMethod<U, Widget = File> + Clone) {
        self.file_cfg.set_input(input);
    }

    pub fn set_print_cfg(&mut self, cfg: PrintCfg) {
        self.file_cfg.set_print_cfg(cfg);
    }

    pub fn set_file_fn(
        &mut self,
        file_fn: impl FnMut(&mut FileBuilder<U>) + Send + Sync + 'static,
    ) {
        self.file_fn = Box::new(file_fn);
    }

    pub fn preffix_file_fn(
        &mut self,
        mut preffix: impl FnMut(&mut FileBuilder<U>) + Send + Sync + 'static,
    ) {
        let mut file_fn = std::mem::replace(&mut self.file_fn, Box::new(|_| {}));

        self.file_fn = Box::new(move |builder| {
            preffix(builder);
            file_fn(builder);
        });
    }

    pub fn suffix_file_fn(
        &mut self,
        mut suffix: impl FnMut(&mut FileBuilder<U>) + Send + Sync + 'static,
    ) {
        let mut file_fn = std::mem::replace(&mut self.file_fn, Box::new(|_| {}));

        self.file_fn = Box::new(move |builder| {
            file_fn(builder);
            suffix(builder);
        });
    }

    pub fn set_window_fn(&mut self, window_fn: impl FnMut(&mut WindowBuilder<U>) + 'static) {
        self.window_fn = Box::new(window_fn);
    }

    pub fn preffix_window_fn(&mut self, mut preffix: impl FnMut(&mut WindowBuilder<U>) + 'static) {
        let mut window_fn = std::mem::replace(&mut self.window_fn, Box::new(|_| {}));

        self.window_fn = Box::new(move |builder| {
            preffix(builder);
            window_fn(builder);
        });
    }

    pub fn suffix_window_fn(&mut self, mut suffix: impl FnMut(&mut WindowBuilder<U>) + 'static) {
        let mut window_fn = std::mem::replace(&mut self.window_fn, Box::new(|_| {}));

        self.window_fn = Box::new(move |builder| {
            window_fn(builder);
            suffix(builder);
        });
    }

    pub fn mut_print_cfg(&mut self) -> &mut PrintCfg {
        self.file_cfg.mut_print_cfg()
    }

    pub fn __new(ui: U, globals: Globals<U>) -> Self {
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);

        SessionCfg {
            ui,
            file_cfg: File::cfg(),
            file_fn: Box::new(|_| {}),
            window_fn: Box::new(|_| {}),
            globals,
        }
    }
}

#[allow(clippy::type_complexity)]
pub struct Session<U>
where
    U: Ui,
{
    ui: U,
    windows: RwData<Vec<Window<U>>>,
    current_window: Arc<AtomicUsize>,
    file_cfg: FileCfg<U>,
    file_fn: Box<dyn FnMut(&mut FileBuilder<U>)>,
    window_fn: Box<dyn FnMut(&mut WindowBuilder<U>)>,
    globals: Globals<U>,
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

        let (area, _) = windows[current_window].push_file(file, checker, PushSpecs::right());

        build_file(
            &mut windows[current_window],
            area,
            &mut self.file_fn,
            self.globals,
        );
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

    pub fn cluster_widget_with<F>(
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
        self.ui.set_sender(Sender::new(self.tx.clone()));

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

            let reason = self.session_loop(&rx);

            match reason {
                BreakTo::QuitDuat => {
                    self.ui.shutdown();
                    break Vec::new();
                }
                BreakTo::ReloadConfig => {
                    self.ui.unload();
                    let windows = self.windows.read();
                    break windows
                        .iter()
                        .flat_map(Window::widgets)
                        .filter_map(|(widget, area)| {
                            widget.downcast::<File>().zip(Some(area.is_active()))
                        })
                        .collect();
                }
                _ => {}
            }
        }
    }

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop(&mut self, rx: &mpsc::Receiver<Event>) -> BreakTo {
        let current_window = self.current_window.load(Ordering::Relaxed);
        let windows = self.windows.read();

        std::thread::scope(|scope| {
            loop {
                let active_window = &windows[current_window];

                match rx.try_recv() {
                    Ok(event) => match event {
                        Event::Key(key) => {
                            active_window.send_key(key, scope, self.globals);
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
                    },
                    Err(TryRecvError::Disconnected) => break BreakTo::QuitDuat,
                    _ => (),
                }

                std::thread::sleep(Duration::from_millis(10));

                for node in active_window.nodes() {
                    if node.needs_update() {
                        scope.spawn(|| node.update_and_print());
                    }
                }
            }
        })
    }

    fn open_file_from_cfg(&mut self, file_cfg: FileCfg<U>, is_active: bool) {
        let mut windows = self.windows.write();
        let current_window = self.current_window.load(Ordering::Relaxed);

        let (widget, checker) = file_cfg.build();

        let (area, _) =
            windows[current_window].push_file(widget.clone(), checker, PushSpecs::right());

        if is_active {
            self.set_active_file(widget, &area);
        }

        build_file(
            &mut windows[current_window],
            area,
            &mut self.file_fn,
            self.globals,
        );
    }

    fn set_active_file(&self, widget: Widget<U>, area: &U::Area) {
        let Some((_, file, input)) = widget.as_active().and_then(|(active, input)| {
            let file = active.clone().try_downcast::<File>()?;
            Some((active, file, input))
        }) else {
            return;
        };
        self.globals.current_file.set((
            file,
            area.clone(),
            input.clone(),
            widget.related_widgets().unwrap(),
        ));
        self.globals.current_widget.set(widget, area.clone());
    }
}

enum BreakTo {
    ReloadConfig,
    OpenFiles,
    QuitDuat,
}

fn add_session_commands<U>(session: &Session<U>, globals: Globals<U>, tx: mpsc::Sender<Event>)
where
    U: Ui,
{
    globals
        .commands
        .add(["quit", "q"], {
            let tx = tx.clone();

            move |_flags, _args| {
                tx.send(Event::Quit).unwrap();
                Ok(None)
            }
        })
        .unwrap();

    globals
        .commands
        .add(["write", "w"], move |_flags, mut args| {
            let paths = {
                let mut paths = Vec::new();

                while let Ok(arg) = args.next() {
                    paths.push(arg.to_string());
                }

                paths
            };

            if paths.is_empty() {
                globals.current_file.inspect(|file, _, _| {
                    if let Some(name) = file.set_name() {
                        file.write()
                            .map(|bytes| {
                                Some(text!(
                                    "Wrote " [AccentErr] bytes
                                    [Default] " bytes to " [AccentErr] name [Default] "."
                                ))
                            })
                            .map_err(Text::from)
                    } else {
                        Err(text!("Give the file a name, to write it with"))
                    }
                })
            } else {
                globals.current_file.inspect(|file, _, _| {
                    let mut bytes = 0;
                    for path in &paths {
                        bytes = file.write_to(path)?;
                    }

                    let files_text = {
                        let mut builder = Text::builder();
                        text!(builder, [AccentErr] { &paths[0] });

                        for path in paths.iter().skip(1).take(paths.len() - 1) {
                            text!(builder, [] ", " [AccentErr] path)
                        }

                        if paths.len() > 1 {
                            text!(builder, [] " and " [AccentErr] { paths.last().unwrap() })
                        }

                        builder.finish()
                    };

                    Ok(Some(text!(
                        "Wrote " [AccentErr] bytes
                        [Default] " bytes to " files_text [Default] "."
                    )))
                })
            }
        })
        .unwrap();

    globals
        .commands
        .add(["edit", "e"], {
            let windows = session.windows.clone();

            move |_, mut args| {
                let file = args.next_else(text!("No path supplied."))?;

                let path = PathBuf::from(file);
                let name = path
                    .file_name()
                    .ok_or(text!("No file in path"))?
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
                    tx.send(Event::OpenFiles).unwrap();
                    return Ok(Some(text!("Created " [AccentOk] file [] ".")));
                };

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, globals);
                });

                Ok(Some(
                    text!("Switched to " [AccentOk] { file_name(&entry) } [Default] "."),
                ))
            }
        })
        .unwrap();

    globals
        .commands
        .add(["buffer", "b"], {
            let windows = session.windows.clone();

            move |_, mut args| {
                let file = args.next_else(text!("No path supplied."))?;

                let path = PathBuf::from(file);
                let name = path
                    .file_name()
                    .ok_or(text!("No file in path"))?
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
                    .ok_or(text!("No open files named " [AccentErr] name [Default] "."))?;

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, globals);
                });

                Ok(Some(
                    text!("Switched to " [AccentOk] { file_name(&entry) } [Default] "."),
                ))
            }
        })
        .unwrap();

    globals
        .commands
        .add(["switch-to"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |_, mut args| {
                let type_name = args.next_else(text!("No widget supplied."))?;

                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let widget = globals.current_file.get_related_widget(type_name);

                let (new_window, entry) = widget
                    .and_then(|(widget, _)| {
                        read_windows
                            .iter()
                            .enumerate()
                            .flat_map(|(i, window)| window.widgets().map(move |entry| (i, entry)))
                            .find(|(_, (cmp, _))| cmp.ptr_eq(&widget))
                    })
                    .or_else(|| {
                        iter_around(&read_windows, window_index, 0)
                            .filter(|(_, (widget, _))| widget.as_active().is_some())
                            .find(|(_, (widget, _))| widget.type_name() == type_name)
                    })
                    .ok_or(text!("No widget of type " [AccentErr] type_name [Default] " found."))?;

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let current_window = current_window.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, globals);
                    current_window.store(new_window, Ordering::Release);
                });

                Ok(Some(
                    text!("Switched to " [AccentOk] type_name [Default] "."),
                ))
            }
        })
        .unwrap();

    globals
        .commands
        .add(["next-file"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |flags, _| {
                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let widget_index = read_windows[window_index]
                    .widgets()
                    .position(|(widget, _)| globals.current_file.file_ptr_eq(widget))
                    .unwrap();

                let (new_window, entry) = if flags.long("global") {
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
                    switch_widget(&(widget, area), &windows.read(), window_index, globals);
                    current_window.store(new_window, Ordering::Release);
                });

                Ok(Some(
                    text!("Switched to " [AccentOk] { file_name(&entry) } [Default] "."),
                ))
            }
        })
        .unwrap();

    globals
        .commands
        .add(["prev-file"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |flags, _| {
                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let widget_index = read_windows[window_index]
                    .widgets()
                    .position(|(widget, _)| globals.current_file.file_ptr_eq(widget))
                    .unwrap();

                let (new_window, entry) = if flags.long("global") {
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
                    switch_widget(&(widget, area), &windows.read(), window_index, globals);
                    current_window.store(new_window, Ordering::Release);
                });

                Ok(Some(
                    text!("Switched to " [AccentOk] { file_name(&entry) } [Default] "."),
                ))
            }
        })
        .unwrap();

    globals
        .commands
        .add(["return-to-file"], {
            let windows = session.windows.clone();
            let current_window = session.current_window.clone();

            move |_, _| {
                let read_windows = windows.read();
                let window_index = current_window.load(Ordering::Acquire);

                let (new_window, entry) = read_windows
                    .iter()
                    .enumerate()
                    .flat_map(window_index_widget)
                    .find(|(_, (widget, _))| globals.current_file.file_ptr_eq(widget))
                    .unwrap();

                let (widget, area) = (entry.0.clone(), entry.1.clone());
                let windows = windows.clone();
                let current_window = current_window.clone();
                std::thread::spawn(move || {
                    switch_widget(&(widget, area), &windows.read(), window_index, globals);
                    current_window.store(new_window, Ordering::Release);
                });

                Ok(Some(
                    text!("Returned to " [AccentOk] { file_name(&entry) } [Default] "."),
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
    globals: Globals<U>,
) {
    let (widget, area) = entry;

    if let Some((widget, area)) = windows[window]
        .widgets()
        .find(|(widget, _)| globals.current_widget.widget_ptr_eq(widget))
    {
        widget.on_unfocus(area);
        widget.update_and_print(area);
    }

    globals.current_widget.set(widget.clone(), area.clone());

    let (active, input) = widget.as_active().unwrap();
    if let Some(file) = active.try_downcast::<File>() {
        globals.current_file.set((
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
