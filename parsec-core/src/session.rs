use std::{
    any::TypeId,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
    time::Duration,
};

use crossterm::event::{self, Event};

use crate::{
    commands::Command,
    data::RwData,
    input::{Editor, InputMethod},
    log_info,
    text::PrintCfg,
    ui::{build_file, Area, FileBuilder, PushSpecs, Ui, Window, WindowBuilder},
    widgets::{
        ActiveWidget, ActiveWidgetCfg, File, FileCfg, LineNumbers, PassiveWidget, StatusLine,
        Widget,
    },
    BREAK_LOOP, COMMANDS, CURRENT_FILE, CURRENT_WIDGET, SHOULD_QUIT,
};

#[allow(clippy::type_complexity)]
pub struct SessionCfg<U, I>
where
    U: Ui,
    I: InputMethod<Widget = File> + Clone,
{
    ui: U,
    file_cfg: FileCfg<I>,
    file_fn: Box<dyn FnMut(&mut FileBuilder<U>, &RwData<File>)>,
    window_fn: Box<dyn FnMut(&mut WindowBuilder<U>)>,
}

impl<U> SessionCfg<U, Editor>
where
    U: Ui,
{
    pub fn new(ui: U) -> Self {
        crate::DEBUG_TIME_START.get_or_init(std::time::Instant::now);
        SessionCfg {
            ui,
            file_cfg: File::cfg(),
            file_fn: Box::new(|builder, _| {
                builder.push(LineNumbers::build);
                builder.push(StatusLine::build);
            }),
            window_fn: Box::new(|_| {}),
        }
    }
}

impl<U, I> SessionCfg<U, I>
where
    U: Ui,
    I: InputMethod<Widget = File> + Clone,
{
    pub fn session_from_args(mut self) -> Session<U, I> {
        let mut args = std::env::args();
        let first = args.nth(1).map(PathBuf::from);

        let (widget, checker) = if let Some(path) = first {
            self.file_cfg.clone().open(path).build()
        } else {
            self.file_cfg.clone().build()
        };

        let (active, input) = widget.as_active().unwrap();
        let file = active.clone().try_downcast::<File>().unwrap();
        CURRENT_FILE.set(file, input.clone());
        CURRENT_WIDGET.set(active.clone(), input.clone());

        let (window, area) = Window::new(&mut self.ui, widget, checker);

        let mut session = Session {
            ui: self.ui,
            windows: RwData::new(vec![window]),
            current_window: Arc::new(AtomicUsize::new(0)),
            file_cfg: self.file_cfg,
            file_fn: self.file_fn,
            window_fn: self.window_fn,
        };

        add_session_commands(&session);

        // Open and process files..
        build_file(&mut session.windows.write()[0], area, &mut session.file_fn);
        args.for_each(|file| session.open_file(PathBuf::from(file)));

        // Build the window's widgets.
        session.windows.mutate(|windows| {
            let mut builder = WindowBuilder::new(&mut windows[0]);
            (session.window_fn)(&mut builder);
        });

        session
    }

    pub fn session_from_prev<OldI>(self, _prev: Session<U, OldI>) -> Session<U, I>
    where
        OldI: InputMethod<Widget = File>,
    {
        todo!()
    }

    pub fn with_input<NewI>(self, input: NewI) -> SessionCfg<U, NewI>
    where
        NewI: InputMethod<Widget = File> + Clone,
    {
        SessionCfg {
            file_cfg: self.file_cfg.with_input(input),
            ui: self.ui,
            file_fn: self.file_fn,
            window_fn: self.window_fn,
        }
    }

    pub fn with_print_cfg(self, cfg: PrintCfg) -> Self {
        Self {
            file_cfg: self.file_cfg.with_print_cfg(cfg),
            ..self
        }
    }

    pub fn with_file_fn(
        self,
        file_fn: impl FnMut(&mut FileBuilder<U>, &RwData<File>) + 'static,
    ) -> Self {
        Self {
            file_fn: Box::new(file_fn),
            ..self
        }
    }

    pub fn with_file_fn_prefix(
        mut self,
        mut preffix: impl FnMut(&FileBuilder<U>, &RwData<File>) + 'static,
    ) -> Self {
        Self {
            file_fn: Box::new(move |builder, file| {
                preffix(builder, file);
                (self.file_fn)(builder, file)
            }),
            ..self
        }
    }

    pub fn with_file_fn_suffix(
        mut self,
        mut suffix: impl FnMut(&FileBuilder<U>, &RwData<File>) + 'static,
    ) -> Self {
        Self {
            file_fn: Box::new(move |builder, file| {
                (self.file_fn)(builder, file);
                suffix(builder, file)
            }),
            ..self
        }
    }

    pub fn with_window_fn(
        self,
        window_builder: impl FnMut(&mut WindowBuilder<U>) + 'static,
    ) -> Self {
        Self {
            window_fn: Box::new(window_builder),
            ..self
        }
    }

    pub fn with_window_fn_prefix(
        mut self,
        mut preffix: impl FnMut(&WindowBuilder<U>) + 'static,
    ) -> Self {
        Self {
            window_fn: Box::new(move |builder| {
                preffix(builder);
                (self.window_fn)(builder)
            }),
            ..self
        }
    }

    pub fn with_window_fn_suffix(
        mut self,
        mut suffix: impl FnMut(&WindowBuilder<U>) + 'static,
    ) -> Self {
        Self {
            window_fn: Box::new(move |builder| {
                (self.window_fn)(builder);
                suffix(builder)
            }),
            ..self
        }
    }
}

#[allow(clippy::type_complexity)]
pub struct Session<U, I>
where
    U: Ui,
    I: InputMethod<Widget = File>,
{
    ui: U,
    windows: RwData<Vec<Window<U>>>,
    current_window: Arc<AtomicUsize>,
    file_cfg: FileCfg<I>,
    file_fn: Box<dyn FnMut(&mut FileBuilder<U>, &RwData<File>)>,
    window_fn: Box<dyn FnMut(&mut WindowBuilder<U>)>,
}

impl<U> Session<U, Editor>
where
    U: Ui,
{
    pub fn config(ui: U) -> SessionCfg<U, Editor> {
        SessionCfg::new(ui)
    }
}

impl<U, I> Session<U, I>
where
    U: Ui + 'static,
    I: InputMethod<Widget = File> + Clone,
{
    pub fn open_file(&mut self, path: PathBuf) {
        let mut windows = self.windows.write();
        let current_window = self.current_window.load(Ordering::Relaxed);

        let (file, checker) = self.file_cfg.clone().open(path).build();

        let (area, _) = windows[current_window].push_file(file, checker, PushSpecs::right());

        build_file(&mut windows[current_window], area, &mut self.file_fn);
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
    pub fn start(&mut self) {
        self.ui.startup();

        // The main loop.
        loop {
            let current_window = self.current_window.load(Ordering::Relaxed);
            for (widget, area) in self.windows.read()[current_window].widgets() {
                widget.update_and_print(area);
            }

            self.session_loop();

            // TODO: Files to open
            // let mut files = std::mem::take(&mut *self.controler.files_to_open.write());
            // for file in files.drain(..) {
            //    self.open_file(file);
            //}

            if SHOULD_QUIT.load(Ordering::Acquire) {
                break;
            }
        }

        self.ui.shutdown();
    }

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop(&mut self) {
        let current_window = self.current_window.load(Ordering::Relaxed);
        let windows = self.windows.read();
        std::thread::scope(|scope| {
            loop {
                let active_window = &windows[current_window];

                if BREAK_LOOP.load(Ordering::Relaxed) {
                    BREAK_LOOP.store(false, Ordering::Relaxed);
                    break;
                }

                if let Ok(true) = event::poll(Duration::from_millis(5)) {
                    if let Event::Key(key) = event::read().unwrap() {
                        active_window.send_key(key, scope);
                    }
                }

                for node in active_window.nodes() {
                    if node.needs_update() {
                        scope.spawn(|| {
                            node.update_and_print();
                        });
                    }
                }

                self.ui.finish_printing()
            }
        });
    }
}

fn add_session_commands<U, I>(session: &Session<U, I>)
where
    U: Ui,
    I: InputMethod<Widget = File>,
{
    let write = Command::new(["write", "w"], move |_, args| {
        let paths: Vec<&str> = args.collect();
        if paths.is_empty() {
            CURRENT_FILE.inspect(|file, _| {
                if let Some(name) = file.name() {
                    file.write()
                        .map(|bytes| Some(format!("Wrote {bytes} bytes to {name}")))
                } else {
                    Err(String::from("Give the file a name, to write it with"))
                }
            })
        } else {
            CURRENT_FILE.inspect(|file, _| {
                let mut bytes = 0;
                for path in &paths {
                    bytes = file.write_to(path)?;
                }

                Ok(Some(format!("Wrote {bytes} to {}", paths.join(", "))))
            })
        }
    });

    let edit = {
        let windows = session.windows.clone();

        Command::new(["edit", "e"], move |_, args| {
            let Some(file) = args.next() else {
                return Err(String::from("No file to edit supplied"));
            };

            let path = PathBuf::from(file);
            let name = path
                .file_name()
                .ok_or(String::from("No file in path"))?
                .to_string_lossy()
                .to_string();

            let windows = windows.read();
            let Some((window_index, entry)) = windows
                .iter()
                .enumerate()
                .flat_map(window_index_widget)
                .find(|(_, (widget, ..))| {
                    widget
                        .inspect_as::<File, bool>(|file| file.name().is_some_and(|cmp| cmp == name))
                        .unwrap_or(false)
                })
            else {
                // TODO: this, lol
                // files_to_open.write().push(path);
                BREAK_LOOP.store(true, Ordering::Release);
                return Ok(Some(format!("Created {file}")));
            };

            switch_widget(&entry, &windows, window_index);

            Ok(Some(format!("Switched to {}.", file_name(&entry))))
        })
    };

    let buffer = {
        let windows = session.windows.clone();

        Command::new(["edit", "e"], move |_, args| {
            let Some(file) = args.next() else {
                return Err(String::from("No file to edit supplied"));
            };

            let path = PathBuf::from(file);
            let name = path
                .file_name()
                .ok_or(String::from("No file in path"))?
                .to_string_lossy()
                .to_string();

            let windows = windows.read();
            let (window_index, entry) = windows
                .iter()
                .enumerate()
                .flat_map(window_index_widget)
                .find(|(_, (widget, ..))| {
                    widget
                        .inspect_as::<File, bool>(|file| file.name().is_some_and(|cmp| cmp == name))
                        .unwrap_or(false)
                })
                .ok_or(format!("No open files with the name \"{name}\""))?;

            switch_widget(&entry, &windows, window_index);

            Ok(Some(format!("Switched to {}.", file_name(&entry))))
        })
    };

    let switch_to = {
        let windows = session.windows.clone();
        let current_window = session.current_window.clone();

        Command::new(["switch-to"], move |_, args| {
            let Some(widget_type) = args.next() else {
                return Err(String::from("No widget to switch to was supplied."));
            };

            let windows = windows.read();
            let window_index = current_window.load(Ordering::Acquire);

            // TODO: Make it prioritize file bound widgets.
            let (new_window, entry) = iter_around(&windows, window_index, 0)
                .filter(|(_, (widget, _))| widget.as_active().is_some())
                .find(|(_, (widget, _))| widget.widget_type() == widget_type)
                .ok_or(format!("No widget of type {widget_type} found."))?;

            switch_widget(&entry, &windows, window_index);
            current_window.store(new_window, Ordering::Release);

            Ok(Some(format!("Switched to {widget_type}.")))
        })
    };

    let next_file = {
        let windows = session.windows.clone();
        let current_window = session.current_window.clone();

        Command::new(["next-file"], move |flags, _| {
            let windows = windows.read();
            let window_index = current_window.load(Ordering::Acquire);

            let widget_index = windows[window_index]
                .widgets()
                .position(|(widget, _)| CURRENT_FILE.file_ptr_eq(widget))
                .unwrap();

            let (new_window, entry) = if flags.unit("global") {
                iter_around(&windows, window_index, widget_index)
                    .find(|(_, (widget, _))| widget.data_is::<File>())
                    .unwrap()
            } else {
                let slice = &windows[window_index..=window_index];
                let (_, entry) = iter_around(slice, 0, widget_index)
                    .find(|(_, (widget, _))| widget.data_is::<File>())
                    .unwrap();

                (window_index, entry)
            };

            switch_widget(&entry, &windows, window_index);
            current_window.store(new_window, Ordering::Release);

            Ok(Some(format!("Switched to {}.", file_name(&entry))))
        })
    };

    let prev_file = {
        let windows = session.windows.clone();
        let current_window = session.current_window.clone();

        Command::new(["prev-file"], move |flags, _| {
            let windows = windows.read();
            let window_index = current_window.load(Ordering::Acquire);

            let widget_index = windows[window_index]
                .widgets()
                .position(|(widget, _)| CURRENT_FILE.file_ptr_eq(widget))
                .unwrap();

            let (new_window, entry) = if flags.unit("global") {
                iter_around_rev(&windows, window_index, widget_index)
                    .find(|(_, (widget, _))| widget.data_is::<File>())
                    .unwrap()
            } else {
                let slice = &windows[window_index..=window_index];
                let (_, entry) = iter_around_rev(slice, 0, widget_index)
                    .find(|(_, (widget, _))| widget.data_is::<File>())
                    .unwrap();

                (window_index, entry)
            };

            switch_widget(&entry, &windows, window_index);
            current_window.store(new_window, Ordering::Release);

            Ok(Some(format!("Switched to {}.", file_name(&entry))))
        })
    };

    let return_to_file = {
        let windows = session.windows.clone();
        let current_window = session.current_window.clone();

        Command::new(["return-to-file"], move |_, _| {
            let windows = windows.read();
            let window_index = current_window.load(Ordering::Acquire);

            let (new_window, entry) = windows
                .iter()
                .enumerate()
                .flat_map(window_index_widget)
                .find(|(_, (widget, _))| CURRENT_FILE.file_ptr_eq(widget))
                .unwrap();

            switch_widget(&entry, &windows, window_index);
            current_window.store(new_window, Ordering::Release);

            Ok(Some(format!("Returned to {}.", file_name(&entry))))
        })
    };

    COMMANDS.try_add(write).unwrap();
    COMMANDS.try_add(edit).unwrap();
    COMMANDS.try_add(buffer).unwrap();
    COMMANDS.try_add(switch_to).unwrap();
    COMMANDS.try_add(next_file).unwrap();
    COMMANDS.try_add(prev_file).unwrap();
    COMMANDS.try_add(return_to_file).unwrap();
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
    let next_len: usize = windows
        .iter()
        .skip(window)
        .map(Window::<U>::len_widgets)
        .sum();

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

fn switch_widget<U: Ui>(entry: &(&Widget<U>, &U::Area), windows: &[Window<U>], window: usize) {
    let (widget, area) = entry;

    windows[window]
        .widgets()
        .find(|(widget, _)| CURRENT_WIDGET.widget_ptr_eq(widget))
        .inspect(|(widget, area)| widget.on_unfocus(area));

    let (active, input) = widget.as_active().unwrap();
    CURRENT_WIDGET.set(active.clone(), input.clone());

    let (active, input) = widget.as_active().unwrap();
    if let Ok(file) = active.clone().try_downcast::<File>() {
        CURRENT_FILE.set(file, input.clone());
    }

    area.set_as_active();
    widget.on_focus(area);
}

fn file_name<U: Ui>((widget, _): &(&Widget<U>, &U::Area)) -> String {
    widget
        .inspect_as::<File, Option<String>>(File::name)
        .flatten()
        .unwrap_or(String::from("*scratch file*"))
}
