use std::{path::PathBuf, sync::atomic::Ordering, time::Duration};

use crossterm::event::{self, Event};

use crate::{
    data::RwData,
    input::{Editor, InputMethod},
    text::PrintCfg,
    ui::{build_file, FileBuilder, PushSpecs, Ui, Window, WindowBuilder},
    widgets::{
        ActiveWidget, ActiveWidgetCfg, File, FileCfg, LineNumbers, PassiveWidget, StatusLine,
        Widget,
    },
    Controler, ACTIVE_FILE, BREAK_LOOP, SHOULD_QUIT,
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
            file_cfg: File::config(),
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

        let active = {
            let (active, input) = widget.as_active().unwrap();
            let file = active.clone().try_downcast::<File>().unwrap();
            ACTIVE_FILE.set(file, input.clone());

            active.clone()
        };

        let (window, area) = Window::new(&mut self.ui, widget, checker);
        let mut controler = Controler::new(window, active);

        build_file(&mut controler, area, &mut self.file_fn);

        let mut session = Session {
            ui: self.ui,
            controler,
            file_cfg: self.file_cfg,
            file_fn: self.file_fn,
            window_fn: self.window_fn,
        };

        for file in args {
            session.open_file(PathBuf::from(file));
        }

        let files_region = session
            .controler
            .inspect_active_window(|window| window.files_region().clone());

        let mut builder = WindowBuilder::new(&mut session.controler, files_region);
        (session.window_fn)(&mut builder);

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
    controler: Controler<U>,
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
        let (file, checker) = self.file_cfg.clone().open(path).build();
        let (area, _) = self
            .controler
            .mutate_active_window(|window| window.push_file(file, checker, PushSpecs::right()));

        build_file(&mut self.controler, area, &mut self.file_fn);
    }

    pub fn push_widget<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        self.controler
            .mutate_active_window(|window| window.push_to_master(widget, checker, specs))
    }

    pub fn push_widget_to<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
        area: &U::Area,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        self.controler
            .mutate_active_window(|window| window.push(widget, area, checker, specs, false))
    }

    pub fn cluster_widget_with<F>(
        &mut self,
        (widget, checker, specs): (Widget<U>, F, PushSpecs),
        area: &U::Area,
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static,
    {
        self.controler
            .mutate_active_window(|window| window.push(widget, area, checker, specs, true))
    }

    /// Start the application, initiating a read/response loop.
    pub fn start(&mut self) {
        self.ui.startup();

        // The main loop.
        loop {
            for (widget, area) in
                self.controler.windows.read()[self.controler.active_window].widgets()
            {
                widget.update_and_print(area);
            }

            self.session_loop();

            let mut files = std::mem::take(&mut *self.controler.files_to_open.write());
            for file in files.drain(..) {
                self.open_file(file);
            }

            if SHOULD_QUIT.load(Ordering::Acquire) {
                break;
            }
        }

        self.ui.shutdown();
    }

    /// Returns the [`RwData<Manager>`].
    pub fn controler(&self) -> &Controler<U> {
        &self.controler
    }

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop(&mut self) {
        let windows = self.controler.windows.read();

        std::thread::scope(|scope| {
            loop {
                let active_window = &windows[self.controler.active_window];

                if BREAK_LOOP.load(Ordering::Relaxed) {
                    BREAK_LOOP.store(false, Ordering::Relaxed);
                    break;
                }

                if let Ok(true) = event::poll(Duration::from_millis(5)) {
                    if let Event::Key(key) = event::read().unwrap() {
                        active_window.send_key(key, &self.controler, scope);
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
