use std::{path::PathBuf, sync::atomic::Ordering, thread, time::Duration};

use crossterm::event::{self, Event, KeyEvent};

use crate::{
    commands::Commands,
    data::{ReadableData, RoData, RwData},
    input::{KeyRemapper, Scheme},
    tags::form::FormPalette,
    text::PrintCfg,
    ui::{activate_hook, ModNode, ParsecWindow, PushSpecs, Ui},
    widgets::{FileWidget, SchemeInputWidget, WidgetType},
    Controler, BREAK_LOOP, SHOULD_QUIT
};

pub struct Session<U>
where
    U: Ui
{
    ui: U,
    pub constructor_hook: ConstructorHook<U>,
    controler: Controler<U>,
    print_cfg: RwData<PrintCfg>
}

impl<U> Session<U>
where
    U: Ui + 'static
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new(
        mut ui: U, print_cfg: PrintCfg, palette: FormPalette,
        mut constructor_hook: impl FnMut(ModNode<U>, RoData<FileWidget<U>>) + 'static
    ) -> Self {
        let file = std::env::args().nth(1).as_ref().map(PathBuf::from);
        let file = FileWidget::<U>::new(file, print_cfg.clone());

        let (window, area) = ParsecWindow::new(&mut ui, file, || false);
        let mut controler = Controler::new(window, palette);
        controler.commands.write().file_id = Some(0);
        activate_hook(&mut controler, area, &mut constructor_hook);
        controler.commands.write().file_id = None;

        let mut session = Session {
            ui,
            constructor_hook: Box::new(constructor_hook),
            controler,
            print_cfg: RwData::new(print_cfg)
        };

        session.open_arg_files();

        session
    }

    fn open_arg_files(&mut self) {
        for file in std::env::args().skip(2) {
            self.open_file(PathBuf::from(file))
        }
    }

    pub fn open_file(&mut self, path: PathBuf) {
        let file_widget = FileWidget::new(Some(path), self.print_cfg.read().clone());
        let (area, _) = self.controler.windows.mutate(|windows| {
            let active_window = &mut windows[self.controler.active_window];
            active_window.push_file(file_widget, PushSpecs::right_free())
        });

        activate_hook(&mut self.controler, area, &mut self.constructor_hook);
    }

    pub fn push<F>(
        &mut self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let (widget_type, checker, _) = f(&self.controler);
        self.controler
            .mutate_active_window(|window| window.push_to_master(widget_type, checker, specs))
    }

    pub fn push_specd<F>(
        &mut self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs)
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let (widget_type, checker, specs) = f(&self.controler);
        self.controler
            .mutate_active_window(|window| window.push_to_master(widget_type, checker, specs))
    }

    pub fn push_to<F>(
        &mut self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), area: &U::Area,
        specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let (widget_type, checker, _) = f(&self.controler);
        self.controler.mutate_active_window(|window| {
            window.push(widget_type, area, checker, specs, None, false)
        })
    }

    pub fn push_specd_to<F>(
        &mut self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), area: &U::Area
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let (widget_type, checker, specs) = f(&self.controler);
        self.controler.mutate_active_window(|window| {
            window.push(widget_type, area, checker, specs, None, false)
        })
    }

    pub fn cluster_to<F>(
        &mut self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), area: &U::Area,
        specs: PushSpecs
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let (widget_type, checker, _) = f(&self.controler);
        self.controler.mutate_active_window(|window| {
            window.push(widget_type, area, checker, specs, None, true)
        })
    }

    pub fn cluster_specd_to<F>(
        &mut self, f: impl FnOnce(&Controler<U>) -> (WidgetType<U>, F, PushSpecs), area: &U::Area
    ) -> (U::Area, Option<U::Area>)
    where
        F: Fn() -> bool + 'static
    {
        let (widget_type, checker, specs) = f(&self.controler);
        self.controler.mutate_active_window(|window| {
            window.push(widget_type, area, checker, specs, None, true)
        })
    }

    /// Start the application, initiating a read/response loop.
    pub fn start<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: Scheme
    {
        self.ui.startup();

        // The main loop.
        loop {
            let palette = &self.controler.palette;
            for (widget_type, area, _) in
                self.controler.windows.read()[self.controler.active_window].widgets()
            {
                widget_type.update(area);
                widget_type.print(area, palette)
            }

            self.session_loop(key_remapper);

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

    /// The primary application loop, executed while no breaking
    /// commands have been sent to [`Controls`].
    fn session_loop<I>(&mut self, key_remapper: &mut KeyRemapper<I>)
    where
        I: Scheme
    {
        let palette = &self.controler.palette;
        let controler = &self.controler;
        let windows = self.controler.windows.read();
        thread::scope(|scope| {
            loop {
                let active_window = &windows[self.controler.active_window];

                if BREAK_LOOP.load(Ordering::Relaxed) {
                    BREAK_LOOP.store(false, Ordering::Relaxed);
                    break;
                }

                for node in active_window.nodes() {
                    if node.needs_update() {
                        let palette = &palette;
                        node.try_update_and_print(scope, palette);
                    }
                }

                if let Ok(true) = event::poll(Duration::from_millis(10)) {
                    send_event(key_remapper, controler);
                } else {
                    continue;
                }
            }
        });
    }

    /// Returns the [`RwData<Manager>`].
    pub fn controler(&self) -> &Controler<U> {
        &self.controler
    }

    pub fn commands(&self) -> &RwData<Commands> {
        &self.controler.commands
    }
}

/// Sends an event to the `Widget` determined by `SessionControl`.
fn send_event<U, I>(key_remapper: &mut KeyRemapper<I>, controler: &Controler<U>)
where
    U: Ui + 'static,
    I: Scheme
{
    if let Event::Key(key_event) = event::read().unwrap() {
        controler.inspect_active_window(|window| {
            if let Some((widget_type, area, _)) = window.widgets().find(|(widget_type, ..)| {
                widget_type.scheme_ptr_eq(&*controler.active_widget.read().unwrap())
            }) {
                let widget = widget_type.as_scheme_input().unwrap();
                blink_cursors_and_send_key(widget, area, controler, key_event, key_remapper);
            }
        })
    }
}

pub type ConstructorHook<U> = Box<dyn FnMut(ModNode<U>, RoData<FileWidget<U>>)>;

/// Removes the cursors, sends an event, and adds them again.
fn blink_cursors_and_send_key<U, Sw, I>(
    widget: &RwData<Sw>, area: &U::Area, controler: &Controler<U>, key_event: KeyEvent,
    key_remapper: &mut KeyRemapper<I>
) where
    U: Ui + 'static,
    Sw: SchemeInputWidget<U> + ?Sized + 'static,
    I: Scheme
{
    widget.mutate(|widget| {
        let (text, cursors, _) = widget.members_for_cursor_tags();
        text.remove_cursor_tags(cursors);
    });

    key_remapper.send_key(key_event, widget, area, controler);

    widget.mutate(|widget| {
        let (text, cursors, main_index) = widget.members_for_cursor_tags();
        text.add_cursor_tags(cursors, main_index);
    });
}
