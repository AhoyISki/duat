pub mod file_widget;
pub mod status_widget;

use std::{
    f64::INFINITY,
    fmt::Write,
    path::PathBuf,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use crossterm::event::{self, Event, KeyCode};

use crate::{
    config::{Config, LineNumbers, RoData, RwData},
    cursor::TextCursor,
    file::Text,
    input::{EditingScheme, FileRemapper},
    log_info,
    tags::{FormPalette, MatchManager},
    ui::{Area, Direction, EndNode, Label, MidNode, NodeManager, Split, Ui}, FOR_TEST,
};

use self::{
    file_widget::{FileWidget, PrintInfo, PrintedLines},
    status_widget::StatusWidget,
};

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait Widget<U>: Send
where
    U: Ui,
{
    /// Returns the `ChildNode` associated with this area.
    fn end_node(&self) -> &RwData<EndNode<U>>;

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>>;

    fn update(&mut self);

    fn needs_update(&self) -> bool;

    fn text(&self) -> RoData<Text>;

    /// Returns the printing information of the file.
    fn print_info(&self) -> Option<RoData<PrintInfo>> {
        None
    }

    /// Scrolls the text vertically by an amount.
    fn scroll_vertically(&mut self, d_y: i32) {}

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, node: &EndNode<U>) {}
}

/// An area where you can edit the text.
pub trait EditArea<M>: Widget<M>
where
    M: Ui,
{
    /// Returns a mutable reference to the text.
    fn mut_text(&mut self) -> &mut Text;

    /// Gets the cursors on the area.
    ///
    /// # Returns
    ///
    /// * A list of cursors. This includes cursors that shouldn't be printed on screen.
    /// * The index of the main cursor. Most of the time, this will be 0.
    fn cursors(&mut self) -> (&mut Vec<TextCursor>, usize);
}

pub struct LineNumbersWidget<U>
where
    U: Ui,
{
    node: RwData<EndNode<U>>,
    printed_lines: PrintedLines<U>,
    main_cursor: RoData<usize>,
    cursors: RoData<Vec<TextCursor>>,
    text: RwData<Text>,
}

unsafe impl<U> Send for LineNumbersWidget<U> where U: Ui {}

impl<U> LineNumbersWidget<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `LineNumbersWidget`.
    pub fn new(
        node: RwData<EndNode<U>>, _: &mut NodeManager<U>, file_widget: RwData<FileWidget<U>>,
    ) -> Box<dyn Widget<U>> {
        let file_widget = file_widget.read();

        let printed_lines = file_widget.printed_lines();
        let main_cursor = RoData::from(&file_widget.main_cursor);
        let cursors = RoData::from(&file_widget.cursors);

        let mut line_numbers = LineNumbersWidget {
            node,
            printed_lines,
            main_cursor,
            cursors,
            text: RwData::new(Text::default()),
        };

        let width = line_numbers.calculate_width();
        if width != line_numbers.node.read().label.read().area().width() {
            line_numbers.node.write().request_width(width);
        }

        line_numbers.update();

        Box::new(line_numbers)
    }

    fn calculate_width(&self) -> usize {
        let mut width = 1;
        let mut num_exp = 10;
        let len = self.printed_lines.text().read().lines().len();

        while len > num_exp {
            num_exp *= 10;
            width += 1;
        }
        width
    }
}

impl<U> Widget<U> for LineNumbersWidget<U>
where
    U: Ui + 'static,
{
    fn update(&mut self) {
        if unsafe { FOR_TEST == 3 } {
            unsafe { FOR_TEST += 1 };
            thread::sleep(Duration::from_millis(1000));
        } else if unsafe { FOR_TEST < 3 } {
            unsafe { FOR_TEST += 1 };
        } else {
            unsafe { FOR_TEST -= 1 };
        }
        let width = self.calculate_width();
        self.node.write().request_width(width);

        let lines = self.printed_lines.lines();
        let main_line = self.cursors.read().get(*self.main_cursor.read()).unwrap().caret().row;
        let node = self.node.read();
        let config = node.config().read();

        // 3 is probably the average length of the numbers, in digits, plus 1 for each "\n".
        let mut line_numbers = String::with_capacity(width * lines.len());

        match config.line_numbers {
            LineNumbers::Absolute => {
                lines.iter().for_each(|&n| write!(&mut line_numbers, "{}\n", n).unwrap());
            }
            LineNumbers::Relative => {
                lines.iter().for_each(|&n| {
                    write!(&mut line_numbers, "{}\n", usize::abs_diff(n, main_line)).unwrap()
                });
            }
            LineNumbers::Hybrid => {
                lines.iter().for_each(|&n| {
                    write!(
                        &mut line_numbers,
                        "{}\n",
                        if n != main_line { usize::abs_diff(n, main_line) } else { n }
                    )
                    .unwrap()
                });
            }
            LineNumbers::None => panic!("How the hell did you get here?"),
        }

        let mut text = self.text.write();
        *text = Text::new(line_numbers, None);
    }

    fn needs_update(&self) -> bool {
        self.printed_lines.has_changed()
    }

    fn text(&self) -> RoData<Text> {
        RoData::from(&self.text)
    }

    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.node
    }
}

pub type WidgetFormer<U> =
    dyn Fn(RwData<EndNode<U>>, &mut NodeManager<U>, RwData<FileWidget<U>>) -> Box<dyn Widget<U>>;

pub struct OneStatusLayout<U>
where
    U: Ui,
{
    node_manager: NodeManager<U>,
    pub status: StatusWidget<U>,
    widgets: Vec<Mutex<Box<dyn Widget<U>>>>,
    files: Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>)>,
    future_file_widgets: Vec<(Box<WidgetFormer<U>>, Direction, Split)>,
    master_node: RwData<MidNode<U>>,
    empty_file: Option<RwData<EndNode<U>>>,
    match_manager: MatchManager,
}

/// A form of organizing the areas on a window.
pub trait Layout<U>
where
    U: Ui,
{
    /// Opens a new file in a new `FileWidget`.
    fn open_file(&mut self, path: &PathBuf);

    /// Pushes a node to an edge of the screen, with all the files in the center.
    fn push_node_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<U> + 'static,
        C: Fn(RwData<EndNode<U>>, &mut NodeManager<U>) -> P;

    /// Pushes a node to the edge of every future `FileWidget`.
    fn push_node_to_file(
        &mut self, constructor: Box<WidgetFormer<U>>, direction: Direction, split: Split,
    );

    /// The main application function.
    fn application_loop(&mut self, key_remapper: &mut FileRemapper<impl EditingScheme>);

    /// Returns a list of files, valid for this moment.
    fn files(&self) -> Vec<RoData<FileWidget<U>>>;
}

impl<U> OneStatusLayout<U>
where
    U: Ui + 'static,
{
    /// Returns a new instance of `OneStatusLayout`.
    pub fn new(
        ui: U, match_manager: MatchManager, config: Config, palette: FormPalette,
        direction: Direction, split: Split,
    ) -> Self {
        let mut node_manager = NodeManager::new(ui);
        let mut node = node_manager.only_child(config, palette, "code").unwrap();

        let (master_node, end_node) = node_manager.split_end(&mut node, direction, split, false);

        let status = StatusWidget::new(end_node, &mut node_manager);

        let layout = OneStatusLayout {
            node_manager,
            status,
            widgets: Vec::new(),
            files: Vec::new(),
            future_file_widgets: Vec::new(),
            master_node,
            empty_file: Some(node),
            match_manager,
        };

        layout
    }

    /// Creates or opens a new file in a given node.
    fn new_file_with_node(&mut self, path: &PathBuf, mut node: RwData<EndNode<U>>) {
        log_info!("\n\nnew file!\n");
        let file = FileWidget::<U>::new(path, node.clone(), &Some(self.match_manager.clone()));
        let (file, mut file_parent) = (RwData::new(file), None);

        for (constructor, direction, split) in &self.future_file_widgets {
            let (mid_node, end_node) = match &mut file_parent {
                None => self.node_manager.split_end(&mut node, *direction, *split, false),
                Some(parent) => self.node_manager.split_mid(parent, *direction, *split, false),
            };

            let widget = constructor(end_node, &mut self.node_manager, file.clone());
            self.widgets.push(Mutex::new(widget));
            file_parent = Some(mid_node);
        }

        self.files.push((file, file_parent));
    }

    fn active_file_mut(&mut self) -> RwData<FileWidget<U>> {
        self.files[0].0.clone()
    }

    pub fn active_file(&self) -> RoData<FileWidget<U>> {
        RoData::from(&self.files[0].0)
    }
}

impl<U> Layout<U> for OneStatusLayout<U>
where
    U: Ui + 'static,
{
    fn open_file(&mut self, path: &PathBuf) {
        if let Some(node) = self.empty_file.take() {
            self.new_file_with_node(path, node);
        } else {
            let (master_node, end_node) = self.node_manager.split_mid(
                &mut self.master_node,
                Direction::Right,
                Split::Ratio(0.5),
                false,
            );
            self.new_file_with_node(path, end_node);
            self.master_node = master_node;
        };

        self.status.set_file(RoData::from(&self.files.last().unwrap().0));
    }

    fn push_node_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<U> + 'static,
        C: Fn(RwData<EndNode<U>>, &mut NodeManager<U>) -> P,
    {
        let (master_node, end_node) =
            self.node_manager.split_mid(&mut self.master_node, direction, split, false);

        self.master_node = master_node;

        let widget = constructor(end_node.clone(), &mut self.node_manager);

        self.widgets.push(Mutex::new(Box::new(widget)));
    }

    fn push_node_to_file(
        &mut self, constructor: Box<WidgetFormer<U>>, direction: Direction, split: Split,
    ) {
        self.future_file_widgets.push((Box::new(constructor), direction, split));
    }

    fn application_loop(&mut self, key_remapper: &mut FileRemapper<impl EditingScheme>) {
        self.node_manager.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let printer = Mutex::new(true);

        // Initial printing.
        self.status.update();
        print_widget(&mut self.status);
        print_files(&mut self.files);
        for widget in &self.widgets {
            let mut widget = widget.lock().unwrap();
            widget.update();
            print_widget(Box::as_mut(&mut widget));
        }

        let resize_requested = Mutex::new(true);
        let mut iteration = 0;

        // The main loop.
        thread::scope(|s_0| {
            loop {
                if let Ok(mut resize_requested) = resize_requested.try_lock() {
                    if *resize_requested {
                        *resize_requested = false;
                        drop(resize_requested);
                        for widget in self.widgets.iter() {
                            if let Ok(mut widget) = widget.try_lock() {
                                widget.end_node_mut().write().try_set_size();
                            }
                        }
                        let _printer_lock = printer.lock().unwrap();
                        for widget in &self.widgets {
                            if let Ok(mut widget) = widget.try_lock() {
                                print_widget(Box::as_mut(&mut widget));
                            }
                        }
                        print_files(&mut self.files);
                    }
                }

                if let Ok(true) = event::poll(Duration::from_micros(100)) {
                    if let Event::Key(key_event) = event::read().unwrap() {
                        // NOTE: Temporary.
                        if let KeyCode::Esc = key_event.code {
                            break;
                        } else {
                            key_remapper
                                .send_key_to_file(key_event, &mut self.files[0].0.write());
                        }
                    }
                } else {
                    continue;
                }

                self.status.update();
                let printer_lock = printer.lock().unwrap();
                print_widget(&mut self.status);
                print_files(&mut self.files);
                drop(printer_lock);
                iteration = 0;

                let widget_indices = widgets_to_update(&self.widgets);
                for index in &widget_indices {
                    let widget = &self.widgets[*index];
                    s_0.spawn(|| {
                        let mut widget = widget.lock().unwrap();
                        widget.update();
                        let widget_resized = widget.end_node().read().resize_requested();
                        let mut resize_requested = resize_requested.lock().unwrap();
                        *resize_requested |= widget_resized;
                        if !widget_resized {
                            drop(resize_requested);
                            let _printer_lock = printer.lock().unwrap();
                            print_widget(Box::as_mut(&mut widget));
                        }
                    });
                }
            }
        });

        self.node_manager.shutdown();
    }

    fn files(&self) -> Vec<RoData<FileWidget<U>>> {
        self.files.iter().map(|(f, ..)| RoData::from(f)).collect()
    }
}

/// Prints all the files.
fn print_files<U>(printer: &mut Vec<(RwData<FileWidget<U>>, Option<RwData<MidNode<U>>>)>)
where
    U: Ui,
{
    for (file_widget, _) in printer.iter_mut() {
        let mut file_widget = file_widget.write();
        file_widget.update();
        let print_info = file_widget.print_info().map(|p| *p.read()).unwrap_or_default();
        file_widget.text().read().print(&mut file_widget.end_node_mut().write(), print_info);
    }
}

/// List of widgets that need to be updated.
fn widgets_to_update<U>(widgets: &Vec<Mutex<Box<dyn Widget<U>>>>) -> Vec<usize>
where
    U: Ui,
{
    let mut indices = Vec::new();

    for (index, widget) in widgets.iter().enumerate() {
        // If the lock is unavailable, that means the widget is being updated.
        if let Ok(lock) = widget.try_lock() {
            if lock.needs_update() {
                indices.push(index);
            }
        }
    }

    indices
}

fn print_widget<W, U>(widget: &mut W)
where
    W: Widget<U> + ?Sized,
    U: Ui,
{
    let print_info = widget.print_info().map(|p| *p.read()).unwrap_or_default();
    widget.text().read().print(&mut widget.end_node_mut().write(), print_info);
}
