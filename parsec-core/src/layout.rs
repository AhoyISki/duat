pub mod file_widget;
pub mod status_widget;

use std::{fmt::Write, path::PathBuf, sync::Mutex, thread, env};

use crossterm::event::{self, Event, KeyCode};

use crate::{
    config::{Config, LineNumbers, RoData, RwData},
    cursor::TextCursor,
    file::Text,
    input::{EditingScheme, FileRemapper},
    tags::MatchManager,
    ui::{Direction, EndNode, MidNode, NodeManager, Split, Ui},
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
    fn end_node(&self) -> &EndNode<U>;

    fn end_node_mut(&mut self) -> &mut EndNode<U>;

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
    node: EndNode<U>,
    printed_lines: PrintedLines,
    main_cursor: RoData<usize>,
    cursors: RoData<Vec<TextCursor>>,
    text: RwData<Text>,
}

unsafe impl<U> Send for LineNumbersWidget<U> where U: Ui {}

impl<U> LineNumbersWidget<U>
where
    U: Ui,
{
    /// Returns a new instance of `LineNumbersWidget`.
    fn new(
        file_widget: &mut FileWidget<U>, node_manager: &mut NodeManager<U>,
    ) -> (Self, MidNode<U>) {
        let mut split = 2;
        let mut num_exp = 10;
        let text = file_widget.text.write();

        while text.lines().len() > num_exp {
            num_exp *= 10;
            split += 1;
        }
        drop(text);

        let node = &mut file_widget.node;
        let (parent_node, child_node) =
            node_manager.split_end(node, Direction::Left, Split::Static(split), true);
        let printed_lines = file_widget.printed_lines();
        let main_cursor = RoData::from(&file_widget.main_cursor);
        let cursors = RoData::from(&file_widget.cursors);

        let mut line_numbers = LineNumbersWidget {
            node: child_node,
            printed_lines,
            main_cursor,
            cursors,
            text: RwData::new(Text::default()),
        };

        line_numbers.update();

        (line_numbers, parent_node)
    }
}

impl<U> Widget<U> for LineNumbersWidget<U>
where
    U: Ui,
{
    fn update(&mut self) {
        let lines = self.printed_lines.lines(&self.end_node());
        let main_line = self.cursors.read().get(*self.main_cursor.read()).unwrap().caret().row;

        // 3 is probably the average length of the numbers, in digits, plus 1 for each "\n".
        let mut line_numbers = String::with_capacity(4 * lines.len());

        match self.node.config().line_numbers {
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

    fn end_node(&self) -> &EndNode<U> {
        &self.node
    }

    fn end_node_mut(&mut self) -> &mut EndNode<U> {
        &mut self.node
    }
}

pub struct OneStatusLayout<U>
where
    U: Ui,
{
    node_manager: NodeManager<U>,
    pub status: StatusWidget<U>,
    widgets: Vec<Mutex<Box<dyn Widget<U>>>>,
    files: Vec<(RwData<FileWidget<U>>, Option<MidNode<U>>)>,
    master_node: MidNode<U>,
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
        C: Fn(EndNode<U>, &mut NodeManager<U>) -> P;

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
    pub fn new(ui: U, match_manager: MatchManager, config: Config) -> Self {
        let paths: Vec<PathBuf> = env::args().map(|p| PathBuf::from(p)).collect();
        let mut node_manager = NodeManager::new(ui);
        let mut node = node_manager.only_child(&config, "code").unwrap();

        let (master_node, end_node) =
            node_manager.split_end(&mut node, Direction::Bottom, Split::Static(1), false);

        let status = StatusWidget::new(end_node, &mut node_manager);

        let mut layout = OneStatusLayout {
            node_manager,
            status,
            widgets: Vec::new(),
            files: Vec::new(),
            master_node,
            match_manager,
        };

        layout.new_file_with_node(&paths[1], node);
        layout.status.set_file(layout.active_file());

        layout
    }

    /// Creates or opens a new file in a given node.
    fn new_file_with_node(&mut self, path: &PathBuf, node: EndNode<U>) {
        let mut file =
            FileWidget::<U>::new(path, node.clone(), &Some(self.match_manager.clone()));

        if matches!(node.config().line_numbers, LineNumbers::None) {
            self.files.push((RwData::new(file), None));
        } else {
            let (line_numbers, file_parent) =
                LineNumbersWidget::new(&mut file, &mut self.node_manager);

            self.widgets.push(Mutex::new(Box::new(line_numbers)));

            self.files.push((RwData::new(file), Some(file_parent)));
        }
    }

    fn active_file_mut(&mut self) -> RwData<FileWidget<U>> {
        self.files[0].0.clone()
    }

    fn active_file(&self) -> RoData<FileWidget<U>> {
        RoData::from(&self.files[0].0)
    }
}

type FilePrinter<U> = (RwData<FileWidget<U>>, Option<MidNode<U>>);

impl<U> Layout<U> for OneStatusLayout<U>
where
    U: Ui + 'static,
{
    fn open_file(&mut self, path: &PathBuf) {
        let (master_node, child_node) = self.node_manager.split_mid(
            &mut self.master_node,
            Direction::Right,
            Split::Ratio(0.5),
            false,
        );

        self.new_file_with_node(path, child_node);
        self.master_node = master_node;
    }

    fn push_node_to_edge<P, C>(&mut self, constructor: C, direction: Direction, split: Split)
    where
        P: Widget<U> + 'static,
        C: Fn(EndNode<U>, &mut NodeManager<U>) -> P,
    {
        let (master_node, end_node) =
            self.node_manager.split_mid(&mut self.master_node, direction, split, false);

        self.master_node = master_node;

        let widget = constructor(end_node.clone(), &mut self.node_manager);

        self.widgets.push(Mutex::new(Box::new(widget)));
    }

    fn application_loop(&mut self, key_remapper: &mut FileRemapper<impl EditingScheme>) {
        self.node_manager.startup();

        // This mutex is only used to prevent multiple printings at the same time.
        let printer = Mutex::new(true);

        // Initial printing.
        self.status.update();
		print_widget(&mut self.status);
        print_files(&mut self.files);
        for widget in &mut self.widgets {
            let mut widget = widget.lock().unwrap();
            widget.update();
            print_widget(Box::as_mut(&mut widget));
        }

        // The main loop.
        thread::scope(|s_0| {
            loop {
                // TODO: Make this generalized.
                if let Event::Key(key_event) = event::read().unwrap() {
                    // NOTE: This is very much temporary.
                    if let KeyCode::Esc = key_event.code {
                        break;
                    } else {
                        key_remapper.send_key_to_file(key_event, &mut self.files[0].0.write());
                    }
                }

                update_files(&mut self.files);
                self.status.update();
                let printer_lock = printer.lock().unwrap();
                print_widget(&mut self.status);
                print_files(&mut self.files);
                drop(printer_lock);

                let widget_indices = widgets_to_update(&self.widgets);
                for index in &widget_indices {
                    let widget = &self.widgets[*index];
                    s_0.spawn(|| {
                        let mut widget = widget.lock().unwrap();
                        widget.update();
                        let _printer_lock = printer.lock().unwrap();
                        print_widget(Box::as_mut(&mut widget));
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

/// Updates all files in different threads.
fn update_files<U>(files: &mut Vec<FilePrinter<U>>)
where
    U: Ui,
{
    thread::scope(|s_1| {
        for file in files {
            s_1.spawn(|| file.0.write().update());
        }
    });
}

/// Prints all the files.
fn print_files<U>(printer: &mut Vec<FilePrinter<U>>)
where
    U: Ui,
{
    for (file_widget, _) in printer.iter_mut() {
        let mut file_widget = file_widget.write();
        let print_info = file_widget.print_info().map(|p| *p.read()).unwrap_or_default();
        file_widget.text().read().print(file_widget.end_node_mut(), print_info);
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
    widget.text().read().print(widget.end_node_mut(), print_info);
}
