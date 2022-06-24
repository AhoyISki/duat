use std::{
    io::{self, Stdout, stdout, Write},
    fmt::Display,
    path::PathBuf
};

use crossterm::{
    style::{PrintStyledContent, Print},
    QueueableCommand, ExecutableCommand,
    cursor::MoveTo, terminal,
    event::{read, Event, KeyEvent, KeyCode, KeyModifiers},
};

use parsec_core::{
    input::InputHandler,
    buffer::FileHandler,
    config::Options,
    output::{
        OutputArea,
        StyledChar,
        OutputPos,
    }
};

/// An area in the terminal used for printing text.
///
/// These should be linked to something that can print. They should never need to
/// tell a printing struct where an origin or end are, since they only need to know
/// how much space is in the area, and the area deals with placing the text in the
/// correct place.
pub struct TermArea {
    origin: OutputPos,
    end: OutputPos,

    stdout: Stdout,
}

impl TermArea {
    fn new(origin: OutputPos, end: OutputPos) -> Self {
        TermArea { origin, end, stdout: stdout() }
    }
}

/// The application itself, it contains all of the input handlers.
pub struct TerminalApp {
    // Dimensions of the whole terminal
    width: u16,
    height: u16,

    /// The list of handlers for input
    ///
    /// Whenever a key is pressed, it will be sent to every input handler.
    /// This key might be tied to an action, that contains a function, and may
    /// contain a name.
    /// - The function is of type fn(&mut Self), where Self is the type of the
    /// input handler. This function will be executed on the instance of the input
    /// handler.
    /// - There may be a name, if there is, the user will be able to execute the
    /// action from the command line, or map a key to the action directly, instead of
    /// mapping it to an already mapped key. Simple actions (moving, placing 
    /// characters, etc) are discouraged from having names.
    pub input_handlers: Vec<Box<dyn InputHandler>>,
}

impl OutputArea for TermArea {
    fn print_and_style(&mut self, ch: StyledChar) {
        self.stdout.queue(PrintStyledContent(ch.text)).unwrap();
    }

    fn print<T: Display>(&mut self, ch: T) {
        self.stdout.queue(Print(ch)).unwrap();
    }

    fn move_cursor(&mut self, pos: OutputPos) {
        self.stdout.queue(MoveTo(self.origin.x + pos.x, self.origin.y + pos.y))
                   .unwrap();
    }

    fn width(&self) -> u16 {
        self.end.x - self.origin.x
    }

    fn height(&self) -> u16 {
        self.end.y - self.origin.y
    }

    fn flush(&mut self) {
        self.stdout.flush().expect("flushing failed, somehow");
    }

    fn partition_x(&mut self, x: u16) -> Self {
        let end = OutputPos { x: self.origin.x + x - 1, y: self.end.y };
        let term_area = TermArea::new(self.origin, end);
        self.origin.x += x;
        term_area
    }

    fn partition_y(&mut self, y: u16) -> Self {
        let end = OutputPos { x: self.end.x, y: self.origin.y + y };
        let term_area = TermArea::new(self.origin, end);
        self.origin.y += y;
        term_area
    }
}

/// The terminal where the program will run.
impl TerminalApp {
    /// Returns a new instance of TermBuffer.
    pub fn new() -> TerminalApp {
        let (width, height) = terminal::size().expect("crossterm");
        TerminalApp {
            width,
            height,

            input_handlers: Vec::new(),
        }
    }

    // TODO: Deal with mouse and resize events.
    /// Processes keyboard, mouse, and resize events.
    pub fn process_events(&mut self) {
        // TODO: Add more event types
        // TODO: Quit in a way that makes more sense
        loop {
            match read().expect("crossterm") {
                Event::Key(key) => {
                    // NOTE: Remove this!!
                    if key == KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE) {
                        break
                    }
                    for input_handler in &mut self.input_handlers {
                        input_handler.handle_key(key);
                    }
                }
                _ => {},
            }
        }
    }
}

/// Preliminary functions for startup.
pub fn startup() {
    // This makes it so that if the application panics, the panic message is printed
    // nicely and the terminal is left in a usable state.
    use std::panic::set_hook;
    set_hook(Box::new(|msg| {
        let mut stdout = stdout();
        
        terminal::disable_raw_mode().unwrap();

        stdout.execute(terminal::EnableLineWrap).unwrap()
              .execute(terminal::Clear(terminal::ClearType::All)).unwrap()
              .execute(MoveTo(0, 0)).unwrap();

        println!("{}", msg);
    }));

    let mut stdout = stdout();

    terminal::enable_raw_mode().unwrap();

    stdout.execute(terminal::DisableLineWrap).unwrap()
          .execute(terminal::Clear(terminal::ClearType::All)).unwrap();
}

/// Quits the app and returns the terminal to usable state.
pub fn quit() {
    let mut stdout = stdout();

    stdout.execute(terminal::EnableLineWrap).unwrap()
          .execute(terminal::Clear(terminal::ClearType::All)).unwrap()
          .execute(MoveTo(0, 0)).unwrap();

    terminal::disable_raw_mode().unwrap();
}

/// The buffer containing the file handler.
pub struct FileBuffer {
    /// The current state of the file, with contents, cursors, and positioning.
    pub file_handler: FileHandler<TermArea>,
}

impl FileBuffer {
    /// Returns a new instance of an editing interface.
    ///
    /// That includes the file, the line numbers, and the status bar.
    pub fn new(
        origin: OutputPos, end: OutputPos, file_path: PathBuf,
        options: &Options) -> io::Result<FileBuffer> {
        // The end  must be after the origin.
        // TODO: Move this to a place where it is required.
        assert!(end > origin);

        Ok(FileBuffer {
            file_handler: {
                let area = TermArea::new(origin, end);

                FileHandler::new(area, file_path, options.file_options.clone())
            },
        })
    }
}
