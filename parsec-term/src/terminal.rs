use std::io::{self, Stdout, stdout, Write};
use std::path::PathBuf;

use crossterm::{
    style::{PrintStyledContent, Print},
    QueueableCommand, ExecutableCommand,
    cursor::MoveTo, terminal,
    event::{read, Event, KeyEvent, KeyCode, KeyModifiers},
};

use parsec_core::output::{
    OutputArea, OutputPos,
    WindowBuffer,
    StyledChar,
    InputHandler,
};

use parsec_core::{
    FileHandler, Options,
};

pub struct TermArea {
    origin: OutputPos,
    end: OutputPos,

    stdout: Stdout,
}

/// A buffer, which contains multiple areas.
pub struct TermBuffer {
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

/// An area in the terminal.
///
/// Examples include: The file buffer, status line, etc.
impl OutputArea for TermArea {
    fn new(origin: OutputPos, end: OutputPos) -> Self {
        TermArea { origin, end, stdout: stdout() }
    }

    fn print_styled(&mut self, ch: StyledChar) {
        self.stdout.queue(PrintStyledContent(ch.text)).unwrap();
    }

    fn print_string(&mut self, ch: String) {
        self.stdout.queue(Print(ch)).unwrap();
    }

    fn move_cursor(&mut self, pos: OutputPos) {
        self.stdout.queue(MoveTo(self.origin.x + pos.x, self.origin.y + pos.y))
                   .unwrap();
    }

    fn move_cursor_to_origin(&mut self) {
        self.stdout.queue(MoveTo(self.origin.x, self.origin.y)).unwrap();
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
}

impl WindowBuffer for TermBuffer {
    type Window = TermBuffer;

    fn new() -> Self::Window {
        let (width, height) = terminal::size().expect("crossterm");
        TermBuffer {
            width,
            height,

            input_handlers: Vec::new(),
        }
    }

    /// Returns the dimensions of the terminal.
    fn dims(&self) -> (u16, u16) {
        (self.width, self.height)
    }

    fn process_events(&mut self) {
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
    // Making it so that if the application panics, the panic message is printed
    // nicely and the terminal is in a somewhat usable state.
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

/// Returning the terminal to usable state.
pub fn quit() {
    let mut stdout = stdout();

    stdout.execute(terminal::EnableLineWrap).unwrap()
          .execute(terminal::Clear(terminal::ClearType::All)).unwrap()
          .execute(MoveTo(0, 0)).unwrap();

    terminal::disable_raw_mode().unwrap();
}

pub struct FileBuffer {
    /// The current state of the file, with contents, cursors, and positioning
    pub file_handler: FileHandler<TermArea>,
}

impl FileBuffer {
    pub fn new(
        origin: OutputPos, end: OutputPos, file_path: PathBuf,
        options: Options) -> io::Result<FileBuffer> {
        // The end  must be after the origin.
        // TODO: Move this to a place where it is required.
        assert!(end > origin);

        // The amount of cells the line numbers should take horizontally.
        let mut file_buffer = FileBuffer {
            file_handler: {
                let area = TermArea::new(origin, end);

                let mut file_handler = FileHandler::new(
                    area, file_path, options.file_options);
                // TODO: Add more parameters to this function.
                // TODO: This should not panic!
                let mut limit = 10;
                let mut i = 3;
                let line_num_width = loop {
                    if file_handler.file.len() > limit {
                        limit *= 10;
                        i += 1;
                    } else {
                        break i;
                    }
                };

                // Related to status bar
                // TODO: Allow 3 status bar types: vim, kakoune, dynamic.
                file_handler.area.end.y -= 2;

                file_handler.area.origin.x += line_num_width;

                file_handler
            },
        };

        file_buffer.file_handler.parse_wrapping();
        file_buffer.file_handler.print_contents();

        Ok(file_buffer)
    }
}
