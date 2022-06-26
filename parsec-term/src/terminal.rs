use std::{
    io::{Stdout, stdout, Write},
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
    buffer::Buffer,
    config::Options,
    output::{
        OutputArea,
        StyledChar,
        OutputPos,
    }
};

/// An area in the terminal used for printing text.
///
/// These should be linked to something that can print. They should never need to tell a printing
/// struct where the origin or end are, since they only need to know how much space is in the area,
/// and the area deals with placing the text in the correct place.
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

impl OutputArea for TermArea {
    fn print_and_style(&mut self, ch: StyledChar) {
        self.stdout.queue(PrintStyledContent(ch.ch)).unwrap();
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

// TODO: Deal with mouse and resize events.
/// Processes keyboard, mouse, and resize events.
pub fn process_events(input_handlers: &mut Vec<Box<dyn InputHandler>>) {
    // TODO: Add more event types
    // TODO: Quit in a way that makes more sense
    loop {
        match read().expect("crossterm") {
            Event::Key(key) => {
                // NOTE: Remove this!!
                if key == KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE) {
                    break
                }
                for input_handler in &mut *input_handlers {
                    input_handler.handle_key(key);
                }
            }
            _ => {},
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

pub fn new_buffer(
    origin: OutputPos, end: OutputPos, file_path: PathBuf, options: &Options) -> Buffer<TermArea> {
    let area = TermArea::new(origin, end);

    Buffer::new(area, file_path, options.file_options.clone())
}
