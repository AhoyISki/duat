use std::{
    fmt::Display,
    io::{stdout, Stdout, Write},
    path::PathBuf,
};

use crossterm::{
    cursor::{Hide, MoveTo, RestorePosition, SavePosition, Show},
    event::{read, Event, KeyCode, KeyEvent, KeyModifiers},
    style::{Attribute, Attributes, Color, ContentStyle, Print, ResetColor, SetStyle},
    terminal, ExecutableCommand, QueueableCommand,
};
use parsec_core::{
    buffer::Buffer,
    config::Options,
    input::InputHandler,
    layout::{OutputArea, OutputPos},
    tags::{CharTag, Form},
};

#[derive(Clone, Copy)]
enum FormType {
    Normal,
    MultiLine,
}

/// An area in the terminal used for printing text.
///
/// These should be linked to something that can print. They should never need to tell a printing
/// struct where the origin or end are, since they only need to know how much space is in the area,
/// and the area deals with placing the text in the correct place.
pub struct TermArea {
    origin: OutputPos,
    end: OutputPos,

    stdout: Stdout,

    form_stack: Vec<(Form, u16, FormType)>,
}

impl TermArea {
    fn new(origin: OutputPos, end: OutputPos) -> Self {
        TermArea { origin, end, stdout: stdout(), form_stack: Vec::new() }
    }

    fn print_form_stack(&mut self) {
        let mut final_style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::from(Attribute::Reset),
        };
        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final, .. }, _, _) in &self.form_stack {
            if let Some(color) = style.foreground_color {
                if !fg_done {
                    final_style.foreground_color = Some(color);
                    if is_final { fg_done = true }
                }
            }
            if let Some(color) = style.background_color {
                if !bg_done {
                    final_style.background_color = Some(color);
                    if is_final { bg_done = true }
                }
            }
            if let Some(color) = style.foreground_color {
                if !ul_done {
                    final_style.underline_color = Some(color);
                    if is_final { ul_done = true }
                }
            }
            if !attr_done && !final_style.attributes.is_empty() {
                final_style.attributes = style.attributes;
                if is_final { attr_done = true }
            }

            if fg_done && bg_done && ul_done && attr_done {
                break;
            }
        }

        self.stdout.queue(SetStyle(final_style)).unwrap();
    }
}

impl OutputArea for TermArea {
    fn can_place_secondary_cursor(&self) -> bool {
        false
    }

    fn place_cursor(&mut self, tag: CharTagType) {
        match tag {
            CharTagType::PrimaryCursor => {
                // I have no idea why I have to do this, but if I don't, forms act weird.
                self.stdout.queue(ResetColor).unwrap().queue(SavePosition).unwrap();
                self.print_form_stack();
            }
            CharTagType::SecondaryCursor => {
                panic!("Secondary cursors not allowed on the terminal!")
            }
            _ => panic!("Other character tags are not supposed to be handled directly!"),
        };
    }

    fn push_form(&mut self, form: &Form, index: u16) {
        self.form_stack.push((*form, index, FormType::Normal));

        self.print_form_stack();
    }

    fn pop_form(&mut self, index: u16) {
        if let Some(element) = self
            .form_stack
            .iter()
            .enumerate()
            .rfind(|(_, &(_, i, f))| i == index && matches!(f, FormType::Normal))
        {
            self.form_stack.remove(element.0);

            self.print_form_stack();
        }
    }

    fn push_ml_form(&mut self, form: &Form, index: u16) {
        self.form_stack.push((*form, index, FormType::MultiLine));

        self.print_form_stack();
    }

    fn pop_ml_form(&mut self, index: u16) {
        if let Some(element) = self
            .form_stack
            .iter()
            .enumerate()
            .rfind(|(_, &(_, i, f))| i == index && matches!(f, FormType::MultiLine))
        {
            self.form_stack.remove(element.0);

            self.print_form_stack();
        }
    }

    fn clear_normal_forms(&mut self) {
        self.form_stack.retain(|(_, _, f)| matches!(f, FormType::MultiLine));

        self.print_form_stack();
    }

    fn clear_all_forms(&mut self) {
        self.form_stack.clear();

        self.print_form_stack();
    }

    fn start_print(&mut self) {
        self.stdout.execute(Hide).unwrap();
    }

    fn print<T: Display>(&mut self, ch: T) {
        self.stdout.queue(Print(ch)).unwrap();
    }

    fn finish_print(&mut self) {
        self.stdout.queue(RestorePosition).unwrap();
        self.stdout.queue(Show).unwrap();
        self.stdout.flush().unwrap();
    }

    fn move_cursor(&mut self, pos: OutputPos) {
        self.stdout.queue(MoveTo(self.origin.x + pos.x, self.origin.y + pos.y)).unwrap();
    }

    fn width(&self) -> usize {
        (self.end.x - self.origin.x) as usize
    }

    fn height(&self) -> usize {
        (self.end.y - self.origin.y) as usize
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
                    break;
                }
                for input_handler in &mut *input_handlers {
                    input_handler.handle_key(key);
                }
            }
            _ => {}
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

        stdout
            .execute(terminal::EnableLineWrap)
            .unwrap()
            .execute(terminal::Clear(terminal::ClearType::All))
            .unwrap()
            .execute(MoveTo(0, 0))
            .unwrap();

        println!("{}", msg);
    }));

    let mut stdout = stdout();

    terminal::enable_raw_mode().unwrap();

    stdout
        .execute(terminal::DisableLineWrap)
        .unwrap()
        .execute(terminal::Clear(terminal::ClearType::All))
        .unwrap();
}

/// Quits the app and returns the terminal to usable state.
pub fn quit() {
    let mut stdout = stdout();

    stdout
        .execute(terminal::EnableLineWrap)
        .unwrap()
        .execute(ResetColor)
        .unwrap()
        .execute(terminal::Clear(terminal::ClearType::All))
        .unwrap()
        .execute(MoveTo(0, 0))
        .unwrap();

    terminal::disable_raw_mode().unwrap();
}

pub fn new_buffer(
    origin: OutputPos, end: OutputPos, file_path: PathBuf, options: &Options,
) -> Buffer<TermArea> {
    let area = TermArea::new(origin, end);

    Buffer::new(area, file_path, options.file_options.clone())
}
