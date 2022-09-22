use std::{
    fmt::Display,
    fs::DirEntry,
    io::{stdout, Stdout, Write},
    path::PathBuf,
    rc::Rc,
    cmp::min
};

use crossterm::{
    cursor::{Hide, MoveTo, RestorePosition, SavePosition, Show},
    event::{read, Event, KeyCode, KeyEvent, KeyModifiers},
    style::{Attribute, Attributes, Color, ContentStyle, Print, ResetColor, SetStyle},
    terminal, ExecutableCommand, QueueableCommand,
};
use parsec_core::{
    input::InputHandler,
    tags::Form,
    ui::{self, Container as UiContainer, Direction, Label as UiLabel, Split},
};
use unicode_width::UnicodeWidthChar;

#[derive(Clone, Copy)]
enum FormType {
    Normal,
    MultiLine,
}

struct UiManager { initial: bool }

#[derive(Clone, Copy)]
struct Coord {
    x: u16,
    y: u16,
}

#[derive(Clone, Copy)]
struct Area {
    tl: Coord,
    br: Coord
}

impl Area {
    fn total() -> Self {
        let size = terminal::size().expect("crossterm");

        Area { tl: Coord { x: 0, y: 0 }, br: Coord { x: size.0 as u16, y: size.1 as u16 } }
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y) as usize
    }

    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }
}

#[derive(Clone)]
struct Container {
    area: Area,
    direction: Direction,
}

impl UiContainer for Container {
    fn direction(&self) -> Direction {
        self.direction
    }

    fn height(&self) -> usize {
        self.area.height()
    }

    fn width(&self) -> usize {
        self.area.width()
    }

    fn request_len(&mut self, width: usize) {
        todo!()
    }
}

struct Label {
    stdout: Stdout,
    area: Area,
    cursor: Coord,
    direction: Direction
}

impl Label {
    fn new(area: Area, direction: Direction) -> Self {
        Label { stdout: stdout(), area, cursor: area.tl, direction }
    }
}

impl Clone for Label {
    fn clone(&self) -> Self {
        Label { stdout: stdout(), ..*self }
    }
}

impl UiLabel for Label {
    fn next_line(&mut self) -> bool {
        if self.cursor.y == self.area.br.y - 1 {
            false
        } else {
            // Print one more "newline" character with the current form.
            self.stdout.queue(Print(' ')).expect("crossterm");

            self.clear_form();

            // The rest of the line is featureless.
            let padding = " ".repeat((self.area.tl.x - self.cursor.x) as usize - 2);
            self.stdout.queue(Print(padding)).expect("crossterm");

            self.cursor.x = self.area.tl.x;
            self.cursor.y += 1;

            self.stdout.queue(MoveTo(self.cursor.x, self.cursor.y)).expect("crossterm");

            true
        }
    }

    fn clear_form(&mut self) {
        self.stdout.queue(ResetColor).expect("crossterm");
    }

    fn set_form(&mut self, form: Form) {
        self.stdout.queue(SetStyle(form.style)).expect("crossterm");
    }

    fn get_char_len(&self, ch: char) -> usize {
        UnicodeWidthChar::width(ch).unwrap_or(0)
    }

    fn width(&self) -> usize {
        self.area.width()
    }

    fn height(&self) -> usize {
        self.area.height()
    }

    fn place_primary_cursor(&mut self) {
        self.stdout.queue(SavePosition).expect("crossterm");
    }

    fn place_secondary_cursor(&mut self) {
        todo!()
    }

    fn print(&mut self, ch: char) {
        self.cursor.x += self.get_char_len(ch) as u16;
        self.stdout.queue(Print(ch)).expect("crossterm");
    }

    fn request_len(&mut self, width: usize) {
        todo!()
    }

    fn start_printing(&mut self) {
        self.stdout.queue(MoveTo(self.area.tl.x, self.area.tl.y)).expect("crossterm");
    }

    fn stop_printing(&mut self) {
        for _ in self.cursor.y..self.area.br.y {
            self.next_line();
        }

        self.stdout.execute(RestorePosition).expect("crossterm");
    }
}

impl ui::Ui for UiManager {
    type Container = Container;
    type Label = Label;

    fn split_container(
        &mut self, container: &mut Self::Container, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label) {
        let len = parse_split(split, container.area, direction);
        let parent_container = container.clone();

        let area =
            split_by(len, &mut container.area, direction);

		let new_label = Label::new(area, direction);

		(parent_container, new_label)
    }

    fn split_label(
        &mut self, label: &mut Self::Label, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label) {
        let len = parse_split(split, label.area, direction);
        let parent_container = Container { area: label.area, direction: label.direction };

        let area =
            split_by(len, &mut label.area, direction);

		let new_label = Label::new(area, direction);

		(parent_container, new_label)
    }

	fn only_label(&mut self) -> Option<Self::Label> {
    	if self.initial {
        	self.initial = false;
        	Some(Label::new(Area::total(), Direction::Left))
    	} else {
        	None
    	}
	}
}

///// An area in the terminal used for printing text.
/////
///// These should be linked to something that can print. They should never need to tell a printing
///// struct where the origin or end are, since they only need to know how much space is in the
///// area, and the area deals with placing the text in the correct place.
//pub struct TermArea {
//    origin: OutputPos,
//    end: OutputPos,
//
//    stdout: Stdout,
//
//    form_stack: Vec<(Form, u16, FormType)>,
//}

//impl TermArea {
//    fn new(origin: OutputPos, end: OutputPos) -> Self {
//        TermArea { origin, end, stdout: stdout(), form_stack: Vec::new() }
//    }
//
//    fn print_form_stack(&mut self) {
//        let mut final_style = ContentStyle {
//            foreground_color: Some(Color::Reset),
//            background_color: Some(Color::Reset),
//            underline_color: Some(Color::Reset),
//            attributes: Attributes::from(Attribute::Reset),
//        };
//        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false,
// false);
//
//        for &(Form { style, is_final, .. }, _, _) in &self.form_stack {
//            if let Some(color) = style.foreground_color {
//                if !fg_done {
//                    final_style.foreground_color = Some(color);
//                    if is_final { fg_done = true }
//                }
//            }
//            if let Some(color) = style.background_color {
//                if !bg_done {
//                    final_style.background_color = Some(color);
//                    if is_final { bg_done = true }
//                }
//            }
//            if let Some(color) = style.foreground_color {
//                if !ul_done {
//                    final_style.underline_color = Some(color);
//                    if is_final { ul_done = true }
//                }
//            }
//            if !attr_done && !final_style.attributes.is_empty() {
//                final_style.attributes = style.attributes;
//                if is_final { attr_done = true }
//            }
//
//            if fg_done && bg_done && ul_done && attr_done {
//                break;
//            }
//        }
//
//        self.stdout.queue(SetStyle(final_style)).unwrap();
//    }
//}
//
//impl OutputArea for TermArea {
//    fn can_place_secondary_cursor(&self) -> bool {
//        false
//    }
//
//    fn place_cursor(&mut self, tag: CharTagType) {
//        match tag {
//            CharTagType::PrimaryCursor => {
//                // I have no idea why I have to do this, but if I don't, forms act weird.
//                self.stdout.queue(ResetColor).unwrap().queue(SavePosition).unwrap();
//                self.print_form_stack();
//            }
//            CharTagType::SecondaryCursor => {
//                panic!("Secondary cursors not allowed on the terminal!")
//            }
//            _ => panic!("Other character tags are not supposed to be handled directly!"),
//        };
//    }
//
//    fn push_form(&mut self, form: &Form, index: u16) {
//        self.form_stack.push((*form, index, FormType::Normal));
//
//        self.print_form_stack();
//    }
//
//    fn pop_form(&mut self, index: u16) {
//        if let Some(element) = self
//            .form_stack
//            .iter()
//            .enumerate()
//            .rfind(|(_, &(_, i, f))| i == index && matches!(f, FormType::Normal))
//        {
//            self.form_stack.remove(element.0);
//
//            self.print_form_stack();
//        }
//    }
//
//    fn push_ml_form(&mut self, form: &Form, index: u16) {
//        self.form_stack.push((*form, index, FormType::MultiLine));
//
//        self.print_form_stack();
//    }
//
//    fn pop_ml_form(&mut self, index: u16) {
//        if let Some(element) = self
//            .form_stack
//            .iter()
//            .enumerate()
//            .rfind(|(_, &(_, i, f))| i == index && matches!(f, FormType::MultiLine))
//        {
//            self.form_stack.remove(element.0);
//
//            self.print_form_stack();
//        }
//    }
//
//    fn clear_normal_forms(&mut self) {
//        self.form_stack.retain(|(_, _, f)| matches!(f, FormType::MultiLine));
//
//        self.print_form_stack();
//    }
//
//    fn clear_all_forms(&mut self) {
//        self.form_stack.clear();
//
//        self.print_form_stack();
//    }
//
//    fn start_print(&mut self) {
//        self.stdout.execute(Hide).unwrap();
//    }
//
//    fn print<T: Display>(&mut self, ch: T) {
//        self.stdout.queue(Print(ch)).unwrap();
//    }
//
//    fn finish_print(&mut self) {
//        self.stdout.queue(RestorePosition).unwrap();
//        self.stdout.queue(Show).unwrap();
//        self.stdout.flush().unwrap();
//    }
//
//    fn move_cursor(&mut self, pos: OutputPos) {
//        self.stdout.queue(MoveTo(self.origin.x + pos.x, self.origin.y + pos.y)).unwrap();
//    }
//
//    fn width(&self) -> usize {
//        (self.end.x - self.origin.x) as usize
//    }
//
//    fn height(&self) -> usize {
//        (self.end.y - self.origin.y) as usize
//    }
//
//    fn partition_x(&mut self, x: u16) -> Self {
//        let end = OutputPos { x: self.origin.x + x - 1, y: self.end.y };
//        let term_area = TermArea::new(self.origin, end);
//        self.origin.x += x;
//        term_area
//    }
//
//    fn partition_y(&mut self, y: u16) -> Self {
//        let end = OutputPos { x: self.end.x, y: self.origin.y + y };
//        let term_area = TermArea::new(self.origin, end);
//        self.origin.y += y;
//        term_area
//    }
//}
//
//// TODO: Deal with mouse and resize events.
///// Processes keyboard, mouse, and resize events.
//pub fn process_events(input_handlers: &mut Vec<Box<dyn InputHandler>>) {
//    // TODO: Add more event types
//    // TODO: Quit in a way that makes more sense
//    loop {
//        match read().expect("crossterm") {
//            Event::Key(key) => {
//                // NOTE: Remove this!!
//                if key == KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE) {
//                    break;
//                }
//                for input_handler in &mut *input_handlers {
//                    input_handler.handle_key(key);
//                }
//            }
//            _ => {}
//        }
//    }
//}
//
///// Preliminary functions for startup.
//pub fn startup() {
//    // This makes it so that if the application panics, the panic message is printed
//    // nicely and the terminal is left in a usable state.
//    use std::panic::set_hook;
//    set_hook(Box::new(|msg| {
//        let mut stdout = stdout();
//
//        terminal::disable_raw_mode().unwrap();
//
//        stdout
//            .execute(terminal::EnableLineWrap)
//            .unwrap()
//            .execute(terminal::Clear(terminal::ClearType::All))
//            .unwrap()
//            .execute(MoveTo(0, 0))
//            .unwrap();
//
//        println!("{}", msg);
//    }));
//
//    let mut stdout = stdout();
//
//    terminal::enable_raw_mode().unwrap();
//
//    stdout
//        .execute(terminal::DisableLineWrap)
//        .unwrap()
//        .execute(terminal::Clear(terminal::ClearType::All))
//        .unwrap();
//}
//
///// Quits the app and returns the terminal to usable state.
//pub fn quit() {
//    let mut stdout = stdout();
//
//    stdout
//        .execute(terminal::EnableLineWrap)
//        .unwrap()
//        .execute(ResetColor)
//        .unwrap()
//        .execute(terminal::Clear(terminal::ClearType::All))
//        .unwrap()
//        .execute(MoveTo(0, 0))
//        .unwrap();
//
//    terminal::disable_raw_mode().unwrap();
//}
//
//pub fn new_buffer(
//    origin: OutputPos, end: OutputPos, file_path: PathBuf, options: &Options,
//) -> Buffer<TermArea> {
//    let area = TermArea::new(origin, end);
//
//    Buffer::new(area, file_path, options.file_options.clone())
//}

fn parse_split(split: Split, area: Area, direction: Direction) -> u16 {
    let current_len = match direction {
        Direction::Left | Direction::Right => area.width(),
        Direction::Top | Direction::Bottom => area.height(),
    };

    match split {
        Split::Locked(len) | Split::Static(len) => min(len, current_len) as u16,
        Split::Ratio(ratio) => {
            (current_len as f32 * ratio).floor() as u16
        }
    }
}

fn split_by(len: u16, area: &mut Area, direction: Direction) -> Area {
    match direction {
        Direction::Left => {
            let old_tl = area.tl.clone();
            area.tl.x += len;
            Area { tl: old_tl, br: Coord { x: area.tl.x, y: area.br.y } }
        }
        Direction::Right => {
            let old_br = area.br;
            area.br.x -= len;
            Area { tl: Coord { x: area.br.x,  y: area.tl.y }, br:old_br }
        }
        Direction::Top => {
            let old_tl = area.tl.clone();
            area.tl.y += len;
            Area { tl: old_tl, br: Coord { x: area.br.x, y: area.tl.y } }
        }
        Direction::Bottom => {
            let old_br = area.br.clone();
            area.br.y -= len;
            Area { tl: Coord { x: area.tl.x,  y: area.br.y }, br:old_br }
        }
    }
}
