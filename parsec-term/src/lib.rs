use std::{
    cmp::min,
    fmt::Display,
    io::{stdout, Stdout},
};

use crossterm::{
    cursor::{self, MoveTo, RestorePosition, SavePosition, SetCursorStyle},
    style::{ContentStyle, Print, ResetColor, SetStyle},
    terminal, ExecutableCommand, QueueableCommand,
};
use parsec_core::{
    tags::{CursorStyle, Form},
    ui::{self, Area, Container, Direction, Label, Split},
};
use unicode_width::UnicodeWidthChar;

pub struct UiManager {
    initial: bool,
}

impl UiManager {
    pub fn new() -> Self {
        UiManager { initial: true }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Coord {
    x: u16,
    y: u16,
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.y, self.x))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TermArea {
    tl: Coord,
    br: Coord,
}

impl Display for TermArea {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({})({})", self.tl, self.br))
    }
}

impl Area for TermArea {
    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y - 1) as usize
    }

    fn resize_children(
        &self, first: &mut Self, second: &mut Self, len: usize, first_dir: Direction,
    ) -> Result<(), ()> {
        let max_len = match first_dir {
            Direction::Left | Direction::Right => self.width(),
            Direction::Top | Direction::Bottom => self.height(),
        };

        if len > max_len {
            return Err(());
        };

        let mut new_second = *self;
        *first = split_by(len as u16, &mut new_second, first_dir);
        *second = new_second;

        Ok(())
    }
}

impl TermArea {
    fn total() -> Self {
        let size = terminal::size().unwrap();

        TermArea { tl: Coord { x: 0, y: 0 }, br: Coord { x: size.0 as u16, y: size.1 as u16 } }
    }
}

#[derive(Clone)]
pub struct TermContainer {
    area: TermArea,
    direction: Direction,
}

impl Container<TermArea> for TermContainer {
    fn area(&self) -> &TermArea {
        &self.area
    }

    fn area_mut(&mut self) -> &mut TermArea {
        &mut self.area
    }
}

pub struct TermLabel {
    stdout: Stdout,
    area: TermArea,
    cursor: Coord,
    direction: Direction,
    style_before_cursor: Option<ContentStyle>,
    last_style: ContentStyle,
}

impl TermLabel {
    fn new(area: TermArea, direction: Direction) -> Self {
        TermLabel {
            stdout: stdout(),
            area,
            cursor: area.tl,
            direction,
            style_before_cursor: None,
            last_style: ContentStyle::default(),
        }
    }

    fn clear_line(&mut self) {
        if self.cursor.x < self.area.br.x {
            self.clear_form();

            // The rest of the line is featureless.
            let padding_count = (self.area.br.x - self.cursor.x) as usize;
            let padding = " ".repeat(padding_count);
            self.stdout.queue(Print(padding)).unwrap();
        }

        self.cursor.x = self.area.tl.x;
        self.cursor.y += 1;

        self.stdout
            .queue(MoveTo(self.cursor.x, self.cursor.y))
            .unwrap()
            .queue(SetStyle(self.last_style))
            .unwrap();
    }
}

impl Clone for TermLabel {
    fn clone(&self) -> Self {
        TermLabel { stdout: stdout(), ..*self }
    }
}

impl Label<TermArea> for TermLabel {
    fn area(&self) -> &TermArea {
        &self.area
    }

    fn area_mut(&mut self) -> &mut TermArea {
        &mut self.area
    }

    fn next_line(&mut self) -> Result<(), ()> {
        if self.cursor.y == self.area.br.y - 1 {
            Err(())
        } else {
            self.clear_line();
            Ok(())
        }
    }

    fn wrap_line(&mut self, indent: usize) -> Result<(), ()> {
        if self.cursor.y == self.area.br.y - 1 {
            Err(())
        } else {
            self.clear_line();

            self.stdout
                .queue(MoveTo(self.cursor.x, self.cursor.y))
                .unwrap()
                .queue(Print(" ".repeat(indent)))
                .unwrap();

            self.cursor.x += indent as u16;

            Ok(())
        }
    }

    fn clear_form(&mut self) {
        self.stdout.queue(ResetColor).unwrap();
    }

    fn set_form(&mut self, form: Form) {
        self.last_style = form.style;
        self.stdout.queue(SetStyle(self.last_style)).unwrap();
    }

    fn get_char_len(&self, ch: char) -> usize {
        UnicodeWidthChar::width(ch).unwrap_or(0)
    }

    fn place_primary_cursor(&mut self, cursor_style: CursorStyle) {
        if let Some(caret) = cursor_style.caret {
            self.stdout.queue(caret).unwrap().queue(SavePosition).unwrap();
        } else {
            self.style_before_cursor = Some(self.last_style);
            self.stdout
                .queue(cursor::Hide)
                .unwrap()
                .queue(SetStyle(cursor_style.form.style))
                .unwrap();
        }
    }

    fn place_secondary_cursor(&mut self, cursor_style: CursorStyle) {
        self.style_before_cursor = Some(self.last_style);
        self.stdout.queue(SetStyle(cursor_style.form.style)).unwrap();
    }

    fn print(&mut self, ch: char) {
        let len = self.get_char_len(ch) as u16;
        if self.cursor.x <= self.area.br.x - len {
            self.cursor.x += len;
            self.stdout.queue(Print(ch)).unwrap();
            if let Some(style) = self.style_before_cursor.take() {
                self.stdout.queue(ResetColor).unwrap();
                self.stdout.queue(SetStyle(style)).unwrap();
            }
        }
    }

    fn start_printing(&mut self) {
        self.cursor = self.area.tl;
        self.stdout.queue(MoveTo(self.area.tl.x, self.area.tl.y)).unwrap();
    }

    fn stop_printing(&mut self) {
        for _ in self.cursor.y..(self.area.br.y.saturating_sub(2)) {
            let _ = self.next_line();
        }

        self.clear_line();

        stdout().execute(RestorePosition).unwrap();
        self.clear_form();
    }
}

impl ui::Ui for UiManager {
    type Area = TermArea;
    type Container = TermContainer;
    type Label = TermLabel;

    fn split_container(
        &mut self, container: &mut Self::Container, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label) {
        let len = get_split_len(split, container.area, direction);
        let parent_container = container.clone();

        let area = split_by(len, &mut container.area, direction);

        let new_label = TermLabel::new(area, direction);

        (parent_container, new_label)
    }

    fn split_label(
        &mut self, label: &mut Self::Label, direction: Direction, split: Split, glued: bool,
    ) -> (Self::Container, Self::Label) {
        let len = get_split_len(split, label.area, direction);
        let parent_container = TermContainer { area: label.area, direction: label.direction };

        let area = split_by(len, &mut label.area, direction);

        let new_label = TermLabel::new(area, direction);

        (parent_container, new_label)
    }

    fn only_label(&mut self) -> Option<Self::Label> {
        if self.initial {
            self.initial = false;
            Some(TermLabel::new(TermArea::total(), Direction::Left))
        } else {
            None
        }
    }

    fn startup(&mut self) {
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

    fn shutdown(&mut self) {
        let mut stdout = stdout();

        stdout
            .queue(ResetColor)
            .unwrap()
            .queue(terminal::Clear(terminal::ClearType::All))
            .unwrap()
            .queue(terminal::EnableLineWrap)
            .unwrap()
            .queue(MoveTo(0, 0))
            .unwrap()
            .queue(SetCursorStyle::DefaultUserShape)
            .unwrap()
            .execute(cursor::Show)
            .unwrap();

        terminal::disable_raw_mode().unwrap();
    }

    fn finish_all_printing(&mut self) {}
}

fn get_split_len(split: Split, area: TermArea, direction: Direction) -> u16 {
    let current_len = match direction {
        Direction::Left | Direction::Right => area.width(),
        Direction::Top | Direction::Bottom => area.height(),
    };

    match split {
        Split::Locked(len) | Split::Static(len) => min(len, current_len) as u16,
        Split::Ratio(ratio) => (current_len as f32 * ratio).floor() as u16,
    }
}

fn split_by(len: u16, area: &mut TermArea, direction: Direction) -> TermArea {
    match direction {
        Direction::Left => {
            let old_tl = area.tl;
            area.tl.x += len;
            TermArea { tl: old_tl, br: Coord { x: area.tl.x, y: area.br.y } }
        }
        Direction::Right => {
            let old_br = area.br;
            area.br.x -= len;
            TermArea { tl: Coord { x: area.br.x, y: area.tl.y }, br: old_br }
        }
        Direction::Top => {
            let old_tl = area.tl;
            area.tl.y += len;
            TermArea { tl: old_tl, br: Coord { x: area.br.x, y: area.tl.y } }
        }
        Direction::Bottom => {
            let old_br = area.br;
            area.br.y -= len;
            TermArea { tl: Coord { x: area.tl.x, y: area.br.y }, br: old_br }
        }
    }
}
