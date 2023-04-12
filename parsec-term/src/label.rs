use std::{
    io::{stdout, StdoutLock, Write},
    sync::atomic::{Ordering, AtomicBool},
};

use crossterm::{
    cursor::{MoveTo, SavePosition, self, RestorePosition},
    queue,
    style::{ContentStyle, Print, ResetColor, SetStyle}, execute,
};
use parsec_core::{
    config::{TabPlaces, WrapMethod, Config},
    tags::form::{CursorStyle, Form},
    text::PrintStatus,
    ui::{self, Area as UiArea},
};
use ropey::RopeSlice;
use unicode_width::UnicodeWidthChar;

use crate::{Area, Coord};

// Static variables, used solely for printing.
static IS_PRINTING: AtomicBool = AtomicBool::new(false);
static SHOW_CURSOR: AtomicBool = AtomicBool::new(false);

pub struct Label {
    area: Area,
    stdout_lock: StdoutLock<'static>,

    cursor: Coord,
    is_active: bool,

    last_style: ContentStyle,
    style_before_cursor: Option<ContentStyle>,

    wrap_method: WrapMethod,
    tab_places: TabPlaces,
    indent: usize,
}

impl Label {
    pub fn new(area: Area) -> Self {
        let cursor = area.tl();
        let stdout_lock = stdout().lock();
        Label {
            area,
            stdout_lock,
            cursor,
            is_active: false,
            last_style: ContentStyle::default(),
            style_before_cursor: None,
            wrap_method: WrapMethod::NoWrap,
            tab_places: TabPlaces::default(),
            indent: 0,
        }
    }

    fn clear_line(&mut self) {
        let _ = queue!(self.stdout_lock, ResetColor);

        if self.cursor.x < self.area.br().x {
            // The rest of the line is featureless.
            let padding_count = (self.area.br().x - self.cursor.x) as usize;
            queue!(self.stdout_lock, Print(" ".repeat(padding_count))).unwrap();
        }

        self.cursor.x = self.area.tl().x;
        self.cursor.y += 1;

        let _ = queue!(
            self.stdout_lock,
            ResetColor,
            MoveTo(self.cursor.x, self.cursor.y),
            SetStyle(self.last_style)
        );
    }

    fn wrap_line(&mut self) -> PrintStatus {
        self.clear_line();

        let _ = queue!(
            self.stdout_lock,
            MoveTo(self.cursor.x, self.cursor.y),
            Print(" ".repeat(self.indent))
        );

        self.cursor.x += self.indent as u16;
        self.indent = 0;

        if self.cursor.y == self.area.br().y - 1 {
            PrintStatus::Finished
        } else {
            PrintStatus::NextChar
        }
    }
}

impl ui::Label<Area> for Label {
    fn area(&self) -> &Area {
        &self.area
    }

    fn set_form(&mut self, form: Form) {
        self.last_style = form.style;
        let _ = queue!(self.stdout_lock, ResetColor, SetStyle(form.style));
    }

    fn place_main_cursor(&mut self, cursor_style: CursorStyle) {
        if let (Some(caret), true) = (cursor_style.caret, self.is_active) {
            let _ = queue!(self.stdout_lock, caret, SavePosition);
            SHOW_CURSOR.store(true, Ordering::Relaxed)
        } else {
            self.style_before_cursor = Some(self.last_style);
            let _ = queue!(self.stdout_lock, SetStyle(cursor_style.form.style));
        }
    }

    fn place_extra_cursor(&mut self, cursor_style: CursorStyle) {
        self.style_before_cursor = Some(self.last_style);
        let _ = queue!(self.stdout_lock, SetStyle(cursor_style.form.style));
    }

    fn set_as_active(&mut self) {
        self.is_active = true;
        SHOW_CURSOR.store(false, Ordering::Relaxed)
    }

    fn start_printing(&mut self, config: &Config) {
        self.wrap_method = config.wrap_method;
        self.tab_places = config.tab_places.clone();

        while IS_PRINTING
            .compare_exchange_weak(false, true, Ordering::Relaxed, Ordering::Relaxed)
            .is_err()
        {
            std::thread::sleep(std::time::Duration::from_micros(500));
        }

        let _ = queue!(self.stdout_lock, MoveTo(self.area.tl().x, self.area.tl().y));
        let _ = execute!(self.stdout_lock, cursor::Hide);
    }

    fn stop_printing(&mut self) {
        while let PrintStatus::NextChar = self.next_line() {}

        let _ = execute!(self.stdout_lock, ResetColor);

        if SHOW_CURSOR.load(Ordering::Relaxed) {
            let _ = execute!(self.stdout_lock, RestorePosition, cursor::Show);
        }

        IS_PRINTING.store(false, Ordering::Relaxed);
    }

    fn print(&mut self, ch: char, x_shift: usize) -> PrintStatus {
        let len = match ch {
            '\t' => self.tab_places.spaces_on_col(x_shift) as u16,
            _ => UnicodeWidthChar::width(ch).unwrap_or(0) as u16,
        };

        if self.cursor.x <= self.area.br().x - len {
            self.cursor.x += len;
            let _ = {
                let mut temp = [b'a'; 4];
                self.stdout_lock.write_all(ch.encode_utf8(&mut temp).as_bytes())
            };
            if let Some(style) = self.style_before_cursor.take() {
                let _ = queue!(self.stdout_lock, ResetColor, SetStyle(style));
            }
        } else if self.cursor.x <= self.area.br().x {
            let width = self.area.br().x - self.cursor.x;
            let _ = queue!(self.stdout_lock, Print(" ".repeat(width as usize)));
            if let Some(style) = self.style_before_cursor.take() {
                let _ = queue!(self.stdout_lock, ResetColor, SetStyle(style));
            }
        }

        if self.cursor.x == self.area.br().x {
            if let WrapMethod::NoWrap = self.wrap_method {
                PrintStatus::NextLine
            } else {
                self.wrap_line()
            }
        } else {
            PrintStatus::NextChar
        }
    }

    fn next_line(&mut self) -> PrintStatus {
        if self.cursor.y + 1 < self.area.br().y {
            self.clear_line();
            PrintStatus::NextChar
        } else {
            PrintStatus::Finished
        }
    }

    fn wrap_count(
        &self,
        slice: RopeSlice,
        wrap_method: WrapMethod,
        tab_places: &TabPlaces,
    ) -> usize {
        match wrap_method {
            WrapMethod::Width => self.get_width(slice, tab_places) / self.area.width(),
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0,
        }
    }

    fn col_at_wrap(
        &self,
        slice: RopeSlice,
        wrap: usize,
        wrap_method: WrapMethod,
        tab_places: &TabPlaces,
    ) -> usize {
        match wrap_method {
            WrapMethod::Width => {
                let dist = wrap * self.area.width();
                slice
                    .chars()
                    .enumerate()
                    .scan((0, false), |(width, end_reached), (index, ch)| {
                        *width += match ch {
                            '\t' => tab_places.spaces_on_col(*width),
                            ch => UnicodeWidthChar::width(ch).unwrap_or(0),
                        };
                        if *end_reached {
                            return None;
                        }
                        if *width >= dist {
                            *end_reached = true
                        }
                        Some(index)
                    })
                    .last()
                    .unwrap()
            }
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0,
        }
    }

    fn get_width(&self, slice: RopeSlice, tab_places: &TabPlaces) -> usize {
        let mut width = 0;
        for ch in slice.chars() {
            width += match ch {
                '\t' => tab_places.spaces_on_col(width),
                ch => UnicodeWidthChar::width(ch).unwrap_or(0),
            }
        }

        width
    }

    fn col_at_dist(&self, slice: RopeSlice, dist: usize, tab_places: &TabPlaces) -> usize {
        slice
            .chars()
            .enumerate()
            .scan((0, false), |(width, end_reached), (index, ch)| {
                *width += match ch {
                    '\t' => tab_places.spaces_on_col(*width),
                    ch => UnicodeWidthChar::width(ch).unwrap_or(0),
                };
                if *end_reached {
                    return None;
                }
                if *width >= dist {
                    *end_reached = true
                }
                Some(index)
            })
            .last()
            .unwrap_or(0)
    }
}

unsafe impl Send for Label {}
unsafe impl Sync for Label {}
