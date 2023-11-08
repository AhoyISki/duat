use std::{
    io::{stdout, Write},
    sync::{Arc, Mutex},
};

use crossterm::cursor::{self, MoveTo, MoveToColumn, MoveToNextLine};

use crate::{area::Coord, Coords};

macro_rules! queue {
    ($writer:expr $(, $command:expr)* $(,)?) => {
        unsafe { crossterm::queue!($writer $(, $command)*).unwrap_unchecked() }
    }
}

#[derive(Debug)]
pub struct Lines {
    bytes: Vec<u8>,
    cutoffs: Vec<usize>,
    coords: Coords,
    show_cursor: bool,
}

impl Lines {
    pub fn show_cursor(&mut self) {
        self.show_cursor = true;
    }

    fn on(&self, y: usize) -> Option<(&[u8], usize, usize)> {
        y.checked_sub(self.coords.tl().y).map(|y| {
            let start = self.cutoffs[y];
            let (start_x, end_x) = (self.coords.tl().x, self.coords.br().x);
            match self.cutoffs.get(y + 1) {
                Some(end) => (&self.bytes[start..*end], start_x, end_x),
                None => (&self.bytes[start..], start_x, end_x),
            }
        })
    }
}

impl Write for Lines {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.bytes.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.cutoffs.push(self.bytes.len());
        Ok(())
    }
}

struct Receiver {
    lines: Arc<Mutex<Option<Lines>>>,
    coords: Coords,
}

impl Receiver {
    fn take(&self) -> Option<Lines> {
        self.lines.lock().unwrap().take()
    }
}

#[derive(Debug)]
pub struct Sender {
    lines: Arc<Mutex<Option<Lines>>>,
    coords: Coords,
}

impl Sender {
    pub fn lines(&self) -> Lines {
        let area = self.coords.width() * self.coords.height();
        let mut cutoffs = Vec::with_capacity(self.coords.height());
        cutoffs.push(0);

        Lines {
            bytes: Vec::with_capacity(area * 2),
            cutoffs,
            coords: self.coords,
            show_cursor: false,
        }
    }

    pub fn send(&self, lines: Lines) {
        *self.lines.lock().unwrap() = Some(lines);
    }
}

pub struct Printer {
    recvs: Vec<Receiver>,
    max: Coord,
}

impl Printer {
    pub fn new(max: Coord) -> Self {
        Self {
            recvs: Vec::new(),
            max,
        }
    }

    pub fn sender(&mut self, coords: Coords) -> Sender {
        let receiver = Receiver {
            lines: Arc::new(Mutex::new(None)),
            coords,
        };

        let sender = Sender {
            lines: receiver.lines.clone(),
            coords,
        };

        match self
            .recvs
            .binary_search_by(|other| other.coords.cmp(&coords))
        {
            Ok(_) => unreachable!("One receiver per area."),
            Err(pos) => self.recvs.insert(pos, receiver),
        }

        sender
    }

    pub fn reset(&mut self) {
        self.recvs.clear();
    }

    pub fn print(&self) {
        let lines: Vec<_> = self.recvs.iter().flat_map(Receiver::take).collect();

        if lines.is_empty() {
            return;
        }

        let mut stdout = stdout().lock();
        queue!(stdout, cursor::Hide, MoveTo(0, 0));

        for y in 0..self.max.y {
            let mut x = 0;

            let lines = lines.iter().flat_map(|lines| lines.on(y));
            for (bytes, start, end) in lines {
                if x != start {
                    queue!(stdout, MoveToColumn(start as u16));
                }

                stdout.write_all(bytes).unwrap();

                x = end;
            }

            queue!(stdout, MoveToNextLine(1))
        }

        if lines.iter().any(|lines| lines.show_cursor) {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        stdout.flush().unwrap();
    }
}
