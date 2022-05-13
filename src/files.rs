use crossterm::{
    self,
    cursor::MoveTo,
    style::{
        ContentStyle,
        Print,
        //Attribute,
        //Color,
        PrintStyledContent,
        StyledContent
    },
    QueueableCommand,
};

use std::io;

use std::path::PathBuf;

use std::fs;

use unicode_segmentation::{UnicodeSegmentation, /*GraphemeCursor*/};

use unicode_width;

// Any use of the terms col and row refers specifically to a column or row in the
// terminal, while the terms line and glyph refers to actual parts of the text

// TODO: move this to a more general file
pub struct Pos {
    pub x: u16,
    pub y: u16,
}

// TODO: move this to a more general file
struct StyledGrapheme {
    glyph: StyledContent<String>,
    style_is_final: bool,
    is_wrapping: bool,
    width: usize,
}

struct FileLine {
    wrap_times: u8,
    text: Vec<StyledGrapheme>,
}

pub struct FileBuffer {
    content: Vec<FileLine>,

    // Position is origin inclusive, and end exclusive.
    origin: Pos,
    end: Pos,

    // Info about which line is on the top of the area.
    top_line: usize,
    top_line_wraps: u8,
    
    // TODO: Move options to a centralized option place.
    // TODO: Turn this into a more versatile enum.
    do_wrap_lines: bool,
}

impl FileBuffer {
    pub fn from(file_path: PathBuf) -> io::Result<FileBuffer> {
        let file_contents = fs::read_to_string(file_path).expect("file not found");

        let mut text = FileBuffer {
            content: Vec::new(),
            origin: Pos { x: 0, y: 0 },
            end: Pos { x: 0, y: 0 },

            top_line: 0,
            top_line_wraps: 0,

            do_wrap_lines: true,
        };

        for line_text in file_contents.lines() {
            let mut line = FileLine {
                wrap_times: 0,
                text: Vec::new(),
            };

            for grapheme in line_text.graphemes(true) {
                line.text.push(StyledGrapheme {
                    glyph: StyledContent::new(
                        ContentStyle::new(), String::from(grapheme)),
                    style_is_final: false,
                    is_wrapping: false,
                    width: unicode_width::UnicodeWidthStr::width(grapheme),
                });
            }

            text.content.push(line);
        }

        Ok(text)
    }

    pub fn set_wrapping_chars(&mut self) {
        if !self.do_wrap_lines {
            for line in self.content.iter_mut() {
                for glyph in line.text.iter_mut() {
                    glyph.is_wrapping = false;
                }
            }
        } else {
            let width = (self.end.x - self.origin.x) as usize;
            let mut index = width;

            for line in self.content.iter_mut() {
                loop {
                    if let Some(grapheme) = line.text.get_mut(index - 1) {
                        grapheme.is_wrapping = true;
                        line.wrap_times += 1;
                        index += width;
                    } else {
                        index = width;
                        break;
                    }
                }
            }
            
        }
        // TODO: add more wrapping modes.
        
    }
}

// TODO: move this to a more general file
pub trait TerminalArea<'a> {
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()>;

    fn allocate_area(&self, origin: Pos, end: Pos) -> (Pos, Pos) {
        (origin, end)
    }

    fn request_area(&mut self, origin: Pos, end: Pos);
}

impl TerminalArea<'_> for FileBuffer {
    /* TODO: Finish this function */
    /// Prints out the contents of an area.
    
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()> {
        let width = (self.end.x - self.origin.x) as usize;
        let height = (self.end.y - self.origin.y) as usize;

        let mut wrap = 0;

        for (row, line) in self.content.iter().enumerate() {
            stdout.queue(MoveTo(self.origin.x, self.origin.y + row as u16 + wrap))?;

            if row == height { break; };

            for (col, styled_char) in line.text.iter().enumerate() {
                if col + styled_char.width / width > 1 {
                    if !self.do_wrap_lines { break; }
                }

                stdout.queue(PrintStyledContent(styled_char.glyph.clone()))?;

                if styled_char.is_wrapping {
                    wrap += 1;
                    stdout.queue(MoveTo(
                        self.origin.x, self.origin.y + row as u16 + wrap))?;
                } 
            }
        }
        stdout.queue(Print("\n"))?;

        Ok(())
    }

    fn request_area(&mut self, origin: Pos, end: Pos) {
        (self.origin, self.end) = self.allocate_area(origin, end);
    }
}

