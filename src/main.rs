use unicode_segmentation::{UnicodeSegmentation, /*GraphemeCursor*/};

use unicode_width;

use std::{env, fs};

use std::io::{self, stdout, Write};

use std::path::PathBuf;

use dirs;

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
    ExecutableCommand,
    terminal,
};

fn main() -> crossterm::Result<()> {
    let mut stdout = stdout();

    let args: Vec<String> = env::args().collect();

    let current_dir = env::current_dir().expect("Invalid directory");
    let home_dir = dirs::home_dir().expect("Home directory not reachable");

    let mut files = Vec::new();

    for file in args[1..].iter() {
        let file_path = PathBuf::from(file);
        let file_path = if file_path.is_absolute() {
            file_path
        } else if file_path.starts_with("~") {
            PathBuf::from(&home_dir).join(file_path)
        } else {
            PathBuf::from(&current_dir).join(file_path)
        };
        files.push(FileBuffer::from(file_path)
            .expect("Something went wrong when reading the file!"),
        );
    }

    stdout.execute(terminal::DisableLineWrap)?
          .execute(terminal::Clear(terminal::ClearType::All))?;

    files.get_mut(0)
         .expect("whoops")
         .request_area(Pos { x: 4, y: 5 }, Pos { x: 50, y: 40 });

    files.get_mut(0)
         .expect("whoops")
         .set_wrapping_chars();

    files.get(0)
         .expect("whoops")
         .print_contents(&mut stdout)
         .expect("Printing failed");

    stdout.queue(Print("\n\n\n\n"))?.flush()?;

    Ok(())
}

struct Pos {
    pub x: u16,
    pub y: u16,
}

trait TerminalArea<'a> {
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()>;

    fn allocate_area(&self, origin: Pos, end: Pos) -> (Pos, Pos) {
        (origin, end)
    }

    fn request_area(&mut self, origin: Pos, end: Pos);
}

struct FileBuffer {
    content: Vec<FileLine>,
    origin: Pos,
    end: Pos,
    
    // TODO: Move options to a centralized option place.
    // TODO: Turn this into a more versatile enum.
    wrap_lines: bool,
}

impl FileBuffer {
    pub fn from(file_path: PathBuf) -> io::Result<FileBuffer> {
        let file_contents = fs::read_to_string(file_path).expect("file not found");

        let mut text = FileBuffer {
            content: Vec::new(),
            origin: Pos { x: 0, y: 0 },
            end: Pos { x: 0, y: 0 },
            wrap_lines: true,
        };

        for line_text in file_contents.lines() {
            let mut line = FileLine {
                line_width: 1,
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
        if !self.wrap_lines {
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

// NOTE: This is how you create a new ContentStyle object:
// ContentStyle {
//     foreground_color: Some(Color::Blue),
//     background_color: None,
//     attributes: Attribute::Bold.into(),
// }
// 

// Any use of the terms col and row refers specifically to a column or row in the
// terminal, while the terms line and glyph refers to actual parts of the text
struct FileLine {
    line_width: usize,
    text: Vec<StyledGrapheme>,
}

struct StyledGrapheme {
    glyph: StyledContent<String>,
    style_is_final: bool,
    is_wrapping: bool,
    width: usize,
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
                if col + styled_char.width > width {
                    if !self.wrap_lines { break; }
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
