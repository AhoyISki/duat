use std::env;

use std::fs::File;

use std::io::{stdout, Write, self, Read};

use std::path::PathBuf;

use dirs;

use crossterm::{
    cursor::MoveTo,
    terminal,
    style::Print,
    QueueableCommand, ExecutableCommand,
    self,
};

fn main() -> crossterm::Result<()> {
    let mut stdout = stdout();

    let args: Vec<String> = env::args().collect();

    let current_dir = env::current_dir().expect("Invalid directory");
    let home_dir = dirs::home_dir().expect("Home directory not available");

    let mut files = Vec::new();

    for file in args[1..].iter() {
        let file_as_path = PathBuf::from(file);
        let file_path = if file_as_path.is_absolute() {
            file_as_path
        } else if file_as_path.starts_with("~") {
            PathBuf::from(&home_dir).join(file_as_path)
        } else {
            PathBuf::from(&current_dir).join(file_as_path)
        };
        files.push(read_lines_to_vector(File::open(file_path))
            .expect("Something went wrong when reading the file!"));
    }

    //files.get(0).expect("empty").iter().for_each(|l| println!("{}", l));

    stdout.execute(terminal::DisableLineWrap)?
          .execute(terminal::Clear(terminal::ClearType::All))?;

    let file_content = Content {
        text: files.get(0).expect("\nFile not indexed correctly"),
    };

    let file_area = TerminalArea {
        origin: Point { x: 2, y: 0 },
        end: Point { x: 16, y: 40 },
        content_buffer: file_content,
    };

    file_area.print_contents(&stdout).expect("Printing failed :");

    stdout.flush()?;

    Ok(())
}

struct Point {
    pub x: u16,
    pub y: u16,
}

/* The Dimensions, as expected, grow from left to right and from top to bottom */
struct TerminalArea<'a> {
    origin: Point,
    end: Point,
    content_buffer: Content<'a>,
}

struct Content<'a> {
    text: &'a Vec<String>,
    // TODO: Implemet this: styles: Vec<Vec<ContentStyle>>,
}

/* TODO: Create content struct that will hold the data put into the areas */
    /*
     * TODO: Additionally, create a trait that dictates the ability to pass data to
     * TerminalAreas.
     */

impl TerminalArea<'_> {
    /* TODO: Finish this function */
    pub fn print_contents(&self, mut stdout: &io::Stdout) -> crossterm::Result<()> {
        
        let width = self.end.x - self.origin.x;
        let height = self.end.y - self.origin.y;

        let mut vec_iter = self.content_buffer.text.iter();

        for line in 0..=height as usize {
            match vec_iter.next() {
                Some(text) => {
                    stdout
                        .queue(MoveTo(self.origin.x, self.origin.y + line as u16))?
                        .queue(Print(text))?;
                },
                None => break,
            }
        }
        stdout.queue(Print("\n"))?;

        Ok(())
    }
}

// TODO: Return as mut in the future.
fn read_lines_to_vector(file: io::Result<File>)
    -> Result<Vec<String>, std::io::Error>
{
    let mut file_contents = String::new();
    file?.read_to_string(&mut file_contents)?;

    let vectored_file = file_contents.lines().map(|l| l.to_string()).collect();

    Ok(vectored_file)
}
