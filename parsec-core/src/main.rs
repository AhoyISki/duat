use std::env;

use dirs;

use std::io::{stdout, Write, Stdout};
use std::path::PathBuf;
use std::cmp::max;

use crossterm::{
    QueueableCommand,
    ExecutableCommand,
    terminal::{self, enable_raw_mode, disable_raw_mode},
    event::{read, Event, KeyCode},
};

mod files;
use files::{PrintableArea, FileBuffer};

fn main() -> crossterm::Result<()> {
    let mut stdout = stdout();

    // Basic utility to print readable error messages.
    use std::panic::set_hook;
    set_hook(Box::new(|msg| {
        disable_raw_mode().expect("failed to disable raw mode");
        println!("\x1b[2J\x1b[H{}", msg);
    }));

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
        files.push(files::FileBuffer::from(file_path)
            .expect("Something went wrong when reading the file!"),
        );
    }

    enable_raw_mode()?;

    stdout.execute(terminal::DisableLineWrap)?
          .execute(terminal::Clear(terminal::ClearType::All))?;

    files.get_mut(0)
         .expect("whoops")
         .request_area(
             files::TermPos { x: 4, y: 0 },
             files::TermPos { x: 50, y: 40 });

    files.get(0)
         .expect("whoops")
         .print_contents(&mut stdout)
         .expect("Printing failed");

    stdout.flush()?;

    process_events(files.get_mut(0).expect("whoops"), &mut stdout)?;

    disable_raw_mode()?;

    Ok(())
}

// NOTE: This is how you create a new ContentStyle object:
// ContentStyle {
//     foreground_color: Some(Color::Blue),
//     background_color: None,
//     attributes: Attribute::Bold.into(),
// }

fn process_events(file: &mut FileBuffer, stdout: &mut Stdout) -> crossterm::Result<()> {
    let mut count: i32 = 0;
    loop {
        match read()? {
            Event::Key(event) => {
                match event.code {
                    KeyCode::Down => {
                        for _ in 0..(max(count - 1, 0)) {
                            file.move_cursor_down(0, false);
                        }
                        file.move_cursor_down(0, true);
                        count = 0;
                    },
                    KeyCode::Up => {
                        for _ in 0..(max(count - 1, 0)) {
                            file.move_cursor_up(0, false);
                        }
                        file.move_cursor_up(0, true);
                        count = 0;
                    },
                    KeyCode::Right => {
                        for _ in 0..(max(count - 1, 0)) {
                            file.move_cursor_right(0, false);
                        }
                        file.move_cursor_right(0, true);
                        count = 0;
                    },
                    KeyCode::Left => {
                        for _ in 0..(max(count - 1, 0)) {
                            file.move_cursor_left(0, false);
                        }
                        file.move_cursor_left(0, true);
                        count = 0;
                    },
                    KeyCode::Char(c) if c.is_numeric() => {
                        count = count * 10 + c.to_digit(10).unwrap() as i32;
                    },
                    KeyCode::Esc => break,
                    _ => {},
                }
                stdout.queue(crossterm::cursor::Hide)?;
                file.print_contents(stdout)?;
                stdout.queue(crossterm::cursor::Show)?;
                stdout.flush()?;
            },
            _ => (),
        }
    }

    Ok(())
}
