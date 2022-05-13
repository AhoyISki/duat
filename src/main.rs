use std::env;

use dirs;

use std::io::{stdout, Write};
use std::path::PathBuf;

use crossterm::{
    QueueableCommand,
    ExecutableCommand,
    terminal,
    style::Print,
};

mod files;
use files::TerminalArea;

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
        files.push(files::FileBuffer::from(file_path)
            .expect("Something went wrong when reading the file!"),
        );
    }

    stdout.execute(terminal::DisableLineWrap)?
          .execute(terminal::Clear(terminal::ClearType::All))?;

    files.get_mut(0)
         .expect("whoops")
         .request_area(files::Pos { x: 4, y: 5 }, files::Pos { x: 50, y: 40 });

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

// NOTE: This is how you create a new ContentStyle object:
// ContentStyle {
//     foreground_color: Some(Color::Blue),
//     background_color: None,
//     attributes: Attribute::Bold.into(),
// }
// 
