use std::env;
use std::path::PathBuf;

use dirs;

mod terminal;

use parsec_core::{
    Options,
    FileOptions,
    WrapType,
    output::{
        OutputPos,
        WindowBuffer,
    }
};
use terminal::{FileBuffer, TermBuffer};

fn main() {
    let mut buffers = TermBuffer::new();

    let (width, height) = crossterm::terminal::size().expect("crossterm");
    let origin = OutputPos { x: 0, y: 0 };
    let end = OutputPos { x: width, y: height };

    // TODO: Option interfacing.
    // TODO: Create a new() function instead of doing it like this.
    // TODO: Create a configuration file somewhere on the system.
    let options = Options {
        file_options: FileOptions {
            wrap_type: if cfg!(feature = "wrapped") {
                WrapType::Width
            } else {
                WrapType::NoWrap
            },

            cursor_x_spacing: 0,
            cursor_y_spacing: 5,
        }
    };


    // Basic utility to print readable error messages.
    let args: Vec<String> = env::args().collect();

    let current_dir = env::current_dir().expect("Invalid directory");
    let home_dir = dirs::home_dir().expect("Home directory not reachable");

    terminal::startup();

    for file in args[1..].iter() {
        let file_path = PathBuf::from(file);
        let file_path = if file_path.is_absolute() {
            file_path
        } else if file_path.starts_with("~") {
            PathBuf::from(&home_dir).join(file_path)
        } else {
            PathBuf::from(&current_dir).join(file_path)
        };
        let file_buffer = FileBuffer::new(origin, end, file_path, options)
                                     .expect("file not found");
        buffers.input_handlers.push(Box::from(file_buffer.file_handler));
    }

    buffers.process_events();

    terminal::quit();
}
