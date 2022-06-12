use std::{
    env,
    path::PathBuf,
};

use dirs;

use parsec_core::{
    config::{
        Options,
        FileOptions,
        WrapType,
    },
    output::OutputPos
};

mod terminal;
use terminal::{
    TerminalApp,
    FileBuffer
};

fn main() {
    let mut buffers = TerminalApp::new();

    let (width, height) = crossterm::terminal::size().expect("crossterm");
    let origin = OutputPos { x: 0, y: 0 };
    let end = OutputPos { x: width, y: height };

    // TODO: Option interfacing. Potentially through compiled rust code.
    // TODO: Create a new() function instead of doing it like this.
    // TODO: Create a configuration file somewhere on the system.
    let options = Options {
        file_options: FileOptions {
            wrap_type: if cfg!(feature = "wrapped") {
                WrapType::Width
            } else {
                WrapType::NoWrap
            },

            x_spacing: 0,
            scrolloff: 5,
        }
    };

    // The files being opened, and the options being set at runtime.
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
