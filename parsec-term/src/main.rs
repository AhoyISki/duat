use std::{env, path::PathBuf};

use dirs;
use parsec_core::{
    config::{ConfigOptions, LineNumbers, Options, ScrollOff, TabPlaces, WrapMethod},
    input::InputHandler,
    output::OutputPos,
};

mod terminal;

fn main() {
    let mut buffers: Vec<Box<dyn InputHandler>> = Vec::new();

    let (width, height) = crossterm::terminal::size().expect("crossterm");
    let origin = OutputPos { x: 0, y: 0 };
    let end = OutputPos { x: width, y: height };

    // TODO: Option interfacing. Potentially through compiled rust code.
    // TODO: Create a new() function instead of doing it like this.
    // TODO: Create a configuration file somewhere on the system.
    let options = Options {
        file_options: ConfigOptions {
            wrap_method: if cfg!(feature = "wrapped") {
                WrapMethod::Width
            } else {
                WrapMethod::NoWrap
            },

            scrolloff: ScrollOff { d_x: 5, d_y: 5 },
            line_numbers: LineNumbers::Hybrid,
            tabs: TabPlaces::Regular(4),
            wrap_indent: true,
            tabs_as_spaces: false,
        },
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
        let buffer = terminal::new_buffer(origin, end, file_path, &options);
        buffers.push(Box::from(buffer));
    }

    terminal::process_events(&mut buffers);

    terminal::quit();
}
