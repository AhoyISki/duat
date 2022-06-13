use std::{path::PathBuf, fs};

use crate::{
    map_actions,
    impl_input_handler,
    config::FileOptions,
    input::{InputHandler, ModeList},
    output::OutputArea,
};

mod file;
use file::{FileLine, File};

pub mod cursor;

use crossterm::event::{
    KeyEvent,
    KeyCode,
    KeyModifiers
};

// NOTE: This struct should strive to be completely UI agnostic, i.e., it should
// work wether the app is used in a terminal or in a GUI.
pub struct FileHandler<T: OutputArea> {
    /// The contents of the file
    pub file: File<T>,

    // Where exactly on the screen the origin and end of the area are placed is not
    // important here.
    /// The area allocated to the status line.
    pub status_line_area: T,
    /// The area allocated to the line numbers.
    pub line_num_area: T,

    /// List of mapped modes for file editing.
    mappings: ModeList<FileHandler<T>>,
}

impl<T: OutputArea> FileHandler<T> {
    /// Returns a new instance of ContentArea
    pub fn new(mut area: T, path: PathBuf, options: FileOptions) -> FileHandler<T> {
        // TODO: In the future, this will not panic!
        // TODO: Move this to a more general file.
        let file = fs::read_to_string(path).expect("file not found");

        let line_num_area;

        let mut file_handler = FileHandler {
            file: {
                let lines: Vec<FileLine> = file.lines().map(|l| FileLine::new(l))
                                               .collect();

                let mut file_area = area.partition_y(area.height() - 2);

                let mut line_num_width = 3;
                let mut num_exp        = 10;
                    
                while lines.len() > num_exp {
                    num_exp *= 10;
                    line_num_width += 1;
                }
                line_num_area = file_area.partition_x(line_num_width);

                File::new(lines, options, file_area)
            },

            status_line_area: area,
            line_num_area,

            mappings: ModeList::new(),
        };

        map_actions! {
            file_handler: FileHandler<T>, mappings;
            "insert" => [
                // Move all cursors up.
                key: (KeyCode::Up, KeyModifiers::NONE) => {
                    |h: &mut FileHandler<T>| {
                        h.file.cursors.iter_mut()
                                      .for_each(|c| c.move_up(&h.file.lines));
                        h.refresh_screen();
                    }
                },
                // Move all cursors down.
                key: (KeyCode::Down, KeyModifiers::NONE) => {
                    |h: &mut FileHandler<T>| {
                        h.file.cursors.iter_mut()
                                      .for_each(|c| c.move_down(&h.file.lines));
                        h.refresh_screen();
                    }
                },
                // Move all cursors left.
                key: (KeyCode::Left, KeyModifiers::NONE) => {
                    |h: &mut FileHandler<T>| {
                        h.file.cursors.iter_mut()
                                      .for_each(|c| c.move_left(&h.file.lines));
                        h.refresh_screen();
                    }
                },
                // Move all cursors right.
                key: (KeyCode::Right, KeyModifiers::NONE) => {
                    |h: &mut FileHandler<T>| {
                        h.file.cursors.iter_mut()
                                      .for_each(|c| c.move_right(&h.file.lines));
                        h.refresh_screen();
                    }
                },
            ]
        }

        file_handler.file.parse_wrapping();
        file_handler.refresh_screen();

        file_handler
    }

    /* TODO: Finish this function */
    /// Prints the contents of the file from line on the file.
    #[inline]
    fn refresh_screen(&mut self) {
        if self.file.has_scrolled() {
            self.file.print_file();
        }

        self.file.cursors.get(self.file.main_cursor).expect("invalid cursor")
                         .print(&mut self.file.area);

        self.file.area.flush();
    }
}

impl_input_handler!(FileHandler<T>, mappings);
