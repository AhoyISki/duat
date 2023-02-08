use crossterm::style::{ContentStyle, Stylize};
use parsec_core::{
    config::{Config, ScrollOff, WrapMethod},
    form_status,
    input::KeyRemapper,
    tags::{
        form::FormPalette,
        form::{CursorStyle, Form},
        MatchManager,
    },
    ui::{Direction, Split},
    widgets::{line_numbers::LineNumbers, Session},
};
use parsec_kak::Editor;
use parsec_term::{UiManager, VertRule};

fn main() {
    let config = Config {
        scrolloff: ScrollOff { d_x: 5, d_y: 5 },
        wrap_method: WrapMethod::Width,
        wrap_indent: true,
        ..Config::default()
    };

    let mut palette = FormPalette {
        //main_cursor: CursorStyle::new(None, Form::new(ContentStyle::new().on_dark_yellow(), true)),
        ..Default::default()
    };

    palette.set_form(
        "MainLineNumber",
        Form::new(ContentStyle::new().dark_yellow(), false),
    );
    palette.add_form("File", Form::new(ContentStyle::new().dark_yellow(), false));
    palette.add_form(
        "Selections",
        Form::new(ContentStyle::new().dark_blue(), false),
    );
    palette.add_form("Number", Form::new(ContentStyle::new().yellow(), false));
    palette.add_form("Separator", Form::new(ContentStyle::new().cyan(), false));
    palette.add_form("Mode", Form::new(ContentStyle::new().dark_green(), false));

    let mut session = Session::new(
        UiManager::new(),
        MatchManager::new(),
        config,
        palette,
        Direction::Bottom,
        Split::Static(1),
    );

    session.push_node_to_file(
        Box::new(VertRule::default),
        Direction::Left,
        Split::Static(1),
    );

    session.push_node_to_file(
        Box::new(LineNumbers::default),
        Direction::Left,
        Split::Static(1),
    );

    let editor = Editor::new();

    form_status!(
        session.status => {
            left: "",
            center: "",
            right: "[File]() [Mode]{} [Selections]() sel [Number]()[Separator]:[Number]()[Separator]/[Number]()",
            file_vars: [
                (|file| file.name()),
                (|file| file.cursors().len()),
                (|file| file.main_cursor().row()),
                (|file| file.main_cursor().col()),
                (|file| file.len())
            ],
            global_vars: [
                (editor.cur_mode())
            ]
        }
    );

    let mut file_remapper = KeyRemapper::new(editor);

    session.open_arg_files();

    session.application_loop(&mut file_remapper);
}
