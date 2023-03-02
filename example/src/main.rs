#![allow(unused_imports)]

use std::fmt::Alignment;

// Even when not on a terminal, Parsec (for the foreseable future) will use the crossterm crate
// in order to get a baseline access to terminal-like features, such as colors and control of
// the cursor's shape. Such features will also be present on any other frontends, and crossterm
// is an easy way to get access to them.
use crossterm::style::{ContentStyle, Stylize};
// parsec-core is the main crate for Parsec. It handles all the functionality, with the
// exception of frontend implementations.
use parsec_core::{
    // The config module handles the `Config` struct and the structs `RwData` and `RoData`, useful
    // for the extension  of Parsec.
    config::{Config, RoData, RwData, ScrollOff, WrapMethod},
    // The input module handles remapping and input methods. Remapping will always be done in the
    // same way, and is not implemented individually for every editing method.
    input::KeyRemapper,
    status_format,
    // Tags are a really powerfull part of Parsec. For now, they handle `Form`s (font styling),
    // cursors, and wrapping, but in the future, they will also allow the creation of buttons and
    // folding zones.
    tags::{
        form::FormPalette,
        form::{CursorStyle, Form},
        LineFlags, MatchManager,
    },
    ui::{ModNode, Side, Split},
    widgets::{
        command_line::CommandLine,
        file_widget::FileWidget,
        line_numbers::{LineNumbers, LineNumbersConfig, Numbering},
        status_line::StatusFormat,
    },
    Session,
};
use parsec_kak::Editor;
use parsec_term::{Ui, VertRule};

fn main() {
    // `FormPalette` is a struct with all of your `Form`s and `CursorStyle`s in it.
    let mut palette = FormPalette::default();
    //palette.set_main_cursor(CursorStyle::new(None, Form::new(true).on_yellow()));
    // A `CursorStyle` is a style unique to cursors. It contains a shape (bar, block,
    // or underscore), and a `Form` to be used when printing the shape is not allowed (e.g. on a
    // terminal, that only has one cursor).
    // Uncomment this if you want a yellow, block like cursor.

    // `FormPalette`s, by default, contain some `Form`s in them.
    // You can modify `Form`s by using the `set_form()` method.
    palette.set_form("MainLineNumber", Form::new(false).on_dark_yellow());
    // `add_form()` will panic if there is already a `Form` with that name.
    palette.add_form("Separator", Form::new(false).cyan());
    palette.add_form("Mode", Form::new(false).dark_green());

    // The `Config` struct is a collection of common configuration options for the end user.
    let config = Config {
        scrolloff: ScrollOff { d_x: 5, d_y: 5 },
        wrap_method: WrapMethod::Width,
        wrap_indent: true,
        ..Config::default()
    };

    // The `Editor` (in this case, the Kakoune editor) is the thing that determines how the program
    // will be handle input to edit a file or any piece of text.
    let editor = Editor::default();

    // The `form_status!` macro takes a `StatusLine` widget and formats it in the desired fashion.
    let status_format: StatusFormat<Ui> = status_format!(
        &config.palette,
        // You can put text in the left, right, or center of the status line. If there is not
        // enough space for all of the sides, some will be ommited.
        left: "",
        center: "",
        // These are the symbols to format your status line:
        // - {}: These are global variables, you must place an entry on "global_vars" for each
        //   of these you put on the text.
        // - (): These are file variables, they refer to information in the file that this
        //   `StatusLine` is associated with. The file in question may change. You must place on
        //   entry on "file_vars" for each of these you put on the text.
        // - [<form_name>]: This will invoke <form_name> as the `Form` to be applied to the text
        //   after this point. A second use of [<form_name_2>] will change the form again. If
        //   a given <form_name> doesn't exist, Parsec will panic.
        right: "[FileName]() [Mode]{} [Selections]() sel [Coords]()[Separator]:[Coords]()[Separator]/[Coords]()",
        // Each "file_var" is essentially a method to apply to a `FileWidget` that must return
        // either a `String` or something that implements `ToString`. As these can be closures,
        // they permit a great level of power as to what will be show up.
        file_vars: [
            (|file| file.name()),
            (|file| file.cursors().len()),
            (|file| file.main_cursor().row()),
            (|file| file.main_cursor().col()),
            (|file| file.len())
        ],
        // Each "global_var" consists of 2 parts:
        // - An object in the form of `RoData<T>`.
        // - A method that returns a `String` from `T`.
        // `RoData` is, in essence, a multithreaded struct capable of holding data that can be
        // updated and read. It is the counterpart of `RwData`, as in, an `RwData<T>` can read
        // and modify `T`, while an `RoData<T>`, created from `&RwData<T>`, will only be able to
        // read `T`. These are the 2 structs that permit a great level of flexibility in sharing
        // information without the process becoming too cumbersome.
        global_vars: [
            // In this case, `Editor::cur_mode()` returns an `RoData<D> where D: Display`. We
            // don't need to provide a closure in order to return a `String`, `form_status!`
            // will automatically assume that `D: Display`.
            (editor.cur_mode())
            // Assuming that, earlier in the code, we wrote this `RwData` from a tuple:
            // let my_var = RwData::new((String::from("text"), ()));
            // let read_only = RoData::from(&my_var);
            //
            // Since `read_only.0` implements `ToString`, we would write the following:
            // (|read_only| read_only.0)
        ]
    );

    // A `Session` is essentially the application itself, it will take arguments such as a user
    // provided `Config` and `FormPalette` in order to start the program.
    let mut session = Session::new(
        Ui::default(),
        // The `MatchManager will eventually be the method of dynamically coloring the program. It
        // is very much a work in progress.
        config,
        Box::new(|mut mod_node, file| {
            mod_node.push_widget(LineNumbers::default(file), Side::Left, Split::Minimum(1), true);
        }),
    );

    //session.push_widget_to_edge(CommandLine::default, Side::Bottom, Split::Locked(1));
    // The `KeyRemapper` is an intermediary struct that takes the input, remaps it, and sends it to
    // the `Editor`.
    let mut file_remapper = KeyRemapper::new(editor);

    // Start Parsec.
    session.start_parsec(&mut file_remapper);
}
