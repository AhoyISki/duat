#![allow(unused_imports)]

use std::fmt::Alignment;

// Even when not on a terminal, Parsec (for the foreseable future) will use the crossterm crate in
// order to get a baseline access to terminal-like features, such as colors and control of the
// cursor's shape. Such features will also be present on any other frontends, and crossterm is an
// easy way to get access to them.
use crossterm::style::{ContentStyle, Stylize};

// parsec-core is the main crate for Parsec. It handles all the functionality, with the exception of
// frontend implementations.
use parsec_core::{
    // The config module handles the `Config` struct and the structs `RwData` and `RoData`, useful
    // for the extension  of Parsec.
    config::{Config, ScrollOff, WrapMethod},
    form_status,
    // The input module handles remapping and input methods. Remapping will always be done in the
    // same way, and is not implemented individually for every editing method.
    input::KeyRemapper,
    // Tags are a really powerfull part of Parsec. For now, they handle `Form`s (font styling),
    // cursors, and wrapping, but in the future, they will also allow the creation of buttons and
    // folding zones.
    tags::{
        form::FormPalette,
        form::{CursorStyle, Form},
        MatchManager, LineFlags,
    },
    ui::{Direction, Split},
    widgets::{line_numbers::{LineNumbers, LineNumbersConfig, Numbering}, command_line::CommandLine},
    Session,
};
use parsec_kak::Editor;
use parsec_term::{UiManager, VertRule};

fn main() {
    // The `Config` struct is a collection of common configuration options for the end user.
    let config = Config {
        scrolloff: ScrollOff { d_x: 5, d_y: 5 },
        wrap_method: WrapMethod::Width,
        wrap_indent: true,
        ..Config::default()
    };

    // `FormPalette` is a struct with all of your `Form`s and `CursorStyle`s in it.
    let mut palette = FormPalette {
        // A `CursorStyle` is a style unique to cursors. It contains a shape (bar, block, or
        // underscore), and a `Form` to be used when printing the shape is not allowed (e.g. on a
        // terminal, that only has one cursor).
        // Uncomment this if you want a yellow, block like cursor.
        //main_cursor: CursorStyle::new(None, Form::new(ContentStyle::new().on_yellow(), true)),
        ..Default::default()
    };

	// `FormPalette`s, by default, contain some `Form`s in them.
	// You can modify `Form`s by using the `set_form()` method.
    palette.set_form(
        "MainLineNumber",
        Form::new(ContentStyle::new().dark_yellow(), false),
    );
    // `add_form()` will panic if there is already a `Form` with that name.
    palette.add_form("File", Form::new(ContentStyle::new().dark_yellow(), false));
    palette.add_form(
        "Selections",
        Form::new(ContentStyle::new().dark_blue(), false),
    );
    palette.add_form("Number", Form::new(ContentStyle::new().yellow(), false));
    palette.add_form("Separator", Form::new(ContentStyle::new().cyan(), false));
    palette.add_form("Mode", Form::new(ContentStyle::new().dark_green(), false));

	// A `Session` is essentially the application itself, it will take arguments such as a user
	// provided `Config` and `FormPalette` in order to start the program.
    let mut session = Session::new(
        UiManager::new(),
        // The `MatchManager will eventually be the method of dynamically coloring the program. It
        // is very much a work in progress.
        MatchManager::new(),
        config,
        palette,
        // For now, this `Direction` specifies where to put the "global" status line.
        Direction::Bottom,
        // And this `Split` dictates a lenght of 1 for that `Direction`. As it is `Static and not
        // `Locked`, you could change its value later.
        Split::Static(1),
    );

	session.push_widget_to_edge(CommandLine::default, Direction::Bottom, Split::Static(1));
	// The method `push_node_to_file()` will "push" a widget, given a node, to every single future
	// file. In this case, we are pushing a `VertRule` to every future file, on the `Left`, with a
	// split of 1. This is what it would do:
	//
	// ╭────────────────────╮       ╭────────────────────╮
	// │Hello World         │       ││Hello World        │
	// │This is my file     │  ==>  ││This is my file    │
	// │It is very short    │       ││It is very short   │
	// ╰────────────────────╯       ╰────────────────────╯
	// 
	// This will happen to every file that is opened AFTER this method is used. It does not apply
	// to files that were opened before then.
    session.push_widget_to_file(
        Box::new(VertRule::default),
        Direction::Left,
        Split::Static(1),
    );

	// Now, we push another widget to every file, that being the `LineNumbers` widget. This is what
	// that would do:
	//
	// ╭────────────────────╮       ╭────────────────────╮
	// ││Hello World        │       │1│Hello World       │
	// ││This is my file    │  ==>  │2│This is my file   │
	// ││It is very short   │       │3│It is very short  │
	// ╰────────────────────╯       ╰────────────────────╯
	//
    session.push_widget_to_file(
        // `LineNumbers::default` is a method with the following signature:
        // 
        // Fn(
        //     RwData<EndNode<U>>, &mut NodeManager<U>, RwData<FileWidget<U>>
        // ) -> Arc<Mutex<dyn Widget<U>>>
        // where
        //     U: UiManager
        //
        // This is the type of method that will need to be provided for the end user to be able to
        // push widgets to files. If you know Rust, you know that this could also be acomplished
        // with the use of closures.
        Box::new(LineNumbers::default),
        Direction::Left,
        Split::Static(1),
    );
	// By replacing the above piece of code with the one below:
	//
    // let config = LineNumbersConfig {
    //     alignment: Alignment::Right,
    //     numbering: Numbering::Relative,
    // };
    // session.push_node_to_file(
    //     Box::new(move |end_node, node_manager, file_widget| {
    //         LineNumbers::new(end_node, node_manager, file_widget, config)
    //     }),
    //     Direction::Left,
    //     Split::Static(2),
    // );
	//
	// You would end up with this instead:
	//
	// ╭────────────────────╮       ╭────────────────────╮
	// ││Hello World        │       │ 0│Hello World      │
	// ││This is my file    │  ==>  │ 1│This is my file  │
	// ││It is very short   │       │ 2│It is very short │
	// ╰────────────────────╯       ╰────────────────────╯
	//
	// The line numbers now represent the relative distance to the number of the cursor's line
	// (line 1). And notice the `Split::Static(2)`, it is the reason the `LineNumbers` widget now
	// occupies 2 cells of width. That is the minimum width it will now have.
	//
	// NOTE: If you added this code, without removing the first one, you would end up with two
	// `LineNumbers`, which is weird, but is something you can do.

	// The `Editor` (in this case, the Kakoune editor) is the thing that determines how the program
	// will be handle input to edit a file or any piece of text.
    let editor = Editor::default();

	// The `form_status!` macro takes a `StatusLine` widget and formats it in the desired fashion.
    form_status!(
        // The first argument is the `StatusLine` widget, in this case, `session.status`.
        session.status => {
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
            right: "[File]() [Mode]{} [Selections]() sel [Number]()[Separator]:[Number]()[Separator]/[Number]()",
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
        }
    );

	// The `KeyRemapper` is an intermediary struct that takes the input, remaps it, and sends it to
	// the `Editor`.
    let mut file_remapper = KeyRemapper::new(editor);

	// Opens the files provided as arguments in the command line.
    session.open_arg_files();

	// Start Parsec.
    session.application_loop(&mut file_remapper);
}
