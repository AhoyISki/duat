#![allow(unused_imports)]

use std::fmt::Alignment;

// Even when not on a terminal, Parsec (for the foreseable future)
// will use the crossterm crate in order to get a baseline access to
// terminal-like features, such as colors and control of the cursor's
// shape. Such features will also be present on any other frontends,
// and crossterm is an easy way to get access to them.
use crossterm::style::{ContentStyle, Stylize};
// parsec-core is the main crate for Parsec. It handles all the
// functionality, with the exception of frontend implementations.
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
    },
    ui::{ModNode, PushSpecs, Side, Split},
    widgets::{
        command_line::CommandLine,
        file_widget::FileWidget,
        line_numbers::{LineNumbers, LineNumbersCfg, Numbering},
        status_line::StatusLine,
    },
    Session,
};
use parsec_kak::Editor;
use parsec_term::{SepChar, SepForm, Ui, VertRule, VertRuleCfg};

fn main() {
    // `FormPalette` is a struct with all of your `Form`s and
    // `CursorStyle`s in it.
    let mut palette = FormPalette::default();
    palette.set_main_cursor(CursorStyle::new(None, Form::new(true).black().on_yellow()));
    palette.set_form("MainLineNumber", Form::new(true).dark_magenta().on_dark_grey());
    // A `CursorStyle` is a style unique to cursors. It contains a shape
    // (bar, block, or underscore), and a `Form` to be used when
    // printing the shape is not allowed (e.g. on a terminal, that
    // only has one cursor). Uncomment this if you want a yellow,
    // block like cursor.

    // `FormPalette`s, by default, contain some `Form`s in them.
    // You can modify `Form`s by using the `set_form()` method.
    // `add_form()` will panic if there is already a `Form` with that
    // name.
    palette.add_form("Mode", Form::new(false).dark_green());
    palette.add_form("VertRule", Form::new(false).dark_grey());
    let (_, id) = palette.get_from_name("VertRule").unwrap();

    // The `Config` struct is a collection of common configuration options
    // for the end user.
    let config = Config {
        scrolloff: ScrollOff { x_gap: 5, y_gap: 5 },
        wrap_indent: true,
        palette,
        ..Config::default()
    };

    // The `Editor` (in this case, the Kakoune editor) is the thing that
    // determines how the program will be handle input to edit a file
    // or any piece of text.
    let editor = Editor::default();

    // A `Session` is essentially the application itself, it will take
    // arguments such as a user provided `Config` and `FormPalette` in
    // order to start the program.
    let mut session = Session::new(
        Ui::default(),
        config,
        Box::new(move |mut mod_node, file| {
            let push_specs = PushSpecs::new(Side::Left, Split::Locked(1), true);
            let cfg = VertRuleCfg {
                sep_char: SepChar::TwoWay('▋', '┃'),
                sep_form: SepForm::Uniform(id),
            };
            mod_node.push_widget(VertRule::config_fn(file.clone(), cfg), push_specs);

            let push_specs = PushSpecs::new(Side::Left, Split::Min(1), true);
            let cfg = LineNumbersCfg {
                alignment: Alignment::Right,
                numbering: Numbering::Absolute,
            };
            mod_node.push_widget(LineNumbers::config_fn(file.clone(), cfg), push_specs);

            let push_specs = PushSpecs::new(Side::Right, Split::Locked(1), true);
            let cfg = VertRuleCfg {
                sep_char: SepChar::TwoWay('▋', '┃'),
                sep_form: SepForm::Uniform(id),
            };
            mod_node.push_widget(VertRule::config_fn(file.clone(), cfg), push_specs);

            let push_specs = PushSpecs::new(Side::Right, Split::Min(1), true);
            let cfg = LineNumbersCfg {
                alignment: Alignment::Right,
                numbering: Numbering::Absolute,
            };
            mod_node.push_widget(LineNumbers::config_fn(file.clone(), cfg), push_specs);

            let push_specs = PushSpecs::new(Side::Bottom, Split::Min(1), true);
            mod_node.push_widget(StatusLine::default_fn(file), push_specs);
        }),
    );

    // session.push_widget_to_edge(CommandLine::default, Side::Bottom,
    // Split::Locked(1)); The `KeyRemapper` is an intermediary struct
    // that takes the input, remaps it, and sends it to the `Editor`.
    let mut file_remapper = KeyRemapper::new(editor);

    // Start Parsec.
    session.start_parsec(&mut file_remapper);
}
