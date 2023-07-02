#![allow(unused_imports)]
// Even when not on a terminal, Parsec (for the foreseable future)
// will use the crossterm crate in order to get a baseline access to
// terminal-like features, such as colors and control of the cursor's
// shape. Such features will also be present on any other frontends,
// and crossterm is an easy way to get access to them.
use crossterm::{
    cursor::SetCursorStyle,
    style::{ContentStyle, Stylize}
};
// parsec-core is the main crate for Parsec. It handles all the
// functionality, with the exception of frontend implementations.
use parsec_core::{
    // The config module handles the `Config` struct and the structs
    // `RwData` and `RoData`, useful for the extension  of Parsec.
    data::{RoData, RwData},
    // The input module handles remapping and input methods.
    // Remapping will always be done in the same way, and is not
    // implemented individually for every editing method.
    input::KeyRemapper,
    join,
    session::Parsec,
    // Tags are a really powerfull part of Parsec. For now,
    // they handle `Form`s (font styling), cursors, and wrapping,
    // but in the future, they will also allow the creation of buttons
    // and folding zones.
    tags::{
        form::FormPalette,
        form::{CursorStyle, Form}
    },
    text::{NewLine, PrintCfg, ScrollOff, WrapMethod},
    ui::{Constraint, ModNode, PushSpecs},
    widgets::{Align, CommandLine, FileWidget, LineNumbers, LineNumbersCfg, StatusLine},
    Controler
};
use parsec_kak::Editor;
use parsec_term::{SepChar, SepForm, Ui, VertRule, VertRuleCfg};

fn main() {
    // `FormPalette` is a struct with all of your `Form`s and
    // `CursorStyle`s in it.
    let mut palette = FormPalette::default();
    // A `CursorStyle` is a style unique to cursors. It contains a shape
    // (bar, block, or underscore), and a `Form` to be used when
    // printing the shape is not allowed (e.g. on a terminal, that
    // only has one cursor). Uncomment this if you want a yellow,
    // block like cursor.

    // `FormPalette`s, by default, contain some `Form`s in them.
    // You can modify `Form`s by using the `set_form()` method.
    // `add_form()` will panic if there is already a `Form` with that
    // name.
    // palette.set_main_cursor(CursorStyle::new(None,
    // Form::new().on_cyan()));
    palette.set_form("Mode", Form::new().dark_green());
    palette.set_form("VertRule", Form::new().dark_grey());
    palette.set_form("WrappedLineNumbers", Form::new().cyan().italic());
    palette.set_form("MainLineNumber", Form::new_final().dark_yellow().bold());
    palette.set_form("WrappedMainLineNumber", Form::new().yellow().italic());

    // The `Config` struct is a collection of common configuration options
    // for the end user.
    let print_cfg = PrintCfg {
        scrolloff: ScrollOff { x_gap: 5, y_gap: 5 },
        wrap_method: WrapMethod::Word,
        new_line: NewLine::AfterSpaceAs('↩'),
        ..PrintCfg::default()
    };

    // The `Editor` (in this case, the Kakoune editor) determines how the
    // program will be handle input to edit a file or any piece of
    // text.
    let editor = Editor::default();
    // A `Session` is essentially the application itself, it takes a `Ui`,
    // a `PrintCfg`, a `FormPalette`, and a closure that determines
    // what will happen when a new file is opened.
    let mut parsec = Parsec::new(Ui::default(), print_cfg, palette, move |mod_node, _file| {
        let sep_form = SepForm::uniform(mod_node.palette(), "VertRule");
        let cfg = VertRuleCfg::new(SepChar::Uniform('┃'), sep_form);
        mod_node.push_specd(VertRule::config_fn(cfg));

        let cfg = LineNumbersCfg::rel_abs(Align::Right, Align::Left);
        mod_node.push_specd(LineNumbers::config_fn(cfg));

        // let (child, _) =
        // mod_node.push_specd(StatusLine::default_fn());
        // mod_node.push_specd_to(CommandLine::default_fn(), child);
    });

    let (status_line, _) = parsec.push_specd(StatusLine::default_global_fn());
    let specs = PushSpecs::left(Constraint::Percent(50));
    parsec.cluster_to(CommandLine::default_fn(), status_line, specs);

    // that takes the input, remaps it, and sends it to the `Editor`.
    let mut file_remapper = KeyRemapper::new(editor);

    // Start Parsec.
    parsec.start(&mut file_remapper);
}
