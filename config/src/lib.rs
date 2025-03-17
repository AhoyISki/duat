//! This is an example configuration file.
//! Note that
//! ```rust
//! setup_duat!(setup);
//! use duat::prelude::*;
//! fn setup() {}
//! ```
//! is a valid configuration.
setup_duat!(setup);
use duat::prelude::*;
// Since duat_kak is a plugin, it must be used explicitly.
// Plugins are usually imported with their "duat_" prefix removed.
use kak::{Insert, Normal};

fn setup() {
    plug!(
        treesitter::TreeSitter::new(),
        // This plugin sets the mode to Kakoune's Normal.
        kak::Kak::new(),
        // This one adds the Catppuccin colorschemes.
        // The modify function gives you access to the
        // colors from Catppuccin, so you can change
        // Forms with them.
        catppuccin::Catppuccin::new().modify(|c| {
            form::set_many!(
                ("MainCursorNormal", Form::with(c.base).on(c.text)),
                ("ExtraCursorNormal", Form::with(c.base).on(c.sapphire)),
                ("MainCursorInsert", Form::with(c.base).on(c.mauve)),
                ("ExtraCursorInsert", Form::with(c.base).on(c.yellow)),
                ("Default.StatusLine", Form::on(c.surface0))
            );
        })
    );

    // You can also set Forms to reference other Forms.
    form::set("MainCursorGoTo", "MainCursorNormal");
    form::set("ExtraCursorGoTo", "ExtraCursorNormal");

    // The print module configures file printing.
    print::wrap_on_width();

    //// Hooks

    hooks::remove("FileWidgets");
    // This hook lets you push widgets to the files.
    hooks::add::<OnFileOpen>(|builder| {
        // These widgets go on the left by default.
        builder.push(VertRule::cfg());
        builder.push(LineNumbers::cfg());
    });

    hooks::remove("WindowWidgets");
    // Same, but on the edges of the window.
    hooks::add::<OnWindowOpen>(|builder| {
        // // Uncomment this and comment the rest for a StatusLine on the side
        // // Square bracket pairs change the Form of text.
        // // The StatusLine goes on the bottom by default.
        // let (child, _) = builder.push(status!(
        //     file_fmt " " mode_fmt " " selections_fmt " " main_fmt
        // ));
        // let cmd_line = CmdLine::cfg().left_ratioed(3, 5);
        // // `push_to` pushes a widget to another.
        // builder.push_to(cmd_line, child);

        // This function takes the mode and uppercases it.
        // The splitting is done to remove generic arguments.
        let mode_upper = mode_name().map(|mode| match mode.split_once('<') {
            Some((mode, _)) => mode.to_uppercase(),
            None => mode.to_uppercase(),
        });

        // Pushes a StatusLine to the bottom
        builder.push(status!([Mode] mode_upper Spacer file_fmt " " selections_fmt " " main_fmt));
        // Pushes a CmdLine to the bottom
        let (child, _) = builder.push(CmdLine::cfg());
        // Pushes a Notifications to the bottom
        // The CmdLine widget has a hook that "hides" the Notifications widget
        // when CmdLine is in focus, this makes them "occupy the same space"
        builder.push_to(Notifications::cfg(), child);
    });
    // // See what happens when you uncomment this hook removal:
    // hooks::remove("HideCmdLine");

    // This hook will change the color of the Kitty as
    // the ColorScheme is altered.
    hooks::add::<ColorSchemeSet>(|scheme| {
        let scheme = match scheme {
            "catppuccin-latte" => "Catppuccin-Latte",
            "catppuccin-frappe" => "Catppuccin-Frappe",
            "catppuccin-macchiato" => "Catppuccin-Macchiato",
            "catppuccin-mocha" => "Catppuccin-Mocha",
            _ => return,
        };
        exec(format!("kitten themes {scheme}")).unwrap();
    });

    //// Remapping

    // A command from Vim, derived from one from Kakoune.
    map::<Normal>("gg", "gk");
    map::<Normal>("G", "gj");
    // Aliases show up on the screen as if they were text.
    alias::<Insert>("jk", "<Esc>");

    //// Commands

    // Adds a command for the LineNumbers widget.
    cmd::add_for!("toggle-relative", |ln: LineNumbers<Ui>, _: Area| {
        let mut cfg = ln.get_cfg();
        cfg.num_rel = match cfg.num_rel {
            LineNum::Abs => LineNum::RelAbs,
            LineNum::Rel | LineNum::RelAbs => LineNum::Abs,
        };
        ln.reconfigure(cfg);
        Ok(None)
    })
    .unwrap();

    form::set_colorscheme("catppuccin-mocha");
}

/// This function is included in Duat, but it is implemented
/// here as a demonstration
fn file_fmt(file: &File) -> Text {
    if let Some(name) = file.name_set() {
        // A TextBuilder lets you build Text incrementally.
        let mut b = Text::builder();
        // [] pairs change the Form of the text
        text!(b, [File] name);
        if !file.exists() {
            text!(b, [NewFile] "[new file]");
        } else if file.text().has_unsaved_changes() {
            text!(b, [UnsavedChanges] "[+]");
        }
        b.finish()
    } else {
        // But you can also create Text directly
        // The second thing is a non identifier expression.
        text!([ScratchFile] { file.name() })
    }
}
