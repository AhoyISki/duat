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
        // Adds a TsParser Reader, which enables syntax highlighting.
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
        // // Uncomment this and comment the rest for one line
        // // StatusLine PromptLine combo
        // let (child, _) = buider.push(PromptLine::cfg());
        // let status = status!(file_fmt " " mode_fmt " " selections_fmt " " main_fmt);
        // builder.push_to(child, status.right_ratioed(4, 7));
        // builder.push_to(child, Notifier::cfg());

        // This function takes the mode and uppercases it.
        // The splitting is done to remove generic arguments.
        let mode_upper = mode_name().map(|mode| match mode.split_once('<') {
            Some((mode, _)) => mode.to_uppercase(),
            None => mode.to_uppercase(),
        });

        // Pushes a StatusLine to the bottom
        // // Square bracket pairs change the Form of text.
        builder.push(status!([Mode] mode_upper Spacer file_fmt " " selections_fmt " " main_fmt));
        // Pushes a PromptLine to the bottom
        let (child, _) = builder.push(PromptLine::cfg());
        // By pushing this Notifications to the `child`, Duat will create a
        // parent that owns only the PromptLine and Notifications widgets.
        // With that, you can tell an Area to occupy the whole parent, "hiding
        // other sibling Areas. That's done in, e.g., the "HidePromptLine" hook.
        builder.push_to(child, Notifier::cfg());
    });
    // // See what happens when you uncomment this hook removal:
    // hooks::remove("HidePromptLine");

    // This hook will change the color of Kitty as
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
    // // You may see them by uncommenting this line:
    // form::set("Alias", Form::red());

    //// Commands

    // Adds a command for the LineNumbers widget.
    cmd::add_for!(["toggle-relative", "tr"], |ln: LineNumbers<Ui>, _: Area| {
        let opts = ln.options_mut();
        opts.num_rel = match opts.num_rel {
            LineNum::Abs => LineNum::RelAbs,
            LineNum::Rel | LineNum::RelAbs => LineNum::Abs,
        };
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
