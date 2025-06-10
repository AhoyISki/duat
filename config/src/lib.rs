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
    print::wrap_on_edge();

    //// Hooks

    hook::remove("FileWidgets");
    // This hook lets you push widgets to the files.
    // The Pass is used to prevent ownership collisions.
    hook::add::<OnFileOpen>(|mut pa, builder| {
        // These widgets go on the left by default.
        builder.push(&mut pa, VertRule::cfg());
        builder.push(&mut pa, LineNumbers::cfg());
    });

    hook::remove("WindowWidgets");
    // Same, but on the edges of the window.
    hook::add::<OnWindowOpen>(|mut pa, builder| {
        // // Uncomment this and comment the rest for one line
        // // StatusLine PromptLine combo
        // let (child, _) = buider.push(PromptLine::cfg());
        // let status = status!(file_fmt " " mode_fmt " " selections_fmt " "
        // main_fmt); builder.push_to(child, status.right_ratioed(4,
        // 7)); builder.push_to(child, Notifier::cfg());

        // This function takes the mode and uppercases it.
        // The splitting is done to remove generic arguments.
        let mode_upper = mode_name().map(|mode| match mode.split_once('<') {
            Some((mode, _)) => mode.to_uppercase(),
            None => mode.to_uppercase(),
        });

        // Pushes a StatusLine to the bottom
        // // Square bracket pairs change the Form of text.
        builder.push(
            &mut pa,
            status!("[Mode]{mode_upper}{Spacer}{file_fmt} {selections_fmt} {main_fmt}"),
        );
        // Pushes a PromptLine to the bottom
        let (child, _) = builder.push(&mut pa, PromptLine::cfg());
        // By pushing this Notifications to the `child`, Duat will create a
        // parent that owns only the PromptLine and Notifier widgets.
        // With that, you can tell an Area to occupy the whole parent, "hiding
        // other sibling Areas. That's done in the "HidePromptLine" hook, for
        // example.
        builder.push_to(&mut pa, child, Notifier::cfg());
    });
    // // See what happens when you uncomment this hook removal:
    // hook::remove("HidePromptLine");

    // This hook will change the color of Kitty as
    // the ColorScheme is altered.
    hook::add::<ColorSchemeSet>(|_pa, scheme| {
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

    form::set_colorscheme("catppuccin-mocha");
}

/// This function is included in Duat, but it is implemented
/// here as a demonstration
fn file_fmt(file: &File) -> Text {
    if let Some(name) = file.name_set() {
        // A TextBuilder lets you build Text incrementally.
        let mut builder = Text::builder();
        // [] pairs change the Form of the text
        builder.push(text!("[File]{name}"));
        if !file.exists() {
            // Like in regular Rust formatting, double a "[" to escape it.
            builder.push(text!("[NewFile][[new file]]"));
        } else if file.text().has_unsaved_changes() {
            text!("[UnsavedChanges][[+]]");
        }
        builder.build()
    } else {
        // But you can also create Text directly
        // The second thing is a non identifier expression.
        text!("[ScratchFile]{}", file.name()).build()
    }
}
