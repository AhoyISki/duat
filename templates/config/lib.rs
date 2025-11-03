// This is an example configuration file.
// Note that
// ```rust
// setup_duat!(setup);
// use duat::prelude::*;
// fn setup() {}
// ```
// is a valid configuration.
setup_duat!(setup);
use duat::prelude::*;
// Since duat_kak is a plugin, it must be used explicitly.
// Plugins are usually imported with their "duat_" prefix removed.
use duat_kak::{Insert, Normal};

fn setup() {
    // This plugin sets the mode to Kakoune's Normal.
    // If you want a regular editing mode, you can remove this line.
    plug(duat_kak::Kak::new());
    // This one adds the Catppuccin colorschemes.
    plug(duat_catppuccin::Catppuccin::new());

    // You can also set Forms to reference other Forms.
    form::set("caret.main.OneKey", "caret.main.Normal");
    form::set("caret.extra.OneKey", "caret.extra.Normal");
    // Disables the cursor's shape.
    cursor::unset();

    opts::set(|opts| {
        opts.wrap_lines = true;
        opts.scrolloff.y = 5;
    });

    opts::set_lines(|opts| {
        opts.align = std::fmt::Alignment::Right;
    });

    opts::set_status(|pa| {
        let upper_mode = mode_name().map(|m| m.to_uppercase());

        status!("[mode]{upper_mode}{Spacer}{custom_name_txt} {sels_txt} {main_txt}")
    });

    //// Remapping

    // A command from Vim, derived from one from Kakoune.
    map::<Normal>("gg", "gk");
    map::<Normal>("G", "gj");
    // Aliases show up on the screen as if they were text.
    alias::<Insert>("jk", "<Esc>");
    // // You may highlight them by uncommenting this line:
    // form::set("alias", Form::red());

    form::set_colorscheme("catppuccin-mocha");
}

/// A custom function to show the name differently.
fn custom_name_txt(file: &Buffer) -> Text {
    if let Some(name) = file.name_set() {
        // A TextBuilder lets you build Text incrementally.
        let mut builder = Text::builder();
        // [] pairs change the Form of the text
        builder.push(txt!("[file]{name}"));

        if !file.exists() {
            // Like in regular Rust formatting, double a "[" to escape it.
            builder.push(txt!("[file.new][[new file]]"))
        } else if file.text().has_unsaved_changes() {
            builder.push(txt!("[file.unsaved][[+]]"))
        }

        match file.filetype() {
            Some("rust") => builder.push('🦀'),
            Some("python") => builder.push('🐍'),
            Some("perl") => builder.push('🐫'),
            Some("swift") => builder.push('🐦'),
            _ => {}
        };

        builder.build()
    } else {
        // But you can also create Text directly
        // The second thing is a non identifier expression.
        txt!("[file.new.scratch]{}", file.name())
    }
}
