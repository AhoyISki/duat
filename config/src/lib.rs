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
        treesitter::TreeSitter,
        match_pairs::MatchPairs::new(),
        // This plugin sets the mode to Kakoune's Normal.
        kak::Kak::new(),
        // This one adds the Catppuccin colorschemes.
        // The modify function gives you access to the
        // colors from Catppuccin, so you can change
        // Forms with them.
        catppuccin::Catppuccin::new().modify(|c| {
            form::set_many!(
                ("caret.main.Normal", Form::with(c.base).on(c.text)),
                ("caret.extra.Normal", Form::with(c.base).on(c.sapphire)),
                ("caret.main.Insert", Form::with(c.base).on(c.mauve)),
                ("caret.extra.Insert", Form::with(c.base).on(c.yellow)),
            );
        })
    );

    // You can also set Forms to reference other Forms.
    form::set("caret.main.OneKey", "caret.main.Normal");
    form::set("caret.extra.OneKey", "caret.extra.Normal");
    // Disables the cursor's shape.
    cursor::unset();

    // The print module configures file printing.
    print::wrap_on_edge();

    //// Hooks
    // Changes every LineNumbers Widget.
    hook::add::<WidgetCreated<LineNumbers<Ui>>>(|_, (ln, _)| {
        ln.align_right().align_main_left().rel_abs()
    });

    // The WindowWidgets hook group defines Widgets to be placed on
    // windows. I can get rid of it, and push my own Widgets instead
    hook::remove("WindowWidgets");
    hook::add::<OnWindowOpen>(|pa, builder| {
        // This function takes the mode and uppercases it.
        // The splitting is done to remove generic arguments.
        let mode = mode_name().map(|mode| match mode.split_once('<') {
            Some((mode, _)) => txt!("[mode]{}", mode.to_uppercase()).build(),
            None => txt!("[mode]{}", mode.to_uppercase()).build(),
        });

        // Pushes a StatusLine, PromptLine, and Notifications combo to the
        // bottom
        // Square bracket pairs change the Form of text.
        builder.push(
            pa,
            FooterWidgets::new(status!(
                "[mode]{mode}{Spacer}{file_fmt} {sels_fmt} {main_fmt}"
            )),
            // FooterWidgets also has the `one_line` method, if you want a one line combo,
            // instead of the usual two. Uncomment this for that
            // FooterWidgets::default().one_line()
        );
    });
    // // See what happens when you uncomment this hook removal:
    // hook::remove("HidePromptLine");

    //// Remapping

    // A command from Vim, derived from one from Kakoune.
    map::<Normal>("gg", "gk");
    map::<Normal>("G", "gj");
    // Aliases show up on the screen as if they were text.
    alias::<Insert>("jk", "<Esc>");
    // // You may see them by uncommenting this line:
    // form::set("alias", Form::red());

    form::set_colorscheme("catppuccin-mocha");
}

/// This function is included in Duat, but it is implemented
/// here as a demonstration
fn file_fmt(file: &File) -> Text {
    if let Some(name) = file.name_set() {
        // A TextBuilder lets you build Text incrementally.
        let mut builder = Text::builder();
        // [] pairs change the Form of the text
        builder.push(txt!("[file]{name}"));
        if !file.exists() {
            // Like in regular Rust formatting, double a "[" to escape it.
            builder.push(txt!("[file.new][[new file]]"));
        } else if file.text().has_unsaved_changes() {
            txt!("[file.unsaved][[+]]");
        }
        builder.build()
    } else {
        // But you can also create Text directly
        // The second thing is a non identifier expression.
        txt!("[file.new.scratch]{}", file.name()).build()
    }
}
