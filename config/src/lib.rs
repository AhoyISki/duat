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
        // This plugin sets the Kakoune mode.
        kak::Kak::new(),
        // This one adds the Catppuccin colorschemes.
        // The modify function gives you access to the
        // colors from Catppuccin, so you can change
        // Forms with them.
        catppuccin::Catppuccin::new().modify(|c| {
            form::set("MainCursorNormal", Form::with(c.base).on(c.text));
            form::set("ExtraCursorNormal", Form::with(c.base).on(c.sapphire));
            form::set("MainCursorInsert", Form::with(c.base).on(c.mauve));
            form::set("ExtraCursorInsert", Form::with(c.base).on(c.yellow));
        })
    );
    form::set_colorscheme("catppuccin-mocha");

    // You can also set Forms to reference other Forms.
    form::set("MainCursorGoTo", "MainCursorNormal");
    form::set("ExtraCursorGoTo", "ExtraCursorNormal");

    // The print module configures file printing.
    print::wrap_on_width();

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
        // "[" "]" pairs change the style of text.
        let (child, _) = builder.push(status!(
            [File] { File::name } " "
            mode_fmt " " selections_fmt " " main_fmt
        ));
        let cmd_line = CmdLine::cfg().left_ratioed(3, 5);
        // `push_to` pushes a widget to another.
        builder.push_to(cmd_line, child);
    });
    // Maps don't send any key if the sequence fails.
    map::<Normal>("gg", "gk");
    map::<Normal>("G", "gj");
    // Aliases show up on the screen as if they were text.
    alias::<Insert>("jk", "<Esc>");
    // Adds a command for the LineNumbers widget.
    cmd::add_for!("toggle-relative", |ln: LineNumbers, _, _| {
        let mut cfg = ln.get_cfg();
        cfg.num_rel = match cfg.num_rel {
            LineNum::Abs => LineNum::RelAbs,
            LineNum::Rel | LineNum::RelAbs => LineNum::Abs,
        };
        ln.reconfigure(cfg);
        Ok(None)
    })
    .unwrap();
}
