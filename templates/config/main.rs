// This is an example configuration buffer.
// Note that
// ```rust
// setup_duat!(setup);
// use duat::prelude::*;
// fn setup() {}
// ```
// is a valid configuration.
setup_duat!(setup);
use duat::prelude::*;

fn setup(opts: &mut Opts) {
    // Disables the cursor's shape.
    cursor::unset();

    //// Setting some options.

    opts.wrap_lines = true;
    opts.scrolloff.y = 5;
    opts.linenumbers.align = std::fmt::Alignment::Right;
    opts.linenumbers.main_align = std::fmt::Alignment::Left;
    opts.fmt_status(|_| {
        let upper_mode = mode_name().map(|m| m.to_uppercase());
        status!("[mode]{upper_mode}{Spacer}{custom_name_txt} {sels_txt} {main_txt}")
    });

    //// Remapping

    // Aliases show up on the screen as if they were text.
    alias::<Insert>("jk", "<Esc>");
    // You can also map to functions directly.
    map::<Normal>("<s-s>", |pa: &mut Pass| {
        _ = context::current_buffer(pa).save(pa);
    });

	// On Rust, diagnostics rely on buffer saving, so this hook should
	// provide a reload whenever switching to Normal mode.
    hook::add::<ModeSwitched>(|pa, switch| {
        if switch.new.is::<Normal>()
            && let buffer = context::current_buffer(pa)
            && let Some("rust") = buffer.filetype(pa)
        {
            _ = buffer.save(pa);
        }
    });

    colorscheme::set("catppuccin-mocha");
}

/// A custom function to show the name differently.
fn custom_name_txt(buffer: &Buffer) -> Text {
    if let Some(name) = buffer.name_set() {
        // A TextBuilder lets you build Text incrementally.
        let mut builder = Text::builder();
        // [] pairs change the Form of the text
        builder.push(txt!("[buffer]{name}"));

        if !buffer.exists() {
            // Like in regular Rust formatting, double a "[" to escape it.
            builder.push(txt!("[buffer.new][[new buffer]]"))
        } else if buffer.has_unsaved_changes() {
            builder.push(txt!("[buffer.unsaved][[+]]"))
        }

        match buffer.filetype() {
            Some("rust") => builder.push(" 🦀"),
            Some("python") => builder.push(" 🐍"),
            Some("perl") => builder.push(" 🐫"),
            Some("swift") => builder.push(" 🐦"),
            _ => {}
        };

        builder.build()
    } else {
        // But you can also create Text directly
        // The second thing is a non identifier expression.
        txt!("[buffer.new.scratch]{} ✍", buffer.name())
    }
}
