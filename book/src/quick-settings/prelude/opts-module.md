# The `opts` module

This module contains a bunch of commonly used options. It covers settings for 
the various `Widget`s of Duat, most notably the `Buffer` widget, which is where 
editing takes place.

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // Default options for the Buffer widget
    opts::set(|opts| {
        opts.wrap_lines = false;
        opts.wrap_on_word = false;
        // Where to wrap, as opposed to at the rightmost edge.
        opts.wrapping_cap = None::<u32>;
        // Indent wrapped lines.
        opts.indent_wraps = true;
        opts.tabstop = 4;
        // Wether to print new lines as space characters.
        opts.print_new_line = true;
        // Minimum cursor distance from the top and bottom edges.
        opts.scrolloff.x = 3;
        // Minimum cursor distance from the left and right edges.
        opts.scrolloff.y = 3;
        opts.extra_word_chars = &[];
        // Forces scrolloff at the end of a line.
        opts.force_scrolloff = false;
        // Wether to allow showing ghosts (When LSP eventually comes, for example).
        opts.show_ghosts = true;
        // Allow scrolling until the Buffer shows only scrolloff.y lines.
        opts.allow_overscroll = false;
    });


    // Default options for the LineNumbers widget
    opts::set_lines(|opts| {
        // Relative as opposed to absolute numbering.
        opts.relative = false;
        // On which side to align numbers other than the main line's.
        opts.align = std::fmt::Alignment::Left;
        // On which side to align the main line's number.
        opts.main_align = std::fmt::Alignment::Right;
        // Place the LineNumbers on the right of the Buffer, instead of on the left.
        opts.on_the_right = false;
    });

    // Sets a Kakoune style "one line" footer
    opts::one_line_footer(false);

    // Place the footer on top, instead of on the bottom of the screen.
    opts::footer_on_top(false);

    // Default options for the StatusLine widget
    opts::set_status(|pa| {
        // If on one line footer mode:
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{AlignRight}{name_txt} {mode} {sels_txt} {param} {main_txt}");
        // If on regular mode (default):
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{mode} {name_txt}{Spacer}{sels_txt} {param} {main_txt}")
    });

    // Default options for the LogBook widget
    opts::set_logs(|opts| {

    });

    // Default options for the Notifications widget
    opts::set_notifs(|opts| {});
}
```

For more information about modification of the `StatusLine`, see the chapter on [modding the `StatusLine`]. For information on modding the `Notifications` and `LogBook` widgets, see the [`Text`] chapter

[modding the `StatusLine`]: ../../scripting-duat/mod-status.md
[`Text`]: ../../scripting-duat/text/chapter.md


