# The `opts` module

This module contains a bunch of commonly used options. It covers settings for 
the various `Widget`s of Duat, most notably the `Buffer` widget, which is where 
editing takes place.

Below are the available functions on this module, as well as their default 
values.

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // Default options for the Buffer widget
    opts::set(|opts| {
        // Buffer options:
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
        
        // General settings:
        // Place the bottom widgets on the top of the screen.
        opts.footer_on_top = false;
        // Make the bottom widgets take up only one line of space.
        opts.one_line_footer = false;
        // Shows available keybindings
        opts.help_key = Some(KeyEvent::new(KeyCode::Char('?'), mode::KeyMod::CONTROL));
        
        // duatmode settings:
        // Inserts a \t instead of spaces when pressing Tab
        opts.duatmode.insert_tabs = false;
        // How to handle the Tab key
        opts.duatmode.tab_mode = opts::TabMode::VerySmart;
        // Auto indent new lines on tree-sitter Buffers
        opts.duatmode.auto_indent = true;
        // Characters that trigger a reindentation
        opts.duatmode.indent_chars = &['\n', '(', ')', '{', '}', '[', ']'];
        // Reindent when pressing 'I' in normal mode
        opts.duatmode.indent_on_capital_i = true;
        // Makes the 'f' and 't' keys set the search pattern
        opts.duatmode.f_and_t_set_search = true;
        // Bracket pairs to be considered by keys like 'm' and the 'u' object
        opts.duatmode.set_brackets([["(", ")"], ["{", "}"], ["[", "]"]]);
        
        // LineNumbers options:
        opts.line_numbers.relative = false;
        // Where to align the numbers
        opts.line_numbers.align = std::fmt::Alignment::Left;
        // Where to align the main line number
        opts.line_numbers.main_align = std::fmt::Alignment::Right;
        // Wether to show wrapped line's numbers
        opts.line_numbers.show_wraps = false;
        // Place the widget on the right, as opposed to on the left
        opts.line_numbers.on_the_right = false;
        
        // Notifications options:
        // Reformat the notifications messages
        opts.notifications.fmt(|rec| todo!("default fmt function"));
        // Which mask to use to show the messages
        opts.notifications.set_mask(|rec| todo!("error for error, info for info, etc"));
        // Which log levels will actually show up on the notifications
        opts.notifications.set_allowed_levels([
            context::Level::Error,
            context::Level::Warn,
            context::Level::Info
        ]);
        
        // WhichKey options:
        // How to format each keybinding entry on the widget
        opts.whichkey.fmt(|desc| todo!("default fmt function"));
        // Disable the widget for the given Mode
        // opts.whichkey.disable_for::<{Mode in question}>();
        // Always show the widget for the given Mode
        opts.whichkey.always_show::<User>();
        // Removes the Mode from the disable_for and always_show lists
        // opts.whichkey.show_normally::<{Mode in question}>();
        
        // LogBook options:
        // How to format each message
        opts.logs.fmt(|rec| todo!("default log fmt"));
        opts.logs.close_on_unfocus = true;
        // It can be shown via the "logs" command
        opts.logs.hidden = false;
        // Where to place it
        opts.logs.side = ui::Side::Below;
        // Is ignored when the side is Left or Right
        opts.logs.height = 8.0;
        // Is ignored when the side is Above or Below
        opts.logs.width = 50.0;
        // Wether to show the source of the message (on the default fmt)
        opts.logs.show_source = true;
    });

    // Default options for the StatusLine widget
    opts::fmt_status(|pa| {
        // If on one line footer mode:
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{Spacer}{name_txt} {mode} {sels_txt} {param} {main_txt}");
        // If on regular mode (default):
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{mode} {name_txt}{Spacer}{sels_txt} {param} {main_txt}")
    });
}
```

For more information about modification of the `StatusLine`, see the chapter on 
[modding the `StatusLine`]. For information on modding the `Notifications` and 
`LogBook` widgets, see the [`Text`] chapter

[modding the `StatusLine`]: ../../scripting-duat/mod-status.md
[`Text`]: ../../scripting-duat/text/chapter.md
