# Nerdfonts StatusLine

> [!IMPORTANT]
> 
> This chapter assumes that you are using some kind of [nerd font] in your
> `Ui`. This also goes for this page in the book. If you are not using
> some kind of nerd font in your browser, you will not be able to see the
> characters being displayed.

If you want to nerd-fontify your `StatusLine`, you can just redefine some of 
the status line parts:

```rust
use duat::prelude::*;

fn name_txt(buffer: &Buffer) -> Text {
    let mut b = Text::builder();

    if let Some(name) = buffer.name_set() {
        b.push(txt!("[buffer]{name}"));
        if !buffer.exists() {
            b.push(txt!(" [buffer.new]󰎔 "));
        } else if buffer.text().has_unsaved_changes() {
            b.push(txt!(" [buffer.unsaved] "));
        }
    } else {
        b.push(txt!(" [buffer.new.scratch]{}󰏫 ", buffer.name()));
    }

    b.build()
}
```

[nerd font]: https://www.nerdfonts.com/font-downloads
