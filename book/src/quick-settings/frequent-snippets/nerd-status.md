# Nerdfonts StatusLine

> [!IMPORTANT]
> 
> This chapter assumes that you are using some kind of [nerd font] in your
> `Ui`. This also goes for this page in the book. If you are not using
> some kind of nerd font in your browser, you will not be able to see the
> characters being displayed. For that, I will provide pictures.

If you want to nerd-fontify your `StatusLine`, you should probably redefine 
some of the status line parts:

```rust
# use duat::prelude::*
fn file_fmt(file: &File) -> Text {
    let mut b = Text::builder();

    if let Some(name) = file.name_set() {
        b.push(txt!("[file]{name}"));
        if !file.exists() {
            b.push(txt!("[file.new]󰎔 "));
        } else if file.text().has_unsaved_changes() {
            b.push(txt!("[file.unsaved] "));
        }
    } else {
        b.push(txt!("[file.new.scratch]{}󰏫 ", file.name())));
    }

    b.build()
}
```

[nerd font]: https://www.nerdfonts.com/font-downloads
