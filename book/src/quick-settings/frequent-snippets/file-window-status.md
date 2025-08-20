# Status on files and on window

In these snippets, you might have noticed that I'm mapping _all_ `StatusLine`s, 
not just some of them. So how would one map only the `StatusLine`s belonging to 
files then? Or depending on other factors?

You could just replace all hooks for `File`s and `WindowCreated`, but that is 
rather tedious and verbose. Instead, you can just do the following:

```rust
# use duat::prelude::*;
hook::add::<StatusLine<Ui>>(|_, (cfg, builder)| {
    if builder.file().is_some() {
        cfg.fmt(status!("{Spacer}{name_txt}"))
    } else {
        cfg
    }
});
```

The snippet above will only remap the `StatusLine` when it is pushe onto a 
`File`.

You can go even further, by, e.g. checking on the filetype:

```rust
# use duat::prelude::*;
hook::add::<StatusLine<Ui>>(|pa, (cfg, builder)| {
    if let Some(file) = builder.file() {
        if let Some("rust") = file.filetype(pa)
            && file.read(pa).path().contains(".config/duat")
        {
            cfg.fmt(status!("[config] {Spacer}{name_txt}"))
        } else {
            cfg.fmt(status!("{Spacer}{name_txt}"))
        }
    } else {
        cfg
    }
});
```
