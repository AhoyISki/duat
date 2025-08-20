# Prompt and Status on same line

In the Kakoune text editor, the status line occupies the same line as the 
command line and notifications. If you want this behavior in Duat, the 
following snippet is enough:

```rust
# use duat::prelude::*;
hook::remove("FooterWidgets");
hook::add::<WindowCreated>(|pa, builder| {
    builder.push(FooterWidgets::default().one_line());
});
```

If you want one of these on each file, you can do this instead:

```rust
# use duat::prelude::*;
hook::remove("FooterWidgets");
hook::add::<File>(|pa, (cfg, builder)| {
    builder.push(FooterWidgets::default().one_line());
    cfg
});
```

With both of these, you can still modify the `StatusLine` with the usual
`hook`s:

```rust
# use duat::prelude::*;
hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
    let mode_upper = mode_txt(pa).map(pa, |mode| 
        txt!("[mode]{}", mode.to_string().to_uppercase()).build()
    );

    cfg.fmt(status!("{Spacer}{name_txt} {mode_upper} {sels_txt} {main_txt}"))
});
```
