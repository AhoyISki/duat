# File wise tabstops

If you want to change the tabstop size per file, you can just modify the 
following  snippet:

```rust
# use duat::prelude::*;

hook::add::<File>(|_, (cfg, _)| {
    match cfg.filetype() {
        Some("markdown" | "bash" | "lua" | "javascript" | "lisp") => cfg.tabstop(2), 
        _ => cfg.tabstop(4)
    }
});
```

If you want, you can also set other options with this, like which characters 
should be a part of words. In this case, I'm adding `'-'` to the list:


```rust
# use duat::prelude::*;

hook::add::<File>(|_, (cfg, _)| {
    match cfg.filetype() {
        Some("lisp" | "scheme" | "markdown" | "css" | "html") => {
            let wc = print::w_chars!("A-Za-z0-9_-_---");
            cfg.tabstop(2).word_chars(wc)
        }
        Some("bash" | "lua" | "javascript" | "typescript") => cfg.tabstop(2),
        _ => cfg.tabstop(4)
    }
});
```
