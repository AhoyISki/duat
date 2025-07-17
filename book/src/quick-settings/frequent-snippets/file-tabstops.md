# File wise tabstops

If you want to change the tabstop size per file, you can just modify the 
following  snippet:

```rust
# use duat::prelude::*;

hook::add::<OnFileOpen>(|pa, builder| {
    builder.write(pa, |file, _| match file.filetype() {
        Some("markdown" | "bash" | "lua" | "javascript" | "lisp") => {
            file.cfg.set_tabstop(2);
        } 
        _ => {
            file.cfg.set_tabstop(4);
        }
    })
});
```

If you want, you can also set other options with this, like which characters 
should be a part of words. In this case, I'm adding `'-'` to the list:


```rust
# use duat::prelude::*;

hook::add::<OnFileOpen>(|pa, builder| {
    builder.write(pa, |file, _| match file.filetype() {
        Some("lisp" | "scheme" | "markdown" | "css" | "html") => {
            let wc = word_chars!("A-Za-z0-9_-_---");
            file.cfg.set_tabstops(2).set_word_chars(wc);
        }
        Some("bash" | "lua" | "javascript" | "typescript") => {
            file.cfg.set_tabstops(2);
        }
        _ => {
            file.cfg.set_tabstops(4);
        }
    }
});
```
