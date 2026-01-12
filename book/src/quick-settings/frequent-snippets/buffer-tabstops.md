# Buffer wise tabstops

If you want to change the tabstop size per `Buffer`, you can just modify the 
following  snippet:

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    hook::add::<BufferOpened>(|pa, handle| {
        let buffer = handle.write(pa);
        buffer.opts.tabstop = match buffer.filetype() {
            Some("markdown" | "bash" | "lua" | "javascript" | "commonlisp") => 2, 
            _ => 4
        };
    });
}
```

If you want, you can also set other options with this, like which characters 
should be a part of words. In this case, I'm adding `'-'` to the list:


```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    hook::add::<BufferOpened>(|pa, handle| {
        let buffer = handle.write(pa);
        match buffer.filetype() {
            Some("lisp" | "scheme" | "markdown" | "css" | "html") => {
                buffer.opts.tabstop = 2;
                buffer.opts.extra_word_chars = &['-'];
            }
            Some("bash" | "lua" | "javascript" | "typescript") => {
                buffer.opts.tabstop = 2;
            }
            _ => buffer.opts.tabstop = 4
        }
    });
}
```
