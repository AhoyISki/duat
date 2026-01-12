# Status on Buffers and windows

If you want to have a `StatusLine` per `Buffer`, you can add the following:

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    hook::add::<Buffer>(|pa, handle| {
        status!("{name_txt}{Spacer}{main_txt}").above().push_on(pa, handle);
    });
}
```

The snippet above will place a `StatusLine` above every single `Buffer`.

You can go further with this, what if you want different `StatusLine`s,
depending on the `Buffer`?

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    hook::add::<Buffer>(|pa, handle| {
        let status = if handle.read(pa).path().contains(".config/duat") {
            status!("{name_txt}[config] []{Spacer}{main_txt}")
        } else {
            status!("{name_txt}{Spacer}{main_txt}")
        };

        status.above().push_on(pa, handle);
    });
}
```
