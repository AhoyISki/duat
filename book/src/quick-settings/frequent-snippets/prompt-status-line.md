# Prompt and Status on same line

In the Kakoune text editor, the status line occupies the same line as the 
command line and notifications. If you want this behavior in Duat, the 
following snippet is enough:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    opts::one_line_footer(true);
}
```

This will call [`FooterWidgets::one_line`] on the window's `FooterWidgets`.

If you want one of these on each file, you can do this instead:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    hook::remove("FooterWidgets");

    hook::add::<Buffer>(|pa, handle| {
        FooterWidgets::default().one_line().push_on(pa, handle);
        Ok(())
    });
}
```
