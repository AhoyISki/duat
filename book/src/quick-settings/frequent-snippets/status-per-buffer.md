# StatusLine on each Buffer

If you want one `StatusLine` on every `Buffer`, you can do that via hooks:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    hook::add::<BufferOpened>(|pa, handle| {
        status!("{name_txt}{Spacer}{main_txt}")
            .above()
            .push_on(pa, handle);
    });
}
```
