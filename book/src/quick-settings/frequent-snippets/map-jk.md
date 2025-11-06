# mapping jk to esc

This one is pretty simple. This is normally done on Duat's native `Insert` mode,
but you could replace that with any other `Insert` mode, provided you got the
plugin for one:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    map::<Insert>("jk", "<Esc>");
}
```

This won't print anything to the screen while you're typing, making it seem 
like the `j` key has a bit of delay. If you wish to print `'j'` to the screen, 
use this:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    map::<Insert>("jk", "<Esc>");
}
```

Additionally, if you want to write to the file on `jk` as well, you can do this:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    map::<Insert>("jk", "<Esc>:w<Enter>");
}
```

If you want to, you can also have this behavioron the `PromptLine`, i.e., while 
writing commands and searches:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    map::<Prompt>("jk", "<Esc>");
}
```

