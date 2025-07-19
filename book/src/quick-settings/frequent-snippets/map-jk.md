# mapping jk to esc

This one is pretty simple. Assuming you are using some sort of `Insert` `Mode`:

```rust
# use duat::prelude::*;
use kak::Insert; // Or vim::Insert, or helix::Insert, when those come out.

map::<Insert>("jk", "<Esc>");
```

This will not print out the `'j'` to the screen unless the following key is not 
`'k'`. If you wish to print `'j'` to the screen, use this:

```rust
# use duat::prelude::*;
use kak::Insert;

alias::<Insert>("jk", "<Esc>");
```

Additionally, if you want to write to the file on `jk` as well, you can do this:

```rust
# use duat::prelude::*;
use kak::Insert;

alias::<Insert>("jk", "<Esc>:w<Enter>");
```

If you want to, you can also make this happen on the `PromptLine`, i.e., while writing commands and searches:

```rust
# use duat::prelude::*;
alias::<Prompt>("jk", "<Esc>");
```

