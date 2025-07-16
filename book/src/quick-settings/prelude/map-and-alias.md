# map and alias: modifying keys

In Duat, mapping works somewhat like Vim/neovim, but not quite. This is how it works:

```rust
setup_duat!(setup);
use duat::prelude::*;
use kak::Kak;

fn setup() {
    // Adds kakoune-like editing modes, like Insert, Normal and OneKey
    plug!(Kak::new());
  
    map::<User>("f", "<Esc>|fold -s<Enter>");
    alias::<Insert>("jk", "<Esc>");
    alias::<Prompt>("jk", "<Esc>");
}
```

In mapping, there are two main functions: `map` and `alias`. `map` will take the keys as is, and if the sequence matches, outputs the remapping, otherwise, outputs the keys that were sent. `alias` does the same thing, but it also ""prints"" the sequence that was sent, making it _look_ like you are typing real text.
