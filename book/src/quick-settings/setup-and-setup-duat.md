# The `setup` function and `setup_duat!`

In order for Duat to be able to run your config crate in `~/.config/duat`, it 
_needs_ to have at the very least this in it: 

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {}
```

That's because Duat will call the `setup` function (after some initial setup, 
and before some final setup) in order configure Duat. For all intents and 
purposes, treat the `setup` function as if it were the `init.lua` on a neovim 
configuration.

Of course, not everything needs to be placed inside of this function. For 
example, you could separate the plugins into a separate function, if you think 
that will look cleaner:

```rust
# mod duat_catppuccin {
#     use duat::prelude::*;
#     #[derive(Default)]
#     pub struct Catppuccin;
#     impl Plugin for Catppuccin {
#         fn plug(self, _: &Plugins) { todo!() }
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    add_plugins();
}

fn add_plugins() {
    plug(duat_catppuccin::Catppuccin::default());
}
```

You can add `Plugin`s to duat by calling the `plug` function. By default, the 
`Treesitter` `MatchPairs`  plugins are added, providing syntax highlighting, 
automatic indentation, and matching parenthesis highlighting. There is also the
`DuatMode` plugin, which provides the default control scheme for duat, heavily inspired by Kakoune.

[`duat-kak`]: https://github.com/AhoyISki/duat-kak
[builder pattern]: https://rust-unofficial.github.io/patterns/patterns/creational/builder.html
