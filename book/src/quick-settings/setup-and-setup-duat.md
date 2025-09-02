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
# mod duat_kak {
#     use duat::prelude::*;
#     #[derive(Default)]
#     pub struct Kak;
#     impl duat_core::Plugin<Ui> for Kak {
#         fn plug(self, _: &duat_core::Plugins<Ui>) { todo!() }
#     }
# }
# mod duat_catppuccin {
#     use duat::prelude::*;
#     #[derive(Default)]
#     pub struct Catppuccin;
#     impl duat_core::Plugin<Ui> for Catppuccin {
#         fn plug(self, _: &duat_core::Plugins<Ui>) { todo!() }
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    add_plugins();
}

fn add_plugins() {
    plug(duat_kak::Kak::default());
    plug(duat_catppuccin::Catppuccin::default());
}
```

### Using `Plugin`s

You can add `Plugin`s to duat by calling the `plug` function. By default, the 
`Treesitter` and `MatchPairs` plugins are added, providing syntax highlighting, 
automatic indentation, and matching parenthesis highlighting.


```rust
# mod duat_kak {
#     use duat::prelude::*;
#     #[derive(Default)]
#     pub struct Kak;
#     impl Kak {
#         pub fn new() -> Self { Self }
#         pub fn f_and_t_set_search(self) -> Self { Self }
#         pub fn with_no_indent_on_capital_i(self) -> Self { Self }
#     }
#     impl duat_core::Plugin<Ui> for Kak {
#         fn plug(self, _: &duat_core::Plugins<Ui>) { todo!() }
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    plug(
        duat_kak::Kak::new()
            .f_and_t_set_search()
            .with_no_indent_on_capital_i(),
    );
}
```

In this case, the `Kak` plugin (from [`duat-kak`]) is being modified via the 
builder pattern in order to have the following settings:

- The `f` and `t` keys will set the search pattern, so `n` will match them.
- When pressing `I`, don't reindent.

Then, it is being added to Duat. The `f_and_t_set_search` and 
`with_no_indent_on_capital_i` modify the `Kak` plugin and then return it back 
to be plugged. This is the [builder pattern], and is how all plugins are added.

[`duat-kak`]: https://github.com/AhoyISki/duat-kak
[builder pattern]: https://rust-unofficial.github.io/patterns/patterns/creational/builder.html
