# The `setup` function and `setup_duat!`

In order for Duat to be able to run your config crate in `~/.config/duat`, it _needs_ to have at the very least this in it: 

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
# mod treesitter {
#     pub struct TreeSitter;
#     impl duat::prelude::Plugin<duat::Ui> for TreeSitter {
#         fn plug(self) { todo!() }
#     }
# }
# mod kak {
#     pub struct Kak;
#     impl Kak {
#         pub fn new() -> Self { Self }
#     }
#     impl duat::prelude::Plugin<duat::Ui> for Kak {
#         fn plug(self) { todo!() }
#     }
# }
# mod catppuccin {
#     pub struct Catppuccin;
#     impl Catppuccin {
#         pub fn new() -> Self { Self }
#     }
#     impl duat::prelude::Plugin<duat::Ui> for Catppuccin {
#         fn plug(self) { todo!() }
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    add_plugins();
}

fn add_plugins() {
    plug!(
        treesitter::TreeSitter,
        kak::Kak::new(),
        catppuccin::Catppuccin::new()
    );
}
```

### The `plug!` macro

The `plug!` macro serves the purpose of adding plugins to Duat. Every plugin 
will have its own configuration options, that you will need to look at their 
documentation to figure out. But they should all be configured in the same way:

```rust
# mod kak {
#     pub struct Kak;
#     impl Kak {
#         pub fn new() -> Self { Self }
#         pub fn f_and_t_set_search(self) -> Self { Self }
#         pub fn with_no_indent_on_capital_i(self) -> Self { Self }
#     }
#     impl duat::prelude::Plugin<duat::Ui> for Kak {
#         fn plug(self) { todo!() }
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    plug!(
        kak::Kak::new()
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
