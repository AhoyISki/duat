# Quick settings

First of all, before you start anything, you should _try_ to become at least 
_somewhat_ accutomed to how rust works as a programming language. If you don't 
know **anything** at all, the [rust book] is a great place to start learning.

One of the main aspects that Duat tries to accomplish is an emergent complexity
for its features. That is, the more you learn about Duat, the easier it becomes
to learn new things about Duat. Every feature should help you understand other
features.

That being said, here are some quick settings, which should hopefully compose
later on to help you better understand duat.

## The `setup` function and `setup_duat!`

In order for Duat to be able to run your config crate in `~/.config/duat`, it _needs_ to have at the very least this in it: 

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {}
```

That's because Duat will call the `setup` function (after some initial setup, 
and before some final setup) in order to do the initial setup for duat. For all 
intents and purposes, treat the `setup` function as if it were the `init.lus` 
on a neovim configuration, as it will do all the setup for Duat.

Of course, not everything needs to be placed inside of this function. For 
example, you could separate the plugins into a separate function, if you think 
that will look cleaner:

```rust
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
documentation to understand. But they should all be configured in the same way:

```rust
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

- The `f` and `t` keys will set the search pattern, so `n` will look for it.
- When pressing `I`, don't reindent.

Then, it is being added to Duat. The `f_and_t_set_search` and 
`with_no_indent_on_capital_i` modify the `Kak` plugin and then return it back 
to be plugged. This is the [builder pattern], and is how all plugins are added.

## The `prelude` module

At the top of your crate, you should be able to find this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // The stuff inside your setup...
}
```

This will import everything in the `prelude` module of `duat`. This should have
everything you will need in order to configure Duat, not including things from
other crates that you may want to import.

Here are some of its items:

### `print`: How duat should print files

The module print has a bunch of options detailing how duat should print files.
Every option in this module can be activated by doing something like this:

```rust
use duat::prelude::*;


```

[rust book]: https://doc.rust-lang.org/book/
[`duat-kak`]: https://github.com/AhoyISki/duat-kak
[builder pattern]: https://rust-unofficial.github.io/patterns/patterns/creational/builder.html
