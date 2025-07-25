# map and alias: modifying keys

In Duat, mapping works somewhat like Vim/neovim, but not quite. This is how it 
works:

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

In mapping, there are two main functions: `map` and `alias`. `map` will take 
the keys as is, and if the sequence matches, outputs the remapping, otherwise, 
outputs the keys that were sent. `alias` does the same thing, but it also 
""prints"" the sequence that was sent, making it _look_ like you are typing 
real text.

In the functions, you can see a type argument. This type argument is the `Mode` 
where this mapping will take place. So here, in `duat-kak`'s `Insert` mode, if 
you type `jk`, the `j` will show up as ""text"", but when you press `k`, you 
will immediately exit to `Normal` `Mode`.

`User` is a standard `Mode` in Duat. It is meant to be a "hub" for Plugin 
writers to put default mappings on. Sort of like the leader key in Vim/Neovim. 
On `duat-kak`, by default, this mode is entered by pressing the space bar. 
While you _can_ change that like this:

```rust
setup_duat!(setup);
use duat::prelude::*;
use kak::Kak;

fn setup() {
    plug!(Kak::new());
  
    map::<Normal>(" ", "");
    // In rust, you have to escap a backslash
    map::<Normal>("\\", " ");
}
```

You _can_ also do this:

```rust
setup_duat!(setup);
use duat::prelude::*;
use kak::Kak;

fn setup() {
    plug!(Kak::new());
  
    map::<Normal>(" ", "");
    map::<Normal>("\\", User);
}
```

This is allowed in order to support custom `Mode`s. That way, you can just 
place the `Mode` as the second argument, and the mapping will switch modes, 
instead of sending keys. This also works with `alias`.

> [!NOTE]
> In this case, since `User` is a struct with no fields, I could just put 
> `User` as the second argument, which acts as a constructor. But in most other 
> `Mode`s, you're gonna have to write something like `Insert::new()` as the 
> argument instead.

## List of keys and modifiers

Syntax wise, the keys are very similar to vim style. Regular characters are 
placed normally, special keys are enclosed in `<`,`>` pairs, and modified keys 
are enclosed in these pairs, with a `<{mod}-{key}>` syntax. Examples:

- `abc<C-Up><F12>`.
- `<A-Enter><AS-Left>`.
- `づあっと`.

This is the list of recognized special keys:

- `<Enter>`,
- `<Tab>`,
- `<Bspc>`,
- `<Del>`,
- `<Esc>`,
- `<Up>`,
- `<Down>`,
- `<Left>`,
- `<Right>`,
- `<PageU>`,
- `<PageD>`,
- `<Home>`,
- `<End>`,
- `<Ins>`,
- `<F{1-12}>`,

And these are the allowed modifiers, which, as you can see above, can be 
composed together:

- `C => Control`,
- `A => Alt`,
- `S => Shift`,
- `M => Meta`,
- `super => Super`,
- `hyper => Hyper`,

