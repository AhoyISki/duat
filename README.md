# duat ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat on crates.io](https://img.shields.io/crates/v/duat)](https://crates.io/crates/duat) [![duat on docs.rs](https://docs.rs/duat/badge.svg)](https://docs.rs/duat)

## Duat

Duat is a text editor meant to have as much modularity as
possible, while keeping a sensible default configuration. It is
written *and configured* in Rust, through the use of a
configuration Rust crate, placed in `~/.config/duat/` (or wherever
`$XDG_CONFIG_HOME` is set to).

When installing Duat, this crate will be automatically placed in
that spot, and it will have a default example configuration.

When you first run Duat, and whenever you update the
configuration, it will be compiled and reloaded automatically, so
you can see the changes in *almost* real time.

Note that this is an alpha project, so there may be some quirks
and bugs, but feel free to ask questions or raise an issue, that
would be very welcome 🥰.

### Getting started

To install Duat, do the following:

```text
cargo install duat
```

Although, since this is an alpha, I would recommend the git
version, since that is kept much  more up to date:

```text
git clone https://github.com/AhoyISki/duat
cargo install --path duat
```

### Configuration

In the configuration file, there should be a `setup_duat!` macro,
which takes in a function with no parameters.

This function is the setup for duat, and it can be empty, which is
the equivalent of the default configuration for Duat.

Here’s an example configuration file, which makes use of the
`duat-kak` crate, which is a plugin for Duat. This plugin, like
all others, is included without the `duat_` prefix, so in the
config it is just `kak`.

```rust
setup_duat!(setup);
use duat::prelude::*;
use kak::{Insert, Normal};

fn setup() {
    plug!(kak::Kak);
    map::<Insert>("jk", "<Esc>");

    print::wrap_on_width();

    hooks::remove("FileWidgets");
    hooks::add::<OnFileOpen>(|builder| {
        builder.push(VertRule::cfg());
        builder.push(LineNumbers::cfg());
    });

    hooks::remove("WindowWidgets");
    hooks::add::<OnWindowOpen>(|builder| {
        let status = status!(
            [File] { File::name } " "
            mode " " selections_fmt " " main_fmt
        );

        let (child, _) = builder.push(status);
        builder.push_to(CmdLine::cfg().left_ratioed(3, 7), child);
    });

    hooks::add::<ModeSwitched>(|&(_, new)| match new {
        "Insert" => cursor::set_main(CursorShape::SteadyBar),
        _ => cursor::set_main(CursorShape::SteadyBlock)
    });

    form::set("Mode", Form::dark_magenta());
}
```

This configuration does the following things:

* [plugs][__link0] the `Kak` plugin, which changes the [default mode][__link1];
* [Maps][__link2] jk to esc in the `Insert` mode;
* [Changes][__link3] the wrapping;
* [Removes][__link4] the hook [group][__link5] “FileWidgets”;
* [Pushes][__link6] a [vertical rule][__link7] and [line numbers][__link8] to every file;
* Removes the hook group “WindowWidgets”;
* Pushes a [custom status line][__link9] and [command line][__link10] to the bottom
  of the screen;
* [Adds][__link11] hooks for [mode changes][__link12] in Duat;
* [Changes][__link13] the [style][__link14] of the mode printed on the
  status line;

These are some of the ways you can configure Duat. You might
notice some things that can be done with these simple options:

```rust
hooks::add::<OnFileOpen>(|builder| {
    builder.push(LineNumbers::cfg());
    builder.push(LineNumbers::cfg());
    builder.push(LineNumbers::cfg().on_the_right());
    builder.push(LineNumbers::cfg().on_the_right());
});
```

Now, every file will open with two lines of numbers, one on each
side. Would you ever want to do this? …No, not really, but it
shows how configurable Duat can be.

Duat also comes with a fully fledged text styling system, which
significantly eases the creation of widgets:

```rust
let text = text!([MyForm] "Waow it's my form! " [] "not anymore 😢");
```

In this example, I’m using the “MyForm” form in order to style the
text, while `[]` reverts back to the “Default” form. The
[`status!`][__link15] macro works similarly.

Duat also has a simple command system, that lets you add commands
with arguments supported by Rust’s type system:

```rust
let callers = ["collapse-cmd-line", "ccmd"];
cmd::add_for!(callers, |_cmd_line: CmdLine, area, _flags| {
    area.constrain_ver(Constraint::Length(0.0))?;
    Ok(None)
})
```

The 2 arguments

### Features

Duat provides a lot of features, trying to be as configurable as
possible, here are some of the things that Duat is capable of:

* Completely custom modes, with full Vim style remapping
* Completely custom widgets, with user created modes
* Arbitrary concealment of text, and arbitrary ghost text
* Custom hooks, whose activation is up to the creator
* Custom commands, with customizable parameters supported by
  Rust’s robust type system
* Multi UI adaptability, although for now, only a terminal UI has
  been made
* And many others still being planned

Additionally, by choosing Rust as its configuration language, Duat
also gains the following features:

* Complete type safety
* A very functional programming language, with lots of native
  features
* Cargo is the plugin manager

### Roadmap

These are the goals that have been accomplished or are on their
way:

* [x] Implement basic visual functionality (printing, scrolling,
  etc);
* [x] Implement wrapping;
* [x] Implement editing;
* [x] Create a kak mode;
* [x] Implement the use of multiple cursors;
* [x] Implement a history system;
* [x] Implement colors;
* [x] Implement widgets and designated areas;
* [x] Make all of these things easy to use on a public interface;
* [x] Create a number line and a separator line;
* [x] Create a status line;
* [x] File switching;
* [x] Create a command creation interface and a command line;
* [x] Add the ability to frame areas;
* [x] Implement concealment;
* [x] Implement hot reloading of configuration;
* [x] Create a “normal editing” mode;
* [x] Add the ability to create hooks;
* [x] Create a more generalized plugin system;
* [x] Implement incremental Regex searching;
* [x] Implement tree-sitter;
* [ ] Add floating widgets, not tied to the session layout;
* [ ] Implement autocompletion lists;
* [ ] Create an LSP plugin;
* [ ] Create a vim mode;

︙

* [ ] Create an Iced frontend;

**NOTE:** These are not set in stone, and may be done out of
order.

### Why should I use this?

I don’t know what your personal reasoning would be, but in my
case, I really like Kakoune’s editing model, but was frustrated
with the lack of some features, like folding, multiple file
editing, the general barebonesness of the configuration, etc.

I know that Neovim has all of these features, and Helix supposedly
tries to solve some of these issues. But I don’t really like
either of their editing styles to be honest.

And so I thought, why not make my own text editor?

I thought, why not make a text editor that is as modular as
possible, while still having a sensible default configuration?
That I could modify however I wanted, and with a language that I
love?

That’s why I decided to create Duat.

### Why the name

idk, cool sounding word that I got from Spelunky 2.

Also, just wanted to say that no AI was used in this project, cuz
I don’t like it.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG2ds7whO6-YnG99fvAXNxtM9G2EGOuZcP2_sGwkD73k7aWU4YWSDgmRkdWF0ZTAuMy4wgmlkdWF0X2NvcmVlMC4zLjCCZHBsdWf2
 [__link0]: https://crates.io/crates/plug
 [__link1]: https://docs.rs/duat/0.3.0/duat/?search=mode::set_default
 [__link10]: https://docs.rs/duat/0.3.0/duat/?search=prelude::CmdLine
 [__link11]: https://docs.rs/duat/0.3.0/duat/?search=hooks::add
 [__link12]: https://docs.rs/duat/0.3.0/duat/?search=hooks::ModeSwitched
 [__link13]: https://docs.rs/duat/0.3.0/duat/?search=form::set
 [__link14]: https://docs.rs/duat/0.3.0/duat/?search=form::Form
 [__link15]: https://docs.rs/duat/0.3.0/duat/?search=prelude::status
 [__link2]: https://docs.rs/duat/0.3.0/duat/?search=prelude::map
 [__link3]: https://docs.rs/duat/0.3.0/duat/?search=prelude::print::wrap_on_width
 [__link4]: https://docs.rs/duat/0.3.0/duat/?search=hooks::remove
 [__link5]: https://docs.rs/duat/0.3.0/duat/?search=hooks::add_grouped
 [__link6]: https://docs.rs/duat_core/0.3.0/duat_core/?search=ui::FileBuilder
 [__link7]: https://docs.rs/duat/0.3.0/duat/?search=prelude::VertRule
 [__link8]: https://docs.rs/duat/0.3.0/duat/?search=prelude::LineNumbers
 [__link9]: https://docs.rs/duat/0.3.0/duat/?search=prelude::status
