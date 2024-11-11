# duat ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat on crates.io](https://img.shields.io/crates/v/duat)](https://crates.io/crates/duat) [![duat on docs.rs](https://docs.rs/duat/badge.svg)](https://docs.rs/duat)

## Duat

Duat is a text editor with Rust as its configuration language. It
makes use of a configuration crate in the user’s `~/.config`
directory. This configuration crate works just like a regular Rust
crate, but it is dynamically loaded by Duat and executed on
startup.

When installed, Duat will be able to automatically detect changes
in the user’s configuration, adapting to them automatically, with
a very short delay.

### Features

Duat provides a lot of features, trying to be as configurable as
possible, here are some of the things that Duat is capable of:

* Completely custom modes, with full Vim style remapping
* Completely custom widgets, with user created modes
* Arbitrary concealment of text, and arbitrary ghost text
* Custom hooks, whose activation is up to the creator
* Multi UI adaptability, although for now, only a terminal UI has
  been made
* And many others still being planned

Additionaly, by choosing Rust as its configuration language, Duat
also gains the following features:

* Complete type safety
* A very functional programming language, with lots of native
  features
* Cargo is the plugin manager

### How to use

In order to use it, you must have `cargo` installed. If you do,
run

`cargo install duat`

This will install the default version of Duat, which uses a
terminal user interface. It will also create a configuration
directory in `$XDG_CONFIG_HOME/duat/` or `~/.config/duat/`. This
config will have some default changes, but you can modify it as
you wish. It also has some documentation explaining the basics of
Duat.

For now, it has a barebones configuration, which is based on
Kakoune, so if you are familiar with that text editor, many of the
commands are going to be the same.

### Configuration

In the configuration file, there should be a `setup_duat!` macro.
This macro takes in a function pointer and executes it as setup
for Duat.

Here’s an example configuration file, which makes use of
`duat-kak`

```rust
setup_duat!(setup);
use duat::prelude::*;
use duat_kak::{Insert, Normal};

fn setup() {
    mode::set_default(Normal);
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
        builder.push_to(CommandLine::cfg().left_ratioed(3, 7), child);
    });

    hooks::add::<ModeSwitched>(|&(_, new)| match new {
        "Insert" => cursor::set_main(CursorShape::SteadyBar),
        _ => cursor::set_main(CursorShape::SteadyBlock)
    });

    forms::set("Mode", Form::dark_magenta());
}
```

This configuration does the following things:

* Changes the [default mode][__link0] to a Kakoune inspired `Normal`;
* [Maps][__link1] jk to esc;
* [Changes][__link2] the wrapping;
* [Removes][__link3] the hook [group][__link4] “FileWidgets”;
* [Pushes][__link5] a [vertical rule][__link6] and [line numbers][__link7] to every file;
* Removes the hook group “WindowWidgets”;
* Pushes a [custom status line][__link8] and [command line][__link9] to the bottom
  of the screen;
* [Adds][__link10] hooks for [mode changes][__link11] in Duat;
* [Changes][__link12] the [style][__link13] of the mode printed on the
  status line;

These are some of the ways you can configure Duat. You might
notice some things that can be done with these simle options:

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
[`status!`][__link14] macro works similarly.

Duat also has a simple command system, that lets you add commands
with arguments supported by Rust’s type system:

```rust
let callers = ["collapse-command-line", "collapse-cmd"];
commands::add_for::<CommandLine>(
    callers,
    |_command_line, area, _cursors, _flags, _args| {
        area.constrain_ver(Constraint::Length(0.0))?;

        Ok(None)
    },
)
```

The 2 arguments

### Roadmap

These are the goals that have been acomplished or are on their
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
* [ ] Add floating widgets, not tied to the session layout;
* [ ] Implement autocompletion lists;
* [ ] Implement tree-sitter;
* [ ] Create an LSP plugin;
* [ ] Create a vim mode;

︙

* [ ] Create an Iced frontend;

**NOTE:** These are not set in stone, and may be done out of
order.

### Why should I use this?

I don’t know what your personal reasoning would be, but in my
case, I really like Kakoune’s editing model, but was frustrated
with the lack of some
features, like folding, multiple file editing, the general
barebonesness of the configuration, etc.

I know that Neovim has all of these features, and Helix supposedly
tries to
solve some of these issues. But I don’t really like either of
their editing
styles to be honest.

And so I thought, why not make my own text editor?

I thought, why not make a text editor that is as modular as
possible, while
still having a sensible default configuration? That I could modify
however I
wanted, and with a language that I love?

That is why I decided to embark on this journey.

### Why the name

idk, cool sounding word that I got from Spelunky 2.

Also, just wanted to say that no AI was used in this project, cuz
I don’t like it.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG3jKs8Ry6S9HG-U1HOgXvPbCG5xPx_fVleseG-3BECEyC3EvYWSCgmRkdWF0ZTAuMS4zgmlkdWF0X2NvcmVlMC4yLjA
 [__link0]: https://docs.rs/duat/0.1.3/duat/?search=mode::set_default
 [__link1]: https://docs.rs/duat/0.1.3/duat/?search=prelude::map
 [__link10]: https://docs.rs/duat/0.1.3/duat/?search=hooks::add
 [__link11]: https://docs.rs/duat/0.1.3/duat/?search=hooks::ModeSwitched
 [__link12]: https://docs.rs/duat/0.1.3/duat/?search=forms::set
 [__link13]: https://docs.rs/duat/0.1.3/duat/?search=forms::Form
 [__link14]: https://docs.rs/duat/0.1.3/duat/?search=prelude::status
 [__link2]: https://docs.rs/duat/0.1.3/duat/?search=prelude::print::wrap_on_width
 [__link3]: https://docs.rs/duat/0.1.3/duat/?search=hooks::remove
 [__link4]: https://docs.rs/duat/0.1.3/duat/?search=hooks::add_grouped
 [__link5]: https://docs.rs/duat_core/0.2.0/duat_core/?search=ui::FileBuilder
 [__link6]: https://docs.rs/duat/0.1.3/duat/?search=prelude::VertRule
 [__link7]: https://docs.rs/duat/0.1.3/duat/?search=prelude::LineNumbers
 [__link8]: https://docs.rs/duat/0.1.3/duat/?search=prelude::status
 [__link9]: https://docs.rs/duat/0.1.3/duat/?search=prelude::CommandLine
