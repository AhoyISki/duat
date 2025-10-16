# duat

![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue)
[![duat on crates.io](https://img.shields.io/crates/v/duat)](https://crates.io/crates/duat)
[![duat on docs.rs](https://docs.rs/duat/badge.svg)](https://docs.rs/duat)
[![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat)
[![github pages](https://github.com/AhoyISki/duat/actions/workflows/mdbook.yml/badge.svg?branch=master)](https://ahoyiski.github.io/duat)

![](./assets/duat-demo.gif)

Duat is a text editor with sane defaults, while still having an
incredible amount of modularity, to the point where you can
replace pretty much anything.

It is written *and configured* in Rust, through the use of a
config crate. The configuration can then be reloaded without
closing Duat, by being recompiled as requested.

I know that Rust isn’t *really* a scripting language, but I’ve
worked really hard to make this API intuitive to use, whilst still
maintaining all the safety and expressiveness that Rust is known
for.

Rust is also known for long compile times, but for Duat, I’ve
managed to reduce the vast majority of reloads to under ~1.3
seconds, with a large chunk taking less than 800 ms.

For now, the default configuration (and the only usable one
really) is the `duat-kak`  based one, which is based on the
Kakoune text editor. In the near future, I will continue work on a
“regular” mode, as well as a multi-cursor Vim inspired one.

Note that this is an alpha project, so there may be some quirks
and bugs. So if you have a problem, or something seems confusing,
feel free to ask questions or raise an issue, that would be very
welcome 🥰.

### Getting started

 > 
 > [!NOTE]
 > 
 > On this section, I will be referring to duat’s configuration by
 > `~/.config/duat/`, but you should replace it with your operating
 > system’s config path.

To install Duat, I am assuming that you have `cargo` installed on
your system, if you don’t, [install it][__link0].

After installing `cargo`, you will also need to install the
`nightly` toolchain:

#### On unix-like operating systems

```bash
rustup install nightly
```

#### On Windows

```bash
rustup install nightly-x86_64-pc-windows-gnu
```

To install Duat, another dependency that you will need is a c
compiler. On unix-like operating systems, that should already
be installed. But on Windows, you need to manually install it,
probably through the most convenient way, which is Visual Studio.
You can follow the instructions on [this guide][__link1] in order to do
that. You can skip the prerequesits section, it’s vscode specific.

Next, in order to run duat, you should add `~/.cargo/bin/` to your
`$PATH` variable. Alternatively, you can just add
`~/.cargo/bin/duat`, if you want to add just `duat` to the
`$PATH`. Now, you can install duat:

```bash
cargo install duat
```

Although, since this is an alpha, I would recommend the git
version, since that is kept much more up to date:

```bash
cargo install --git https://github.com/AhoyISki/duat --force --features git-deps
```

And if you want to nuke your config in order to get the newest
default config crate, you can do the following:

```bash
rm -rf ~/.config/duat
cargo install --git https://github.com/AhoyISki/duat --force --features git-deps
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

fn setup() {
    plug(kak::Kak::new());
    map::<kak::Insert>("jk", "<Esc>");

    print::wrap_on_edge();

    hook::add::<LineNumbers>(|pa, handle| {
        handle.write(pa).align = std::fmt::Alignment::Right;
        Ok(())
    });

    hook::add::<StatusLine>(|pa, handle| {
        let upper_mode = mode_name(pa).map(pa, |m| match m.split_once('<') {
            Some((no_generics, _)) => no_generics.to_uppercase(),
            None => m.to_uppercase(),
        });

        handle.write(pa).fmt(status!(
            "[Mode]{upper_mode}{Spacer}{name_txt} {sels_txt} {main_txt}"
        ));
        Ok(())
    });

    hook::add::<ModeSwitched>(|_, (_, new)| {
        match new {
            "Insert" => cursor::set_main(CursorShape::SteadyBar),
            _ => cursor::unset(),
        }
        Ok(())
    });

    form::set("Mode", Form::dark_magenta());
}
```

This configuration does the following things:

* [plugs][__link2] the `Kak` plugin, which changes the [default mode][__link3];
* [Maps][__link4] jk to esc in the `Insert` mode;
* [Changes][__link5] the wrapping;
* Changes the alignment of the [`LineNumbers`][__link6] [`Widget`][__link7];
* Changes the [status line][__link8] (with a [Spacer][__link9] for 2 separate sides,
  and a reformatted [`mode_name`][__link10]);
* [Adds][__link11] hooks for [mode changes][__link12] in Duat, which change the shape
  of the cursor;
* [Changes][__link13] the [style][__link14] of the mode printed on the
  status line;

These are only some of the options available to configure Duat,
you can also add [custom commands][__link15], place widgets around other
[`Widget`][__link16]s and [windows][__link17], create
[`Parser`][__link18]s that can track every change on a [`File`][__link19], and many
other things.

Duat also comes with a fully fledged [text creation system][__link20], which
significantly eases the creation of widgets and inline hints:

```rust
let infix = "text";

let text = txt!("This {infix} is [form1]colored and {Spacer} distant").build();
```

In the example above, `[form1]` will change the style of the text
to the `"form1"` [`Form`][__link21], while `{Spacer}` will place a [spacer][__link22] in
between the two parts of the text (See the status line in the GIF,
it uses spacers).

This macro works very similarly to the [`format!`][__link23] family of
macros, so you also have inlining, as you can see with the
`{infix}` part. All of this is, of course, checked at compile
time.

## Troubleshooting

These issues asume that you are working with the `--git-deps`
version of `duat`

### The configuration fails to compile/recompile

Try running this in `~/.config/duat`:

```bash
cargo clean && cargo update && cargo build --release
```

This could solve inconsistencies in the API, given that it could
change without the plugins being aware of those changes.

### It still fails to compile!

In this case, you should open an issue with the error message that
`cargo build --release` sent you.

### It’s still segfaulting as I reopen!

This is an indication that your installed version of duat became
incompatible with that of your config. Rerun the installation
process, no need to remove `~/.config/duat`.

### It’s still segfaulting!

In that case open an issue

## Default plugins

When you install duat, the default config crate will come with
the following plugins:

* [`duat-kak`][__link24] is a plugin that changes the default mode of Duat
  to one inspired by [Kakoune][__link25]’s “Normal”, also bringing with it
  various other modes from Kakoune.
* [`duat-catppuccin`][__link26] is a just a simple colorscheme plugin, it
  adds the four flavors from the [catppuccin][__link27] colorscheme. You can
  pick between the four of them, you can apply its colors to other
  [`Form`][__link28]s and you can allow or disallow the colorscheme to set
  the background color.

It also comes with the following built-in plugins, which I will
later on add the ability to disable:

* [`duat-treesitter`][__link29] brings [tree-sitter][__link30] to Duat in the form of
  syntax highlighting and indentation calculation, which can be
  used by Modes (such as those from `duat-kak`) in order to give
  better feedback when editing files.
* [`duat-match-pairs`][__link31] adds matched parentheses highlighting to
  duat. Has some ntegration with `duat-treesitter`.
* [`duat-utils`][__link32] adds all of the default plugins that you see,
  like the line numbers, status line, prompt line, etc.

### Features

Duat provides a lot of features, trying to be as configurable as
possible, here are some of the things that Duat is capable of:

* Completely custom modes, with full Vim style remapping;
* Completely custom widgets, with user created modes;
* Arbitrary concealment of text, and arbitrary ghost text;
* Custom hooks, whose activation is up to the creator;
* Custom commands, with customizable parameters supported by;
  Rust’s robust type system;
* Multi UI adaptability, although for now, only a terminal UI has
  been made;
* And many others still being planned;

Additionally, by choosing Rust as its configuration language, Duat
also gains the following features:

* Complete type safety;
* A very functional programming language, with lots of native
  features;
* Cargo is the plugin manager;

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
* [x] Add floating widgets, not tied to the session layout;
* [ ] Implement autocompletion lists;
* [ ] Create an LSP plugin;
* [ ] Create a vim mode;

︙

* [ ] Create an Iced frontend;

An internal (and more detailed) TODO list, which might hard to
understand, can be found in [TODO][__link33]. This list will is
*not* a comprehensive roadmap, as I will ocasionally remove
entries from it, particularly those in the `FOR NEXT UPDATE`
section, when said update comes out.

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


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG28twsakgeweG89ory0HETUFG8cwEFhxMqC5GzP5sGaT2qvKYXKEG60V0Tu5CbAYGz44HWmx87__G2NIq58yfzSHGw29UB1uSc7cYWSCgmRkdWF0ZTAuNy4wgmlkdWF0X2NvcmVlMC43LjA
 [__link0]: https://www.rust-lang.org/tools/install
 [__link1]: https://code.visualstudio.com/docs/cpp/config-mingw
 [__link10]: https://docs.rs/duat/0.7.0/duat/?search=prelude::mode_name
 [__link11]: https://docs.rs/duat/0.7.0/duat/?search=prelude::hook::add
 [__link12]: https://docs.rs/duat/0.7.0/duat/?search=prelude::hook::ModeSwitched
 [__link13]: https://docs.rs/duat/0.7.0/duat/?search=form::set
 [__link14]: https://docs.rs/duat/0.7.0/duat/?search=prelude::form::Form
 [__link15]: https://docs.rs/duat/0.7.0/duat/?search=prelude::cmd
 [__link16]: https://docs.rs/duat/0.7.0/duat/?search=hook::WidgetCreated
 [__link17]: https://docs.rs/duat/0.7.0/duat/?search=hook::WindowCreated
 [__link18]: https://docs.rs/duat_core/0.7.0/duat_core/?search=file::Parser
 [__link19]: https://docs.rs/duat/0.7.0/duat/?search=prelude::File
 [__link2]: https://docs.rs/duat/0.7.0/duat/?search=prelude::plug
 [__link20]: https://docs.rs/duat/0.7.0/duat/?search=prelude::text::txt
 [__link21]: https://docs.rs/duat/0.7.0/duat/?search=prelude::Form
 [__link22]: https://docs.rs/duat/0.7.0/duat/?search=prelude::Spacer
 [__link23]: https://doc.rust-lang.org/stable/std/macro.format.html
 [__link24]: https://github.com/AhoyISki/duat-kak
 [__link25]: https://github.com/mawww/kakoune
 [__link26]: https://github.com/AhoyISki/duat-catppuccin
 [__link27]: https://catppuccin.com
 [__link28]: https://docs.rs/duat/0.7.0/duat/?search=prelude::Form
 [__link29]: https://github.com/AhoyISki/duat-treesitter
 [__link3]: https://docs.rs/duat/0.7.0/duat/?search=prelude::mode::set_default
 [__link30]: https://tree-sitter.github.io/tree-sitter
 [__link31]: https://github.com/AhoyISki/duat-match-pairs
 [__link32]: https://github.com/AhoyISki/duat/tree/master/duat-utils
 [__link33]: ./TODO
 [__link4]: https://docs.rs/duat/0.7.0/duat/?search=prelude::map
 [__link5]: https://docs.rs/duat/0.7.0/duat/?search=prelude::print::wrap_on_edge
 [__link6]: https://docs.rs/duat/0.7.0/duat/?search=prelude::LineNumbers
 [__link7]: https://docs.rs/duat/0.7.0/duat/?search=prelude::Widget
 [__link8]: https://docs.rs/duat/0.7.0/duat/?search=prelude::status
 [__link9]: https://docs.rs/duat/0.7.0/duat/?search=prelude::Spacer
