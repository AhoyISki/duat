# Duat

Duat is the underworld in egyptian mythology, home to the god Osiris, where the deceased go in order to be judged, and possibly have their rights to an afterlife revoked by the weight of a feather. It is also a text editor.

Duat is a text editor configured through the use of a Rust crate. The use of a crate for such a purpose means that the extensibility potential — as well as my ability to add features — is increased dramatically.

Duat, when installed, will be able to automatically detect changes in the user's configuration, and change in real time — with a delay of a few seconds — in order to reflect the new configuration.

It is currently a work in progress, and many features are yet to come (such as an LSP plugin...).

## How to use

In order to use it, you must have `cargo` installed. If you do, run

`cargo install duat`

This will install the default version of Duat, which uses a terminal user interface. It will also create a configuration directory in `$XDG_CONFIG_HOME/duat/` or `~/.config/duat/`. This config will have some default changes, but you can modify it as you wish. It also has some documentation explaining the basics of Duat.

At the moment, the only Ui is the terminal one, but Duat is built in such a way that creating a new Ui is within the realm of possibility.

For now, it has a barebones configuration, which is based on Kakoune, so if you are familiar with that text editor, many of the commands are going to be the same.

## Configuration

In the configuration file, there should be a `run!` macro. This macro is what will be executed in order to configure Duat. (Eventually, I would like to replace it with a proc macro, since those _seem_ to be better handled by rust-analyzer)

Here's an example configuration file, which makes use of `duat-kak`

```rust
use duat::prelude::*;
// Since duat_kak is a plugin, it must be used directly.
use duat_kak::{KeyMap, Mode, OnModeChange};
run! {
    // The print module manages the printing of files.
    print::wrap_on_width();
    // The hooks module manages hooks.
    hooks::remove_group("FileWidgets");
    hooks::add::<OnFileOpen>(|builder| {
        // This hook lets you adjust what widgets get
        // added to the file.
        builder.push::<VertRule>();
        builder.push::<LineNumbers>();
    });
    hooks::remove_group("WindowWidgets");
    hooks::add::<OnWindowOpen>(|builder| {
        // Same, but for widgets that go around the
        // files in the middle of the window.
        // You can create your own status line.
        // "[" "]" pairs change the style of text.
        let status = status!(
            [File] { File::name } " "
            { KeyMap::mode_fmt } " "
            selections_fmt " " main_fmt
        );
        // As opposed to `builder.push`, this one
        // takes a user defined configuration.
        let (child, _) = builder.push_cfg(status);
        // `push_cfg_to` pushes a widget to another.
        builder.push_cfg_to(CommandLine::cfg().left_with_percent(30), child);
    });
    input::set(KeyMap::new());
    // This is a hook provided by duat-kak.
    hooks::add::<OnModeChange>(|(_, new)| match new {
        Mode::Insert => cursor::set_main(CursorShape::SteadyBar),
        - => cursor::set_main(CursorShape::SteadyBlock);
    });
    // This is a form also provided by duat-kak.
    forms::set("Mode", Form::new().dark_magenta());
}
```

## Roadmap

These are the goals that have been acomplished or are on their way:

- [x] Implement basic visual functionality (printing, scrolling, etc);
- [x] Implement wrapping;
- [x] Implement editing;
- [x] Create a kak mode;
- [x] Implement the use of multiple cursors;
- [x] Implement a history system;
- [x] Implement colors;
- [x] Implement widgets and designated areas;
- [x] Make all of these things easy to use on a public interface;
- [x] Create a number line and a separator line;
- [x] Create a status line;
- [x] File switching;
- [x] Create a command creation interface and a command line;
- [x] Add the ability to frame areas;
- [x] Implement concealment;
- [x] Implement hot reloading of configuration;
- [x] Create a "normal editing" mode;
- [x] Add the ability to create hooks;
- [ ] Create a more generalized plugin system;
- [ ] Add floating widgets, not tied to the session layout;
- [ ] Implement autocompletion lists;
- [ ] Create an LSP plugin;
- [ ] Create a vim mode;
- [ ] Add a regex searcher (possibly a new regex-streams?!?!);

︙

- [ ] Create an EGUI frontend;

__NOTE:__ These are not set in stone, and may be done out of order.

## Motivation

This project was mostly created as a fun side project to occupy my time, and I was also unsatified with the current offerings of open source text editors.

## Why the name

idk, cool sounding word that I got from Spelunky 2.
