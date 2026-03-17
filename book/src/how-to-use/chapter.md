# How to use

When calling `duat`, with no arguments, you will open a scratch buffer. If you 
pass files as arguments, then `duat` will open one window for each file.

`duat` also comes with various flags that you can pass:

- `--cfg` will open the `~/.config/duat/src/lib.rs`.
- `--cfg-manifest` will open `~/.config/duat/Cargo.toml`. You can also 
  pass `--cfg` to open both.
- `--no-load` will not load any config, running the default one.
- `--load` will pick a path to look for a config on. This lets you have.
- `--profile` will pick a profile to load.
- `--open` will open `N` windows to place the files passed as arguments.
  multiple configurations, if you wish.
- `--reload` will recompile the config crate.
- `--clean` calls [`cargo clean`] on the config crate.
- `--update` will update dependencies to their latest compatible version.
- `--init-config` will clear the replace the config crate with the default 
  version. If `--load` was passed, clears that path instead.
- `--init-plugin` will initialize a crate for plugin development at the given 
  path.
- `--repository` will pick a repository for the plugin.
- `--author` will pick an author for the plugin.
- `--help` displays these flags.
- `--version` displays Duat's version.

Got to the next chapter to figure out how to control Duat once you're in it.

[`cargo clean`]: https://doc.rust-lang.org/cargo/commands/cargo-clean.html
