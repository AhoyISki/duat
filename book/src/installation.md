# Installation

In order to begin using Duat, you will need `cargo` installed in your system. See [cargo's installation instructions] for more info. After installing `cargo`, you will want to make sure you're using rust's nighlty features:

```bash
rustup install nightly
```

Now, you can go ahead and install duat using `cargo`. Duat will be installed for the current user, in the `~/.cargo/bin/` directory:

```bash
cargo install duat
```

Or, if you want the `master` version of duat, from the latest commit, you can do this:

```bash
cargo install --git https://github.com/AhoyISki/duat --features git-deps
```

When calling either of these commands, if it didn't already exist, a default version of Duat's configuration will be installed in `$XDG_CONFIG_HOME/duat` (`~/.config/duat` for Linux). If you want to get the latest version of the default configuration, you can just remove that, alongside other directories used by Duat:

```bash
rm -rf ~/.config/duat/ ~/.cache/duat/ ~/.local/share/duat/
cargo install --git https://github.com/AhoyISki/duat --features gid-deps
```

If you don't have `~/.cargo/bin/` in your `$PATH` variable, you should add that in order to be able to call `duat`. Alternatively, you may add just `~/.cargo/bin/duat`, if you don't want to bring the other things in `~/.cargo/bin/` to global executable scope.

At this point, you should be able to use Duat by calling `duat` or `duat some_file`. Initially, the config for Duat will be compiled

[cargo's installation instructions]:
https://www.rust-lang.org/tools/install
