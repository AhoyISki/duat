# Installation

In order to begin using Duat, you will need `cargo` installed in your system. 
See [cargo's installation instructions] for more info. After installing 
`cargo`, you will want to make sure you're using rust's nighlty toolchain:

```bash
rustup install nightly
```

Now, you can go ahead and install duat using `cargo`. Duat will be installed 
for the current user, in the `~/.cargo/bin/` directory:

```bash
cargo install duat
```

Or, if you want the `master` version of duat, from the latest commit, you can 
do this:

```bash
cargo install --git https://github.com/AhoyISki/duat --features git-deps
```

If you don't have `~/.cargo/bin/` in your `$PATH` variable, you should add that 
in order to be able to call `duat`. Alternatively, you may add just 
`~/.cargo/bin/duat`, if you don't want to bring the other things in 
`~/.cargo/bin/` to global executable scope.

At this point, you should be able to use Duat by calling `duat` or `duat 
some_file`. When you first run `duat`, you will be prompted for the creation
of a new configuration crate at `~/.config/duat/`. If you accept, this new
configuration will be created and compiled.

The first compilation will take a while, but subsequent compilations should be
really speedy. In my 4.5 year old low level gaming laptop, it usually takes at
most ~1.2 seconds to reload, but most reloads take far less than that.

If you already have a configuration in that directory, and wish to see what the
new default config crate looks like, you can run:

```bash
duat --init-config ~/.config/duat
```

This will warn you about there already being a crate in there, and prompty you
for its substitution. If you agree, a new default config crate will replace the
old one.

This is just for the default crate however. The configuration of Duat is what the rest of this book is for.

[cargo's installation instructions]: https://www.rust-lang.org/tools/install
