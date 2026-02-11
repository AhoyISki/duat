# duat-core ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duat-core on crates.io](https://img.shields.io/crates/v/duat-core)](https://crates.io/crates/duat-core) [![duat-core on docs.rs](https://docs.rs/duat-core/badge.svg)](https://docs.rs/duat-core) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-core)

The core of Duat, for use by Duat’s built-in plugins.

This crate isn’t really meant for public use, since it is used
only by a select few plugins. Configuration crates and plugins
should make use of the [duat][__link0] crate.

One thing to note about this “builti-in plugins” thing though, is that the api of `duat` is a superset of `duat-core`’s api, the only reason why this distinction exists is so I can include some other plugins in `duat`’s api, like `duat-base`, duat-treesitter`, and `duat-lsp\`.


# Plugin examples

## `duat-sneak`

![sneak](../assets/sneak-demonstration.gif)

[`duat-sneak`], inspired by [`vim-sneak`], lets you traverse the
screen by searching through character sequences.

[`duat-sneak`]: https://github.com/AhoyISki/duat-sneak
[`vim-sneak`]: https://github.com/justinmk/vim-sneak
 [__link0]: https://crates.io/duat
