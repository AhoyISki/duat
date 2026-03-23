# duat-treesitter ![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-blue) [![duat-treesitter on crates.io](https://img.shields.io/crates/v/duat-treesitter)](https://crates.io/crates/duat-treesitter) [![duat-treesitter on docs.rs](https://docs.rs/duat-treesitter/badge.svg)](https://docs.rs/duat-treesitter) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat-treesitter)

A [tree-sitter][__link0] implementation for Duat

`duat-treesitter` currently does two things:

* Syntax highlighting
* Indentation calculation

## Installation

Just like other Duat plugins, this one can be installed by calling
`cargo add` in the config directory:

```bash
cargo add duat-treesitter@"*"
```

Or, if you are using a `--git-deps` version of duat, do this:

```bash
cargo add --git https://github.com/AhoyISki/duat-treesitter
```

But this is a default plugin, so you most likely won’t have to do
that.


 [__link0]: https://tree-sitter.github.io/tree-sitter
