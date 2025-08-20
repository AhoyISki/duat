# duat-match-pairs ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-match-pairs on crates.io](https://img.shields.io/crates/v/duat-match-pairs)](https://crates.io/crates/duat-match-pairs) [![duat-match-pairs on docs.rs](https://docs.rs/duat-match-pairs/badge.svg)](https://docs.rs/duat-match-pairs) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat-match-pairs)

A simple [`Plugin`][__link0] to match pairs of parentheses

## Installation

This [`Plugin`][__link1] is added on the config crate by default, there is
no need to install it. However, if you have uninstalled it and
need to reinstall, you can do the following:

```bash
cargo add duat-match-pairs@"*" --rename match-pairs
```

Or, if you are using a `--git-deps` version of duat, do this:

```bash
cargo add --git https://github.com/AhoyISki/duat-match-pairs --rename match-pairs
```

## Usage

In order to make use of it, just add the following to your `setup`
function:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    plug!(match_pairs::MatchPairs::new());
}
```

In this plugin, there are two types of “pairs”, these are the
normal pairs and the treesitter pairs. The normal pairs match
based on the content of the text itself, so for example, in this
situation:

```rust
let my_string = "(this is my string)";
```

There is a normal pair within the string of `(`,`)`. However,
there is no treesitter pair in there, because a treesitter pair
only matches if the pairs are on the language’s syntax tree.

This distinction allows for some combination of pairings that can
also be used as non pairs. For example, in Rust, `<`,`>` is a pair
only on type arguments and things of the sort, in other cases, it
is just a comparison operator. That’s where the treesitter pairs
come in, as they can identify when it is an actual pair, or just
the operator.

In order to change what counts as a normal pair and what counts as
a treesitter pair, you can add the following to the setup
function:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    plug!(
        match_pairs::MatchPairs::new()
            .match_pairs([["\\(", "\\)"], ["\\{", "\\}"], ["\\[", "\\]"]])
            .match_ts_pairs([["<", ">"], ["|", "|"]])
    );
}
```

Two things to note here:

* For now, normal pairs only support one character regexes.
* Also for now, normal pairs use regex, while treesitter pairs use
  strings.


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG455e5ZdiAjrG3UY4XQ7zxWeG0j8baWvHaIvG7ElFWfOt_YnYWSBgmZQbHVnaW72
 [__link0]: https://crates.io/crates/Plugin
 [__link1]: https://crates.io/crates/Plugin
