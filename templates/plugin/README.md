A duat plugin to ...

This plugin has a longer description here...

## Installation

Just like other Duat plugins, this one can be installed by calling
`cargo add` in the config directory:

```bash
cargo add plugin-name@"*"
```

Or, if you are using a `--git-deps` version of duat, do this:

```bash
cargo add --git {repository_url}
```

## Usage

In order to make use of it, just add the following to your `setup`
function:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    plug(plugin_name::PluginName::default());
}
```

When adding this plugin, it will...

## Features

Here are some of the features of this plugin, and how to use them:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
	plug(plugin_name::PluginName::default());
	// Some useful things that you can do...
}
```

## Forms

This `Plugin` makes use of these `Form`s:

- `plugin_name.some_suff1`: ...
- `plugin_name.some_suff2`: ...
