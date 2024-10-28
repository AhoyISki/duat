# Duat-core

The core of the Duat text editor, containing the API that powers Duat and plugins for Duat. If you wish to create a plugin for Duat, you should declare this crate as a dependency, not `duat`.

This is because `duat-core` is compatible with a number of different UIs, while `duat` already assumes that a Ui was already given. By depending on `duat-core` you can decide which of the Uis your plugin is going to support.

# Plugin creation

In order to create a plugin, you will first need to create a Rust crate, with `duat-core` as a dependency:

```sh
cargo init --lib {crate_name}
cd {crate_name}
cargo add duat-core
```

Inside `{crate_name}/src/lib.rs`, 
