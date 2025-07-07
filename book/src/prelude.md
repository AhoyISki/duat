# The `prelude` module

At the top of your crate, you should be able to find this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    // The stuff inside your setup...
}
```

This will import everything in the `prelude` module of `duat`. This should have
everything you will need in order to configure Duat, not including things from
other crates that you may want to import (such as plugins).

Most of these imports will be imported as a module, like this:

```rust
use duat::print;
```

As opposed to this:

```rust
use duat::print::*;
```
