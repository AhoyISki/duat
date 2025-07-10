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

When calling `use duat::prelude::*`, most imported things will be in the form 
of modules, like this:

```rust
use duat::print;
```

This is importing the `print` module, as opposed to importing its items 
directly:

```rust
use duat::print::*;
```

This means that, for most options, their path is made up of a 
`{module}::{function}` combo. This means that the usual `config` crate should 
look something like this:

```rust
setup_duat!(setup);
use duat::print::*;

fn setup() {
    print::wrap_on_cap(150);
    print::trailing_new_line('󱁐');
    
    form::set("caret.main", Form::yellow());
}
```
