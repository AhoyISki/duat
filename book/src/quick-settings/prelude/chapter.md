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
# use duat::prelude::*;
use duat::opts;
```

This is importing the `opts` module, as opposed to importing its items 
directly, like this:

```rust
# use duat::prelude::*;
use duat::opts::*;
```

This means that, for most options, their path is made up of a 
`{module}::{function}` combo. So the usual `setup` function should look 
something like this:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup() {
    opts::set(|opts| {
        opts.wrap_lines = true;
        opts.wrapping_cap = Some(80);
    });
    
    form::set("caret.main", Form::yellow());
    
    cmd::add!("set-rel-lines", |pa| {
        let handles: Vec<_> = context::windows()
            .handles_of::<LineNumbers>(pa)
            .collect();

        for handle in handles {
            handle.write(pa).relative = true;
        }
        
        Ok(Some(txt!("Lines were set to [a]relative")))
    });
    
    map::<Insert>("jk", "<Esc>:w<Enter>");
}
```

The exceptions to this are the `map`, `alias` and `plug` functions and the 
`setup_duat!` macro. These items are imported directly.

The following chapters should give a quick overview of these items imported 
from the prelude module.
