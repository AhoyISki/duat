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
directly, like this:

```rust
use duat::print::*;
```

This means that, for most options, their path is made up of a 
`{module}::{function}` combo. This means that the usual `config` crate should 
look something like this:

```rust
# mod kak {
#     use duat::{prelude::{*, mode::KeyEvent}};
#     #[derive(Clone)]
#     pub struct Insert;
#     impl Mode<Ui> for Insert {
#         type Widget = File;
#         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<File>) {
#             todo!();
#         }
#     }
#     pub struct Kak;
#     impl Kak {
#         pub fn new() -> Self { Self }
#     }
#     impl duat_core::Plugin<Ui> for Kak {
#         fn plug(self) {}
#     }
# }
setup_duat!(setup);
use duat::print::*;

fn setup() {
    plug!(Kak::new());
    
    print::wrap_on_cap(150);
    print::trailing_new_line('󱁐');
    
    form::set("caret.main", Form::yellow());
    
    cmd::add!("set-rel-lines", |pa, ln: Handles<LineNumbers<Ui>>| {
        ln.on_flags(pa, |pa, handle| {
            handle.write(pa, |ln, _| ln.options = ln.options.rel_abs());
        });
    });
    
    map::<Insert>("jk", "<Esc>:w<Enter>");
}

```

The exceptions to this are the `map` and `alias` functions, as well as the 
`plug!` and `setup_duat!` macros. These items are imported directly.

The following chapters should give a quick overview of these items imported 
from the prelude module.
