# mapping jk to esc

This one is pretty simple. Assuming you are using some sort of `Insert` `Mode`:

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
# }
# use duat::prelude::*;
# setup_duat!(setup);
# fn setup() {
// Or vim::Insert, or helix::Insert, when those come out.
map::<kak::Insert>("jk", "<Esc>");
# }
```

This won't print anything to the screen while you're typing, making it seem 
like the `j` key has a bit of delay. If you wish to print `'j'` to the screen, 
use this:

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
# }
# use duat::prelude::*;
# setup_duat!(setup);
# fn setup() {
alias::<kak::Insert>("jk", "<Esc>");
# }
```

Additionally, if you want to write to the file on `jk` as well, you can do this:

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
# }
# use duat::prelude::*;
# setup_duat!(setup);
# fn setup() {
alias::<kak::Insert>("jk", "<Esc>:w<Enter>");
# }
```

If you want to, you can also make this happen on the `PromptLine`, i.e., while writing commands and searches:

```rust
# use duat::prelude::*;
# setup_duat!(setup);
# fn setup() {
alias::<Prompt<Ui>>("jk", "<Esc>");
# }
```

