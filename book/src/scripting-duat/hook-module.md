# The hook module

Hooks in duat are functions that are called automatically whenever some
specific event happens. They are very similar to kakoune's hooks or neovim's
autocmds. However, one thing that distinguishes the versatility of duat's hooks
is the fact that they present you with arguments whose type is inferred at
compile time:

```rust
setup_duat!(setup);
use duat::prelude::*;
use widgets::*;

fn setup(opts: &mut Opts) {
    // Note the Pass, so you have mutable global state access.
    // The type of the second argument is inferred.
    hook::add::<ModeSwitched>(|pa, switch| {
        if switch.old.name == "Insert" && switch.new.name == "Normal" {
            _ = context::current_buffer(pa).save(pa);
        }
    });

    // Handle is an alias for Handle<Buffer> and is frequently
    // seen in various hooks.
    hook::add::<BufferOpened>(|pa, buffer: &Handle| {
        let buf = buffer.write(pa);
        match buf.filetype() {
            Some("rust" | "cpp") => {
                buf.opts.wrap_lines = true;
                buf.opts.wrapping_cap = Some(100);
            }
            Some("markdown" | "asciidoc") => {
                buf.opts.tabstop = 2;
                buf.opts.extra_word_chars = &['-'];
            }
            Some("lua" | "javascript") => buf.opts.tabstop = 2,
            _ => {}
        };
    });

    // You can call hooks for many things...
    hook::add::<WidgetOpened<LineNumbers>>(|pa, handle| {
        // This will put a vertical ruler on the left of the `LineNumbers`
        // making for a "stylish" column of numbers.
        VertRule::builder().push_on(pa, handle);
    });

    // In this hook, I'm incrementing a key_count every time a key is
    // typed...
    let key_count = RwData::new(0);
    hook::add::<KeyTyped>({
        let key_count = key_count.clone();
        move |pa, _| {
            *key_count.write(pa) += 1;
        }
    });

    // ...then I'm adding said key_count to the status line.
    opts.fmt_status(move |_| {
        let mode_txt = mode_txt();
        let key_count = key_count.clone();
        status!("{name_txt} {mode_txt}{Spacer}{sels_txt} {main_txt} {key_count}")
    })
}
```

## Creating new hooks

Another interesting thing about hooks in duat is that you can create your own.
You do that by implementing the `Hookable` trait on a type:

```rust
use duat::prelude::*;

struct OnIdle(Handle);

impl Hookable for OnIdle {
    // The Input type is the value available when calling `hook::add`
    type Input<'h> = &'h Handle;

    fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
        &self.0
    }
}
```

Then, you decide when to `trigger` said hook:

```rust
# struct OnIdle(Handle);
#
# impl Hookable for OnIdle {
#     type Input<'h> = &'h Handle;
#
#     fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
#         &self.0
#     }
# }
use duat::prelude::*;
use std::{
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
    time::Duration,
};

static COUNTER: AtomicUsize = AtomicUsize::new(0);
static QUITTING: AtomicBool = AtomicBool::new(false);

fn setup_hook() {
    // Start counting as soon as the user stops typing.
    hook::add::<KeyTyped>(|pa, _| COUNTER.store(0, Ordering::Relaxed));
    hook::add::<ConfigUnloaded>(|_, _| QUITTING.store(true, Ordering::Relaxed));

    std::thread::spawn(|| {
        while !QUITTING.load(Ordering::Relaxed) {
            std::thread::sleep(Duration::from_secs(1));
            let elapsed = COUNTER.fetch_add(1, Ordering::Relaxed);

            // Every 60 seconds, trigger an `OnIdle` event
            if elapsed + 1 == 60 {
                // We have to queue it, since this is being
                // called from another thread.
                context::queue(|pa| {
                    let buffer = context::current_buffer(pa);
                    hook::trigger(pa, OnIdle(buffer));
                });
            }
        }
    });
}
```

Then, the user can just add their own hooks, which will be called accordingly:

```rust
# struct OnIdle(Handle);
#
# impl Hookable for OnIdle {
#     type Input<'h> = &'h Handle;
#
#     fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
#         &self.0
#     }
# }
# fn setup_hook() {}
setup_duat!(setup);
use duat::prelude::*;

fn setup(opts: &mut Opts) {
    // This would be called from a `Plugin::plug` function
    setup_hook();

    hook::add::<OnIdle>(|pa, _| {
        let mut saved = 0;

        for buffer in context::buffers(pa) {
            if buffer.read(pa).has_unsaved_changes() && buffer.save(pa).is_ok() {
                saved += 1;
            }
        }

        if saved > 0 {
            context::info!("Saved [a]{saved}[] buffers from idling");
        }
    });
}
```

## List of hooks

Here's the list of currently available hooks, more will be added in the future.
For the list of arguments of each hook, remember that there will always be
`&mut Pass` argument, and that the other arguments will come in a tuple.

For example, if a hook has two arguments, `i32` and `bool`, they will actually
come in as `(i32, bool)` in the second argument of the hook. So you should pass
a function like this:

```rust
# struct FooHook;
# impl Hookable for FooHook {
#     type Input<'h> = (i32, bool);
#
#     fn get_input<'h>(&'h mut self, _: &mut Pass) -> Self::Input<'h> {
#         (0, true)
#     }
# }
setup_duat!(setup);
use duat::prelude::*;

fn setup(opts: &mut Opts) {
    hook::add::<FooHook>(|pa, (arg1, arg2): (i32, bool)| {
        // ...
    });
}
```

Also, when a hook says that it has no arguments, what this actually means is
that the second argument is of type `()`, so the function argument still needs
to have two arguments in it.

### `BufferOpened`

Triggers after opening a `Buffer`. You will want to use this hook to set
buffer-wise configuration options:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup(opts: &mut Opts) {
    // Options set initially for all `Buffer`s
    opts.tabstop = 4;

    // Changing those options on a buffer by buffer basis.
    hook::add::<BufferOpened>(|pa, buffer| {
        let buf = buffer.write(pa);

        match buf.filetype() {
            Some("haskell" | "commonlisp") => buf.opts.tabstop = 2,
            Some("txt" | "markdown" | "asciidoc") => {
                buf.opts.tabstop = 2;
                buf.opts.wrap_lines = true;
                buf.opts.wrapping_cap = Some(80);
            }
            _ => {}
        }
    });
}
```

Note that this hook is just a an alias for `WidgetOpened<Buffer>`.

*Arguments*

- The `Handle<Buffer>` of the `Buffer` that was
  opened.

### `BufferClosed`

Triggers as a `Buffer` is being closed. This can happen for two reasons:

- The `Buffer` has been closed, through something like `:q` or `:wq`.
- The `Buffer` is being unloaded to be reloaded after reloading the config,
  because you called `:reload` or typed `<c-r>` in normal mode.

*Arguments*

- The `Handle<Buffer>` that was unloaded.
- `true` if the `Buffer` is going to reload, `false` if it is just being closed.

### `BufferUpdated`

Triggers whenever the `Buffer` is modified in any way.

In order to trigger many things automatically, this is the most frequently used
hook in Duat. It is useful in many areas, most notably to keep track of every
change that takes place in `Buffer`s:

```rust
use duat::prelude::*;
struct MyPlugin;

impl duat::Plugin for MyPlugin {
    fn plug(self, opts: &mut Opts, _: &duat::Plugins) {
        // A namespace used to track changes to every `Buffer`.
        let tracker_ns = Ns::new();

        hook::add::<BufferOpened>(move |pa, buffer| {
            // Flush initial empty moment.
            _ = buffer.read(pa).moment_for(tracker_ns)
        });

        hook::add::<BufferUpdated>(move |pa, buffer| {
            let moment = buffer.read(pa).moment_for(tracker_ns);

            for change in moment.iter() {
                // Update Plugin state based on changes.
            }
        });
    }
}
```

This particular pattern can be found all over the place. Two major examples being
`duat-lsp` and `duat-treesitter`.

Do note here that the `Buffer::moment_for` function may be called from _anywhere_
it'll just retrieve all the `Change`s that took place since the last call with the
same `Ns`.

*Arguments*

- The `Handle<Buffer>` that was updated.

### `BufferSwitched`

Triggers whenever you switch the active `Buffer`.

*Arguments*

- The previous `Handle<Buffer>`.
- The current `Handle<Buffer>`.

### `BufferSaved`

Triggers right after saving a `Buffer`. This will happen whenever you call any
of the `write` family of commands, or if `Handle::<Buffer>::save` is called.

*Arguments*

- The `Handle<Buffer>` that was saved.
- A `bool`, which is `true` if the `Buffer` is being closed, through commands
  like `wq`.

### `ConfigLoaded`

Will trigger right after initially loading the config crate on
`~/.config/duat/` or wherever you're loading the config from.

*Arguments*

- There are no arguments, just the normal `&mut Pass`

### `ConfigUnloaded`

Will trigger right before unloading the config crate on
`~/.config/duat/` or wherever you're loading the config from.
This will also trigger upon exiting Duat.

*Arguments*

- Wether we are also about to quit Duat (i.e. after `:q`, `:wq`, etc).

### `FocusedOnDuat`

Triggers when Duat gains focus from the operating system.

*Arguments*

- There are no arguments, just the normal `&mut Pass`

### `UnfocusedFromDuat`

Triggers when Duat loses focus from the operating system.

*Arguments*

- There are no arguments, just the normal `&mut Pass`

### `WidgetOpened<W>`

Triggers when a widget `W` is opened. Its most common form is
`WidgetOpened<Buffer>`, which is aliased to `BufferOpened`.

This hook is very useful for building the layout of Duat. In fact, it's
what places all the widgets on screen, including line numbers, gutters,
etc.

It's also used in other scenarios, like delayed setup of widgets, which
is sometimes necessary when spawning them.

*Arguments*

- The `Handle<W>` that was just opened.

### `WindowOpened`

Triggers when a new window is opened. This includes the first one.

Much like `WidgetOpened`, this hook is very useful for building the
layout of Duat. By default, it's what adds the footer widgets and the
logs.

*Arguments*

- The `Window` that was just opened.

### `ModeSwitched`
