# Frequently used snippets

If you just want some no nonsense snippets to copy paste into your config, this 
is the chapter for you.

These should also serve as a good entry point for light modification and learning
by example.

## mapping jk and others to esc

This one is pretty simple. This is normally done on Duat's native `Insert` mode,
but you could replace that with any other `Insert` mode, provided you got the
plugin for one:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    map::<Insert>("jk", "<Esc>");
}
```

This won't print anything to the screen while you're typing, making it seem 
like the `j` key has a bit of delay. If you wish to print `'j'` to the screen, 
use this:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    map::<Insert>("jk", "<Esc>");
}
```

Additionally, if you want to write to the file on `jk` as well, you can do this:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    map::<Insert>("jk", "<Esc>:w<Enter>");
}
```

If you want to, you can also have this behavior on the `PromptLine`, i.e., while
writing commands and searches:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    map::<Prompt>("jk", "<Esc>");
}
```

## `StatusLine` on each `Buffer`

If you want one `StatusLine` on every `Buffer`, you can do that via hooks:

```rust
setup_duat!(setup);
use duat::prelude::*;

fn setup(opts: &mut Opts) {
    hook::add::<BufferOpened>(|pa, buffer| {
        status!("{name_txt}{Spacer}{main_txt}")
            .above()
            .push_on(pa, buffer);
    });
}
```

## Prompt and status on same line

In the Kakoune text editor, the status line occupies the same line as the 
command line and notifications. If you want this behavior in Duat, the 
following snippet is enough:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    opts.one_line_footer = true;
}
```

This will call `FooterWidgets::one_line` on the window's `FooterWidgets`.

If you want one of these on each `Buffer`, you can do this instead:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
	opts.enabled_hooks.default_footer_widgets = false;

    hook::add::<BufferOpened>(|pa, handle| {
        widgets::FooterWidgets::default().one_line().push_on(pa, handle);
    });
}
```

## Common StatusLine parts

The most relevant parts for pretty much every `StatusLine` are the following.

Formatted status parts:

- `name_txt`: Prints the `Buffer`'s name and some info about it's newness.
- Uses the forms `buffer`, `buffer.new`, `buffer.new.scratch` and `buffer.unsaved`.
- `mode_txt`: The lowercased name of the `Mode`, e.g. "insert", "normal".
  - Uses the form `mode`.
- `main_txt`: Prints the main selection's column and line, and the number of 
  lines. 1 indexed.
  - Uses the forms `coord` and `separator`.
- `sels_txt`: Prints the number of selections.
  - Uses the form `selections`;
- `current_sequence_txt`: Prints the keys being mapped.
  - Uses the forms `key` and `key.special`

Unformatted status parts:

- `main_byte`, `main_char`, `main_line`, `main_col`: Parts of the main cursor. 
  1 indexed.
- `mode_name`: The non `Text` version of `mode_txt`, just a string.
- `raw_mode`: The raw type name of the mode. Could look something like 
  `Pager<SomeWidget>`.
- `selections`: The number of selections, no formatting.
- `last_key`: The last key that was typed. Useful for asciinema demonstrations. 

Other:

- `Spacer`: This isn't actually a `StatusPart`, it's a `Tag` that can go in any 
  `Text`, which includes the `StatusLine`'s.
- Forms like `[buffer]`. Any form can be placed within those braces, and they are 
  all evaluated at compile time. For more information about them, see the 
  [forms] chapter

This is the default:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    // Default options for the StatusLine widget
    opts.fmt_status(|pa| {
        // If on one line footer mode:
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{Spacer}{name_txt} {mode} {sels_txt} {param} {main_txt}");
        // If on regular mode (default):
        let mode = mode_txt();
        let param = duat_param_txt();
        status!("{mode} {name_txt}{Spacer}{sels_txt} {param} {main_txt}")
    });
}
```

Customized `main_txt`:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    opts.fmt_status(|_| {
        status!(
            "{name_txt}{Spacer}{} {sels_txt} [coord]s{} l{}[separator]|[coord]{}",
            mode_txt(),
            main_col,
            main_line,
            Buffer::len_lines 
        )
    });
}
```

Customized `name_txt`:

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    opts.fmt_status(|_| {
        status!("{name_txt}{Spacer}{} {sels_txt} {main_txt}", mode_txt()) 
    });
}

fn name_txt(buf: &Buffer) -> Text {
    let mut b = Text::builder();

    if let Some(name) = buf.name_set() {
        b.push(txt!("[buf]{name}"));
        if !buf.exists() {
            b.push(txt!("[buf.new][[new buf]]"));
        } else if buf.has_unsaved_changes() {
            b.push(txt!("[buf.unsaved][[has changes]]"));
        }
        if let Some("rust") = buf.filetype() {
            b.push(txt!("[[🦀]]"));
        }
    } else {
        b.push(txt!("[buf.new.scratch]?!?!?!"));
    }

    b.build()
}
```

## Buffer wise tabstops

If you want to change the tabstop size per `Buffer`, you can just modify the 
following  snippet:

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<BufferOpened>(|pa, buffer| {
        let buf = buffer.write(pa);
        buf.opts.tabstop = match buf.filetype() {
            Some("markdown" | "bash" | "lua" | "javascript" | "commonlisp") => 2, 
            _ => 4
        };
    });
}
```

If you want, you can also set other options with this, like which characters 
should be a part of words. In this case, I'm adding `'-'` to the list:

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<BufferOpened>(|pa, buffer| {
        let buf = buffer.write(pa);
        match buf.filetype() {
            Some("lisp" | "scheme" | "markdown" | "css" | "html") => {
                buf.opts.tabstop = 2;
                buf.opts.extra_word_chars = &['-'];
            }
            Some("bash" | "lua" | "javascript" | "typescript") => {
                buf.opts.tabstop = 2;
            }
            _ => buf.opts.tabstop = 4
        }
    });
}
```

## Nerdfonts StatusLine

> [!IMPORTANT]
> 
> This chapter assumes that you are using some kind of [nerd font] in your
> `Ui`. This also goes for this page in the book. If you are not using
> some kind of nerd font in your browser, you will not be able to see the
> characters being displayed.

If you want to nerd-fontify your `StatusLine`, you can just redefine some of 
the status line parts:

```rust
use duat::prelude::*;

fn name_txt(buf: &Buffer) -> Text {
    let mut b = Text::builder();

    if let Some(name) = buf.name_set() {
        b.push(txt!("[buf]{name}"));
        if !buf.exists() {
            b.push(txt!(" [buf.new]󰎔 "));
        } else if buf.has_unsaved_changes() {
            b.push(txt!(" [buf.unsaved] "));
        }
    } else {
        b.push(txt!(" [buf.new.scratch]{}󰏫 ", buf.name()));
    }

    b.build()
}
```

## Status on Buffers and windows

If you want to have a `StatusLine` per `Buffer`, you can add the following:

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<BufferOpened>(|pa, buffer| {
        status!("{name_txt}{Spacer}{main_txt}").above().push_on(pa, buffer);
    });
}
```

The snippet above will place a `StatusLine` above every single `Buffer`.

You can go further with this, what if you want different `StatusLine`s,
depending on the `Buffer`?

```rust
# use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<BufferOpened>(|pa, buffer| {
        let status = if let Ok(crate_dir) = duat::utils::crate_dir()
            && buffer.read(pa).path().starts_with(crate_dir) {
            status!("{name_txt}[config] []{Spacer}{main_txt}")
        } else {
            status!("{name_txt}{Spacer}{main_txt}")
        };

        status.above().push_on(pa, buffer);
    });
}
```

## Frequent auto saving

One way you could automatically save frequently is by doing
that every time you enter `Normal` mode:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<ModeSwitched>(|pa, switch| {
        if switch.new.is::<Normal>() {
            let buffer = context::current_buffer(pa);
            _ = buffer.save(pa);
        }
    });
}
```

You could also have this behavior only on some languages, or with
some specific conditions:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<ModeSwitched>(|pa, switch| {
        if switch.new.is::<Normal>()
            && let buffer = context::current_buffer(pa)
            && let Some("rust" | "markdown" | "html") = buffer.filetype(pa)
        {
            _ = buffer.save(pa);
        }
    });
}
```

You might also want some frequency of automatic saves on `Insert`
mode. You could do that with certain keys:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup(opts: &mut Opts) {
    hook::add::<KeyTyped>(|pa, key_event| {
        let buffer = context::current_buffer(pa);

        if let Some("rust") = buffer.filetype(pa)
            && let event!('}' | ')' | ']') = key_event
        {
            _ = buffer.save(pa);
        }
    });
}
```

[nerd font]: https://www.nerdfonts.com/font-downloads
[forms]: ../prelude/form-module.md
