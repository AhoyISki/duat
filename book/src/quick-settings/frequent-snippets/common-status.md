# Common StatusLine parts

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

## Some examples

This is the default:

```rust
use duat::prelude::*;
setup_duat!(setup);

fn setup() {
    // Default options for the StatusLine widget
    opts::fmt_status(|pa| {
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

fn setup() {
    opts::fmt_status(|_| {
        status!(
            "{name_txt}{Spacer}{} {sels_txt} [coord]c{} l{}[separator]|[coord]{}",
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

fn setup() {
    opts::fmt_status(|_| {
        status!("{name_txt}{Spacer}{} {sels_txt} {main_txt}", mode_txt()) 
    });
}

fn name_txt(buffer: &Buffer) -> Text {
    let mut b = Text::builder();

    if let Some(name) = buffer.name_set() {
        b.push(txt!("[buffer]{name}"));
        if !buffer.exists() {
            b.push(txt!("[buffer.new][[new buffer]]"));
        } else if buffer.text().has_unsaved_changes() {
            b.push(txt!("[buffer.unsaved][[has changes]]"));
        }
        if let Some("rust") = buffer.filetype() {
            b.push(txt!("[[🦀]]"));
        }
    } else {
        b.push(txt!("[buffer.new.scratch]?!?!?!"));
    }

    b.build()
}
```

[forms]: ../prelude/form-module.md
