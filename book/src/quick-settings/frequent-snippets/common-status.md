# Common StatusLine parts

The most relevant parts for pretty much every `StatusLine` are the following.

Formatted status parts:

- `file_fmt`: Prints the `File`'s name and some info about it's newness.
- Uses the forms `file`, `file.new`, `file.new.scratch` and `file.unsaved`.
- `mode_fmt`: The lowercased name of the `Mode`, e.g. "insert", "normal".
  - Uses the form `mode`.
- `main_fmt`: Prints the main selection's column and line, and the number of 
  lines. 1 indexed.
  - Uses the forms `coord` and `separator`.
- `sels_fmt`: Prints the number of selections.
  - Uses the form `selections`;
- `cur_map_fmt`: Prints the keys being mapped.
  - Uses the forms `key` and `key.special`

Unformatted status parts:

- `main_byte`, `main_char`, `main_line`, `main_col`: Parts of the main cursor.
- `mode_name`: The raw type name of the mode. Could look something like 
  `Prompt<IncSearcher<SearchFwd>, Ui>, Ui>`.
- `selections`: The number of selections, no formatting.
- `last_key`: The last key that was typed. Useful for asciinema demonstrations. 

Other:

- `Spacer`: This isn't actually a `StatusPart`, it's a `Tag` that can go in any 
  `Text`, which includes the `StatusLine`'s.
- `AlignLeft`, `AlignCenter`, `AlignRight`: These `Tag`s (like all others) can 
  also be used in the `StatusLine`. However, do note that they are applied 
  _line wise_. Using any of them will shift the _whole line_'s alignment. For 
  that reason, a `Spacer` should generally be preferred.
- Forms like `[file]`. Any form can be placed within those braces, and they are 
  all evaluated at compile time.

## Some examples

This is the default:

```rust
# use duat::prelude::*;
hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
    cfg.fmt(status!("{file_txt}{Spacer}{} {sels_txt} {main_txt}", mode_txt(pa)))
});
```

If you want a one sided `StatusLine`, you can do this:

```rust
# use duat::prelude::*;
hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
    cfg.fmt(status!("{Spacer}{file_txt} {} {sels_txt} {main_txt}", mode_txt(pa)))
});
```

Customized `main_fmt`:

```rust
# use duat::prelude::*;
hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
    cfg.fmt(status!(
        "{file_txt}{Spacer}{} {sels_txt} [coord]c{main_col} l{main_line}[separator]|[coord]{}",
        mode_txt(pa),
        |file: &File| file.text().len().line()
    ))
});
```

Customized `file_fmt`:

```rust
# use duat::prelude::*;

fn file_fmt(file: &File) -> Text {
    let mut b = Text::builder();

    if let Some(name) = file.name_set() {
        b.push(txt!("[file]{name}"));
        if !file.exists() {
            b.push(txt!("[file.new][[new file]]"));
        } else if file.text().has_unsaved_changes() {
            b.push(txt!("[file.unsaved][[has changes]]"));
        }
        if let Some("rust") = file.filetype() {
            b.push(txt!("[[🦀]]"));
        }
    } else {
        b.push(txt!("[file.new.scratch]?!?!?!"));
    }

    b.build()
}

hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
    cfg.fmt(status!("{file_txt}{Spacer}{} {sels_txt} {main_txt}", mode_txt(pa)))
});
```
