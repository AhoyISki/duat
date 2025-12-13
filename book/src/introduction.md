# The Duat Text Editor

Duat is a text editor that is built and _configured_ in the Rust programming 
language. It is meant to be very modular in its design, while still having 
great defaults for anyone used to TUI and modal text editors.

Despite having a rather complex language as its configuration language of choice,
one of the goals of Duat is to have relatively straightforward configuration,
and easy pluginability.

As a motivating example, here's a small plugin, which shows matches to the word under
the cursor:

```rust
use duat::prelude::*;

pub struct HighlightMatch;

impl Plugin for HighlightMatch {
    fn plug(self, _: &Plugins) {
        form::set_weak("same_word", Form::underlined());
        let tagger = Tagger::new();

        hook::add::<BufferUpdated>(move |pa, handle| {
            let lines = handle.printed_line_byte_ranges(pa);

            handle.text_mut(pa).remove_tags(tagger, ..);
            let Some(range) = handle.edit_main(pa, |c| c.search(r"\A\w+").from_caret().next())
            else {
                return Ok(());
            };

            let start = handle
                .edit_main(pa, |c| c.search(r"\w*\z").to_caret().next_back())
                .map(|range| range.start)
                .unwrap_or(range.start);

            let mut parts = handle.text_parts(pa);
            let pat = parts.bytes.strs(start..range.end);
            let form_id = form::id_of!("same_word");
            
            for range in lines {
                for (i, range) in parts
                    .bytes
                    .search(r"\w+")
                    .range(range.clone())
                    .filter(|r| parts.bytes.strs(r.clone()) == pat)
                    .enumerate()
                {
                    parts.tags.insert(tagger, range, form_id.to_tag(50));
                }
            }

            Ok(())
        });
    }
}
```

The goal of this book is to help you understand not only how to operate Duat, but
hopefully how to do something like this as well.
