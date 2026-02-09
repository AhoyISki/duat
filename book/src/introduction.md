# The Duat Text Editor

Duat is a text editor that is built and _configured_ in the Rust programming 
language. It is meant to be very modular in its design, while still having 
great defaults for anyone used to TUI and modal text editors.

Despite having a rather complex language as its configuration language of choice,
one of the goals of Duat is to have relatively straightforward configuration,
and easy pluginability.

As a motivating example, here's a small plugin, which highlights matches to the 
word underthe cursor:

```rust
use duat::prelude::*;

pub struct HighlightMatches;

impl Plugin for HighlightMatches {
    fn plug(self, _: &Plugins) {
        form::set_weak("same_word", Form::new().underlined());
        let tagger = Tagger::new();

        hook::add::<BufferUpdated>(move |pa, handle| {
            let lines = handle.printed_line_ranges(pa);

            handle.text_mut(pa).remove_tags(tagger, ..);
            let caret = handle.text(pa).main_sel().caret();
            let Some(range) = handle.text(pa).search(r"\A\w+").range(..caret).next_back() else {
                return;
            };

            let start = handle
                .edit_main(pa, |c| c.search(r"\w*\z").to_caret().next_back())
                .map(|range| range.start)
                .unwrap_or(range.start);

            let mut parts = handle.text_parts(pa);
            let pat = &parts.strs[start..range.end];
            let form_id = form::id_of!("same_word");

            for range in lines.into_iter().flat_map(|r| parts.strs.search(r"\w+").range(r)) {
                if &parts.strs[range.clone()] == pat {
                    parts.tags.insert(tagger, range, form_id.to_tag(50));
                }
            }
        });
    }
}
```

The goal of this book is to help you understand not only how to operate Duat, but
hopefully how to do something like this as well.
