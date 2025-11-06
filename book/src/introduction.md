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
    fn plug(self, plugins: &Plugins) {
        hook::add::<Buffer>(|pa, handle| {
            form::set_weak("same_word", Form::underlined());
            handle.write(pa).add_parser(|mut tracker| {
                tracker.track_area();
                HighlightMatchParser {
                    tagger: Tagger::new(),
                }
            })
        });
    }
}

struct HighlightMatchParser {
    tagger: Tagger,
}

impl Parser for HighlightMatchParser {
    fn update(&mut self, pa: &mut Pass, handle: &Handle, on: Vec<Range<Point>>) {
        handle.text_mut(pa).remove_tags(self.tagger, ..);
        let Some(range) = handle.edit_main(pa, |c| c.search_fwd(r"\A\w+").next()) else {
            return;
        };
        let start = handle
            .edit_main(pa, |c| c.search_rev(r"\w*\z").next())
            .map(|range| range.start)
            .unwrap_or(range.start);

        let mut parts = handle.text_parts(pa);
        let pat = parts.bytes.strs(start..range.end);
        let form_id = form::id_of!("same_word");

        let mut first_range: Option<Range<usize>> = None;
        for range in on {
            for (i, range) in parts
                .bytes
                .search_fwd(r"\w+", range.clone())
                .unwrap()
                .filter(|r| parts.bytes.strs(r.clone()) == pat)
                .enumerate()
            {
                if let Some(first_range) = first_range.clone() {
                    if i == 1 {
                        parts
                            .tags
                            .insert(self.tagger, first_range, form_id.to_tag(50));
                    }
                    parts.tags.insert(self.tagger, range, form_id.to_tag(50));
                } else {
                    first_range = Some(range);
                }
            }
        }
    }
}
```

The goal of this book is to help you understand not only how to operate Duat, but
hopefully how to do something like this as well.
