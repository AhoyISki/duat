# duat-core ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-core on crates.io](https://img.shields.io/crates/v/duat-core)](https://crates.io/crates/duat-core) [![duat-core on docs.rs](https://docs.rs/duat-core/badge.svg)](https://docs.rs/duat-core) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-core)

The core of Duat, this crate is meant to be used only for the
creation of plugins for Duat.

## Quick Start

The capabilities of `duat-core` are largely the same as the those
of Duat, however, the main difference is the multi [`Ui`][__link0] APIs of
this crate. In it, the public functions and types are defined in
terms of `U: Ui`,  which means that they can work on various
different interfaces:

What’s great about it is that it will work no matter what editing
model the user is using. It could be Vim inspired, Kakoune
inspired, Emacs inspired, doesn’t matter. All the user has to do
to use this mode is this:

```rust
map::<Normal>("<C-s>", Sneak::default());
```

And now, whenever the usert types `Control S` in `Normal` mode,
the mode will switch to `Sneak`. You could replace `Normal` with
any other mode, from any other editing model, and this would still
work.

Of course, this is most useful for plugins, for your own
configuration, you should probably just rely on [`map`][__link1] to
accomplish the same thing.

This is a copy of [EasyMotion][__link2], a plugin for
Vim/Neovim/Kakoune/Emacs that lets you skip around the screen with
at most 2 keypresses.

In order to emulate it, we use [ghost text][__link3] and [concealment][__link4]:

```rust
use duat_core::{prelude::*, text::Point};
#[derive(Clone)]
pub struct EasyMotion {
    is_line: bool,
    key: Tagger,
    points: Vec<[Point; 2]>,
    seq: String,
}

impl EasyMotion {
    pub fn word() -> Self {
        Self {
            is_line: false,
            key: Tagger::new(),
            points: Vec::new(),
            seq: String::new(),
        }
    }

    pub fn line() -> Self {
        Self {
            is_line: true,
            key: Tagger::new(),
            points: Vec::new(),
            seq: String::new(),
        }
    }
}

impl<U: Ui> Mode<U> for EasyMotion {
    type Widget = File<U>;

    fn on_switch(&mut self, pa: &mut Pass, handle: Handle<File<U>, U>) {
        handle.write(pa, |file, _| {
            let cfg = file.print_cfg();
            let text = file.text_mut();

            let regex = if self.is_line {
                "[^\n\\s][^\n]+"
            } else {
                "[^\n\\s]+"
            };

            let (start, _) = handle.area().start_points(text, cfg);
            let (end, _) = handle.area().end_points(text, cfg);
            self.points = text.search_fwd(regex, start..end).unwrap().collect();

            let seqs = key_seqs(self.points.len());

            for (seq, [p0, _]) in seqs.iter().zip(&self.points) {
                let ghost = Ghost(txt!("[easy_motion.word]{seq}"));
                text.insert_tag(self.key, *p0, ghost);

                let seq_end = p0.byte() + seq.chars().count();
                text.insert_tag(self.key, p0.byte()..seq_end, Conceal);
            }
        });
    }

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<File<U>, U>) {
        let char = match key {
            key!(KeyCode::Char(c)) => c,
            // Return a char that will never match.
            _ => '❌',
        };
        self.seq.push(char);

        handle.write_selections(pa, |c| c.remove_extras());

        let seqs = key_seqs(self.points.len());
        for (seq, &[p0, p1]) in seqs.iter().zip(&self.points) {
            if *seq == self.seq {
                handle.edit_main(pa, |mut e| {
                    e.move_to(p0);
                    e.set_anchor();
                    e.move_to(p1);
                });
                mode::reset::<File<U>, U>();
            } else if seq.starts_with(&self.seq) {
                continue;
            }

            // Removing one end of the conceal range will remove both ends.
            handle.write_text(pa, |text| text.remove_tags(self.key, p1.byte()));
        }

        if self.seq.chars().count() == 2 || !LETTERS.contains(char) {
            mode::reset::<File<U>, U>();
        }
    }
}

fn key_seqs(len: usize) -> Vec<String> {
    let double = len / LETTERS.len();

    let mut seqs = Vec::new();
    seqs.extend(LETTERS.chars().skip(double).map(char::into));

    let chars = LETTERS.chars().take(double);
    seqs.extend(chars.flat_map(|c1| LETTERS.chars().map(move |c2| format!("{c1}{c2}"))));

    seqs
}

static LETTERS: &str = "abcdefghijklmnopqrstuvwxyz";
```

All that this plugin is doing is:

* Search on the screen for words/lines;
* In the beginning of said words/lines, add a [`Ghost`][__link5];
* Also add a [`Conceal`][__link6];
* Then, just match the typed keys and [remove][__link7] tags accordingly;
* [Move][__link8] to the matched sequence, if it exists;

Now, in order to use this mode, it’s the exact same thing as
`Sneak`:

```rust
map::<Normal>("<CA-w>", EasyMotion::word());
map::<Normal>("<CA-l>", EasyMotion::line());
```


# Plugin examples

## `duat-sneak`

![duat sneak demonstration](../assets/new-demontration.gif)

[`duat-sneak`], inspired by [`vim-sneak`], lets you traverse the
screen by searching through character sequences.

abcde

[`duat-sneak`]: https://github.com/AhoyISki/duat-sneak
[`vim-sneak`]: https://github.com/justinmk/vim-sneak
 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG5OkJcgecQu9G-7ULsYFztE3G0VDp5QmqsNwGxojJZu3dIVvYXKEG4sEp1FGs1qkGzpiuCkL9zwqG0Ef9oCA893OG4ztl0I7Rre9YWSBg2lkdWF0LWNvcmVlMC41LjFpZHVhdF9jb3Jl
 [__link0]: https://docs.rs/duat-core/0.5.1/duat_core/?search=ui::Ui
 [__link1]: https://docs.rs/duat/0.2.0/duat/prelude/fn.map.html
 [__link2]: https://github.com/easymotion/vim-easymotion
 [__link3]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Ghost
 [__link4]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Conceal
 [__link5]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Ghost
 [__link6]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Conceal
 [__link7]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Text::remove_tags
 [__link8]: https://docs.rs/duat-core/0.5.1/duat_core/?search=mode::Cursor::move_to
