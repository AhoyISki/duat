# duat-core ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-core on crates.io](https://img.shields.io/crates/v/duat-core)](https://crates.io/crates/duat-core) [![duat-core on docs.rs](https://docs.rs/duat-core/badge.svg)](https://docs.rs/duat-core) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-core)

![sneak demonstration](./assets/sneak-demonstration.gif)

The core of Duat, this crate is meant to be used only for the
creation of plugins for Duat.

## Quick Start

The capabilities of `duat-core` are largely the same as the those
of Duat, however, the main difference is the multi [`Ui`][__link0] APIs of
this crate. In it, the public functions and types are defined in
terms of `U: Ui`,  which means that they can work on various
different interfaces:

```rust
// I recommend pulling the prelude in plugins.
use std::sync::LazyLock;

use duat_core::{prelude::*, text::Point};

static TAGGER: LazyLock<Tagger> = Tagger::new_static();
static CUR_TAGGER: LazyLock<Tagger> = Tagger::new_static();

#[derive(Default, Clone)]
pub struct Sneak {
    step: Step,
}

impl<U: Ui> Mode<U> for Sneak {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: mode::KeyEvent, handle: Handle<File<U>, U>) {
        use mode::{KeyCode::*, KeyMod as Mod};

        let cur_id = form::id_of!("sneak.current");

        match &mut self.step {
            Step::Start => {
                // Make sure that the typed key is a character.
                let mode::key!(Char(c0)) = key else {
                    mode::reset::<File<U>, U>();
                    return;
                };

                let pat = format!("{c0}[A-Za-z0-9]");
                if highlight_matches(pa, &pat, &handle).1.is_none() {
                    handle.write_text(pa, |text| text.remove_tags(*TAGGER, ..));
                    context::error!("No matches found for [a]{pat}");
                    mode::reset::<File<U>, U>();
                    return;
                }

                self.step = Step::Filter(c0);
            }
            Step::Filter(c0) => {
                handle.write_text(pa, |text| text.remove_tags(*TAGGER, ..));

                let mode::key!(Char(c1)) = key else {
                    mode::reset::<File<U>, U>();
                    return;
                };

                let pat = format!("{c0}{c1}");
                let (matches, cur) = highlight_matches(pa, &pat, &handle);

                let Some(cur) = cur else {
                    handle.write_text(pa, |text| text.remove_tags(*TAGGER, ..));
                    context::error!("No matches found for [a]{pat}");
                    mode::reset::<File<U>, U>();
                    return;
                };
                let [p0, p1] = matches[cur];
                handle.write_text(pa, |text| {
                    text.insert_tag(*CUR_TAGGER, p0..p1, cur_id.to_tag(51))
                });

                self.step = Step::Matched(matches, cur);
            }
            Step::Matched(matches, cur) => match (key, mode::alt_is_reverse()) {
                (mode::key!(Char('n')), _) => {
                    let prev = *cur;
                    let last = matches.len() - 1;
                    *cur = if *cur == last { 0 } else { *cur + 1 };

                    handle.write_text(pa, |text| {
                        let [p0, _] = matches[prev];
                        text.remove_tags(*CUR_TAGGER, p0);

                        let [p0, p1] = matches[*cur];
                        text.insert_tag(*CUR_TAGGER, p0..p1, cur_id.to_tag(51));
                    });
                }
                (mode::key!(Char('N')), false) | (mode::key!(Char('n'), Mod::ALT), true) => {
                    let prev = *cur;
                    let last = matches.len() - 1;
                    *cur = if *cur == 0 { last } else { *cur - 1 };

                    handle.write_text(pa, |text| {
                        let [p0, _] = matches[prev];
                        text.remove_tags(*CUR_TAGGER, p0);

                        let [p0, p1] = matches[*cur];
                        text.insert_tag(*CUR_TAGGER, p0..p1, cur_id.to_tag(51));
                    });
                }
                _ => {
                    let [p0, p1] = matches[*cur];

                    handle.edit_main(pa, |mut e| e.move_to(p0..p1));

                    handle.write_text(pa, |text| text.remove_tags([*TAGGER, *CUR_TAGGER], ..));
                    mode::reset::<File<U>, U>();
                }
            },
        }
    }
}

fn highlight_matches<U: Ui>(
    pa: &mut Pass,
    pat: &str,
    handle: &Handle<File<U>, U>,
) -> (Vec<[Point; 2]>, Option<usize>) {
    handle.write(pa, |file, area| {
        let (start, _) = area.start_points(file.text(), file.print_cfg());
        let (end, _) = area.end_points(file.text(), file.print_cfg());
        let caret = file.selections().get_main().unwrap().caret();

        let (bytes, mut tags) = file.text_mut().bytes_and_tags();

        let matches: Vec<_> = bytes.search_fwd(pat, start..end).unwrap().collect();

        let id = form::id_of!("sneak.match");

        let tagger = *TAGGER;
        let mut next = None;
        for (i, &[p0, p1]) in matches.iter().enumerate() {
            if p0 > caret && next.is_none() {
                next = Some(i);
            }
            tags.insert(tagger, p0..p1, id.to_tag(50));
        }

        let last = matches.len().checked_sub(1);
        (matches, next.or(last))
    })
}

#[derive(Default, Clone)]
enum Step {
    #[default]
    Start,
    Filter(char),
    Matched(Vec<[Point; 2]>, usize),
}
```

In this example, I have created a [`Mode`][__link1] for [`File`][__link2]s. This
mode is based on [`vim-sneak`][__link3], which is popular (I think) within
Vim circles. It’s like the `f` key in Vim, but it lets you look
for a sequence of 2 characters, instead of just one, also letting
you pick between matches ahead and behind on the screen.

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
configuration, you should probably just rely on [`map`][__link4] to
accomplish the same thing.

Okay, but that was a relatively simple example, here’s a more
advanced example, which makes use of more of Duat’s features.

This is a copy of [EasyMotion][__link5], a plugin for
Vim/Neovim/Kakoune/Emacs that lets you skip around the screen with
at most 2 keypresses.

In order to emulate it, we use [ghost text][__link6] and [concealment][__link7]:

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
* In the beginning of said words/lines, add a [`Ghost`][__link8];
* Also add a [`Conceal`][__link9];
* Then, just match the typed keys and [remove][__link10] tags accordingly;
* [Move][__link11] to the matched sequence, if it exists;

Now, in order to use this mode, it’s the exact same thing as
`Sneak`:

```rust
map::<Normal>("<CA-w>", EasyMotion::word());
map::<Normal>("<CA-l>", EasyMotion::line());
```


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG7OnKS_PkemFG56IpJ_QmFWVGx-ENAHaG7xgG84zvdQl8TPsYXKEG3iZRkqif6hpG9721XxfHELdG9StpAm4nCL4G1hLfc0Ir397YWSBg2lkdWF0LWNvcmVlMC41LjFpZHVhdF9jb3Jl
 [__link0]: https://docs.rs/duat-core/0.5.1/duat_core/?search=ui::Ui
 [__link1]: https://docs.rs/duat-core/0.5.1/duat_core/?search=mode::Mode
 [__link10]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Text::remove_tags
 [__link11]: https://docs.rs/duat-core/0.5.1/duat_core/?search=mode::Cursor::move_to
 [__link2]: https://docs.rs/duat-core/0.5.1/duat_core/?search=file::File
 [__link3]: https://github.com/justinmk/vim-sneak
 [__link4]: https://docs.rs/duat/0.2.0/duat/prelude/fn.map.html
 [__link5]: https://github.com/easymotion/vim-easymotion
 [__link6]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Ghost
 [__link7]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Conceal
 [__link8]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Ghost
 [__link9]: https://docs.rs/duat-core/0.5.1/duat_core/?search=text::Conceal
