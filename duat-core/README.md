# duat-core ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-core on crates.io](https://img.shields.io/crates/v/duat-core)](https://crates.io/crates/duat-core) [![duat-core on docs.rs](https://docs.rs/duat-core/badge.svg)](https://docs.rs/duat-core) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/duat/tree/master/duat-core)

## duat-core

The core of Duat, this crate is meant to be used only for the
creation of plugins for Duat.

## Quick Start

The capabilities of `duat-core` are largely the same as the
capabilities of Duat, however, the main difference is the multi
interface capabilities of this crate. In this crate, the
interfaces are defined in terms of `U: Ui`,  which means that they
can work on various different interfaces:

```rust
#[derive(Default, Clone)]
struct FindSeq(Option<char>);

impl<U: Ui> Mode<U> for FindSeq {
    type Widget = File;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &<U as Ui>::Area,
        cursors: &mut Cursors,
    ) {
        use KeyCode::*;
        let mut helper = EditHelper::new(widget, area, cursors);

        // Make sure that the typed key is a character.
        let key!(Char(c)) = key else {
            mode::reset();
            return;
        };
        // Checking if a character was already sent.
        let Some(first) = self.0 else {
            self.0 = Some(c);
            return;
        };

        helper.move_each(|mut m| {
            let pat: String = [first, c].iter().collect();
            let matched = m.search_fwd(pat, None).next();
            if let Some((p0, p1)) = matched {
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                if m.is_incl() {
                    m.move_hor(-1)
                }
            }
        });

        mode::reset();
    }
}
```

In this example, I have created a [`Mode`][__link0] for [`File`][__link1]s. This
mode is (I think) popular within Vim circles. It’s like the `f`
key in Vim, but it lets you look for a sequence of 2 characters,
inst ead of just one.

What’s great about this mode is that it will work no matter what
editing model the user is using. It could be Vim inspired, Kakoune
inspired, Emacs inspired, doesn’t matter. All the user has to do
to use this mode is this:

```rust
map::<Normal>("<C-s>", &FindSeq::default());
```

And now, whenever the usert types Control S in `Normal` mode, the
mode will switch to `FindSeq`. You could replace `Normal` with any
other mode, from any other editing model, and this would still
work.

Of course, this is most useful for plugins, for your own
configuration, you should probably just rely on [`map`][__link2] to
accomplish the same thing.

Okay, but that was a relatively simple example, here’s a more
advanced example, which makes use of more of Duat’s features.

This is a copy of [EasyMotion][__link3], a plugin for Vim/Neovim/Kakoune
that lets you skip around the screen with at most 2 keypresses.

In order to emulate it, we use [ghost text][__link4] and [concealment][__link5]:

```rust
#[derive(Clone)]
pub struct EasyMotion {
    is_line: bool,
    key: Key,
    points: Vec<(Point, Point)>,
    seq: String,
}

impl EasyMotion {
    pub fn word() -> Self {
        Self {
            is_line: false,
            key: Key::new(),
            points: Vec::new(),
            seq: String::new(),
        }
    }

    pub fn line() -> Self {
        Self {
            is_line: true,
            key: Key::new(),
            points: Vec::new(),
            seq: String::new(),
        }
    }
}

impl<U: Ui> Mode<U> for EasyMotion {
    type Widget = File;

    fn on_switch(
        &mut self,
        widget: &RwData<Self::Widget>,
        area: &<U as Ui>::Area,
        _cursors: &mut Cursors,
    ) {
        let mut widget = widget.write();
        let cfg = widget.print_cfg();
        let text = widget.text_mut();

        let regex = match self.is_line {
            true => "[^\n\\s][^\n]+",
            false => "[^\n\\s]+",
        };
        let start = area.first_point(text, cfg);
        let end = area.last_point(text, cfg);
        self.points = text.search_fwd(regex, start, Some(end)).unwrap().collect();

        let seqs = key_seqs(self.points.len());

        for (seq, (p1, _)) in seqs.iter().zip(&self.points) {
            let ghost = text!([EasyMotionWord] seq);

            text.insert_tag(p1.byte(), Tag::GhostText(ghost), self.key);
            text.insert_tag(p1.byte(), Tag::StartConceal, self.key);
            let seq_end = p1.byte() + seq.chars().count() as u32;
            text.insert_tag(seq_end, Tag::EndConceal, self.key);
        }
    }

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &<U as Ui>::Area,
        cursors: &mut Cursors,
    ) {
        let char = match key {
            key!(KeyCode::Char(c)) => c,
            // Return a char that will never match.
            _ => '❌'
        };
        self.seq.push(char);

        let mut helper = EditHelper::new(widget, area, cursors);
        helper.remove_extra_cursors();

        let seqs = key_seqs(self.points.len());
        for (seq, &(p1, p2)) in seqs.iter().zip(&self.points) {
            if *seq == self.seq {
                helper.move_main(|mut m| {
                    m.move_to(p1);
                    m.set_anchor();
                    m.move_to(p2);
                });
                mode::reset();
            } else if seq.starts_with(&self.seq) {
                continue;
            }

            helper.remove_tags_on(p1.byte(), self.key);
            helper.remove_tags_on(p1.byte() + seq.len() as u32, self.key);
        }

        if self.seq.chars().count() == 2 || !LETTERS.contains(char) {
            mode::reset();
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


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG6WtGrZaoU98GxOP7YwSWF3jG4_szzkj5qBsG97s12V32WG8YWSBg2lkdWF0LWNvcmVlMC4yLjFpZHVhdF9jb3Jl
 [__link0]: https://docs.rs/duat-core/0.2.1/duat_core/?search=mode::Mode
 [__link1]: https://docs.rs/duat-core/0.2.1/duat_core/?search=widgets::File
 [__link2]: https://docs.rs/duat/0.2.0/duat/prelude/fn.map.html
 [__link3]: https://github.com/easymotion/vim-easymotion
 [__link4]: https://docs.rs/duat-core/0.2.1/duat_core/?search=text::Tag::GhostText
 [__link5]: https://docs.rs/duat-core/0.2.1/duat_core/?search=text::Tag::StartConceal
