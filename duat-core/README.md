# duat-core ![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-blue) [![duat-core on crates.io](https://img.shields.io/crates/v/duat-core)](https://crates.io/crates/duat-core) [![duat-core on docs.rs](https://docs.rs/duat-core/badge.svg)](https://docs.rs/duat-core) [![Source Code Repository](https://img.shields.io/badge/Code-On%20GitHub-blue?logo=GitHub)](https://github.com/AhoyISki/parsec/tree/master/parsec-core)

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

        let key!(Char(c)) = key else {
            commands::reset_mode();
            return;
        };
        let Some(first) = self.0 else {
            self.0 = Some(c);
            return;
        };

        helper.move_each(|m| {
            let pat: String = [first, c].iter().collect();
            let matched = m.search(pat, None).next();
            if let Some((p0, p1)) = matched {
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                if m.is_incl() {
                    m.move_hor(-1)
                }
            }
        })

        commands::reset_mode();
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
map::<Normal>(keys!(C-"s"),
```


 [__cargo_doc2readme_dependencies_info]: ggGkYW0BYXSEG_W_Gn_kaocAGwCcVPfenh7eGy6gYLEwyIe4G6-xw_FwcbpjYXKEG4kBLKMaG_QMG79IAiU7WMrwG2dolTSCMLjnG0A8bhfPEt1zYWSBg2lkdWF0LWNvcmVlMC4xLjNpZHVhdF9jb3Jl
 [__link0]: https://docs.rs/duat-core/0.1.3/duat_core/?search=input::Mode
 [__link1]: https://docs.rs/duat-core/0.1.3/duat_core/?search=widgets::File
