use std::sync::Mutex;

use duat_core::prelude::Lender;
use duat_utils::modes::{IncSearch, RunCommands, SearchFwd};
use duat_treesitter::TsCursor;

use crate::{
    Ui,
    prelude::{
        Handle, cmd,
        data::Pass,
        mode::{
            self,
            KeyCode::{self, *},
            KeyEvent, KeyMod as Mod, key,
        },
        ui::Widget,
    },
    widgets::File,
};

/// The regular, bogstandard mode, a.k.a., supposed to be like VSCode
///
/// There are some modifications, mostly because a lot of the commands
/// overlap with default terminal commands, so I had to use slight
/// alternatives.
#[derive(Clone)]
pub struct Regular;

impl mode::Mode<Ui> for Regular {
    type Widget = File;

    fn send_key(&mut self, pa: &mut Pass, event: KeyEvent, handle: Handle<Self::Widget>) {
        static LAST_CODE: Mutex<Option<KeyCode>> = Mutex::new(None);
        let mut last_code = LAST_CODE.lock().unwrap();

        match event {
            // Characters
            key!(Char(char)) => {
                if !matches!(*last_code, Some(Char(_))) || char == ' ' {
                    handle.write(pa).text_mut().new_moment();
                }
                handle.edit_all(pa, |mut c| {
                    c.insert(char);
                    c.move_hor(1);
                })
            }
            key!(Enter) => {
                handle.write(pa).text_mut().new_moment();
                handle.edit_all(pa, |mut c| {
                    c.insert('\n');
                    c.move_hor(1);
                    c.ts_reindent();
                })
            }

            // Text Removal
            key!(Backspace) => {
                let major_removal = handle.edit_iter(pa, |mut cursors| {
                    cursors.any(|mut c| {
                        c.anchor().is_some()
                            || (c.move_hor(-1) == -1
                                && c.set_anchor_if_needed()
                                && c.selection().any(|str| str.contains('\n')))
                    })
                });
                if !matches!(*last_code, Some(Backspace)) || major_removal {
                    handle.write(pa).text_mut().new_moment();
                }
                handle.edit_all(pa, |mut c| {
                    c.replace("");
                    c.unset_anchor();
                })
            }
            key!(Delete) => {
                let major_removal = handle.edit_iter(pa, |mut cursors| {
                    cursors.any(|mut c| {
                        c.set_anchor_if_needed();
                        c.selection().contains('\n').unwrap()
                    })
                });
                if !matches!(*last_code, Some(Delete)) || major_removal {
                    handle.write(pa).text_mut().new_moment();
                }
                handle.edit_all(pa, |mut c| {
                    c.replace("");
                    c.unset_anchor();
                })
            }

            // Movement
            key!(Left) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_hor(-1);
            }),
            key!(Right) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_hor(1);
            }),
            key!(Up) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_ver(-1);
            }),
            key!(Down) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_ver(1);
            }),
            key!(Left, Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_hor(-1);
            }),
            key!(Right, Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_hor(1);
            }),
            key!(Up, Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_ver(-1);
            }),
            key!(Down, Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_ver(1);
            }),

            // Basic commands
            key!(Char('z'), Mod::CONTROL) => handle.write(pa).text_mut().undo(),
            key!(Char('y' | 'Z'), Mod::CONTROL) => handle.write(pa).text_mut().redo(),
            key!(Char('x' | 'c'), Mod::CONTROL) => {
                if let Char('x') = event.code {
                    handle.write(pa).text_mut().new_moment();
                }
                let mut prev = Vec::new();
                handle.edit_all(pa, |mut c| {
                    prev.push((c.range(), c.anchor_is_start()));
                    if c.anchor().is_none() {
                        let [start, end] = c.text().points_of_line(c.caret().line());
                        c.move_to(start..=end);
                    }
                });
                duat_utils::modes::copy_selections(pa, &handle);
                let mut ranges = prev.into_iter();
                handle.edit_all(pa, |mut c| {
                    if event.code == Char('x') {
                        c.replace("");
                    } else {
                        let (range, anchor_is_start) = ranges.next().unwrap();
                        c.move_to(range[0]..range[1]);
                        if !anchor_is_start {
                            c.swap_ends();
                        }
                    }
                });
            }
            key!(Char('v'), Mod::CONTROL) => {
                handle.write(pa).text_mut().new_moment();
                handle.edit_all(pa, |mut c| {
                    if let Some(text) = duat_core::clipboard::get_text() {
                        let anchor_is_start = c.set_caret_on_start();
                        c.insert(text);
                        if anchor_is_start {
                            c.set_caret_on_end();
                        }
                    }
                })
            }

            // Searching
            key!(Char('f'), Mod::CONTROL) => mode::set(IncSearch::new(SearchFwd)),

            // Control
            key!(Char('P'), Mod::CONTROL) | key!(F(1)) => mode::set(RunCommands::new()),
            key!(Char('p'), Mod::CONTROL) => {
                mode::set(RunCommands::new());
                mode::send_keys("edit ");
            }
            key!(Char('n'), Mod::CONTROL) => {
                mode::set(RunCommands::new());
                mode::send_keys("open ");
            }
            key!(Char('w'), Mod::CONTROL) => cmd::queue_notify("quit"),
            key!(Char(','), Mod::CONTROL) => cmd::queue_notify("open --cfg"),

            _ => {}
        }

        *last_code = Some(event.code);
    }
}
