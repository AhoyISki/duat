use std::sync::Mutex;

use duat_base::modes::{IncSearch, RunCommands, SearchFwd};
use duat_core::{
    Lender,
    mode::{ctrl, shift},
};
#[cfg(feature = "treesitter")]
use duat_treesitter::TsCursor;

use crate::{
    prelude::{
        Handle, cmd,
        data::Pass,
        mode::{
            self,
            KeyCode::{self, *},
            KeyEvent, event,
        },
        ui::Widget,
    },
    widgets::Buffer,
};

/// The regular, bogstandard mode, a.k.a., supposed to be like VSCode
///
/// There are some modifications, mostly because a lot of the commands
/// overlap with default terminal commands, so I had to use slight
/// alternatives.
#[derive(Clone)]
pub struct Regular;

impl mode::Mode for Regular {
    type Widget = Buffer;

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle<Self::Widget>) {
        static LAST_CODE: Mutex<Option<KeyCode>> = Mutex::new(None);
        let mut last_code = LAST_CODE.lock().unwrap();

        match key_event {
            // Characters
            event!(Char(char)) => {
                if !matches!(*last_code, Some(Char(_))) || char == ' ' {
                    handle.write(pa).text_mut().new_moment();
                }
                handle.edit_all(pa, |mut c| {
                    c.replace(char);
                    c.unset_anchor();
                    c.move_hor(1);
                })
            }
            event!(Enter) => {
                handle.write(pa).text_mut().new_moment();
                handle.edit_all(pa, |mut c| {
                    #[cfg(not(feature = "treesitter"))]
                    let indent = c.indent();

                    c.replace('\n');
                    c.unset_anchor();
                    c.move_hor(1);

                    #[cfg(not(feature = "treesitter"))]
                    c.insert(' '.repeat(indent));

                    #[cfg(feature = "treesitter")]
                    c.ts_reindent();
                })
            }

            // Text Removal
            event!(Backspace) => {
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
            event!(Delete) => {
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
            event!(Left) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_hor(-1);
            }),
            event!(Right) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_hor(1);
            }),
            event!(Up) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_ver(-1);
            }),
            event!(Down) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
                c.move_ver(1);
            }),
            shift!(Left) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_hor(-1);
            }),
            shift!(Right) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_hor(1);
            }),
            shift!(Up) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_ver(-1);
            }),
            shift!(Down) => handle.edit_all(pa, |mut c| {
                c.set_anchor_if_needed();
                c.move_ver(1);
            }),

            // Basic commands
            ctrl!('z') => handle.write(pa).text_mut().undo(),
            ctrl!('y' | 'Z') => handle.write(pa).text_mut().redo(),
            ctrl!('x' | 'c') => {
                if let Char('x') = key_event.code {
                    handle.write(pa).text_mut().new_moment();
                }
                let mut prev = Vec::new();
                handle.edit_all(pa, |mut c| {
                    prev.push((c.range(), c.anchor_is_start()));
                    if c.anchor().is_none() {
                        let range = c.text().line_range(c.caret().line());
                        c.move_to(range);
                    }
                });
                crate::mode::copy_selections(pa, &handle);
                let mut ranges = prev.into_iter();
                handle.edit_all(pa, |mut c| {
                    if key_event.code == Char('x') {
                        c.replace("");
                    } else {
                        let (range, anchor_is_start) = ranges.next().unwrap();
                        c.move_to(range);
                        if !anchor_is_start {
                            c.swap_ends();
                        }
                    }
                });
            }
            ctrl!('v') => {
                let pastes = crate::mode::paste_strings();
                if !pastes.is_empty() {
                    handle.write(pa).text_mut().new_moment();
                    let mut p_iter = pastes.iter().cycle();

                    handle.edit_all(pa, |mut c| {
                        let paste = p_iter.next().unwrap();
                        if !paste.is_empty() {
                            let mut c = c.copy();
                            c.set_caret_on_start();
                            c.unset_anchor();
                            if paste.ends_with('\n') {
                                c.move_to_col(0);
                                c.insert(paste);
                            } else {
                                c.insert(paste)
                            }
                            c.destroy();
                        }
                        c.move_hor(paste.chars().count() as i32);
                        if c.anchor().is_some() {
                            c.swap_ends();
                            c.move_hor(paste.chars().count() as i32);
                            c.swap_ends();
                        }
                    });
                }
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
            key!(Char('s'), Mod::CONTROL) => cmd::queue_notify("write"),
            key!(Char('w'), Mod::CONTROL) => cmd::queue_notify("write-quit"),
            key!(Char(','), Mod::CONTROL) => cmd::queue_notify("open --cfg"),

            _ => {}
        }

        *last_code = Some(key_event.code);
    }
}
