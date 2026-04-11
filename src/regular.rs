use std::sync::Mutex;

use duat_base::modes::{IncSearch, RunCommands, SearchFwd};
use duat_core::mode::{ctrl, shift};
#[cfg(feature = "treesitter")]
use duat_treesitter::TsHandle;

use crate::{
    prelude::{
        Handle, cmd,
        data::Pass,
        mode::{
            self,
            KeyCode::{self, *},
            KeyEvent, event,
        },
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
                handle.edit_all(pa, |mut s| {
                    s.replace(char);
                    s.unset_anchor();
                    s.move_hor(1);
                })
            }
            event!(Enter) => {
                handle.write(pa).text_mut().new_moment();
                handle.edit_all(pa, |mut s| {
                    #[cfg(not(feature = "treesitter"))]
                    let indent = s.indent();

                    s.replace('\n');
                    s.unset_anchor();
                    s.move_hor(1);

                    #[cfg(not(feature = "treesitter"))]
                    s.insert(" ".repeat(indent));
                });

                #[cfg(feature = "treesitter")]
                if let Some(indents) = handle.ts_get_indentations(pa, ..) {
                    let mut indents = indents.into_iter();
                    handle.edit_all(pa, |mut s| {
                        duatmode::reindent(s.indent(), indents.next().unwrap(), &mut s);
                    });
                }
            }

            // Text Removal
            event!(Backspace) => {
                let mut major_removal = false;
                handle.edit_all(pa, |mut s| {
                    if s.anchor().is_some()
                        || (s.move_hor(-1) != 0
                            && s.set_anchor_if_needed()
                            && s.selection().chars().any(|s| s == '\n'))
                    {
                        major_removal = true;
                    }
                });

                if !matches!(*last_code, Some(Backspace)) || major_removal {
                    handle.write(pa).text_mut().new_moment();
                }

                handle.edit_all(pa, |mut s| {
                    s.replace("");
                    s.unset_anchor();
                })
            }
            event!(Delete) => {
                let mut major_removal = false;
                handle.edit_all(pa, |mut s| {
                    s.set_anchor_if_needed();
                    major_removal |= s.selection().chars().any(|s| s == '\n');
                });

                if !matches!(*last_code, Some(Delete)) || major_removal {
                    handle.write(pa).text_mut().new_moment();
                }

                handle.edit_all(pa, |mut s| {
                    s.replace("");
                    s.unset_anchor();
                })
            }

            // Movement
            event!(Left) => handle.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_hor(-1);
            }),
            event!(Right) => handle.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_hor(1);
            }),
            event!(Up) => handle.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_ver(-1);
            }),
            event!(Down) => handle.edit_all(pa, |mut s| {
                s.unset_anchor();
                s.move_ver(1);
            }),
            shift!(Left) => handle.edit_all(pa, |mut s| {
                s.set_anchor_if_needed();
                s.move_hor(-1);
            }),
            shift!(Right) => handle.edit_all(pa, |mut s| {
                s.set_anchor_if_needed();
                s.move_hor(1);
            }),
            shift!(Up) => handle.edit_all(pa, |mut s| {
                s.set_anchor_if_needed();
                s.move_ver(-1);
            }),
            shift!(Down) => handle.edit_all(pa, |mut s| {
                s.set_anchor_if_needed();
                s.move_ver(1);
            }),

            // Basic commands
            ctrl!('z') => handle.write(pa).text_mut().undo(),
            ctrl!('y' | 'Z') => handle.write(pa).text_mut().redo(),
            ctrl!(char @ ('x' | 's')) => {
                if char == 'x' {
                    handle.write(pa).text_mut().new_moment();
                }
                let mut prev = Vec::new();
                handle.edit_all(pa, |mut s| {
                    prev.push((s.range(), s.anchor_is_start()));
                    if s.anchor().is_none() {
                        let range = s.text().line(s.cursor().line()).byte_range();
                        s.move_to(range);
                    }
                });
                crate::mode::copy_selections(pa, &handle);
                let mut ranges = prev.into_iter();
                handle.edit_all(pa, |mut s| {
                    if char == 'x' {
                        s.replace("");
                    } else {
                        let (range, anchor_is_start) = ranges.next().unwrap();
                        s.move_to(range);
                        if !anchor_is_start {
                            s.swap_ends();
                        }
                    }
                });
            }
            ctrl!('v') => {
                let pastes = crate::mode::paste_strings();
                if !pastes.is_empty() {
                    handle.write(pa).text_mut().new_moment();
                    let mut p_iter = pastes.iter().cycle();

                    handle.edit_all(pa, |mut s| {
                        let paste = p_iter.next().unwrap();
                        if !paste.is_empty() {
                            let mut s = s.copy();
                            s.set_cursor_on_start();
                            s.unset_anchor();
                            if paste.ends_with('\n') {
                                s.move_to_col(0);
                                s.insert(paste);
                            } else {
                                s.insert(paste)
                            }
                            s.destroy();
                        }
                        s.move_hor(paste.chars().count() as i32);
                        if s.anchor().is_some() {
                            s.swap_ends();
                            s.move_hor(paste.chars().count() as i32);
                            s.swap_ends();
                        }
                    });
                }
            }

            // Searching
            ctrl!('f') => mode::set(pa, IncSearch::new(SearchFwd)),

            // Control
            ctrl!('P') | event!(F(1)) => mode::set(pa, RunCommands::new()),
            ctrl!('p') => mode::set(pa, RunCommands::new_with("edit ")),
            ctrl!('n') => mode::set(pa, RunCommands::new_with("open ")),
            event!('s') => cmd::queue_notify("write"),
            event!('w') => cmd::queue_notify("write-quit"),
            event!(',') => cmd::queue_notify("open --cfg"),

            _ => {}
        }

        *last_code = Some(key_event.code);
    }
}
