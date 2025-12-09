use std::sync::atomic::Ordering;

use duat_base::widgets::Completions;
use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    lender::Lender,
    mode::{self, Cursor, KeyEvent, KeyMod, Mode, alt, ctrl, event, shift},
};
use treesitter::TsCursor;

use crate::{Normal, opts::INSERT_TABS, set_anchor_if_needed};

#[derive(Clone, Copy)]
pub struct Insert;

impl Mode for Insert {
    type Widget = Buffer;

    fn bindings() -> mode::Bindings {
        use duat_core::text::txt;
        use mode::KeyCode::*;

        mode::bindings!(match _ {
            event!(Char(..) | Enter) => txt!("Insert the character"),
            event!(Left | Down | Up | Right) => txt!("Move cursor"),
            shift!(Left | Down | Up | Right) => txt!("Select and move cursor"),
            event!(Home | End) => txt!("Move to [a]start[][separator],[a]end[] of line"),
            ctrl!('n') => txt!("Next completion entry"),
            ctrl!('p') | shift!(BackTab) => txt!("Previous completion entry"),
            event!(Tab) => txt!("Reindent or next completion entry"),
            event!(Backspace | Delete) => txt!("Remove character or selection"),
            event!(Esc) => txt!("Return to [mode]Normal[] mode"),
            alt!(';') => txt!("Run a single [mode]Normal[] mode command"),
            ctrl!('u') => txt!("Merge changes to this point in a single [a]Moment"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle) {
        use mode::KeyCode::*;

        let opts = crate::opts::get();
        if let shift!(Left | Down | Up | Right) = key_event {
            handle.edit_all(pa, |mut c| {
                if c.anchor().is_none() {
                    c.set_anchor()
                }
            });
        }

        match key_event {
            // Autocompletion commands
            ctrl!('n') => Completions::scroll(pa, 1),
            ctrl!('p') | shift!(BackTab) => Completions::scroll(pa, -1),
            event!(Tab) => match opts.tab_mode {
                TabMode::Normal => handle.edit_all(pa, |mut c| {
                    if opts.indent_chars.contains(&'\t') {
                        c.ts_reindent(false);
                    }

                    if INSERT_TABS.load(Ordering::Relaxed) {
                        c.insert('\t');
                        c.move_hor(1);
                    } else {
                        let tab_len = c.opts().tabstop_spaces_at(c.v_caret().visual_col() as u32);
                        c.insert(" ".repeat(tab_len as usize));
                        c.move_hor(tab_len as i32);
                    }
                }),
                TabMode::Smart => handle.edit_all(pa, |mut c| {
                    let char_col = c.v_caret().char_col();
                    if (opts.indent_chars.contains(&'\t') || char_col <= c.indent())
                        && c.ts_reindent(false)
                    {
                        return;
                    }

                    if INSERT_TABS.load(Ordering::Relaxed) {
                        c.insert('\t');
                        c.move_hor(1);
                    } else {
                        let tab_len = c.opts().tabstop_spaces_at(c.v_caret().visual_col() as u32);
                        c.insert(" ".repeat(tab_len as usize));
                        c.move_hor(tab_len as i32);
                    }
                }),
                TabMode::VerySmart => {
                    let do_scroll = handle.edit_main(pa, |mut c| {
                        let char_col = c.v_caret().char_col();
                        !((opts.indent_chars.contains(&'\t') || char_col <= c.indent())
                            && c.ts_reindent(false))
                    });

                    if do_scroll {
                        Completions::scroll(pa, 1);
                    }
                }
            },

            // Regular commands
            event!(Char(char)) => handle.edit_all(pa, |mut c| {
                c.insert(char);
                c.move_hor(1);
                if opts.indent_chars.contains(&char) && c.indent() == c.v_caret().char_col() - 1 {
                    c.ts_reindent(false);
                }
            }),

            event!(Enter) => handle.edit_all(pa, |mut c| {
                c.insert('\n');
                c.move_hor(1);
                if opts.indent_chars.contains(&'\n') {
                    c.ts_reindent(opts.auto_indent);
                }
            }),
            event!(Backspace) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();

                if c.move_hor(-1) < 0 {
                    c.set_anchor();
                    c.replace("");
                    c.unset_anchor();

                    if let Some(prev_anchor) = prev_anchor {
                        c.set_anchor();
                        if prev_anchor > prev_caret {
                            c.move_hor((prev_anchor.char() - prev_caret.char()) as i32);
                        } else {
                            c.move_to(prev_anchor);
                        }
                        c.swap_ends();
                    }
                }
            }),
            event!(Delete) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();
                c.replace("");
                if let Some(prev_anchor) = prev_anchor {
                    c.set_anchor();
                    if prev_anchor > prev_caret {
                        c.move_hor((prev_anchor.char() - prev_caret.char()) as i32 - 1);
                    } else {
                        c.move_to(prev_anchor);
                    }
                    c.swap_ends();
                }
            }),
            event!(Left) | shift!(Left) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                c.move_hor(-1);
            }),
            event!(Down) | shift!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                if key_event.modifiers == KeyMod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                c.move_ver_wrapped(1);
            }),
            event!(Up) | shift!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                if key_event.modifiers == KeyMod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                c.move_ver_wrapped(-1)
            }),
            event!(Right) | shift!(Right) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                c.move_hor(1);
            }),

            event!(Home) => handle.edit_all(pa, |mut c| c.move_to_col(0)),
            event!(End) => handle.edit_all(pa, |mut c| c.move_to_col(usize::MAX)),

            event!(Esc) => {
                handle.text_mut(pa).new_moment();
                mode::set(Normal::new());
            }
            alt!(';') => mode::set(Normal::only_one_action()),
            ctrl!('u') => handle.text_mut(pa).new_moment(),
            _ => {}
        }
    }

    fn on_switch(&mut self, pa: &mut Pass, handle: Handle) {
        Completions::open_default(pa);
        handle.set_mask("Insert");
    }

    fn before_exit(&mut self, pa: &mut Pass, _: Handle<Self::Widget>) {
        Completions::close(pa)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TabMode {
    Normal,
    Smart,
    VerySmart,
}

/// removes an empty line
fn remove_empty_line(c: &mut Cursor) {
    let mut lines = c.lines_on(c.caret()..);
    let (_, line) = lines.next().unwrap();
    if !line.chars().all(char::is_whitespace) || line.is_empty() {
        return;
    }
    let chars_count = line.chars().count();

    let dvcol = c.v_caret().desired_visual_col();
    c.move_to_col(0);
    c.set_anchor();
    c.move_hor(chars_count as i32 - 1);

    c.replace("");
    c.unset_anchor();
    c.set_desired_vcol(dvcol);
}
