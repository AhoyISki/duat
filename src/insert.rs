use std::sync::atomic::{AtomicBool, Ordering};

use duat_base::widgets::Completions;
use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    lender::Lender,
    mode::{self, Cursor, KeyEvent, KeyMod, Mode, ctrl, event, shift},
};
use treesitter::TsCursor;

use crate::{Normal, set_anchor_if_needed};

#[derive(Clone)]
pub struct Insert {
    insert_tabs: bool,
    indent_keys: Vec<char>,
}

impl Insert {
    /// Returns a new instance of Kakoune's [`Insert`]
    pub fn new() -> Self {
        Self {
            insert_tabs: INSERT_TABS.load(Ordering::Relaxed),
            indent_keys: vec!['\n', '\t', '(', ')', '{', '}', '[', ']'],
        }
    }

    /// Returns Kakoune's [`Insert`] mode, inserting tabs
    pub fn with_tabs(self) -> Self {
        Self { insert_tabs: true, ..self }
    }

    /// Returns Kakoune's [`Insert`] mode, not inserting tabs
    pub fn without_tabs(self) -> Self {
        Self { insert_tabs: false, ..self }
    }

    /// Which [`char`]s, when sent, should reindent the line
    ///
    /// By default, this is `'\n'` and the `'('`, `'{'`, `'['` pairs.
    /// Note that you have to include `'\n'` in order for the
    /// [`Enter`] key to reindent.
    ///
    /// Additionally, if you add '\t' to the list of indent keys, upon
    /// pressint [`Tab`] on the first character of the line, it will
    /// automatically be indented by the right amount.
    pub fn with_indent_keys(self, chars: impl Iterator<Item = char>) -> Self {
        Self { indent_keys: chars.collect(), ..self }
    }
}

impl Default for Insert {
    fn default() -> Self {
        Self::new()
    }
}

impl Mode for Insert {
    type Widget = Buffer;

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle) {
        use mode::KeyCode::*;

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
            event!(Tab) => match (crate::opts::get_tab_mode(), handle.selections(pa).len() > 1) {
                (TabMode::Normal, _) => handle.edit_all(pa, |mut c| {
                    if self.indent_keys.contains(&'\t') {
                        c.ts_reindent();
                    }

                    if self.insert_tabs {
                        c.insert('\t');
                        c.move_hor(1);
                    } else {
                        let tab_len = c.opts().tabstop_spaces_at(c.v_caret().visual_col() as u32);
                        c.insert(" ".repeat(tab_len as usize));
                        c.move_hor(tab_len as i32);
                    }
                }),
                (TabMode::Smart, _) | (TabMode::VerySmart, true) => handle.edit_all(pa, |mut c| {
                    let char_col = c.v_caret().char_col();
                    if (self.indent_keys.contains(&'\t') || char_col <= c.indent())
                        && c.ts_reindent()
                    {
                        return;
                    }

                    if self.insert_tabs {
                        c.insert('\t');
                        c.move_hor(1);
                    } else {
                        let tab_len = c.opts().tabstop_spaces_at(c.v_caret().visual_col() as u32);
                        c.insert(" ".repeat(tab_len as usize));
                        c.move_hor(tab_len as i32);
                    }
                }),
                (TabMode::VerySmart, false) => {
                    let do_scroll = handle.edit_main(pa, |mut c| {
                        let char_col = c.v_caret().char_col();
                        !((self.indent_keys.contains(&'\t') || char_col <= c.indent())
                            && c.ts_reindent())
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
                if self.indent_keys.contains(&char) && c.indent() == c.v_caret().char_col() - 1 {
                    c.ts_reindent();
                }
            }),

            event!(Enter) => handle.edit_all(pa, |mut c| {
                c.insert('\n');
                c.move_hor(1);
                if self.indent_keys.contains(&'\n') {
                    c.ts_reindent();
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

            event!(Esc) => {
                handle.text_mut(pa).new_moment();
                mode::set(Normal::new());
            }
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
pub(crate) enum TabMode {
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
    c.move_hor(-(c.v_caret().char_col() as i32));
    c.set_anchor();
    c.move_hor(chars_count as i32 - 1);

    c.replace("");
    c.unset_anchor();
    c.set_desired_vcol(dvcol);
}

pub(crate) static INSERT_TABS: AtomicBool = AtomicBool::new(false);
