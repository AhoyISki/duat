use std::sync::atomic::{AtomicBool, Ordering};

use duat_core::{
    mode::{Cursor, KeyCode::*, KeyMod as Mod},
    prelude::*,
};

use crate::{reindent, set_anchor_if_needed, Normal};

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

impl<U: Ui> Mode<U> for Insert {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
        if let key!(Left | Down | Up | Right, mods) = key
            && mods.contains(Mod::SHIFT)
        {
            handle.edit_all(pa, |mut c| {
                if c.anchor().is_none() {
                    c.set_anchor()
                }
            });
        }

        let mut processed_lines = Vec::new();
        match key {
            key!(Tab) => handle.edit_all(pa, |mut c| {
                let char_col = c.v_caret().char_col();
                if self.indent_keys.contains(&'\t') && char_col == 0 {
                    reindent(&mut c, &mut processed_lines);
                    if c.indent() > 0 {
                        return;
                    }
                }

                if self.insert_tabs {
                    c.insert('\t');
                    c.move_hor(1);
                } else {
                    let tab_len = c.cfg().tab_stops.spaces_at(c.v_caret().visual_col() as u32);
                    c.insert(" ".repeat(tab_len as usize));
                    c.move_hor(tab_len as i32);
                }
            }),
            key!(Char(char)) => handle.edit_all(pa, |mut c| {
                c.insert(char);
                c.move_hor(1);
                if self.indent_keys.contains(&char) && c.indent() == c.v_caret().char_col() - 1 {
                    reindent(&mut c, &mut processed_lines);
                }
            }),
            key!(Enter) => handle.edit_all(pa, |mut c| {
                c.insert('\n');
                c.move_hor(1);
                if self.indent_keys.contains(&'\n') {
                    reindent(&mut c, &mut processed_lines);
                }
            }),
            key!(Backspace) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();

                c.move_hor(-1);
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
            }),
            key!(Delete) => handle.edit_all(pa, |mut c| {
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
            key!(Left, Mod::NONE | Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                c.move_hor(-1);
            }),
            key!(Down, Mod::NONE | Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                if key.modifiers == Mod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                c.move_ver_wrapped(1);
            }),
            key!(Up, Mod::NONE | Mod::SHIFT) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                if key.modifiers == Mod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                c.move_ver_wrapped(-1)
            }),
            key!(Right) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers == Mod::SHIFT, &mut c);
                c.move_hor(1);
            }),

            key!(Esc) => {
                handle.text_mut(pa).new_moment();
                mode::set::<U>(Normal::new());
            }
            _ => {}
        }
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget, U>) {
        handle.set_mask("Insert");
    }
}

/// removes an empty line
fn remove_empty_line<S, U: Ui>(c: &mut Cursor<File<U>, U::Area, S>) {
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
