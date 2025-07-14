use std::sync::{Mutex, atomic::Ordering};

use duat_core::{
    cfg::WordChars,
    mode::{KeyCode::*, KeyMod as Mod},
    prelude::*,
};
use duat_utils::modes::{
    ExtendFwd, ExtendRev, IncSearch, PipeSelections, RunCommands, SearchFwd, SearchRev,
};

use crate::{
    Category, Insert, Memoized, Object, SEARCH, SelType, edit_or_destroy_all, escaped_regex,
    inc_searchers::{Select, Split},
    insert::INSERT_TABS,
    one_key::OneKey,
    reindent, select_to_end_of_line, set_anchor_if_needed, w_char_cat,
};

#[derive(Clone, Copy)]
pub struct Normal {
    sel_type: SelType,
    brackets: Brackets,
    indent_on_capital_i: bool,
    f_and_t_set_search: bool,
}

impl Normal {
    /// Returns an instance of the [`Normal`] mode, inspired by
    /// Kakoune
    pub fn new() -> Self {
        const B_PATS: Brackets = Brackets(&[[r"\(", r"\)"], [r"\{", r"\}"], [r"\[", r"\]"]]);
        Normal {
            sel_type: SelType::Normal,
            brackets: B_PATS,
            indent_on_capital_i: false,
            f_and_t_set_search: false,
        }
    }

    /// [`Normal`] mode with different type of selection
    pub(crate) fn new_with_sel_type(sel_type: SelType) -> Self {
        let mut normal = Self::new();
        normal.sel_type = sel_type;
        normal
    }

    /// Changes what is considered a "bracket" in [`Normal`] mode
    ///
    /// More specifically, this will change the behavior of keys like
    /// `'m'` and the `'u'` object, which will now consider more
    /// patterns when selecting.
    pub fn with_brackets<'a>(self, brackets: impl Iterator<Item = [&'a str; 2]>) -> Self {
        static BRACKETS: Memoized<Vec<[&str; 2]>, Brackets> = Memoized::new();

        let brackets: Vec<[&str; 2]> = brackets.map(|bs| bs.map(escaped_regex)).collect();
        assert!(
            brackets.iter().all(|[s_b, e_b]| s_b != e_b),
            "Brackets are not allowed to look the same"
        );

        let brackets = BRACKETS.get_or_insert_with(brackets.clone(), || Brackets(brackets.leak()));
        Self { brackets, ..self }
    }

    /// Makes it so the `'I'` key no longer indents the line
    ///
    /// By default, when you press `'I'`, the line will be reindented,
    /// in order to send you to the "proper" insertion spot, not just
    /// to the first non whitespace character.
    ///
    /// This function disables that behavior.
    pub fn with_no_indent_on_capital_i(self) -> Self {
        Self { indent_on_capital_i: false, ..self }
    }

    /// Makes the `'f'` and `'t'` keys set the search pattern
    ///
    /// If you type `"fm"`, for example, and then type `'n'`, `'n'`
    /// will search for the next instance of an `'m'` in the [`File`]
    pub fn f_and_t_set_search(self) -> Self {
        Self { f_and_t_set_search: true, ..self }
    }
}

impl<U: Ui> Mode<U> for Normal {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
        let wc = handle.cfg(pa).word_chars;

        match key {
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = key.code == Char('H') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(-1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut c);
                c.move_ver_wrapped(1);
            }),
            key!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.modifiers.contains(Mod::SHIFT), &mut c);
                c.move_ver_wrapped(-1);
            }),
            key!(Down, Mod::ALT) => handle.scroll_ver(pa, 1),
            key!(Up, Mod::ALT) => handle.scroll_ver(pa, -1),
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = key.code == Char('L') || key.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Char('j' | 'J')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.code == Char('J'), &mut c);
                c.move_ver(1);
                let v_caret = c.v_caret();
                if c.char() == '\n'
                    && v_caret.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    c.move_hor(-1);
                    c.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),
            key!(Char('k' | 'K')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key.code == Char('K'), &mut c);
                c.move_ver(-1);
                let v_caret = c.v_caret();
                if c.char() == '\n'
                    && v_caret.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    c.move_hor(-1);
                    c.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_caret.desired_visual_col()
                    });
                }
            }),

            ////////// Object selection keys
            key!(Char('w'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(c.chars_fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        c.move_to(p0);
                    } else {
                        c.move_to(p1);
                    }

                    let points = c.search_fwd(word_and_space(alt_word, wc), None).next();
                    if let Some([_, p1]) = points {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    }
                };
            }),
            key!(Char('e'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = no_nl_windows(c.chars_fwd()).next();
                if let Some(((p0, c0), (p1, c1))) = init {
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        c.move_to(p0);
                    } else {
                        c.move_to(p1);
                    }

                    let points = c.search_fwd(space_and_word(alt_word, wc), None).next();
                    if let Some([_, p1]) = points {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    }
                };
            }),
            key!(Char('b'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                let init = {
                    let iter = [(c.caret(), c.char())].into_iter().chain(c.chars_rev());
                    no_nl_windows(iter).next()
                };
                if let Some(((p1, c1), (_, c0))) = init {
                    c.move_to(p1);
                    if Category::of(c0, wc) == Category::of(c1, wc) {
                        c.move_hor(1);
                    }
                    let points = c.search_rev(word_and_space(alt_word, wc), None).next();
                    if let Some([p0, p1]) = points {
                        c.move_to(p0);
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                        c.swap_ends();
                    };
                };
            }),

            key!(Char('W'), Mod::ALT | Mod::NONE) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                let points = c.search_fwd(word_and_space(alt_word, wc), None).next();
                if let Some([_, p1]) = points {
                    c.move_to(p1);
                    c.move_hor(-1);
                }
            }),
            key!(Char('E'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                let points = c.search_fwd(space_and_word(alt_word, wc), None).next();
                if let Some([_, p1]) = points {
                    c.move_to(p1);
                    c.move_hor(-1);
                }
            }),
            key!(Char('B'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = key.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                let points = c.search_rev(word_and_space(alt_word, wc), None).next();
                if let Some([p0, _]) = points {
                    c.move_to(p0);
                }
            }),

            key!(Char('x')) => handle.edit_all(pa, |mut c| {
                self.sel_type = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut c);
                c.set_caret_on_start();
                let p0 = c.search_rev("\n", None).next().map(|[_, p0]| p0);
                c.move_to(p0.unwrap_or_default());
                c.swap_ends();

                let p1 = c.search_fwd("\n", None).next().map(|[p1, _]| p1);
                c.move_to(p1.unwrap_or(c.last_point()));
                c.set_desired_vcol(usize::MAX);
            }),
            key!(Char('f' | 'F' | 't' | 'T'), Mod::NONE | Mod::ALT) => {
                let mf = key.modifiers;
                let sel_type = match (mf.contains(Mod::SHIFT), mf.contains(Mod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };

                mode::set::<U>(if let Char('f' | 'F') = key.code {
                    OneKey::Find(sel_type, self.f_and_t_set_search)
                } else {
                    OneKey::Until(sel_type, self.f_and_t_set_search)
                });
            }
            key!(Char('l' | 'L'), Mod::ALT) | key!(End) => handle.edit_all(pa, |mut c| {
                if key.code == Char('l') {
                    c.unset_anchor();
                }
                select_to_end_of_line(true, c);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            key!(Char('h' | 'H'), Mod::ALT) | key!(Home) => handle.edit_all(pa, |mut c| {
                if key.code == Char('h') {
                    c.unset_anchor();
                }
                set_anchor_if_needed(true, &mut c);
                c.move_hor(-(c.v_caret().char_col() as i32));
            }),
            key!(Char('a'), Mod::ALT) => mode::set::<U>(OneKey::Around(self.brackets)),
            key!(Char('i'), Mod::ALT) => mode::set::<U>(OneKey::Inside(self.brackets)),
            key!(Char('%')) => {
                handle.edit_main(pa, |mut c| {
                    c.move_to_start();
                    c.set_anchor();
                    c.move_to(c.last_point())
                });
            }
            key!(Char('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::from_char('m', c.cfg().word_chars, self.brackets).unwrap();
                    let [p2, p3] = object.find_ahead(c, 0, None)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(key.code == Char('M'), c);
                    c.move_to(p3);
                    c.move_hor(-1);

                    let bound = c.strs(p2..p3).to_string();
                    let [s_b, e_b] = self.brackets.bounds_matching(&bound)?;
                    let [p0, _] = Object::Bounds(s_b, e_b).find_behind(c, 1, None)?;
                    if key.code == Char('m') {
                        c.set_anchor();
                    }
                    c.move_to(p0);
                    if prev_caret.char() != p3.char() - 1 {
                        if key.code == Char('m') {
                            c.set_anchor();
                        }
                        c.move_to(p3);
                        c.move_hor(-1);
                    }

                    Some(())
                })
            }
            key!(Char('m' | 'M'), Mod::ALT) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::from_char('m', c.cfg().word_chars, self.brackets).unwrap();
                    let [p0, p1] = object.find_behind(c, 0, None)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(key.code == Char('M'), c);
                    c.move_to(p0);

                    let bound = c.strs(p0..p1).to_string();
                    let [s_b, e_b] = self.brackets.bounds_matching(&bound)?;
                    let [_, p3] = Object::Bounds(s_b, e_b).find_ahead(c, 1, None)?;
                    if key.code == Char('m') {
                        c.set_anchor();
                    }
                    c.move_to(p3);
                    c.move_hor(-1);
                    if prev_caret != p0 {
                        if key.code == Char('m') {
                            c.set_anchor();
                        }
                        c.move_to(p0);
                    }

                    Some(())
                })
            }

            ////////// Insertion mode keys
            key!(Char('i')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_start();
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('I')) => {
                handle.new_moment(pa);
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    if self.indent_on_capital_i {
                        reindent(&mut c, &mut processed_lines);
                    } else {
                        c.unset_anchor();
                        c.move_hor(-(c.v_caret().char_col() as i32));
                        c.set_anchor();
                        let indent = c.indent();
                        c.move_hor(indent as i32);
                    }
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('a')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('A')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    c.move_to(p);
                });
                mode::set::<U>(Insert::new());
            }
            key!(Char('o' | 'O'), Mod::NONE | Mod::ALT) => {
                handle.new_moment(pa);
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    if key.code == Char('O') {
                        c.set_caret_on_start();
                        let char_col = c.v_caret().char_col();
                        c.move_hor(-(char_col as i32));
                        c.insert("\n");
                        if key.modifiers == Mod::NONE {
                            reindent(&mut c, &mut processed_lines);
                        } else {
                            c.move_hor(char_col as i32 + 1);
                        }
                    } else {
                        c.set_caret_on_end();
                        let caret = c.caret();
                        let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                        c.move_to(p);
                        c.append("\n");
                        if key.modifiers == Mod::NONE {
                            c.move_hor(1);
                            reindent(&mut c, &mut processed_lines);
                        } else {
                            c.move_to(caret);
                        }
                    }
                });
                if key.modifiers == Mod::NONE {
                    mode::set::<U>(Insert::new());
                }
            }

            ////////// Selection alteration keys
            key!(Char('r')) => {
                handle.new_moment(pa);
                mode::set::<U>(OneKey::Replace)
            }
            key!(Char('`')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    let lower = c
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_lowercase);
                    c.replace(lower.collect::<String>());
                })
            }
            key!(Char('~')) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    let upper = c
                        .selection()
                        .flat_map(str::chars)
                        .flat_map(char::to_uppercase);
                    c.replace(upper.collect::<String>());
                })
            }
            key!(Char('`'), Mod::ALT) => {
                handle.new_moment(pa);
                handle.edit_all(pa, |mut c| {
                    let inverted = c.selection().flat_map(str::chars).map(|c| {
                        if c.is_uppercase() {
                            c.to_lowercase().collect::<String>()
                        } else {
                            c.to_uppercase().collect()
                        }
                    });
                    c.replace(inverted.collect::<String>());
                })
            }

            ////////// Advanced selection manipulation
            key!(Char(';'), Mod::ALT) => handle.edit_all(pa, |mut c| c.swap_ends()),
            key!(Char(';')) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
            }),
            key!(Char(':'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                c.set_caret_on_end();
            }),
            key!(Char(')')) => handle.write_selections(pa, |s| s.rotate_main(1)),
            key!(Char('(')) => handle.write_selections(pa, |s| s.rotate_main(-1)),
            key!(Char(')'), Mod::ALT) => {
                handle.new_moment(pa);
                let last_sel = handle.edit_iter(pa, |mut iter| {
                    let mut last_sel = iter.next().map(|c| c.selection().to_string());

                    while let Some(mut c) = iter.next() {
                        let selection = c.selection().to_string();
                        c.replace(last_sel.replace(selection).unwrap());
                    }

                    last_sel
                });

                handle.edit_nth(pa, 0, |mut c| c.replace(last_sel.unwrap()));
            }
            key!(Char('('), Mod::ALT) => {
                handle.new_moment(pa);
                let mut selections = Vec::<String>::new();
                handle.edit_all(pa, |c| selections.push(c.selection().collect()));
                let mut s_iter = selections.into_iter().cycle();
                s_iter.next();
                handle.edit_all(pa, |mut c| {
                    if let Some(next) = s_iter.next() {
                        c.replace(next);
                    }
                });
            }
            key!(Char('_'), Mod::ALT) => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                // In the first iteration, connected Cursors are joined, this one just
                // undoes the movement.
                handle.edit_all(pa, |mut c| {
                    c.move_hor(-1);
                });
            }
            key!(Char('s'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                c.set_caret_on_start();
                let Some(end) = c.anchor() else {
                    return;
                };
                let lines: Vec<[Point; 2]> = c.search_fwd("[^\n]*\n", Some(end)).collect();
                let mut last_p1 = c.caret();
                for [p0, p1] in lines {
                    let mut e_copy = c.copy();
                    e_copy.move_to(p0);
                    e_copy.set_anchor();
                    e_copy.move_to(p1);
                    e_copy.move_hor(-1);

                    last_p1 = p1;
                }
                c.move_to(last_p1);
                c.swap_ends();
            }),
            key!(Char('S'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                if c.anchor().is_some() {
                    let mut e_copy = c.copy();
                    e_copy.swap_ends();
                    e_copy.unset_anchor();
                    c.unset_anchor();
                }
            }),

            ////////// Line alteration keys
            key!(Char('>')) => {
                handle.new_moment(pa);
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let range = c.range();
                    let old_caret = c.v_caret();
                    let old_anchor = c.v_anchor();
                    c.unset_anchor();

                    let insert = if INSERT_TABS.load(Ordering::Relaxed) {
                        "\t".to_string()
                    } else {
                        " ".repeat(c.cfg().tab_stops.spaces_at(0) as usize)
                    };

                    for line in range[0].line()..=range[1].line() {
                        if processed_lines.contains(&line) {
                            continue;
                        }
                        c.move_to_coords(line, 0);
                        c.insert(&insert);
                    }

                    let cols = insert.chars().count();
                    if let Some(old_anchor) = old_anchor {
                        c.move_to_coords(old_anchor.line(), old_anchor.char_col() + cols);
                        c.set_anchor();
                    }
                    c.move_to_coords(old_caret.line(), old_caret.char_col() + cols);

                    processed_lines.extend(range[0].line()..=range[1].line());
                });
            }
            key!(Char('<')) => {
                handle.new_moment(pa);
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let range = c.range();

                    let mut caret = (c.caret().line(), c.v_caret().char_col());
                    let mut anchor = c.v_anchor().map(|vp| (vp.line(), vp.char_col()));

                    c.unset_anchor();

                    let find = format!("^(\t| {{1,{}}})", c.cfg().tab_stops.spaces_at(0) as usize);

                    for line in (range[0].line()..=range[1].line()).rev() {
                        if processed_lines.contains(&line) {
                            continue;
                        }
                        let [p0, p1] = c.text().points_of_line(line);
                        context::debug!("line points are {p0}, {p1}");
                        c.move_to(p0);
                        let Some([p0, p1]) = c.search_fwd(&find, Some(p1)).next() else {
                            continue;
                        };

                        context::info!("deindent points are {p0}, {p1}");
                        c.move_to(p0..p1);
                        c.replace("");
                        if line == caret.0 {
                            caret.1 = caret.1.saturating_sub(p1.char() - p0.char());
                        }
                        if let Some(anchor) = &mut anchor
                            && line == anchor.0
                        {
                            anchor.1 = anchor.1.saturating_sub(p1.char() - p0.char());
                        }
                    }

                    if let Some((line, col)) = anchor {
                        c.move_to_coords(line, col);
                        c.set_anchor();
                    }
                    c.move_to_coords(caret.0, caret.1);

                    processed_lines.extend(range[0].line()..=range[1].line());
                });
            }

            ////////// Clipboard keys
            key!(Char('y')) => {
                handle.new_moment(pa);
                copy_selections(pa, &handle)
            }
            key!(Char('d' | 'c'), Mod::NONE | Mod::ALT) => {
                handle.new_moment(pa);
                if key.modifiers == Mod::NONE {
                    copy_selections(pa, &handle);
                }
                handle.edit_all(pa, |mut c| {
                    let prev_char = c.chars_rev().next();
                    if c.range()[1] == c.len()
                        && c.selection() == "\n"
                        && let Some((_, '\n')) = prev_char
                    {
                        c.set_anchor();
                        c.move_hor(-1);
                    }

                    c.set_anchor_if_needed();
                    c.replace("");
                    c.unset_anchor();
                });
                if key.code == Char('c') {
                    mode::set::<U>(Insert::new());
                }
            }
            key!(Char('p' | 'P')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    handle.new_moment(pa);
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| {
                        let paste = p_iter.next().unwrap();

                        let anchor_is_start = c.anchor_is_start();

                        // If it ends in a new line, we gotta move to the start of the line.
                        let appended = if key.code == Char('p') {
                            c.set_caret_on_end();
                            if paste.ends_with('\n') {
                                let (p, _) =
                                    c.chars_fwd().find(|(_, c)| *c == '\n').unwrap_or_default();
                                c.move_to(p);
                                c.append(paste);
                            } else {
                                c.append(paste)
                            }
                            true
                        } else {
                            c.set_caret_on_start();
                            if paste.ends_with('\n') {
                                let char_col = c.v_caret().char_col();
                                c.move_hor(-(char_col as i32));
                                c.insert(paste);
                            } else {
                                c.insert(paste)
                            }
                            false
                        };

                        if !paste.is_empty() {
                            c.move_hor(appended as i32);
                            c.set_anchor();
                            c.move_hor(paste.chars().count() as i32 - 1);
                            if !anchor_is_start {
                                c.set_caret_on_start();
                            }
                        }
                    });
                }
            }
            key!(Char('R')) => {
                let pastes = paste_strings();
                if !pastes.is_empty() {
                    handle.new_moment(pa);
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| c.replace(p_iter.next().unwrap()));
                }
            }

            ////////// Cursor creation and destruction
            key!(Char(',')) => {
                handle.write(pa, |f, _| f.text_mut().selections_mut().remove_extras())
            }
            key!(Char('C')) => {
                handle.new_moment(pa);
                handle.edit_last(pa, |mut c| {
                    let v_caret = c.v_caret();
                    c.copy();
                    if let Some(v_anchor) = c.v_anchor() {
                        let lines_diff = v_anchor.line() as i32 - c.caret().line() as i32;
                        let len_lines = lines_diff.unsigned_abs() as usize;
                        while c.caret().line() + len_lines < c.len().line() {
                            c.move_ver(len_lines as i32 + 1);
                            c.set_anchor();
                            c.set_desired_vcol(v_anchor.visual_col());
                            c.move_ver(lines_diff);
                            c.swap_ends();
                            if c.v_caret().visual_col() <= v_caret.visual_col()
                                && c.v_anchor().unwrap().visual_col() <= v_anchor.visual_col()
                            {
                                return;
                            }
                            c.swap_ends();
                        }
                    } else {
                        while c.caret().line() < c.len().line() {
                            if c.move_ver(1) == 0 {
                                break;
                            }
                            if c.v_caret().visual_col() == v_caret.visual_col() {
                                return;
                            }
                        }
                    }
                    c.destroy();
                });
            }
            key!(Char('C'), Mod::ALT) => {
                handle.new_moment(pa);
                handle.edit_nth(pa, 0, |mut c| {
                    let v_caret = c.v_caret();
                    c.copy();
                    if let Some(v_anchor) = c.v_anchor() {
                        let lines_diff = v_anchor.line() as i32 - c.caret().line() as i32;
                        let len_lines = lines_diff.unsigned_abs() as usize;
                        while c.caret().line().checked_sub(len_lines + 1).is_some() {
                            c.move_ver(-1 - len_lines as i32);
                            c.set_anchor();
                            c.set_desired_vcol(v_anchor.visual_col());
                            c.move_ver(lines_diff);
                            c.swap_ends();
                            if c.v_caret().visual_col() == v_caret.visual_col()
                                && c.v_anchor().unwrap().visual_col() == v_anchor.visual_col()
                            {
                                return;
                            }
                            c.swap_ends();
                        }
                    } else {
                        while c.caret().line() > 0 {
                            c.move_ver(-1);
                            if c.v_caret().visual_col() == v_caret.visual_col() {
                                return;
                            }
                        }
                    }
                    c.destroy();
                });
            }

            ////////// Search keys
            key!(Char('/')) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(Char('/'), Mod::ALT) => mode::set::<U>(IncSearch::new(SearchRev)),
            key!(Char('?')) => mode::set::<U>(IncSearch::new(ExtendFwd)),
            key!(Char('?'), Mod::ALT) => mode::set::<U>(IncSearch::new(ExtendRev)),
            key!(Char('s')) => mode::set::<U>(IncSearch::new(Select)),
            key!(Char('S')) => mode::set::<U>(IncSearch::new(Split)),
            key!(Char('n' | 'N'), Mod::NONE | Mod::ALT) => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::error!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut c| {
                    if key.code == Char('N') {
                        c.copy();
                    }
                    let caret = c.caret();
                    let next = if key.modifiers == Mod::ALT {
                        c.search_rev(&*search, None).find(|[p, _]| *p != caret)
                    } else {
                        c.search_fwd(&*search, None).find(|[p, _]| *p != caret)
                    };
                    if let Some([p0, p1]) = next {
                        c.move_to(p0);
                        if p1 > p0 {
                            c.set_anchor();
                            c.move_to(p1);
                            c.move_hor(-1);
                        }
                    }
                });
            }

            ////////// Other mode changing keys
            key!(Char(':')) => mode::set::<U>(RunCommands::new()),
            key!(Char('|')) => {
                handle.new_moment(pa);
                mode::set::<U>(PipeSelections::new())
            }
            key!(Char('G')) => mode::set::<U>(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set::<U>(OneKey::GoTo(SelType::Normal)),
            key!(Char(' ')) => mode::set::<U>(mode::User),

            ////////// History manipulation
            key!(Char('u')) => handle.undo(pa),
            key!(Char('U')) => handle.redo(pa),
            _ => {}
        }
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget, U>) {
        handle.set_mask("Normal");
    }
}

impl Default for Normal {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct Brackets(&'static [[&'static str; 2]]);

impl Brackets {
    pub(crate) fn bounds_matching(&self, bound: &str) -> Option<[&'static str; 2]> {
        self.0
            .iter()
            .find(|bs| bs.contains(&escaped_regex(bound)))
            .copied()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &[&'static str; 2]> + '_ {
        self.0.iter()
    }
}

fn no_nl_windows<'a>(
    iter: impl Iterator<Item = (Point, char)> + 'a,
) -> impl Iterator<Item = ((Point, char), (Point, char))> + 'a {
    iter.map_windows(|[first, second]| (*first, *second))
        .skip_while(|((_, c0), (_, c1))| *c0 == '\n' || *c1 == '\n')
}

fn word_and_space(alt_word: bool, wc: WordChars) -> String {
    if alt_word {
        "[^ \t\n]*[ \t]*".to_string()
    } else {
        let cat = w_char_cat(wc);
        format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t]+")
    }
}

fn space_and_word(alt_word: bool, wc: WordChars) -> String {
    if alt_word {
        "[ \t]*[^ \t\n]*".to_string()
    } else {
        let cat = w_char_cat(wc);
        format!("[ \t]*([{cat}]+|[^{cat} \t\n]+)|[ \t]+")
    }
}

fn copy_selections<U: Ui>(pa: &mut Pass, handle: &Handle<File<U>, U, ()>) {
    let mut copies: Vec<String> = Vec::new();
    handle.edit_all(pa, |c| copies.push(c.selection().collect()));
    if !copies.iter().all(String::is_empty) {
        if copies.len() == 1 {
            duat_core::clipboard::set_text(copies.first().unwrap());
        }
        *CLIPBOARD.lock().unwrap() = copies
    }
}

fn paste_strings() -> Vec<String> {
    static SYSTEM_CLIPB: Mutex<Option<String>> = Mutex::new(None);

    let paste = duat_core::clipboard::get_text();

    let mut sys_clipb = SYSTEM_CLIPB.lock().unwrap();

    // If there was no previous clipboard, or it has changed, copy the new
    // pasted text
    if let Some(paste) = paste
        && sys_clipb.as_ref().is_none_or(|sc| *sc != paste)
    {
        *CLIPBOARD.lock().unwrap() = vec![paste.clone()];
        *sys_clipb = Some(paste.clone());
        vec![paste]
    } else {
        CLIPBOARD.lock().unwrap().clone()
    }
}

static CLIPBOARD: Mutex<Vec<String>> = Mutex::new(Vec::new());
