use std::sync::atomic::Ordering;

use duat::{
    mode::{KeyCode::*, KeyMod as Mod, VPoint},
    prelude::*,
    print::WordChars,
    text::Point,
};
use duat_utils::modes::{
    ExtendFwd, ExtendRev, IncSearch, PipeSelections, RunCommands, SearchFwd, SearchRev,
};
use jump_list::FileJumps;
use treesitter::TsCursor;

use crate::{
    Category, Insert, Memoized, Object, SEARCH, SelType, edit_or_destroy_all, escaped_regex,
    inc_searchers::{Select, Split},
    insert::INSERT_TABS,
    one_key::OneKey,
    select_to_end_of_line, set_anchor_if_needed, w_char_cat,
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

impl Mode for Normal {
    type Widget = File;

    fn send_key(&mut self, pa: &mut Pass, event: KeyEvent, handle: Handle<Self::Widget>) {
        let wc = handle.cfg(pa).word_chars;

        handle.write(pa).text_mut().new_moment();
        let rec = if handle.write(pa).record_selections(false) {
            2
        } else {
            1
        };

        match event {
            ////////// Basic movement keys
            key!(Char('h' | 'H') | Left, Mod::NONE | Mod::SHIFT) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = event.code == Char('H') || event.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(-1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(event.modifiers.contains(Mod::SHIFT), &mut c);
                c.move_ver_wrapped(1);
            }),
            key!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(event.modifiers.contains(Mod::SHIFT), &mut c);
                c.move_ver_wrapped(-1);
            }),
            key!(Down, Mod::ALT) => handle.scroll_ver(pa, 1),
            key!(Up, Mod::ALT) => handle.scroll_ver(pa, -1),
            key!(Char('l' | 'L') | Right, Mod::NONE | Mod::SHIFT) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = event.code == Char('L') || event.modifiers == Mod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(1);
                });
                self.sel_type = SelType::Normal;
            }
            key!(Char('j' | 'J')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(event.code == Char('J'), &mut c);
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
                set_anchor_if_needed(event.code == Char('K'), &mut c);
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
                let alt_word = event.modifiers.contains(Mod::ALT);
                if let Some(((p0, c0), (p1, c1))) = { no_nl_windows(c.chars_fwd()).next() } {
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
                let alt_word = event.modifiers.contains(Mod::ALT);
                if let Some(((p0, c0), (p1, c1))) = { no_nl_windows(c.chars_fwd()).next() } {
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
                let alt_word = event.modifiers.contains(Mod::ALT);
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
                let alt_word = event.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                if let Some([_, p1]) = { c.search_fwd(word_and_space(alt_word, wc), None).next() } {
                    c.move_to(p1);
                    c.move_hor(-1);
                }
            }),
            key!(Char('E'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                if let Some([_, p1]) = { c.search_fwd(space_and_word(alt_word, wc), None).next() } {
                    c.move_to(p1);
                    c.move_hor(-1);
                }
            }),
            key!(Char('B'), Mod::NONE | Mod::ALT) => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(Mod::ALT);
                set_anchor_if_needed(true, &mut c);
                if let Some([p0, _]) = { c.search_rev(word_and_space(alt_word, wc), None).next() } {
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
                let mf = event.modifiers;
                let sel_type = match (mf.contains(Mod::SHIFT), mf.contains(Mod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };

                mode::set(if let Char('f' | 'F') = event.code {
                    OneKey::Find(sel_type, self.f_and_t_set_search)
                } else {
                    OneKey::Until(sel_type, self.f_and_t_set_search)
                });
            }
            key!(Char('l' | 'L'), Mod::ALT) | key!(End) => handle.edit_all(pa, |mut c| {
                if event.code == Char('l') {
                    c.unset_anchor();
                }
                select_to_end_of_line(true, c);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            key!(Char('h' | 'H'), Mod::ALT) | key!(Home) => handle.edit_all(pa, |mut c| {
                if event.code == Char('h') {
                    c.unset_anchor();
                }
                set_anchor_if_needed(true, &mut c);
                c.move_hor(-(c.v_caret().char_col() as i32));
            }),
            key!(Char('a'), Mod::ALT) => mode::set(OneKey::Around(self.brackets)),
            key!(Char('i'), Mod::ALT) => mode::set(OneKey::Inside(self.brackets)),
            key!(Char('%')) => handle.edit_main(pa, |mut c| {
                c.move_to_start();
                c.set_anchor();
                c.move_to(c.last_point())
            }),
            key!(Char('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::new(event, c.cfg().word_chars, self.brackets).unwrap();
                    let [p2, p3] = object.find_ahead(c, 0, None)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(event.code == Char('M'), c);
                    c.move_to(p3);
                    c.move_hor(-1);

                    let bound = c.strs(p2..p3).unwrap().to_string();
                    let [s_b, e_b] = self.brackets.bounds_matching(&bound)?;
                    let [p0, _] = Object::Bounds(s_b, e_b).find_behind(c, 1, None)?;
                    if event.code == Char('m') {
                        c.set_anchor();
                    }
                    c.move_to(p0);
                    if prev_caret.char() != p3.char() - 1 {
                        if event.code == Char('m') {
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
                    let object = Object::new(event, c.cfg().word_chars, self.brackets).unwrap();
                    let [p0, p1] = object.find_behind(c, 0, None)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(event.code == Char('M'), c);
                    c.move_to(p0);

                    let bound = c.strs(p0..p1).unwrap().to_string();
                    let [s_b, e_b] = self.brackets.bounds_matching(&bound)?;
                    let [_, p3] = Object::Bounds(s_b, e_b).find_ahead(c, 1, None)?;
                    if event.code == Char('m') {
                        c.set_anchor();
                    }
                    c.move_to(p3);
                    c.move_hor(-1);
                    if prev_caret != p0 {
                        if event.code == Char('m') {
                            c.set_anchor();
                        }
                        c.move_to(p0);
                    }

                    Some(())
                })
            }

            ////////// Insertion mode keys
            key!(Char('i')) => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_start();
                });
                mode::set(Insert::new());
            }
            key!(Char('I')) => {
                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    if self.indent_on_capital_i {
                        c.ts_reindent();
                    } else {
                        c.move_to_col(c.indent());
                    }
                });
                mode::set(Insert::new());
            }
            key!(Char('a')) => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                mode::set(Insert::new());
            }
            key!(Char('A')) => {
                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    c.move_to(p);
                });
                mode::set(Insert::new());
            }
            key!(Char('o'), Mod::NONE | Mod::ALT) => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    let caret = c.caret();
                    let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    c.move_to(p);
                    c.append("\n");
                    if event.modifiers == Mod::NONE {
                        c.move_hor(1);
                        c.ts_reindent();
                    } else {
                        c.move_to(caret);
                    }
                });
                if event.modifiers == Mod::NONE {
                    mode::set(Insert::new());
                }
            }
            key!(Char('O'), Mod::NONE | Mod::ALT) => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_start();
                    let char_col = c.v_caret().char_col();
                    c.move_hor(-(char_col as i32));
                    c.insert("\n");
                    if event.modifiers == Mod::NONE {
                        c.ts_reindent();
                    } else {
                        c.move_hor(char_col as i32 + 1);
                    }
                });
                if event.modifiers == Mod::NONE {
                    mode::set(Insert::new());
                }
            }

            ////////// Selection alteration keys
            key!(Char('r')) => mode::set(OneKey::Replace),
            key!(Char('`')) => handle.edit_all(pa, |mut c| {
                let lower = c
                    .selection()
                    .flat_map(str::chars)
                    .flat_map(char::to_lowercase);
                c.replace(lower.collect::<String>());
            }),
            key!(Char('~')) => handle.edit_all(pa, |mut c| {
                let upper = c
                    .selection()
                    .flat_map(str::chars)
                    .flat_map(char::to_uppercase);
                c.replace(upper.collect::<String>());
            }),
            key!(Char('`'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                let inverted = c.selection().flat_map(str::chars).map(|c| {
                    if c.is_uppercase() {
                        c.to_lowercase().collect::<String>()
                    } else {
                        c.to_uppercase().collect()
                    }
                });
                c.replace(inverted.collect::<String>());
            }),

            ////////// Advanced selection manipulation
            key!(Char(';'), Mod::ALT) => handle.edit_all(pa, |mut c| c.swap_ends()),
            key!(Char(';')) => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
            }),
            key!(Char(':'), Mod::ALT) => handle.edit_all(pa, |mut c| {
                c.set_caret_on_end();
            }),
            key!(Char(')')) => handle.selections_mut(pa).rotate_main(1),
            key!(Char('(')) => handle.selections_mut(pa).rotate_main(-1),
            key!(Char(')'), Mod::ALT) => {
                let last_sel = handle.edit_iter(pa, |mut iter| {
                    let mut last_sel = iter.next().map(|c| c.selection().to_string());

                    while let Some(mut c) = iter.next() {
                        let selection = c.selection().to_string();
                        c.set_anchor_if_needed();
                        c.replace(last_sel.replace(selection).unwrap());
                    }

                    last_sel
                });

                handle.edit_nth(pa, 0, |mut c| c.replace(last_sel.unwrap()));
            }
            key!(Char('('), Mod::ALT) => {
                let mut selections = Vec::<String>::new();
                handle.edit_all(pa, |c| selections.push(c.selection().collect()));
                let mut s_iter = selections.into_iter().cycle();
                s_iter.next();
                handle.edit_all(pa, |mut c| {
                    if let Some(next) = s_iter.next() {
                        c.set_anchor_if_needed();
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
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let range = c.range_excl();
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
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let range = c.range_excl();

                    let mut caret = (c.caret().line(), c.v_caret().char_col());
                    let mut anchor = c.v_anchor().map(|vp| (vp.line(), vp.char_col()));

                    c.unset_anchor();

                    let find = format!("^(\t| {{1,{}}})", c.cfg().tab_stops.spaces_at(0) as usize);

                    for line in (range[0].line()..=range[1].line()).rev() {
                        if processed_lines.contains(&line) {
                            continue;
                        }
                        let [p0, p1] = c.text().points_of_line(line);
                        c.move_to(p0);
                        let Some([p0, p1]) = c.search_fwd(&find, Some(p1)).next() else {
                            continue;
                        };

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
                    } else {
                        c.unset_anchor();
                    }
                    c.move_to_coords(caret.0, caret.1);

                    processed_lines.extend(range[0].line()..=range[1].line());
                });
            }
            key!(Char('j'), Mod::ALT) => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let [start, end] = c.range();

                    if start.line() == end.line() {
                        if !processed_lines.contains(&start.line())
                            && let Some([p0, p1]) = { c.search_fwd("\n", None).next() }
                        {
                            c.move_to(p0..p1);
                            c.replace("");
                            c.reset();
                            processed_lines.push(start.line());
                        }
                    } else {
                        let caret_was_on_end = c.set_caret_on_start();
                        let nls: Vec<[Point; 2]> = c.search_fwd("\n", Some(end)).collect();

                        let mut lines_joined = 0;
                        for [p0, p1] in nls.into_iter().rev() {
                            if processed_lines.contains(&p0.line()) {
                                continue;
                            }

                            c.move_to(p0..p1);
                            c.replace("");
                            lines_joined += 1;
                        }

                        c.unset_anchor();
                        c.move_to(start);
                        c.set_anchor();
                        c.move_hor((end.char() - (start.char() + lines_joined)) as i32);
                        if !caret_was_on_end {
                            c.swap_ends();
                        }
                    }
                });
            }

            ////////// Clipboard keys
            key!(Char('y')) => duat_utils::modes::copy_selections(pa, &handle),
            key!(Char('d' | 'c'), Mod::NONE | Mod::ALT) => {
                if event.modifiers == Mod::NONE {
                    duat_utils::modes::copy_selections(pa, &handle);
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
                if event.code == Char('c') {
                    mode::set(Insert::new());
                }
            }
            key!(Char('p' | 'P')) => {
                let pastes = duat_utils::modes::paste_strings();
                if pastes.is_empty() {
                    return;
                }
                let mut p_iter = pastes.iter().cycle();
                handle.edit_all(pa, |mut c| {
                    let paste = p_iter.next().unwrap();

                    let anchor_is_start = c.anchor_is_start();

                    // If it ends in a new line, we gotta move to the start of the line.
                    let appended = if event.code == Char('p') {
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
            key!(Char('R')) => {
                let pastes = duat_utils::modes::paste_strings();
                if !pastes.is_empty() {
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| c.replace(p_iter.next().unwrap()));
                }
            }

            ////////// Cursor creation and destruction
            key!(Char(',')) => handle.selections_mut(pa).remove_extras(),
            key!(Char('C'), Mod::NONE | Mod::ALT) => {
                let (nth, mult) = if event.modifiers == Mod::NONE {
                    (handle.read(pa).selections().len() - 1, 1)
                } else {
                    (0, -1)
                };

                handle.edit_nth(pa, nth, |mut c| {
                    c.copy();
                    let (v_caret, v_anchor) = (c.v_caret(), c.v_anchor());
                    let lines_diff = v_anchor.map(|vp| vp.line() as i32 - v_caret.line() as i32);
                    let mut lines = mult * (lines_diff.unwrap_or(0) + 1);

                    while c.move_ver(lines) == lines {
                        if v_anchor.is_some() {
                            c.swap_ends();
                            c.move_ver(lines);
                            c.swap_ends();
                        }
                        if cols_eq((v_caret, v_anchor), (c.v_caret(), c.v_anchor())) {
                            return;
                        }
                        lines = mult;
                    }
                    c.destroy();
                });
            }

            ////////// Search keys
            key!(Char('/')) => mode::set(IncSearch::new(SearchFwd)),
            key!(Char('/'), Mod::ALT) => mode::set(IncSearch::new(SearchRev)),
            key!(Char('?')) => mode::set(IncSearch::new(ExtendFwd)),
            key!(Char('?'), Mod::ALT) => mode::set(IncSearch::new(ExtendRev)),
            key!(Char('s')) => mode::set(IncSearch::new(Select)),
            key!(Char('S')) => mode::set(IncSearch::new(Split)),
            key!(Char('n' | 'N'), Mod::NONE | Mod::ALT) => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::error!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut c| {
                    if event.code == Char('N') {
                        c.copy();
                    }
                    let caret = c.caret();
                    let next = if event.modifiers == Mod::ALT {
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

            ////////// Jumping
            key!(Char('u' | 'U'), Mod::ALT) => {
                let jmp = if event.code == Char('u') { -rec } else { rec };
                if let Some(jump) = handle.write(pa).jump_selections_by(jmp) {
                    match jump {
                        jump_list::Jump::Single(selection) => {
                            handle.write(pa).selections_mut().remove_extras();
                            handle.edit_main(pa, |mut c| {
                                let start = c.text().point_at_byte(selection.start);
                                let end = c.text().point_at_byte(selection.end);
                                c.move_to(start..end)
                            });
                        }
                        jump_list::Jump::Multiple(selections, main) => {
                            handle.write(pa).selections_mut().remove_extras();

                            handle.edit_main(pa, |mut c| {
                                let mut is_first = true;
                                for selection in selections {
                                    if !is_first {
                                        c.copy();
                                    }

                                    let start = c.text().point_at_byte(selection.start);
                                    let end = c.text().point_at_byte(selection.end);
                                    c.move_to(start..end);
                                    is_first = false;
                                }
                            });
                            handle.write(pa).selections_mut().set_main(main);
                        }
                    }
                }
            }

            ////////// Other mode changing keys
            key!(Char(':')) => mode::set(RunCommands::new()),
            key!(Char('|')) => mode::set(PipeSelections::new()),
            key!(Char('G')) => mode::set(OneKey::GoTo(SelType::Extend)),
            key!(Char('g')) => mode::set(OneKey::GoTo(SelType::Normal)),
            key!(Char(' ')) => mode::set(mode::User),

            ////////// History manipulation
            key!(Char('u')) => handle.text_mut(pa).undo(),
            key!(Char('U')) => handle.text_mut(pa).redo(),
            _ => {}
        }
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget>) {
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

fn cols_eq(lhs: (VPoint, Option<VPoint>), rhs: (VPoint, Option<VPoint>)) -> bool {
    lhs.0.visual_col() == rhs.0.visual_col()
        && lhs
            .1
            .zip(rhs.1)
            .is_none_or(|(lhs, rhs)| lhs.visual_col() == rhs.visual_col())
}
