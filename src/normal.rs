use std::sync::atomic::Ordering;

use duat::{
    mode::{
        ExtendFwd, ExtendRev, IncSearch, KeyCode::*, KeyMod, PipeSelections, RunCommands,
        SearchFwd, SearchRev, VPoint,
    },
    opts::PrintOpts,
    prelude::*,
    text::Point,
};
use jump_list::BufferJumps;
use treesitter::TsCursor;

use crate::{
    Category, Insert, Memoized, Object, SEARCH, SelType, edit_or_destroy_all, escaped_regex,
    escaped_str,
    inc_searchers::{Select, Split},
    insert::INSERT_TABS,
    one_key::OneKey,
    select_to_end_of_line, set_anchor_if_needed,
};

#[derive(Clone, Copy)]
pub struct Normal {
    sel_type: SelType,
    brackets: Brackets,
    /// Wheter to indent the line when pressing the `I` key
    ///
    /// The default is `true`.
    ///
    /// Normally, when you press `'I'`, the line will be reindented,
    /// in order to send you to the "proper" insertion spot, not just
    /// to the first non whitespace character.
    pub indent_on_capital_i: bool,
    /// Makes the `'f'` and `'t'` keys set the search pattern
    ///
    /// If you type `"fm"`, for example, and then type `'n'`, `'n'`
    /// will search for the next instance of an `'m'` in the
    /// [`Buffer`]
    pub f_and_t_set_search: bool,
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
    pub fn set_brackets<'a>(&mut self, brackets: impl Iterator<Item = [&'a str; 2]>) {
        static BRACKETS: Memoized<Vec<[&str; 2]>, Brackets> = Memoized::new();

        let brackets: Vec<[&str; 2]> = brackets.map(|bs| bs.map(escaped_regex)).collect();
        assert!(
            brackets.iter().all(|[s_b, e_b]| s_b != e_b),
            "Brackets are not allowed to look the same"
        );

        self.brackets = BRACKETS.get_or_insert_with(brackets.clone(), || Brackets(brackets.leak()));
    }

    pub fn with_no_indent_on_capital_i(self) -> Self {
        Self { indent_on_capital_i: false, ..self }
    }
}

impl Mode for Normal {
    type Widget = Buffer;

    fn send_key(&mut self, pa: &mut Pass, event: KeyEvent, handle: Handle) {
        let opts = handle.opts(pa);

        let move_to_match = |[c0, c1]: [_; 2], alt_word, moved| {
            use Category::*;
            let (cat0, cat1) = (Category::of(c0, opts), Category::of(c1, opts));
            !matches!(
                (cat0, cat1, alt_word, moved),
                (Word, Word, ..)
                    | (Special, Special, ..)
                    | (Space, Space, ..)
                    | (Word | Special, Word | Special, true, _)
                    | (.., true)
            )
        };

        handle.write(pa).text_mut().new_moment();
        let rec = if handle.write(pa).record_selections(false) {
            2
        } else {
            1
        };

        let param = if let event!(Char(char)) = event
            && let Some(digit) = char.to_digit(10)
        {
            crate::parameter::add(pa, digit);
            return;
        } else {
            crate::parameter::take(pa) as usize
        };

        match event {
            ////////// Basic movement keys
            event!(Char('h' | 'H') | Left) | shift!(Left) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = event.code == Char('H') || event.modifiers == KeyMod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(-(param as i32));
                });
                self.sel_type = SelType::Normal;
            }
            event!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(event.modifiers.contains(KeyMod::SHIFT), &mut c);
                c.move_ver_wrapped(param as i32);
            }),
            event!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(event.modifiers.contains(KeyMod::SHIFT), &mut c);
                c.move_ver_wrapped(-(param as i32));
            }),
            alt!(Down) => handle.scroll_ver(pa, param as i32),
            alt!(Up) => handle.scroll_ver(pa, -(param as i32)),
            event!(Char('l' | 'L') | Right) | shift!(Right) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor = event.code == Char('L') || event.modifiers == KeyMod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(param as i32);
                });
                self.sel_type = SelType::Normal;
            }
            event!(char @ ('j' | 'J')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(char == 'J', &mut c);
                c.move_ver(param as i32);
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
            event!(char @ ('k' | 'K')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(char == 'K', &mut c);
                c.move_ver(-(param as i32));
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
            event!('w') | alt!('w') => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(KeyMod::ALT);
                if let Some(((p0, c0), (p1, c1))) = { no_nl_windows(c.chars_fwd()).next() } {
                    let move_to_match = move_to_match([c0, c1], alt_word, p0 != c.caret());
                    c.move_to(if move_to_match { p1 } else { p0 });

                    let range = c.search_fwd(word_and_space(alt_word, opts)).nth(param - 1);
                    if let Some(range) = range {
                        c.move_to(range);
                    }
                };
            }),
            event!('e') | alt!('e') => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(KeyMod::ALT);
                if let Some(((p0, c0), (p1, c1))) = { no_nl_windows(c.chars_fwd()).next() } {
                    let move_to_match = move_to_match([c0, c1], alt_word, p0 != c.caret());
                    c.move_to(if move_to_match { p1 } else { p0 });

                    let range = c.search_fwd(space_and_word(alt_word, opts)).nth(param - 1);
                    if let Some(range) = range {
                        c.move_to(range);
                    }
                };
            }),
            event!('b') | alt!('b') => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(KeyMod::ALT);
                let init = {
                    let iter = [(c.caret(), c.char())].into_iter().chain(c.chars_rev());
                    no_nl_windows(iter).next()
                };
                if let Some(((p1, c1), (_, c0))) = init {
                    let moved = p1 != c.caret();
                    c.move_to(p1);
                    if !move_to_match([c1, c0], alt_word, moved) {
                        c.move_hor(1);
                    }

                    let range = c.search_rev(word_and_space(alt_word, opts)).nth(param - 1);
                    if let Some(range) = range {
                        c.move_to(range);
                        c.set_caret_on_start();
                    };
                };
            }),

            event!('W') | alt!('W') => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(KeyMod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                if let Some(range) = { c.search_fwd(word_and_space(alt_word, opts)).nth(param - 1) }
                {
                    c.move_to(range.end);
                    c.move_hor(-1);
                }
            }),
            event!('E') | alt!('E') => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(KeyMod::ALT);
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                if let Some(range) = { c.search_fwd(space_and_word(alt_word, opts)).nth(param - 1) }
                {
                    c.move_to(range.end);
                    c.move_hor(-1);
                }
            }),
            event!('B') | alt!('B') => handle.edit_all(pa, |mut c| {
                let alt_word = event.modifiers.contains(KeyMod::ALT);
                set_anchor_if_needed(true, &mut c);
                if let Some(range) = { c.search_rev(word_and_space(alt_word, opts)).nth(param - 1) }
                {
                    c.move_to(range.start);
                }
            }),

            event!('x') => handle.edit_all(pa, |mut c| {
                self.sel_type = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut c);
                c.set_caret_on_start();
                let p0 = c.search_rev("\n").next().map(|range| range.end);
                c.move_to(p0.unwrap_or_default());
                c.swap_ends();

                let b1 = c.search_fwd("\n").next().map(|range| range.start);
                c.move_to(b1.unwrap_or(c.last_point().byte()));
                c.set_desired_vcol(usize::MAX);
            }),
            event!(char @ ('f' | 'F' | 't' | 'T')) | alt!(char @ ('f' | 'F' | 't' | 'T')) => {
                let mf = event.modifiers;
                let sel_type = match (mf.contains(KeyMod::SHIFT), mf.contains(KeyMod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };

                mode::set(if let 'f' | 'F' = char {
                    OneKey::Find(param - 1, sel_type, self.f_and_t_set_search)
                } else {
                    OneKey::Until(param - 1, sel_type, self.f_and_t_set_search)
                });
            }
            alt!('l' | 'L') | event!(End) => handle.edit_all(pa, |mut c| {
                if event.code == Char('l') {
                    c.unset_anchor();
                }
                select_to_end_of_line(true, c);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            alt!('h' | 'H') | event!(Home) => handle.edit_all(pa, |mut c| {
                if event.code == Char('h') {
                    c.unset_anchor();
                }
                set_anchor_if_needed(true, &mut c);
                c.move_hor(-(c.v_caret().char_col() as i32));
            }),
            alt!('a') => mode::set(OneKey::Around(param - 1, self.brackets)),
            alt!('i') => mode::set(OneKey::Inside(param - 1, self.brackets)),
            event!('%') => handle.edit_main(pa, |mut c| {
                c.move_to_start();
                c.set_anchor();
                c.move_to(c.last_point())
            }),
            event!(char @ ('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::new(event, opts, self.brackets).unwrap();
                    let e_range = object.find_ahead(c, 0)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(char == 'M', c);
                    c.move_to(e_range.end);
                    c.move_hor(-1);

                    let bound = c.strs(e_range.clone()).unwrap().to_string();
                    let [s_b, e_b] = self.brackets.bounds_matching(&bound)?;
                    let s_range = Object::Bounds(s_b, e_b).find_behind(c, 1)?;
                    if char == 'm' {
                        c.set_anchor();
                    }
                    c.move_to(s_range.start);
                    if prev_caret.byte() != e_range.end - 1 {
                        if char == 'm' {
                            c.set_anchor();
                        }
                        c.move_to(e_range.end);
                        c.move_hor(-1);
                    }

                    Some(())
                })
            }
            alt!(char @ ('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let object = Object::new(event, opts, self.brackets).unwrap();
                    let s_range = object.find_behind(c, 0)?;
                    let prev_caret = c.caret();
                    set_anchor_if_needed(char == 'M', c);
                    c.move_to(s_range.start);

                    let bound = c.strs(s_range.clone()).unwrap().to_string();
                    let [s_b, e_b] = self.brackets.bounds_matching(&bound)?;

                    let e_range = Object::Bounds(s_b, e_b).find_ahead(c, 1)?;
                    if char == 'm' {
                        c.set_anchor();
                    }
                    c.move_to(e_range.end);
                    c.move_hor(-1);
                    if prev_caret.byte() != s_range.start {
                        if char == 'm' {
                            c.set_anchor();
                        }
                        c.move_to(s_range.start);
                    }

                    Some(())
                })
            }

            ////////// Insertion mode keys
            event!('i') => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_start();
                });
                mode::set(Insert::new());
            }
            event!('I') => {
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
            event!('a') => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                mode::set(Insert::new());
            }
            event!('A') => {
                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    c.move_to(p);
                });
                mode::set(Insert::new());
            }
            // TODO: Implement parameter
            event!('o') | alt!('o') => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    let caret = c.caret();
                    let (p, _) = c.chars_fwd().find(|(_, c)| *c == '\n').unwrap();
                    c.move_to(p);
                    c.append("\n");
                    if event.modifiers == KeyMod::NONE {
                        c.move_hor(1);
                        c.ts_reindent();
                    } else {
                        c.move_to(caret);
                    }
                });
                if event.modifiers == KeyMod::NONE {
                    mode::set(Insert::new());
                }
            }
            // TODO: Implement parameter
            event!('O') | alt!('O') => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_start();
                    let char_col = c.v_caret().char_col();
                    c.move_hor(-(char_col as i32));
                    c.insert("\n");
                    if event.modifiers == KeyMod::NONE {
                        c.ts_reindent();
                    } else {
                        c.move_hor(char_col as i32 + 1);
                    }
                });
                if event.modifiers == KeyMod::NONE {
                    mode::set(Insert::new());
                }
            }

            ////////// Selection alteration keys
            event!('r') => mode::set(OneKey::Replace),
            event!('`') => handle.edit_all(pa, |mut c| {
                let lower = c
                    .selection()
                    .flat_map(str::chars)
                    .flat_map(char::to_lowercase);
                c.replace(lower.collect::<String>());
            }),
            event!('~') => handle.edit_all(pa, |mut c| {
                let upper = c
                    .selection()
                    .flat_map(str::chars)
                    .flat_map(char::to_uppercase);
                c.replace(upper.collect::<String>());
            }),
            alt!('`') => handle.edit_all(pa, |mut c| {
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
            alt!(';') => handle.edit_all(pa, |mut c| c.swap_ends()),
            event!(';') => handle.edit_all(pa, |mut c| {
                c.unset_anchor();
            }),
            alt!(':') => handle.edit_all(pa, |mut c| {
                c.set_caret_on_end();
            }),
            event!(')') => handle.selections_mut(pa).rotate_main(1),
            event!('(') => handle.selections_mut(pa).rotate_main(-1),
            // TODO: Implement parameter
            alt!(')') => {
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
            // TODO: Implement parameter
            alt!('(') => {
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
            alt!('_') => {
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
            alt!('s') => handle.edit_all(pa, |mut c| {
                c.set_caret_on_start();
                let Some(end) = c.anchor() else {
                    return;
                };
                let lines: Vec<_> = c.search_fwd_until("[^\n]*\n", end).collect();
                let mut last_end = c.caret().byte();
                for range in lines {
                    let mut e_copy = c.copy();
                    e_copy.move_to(range.start);
                    e_copy.set_anchor();
                    e_copy.move_to(range.end);
                    e_copy.move_hor(-1);

                    last_end = range.end;
                }
                c.move_to(last_end);
                c.swap_ends();
            }),
            alt!('S') => handle.edit_all(pa, |mut c| {
                if c.anchor().is_some() {
                    let mut e_copy = c.copy();
                    e_copy.swap_ends();
                    e_copy.unset_anchor();
                    c.unset_anchor();
                }
            }),

            ////////// Line alteration keys
            event!('>') => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let range = c.range_excl();
                    let old_caret = c.v_caret();
                    let old_anchor = c.v_anchor();
                    c.unset_anchor();

                    let insert = if INSERT_TABS.load(Ordering::Relaxed) {
                        "\t".repeat(param)
                    } else {
                        " ".repeat(c.opts().tabstop_spaces_at(0) as usize * param)
                    };

                    for line in range.start.line()..=range.end.line() {
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

                    processed_lines.extend(range.start.line()..=range.end.line());
                });
            }
            event!('<') => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let range = c.range_excl();

                    let mut caret = (c.caret().line(), c.v_caret().char_col());
                    let mut anchor = c.v_anchor().map(|vp| (vp.line(), vp.char_col()));

                    c.unset_anchor();

                    let find = format!(
                        "^(\t{{1,{}}}| {{1,{}}})",
                        param,
                        c.opts().tabstop_spaces_at(0) as usize * param
                    );

                    for line in (range.start.line()..=range.end.line()).rev() {
                        if processed_lines.contains(&line) {
                            continue;
                        }
                        let s_range = c.text().line_range(line);
                        c.move_to(s_range.start);
                        let Some(s_range) = c.search_fwd_until(&find, s_range.end).next() else {
                            continue;
                        };

                        c.move_to(s_range.clone());
                        c.replace("");
                        if line == caret.0 {
                            caret.1 = caret.1.saturating_sub(s_range.end - s_range.start);
                        }
                        if let Some(anchor) = &mut anchor
                            && line == anchor.0
                        {
                            anchor.1 = anchor.1.saturating_sub(s_range.end - s_range.start);
                        }
                    }

                    if let Some((line, col)) = anchor {
                        c.move_to_coords(line, col);
                        c.set_anchor();
                    } else {
                        c.unset_anchor();
                    }
                    c.move_to_coords(caret.0, caret.1);

                    processed_lines.extend(range.start.line()..=range.end.line());
                });
            }
            alt!('j') => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut c| {
                    let c_range = c.range();

                    if c_range.start.line() == c_range.end.line() {
                        if !processed_lines.contains(&c_range.start.line())
                            && let Some(range) = { c.search_fwd("\n").next() }
                        {
                            c.move_to(range);
                            c.replace("");
                            c.reset();
                            processed_lines.push(c_range.start.line());
                        }
                    } else {
                        let caret_was_on_end = c.set_caret_on_start();
                        let nls: Vec<_> = c.search_fwd_until("\n", c_range.end).collect();

                        let mut lines_joined = 0;
                        for (line, range) in nls.into_iter().rev().enumerate() {
                            if processed_lines.contains(&(line + c_range.start.line())) {
                                continue;
                            }

                            c.move_to(range);
                            c.replace("");
                            lines_joined += 1;
                            processed_lines.push(c_range.start.line());
                        }

                        c.unset_anchor();
                        c.move_to(c_range.start);
                        c.set_anchor();
                        c.move_hor(
                            (c_range.end.char() - (c_range.start.char() + lines_joined)) as i32,
                        );
                        if !caret_was_on_end {
                            c.swap_ends();
                        }
                    }
                });
            }

            ////////// Clipboard keys
            event!('y') => duat::mode::copy_selections(pa, &handle),
            event!(char @ ('d' | 'c')) => {
                if event.modifiers == KeyMod::NONE {
                    duat::mode::copy_selections(pa, &handle);
                }
                handle.edit_all(pa, |mut c| {
                    let prev_char = c.chars_rev().next();
                    if c.range().end == c.len()
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
                if char == 'c' {
                    mode::set(Insert::new());
                }
            }
            event!(char @ ('p' | 'P')) => {
                let pastes = duat::mode::paste_strings();
                if pastes.is_empty() {
                    return;
                }
                let mut p_iter = pastes.iter().cycle();
                handle.edit_all(pa, |mut c| {
                    let paste = p_iter.next().unwrap().repeat(param);

                    let anchor_is_start = c.anchor_is_start();

                    // If it ends in a new line, we gotta move to the start of the line.
                    let appended = if char == 'p' {
                        c.set_caret_on_end();
                        if paste.ends_with('\n') {
                            let (p, _) =
                                c.chars_fwd().find(|(_, c)| *c == '\n').unwrap_or_default();
                            c.move_to(p);
                            c.append(&paste);
                        } else {
                            c.append(&paste)
                        }
                        true
                    } else {
                        c.set_caret_on_start();
                        if paste.ends_with('\n') {
                            let char_col = c.v_caret().char_col();
                            c.move_hor(-(char_col as i32));
                            c.insert(&paste);
                        } else {
                            c.insert(&paste)
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
            event!('R') => {
                let pastes = duat::mode::paste_strings();
                if !pastes.is_empty() {
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| c.replace(p_iter.next().unwrap().repeat(param)));
                }
            }

            ////////// Cursor creation and destruction
            event!(',') => handle.selections_mut(pa).remove_extras(),
            event!('C') | alt!('C') => {
                let (nth, mult) = if event.modifiers == KeyMod::NONE {
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
            event!('/') => mode::set(IncSearch::new(SearchFwd)),
            alt!('/') => mode::set(IncSearch::new(SearchRev)),
            event!('?') => mode::set(IncSearch::new(ExtendFwd)),
            alt!('?') => mode::set(IncSearch::new(ExtendRev)),
            event!('s') => mode::set(IncSearch::new(Select)),
            event!('S') => mode::set(IncSearch::new(Split)),
            event!(char @ ('n' | 'N')) | alt!(char @ ('n' | 'N')) => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::error!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut c| {
                    if char == 'N' {
                        c.copy();
                    }
                    let caret = c.caret();
                    let next = if event.modifiers == KeyMod::ALT {
                        c.search_rev(&*search)
                            .filter(|r| r.start != caret.byte())
                            .nth(param - 1)
                    } else {
                        c.search_fwd(&*search)
                            .filter(|r| r.start != caret.byte())
                            .nth(param - 1)
                    };
                    if let Some(range) = next {
                        c.move_to(range.start);
                        if !range.is_empty() {
                            c.set_anchor();
                            c.move_to(range.end);
                            c.move_hor(-1);
                        }
                    }
                });
            }
            event!('*') => handle.edit_main(pa, |c| {
                let pat = c.selection().to_string();
                *SEARCH.lock().unwrap() = escaped_str(&pat);
                context::info!("Set search pattern to [a]{pat}");
            }),

            ////////// Jumping
            alt!(char @ ('u' | 'U')) => {
                let jmp = if char == 'u' { -rec } else { rec };
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
            event!(':') => mode::set(RunCommands::new()),
            event!('|') => mode::set(PipeSelections::new()),
            event!('g') if param > 1 => {
                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut c| {
                    c.unset_anchor();
                    c.move_to_coords(param - 1, 0);
                })
            }
            event!('g') => mode::set(OneKey::GoTo(SelType::Normal)),
            event!('G') if param > 1 => {
                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut c| {
                    c.set_anchor_if_needed();
                    c.move_to_coords(param - 1, 0)
                })
            }
            event!('G') => mode::set(OneKey::GoTo(SelType::Extend)),
            event!(' ') => mode::set(mode::User),

            ////////// History manipulation
            event!('u') => handle.text_mut(pa).undo(),
            event!('U') => handle.text_mut(pa).redo(),
            _ => {}
        }
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle) {
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

fn word_and_space(alt_word: bool, opts: PrintOpts) -> String {
    if alt_word {
        "[^ \t\n]*[ \t]*".to_string()
    } else {
        let cat = opts.word_chars_regex();
        format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t]+")
    }
}

fn space_and_word(alt_word: bool, opts: PrintOpts) -> String {
    if alt_word {
        "[ \t]*[^ \t\n]*".to_string()
    } else {
        let cat = opts.word_chars_regex();
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
