use std::sync::{LazyLock, Mutex, atomic::Ordering};

use duat_base::modes::{
    ExtendFwd, ExtendRev, IncSearch, PipeSelections, RunCommands, SearchFwd, SearchRev,
};
use duat_core::{
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    hook::{self, KeyTyped},
    mode::{self, Bindings, KeyEvent, KeyMod, Mode, VPoint, alt, ctrl, event, shift},
    opts::PrintOpts,
    text::{Strs, txt},
};
use duat_filetype::AutoPrefix;
use duat_jump_list::{BufferJumps, JumpListId};

use crate::{
    Category, Object, SEARCH, SelType, edit_or_destroy_all, escaped_regex, escaped_str,
    inc_searchers::{KeepMatching, Select, Split},
    insert::reindent,
    one_key::OneKey,
    opts::INSERT_TABS,
    select_to_end_of_line, set_anchor_if_needed,
};

#[derive(Clone, Copy)]
pub struct Normal {
    sel_type: SelType,
    one_key: Option<OneKey>,
    only_one_action: bool,
}

impl Normal {
    /// Returns an instance of the [`Normal`] mode, inspired by
    /// Kakoune
    pub const fn new() -> Self {
        Normal {
            sel_type: SelType::Normal,
            one_key: None,
            only_one_action: false,
        }
    }

    /// The same as [`Self::new`], but immediately returns to insert
    /// mode
    pub(crate) const fn only_one_action() -> Self {
        Self { only_one_action: true, ..Self::new() }
    }
}

impl Mode for Normal {
    type Widget = Buffer;

    fn bindings() -> mode::Bindings {
        use mode::{KeyCode::*, alt, event, shift};

        let word = txt!("[a]word[separator],[a]WORD");
        let select = txt!("[a]Select[separator],[a]extend");
        let below = txt!("[a]below[separator],[a]above");
        let ahead = txt!("[a]ahead[separator],[a]behind");
        let undo = txt!("[a]Undo[separator],[a]redo");

        let object = {
            let object = mode::bindings!(match _ {
                event!('b' | '(' | ')') => txt!("parenthesis block"),
                event!('B' | '{' | '}') => txt!("brace block"),
                event!('r' | '[' | ']') => txt!("bracket block"),
                event!('a' | '<' | '>') => txt!("angle bracket block"),
                event!('"' | 'Q') => txt!("double quote string"),
                event!('\'' | 'q') => txt!("single quote string"),
                event!('`' | 'g') => txt!("grave quote string"),
                event!('w') | event!('e') => word.clone(),
                event!('s') => txt!("sentence"),
                event!('p') => txt!("paragraph"),
                event!(' ') => txt!("whitespace"),
                event!('i') => txt!("indent"),
                event!('u') => txt!("argument"),
            });

            move |action: &str| Bindings {
                title: Some(txt!("{action}")),
                ..object.clone()
            }
        };

        let goto = Bindings {
            title: Some(txt!("goto")),
            ..mode::bindings!(match _ {
                event!('h') => txt!("start of line"),
                event!('j') => txt!("end of [a]Buffer"),
                event!('k' | 'g') => txt!("start of [a]Buffer"),
                event!('l') => txt!("end of line"),
                event!('i') => txt!("first character in line"),
                event!('a') => txt!("last swapped [a]Buffer"),
                event!('n') => txt!("next [a]Buffer"),
                event!('N') => txt!("previous [a]Buffer"),
            })
        };

        let mut tf = {
            let words = [
                ["select", "to", "next"],
                ["extend", "to", "next"],
                ["select", "until", "next"],
                ["extend", "until", "next"],
                ["select", "to", "previous"],
                ["extend", "to", "previous"],
                ["select", "until", "previous"],
                ["extend", "until", "previous"],
            ];

            words
                .map(|[sel, to, next]| Bindings {
                    title: Some(txt!("{sel} {to} {next}")),
                    ..mode::bindings!(match _ {
                        event!(Char(..)) => txt!("[key.char]{{char}}[] to {sel} {to}"),
                    })
                })
                .into_iter()
        };

        let mut obj = {
            let words = [
                ["select", "to", "start"],
                ["select", "to", "end"],
                ["extend", "to", "start"],
                ["extend", "to", "end"],
                ["select", "until", "start"],
                ["select", "until", "end"],
                ["extend", "until", "start"],
                ["extend", "until", "end"],
            ];
            words
                .map(|[sel, to, dir]| object(format!("{sel} {to} whole object {dir}").as_str()))
                .into_iter()
        };

        mode::bindings!(match _ {
            event!('h' | 'j' | 'k' | 'l') => txt!("Move cursor"),
            event!('H' | 'J' | 'K' | 'L') => txt!("Select and move cursor"),
            event!(Left | Up | Down | Right) => txt!("Move cursor wrapped"),
            shift!(Left | Up | Down | Right) => txt!("Select and move cursor wrapped"),
            event!('w' | 'W') => txt!("{select} to end of [a]word[]/space"),
            event!('e' | 'E') => txt!("{select} to end of [a]WORD[]/space"),
            event!('b' | 'B') => txt!("{select} to start of [a]word[]/space"),
            event!('v' | 'V') => txt!("{select} to start of [a]WORD[]/space"),
            event!('x') => txt!("Select whole line"),
            event!('f') => (txt!("Select to next match"), tf.next().unwrap()),
            event!('F') => (txt!("Extend to next match"), tf.next().unwrap()),
            event!('t') => (txt!("Select until next match"), tf.next().unwrap()),
            event!('T') => (txt!("Extend until next match"), tf.next().unwrap()),
            alt!('f') => (txt!("Select to previous match"), tf.next().unwrap()),
            alt!('F') => (txt!("Extend to previous match"), tf.next().unwrap()),
            alt!('t') => (txt!("Select until previous match"), tf.next().unwrap()),
            alt!('T') => (txt!("Extend until previous match"), tf.next().unwrap()),
            alt!('.') => txt!(
                "Repeats the last [a]'[separator],[a]\"[separator],[a]g[separator],[a]f[separator],[a]t[] [separator]or[] v sequence"
            ),
            alt!('l' | 'L') | event!(End) | shift!(End) => txt!("{select} to end of line"),
            alt!('h' | 'H') | event!(Home) | shift!(Home) => txt!("{select} to start of line"),
            event!('\"') => (txt!("Select around [a]object"), object("select around")),
            event!('\'') => (txt!("Select inside [a]object"), object("select inside")),
            event!('[') => (txt!("Select to [a]object[] start"), obj.next().unwrap()),
            event!(']') => (txt!("Select to [a]object[] end"), obj.next().unwrap()),
            event!('{') => (txt!("Extend to [a]object[] start"), obj.next().unwrap()),
            event!('}') => (txt!("Extend to [a]object[] end"), obj.next().unwrap()),
            alt!('[') => (txt!("Select until [a]object[] start"), obj.next().unwrap()),
            alt!(']') => (txt!("Select until [a]object[] end"), obj.next().unwrap()),
            alt!('{') => (txt!("Extend until [a]object[] start"), obj.next().unwrap()),
            alt!('}') => (txt!("Extend until [a]object[] end"), obj.next().unwrap()),
            event!('%') => txt!("Select whole [a]Buffer"),
            event!('m' | 'M') => txt!("{select} to next matching pair"),
            alt!('m' | 'M') => txt!("{select} to previous matching pair"),
            event!('i') => txt!("[mode]Insert[] before selection"),
            event!('I') => txt!("[mode]Insert[] at the line's start"),
            event!('a') => txt!("[mode]Insert[] after selection"),
            event!('A') => txt!("[mode]Insert[] at the line's end"),
            event!('o' | 'O') => txt!("[mode]Insert[] on new line {below}"),
            alt!('o' | 'O') => txt!("Add new line {below}"),
            event!('.') => txt!("Repeats the last [mode]Insert[] command"),
            event!('r') => (txt!("Replace range"), match _ {
                event!(Char(..)) => txt!("Replace range with [key.char]{{char}}"),
            }),
            event!('`') => txt!("Lowercase the selection"),
            event!('~') => txt!("Uppercase the selection"),
            alt!('`') => txt!("Swap case of selection"),
            alt!(';') => txt!("Swap caret and anchor"),
            event!(';') => txt!("Reduce selection to caret"),
            alt!(':') => txt!("Place caret on end"),
            event!(')') => txt!("Rotate main selection forwards"),
            event!('(') => txt!("Rotate main selection backwards"),
            alt!(')') => txt!("Rotate selections's content forwards"),
            alt!('(') => txt!("Rotate selections's content backwards"),
            alt!('_') => txt!("Merge adjacent selections"),
            event!('X') => txt!("Split selections on lines"),
            shift!('D') => txt!("Divide selections on each end"),
            event!('>') => txt!("Indent selections's lines"),
            event!('<') => txt!("Dedent selections's lines"),
            alt!('j') => txt!("Merge selections's lines"),
            event!('y') => txt!("Yank selections"),
            event!('d' | 'c') => txt!("[a]Delete[separator],[a]change[] selection"),
            alt!('d' | 'c') => txt!("[a]Delete[separator],[a]change[] selection w/o yanking"),
            event!('p' | 'P') => txt!("Paste [a]ahead[separator],[a]behind[]"),
            event!('R') => txt!("Replace selections with pasted content"),
            event!(',') => txt!("Remove extra selections"),
            event!('C') | alt!('C') => txt!("Copy selection {below}"),
            event!('/') | alt!('/') => txt!("[mode]Search[] {ahead}"),
            event!('?') | alt!('?') => txt!("[move]Search[] and select {ahead}"),
            event!('s') => txt!("[mode]Select[] matches in selections"),
            event!('S') => txt!("[mode]Split[] selections by matches"),
            alt!('k') => txt!("[mode]Keep[] matching selections"),
            alt!('K') => txt!("[mode]Keep[] [a]non[] matching selections"),
            event!('n' | 'N') => txt!("{select} to next search match"),
            alt!('n' | 'N') => txt!("{select} to previous search match"),
            event!('*') => txt!("Set main selection as search pattern"),
            event!('u' | 'U') => undo.clone(),
            event!(':') => txt!("[a]Run commands[] in prompt line"),
            event!('|') => txt!("[a]Pipe selections[] to external command"),
            event!('g') => (txt!("Go to [parameter]line[] or to places"), goto.clone()),
            event!('G') => (txt!("Select to [paramenter]line[] or to places"), goto),
            alt!('Q') => txt!("Toggle macro recording"),
            alt!('q') => txt!("Replay macro"),
            ctrl!('o') => txt!("Go to previous jump"),
            ctrl!('i') | event!(Tab) => txt!("Go to next jump"),
            event!(' ') => txt!("Enter [mode]User[] mode"),
            alt!('u' | 'U') => txt!("{undo} last selection change"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle) {
        static LAST_INSERT_KEY: Mutex<Option<InsertKey>> = Mutex::new(None);
        static ALT_DOT: Mutex<Option<(OneKey, KeyEvent)>> = Mutex::new(None);

        static MACRO: Mutex<Option<Vec<KeyEvent>>> = Mutex::new(None);
        static MACRO_GROUP: LazyLock<hook::GroupId> = LazyLock::new(hook::GroupId::new);

        static U_ALT_U_ID: LazyLock<JumpListId> = LazyLock::new(JumpListId::new);

        use mode::KeyCode::*;

        let popts = handle.opts(pa);
        let opts = crate::opts::get();

        let do_match_on_spot = |[c0, c1]: [_; 2], alt_word, moved| {
            use Category::*;
            let (cat0, cat1) = (Category::of(c0, popts), Category::of(c1, popts));
            !matches!(
                (cat0, cat1, alt_word, moved),
                (Word, Word, ..)
                    | (Special, Special, ..)
                    | (Space, Space, ..)
                    | (Word | Special, Word | Special, true, _)
                    | (.., true)
            )
        };

        handle.text_mut(pa).new_moment();

        if let Some(one_key) = self.one_key.take() {
            let (sel_type, succeeded) = one_key.send_key(pa, key_event, &handle);
            self.sel_type = sel_type;
            if self.only_one_action {
                mode::set(pa, crate::Insert);
            }

            if succeeded {
                match one_key {
                    OneKey::GoTo(..) | OneKey::Replace => {}
                    _ => *ALT_DOT.lock().unwrap() = Some((one_key, key_event)),
                }
            }

            return;
        }

        let jump_id = handle.record_jump(pa, *U_ALT_U_ID, false);
        let rec = if jump_id.is_some() { 2 } else { 1 };

        let (param, param_was_set) = if let event!(Char(char)) = key_event
            && let Some(digit) = char.to_digit(10)
        {
            crate::parameter::add_to_param(pa, digit);
            return;
        } else {
            let (param, param_was_set) = crate::parameter::take_param(pa);
            (param as usize, param_was_set)
        };

        // Insert mode functions
        let open_new_line_below = |pa: &mut Pass| {
            handle.edit_all(pa, |mut c| {
                c.set_caret_on_end();
                let caret = c.caret();
                c.move_to_col(usize::MAX);
                c.insert("\n");
                if key_event.modifiers == KeyMod::NONE {
                    c.move_hor(1);
                } else {
                    c.move_to(caret);
                }
            });

            if key_event.modifiers == KeyMod::NONE {
                handle.edit_all(pa, |mut c| {
                    if c.add_comment() {
                        c.insert(' ');
                        c.move_hor(1);
                    }
                });
                let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut c| _ = reindent(0, indents.next().unwrap(), &mut c));
                }
            }
        };
        let open_new_line_above = |pa: &mut Pass| {
            handle.edit_all(pa, |mut c| {
                c.set_caret_on_start();
                let char_col = c.v_caret().char_col();
                c.move_to_col(0);
                c.insert("\n");
                if key_event.modifiers != KeyMod::NONE {
                    c.move_hor(char_col as i32 + 1);
                }
            });

            if key_event.modifiers == KeyMod::NONE {
                handle.edit_all(pa, |mut c| {
                    if c.add_comment() {
                        c.insert(' ');
                        c.move_hor(1);
                    }
                });
                let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut c| _ = reindent(0, indents.next().unwrap(), &mut c));
                }
            }
        };
        let delete_selections = |pa: &mut Pass| {
            handle.edit_all(pa, |mut c| {
                let prev_char = c.text()[..c.caret()].chars().next_back();
                if c.range().end == c.len()
                    && c.selection() == "\n"
                    && let Some('\n') = prev_char
                {
                    c.set_anchor();
                    c.move_hor(-1);
                }

                c.set_anchor_if_needed();
                c.replace("");
                c.unset_anchor();
            });
        };

        match key_event {
            ////////// Basic movement keys
            event!(Char('h' | 'H') | Left) | shift!(Left) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor =
                        key_event.code == Char('H') || key_event.modifiers == KeyMod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(-(param as i32));
                });
                self.sel_type = SelType::Normal;
            }
            event!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers.contains(KeyMod::SHIFT), &mut c);
                c.move_ver_wrapped(param as i32);
            }),
            event!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers.contains(KeyMod::SHIFT), &mut c);
                c.move_ver_wrapped(-(param as i32));
            }),
            alt!(Down) => handle.scroll_ver(pa, param as i32),
            alt!(Up) => handle.scroll_ver(pa, -(param as i32)),
            event!(Char('l' | 'L') | Right) | shift!(Right) => {
                handle.edit_all(pa, |mut c| {
                    let set_anchor =
                        key_event.code == Char('L') || key_event.modifiers == KeyMod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut c);
                    c.move_hor(param as i32);
                });
                self.sel_type = SelType::Normal;
            }
            event!(char @ ('j' | 'J' | 'k' | 'K')) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(char == 'J' || char == 'K', &mut c);

                let param = match char {
                    'j' | 'J' => param as i32,
                    'k' | 'K' => -(param as i32),
                    _ => unreachable!(),
                };
                if !c.move_ver(param) {
                    c.reset();
                    return;
                }

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
            event!(char @ ('w' | 'e')) => handle.edit_all(pa, |mut c| {
                let alt = char == 'e';
                let iter = c.text()[c.caret()..]
                    .char_indices()
                    .map(|(b, char)| (b + c.caret().byte(), char));
                if let Some([(b0, c0), (b1, c1)]) = no_nl_pair(iter) {
                    let move_to_match = do_match_on_spot([c0, c1], alt, b0 != c.caret().byte());
                    c.move_to(if move_to_match { b1 } else { b0 });

                    let range = c
                        .search(word_or_space(alt, false, popts))
                        .from_caret()
                        .nth(param - 1);
                    if let Some(range) = range {
                        c.move_to(range);
                    }
                };
            }),
            event!(char @ ('b' | 'v')) => handle.edit_all(pa, |mut c| {
                let alt = char == 'v';
                let init = {
                    let iter = [(c.caret().byte(), c.char())]
                        .into_iter()
                        .chain(c.text()[..c.caret()].char_indices().rev());
                    no_nl_pair(iter)
                };
                if let Some([(b1, c1), (_, c0)]) = init {
                    let moved = b1 != c.caret().byte();
                    c.move_to(b1);
                    if !do_match_on_spot([c1, c0], alt, moved) {
                        c.move_hor(1);
                    }

                    let range = c
                        .search(word_or_space(alt, true, popts))
                        .to_caret()
                        .nth_back(param - 1);
                    if let Some(range) = range {
                        c.move_to(range);
                        c.set_caret_on_start();
                    };
                };
            }),

            event!(char @ ('W' | 'E')) => handle.edit_all(pa, |mut c| {
                let alt = char == 'E';
                set_anchor_if_needed(true, &mut c);
                c.move_hor(1);
                if let Some(range) = {
                    c.search(word_or_space(alt, false, popts))
                        .from_caret()
                        .nth(param - 1)
                } {
                    c.move_to(range.end);
                    c.move_hor(-1);
                }
            }),
            event!(char @ ('B' | 'V')) => handle.edit_all(pa, |mut c| {
                let alt = char == 'V';
                set_anchor_if_needed(true, &mut c);
                if let Some(range) = {
                    c.search(word_or_space(alt, true, popts))
                        .to_caret()
                        .nth_back(param - 1)
                } {
                    c.move_to(range.start);
                }
            }),

            event!('x') => handle.edit_all(pa, |mut c| {
                self.sel_type = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut c);
                c.set_caret_on_start();
                let b0 = c.search("\n").to_caret().next_back().map(|r| r.end);
                c.move_to(b0.unwrap_or_default());
                c.swap_ends();

                let b1 = c.search("\n").from_caret().next().map(|r| r.start);
                c.move_to(b1.unwrap_or(c.last_point().byte()));
                c.set_desired_vcol(usize::MAX);
            }),
            event!(char @ ('f' | 'F' | 't' | 'T')) | alt!(char @ ('f' | 'F' | 't' | 'T')) => {
                let mf = key_event.modifiers;
                let sel_type = match (mf.contains(KeyMod::SHIFT), mf.contains(KeyMod::ALT)) {
                    (true, true) => SelType::ExtendRev,
                    (true, false) => SelType::Extend,
                    (false, true) => SelType::Reverse,
                    (false, false) => SelType::Normal,
                };

                self.one_key = Some(if let 'f' | 'F' = char {
                    OneKey::Find(param - 1, sel_type, opts.f_and_t_set_search)
                } else {
                    OneKey::Until(param - 1, sel_type, opts.f_and_t_set_search)
                });
            }
            alt!('.') => {
                if let Some((one_key, key_event)) = *ALT_DOT.lock().unwrap() {
                    let (sel_type, _) = one_key.send_key(pa, key_event, &handle);
                    self.sel_type = sel_type;
                } else {
                    context::warn!("No previous 2 key sequence");
                }
            }
            alt!('l' | 'L') | event!(End) | shift!(End) => handle.edit_all(pa, |mut c| {
                if key_event.code == Char('l') {
                    c.unset_anchor();
                }
                select_to_end_of_line(true, c);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            alt!('h' | 'H') | event!(Home) | shift!(Home) => handle.edit_all(pa, |mut c| {
                if key_event.code == Char('h') {
                    c.unset_anchor();
                }
                set_anchor_if_needed(true, &mut c);
                c.move_hor(-(c.v_caret().char_col() as i32));
            }),
            event!('\"') => self.one_key = Some(OneKey::Surrounding(param, false)),
            event!('\'') => self.one_key = Some(OneKey::Surrounding(param, true)),
            event!('[') => self.one_key = Some(OneKey::ToPrevious(param, false, true)),
            event!(']') => self.one_key = Some(OneKey::ToNext(param, false, true)),
            event!('{') => self.one_key = Some(OneKey::ToPrevious(param, false, false)),
            event!('}') => self.one_key = Some(OneKey::ToNext(param, false, false)),
            alt!('[') => self.one_key = Some(OneKey::ToPrevious(param, true, true)),
            alt!(']') => self.one_key = Some(OneKey::ToNext(param, true, true)),
            alt!('{') => self.one_key = Some(OneKey::ToPrevious(param, true, false)),
            alt!('}') => self.one_key = Some(OneKey::ToNext(param, true, false)),
            event!('%') => handle.edit_main(pa, |mut c| {
                c.move_to_start();
                c.set_anchor();
                c.move_to(c.last_point())
            }),
            event!(char @ ('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let mut i = 0;
                    let object = Object::new(key_event, popts, opts.brackets).unwrap();

                    set_anchor_if_needed(char == 'M', c);

                    (0..param).try_for_each(|_| {
                        c.move_hor(1);
                        let end = object.find_ahead(c, 0, true)?;
                        c.move_to(end);
                        if char == 'm' {
                            c.set_anchor();
                            let bounds = opts.brackets.bounds_matching(c.selection())?;
                            let object = Object::two_bounds_simple(bounds[0], bounds[1]);
                            let start = object.find_behind(c, 1, false)?;
                            c.move_to(start);
                            c.set_caret_on_end();
                        }
                        i += 1;
                        Some(())
                    });

                    (i > 0).then_some(())
                })
            }
            alt!(char @ ('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |c| {
                    let mut i = 0;
                    let object = Object::new(key_event, popts, opts.brackets).unwrap();

                    set_anchor_if_needed(char == 'M', c);

                    (0..param).try_for_each(|_| {
                        let start = object.find_behind(c, 0, false)?;
                        c.move_to(start);

                        if char == 'm' {
                            c.set_anchor();
                            let bounds = opts.brackets.bounds_matching(c.selection())?;
                            let object = Object::two_bounds_simple(bounds[0], bounds[1]);
                            let end = object.find_ahead(c, 0, true)?;
                            c.move_to(end);
                            c.set_caret_on_start();
                        }
                        i += 1;
                        Some(())
                    });

                    (i > 0).then_some(())
                })
            }

            ////////// Insertion mode keys
            event!('i') => {
                handle.edit_all(pa, |mut c| _ = c.set_caret_on_start());
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Insert);
                mode::set(pa, crate::Insert);
            }
            event!('I') => {
                if opts.indent_on_capital_i {
                    let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                    if is_ts_indent {
                        handle.edit_all(pa, |mut c| {
                            reindent(c.indent(), indents.next().unwrap(), &mut c);
                        });
                    }
                }

                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    c.move_to_col(c.indent());
                });

                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::InsertStart);
                mode::set(pa, crate::Insert);
            }
            event!('a') => {
                handle.edit_all(pa, |mut c| {
                    c.set_caret_on_end();
                    c.move_hor(1);
                });
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Append);
                mode::set(pa, crate::Insert);
            }
            event!('A') => {
                handle.edit_all(pa, |mut c| {
                    c.unset_anchor();
                    c.move_to_col(usize::MAX);
                });
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::AppendEnd);
                mode::set(pa, crate::Insert);
            }
            // TODO: Implement parameter
            event!('o') | alt!('o') => {
                open_new_line_below(pa);
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::NewLineBelow);
                if key_event.modifiers == KeyMod::NONE {
                    mode::set(pa, crate::Insert);
                }
            }
            // TODO: Implement parameter
            event!('O') | alt!('O') => {
                open_new_line_above(pa);
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::NewLineAbove);
                if key_event.modifiers == KeyMod::NONE {
                    mode::set(pa, crate::Insert);
                }
            }
            event!('.') => {
                match *LAST_INSERT_KEY.lock().unwrap() {
                    Some(InsertKey::Insert) => {
                        handle.edit_all(pa, |mut c| _ = c.set_caret_on_start())
                    }
                    Some(InsertKey::Append) => handle.edit_all(pa, |mut c| {
                        c.set_caret_on_end();
                        c.move_hor(1);
                    }),
                    Some(InsertKey::Change) => delete_selections(pa),
                    Some(InsertKey::InsertStart) => {
                        handle.edit_all(pa, |mut c| {
                            c.unset_anchor();
                            if !opts.indent_on_capital_i {
                                c.move_to_col(c.indent());
                            }
                        });

                        if opts.indent_on_capital_i {
                            let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                            if is_ts_indent {
                                handle.edit_all(pa, |mut c| {
                                    reindent(c.indent(), indents.next().unwrap(), &mut c);
                                })
                            }
                        }
                    }
                    Some(InsertKey::AppendEnd) => handle.edit_all(pa, |mut c| {
                        c.unset_anchor();
                        c.move_to_col(usize::MAX);
                    }),
                    Some(InsertKey::NewLineAbove) => open_new_line_above(pa),
                    Some(InsertKey::NewLineBelow) => open_new_line_below(pa),
                    None => context::warn!("No previous insertion"),
                }
                crate::insert::repeat_last_insert(pa, &handle)
            }

            ////////// Selection alteration keys
            event!('r') => self.one_key = Some(OneKey::Replace),
            event!('`') => handle.edit_all(pa, |mut c| {
                let lower = c.selection().chars().flat_map(char::to_lowercase);
                c.replace(lower.collect::<String>());
            }),
            event!('~') => handle.edit_all(pa, |mut c| {
                let upper = c.selection().chars().flat_map(char::to_uppercase);
                c.replace(upper.collect::<String>());
            }),
            alt!('`') => handle.edit_all(pa, |mut c| {
                let inverted = c.selection().chars().map(|c| {
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
            event!(';') => handle.edit_all(pa, |mut c| _ = c.unset_anchor()),
            alt!(':') => handle.edit_all(pa, |mut c| _ = c.set_caret_on_end()),
            event!(')') => handle.selections_mut(pa).rotate_main(1),
            event!('(') => handle.selections_mut(pa).rotate_main(-1),
            // TODO: Implement parameter
            alt!(')') => {
                if handle.selections(pa).len() == 1 {
                    return;
                }

                let mut last_sel = None;

                handle.edit_all(pa, |mut c| {
                    if let Some(last_sel) = last_sel.replace(c.selection().to_string()) {
                        c.set_anchor_if_needed();
                        c.replace(last_sel);
                    }
                });

                handle.edit_nth(pa, 0, |mut c| c.replace(last_sel.unwrap()));
            }
            // TODO: Implement parameter
            alt!('(') => {
                if handle.selections(pa).len() == 1 {
                    return;
                }
                let mut selections = Vec::<String>::new();
                handle.edit_all(pa, |c| selections.push(c.selection().to_string()));
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
            event!('X') => handle.edit_all(pa, |mut c| {
                c.set_caret_on_start();
                let Some(end) = c.anchor() else {
                    return;
                };
                let caret = c.caret();
                let lines: Vec<_> = c.search("[^\n]*\n").range(caret..end).collect();
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
            event!('D') => handle.edit_all(pa, |mut c| {
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
                        let s_range = c.text().line(line).byte_range();
                        c.move_to(s_range.start);
                        let range = c.caret().byte()..s_range.end;
                        let Some(s_range) = c.search(&find).range(range).next() else {
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
                            && let Some(range) = { c.search("\n").from_caret().next() }
                        {
                            c.move_to(range);
                            c.replace("");
                            c.reset();
                            processed_lines.push(c_range.start.line());
                        }
                    } else {
                        let caret_was_on_end = c.set_caret_on_start();
                        let range = c.caret()..c_range.end;
                        let nls: Vec<_> = c.search("\n").range(range).collect();

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
            event!('y') => duat_base::modes::copy_selections(pa, &handle),
            event!(char @ ('d' | 'c')) | alt!(char @ ('d' | 'c')) => {
                if key_event.modifiers == KeyMod::NONE {
                    duat_base::modes::copy_selections(pa, &handle);
                }
                delete_selections(pa);
                if char == 'c' {
                    *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Change);
                    mode::set(pa, crate::Insert);
                }
            }
            event!(char @ ('p' | 'P')) => {
                let pastes = duat_base::modes::paste_strings();
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
                            let (b, _) = c.text()[c.caret()..]
                                .char_indices()
                                .find(|(_, char)| *char == '\n')
                                .unwrap_or_default();
                            c.move_to(c.caret().byte() + b);
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
                let pastes = duat_base::modes::paste_strings();
                if !pastes.is_empty() {
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut c| {
                        c.set_anchor_if_needed();
                        c.replace(p_iter.next().unwrap().repeat(param))
                    });
                }
            }

            ////////// Cursor creation and destruction
            event!(',') => handle.selections_mut(pa).remove_extras(),
            event!('C') | alt!('C') => {
                fn cols_eq(lhs: (VPoint, Option<VPoint>), rhs: (VPoint, Option<VPoint>)) -> bool {
                    lhs.0.visual_col() == rhs.0.visual_col()
                        && lhs
                            .1
                            .zip(rhs.1)
                            .is_none_or(|(lhs, rhs)| lhs.visual_col() == rhs.visual_col())
                }

                let (nth, mult) = if key_event.modifiers == KeyMod::NONE {
                    (handle.read(pa).selections().len() - 1, 1)
                } else {
                    (0, -1)
                };

                handle.edit_nth(pa, nth, |mut c| {
                    c.copy();
                    let (v_caret, v_anchor) = (c.v_caret(), c.v_anchor());
                    let lines_diff = v_anchor.map(|vp| vp.line().abs_diff(v_caret.line()) as i32);
                    let mut lines = mult * (lines_diff.unwrap_or(0) + 1);

                    while c.move_ver(lines) {
                        c.move_to_col(v_caret.visual_col());
                        if let Some(v_anchor) = v_anchor {
                            c.swap_ends();
                            c.move_ver(lines);
                            c.move_to_col(v_anchor.visual_col());
                            c.swap_ends();
                        }

                        if cols_eq((v_caret, v_anchor), (c.v_caret(), c.v_anchor())) {
                            return;
                        }
                        lines = mult;
                    }
                    c.destroy();
                });

                handle
                    .selections_mut(pa)
                    .set_main(nth + (key_event.modifiers == KeyMod::NONE) as usize);
            }

            ////////// Search keys
            event!('/') => _ = mode::set(pa, IncSearch::new(SearchFwd)),
            alt!('/') => _ = mode::set(pa, IncSearch::new(SearchRev)),
            event!('?') => _ = mode::set(pa, IncSearch::new(ExtendFwd)),
            alt!('?') => _ = mode::set(pa, IncSearch::new(ExtendRev)),
            event!('s') => _ = mode::set(pa, IncSearch::new(Select)),
            event!('S') => _ = mode::set(pa, IncSearch::new(Split)),
            alt!('k') => _ = mode::set(pa, IncSearch::new(KeepMatching(true))),
            alt!('K') => _ = mode::set(pa, IncSearch::new(KeepMatching(false))),

            event!('n') | alt!('n') => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::warn!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut c| {
                    let found = if key_event.modifiers == KeyMod::ALT {
                        c.search(&*search)
                            .to_caret()
                            .rev()
                            .chain(c.search(&*search).from_caret_excl().rev())
                            .nth(param - 1)
                    } else {
                        c.search(&*search)
                            .from_caret_excl()
                            .chain(c.search(&*search).to_caret())
                            .nth(param - 1)
                    };
                    if let Some(range) = found {
                        c.move_to(range.start);
                        if !range.is_empty() {
                            c.set_anchor();
                            c.move_to(range.end);
                            c.move_hor(-1);
                        }
                    }
                });
            }
            event!('N') | alt!('N') => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::warn!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut c| {
                    let mut found = Vec::new();
                    if key_event.modifiers == KeyMod::ALT {
                        found.extend(c.search(&*search).to_caret().rev().take(param));
                        found.extend(
                            c.search(&*search)
                                .from_caret_excl()
                                .rev()
                                .take(param - found.len()),
                        );
                    } else {
                        found.extend(c.search(&*search).from_caret_excl().take(param));
                        found.extend(c.search(&*search).to_caret().take(param - found.len()));
                    };

                    for range in found {
                        c.copy();
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
                if let Some(jump) = handle.move_jumps_by(pa, *U_ALT_U_ID, jmp) {
                    jump.apply(pa, &handle);
                }
            }

            ////////// Macro keys
            alt!('q') if hook::group_exists(*MACRO_GROUP) => {
                context::warn!("Recursive macro calls are not permitted");
                return;
            }
            alt!('q') => {
                if let Some(key_events) = MACRO.lock().unwrap().clone() {
                    duat_core::mode::type_keys(key_events);
                } else {
                    context::warn!("No macro recorded");
                }
            }
            alt!('Q') => {
                if hook::group_exists(*MACRO_GROUP) {
                    context::info!("Stopped recording macro");
                    hook::remove(*MACRO_GROUP);
                } else {
                    context::info!("Started recording macro");
                    *MACRO.lock().unwrap() = None;
                    hook::add::<KeyTyped>(|_, key_event| {
                        if mode::is_currently::<Normal>()
                            && let event!('Q' | 'q') = key_event
                        {
                            return;
                        }

                        let mut macro_keys = MACRO.lock().unwrap();
                        if let Some(key_events) = macro_keys.as_mut() {
                            key_events.push(key_event);
                        } else {
                            *macro_keys = Some(vec![key_event]);
                        }
                    })
                    .grouped(*MACRO_GROUP);
                }
            }

            ////////// Jumping around
            ctrl!('o') => jump_list::jump_by(pa, &handle, -1),
            ctrl!('i') | event!(Tab) => jump_list::jump_by(pa, &handle, 1),

            ////////// Other mode changing keys
            event!(':') => _ = mode::set(pa, RunCommands::new()),
            event!('|') => _ = mode::set(pa, PipeSelections::new()),
            event!('g') if param_was_set => {
                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut c| {
                    c.unset_anchor();
                    c.move_to_coords(param - 1, 0);
                });
                mode::reset_current_sequence(pa);
                jump_list::register(pa, &handle, 5);
            }
            event!('g') => self.one_key = Some(OneKey::GoTo(SelType::Normal)),
            event!('G') if param_was_set => {
                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut c| {
                    c.set_anchor_if_needed();
                    c.move_to_coords(param - 1, 0)
                });
                mode::reset_current_sequence(pa);
                jump_list::register(pa, &handle, 5);
            }
            event!('G') => self.one_key = Some(OneKey::GoTo(SelType::Extend)),
            event!(' ') => _ = mode::set(pa, mode::User),

            ////////// History manipulation
            event!('u') => handle.text_mut(pa).undo(),
            event!('U') => handle.text_mut(pa).redo(),
            _ => {}
        }

        if self.one_key.is_none() && self.only_one_action {
            mode::set(pa, crate::Insert);
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
pub(crate) struct Brackets(pub(crate) &'static [[&'static str; 2]]);

impl Brackets {
    pub(crate) fn bounds_matching(&self, bound: &Strs) -> Option<[&'static str; 2]> {
        self.0
            .iter()
            .find(|bs| bs.contains(&escaped_regex(bound)))
            .copied()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &[&'static str; 2]> + '_ {
        self.0.iter()
    }
}

fn no_nl_pair(iter: impl Iterator<Item = (usize, char)>) -> Option<[(usize, char); 2]> {
    let mut entry0 = None;

    for (point, char) in iter {
        if char == '\n' {
            entry0 = None;
        } else if let Some(entry0) = entry0 {
            return Some([entry0, (point, char)]);
        } else {
            entry0 = Some((point, char));
        }
    }

    None
}

fn word_or_space(alt_word: bool, backwards: bool, opts: PrintOpts) -> String {
    if alt_word {
        if backwards {
            "[^ \t\n]+[ \t]*".to_string()
        } else {
            "[ \t]*[^ \t\n]+".to_string()
        }
    } else {
        let cat = opts.word_chars_regex();
        format!("[{cat}]+|[^{cat} \t\n]+|[ \t]+")
    }
}

#[derive(Clone, Copy)]
enum InsertKey {
    Insert,
    Append,
    Change,
    InsertStart,
    AppendEnd,
    NewLineBelow,
    NewLineAbove,
}

pub(crate) mod jump_list {
    use std::sync::{LazyLock, Mutex};

    use duat_core::{
        buffer::BufferId,
        context::{self, Handle},
        data::Pass,
        hook::{self, BufferSwitched},
        mode,
    };
    use duat_jump_list::{BufferJumps, JumpId, JumpListId};

    static JUMP_LIST: Mutex<JumpList> = Mutex::new(JumpList::new());
    static JUMPS_ID: LazyLock<JumpListId> = LazyLock::new(JumpListId::new);
    static JUMPS_HOOK_ID: LazyLock<hook::GroupId> = LazyLock::new(hook::GroupId::new);

    /// A list for jumping around [`Buffer`]s
    #[derive(Debug)]
    pub struct JumpList {
        list: Vec<(JumpId, BufferId)>,
        cur: usize,
    }

    impl JumpList {
        /// Returns a new `JumpList`
        pub const fn new() -> Self {
            Self { list: Vec::new(), cur: 0 }
        }
    }

    /// Register a new jump, if it would be different
    ///
    /// If an equal jump was found at most `eq_lookback` jumps
    /// back, then don't register.
    pub fn register(pa: &mut Pass, handle: &Handle, eq_lookback: usize) {
        let mut jl = JUMP_LIST.lock().unwrap();

        let jump_id = handle.record_or_get_current_jump(pa, *JUMPS_ID);

        for (jump_id, buffer_id) in jl.list[..jl.cur].iter().rev().take(eq_lookback) {
            if *buffer_id == handle.read(pa).buffer_id()
                && let Some(jump) = handle.get_jump(pa, *JUMPS_ID, *jump_id)
                && jump.is_eq(handle.read(pa))
            {
                return;
            }
        }

        let cur = jl.cur;
        jl.list.truncate(cur);
        jl.list.push((jump_id, handle.read(pa).buffer_id()));
        jl.cur += 1;
    }

    /// Jumps by `by` jumps, which can go across [`Buffer`]s and
    /// stuff.
    pub fn jump_by(pa: &mut Pass, handle: &Handle, by: i32) {
        let mut jl = JUMP_LIST.lock().unwrap();

        if jl.list.is_empty() || by == 0 {
            return;
        }

        jl.cur = jl.cur.saturating_add_signed(by as isize).min(jl.list.len());

        while let Some((jump_id, buffer_id)) = jl.list.get(jl.cur).cloned() {
            let cur = jl.cur;

            let new_handle = if buffer_id != handle.read(pa).buffer_id() {
                if let Some(handle) = {
                    context::windows()
                        .buffers(pa)
                        .into_iter()
                        .find(|handle| handle.read(pa).buffer_id() == buffer_id)
                } {
                    hook::remove(*JUMPS_HOOK_ID);
                    mode::reset_to(pa, &handle);
                    add_jump_hook();

                    handle.clone()
                } else {
                    jl.list.remove(cur);
                    jl.cur = jl.cur.saturating_sub((by < 0) as usize);
                    continue;
                }
            } else {
                handle.clone()
            };

            if let Some(jump) = new_handle.go_to_jump(pa, *JUMPS_ID, jump_id) {
                if !new_handle.ptr_eq(handle.widget()) || !jump.is_eq(new_handle.read(pa)) {
                    jump.apply(pa, &new_handle);
                } else if jl.cur > 0 {
                    jl.cur -= 1;
                    continue;
                }
                break;
            } else {
                jl.list.remove(cur);
                jl.cur = jl.cur.saturating_sub((by < 0) as usize);
            }
        }
    }

    /// Add the hook for automatic insertion of jumps
    pub fn add_jump_hook() {
        hook::add::<BufferSwitched>(|pa, (former, current)| {
            if !former.is_closed(pa) {
                register(pa, former, 5);
            }
            register(pa, current, 5);
        })
        .grouped(*JUMPS_HOOK_ID);
    }
}
