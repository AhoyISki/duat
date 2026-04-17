use std::sync::{LazyLock, Mutex, atomic::Ordering};

use duat_base::modes::{
    ExtendFwd, ExtendRev, IncSearch, PipeSelections, RunCommands, SearchFwd, SearchRev,
};
use duat_core::{
    Ns,
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
                "Repeats the last \
                 [a]'[separator],[a]\"[separator],[a]g[separator],[a]f[separator],[a]t[] \
                 [separator]or[] v sequence"
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
            alt!(';') => txt!("Swap cursor and anchor"),
            event!(';') => txt!("Reduce selection to cursor"),
            alt!(':') => txt!("Place cursor on end"),
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
            ctrl!('r') => txt!("Reload the config crate"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle) {
        static LAST_INSERT_KEY: Mutex<Option<InsertKey>> = Mutex::new(None);
        static ALT_DOT: Mutex<Option<(OneKey, KeyEvent)>> = Mutex::new(None);

        static MACRO: Mutex<Option<Vec<KeyEvent>>> = Mutex::new(None);
        static MACRO_NS: LazyLock<Ns> = Ns::new_lazy();

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
            handle.edit_all(pa, |mut s| {
                s.set_cursor_on_end();
                let cursor = s.cursor();
                s.move_to_col(usize::MAX);
                s.insert("\n");
                if key_event.modifiers == KeyMod::NONE {
                    s.move_hor(1);
                } else {
                    s.move_to(cursor);
                }
            });

            if key_event.modifiers == KeyMod::NONE {
                handle.edit_all(pa, |mut s| {
                    if s.add_comment() {
                        s.insert(' ');
                        s.move_hor(1);
                    }
                });
                let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut s| _ = reindent(0, indents.next().unwrap(), &mut s));
                }
            }
        };
        let open_new_line_above = |pa: &mut Pass| {
            handle.edit_all(pa, |mut s| {
                s.set_cursor_on_start();
                let char_col = s.v_cursor().char_col();
                s.move_to_col(0);
                s.insert("\n");
                if key_event.modifiers != KeyMod::NONE {
                    s.move_hor(char_col as i32 + 1);
                }
            });

            if key_event.modifiers == KeyMod::NONE {
                handle.edit_all(pa, |mut s| {
                    if s.add_comment() {
                        s.insert(' ');
                        s.move_hor(1);
                    }
                });
                let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut s| _ = reindent(0, indents.next().unwrap(), &mut s));
                }
            }
        };
        let delete_selections = |pa: &mut Pass| {
            handle.edit_all(pa, |mut s| {
                let prev_char = s.text()[..s.cursor()].chars().next_back();
                if s.range().end == s.len()
                    && s.selection() == "\n"
                    && let Some('\n') = prev_char
                {
                    s.set_anchor();
                    s.move_hor(-1);
                }

                s.set_anchor_if_needed();
                s.replace("");
                s.unset_anchor();
            });
        };

        match key_event {
            ////////// Basic movement keys
            event!(Char('h' | 'H') | Left) | shift!(Left) => {
                handle.edit_all(pa, |mut s| {
                    let set_anchor =
                        key_event.code == Char('H') || key_event.modifiers == KeyMod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut s);
                    s.move_hor(-(param as i32));
                });
                self.sel_type = SelType::Normal;
            }
            event!(Down) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(key_event.modifiers.contains(KeyMod::SHIFT), &mut s);
                s.move_ver_wrapped(param as i32);
            }),
            event!(Up) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(key_event.modifiers.contains(KeyMod::SHIFT), &mut s);
                s.move_ver_wrapped(-(param as i32));
            }),
            alt!(Down) => handle.scroll_ver(pa, param as i32),
            alt!(Up) => handle.scroll_ver(pa, -(param as i32)),
            event!(Char('l' | 'L') | Right) | shift!(Right) => {
                handle.edit_all(pa, |mut s| {
                    let set_anchor =
                        key_event.code == Char('L') || key_event.modifiers == KeyMod::SHIFT;
                    set_anchor_if_needed(set_anchor, &mut s);
                    s.move_hor(param as i32);
                });
                self.sel_type = SelType::Normal;
            }
            event!(char @ ('j' | 'J' | 'k' | 'K')) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(char == 'J' || char == 'K', &mut s);

                let param = match char {
                    'j' | 'J' => param as i32,
                    'k' | 'K' => -(param as i32),
                    _ => unreachable!(),
                };
                if !s.move_ver(param) {
                    s.reset();
                    return;
                }

                let v_cursor = s.v_cursor();
                if s.char() == '\n'
                    && v_cursor.char_col() > 0
                    && self.sel_type != SelType::ToEndOfLine
                {
                    s.move_hor(-1);
                    s.set_desired_vcol(if self.sel_type == SelType::BeforeEndOfLine {
                        usize::MAX
                    } else {
                        v_cursor.desired_visual_col()
                    });
                }
            }),

            ////////// Object selection keys
            event!(char @ ('w' | 'e')) => handle.edit_all(pa, |mut s| {
                let alt = char == 'e';
                let iter = s.text()[s.cursor()..]
                    .char_indices()
                    .map(|(b, char)| (b + s.cursor().byte(), char));
                if let Some([(b0, c0), (b1, c1)]) = no_nl_pair(iter) {
                    let move_to_match = do_match_on_spot([c0, c1], alt, b0 != s.cursor().byte());
                    s.move_to(if move_to_match { b1 } else { b0 });

                    let range = s
                        .search(word_and_space(alt, popts))
                        .from_cursor()
                        .nth(param - 1);
                    if let Some(range) = range {
                        s.move_to(range);
                    }
                };
            }),
            event!(char @ ('b' | 'v')) => handle.edit_all(pa, |mut s| {
                let alt = char == 'v';
                let init = {
                    let iter = [(s.cursor().byte(), s.char())]
                        .into_iter()
                        .chain(s.text()[..s.cursor()].char_indices().rev());
                    no_nl_pair(iter)
                };
                if let Some([(b1, c1), (_, c0)]) = init {
                    let moved = b1 != s.cursor().byte();
                    s.move_to(b1);
                    if !do_match_on_spot([c1, c0], alt, moved) {
                        s.move_hor(1);
                    }

                    let range = s
                        .search(word_and_space(alt, popts))
                        .to_cursor()
                        .nth_back(param - 1);
                    if let Some(range) = range {
                        s.move_to(range);
                        s.set_cursor_on_start();
                    };
                };
            }),

            event!(char @ ('W' | 'E')) => handle.edit_all(pa, |mut s| {
                let alt = char == 'E';
                set_anchor_if_needed(true, &mut s);
                s.move_hor(1);
                if let Some(range) = {
                    s.search(word_and_space(alt, popts))
                        .from_cursor()
                        .nth(param - 1)
                } {
                    s.move_to(range.end);
                    s.move_hor(-1);
                }
            }),
            event!(char @ ('B' | 'V')) => handle.edit_all(pa, |mut s| {
                let alt = char == 'V';
                set_anchor_if_needed(true, &mut s);
                if let Some(range) = {
                    s.search(word_and_space(alt, popts))
                        .to_cursor()
                        .nth_back(param - 1)
                } {
                    s.move_to(range.start);
                }
            }),

            event!('x') => handle.edit_all(pa, |mut s| {
                self.sel_type = SelType::ToEndOfLine;
                set_anchor_if_needed(true, &mut s);
                s.set_cursor_on_start();
                let b0 = s.search("\n").to_cursor().next_back().map(|r| r.end);
                s.move_to(b0.unwrap_or_default());
                s.swap_ends();

                let b1 = s.search("\n").from_cursor().next().map(|r| r.start);
                s.move_to(b1.unwrap_or(s.last_point().byte()));
                s.set_desired_vcol(usize::MAX);
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
            alt!('l' | 'L') | event!(End) | shift!(End) => handle.edit_all(pa, |mut s| {
                if key_event.code == Char('l') {
                    s.unset_anchor();
                }
                select_to_end_of_line(true, s);
                self.sel_type = SelType::BeforeEndOfLine;
            }),
            alt!('h' | 'H') | event!(Home) | shift!(Home) => handle.edit_all(pa, |mut s| {
                if key_event.code == Char('h') {
                    s.unset_anchor();
                }
                set_anchor_if_needed(true, &mut s);
                s.move_hor(-(s.v_cursor().char_col() as i32));
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
            event!('%') => handle.edit_main(pa, |mut s| {
                s.move_to_start();
                s.set_anchor();
                s.move_to(s.last_point())
            }),
            event!(char @ ('m' | 'M')) => {
                let mut failed = false;
                let failed = &mut failed;
                edit_or_destroy_all(pa, &handle, failed, |s| {
                    let mut i = 0;
                    let object = Object::new(key_event, popts, opts.brackets).unwrap();

                    set_anchor_if_needed(char == 'M', s);

                    (0..param).try_for_each(|_| {
                        s.move_hor(1);
                        let end = object.find_ahead(s, 0, true)?;
                        s.move_to(end);
                        if char == 'm' {
                            s.set_anchor();
                            let bounds = opts.brackets.bounds_matching(s.selection())?;
                            let object = Object::two_bounds_simple(bounds[0], bounds[1]);
                            let start = object.find_behind(s, 1, false)?;
                            s.move_to(start);
                            s.set_cursor_on_end();
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
                edit_or_destroy_all(pa, &handle, failed, |s| {
                    let mut i = 0;
                    let object = Object::new(key_event, popts, opts.brackets).unwrap();

                    set_anchor_if_needed(char == 'M', s);

                    (0..param).try_for_each(|_| {
                        let start = object.find_behind(s, 0, false)?;
                        s.move_to(start);

                        if char == 'm' {
                            s.set_anchor();
                            let bounds = opts.brackets.bounds_matching(s.selection())?;
                            let object = Object::two_bounds_simple(bounds[0], bounds[1]);
                            let end = object.find_ahead(s, 0, true)?;
                            s.move_to(end);
                            s.set_cursor_on_start();
                        }
                        i += 1;
                        Some(())
                    });

                    (i > 0).then_some(())
                })
            }

            ////////// Insertion mode keys
            event!('i') => {
                handle.edit_all(pa, |mut s| _ = s.set_cursor_on_start());
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Insert);
                mode::set(pa, crate::Insert);
            }
            event!('I') => {
                if opts.indent_on_capital_i {
                    let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                    if is_ts_indent {
                        handle.edit_all(pa, |mut s| {
                            reindent(s.indent(), indents.next().unwrap(), &mut s);
                        });
                    }
                }

                handle.edit_all(pa, |mut s| {
                    s.unset_anchor();
                    s.move_to_col(s.indent());
                });

                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::InsertStart);
                mode::set(pa, crate::Insert);
            }
            event!('a') => {
                handle.edit_all(pa, |mut s| {
                    s.set_cursor_on_end();
                    s.move_hor(1);
                });
                *LAST_INSERT_KEY.lock().unwrap() = Some(InsertKey::Append);
                mode::set(pa, crate::Insert);
            }
            event!('A') => {
                handle.edit_all(pa, |mut s| {
                    s.unset_anchor();
                    s.move_to_col(usize::MAX);
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
                        handle.edit_all(pa, |mut s| _ = s.set_cursor_on_start())
                    }
                    Some(InsertKey::Append) => handle.edit_all(pa, |mut s| {
                        s.set_cursor_on_end();
                        s.move_hor(1);
                    }),
                    Some(InsertKey::Change) => delete_selections(pa),
                    Some(InsertKey::InsertStart) => {
                        handle.edit_all(pa, |mut s| {
                            s.unset_anchor();
                            if !opts.indent_on_capital_i {
                                s.move_to_col(s.indent());
                            }
                        });

                        if opts.indent_on_capital_i {
                            let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                            if is_ts_indent {
                                handle.edit_all(pa, |mut s| {
                                    reindent(s.indent(), indents.next().unwrap(), &mut s);
                                })
                            }
                        }
                    }
                    Some(InsertKey::AppendEnd) => handle.edit_all(pa, |mut s| {
                        s.unset_anchor();
                        s.move_to_col(usize::MAX);
                    }),
                    Some(InsertKey::NewLineAbove) => open_new_line_above(pa),
                    Some(InsertKey::NewLineBelow) => open_new_line_below(pa),
                    None => context::warn!("No previous insertion"),
                }
                crate::insert::repeat_last_insert(pa, &handle)
            }

            ////////// Selection alteration keys
            event!('r') => self.one_key = Some(OneKey::Replace),
            event!('`') => handle.edit_all(pa, |mut s| {
                let lower = s.selection().chars().flat_map(char::to_lowercase);
                s.replace(lower.collect::<String>());
            }),
            event!('~') => handle.edit_all(pa, |mut s| {
                let upper = s.selection().chars().flat_map(char::to_uppercase);
                s.replace(upper.collect::<String>());
            }),
            alt!('`') => handle.edit_all(pa, |mut s| {
                let inverted = s.selection().chars().map(|s| {
                    if s.is_uppercase() {
                        s.to_lowercase().collect::<String>()
                    } else {
                        s.to_uppercase().collect()
                    }
                });
                s.replace(inverted.collect::<String>());
            }),

            ////////// Advanced selection manipulation
            alt!(';') => handle.edit_all(pa, |mut s| s.swap_ends()),
            event!(';') => handle.edit_all(pa, |mut s| _ = s.unset_anchor()),
            alt!(':') => handle.edit_all(pa, |mut s| _ = s.set_cursor_on_end()),
            event!(')') => handle.selections_mut(pa).rotate_main(1),
            event!('(') => handle.selections_mut(pa).rotate_main(-1),
            // TODO: Implement parameter
            alt!(')') => {
                if handle.selections(pa).len() == 1 {
                    return;
                }

                let mut last_sel = None;

                handle.edit_all(pa, |mut s| {
                    if let Some(last_sel) = last_sel.replace(s.selection().to_string()) {
                        s.set_anchor_if_needed();
                        s.replace(last_sel);
                    }
                });

                handle.edit_nth(pa, 0, |mut s| s.replace(last_sel.unwrap()));
            }
            // TODO: Implement parameter
            alt!('(') => {
                if handle.selections(pa).len() == 1 {
                    return;
                }
                let mut selections = Vec::<String>::new();
                handle.edit_all(pa, |s| selections.push(s.selection().to_string()));
                let mut s_iter = selections.into_iter().cycle();
                s_iter.next();
                handle.edit_all(pa, |mut s| {
                    if let Some(next) = s_iter.next() {
                        s.set_anchor_if_needed();
                        s.replace(next);
                    }
                });
            }
            alt!('_') => {
                handle.edit_all(pa, |mut s| {
                    s.set_cursor_on_end();
                    s.move_hor(1);
                });
                // In the first iteration, connected SelectionMuts are joined, this
                // one just undoes the movement.
                handle.edit_all(pa, |mut s| {
                    s.move_hor(-1);
                });
            }
            event!('X') => handle.edit_all(pa, |mut s| {
                s.set_cursor_on_start();
                let Some(end) = s.anchor() else {
                    return;
                };
                let cursor = s.cursor();
                let lines: Vec<_> = s.search("[^\n]*\n").range(cursor..end).collect();
                let mut last_end = s.cursor().byte();
                for range in lines {
                    let mut e_copy = s.copy();
                    e_copy.move_to(range.start);
                    e_copy.set_anchor();
                    e_copy.move_to(range.end);
                    e_copy.move_hor(-1);

                    last_end = range.end;
                }
                s.move_to(last_end);
                s.swap_ends();
            }),
            event!('D') => handle.edit_all(pa, |mut s| {
                if s.anchor().is_some() {
                    let mut e_copy = s.copy();
                    e_copy.swap_ends();
                    e_copy.unset_anchor();
                    s.unset_anchor();
                }
            }),

            ////////// Line alteration keys
            event!('>') => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut s| {
                    let range = s.range_excl();
                    let old_cursor = s.v_cursor();
                    let old_anchor = s.v_anchor();
                    s.unset_anchor();

                    let insert = if INSERT_TABS.load(Ordering::Relaxed) {
                        "\t".repeat(param)
                    } else {
                        " ".repeat(s.opts().tabstop_spaces_at(0) as usize * param)
                    };

                    for line in range.start.line()..=range.end.line() {
                        if processed_lines.contains(&line) {
                            continue;
                        }
                        s.move_to_coords(line, 0);
                        s.insert(&insert);
                    }

                    let cols = insert.chars().count();
                    if let Some(old_anchor) = old_anchor {
                        s.move_to_coords(old_anchor.line(), old_anchor.char_col() + cols);
                        s.set_anchor();
                    }
                    s.move_to_coords(old_cursor.line(), old_cursor.char_col() + cols);

                    processed_lines.extend(range.start.line()..=range.end.line());
                });
            }
            event!('<') => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut s| {
                    let range = s.range_excl();

                    let mut cursor = (s.cursor().line(), s.v_cursor().char_col());
                    let mut anchor = s.v_anchor().map(|vp| (vp.line(), vp.char_col()));

                    s.unset_anchor();

                    let find = format!(
                        "^(\t{{1,{}}}| {{1,{}}})",
                        param,
                        s.opts().tabstop_spaces_at(0) as usize * param
                    );

                    for line in (range.start.line()..=range.end.line()).rev() {
                        if processed_lines.contains(&line) {
                            continue;
                        }
                        let s_range = s.text().line(line).byte_range();
                        s.move_to(s_range.start);
                        let range = s.cursor().byte()..s_range.end;
                        let Some(s_range) = s.search(&find).range(range).next() else {
                            continue;
                        };

                        s.move_to(s_range.clone());
                        s.replace("");
                        if line == cursor.0 {
                            cursor.1 = cursor.1.saturating_sub(s_range.end - s_range.start);
                        }
                        if let Some(anchor) = &mut anchor
                            && line == anchor.0
                        {
                            anchor.1 = anchor.1.saturating_sub(s_range.end - s_range.start);
                        }
                    }

                    if let Some((line, col)) = anchor {
                        s.move_to_coords(line, col);
                        s.set_anchor();
                    } else {
                        s.unset_anchor();
                    }
                    s.move_to_coords(cursor.0, cursor.1);

                    processed_lines.extend(range.start.line()..=range.end.line());
                });
            }
            alt!('j') => {
                let mut processed_lines = Vec::new();
                handle.edit_all(pa, |mut s| {
                    let c_range = s.range();

                    if c_range.start.line() == c_range.end.line() {
                        if !processed_lines.contains(&c_range.start.line())
                            && let Some(range) = { s.search("\n").from_cursor().next() }
                        {
                            s.move_to(range);
                            s.replace(" ");
                            s.reset();
                            processed_lines.push(c_range.start.line());
                        }
                    } else {
                        let cursor_was_on_end = s.set_cursor_on_start();
                        let range = s.cursor()..c_range.end;
                        let nls: Vec<_> = s.search("\n").range(range).collect();

                        let mut lines_joined = 0;
                        for (line, range) in nls.into_iter().rev().enumerate() {
                            if processed_lines.contains(&(line + c_range.start.line())) {
                                continue;
                            }

                            s.move_to(range);
                            s.replace(" ");
                            lines_joined += 1;
                            processed_lines.push(c_range.start.line());
                        }

                        s.unset_anchor();
                        s.move_to(c_range.start);
                        s.set_anchor();
                        s.move_hor(
                            (c_range.end.char() - (c_range.start.char() + lines_joined)) as i32,
                        );
                        if !cursor_was_on_end {
                            s.swap_ends();
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
                handle.edit_all(pa, |mut s| {
                    let paste = p_iter.next().unwrap().repeat(param);

                    let anchor_is_start = s.anchor_is_start();

                    // If it ends in a new line, we gotta move to the start of the line.
                    let appended = if char == 'p' {
                        s.set_cursor_on_end();
                        if paste.ends_with('\n') {
                            let (b, _) = s.text()[s.cursor()..]
                                .char_indices()
                                .find(|(_, char)| *char == '\n')
                                .unwrap_or_default();
                            s.move_to(s.cursor().byte() + b);
                            s.append(&paste);
                        } else {
                            s.append(&paste)
                        }
                        true
                    } else {
                        s.set_cursor_on_start();
                        if paste.ends_with('\n') {
                            let char_col = s.v_cursor().char_col();
                            s.move_hor(-(char_col as i32));
                            s.insert(&paste);
                        } else {
                            s.insert(&paste)
                        }
                        false
                    };

                    if !paste.is_empty() {
                        s.move_hor(appended as i32);
                        s.set_anchor();
                        s.move_hor(paste.chars().count() as i32 - 1);
                        if !anchor_is_start {
                            s.set_cursor_on_start();
                        }
                    }
                });
            }
            event!('R') => {
                let pastes = duat_base::modes::paste_strings();
                if !pastes.is_empty() {
                    let mut p_iter = pastes.iter().cycle();
                    handle.edit_all(pa, |mut s| {
                        s.set_anchor_if_needed();
                        s.replace(p_iter.next().unwrap().repeat(param))
                    });
                }
            }

            ////////// SelectionMut creation and destruction
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

                handle.edit_nth(pa, nth, |mut s| {
                    s.copy();
                    let (v_cursor, v_anchor) = (s.v_cursor(), s.v_anchor());
                    let lines_diff = v_anchor.map(|vp| vp.line().abs_diff(v_cursor.line()) as i32);
                    let mut lines = mult * (lines_diff.unwrap_or(0) + 1);

                    while s.move_ver(lines) {
                        s.move_to_col(v_cursor.visual_col());
                        if let Some(v_anchor) = v_anchor {
                            s.swap_ends();
                            s.move_ver(lines);
                            s.move_to_col(v_anchor.visual_col());
                            s.swap_ends();
                        }

                        if cols_eq((v_cursor, v_anchor), (s.v_cursor(), s.v_anchor())) {
                            return;
                        }
                        lines = mult;
                    }
                    s.destroy();
                });

                handle
                    .selections_mut(pa)
                    .set_main(nth + (key_event.modifiers == KeyMod::NONE) as usize);
            }

            ////////// Search keys
            event!('/') => mode::set(pa, IncSearch::new(SearchFwd)),
            alt!('/') => mode::set(pa, IncSearch::new(SearchRev)),
            event!('?') => mode::set(pa, IncSearch::new(ExtendFwd)),
            alt!('?') => mode::set(pa, IncSearch::new(ExtendRev)),
            event!('s') => mode::set(pa, IncSearch::new(Select)),
            event!('S') => mode::set(pa, IncSearch::new(Split)),
            alt!('k') => mode::set(pa, IncSearch::new(KeepMatching(true))),
            alt!('K') => mode::set(pa, IncSearch::new(KeepMatching(false))),

            event!('n') | alt!('n') => {
                let search = SEARCH.lock().unwrap();
                if search.is_empty() {
                    context::warn!("No search pattern set");
                    return;
                }
                handle.edit_main(pa, |mut s| {
                    let found = if key_event.modifiers == KeyMod::ALT {
                        s.search(&*search)
                            .to_cursor()
                            .rev()
                            .chain(s.search(&*search).from_cursor_excl().rev())
                            .nth(param - 1)
                    } else {
                        s.search(&*search)
                            .from_cursor_excl()
                            .chain(s.search(&*search).to_cursor())
                            .nth(param - 1)
                    };
                    if let Some(range) = found {
                        s.move_to(range.start);
                        if !range.is_empty() {
                            s.set_anchor();
                            s.move_to(range.end);
                            s.move_hor(-1);
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
                handle.edit_main(pa, |mut s| {
                    let mut found = Vec::new();
                    if key_event.modifiers == KeyMod::ALT {
                        found.extend(s.search(&*search).to_cursor().rev().take(param));
                        found.extend(
                            s.search(&*search)
                                .from_cursor_excl()
                                .rev()
                                .take(param - found.len()),
                        );
                    } else {
                        found.extend(s.search(&*search).from_cursor_excl().take(param));
                        found.extend(s.search(&*search).to_cursor().take(param - found.len()));
                    };

                    for range in found {
                        s.copy();
                        s.move_to(range.start);
                        if !range.is_empty() {
                            s.set_anchor();
                            s.move_to(range.end);
                            s.move_hor(-1);
                        }
                    }
                });
            }
            event!('*') => handle.edit_main(pa, |s| {
                let pat = s.selection().to_string();
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
            alt!('q') if hook::group_exists(*MACRO_NS) => {
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
                if hook::group_exists(*MACRO_NS) {
                    context::info!("Stopped recording macro");
                    hook::remove(*MACRO_NS);
                } else {
                    context::info!("Started recording macro");
                    *MACRO.lock().unwrap() = None;
                    hook::add::<KeyTyped>(|_, key_event| {
                        if mode::is::<Normal>()
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
                    .grouped(*MACRO_NS);
                }
            }

            ////////// Jumping around
            ctrl!('o') => jump_list::jump_by(pa, &handle, -1),
            ctrl!('i') | event!(Tab) => jump_list::jump_by(pa, &handle, 1),

            ////////// Other mode changing keys
            event!(':') => mode::set(pa, RunCommands::new()),
            event!('|') => mode::set(pa, PipeSelections::new()),
            event!('g') if param_was_set => {
                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut s| {
                    s.unset_anchor();
                    s.move_to_coords(param - 1, 0);
                });
                mode::reset_current_sequence(pa);
                jump_list::register(pa, &handle, 5);
            }
            event!('g') => self.one_key = Some(OneKey::GoTo(SelType::Normal)),
            event!('G') if param_was_set => {
                handle.selections_mut(pa).remove_extras();
                handle.edit_main(pa, |mut s| {
                    s.set_anchor_if_needed();
                    s.move_to_coords(param - 1, 0)
                });
                mode::reset_current_sequence(pa);
                jump_list::register(pa, &handle, 5);
            }
            event!('G') => self.one_key = Some(OneKey::GoTo(SelType::Extend)),
            event!(' ') => mode::set(pa, mode::User),

            ////////// History manipulation
            event!('u') => handle.text_mut(pa).undo(),
            event!('U') => handle.text_mut(pa).redo(),
            ctrl!('r') => _ = duat_core::cmd::call_notify(pa, "reload"),
            _ => {}
        }

        if self.one_key.is_none() && self.only_one_action {
            mode::set(pa, crate::Insert);
        }
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

fn word_and_space(alt_word: bool, opts: PrintOpts) -> String {
    if alt_word {
        "[^ \t\n]+[ \t]*|[ \t]+".to_string()
    } else {
        let cat = opts.word_chars_regex();
        format!("([{cat}]+|[^{cat} \t\n]+)[ \t]*|[ \t]+")
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
        Ns,
        buffer::BufferId,
        context::{self, Handle},
        data::Pass,
        hook::{self, BufferSwitched},
        mode,
    };
    use duat_jump_list::{BufferJumps, JumpId, JumpListId};

    static JUMP_LIST: Mutex<JumpList> = Mutex::new(JumpList::new());
    static JUMPS_ID: LazyLock<JumpListId> = LazyLock::new(JumpListId::new);
    static JUMPS_NS: LazyLock<Ns> = Ns::new_lazy();

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
                    hook::remove(*JUMPS_NS);
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
            if !former.is_closed() {
                register(pa, former, 5);
            }
            register(pa, current, 5);
        })
        .grouped(*JUMPS_NS);
    }
}
