use duat_core::{
    cmd,
    context::{self, Handle},
    data::Pass,
    mode::{self, KeyCode, KeyEvent, alt, event},
};

use crate::{
    Object, SEARCH, SelType, edit_or_destroy_all, select_to_end_of_line, set_anchor_if_needed,
};

#[derive(Clone, Copy)]
pub(crate) enum OneKey {
    GoTo(SelType),
    Find(usize, SelType, bool),
    Until(usize, SelType, bool),
    Surrounding(usize, bool, bool),
    ToNext(usize, bool, bool),
    ToPrevious(usize, bool, bool),
    Match(usize, bool),
    Rotate(usize, bool),
    Replace,
}

impl OneKey {
    /// Sends a key to this "[`Mode`]"
    ///
    /// [`Mode`]: duat_core::mode::Mode
    pub(crate) fn send_key(&self, pa: &mut Pass, event: KeyEvent) -> OneKeyOrResult {
        let just_char = just_char(event);
        let widget = context::current_widget(pa);

        match (*self, just_char) {
            (OneKey::GoTo(st), _) => match_goto(pa, &widget, event, st),
            (OneKey::Find(count, st, ss) | OneKey::Until(count, st, ss), Some(char)) => {
                let is_t = matches!(*self, OneKey::Until(..));
                match_find_until(pa, &widget, char, count, is_t, st);
                if ss {
                    *SEARCH.lock().unwrap() = char.to_string();
                }
                OneKeyOrResult::Result(SelType::Normal, true)
            }
            (OneKey::Surrounding(count, inside, extend), _) => {
                match_bounds(pa, &widget, event, count, inside, extend, Bounds::Both)
            }
            (OneKey::ToNext(count, inside, extend), _) => {
                match_bounds(pa, &widget, event, count, inside, extend, Bounds::Ahead)
            }
            (OneKey::ToPrevious(count, inside, extend), _) => {
                match_bounds(pa, &widget, event, count, inside, extend, Bounds::Behind)
            }
            (OneKey::Replace, Some(char)) => {
                widget.edit_all(pa, |mut s| {
                    let anchor_didnt_exist = s.set_anchor_if_needed();
                    let len = s.selection().chars().count();
                    s.replace(char.to_string().repeat(len));
                    if anchor_didnt_exist {
                        s.unset_anchor();
                    }
                });
                OneKeyOrResult::Result(SelType::Normal, true)
            }
            (OneKey::Match(count, set_anchor), _) => {
                match_match(pa, &widget, event, count, set_anchor)
            }
            (OneKey::Rotate(count, fwd), Some(char)) => match_rotate(pa, &widget, count, fwd, char),
            _ => OneKeyOrResult::Result(SelType::Normal, false),
        }
    }

    /// Returns a `OneKey::Until` or `OneKey::Find`.
    pub(crate) fn ft(count: i32, until: bool, extend_selections: bool) -> Self {
        let opts = crate::opts::get();

        let sel_type = match (extend_selections, count < 0) {
            (true, true) => SelType::ExtendRev,
            (true, false) => SelType::Extend,
            (false, true) => SelType::Reverse,
            (false, false) => SelType::Normal,
        };

        let abs = count.unsigned_abs() as usize;
        if until {
            OneKey::Until(abs - 1, sel_type, opts.f_and_t_set_search)
        } else {
            OneKey::Find(abs - 1, sel_type, opts.f_and_t_set_search)
        }
    }
}
/// The result of a [`OneKey`] key being sent.
pub(crate) enum OneKeyOrResult {
    OneKey(OneKey),
    Result(SelType, bool),
}

fn match_goto(
    pa: &mut Pass,
    widget: &Handle,
    key_event: KeyEvent,
    mut sel_type: SelType,
) -> OneKeyOrResult {
    let mut switch_and_register = |cmd| {
        if cmd::call_notify(pa, cmd).is_ok() {
            let handle = context::current_buffer(pa);
            crate::normal::jump_list::register(pa, &handle, 5);
        }
    };

    match key_event {
        event!('h') => widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            let range = s.search("\n").to_cursor().next_back();
            s.move_to(range.unwrap_or_default().end);
        }),
        event!('j') => widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            s.move_ver(i32::MAX);
        }),
        event!('k' | 'g') => widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            s.move_to_coords(0, 0)
        }),
        event!('l') => {
            widget.edit_all(pa, |s| {
                select_to_end_of_line(sel_type == SelType::Extend, s)
            });
            sel_type = SelType::BeforeEndOfLine;
        }
        event!('i') => widget.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            let range = s.search("(\\A|\n)[ \t]*").to_cursor().next_back();
            if let Some(range) = range {
                s.move_to(range.end);

                let points = s.search("[^ \t]").from_cursor().next();
                if let Some(range) = points {
                    s.move_to(range.start)
                }
            }
        }),

        ////////// Buffer change keys
        event!('a') => switch_and_register("last-switched-buffer"),
        event!('n') => switch_and_register("next-buffer --global"),
        event!('N') => switch_and_register("prev-buffer --global"),
        event!('o') => _ = cmd::call(pa, "open"),
        _ => {}
    }

    OneKeyOrResult::Result(sel_type, false)
}

fn match_find_until(
    pa: &mut Pass,
    widget: &Handle,
    char: char,
    count: usize,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    widget.edit_all(pa, |mut s| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let (points, back) = match st {
            Reverse | ExtendRev => (s.search(search).to_cursor().nth_back(count), 1),
            Normal | Extend => (s.search(search).from_cursor_excl().nth(count), -1),
            _ => unreachable!(),
        };

        if let Some(range) = points
            && range.start != s.cursor().byte()
        {
            let is_extension = !matches!(st, Extend | ExtendRev);
            if is_extension || s.anchor().is_none() {
                s.set_anchor();
            }
            s.move_to(range.start);
            if is_t {
                s.move_hor(back);
            }
        } else if count == 0 {
            context::warn!("Char [a]{char}[] not found")
        } else {
            let suffix = match (count + 1) % 10 {
                1 => "st",
                2 => "nd",
                3 => "rd",
                _ => "th",
            };

            context::warn!("{}{suffix} char [a]{char}[] not found", count + 1)
        }
    });
}

fn match_match(
    pa: &mut Pass,
    widget: &Handle,
    event: KeyEvent,
    count: usize,
    extend: bool,
) -> OneKeyOrResult {
    let opts = crate::opts::get();
    let popts = widget.opts(pa);
    let key_event = KeyEvent::from(KeyCode::Char('m'));

    match event {
        event!(char @ ('l' | 'm' | 'M')) => {
            let mut failed = false;
            let failed = &mut failed;
            edit_or_destroy_all(pa, &widget, failed, |s| {
                let mut i = 0;
                let object = Object::new(key_event, popts, opts.brackets).unwrap();

                set_anchor_if_needed(extend || char == 'M', s);

                (0..count).try_for_each(|_| {
                    s.move_hor(1);
                    let (_, end) = object.find(s, 0, true);
                    s.move_to(end?);
                    if !(extend || char == 'M') {
                        s.set_anchor();
                        let bounds = opts.brackets.bounds_matching(s.selection())?;
                        let object = Object::two_bounds_simple(bounds[0], bounds[1]);
                        let (start, _) = object.find(s, 1, false);
                        s.move_to(start?);
                        s.set_cursor_on_end();
                    }
                    i += 1;
                    Some(())
                });

                (i > 0).then_some(())
            })
        }
        event!(char @ 'h') | alt!(char @ ('m' | 'M')) => {
            let mut failed = false;
            let failed = &mut failed;
            edit_or_destroy_all(pa, &widget, failed, |s| {
                let mut i = 0;
                let object = Object::new(key_event, popts, opts.brackets).unwrap();

                set_anchor_if_needed(extend, s);

                (0..count).try_for_each(|_| {
                    let (start, _) = object.find(s, 0, false);
                    s.move_to(start?);

                    if !extend {
                        s.set_anchor();
                        let bounds = opts.brackets.bounds_matching(s.selection())?;
                        let object = Object::two_bounds_simple(bounds[0], bounds[1]);
                        let (_, end) = object.find(s, 0, true);
                        s.move_to(end?);
                        s.set_cursor_on_start();
                    }
                    i += 1;
                    Some(())
                });

                (i > 0).then_some(())
            })
        }
        event!('i') => return OneKeyOrResult::OneKey(OneKey::Surrounding(count, true, extend)),
        event!('a') => return OneKeyOrResult::OneKey(OneKey::Surrounding(count, false, extend)),
        _ => return OneKeyOrResult::Result(SelType::Normal, false),
    }

    OneKeyOrResult::Result(SelType::Normal, true)
}

fn match_rotate(
    pa: &mut Pass,
    widget: &Handle,
    count: usize,
    fwd: bool,
    char: char,
) -> OneKeyOrResult {
    match char {
        's' if fwd => widget.rotate_main_selection(pa, count as i32),
        's' => widget.rotate_main_selection(pa, -(count as i32)),
        // TODO: Implement parameter
        'c' if fwd => {
            if widget.selections(pa).len() == 1 {
                return OneKeyOrResult::Result(SelType::Normal, true);
            }

            let mut last_sel = None;

            widget.edit_all(pa, |mut s| {
                if let Some(last_sel) = last_sel.replace(s.selection().to_string()) {
                    s.set_anchor_if_needed();
                    s.replace(last_sel);
                }
            });

            widget.edit_nth(pa, 0, |mut s| s.replace(last_sel.unwrap()));
        }
        // TODO: Implement parameter
        'c' => {
            if widget.selections(pa).len() == 1 {
                return OneKeyOrResult::Result(SelType::Normal, true);
            }
            let mut selections = Vec::<String>::new();
            widget.edit_all(pa, |s| selections.push(s.selection().to_string()));
            let mut s_iter = selections.into_iter().cycle();
            s_iter.next();
            widget.edit_all(pa, |mut s| {
                if let Some(next) = s_iter.next() {
                    s.set_anchor_if_needed();
                    s.replace(next);
                }
            });
        }
        _ => return OneKeyOrResult::Result(SelType::Normal, false),
    }

    OneKeyOrResult::Result(SelType::Normal, true)
}

fn match_bounds(
    pa: &mut Pass,
    widget: &Handle,
    event: KeyEvent,
    count: usize,
    inside: bool,
    extend: bool,
    bounds: Bounds,
) -> OneKeyOrResult {
    let opts = widget.opts(pa);
    let initial_sels_len = widget.selections(pa).len();
    let brackets = crate::opts::get().brackets;

    let mut failed = false;

    if let Some(object) = Object::new(event, opts, brackets) {
        edit_or_destroy_all(pa, widget, &mut failed, |s| {
            let old_range = s.range();
            match bounds {
                Bounds::Ahead => {
                    if extend {
                        s.set_anchor();
                    } else {
                        s.set_anchor_if_needed();
                    }
                    let (_, p) = object.find(s, count, inside);
                    s.move_to(p?.saturating_sub(1));
                }
                Bounds::Behind => {
                    if extend {
                        s.set_anchor();
                    } else {
                        s.set_anchor_if_needed();
                    }
                    let (p, _) = object.find(s, count, inside);
                    s.move_to(p?);
                }
                Bounds::Both => {
                    let (start, end) = object.find(s, count, inside);
                    let start = if extend {
                        start?.min(old_range.start.byte())
                    } else {
                        start?
                    };
                    let end = if extend {
                        end?.max(old_range.end.byte())
                    } else {
                        end?
                    };

                    s.move_to(start..end);
                }
            };
            Some(())
        });

        if initial_sels_len == 1 && failed {
            let rel = if inside { "inside" } else { "around" };
            context::warn!("Failed selecting {rel} object");
        }

        OneKeyOrResult::Result(SelType::Normal, true)
    } else {
        context::warn!(
            "Invalid object: [a]{}",
            duat_core::mode::keys_to_string(&[event])
        );

        OneKeyOrResult::Result(SelType::Normal, false)
    }
}

fn just_char(key_event: KeyEvent) -> Option<char> {
    if let event!(mode::KeyCode::Char(char)) = key_event {
        Some(char)
    } else {
        None
    }
}

enum Bounds {
    Ahead,
    Behind,
    Both,
}
