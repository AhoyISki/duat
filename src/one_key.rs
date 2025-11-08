use duat_core::{
    buffer::Buffer,
    cmd,
    context::{self, Handle},
    data::Pass,
    mode::{self, KeyEvent, KeyMod, Mode, event},
};

use crate::{
    Category, Normal, Object, SEARCH, SelType, edit_or_destroy_all, normal::Brackets,
    select_to_end_of_line, set_anchor_if_needed,
};

#[derive(Clone, Copy)]
pub(crate) enum OneKey {
    GoTo(SelType),
    Find(usize, SelType, bool),
    Until(usize, SelType, bool),
    Inside(usize, Brackets),
    Around(usize, Brackets),
    Replace,
}

impl Mode for OneKey {
    type Widget = Buffer;

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle) {
        let just_char = just_char(key_event);
        let sel_type = match (*self, just_char) {
            (OneKey::GoTo(st), _) => match_goto(pa, &handle, key_event, st),
            (OneKey::Find(nth, st, ss) | OneKey::Until(nth, st, ss), Some(char)) => {
                let is_t = matches!(*self, OneKey::Until(..));
                match_find_until(pa, handle, char, nth, is_t, st);
                if ss {
                    *SEARCH.lock().unwrap() = char.to_string();
                }
                SelType::Normal
            }
            (OneKey::Inside(nth, brackets) | OneKey::Around(nth, brackets), _) => {
                let is_inside = matches!(*self, OneKey::Inside(..));
                match_inside_around(pa, handle, key_event, nth, brackets, is_inside);
                SelType::Normal
            }
            (OneKey::Replace, Some(char)) => {
                handle.edit_all(pa, |mut c| {
                    let anchor_didnt_exist = c.set_anchor_if_needed();
                    let len = c.selection().flat_map(str::chars).count();
                    c.replace(char.to_string().repeat(len));
                    if anchor_didnt_exist {
                        c.unset_anchor();
                    }
                });
                SelType::Normal
            }
            _ => SelType::Normal,
        };

        mode::set(Normal::new_with_sel_type(sel_type));
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle) {
        handle.set_mask("OneKey");
    }
}

fn match_goto(
    pa: &mut Pass,
    handle: &Handle,
    key_event: KeyEvent,
    mut sel_type: SelType,
) -> SelType {
    match key_event {
        event!('h') => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            let range = c.search_rev("\n").next();
            c.move_to(range.unwrap_or_default().end);
        }),
        event!('j') => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            c.move_ver(i32::MAX);
        }),
        event!('k' | 'g') => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            c.move_to_coords(0, 0)
        }),
        event!('l') => handle.edit_all(pa, |c| {
            select_to_end_of_line(sel_type == SelType::Extend, c);
            sel_type = SelType::BeforeEndOfLine;
        }),
        event!('i') => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            let range = c.search_rev("(^|\n)[ \t]*").next();
            if let Some(range) = range {
                c.move_to(range.end);

                let points = c.search_fwd("[^ \t]").next();
                if let Some(range) = points {
                    c.move_to(range.start)
                }
            }
        }),

        ////////// File change keys
        event!('a') => _ = cmd::call_notify(pa, "last-buffer"),
        event!('n') => _ = cmd::call_notify(pa, "next-buffer --global"),
        event!('N') => _ = cmd::call_notify(pa, "prev-buffer --global"),
        key_event => context::warn!(
            "[a]{}[] not mapped on [a]go to",
            duat_core::mode::keys_to_string(&[key_event])
        ),
    }

    sel_type
}

fn match_find_until(
    pa: &mut Pass,
    handle: Handle,
    char: char,
    nth: usize,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    handle.edit_all(pa, |mut c| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let b = c.caret().byte();
        let (points, back) = match st {
            Reverse | ExtendRev => (
                c.search_rev(search).filter(|range| range.end != b).nth(nth),
                1,
            ),
            Normal | Extend => (
                c.search_fwd(search)
                    .filter(|range| range.start != b)
                    .nth(nth),
                -1,
            ),
            _ => unreachable!(),
        };

        if let Some(range) = points
            && range.start != c.caret().byte()
        {
            let is_extension = !matches!(st, Extend | ExtendRev);
            if is_extension || c.anchor().is_none() {
                c.set_anchor();
            }
            c.move_to(range.start);
            if is_t {
                c.move_hor(back);
            }
        } else if nth == 0 {
            context::warn!("Char [a]{char}[] not found")
        } else {
            let suffix = match (nth + 1) % 10 {
                1 => "st",
                2 => "nd",
                3 => "rd",
                _ => "th",
            };

            context::warn!("{}{suffix} char [a]{char}[] not found", nth + 1)
        }
    });
}

fn match_inside_around(
    pa: &mut Pass,
    handle: Handle,
    event: KeyEvent,
    nth: usize,
    brackets: Brackets,
    is_inside: bool,
) {
    let mode::KeyCode::Char(char) = event.code else {
        context::warn!("Key [a]{event.code}[] not mapped on this mode");
        return;
    };

    let opts = handle.opts(pa);
    let initial_cursors_len = handle.selections(pa).len();

    let mut failed = false;

    if let Some(object) = Object::new(event, opts, brackets) {
        match char {
            'w' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let prefix = object.find_behind(c, 0);
                let suffix = object.find_ahead(c, 0)?;
                let b0 = {
                    let b0 = prefix.map(|range| range.start).unwrap_or(c.caret().byte());
                    let b0_cat = Category::of(c.char_at(b0).unwrap(), opts);
                    let b1_cat = Category::of(c.char(), opts);
                    let is_same_cat = event.modifiers == KeyMod::ALT || b0_cat == b1_cat;
                    if is_same_cat { b0 } else { c.caret().byte() }
                };
                c.move_to(b0..suffix.end);
                Some(())
            }),
            's' | ' ' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let start = object.find_behind(c, 0)?.end;
                let end_range = object.find_ahead(c, 0)?;
                if is_inside {
                    c.move_to(start..=end_range.start)
                } else {
                    c.move_to(start..end_range.end)
                }

                Some(())
            }),
            'p' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let end_range = object.find_ahead(c, 0)?;
                let start_range = c
                    .text()
                    .search_rev(r"^\n+", ..end_range.start)
                    .unwrap()
                    .next()?;

                if is_inside {
                    c.move_to(start_range.end..end_range.start);
                } else {
                    c.move_to(start_range.end..end_range.end);
                }
                Some(())
            }),
            'u' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let e_range = object.find_ahead(c, nth + 1)?;
                c.move_to(e_range.start);
                let s_range = object.find_behind(c, nth + 1)?;
                if is_inside {
                    c.move_to(s_range.end..e_range.start);
                } else {
                    let space_start = c.text().search_fwd(r"\A\s+", e_range.start..);
                    let end = if let Some(range) = space_start.unwrap().next() {
                        range.end
                    } else {
                        e_range.end
                    };

                    let space_start = c.text().search_fwd(r"\s+\z", ..s_range.end);
                    let start = if let Some(range) = space_start.unwrap().next() {
                        range.start
                    } else {
                        s_range.start
                    };

                    c.move_to(start..end);
                }

                Some(())
            }),
            _char => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let e_range = object.find_ahead(c, nth + 1)?;
                let s_range = object.find_behind(c, nth + 1)?;
                c.move_to(if is_inside {
                    s_range.end..e_range.start
                } else {
                    s_range.start..e_range.end
                });
                Some(())
            }),
        }
    } else {
        match char {
            'i' => handle.edit_all(pa, |mut c| {
                let indent = c.indent();
                if indent == 0 {
                    let end = c.len();
                    c.move_to(..end);
                } else {
                    c.set_anchor();
                    c.move_hor(-(c.v_caret().char_col() as i32));

                    while c.indent() >= indent && c.caret().line() > 0 {
                        c.move_ver(-1);
                    }
                    c.move_ver(1);
                    c.swap_ends();

                    while c.indent() >= indent && c.caret().line() + 1 < c.text().len().line() {
                        c.move_ver(1);
                    }
                    c.move_ver(-1);

                    if is_inside {
                        let range = c.text().line_range(c.caret().line());
                        c.move_to(range.end);
                        c.move_hor(-1);
                    } else {
                        let end = c.search_fwd("\n+").next().unwrap().end;
                        c.move_to(end);
                    }
                }
            }),
            _ => context::warn!("Key [a]{event.code}[] not mapped on this mode"),
        }
    }

    if initial_cursors_len == 1 && failed {
        let rel = if is_inside { "inside" } else { "around" };
        context::warn!("Failed selecting {rel} object");
    }
}

fn just_char(key_event: KeyEvent) -> Option<char> {
    if let event!(mode::KeyCode::Char(char)) = key_event {
        Some(char)
    } else {
        None
    }
}
