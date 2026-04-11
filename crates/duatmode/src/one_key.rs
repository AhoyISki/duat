use duat_core::{
    cmd,
    context::{self, Handle},
    data::Pass,
    mode::{self, KeyEvent, event},
};

use crate::{
    Object, SEARCH, SelType, edit_or_destroy_all, select_to_end_of_line, set_anchor_if_needed,
};

#[derive(Clone, Copy)]
pub(crate) enum OneKey {
    GoTo(SelType),
    Find(usize, SelType, bool),
    Until(usize, SelType, bool),
    Surrounding(usize, bool),
    ToNext(usize, bool, bool),
    ToPrevious(usize, bool, bool),
    Replace,
}

impl OneKey {
    /// Sends a key to this "[`Mode`]"
    ///
    /// [`Mode`]: duat_core::mode::Mode
    pub(crate) fn send_key(
        &self,
        pa: &mut Pass,
        event: KeyEvent,
        handle: &Handle,
    ) -> (SelType, bool) {
        let just_char = just_char(event);

        match (*self, just_char) {
            (OneKey::GoTo(st), _) => (match_goto(pa, handle, event, st), false),
            (OneKey::Find(nth, st, ss) | OneKey::Until(nth, st, ss), Some(char)) => {
                let is_t = matches!(*self, OneKey::Until(..));
                match_find_until(pa, handle, char, nth, is_t, st);
                if ss {
                    *SEARCH.lock().unwrap() = char.to_string();
                }
                (SelType::Normal, true)
            }
            (OneKey::Surrounding(nth, is_inside), _) => {
                match_bounds(pa, handle, event, nth, is_inside, Bounds::Both);
                (SelType::Normal, true)
            }
            (OneKey::ToNext(nth, is_inside, set_anchor), _) => {
                if set_anchor {
                    handle.edit_all(pa, |mut s| s.set_anchor());
                } else {
                    handle.edit_all(pa, |mut s| _ = s.set_anchor_if_needed());
                }
                match_bounds(pa, handle, event, nth, is_inside, Bounds::Ahead);
                (SelType::Normal, true)
            }
            (OneKey::ToPrevious(nth, is_inside, set_anchor), _) => {
                if set_anchor {
                    handle.edit_all(pa, |mut s| s.set_anchor());
                } else {
                    handle.edit_all(pa, |mut s| _ = s.set_anchor_if_needed());
                }
                match_bounds(pa, handle, event, nth, is_inside, Bounds::Behind);
                (SelType::Normal, true)
            }
            (OneKey::Replace, Some(char)) => {
                handle.edit_all(pa, |mut s| {
                    let anchor_didnt_exist = s.set_anchor_if_needed();
                    let len = s.selection().chars().count();
                    s.replace(char.to_string().repeat(len));
                    if anchor_didnt_exist {
                        s.unset_anchor();
                    }
                });
                (SelType::Normal, true)
            }
            _ => (SelType::Normal, false),
        }
    }
}

fn match_goto(
    pa: &mut Pass,
    handle: &Handle,
    key_event: KeyEvent,
    mut sel_type: SelType,
) -> SelType {
    let mut switch_and_register = |cmd| {
        if cmd::call_notify(pa, cmd).is_ok() {
            let handle = context::current_buffer(pa);
            crate::normal::jump_list::register(pa, &handle, 5);
        }
    };

    match key_event {
        event!('h') => handle.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            let range = s.search("\n").to_cursor().next_back();
            s.move_to(range.unwrap_or_default().end);
        }),
        event!('j') => handle.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            s.move_ver(i32::MAX);
        }),
        event!('k' | 'g') => handle.edit_all(pa, |mut s| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut s);
            s.move_to_coords(0, 0)
        }),
        event!('l') => {
            handle.edit_all(pa, |s| {
                select_to_end_of_line(sel_type == SelType::Extend, s)
            });
            sel_type = SelType::BeforeEndOfLine;
        }
        event!('i') => handle.edit_all(pa, |mut s| {
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

        ////////// File change keys
        event!('a') => switch_and_register("last-switched-buffer"),
        event!('n') => switch_and_register("next-buffer --global"),
        event!('N') => switch_and_register("prev-buffer --global"),
        _ => {}
    }

    sel_type
}

fn match_find_until(
    pa: &mut Pass,
    handle: &Handle,
    char: char,
    nth: usize,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    handle.edit_all(pa, |mut s| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let (points, back) = match st {
            Reverse | ExtendRev => (s.search(search).to_cursor().nth_back(nth), 1),
            Normal | Extend => (s.search(search).from_cursor_excl().nth(nth), -1),
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

fn match_bounds(
    pa: &mut Pass,
    handle: &Handle,
    event: KeyEvent,
    nth: usize,
    is_inside: bool,
    bounds: Bounds,
) {
    let opts = handle.opts(pa);
    let initial_sels_len = handle.selections(pa).len();
    let brackets = crate::opts::get().brackets;

    let mut failed = false;

    if let Some(object) = Object::new(event, opts, brackets) {
        edit_or_destroy_all(pa, handle, &mut failed, |s| {
            match bounds {
                Bounds::Ahead => {
                    let p = object.find_ahead(s, nth, is_inside)?;
                    s.move_to(p.saturating_sub(1));
                }
                Bounds::Behind => {
                    let p = object.find_behind(s, nth, is_inside)?;
                    s.move_to(p);
                }
                Bounds::Both => {
                    let start = object.find_behind(s, nth, is_inside)?;
                    let end = object.find_ahead(s, nth, is_inside)?;
                    s.move_to(start..end);
                }
            };
            Some(())
        });
    }

    if initial_sels_len == 1 && failed {
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

enum Bounds {
    Ahead,
    Behind,
    Both,
}
