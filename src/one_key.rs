use std::sync::{LazyLock, Mutex};

use duat_core::{
    mode::{Cursor, KeyCode::*, Selections},
    prelude::*,
};

use crate::{
    Category, Normal, Object, SEARCH, SelType, edit_or_destroy_all, normal::Brackets,
    select_to_end_of_line, set_anchor_if_needed,
};

#[derive(Clone)]
pub(crate) enum OneKey {
    GoTo(SelType),
    Find(SelType, bool),
    Until(SelType, bool),
    Inside(Brackets),
    Around(Brackets),
    Replace,
}

impl<U: Ui> Mode<U> for OneKey {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
        let sel_type = match *self {
            OneKey::GoTo(st) => match_goto::<(), U>(pa, &handle, key, st),
            OneKey::Find(st, ss) | OneKey::Until(st, ss) if let Some(char) = just_char(key) => {
                match_find_until(pa, handle, char, matches!(*self, OneKey::Until(..)), st);
                if ss {
                    *SEARCH.lock().unwrap() = char.to_string();
                }
                SelType::Normal
            }
            OneKey::Inside(brackets) | OneKey::Around(brackets) => {
                let is_inside = matches!(*self, OneKey::Inside(_));
                match_inside_around(pa, handle, key, brackets, is_inside);
                SelType::Normal
            }
            OneKey::Replace if let Some(char) = just_char(key) => {
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

        mode::set::<U>(Normal::new_with_sel_type(sel_type));
    }

    fn on_switch(&mut self, _: &mut Pass, handle: Handle<Self::Widget, U>) {
        handle.set_mask("OneKey");
    }
}

fn match_goto<S, U: Ui>(
    pa: &mut Pass,
    handle: &Handle<File<U>, U, S>,
    key: KeyEvent,
    mut sel_type: SelType,
) -> SelType {
    static LAST_FILE: LazyLock<Mutex<Option<String>>> = LazyLock::new(Mutex::default);

    match key {
        key!(Char('h')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            let p1 = c.search_rev("\n", None).next().map(|[_, p1]| p1);
            c.move_to(p1.unwrap_or_default());
        }),
        key!(Char('j')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            c.move_ver(i32::MAX);
        }),
        key!(Char('k')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            c.move_to_coords(0, 0)
        }),
        key!(Char('l')) => handle.edit_all(pa, |c| {
            select_to_end_of_line(sel_type == SelType::Extend, c);
            sel_type = SelType::BeforeEndOfLine;
        }),
        key!(Char('i')) => handle.edit_all(pa, |mut c| {
            set_anchor_if_needed(sel_type == SelType::Extend, &mut c);
            let p1 = c.search_rev("(^|\n)[ \t]*", None).next().map(|[_, p1]| p1);
            if let Some(p1) = p1 {
                c.move_to(p1);

                let points = c.search_fwd("[^ \t]", None).next();
                if let Some([p0, _]) = points {
                    c.move_to(p0)
                }
            }
        }),

        ////////// File change keys
        key!(Char('a')) => {
            let cur_name = handle.read(pa, |file, _| file.name());
            let last_file = LAST_FILE.lock().unwrap().clone();
            if let Some(last_file) = last_file {
                cmd::queue_notify_and(format!("b {last_file}"), |res| {
                    if res.is_ok() {
                        *LAST_FILE.lock().unwrap() = Some(cur_name)
                    }
                })
            } else {
                context::error!("There is no previous file");
            }
        }
        key!(Char('n')) => {
            let cur_name = handle.read(pa, |file, _| file.name());
            cmd::queue_notify_and("next-file --global", |res| {
                if res.is_ok() {
                    *LAST_FILE.lock().unwrap() = Some(cur_name)
                }
            })
        }
        key!(Char('N')) => {
            let cur_name = handle.read(pa, |file, _| file.name());
            cmd::queue_notify_and("prev-file --global", |res| {
                if res.is_ok() {
                    *LAST_FILE.lock().unwrap() = Some(cur_name)
                }
            })
        }
        KeyEvent { code, .. } => {
            let code = format!("{code:?}");
            context::warn!("Key [a]{code}[] not mapped on [a]go to")
        }
    }

    sel_type
}

fn match_find_until<U: Ui>(
    pa: &mut Pass,
    handle: Handle<File<U>, U, ()>,
    char: char,
    is_t: bool,
    st: SelType,
) {
    use SelType::*;
    handle.edit_all(pa, |mut c| {
        let search = format!("\\x{{{:X}}}", char as u32);
        let cur = c.caret();
        let (points, back) = match st {
            Reverse | ExtendRev => (c.search_rev(search, None).find(|[p1, _]| *p1 != cur), 1),
            Normal | Extend => (c.search_fwd(search, None).find(|[p0, _]| *p0 != cur), -1),
            _ => unreachable!(),
        };

        if let Some([p0, _]) = points
            && p0 != c.caret()
        {
            let is_extension = !matches!(st, Extend | ExtendRev);
            if is_extension || c.anchor().is_none() {
                c.set_anchor();
            }
            c.move_to(p0);
            if is_t {
                c.move_hor(back);
            }
        } else {
            context::warn!("Char [a]{char}[] not found")
        }
    });
}

fn match_inside_around<U: Ui>(
    pa: &mut Pass,
    handle: Handle<File<U>, U, ()>,
    key: KeyEvent,
    brackets: Brackets,
    is_inside: bool,
) {
    fn move_to_points<S, U: Ui>(m: &mut Cursor<File<U>, U::Area, S>, [p0, p1]: [Point; 2]) {
        m.move_to(p0);
        m.set_anchor();
        m.move_to(p1);
    }

    let Char(char) = key.code else {
        context::warn!("Key [a]{key.code}[] not mapped on this mode");
        return;
    };

    let wc = handle.cfg(pa).word_chars;
    let initial_cursors_len = handle.read_selections(pa, Selections::len);

    let mut failed = false;

    if let Some(object) = Object::from_char(char, wc, brackets) {
        match char {
            'w' | 'W' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let start = object.find_behind(c, 0, None);
                let [_, p1] = object.find_ahead(c, 0, None)?;
                let p0 = {
                    let p0 = start.map(|[p0, _]| p0).unwrap_or(c.caret());
                    let p0_cat = Category::of(c.char_at(p0).unwrap(), wc);
                    let p1_cat = Category::of(c.char(), wc);
                    let is_same_cat = char == 'W' || p0_cat == p1_cat;
                    if is_same_cat { p0 } else { c.caret() }
                };
                move_to_points(c, [p0, p1]);
                c.move_hor(-1);
                Some(())
            }),
            's' | ' ' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let [_, p0] = object.find_behind(c, 0, None)?;
                let [p1, _] = object.find_ahead(c, 0, None)?;
                move_to_points(c, [p0, p1]);
                if is_inside || char == ' ' && p0 < c.text().len() {
                    c.move_hor(-1);
                }
                Some(())
            }),
            'p' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let end = object.find_ahead(c, 0, None);
                let [p1, _] = end?;
                c.move_to(p1);
                c.set_anchor();
                let [_, p0] = object.find_behind(c, 0, None).unwrap_or_default();
                c.move_to(p0);
                c.swap_ends();
                if is_inside {
                    c.move_hor(-1);
                }
                Some(())
            }),
            'u' => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let [p2, _] = object.find_ahead(c, 1, None)?;
                c.move_to(p2);
                let [p0, p1] = object.find_behind(c, 1, None)?;
                if is_inside {
                    move_to_points(c, [p1, p2]);
                    c.move_hor(-1);
                } else {
                    move_to_points(c, [p0, p2]);
                    if !matches!(c.char_at(p2), Some(';' | ',')) {
                        c.move_hor(-1);
                    }
                    if !matches!(c.char_at(p0), Some(';' | ',')) {
                        c.swap_ends();
                        c.move_hor(1);
                        c.swap_ends();
                    }
                }

                Some(())
            }),
            _char => edit_or_destroy_all(pa, &handle, &mut failed, |c| {
                let [p2, p3] = object.find_ahead(c, 1, None)?;
                let [p0, p1] = object.find_behind(c, 1, None)?;
                let [p0, p1] = if is_inside { [p1, p2] } else { [p0, p3] };
                move_to_points(c, [p0, p1]);
                c.move_hor(-1);
                Some(())
            }),
        }
    } else {
        match char {
            'i' => handle.edit_all(pa, |mut c| {
                let indent = c.indent();
                if indent == 0 {
                    let end = c.len();
                    move_to_points(&mut c, [Point::default(), end]);
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
                        let [_, p1] = c.text().points_of_line(c.caret().line());
                        c.move_to(p1);
                        c.move_hor(-1);
                    } else {
                        let p1 = c.search_fwd("\n+", None).next().map(|[_, p1]| p1).unwrap();
                        c.move_to(p1);
                    }
                }
            }),
            _ => context::warn!("Key [a]{key.code}[] not mapped on this mode"),
        }
    }

    if initial_cursors_len == 1 && failed {
        let rel = if is_inside { "inside" } else { "around" };
        context::warn!("Failed selecting {rel} object");
    }
}

fn just_char(key: KeyEvent) -> Option<char> {
    if let key!(Char(char)) = key {
        Some(char)
    } else {
        None
    }
}
