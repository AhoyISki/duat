use std::sync::{Mutex, atomic::Ordering};

use duat_base::widgets::Completions;
use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    lender::Lender,
    mode::{self, Cursor, KeyEvent, KeyMod, Mode, alt, ctrl, event, shift},
};

use crate::{Normal, opts::INSERT_TABS, set_anchor_if_needed};

#[derive(Clone, Copy)]
pub struct Insert;

impl Mode for Insert {
    type Widget = Buffer;

    fn bindings() -> mode::Bindings {
        use duat_core::text::txt;
        use mode::KeyCode::*;

        mode::bindings!(match _ {
            event!(Char(..) | Enter) => txt!("Insert the character"),
            event!(Left | Down | Up | Right) => txt!("Move cursor"),
            shift!(Left | Down | Up | Right) => txt!("Select and move cursor"),
            event!(Home | End) => txt!("Move to [a]start[][separator],[a]end[] of line"),
            ctrl!('n') => txt!("Next completion entry"),
            ctrl!('p') | shift!(BackTab) => txt!("Previous completion entry"),
            event!(Tab) => txt!("Reindent or next completion entry"),
            event!(Backspace | Delete) => txt!("Remove character or selection"),
            event!(Esc) => txt!("Return to [mode]Normal[] mode"),
            alt!(';') => txt!("Run a single [mode]Normal[] mode command"),
            ctrl!('u') => txt!("Merge changes to this point in a single [a]Moment"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, handle: Handle) {
        use mode::KeyCode::*;

        let opts = crate::opts::get();
        if let shift!(Left | Down | Up | Right) = key_event {
            handle.edit_all(pa, |mut c| {
                if c.anchor().is_none() {
                    c.set_anchor()
                }
            });
        }

        let mut insert_events = INSERT_EVENTS.lock().unwrap();

        let add_reindent = |insert_events: &mut Vec<InsertEvent>| {
            if !matches!(insert_events.last(), Some(InsertEvent::Reindent)) {
                insert_events.push(InsertEvent::Reindent);
            }
        };

        match key_event {
            // Autocompletion commands
            ctrl!('n') => complete(pa, 1, &mut insert_events),
            ctrl!('p') | shift!(BackTab) => complete(pa, -1, &mut insert_events),
            event!(Tab) => {
                let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                add_reindent(&mut insert_events);

                match opts.tab_mode {
                    TabMode::Normal => {
                        handle.edit_all(pa, |mut c| {
                            let indent = indents.next().unwrap();
                            if opts.indent_chars.contains(&'\t')
                                && is_ts_indent
                                && reindent(&mut c, indent)
                            {
                                return;
                            }
                            if INSERT_TABS.load(Ordering::Relaxed) {
                                insert_str(&mut c, '\t', 1, &mut insert_events);
                            } else {
                                let tab_len =
                                    c.opts().tabstop_spaces_at(c.v_caret().visual_col() as u32);
                                let tab = " ".repeat(tab_len as usize);
                                insert_str(&mut c, tab, tab_len as i32, &mut insert_events);
                            }
                        });
                    }
                    TabMode::Smart => handle.edit_all(pa, |mut c| {
                        let char_col = c.v_caret().char_col();
                        let indent = indents.next().unwrap();
                        if (opts.indent_chars.contains(&'\t') || char_col <= c.indent())
                            && is_ts_indent
                            && reindent(&mut c, indent)
                        {
                            return;
                        }

                        if INSERT_TABS.load(Ordering::Relaxed) {
                            insert_str(&mut c, '\t', 1, &mut insert_events);
                        } else {
                            let tab_len =
                                c.opts().tabstop_spaces_at(c.v_caret().visual_col() as u32);
                            let tab = " ".repeat(tab_len as usize);
                            insert_str(&mut c, tab, tab_len as i32, &mut insert_events);
                        }
                    }),
                    TabMode::VerySmart => {
                        let mut do_scroll = true;
                        handle.edit_all(pa, |mut c| {
                            let char_col = c.v_caret().char_col();
                            let indent = indents.next().unwrap();
                            let has_reindented = (opts.indent_chars.contains(&'\t')
                                || char_col <= c.indent())
                                && is_ts_indent
                                && reindent(&mut c, indent);

                            do_scroll &= !has_reindented
                        });

                        if do_scroll {
                            complete(pa, 1, &mut insert_events);
                        }
                    }
                }
            }

            // Regular commands
            event!(Char(char)) => {
                handle.edit_all(pa, |mut c| {
                    insert_str(&mut c, char, 1, &mut insert_events);
                });
                let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut c| {
                        let indent = indents.next().unwrap();
                        if opts.indent_chars.contains(&char)
                            && c.indent() == c.v_caret().char_col() - 1
                        {
                            reindent(&mut c, indent);
                        }
                    })
                }
            }

            event!(Enter) => {
                handle.edit_all(pa, |mut c| insert_str(&mut c, '\n', 1, &mut insert_events));
                if opts.indent_chars.contains(&'\n') {
                    let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                    if is_ts_indent || opts.auto_indent {
                        handle.edit_all(pa, |mut c| _ = reindent(&mut c, indents.next().unwrap()));
                    }
                }
            }
            event!(Backspace) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();

                if c.move_hor(-1) < 0 {
                    c.set_anchor();
                    c.replace("");
                    c.unset_anchor();

                    match insert_events.last_mut() {
                        Some(InsertEvent::Backspace(total)) => *total += 1,
                        Some(_) | None => insert_events.push(InsertEvent::Backspace(1)),
                    }

                    if let Some(prev_anchor) = prev_anchor {
                        c.set_anchor();
                        if prev_anchor > prev_caret {
                            c.move_hor((prev_anchor.char() - prev_caret.char()) as i32);
                        } else {
                            c.move_to(prev_anchor);
                        }
                        c.swap_ends();
                    }
                }
            }),
            event!(Delete) => handle.edit_all(pa, |mut c| {
                let prev_caret = c.caret();
                let prev_anchor = c.unset_anchor();
                c.replace("");

                match insert_events.last_mut() {
                    Some(InsertEvent::Delete(total)) => *total += 1,
                    Some(_) | None => insert_events.push(InsertEvent::Delete(1)),
                }

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
            event!(Left) | shift!(Left) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                move_hor(&mut c, -1, &mut insert_events);
            }),
            event!(Down) | shift!(Down) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                if key_event.modifiers == KeyMod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                move_ver(&mut c, 1, &mut insert_events);
            }),
            event!(Up) | shift!(Up) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                if key_event.modifiers == KeyMod::NONE {
                    c.unset_anchor();
                    remove_empty_line(&mut c);
                }
                move_ver(&mut c, -1, &mut insert_events);
            }),
            event!(Right) | shift!(Right) => handle.edit_all(pa, |mut c| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut c);
                move_hor(&mut c, 1, &mut insert_events);
            }),

            event!(Home) => handle.edit_all(pa, |mut c| c.move_to_col(0)),
            event!(End) => handle.edit_all(pa, |mut c| c.move_to_col(usize::MAX)),

            event!(Esc) => {
                handle.text_mut(pa).new_moment();
                mode::set(pa, Normal::new());
            }
            alt!(';') => _ = mode::set(pa, Normal::only_one_action()),
            ctrl!('u') => handle.text_mut(pa).new_moment(),
            _ => {}
        }
    }

    fn on_switch(&mut self, pa: &mut Pass, handle: Handle) {
        INSERT_EVENTS.lock().unwrap().clear();
        Completions::open_default(pa);
        handle.set_mask("Insert");
    }

    fn before_exit(&mut self, pa: &mut Pass, _: Handle<Self::Widget>) {
        Completions::close(pa)
    }
}

/// How `<Tab>`s should be handled in [`Insert`] mode
///
/// These options concern `\t` insertion, reindentation, and command
/// completion.
#[derive(Clone, Copy, Debug)]
pub enum TabMode {
    /// Just insert the tab characters
    ///
    /// If the option `insert_tabs` is set to `true`, then this will
    /// insert a `\t` character. Otherwise, it will insert an
    /// equivalent amount of spaces.
    ///
    /// If `\t` is in `indent_chars`, then it will reindent the line.
    /// If nothing happens, it will insert as explained before.
    Normal,
    /// Reindent when necessary, insert character otherwise
    ///
    /// This will reindent the line if the caret is in the leading
    /// space. If nothing happens, then it will insert the characters
    /// as defined in [`TabMode::Normal`].
    ///
    /// If `\t` is in `indent_chars`, then it will reindent the line.
    /// If nothing happens, it will insert as explained before.
    Smart,
    /// Go to next autocompletion entry
    ///
    /// This essentially becomes a mapping to `<C-p>`. The difference
    /// is that, if `\t` is in `indent_chars`, then It will attempt to
    /// indent first, and only if nothing happens will it go to the
    /// next autocompletion entry.
    ///
    /// This is the default `TabMode`.
    VerySmart,
}

pub(crate) fn repeat_last_insert(pa: &mut Pass, handle: &Handle) {
    let insert_events = INSERT_EVENTS.lock().unwrap();

    for event in insert_events.iter() {
        match event {
            InsertEvent::MoveHor(hor) => handle.edit_all(pa, |mut c| _ = c.move_hor(*hor)),
            InsertEvent::MoveVer(ver) => handle.edit_all(pa, |mut c| c.move_ver_wrapped(*ver)),
            InsertEvent::Delete(del) => handle.edit_all(pa, |mut c| {
                c.set_anchor();
                c.move_hor(*del as i32 - 1);
                c.replace("");
            }),
            InsertEvent::Backspace(back) => handle.edit_all(pa, |mut c| {
                c.move_hor(-1);
                c.set_anchor();
                c.move_hor(-(*back as i32 - 1));
                c.replace("");
            }),
            InsertEvent::Insert(str, hor) => handle.edit_all(pa, |mut c| {
                c.insert(str);
                c.move_hor(*hor as i32);
            }),
            InsertEvent::Reindent => {
                let (mut indents, is_ts_indent) = crate::indents(pa, handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut c| _ = reindent(&mut c, indents.next().unwrap()))
                }
            }
        }
    }
}

static INSERT_EVENTS: Mutex<Vec<InsertEvent>> = Mutex::new(Vec::new());

#[derive(Clone)]
enum InsertEvent {
    MoveHor(i32),
    MoveVer(i32),
    Delete(usize),
    Backspace(usize),
    Insert(String, usize),
    Reindent,
}

/// removes an empty line
fn remove_empty_line(c: &mut Cursor) {
    let mut lines = c.lines_on(c.caret()..);
    let (_, line) = lines.next().unwrap();
    if !line.chars().all(char::is_whitespace) || line.is_empty() {
        return;
    }
    let chars_count = line.chars().count();

    let dvcol = c.v_caret().desired_visual_col();
    c.move_to_col(0);
    c.set_anchor();
    c.move_hor(chars_count as i32 - 1);

    c.replace("");
    c.unset_anchor();
    c.set_desired_vcol(dvcol);
}

fn complete(pa: &mut Pass, scroll: i32, insert_events: &mut Vec<InsertEvent>) {
    if let Some((before, after)) = Completions::scroll(pa, scroll) {
        let after_len = after.chars().count();
        match (before.chars().count(), insert_events.last_mut()) {
            (0, Some(InsertEvent::Insert(str, hor))) => {
                str.push_str(&after);
                *hor += after_len;
            }
            (0, None | Some(_)) => insert_events.push(InsertEvent::Insert(after, after_len)),
            (removed, Some(InsertEvent::Backspace(len))) => {
                *len += removed;
                insert_events.push(InsertEvent::Insert(after, after_len))
            }
            (removed, None | Some(_)) => insert_events.extend([
                InsertEvent::Backspace(removed),
                InsertEvent::Insert(after, after_len),
            ]),
        }
    }
}

fn insert_str(c: &mut Cursor, new: impl ToString, len: i32, insert_events: &mut Vec<InsertEvent>) {
    let new = new.to_string();
    c.insert(&new);
    c.move_hor(len);
    if c.is_main() {
        match insert_events.last_mut() {
            Some(InsertEvent::Insert(str, prev)) => {
                str.push_str(&new);
                *prev += len as usize;
            }
            Some(_) | None => insert_events.push(InsertEvent::Insert(new, len as usize)),
        }
    }
}

fn move_hor(c: &mut Cursor, hor: i32, insert_events: &mut Vec<InsertEvent>) {
    let hor = c.move_hor(hor);
    if hor != 0 && c.is_main() {
        match insert_events.last_mut() {
            Some(InsertEvent::MoveHor(total)) => {
                *total += hor;
                if *total == 0 {
                    insert_events.pop();
                }
            }
            Some(_) | None => insert_events.push(InsertEvent::MoveHor(hor)),
        }
    }
}

fn move_ver(c: &mut Cursor, ver: i32, insert_events: &mut Vec<InsertEvent>) {
    c.move_ver_wrapped(ver);
    if c.is_main() {
        match insert_events.last_mut() {
            Some(InsertEvent::MoveVer(total)) => {
                *total += ver;
                if *total == 0 {
                    insert_events.pop();
                }
            }
            Some(_) | None => insert_events.push(InsertEvent::MoveVer(ver)),
        }
    }
}

/// Reindents a [`Cursor`]'s line by a certain amount
pub fn reindent(c: &mut Cursor, new_indent: usize) -> bool {
    let old_indent = c.indent();
    let old_col = c.v_caret().char_col();
    let anchor_existed = c.anchor().is_some();

    let indent_diff = new_indent as i32 - old_indent as i32;

    c.move_hor(-(old_col as i32));
    c.set_anchor();
    c.move_hor(old_indent as i32);

    if c.caret() == c.anchor().unwrap() {
        c.insert(" ".repeat(new_indent));
    } else {
        c.move_hor(-1);
        c.replace(" ".repeat(new_indent));
    }
    c.set_caret_on_start();
    c.unset_anchor();

    if anchor_existed {
        c.set_anchor();
        if old_col < old_indent {
            c.move_hor(old_col as i32);
        } else {
            c.move_hor(old_col as i32 + indent_diff);
        }
        c.swap_ends();
    }

    if old_col < old_indent {
        c.move_hor(old_col as i32);
    } else {
        c.move_hor(old_col as i32 + indent_diff);
    }

    indent_diff != 0
}
