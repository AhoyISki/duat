use std::sync::{Mutex, atomic::Ordering};

use duat_base::widgets::Completions;
use duat_core::{
    Ns,
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    hook::{self, BufferSwitched, ModeSwitched},
    mode::{self, KeyEvent, KeyMod, Mode, SelectionMut, alt, ctrl, event, shift},
    text::Mask,
};
use duat_filetype::AutoPrefix;

use crate::{Normal, opts::INSERT_TABS, set_anchor_if_needed};

pub fn add_insert_hook() {
    let mask_ns = Ns::new();

    hook::add::<ModeSwitched>(move |pa, switch| {
        let buffer = context::current_buffer(pa);
        buffer.text_parts(pa).tags.remove(mask_ns, ..);

        if switch.new.is::<Insert>() {
            INSERT_EVENTS.lock().unwrap().clear();
            Completions::open_default(pa);

            let mask = Mask("Insert");
            buffer.text_parts(pa).tags.insert(mask_ns, .., mask);
        } else if switch.old.is::<Insert>() {
            Completions::close(pa);
        }

        if switch.new.is::<Normal>() {
            let mask = Mask("Normal");
            buffer.text_parts(pa).tags.insert(mask_ns, .., mask);
        }
    });

    hook::add::<BufferSwitched>(move |pa, (old_buffer, _)| {
        old_buffer.text_parts(pa).tags.remove(mask_ns, ..)
    });
}

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
            handle.edit_all(pa, |mut s| {
                if s.anchor().is_none() {
                    s.set_anchor()
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
                        handle.edit_all(pa, |mut s| {
                            let indent = indents.next().unwrap();
                            if opts.indent_chars.contains(&'\t')
                                && is_ts_indent
                                && reindent(s.indent(), indent, &mut s)
                            {
                                return;
                            }
                            if INSERT_TABS.load(Ordering::Relaxed) {
                                insert_str(&mut s, '\t', 1, &mut insert_events);
                            } else {
                                let tab_len =
                                    s.opts().tabstop_spaces_at(s.v_cursor().visual_col() as u32);
                                let tab = " ".repeat(tab_len as usize);
                                insert_str(&mut s, tab, tab_len as i32, &mut insert_events);
                            }
                        });
                    }
                    TabMode::Smart => handle.edit_all(pa, |mut s| {
                        let char_col = s.v_cursor().char_col();
                        let indent = indents.next().unwrap();
                        if (opts.indent_chars.contains(&'\t') || char_col <= s.indent())
                            && is_ts_indent
                            && reindent(s.indent(), indent, &mut s)
                        {
                            return;
                        }

                        if INSERT_TABS.load(Ordering::Relaxed) {
                            insert_str(&mut s, '\t', 1, &mut insert_events);
                        } else {
                            let tab_len =
                                s.opts().tabstop_spaces_at(s.v_cursor().visual_col() as u32);
                            let tab = " ".repeat(tab_len as usize);
                            insert_str(&mut s, tab, tab_len as i32, &mut insert_events);
                        }
                    }),
                    TabMode::VerySmart => {
                        let mut do_scroll = true;
                        handle.edit_all(pa, |mut s| {
                            let char_col = s.v_cursor().char_col();
                            let indent = indents.next().unwrap();
                            let has_reindented = (opts.indent_chars.contains(&'\t')
                                || char_col <= s.indent())
                                && is_ts_indent
                                && reindent(s.indent(), indent, &mut s);

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
                handle.edit_all(pa, |mut s| {
                    insert_str(&mut s, char, 1, &mut insert_events);
                });
                if opts.indent_chars.contains(&char) {
                    let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                    if is_ts_indent {
                        handle.edit_all(pa, |mut s| {
                            let indent = indents.next().unwrap();
                            if opts.indent_chars.contains(&char)
                                && s.indent() == s.v_cursor().char_col() - 1
                            {
                                reindent(s.indent(), indent, &mut s);
                            }
                        })
                    }
                }
            }

            event!(Enter) => {
                handle.edit_all(pa, |mut s| {
                    let (cursor, anchor) = (s.cursor(), s.anchor());
                    remove_trailing_before_cursor(&mut s);
                    if let Some(anchor) = anchor {
                        s.set_anchor();
                        if anchor < cursor {
                            s.move_to(anchor);
                        } else {
                            s.move_hor(anchor.byte() as i32 - cursor.byte() as i32);
                        }
                        s.swap_ends();
                    }

                    insert_str(&mut s, '\n', 1, &mut insert_events)
                });
                if opts.indent_chars.contains(&'\n') {
                    handle.edit_all(pa, |mut s| {
                        if s.add_comment() {
                            s.insert(' ');
                            s.move_hor(1);
                        }
                    });
                    let (mut indents, is_ts_indent) = crate::indents(pa, &handle);
                    handle.edit_all(pa, |mut s| {
                        if is_ts_indent || opts.auto_indent {
                            _ = reindent(0, indents.next().unwrap(), &mut s)
                        }
                    });
                }
            }
            event!(Backspace) => handle.edit_all(pa, |mut s| {
                let prev_cursor = s.cursor();
                let prev_anchor = s.unset_anchor();

                if s.move_hor(-1) != 0 {
                    s.set_anchor();
                    s.replace("");
                    s.unset_anchor();

                    match insert_events.last_mut() {
                        Some(InsertEvent::Backspace(total)) => *total += 1,
                        Some(_) | None => insert_events.push(InsertEvent::Backspace(1)),
                    }

                    if let Some(prev_anchor) = prev_anchor {
                        s.set_anchor();
                        if prev_anchor > prev_cursor {
                            s.move_hor((prev_anchor.char() - prev_cursor.char()) as i32);
                        } else {
                            s.move_to(prev_anchor);
                        }
                        s.swap_ends();
                    }
                }
            }),
            event!(Delete) => handle.edit_all(pa, |mut s| {
                let prev_cursor = s.cursor();
                let prev_anchor = s.unset_anchor();
                s.set_anchor();
                s.replace("");
                s.unset_anchor();

                match insert_events.last_mut() {
                    Some(InsertEvent::Delete(total)) => *total += 1,
                    Some(_) | None => insert_events.push(InsertEvent::Delete(1)),
                }

                if let Some(prev_anchor) = prev_anchor {
                    s.set_anchor();
                    if prev_anchor > prev_cursor {
                        s.move_hor((prev_anchor.char() - prev_cursor.char()) as i32 - 1);
                    } else {
                        s.move_to(prev_anchor);
                    }
                    s.swap_ends();
                }
            }),
            event!(Left) | shift!(Left) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut s);
                move_hor(&mut s, -1, &mut insert_events);
            }),
            event!(Down) | shift!(Down) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut s);
                if key_event.modifiers == KeyMod::NONE {
                    s.unset_anchor();
                    remove_trailing_before_cursor(&mut s);
                }
                move_ver(&mut s, 1, &mut insert_events);
            }),
            event!(Up) | shift!(Up) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut s);
                if key_event.modifiers == KeyMod::NONE {
                    s.unset_anchor();
                    remove_trailing_before_cursor(&mut s);
                }
                move_ver(&mut s, -1, &mut insert_events);
            }),
            event!(Right) | shift!(Right) => handle.edit_all(pa, |mut s| {
                set_anchor_if_needed(key_event.modifiers == KeyMod::SHIFT, &mut s);
                move_hor(&mut s, 1, &mut insert_events);
            }),

            event!(Home) => handle.edit_all(pa, |mut s| s.move_to_col(0)),
            event!(End) => handle.edit_all(pa, |mut s| s.move_to_col(usize::MAX)),

            event!(Esc) => {
                handle.text_mut(pa).new_moment();
                mode::set(pa, Normal::new());
            }
            alt!(';') => mode::set(pa, Normal::only_one_action()),
            ctrl!('u') => handle.text_mut(pa).new_moment(),
            _ => {}
        }
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
    /// This will reindent the line if the cursor is in the leading
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
            InsertEvent::MoveHor(hor) => handle.edit_all(pa, |mut s| _ = s.move_hor(*hor)),
            InsertEvent::MoveVer(ver) => handle.edit_all(pa, |mut s| _ = s.move_ver_wrapped(*ver)),
            InsertEvent::Delete(del) => handle.edit_all(pa, |mut s| {
                s.set_anchor();
                s.move_hor(*del as i32 - 1);
                s.replace("");
            }),
            InsertEvent::Backspace(back) => handle.edit_all(pa, |mut s| {
                s.move_hor(-1);
                s.set_anchor();
                s.move_hor(-(*back as i32 - 1));
                s.replace("");
            }),
            InsertEvent::Insert(str, hor) => handle.edit_all(pa, |mut s| {
                s.insert(str);
                s.move_hor(*hor as i32);
            }),
            InsertEvent::Reindent => {
                let (mut indents, is_ts_indent) = crate::indents(pa, handle);
                if is_ts_indent {
                    handle.edit_all(pa, |mut s| {
                        reindent(s.indent(), indents.next().unwrap(), &mut s);
                    })
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

/// Removes an empty line, expects a movent down or up after.
fn remove_trailing_before_cursor(s: &mut SelectionMut) {
    let Some(line) = s.text()[..s.cursor()].lines().next_back() else {
        return;
    };
    let chars_count = line.chars().rev().take_while(|char| *char == ' ').count();

    if chars_count == 0 {
        return;
    }

    let dvcol = s.v_cursor().desired_visual_col();
    let col = s.v_cursor().char_col();

    s.move_to_col(col - chars_count);
    s.set_anchor();
    s.move_hor(chars_count as i32 - 1);

    s.replace("");
    s.unset_anchor();
    s.set_desired_vcol(dvcol);
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

fn insert_str(
    s: &mut SelectionMut,
    new: impl ToString,
    len: i32,
    insert_events: &mut Vec<InsertEvent>,
) {
    let new = new.to_string();
    s.insert(&new);
    s.move_hor(len);
    if s.is_main() {
        match insert_events.last_mut() {
            Some(InsertEvent::Insert(str, prev)) => {
                str.push_str(&new);
                *prev += len as usize;
            }
            Some(_) | None => insert_events.push(InsertEvent::Insert(new, len as usize)),
        }
    }
}

fn move_hor(s: &mut SelectionMut, hor: i32, insert_events: &mut Vec<InsertEvent>) {
    if s.move_hor(hor) != 0 && s.is_main() {
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

fn move_ver(s: &mut SelectionMut, ver: i32, insert_events: &mut Vec<InsertEvent>) {
    s.move_ver_wrapped(ver);
    if s.is_main() {
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

/// Reindents a [`SelectionMut`]'s line by a certain amount
pub fn reindent(old_indent: usize, new_indent: usize, s: &mut SelectionMut) -> bool {
    let old_col = s.v_cursor().char_col();
    let (cursor, anchor) = (s.cursor(), s.anchor());

    let indent_diff = new_indent as i32 - old_indent as i32;

    s.move_hor(-(old_col as i32));
    s.set_anchor();
    s.move_hor(old_indent as i32);

    if s.cursor() == s.anchor().unwrap() {
        s.insert(" ".repeat(new_indent));
    } else {
        s.move_hor(-1);
        s.replace(" ".repeat(new_indent));
    }
    s.set_cursor_on_start();
    s.unset_anchor();

    if old_col < old_indent {
        s.move_hor(old_col as i32);
    } else {
        s.move_hor(old_col as i32 + indent_diff);
    }

    if let Some(anchor) = anchor {
        s.set_anchor();
        if anchor < cursor {
            s.move_to(anchor);
        } else {
            s.move_hor(anchor.byte() as i32 - cursor.byte() as i32);
        }
        s.swap_ends();
    }

    indent_diff != 0
}
