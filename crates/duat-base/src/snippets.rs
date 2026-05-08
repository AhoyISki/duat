//! Snippet support for Duat.
//!
//! This module provides functions and structures for enabling
//! the use of snippets within Duat. Snippets are just pieces of
//! text that can be quickly jumped on via single keypresses. They
//! are one of the most convenient ways of typing out things like
//! function calls, struct definitions, etc.
use std::{ops::Range, sync::LazyLock};

use duat_core::{
    Ns,
    buffer::{Moment, PerBuffer},
    context::Handle,
    data::Pass,
    form,
    hook::{self, BufferOpened, BufferUpdated},
};

static SNIPPETS: PerBuffer<Snippets> = PerBuffer::new();
static NS: LazyLock<Ns> = Ns::new_lazy();

pub(crate) fn add_snippet_hook() {
    hook::add::<BufferOpened>(|pa, buffer| {
        SNIPPETS.register(
            pa,
            buffer,
            Snippets {
                list: Vec::new(),
                cur_snippet: 0,
                cur_jump: 0,
            },
        );
    });

    hook::add::<BufferUpdated>(|pa, buffer| {
        if let Some((snippets, buf)) = SNIPPETS.write(pa, buffer) {
            snippets.apply_changes(buf.moment_for(*NS));
        }
    })
    .lateness(usize::MAX);
}

struct Snippets {
    list: Vec<Vec<Vec<Range<usize>>>>,
    cur_snippet: usize,
    cur_jump: usize,
}

impl Snippets {
    fn apply_changes(&mut self, moment: Moment) {
        if moment.is_empty() {
            return;
        }

        let mut snippet_idx = 0;
        self.list.retain_mut(|snippet| {
            let mut jump_idx = 0;

            snippet.retain_mut(|jump| {
                let mut shift = 0;
                let mut ranges = jump.iter_mut().peekable();

                for change in moment.iter() {
                    let taken_range =
                        change.taken_range().start.byte()..change.taken_range().end.byte();

                    while let Some(range) = ranges.next_if(|r| taken_range.start > r.end) {
                        range.start = range.start.saturating_add_signed(shift);
                        range.end = range.end.saturating_add_signed(shift);
                    }

                    if let Some(range) = ranges.peek()
                        && (taken_range.contains(&range.start) || taken_range.contains(&range.end))
                    {
                        if self.cur_snippet == snippet_idx && self.cur_jump >= jump_idx {
                            self.cur_jump = self.cur_jump.saturating_sub(1);
                        }
                        return false;
                    }

                    shift += change.shift()[0] as isize;
                }

                for range in ranges {
                    range.start = range.start.saturating_add_signed(shift);
                    range.end = range.end.saturating_add_signed(shift);
                }

                jump_idx += 1;
                true
            });

            if snippet.is_empty() {
                if self.cur_snippet >= snippet_idx {
                    self.cur_snippet = self.cur_snippet.saturating_sub(1);
                }
                false
            } else {
                snippet_idx += 1;
                true
            }
        });
    }
}

pub(crate) fn add_snippet_jumps(buffer: &Handle, pa: &mut Pass, snippet: Vec<Vec<Range<usize>>>) {
    let (snippets, buf) = if let Some((snippets, buf)) = SNIPPETS.write(pa, buffer) {
        snippets.apply_changes(buf.moment_for(*NS));
        snippets.list.push(snippet);
        snippets.cur_jump = 0;
        snippets.cur_snippet = snippets.list.len() - 1;
        (snippets, buf)
    } else {
        buffer.write(pa).moment_for(*NS);
        SNIPPETS.register(
            pa,
            buffer,
            Snippets {
                list: vec![snippet],
                cur_snippet: 0,
                cur_jump: 0,
            },
        )
    };

    let snippet_form = form::id_of!("snippet").to_tag(150);
    let ns = *NS;

    let mut text = buf.text_mut();
    text.remove_tags(ns, ..);

    for range in snippets.list.iter().flatten().flatten() {
        text.insert_tag(ns, range.clone(), snippet_form)
    }
}

/// Replace a range with a snippet.
///
/// Use `$num` or `${num}` for a jump and `${num:placeholder}` for a
/// jump with a placeholder.
#[track_caller]
pub(crate) fn replace_with_snippet(
    buffer: &Handle,
    pa: &mut Pass,
    range: Range<usize>,
    mut snippet: String,
) {
    let mut jumps = Vec::new();
    let mut start_idx = 0;
    let mut zeroth_jump: Option<Vec<_>> = None;

    while let Some((idx, _)) = find_unesc(&snippet, start_idx, ['$']) {
        let mut chars = snippet[idx + 1..].chars();
        let (is_placeholder, number) = {
            let num_start = match chars.next() {
                Some('{') if let Some('0'..='9') = chars.next() => idx + 2,
                Some('{') => panic!("Invalid snippet syntax: expected a number"),
                Some('0'..='9') => idx + 1,
                Some(char) => {
                    panic!("Invalid snippet syntax: expected '{{' or a number, found {char}")
                }
                None => panic!("Invalid snippet syntax: can't finish with a '$'"),
            };

            let num_len = snippet[num_start..]
                .chars()
                .take_while(|c| c.is_ascii_digit())
                .count();

            let number: usize = snippet[num_start..num_start + num_len].parse().unwrap();

            replace(
                &mut snippet,
                "",
                &mut jumps,
                range.start,
                idx..num_start + num_len,
            );
            (num_start == idx + 2, number)
        };

        let range = if is_placeholder {
            if !snippet[idx..].starts_with(':') {
                panic!("Invalid snippet syntax: expected a ':' after the number");
            }
            replace(&mut snippet, "", &mut jumps, range.start, idx..idx + 1);

            let mut start_count = 1;
            let mut end_idx = None;

            while let Some((idx, str)) = find_unesc(&snippet, idx, ['{', '}']) {
                if str == "{" {
                    start_count += 1;
                } else {
                    start_count -= 1;
                    if start_count == 0 {
                        end_idx = Some(idx);
                        break;
                    }
                }
            }

            let Some(end_idx) = end_idx else {
                panic!("Invalid snippet syntax: unclosed placeholder at byte {idx}");
            };
            replace(
                &mut snippet,
                "",
                &mut jumps,
                range.start,
                end_idx..end_idx + 1,
            );

            range.start + idx..range.start + end_idx
        } else {
            replace(&mut snippet, " ", &mut jumps, range.start, idx..idx);
            range.start + idx..range.start + idx + 1
        };

        if number == 0 {
            zeroth_jump.get_or_insert_default().push(range);
        } else {
            jumps.resize(jumps.len().max(number), Vec::new());
            jumps[number - 1].push(range);
        }

        start_idx = idx;
    }

    if let Some(jump) = zeroth_jump {
        jumps.push(jump)
    } else {
        let end_range = snippet.len()..snippet.len();
        replace(&mut snippet, " ", &mut jumps, range.start, end_range);
        jumps.push(vec![
            range.start + snippet.len()..range.start + snippet.len() + 1,
        ]);
    }

    buffer.text_mut(pa).replace_range(range, snippet);

    duat_core::debug!("{jumps:#?}");

    add_snippet_jumps(buffer, pa, jumps);
}

/// Jumps on the snippets by a given amount.
///
/// Returns `true` if anything happened.
pub(crate) fn jump_snippets(buffer: &Handle, pa: &mut Pass, mut by: i32) -> bool {
    if let Some((snippets, _)) = SNIPPETS.write(pa, buffer)
        && !snippets.list.is_empty()
    {
        snippets.cur_snippet = snippets.cur_snippet.min(snippets.list.len() - 1);

        snippets.cur_jump = snippets
            .cur_jump
            .min(snippets.list[snippets.cur_snippet].len() - 1);

        while by != 0 {
            let snippet = &snippets.list[snippets.cur_snippet];
            let (current, len) = (snippets.cur_jump as i32, snippet.len() as i32);

            if current + by >= len {
                snippets.cur_jump = 0;

                snippets.cur_snippet = if snippets.cur_snippet == snippets.list.len() - 1 {
                    0
                } else {
                    snippets.cur_snippet + 1
                };

                by -= len - current;
            } else if current + by < 0 {
                snippets.cur_snippet = if snippets.cur_snippet == 0 {
                    snippets.list.len() - 1
                } else {
                    snippets.cur_snippet - 1
                };

                snippets.cur_jump = snippets.list[snippets.cur_snippet].len();

                by += current;
            } else {
                snippets.cur_jump = (current + by) as usize;
                by = 0;
            }
        }

        let mut ranges = snippets.list[snippets.cur_snippet][snippets.cur_jump]
            .clone()
            .into_iter();

        buffer.selections_mut(pa).remove_extras();
        buffer.edit_main(pa, |mut s| {
            if let Some(range) = ranges.next() {
                s.move_to(range);
                for range in ranges {
                    let mut s = s.copy();
                    s.move_to(range);
                }
            }
        });

        true
    } else {
        false
    }
}

fn find_unesc<const N: usize>(
    haystack: &str,
    start: usize,
    chars: [char; N],
) -> Option<(usize, &str)> {
    if let Some(idx) = haystack[start..].find(chars)
        && !haystack[..idx].ends_with('\\')
    {
        Some((start + idx, &haystack[start + idx..start + idx + 1]))
    } else {
        None
    }
}

fn replace(
    snippet: &mut String,
    rep: &str,
    jumps: &mut [Vec<Range<usize>>],
    start: usize,
    range: Range<usize>,
) {
    snippet.replace_range(range.clone(), rep);

    for jump in jumps.iter_mut().flatten() {
        for bound in [&mut jump.start, &mut jump.end] {
            if *bound > start + range.start {
                // If the snippet is well-formed, no partial clipping should happen.
                *bound = (*bound + rep.len()).saturating_sub(range.len())
            }
        }
    }
}
