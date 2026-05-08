use std::{ops::Range, sync::LazyLock};

use duat_core::{
    Ns,
    buffer::{Moment, PerBuffer},
    context::Handle,
    data::Pass,
};

static SNIPPETS: PerBuffer<Snippets> = PerBuffer::new();
static NS: LazyLock<Ns> = Ns::new_lazy();

struct Snippets {
    list: Vec<Snippet>,
}

impl Snippets {
    fn apply_changes(&mut self, moment: Moment) {
        for snippet in self.list.iter_mut() {
            snippet.jumps.retain_mut(|jump| {
                let mut shift = 0;
                let mut ranges = jump.iter_mut().peekable();

                for change in moment.iter() {
                    let taken_range =
                        change.taken_range().start.byte()..change.taken_range().end.byte();

                    for range in (&mut ranges).take_while(|r| change.start().byte() > r.end) {
                        range.start = range.start.saturating_add_signed(shift);
                        range.end = range.end.saturating_add_signed(shift);
                    }
                    if let Some(range) = ranges.peek()
                        && (range.contains(&taken_range.start) || range.contains(&taken_range.end))
                    {
                        return false;
                    }

                    shift += change.shift()[0] as isize;
                }

                true
            });
        }
    }
}

struct Snippet {
    jumps: Vec<Vec<Range<usize>>>,
    current: usize,
}

pub(crate) fn add_snippet_jumps(buffer: &Handle, pa: &mut Pass, jumps: Vec<Vec<Range<usize>>>) {
    let current = (jumps.len() - 1).min(1);
    let snippet = Snippet { jumps, current };

    let mut text = buffer.text_mut(pa);

    for range in snippet.jumps.iter().flatten() {}

    if let Some((snippets, buf)) = SNIPPETS.write(pa, buffer) {
        snippets.apply_changes(buf.moment_for(*NS));
        snippets.list.push(snippet)
    } else {
        SNIPPETS.register(pa, buffer, Snippets { list: vec![snippet] });
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

            idx..end_idx
        } else {
            replace(&mut snippet, " ", &mut jumps, range.start, idx..idx);
            idx..idx + 1
        };

        jumps.resize(jumps.len().max(number + 1), Vec::new());
        jumps[number].push(range);

        start_idx = idx;
    }

    buffer.text_mut(pa).replace_range(range, snippet);

    add_snippet_jumps(buffer, pa, jumps);
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
