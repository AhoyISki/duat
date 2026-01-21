use duat_core::{
    opts::PrintOpts,
    text::{Item, Part, Text, TwoPoints},
    ui::Caret,
};
use unicode_width::UnicodeWidthChar;

/// An [`Iterator`] that returns both an [`Item`] and a [`Caret`].
///
/// This function expects that `cap` has been validated, and that the
/// iterator starts in the visual start of the line.
pub fn print_iter(
    text: &Text,
    points: TwoPoints,
    width: u32,
    opts: PrintOpts,
) -> impl Iterator<Item = (Caret, Item)> + '_ {
    let start_points = text.visual_line_start(points, 0);
    let max_indent = if opts.indent_wraps { width } else { 0 };
    let cap = opts.wrap_width(width).unwrap_or(width);

    // Line construction variables.
    let (mut x, mut spacers) = (0, 0);
    let (mut indent, mut on_indent, mut wrapped_indent) = (0, true, 0);
    let mut replace_chars = Vec::with_capacity(opts.tabstop as usize);
    let mut tab_chars = Vec::new();

    if start_points != points {
        let (indent, on_indent, replace_chars) = (&mut indent, &mut on_indent, &mut replace_chars);
        let mut iter = text
            .iter_fwd(start_points)
            .take_while(|item| item.points() < points);

        while let Some(item) = (!tab_chars.is_empty())
            .then(|| tab_chars.remove(0))
            .or_else(|| iter.next())
        {
            let old_indent = *indent;
            let (part, len) = match item.part {
                Part::Char('\t') if !replace_chars.is_empty() => {
                    tab_chars = process_tab(indent, on_indent, x, opts, item, replace_chars);
                    continue;
                }
                Part::Char(char) => match replace_chars.drain(..).next().unwrap_or(char) {
                    '\n' => unreachable!("Shouldn't be possible, given the visual line start"),
                    char => (
                        Part::Char(char),
                        process_char(indent, on_indent, x, char, opts),
                    ),
                },
                _ => (item.part, 0),
            };

            spacers += matches!(item.part, Part::Spacer) as usize;

            if let Part::ReplaceChar(char) = item.part {
                replace_chars.push(char);
            }

            x += len;
            if x > cap && opts.wrap_lines {
                spacers = 0;

                if let Part::Char('\t') = part {
                    let desired = old_indent + x - cap;
                    wrapped_indent = if desired < max_indent {
                        desired
                    } else {
                        x - cap
                    };
                    x = wrapped_indent;
                } else {
                    wrapped_indent = old_indent * (old_indent < max_indent) as u32;
                    x = wrapped_indent + len;
                }
            }
        }
    }

    inner_iter(
        text.iter_fwd(points),
        (x, spacers),
        (indent, on_indent, wrapped_indent),
        (cap, opts),
        (replace_chars, tab_chars),
    )
}

pub fn rev_print_iter(
    text: &Text,
    points: TwoPoints,
    cap: u32,
    opts: PrintOpts,
) -> impl Iterator<Item = (Caret, Item)> + '_ {
    let mut iter = text.iter_rev(points);

    let mut returns = Vec::new();
    let mut items = Vec::new();

    // Used exclusively for the ReplaceChar tag
    let mut parts_in_process = Vec::new();
    let mut char_in_process = None;

    std::iter::from_fn(move || {
        if let Some(next) = returns.pop() {
            Some(next)
        } else {
            let iter = loop {
                if let Some(item) = iter.next() {
                    match item.part {
                        Part::Char(char) => match char_in_process.replace(char) {
                            Some('\n') => {
                                let len = items.len();
                                items.append(&mut parts_in_process);
                                if len > 0 {
                                    parts_in_process.push(item);
                                    break items.drain(..len).rev();
                                }
                            }
                            _ => items.append(&mut parts_in_process),
                        },
                        Part::ReplaceChar(char) => {
                            char_in_process = char_in_process.and(Some(char));
                        }
                        _ => {}
                    }

                    parts_in_process.push(item);
                } else {
                    items.append(&mut parts_in_process);
                    break items.drain(..).rev();
                }
            };

            let vecs = (Vec::new(), Vec::new());
            returns.extend(inner_iter(iter, (0, 0), (0, true, 0), (cap, opts), vecs));

            returns.pop()
        }
    })
}

#[inline(always)]
fn inner_iter<'a>(
    iter: impl Iterator<Item = Item> + 'a,
    (mut x, mut spacers): (u32, usize),
    (mut indent, mut on_indent, mut wrapped_indent): (u32, bool, u32),
    (cap, opts): (u32, PrintOpts),
    (mut replace_chars, mut tab_chars): (Vec<char>, Vec<Item>),
) -> impl Iterator<Item = (Caret, Item)> + 'a {
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line return variables.
    let (mut line, mut leftover_nl): (Vec<(u32, Item)>, _) = (Vec::new(), None);
    let (mut printed_x, mut i, mut has_wrapped) = (0, 0, false);

    let mut iter = iter.peekable();

    let mut initial_printed_x = 0;
    let mut first_printed_x = x * (x < cap || !opts.wrap_lines) as u32;

    std::iter::from_fn(move || {
        let (indent, on_indent, replace_chars) = (&mut indent, &mut on_indent, &mut replace_chars);
        loop {
            // Emptying the line, most next calls should come here.
            if let Some(&(len, item)) = line.get(i) {
                let wrap = !has_wrapped && (len > 0 || item.part.is_char());
                if wrap {
                    printed_x = initial_printed_x;
                }
                let caret = Caret { x: printed_x, len, wrap };
                i += 1;
                printed_x += len;
                has_wrapped |= wrap;
                break Some((caret, item));
            }

            line.clear();
            i = 0;
            has_wrapped = false;

            // Emptying a leftover '\n', which may come after the end of a line.
            if let Some((x, item)) = leftover_nl.take() {
                break Some((Caret { x, len: 1, wrap: true }, item));
            }

            // Preparing the line itself.
            loop {
                let Some(item) = (!tab_chars.is_empty())
                    .then(|| tab_chars[0])
                    .or_else(|| iter.peek().cloned())
                else {
                    if line.is_empty() {
                        return None;
                    } else {
                        break;
                    }
                };

                let mut remove_next = || {
                    if tab_chars.is_empty() {
                        iter.next();
                    } else {
                        tab_chars.remove(0);
                    }
                };

                let old_indent = *indent;
                let (item, len) = match item.part {
                    Part::Char('\t') if !replace_chars.is_empty() => {
                        remove_next();
                        tab_chars = process_tab(indent, on_indent, x, opts, item, replace_chars);
                        continue;
                    }
                    Part::Char(char) => {
                        let char = replace_chars.first().copied().unwrap_or(char);
                        (
                            Item { part: Part::Char(char), ..item },
                            match char {
                                '\n' if item.ghost.is_some() => {
                                    *indent = 0;
                                    *on_indent = true;
                                    0
                                }
                                '\n' => process_nl(indent, on_indent, x, opts),
                                char => process_char(indent, on_indent, x, char, opts),
                            },
                        )
                    }
                    Part::ReplaceChar(char) => {
                        remove_next();
                        replace_chars.push(char);
                        continue;
                    }
                    Part::Spacer => {
                        spacers += 1;
                        (item, 0)
                    }
                    _ => (item, 0),
                };

                x += len;

                let must_wrap = x > cap && opts.wrap_lines;

                if let Part::Char(char) = item.part
                    && (must_wrap || char == '\n')
                {
                    let is_replacement = !replace_chars.is_empty();
                    replace_chars.clear();

                    initial_printed_x = first_printed_x + wrapped_indent;
                    space_line(spacers, &mut line, cap, x);
                    spacers = 0;

                    let leftover = x.saturating_sub(cap);
                    wrapped_indent = match char {
                        '\t' if leftover > 0 => {
                            remove_next();
                            line.push((len - leftover, item));
                            if old_indent + leftover < max_indent && opts.indent_wraps {
                                old_indent + leftover
                            } else {
                                leftover
                            }
                        }
                        '\n' => {
                            remove_next();
                            if len > 0 && must_wrap {
                                let position = old_indent
                                    * (old_indent < max_indent && opts.indent_wraps) as u32;
                                leftover_nl = Some((position, item))
                            } else {
                                line.push((len, item))
                            }
                            0
                        }
                        _ => {
                            // At this point, the only logical course of action is to wrap on
                            // every character.
                            if cap == 0 && !line.iter().any(|(_, item)| item.part.is_char()) {
                                remove_next();
                                line.push((len, item));
                            } else if is_replacement {
                                remove_next();
                            }

                            old_indent * (old_indent < max_indent && opts.indent_wraps) as u32
                        }
                    };

                    x = wrapped_indent;
                    break;
                } else {
                    if item.part.is_char() {
                        replace_chars.clear();
                    }
                    remove_next();
                    line.push((len, item));
                }
            }

            first_printed_x = 0;
        }
    })
}

/// Wether the [`TwoPoints`] actually does start a wrapped line.
pub fn is_starting_points(text: &Text, points: TwoPoints, width: u32, opts: PrintOpts) -> bool {
    let start_points = text.visual_line_start(points, 0);
    let max_indent = if opts.indent_wraps { width } else { 0 };
    let cap = opts.wrap_width(width).unwrap_or(width);

    // Line construction variables.
    let mut x = 0;
    let (mut indent, mut on_indent) = (0, true);
    let (mut replace_chars, mut tab_chars) = (Vec::new(), Vec::new());

    let (indent, on_indent, replace_chars) = (&mut indent, &mut on_indent, &mut replace_chars);

    if start_points == points {
        true
    } else if !opts.wrap_lines {
        false
    } else {
        let mut wrapped = true;
        let mut iter = text
            .iter_fwd(start_points)
            .take_while(|item| item.points() <= points);

        while let Some(item) = (!tab_chars.is_empty())
            .then(|| tab_chars.remove(0))
            .or_else(|| iter.next())
        {
            wrapped = false;

            let old_indent = *indent;
            let (part, len) = match item.part {
                Part::Char('\t') if !replace_chars.is_empty() => {
                    tab_chars = process_tab(indent, on_indent, x, opts, item, replace_chars);
                    continue;
                }
                Part::Char(char) => {
                    let char = replace_chars.drain(..).next().unwrap_or(char);
                    (
                        Part::Char(char),
                        match char {
                            '\n' if item.ghost.is_some() => {
                                *indent = 0;
                                *on_indent = true;
                                0
                            }
                            '\n' => process_nl(indent, on_indent, x, opts),
                            char => process_char(indent, on_indent, x, char, opts),
                        },
                    )
                }
                _ => (item.part, 0),
            };

            x += len;
            if x > cap && opts.wrap_lines {
                wrapped = true;

                x = if let Part::Char('\t') = part {
                    let desired = old_indent + x - cap;
                    if desired < max_indent {
                        desired
                    } else {
                        x - cap
                    }
                } else {
                    old_indent * (old_indent < max_indent) as u32 + len
                };
            }
        }

        wrapped
    }
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
#[inline(always)]
fn _words<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    (mut total_len, mut spacers): (u32, usize),
    (mut indent, mut on_indent, mut wrapped_indent): (u32, bool, u32),
    (cap, opts): (u32, PrintOpts),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line return variables.
    let (mut line, mut leftover_nl): (Vec<(u32, Item)>, _) = (Vec::new(), None);
    let (mut x, mut i, mut first_char_was_printed) = (0, 0, false);

    // Line construction variables
    let mut iter = iter.peekable();
    let mut word = Vec::new();
    let (mut new_x, mut first_x) = (0, total_len * (total_len < cap) as u32);

    std::iter::from_fn(move || {
        loop {
            if let Some(&(len, item)) = line.get(i) {
                let wrap = !first_char_was_printed && item.part.is_char();
                if wrap {
                    x = new_x;
                }
                let caret = Caret { x, len, wrap };
                i += 1;
                x += len;
                first_char_was_printed |= item.part.is_char();
                break Some((caret, item));
            }

            line.clear();
            i = 0;
            first_char_was_printed = false;

            if let Some((x, item)) = leftover_nl.take() {
                break Some((Caret { x, len: 1, wrap: true }, item));
            }

            loop {
                let Some(&item) = iter.peek() else {
                    if line.is_empty() {
                        return None;
                    } else {
                        break;
                    }
                };

                let old_indent = indent;
                let len = match item.part {
                    Part::Char('\n') => process_nl(&mut indent, &mut on_indent, total_len, opts),
                    Part::Char(char) => {
                        process_char(&mut indent, &mut on_indent, total_len, char, opts)
                    }
                    _ => 0,
                };

                spacers += matches!(item.part, Part::Spacer) as usize;
                total_len += len;

                if let Part::Char(char) = item.part
                    && ((total_len > cap && opts.wrap_lines) || char == '\n')
                {
                    new_x = first_x + wrapped_indent;
                    space_line(spacers, &mut line, cap, total_len);
                    spacers = 0;

                    let leftover = total_len.saturating_sub(cap);
                    wrapped_indent = match char {
                        '\t' if leftover > 0 => {
                            line.push((len - leftover, item));
                            iter.next();
                            if old_indent + leftover < max_indent && opts.indent_wraps {
                                old_indent + leftover
                            } else {
                                leftover
                            }
                        }
                        '\n' => {
                            match (opts.print_new_line, total_len > cap) {
                                (true, true) => {
                                    let position = old_indent
                                        * (old_indent < max_indent && opts.indent_wraps) as u32;
                                    leftover_nl = Some((position, iter.next().unwrap()))
                                }
                                (true, false) => line.push((1, iter.next().unwrap())),
                                (false, _) => line.push((0, iter.next().unwrap())),
                            }
                            0
                        }
                        _ => old_indent * (old_indent < max_indent && opts.indent_wraps) as u32,
                    };

                    total_len = wrapped_indent;
                    break;
                } else {
                    word.push((len, item));
                }

                iter.next();
            }

            first_x = 0;
        }
    })
}

#[inline(always)]
fn len_from(char: char, start: u32, opts: &PrintOpts) -> u32 {
    match char {
        '\t' => (opts.tabstop_spaces_at(start)).max(1),
        '\n' => 0,
        '\0'..='\u{1f}' => 2,
        '\u{80}'..='\u{9f}' => 4,
        char => UnicodeWidthChar::width(char).unwrap_or(0) as u32,
    }
}

#[inline(always)]
fn process_char(
    indent: &mut u32,
    on_indent: &mut bool,
    x: u32,
    char: char,
    opts: PrintOpts,
) -> u32 {
    let len = len_from(char, x, &opts);
    *on_indent &= char == ' ' || char == '\t';
    *indent += len * *on_indent as u32;
    len
}

#[inline(always)]
fn process_nl(indent: &mut u32, on_indent: &mut bool, x: u32, opts: PrintOpts) -> u32 {
    *indent = 0;
    *on_indent = true;
    let char = if opts.print_new_line { ' ' } else { '\n' };
    len_from(char, x, &opts)
}

fn process_tab(
    indent: &mut u32,
    on_indent: &mut bool,
    x: u32,
    opts: PrintOpts,
    item: Item,
    replace_chars: &mut Vec<char>,
) -> Vec<Item> {
    let mut new_tab_chars = Vec::new();
    let mut len = process_char(indent, on_indent, x, '\t', opts);

    while !replace_chars.is_empty() && len > 0 {
        let char = replace_chars.remove(0);
        let char_len = process_char(indent, on_indent, x, char, opts);
        len = len.saturating_sub(char_len);

        new_tab_chars.push(Item { part: Part::Char(char), ..item })
    }

    replace_chars.clear();

    new_tab_chars.extend((0..len).map(|_| Item { part: Part::Char(' '), ..item }));
    new_tab_chars
}

/// Adds spacing to a line, and returns the initial amount of
/// space needed
fn space_line(spacers: usize, line: &mut [(u32, Item)], cap: u32, total_len: u32) {
    if spacers == 0 {
        return;
    }

    let space = cap.saturating_sub(total_len) as usize;
    let mut enough_space = space;
    while !enough_space.is_multiple_of(spacers) {
        enough_space += 1;
    }
    let diff = enough_space - space;

    // Do it in reverse, so skipped Spacers won't change.
    for (i, (len, _)) in line
        .iter_mut()
        .rev()
        .filter(|(_, item)| matches!(item.part, Part::Spacer))
        .enumerate()
    {
        *len = ((enough_space / spacers) - (i < diff) as usize) as u32;
    }
}
