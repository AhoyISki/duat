use duat_core::{
    opts::PrintOpts,
    text::{Text, TextPart, TextPlace, TwoPoints},
};
use unicode_width::UnicodeWidthChar;

/// A struct representing a printed [`TextPlace`] on the screen
///
/// This position differs from a [`VPoint`] in the sense that it
/// represents three properties of a printed character:
///
/// - The x position in which it was printed;
/// - The amount of horizontal space it occupies;
/// - Wether this character is the first on the line (i.e. it wraps)
///
/// [`VPoint`]: crate::mode::VPoint
/// [`TextPlace`]: crate::text::TextPlace
#[derive(Debug, Clone, Copy)]
pub struct PrintedPlace {
    /// The horizontal position in which a character was printed
    pub x: u32,
    /// The horizontal space it occupied
    pub len: u32,
    /// Wether it is the first character in the line
    pub wrap: bool,
}

/// An [`Iterator`] that returns both an [`TextPlace`] and a
/// [`Caret`].
///
/// This function expects that `cap` has been validated, and that the
/// iterator starts in the visual start of the line.
pub fn print_iter(
    text: &Text,
    points: TwoPoints,
    width: u32,
    opts: PrintOpts,
) -> impl Iterator<Item = (PrintedPlace, TextPlace)> + '_ {
    let start_points = text.visual_line_start(points, 0);
    let cap = opts.wrap_width(width).unwrap_or(width);
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line construction variables.
    let (mut x, mut spacers) = (0, 0);
    let (mut indent, mut on_indent, mut wrapped_indent) = (0, true, 0);
    let mut replace_chars = Vec::with_capacity(opts.tabstop as usize);

    if start_points != points {
        for item in text
            .iter_fwd(start_points)
            .take_while(|item| item.points() < points)
        {
            let old_indent = indent;
            let indent = (&mut indent, &mut on_indent);
            let len = process_part(item, x, opts, indent, &mut spacers, &mut replace_chars);

            x += len;
            if x > cap && opts.wrap_lines {
                spacers = 0;

                if let TextPart::Char('\t') = item.part {
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
        replace_chars,
    )
}

pub fn rev_print_iter(
    text: &Text,
    points: TwoPoints,
    cap: u32,
    opts: PrintOpts,
) -> impl Iterator<Item = (PrintedPlace, TextPlace)> + '_ {
    let mut iter = text.iter_rev(points);

    let mut returns = Vec::new();
    let mut items = Vec::new();

    std::iter::from_fn(move || {
        if let Some(next) = returns.pop() {
            Some(next)
        } else {
            let iter = loop {
                if let Some(item) = iter.next() {
                    items.push(item);
                    if let TextPart::Char('\n') = item.part {
                        let len = items.len();
                        if len > 1 {
                            break items.drain(..len - 1).rev();
                        }
                    }
                } else {
                    break items.drain(..).rev();
                }
            };

            let reps = Vec::new();
            returns.extend(inner_iter(iter, (0, 0), (0, true, 0), (cap, opts), reps));

            returns.pop()
        }
    })
}

#[inline(always)]
fn inner_iter(
    iter: impl Iterator<Item = TextPlace>,
    (mut x, mut spacers): (u32, usize),
    (mut indent, mut on_indent, mut wrapped_indent): (u32, bool, u32),
    (cap, opts): (u32, PrintOpts),
    mut replace_chars: Vec<char>,
) -> impl Iterator<Item = (PrintedPlace, TextPlace)> {
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line return variables.
    let (mut line, mut leftover_nl) = (Vec::<(u32, TextPlace)>::new(), None);
    let (mut printed_x, mut i, mut has_wrapped) = (0, 0, false);

    let mut iter = iter.peekable();

    let mut initial_printed_x = 0;
    let mut first_printed_x = x * (x < cap || !opts.wrap_lines) as u32;

    std::iter::from_fn(move || {
        loop {
            // Emptying the line, most next calls should come here.
            if let Some(&(len, item)) = line.get(i) {
                let wrap = !has_wrapped && (len > 0 || item.part.is_char());
                if wrap {
                    printed_x = initial_printed_x;
                }
                let caret = PrintedPlace { x: printed_x, len, wrap };
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
                break Some((PrintedPlace { x, len: 1, wrap: true }, item));
            }

            // Preparing the line itself.
            loop {
                let Some(&item) = iter.peek() else {
                    if line.is_empty() {
                        return None;
                    } else {
                        break;
                    }
                };

                let old_indent = indent;
                let indent = (&mut indent, &mut on_indent);
                let len = process_part(item, x, opts, indent, &mut spacers, &mut replace_chars);

                x += len;

                let must_wrap = x > cap && opts.wrap_lines;

                if let TextPart::Char(char) = item.part
                    && (must_wrap || char == '\n')
                {
                    replace_chars.clear();

                    initial_printed_x = first_printed_x + wrapped_indent;
                    space_line(spacers, &mut line, cap, x);
                    spacers = 0;

                    let leftover = x.saturating_sub(cap);
                    wrapped_indent = match char {
                        '\t' if leftover > 0 => {
                            line.push((len - leftover, iter.next().unwrap()));
                            if old_indent + leftover < max_indent && opts.indent_wraps {
                                old_indent + leftover
                            } else {
                                leftover
                            }
                        }
                        '\n' => {
                            if len > 0 && must_wrap {
                                let position = old_indent
                                    * (old_indent < max_indent && opts.indent_wraps) as u32;
                                leftover_nl = Some((position, iter.next().unwrap()))
                            } else {
                                line.push((len, iter.next().unwrap()))
                            }
                            0
                        }
                        _ => {
                            // At this point, the only logical course of action is to wrap on
                            // every character.
                            if cap == 0 && !line.iter().any(|(_, item)| item.part.is_char()) {
                                line.push((len, iter.next().unwrap()));
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
                    line.push((len, iter.next().unwrap()));
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
    let (mut x, mut spacers) = (0, 0);
    let (mut indent, mut on_indent) = (0, true);
    let mut replace_chars = Vec::new();

    if start_points == points {
        true
    } else if !opts.wrap_lines {
        false
    } else {
        let mut wrapped = true;

        for item in text
            .iter_fwd(start_points)
            .take_while(|item| item.points() <= points)
        {
            wrapped = false;

            let old_indent = indent;
            let indent = (&mut indent, &mut on_indent);
            let len = process_part(item, x, opts, indent, &mut spacers, &mut replace_chars);

            x += len;
            if x > cap && opts.wrap_lines {
                wrapped = true;

                x = if let TextPart::Char('\t') = item.part {
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
fn _words(
    iter: impl Iterator<Item = TextPlace> + Clone,
    (mut total_len, mut spacers): (u32, usize),
    (mut indent, mut on_indent, mut wrapped_indent): (u32, bool, u32),
    (cap, opts): (u32, PrintOpts),
) -> impl Iterator<Item = (PrintedPlace, TextPlace)> + Clone {
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line return variables.
    let (mut line, mut leftover_nl): (Vec<(u32, TextPlace)>, _) = (Vec::new(), None);
    let (mut x, mut i, mut first_char_was_printed) = (0, 0, false);

    // Line construction variables
    let mut iter = iter.peekable();
    let mut word = Vec::new();
    let (mut new_x, mut first_x) = (0, total_len * (total_len < cap) as u32);
    let mut replace_chars = Vec::with_capacity(opts.tabstop as usize);

    std::iter::from_fn(move || {
        loop {
            if let Some(&(len, item)) = line.get(i) {
                let wrap = !first_char_was_printed && item.part.is_char();
                if wrap {
                    x = new_x;
                }
                let caret = PrintedPlace { x, len, wrap };
                i += 1;
                x += len;
                first_char_was_printed |= item.part.is_char();
                break Some((caret, item));
            }

            line.clear();
            i = 0;
            first_char_was_printed = false;

            if let Some((x, item)) = leftover_nl.take() {
                break Some((PrintedPlace { x, len: 1, wrap: true }, item));
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
                let indent = (&mut indent, &mut on_indent);
                let len = process_part(item, x, opts, indent, &mut spacers, &mut replace_chars);

                total_len += len;

                if let TextPart::Char(char) = item.part
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
fn len_from(char: char, start: u32, opts: PrintOpts) -> u32 {
    match char {
        '\t' => (opts.tabstop_spaces_at(start)).max(1),
        '\n' => 0,
        '\0'..='\u{1f}' => 2,
        '\u{80}'..='\u{9f}' => 4,
        char => UnicodeWidthChar::width(char).unwrap_or(0) as u32,
    }
}

fn process_part(
    item: TextPlace,
    x: u32,
    opts: PrintOpts,
    indent: (&mut u32, &mut bool),
    spacers: &mut usize,
    replace_chars: &mut Vec<char>,
) -> u32 {
    match item.part {
        TextPart::Char('\t') => {
            replace_chars.clear();
            process_char(indent, x, '\t', opts)
        }
        TextPart::Char(orig) => match replace_chars.drain(..).next() {
            Some(char) => {
                process_char(indent, x, orig, opts);
                len_from(char, x, opts)
            }
            None => process_char(indent, x, orig, opts),
        },
        TextPart::Spacer => {
            *spacers += 1;
            0
        }
        TextPart::ReplaceChar(char) => {
            replace_chars.push(char);
            0
        }
        _ => 0,
    }
}

#[inline(always)]
fn process_char(
    (indent, on_indent): (&mut u32, &mut bool),
    x: u32,
    char: char,
    opts: PrintOpts,
) -> u32 {
    match char {
        '\n' => {
            *on_indent = true;
            *indent = 0;
            opts.print_new_line as u32
        }
        char => {
            let len = len_from(char, x, opts);
            *on_indent &= char == ' ' || char == '\t';
            *indent += len * *on_indent as u32;
            len
        }
    }
}

/// Adds spacing to a line, and returns the initial amount of
/// space needed
fn space_line(spacers: usize, line: &mut [(u32, TextPlace)], cap: u32, total_len: u32) {
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
        .filter(|(_, item)| matches!(item.part, TextPart::Spacer))
        .enumerate()
    {
        *len = ((enough_space / spacers) - (i < diff) as usize) as u32;
    }
}
