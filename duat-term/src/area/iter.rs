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
    cap: u32,
    opts: PrintOpts,
) -> impl Iterator<Item = (Caret, Item)> + Clone + '_ {
    let start_points = text.visual_line_start(points, 0);
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line construction variables.
    let (mut total_len, mut gaps) = (0, Gaps::OnRight);
    let (mut indent, mut on_indent, mut wrapped_indent) = (0, true, 0);

    if start_points != points {
        for item in text
            .iter_fwd(start_points)
            .take_while(|item| item.points() < points)
        {
            let old_indent = indent;
            let len = match item.part {
                Part::Char('\n') => {
                    unreachable!("Shouldn't be possible, given the visual line start")
                }
                Part::Char(char) => {
                    process_char(&mut indent, &mut on_indent, total_len, char, opts)
                }
                _ => 0,
            };

            gaps = gaps.replace_by_part(item.part);

            total_len += len;
            if total_len > cap && !opts.dont_wrap {
                if let Part::Char('\t') = item.part {
                    let desired = old_indent + total_len - cap;
                    wrapped_indent = if desired < max_indent {
                        desired
                    } else {
                        total_len - cap
                    };
                    total_len = wrapped_indent;
                } else {
                    wrapped_indent = old_indent * (old_indent < max_indent) as u32;
                    total_len = wrapped_indent + len;
                }
            }
        }
    }

    inner_iter(
        text.iter_fwd(points),
        (total_len, gaps),
        (indent, on_indent, wrapped_indent),
        (cap, opts),
    )
}

pub fn rev_print_iter(
    text: &Text,
    points: TwoPoints,
    cap: u32,
    opts: PrintOpts,
) -> impl Iterator<Item = (Caret, Item)> + Clone + '_ {
    let mut iter = text.iter_rev(points);

    let mut returns = Vec::new();
    let mut prev_line_nl = None;

    std::iter::from_fn(move || {
        if let Some(next) = returns.pop() {
            Some(next)
        } else {
            let mut items: Vec<Item> = prev_line_nl.take().into_iter().collect();
            #[allow(clippy::while_let_on_iterator)]
            while let Some(item) = iter.next() {
                if let Part::Char('\n') = item.part {
                    if items.is_empty() {
                        items.push(item);
                    } else {
                        prev_line_nl = Some(item);
                        break;
                    }
                } else {
                    items.push(item);
                }
            }

            returns.extend(inner_iter(
                items.into_iter().rev(),
                (0, Gaps::OnRight),
                (0, true, 0),
                (cap, opts),
            ));

            returns.pop()
        }
    })
}

#[inline(always)]
fn inner_iter<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    (mut total_len, mut gaps): (u32, Gaps),
    (mut indent, mut on_indent, mut wrapped_indent): (u32, bool, u32),
    (cap, opts): (u32, PrintOpts),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let max_indent = if opts.indent_wraps { cap } else { 0 };

    // Line return variables.
    let (mut line, mut leftover_nl): (Vec<(u32, Item)>, _) = (Vec::new(), None);
    let (mut x, mut i, mut first_char_was_printed) = (0, 0, false);
    // Given the backtracked iteration, these should be replaced
    // correctly.
    let mut non_spacer_gaps = Gaps::OnRight;

    let mut iter = iter.peekable();
    let mut new_x = 0;
    let mut first_x = total_len * (total_len < cap) as u32;

    std::iter::from_fn(move || {
        loop {
            // Emptying the line, most next calls should come here.
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

            // Emptying a leftover '\n', which may come after the end of a line.
            if let Some((x, item)) = leftover_nl.take() {
                break Some((Caret { x, len: 1, wrap: true }, item));
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
                let len = match item.part {
                    Part::Char('\n') => process_nl(&mut indent, &mut on_indent, total_len, opts),
                    Part::Char(char) => {
                        process_char(&mut indent, &mut on_indent, total_len, char, opts)
                    }
                    _ => 0,
                };

                non_spacer_gaps = non_spacer_gaps.replace_by_part_no_spacers(item.part);
                gaps = gaps.replace_by_part(item.part);

                total_len += len;

                let must_wrap = total_len > cap && !opts.dont_wrap;
                if let Part::Char(char) = item.part
                    && (must_wrap || char == '\n')
                {
                    new_x = first_x + wrapped_indent + gaps.space_line(&mut line, cap, total_len);
                    gaps = non_spacer_gaps;

                    let leftover = total_len.saturating_sub(cap);
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
                            match (opts.print_new_line, must_wrap) {
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
                        _ => {
                            // At this point, the only logical course of action is to wrap on
                            // every character.
                            if cap == 0 && !line.iter().any(|(_, item)| item.part.is_char()) {
                                line.push((len, iter.next().unwrap()));
                            }
                            old_indent * (old_indent < max_indent && opts.indent_wraps) as u32
                        }
                    };

                    total_len = wrapped_indent;
                    break;
                } else {
                    line.push((len, item));
                }

                iter.next();
            }

            first_x = 0;
        }
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
#[inline(always)]
fn _words<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    (mut total_len, mut gaps): (u32, Gaps),
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

                gaps = gaps.replace_by_part(item.part);
                total_len += len;

                if let Part::Char(char) = item.part
                    && ((total_len > cap && !opts.dont_wrap) || char == '\n')
                {
                    new_x = first_x + wrapped_indent + gaps.space_line(&mut line, cap, total_len);

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

#[derive(Debug, Clone, Copy)]
enum Gaps {
    OnRight,
    OnLeft,
    OnSides,
    Spacers(usize),
}

impl Gaps {
    /// Replaces the `Gaps`, given an appropriate [`Part`]
    fn replace_by_part(self, part: Part) -> Self {
        match (self, part) {
            (_, Part::AlignLeft) => Gaps::OnRight,
            (_, Part::AlignCenter) => Gaps::OnSides,
            (_, Part::AlignRight) => Gaps::OnLeft,
            (Gaps::Spacers(count), Part::Spacer) => Gaps::Spacers(count + 1),
            (_, Part::Spacer) => Gaps::Spacers(1),
            _ => self,
        }
    }

    /// Replaces the `Gaps`, but ignores [`Part::Spacer`]
    fn replace_by_part_no_spacers(self, part: Part) -> Self {
        match part {
            Part::AlignLeft => Gaps::OnRight,
            Part::AlignCenter => Gaps::OnSides,
            Part::AlignRight => Gaps::OnLeft,
            _ => self,
        }
    }

    /// Adds spacing to a line, and returns the initial amount of
    /// space needed
    fn space_line(self, line: &mut [(u32, Item)], cap: u32, total_len: u32) -> u32 {
        match self {
            Gaps::OnRight => 0,
            Gaps::OnLeft => cap.saturating_sub(total_len),
            Gaps::OnSides => cap.saturating_sub(total_len) / 2,
            Gaps::Spacers(count) => {
                let space = cap.saturating_sub(total_len) as usize;
                let mut enough_space = space;
                while !enough_space.is_multiple_of(count) {
                    enough_space += 1;
                }
                let diff = enough_space - cap.saturating_sub(total_len) as usize;

                // Do it in reverse, so skipped Spacers won't change.
                for (i, (len, _)) in line
                    .iter_mut()
                    .rev()
                    .filter(|(_, item)| matches!(item.part, Part::Spacer))
                    .enumerate()
                {
                    *len = ((enough_space / count) - (i < diff) as usize) as u32;
                }

                0
            }
        }
    }
}
