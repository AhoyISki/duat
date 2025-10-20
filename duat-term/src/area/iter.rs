use std::marker::PhantomData;

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
    let max_indent = if opts.indent_wrapped { cap } else { 0 };

    // Line construction variables.
    let (mut total_len, mut gaps) = (0, Gaps::OnRight);
    let (mut indent, mut on_indent, mut tab_leftovers) = (0, true, 0);

    let iter = if start_points != points {
        for item in text
            .iter_fwd(start_points)
            .take_while(|item| item.points() < points)
        {
            let old_indent = indent * (indent < max_indent) as u32;
            let (len, _) = match item.part {
                Part::Char('\n') => process_nl(&mut indent, &mut on_indent, total_len, opts),
                Part::Char(char) => {
                    process_char(&mut indent, &mut on_indent, total_len, char, opts)
                }
                _ => (0, item.part),
            };

            gaps = gaps.replace_by_part(item.part);

            total_len += len;
            if total_len > cap && !opts.dont_wrap {
                total_len = (old_indent + tab_leftovers) * (item.part != Part::Char('\n')) as u32;

                if let Part::Char('\t') = item.part {
                    tab_leftovers = total_len.saturating_sub(cap);
                }
            }
        }

        text.iter_fwd(points).peekable()
    } else {
        text.iter_fwd(start_points).peekable()
    };

    inner_iter(
        iter,
        (total_len, gaps),
        (indent, on_indent, tab_leftovers),
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

#[derive(Clone)]
enum Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = (Caret, Item)> + Clone + 'a,
    Words: Iterator<Item = (Caret, Item)> + Clone + 'a,
{
    Parts(Bits, PhantomData<&'a ()>),
    Words(Words),
}

impl<'a, Parts, Words> Iterator for Iter<'a, Parts, Words>
where
    Parts: Iterator<Item = (Caret, Item)> + Clone + 'a,
    Words: Iterator<Item = (Caret, Item)> + Clone + 'a,
{
    type Item = (Caret, Item);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Parts(parts, _) => parts.next(),
            Iter::Words(words) => words.next(),
        }
    }
}

#[inline(always)]
fn parts<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    cap: u32,
    opts: PrintOpts,
    initial: (u32, bool),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let (mut x, mut needs_to_wrap, mut prev_char) = (0, true, None);
    let max_indent = if opts.indent_wrapped { cap } else { 0 };
    let (mut indent, mut on_indent) = initial;

    iter.map(move |mut item| {
        let old_indent = indent * (indent < max_indent) as u32;
        let (len, processed_part) = {
            match item.part {
                Part::Char(char) => {
                    let ret = if char == '\n' {
                        indent = 0;
                        on_indent = true;
                        let char = if opts.print_new_line { ' ' } else { '\n' };
                        (len_from(char, x, &opts), Part::Char(char))
                    } else {
                        let len = len_from(char, x, &opts);
                        if on_indent && (char == ' ' || char == '\t') {
                            indent += len;
                        } else {
                            on_indent = false
                        }
                        (len, Part::Char(char))
                    };

                    prev_char = Some(char);
                    ret
                }
                _ => (0, item.part),
            }
        };

        let mut old_x = x;
        x += len;

        let width_wrap = x > cap;
        let nl_wrap = needs_to_wrap && prev_char.is_some();
        if nl_wrap || width_wrap {
            old_x = old_indent;
            x = old_indent + len;
            needs_to_wrap = false;
        };

        if item.part == Part::Char('\n') {
            needs_to_wrap = true;
            x = 0;
        }

        item.part = processed_part;
        (Caret::new(old_x, len, nl_wrap || width_wrap), item)
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
#[inline(always)]
fn words<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    cap: u32,
    opts: PrintOpts,
    initial: (u32, bool),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let max_indent = if opts.indent_wrapped { cap } else { 0 };

    let mut iter = iter.peekable();
    let (mut indent, mut on_indent) = initial;
    let mut word = Vec::new();

    let mut prev_char = None;
    let mut finished_word = Vec::new();
    let mut x = 0;
    let mut needs_wrap = true;
    std::iter::from_fn(move || {
        let old_indent = indent * (indent < max_indent) as u32;
        if let Some(item) = finished_word.pop() {
            let vars = (&mut x, &mut needs_wrap, &mut prev_char);
            return attach_caret(vars, old_indent, item, cap, &opts);
        }

        let mut word_len = 0;
        while let Some(item) = iter.peek() {
            if let Part::Char(char) = item.part {
                match char {
                    ' ' => indent += on_indent as u32,
                    '\t' => indent += on_indent as u32 * opts.tabstop_spaces_at(indent),
                    '\n' => (indent, on_indent) = (0, true),
                    _ => on_indent = false,
                }

                if opts.word_chars.contains(char) {
                    word_len += len_from(char, x + word_len, &opts)
                } else {
                    word.push(iter.next().unwrap());
                    break;
                }
            }
            word.push(iter.next().unwrap());
        }

        needs_wrap |= x + word_len > cap;

        std::mem::swap(&mut word, &mut finished_word);
        finished_word.reverse();
        finished_word.pop().and_then(|item| {
            let vars = (&mut x, &mut needs_wrap, &mut prev_char);
            attach_caret(vars, indent, item, cap, &opts)
        })
    })
}

#[inline(always)]
fn attach_caret(
    (x, needs_to_wrap, prev_char): (&mut u32, &mut bool, &mut Option<char>),
    indent: u32,
    mut item: Item,
    cap: u32,
    opts: &PrintOpts,
) -> Option<(Caret, Item)> {
    let (len, processed_part) = process_part(item.part, opts, prev_char, *x);

    let mut old_x = *x;
    *x += len;

    let width_wrap = *x > cap;
    let nl_wrap = *needs_to_wrap && prev_char.is_some();
    if nl_wrap || width_wrap {
        old_x = indent;
        *x = indent + len;
        *needs_to_wrap = false;
    };

    if let Some(char) = item.part.as_char()
        && char == '\n'
    {
        *needs_to_wrap = true;
        *x = 0;
    }

    item.part = processed_part;
    Some((Caret::new(old_x, len, nl_wrap || width_wrap), item))
}

#[inline(always)]
fn process_part(part: Part, opts: &PrintOpts, prev_char: &mut Option<char>, x: u32) -> (u32, Part) {
    match part {
        Part::Char(char) => {
            let ret = if char == '\n' {
                let char = if opts.print_new_line { ' ' } else { '\n' };
                (len_from(char, x, opts), Part::Char(char))
            } else {
                (len_from(char, x, opts), Part::Char(char))
            };
            *prev_char = Some(char);
            ret
        }
        _ => (0, part),
    }
}

fn inner_iter<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    (mut total_len, mut gaps): (u32, Gaps),
    (mut indent, mut on_indent, mut tab_leftovers): (u32, bool, u32),
    (cap, opts): (u32, PrintOpts),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let max_indent = if opts.indent_wrapped { cap } else { 0 };

    // Line return variables.
    let mut line: Vec<(u32, Item)> = Vec::new();
    let (mut x, mut i) = (0, 0);

    let mut iter = iter.peekable();

    std::iter::from_fn(move || {
        if let Some(&(len, item)) = line.get(i) {
            let caret = Caret { x, len, wrap: false };
            i += 1;
            x += len;
            return Some((caret, item));
        }

        line.clear();
        i = 0;
        x = 0;

        let mut first_x = None;

        while let Some(&item) = iter.peek() {
            let old_indent = indent * (indent < max_indent) as u32;
            let (len, processed_part) = match item.part {
                Part::Char('\n') => process_nl(&mut indent, &mut on_indent, x, opts),
                Part::Char(char) => process_char(&mut indent, &mut on_indent, x, char, opts),
                _ => (0, item.part),
            };

            gaps = gaps.replace_by_part(item.part);

            let old_total_len = total_len;
            total_len += len;
            if (total_len > cap && !opts.dont_wrap) || item.part == Part::Char('\n') {
                x = first_x.unwrap_or(0) + gaps.space_line(&mut line, cap, total_len);

                total_len = (old_indent + tab_leftovers) * (item.part != Part::Char('\n')) as u32;

                if let Part::Char('\t' | '\n') = item.part {
                    tab_leftovers = (old_total_len + len).saturating_sub(cap);
                    first_x.get_or_insert(old_total_len);
                    let len = len - tab_leftovers;
                    line.push((len, Item { part: processed_part, ..item }));
                    iter.next();
                }

                if !line.is_empty() {
                    break;
                }
            } else {
                first_x.get_or_insert(old_total_len);
                line.push((len, Item { part: processed_part, ..item }));
            }

            iter.next();
        }

        line.first().map(|&(len, item)| {
            let caret = Caret { x, len, wrap: true };
            i += 1;
            x += len;
            (caret, item)
        })
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
) -> (u32, Part) {
    let len = len_from(char, x, &opts);
    if *on_indent && (char == ' ' || char == '\t') {
        *indent += len;
    } else {
        *on_indent = false
    }
    (len, Part::Char(char))
}

#[inline(always)]
fn process_nl(indent: &mut u32, on_indent: &mut bool, x: u32, opts: PrintOpts) -> (u32, Part) {
    *indent = 0;
    *on_indent = true;
    let char = if opts.print_new_line { ' ' } else { '\n' };
    (len_from(char, x, &opts), Part::Char(char))
}

#[derive(Debug, Clone, Copy)]
enum Gaps {
    OnRight,
    OnLeft,
    OnSides,
    Spacers(usize),
}

impl Gaps {
    /// Adds a [`Spacer`] to this [`Gaps`]
    ///
    /// [`Spacer`]: duat_core::text::Tag::Spacer
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
