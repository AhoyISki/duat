use std::{marker::PhantomData, ops::ControlFlow::*};

use duat_core::{
    cfg::{PrintCfg, WrapMethod},
    text::{FwdIter as TextIter, Item, Part, Point, RevIter as RevTextIter},
    ui::Caret,
};
use unicode_width::UnicodeWidthChar;

#[inline(always)]
fn parts<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    cap: u32,
    cfg: PrintCfg,
    initial: (u32, bool),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let (mut x, mut needs_to_wrap, mut prev_char) = (0, true, None);
    let max_indent = if cfg.indent_wrap { cap } else { 0 };
    let (mut indent, mut on_indent) = initial;

    iter.map(move |mut item| {
        let old_indent = indent * (indent < max_indent) as u32;
        let (len, processed_part) = {
            match item.part {
                Part::Char(char) => {
                    let ret = if char == '\n' {
                        indent = 0;
                        on_indent = true;
                        let char = cfg.new_line.char(prev_char);
                        match char {
                            Some(char) => (len_from(char, x, cap, &cfg), Part::Char(char)),
                            None => (0, Part::Char('\n')),
                        }
                    } else {
                        let len = len_from(char, x, cap, &cfg);
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

        let width_wrap = x > cap || (x == cap && len == 0);
        let nl_wrap = needs_to_wrap && prev_char.is_some();
        if nl_wrap || width_wrap {
            old_x = old_indent;
            x = old_indent + len;
            needs_to_wrap = false;
        };

        if let Some(char) = item.part.as_char() {
            if char == '\n' {
                needs_to_wrap = true;
                x = 0;
            }
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
    cfg: PrintCfg,
    initial: (u32, bool),
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let max_indent = if cfg.indent_wrap { cap } else { 0 };

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
            return attach_caret(vars, old_indent, item, cap, &cfg);
        }

        let mut word_len = 0;
        while let Some(item) = iter.peek() {
            if let Part::Char(char) = item.part {
                match char {
                    ' ' => indent += on_indent as u32,
                    '\t' => indent += on_indent as u32 * cfg.tab_stops.spaces_at(indent),
                    '\n' => (indent, on_indent) = (0, true),
                    _ => on_indent = false,
                }

                if cfg.word_chars.contains(char) {
                    word_len += len_from(char, x + word_len, cap, &cfg)
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
            attach_caret(vars, indent, item, cap, &cfg)
        })
    })
}

#[inline(always)]
fn attach_caret(
    (x, needs_to_wrap, prev_char): (&mut u32, &mut bool, &mut Option<char>),
    indent: u32,
    mut item: Item,
    cap: u32,
    cfg: &PrintCfg,
) -> Option<(Caret, Item)> {
    let (len, processed_part) = process_part(item.part, cfg, prev_char, *x, cap);

    let mut old_x = *x;
    *x += len;

    let width_wrap = *x > cap || (*x == cap && len == 0);
    let nl_wrap = *needs_to_wrap && prev_char.is_some();
    if nl_wrap || width_wrap {
        old_x = indent;
        *x = indent + len;
        *needs_to_wrap = false;
    };

    if let Some(char) = item.part.as_char() {
        if char == '\n' {
            *needs_to_wrap = true;
            *x = 0;
        }
    }

    item.part = processed_part;
    Some((Caret::new(old_x, len, nl_wrap || width_wrap), item))
}

#[inline(always)]
fn process_part(
    part: Part,
    cfg: &PrintCfg,
    prev_char: &mut Option<char>,
    x: u32,
    cap: u32,
) -> (u32, Part) {
    match part {
        Part::Char(b) => {
            let ret = if b == '\n' {
                let char = cfg.new_line.char(*prev_char);
                match char {
                    Some(char) => (len_from(char, x, cap, cfg), Part::Char(char)),
                    None => (0, Part::Char('\n')),
                }
            } else {
                (len_from(b, x, cap, cfg), Part::Char(b))
            };

            *prev_char = Some(b);
            ret
        }
        _ => (0, part),
    }
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

/// An [`Iterator`] that returns both an [`Item`] and a [`Caret`].
///
/// This function expects that `cap` has been validated, and that the
/// iterator starts in the visual start of the line.
pub fn print_iter(
    mut iter: TextIter<'_>,
    cap: u32,
    cfg: PrintCfg,
    points: (Point, Option<Point>),
) -> impl Iterator<Item = (Caret, Item)> + Clone + '_ {
    let (Continue(indent) | Break(indent)) = iter
        .clone()
        .take_while(|&Item { real, ghost, .. }| (real, ghost) < points)
        .try_fold(0, |indent, item| match item.part {
            Part::Char(_) if indent >= cap => Break(0),
            Part::Char('\t') => Continue(indent + cfg.tab_stops.spaces_at(indent)),
            Part::Char(' ') => Continue(indent + 1),
            Part::Char(_) => Break(indent),
            _ => Continue(indent),
        });

    let iter_at_line_start = points == iter.points();
    if !iter_at_line_start {
        iter.skip_to(points);
    }
    inner_iter(iter, cap, (indent, iter_at_line_start), cfg)
}

/// An [`Iterator`] that returns both an [`Item`] and a [`Caret`].
///
/// This function will function properly given that, elsewhere in
/// the code, the passed [`PrintInfo`] and `width` have beend
/// validated.
pub(super) fn print_iter_indented(
    iter: TextIter<'_>,
    cap: u32,
    cfg: PrintCfg,
    indent: u32,
) -> impl Iterator<Item = (Caret, Item)> + Clone + '_ {
    inner_iter(iter, cap, (indent, false), cfg)
}

pub fn rev_print_iter(
    mut iter: RevTextIter<'_>,
    width: u32,
    cfg: PrintCfg,
) -> impl Iterator<Item = (Caret, Item)> + Clone + '_ {
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

            returns.extend(inner_iter(items.into_iter().rev(), width, (0, true), cfg));

            returns.pop()
        }
    })
}

fn inner_iter<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    cap: u32,
    initial: (u32, bool),
    cfg: PrintCfg,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    match cfg.wrap_method {
        WrapMethod::Width | WrapMethod::NoWrap | WrapMethod::Capped(_) => {
            Iter::Parts(parts(iter, cap, cfg, initial), PhantomData)
        }
        WrapMethod::Word => Iter::Words(words(iter, cap, cfg, initial)),
    }
}

#[inline(always)]
fn len_from(char: char, start: u32, max_width: u32, cfg: &PrintCfg) -> u32 {
    match char {
        '\t' => (cfg.tab_stops.spaces_at(start))
            .min(max_width.saturating_sub(start))
            .max(1),
        '\n' => 0,
        _ => UnicodeWidthChar::width(char).unwrap_or(0) as u32,
    }
}
