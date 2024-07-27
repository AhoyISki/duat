use std::{marker::PhantomData, ops::ControlFlow::*};

use duat_core::{
    text::{Item, Iter as TextIter, IterCfg, Part, RevIter as RevTextIter, WrapMethod},
    ui::Caret,
};
use unicode_width::UnicodeWidthChar;

use super::PrintInfo;

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
#[inline(always)]
fn indents<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    cap: usize,
    initial: (usize, bool),
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (usize, Item)> + Clone + 'a {
    iter.scan(initial, move |(indent, on_indent), item| {
        if cfg.indent_wrap() {
            let old_indent = if *indent < cap { *indent } else { 0 };
            (*indent, *on_indent) = match (item.part, *on_indent) {
                (Part::Char('\t'), true) => (*indent + cfg.tab_stops().spaces_at(*indent), true),
                (Part::Char(' '), true) => (*indent + 1, true),
                (Part::Char('\n'), _) => (0, true),
                (Part::Char(_), _) => (*indent, false),
                (_, on_indent) => (*indent, on_indent),
            };

            Some((old_indent, item))
        } else {
            Some((0, item))
        }
    })
}

#[inline(always)]
fn parts<'a>(
    iter: impl Iterator<Item = (usize, Item)> + Clone + 'a,
    cap: usize,
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    iter.scan(
        (0, true, None),
        move |(x, needs_to_wrap, prev_char), (indent, unit)| {
            attach_caret((x, needs_to_wrap, prev_char), indent, unit, cap, &cfg)
        },
    )
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
#[inline(always)]
fn words<'a>(
    iter: impl Iterator<Item = (usize, Item)> + Clone + 'a,
    width: usize,
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let mut iter = iter.peekable();
    let mut indent = 0;
    let mut word = Vec::new();

    let mut prev_char = None;
    let mut finished_word = Vec::new();
    let mut x = 0;
    let mut needs_wrap = true;
    std::iter::from_fn(move || {
        if let Some(unit) = finished_word.pop() {
            let vars = (&mut x, &mut needs_wrap, &mut prev_char);
            return attach_caret(vars, indent, unit, width, &cfg);
        }

        let mut word_len = 0;
        while let Some((new_indent, item)) = iter.peek() {
            if let Part::Char(c) = item.part {
                indent = *new_indent;

                if cfg.word_chars().contains(c) {
                    word_len += len_from(c, x + word_len, width, &cfg, prev_char)
                } else {
                    word.push(iter.next().map(|(_, unit)| unit).unwrap());
                    break;
                }
            }
            word.push(iter.next().map(|(_, unit)| unit).unwrap());
        }

        needs_wrap |= x + word_len > width;

        std::mem::swap(&mut word, &mut finished_word);
        finished_word.reverse();
        finished_word.pop().and_then(|unit| {
            let vars = (&mut x, &mut needs_wrap, &mut prev_char);
            attach_caret(vars, indent, unit, width, &cfg)
        })
    })
}

#[inline(always)]
fn attach_caret(
    (x, needs_to_wrap, prev_char): (&mut usize, &mut bool, &mut Option<char>),
    indent: usize,
    mut item: Item,
    cap: usize,
    cfg: &IterCfg,
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

    if let Some(b) = item.part.as_char() {
        if b == '\n' {
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
    cfg: &IterCfg,
    prev_char: &mut Option<char>,
    x: usize,
    cap: usize,
) -> (usize, Part) {
    match part {
        Part::Char(b) => {
            let ret = if b == '\n' {
                let char = cfg.new_line().char(*prev_char);
                match char {
                    Some(char) => (len_from(char, x, cap, cfg, *prev_char), Part::Char(char)),
                    None => (0, Part::Char('\n')),
                }
            } else {
                (len_from(b, x, cap, cfg, *prev_char), Part::Char(b))
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
/// This function will function properly given that, elsewhere in
/// the code, the passed [`PrintInfo`] and `width` have beend
/// validated.
pub fn print_iter<'a>(
    mut iter: TextIter<'a>,
    cap: usize,
    cfg: IterCfg<'a>,
    info: PrintInfo,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let (Continue(indent) | Break(indent)) = iter
        .clone()
        .take_while(|&Item { real, ghost, .. }| (real, ghost) < info.points)
        .try_fold(0, |indent, item| match item.part {
            Part::Char(_) if indent >= cap => Break(0),
            Part::Char('\t') => Continue(indent + cfg.tab_stops().spaces_at(indent)),
            Part::Char(' ') => Continue(indent + 1),
            Part::Char(_) => Break(indent),
            _ => Continue(indent),
        });

    let iter_at_line_start = info.points == iter.points();
    if !iter_at_line_start {
        iter.skip_to(info.points);
    }
    inner_iter(iter, cap, (indent, iter_at_line_start), cfg)
}

/// An [`Iterator`] that returns both an [`Item`] and a [`Caret`].
///
/// This function will function properly given that, elsewhere in
/// the code, the passed [`PrintInfo`] and `width` have beend
/// validated.
pub(super) fn print_iter_indented<'a>(
    iter: TextIter<'a>,
    cap: usize,
    cfg: IterCfg<'a>,
    indent: usize,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    inner_iter(iter, cap, (indent, false), cfg)
}

pub fn rev_print_iter<'a>(
    mut iter: RevTextIter<'a>,
    width: usize,
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
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
    cap: usize,
    initial: (usize, bool),
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let indents = indents(iter, cap, initial, cfg);

    match cfg.wrap_method() {
        WrapMethod::Width | WrapMethod::NoWrap | WrapMethod::Capped(_) => {
            Iter::Parts(parts(indents, cap, cfg), PhantomData)
        }
        WrapMethod::Word => Iter::Words(words(indents, cap, cfg)),
    }
}

#[inline(always)]
fn len_from(
    char: char,
    start: usize,
    max_width: usize,
    cfg: &IterCfg,
    prev_char: Option<char>,
) -> usize {
    let char = if char == '\n' {
        cfg.new_line().char(prev_char).unwrap_or('\n')
    } else {
        char
    };
    match char {
        '\t' => (cfg.tab_stops().spaces_at(start))
            .min(max_width.saturating_sub(start))
            .max(1),
        '\n' => 0,
        _ => UnicodeWidthChar::width(char).unwrap_or(0),
    }
}
