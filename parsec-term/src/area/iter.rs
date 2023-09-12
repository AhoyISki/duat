use std::marker::PhantomData;

use parsec_core::{
    text::{Item, IterCfg, Part, WrapMethod},
    ui::Caret,
};
use unicode_width::UnicodeWidthChar;

use super::PrintInfo;

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
#[inline(always)]
fn indents<'a>(
    iter: impl Iterator<Item = Item> + Clone + 'a,
    width: usize,
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (usize, Item)> + Clone + 'a {
    iter.scan((0, true), move |(indent, on_indent), item| {
        if cfg.indent_wrap() {
            let old_indent = if *indent < width { *indent } else { 0 };
            (*indent, *on_indent) = match (&item.part, *on_indent) {
                (&Part::Char('\t'), true) => (*indent + cfg.tab_stops().spaces_at(*indent), true),
                (&Part::Char(' '), true) => (*indent + 1, true),
                (&Part::Char('\n'), _) => (0, true),
                (&Part::Char(_), _) => (*indent, false),
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
    width: usize,
    cfg: IterCfg<'a>,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    iter.scan(
        (0, true, None),
        move |(x, needs_to_wrap, prev_char), (indent, unit)| {
            attach_caret((x, needs_to_wrap, prev_char), indent, unit, width, &cfg)
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
    let mut needs_to_wrap = true;
    std::iter::from_fn(move || {
        if let Some(unit) = finished_word.pop() {
            return attach_caret(
                (&mut x, &mut needs_to_wrap, &mut prev_char),
                indent,
                unit,
                width,
                &cfg,
            );
        }

        let mut word_len = 0;
        while let Some((new_indent, item)) = iter.peek() {
            if let Part::Char(char) = item.part {
                indent = *new_indent;
                if cfg.word_chars().contains(char) {
                    word_len += len_from(char, x + word_len, width, &cfg, prev_char)
                } else {
                    word.push(iter.next().map(|(_, unit)| unit).unwrap());
                    break;
                }
            }
            word.push(iter.next().map(|(_, unit)| unit).unwrap());
        }

        needs_to_wrap |= x + word_len > width;

        std::mem::swap(&mut word, &mut finished_word);
        finished_word.reverse();
        finished_word.pop().and_then(|unit| {
            attach_caret(
                (&mut x, &mut needs_to_wrap, &mut prev_char),
                indent,
                unit,
                width,
                &cfg,
            )
        })
    })
}

#[inline(always)]
fn attach_caret(
    (x, needs_to_wrap, prev_char): (&mut usize, &mut bool, &mut Option<char>),
    indent: usize,
    mut item: Item,
    width: usize,
    cfg: &IterCfg,
) -> Option<(Caret, Item)> {
    let (len, processed_part) = process_part(item.part, cfg, prev_char, *x, width);

    let mut old_x = *x;
    *x += len;

    let width_wrap = (*x > width || (*x == width && len == 0)) && !cfg.wrap_method().is_no_wrap();
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
    cfg: &IterCfg,
    prev_char: &mut Option<char>,
    x: usize,
    width: usize,
) -> (usize, Part) {
    match part {
        Part::Char(char) => {
            let ret = if char == '\n' {
                let char = cfg.new_line().char(*prev_char);
                if let Some(char) = char {
                    (len_from(char, x, width, cfg, *prev_char), Part::Char(char))
                } else {
                    (0, Part::Char('\n'))
                }
            } else {
                (len_from(char, x, width, cfg, *prev_char), Part::Char(char))
            };

            *prev_char = Some(char);
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

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Parts(parts, _) => parts.next(),
            Iter::Words(words) => words.next(),
        }
    }
}

pub fn print_iter<'a>(
    text: impl Iterator<Item = Item> + Clone + 'a,
    width: usize,
    cfg: IterCfg<'a>,
    info: PrintInfo,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let width = if let WrapMethod::Capped(cap) = cfg.wrap_method() {
        cap
    } else {
        width
    };

    let indents = indents(text, width, cfg).filter(move |(_, item)| {
        if item.part.is_char() {
            match item.pos.cmp(&info.first_char) {
                std::cmp::Ordering::Greater => true,
                std::cmp::Ordering::Equal => {
                    item.ghost_pos.map_or(true, |pos| pos >= info.first_ghost)
                }
                std::cmp::Ordering::Less => false,
            }
        } else {
            true
        }
    });

    match cfg.wrap_method() {
        WrapMethod::Width | WrapMethod::NoWrap | WrapMethod::Capped(_) => {
            Iter::Parts(parts(indents, width, cfg), PhantomData)
        }
        WrapMethod::Word => Iter::Words(words(indents, width, cfg)),
    }
}

pub fn rev_print_iter<'a>(
    mut iter: impl Iterator<Item = Item> + Clone + 'a,
    width: usize,
    mut cfg: IterCfg<'a>,
) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
    let mut returns = Vec::new();
    let mut prev_line_nl = None;
    let info = PrintInfo::default();

    std::iter::from_fn(move || {
        if let Some(next) = returns.pop() {
            Some(next)
        } else {
            let mut give_up = false;
            let mut items: Vec<Item> = prev_line_nl.take().into_iter().collect();
            while let Some(item) = iter.next() {
                if let Part::Char('\n') = item.part {
                    if items.is_empty() {
                        items.push(item);
                    } else {
                        prev_line_nl = Some(item);
                        break;
                    }
                // NOTE: 20000 is a magic number, it's merely a guess
                // at what would be reasonable.
                } else if items.len() == 20000 {
                    give_up = true;
                    break;
                } else {
                    items.push(item);
                }
            }

            if give_up {
                cfg = cfg.no_word_wrap().no_indent_wrap();
            }

            returns.extend(print_iter(items.into_iter().rev(), width, cfg, info));
            returns.pop()
        }
    })
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
