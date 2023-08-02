use std::marker::PhantomData;

use parsec_core::text::{Part, PrintCfg, WrapMethod};
use unicode_width::UnicodeWidthChar;

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
#[inline(always)]
fn indents<'a>(
    iter: impl Iterator<Item = (usize, usize, Part)> + Clone + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = (usize, (usize, usize, Part))> + Clone + 'a {
    iter.scan((0, true), move |(indent, on_indent), (pos, line, part)| {
        let old_indent = if *indent < width { *indent } else { 0 };
        (*indent, *on_indent) = match (&part, *on_indent) {
            (&Part::Char('\t'), true) => (*indent + cfg.tab_stops.spaces_at(*indent), true),
            (&Part::Char(' '), true) => (*indent + 1, true),
            (&Part::Char('\n'), _) => (0, true),
            (&Part::Char(_), _) => (*indent, false),
            (_, on_indent) => (*indent, on_indent)
        };

        Some((old_indent, (pos, line, part)))
    })
}

#[inline(always)]
fn parts<'a>(
    iter: impl Iterator<Item = (usize, (usize, usize, Part))> + Clone + 'a, width: usize,
    cfg: &'a PrintCfg
) -> impl Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a {
    iter.scan((0, true, None), move |(x, needs_to_wrap, prev_char), (indent, unit)| {
        item((x, needs_to_wrap, prev_char), indent, unit, width, cfg)
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
#[inline(always)]
fn words<'a>(
    iter: impl Iterator<Item = (usize, (usize, usize, Part))> + Clone + 'a, width: usize,
    cfg: &'a PrintCfg
) -> impl Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a {
    let mut iter = iter.peekable();
    let mut indent = 0;
    let mut word = Vec::new();

    let mut prev_char = None;
    let mut finished_word = Vec::new();
    let mut x = 0;
    let mut needs_to_wrap = true;
    std::iter::from_fn(move || {
        if let Some(unit) = finished_word.pop() {
            return item((&mut x, &mut needs_to_wrap, &mut prev_char), indent, unit, width, cfg);
        }

        let mut word_len = 0;
        while let Some((new_indent, (.., bit))) = iter.peek() {
            if let &Part::Char(char) = bit {
                indent = *new_indent;
                if cfg.word_chars.contains(char) {
                    word_len += len_from(char, x + word_len, width, cfg, prev_char)
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
            item((&mut x, &mut needs_to_wrap, &mut prev_char), indent, unit, width, cfg)
        })
    })
}

#[inline(always)]
fn item(
    (x, needs_to_wrap, prev_char): (&mut usize, &mut bool, &mut Option<char>), indent: usize,
    (pos, line, part): (usize, usize, Part), width: usize, cfg: &PrintCfg
) -> Option<((usize, usize, Option<usize>), (usize, Part))> {
    let (len, processed_part) = process_part(part, cfg, prev_char, x, width);
    *x += len;

    let width_wrap = (*x > width || (*x == width && len == 0)) && !cfg.wrap_method.is_no_wrap();
    let nl_wrap = *needs_to_wrap && prev_char.is_some();
    let go_to_nl = (nl_wrap || width_wrap).then(|| {
        *x = indent + len;
        *needs_to_wrap = false;
        line
    });

    if let Some(char) = part.as_char() {
        *needs_to_wrap = char == '\n'
    }

    Some(((*x - len, len, go_to_nl), (pos, processed_part)))
}

#[inline(always)]
fn process_part(
    part: Part, cfg: &PrintCfg, prev_char: &mut Option<char>, x: &mut usize, width: usize
) -> (usize, Part) {
    match part {
        Part::Char(char) => {
            let ret = if char == '\n' {
                let char = cfg.new_line.char(*prev_char);
                if let Some(char) = char {
                    (len_from(char, *x, width, cfg, *prev_char), Part::Char(char))
                } else {
                    (0, Part::Char('\n'))
                }
            } else {
                (len_from(char, *x, width, cfg, *prev_char), Part::Char(char))
            };

            *prev_char = Some(char);
            ret
        }
        _ => (0, part)
    }
}

#[derive(Clone)]
enum Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a,
    Words: Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a
{
    Parts(Bits, PhantomData<&'a ()>),
    Words(Words)
}

pub fn print_iter<'a>(
    iter: impl Iterator<Item = (usize, usize, Part)> + Clone + 'a, char_start: usize, width: usize,
    cfg: &'a PrintCfg
) -> impl Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a {
    let width = if let WrapMethod::Capped(cap) = cfg.wrap_method { cap } else { width };
    match cfg.wrap_method {
        WrapMethod::Width | WrapMethod::NoWrap | WrapMethod::Capped(_) => {
            let indents = indents(iter, width, cfg)
                .filter(move |(_, (pos, _, part))| *pos >= char_start || part.is_tag());
            Iter::Parts(parts(indents, width, cfg), PhantomData)
        }
        WrapMethod::Word => {
            let indents = indents(iter, width, cfg)
                .filter(move |(_, (pos, _, part))| *pos >= char_start || part.is_tag());
            Iter::Words(words(indents, width, cfg))
        }
    }
}

pub fn rev_print_iter<'a>(
    mut iter: impl Iterator<Item = (usize, usize, Part)> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + 'a {
    let mut returns = Vec::new();
    let mut prev_line_nl = None;
    std::iter::from_fn(move || {
        if let Some(next) = returns.pop() {
            Some(next)
        } else {
            let mut units: Vec<(usize, usize, Part)> = prev_line_nl.take().into_iter().collect();
            while let Some((pos, line, part)) = iter.next() {
                if let Part::Char('\n') = part {
                    if units.is_empty() {
                        units.push((pos, line, part));
                    } else {
                        prev_line_nl = Some((pos, line, Part::Char('\n')));
                        break;
                    }
                } else {
                    units.push((pos, line, part));
                }
            }

            print_iter(units.into_iter().rev(), 0, width, cfg).collect_into(&mut returns);
            returns.pop()
        }
    })
}

impl<'a, Parts, Words> Iterator for Iter<'a, Parts, Words>
where
    Parts: Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a,
    Words: Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a
{
    type Item = ((usize, usize, Option<usize>), (usize, Part));

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Parts(parts, _) => parts.next(),
            Iter::Words(words) => words.next()
        }
    }
}

#[inline(always)]
fn len_from(
    char: char, start: usize, max_width: usize, cfg: &PrintCfg, prev_char: Option<char>
) -> usize {
    let char = if char == '\n' { cfg.new_line.char(prev_char).unwrap_or('\n') } else { char };
    match char {
        '\t' => (cfg.tab_stops.spaces_at(start)).min(max_width.saturating_sub(start)).max(1),
        '\n' => 0,
        _ => UnicodeWidthChar::width(char).unwrap_or(0)
    }
}
