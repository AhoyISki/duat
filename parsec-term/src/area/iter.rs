use std::marker::PhantomData;

use parsec_core::text::{Part, PrintCfg, WrapMethod};

use super::len_from;

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
fn indents<'a>(
    iter: impl Iterator<Item = (usize, usize, Part)> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = (u16, (usize, usize, Part))> + 'a {
    iter.scan((0, true), move |(indent, on_indent), (pos, line, part)| {
        let old_indent = if *indent < width { *indent } else { 0 };
        (*indent, *on_indent) = match (&part, *on_indent) {
            (&Part::Char('\t'), true) => (*indent + cfg.tab_stops.spaces_at(*indent), true),
            (&Part::Char(' '), true) => (*indent + 1, true),
            (&Part::Char('\n'), _) => (0, true),
            (&Part::Char(_), _) => (*indent, false),
            (_, on_indent) => (*indent, on_indent)
        };

        Some((old_indent as u16, (pos, line, part)))
    })
}

fn bits<'a>(
    iter: impl Iterator<Item = (u16, (usize, usize, Part))> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a {
    let width = width as u16;
    iter.scan((0, true, None), move |(x, next_line, prev_char), (indent, (pos, line, part))| {
        let (len, processed_part) = process_part(part, cfg, prev_char, x, width);
        *x += len;

        let surpassed_width = *x > width || (*x == width && len == 0);
        let needs_to_wrap = !cfg.wrap_method.is_no_wrap() && surpassed_width;
        let go_to_nl = ((*next_line && prev_char.is_some()) || needs_to_wrap).then(|| {
            *x = indent + len;
            *next_line = false;
            line
        });

        if let Part::Char('\n') = part {
            *next_line = true;
        }

        Some(((*x - len, len, go_to_nl), (pos, processed_part)))
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
fn words<'a>(
    iter: impl Iterator<Item = (u16, (usize, usize, Part))> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a {
    let mut iter = iter.peekable();
    let width = width as u16;
    let mut indent = 0;
    let mut word = Vec::new();

    let mut prev_char = None;
    let mut finished_word = Vec::new();
    let mut x = 0;
    let mut needs_to_wrap = true;
    std::iter::from_fn(move || {
        if let Some(unit) = finished_word.pop() {
            return words_bit(unit, indent, &mut x, &mut needs_to_wrap, &mut prev_char, width, cfg);
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
            words_bit(unit, indent, &mut x, &mut needs_to_wrap, &mut prev_char, width, cfg)
        })
    })
}

fn words_bit(
    (pos, line, part): (usize, usize, Part), indent: u16, x: &mut u16, needs_to_wrap: &mut bool,
    prev_char: &mut Option<char>, width: u16, cfg: &PrintCfg
) -> Option<((u16, u16, Option<usize>), (usize, Part))> {
    let (len, processed_part) = process_part(part, cfg, prev_char, x, width);
    let next_line = if *needs_to_wrap {
        *needs_to_wrap = false;
        *x = indent;
        Some(line)
    } else if let Part::Char(_) = part {
        if *x + len > width {
            *x = indent + len;
            Some(line)
        } else {
            *x += len;
            None
        }
    } else if *x >= width {
        *x = indent;
        Some(line)
    } else {
        None
    };

    if let Some(char) = part.as_char() {
        *needs_to_wrap = char == '\n'
    }

    Some(((*x - len, len, next_line), (pos, processed_part)))
}

fn process_part(
    part: Part, cfg: &PrintCfg, prev_char: &mut Option<char>, x: &mut u16, width: u16
) -> (u16, Part) {
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

enum Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a
{
    Parts(Bits, PhantomData<&'a ()>),
    Words(Words)
}

pub fn print_iter<'a>(
    iter: impl Iterator<Item = (usize, usize, Part)> + 'a, char_start: usize, width: usize,
    cfg: &'a PrintCfg
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a {
    let width = if let WrapMethod::Capped(cap) = cfg.wrap_method { cap } else { width };
    match cfg.wrap_method {
        WrapMethod::Width | WrapMethod::NoWrap | WrapMethod::Capped(_) => {
            let indents = indents(iter, width, cfg)
                .filter(move |(_, (pos, _, part))| *pos >= char_start || part.is_tag());
            Iter::Parts(bits(indents, width, cfg), PhantomData)
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
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a {
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
    Parts: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a
{
    type Item = ((u16, u16, Option<usize>), (usize, Part));

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Parts(parts, _) => parts.next(),
            Iter::Words(words) => words.next()
        }
    }
}
