use std::marker::PhantomData;

use parsec_core::text::{Part, PrintCfg, WrapMethod};

use super::len_from;

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
fn indents<'a>(
    iter: impl Iterator<Item = (usize, usize, Part)> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = (u16, (usize, usize, Part))> + 'a {
    iter.scan((0, true), move |(indent, on_indent), (line, index, bit)| {
        let old_indent = if *indent < width { *indent } else { 0 };
        (*indent, *on_indent) = match (&bit, *on_indent) {
            (&Part::Char('\t'), true) => (*indent + cfg.tab_stops.spaces_at(*indent), true),
            (&Part::Char(' '), true) => (*indent + 1, true),
            (&Part::Char('\n'), _) => (0, true),
            (&Part::Char(_), _) => (*indent, false),
            (_, on_indent) => (*indent, on_indent)
        };

        Some((old_indent as u16, (line, index, bit)))
    })
}

fn bits<'a>(
    iter: impl Iterator<Item = (u16, (usize, usize, Part))> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a {
    let width = width as u16;
    iter.scan((0, true, false), move |(x, next_line, printed), (indent, (line, index, bit))| {
        let prev_x = *x;
        let len = bit
            .as_char()
            .map(|char| {
                *printed = true;
                len_from(char, *x, width, &cfg.tab_stops)
            })
            .unwrap_or(0);
        *x += len;

        let surpassed_width = *x > width || (*x == width && len == 0);
        if (*next_line && *printed) || (!cfg.wrap_method.is_no_wrap() && surpassed_width) {
            *x = indent + len;
            *next_line = false;
            return Some(((prev_x, len, Some(line)), (index, bit)));
        }

        if let Part::Char('\n') = bit {
            *next_line = true;
        }

        Some(((prev_x, len, None), (index, bit)))
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

    let mut finished_word = Vec::new();
    let mut x = 0;
    let mut next_is_nl = true;
    std::iter::from_fn(move || {
        if let Some(unit) = finished_word.pop() {
            return words_bit(unit, indent, &mut x, &mut next_is_nl, width, cfg);
        }

        let mut word_len = 0;
        while let Some((new_indent, (.., bit))) = iter.peek() {
            if let &Part::Char(char) = bit {
                indent = *new_indent;
                if cfg.word_chars.contains(char) {
                    word_len += len_from(char, x + word_len, width, &cfg.tab_stops)
                } else {
                    word.push(iter.next().map(|(_, unit)| unit).unwrap());
                    break;
                }
            }
            word.push(iter.next().map(|(_, unit)| unit).unwrap());
        }

        next_is_nl |= x + word_len > width;

        std::mem::swap(&mut word, &mut finished_word);
        finished_word.reverse();
        finished_word
            .pop()
            .and_then(|unit| words_bit(unit, indent, &mut x, &mut next_is_nl, width, cfg))
    })
}

fn words_bit(
    (line, index, part): (usize, usize, Part), indent: u16, x: &mut u16, next_is_nl: &mut bool,
    width: u16, cfg: &PrintCfg
) -> Option<((u16, u16, Option<usize>), (usize, Part))> {
    let len = part.as_char().map(|char| len_from(char, *x, width, &cfg.tab_stops)).unwrap_or(0);
    let next_line = if *next_is_nl {
        *next_is_nl = false;
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
        *next_is_nl = char == '\n'
    }

    Some(((*x - len, len, next_line), (index, part)))
}

pub enum Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a
{
    Bits(Bits, PhantomData<&'a ()>),
    Words(Words)
}

pub fn print_iter<'a>(
    iter: impl Iterator<Item = (usize, usize, Part)> + 'a, char_start: usize, width: usize,
    cfg: &'a PrintCfg
) -> Iter<
    'a,
    impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a,
    impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a
> {
    let width = if let WrapMethod::Capped(cap) = cfg.wrap_method { cap } else { width };
    match cfg.wrap_method {
        WrapMethod::Width | WrapMethod::NoWrap | WrapMethod::Capped(_) => {
            let indents = indents(iter, width, cfg)
                .filter(move |(_, (pos, _, part))| *pos >= char_start || part.is_tag());
            Iter::Bits(bits(indents, width, cfg), PhantomData)
        }
        WrapMethod::Word => {
            let indents = indents(iter, width, cfg)
                .filter(move |(_, (pos, _, part))| *pos >= char_start || part.is_tag());
            Iter::Words(words(indents, width, cfg))
        }
    }
}

impl<'a, Bits, Words> Iterator for Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Part))> + 'a
{
    type Item = ((u16, u16, Option<usize>), (usize, Part));

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Bits(bits, _) => bits.next(),
            Iter::Words(words) => words.next()
        }
    }
}
