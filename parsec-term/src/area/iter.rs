use std::marker::PhantomData;

use parsec_core::text::{PrintCfg, Item};

use super::{len_from, PrintInfo};

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
fn indents<'a>(
    iter: impl Iterator<Item = (usize, usize, Item)> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = (u16, (usize, usize, Item))> + 'a {
    iter.scan((0, true), move |(indent, on_indent), (line, index, bit)| {
        let old_indent = if *indent < width { *indent } else { 0 };
        (*indent, *on_indent) = match (&bit, *on_indent) {
            (&Item::Char('\t'), true) => (*indent + cfg.tab_stops.spaces_at(*indent), true),
            (&Item::Char(' '), true) => (*indent + 1, true),
            (&Item::Char('\n'), _) => (0, true),
            (&Item::Char(_), _) => (*indent, false),
            (&Item::Tag(_), on_indent) => (*indent, on_indent)
        };

        Some((old_indent as u16, (line, index, bit)))
    })
}

fn bits<'a>(
    iter: impl Iterator<Item = (u16, (usize, usize, Item))> + 'a, width: usize,
    cfg: &'a PrintCfg
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a {
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
        if *next_line && *printed || (!cfg.wrap_method.is_no_wrap() && surpassed_width) {
            *x = indent + len;
            *next_line = false;
            return Some(((prev_x, len, Some(line)), (index, bit)));
        }

        if let Item::Char('\n') = bit {
            *next_line = true;
        }

        Some(((prev_x, len, None), (index, bit)))
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
fn words<'a>(
    iter: impl Iterator<Item = (u16, (usize, usize, Item))> + 'a, width: usize,
    cfg: &'a PrintCfg
) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a {
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
            if let &Item::Char(char) = bit {
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
    (line, index, bit): (usize, usize, Item), indent: u16, x: &mut u16, next_is_nl: &mut bool,
    width: u16, cfg: &PrintCfg
) -> Option<((u16, u16, Option<usize>), (usize, Item))> {
    let len = bit.as_char().map(|char| len_from(char, *x, width, &cfg.tab_stops)).unwrap_or(0);
    let next_line = if *next_is_nl {
        *next_is_nl = false;
        *x = indent;
        Some(line)
    } else if let Item::Char(char) = bit {
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

    if let Some(char) = bit.as_char() {
        *next_is_nl = char == '\n'
    }

    Some(((*x - len, len, next_line), (index, bit)))
}

pub enum Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a
{
    Bits(Bits, PhantomData<&'a ()>),
    Words(Words)
}

impl<'a, Bits, Words> Iter<'a, Bits, Words>
where
    Bits: Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a
{
    pub fn new(
        iter: impl Iterator<Item = (usize, usize, Item)> + 'a, skip: usize, width: usize,
        cfg: &'a PrintCfg
    ) -> impl Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))> + 'a {
        match cfg.wrap_method {
            parsec_core::text::WrapMethod::Width | parsec_core::text::WrapMethod::NoWrap => {
                let indents = indents(iter, width, cfg);
                Iter::Bits(bits(indents, width, cfg), PhantomData)
            }
            parsec_core::text::WrapMethod::Capped(cap) => {
                let indents = indents(iter, cap, cfg);
                Iter::Bits(bits(indents, cap, cfg), PhantomData)
            }
            parsec_core::text::WrapMethod::Word => {
                let indents = indents(iter, width, cfg);
                Iter::Words(words(indents, cap, cfg))
            }
        }
    }
}

impl<Bits, Words> Iterator for Iter<Bits, Words>
where
    Bits: Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))>,
    Words: Iterator<Item = ((u16, u16, Option<usize>), (usize, Item))>
{
    type Item = ((u16, u16, Option<usize>), (usize, Item));

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Bits(bits) => bits.next(),
            Iter::Words(words) => words.next()
        }
    }
}
