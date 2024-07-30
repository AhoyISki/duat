use std::{
    iter::{Chain, Rev},
    str::Chars,
};

use gapbuf::GapBuffer;

use super::{
    point::TwoPoints,
    tags::{self, RawTag},
    Part, Point, Text,
};
use crate::position::Cursor;

#[derive(Debug, Clone, Copy)]
pub struct Item {
    pub real: Point,
    pub ghost: Option<Point>,
    pub part: Part,
}

impl Item {
    pub fn as_real_char(self) -> Option<(Point, char)> {
        if self.ghost.is_none() {
            Some(self.real).zip(self.part.as_char())
        } else {
            None
        }
    }

    pub fn byte(&self) -> usize {
        self.real.byte()
    }

    pub fn char(&self) -> usize {
        self.real.char()
    }

    pub fn line(&self) -> usize {
        self.real.line()
    }

    pub fn points(&self) -> (Point, Option<Point>) {
        (self.real, self.ghost)
    }

    pub fn lines(&self) -> (usize, Option<usize>) {
        (self.real.line(), self.ghost.map(|g| g.line()))
    }

    #[inline]
    fn new(tp: impl TwoPoints, part: Part) -> Self {
        let (real, ghost) = tp.to_points();
        Self { real, ghost, part }
    }
}

/// An [`Iterator`] over the [`TextBit`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct Iter<'a> {
    text: &'a Text,
    point: Point,
    chars: FwdChars<'a>,
    tags: tags::FwdTags<'a>,
    conceals: usize,

    // Things to deal with ghost text.
    main_iter: Option<(Point, FwdChars<'a>, tags::FwdTags<'a>)>,
    ghost: Option<(Point, usize)>,

    // Configuration on how to iterate.
    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> Iter<'a> {
    pub(super) fn new_at(text: &'a Text, tp: impl TwoPoints) -> Self {
        let (real, ghost) = tp.to_points();
        let point = real.min(text.max_point());

        Self {
            text,
            point,
            chars: buf_chars(&text.buf, point.byte()),
            tags: text.tags.iter_at(point.byte()),
            conceals: 0,

            main_iter: None,
            ghost: ghost.zip(Some(0)),

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub fn no_conceals(self) -> Self {
        Self {
            _conceals: Conceal::None,
            ..self
        }
    }

    pub fn dont_conceal_containing(self, list: &'a [Cursor]) -> Self {
        Self {
            _conceals: Conceal::Excluding(list),
            ..self
        }
    }

    pub fn no_ghosts(self) -> Self {
        Self {
            print_ghosts: false,
            ..self
        }
    }

    pub fn no_tags(self) -> impl Iterator<Item = Item> + 'a {
        self.filter(|item| item.part.is_char())
    }

    pub fn skip_to(&mut self, tp: impl TwoPoints) {
        *self = self.text.iter_at(tp.to_points().max(self.points()))
    }

    #[inline]
    fn handled_meta_tag(&mut self, tag: &RawTag, b: usize) -> bool {
        match tag {
            RawTag::GhostText(_, id) => {
                if !self.print_ghosts || b < self.point.byte() || self.conceals > 0 {
                    return true;
                }
                let text = self.text.tags.get_text(id).unwrap();

                let (this_ghost, total_ghost) = if let Some((ghost, dist)) = &mut self.ghost {
                    if ghost.byte() >= *dist + text.len_bytes() {
                        *dist += text.len_bytes();
                        return true;
                    }
                    (text.point_at(ghost.byte() - *dist), *ghost)
                } else {
                    (Point::default(), Point::default())
                };

                let iter = text.iter_at(this_ghost);
                let point = std::mem::replace(&mut self.point, this_ghost);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.ghost = Some((total_ghost, total_ghost.byte()));
                self.main_iter = Some((point, chars, tags));
            }

            RawTag::StartConceal(_) => {
                self.conceals += 1;
            }
            RawTag::EndConceal(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.point = self.point.max(self.text.point_at(b));
                    self.chars = buf_chars(&self.text.buf, self.point.byte());
                }
            }
            RawTag::Concealed(skip) => {
                let b = b.saturating_add(*skip as usize);
                let point = self.text.point_at(b);
                *self = Iter::new_at(self.text, point);
                return false;
            }
            _ => return false,
        }

        true
    }

    pub fn on_ghost(&self) -> bool {
        self.main_iter.is_some()
    }

    pub fn points(&self) -> (Point, Option<Point>) {
        if let Some((real, ..)) = self.main_iter.as_ref() {
            (*real, self.ghost.map(|(tg, _)| tg))
        } else {
            (self.point, self.ghost.map(|(p, _)| p))
        }
    }
}

impl Iterator for Iter<'_> {
    /// In order:
    ///
    /// - The position of the [`Part`] in the [`Text`], it can be
    ///   [`None`], when iterating over ghost text.
    /// - The line the [`Part`] would be situated in, given a count of
    ///   `'\n'`s before it, iterating over the unconcealed text
    ///   without any ghost texts within.
    /// - The [`Part`] itself, giving either a [`char`] or a text
    ///   modifier, which should be used to change the way the
    ///   [`Text`] is printed.
    type Item = Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(b, tag)) = tag
                && (b <= self.point.byte() || self.conceals > 0)
            {
                self.tags.next();

                if !self.handled_meta_tag(&tag, b) {
                    break Some(Item::new(self.points(), Part::from_raw(tag)));
                }
            } else if let Some(char) = self.chars.next() {
                let points = self.points();
                self.point = self.point.fwd(char);

                self.ghost = match self.main_iter {
                    Some(..) => self.ghost.map(|(g, d)| (g.fwd(char), d + char.len_utf8())),
                    None => None,
                };

                break Some(Item::new(points, Part::Char(char)));
            } else if let Some(backup) = self.main_iter.take() {
                (self.point, self.chars, self.tags) = backup;
            } else {
                break None;
            }
        }
    }
}

/// An [`Iterator`] over the [`Part`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct RevIter<'a> {
    text: &'a Text,
    chars: RevChars<'a>,
    tags: tags::RevTags<'a>,
    point: Point,
    conceals: usize,

    main_iter: Option<(Point, RevChars<'a>, tags::RevTags<'a>)>,
    ghost: Option<(Point, usize)>,

    // Iteration options:
    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> RevIter<'a> {
    pub(super) fn new_at(text: &'a Text, tp: impl TwoPoints) -> Self {
        let (real, ghost) = tp.to_points();
        let point = real.min(text.max_point());

        let ghost = ghost.map(|ghost| {
            let dist = text.tags.ghosts_total_at(real.byte()).unwrap().byte();
            (ghost, dist)
        });

        Self {
            text,
            chars: buf_chars_rev(&text.buf, point.byte()),
            tags: text.tags.rev_iter_at(point.byte()),
            point,
            conceals: 0,

            main_iter: None,
            ghost,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub fn no_conceals(self) -> Self {
        Self {
            _conceals: Conceal::None,
            ..self
        }
    }

    pub fn no_ghosts(self) -> Self {
        Self {
            print_ghosts: false,
            ..self
        }
    }

    pub fn no_tags(self) -> impl Iterator<Item = Item> + 'a {
        self.filter(|item| item.part.is_char())
    }

    #[inline]
    fn handled_meta_tag(&mut self, tag: &RawTag, b: usize) -> bool {
        match tag {
            RawTag::GhostText(_, id) => {
                if !self.print_ghosts || b > self.point.byte() || self.conceals > 0 {
                    return true;
                }
                let text = self.text.tags.get_text(id).unwrap();

                let (this_ghost, total_ghost) = if let Some((total, dist)) = &mut self.ghost {
                    if *dist - text.len_bytes() >= total.byte() {
                        *dist -= text.len_bytes();
                        return true;
                    }
                    (
                        text.point_at(total.byte() + text.len_bytes() - *dist),
                        *total,
                    )
                } else {
                    let this = text.max_point();
                    let total = self.text.tags.ghosts_total_at(b);
                    (this, total.unwrap())
                };

                let iter = text.rev_iter_at(this_ghost);
                let point = std::mem::replace(&mut self.point, total_ghost);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.ghost = Some((total_ghost, total_ghost.byte()));
                self.main_iter = Some((point, chars, tags));
            }

            RawTag::StartConceal(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.point = self.point.min(self.text.point_at(b));
                    self.chars = buf_chars_rev(&self.text.buf, self.point.byte());
                }
            }
            RawTag::EndConceal(_) => self.conceals += 1,
            RawTag::Concealed(skip) => {
                let b = b.saturating_sub(*skip as usize);
                let point = self.text.point_at(b);
                *self = RevIter::new_at(self.text, point);
                return false;
            }
            _ => return false,
        }

        true
    }

    pub fn points(&self) -> (Point, Option<Point>) {
        if let Some((real, ..)) = self.main_iter.as_ref() {
            (*real, Some(self.point))
        } else {
            (self.point, self.ghost.map(|(p, _)| p))
        }
    }

    pub fn is_on_ghost(&self) -> bool {
        self.main_iter.is_some()
    }
}

impl Iterator for RevIter<'_> {
    type Item = Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(b, tag)) =
                tag.filter(|(b, _)| *b >= self.point.byte() || self.conceals > 0)
            {
                self.tags.next();

                if !self.handled_meta_tag(&tag, b) {
                    break Some(Item::new(self.points(), Part::from_raw(tag)));
                }
            } else if let Some(char) = self.chars.next() {
                self.point = self.point.rev(char);

                self.ghost = match self.main_iter {
                    Some(..) => self.ghost.map(|(g, d)| (g.rev(char), d - char.len_utf8())),
                    None => None,
                };

                break Some(Item::new(self.points(), Part::Char(char)));
            } else if let Some(last_iter) = self.main_iter.take() {
                (self.point, self.chars, self.tags) = last_iter;
            } else {
                break None;
            }
        }
    }
}

fn buf_chars(buf: &GapBuffer<u8>, b: usize) -> FwdChars {
    unsafe {
        let (slice_0, slice_1) = buf.as_slices();
        let slice_0 = std::str::from_utf8_unchecked(slice_0);
        let slice_1 = std::str::from_utf8_unchecked(slice_1);

        let skip_0 = b.min(slice_0.len());
        let skip_1 = b - skip_0;

        slice_0[skip_0..].chars().chain(slice_1[skip_1..].chars())
    }
}

fn buf_chars_rev(buf: &GapBuffer<u8>, b: usize) -> RevChars {
    unsafe {
        let (slice_0, slice_1) = buf.as_slices();
        let s0 = std::str::from_utf8_unchecked(slice_0);
        let s1 = std::str::from_utf8_unchecked(slice_1);

        let skip_0 = b.min(s0.len());
        let skip_1 = b - skip_0;

        s1[..skip_1].chars().rev().chain(s0[..skip_0].chars().rev())
    }
}

// To be rethought
#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
enum Conceal<'a> {
    #[default]
    All,
    None,
    Excluding(&'a [Cursor]),
    NotOnLineOf(&'a [Cursor]),
}

type FwdChars<'a> = Chain<Chars<'a>, Chars<'a>>;
type RevChars<'a> = Chain<Rev<Chars<'a>>, Rev<Chars<'a>>>;
