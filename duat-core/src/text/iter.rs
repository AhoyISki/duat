use std::{
    iter::{Chain, Rev},
    str::Chars,
};

use gapbuf::GapBuffer;

use super::{
    Point, Text, ToggleId, TwoPoints,
    tags::{self, RawTag},
};
use crate::mode::Cursor;

#[derive(Clone, Copy)]
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

/// An [`Iterator`] over the [`Part`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct Iter<'a> {
    text: &'a Text,
    point: Point,
    chars: FwdChars<'a>,
    tags: tags::FwdTags<'a>,
    conceals: u32,

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
        let point = real.min(text.len());

        let ghost =
            ghost.and_then(|ghost| text.tags.ghosts_total_at(real.byte()).and(Some((ghost, 0))));

        Self {
            text,
            point,
            chars: buf_chars(&text.buf, point.byte()),
            tags: text.tags_fwd(point.byte()),
            conceals: 0,

            main_iter: None,
            ghost,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub fn no_conceals(self) -> Self {
        Self { _conceals: Conceal::None, ..self }
    }

    pub fn dont_conceal_containing(self, list: &'a [Cursor]) -> Self {
        Self {
            _conceals: Conceal::Excluding(list),
            ..self
        }
    }

    pub fn no_ghosts(self) -> Self {
        Self { print_ghosts: false, ..self }
    }

    pub fn no_tags(self) -> impl Iterator<Item = Item> + 'a {
        self.filter(|item| item.part.is_char())
    }

    pub fn skip_to(&mut self, tp: impl TwoPoints) {
        *self = self.text.iter_fwd(tp.to_points().max(self.points()))
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
                    if ghost.byte() >= *dist + text.len().byte() {
                        *dist += text.len().byte();
                        return true;
                    }
                    (text.point_at(ghost.byte() - *dist), *ghost)
                } else {
                    (Point::default(), Point::default())
                };

                let iter = text.iter_fwd(this_ghost);
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
                    // If we have moved forward and were in a ghost, that ghost is no
                    // longer valid.
                    self.ghost.take_if(|_| self.point.byte() < b);
                    self.point = self.point.max(self.text.point_at(b));
                    self.chars = buf_chars(&self.text.buf, self.point.byte());
                }
            }
            RawTag::ConcealUntil(b) => {
                let point = self.text.point_at(*b as usize);
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
        let point = real.min(text.len());

        let ghost = ghost.and_then(|offset| {
            text.tags
                .ghosts_total_at(real.byte())
                .map(|ghost| (offset, ghost.byte()))
        });

        Self {
            text,
            chars: buf_chars_rev(&text.buf, point.byte()),
            tags: text.tags_rev(point.byte()),
            point,
            conceals: 0,

            main_iter: None,
            ghost,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub fn no_conceals(self) -> Self {
        Self { _conceals: Conceal::None, ..self }
    }

    pub fn no_ghosts(self) -> Self {
        Self { print_ghosts: false, ..self }
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

                let (ghost_b, offset) = if let Some((offset, dist)) = &mut self.ghost {
                    if *dist - text.len().byte() >= offset.byte() {
                        *dist -= text.len().byte();
                        return true;
                    }
                    (
                        text.point_at(offset.byte() + text.len().byte() - *dist),
                        *offset,
                    )
                } else {
                    let this = text.len();
                    let total = self.text.tags.ghosts_total_at(b);
                    (this, total.unwrap())
                };

                let iter = text.iter_rev(ghost_b);
                let point = std::mem::replace(&mut self.point, offset);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.ghost = Some((offset, offset.byte()));
                self.main_iter = Some((point, chars, tags));
            }

            RawTag::StartConceal(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.ghost.take_if(|_| b < self.point.byte());
                    self.point = self.point.min(self.text.point_at(b));
                    self.chars = buf_chars_rev(&self.text.buf, self.point.byte());
                }
            }
            RawTag::EndConceal(_) => self.conceals += 1,
            RawTag::ConcealUntil(b) => {
                let point = self.text.point_at(*b as usize);
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

            if let Some(&(b, tag)) = tag
                && (b >= self.point.byte() || self.conceals > 0)
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

use crate::form::FormId;

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
///
/// This type is used in iteration by [`Ui`]s in order to
/// correctly print Duat's content. Additionally, you may be
/// able to tell that there is no ghost text or concealment
/// tags, and there is a [`ResetState`].
///
/// That is because the [`Text`]'s iteration process automatically
/// gets rid of these tags, since, from the point of view of the
/// ui, ghost text is just regular text, while conceals are
/// simply the lack of text. And if the ui can handle printing
/// regular text, printing ghost text should be a breeze.
///
/// [`Text`]: super::Text
/// [`Tag`]: super::Tag
/// [`Ui`]: crate::ui::Ui
/// [`ResetState`]: Part::ResetState
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Part {
    Char(char),
    PushForm(FormId),
    PopForm(FormId),
    MainCursor,
    ExtraCursor,
    AlignLeft,
    AlignCenter,
    AlignRight,
    ToggleStart(ToggleId),
    ToggleEnd(ToggleId),
    ResetState,
}

impl Part {
    /// Returns a new [`Part`] from a [`RawTag`]
    #[inline]
    pub(super) fn from_raw(value: RawTag) -> Self {
        match value {
            RawTag::PushForm(_, id) => Part::PushForm(id),
            RawTag::PopForm(_, id) => Part::PopForm(id),
            RawTag::MainCursor(_) => Part::MainCursor,
            RawTag::ExtraCursor(_) => Part::ExtraCursor,
            RawTag::StartAlignCenter(_) => Part::AlignCenter,
            RawTag::EndAlignCenter(_) => Part::AlignLeft,
            RawTag::StartAlignRight(_) => Part::AlignRight,
            RawTag::EndAlignRight(_) => Part::AlignLeft,
            RawTag::ToggleStart(_, id) => Part::ToggleStart(id),
            RawTag::ToggleEnd(_, id) => Part::ToggleEnd(id),
            RawTag::ConcealUntil(_) => Part::ResetState,
            RawTag::StartConceal(_) | RawTag::EndConceal(_) | RawTag::GhostText(..) => {
                unreachable!("These tags are automatically processed elsewhere.")
            }
        }
    }

    /// Returns `true` if the part is [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[must_use]
    #[inline]
    pub fn is_char(&self) -> bool {
        matches!(self, Part::Char(_))
    }

    /// Returns [`Some`] if the part is [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[inline]
    pub fn as_char(&self) -> Option<char> {
        if let Self::Char(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the part is not [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[inline]
    pub fn is_tag(&self) -> bool {
        !self.is_char()
    }
}
