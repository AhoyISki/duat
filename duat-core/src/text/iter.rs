use std::{
    iter::{Chain, Rev},
    ops::ControlFlow,
    str::Chars,
};

use gapbuf::GapBuffer;

use super::{
    point::TwoPoints,
    tags::{self, RawTag, TextId},
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
    #[inline]
    fn new(p: impl TwoPoints, part: Part) -> Self {
        let (real, ghost) = p.to_points();
        Self { real, ghost, part }
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
    ghost: Option<Point>,

    // Configuration on how to iterate.
    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> Iter<'a> {
    pub(super) fn new_at(text: &'a Text, p: impl TwoPoints) -> Self {
        let (real, ghost) = p.to_points();
        let real = real.min(text.max_point());
        let tags_start = real.byte().saturating_sub(text.tags.back_check());

        Self {
            text,
            point: real,
            chars: buf_chars(&text.buf, real),
            tags: text.tags.iter_at(tags_start),
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

    #[inline]
    fn handle_meta_tags(&mut self, tag: &RawTag, p: Point) -> ControlFlow<(), ()> {
        match tag {
            RawTag::GhostText(_, id) if self.print_ghosts => {
                if p < self.point || self.conceals > 0 {
                    return ControlFlow::Continue(());
                }

                let Some(text) = self.text.tags.iter_only_at(p).find_map(|tag| {
                    if let RawTag::GhostText(_, id) = tag {
                        let text = self.text.tags.texts.get(*id).unwrap();
                        if let Some(ghost) = &mut self.ghost
                            && *ghost >= text.max_point()
                        {
                            *ghost -= text.max_point();
                            None
                        } else {
                            Some(text)
                        }
                    } else {
                        None
                    }
                }) else {
                    return ControlFlow::Continue(());
                };

                let iter = text.iter_at(self.ghost.unwrap());
                let point = std::mem::replace(&mut self.point, iter.point);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.main_iter = Some((point, chars, tags));
                ControlFlow::Continue(())
            }
            RawTag::GhostText(..) => ControlFlow::Continue(()),

            RawTag::ConcealStart(_) => {
                self.conceals += 1;
                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.pos = self.pos.max(p);
                    self.line = self.text.char_to_line(p);
                    self.chars = buf_chars(&self.text.buf, self.pos);
                }

                ControlFlow::Continue(())
            }
            RawTag::Concealed(skip) => {
                let pos = p.saturating_add(*skip as usize);
                *self = Iter::new_at(self.text, pos);
                ControlFlow::Break(())
            }
            _ => ControlFlow::Break(()),
        }
    }

    pub fn on_ghost(&self) -> bool {
        self.main_iter.is_some()
    }

    pub fn points(&self) -> (Point, Option<Point>) {
        if let Some((real, ..)) = self.main_iter.as_ref() {
            (*real, Some(*real + self.point))
        } else {
            (self.point, None)
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

            if let Some(&(p, tag)) = tag.filter(|(p, _)| *p <= self.point || self.conceals > 0) {
                self.tags.next();

                if let ControlFlow::Break(_) = self.handle_meta_tags(&tag, p) {
                    break Some(Item::new(self.points(), Part::from_raw(tag)));
                }
            } else if let Some(char) = self.chars.next() {
                let points = self.points();
                self.point = self.point.move_fwd(char);

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
    pos: usize,
    line: usize,
    conceals: usize,

    backup_iter: Option<(usize, RevChars<'a>, tags::RevTags<'a>)>,
    ghosts_to_ignore: Vec<TextId>,
    ghost_shift: usize,

    // Iteration options:
    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> RevIter<'a> {
    pub fn on_ghost(&self) -> bool {
        self.backup_iter.is_some()
    }

    pub(super) fn new_at(text: &'a Text, pos: impl TwoPoints) -> Self {
        let (real, ghost) = pos.to_points();
        let mut ghosts_to_ignore = Vec::new();
        let mut ghost_shift = 0;

        let (chars, tags) = {
            let mut text_ids = text.tags.iter_only_at(real).filter_map(|tag| match tag {
                RawTag::GhostText(_, id) => Some(id),
                _ => None,
            });

            let text = text_ids.find_map(|id| {
                text.tags.texts.get(&id).and_then(|text| {
                    if ghost < text.len_bytes() {
                        ghosts_to_ignore.push(id);
                        Some(text)
                    } else {
                        ghost_shift += text.len_bytes();
                        ghost -= text.len_bytes();
                        None
                    }
                })
            });

            ghosts_to_ignore.extend(text_ids);

            text.map(|text| {
                ghost = ghost.min(text.len_bytes());
                let tags_start = ghost + text.tags.back_check();

                let chars = buf_chars_rev(&text.buf, ghost);
                let tags = text.tags.rev_iter_at(tags_start);

                (chars, tags)
            })
            .unzip()
        };

        let tags_start = real + text.tags.back_check();

        let pos = if chars.is_some() { ghost } else { real };

        let backup_iter = chars.is_some().then(|| {
            let chars = buf_chars_rev(&text.buf, real);
            let tags = text.tags.rev_iter_at(tags_start);

            (real, chars, tags)
        });

        Self {
            text,
            chars: chars.unwrap_or_else(|| buf_chars_rev(&text.buf, real)),
            tags: tags.unwrap_or_else(|| text.tags.rev_iter_at(tags_start)),
            pos,
            line: text.char_to_line(real),
            conceals: 0,

            backup_iter,
            ghosts_to_ignore,
            ghost_shift,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub(super) fn new_following(text: &'a Text, pos: impl Positional) -> Self {
        let ExactPos { real, mut ghost } = pos.ghost().clamp(text);
        let exact_pos = if text.tags.iter_only_at(real).any(|tag| {
            if let RawTag::GhostText(_, id) = tag {
                text.tags.texts.get(&id).is_some_and(|text| {
                    if ghost < text.len_bytes() {
                        true
                    } else {
                        ghost -= text.len_bytes();
                        false
                    }
                })
            } else {
                false
            }
        }) {
            ExactPos::new(real, ghost + 1)
        } else {
            ExactPos::new(real + 1, 0)
        };

        Self::new_at(text, exact_pos)
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

    #[inline]
    fn process_meta_tags(&mut self, tag: &RawTag, pos: usize) -> ControlFlow<()> {
        match tag {
            RawTag::GhostText(_, id) if self.print_ghosts => {
                if self.ghosts_to_ignore.contains(id) || pos > self.pos || self.conceals > 0 {
                    return ControlFlow::Continue(());
                }
                self.ghost_shift = 0;

                let Some(text) = self.text.tags.iter_only_at(pos).find_map(|tag| match tag {
                    RawTag::GhostText(_, cmp) if cmp == *id => self.text.tags.texts.get(id),
                    RawTag::GhostText(_, id) => {
                        let text = self.text.tags.texts.get(&id);
                        self.ghost_shift += text.map(|t| t.len_bytes()).unwrap_or(0);
                        None
                    }
                    _ => None,
                }) else {
                    return ControlFlow::Continue(());
                };

                let iter = text.rev_iter();
                let pos = std::mem::replace(&mut self.pos, iter.pos);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.backup_iter = Some((pos, chars, tags));

                ControlFlow::Continue(())
            }
            RawTag::GhostText(..) => ControlFlow::Continue(()),

            RawTag::ConcealStart(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.pos = self.pos.min(pos);
                    self.line = self.text.char_to_line(self.pos);
                    let skip = self.text.buf.len() - self.pos;
                    self.chars = buf_chars_rev(&self.text.buf, skip);
                }

                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceals += 1;

                ControlFlow::Continue(())
            }
            RawTag::Concealed(skip) => {
                self.pos = pos.saturating_sub(*skip as usize);
                self.line = self.text.char_to_line(self.pos);
                let skip = self.text.buf.len() - self.pos;
                self.chars = buf_chars_rev(&self.text.buf, skip);
                self.tags = self.text.tags.rev_iter_at(self.pos);
                self.conceals = 0;

                ControlFlow::Break(())
            }
            _ => ControlFlow::Break(()),
        }
    }
}

impl Iterator for RevIter<'_> {
    type Item = Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(pos, tag)) = tag.filter(|(pos, _)| *pos >= self.pos || self.conceals > 0)
            {
                self.tags.next();

                if let ControlFlow::Break(_) = self.process_meta_tags(&tag, pos) {
                    let pos = if let Some((real, ..)) = self.backup_iter.as_ref() {
                        ExactPos::new(*real, self.ghost_shift + self.pos)
                    } else {
                        ExactPos::new(self.pos, self.ghost_shift)
                    };

                    break Some(Item::new(pos, self.line, Part::from_raw(tag)));
                }
            } else if let Some(c) = self.chars.next() {
                self.pos -= 1;
                let pos = if let Some((real, ..)) = self.backup_iter.as_ref() {
                    ExactPos::new(*real, self.ghost_shift + self.pos)
                } else {
                    let ghost = self.ghost_shift;
                    self.ghost_shift = usize::MAX;
                    self.line -= (c == '\n') as usize;
                    ExactPos::new(self.pos, ghost)
                };

                break Some(Item::new(pos, self.line, Part::Char(c)));
            } else if let Some(last_iter) = self.backup_iter.take() {
                (self.pos, self.chars, self.tags) = last_iter;
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
        let slice_0 = std::str::from_utf8_unchecked(slice_0);
        let slice_1 = std::str::from_utf8_unchecked(slice_1);

        let skip_0 = b.min(slice_0.len());
        let skip_1 = b - skip_0;

        slice_1[..skip_1]
            .chars()
            .rev()
            .chain(slice_0[..skip_0].chars().rev())
    }
}

// To be rethought
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
