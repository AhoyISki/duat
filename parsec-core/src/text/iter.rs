use std::{cmp::Ordering, ops::ControlFlow};

use super::{
    tags::{self, RawTag, TextId},
    Part, Text,
};
use crate::{position::Cursor};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ExactPos {
    real: usize,
    ghost: usize,
}

impl Ord for ExactPos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.real.cmp(&other.real) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            Ordering::Equal => {
                if (self.ghost == 0 && other.ghost == usize::MAX)
                    || (self.ghost == usize::MAX && other.ghost == 0)
                {
                    Ordering::Equal
                } else {
                    self.ghost.cmp(&other.ghost)
                }
            }
        }
    }
}

impl PartialOrd for ExactPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl ExactPos {
    pub fn at_cursor_char(real: usize) -> Self {
        Self {
            real,
            ghost: usize::MAX,
        }
    }

    pub fn real(&self) -> usize {
        self.real
    }

    pub fn ghost(&self) -> usize {
        self.ghost
    }

    #[inline]
    pub(crate) fn new(real: usize, ghost: usize) -> Self {
        Self { real, ghost }
    }

    fn clamp(self, text: &Text) -> Self {
        ExactPos {
            real: self.real.min(text.len_chars()),
            ghost: self.ghost,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Item {
    pub pos: ExactPos,
    pub line: usize,
    pub part: Part,
}

impl Item {
    #[inline]
    fn new(pos: ExactPos, line: usize, part: Part) -> Self {
        Self { pos, line, part }
    }

    #[inline]
    pub fn real(&self) -> usize {
        self.pos.real()
    }

    #[inline]
    pub fn ghost(&self) -> usize {
        self.pos.ghost()
    }
}

/// An [`Iterator`] over the [`TextBit`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct Iter<'a> {
    text: &'a Text,
    chars: ropey::iter::Chars<'a>,
    tags: tags::ForwardIter<'a>,
    pos: usize,
    line: usize,
    conceals: usize,
    backup_iter: Option<(usize, ropey::iter::Chars<'a>, tags::ForwardIter<'a>)>,
    ghost_to_ignore: Option<TextId>,
    final_ghost: Option<usize>,

    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> Iter<'a> {
    pub(super) fn new_at(text: &'a Text, pos: usize) -> Self {
        let pos = pos.min(text.len_chars());
        let tags_start = pos.saturating_sub(text.tags.back_check_amount());

        Self {
            text,
            chars: text.rope.chars_at(pos),
            tags: text.tags.iter_at(tags_start),
            pos,
            line: text.rope.char_to_line(pos),
            conceals: 0,
            backup_iter: None,
            ghost_to_ignore: None,
            final_ghost: None,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub(super) fn new_exactly_at(text: &'a Text, exact_pos: ExactPos) -> Self {
        let ExactPos { real, mut ghost } = exact_pos.clamp(text);

        let (chars, (tags, text_id)) = {
            let text_id: Option<TextId> = text
                .tags
                .iter_at(real)
                .take_while(|(pos, _)| *pos <= real)
                .find_map(|(pos, tag)| match tag {
                    RawTag::GhostText(_, id) => (pos == real).then_some(id),
                    _ => None,
                });

            let (chars, tags_and_id) = text_id
                .and_then(|id| text.tags.texts.get(&id).zip(Some(id)))
                .map(|(text, id)| {
                    ghost = ghost.min(text.len_chars());
                    let tags_start = ghost.saturating_sub(text.tags.back_check_amount());

                    let chars = text.rope.chars_at(ghost);
                    let tags = text.tags.iter_at(tags_start);

                    (chars, (tags, id))
                })
                .unzip();

            (chars, tags_and_id.unzip())
        };

        let tags_start = real.saturating_sub(text.tags.back_check_amount());
        let pos = if chars.is_some() { ghost } else { real };
        let backup_iter = chars.is_some().then(|| {
            let chars = text.rope.chars_at(real);
            let tags = text.tags.iter_at(tags_start);

            (real, chars, tags)
        });

        Self {
            text,
            chars: chars.unwrap_or_else(|| text.rope.chars_at(real)),
            tags: tags.unwrap_or_else(|| text.tags.iter_at(tags_start)),
            pos,
            line: text.rope.char_to_line(real),
            conceals: 0,
            backup_iter,
            ghost_to_ignore: text_id,
            final_ghost: None,

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
    fn process_meta_tags(&mut self, tag: &RawTag, pos: usize) -> ControlFlow<(), ()> {
        match tag {
            RawTag::GhostText(_, id) if self.print_ghosts => {
                if self
                    .ghost_to_ignore
                    .is_some_and(|to_ignore| to_ignore == *id)
                {
                    return ControlFlow::Continue(());
                }

                let text = self.text.tags.texts.get(id).unwrap();
                let iter = if pos == self.pos && self.conceals == 0 {
                    text.iter()
                } else {
                    text.iter_at(text.len_chars())
                };

                let pos = std::mem::replace(&mut self.pos, iter.pos);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.backup_iter = Some((pos, chars, tags));
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
                    self.pos = self.pos.max(pos);
                    self.line = self.text.rope.char_to_line(pos);
                    self.chars = self.text.rope.chars_at(self.pos);
                }

                ControlFlow::Continue(())
            }
            RawTag::Concealed(skip) => {
                let pos = pos.saturating_add(*skip);
                *self = Iter::new_at(self.text, pos);
                ControlFlow::Break(())
            }
            _ => ControlFlow::Break(()),
        }
    }
}

impl Iterator for Iter<'_> {
    /// In order:
    ///
    /// - The position of the [`Part`] in the [`Text`], it can be [`None`], when
    ///   iterating over ghost text.
    /// - The line the [`Part`] would be situated in, given a count of `'\n'`s
    ///   before it, iterating over the unconcealed text without any ghost texts
    ///   within.
    /// - The [`Part`] itself, giving either a [`char`] or a text modifier,
    ///   which should be used to change the way the [`Text`] is printed.
    type Item = Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(pos, tag)) = tag.filter(|(pos, _)| *pos <= self.pos || self.conceals > 0)
            {
                self.tags.next();

                if let ControlFlow::Break(_) = self.process_meta_tags(&tag, pos) {
                    let part = Part::from_raw(tag);
                    if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                        let pos = ExactPos::new(pos, self.pos);
                        break Some(Item::new(pos, self.line, part));
                    } else {
                        let pos = ExactPos::new(self.pos, self.final_ghost.unwrap_or(0));
                        break Some(Item::new(pos, self.line, part));
                    }
                }
            } else if let Some(char) = self.chars.next() {
                let prev_line = self.line;

                if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                    let pos = ExactPos::new(pos, self.pos);
                    self.pos += 1;
                    break Some(Item::new(pos, prev_line, Part::Char(char)));
                } else {
                    let pos = ExactPos::new(self.pos, self.final_ghost.take().unwrap_or(0));
                    self.pos += 1;
                    self.line += (char == '\n') as usize;
                    break Some(Item::new(pos, prev_line, Part::Char(char)));
                }
            } else if let Some(backup) = self.backup_iter.take() {
                self.final_ghost = Some(self.pos);
                (self.pos, self.chars, self.tags) = backup;
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
    chars: ropey::iter::Chars<'a>,
    tags: tags::ReverseTags<'a>,
    pos: usize,
    line: usize,
    conceals: usize,
    backup_iter: Option<(usize, ropey::iter::Chars<'a>, tags::ReverseTags<'a>)>,
    ghost_to_ignore: Option<TextId>,
    final_ghost: Option<usize>,

    // Iteration options:
    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> RevIter<'a> {
    pub(super) fn new_at(text: &'a Text, pos: usize) -> Self {
        let pos = pos.min(text.len_chars());
        let tags_start = pos + text.tags.back_check_amount();
        Self {
            text,
            chars: text.rope.chars_at(pos).reversed(),
            tags: text.tags.rev_iter_at(tags_start),
            pos,
            line: text.char_to_line(pos),
            conceals: 0,
            backup_iter: None,
            ghost_to_ignore: None,
            final_ghost: None,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub(super) fn new_exactly_at(text: &'a Text, exact_pos: ExactPos) -> Self {
        let ExactPos { real, mut ghost } = exact_pos.clamp(text);

        let (chars, (tags, ghost_to_ignore)) = {
            let text = text.tags.on(real).find_map(|tag| match tag {
                RawTag::GhostText(_, id) => text.tags.texts.get(&id).zip(Some(id)),
                _ => None,
            });

            let (chars, tags_and_id) = text
                .map(|(text, id)| {
                    ghost = ghost.min(text.len_chars());
                    let tags_start = ghost + text.tags.back_check_amount();

                    let chars = text.rope.chars_at(ghost).reversed();
                    let tags = text.tags.rev_iter_at(tags_start);

                    (chars, (tags, id))
                })
                .unzip();

            (chars, tags_and_id.unzip())
        };

        let tags_start = real + text.tags.back_check_amount();

        let pos = if chars.is_some() { ghost } else { real };

        let backup_iter = chars.is_some().then(|| {
            let chars = text.rope.chars_at(real).reversed();
            let tags = text.tags.rev_iter_at(tags_start);

            (real, chars, tags)
        });

        Self {
            text,
            chars: chars.unwrap_or_else(|| text.rope.chars_at(real).reversed()),
            tags: tags.unwrap_or_else(|| text.tags.rev_iter_at(tags_start)),
            pos,
            line: text.rope.char_to_line(real),
            conceals: 0,
            backup_iter,
            ghost_to_ignore,
            final_ghost: None,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    pub(super) fn new_following(text: &'a Text, exact_pos: ExactPos) -> Self {
        let exact_pos = if let Some(text) = text.tags.on(exact_pos.real()).find_map(|tag| {
            if let RawTag::GhostText(_, text_id) = tag {
                text.tags.texts.get(&text_id)
            } else {
                None
            }
        }) {
            if exact_pos.ghost() >= text.len_chars() {
                ExactPos::new(exact_pos.real() + 1, 0)
            } else {
                ExactPos::new(exact_pos.real(), exact_pos.ghost() + 1)
            }
        } else {
            ExactPos::new(exact_pos.real() + 1, 0)
        };

        Self::new_exactly_at(text, exact_pos)
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

    pub fn dont_conceal_on_lines(self, list: &'a [Cursor]) -> Self {
        Self {
            _conceals: Conceal::NotOnLineOf(list),
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
                if self
                    .ghost_to_ignore
                    .is_some_and(|to_ignore| to_ignore == *id)
                {
                    return ControlFlow::Continue(());
                }

                let text = self.text.tags.texts.get(id).unwrap();
                if pos > self.pos || self.conceals != 0 {
                    return ControlFlow::Continue(());
                }

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
                    self.line = self.text.rope.char_to_line(self.pos);
                    self.chars = self.text.rope.chars_at(self.pos).reversed();
                }

                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceals += 1;

                ControlFlow::Continue(())
            }
            RawTag::Concealed(skip) => {
                self.pos = pos.saturating_sub(*skip);
                self.line = self.text.rope.char_to_line(self.pos);
                self.chars = self.text.rope.chars_at(self.pos).reversed();
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
                    let part = Part::from_raw(tag);
                    if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                        let pos = ExactPos::new(pos, self.pos);
                        break Some(Item::new(pos, self.line, part));
                    } else {
                        let pos = ExactPos::new(self.pos, self.final_ghost.unwrap_or(usize::MAX));
                        break Some(Item::new(pos, self.line, part));
                    }
                }
            } else if let Some(char) = self.chars.next() {
                self.pos -= 1;

                if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                    let pos = ExactPos::new(pos, self.pos);
                    break Some(Item::new(pos, self.line, Part::Char(char)));
                } else {
                    self.line -= (char == '\n') as usize;
                    let final_ghost = self.final_ghost.take();
                    let pos = ExactPos::new(self.pos, final_ghost.unwrap_or(usize::MAX));
                    break Some(Item::new(pos, self.line, Part::Char(char)));
                }
            } else if let Some(last_iter) = self.backup_iter.take() {
                self.final_ghost = Some(self.pos);
                (self.pos, self.chars, self.tags) = last_iter;
            } else {
                break None;
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
enum Conceal<'a> {
    #[default]
    All,
    None,
    Excluding(&'a [Cursor]),
    NotOnLineOf(&'a [Cursor]),
}
