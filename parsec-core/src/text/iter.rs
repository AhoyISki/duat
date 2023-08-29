use std::ops::ControlFlow;

use super::{
    chars,
    tags::{self, RawTag},
    Part, Text
};
use crate::position::Cursor;

#[derive(Debug, Clone, Copy)]
pub struct Item {
    pub pos: usize,
    pub line: usize,
    pub ghost_pos: Option<usize>,
    pub part: Part
}

impl Item {
    fn new(pos: usize, line: usize, ghost_pos: Option<usize>, part: Part) -> Self {
        Self { pos, line, ghost_pos, part }
    }
}

/// An [`Iterator`] over the [`TextBit`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct Iter<'a> {
    chars: chars::Iter<'a>,
    tags: tags::Iter<'a>,
    pos: usize,
    line: usize,
    conceals: usize,
    texts: &'a [Text],
    backup_iter: Option<(usize, chars::Iter<'a>, tags::Iter<'a>)>,
    ghost_pos: usize,

    print_ghosts: bool,
    _conceals: Conceal<'a>
}

impl<'a> Iter<'a> {
    pub(super) fn new(
        chars: chars::Iter<'a>, tags: tags::Iter<'a>, texts: &'a [Text], pos: usize, line: usize
    ) -> Self {
        Self {
            chars,
            tags,
            pos,
            line,
            conceals: 0,
            texts,
            backup_iter: None,
            ghost_pos: 0,
            print_ghosts: true,
            _conceals: Conceal::All
        }
    }

    pub fn no_conceals(self) -> Self {
        Self { _conceals: Conceal::None, ..self }
    }

    pub fn dont_conceal_containing(self, list: &'a [Cursor]) -> Self {
        Self { _conceals: Conceal::Excluding(list), ..self }
    }

    pub fn no_ghosts(self) -> Self {
        Self { print_ghosts: false, ..self }
    }

    #[inline(always)]
    fn process_meta_tags(&mut self, tag: RawTag, pos: usize) -> ControlFlow<(), ()> {
        match tag {
            RawTag::ConcealStart(_) => {
                self.conceals += 1;
                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.pos = self.pos.max(pos);
                    self.line = self.chars.move_to(self.pos);
                }

                ControlFlow::Continue(())
            }
            RawTag::GhostText(id, _) => {
                if self.print_ghosts && let Some(text) = self.texts.get(usize::from(id)) {
                    let iter = if pos >= self.pos && self.conceals == 0 {
                        text.iter()
                    } else {
                        text.iter_at(text.len_chars())
                    };

                    let pos = std::mem::replace(&mut self.pos, iter.pos);
                    let chars = std::mem::replace(&mut self.chars, iter.chars);
                    let tags = std::mem::replace(&mut self.tags, iter.tags);

                    self.backup_iter = Some((pos, chars, tags));
                }

                ControlFlow::Continue(())
            }

            RawTag::Concealed(skip) => {
                self.pos = pos.saturating_add(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceals = 0;
                ControlFlow::Break(())
            }
            _ => ControlFlow::Break(())
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

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(pos, tag)) = tag.filter(|(pos, _)| *pos <= self.pos || self.conceals > 0)
            {
                self.tags.next();

                if let ControlFlow::Break(_) = self.process_meta_tags(tag, pos) {
                    if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                        break Some(Item::new(pos, self.line, Some(self.ghost_pos), Part::from(tag)));
                    } else {
                        break Some(Item::new(self.pos, self.line, None, Part::from(tag)));
                    }
                }
            } else if let Some(char) = self.chars.next() {
                let prev_line = self.line;
                self.pos += 1;

                if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                    self.ghost_pos += 1;
                    break Some(Item::new(pos, prev_line, Some(self.ghost_pos - 1), Part::Char(char)));
                } else {
                    self.line += (char == '\n') as usize;
                    break Some(Item::new(self.pos - 1, prev_line, None, Part::Char(char)));
                }
            } else if let Some((pos, chars, tags)) = self.backup_iter.take() {
                self.ghost_pos = 0;
                (self.pos, self.chars, self.tags) = (pos, chars, tags);
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
    chars: chars::Iter<'a>,
    tags: tags::RevIter<'a>,
    pos: usize,
    line: usize,
    conceals: usize,
    texts: &'a [Text],
    backup_iter: Option<(usize, chars::Iter<'a>, tags::RevIter<'a>)>,
    ghost_pos: usize,

    // Iteration options:
    print_ghosts: bool,
    _conceals: Conceal<'a>
}

impl<'a> RevIter<'a> {
    pub(super) fn new(
        chars: chars::Iter<'a>, tags: tags::RevIter<'a>, texts: &'a [Text], pos: usize, line: usize
    ) -> Self {
        Self {
            chars,
            tags,
            pos,
            line,
            conceals: 0,
            texts,
            backup_iter: None,
            ghost_pos: 0,
            print_ghosts: true,
            _conceals: Conceal::All
        }
    }

    pub fn no_conceals(self) -> Self {
        Self { _conceals: Conceal::None, ..self }
    }

    pub fn dont_conceal_containing(self, list: &'a [Cursor]) -> Self {
        Self { _conceals: Conceal::Excluding(list), ..self }
    }

    pub fn dont_conceal_on_lines(self, list: &'a [Cursor]) -> Self {
        Self { _conceals: Conceal::NotOnLineOf(list), ..self }
    }

    pub fn no_ghosts(self) -> Self {
        Self { print_ghosts: false, ..self }
    }

    #[inline(always)]
    fn process_meta_tags(&mut self, tag: RawTag, pos: usize) -> ControlFlow<()> {
        match tag {
            RawTag::ConcealStart(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.pos = self.pos.min(pos);
                    self.line = self.chars.move_to(self.pos);
                }

                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceals += 1;

                ControlFlow::Continue(())
            }
            RawTag::GhostText(id, _) => {
                if self.print_ghosts && let Some(text) = self.texts.get(usize::from(id)) {
                    let iter = if pos <= self.pos && self.conceals == 0 {
                        self.ghost_pos = text.len_chars();
                        text.rev_iter()
                    } else {
                        text.rev_iter_at(0)
                    };

                    let pos = std::mem::replace(&mut self.pos, iter.pos);
                    let chars = std::mem::replace(&mut self.chars, iter.chars);
                    let tags = std::mem::replace(&mut self.tags, iter.tags);

                    self.backup_iter = Some((pos, chars, tags));
                }

                ControlFlow::Continue(())
            }
            RawTag::Concealed(skip) => {
                self.pos = pos.saturating_sub(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceals = 0;

                ControlFlow::Break(())
            }
            _ => ControlFlow::Break(())
        }
    }
}

impl Iterator for RevIter<'_> {
    type Item = Item;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(pos, tag)) = tag.filter(|(pos, _)| *pos >= self.pos || self.conceals > 0)
            {
                self.tags.next();

                if let ControlFlow::Break(_) = self.process_meta_tags(tag, pos) {
                    if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                        break Some(Item::new(pos, self.line, Some(self.ghost_pos), Part::from(tag)));
                    } else {
                        break Some(Item::new(self.pos, self.line, None, Part::from(tag)));
                    }
                }
            } else if let Some(char) = self.chars.next() {
                self.pos -= 1;

                if let Some(pos) = self.backup_iter.as_ref().map(|(pos, ..)| *pos) {
                    self.ghost_pos -= 1;
                    break Some(Item::new(pos, self.line, Some(self.ghost_pos), Part::Char(char)));
                } else {
                    self.line -= (char == '\n') as usize;
                    break Some(Item::new(self.pos, self.line, None, Part::Char(char)));
                }
            } else if let Some(last_iter) = self.backup_iter.take() {
                self.ghost_pos = 0;
                (self.pos, self.chars, self.tags) = last_iter;
            } else {
                break None;
            }
        }
    }
}

#[derive(Clone, Default)]
enum Conceal<'a> {
    #[default]
    All,
    None,
    Excluding(&'a [Cursor]),
    NotOnLineOf(&'a [Cursor])
}
