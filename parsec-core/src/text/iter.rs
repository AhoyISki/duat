use std::ops::ControlFlow;

use super::{
    chars,
    tags::{self, RawTag},
    Part, Text
};
use crate::position::Cursor;

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
    conceal_count: usize,
    texts: &'a [Text],
    backup_iters: Vec<(usize, chars::Iter<'a>, tags::Iter<'a>)>,

    ghosts: bool,
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
            conceal_count: 0,
            texts,
            backup_iters: Vec::new(),
            ghosts: true,
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
        Self { ghosts: false, ..self }
    }

	#[inline(always)]
    fn process_meta_tags(&mut self, tag: RawTag, pos: usize) -> ControlFlow<(), ()> {
        match tag {
            RawTag::ConcealStart(_) => {
                self.conceal_count += 1;
                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceal_count = self.conceal_count.saturating_sub(1);
                if self.conceal_count == 0 {
                    self.pos = self.pos.max(pos);
                    self.line = self.chars.move_to(self.pos);
                }

                ControlFlow::Continue(())
            }
            RawTag::GhostText(id, _) => {
                if self.ghosts && let Some(text) = self.texts.get(id) {
                    let iter =
                        if pos >= self.pos { text.iter() } else { text.iter_at(text.len_chars()) };

                    let pos = std::mem::replace(&mut self.pos, iter.pos);
                    let chars = std::mem::replace(&mut self.chars, iter.chars);
                    let tags = std::mem::replace(&mut self.tags, iter.tags);

                    self.backup_iters.push((pos, chars, tags));
                }

                ControlFlow::Continue(())
            }

            RawTag::Concealed(skip) => {
                self.pos = pos.saturating_add(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceal_count = 0;
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
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(pos, tag)) =
                tag.filter(|(pos, _)| *pos <= self.pos || self.conceal_count > 0)
            {
                self.tags.next();

                if let ControlFlow::Break(_) = self.process_meta_tags(tag, pos) {
                    break Some((pos, self.line, Part::from(tag)));
                }
            } else if let Some(char) = self.chars.next() {
                let prev_line = self.line;
                self.pos += 1;
                let pos = if let Some(pos) = self.backup_iters.first().map(|(pos, ..)| *pos) {
                    pos
                } else {
                    self.line += (char == '\n') as usize;
                    self.pos - 1
                };

                break Some((pos, prev_line, Part::Char(char)));
            } else if let Some((pos, chars, tags)) = self.backup_iters.pop() {
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
    conceal_count: usize,
    texts: &'a [Text],
    backup_iters: Vec<(usize, chars::Iter<'a>, tags::RevIter<'a>)>,

    // Iteration options:
    ghosts: bool,
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
            conceal_count: 0,
            texts,
            backup_iters: Vec::new(),
            ghosts: true,
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
        Self { ghosts: false, ..self }
    }

	#[inline(always)]
    fn process_meta_tags(&mut self, tag: RawTag, pos: usize) -> ControlFlow<()> {
        match tag {
            RawTag::ConcealStart(_) => {
                self.conceal_count = self.conceal_count.saturating_sub(1);
                if self.conceal_count == 0 {
                    self.pos = self.pos.min(pos);
                    self.line = self.chars.move_to(self.pos);
                }

                ControlFlow::Continue(())
            }
            RawTag::ConcealEnd(_) => {
                self.conceal_count += 1;

                ControlFlow::Continue(())
            }
            RawTag::GhostText(id, _) => {
                if self.ghosts && let Some(text) = self.texts.get(id) {
                    let iter = if pos <= self.pos { text.rev_iter() } else { text.rev_iter_at(0) };

                    let pos = std::mem::replace(&mut self.pos, iter.pos);
                    let chars = std::mem::replace(&mut self.chars, iter.chars);
                    let tags = std::mem::replace(&mut self.tags, iter.tags);

                    self.backup_iters.push((pos, chars, tags));
                }

                ControlFlow::Continue(())
            }
            RawTag::Concealed(skip) => {
                self.pos = pos.saturating_sub(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceal_count = 0;

                ControlFlow::Break(())
            }
            _ => ControlFlow::Break(())
        }
    }
}

impl Iterator for RevIter<'_> {
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let tag = self.tags.peek();

            if let Some(&(pos, tag)) =
                tag.filter(|(pos, _)| *pos >= self.pos || self.conceal_count > 0)
            {
                self.tags.next();

                if let ControlFlow::Break(_) = self.process_meta_tags(tag, pos) {
                    break Some((pos, self.line, Part::from(tag)));
                }
            } else if let Some(char) = self.chars.next() {
                self.pos -= 1;
                let pos = if let Some(pos) = self.backup_iters.first().map(|(pos, ..)| *pos) {
                    pos
                } else {
                    self.line -= (char == '\n') as usize;
                    self.pos
                };

                break Some((pos, self.line, Part::Char(char)));
            } else if let Some((pos, chars, tags)) = self.backup_iters.pop() {
                (self.pos, self.chars, self.tags) = (pos, chars, tags);
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
