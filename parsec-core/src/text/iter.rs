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

    fn peek_valid_tag(&mut self) -> Option<(usize, RawTag)> {
        self.tags.peek().filter(|(pos, _)| *pos <= self.pos || self.conceal_count > 0).cloned()
    }
}

impl Iterator for Iter<'_> {
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let mut tag = self.peek_valid_tag();
        while let Some((
            pos,
            RawTag::ConcealStart(_) | RawTag::ConcealEnd(_) | RawTag::GhostText(..)
        )) = tag
        {
            self.tags.next();

            match tag.unwrap() {
                (_, RawTag::ConcealStart(_)) => self.conceal_count += 1,
                (_, RawTag::ConcealEnd(_)) => {
                    self.conceal_count = self.conceal_count.saturating_sub(1);
                    if self.conceal_count == 0 {
                        self.pos = self.pos.max(pos);
                        self.line = self.chars.move_to(self.pos);
                    }
                }
                (_, RawTag::GhostText(id, _)) if self.ghosts => {
                    if let Some(text) = self.texts.get(id) {
                        let iter = if pos >= self.pos {
                            text.iter()
                        } else {
                            text.iter_at(text.len_chars())
                        };

                        let pos = std::mem::replace(&mut self.pos, iter.pos);
                        let chars = std::mem::replace(&mut self.chars, iter.chars);
                        let tags = std::mem::replace(&mut self.tags, iter.tags);

                        self.backup_iters.push((pos, chars, tags));
                    }
                }
                _ => {}
            }

            tag = self.peek_valid_tag()
        }

        if let Some((pos, tag)) = tag {
            if let RawTag::Concealed(skip) = tag {
                self.pos = pos.saturating_add(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceal_count = 0;
            }
            self.tags.next();
            Some((pos, self.line, Part::from(tag)))
        } else if let Some(char) = self.chars.next().or_else(|| {
            self.backup_iters.pop().and_then(|(pos, chars, tags)| {
                (self.pos, self.chars, self.tags) = (pos, chars, tags);
                self.chars.next()
            })
        }) {
            let prev_line = self.line;
            self.pos += 1;
            let pos = if let Some(pos) = self.backup_iters.first().map(|(pos, ..)| *pos) {
                pos
            } else {
                self.line += (char == '\n') as usize;
                self.pos - 1
            };
            Some((pos, prev_line, Part::Char(char)))
        } else {
            None
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
}

impl Iterator for RevIter<'_> {
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let mut tag =
            self.tags.peek().filter(|(pos, _)| *pos >= self.pos || self.conceal_count > 0).cloned();
        while let Some((
            pos,
            RawTag::ConcealStart(_) | RawTag::ConcealEnd(_) | RawTag::GhostText(..)
        )) = tag
        {
            let prev_conceal_count = self.conceal_count;

            self.tags.next();

            match tag.unwrap() {
                (_, RawTag::ConcealStart(_)) => {
                    self.conceal_count = self.conceal_count.saturating_sub(1)
                }
                (_, RawTag::ConcealEnd(_)) => self.conceal_count += 1,
                (_, RawTag::GhostText(id, _)) if self.ghosts => {
                    if let Some(text) = self.texts.get(id) {
                        let iter =
                            if pos <= self.pos { text.rev_iter() } else { text.rev_iter_at(0) };

                        let pos = std::mem::replace(&mut self.pos, iter.pos);
                        let chars = std::mem::replace(&mut self.chars, iter.chars);
                        let tags = std::mem::replace(&mut self.tags, iter.tags);

                        self.backup_iters.push((pos, chars, tags));
                    }
                }
                _ => {}
            }

            if self.conceal_count == 0 && prev_conceal_count == 1 {
                self.pos = self.pos.min(pos);
                self.line = self.chars.move_to(self.pos);
            }

            tag = self
                .tags
                .peek()
                .filter(|(pos, _)| *pos >= self.pos || self.conceal_count > 0)
                .cloned();
        }

        if let Some((pos, tag)) = tag {
            if let RawTag::Concealed(skip) = tag {
                self.pos = pos.saturating_sub(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceal_count = 0;
            }
            self.tags.next();
            Some((pos, self.line, Part::from(tag)))
        } else if let Some(char) = self.chars.next().or_else(|| {
            self.backup_iters.pop().and_then(|(pos, chars, tags)| {
                (self.pos, self.chars, self.tags) = (pos, chars, tags);
                self.chars.next()
            })
        }) {
            self.pos -= 1;
            let pos = if let Some(pos) = self.backup_iters.first().map(|(pos, ..)| *pos) {
                pos
            } else {
                self.line -= (char == '\n') as usize;
                self.pos
            };
            Some((pos, self.line, Part::Char(char)))
        } else {
            None
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
