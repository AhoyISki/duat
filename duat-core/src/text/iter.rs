//! The functions for iteration of [`Text`]s
//!
//! These functions will iterate over the text, reverse or forwards,
//! keeping track of characters and [`Tag`]s, sending them in order,
//! while also hiding the existance of certain "meta" tags, namely the
//! [ghost] and [concealment] tags. This allows for a seemless
//! iteration which is especially useful for printing, as the printer
//! only needs to care about [`char`]s and [`Tag`]s, most of which are
//! just [`Form`] changing [`Tag`]s
//!
//! [`Tag`]: super::Tag
//! [ghost]: super::Ghost
//! [concealment]: super::StartConceal
//! [`Form`]: crate::form::Form
use std::{
    iter::{Chain, Rev},
    str::Chars,
};

use super::{
    Point, Text, ToggleId, TwoPoints,
    tags::{self, RawTag},
};
use crate::mode::Selection;

/// An [`Iterator`] over the [`Part`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct FwdIter<'a> {
    text: &'a Text,
    point: Point,
    init_point: Point,
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

impl<'a> FwdIter<'a> {
    /// Returns a new forward [`Iterator`] over the [`Item`]s in the
    /// [`Text`]
    pub(super) fn new_at(text: &'a Text, tp: impl TwoPoints) -> Self {
        let (r, g) = tp.to_points();
        let point = r.min(text.len());

        let ghost = g.and_then(|offset| {
            let (_, max) = text.ghost_max_points_at(r.byte());
            max.map(|max| (max.min(offset), 0))
        });

        Self {
            text,
            point,
            init_point: point,
            chars: buf_chars_fwd(text, point.byte()),
            tags: text.tags_fwd(point.byte()),
            conceals: 0,

            main_iter: None,
            ghost,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    ////////// Iteration modifiers

    /// Disable all [`Conceal`]s
    ///
    /// [`Conceal`]: super::Conceal
    pub fn no_conceals(self) -> Self {
        Self { _conceals: Conceal::None, ..self }
    }

    /// Disable all [`Conceal`]s containing any of the
    /// `caret`s
    ///
    /// Not yet implemented
    ///
    /// [`Conceal`]: super::Conceal
    pub fn dont_conceal_containing(self, list: &'a [Selection]) -> Self {
        Self {
            _conceals: Conceal::Excluding(list),
            ..self
        }
    }

    /// Disable all [`Ghost`]s
    ///
    /// [`Ghost`]: super::Ghost
    pub fn no_ghosts(self) -> Self {
        Self { print_ghosts: false, ..self }
    }

    /// Returns an [`Iterator`] over only the `char`s
    ///
    /// The difference betwen this and a regular [`Text::chars_fwd`]
    /// is that this [`Iterator`] will return [`Ghost`] `char`s and
    /// won't return `char`s that have been concealed
    ///
    /// [`Tag`]: super::Tag
    /// [`Ghost`]: super::Ghost
    pub fn no_tags(self) -> impl Iterator<Item = Item> + 'a {
        self.filter(|item| item.part.is_char())
    }

    /// Skips to a certain [`TwoPoints`]
    ///
    /// Does nothing if the [`TwoPoints`] are behind.
    pub fn skip_to(&mut self, tp: impl TwoPoints) {
        *self = self.text.iter_fwd(tp.to_points().max(self.points()))
    }

    ////////// Querying functions

    /// Wether the [`Iterator`] is on a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    #[inline(always)]
    pub fn is_on_ghost(&self) -> bool {
        self.main_iter.is_some()
    }

    /// Returns the current real and ghost [`Point`]s of the
    /// [`Iterator`]
    #[inline(always)]
    pub fn points(&self) -> (Point, Option<Point>) {
        if let Some((real, ..)) = self.main_iter.as_ref() {
            (*real, self.ghost.map(|(tg, _)| tg))
        } else {
            (self.point, self.ghost.map(|(p, _)| p))
        }
    }

    /// Handles special [`Tag`]s and [`Tag`] exceptions
    ///
    /// [`Tag`]: super::Tag
    #[inline(always)]
    fn handle_special_tag(&mut self, tag: &RawTag, b: usize) -> bool {
        match tag {
            RawTag::Ghost(_, id) => {
                if !self.print_ghosts || b < self.point.byte() || self.conceals > 0 {
                    return true;
                }
                let text = self.text.get_ghost(*id).unwrap();

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
                    self.chars = buf_chars_fwd(self.text, self.point.byte());
                }
            }
            RawTag::ConcealUntil(b) => {
                let point = self.text.point_at(*b as usize);
                *self = FwdIter::new_at(self.text, point);
                return false;
            }
            RawTag::MainCaret(_) | RawTag::ExtraCaret(_) | RawTag::Spacer(_)
                if b < self.init_point.byte() => {}
            _ => return false,
        }

        true
    }
}

impl Iterator for FwdIter<'_> {
    type Item = Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let tag = self.tags.peek();

        if let Some(&(b, tag)) = tag
            && (b <= self.point.byte() || self.conceals > 0)
        {
            self.tags.next();

            if self.handle_special_tag(&tag, b) {
                self.next()
            } else {
                Some(Item::new(self.points(), Part::from_raw(tag)))
            }
        } else if let Some(char) = self.chars.next() {
            let points = self.points();
            self.point = self.point.fwd(char);

            self.ghost = match self.main_iter {
                Some(..) => self.ghost.map(|(g, d)| (g.fwd(char), d + char.len_utf8())),
                None => None,
            };

            Some(Item::new(points, Part::Char(char)))
        } else if let Some(backup) = self.main_iter.take() {
            (self.point, self.chars, self.tags) = backup;
            self.next()
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
    text: &'a Text,
    point: Point,
    init_point: Point,
    chars: RevChars<'a>,
    tags: tags::RevTags<'a>,
    conceals: usize,

    main_iter: Option<(Point, RevChars<'a>, tags::RevTags<'a>)>,
    ghost: Option<(Point, usize)>,

    // Iteration options:
    print_ghosts: bool,
    _conceals: Conceal<'a>,
}

impl<'a> RevIter<'a> {
    /// Returns a new reverse [`Iterator`] over the [`Item`]s in the
    /// [`Text`]
    pub(super) fn new_at(text: &'a Text, tp: impl TwoPoints) -> Self {
        let (r, g) = tp.to_points();
        let point = r.min(text.len());

        let ghost = g.and_then(|offset| {
            let (_, max) = text.ghost_max_points_at(r.byte());
            max.map(|max| (max.min(offset), max.byte()))
        });

        Self {
            text,
            point,
            init_point: point,
            chars: buf_chars_rev(text, point.byte()),
            tags: text.tags_rev(point.byte()),
            conceals: 0,

            main_iter: None,
            ghost,

            print_ghosts: true,
            _conceals: Conceal::All,
        }
    }

    ////////// Iteration modifiers

    /// Disable all [`Conceal`]s
    ///
    /// [`Conceal`]: super::Conceal
    pub fn no_conceals(self) -> Self {
        Self { _conceals: Conceal::None, ..self }
    }

    /// Disable all [`Ghost`]s
    ///
    /// [`Ghost`]: super::Ghost
    pub fn no_ghosts(self) -> Self {
        Self { print_ghosts: false, ..self }
    }

    /// Returns an [`Iterator`] over only the `char`s
    ///
    /// The difference betwen this and a regular [`Text::chars_rev`]
    /// is that this [`Iterator`] will return [`Ghost`] `char`s and
    /// won't return `char`s that have been concealed
    ///
    /// [`Tag`]: super::Tag
    /// [`Ghost`]: super::Ghost
    pub fn no_tags(self) -> impl Iterator<Item = Item> + 'a {
        self.filter(|item| item.part.is_char())
    }

    ////////// Querying functions

    /// Returns the current real and ghost [`Point`]s
    pub fn points(&self) -> (Point, Option<Point>) {
        if let Some((real, ..)) = self.main_iter.as_ref() {
            (*real, Some(self.point))
        } else {
            (self.point, self.ghost.map(|(p, _)| p))
        }
    }

    /// Wether the [`Iterator`] is on a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    pub fn is_on_ghost(&self) -> bool {
        self.main_iter.is_some()
    }

    /// Handles special [`Tag`]s and [`Tag`] exceptions
    ///
    /// [`Tag`]: super::Tag
    #[inline]
    fn handled_meta_tag(&mut self, tag: &RawTag, b: usize) -> bool {
        match tag {
            RawTag::Ghost(_, id) => {
                if !self.print_ghosts || b > self.point.byte() || self.conceals > 0 {
                    return true;
                }
                let text = self.text.get_ghost(*id).unwrap();

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
                    let (_, max) = self.text.ghost_max_points_at(b);
                    (this, max.unwrap())
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
                    self.chars = buf_chars_rev(self.text, self.point.byte());
                }
            }
            RawTag::EndConceal(_) => self.conceals += 1,
            RawTag::ConcealUntil(b) => {
                let point = self.text.point_at(*b as usize);
                *self = RevIter::new_at(self.text, point);
                return false;
            }
            RawTag::MainCaret(_) | RawTag::ExtraCaret(_) | RawTag::Spacer(_)
                if b > self.init_point.byte() => {}
            _ => return false,
        }

        true
    }
}

impl Iterator for RevIter<'_> {
    type Item = Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let tag = self.tags.peek();

        if let Some(&(b, tag)) = tag
            && (b >= self.point.byte() || self.conceals > 0)
        {
            self.tags.next();

            if self.handled_meta_tag(&tag, b) {
                self.next()
            } else {
                Some(Item::new(self.points(), Part::from_raw(tag)))
            }
        } else if let Some(char) = self.chars.next() {
            self.point = self.point.rev(char);

            self.ghost = match self.main_iter {
                Some(..) => self.ghost.map(|(g, d)| (g.rev(char), d - char.len_utf8())),
                None => None,
            };

            Some(Item::new(self.points(), Part::Char(char)))
        } else if let Some(last_iter) = self.main_iter.take() {
            (self.point, self.chars, self.tags) = last_iter;
            self.next()
        } else {
            None
        }
    }
}

fn buf_chars_fwd(text: &Text, b: usize) -> FwdChars {
    let [s0, s1] = text.strs(b..).to_array();
    s0.chars().chain(s1.chars())
}

fn buf_chars_rev(text: &Text, b: usize) -> RevChars {
    let [s0, s1] = text.strs(..b).to_array();
    s1.chars().rev().chain(s0.chars().rev())
}

/// An element of a [`Text`]
///
/// This struct is comprised of three parts:
///
/// - A real [`Point`], representing a position on the real [`Text`];
/// - A ghost [`Point`], which is a position in a [`Ghost`], [`None`]
///   if not in a [`Ghost`];
/// - A [`Part`], which will either be a `char` or a [`Tag`];
///
/// [`Ghost`]: super::Ghost
/// [`Tag`]: super::Tag
#[derive(Debug, Clone, Copy)]
pub struct Item {
    /// The real [`Point`]
    pub real: Point,
    /// The [`Point`] in a [`Ghost`]
    ///
    /// If there are multiple [`Ghost`]s in the same byte, this
    /// [`Point`] will point to a sum of the previous [`Text`]'s
    /// [lengths] plus the position on this specific [`Ghost`], so
    /// every [`Point`] should point to a specific position in a byte.
    ///
    /// [`Ghost`]: super::Ghost
    /// [lengths]: Text::len
    pub ghost: Option<Point>,
    /// A [`Part`], which will either be a `char` or a [`Tag`];
    ///
    /// [`Tag`]: super::Tag
    pub part: Part,
}

impl Item {
    /// Returns a new [`Item`]
    #[inline]
    fn new(tp: impl TwoPoints, part: Part) -> Self {
        let (real, ghost) = tp.to_points();
        Self { real, ghost, part }
    }

    /// Whether this [`Item`] is in a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    pub fn is_real(&self) -> bool {
        self.ghost.is_none()
    }

    /// Returns the real position, if not on a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    pub fn as_real_char(self) -> Option<(Point, char)> {
        if self.ghost.is_none() {
            Some(self.real).zip(self.part.as_char())
        } else {
            None
        }
    }

    /// The real [byte](Point::byte)
    pub fn byte(&self) -> usize {
        self.real.byte()
    }

    /// The real [char](Point::char)
    pub fn char(&self) -> usize {
        self.real.char()
    }

    /// The real [line](Point::line)
    pub fn line(&self) -> usize {
        self.real.line()
    }

    /// The real and ghost [`Point`]s, can be used as [`TwoPoints`]
    pub fn points(&self) -> (Point, Option<Point>) {
        (self.real, self.ghost)
    }
}

// To be rethought
#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
enum Conceal<'a> {
    #[default]
    All,
    None,
    Excluding(&'a [Selection]),
    NotOnLineOf(&'a [Selection]),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Part {
    /// A printed `char`, can be real or a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    Char(char),
    /// Push a [`Form`] to the [`Painter`]
    ///
    /// [`Form`]: crate::form::Form
    /// [`Painter`]: crate::form::Painter
    PushForm(FormId),
    /// Pop a [`Form`] from the [`Painter`]
    ///
    /// [`Form`]: crate::form::Form
    /// [`Painter`]: crate::form::Painter
    PopForm(FormId),
    /// Place the main `caret` or the `"MainCaret"` [`Form`] to
    /// the [`Painter`]
    ///
    /// [`Form`]: crate::form::Form
    /// [`Painter`]: crate::form::Painter
    MainCaret,
    /// Place the extra `caret` or the `"ExtraCaret"` [`Form`] to
    /// the [`Painter`]
    ///
    /// [`Form`]: crate::form::Form
    /// [`Painter`]: crate::form::Painter
    ExtraCaret,
    /// End other forms of alignment
    AlignLeft,
    /// Begin centered alignment
    AlignCenter,
    /// Begin right alignment
    AlignRight,
    /// Add a [`Spacer`]
    ///
    /// [`Spacer`]: super::Spacer
    Spacer,
    /// Starts a toggleable region for the given [`ToggleId`]
    ///
    /// Not yet implemented
    ToggleStart(ToggleId),
    /// Ends a toggleable region for the given [`ToggleId`]
    ///
    /// Not yet implemented
    ToggleEnd(ToggleId),
    /// Resets all [`FormId`]s, [`ToggleId`]s and alignments
    ///
    /// Used when a [`Conceal`] covers a large region, which Duat
    /// optimizes by just not iterating over the [`Part`]s within.
    /// This could skip some [`Tag`]s, so this variant serves the
    /// purpose of terminating or initiating in place of skipped
    /// [`Tag`]s
    ///
    /// [`Conceal`]: super::Conceal
    /// [`Tag`]: super::Tag
    ResetState,
}

impl Part {
    /// Returns a new [`Part`] from a [`RawTag`]
    #[inline]
    pub(super) fn from_raw(value: RawTag) -> Self {
        match value {
            RawTag::PushForm(_, id, _) => Part::PushForm(id),
            RawTag::PopForm(_, id) => Part::PopForm(id),
            RawTag::MainCaret(_) => Part::MainCaret,
            RawTag::ExtraCaret(_) => Part::ExtraCaret,
            RawTag::StartAlignCenter(_) => Part::AlignCenter,
            RawTag::EndAlignCenter(_) => Part::AlignLeft,
            RawTag::StartAlignRight(_) => Part::AlignRight,
            RawTag::EndAlignRight(_) => Part::AlignLeft,
            RawTag::Spacer(_) => Part::Spacer,
            RawTag::ToggleStart(_, id) => Part::ToggleStart(id),
            RawTag::ToggleEnd(_, id) => Part::ToggleEnd(id),
            RawTag::ConcealUntil(_) => Part::ResetState,
            RawTag::StartConceal(_) | RawTag::EndConceal(_) | RawTag::Ghost(..) => {
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
