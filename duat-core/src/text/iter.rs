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
    Point, SpawnId, Text, ToggleId,
    tags::{self, RawTag},
};
use crate::text::{TwoPoints, tags::InnerTags};

/// An [`Iterator`] over the [`TextPart`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct FwdIter<'t> {
    text: &'t Text,
    point: Point,
    init_point: Point,
    chars: FwdChars<'t>,
    tags: tags::FwdTags<'t>,
    conceals: u32,

    // Things to deal with ghost text.
    main_iter: Option<MainIter<FwdChars<'t>, tags::FwdTags<'t>>>,
    ghost: Option<(Point, usize)>,
}

impl<'t> FwdIter<'t> {
    /// Returns a new forward [`Iterator`] over the [`TextPlace`]s in
    /// the [`Text`]
    #[track_caller]
    pub(super) fn new_at(text: &'t Text, points: TwoPoints) -> Self {
        let TwoPoints { real, ghost } = points;
        let point = real.min(text.len());

        // The second usize argument of ghost is the "distance traversed".
        // When iterating over this starting point, the Tags iterator will
        // still iterate over prior Ghosts in the same byte, even if they were
        // supposed to be skipped.
        // The "distance traversed" serves the purpose of skipping those
        // ghosts until the correct one is reached, hence why it starts at 0.
        let ghost = if let Some(offset) = ghost {
            let points = text.ghost_max_points_at(real.byte());
            points.ghost.map(|max| (max.min(offset), 0))
        } else {
            let points = text.ghost_max_points_at(real.byte());
            points.ghost.zip(Some(0))
        };

        Self {
            text,
            point,
            init_point: point,
            chars: buf_chars_fwd(text, point.byte()),
            tags: text.tags_fwd(point.byte(), None),
            conceals: 0,

            main_iter: None,
            ghost,
        }
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
    pub fn points(&self) -> TwoPoints {
        if let Some(MainIter { point, .. }) = self.main_iter.as_ref() {
            TwoPoints::new(*point, self.ghost.map(|(tg, _)| tg).unwrap())
        } else {
            TwoPoints::new_after_ghost(self.point)
        }
    }

    /// The [`Text`] that's being iterated over
    pub fn text(&self) -> &Text {
        self.text
    }

    /// Handles special [`Tag`]s and [`Tag`] exceptions
    ///
    /// [`Tag`]: super::Tag
    #[inline(always)]
    fn handle_meta_tag(&mut self, tag: &RawTag, b: usize) -> bool {
        match tag {
            RawTag::Ghost(_, id) => {
                if b < self.point.byte() || self.conceals > 0 {
                    return true;
                }
                let text = self.text.get_ghost(*id).unwrap();

                let (this_ghost, total_ghost) = if let Some((ghost, dist)) = &mut self.ghost {
                    if ghost.byte() >= *dist + text.len().byte() {
                        *dist += text.len().byte();
                        return true;
                    }
                    (text.point_at_byte(ghost.byte() - *dist), *ghost)
                } else {
                    (Point::default(), Point::default())
                };

                let iter = text.iter_fwd(this_ghost.to_two_points_before());
                let point = std::mem::replace(&mut self.point, this_ghost);
                let init_point = std::mem::replace(&mut self.init_point, this_ghost);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.ghost = Some((total_ghost, total_ghost.byte()));
                self.main_iter = Some(MainIter { point, init_point, chars, tags });
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
                    self.point = self.point.max(self.text.point_at_byte(b));
                    self.chars = buf_chars_fwd(self.text, self.point.byte());
                }
            }
            RawTag::ConcealUntil(b) => {
                let point = self.text.point_at_byte(*b as usize);
                *self = FwdIter::new_at(self.text, point.to_two_points_before());
                return false;
            }
            RawTag::MainCaret(_)
            | RawTag::ExtraCaret(_)
            | RawTag::Spacer(_)
            | RawTag::SwapChar(..)
            | RawTag::SpawnedWidget(..)
                if b < self.init_point.byte() => {}
            _ => return false,
        }

        true
    }
}

impl<'t> Iterator for FwdIter<'t> {
    type Item = TextPlace<'t>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let tag = self.tags.peek();

        if let Some(&(b, tag)) = tag
            && (b <= self.point.byte() || self.conceals > 0)
        {
            self.tags.next();

            if self.handle_meta_tag(&tag, b) {
                self.next()
            } else {
                let tags = &self.text.0.tags;
                Some(TextPlace::new(self.points(), TextPart::from_raw(tags, tag)))
            }
        } else if let Some(char) = self.chars.next() {
            let points = self.points();
            self.point = self.point.fwd(char);

            self.ghost = match self.main_iter {
                Some(..) => self.ghost.map(|(g, d)| (g.fwd(char), d + 1)),
                None => None,
            };

            Some(TextPlace::new(points, TextPart::Char(char)))
        } else if let Some(main_iter) = self.main_iter.take() {
            self.point = main_iter.point;
            self.init_point = main_iter.init_point;
            self.chars = main_iter.chars;
            self.tags = main_iter.tags;

            self.next()
        } else {
            None
        }
    }
}

/// An [`Iterator`] over the [`TextPart`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct RevIter<'t> {
    text: &'t Text,
    point: Point,
    init_point: Point,
    chars: RevChars<'t>,
    tags: tags::RevTags<'t>,
    conceals: usize,

    main_iter: Option<MainIter<RevChars<'t>, tags::RevTags<'t>>>,
    ghost: Option<(Point, usize)>,
}

impl<'t> RevIter<'t> {
    /// Returns a new reverse [`Iterator`] over the [`TextPlace`]s in
    /// the [`Text`]
    #[track_caller]
    pub(super) fn new_at(text: &'t Text, points: TwoPoints) -> Self {
        let TwoPoints { real, ghost } = points;
        let point = real.min(text.len());

        let ghost = ghost.and_then(|offset| {
            let points = text.ghost_max_points_at(real.byte());
            points.ghost.map(|max| (max.min(offset), max.byte()))
        });

        Self {
            text,
            point,
            init_point: point,
            chars: buf_chars_rev(text, point.byte()),
            tags: text.tags_rev(point.byte(), None),
            conceals: 0,

            main_iter: None,
            ghost,
        }
    }

    ////////// Querying functions

    /// Returns the current real and ghost [`Point`]s
    pub fn points(&self) -> TwoPoints {
        if let Some(MainIter { point, .. }) = self.main_iter.as_ref() {
            TwoPoints::new(*point, self.point)
        } else if let Some((ghost, _)) = self.ghost {
            TwoPoints::new(self.point, ghost)
        } else {
            TwoPoints::new_after_ghost(self.point)
        }
    }

    /// The [`Text`] that's being iterated over
    pub fn text(&self) -> &'t Text {
        self.text
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
                if b > self.point.byte() || self.conceals > 0 {
                    return true;
                }
                let text = self.text.get_ghost(*id).unwrap();

                let (ghost_b, this_ghost) = if let Some((offset, dist)) = &mut self.ghost {
                    if *dist - text.len().byte() >= offset.byte() {
                        *dist -= text.len().byte();
                        return true;
                    }
                    (
                        text.point_at_byte(offset.byte() + text.len().byte() - *dist),
                        *offset,
                    )
                } else {
                    let this = text.len();
                    let points = self.text.ghost_max_points_at(b);
                    (this, points.ghost.unwrap())
                };

                let iter = text.iter_rev(ghost_b.to_two_points_before());
                let point = std::mem::replace(&mut self.point, this_ghost);
                let init_point = std::mem::replace(&mut self.init_point, this_ghost);
                let chars = std::mem::replace(&mut self.chars, iter.chars);
                let tags = std::mem::replace(&mut self.tags, iter.tags);

                self.ghost = Some((this_ghost, this_ghost.byte()));
                self.main_iter = Some(MainIter { point, init_point, chars, tags });
            }

            RawTag::StartConceal(_) => {
                self.conceals = self.conceals.saturating_sub(1);
                if self.conceals == 0 {
                    self.ghost.take_if(|_| b < self.point.byte());
                    self.point = self.point.min(self.text.point_at_byte(b));
                    self.chars = buf_chars_rev(self.text, self.point.byte());
                }
            }
            RawTag::EndConceal(_) => self.conceals += 1,
            RawTag::ConcealUntil(b) => {
                let point = self.text.point_at_byte(*b as usize);
                *self = RevIter::new_at(self.text, point.to_two_points_before());
                return false;
            }
            RawTag::MainCaret(_)
            | RawTag::ExtraCaret(_)
            | RawTag::Spacer(_)
            | RawTag::SwapChar(..)
            | RawTag::SpawnedWidget(..)
                if b > self.init_point.byte() => {}
            _ => return false,
        }

        true
    }
}

impl<'t> Iterator for RevIter<'t> {
    type Item = TextPlace<'t>;

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
                let tags = &self.text.0.tags;
                Some(TextPlace::new(self.points(), TextPart::from_raw(tags, tag)))
            }
        } else if let Some(char) = self.chars.next() {
            self.point = self.point.rev(char);

            self.ghost = match self.main_iter {
                Some(..) => self.ghost.map(|(g, d)| (g.rev(char), d - 1)),
                None => None,
            };

            Some(TextPlace::new(self.points(), TextPart::Char(char)))
        } else if let Some(main_iter) = self.main_iter.take() {
            self.point = main_iter.point;
            self.init_point = main_iter.init_point;
            self.chars = main_iter.chars;
            self.tags = main_iter.tags;

            self.next()
        } else {
            None
        }
    }
}

fn buf_chars_fwd(text: &Text, b: usize) -> FwdChars<'_> {
    let [s0, s1] = text
        .slices(b..)
        .to_array()
        .map(|s| unsafe { std::str::from_utf8_unchecked(s) });
    s0.chars().chain(s1.chars())
}

fn buf_chars_rev(text: &Text, b: usize) -> RevChars<'_> {
    let [s0, s1] = text
        .slices(..b)
        .to_array()
        .map(|s| unsafe { std::str::from_utf8_unchecked(s) });
    s1.chars().rev().chain(s0.chars().rev())
}

/// An element of a [`Text`]
///
/// This struct is comprised of three parts:
///
/// - A real [`Point`], representing a position on the real [`Text`];
/// - A ghost [`Point`], which is a position in a [`Ghost`], [`None`]
///   if not in a [`Ghost`];
/// - A [`TextPart`], which will either be a `char` or a [`Tag`];
///
/// [`Ghost`]: super::Ghost
/// [`Tag`]: super::Tag
#[derive(Debug, Clone, Copy)]
pub struct TextPlace<'t> {
    /// The real [`Point`]
    pub real: Point,
    /// The [`Point`] in a [`Ghost`]
    ///
    /// If there are multiple [`Ghost`]s in the same character, this
    /// [`Point`] will point to a sum of the previous [`Text`]'s
    /// [lengths] plus the position on this specific [`Ghost`], so
    /// every [`Point`] should point to a specific position in a char.
    ///
    /// [`Ghost`]: super::Ghost
    /// [lengths]: super::Bytes::len
    pub ghost: Option<Point>,
    /// A [`TextPart`], which will either be a `char` or a [`Tag`];
    ///
    /// [`Tag`]: super::Tag
    pub part: TextPart<'t>,
}

impl<'t> TextPlace<'t> {
    /// Returns a new [`TextPlace`]
    #[inline]
    const fn new(points: TwoPoints, part: TextPart<'t>) -> Self {
        let TwoPoints { real, ghost } = points;
        Self { real, ghost, part }
    }

    /// Whether this [`TextPlace`] is in a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    pub const fn is_real(&self) -> bool {
        self.ghost.is_none()
    }

    /// Returns the real position, if not on a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    pub const fn as_real_char(self) -> Option<(Point, char)> {
        let Some(char) = self.part.as_char() else {
            return None;
        };
        if self.ghost.is_none() {
            Some((self.real, char))
        } else {
            None
        }
    }

    /// The real [byte](Point::byte)
    pub const fn byte(&self) -> usize {
        self.real.byte()
    }

    /// The real [char](Point::char)
    pub const fn char(&self) -> usize {
        self.real.char()
    }

    /// The real [line](Point::line)
    pub const fn line(&self) -> usize {
        self.real.line()
    }

    /// The real and ghost [`Point`]s, can be used as [`TwoPoints`]
    pub const fn points(&self) -> TwoPoints {
        if let Some(ghost) = self.ghost {
            TwoPoints::new(self.real, ghost)
        } else {
            TwoPoints::new_after_ghost(self.real)
        }
    }
}

type FwdChars<'t> = Chain<Chars<'t>, Chars<'t>>;
type RevChars<'t> = Chain<Rev<Chars<'t>>, Rev<Chars<'t>>>;

use crate::form::FormId;

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
///
/// This type is used in iteration by [Ui]s in order to
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
/// [Ui]: crate::ui::traits::RawUi
/// [`ResetState`]: Part::ResetState
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[doc(hidden)]
pub enum TextPart<'t> {
    /// A printed `char`, can be real or a [`Ghost`]
    ///
    /// [`Ghost`]: super::Ghost
    Char(char),
    /// Push a [`Form`] to the [`Painter`]
    ///
    /// [`Form`]: crate::form::Form
    /// [`Painter`]: crate::form::Painter
    PushForm(FormId, u8),
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
    /// Add a [`Spacer`]
    ///
    /// [`Spacer`]: super::Spacer
    Spacer,
    /// Replaces the next character, or the next space of a tab
    SwapChar(char),
    /// Starts a toggleable region for the given [`ToggleId`]
    ///
    /// Not yet implemented
    ToggleStart(ToggleId),
    /// Ends a toggleable region for the given [`ToggleId`]
    ///
    /// Not yet implemented
    ToggleEnd(ToggleId),
    /// A spawned [`Widget`]
    ///
    /// [`Widget`]: crate::ui::Widget
    SpawnedWidget(SpawnId),

    /// An inlay [`Text`]
    Inlay(&'t Text),

    /// Resets all [`FormId`]s, [`ToggleId`]s and alignments
    ///
    /// Used when a [`Conceal`] covers a large region, which Duat
    /// optimizes by just not iterating over the [`TextPart`]s within.
    /// This could skip some [`Tag`]s, so this variant serves the
    /// purpose of terminating or initiating in place of skipped
    /// [`Tag`]s
    ///
    /// This variant does not actually represent any [`Tag`].
    ///
    /// [`Conceal`]: super::Conceal
    /// [`Tag`]: super::Tag
    ResetState,
}

impl<'t> TextPart<'t> {
    /// Returns a new [`TextPart`] from a [`RawTag`]
    #[inline]
    pub(super) fn from_raw(tags: &'t InnerTags, value: RawTag) -> Self {
        match value {
            RawTag::PushForm(_, id, prio) => Self::PushForm(id, prio),
            RawTag::PopForm(_, id) => Self::PopForm(id),
            RawTag::MainCaret(_) => Self::MainCaret,
            RawTag::ExtraCaret(_) => Self::ExtraCaret,
            RawTag::Spacer(_) => Self::Spacer,
            RawTag::SwapChar(_, char) => Self::SwapChar(char),
            RawTag::StartToggle(_, id) => Self::ToggleStart(id),
            RawTag::EndToggle(_, id) => Self::ToggleEnd(id),
            RawTag::ConcealUntil(_) => Self::ResetState,
            RawTag::SpawnedWidget(_, id) => Self::SpawnedWidget(id),
            RawTag::Inlay(_, id) => Self::Inlay(tags.get_ghost(id).unwrap()),
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
    pub const fn is_char(&self) -> bool {
        matches!(self, TextPart::Char(_))
    }

    /// Returns `true` if the part is not [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[inline]
    pub const fn is_tag(&self) -> bool {
        !self.is_char()
    }

    /// Returns [`Some`] if the part is [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[inline]
    pub const fn as_char(&self) -> Option<char> {
        if let Self::Char(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct MainIter<Chars, Tags> {
    point: Point,
    init_point: Point,
    chars: Chars,
    tags: Tags,
}
