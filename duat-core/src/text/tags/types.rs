//! Types for convenience and efficiency
//!
//! There are two "types" of tag: [`Tag`]s and [`RawTag`]s. [`Tag`]s
//! are what is show to the end user, being convenient in the way they
//! include extra information, like a whole function in the case of
//! [`StartToggle`]. [`RawTag`]s, on the other hand, are meant to
//! be as small as possible in order not to waste memory, as they will
//! be stored in the [`Text`]. As such, they have as little
//! information as possible, occupying only 8 bytes.
use std::{ops::Range, sync::Arc};

use RawTag::*;
use crossterm::event::MouseEventKind;

use super::{GhostId, Tagger, ToggleId};
use crate::{
    form::FormId,
    text::{Point, Text, TextRange},
};

/// [`Tag`]s are used for every visual modification to [`Text`]
///
/// [`Tag`]s allow for all sorts of configuration on the [`Text`],
/// like changing colors throug [`Form`]s, or text alignment, or
/// [`Spacer`]s, or even concealing and ghost [`Text`].
///
/// Currently, these are the [`Tag`]s in Duat:
///
/// - [`FormTag`]: Applies a [`Form`] on a [range];
/// - [`MainCaret`] and [`ExtraCaret`]: Place `caret`s on the
///   [`Text`]. Can be an actual `caret` or just a temporary [`Form`];
/// - [`AlignCenter`] and [`AlignRight`]: Change the text alignment in
///   a [range];
/// - [`Spacer`]: Lets you put arbitrary equally sized spaces on a
///   line;
/// - [`Ghost`]: Places "ghost [`Text`]" on the [`Text`]. This is
///   [`Text`] that can be easily ignored when parsing the regular
///   [`Text`], and `caret`s can't interact with;
/// - [`Conceal`]: Hides a [range] in the [`Text`], mostly only useful
///   in the [`File`] [`Widget`];
///
/// Additionally, there is also a `Toggle` internal [`Tag`], but it is
/// not currently implemented.
///
/// [`Form`]: crate::form::Form
/// [range]: TextRange
/// [`File`]: crate::file::File
/// [`Widget`]: crate::ui::Widget
pub trait Tag<I>: Sized + std::fmt::Debug {
    /// Decomposes the [`Tag`] to its base elements
    #[doc(hidden)]
    fn decompose(
        self,
        index: I,
        max: usize,
        key: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>);
}

////////// Form-like InnerTags

/// [`Tag`]: Applies a [`Form`] to a given [range] in the [`Text`]
///
/// This struct can be created from the [`FormId::to_tag`] method,
/// granting it a priority that is used to properly order the
/// [`RawTag`]s within.
///
/// The [`Form`] is able to intersect with other [`Form`]s, which,
/// unlike when pushing [`Form`]s to a [`Builder`], interfere
/// constructively, with the latest attributes and colors winning out.
///
/// [`Form`]: crate::form::Form
/// [range]: TextRange
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormTag(pub FormId, pub u8);

impl<I: TextRange> Tag<I> for FormTag {
    fn decompose(
        self,
        index: I,
        max: usize,
        key: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
        let FormTag(id, prio) = self;
        let range = index.to_range(max);
        ranged(range, PushForm(key, id, prio), PopForm(key, id), None)
    }
}

/// [`Tag`]: Places the main caret on the [`Text`]
///
/// You shouldn't have to use this most of the time, since the
/// [`Text`] can come equipped with [`Selections`], which manage that
/// automatically for you.
///
/// [`Selections`]: crate::mode::Selections
#[derive(Debug, Clone, Copy)]
pub struct MainCaret;
simple_impl_Tag!(MainCaret, RawTag::MainCaret);

/// [`Tag`]: Places an extra Caret on the [`Text`]
///
/// How the extra cursor gets inserted is [`Ui`] dependant, for
/// example, a terminal can't show more than one cursor, so in
/// [`duat-term`], it defaults to showing the `"ExtraCaret"`
/// [`Form`], but in other platforms, it could show an actual cursor.
///
/// You shouldn't have to use this most of the time, since the
/// [`Text`] can come equipped with [`Selections`], which manage that
/// automatically for you.
///
/// [`Selections`]: crate::mode::Selections
/// [`Ui`]: crate::ui::Ui
/// [`duat-term`]: https://crates.io/crates/duat-term
/// [`Form`]: crate::form::Form
#[derive(Debug, Clone, Copy)]
pub struct ExtraCaret;
simple_impl_Tag!(ExtraCaret, RawTag::ExtraCaret);

/////////// Alignment InnerTags

/// [`Builder`] part: Begins centered alignment
///
/// [`Tag`]: Aligns the [`Text`] on the center in a [range]
///
/// [`Builder`]: crate::text::Builder
/// [range]: TextRange
#[derive(Debug, Clone, Copy)]
pub struct AlignCenter;
ranged_impl_tag!(
    AlignCenter,
    RawTag::StartAlignCenter,
    RawTag::EndAlignCenter
);

/// [`Builder`] part: Begins alignment on the right
///
/// [`Tag`]: Aligns the [`Text`] on the right in a [range]
///
/// [`Builder`]: crate::text::Builder
/// [range]: TextRange
#[derive(Debug, Clone, Copy)]
pub struct AlignRight;
ranged_impl_tag!(AlignRight, RawTag::StartAlignRight, RawTag::EndAlignRight);

/// [`Builder`] part: Begins alignment on the left
///
/// Note that, unlike [`AlignCenter`] and [`AlignRight`], this struct
/// is not a [`Tag`], since the left alignment is the default
/// alignment, i.e., the lack of those other alignment tags. So this
/// just serves to cancel these other alignments inside of a
/// [`Builder`].
///
/// [`Builder`]: crate::text::Builder
/// [range]: TextRange
#[derive(Debug, Clone, Copy)]
pub struct AlignLeft;

/// [`Builder`] part and [`Tag`]: A spacer for more advanced alignment
///
/// When printing this screen line (one row on screen, i.e. until
/// it wraps), Instead of following the current alignment, will
/// put spacing between the next and previous characters. The
/// length of the space will be roughly equal to the available
/// space on this line divided by the number of [`Spacer`]s on it.
///
/// # Example
///
/// Let's say that this is the line being printed:
///
/// ```
/// # use duat_core::prelude::*;
/// txt!("This is my line,please,pretend it has tags");
/// ```
///
/// If we were to print it with `{Spacer}` like this:
///
/// ```
/// # use duat_core::prelude::*;
/// txt!("This is my line,{Spacer}please,{Spacer}pretend it has tags");
/// ```
///
/// In a region with a width of 50, it would come out like:
///
/// ```text
/// This is my line,    please,    pretend it has tags
/// ```
///
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone, Copy)]
pub struct Spacer;
simple_impl_Tag!(Spacer, RawTag::Spacer);

////////// Text modification InnerTags

/// [`Builder`] part and [`Tag`]: Places ghost text
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
///
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone, Copy)]
pub struct Ghost<T: Into<Text>>(pub T);

impl<T: Into<Text> + std::fmt::Debug> Tag<usize> for Ghost<T> {
    fn decompose(
        self,
        byte: usize,
        max: usize,
        key: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
        assert!(
            byte <= max,
            "index out of bounds: the len is {max}, but the index is {byte}",
        );
        let id = GhostId::new();
        let tag_id = TagId::Ghost(id, Into::<Text>::into(self.0).without_last_nl());
        ((byte, RawTag::Ghost(key, id)), None, Some(tag_id))
    }
}

impl<T: Into<Text> + std::fmt::Debug> Tag<Point> for Ghost<T> {
    fn decompose(
        self,
        point: Point,
        max: usize,
        key: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
        let byte = point.byte();
        self.decompose(byte, max, key)
    }
}

/// [`Tag`]: Conceals a [range] in the [`Text`]
///
/// This [range] is completely arbitrary, being able to partially
/// contain lines, as long as it is contained within the length of the
/// [`Text`].
///
/// [range]: TextRange
#[derive(Debug, Clone, Copy)]
pub struct Conceal;
ranged_impl_tag!(Conceal, RawTag::StartConceal, RawTag::EndConceal);

/// An internal representation of [`Tag`]s
///
/// Unlike [`Tag`]s, however, each variant here is only placed in a
/// single position, and [`Tag`]s that occupy a range are replaced by
/// two [`RawTag`]s, like [`PushForm`] and [`PopForm`], for example.
#[derive(Clone, Copy, Eq)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(Tagger, FormId, u8),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(Tagger, FormId),

    /// Places the main cursor.
    MainCaret(Tagger),
    /// Places an extra cursor.
    ExtraCaret(Tagger),

    /// Starts aligning to the center, should happen to the whole
    /// line, even if it shows up in the middle of it.
    StartAlignCenter(Tagger),
    /// Ends aligning to the center, reverting to left alignment.
    /// Should happen to the whole line, even if it shows up in the
    /// middle of it.
    EndAlignCenter(Tagger),
    /// Starts aligning to the right, should happen to the whole
    /// line, even if it shows up in the middle of it.
    StartAlignRight(Tagger),
    /// Ends aligning to the right, reverting to left alignment.
    /// Should happen to the whole line, even if it shows up in the
    /// middle of it.
    EndAlignRight(Tagger),
    /// A spacer for the current screen line, replaces alignment.
    Spacer(Tagger),

    // In the process of implementing.
    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`EndConceal`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`EndConceal`]: RawTag::EndConceal
    StartConceal(Tagger),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    EndConceal(Tagger),

    // TODO: Deal with the consequences of changing this from a usize.
    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    ///
    /// This variant is not actually stored in the buffer, but is
    /// created when iterating.
    ConcealUntil(u32),

    /// Text that shows up on screen, but is ignored otherwise.
    Ghost(Tagger, GhostId),

    // Not Implemented:
    /// Begins a toggleable section in the text.
    StartToggle(Tagger, ToggleId),
    /// Ends a toggleable section in the text.
    EndToggle(Tagger, ToggleId),
}

impl RawTag {
    /// Inverts a [`RawTag`] that occupies a range
    pub fn inverse(&self) -> Option<Self> {
        match self {
            Self::PushForm(key, id, _) => Some(Self::PopForm(*key, *id)),
            Self::PopForm(key, id) => Some(Self::PushForm(*key, *id, 0)),
            Self::StartToggle(key, id) => Some(Self::EndToggle(*key, *id)),
            Self::EndToggle(key, id) => Some(Self::StartToggle(*key, *id)),
            Self::StartConceal(key) => Some(Self::EndConceal(*key)),
            Self::EndConceal(key) => Some(Self::StartConceal(*key)),
            Self::StartAlignCenter(key) => Some(Self::EndAlignCenter(*key)),
            Self::EndAlignCenter(key) => Some(Self::StartAlignCenter(*key)),
            Self::StartAlignRight(key) => Some(Self::EndAlignRight(*key)),
            Self::EndAlignRight(key) => Some(Self::StartAlignRight(*key)),
            _ => None,
        }
    }

    /// Wether this [`RawTag`] ends with another
    pub fn ends_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_tagger, l_id, _), Self::PopForm(r_tagger, r_id)) => {
                l_id == r_id && l_tagger == r_tagger
            }
            (Self::StartToggle(l_tagger, l_id), Self::EndToggle(r_tagger, r_id)) => {
                l_id == r_id && l_tagger == r_tagger
            }
            (Self::StartAlignCenter(l_tagger), Self::EndAlignCenter(r_tagger))
            | (Self::StartAlignRight(l_tagger), Self::EndAlignRight(r_tagger))
            | (Self::StartConceal(l_tagger), Self::EndConceal(r_tagger)) => l_tagger == r_tagger,
            _ => false,
        }
    }

    /// Wether this [`RawTag`] is the start of a range
    pub fn is_start(&self) -> bool {
        matches!(
            self,
            Self::PushForm(..)
                | Self::StartAlignCenter(_)
                | Self::StartAlignRight(_)
                | Self::StartToggle(..)
                | Self::StartConceal(_)
        )
    }

    /// Wether this [`RawTag`] is the end of a range
    pub fn is_end(&self) -> bool {
        matches!(
            self,
            Self::PopForm(..)
                | Self::EndAlignCenter(_)
                | Self::EndAlignRight(_)
                | Self::EndToggle(..)
                | Self::EndConceal(_)
        )
    }

    /// Wether this is [`StartAlignCenter`] or [`StartAlignRight`]
    pub fn is_start_align(&self) -> bool {
        matches!(self, Self::StartAlignCenter(_) | Self::StartAlignRight(_))
    }

    /// Wether this is [`EndAlignCenter`] or [`EndAlignRight`]
    pub fn is_end_align(&self) -> bool {
        matches!(self, Self::EndAlignCenter(_) | Self::EndAlignRight(_))
    }

    /// The [`Tagger`] of this [`RawTag`]
    pub(in crate::text) fn tagger(&self) -> Tagger {
        match self.get_tagger() {
            Some(key) => key,
            None => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }

    /// Gets the [`Tagger`] of this [`RawTag`], if it is not
    /// [`ConcealUntil`], since that one is never actually stored in
    /// [`InnerTags`]
    ///
    /// [`InnerTags`]: super::InnerTags
    fn get_tagger(&self) -> Option<Tagger> {
        match self {
            Self::PushForm(key, ..)
            | Self::PopForm(key, _)
            | Self::MainCaret(key)
            | Self::ExtraCaret(key)
            | Self::StartAlignCenter(key)
            | Self::EndAlignCenter(key)
            | Self::StartAlignRight(key)
            | Self::EndAlignRight(key)
            | Self::Spacer(key)
            | Self::StartConceal(key)
            | Self::EndConceal(key)
            | Self::Ghost(key, _)
            | Self::StartToggle(key, _)
            | Self::EndToggle(key, _) => Some(*key),
            Self::ConcealUntil(_) => None,
        }
    }

    /// The prioriy of this [`RawTag`], only varies with [`Form`]
    /// [`RawTag`]s
    ///
    /// [`Form`]: crate::form::Form
    pub(super) fn priority(&self) -> u8 {
        match self {
            Self::PushForm(.., priority) => *priority + 5,
            Self::PopForm(..) => 0,
            Self::MainCaret(..) => 4,
            Self::ExtraCaret(..) => 4,
            Self::StartAlignCenter(..) => 3,
            Self::EndAlignCenter(..) => 1,
            Self::StartAlignRight(..) => 3,
            Self::EndAlignRight(..) => 1,
            Self::Spacer(..) => 1,
            Self::StartConceal(..) => 3,
            Self::EndConceal(..) => 1,
            Self::Ghost(..) => 2,
            Self::StartToggle(..) => 3,
            Self::EndToggle(..) => 1,
            Self::ConcealUntil(_) => unreachable!("This shouldn't be queried"),
        }
    }
}

impl PartialEq for RawTag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_tagger, l_id, _), Self::PushForm(r_tagger, r_id, _)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::PopForm(l_tagger, l_id), Self::PopForm(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::MainCaret(l_tagger), Self::MainCaret(r_tagger)) => l_tagger == r_tagger,
            (Self::ExtraCaret(l_tagger), Self::ExtraCaret(r_tagger)) => l_tagger == r_tagger,
            (Self::StartAlignCenter(l_tagger), Self::StartAlignCenter(r_tagger)) => {
                l_tagger == r_tagger
            }
            (Self::EndAlignCenter(l_tagger), Self::EndAlignCenter(r_tagger)) => {
                l_tagger == r_tagger
            }
            (Self::StartAlignRight(l_tagger), Self::StartAlignRight(r_tagger)) => {
                l_tagger == r_tagger
            }
            (Self::EndAlignRight(l_tagger), Self::EndAlignRight(r_tagger)) => l_tagger == r_tagger,
            (Self::Spacer(l_tagger), Self::Spacer(r_tagger)) => l_tagger == r_tagger,
            (Self::StartConceal(l_tagger), Self::StartConceal(r_tagger)) => l_tagger == r_tagger,
            (Self::EndConceal(l_tagger), Self::EndConceal(r_tagger)) => l_tagger == r_tagger,
            (Self::ConcealUntil(l_tagger), Self::ConcealUntil(r_tagger)) => l_tagger == r_tagger,
            (Self::Ghost(l_tagger, l_id), Self::Ghost(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::StartToggle(l_tagger, l_id), Self::StartToggle(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::EndToggle(l_tagger, l_id), Self::EndToggle(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            _ => false,
        }
    }
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for RawTag {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(match (self, other) {
            (PushForm(l_tagger, l_id, l_prio), PushForm(r_tagger, r_id, r_prio)) => l_prio
                .cmp(r_prio)
                .then(l_id.cmp(r_id))
                .then(l_tagger.cmp(r_tagger)),
            (PopForm(l_tagger, l_id), PopForm(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            (RawTag::MainCaret(l_tagger), RawTag::MainCaret(r_tagger))
            | (RawTag::ExtraCaret(l_tagger), RawTag::ExtraCaret(r_tagger))
            | (StartAlignCenter(l_tagger), StartAlignCenter(r_tagger))
            | (EndAlignCenter(l_tagger), EndAlignCenter(r_tagger))
            | (StartAlignRight(l_tagger), StartAlignRight(r_tagger))
            | (EndAlignRight(l_tagger), EndAlignRight(r_tagger))
            | (RawTag::Spacer(l_tagger), RawTag::Spacer(r_tagger))
            | (StartConceal(l_tagger), StartConceal(r_tagger))
            | (EndConceal(l_tagger), EndConceal(r_tagger)) => l_tagger.cmp(r_tagger),
            (ConcealUntil(l_byte), ConcealUntil(r_byte)) => l_byte.cmp(r_byte),
            (RawTag::Ghost(l_tagger, l_id), RawTag::Ghost(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            (StartToggle(l_tagger, l_id), StartToggle(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            (EndToggle(l_tagger, l_id), EndToggle(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            _ => self.priority().cmp(&other.priority()),
        })
    }
}

impl Ord for RawTag {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::fmt::Debug for RawTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PushForm(key, id, prio) => {
                write!(f, "PushForm({key:?}, {}, {prio})", id.name())
            }
            Self::PopForm(key, id) => write!(f, "PopForm({key:?}, {})", id.name()),
            Self::MainCaret(key) => write!(f, "MainCaret({key:?})"),
            Self::ExtraCaret(key) => write!(f, "ExtraCaret({key:?})"),
            Self::StartAlignCenter(key) => write!(f, "StartAlignCenter({key:?})"),
            Self::EndAlignCenter(key) => write!(f, "EndAlignCenter({key:?})"),
            Self::StartAlignRight(key) => write!(f, "StartAlignRight({key:?})"),
            Self::EndAlignRight(key) => write!(f, "EndAlignRight({key:?})"),
            Self::Spacer(key) => write!(f, "Spacer({key:?})"),
            Self::StartConceal(key) => write!(f, "StartConceal({key:?})"),
            Self::EndConceal(key) => write!(f, "EndConceal({key:?})"),
            Self::ConcealUntil(key) => write!(f, "ConcealUntil({key:?})"),
            Self::Ghost(key, id) => write!(f, "Ghost({key:?}, {id:?})"),
            Self::StartToggle(key, id) => write!(f, "ToggleStart({key:?}), {id:?})"),
            Self::EndToggle(key, id) => write!(f, "ToggleEnd({key:?}, {id:?})"),
        }
    }
}

/// A toggleable function in a range of [`Text`], kind of like a
/// button
pub type Toggle = Arc<dyn Fn(Point, MouseEventKind) + 'static + Send + Sync>;

#[derive(Clone)]
#[allow(dead_code)]
pub enum TagId {
    Ghost(GhostId, Text),
    Toggle(ToggleId, Toggle),
}

fn ranged(
    r: Range<usize>,
    s_tag: RawTag,
    e_tag: RawTag,
    id: Option<TagId>,
) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
    ((r.start, s_tag), Some((r.end, e_tag)), id)
}

macro simple_impl_Tag($tag:ty, $raw_tag:expr) {
    impl Tag<usize> for $tag {
        fn decompose(
            self,
            byte: usize,
            max: usize,
            key: Tagger,
        ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
            assert!(
                byte <= max,
                "byte out of bounds: the len is {max}, but the byte is {byte}",
            );
            ((byte, $raw_tag(key)), None, None)
        }
    }

    impl Tag<Point> for $tag {
        fn decompose(
            self,
            point: Point,
            max: usize,
            key: Tagger,
        ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
            let byte = point.byte();
            self.decompose(byte, max, key)
        }
    }
}

macro ranged_impl_tag($tag:ty, $start:expr, $end:expr) {
    impl<I: TextRange> Tag<I> for $tag {
        fn decompose(
            self,
            index: I,
            max: usize,
            key: Tagger,
        ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
            let range = index.to_range(max);
            (
                (range.start, $start(key)),
                Some((range.end, $end(key))),
                None,
            )
        }
    }
}
