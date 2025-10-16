//! [`Ui`] structs and functions
//!
//! Although there is only a terminal [`Ui`] implemented at the
//! moment, Duat is supposed to be Ui agnostic, and I plan to create a
//! GUI app (probably in `iced` or something), and a web app as well,
//! which is honestly more of an excuse for me to become more well
//! versed on javascript.
//!
//! Each [`Ui`] is essentially a screen separated by a bunch of
//! [`Ui::Area`]s. This happens by splitting a main [`Ui::Area`]
//! continuously, by pushing [`Widget`]s on other [`Widget`]s. When a
//! [`Widget`] is pushed to another, the area of the prior [`Widget`]
//! is split in half, with [`Constraint`]s set by the [`PushSpecs`],
//! letting the user define the exact space that each [`Widget`] will
//! take up on the screen.
//!
//! Duat also supports multiple tabs, each of which is defined by a
//! main [`Ui::Area`] that was split many times over.
//!
//! The [`Ui`] also supports the concept of "clustering", that is,
//! when you push a [`Widget`] to a [`Buffer`] via the [`WidgetCreated`]
//! [`hook`], it gets "clustered" to that [`Buffer`]. This means a few
//! things. For one, if you close a [`Buffer`], all of its clustered
//! [`Widget`]s will also close. If you swap two [`Buffer`]s, what you
//! will actually swap is the [`Ui::Area`] that contains the [`Buffer`]
//! and all of its clustered [`Widget`].
//!
//! Additionally, on the terminal [`Ui`], clustering is used to
//! determine where to draw borders between [`Ui::Area`]s, and it
//! should be used like that in other [`Ui`] implementations as well.
//!
//! [`hook`]: crate::hook
//! [`Buffer`]: crate::buffer::Buffer
//! [`WidgetCreated`]: crate::hook::WidgetCreated
use std::fmt::Debug;

pub(crate) use self::widget::Node;
pub use self::{
    type_erased::{Area, PrintInfo, Ui},
    widget::Widget,
    window::{UiBuilder, Window, Windows},
};
use crate::{context::Handle, data::Pass};

pub mod layout;
pub mod traits;
mod type_erased;
mod widget;
mod window;

/// A dimension on screen, can either be horizontal or vertical
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Axis {
    /// The horizontal axis
    Horizontal,
    /// The vertical axis
    Vertical,
}

impl Axis {
    /// The [`Axis`] perpendicular to this one
    pub fn perp(&self) -> Self {
        match self {
            Axis::Horizontal => Axis::Vertical,
            Axis::Vertical => Axis::Horizontal,
        }
    }

    /// Returns `true` if the axis is [`Horizontal`].
    ///
    /// [`Horizontal`]: Axis::Horizontal
    #[must_use]
    pub fn is_hor(&self) -> bool {
        matches!(self, Self::Horizontal)
    }

    /// Returns `true` if the axis is [`Vertical`].
    ///
    /// [`Vertical`]: Axis::Vertical
    #[must_use]
    pub fn is_ver(&self) -> bool {
        matches!(self, Self::Vertical)
    }
}

impl From<PushSpecs> for Axis {
    fn from(value: PushSpecs) -> Self {
        match value.side {
            Side::Above | Side::Below => Axis::Vertical,
            _ => Axis::Horizontal,
        }
    }
}

/// Information on how a [`Widget`] should be pushed onto another
///
/// This information is composed of three parts:
///
/// * A side to push;
/// * A horizontal [`Constraint`];
/// * A vertical [`Constraint`];
///
/// Constraints are demands that must be met by the widget's
/// [`Area`], on a best effort basis.
///
/// So, for example, if the [`PushSpecs`] are:
///
/// ```rust
/// use duat_core::ui::PushSpecs;
/// let specs = PushSpecs {
///     side: Side::Left,
///     width: Some(3.0),
///     height: None,
///     hidden: false,
/// };
/// ```
///
/// Then the widget should be pushed to the left, with a width of 3,
/// an unspecified height, and _not_ hidden by default. Note that,
/// with `#[feature(default_field_values)]`, the same can be
/// accomplished by the following:
///
/// ```rust
/// let specs = PushSpecs { side: Side::left, width: Some(3.0), .. };
/// ```
#[derive(Clone, Copy, Debug, Default)]
pub struct PushSpecs {
    /// Which [`Side`] to push the [`Widget`] to
    pub side: Side = Side::Below,
    /// A width (in character cells) for this `Widget`
    ///
    /// Note that this may be ignored if it is not possible to
    /// create an area big (or small) enough.
    pub width: Option<f32> = None,
    /// A height (in lines) for this `Widget`
    ///
    /// Note that this may be ignored if it is not possible to
    /// create an area big (or small) enough.
    pub height: Option<f32> = None,
    /// Hide this `Widget` by default
    ///
    /// You can call [`Area::hide`] or [`Area::reveal`] to toggle
    /// this property.
    pub hidden: bool = false,
    /// Cluster this `Widget` when pushing
    ///
    /// This makes it so, if the main `Widget` is moved or deleted,
    /// then this one will follow. Useful for things like
    /// [`LineNumbers`], since they should follow their [`Buffer`] around.
    pub cluster: bool = true,
}

impl PushSpecs {
    /// The [`Axis`] where it will be pushed
    ///
    /// - left/right: [`Axis::Horizontal`]
    /// - above/below: [`Axis::Vertical`]
    pub const fn axis(&self) -> Axis {
        match self.side {
            Side::Above | Side::Below => Axis::Vertical,
            Side::Right | Side::Left => Axis::Horizontal,
        }
    }

    /// Wether this "comes earlier" on the screen
    ///
    /// This returns true if `self.side() == Side::Left || self.side()
    /// == Side::Above`, since that is considered "earlier" on
    /// screens.
    pub const fn comes_earlier(&self) -> bool {
        matches!(self.side, Side::Left | Side::Above)
    }

    /// The constraints on a given [`Axis`]
    pub fn len_on(&self, axis: Axis) -> Option<f32> {
        match axis {
            Axis::Horizontal => self.width,
            Axis::Vertical => self.height,
        }
    }
}

/// A direction, where a [`Widget`] will be placed in relation to
/// another.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Side {
    /// Put the [`Widget`] above another
    Above,
    /// Put the [`Widget`] on the right
    Right,
    /// Put the [`Widget`] on the left
    Below,
    /// Put the [`Widget`] below another
    Left,
}

impl Side {
    /// Which [`Axis`] this [`Side`] belongs to
    pub fn axis(&self) -> Axis {
        match self {
            Side::Above | Side::Below => Axis::Vertical,
            Side::Right | Side::Left => Axis::Horizontal,
        }
    }
}

/// Much like [`PushSpecs`], but for floating [`Widget`]s
#[derive(Default, Debug, Clone, Copy)]
pub struct SpawnSpecs {
    /// Potential spawning [`Corner`]s to connect to and from
    pub orientation: Orientation = Orientation::VerLeftBelow,
    /// A width (in character cells) for this `Widget`
    ///
    /// Note that this may be ignored if it is not possible to
    /// create an area big (or small) enough.
    pub width: Option<f32> = None,
    /// A height (in lines) for this `Widget`
    ///
    /// Note that this may be ignored if it is not possible to
    /// create an area big (or small) enough.
    pub height: Option<f32> = None,
    /// Hide this `Widget` by default
    ///
    /// You can call [`Area::hide`] or [`Area::reveal`] to toggle
    /// this property.
    pub hidden: bool = false,
}

impl SpawnSpecs {
    /// The constraints on a given [`Axis`]
    pub fn len_on(&self, axis: Axis) -> Option<f32> {
        match axis {
            Axis::Horizontal => self.width,
            Axis::Vertical => self.height,
        }
    }
}

/// Where to place a spawned [`Widget`]
///
/// The `Orientation` has 3 components of positioning, which follow
/// priorities in order to relocate the `Widget` in case there isn't
/// enough space. Respectively, they are the following:
///
/// - An axis to align the `Widget`.
/// - How to align said `Widget` on said axis.
/// - Which side of the parent should be prioritized.
///
/// For example, [`Orientation::HorTopLeft`] means: Spawn this
/// `Widget` horizontally, trying to align its top edge with the top
/// edge of the parent, prioritizing the left side. Visually speaking,
/// it will try to spawn a `Widget` like this:
///
/// ```text
/// ╭─────────┬────────╮
/// │         │ Parent │
/// │ Spawned ├────────╯
/// │         │
/// ╰─────────╯
/// ```
///
/// Notice that their tops are aligned, the edges connect on the
/// horizontal axis, and it is on the left side. However, if there is
/// not enough space, (e.g. the parent is very close to the bottom
/// left edge of the screen) it might try to spawn it like this:
///
/// ```text
/// ╭─────────╮                                 ╭─────────╮
/// │         ├────────╮                        │         │
/// │ Spawned │ Parent │, or even like ╭────────┤ Spawned │
/// │         ├────────╯               │ Parent │         │
/// ╰─────────╯                        ╰────────┴─────────╯
/// ```
///
/// This prioritization gives more flexibility to the spawning of
/// `Widget`s, which usually follows patterns of where to spawn and
/// how to place things, mostly to prevent obscuring information. The
/// most notable example of this are completion lists. For obvious
/// reasons, those should only be placed above or below (`Ver`),
/// alignment should try to be on the left edge (`VerLeft`), and
/// ideally below the cursor ([`Orientation::VerLeftBelow`]).
/// Likewise, these completion lists are sometimes accompanied by
/// description panels, which should ideally follow a
/// [`HorCenterRight`] or [`HorBottomRight`] orientation.
///
/// [`HorCenterRight`]: Orientation::HorCenterRight
/// [`HorBottomRight`]: Orientation::HorBottomRight
#[derive(Clone, Copy, Debug)]
pub enum Orientation {
    /// Place the [`Widget`] vertically, prioritizing the left edge
    /// above
    VerLeftAbove,
    /// Place the [`Widget`] vertically, prioritizing centering above
    VerCenterAbove,
    /// Place the [`Widget`] vertically, prioritizing the right edge
    /// above
    VerRightAbove,
    /// Place the [`Widget`] vertically, prioritizing the left edge
    /// below
    VerLeftBelow,
    /// Place the [`Widget`] vertically, prioritizing centering below
    VerCenterBelow,
    /// Place the [`Widget`] vertically, prioritizing the right edge
    /// below
    VerRightBelow,
    /// Place the [`Widget`] horizontally, prioritizing the top edge
    /// on the left
    HorTopLeft,
    /// Place the [`Widget`] horizontally, prioritizing centering
    /// on the left
    HorCenterLeft,
    /// Place the [`Widget`] horizontally, prioritizing the right edge
    /// on the left
    HorBottomLeft,
    /// Place the [`Widget`] horizontally, prioritizing the top edge
    /// on the right
    HorTopRight,
    /// Place the [`Widget`] horizontally, prioritizing centering
    /// on the right
    HorCenterRight,
    /// Place the [`Widget`] horizontally, prioritizing the bottom
    /// edge on the right
    HorBottomRight,
}

impl Orientation {
    /// The [`Axis`] to which this `Orientation` pushes
    pub fn axis(&self) -> Axis {
        match self {
            Orientation::VerLeftAbove
            | Orientation::VerCenterAbove
            | Orientation::VerRightAbove
            | Orientation::VerLeftBelow
            | Orientation::VerCenterBelow
            | Orientation::VerRightBelow => Axis::Vertical,
            Orientation::HorTopLeft
            | Orientation::HorCenterLeft
            | Orientation::HorBottomLeft
            | Orientation::HorTopRight
            | Orientation::HorCenterRight
            | Orientation::HorBottomRight => Axis::Horizontal,
        }
    }

    /// Wether this should prefer being pushed before (left or above)
    pub fn prefers_before(&self) -> bool {
        match self {
            Orientation::VerLeftAbove
            | Orientation::VerCenterAbove
            | Orientation::VerRightAbove
            | Orientation::HorTopLeft
            | Orientation::HorCenterLeft
            | Orientation::HorBottomLeft => true,
            Orientation::VerLeftBelow
            | Orientation::VerCenterBelow
            | Orientation::VerRightBelow
            | Orientation::HorTopRight
            | Orientation::HorCenterRight
            | Orientation::HorBottomRight => false,
        }
    }
}

/// A struct representing a "visual position" on the screen
///
/// This position differs from a [`VPoint`] in the sense that it
/// represents three properties of a printed character:
///
/// - The x position in which it was printed;
/// - The amount of horizontal space it occupies;
/// - Wether this character is the first on the line (i.e. it wraps)
///
/// [`VPoint`]: crate::mode::VPoint
#[derive(Debug, Clone, Copy)]
pub struct Caret {
    /// The horizontal position in which a character was printed
    pub x: u32,
    /// The horizontal space it occupied
    pub len: u32,
    /// Wether it is the first character in the line
    pub wrap: bool,
}

impl Caret {
    /// Returns a new [`Caret`]
    #[inline(always)]
    pub fn new(x: u32, len: u32, wrap: bool) -> Self {
        Self { x, len, wrap }
    }
}

/// A target for pushing [`Widget`]s to
///
/// This can either be a [`Handle`], which will push around a `Widget`
/// or a [`UiBuilder`], which will push around the window.
///
/// This trait is useful if you wish to let your [`Widget`] both be
/// pushed around other `Widget`s and also around the window with the
/// [`UiBuilder`]. One example of this is the [`StatusLine`] widget,
/// which behaves differently depending on if it was pushed to a
/// [`Handle<Buffer>`].
///
/// [`StatusLine`]: https://docs.rs/duat_utils/duat-utils/latest/widgets/struct.StatusLine.html
pub trait PushTarget {
    /// Pushes a [`Widget`] around `self`
    ///
    /// If `self` is a [`Handle`], this will push around the
    /// [`Handle`]'s own [`Ui::Area`]. If this is a [`UiBuilder`],
    /// this will push around the master [`Ui::Area`] of the central
    /// region of buffers.
    ///
    /// This `Widget` will be placed internally, i.e., around the
    /// [`Ui::Area`] of `self`. This is in contrast to
    /// [`Handle::push_outer_widget`], which will push around the
    /// "cluster master" of `self`.
    ///
    /// A cluster master is the collection of every `Widget` that was
    /// pushed around a central one with [`PushSpecs::cluster`] set to
    /// `true`.
    ///
    /// Both of these functions behave identically in the situation
    /// where no other [`Widget`]s were pushed around `self`.
    ///
    /// However, if, for example, a [`Widget`] was previously pushed
    /// below `self`, when pushing to the left, the following would
    /// happen:
    ///
    /// ```text
    /// ╭────────────────╮    ╭─────┬──────────╮
    /// │                │    │     │          │
    /// │      self      │    │ new │   self   │
    /// │                │ -> │     │          │
    /// ├────────────────┤    ├─────┴──────────┤
    /// │      old       │    │      old       │
    /// ╰────────────────╯    ╰────────────────╯
    /// ```
    ///
    /// While in [`Handle::push_outer_widget`], this happens instead:
    ///
    /// ```text
    /// ╭────────────────╮    ╭─────┬──────────╮
    /// │                │    │     │          │
    /// │      self      │    │     │   self   │
    /// │                │ -> │ new │          │
    /// ├────────────────┤    │     ├──────────┤
    /// │      old       │    │     │   old    │
    /// ╰────────────────╯    ╰─────┴──────────╯
    /// ```
    ///
    /// Note that `new` was pushed _around_ other clustered widgets in
    /// the second case, not just around `self`.
    fn push_inner<PW: Widget>(&self, pa: &mut Pass, widget: PW, specs: PushSpecs) -> Handle<PW>;

    /// Pushes a [`Widget`] around the "master region" of `self`
    ///
    /// If `self` is a [`Handle`], this will push its "cluster
    /// master". If this is a [`UiBuilder`], this will push the
    /// `Widget` to the edges of the window.
    ///
    /// A cluster master is the collection of every `Widget` that was
    /// pushed around a central one with [`PushSpecs::cluster`] set to
    /// `true`.
    ///
    /// This [`Widget`] will be placed externally, i.e., around every
    /// other [`Widget`] that was pushed around `self`. This is in
    /// contrast to [`Handle::push_inner_widget`], which will push
    /// only around `self`.
    ///
    /// Both of these functions behave identically in the situation
    /// where no other [`Widget`]s were pushed around `self`.
    ///
    /// However, if, for example, a [`Widget`] was previously pushed
    /// to the left of `self`, when pushing to the left again, the
    /// following would happen:
    ///
    /// ```text
    /// ╭──────┬──────────╮    ╭─────┬─────┬──────╮
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// │  old │   self   │ -> │ new │ old │ self │
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// ╰──────┴──────────╯    ╰─────┴─────┴──────╯
    /// ```
    ///
    /// While in [`Handle::push_inner_widget`], this happens instead:
    ///
    /// ```text
    /// ╭──────┬──────────╮    ╭─────┬─────┬──────╮
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// │  old │   self   │ -> │ old │ new │ self │
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// ╰──────┴──────────╯    ╰─────┴─────┴──────╯
    /// ```
    ///
    /// Note that `new` was pushed _around_ other clustered widgets in
    /// the first case, not just around `self`.
    fn push_outer<PW: Widget>(&self, pa: &mut Pass, widget: PW, specs: PushSpecs) -> Handle<PW>;

    /// Tries to downcast to a [`Handle`] of some `W`
    fn try_downcast<W: Widget>(&self) -> Option<Handle<W>>;
}

impl<W: Widget + ?Sized> PushTarget for Handle<W> {
    #[doc(hidden)]
    fn push_inner<PW: Widget>(&self, pa: &mut Pass, widget: PW, specs: PushSpecs) -> Handle<PW> {
        self.push_inner_widget(pa, widget, specs)
    }

    #[doc(hidden)]
    fn push_outer<PW: Widget>(&self, pa: &mut Pass, widget: PW, specs: PushSpecs) -> Handle<PW> {
        self.push_outer_widget(pa, widget, specs)
    }

    fn try_downcast<DW: Widget>(&self) -> Option<Handle<DW>> {
        self.try_downcast()
    }
}

impl PushTarget for UiBuilder {
    #[doc(hidden)]
    fn push_inner<PW: Widget>(&self, pa: &mut Pass, widget: PW, specs: PushSpecs) -> Handle<PW> {
        UiBuilder::push_inner(self, pa, widget, specs)
    }

    #[doc(hidden)]
    fn push_outer<PW: Widget>(&self, pa: &mut Pass, widget: PW, specs: PushSpecs) -> Handle<PW> {
        UiBuilder::push_outer(self, pa, widget, specs)
    }

    fn try_downcast<W: Widget>(&self) -> Option<Handle<W>> {
        None
    }
}
