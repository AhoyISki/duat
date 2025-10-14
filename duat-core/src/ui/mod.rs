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
//! when you push a [`Widget`] to a [`File`] via the [`WidgetCreated`]
//! [`hook`], it gets "clustered" to that [`File`]. This means a few
//! things. For one, if you close a [`File`], all of its clustered
//! [`Widget`]s will also close. If you swap two [`File`]s, what you
//! will actually swap is the [`Ui::Area`] that contains the [`File`]
//! and all of its clustered [`Widget`].
//!
//! Additionally, on the terminal [`Ui`], clustering is used to
//! determine where to draw borders between [`Ui::Area`]s, and it
//! should be used like that in other [`Ui`] implementations as well.
//!
//! [`hook`]: crate::hook
//! [`File`]: crate::file::File
//! [`WidgetCreated`]: crate::hook::WidgetCreated
use std::{
    any::Any,
    cell::UnsafeCell,
    fmt::Debug,
    sync::{Arc, OnceLock},
};

use bincode::{Decode, Encode};

pub(crate) use self::widget::Node;
pub use self::{
    widget::Widget,
    window::{UiBuilder, Window, Windows},
};
use crate::{
    cfg::PrintCfg,
    context::{self, Cache, Handle},
    data::{Pass, RwData},
    file::File,
    form::Painter,
    session::DuatSender,
    text::{FwdIter, Item, Point, RevIter, SpawnId, Text, TwoPoints},
};

pub mod layout;
mod widget;
mod window;

/// All the methods that a working gui/tui will need to implement, in
/// order to use Duat.
///
/// # NOTE
///
/// The dependencies on [`Clone`] and [`Default`] is only here for
/// convenience. Many types require a [`Ui`] as a generic parameter,
/// and if [`Ui`] does not implement [`Clone`] or [`Default`],
/// deriving [`Clone`] or [`Default`] for said types would
/// be a very manual task.
///
/// Below is the recommended implementation of [`Clone`] adn
/// [`Default`] for all types that implement [`Ui`]:
///
/// ```rust
/// # mod duat_smart_fridge {
/// #     pub struct Ui;
/// # }
/// impl Clone for duat_smart_fridge::Ui {
///     fn clone(&self) -> Self {
///         panic!("You are not supposed to clone the Ui");
///     }
/// }
/// impl Default for duat_smart_fridge::Ui {
///     fn default() -> Self {
///         panic!("You are not supposed to call the Ui's default constructor");
///     }
/// }
/// ```
pub trait Ui: Send + Sync + 'static {
    /// The [`Area`] of this [`Ui`]
    type Area: Area
    where
        Self: Sized;

    /// Return [`Some`] only on the first call
    fn get_once() -> Option<&'static Self>
    where
        Self: Sized;

    ////////// Functions executed from the outer loop

    /// Functions to trigger when the program begins
    ///
    /// These will happen in the main `duat` runner
    fn open(&self, duat_tx: DuatSender);

    /// Functions to trigger when the program ends
    ///
    /// These will happen in the main `duat` runner
    fn close(&self);

    ////////// Functions executed from within the configuration loop

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    fn new_root(&self, cache: <Self::Area as Area>::Cache) -> Self::Area
    where
        Self: Sized;

    /// Initiates and returns a new "floating" [`Area`]
    ///
    /// This is one of two ways of spawning floating [`Widget`]s. The
    /// other way is with [`Area::spawn`], in which a [`Widget`] will
    /// be bolted on the edges of another.
    ///
    /// TODO: There will probably be some way of defining floating
    /// [`Widget`]s with coordinates in the not too distant future as
    /// well.
    ///
    /// [`Area`]: Ui::Area
    fn new_spawned(
        &self,
        id: SpawnId,
        specs: SpawnSpecs,
        cache: <Self::Area as Area>::Cache,
        win: usize,
    ) -> Self::Area
    where
        Self: Sized;

    /// Switches the currently active window
    ///
    /// This will only happen to with window indices that are actual
    /// windows. If at some point, a window index comes up that is not
    /// actually a window, that's a bug.
    fn switch_window(&self, win: usize);

    /// Flush the layout
    ///
    /// When this function is called, it means that Duat has finished
    /// adding or removing widgets, so the ui should calculate the
    /// layout.
    fn flush_layout(&self);

    /// Prints the layout
    ///
    /// Since printing runs all on the same thread, it is most
    /// efficient to call a printing function after all the widgets
    /// are done updating, I think.
    fn print(&self);

    /// Functions to trigger when the program reloads
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn load(&self);

    /// Unloads the [`Ui`]
    ///
    /// Unlike [`Ui::close`], this will happen both when Duat reloads
    /// the configuratio and when it closes the app.
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn unload(&self);

    /// Removes a window from the [`Ui`]
    ///
    /// This should keep the current active window consistent. That
    /// is, if the current window was ahead of the deleted one, it
    /// should be shifted back, so that the same window is still
    /// displayed.
    fn remove_window(&self, win: usize);
}

/// An [`Area`] that supports printing [`Text`]
///
/// These represent the entire GUI of Duat, the only parts of the
/// screen where text may be printed.
pub trait Area: 'static {
    /// Something to be kept between app instances/reloads
    ///
    /// The most useful thing to keep in this case is the
    /// [`PrintInfo`], but you could include other things
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    type Cache: Default + Encode + Decode<()> + 'static
    where
        Self: Sized;
    /// Information about what parts of a [`Text`] should be printed
    ///
    /// For the [`term-ui`], for example, this is quite simple, it
    /// only needs to include the real and ghost [`Point`]s on the
    /// top left corner in order to print correctly, but your own
    /// [`Ui`] could differ in what it needs to keep, if it makes
    /// use of smooth scrolling, for example.
    ///
    /// [`term-ui`]: docs.rs/term-ui/latest/term_ui
    type PrintInfo: Default + Clone + Send + Sync + PartialEq + Eq + 'static
    where
        Self: Sized;

    ////////// Area modification

    /// Creates an `Area` around this one
    ///
    /// Will return the newly created `Area` as well as a parent
    /// `Area`, if one was created to house both of them.
    ///
    /// If this `Area` was previously [deleted], will return [`None`].
    ///
    /// As an example, assuming that [`self`] has an index of `0`,
    /// pushing an area to [`self`] on [`Side::Left`] would create
    /// 2 new areas:
    ///
    /// ```text
    /// ╭────────0────────╮     ╭────────1────────╮
    /// │                 │     │╭──2───╮╭───0───╮│
    /// │      self       │ --> ││      ││ self  ││
    /// │                 │     │╰──────╯╰───────╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// So now, there is a new area `1`, which is the parent of the
    /// areas `0` and `2`. When a new parent is created, it should be
    /// returned as the second element in the tuple.
    ///
    /// That doesn't always happen though. For example, pushing
    /// another area to the [`Side::Right`] of `1`, `2`, or `0`,
    /// in this situation, should not result in the creation of a
    /// new parent:
    ///
    /// ```text
    /// ╭────────1────────╮     ╭────────1────────╮
    /// │╭──2───╮╭───0───╮│     │╭─2─╮╭──0──╮╭─3─╮│
    /// ││      ││ self  ││     ││   ││self ││   ││
    /// │╰──────╯╰───────╯│     │╰───╯╰─────╯╰───╯│
    /// ╰─────────────────╯     ╰─────────────────╯
    /// ```
    ///
    /// And so [`Area::bisect`] should return `(3, None)`.
    ///
    /// [deleted]: Area::delete
    fn push(
        &mut self,
        specs: PushSpecs,
        on_files: bool,
        cache: Self::Cache,
    ) -> Option<(Self, Option<Self>)>
    where
        Self: Sized;

    /// Spawns a floating area on this `Area`
    ///
    /// This function will take a list of [`SpawnSpecs`], taking the
    /// first one that fits, and readapting as the constraints are
    /// altered
    ///
    /// If this `Area` was previously [deleted], will return [`None`].
    fn spawn(&mut self, id: SpawnId, specs: SpawnSpecs, cache: Self::Cache) -> Option<Self>
    where
        Self: Sized;

    /// Deletes this `Area`, signaling the closing of a
    /// [`Widget`]
    ///
    /// The first return value shall be `true` if the window housing
    /// this `Area` should be removed.
    ///
    /// If the [`Area`]'s parent was also deleted, return it.
    fn delete(&mut self) -> (bool, Vec<Self>)
    where
        Self: Sized;

    /// Swaps this `Area` with another one
    ///
    /// The swapped `Area`s will be cluster masters of the
    /// respective `Area`s. As such, if they belong to the same
    /// master, nothing happens.
    ///
    /// This function will _never_ be called such that one of the
    /// `Area`s is a decendant of the other, so the [`Ui`] implementor
    /// doesn't need to worry about that possibility.
    ///
    /// It can fail if either of the [`Area`]s was already deleted, or
    /// if no swap happened because they belonged to the same cluster
    /// master.
    fn swap(&mut self, rhs: &mut Self) -> bool
    where
        Self: Sized;

    ////////// Constraint changing functions

    /// Changes the horizontal constraint of the area
    fn set_width(&self, width: f32) -> Result<(), Text>;

    /// Changes the vertical constraint of the area
    fn set_height(&self, height: f32) -> Result<(), Text>;

    /// Changes [`Constraint`]s such that the [`Area`] becomes
    /// hidden
    fn hide(&self) -> Result<(), Text>;

    /// Changes [`Constraint`]s such that the [`Area`] is revealed
    fn reveal(&self) -> Result<(), Text>;

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    fn request_width_to_fit(&self, cfg: PrintCfg, text: &Text) -> Result<(), Text>;

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    fn set_as_active(&self);

    ////////// Printing functions

    /// Prints the [`Text`] via an [`Iterator`]
    fn print(&self, text: &Text, cfg: PrintCfg, painter: Painter);

    /// Prints the [`Text`] with a callback function
    fn print_with<'a>(
        &self,
        text: &Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) where
        Self: Sized;

    /// The current printing information of the area
    fn get_print_info(&self) -> Self::PrintInfo
    where
        Self: Sized;

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    fn set_print_info(&mut self, info: Self::PrintInfo)
    where
        Self: Sized;

    /// Returns a printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns an iterator
    /// which assigns to each of them a [`Caret`]. This struct
    /// essentially represents where horizontally would this character
    /// be printed.
    ///
    /// If you want a reverse iterator, see
    /// [`Area::rev_print_iter`].
    ///
    /// [`text::Item`]: Item
    fn print_iter<'a>(
        &self,
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a>;

    /// Returns a reversed printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns a reversed
    /// iterator which assigns to each of them a [`Caret`]. This
    /// struct essentially represents where horizontally each
    /// character would be printed.
    ///
    /// If you want a forwards iterator, see [`Area::print_iter`].
    ///
    /// [`text::Item`]: Item
    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: PrintCfg,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a>;

    ////////// Points functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    fn scroll_ver(&self, text: &Text, dist: i32, cfg: PrintCfg);

    /// Scrolls the [`Text`] on all four directions until the given
    /// [`Point`] is within the [`ScrollOff`] range
    ///
    /// There are two other scrolling methods for [`Area`]:
    /// [`scroll_ver`] and [`scroll_to_points`]. The difference
    /// between this and [`scroll_to_points`] is that this method
    /// doesn't do anything if the [`Point`] is already on screen.
    ///
    /// [`ScrollOff`]: crate::cfg::ScrollOff
    /// [`scroll_ver`]: Area::scroll_ver
    /// [`scroll_to_points`]: Area::scroll_to_points
    fn scroll_around_points(&self, text: &Text, points: (Point, Option<Point>), cfg: PrintCfg);

    /// Scrolls the [`Text`] to the visual line of a [`TwoPoints`]
    ///
    /// This method takes [line wrapping] into account, so it's not
    /// the same as setting the starting points to the
    /// [`Text::visual_line_start`] of these [`TwoPoints`].
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    ///
    /// [line wrapping]: crate::cfg::WrapMethod
    fn scroll_to_points(&self, text: &Text, points: (Point, Option<Point>), cfg: PrintCfg);

    /// The start points that should be printed
    fn start_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>);

    /// The points immediately after the last printed [`Point`]
    fn end_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>);

    ////////// Queries

    /// Whether or not [`self`] has changed
    ///
    /// This would mean anything relevant that wouldn't be determined
    /// by [`PrintInfo`], this is most likely going to be the bounding
    /// box, but it may be something else.
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    fn has_changed(&self) -> bool;

    /// Whether or not [`self`] is the "master" of `other`
    ///
    /// This can only happen if, by following [`self`]'s children, you
    /// would eventually reach `other`.
    fn is_master_of(&self, other: &Self) -> bool
    where
        Self: Sized;

    /// Returns the clustered master of [`self`], if there is one
    ///
    /// If [`self`] belongs to a clustered group, return the most
    /// senior member of said cluster, which must hold all other
    /// members of the cluster.
    fn get_cluster_master(&self) -> Option<Self>
    where
        Self: Sized;

    /// Returns the statics from `self`
    fn cache(&self) -> Option<Self::Cache>
    where
        Self: Sized;

    /// Gets the width of the area
    fn width(&self) -> f32;

    /// Gets the height of the area
    fn height(&self) -> f32;

    /// Returns `true` if this is the currently active [`Area`]
    ///
    /// Only one [`Area`] should be active at any given moment.
    fn is_active(&self) -> bool;
}

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
    /// [`LineNumbers`], since they should follow their [`File`] around.
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

/// A struct used to modify the layout of [`Area`]s
///
/// The end user should not have access to methods that directly
/// modify the layout, like [`Area::delete`] or
/// [`Area::bisect`], since they will modify the layout without
/// any coordination with the rest of Duat, so this struct is used to
/// "hide" those methods, in order to prevent users from directly
/// accessing them.
///
/// Higher lever versions of these methods are still available to the
/// end user, in the more controled APIs of [`Area`]
pub struct MutArea<'area, A: Area>(pub(crate) &'area A);

impl<A: Area> std::ops::Deref for MutArea<'_, A> {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        self.0
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
/// [`Handle<File>`].
///
/// [`StatusLine`]: https://docs.rs/duat_utils/duat-utils/latest/widgets/struct.StatusLine.html
pub trait PushTarget {
    /// Pushes a [`Widget`] around `self`
    ///
    /// If `self` is a [`Handle`], this will push around the
    /// [`Handle`]'s own [`Ui::Area`]. If this is a [`UiBuilder`],
    /// this will push around the master [`Ui::Area`] of the central
    /// region of files.
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

static DEFAULT_PRINT_INFO: OnceLock<fn() -> TypeErasedPrintInfo> = OnceLock::new();

#[derive(Clone, Copy)]
pub struct TypeErasedUi {
    ui: &'static dyn Ui,
    fns: &'static TypeErasedUiFunctions,
}

impl TypeErasedUi {
    /// Returns a new type erased [`Ui`]
    ///
    /// Given the [`Ui::get_once`] function, this should only be
    /// callable _once_.
    pub fn new<U: Ui>() -> Self
    where
        U::Area: PartialEq,
    {
        DEFAULT_PRINT_INFO
            .set(|| TypeErasedPrintInfo::new::<U>(<U::Area as Area>::PrintInfo::default()))
            .expect("Multiple Uis were setup, you are not supposed to do that");

        TypeErasedUi {
            ui: U::get_once().expect("Ui was acquired more than once"),
            fns: TypeErasedUiFunctions::new::<U>(),
        }
    }

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    pub fn new_root(&self, pa: &Pass, widget: &RwData<dyn Widget>) -> TypeErasedArea {
        (self.fns.new_root)(pa, &self.ui, widget)
    }

    /// Initiates and returns a new "floating" [`Area`]
    ///
    /// This is one of two ways of spawning floating [`Widget`]s. The
    /// other way is with [`Area::spawn`], in which a [`Widget`] will
    /// be bolted on the edges of another.
    ///
    /// TODO: There will probably be some way of defining floating
    /// [`Widget`]s with coordinates in the not too distant future as
    /// well.
    ///
    /// [`Area`]: Ui::Area
    pub fn new_spawned(
        &self,
        pa: &Pass,
        widget: &RwData<dyn Widget>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
        win: usize,
    ) -> TypeErasedArea {
        (self.fns.new_spawned)(pa, &self.ui, widget, spawn_id, specs, win)
    }
}

struct TypeErasedUiFunctions {
    new_root: fn(pa: &Pass, &dyn Any, widget: &RwData<dyn Widget>) -> TypeErasedArea,
    new_spawned: fn(
        pa: &Pass,
        &dyn Any,
        widget: &RwData<dyn Widget>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
        win: usize,
    ) -> TypeErasedArea,
}

impl TypeErasedUiFunctions {
    fn new<U: Ui>() -> &'static Self
    where
        U::Area: PartialEq,
    {
        &Self {
            new_root: |pa, ui, widget| {
                let ui: &U = ui.downcast_ref().unwrap();

                TypeErasedArea::new::<U>(ui.new_root(get_cache::<U>(pa, widget)))
            },
            new_spawned: |pa, ui, widget, spawn_id, specs, win| {
                let ui: &U = ui.downcast_ref().unwrap();

                TypeErasedArea::new::<U>(ui.new_spawned(
                    spawn_id,
                    specs,
                    get_cache::<U>(pa, widget),
                    win,
                ))
            },
        }
    }
}

impl std::ops::Deref for TypeErasedUi {
    type Target = dyn Ui + 'static;

    fn deref(&self) -> &Self::Target {
        self.ui
    }
}

/// A type erased [`Area`]
///
/// This type houses the inner `Area`, and provides type erased access
/// to its functions.
#[derive(Clone)]
pub struct TypeErasedArea {
    area: RwData<dyn Area>,
    fns: &'static TypeErasedAreaFunctions,
}

impl TypeErasedArea {
    /// Returns a new type erased [`Area`]
    ///
    /// This is the only moment where the [`Ui`] and `Area` will be
    /// statically known.
    fn new<U: Ui>(area: U::Area) -> Self
    where
        U::Area: PartialEq,
        <U::Area as Area>::PrintInfo: Default + Clone + Send + PartialEq + Eq,
    {
        Self {
            area: unsafe { RwData::new_unsized::<U::Area>(Arc::new(UnsafeCell::new(area))) },
            fns: TypeErasedAreaFunctions::new::<U>(),
        }
    }

    /// Shared access to an [`Area`] trait object
    ///
    /// You can use this to call any of the non restricted functions
    /// from the [`Area`] trait, which include a bunch internal
    /// mutability, like with [`Area::set_width`],
    /// [`Area::set_height`], [`Area::reveal`], etc.
    pub fn read<'a>(&'a self, pa: &'a Pass) -> &'a dyn Area {
        self.area.read(pa)
    }

    ////////// Area Modification functions

    /// Pushes a [`Widget`] to this [`Area`]
    pub(crate) fn push(
        &self,
        pa: &mut Pass,
        widget: &RwData<dyn Widget>,
        specs: PushSpecs,
        on_files: bool,
    ) -> Option<(Self, Option<Self>)> {
        (self.fns.push)(pa, &self.area, widget, specs, on_files)
    }

    /// Spawns a [`Widget`] on this [`Area`]
    pub(crate) fn spawn(
        &self,
        pa: &mut Pass,
        widget: &RwData<dyn Widget>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
    ) -> Option<Self> {
        (self.fns.spawn)(pa, &self.area, widget, spawn_id, specs)
    }

    /// Deletes this [`Area`], returning wether the window should be
    /// removed, as well as all the other ares that were deleted
    fn delete(&self, pa: &mut Pass) -> (bool, Vec<Self>) {
        (self.fns.delete)(pa, &self.area)
    }

    /// Swaps this [`Area`] with another
    fn swap(&self, pa: &mut Pass, rhs: &Self) -> bool {
        (self.fns.swap)(pa, self, rhs)
    }

    ////////// Constraint changing functions

    /// Changes the horizontal constraint of the area
    pub fn set_width(&self, pa: &Pass, width: f32) -> Result<(), Text> {
        self.area.read(pa).set_width(width)
    }

    /// Changes the vertical constraint of the area
    pub fn set_height(&self, pa: &Pass, height: f32) -> Result<(), Text> {
        self.area.read(pa).set_height(height)
    }

    /// Changes [`Constraint`]s such that the [`Area`] becomes
    /// hidden
    pub fn hide(&self, pa: &Pass) -> Result<(), Text> {
        self.area.read(pa).hide()
    }

    /// Changes [`Constraint`]s such that the [`Area`] is revealed
    pub fn reveal(&self, pa: &Pass) -> Result<(), Text> {
        self.area.read(pa).reveal()
    }

    /// Requests that the width be enough to fit a certain piece of
    /// text.
    pub fn request_width_to_fit(&self, pa: &Pass, cfg: PrintCfg, text: &Text) -> Result<(), Text> {
        self.area.read(pa).request_width_to_fit(cfg, text)
    }

    /// Tells the [`Ui`] that this [`Area`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`Area`] while deactivating
    /// any other active [`Area`].
    pub fn set_as_active(&self, pa: &Pass) {
        self.area.read(pa).set_as_active()
    }

    ////////// Printing functions

    /// Prints the [`Text`] via an [`Iterator`]
    pub fn print(&self, pa: &Pass, text: &Text, cfg: PrintCfg, painter: Painter) {
        self.area.read(pa).print(text, cfg, painter)
    }

    /// Prints the [`Text`] with a callback function
    pub fn print_with<'a>(
        &self,
        pa: &Pass,
        text: &Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        (self.fns.print_with)(pa, &self.area, text, cfg, painter, Box::new(f))
    }

    /// The current printing information of the area
    pub fn get_print_info(&self, pa: &Pass) -> TypeErasedPrintInfo {
        (self.fns.get_print_info)(pa, &self.area)
    }

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    pub fn set_print_info(&self, pa: &mut Pass, info: TypeErasedPrintInfo) {
        (self.fns.set_print_info)(pa, &self.area, info)
    }

    /// Returns a printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns an iterator
    /// which assigns to each of them a [`Caret`]. This struct
    /// essentially represents where horizontally would this character
    /// be printed.
    ///
    /// If you want a reverse iterator, see
    /// [`Area::rev_print_iter`].
    ///
    /// [`text::Item`]: Item
    pub fn print_iter<'a>(
        &self,
        pa: &Pass,
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a> {
        self.area.read(pa).print_iter(iter, cfg)
    }

    /// Returns a reversed printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns a reversed
    /// iterator which assigns to each of them a [`Caret`]. This
    /// struct essentially represents where horizontally each
    /// character would be printed.
    ///
    /// If you want a forwards iterator, see [`Area::print_iter`].
    ///
    /// [`text::Item`]: Item
    pub fn rev_print_iter<'a>(
        &self,
        pa: &Pass,
        iter: RevIter<'a>,
        cfg: PrintCfg,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a> {
        self.area.read(pa).rev_print_iter(iter, cfg)
    }

    ////////// Points functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    pub fn scroll_ver(&self, pa: &Pass, text: &Text, dist: i32, cfg: PrintCfg) {
        self.area.read(pa).scroll_ver(text, dist, cfg);
    }

    /// Scrolls the [`Text`] on all four directions until the given
    /// [`Point`] is within the [`ScrollOff`] range
    ///
    /// There are two other scrolling methods for [`Area`]:
    /// [`scroll_ver`] and [`scroll_to_points`]. The difference
    /// between this and [`scroll_to_points`] is that this method
    /// doesn't do anything if the [`Point`] is already on screen.
    ///
    /// [`ScrollOff`]: crate::cfg::ScrollOff
    /// [`scroll_ver`]: Area::scroll_ver
    /// [`scroll_to_points`]: Area::scroll_to_points
    pub fn scroll_around_points(
        &self,
        pa: &Pass,
        text: &Text,
        points: (Point, Option<Point>),
        cfg: PrintCfg,
    ) {
        self.area.read(pa).scroll_to_points(text, points, cfg);
    }

    /// Scrolls the [`Text`] to the visual line of a [`TwoPoints`]
    ///
    /// This method takes [line wrapping] into account, so it's not
    /// the same as setting the starting points to the
    /// [`Text::visual_line_start`] of these [`TwoPoints`].
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    ///
    /// [line wrapping]: crate::cfg::WrapMethod
    pub fn scroll_to_points(&self, pa: &Pass, text: &Text, points: impl TwoPoints, cfg: PrintCfg) {
        self.area
            .read(pa)
            .scroll_to_points(text, points.to_points(), cfg);
    }

    /// Scrolls the [`Area`] to the given [`TwoPoints`]
    pub fn start_points(&self, pa: &Pass, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        self.area.read(pa).start_points(text, cfg)
    }

    /// Scrolls the [`Area`] to the given [`TwoPoints`]
    pub fn end_points(&self, pa: &Pass, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        self.area.read(pa).end_points(text, cfg)
    }

    /////////// Querying functions

    /// Wether this [`Area`] has changed since last being printed
    pub fn has_changed(&self, pa: &Pass) -> bool {
        self.area.read(pa).has_changed()
    }

    /// Whether or not this [`Area`] is the "master" of another
    pub fn is_master_of(&self, pa: &Pass, other: &Self) -> bool {
        (self.fns.is_master_of)(pa, &self.area, &other.area)
    }

    /// Returns the clustered master of the [`Area`], if there is one
    pub(crate) fn get_cluster_master(&self, pa: &Pass) -> Option<Self> {
        (self.fns.get_cluster_master)(pa, &self.area)
    }

    /// Stores the cache of the [`Area`], given a path to associate
    /// with this cache
    pub fn store_cache(&self, pa: &Pass, path: &str) -> Result<(), Text> {
        (self.fns.store_cache)(pa, &self.area, path)
    }

    /// Gets the width of the area
    pub fn width(&self, pa: &Pass) -> f32 {
        self.area.read(pa).width()
    }

    /// Gets the height of the area
    pub fn height(&self, pa: &Pass) -> f32 {
        self.area.read(pa).height()
    }

    /// Returns `true` if this is the currently active [`Area`]
    ///
    /// Only one [`Area`] should be active at any given moment.
    pub fn is_active(&self, pa: &Pass) -> bool {
        self.area.read(pa).is_active()
    }

    /// Wether this [`Area`] is the same as another
    pub fn area_is_eq(&self, pa: &Pass, other: &TypeErasedArea) -> bool {
        (self.fns.eq)(pa, &self.area, &other.area)
    }
}

#[derive(Clone, Copy)]
struct TypeErasedAreaFunctions {
    /// Push one [`Area`] to another
    push: fn(
        pa: &mut Pass,
        area: &RwData<dyn Area>,
        widget: &RwData<dyn Widget>,
        specs: PushSpecs,
        on_files: bool,
    ) -> Option<(TypeErasedArea, Option<TypeErasedArea>)>,
    /// Spawn an [`Area`] on another
    spawn: fn(
        pa: &mut Pass,
        area: &RwData<dyn Area>,
        widget: &RwData<dyn Widget>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
    ) -> Option<TypeErasedArea>,
    /// Deletes an [`Area`]
    delete: fn(pa: &mut Pass, area: &RwData<dyn Area>) -> (bool, Vec<TypeErasedArea>),
    /// Swaps two [`Area`]s
    swap: fn(pa: &mut Pass, lhs: &TypeErasedArea, rhs: &TypeErasedArea) -> bool,
    /// Prints to an [`Area`], with a callback function
    print_with: for<'a> fn(
        pa: &Pass,
        area: &RwData<dyn Area>,
        text: &Text,
        cfg: PrintCfg,
        painter: Painter,
        f: Box<dyn FnMut(&Caret, &Item) + 'a>,
    ),
    /// Gets the type erased [`Area::PrintInfo`]
    get_print_info: fn(pa: &Pass, area: &RwData<dyn Area>) -> TypeErasedPrintInfo,
    /// Sets the type erased [`Area::PrintInfo`]
    set_print_info: fn(pa: &mut Pass, area: &RwData<dyn Area>, info: TypeErasedPrintInfo),
    /// Wether this [`Area`] is the master of another
    is_master_of: fn(pa: &Pass, lhs: &RwData<dyn Area>, rhs: &RwData<dyn Area>) -> bool,
    /// Gets the master [`Area`] of another's cluster
    get_cluster_master: fn(pa: &Pass, area: &RwData<dyn Area>) -> Option<TypeErasedArea>,
    /// Store the [`Area::Cache`] of this [`Area`]
    store_cache: fn(pa: &Pass, area: &RwData<dyn Area>, path: &str) -> Result<(), Text>,
    /// Wether two [`Area`]s are the same
    eq: fn(pa: &Pass, lhs: &RwData<dyn Area>, rhs: &RwData<dyn Area>) -> bool,
}

impl TypeErasedAreaFunctions {
    const fn new<U: Ui>() -> &'static Self
    where
        U::Area: PartialEq,
    {
        &Self {
            push: |pa, area, widget, specs, on_files| {
                let cache = get_cache::<U>(pa, widget);
                let area = area.write_as::<U::Area>(pa).unwrap();
                let (child, parent) = area.push(specs, on_files, cache)?;

                let child = TypeErasedArea::new::<U>(child);
                let parent = parent.map(TypeErasedArea::new::<U>);

                Some((child, parent))
            },
            spawn: |pa, area, widget, spawn_id, specs| {
                let cache = get_cache::<U>(pa, widget);
                let area = area.write_as::<U::Area>(pa).unwrap();
                let spawned = area.spawn(spawn_id, specs, cache)?;

                Some(TypeErasedArea::new::<U>(spawned))
            },
            delete: |pa, area| {
                let area: &mut U::Area = area.write_as(pa).unwrap();

                let (do_rm_window, removed) = area.delete();

                (
                    do_rm_window,
                    removed.into_iter().map(TypeErasedArea::new::<U>).collect(),
                )
            },
            swap: |pa, lhs, rhs| {
                if lhs.area_is_eq(pa, rhs) {
                    context::warn!("Attempted two swap an Area with itself");
                    return false;
                }

                // SAFETY: The check above ensures the two Areas
                // don't point to the same thing.
                let internal_pass = &mut unsafe { Pass::new() };

                let lhs = lhs.area.write_as::<U::Area>(pa).unwrap();
                let rhs = rhs.area.write_as(internal_pass).unwrap();

                lhs.swap(rhs)
            },
            print_with: |pa, area, text, print_cfg, painter, f| {
                let area = area.read_as::<U::Area>(pa).unwrap();

                area.print_with(text, print_cfg, painter, f);
            },
            get_print_info: |pa, area| {
                let area = area.read_as::<U::Area>(pa).unwrap();

                TypeErasedPrintInfo::new::<U>(area.get_print_info())
            },
            set_print_info: |pa, area, info| {
                let area = area.write_as::<U::Area>(pa).unwrap();
                let Some(info) = info.info.downcast_ref::<<U::Area as Area>::PrintInfo>() else {
                    panic!("Attempted to get PrintInfo of the wrong type");
                };

                area.set_print_info(info.clone());
            },
            eq: |pa, lhs, rhs| {
                let lhs = lhs.read_as::<U::Area>(pa).unwrap();
                let rhs = rhs.read_as::<U::Area>(pa).unwrap();

                lhs == rhs
            },
            is_master_of: |pa, lhs, rhs| {
                let lhs = lhs.read_as::<U::Area>(pa).unwrap();
                let rhs = rhs.read_as::<U::Area>(pa).unwrap();

                lhs.is_master_of(rhs)
            },
            get_cluster_master: |pa, area| {
                let area = area.read_as::<U::Area>(pa).unwrap();
                area.get_cluster_master().map(TypeErasedArea::new::<U>)
            },
            store_cache: |pa, area, path| {
                let area = area.read_as::<U::Area>(pa).unwrap();

                if let Some(area_cache) = area.cache() {
                    Cache::new().store(path, area_cache)?;
                }

                Ok(())
            },
        }
    }
}

/// Type erased [`Area::PrintInfo`]
pub struct TypeErasedPrintInfo {
    info: Box<dyn Any + Send>,
    fns: &'static TypeErasedPrintInfoFunctions,
}

impl TypeErasedPrintInfo {
    fn new<U: Ui>(info: <U::Area as Area>::PrintInfo) -> Self {
        Self {
            info: Box::new(info),
            fns: TypeErasedPrintInfoFunctions::new::<U>(),
        }
    }
}

impl Default for TypeErasedPrintInfo {
    fn default() -> Self {
        DEFAULT_PRINT_INFO.get().expect("PrintInfo wasn't setup")()
    }
}

impl Clone for TypeErasedPrintInfo {
    fn clone(&self) -> Self {
        (self.fns.clone)(&self.info)
    }
}

impl PartialEq for TypeErasedPrintInfo {
    fn eq(&self, other: &Self) -> bool {
        (self.fns.eq)(&self.info, &other.info)
    }
}

struct TypeErasedPrintInfoFunctions {
    clone: fn(info: &(dyn Any + Send)) -> TypeErasedPrintInfo,
    eq: fn(lhs: &(dyn Any + Send), rhs: &(dyn Any + Send)) -> bool,
}

impl TypeErasedPrintInfoFunctions {
    fn new<U: Ui>() -> &'static Self {
        &Self {
            clone: |info| {
                let Some(info) = info.downcast_ref::<<U::Area as Area>::PrintInfo>() else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                TypeErasedPrintInfo {
                    info: Box::new(info.clone()),
                    fns: Self::new::<U>(),
                }
            },
            eq: |lhs, rhs| {
                let [Some(lhs), Some(rhs)] = [
                    lhs.downcast_ref::<<U::Area as Area>::PrintInfo>(),
                    rhs.downcast_ref(),
                ] else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                lhs == rhs
            },
        }
    }
}

fn get_cache<U: Ui>(pa: &Pass, widget: &RwData<dyn Widget>) -> <<U as Ui>::Area as Area>::Cache {
    if let Some(file) = widget.read_as::<File>(pa) {
        match Cache::new().load::<<U::Area as Area>::Cache>(file.path()) {
            Ok(cache) => cache,
            Err(err) => {
                context::error!("{err}");
                <U::Area as Area>::Cache::default()
            }
        }
    } else {
        <U::Area as Area>::Cache::default()
    }
}
