//! Traits that should be implemented by interface implementations of
//! Duat
//!
//! This module contains the traits [`RawUi`] and [`RawArea`], which
//! should be used by RawUi implementations of Duat. By implementing
//! those traits, you will comply with the requirements to run Duat
//! with a custom interface, such as web app or some other type of
//! GUI.
//!
//! Normally, in user code, they will encounter the
//! [`Area`] and (sometimes) the [`Ui`] from the `type_erased` module.
//! These are dynamic containers for the traits in this module, and
//! are used in order to improve ergonomics and compile times.
//!
//! [`Area`]: super::Area
//! [`Ui`]: super::Ui
use bincode::{Decode, Encode};

use crate::{
    form::Painter,
    opts::PrintOpts,
    session::DuatSender,
    text::{Item, SpawnId, Text, TwoPoints},
    ui::{Caret, Coord, DynSpawnSpecs, PushSpecs},
};

/// All the methods that a working gui/tui will need to implement in
/// order to be used by Duat.
///
/// This includes the following functionalities:
///
/// - Creating new windows, which start out with one [`RawArea`].
/// - Spawning floating `RawArea`s around other `RawArea`s.
/// - Spawning floating `RawArea`s in [`Text`]s, which should be able
///   to move as the `Text` itself does.
/// - Pushing `RawArea`s around other `RawArea`s, which include
///   floating ones.
/// - Closing `RawArea`s at will, which should cascading all pushed or
///   spawned `RawArea`s
///
/// # Two address spaces
///
/// With the `RawUi` and [`RawArea`] traits, there is a dystinction
/// that can be made between two address spaces. Since the `RawUi` is
/// the only thing that gets initialized in the Duat runner, rather
/// than the configuration crate, it uses the address space of Duat,
/// not the configuration, like every other thing in Duat uses.
///
/// There are two main consequences to this:
///
/// - `&'static'` references will not match (!).
/// - [`TypeId`]s will not match.
///
/// Which address space is in use is easy to tell however. If calling
/// a function from the `RawUi` or [`RawArea`] traits, then the
/// address space of Duat will be used. If calling any other function,
/// _not inherent_ to these traits, then the address space of the
/// configuration crate will be used.
///
/// [`TypeId`]: std::any::TypeId
pub trait RawUi: Sized + Send + Sync + 'static {
    /// The [`RawArea`] of this [`RawUi`]
    type Area: RawArea;

    /// Return [`Some`] only on the first call
    fn get_once() -> Option<&'static Self>;

    /// Functions to trigger when the program begins
    fn open(&'static self, duat_tx: DuatSender);

    /// Functions to trigger when the program ends
    fn close(&'static self);

    /// Initiates and returns a new "master" [`RawArea`]
    ///
    /// This [`RawArea`] must not have any parents, and must be placed
    /// on a new window, that is, a plain region with nothing in
    /// it.
    ///
    /// [`RawArea`]: RawUi::Area
    fn new_root(&'static self, cache: <Self::Area as RawArea>::Cache) -> Self::Area;

    /// Initiates and returns a new "floating" [`RawArea`]
    ///
    /// This is one of two ways of spawning floating [`Widget`]s. The
    /// other way is with [`RawArea::spawn`], in which a `Widget`
    /// will be bolted on the edges of another.
    ///
    /// TODO: There will probably be some way of defining floating
    /// `Widget`s with coordinates in the not too distant future as
    /// well.
    ///
    /// [`RawArea`]: RawUi::Area
    /// [`Widget`]: super::Widget
    fn new_spawned(
        &'static self,
        id: SpawnId,
        specs: DynSpawnSpecs,
        cache: <Self::Area as RawArea>::Cache,
        win: usize,
    ) -> Self::Area;

    /// Switches the currently active window
    ///
    /// This will only happen to with window indices that are actual
    /// windows. If at some point, a window index comes up that is not
    /// actually a window, that's a bug.
    fn switch_window(&'static self, win: usize);

    /// Flush the layout
    ///
    /// When this function is called, it means that Duat has finished
    /// adding or removing widgets, so the ui should calculate the
    /// layout.
    fn flush_layout(&'static self);

    /// Prints the layout
    ///
    /// Since printing runs all on the same thread, it is most
    /// efficient to call a printing function after all the widgets
    /// are done updating, I think.
    fn print(&'static self);

    /// Functions to trigger when the program reloads
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn load(&'static self);

    /// Unloads the [`RawUi`]
    ///
    /// Unlike [`RawUi::close`], this will happen both when Duat
    /// reloads the configuratio and when it closes the app.
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn unload(&'static self);

    /// Removes a window from the [`RawUi`]
    ///
    /// This should keep the current active window consistent. That
    /// is, if the current window was ahead of the deleted one, it
    /// should be shifted back, so that the same window is still
    /// displayed.
    fn remove_window(&'static self, win: usize);

    /// The bottom right [`Coord`] on the screen
    ///
    /// Since the top left coord is `Coord { x: 0.0, y: 0.0 }`, this
    /// is also the size of the window.
    fn size(&'static self) -> Coord;
}

/// A region on screen where you can print [`Text`]
///
/// These represent the entire GUI of Duat, the only parts of the
/// screen where text may be printed.
///
/// # Two address spaces
///
/// With the `RawUi` and `RawArea` traits, there is a dystinction
/// that can be made between two address spaces. Since the `RawUi` is
/// the only thing that gets initialized in the Duat runner, rather
/// than the configuration crate, it uses the address space of Duat,
/// not the configuration, like every other thing in Duat uses.
///
/// There are two main consequences to this:
///
/// - `&'static'` references will not match (!).
/// - [`TypeId`]s will not match.
///
/// Which address space is in use is easy to tell however. If calling
/// a function from the [`RawUi`] or `RawArea` traits, then the
/// address space of Duat will be used. If calling any other function,
/// _not inherent_ to these traits, then the address space of the
/// configuration crate will be used.
///
/// [`TypeId`]: std::any::TypeId
pub trait RawArea: Sized + PartialEq + 'static {
    /// Something to be kept between app instances/reloads
    ///
    /// The most useful thing to keep in this case is the
    /// [`PrintInfo`], but you could include other things
    ///
    /// [`PrintInfo`]: RawArea::PrintInfo
    type Cache: Default + std::fmt::Debug + Encode + Decode<()> + 'static;
    /// Information about what parts of a [`Text`] should be printed
    ///
    /// For the [`term-ui`], for example, this is quite simple, it
    /// only needs to include the [`TwoPoints`]s on the top left
    /// corner in order to print correctly, but your own [`RawUi`]
    /// could differ in what it needs to keep, if it makes
    /// use of smooth scrolling, for example.
    ///
    /// [`term-ui`]: docs.rs/term-ui/latest/term_ui
    type PrintInfo: Default + Clone + Send + Sync + PartialEq + Eq + 'static;

    ////////// RawArea modification

    /// Creates an `RawArea` around this one
    ///
    /// Will return the newly created `RawArea` as well as a parent
    /// `RawArea`, if one was created to house both of them.
    ///
    /// If this `RawArea` was previously [deleted], will return
    /// [`None`].
    ///
    /// As an example, assuming that [`self`] has an index of `0`,
    /// pushing an area to `self` on [`Side::Left`] would create
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
    /// And so this function should return `(3, None)`.
    ///
    /// [deleted]: RawArea::delete
    /// [`Side::Left`]: super::Side::Left
    /// [`Side::Right`]: super::Side::Right
    fn push(
        &self,
        _: CoreAccess,
        specs: PushSpecs,
        on_files: bool,
        cache: Self::Cache,
    ) -> Option<(Self, Option<Self>)>;

    /// Spawns a floating area on this `RawArea`
    ///
    /// This function will take a list of [`DynSpawnSpecs`], taking
    /// the first one that fits, and readapting as the constraints
    /// are altered
    ///
    /// If this `RawArea` was previously [deleted], will return
    /// [`None`].
    ///
    /// [deleted]: RawArea::delete
    fn spawn(
        &self,
        _: CoreAccess,
        id: SpawnId,
        specs: DynSpawnSpecs,
        cache: Self::Cache,
    ) -> Option<Self>;

    /// Deletes this `RawArea`, signaling the closing of a
    /// [`Widget`]
    ///
    /// The first return value shall be `true` if the window housing
    /// this `RawArea` should be removed.
    ///
    /// If the `RawArea`'s parent was also deleted, return it.
    ///
    /// [`Widget`]: super::Widget
    fn delete(&self, _: CoreAccess) -> (bool, Vec<Self>);

    /// Swaps this `RawArea` with another one
    ///
    /// The swapped `RawArea`s will be cluster masters of the
    /// respective `RawArea`s. As such, if they belong to the same
    /// master, nothing happens.
    ///
    /// This function will _never_ be called such that one of the
    /// `RawArea`s is a decendant of the other, so the [`RawUi`]
    /// implementor doesn't need to worry about that possibility.
    ///
    /// It can fail if either of the `RawArea`s was already deleted,
    /// or if no swap happened because they belonged to the same
    /// cluster master.
    fn swap(&self, _: CoreAccess, rhs: &Self) -> bool;

    ////////// Constraint changing functions

    /// Sets a width for the `RawArea`
    fn set_width(&self, _: CoreAccess, width: f32) -> Result<(), Text>;

    /// Sets a height for the `RawArea`
    fn set_height(&self, _: CoreAccess, height: f32) -> Result<(), Text>;

    /// Hides the `RawArea`
    fn hide(&self, _: CoreAccess) -> Result<(), Text>;

    /// Reveals the `RawArea`
    fn reveal(&self, _: CoreAccess) -> Result<(), Text>;

    /// What width the given [`Text`] would occupy, if unwrapped
    fn width_of_text(&self, _: CoreAccess, opts: PrintOpts, text: &Text) -> Result<f32, Text>;

    /// Tells the [`RawUi`] that this `RawArea` is the one that is
    /// currently focused.
    ///
    /// Should make `self` the active `RawArea` while deactivating
    /// any other active `RawArea`.
    fn set_as_active(&self, _: CoreAccess);

    ////////// Printing functions

    /// Prints the [`Text`]
    fn print(&self, _: CoreAccess, text: &Text, opts: PrintOpts, painter: Painter);

    /// Prints the [`Text`] with a callback function
    fn print_with<'a>(
        &self,
        _: CoreAccess,
        text: &Text,
        opts: PrintOpts,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    );

    /// The current printing information of the area
    fn get_print_info(&self, _: CoreAccess) -> Self::PrintInfo;

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: RawArea::PrintInfo
    fn set_print_info(&self, _: CoreAccess, info: Self::PrintInfo);

    /// Returns a printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns an iterator
    /// which assigns to each of them a [`Caret`]. This struct
    /// essentially represents where horizontally would this character
    /// be printed.
    ///
    /// If you want a reverse iterator, see
    /// [`RawArea::rev_print_iter`].
    ///
    /// [`text::Item`]: Item
    fn print_iter<'a>(
        &self,
        _: CoreAccess,
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> impl Iterator<Item = (Caret, Item)> + 'a;

    /// Returns a reversed printing iterator
    ///
    /// Given an iterator of [`text::Item`]s, returns a reversed
    /// iterator which assigns to each of them a [`Caret`]. This
    /// struct essentially represents where horizontally each
    /// character would be printed.
    ///
    /// If you want a forwards iterator, see [`RawArea::print_iter`].
    ///
    /// [`text::Item`]: Item
    fn rev_print_iter<'a>(
        &self,
        _: CoreAccess,
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> impl Iterator<Item = (Caret, Item)> + 'a;

    ////////// Points functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    fn scroll_ver(&self, _: CoreAccess, text: &Text, dist: i32, opts: PrintOpts);

    /// Scrolls the [`Text`] on all four directions until the given
    /// [`TwoPoints`] is within the [`ScrollOff`] range
    ///
    /// There are two other scrolling methods for `RawArea`:
    /// [`scroll_ver`] and [`scroll_to_points`]. The difference
    /// between this and [`scroll_to_points`] is that this method
    /// doesn't do anything if the [`TwoPoints`] is already on screen.
    ///
    /// [`ScrollOff`]: crate::opts::ScrollOff
    /// [`scroll_ver`]: RawArea::scroll_ver
    /// [`scroll_to_points`]: RawArea::scroll_to_points
    fn scroll_around_points(&self, _: CoreAccess, text: &Text, points: TwoPoints, opts: PrintOpts);

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
    /// [line wrapping]: crate::opts::PrintOpts::wrap_lines
    fn scroll_to_points(&self, _: CoreAccess, text: &Text, points: TwoPoints, opts: PrintOpts);

    /// The start points that should be printed
    fn start_points(&self, _: CoreAccess, text: &Text, opts: PrintOpts) -> TwoPoints;

    /// The [`TwoPoints`] immediately after the last printed one
    fn end_points(&self, _: CoreAccess, text: &Text, opts: PrintOpts) -> TwoPoints;

    ////////// Queries

    /// Whether or not [`self`] has changed
    ///
    /// This would mean anything relevant that wouldn't be determined
    /// by [`PrintInfo`], this is most likely going to be the bounding
    /// box, but it may be something else.
    ///
    /// [`PrintInfo`]: RawArea::PrintInfo
    fn has_changed(&self, _: CoreAccess) -> bool;

    /// Whether or not [`self`] is the "master" of `other`
    ///
    /// This can only happen if, by following [`self`]'s children, you
    /// would eventually reach `other`.
    fn is_master_of(&self, _: CoreAccess, other: &Self) -> bool;

    /// Returns the clustered master of [`self`], if there is one
    ///
    /// If [`self`] belongs to a clustered group, return the most
    /// senior member of said cluster, which must hold all other
    /// members of the cluster.
    fn get_cluster_master(&self, _: CoreAccess) -> Option<Self>;

    /// Returns the statics from `self`
    fn cache(&self, _: CoreAccess) -> Option<Self::Cache>;

    /// The top left [`Coord`] of this `Area`
    fn top_left(&self, _: CoreAccess) -> Coord;

    /// The bottom right [`Coord`] of this `Area`
    fn bottom_right(&self, _: CoreAccess) -> Coord;

    /// The [`Coord`] where the given [`TwoPoints`] would be printed
    ///
    /// Returns [`None`] if the `TwoPoints` are not part of the [`Text`]
    fn coord_at_points(
        &self,
        _: CoreAccess,
        text: &Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Option<Coord>;

    /// The [`TwoPoints`] where a [`Coord`] is found
    ///
    /// Returns [`None`] if either the `RawArea` does not contain
    /// the given `Coord`, or if the `Coord` is in a position where
    /// [`Text`] is not printed.
    fn points_at_coord(
        &self,
        _: CoreAccess,
        text: &Text,
        coord: Coord,
        opts: PrintOpts,
    ) -> Option<TwoPoints>;

    /// Returns `true` if this is the currently active `RawArea`
    ///
    /// Only one `RawArea` should be active at any given moment.
    fn is_active(&self, _: CoreAccess) -> bool;
}

/// Prevents direct use of the [`RawArea`] functions
///
/// The methods of `RawArea` are all meant to be accessed only
/// through the type erased [`RwArea`] and [`Area`]
///
/// [`RwArea`]: super::RwArea
/// [`Area`]: super::Area
#[non_exhaustive]
#[derive(Clone, Copy)]
pub struct CoreAccess {}

impl CoreAccess {
    pub(super) fn new() -> Self {
        CoreAccess {}
    }
}
