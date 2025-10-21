//! Traits that should be implemented by interface implementations of
//! Duat
//!
//! This module contains the traits [`Ui`] and [`RawArea`], which
//! should be used by Ui implementations of Duat. By implementing
//! those traits, you will comply with the requirements to run Duat
//! with a custom interface, such as web app or some other type of
//! GUI.
//!
//! Normally, in user code, they will encounter the
//! [`RawArea`](super::type_erased::RawArea) and (sometimes) the
//! [`Ui`](super::type_erased::Ui) from the [`type_erased`] module.
//! These are dynamic containers for the traits in this module, and
//! are used in order to improve ergonomics and compile times.
//!
//! [`type_erased`]: super::type_erased

use bincode::{Decode, Encode};

use crate::{
    form::Painter,
    opts::PrintOpts,
    session::DuatSender,
    text::{Item, SpawnId, Text, TwoPoints},
    ui::{Caret, PushSpecs, SpawnSpecs},
};

/// All the methods that a working gui/tui will need to implement in
/// order to be used by Duat.
///
/// This includes the following functionalities:
///
/// - Creating new windows, which start out with one [`RawArea`].
/// - Spawning floating [`RawArea`]s around other [`RawArea`]s.
/// - Spawning floating [`RawArea`]s in text [`Point`]s, which should
///   be able to move as the [`Text`] itself does.
/// - Pushing [`RawArea`]s around other [`RawArea`]s, which include
///   floating ones.
/// - Closing [`RawArea`]s at will, which should cascading all pushed
///   or spawned [`RawArea`]s
pub trait RawUi: Sized + Send + Sync + 'static {
    /// The [`RawArea`] of this [`Ui`]
    type Area: RawArea;

    /// Return [`Some`] only on the first call
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate. This means that you can't
    /// rely on `static` variables to contain the same values as they
    /// do in the config crate.
    fn get_once() -> Option<&'static Self>;

    /// Functions to trigger when the program begins
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate.
    fn open(&'static self, duat_tx: DuatSender);

    /// Functions to trigger when the program ends
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate.
    fn close(&'static self);

    /// Initiates and returns a new "master" [`RawArea`]
    ///
    /// This [`RawArea`] must not have any parents, and must be placed
    /// on a new window, that is, a plain region with nothing in
    /// it.
    ///
    /// [`RawArea`]: Ui::RawArea
    fn new_root(&'static self, cache: <Self::Area as RawArea>::Cache) -> Self::Area;

    /// Initiates and returns a new "floating" [`RawArea`]
    ///
    /// This is one of two ways of spawning floating [`Widget`]s. The
    /// other way is with [`RawArea::spawn`], in which a [`Widget`]
    /// will be bolted on the edges of another.
    ///
    /// TODO: There will probably be some way of defining floating
    /// [`Widget`]s with coordinates in the not too distant future as
    /// well.
    ///
    /// [`RawArea`]: Ui::RawArea
    fn new_spawned(
        &'static self,
        id: SpawnId,
        specs: SpawnSpecs,
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

    /// Unloads the [`Ui`]
    ///
    /// Unlike [`Ui::close`], this will happen both when Duat reloads
    /// the configuratio and when it closes the app.
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    fn unload(&'static self);

    /// Removes a window from the [`Ui`]
    ///
    /// This should keep the current active window consistent. That
    /// is, if the current window was ahead of the deleted one, it
    /// should be shifted back, so that the same window is still
    /// displayed.
    fn remove_window(&'static self, win: usize);
}

/// An [`RawArea`] that supports printing [`Text`]
///
/// These represent the entire GUI of Duat, the only parts of the
/// screen where text may be printed.
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
    /// only needs to include the real and ghost [`Point`]s on the
    /// top left corner in order to print correctly, but your own
    /// [`Ui`] could differ in what it needs to keep, if it makes
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
    /// And so [`RawArea::bisect`] should return `(3, None)`.
    ///
    /// [deleted]: RawArea::delete
    fn push(
        self: CoreAccess<Self>,
        specs: PushSpecs,
        on_files: bool,
        cache: Self::Cache,
    ) -> Option<(Self, Option<Self>)>;

    /// Spawns a floating area on this `RawArea`
    ///
    /// This function will take a list of [`SpawnSpecs`], taking the
    /// first one that fits, and readapting as the constraints are
    /// altered
    ///
    /// If this `RawArea` was previously [deleted], will return
    /// [`None`].
    fn spawn(
        self: CoreAccess<Self>,
        id: SpawnId,
        specs: SpawnSpecs,
        cache: Self::Cache,
    ) -> Option<Self>;

    /// Deletes this `RawArea`, signaling the closing of a
    /// [`Widget`]
    ///
    /// The first return value shall be `true` if the window housing
    /// this `RawArea` should be removed.
    ///
    /// If the [`RawArea`]'s parent was also deleted, return it.
    fn delete(self: CoreAccess<Self>) -> (bool, Vec<Self>);

    /// Swaps this `RawArea` with another one
    ///
    /// The swapped `RawArea`s will be cluster masters of the
    /// respective `RawArea`s. As such, if they belong to the same
    /// master, nothing happens.
    ///
    /// This function will _never_ be called such that one of the
    /// `RawArea`s is a decendant of the other, so the [`Ui`]
    /// implementor doesn't need to worry about that possibility.
    ///
    /// It can fail if either of the [`RawArea`]s was already deleted,
    /// or if no swap happened because they belonged to the same
    /// cluster master.
    fn swap(self: CoreAccess<Self>, rhs: &Self) -> bool;

    ////////// Constraint changing functions

    /// Changes the horizontal constraint of the area
    fn set_width(self: CoreAccess<Self>, width: f32) -> Result<(), Text>;

    /// Changes the vertical constraint of the area
    fn set_height(self: CoreAccess<Self>, height: f32) -> Result<(), Text>;

    /// Changes [`Constraint`]s such that the [`RawArea`] becomes
    /// hidden
    fn hide(self: CoreAccess<Self>) -> Result<(), Text>;

    /// Changes [`Constraint`]s such that the [`RawArea`] is revealed
    fn reveal(self: CoreAccess<Self>) -> Result<(), Text>;

    /// What width the given [`Text`] would occupy, if unwrapped
    fn width_of_text(self: CoreAccess<Self>, opts: PrintOpts, text: &Text) -> Result<f32, Text>;

    /// Tells the [`Ui`] that this [`RawArea`] is the one that is
    /// currently focused.
    ///
    /// Should make [`self`] the active [`RawArea`] while deactivating
    /// any other active [`RawArea`].
    fn set_as_active(self: CoreAccess<Self>);

    ////////// Printing functions

    /// Prints the [`Text`] via an [`Iterator`]
    fn print(self: CoreAccess<Self>, text: &Text, opts: PrintOpts, painter: Painter);

    /// Prints the [`Text`] with a callback function
    fn print_with<'a>(
        self: CoreAccess<Self>,
        text: &Text,
        opts: PrintOpts,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    );

    /// The current printing information of the area
    fn get_print_info(self: CoreAccess<Self>) -> Self::PrintInfo;

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: RawArea::PrintInfo
    fn set_print_info(self: CoreAccess<Self>, info: Self::PrintInfo);

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
        self: CoreAccess<Self>,
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
        self: CoreAccess<Self>,
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
    fn scroll_ver(self: CoreAccess<Self>, text: &Text, dist: i32, opts: PrintOpts);

    /// Scrolls the [`Text`] on all four directions until the given
    /// [`Point`] is within the [`ScrollOff`] range
    ///
    /// There are two other scrolling methods for [`RawArea`]:
    /// [`scroll_ver`] and [`scroll_to_points`]. The difference
    /// between this and [`scroll_to_points`] is that this method
    /// doesn't do anything if the [`Point`] is already on screen.
    ///
    /// [`ScrollOff`]: crate::opts::ScrollOff
    /// [`scroll_ver`]: RawArea::scroll_ver
    /// [`scroll_to_points`]: RawArea::scroll_to_points
    fn scroll_around_points(
        self: CoreAccess<Self>,
        text: &Text,
        points: TwoPoints,
        opts: PrintOpts,
    );

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
    /// [line wrapping]: crate::opts::WrapMethod
    fn scroll_to_points(self: CoreAccess<Self>, text: &Text, points: TwoPoints, opts: PrintOpts);

    /// The start points that should be printed
    fn start_points(self: CoreAccess<Self>, text: &Text, opts: PrintOpts) -> TwoPoints;

    /// The points immediately after the last printed [`Point`]
    fn end_points(self: CoreAccess<Self>, text: &Text, opts: PrintOpts) -> TwoPoints;

    ////////// Queries

    /// Whether or not [`self`] has changed
    ///
    /// This would mean anything relevant that wouldn't be determined
    /// by [`PrintInfo`], this is most likely going to be the bounding
    /// box, but it may be something else.
    ///
    /// [`PrintInfo`]: RawArea::PrintInfo
    fn has_changed(self: CoreAccess<Self>) -> bool;

    /// Whether or not [`self`] is the "master" of `other`
    ///
    /// This can only happen if, by following [`self`]'s children, you
    /// would eventually reach `other`.
    fn is_master_of(self: CoreAccess<Self>, other: &Self) -> bool;

    /// Returns the clustered master of [`self`], if there is one
    ///
    /// If [`self`] belongs to a clustered group, return the most
    /// senior member of said cluster, which must hold all other
    /// members of the cluster.
    fn get_cluster_master(self: CoreAccess<Self>) -> Option<Self>;

    /// Returns the statics from `self`
    fn cache(self: CoreAccess<Self>) -> Option<Self::Cache>;

    /// Gets the width of the area
    fn width(self: CoreAccess<Self>) -> f32;

    /// Gets the height of the area
    fn height(self: CoreAccess<Self>) -> f32;

    /// Returns `true` if this is the currently active [`RawArea`]
    ///
    /// Only one [`RawArea`] should be active at any given moment.
    fn is_active(self: CoreAccess<Self>) -> bool;
}

/// A smart pointer, meant to prevent direct calling of [`RawArea`]
/// methods
///
/// The methods of [`RawArea`] are all meant to be accessed only
/// through the type erased [`RwArea`]
#[non_exhaustive]
pub struct CoreAccess<'a, A>(&'a A);

impl<'a, A> CoreAccess<'a, A> {
    /// Returns a new instance of `CoreAccess`, to prevent direct
    /// calls to [`RawArea`] methods
    pub(super) fn new(area: &'a A) -> Self {
        CoreAccess(area)
    }
}

impl<A> std::ops::Deref for CoreAccess<'_, A> {
    type Target = A;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
