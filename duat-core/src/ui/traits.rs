//! Traits that should be implemented by interface implementations of
//! Duat
//!
//! This module contains the traits [`Ui`] and [`Area`], which should
//! be used by Ui implementations of Duat. By implementing those
//! traits, you will comply with the requirements to run Duat with a
//! custom interface, such as web app or some other type of GUI.
//!
//! Normally, in user code, they will encounter the
//! [`Area`](super::type_erased::Area) and (sometimes) the
//! [`Ui`](super::type_erased::Ui) from the [`type_erased`] module.
//! These are dynamic containers for the traits in this module, and
//! are used in order to improve ergonomics and compile times.
//!
//! [`type_erased`]: super::type_erased

use bincode::{Decode, Encode};

use crate::{
    cfg::PrintCfg,
    form::Painter,
    session::DuatSender,
    text::{FwdIter, Item, Point, RevIter, SpawnId, Text},
    ui::{Caret, PushSpecs, SpawnSpecs},
};

/// All the methods that a working gui/tui will need to implement in
/// order to be used by Duat.
///
/// This includes the following functionalities:
///
/// - Creating new windows, which start out with one [`Area`].
/// - Spawning floating [`Area`]s around other [`Area`]s.
/// - Spawning floating [`Area`]s in text [`Point`]s, which should be
///   able to move as the [`Text`] itself does.
/// - Pushing [`Area`]s around other [`Area`]s, which include floating
///   ones.
/// - Closing [`Area`]s at will, which should cascading all pushed or
///   spawned [`Area`]s
pub trait Ui: Send + Sync + 'static {
    /// The [`Area`] of this [`Ui`]
    type Area: Area
    where
        Self: Sized;

    /// Return [`Some`] only on the first call
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate. This means that you can't
    /// rely on `static` variables to contain the same values as they
    /// do in the config crate.
    fn get_once() -> Option<&'static Self>
    where
        Self: Sized;

    /// Functions to trigger when the program begins
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate.
    fn open(&self, duat_tx: DuatSender);

    /// Functions to trigger when the program ends
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate.
    fn close(&self);

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
    type Cache: Default + std::fmt::Debug + Encode + Decode<()> + 'static
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
