//! Type erased versions of [`ui`] elements
//!
//! This module serves the purpose of keeping Duat code less verbose,
//! by making it so end users and plugin writers alike don't have to
//! worry about implementing their code for specific
//! [`RawUi`] versions.
//!
//! This works by type erasing the `Ui` and [`RawArea`] traits, as
//! well as [`RawArea::PrintInfo`], so that they become [`Any`], even
//! though they have many elements that should not be `dyn`
//! compatible.
//!
//! Of course, you can still go into specifics about certain `Area`
//! implementations with the [`RwArea::read_as`] method.
//!
//! [`ui`]: super
//! [`Area`]: super::traits::Area
//! [`Area::PrintInfo`]: super::traits::Area::PrintInfo
use std::{any::Any, path::Path, sync::OnceLock};

use crate::{
    context::{self, Cache},
    data::{Pass, RwData},
    form::Painter,
    opts::PrintOpts,
    session::DuatSender,
    text::{Item, SpawnId, Text, TwoPoints},
    ui::{
        Caret, PushSpecs, SpawnSpecs,
        traits::{CoreAccess, RawArea, RawUi},
    },
};

/// A type erased [`Ui`]
#[derive(Clone, Copy)]
pub struct Ui {
    ui: &'static (dyn Any + Send + Sync),
    fns: &'static UiFunctions,
    default_print_info: fn() -> PrintInfo,
}

impl Ui {
    /// Returns a new type erased [`Ui`]
    ///
    /// Given the [`RawUi::get_once`] function, this should only be
    /// callable _once_.
    pub fn new<U: RawUi>() -> Self
    where
        U::Area: PartialEq,
    {
        Ui {
            ui: U::get_once().expect("Ui was acquired more than once"),
            fns: UiFunctions::new::<U>(),
            default_print_info: || PrintInfo::new::<U>(<U::Area as RawArea>::PrintInfo::default()),
        }
    }

    /// Functions to trigger when the program begins
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate.
    pub fn open(&self, duat_tx: DuatSender) {
        (self.fns.open)(self.ui, duat_tx)
    }

    /// Functions to trigger when the program ends
    ///
    /// Will happen on the address space of the Duat application,
    /// rather than the configuration crate.
    pub fn close(&self) {
        (self.fns.close)(self.ui)
    }

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: RawUi::Area
    pub fn new_root(&self, file_path: Option<&Path>) -> RwArea {
        (self.fns.new_root)(self.ui, file_path)
    }

    /// Initiates and returns a new "floating" [`Area`]
    ///
    /// This is one of two ways of spawning floating [`Widget`]s. The
    /// other way is with [`RawArea::spawn`], in which a [`Widget`]
    /// will be bolted on the edges of another.
    ///
    /// TODO: There will probably be some way of defining floating
    /// [`Widget`]s with coordinates in the not too distant future as
    /// well.
    ///
    /// [`Area`]: RawUi::Area
    /// [`Widget`]: super::Widget
    pub fn new_spawned(
        &self,
        file_path: Option<&Path>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
        win: usize,
    ) -> RwArea {
        (self.fns.new_spawned)(self.ui, file_path, spawn_id, specs, win)
    }

    /// Switches the currently active window
    ///
    /// This will only happen to with window indices that are actual
    /// windows. If at some point, a window index comes up that is not
    /// actually a window, that's a bug.
    pub fn switch_window(&self, win: usize) {
        (self.fns.switch_window)(self.ui, win)
    }

    /// Flush the layout
    ///
    /// When this function is called, it means that Duat has finished
    /// adding or removing widgets, so the ui should calculate the
    /// layout.
    pub fn flush_layout(&self) {
        (self.fns.flush_layout)(self.ui)
    }

    /// Prints the layout
    ///
    /// Since printing runs all on the same thread, it is most
    /// efficient to call a printing function after all the widgets
    /// are done updating, I think.
    pub fn print(&self) {
        (self.fns.print)(self.ui)
    }

    /// Functions to trigger when the program reloads
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    pub fn load(&self) {
        (self.fns.load)(self.ui)
    }

    /// Unloads the [`RawUi`]
    ///
    /// Unlike [`RawUi::close`], this will happen both when Duat
    /// reloads the configuratio and when it closes the app.
    ///
    /// These will happen inside of the dynamically loaded config
    /// crate.
    pub fn unload(&self) {
        (self.fns.unload)(self.ui)
    }

    /// Removes a window from the [`RawUi`]
    ///
    /// This should keep the current active window consistent. That
    /// is, if the current window was ahead of the deleted one, it
    /// should be shifted back, so that the same window is still
    /// displayed.
    pub fn remove_window(&self, win: usize) {
        (self.fns.remove_window)(self.ui, win)
    }

    /// Sets the default [`PrintInfo`]
    pub(crate) fn setup_default_print_info(&self) {
        DEFAULT_PRINT_INFO
            .set(self.default_print_info)
            .expect("PrintInfo was set twice");
    }
}

struct UiFunctions {
    open: fn(&'static dyn Any, DuatSender),
    close: fn(&'static dyn Any),
    new_root: fn(&'static dyn Any, Option<&Path>) -> RwArea,
    new_spawned: fn(&'static dyn Any, Option<&Path>, SpawnId, SpawnSpecs, usize) -> RwArea,
    switch_window: fn(&'static dyn Any, win: usize),
    flush_layout: fn(&'static dyn Any),
    print: fn(&'static dyn Any),
    load: fn(&'static dyn Any),
    unload: fn(&'static dyn Any),
    remove_window: fn(&'static dyn Any, win: usize),
}

impl UiFunctions {
    const fn new<U: RawUi>() -> &'static Self {
        &Self {
            open: |ui, duat_tx| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.open(duat_tx)
            },
            close: |ui| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.close()
            },
            new_root: |ui, file_path| {
                let ui = ui.downcast_ref::<U>().unwrap();
                RwArea::new::<U>(ui.new_root(get_cache::<U>(file_path)))
            },
            new_spawned: |ui, file_path, spawn_id, specs, win| {
                let ui = ui.downcast_ref::<U>().unwrap();
                RwArea::new::<U>(ui.new_spawned(spawn_id, specs, get_cache::<U>(file_path), win))
            },
            switch_window: |ui, win| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.switch_window(win);
            },
            flush_layout: |ui| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.flush_layout();
            },
            print: |ui| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.print();
            },
            load: |ui| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.load();
            },
            unload: |ui| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.unload();
            },
            remove_window: |ui, win| {
                let ui = ui.downcast_ref::<U>().unwrap();
                ui.remove_window(win);
            },
        }
    }
}

/// A type erased [`RawArea`]
///
/// This type houses the inner `Area`, and provides type erased access
/// to its functions.
#[derive(Clone)]
pub struct RwArea(pub(crate) RwData<Area>);

impl RwArea {
    /// Returns a new type erased [`RawArea`]
    ///
    /// This is the only moment where the [`RawUi`] and `Area` will be
    /// statically known.
    fn new<U: RawUi>(area: U::Area) -> Self {
        Self(RwData::new(Area {
            inner: Box::new(area),
            fns: AreaFunctions::new::<U>(),
        }))
    }

    /// Shared access to an [`Area`]
    ///
    /// You should use this if you want "prolonged" access to the
    /// `Area`'s methods, without necessarily bringing a [`Pass`]
    /// with you.
    pub fn read<'a>(&'a self, pa: &'a Pass) -> &'a Area {
        self.0.read(pa)
    }

    /// Mutable access to an [`Area`]
    ///
    /// You should use this if you want "prolonged" access to the
    /// `Area`'s methods, without necessarily bringing a [`Pass`]
    /// with you.
    pub fn write<'a>(&'a self, pa: &'a mut Pass) -> &'a mut Area {
        self.0.write(pa)
    }

    /// Attempt to read this as a specific implementation of
    /// [`RawArea`]
    ///
    /// You can use this to deal with individual [`RawArea`]s, so you
    /// can do a "per Ui" configuration for your
    /// [`Plugin`]/configuration.
    ///
    /// This will return [`None`] if the `RawArea` within is of a
    /// different type.
    ///
    /// [`Plugin`]: crate::Plugin
    pub fn read_as<'a, A: RawArea>(&'a self, pa: &'a Pass) -> Option<&'a A> {
        self.0.read(pa).inner.downcast_ref()
    }

    /// Attempt to write this as a specific implementation of
    /// [`RawArea`]
    ///
    /// You can use this to deal with individual `RawArea`s, so you
    /// can do a "per Ui" configuration for your
    /// [`Plugin`]/configuration. This could be used to, for example,
    /// place frames around an [`Area`] when making use of a terminal
    /// `Ui`, or using some custom css when using a web `Ui`.
    ///
    /// This will return [`None`] if the `RawArea` within is of a
    /// different type.
    ///
    /// [`Plugin`]: crate::Plugin
    pub fn write_as<'a, A: RawArea>(&'a self, pa: &'a mut Pass) -> Option<&'a mut A> {
        self.0.write(pa).inner.downcast_mut()
    }

    ////////// Area Modification functions

    /// Pushes a [`Widget`] to this [`Area`]
    ///
    /// [`Widget`]: super::Widget
    pub(super) fn push(
        &self,
        pa: &mut Pass,
        file_path: Option<&Path>,
        specs: PushSpecs,
        on_files: bool,
    ) -> Option<(Self, Option<Self>)> {
        (self.0.read(pa).fns.push)(self.0.read(pa), file_path, specs, on_files)
    }

    /// Spawns a [`Widget`] on this [`Area`]
    ///
    /// [`Widget`]: super::Widget
    pub(super) fn spawn(
        &self,
        pa: &mut Pass,
        file_path: Option<&Path>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
    ) -> Option<Self> {
        (self.0.read(pa).fns.spawn)(self.0.read(pa), file_path, spawn_id, specs)
    }

    /// Deletes this [`Area`], returning wether the window should be
    /// removed, as well as all the other ares that were deleted
    pub(super) fn delete(&self, pa: &mut Pass) -> (bool, Vec<Self>) {
        (self.0.read(pa).fns.delete)(self.0.read(pa))
    }

    /// Swaps this [`Area`] with another
    pub(super) fn swap(&self, pa: &mut Pass, rhs: &Self) -> bool {
        (self.0.read(pa).fns.swap)(self.0.read(pa), rhs.read(pa))
    }

    ////////// Constraint changing functions

    /// Sets the width of  the [`RawArea`]
    pub fn set_width(&self, pa: &mut Pass, width: f32) -> Result<(), Text> {
        self.0.write(pa).set_width(width)
    }

    /// Sets the height of  the [`RawArea`]
    pub fn set_height(&self, pa: &mut Pass, height: f32) -> Result<(), Text> {
        self.0.write(pa).set_height(height)
    }

    /// Hides the [`RawArea`]
    pub fn hide(&self, pa: &mut Pass) -> Result<(), Text> {
        self.0.write(pa).hide()
    }

    /// Reveals the [`RawArea`]
    pub fn reveal(&self, pa: &mut Pass) -> Result<(), Text> {
        self.0.write(pa).reveal()
    }

    /// Tells the [`RawUi`] that this [`RawArea`] is the one that is
    /// currently focused.
    ///
    /// Should make `self` the active `RawArea` while deactivating
    /// any other active `RawArea`.
    pub fn set_as_active(&self, pa: &mut Pass) {
        self.0.write(pa).set_as_active()
    }

    /// What width the given [`Text`] would occupy, if unwrapped
    pub fn width_of_text(&self, pa: &Pass, opts: PrintOpts, text: &Text) -> Result<f32, Text> {
        self.0.read(pa).width_of_text(opts, text)
    }

    ////////// Printing functions

    /// Prints the [`Text`]
    pub fn print(&self, pa: &Pass, text: &Text, opts: PrintOpts, painter: Painter) {
        self.0.read(pa).print(text, opts, painter)
    }

    /// Prints the [`Text`] with a callback function
    pub fn print_with<'a>(
        &self,
        pa: &Pass,
        text: &Text,
        opts: PrintOpts,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        self.0.read(pa).print_with(text, opts, painter, Box::new(f))
    }

    /// The current printing information of the area
    pub fn get_print_info(&self, pa: &Pass) -> PrintInfo {
        self.0.read(pa).get_print_info()
    }

    /// Sets a previously acquired [`PrintInfo`] to the area
    pub fn set_print_info(&self, pa: &mut Pass, info: PrintInfo) {
        self.0.write(pa).set_print_info(info)
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
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a> {
        self.0.read(pa).print_iter(text, points, opts)
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
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a> {
        self.0.read(pa).rev_print_iter(text, points, opts)
    }

    ////////// Points functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    pub fn scroll_ver(&self, pa: &mut Pass, text: &Text, dist: i32, opts: PrintOpts) {
        self.0.write(pa).scroll_ver(text, dist, opts)
    }

    /// Scrolls the [`Text`] on all four directions until the given
    /// [`TwoPoints`] is within the [`ScrollOff`] range
    ///
    /// There are two other scrolling methods for [`Area`]:
    /// [`scroll_ver`] and [`scroll_to_points`]. The difference
    /// between this and [`scroll_to_points`] is that this method
    /// doesn't do anything if the [`TwoPoints`] is already on screen.
    ///
    /// [`ScrollOff`]: crate::opts::ScrollOff
    /// [`scroll_ver`]: Area::scroll_ver
    /// [`scroll_to_points`]: Area::scroll_to_points
    pub fn scroll_around_points(
        &self,
        pa: &mut Pass,
        text: &Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) {
        self.0.write(pa).scroll_around_points(text, points, opts)
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
    /// [line wrapping]: crate::opts::PrintOpts::wrap_lines
    pub fn scroll_to_points(&self, pa: &mut Pass, text: &Text, points: TwoPoints, opts: PrintOpts) {
        self.0.write(pa).scroll_to_points(text, points, opts)
    }

    /// Scrolls the [`Area`] to the given [`TwoPoints`]
    pub fn start_points(&self, pa: &Pass, text: &Text, opts: PrintOpts) -> TwoPoints {
        self.0.read(pa).start_points(text, opts)
    }

    /// Scrolls the [`Area`] to the given [`TwoPoints`]
    pub fn end_points(&self, pa: &Pass, text: &Text, opts: PrintOpts) -> TwoPoints {
        self.0.read(pa).end_points(text, opts)
    }

    /////////// Querying functions

    /// Wether this [`Area`] has changed since last being printed
    pub fn has_changed(&self, pa: &Pass) -> bool {
        self.0.read(pa).has_changed()
    }

    /// Whether or not this [`Area`] is the "master" of another
    pub fn is_master_of(&self, pa: &Pass, other: &Self) -> bool {
        self.0.read(pa).is_master_of(other.read(pa))
    }

    /// Returns the clustered master of the [`Area`], if there is one
    pub(crate) fn get_cluster_master(&self, pa: &Pass) -> Option<Self> {
        (self.0.read(pa).fns.get_cluster_master)(self.0.read(pa))
    }

    /// Stores the cache of the [`Area`], given a path to associate
    /// with this cache
    pub fn store_cache(&self, pa: &Pass, path: &str) -> Result<(), Text> {
        (self.0.read(pa).fns.store_cache)(self.0.read(pa), path)
    }

    /// Gets the width of the area
    pub fn width(&self, pa: &Pass) -> f32 {
        self.0.read(pa).width()
    }

    /// Gets the height of the area
    pub fn height(&self, pa: &Pass) -> f32 {
        self.0.read(pa).height()
    }

    /// Returns `true` if this is the currently active [`Area`]
    ///
    /// Only one `Area` should be active at any given moment.
    pub fn is_active(&self, pa: &Pass) -> bool {
        self.0.read(pa).is_active()
    }

    /// Wether this [`Area`] is the same as another
    pub fn area_is_eq(&self, pa: &Pass, other: &RwArea) -> bool {
        self.0.read(pa).area_is_eq(other.0.read(pa))
    }
}

/// A type erased [`RawUi::Area`]
///
/// This struct is accessed by calling [`RwArea::read`] or
/// [`RwData::write`].
pub struct Area {
    inner: Box<dyn Any>,
    fns: &'static AreaFunctions,
}

impl Area {
    /// Sets the width of  the [`RawArea`]
    pub fn set_width(&mut self, width: f32) -> Result<(), Text> {
        (self.fns.set_width)(self, width)
    }

    /// Sets the height of  the [`RawArea`]
    pub fn set_height(&mut self, height: f32) -> Result<(), Text> {
        (self.fns.set_height)(self, height)
    }

    /// Hides the [`RawArea`]
    pub fn hide(&mut self) -> Result<(), Text> {
        (self.fns.hide)(self)
    }

    /// Reveals the [`RawArea`]
    pub fn reveal(&mut self) -> Result<(), Text> {
        (self.fns.reveal)(self)
    }

    /// Tells the [`RawUi`] that this [`RawArea`] is the one that is
    /// currently focused.
    ///
    /// Should make `self` the active [`RawArea`] while deactivating
    /// any other active [`RawArea`].
    pub fn set_as_active(&mut self) {
        (self.fns.set_as_active)(self)
    }

    /// What width the given [`Text`] would occupy, if unwrapped
    pub fn width_of_text(&self, opts: PrintOpts, text: &Text) -> Result<f32, Text> {
        (self.fns.width_of_text)(self, opts, text)
    }

    ////////// Printing functions

    /// Prints the [`Text`] via an [`Iterator`]
    pub fn print(&self, text: &Text, opts: PrintOpts, painter: Painter) {
        (self.fns.print)(self, text, opts, painter)
    }

    /// Prints the [`Text`] with a callback function
    pub fn print_with<'a>(
        &self,
        text: &Text,
        opts: PrintOpts,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        (self.fns.print_with)(self, text, opts, painter, Box::new(f))
    }

    /// The current printing information of the area
    pub fn get_print_info(&self) -> PrintInfo {
        (self.fns.get_print_info)(self)
    }

    /// Sets a previously acquired [`PrintInfo`] to the area
    pub fn set_print_info(&mut self, info: PrintInfo) {
        (self.fns.set_print_info)(self, info)
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
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a> {
        (self.fns.print_iter)(self, text, points, opts)
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
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a> {
        (self.fns.rev_print_iter)(self, text, points, opts)
    }

    ////////// Points functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    pub fn scroll_ver(&mut self, text: &Text, dist: i32, opts: PrintOpts) {
        (self.fns.scroll_ver)(self, text, dist, opts);
    }

    /// Scrolls the [`Text`] on all four directions until the given
    /// [`TwoPoints`] is within the [`ScrollOff`] range
    ///
    /// There are two other scrolling methods for `Area`:
    /// [`scroll_ver`] and [`scroll_to_points`]. The difference
    /// between this and [`scroll_to_points`] is that this method
    /// doesn't do anything if the [`TwoPoints`] is already on screen.
    ///
    /// [`ScrollOff`]: crate::opts::ScrollOff
    /// [`scroll_ver`]: Area::scroll_ver
    /// [`scroll_to_points`]: Area::scroll_to_points
    pub fn scroll_around_points(&mut self, text: &Text, points: TwoPoints, opts: PrintOpts) {
        (self.fns.scroll_around_points)(self, text, points, opts);
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
    /// [line wrapping]: crate::opts::PrintOpts::wrap_lines
    pub fn scroll_to_points(&mut self, text: &Text, points: TwoPoints, opts: PrintOpts) {
        (self.fns.scroll_to_points)(self, text, points, opts);
    }

    /// Scrolls the `Area` to the given [`TwoPoints`]
    pub fn start_points(&self, text: &Text, opts: PrintOpts) -> TwoPoints {
        (self.fns.start_points)(self, text, opts)
    }

    /// Scrolls the `Area` to the given [`TwoPoints`]
    pub fn end_points(&self, text: &Text, opts: PrintOpts) -> TwoPoints {
        (self.fns.end_points)(self, text, opts)
    }

    /////////// Querying functions

    /// Wether this `Area` has changed since last being printed
    pub fn has_changed(&self) -> bool {
        (self.fns.has_changed)(self)
    }

    /// Whether or not this `Area` is the "master" of another
    pub fn is_master_of(&self, other: &Self) -> bool {
        (self.fns.is_master_of)(self, other)
    }

    /// Stores the cache of the `Area`, given a path to associate
    /// with this cache
    pub fn store_cache(&mut self, path: &str) -> Result<(), Text> {
        (self.fns.store_cache)(self, path)
    }

    /// Gets the width of the area
    pub fn width(&self) -> f32 {
        (self.fns.width)(self)
    }

    /// Gets the height of the area
    pub fn height(&self) -> f32 {
        (self.fns.height)(self)
    }

    /// Returns `true` if this is the currently active `Area`
    ///
    /// Only one `Area` should be active at any given moment.
    pub fn is_active(&self) -> bool {
        (self.fns.is_active)(self)
    }

    /// Wether this `Area` is the same as another
    pub fn area_is_eq(&self, other: &Area) -> bool {
        (self.fns.eq)(self, other)
    }
}

#[derive(Clone, Copy)]
struct AreaFunctions {
    push: fn(&Area, Option<&Path>, PushSpecs, bool) -> Option<(RwArea, Option<RwArea>)>,
    spawn: fn(&Area, Option<&Path>, SpawnId, SpawnSpecs) -> Option<RwArea>,
    delete: fn(&Area) -> (bool, Vec<RwArea>),
    swap: fn(&Area, &Area) -> bool,
    set_width: fn(&Area, width: f32) -> Result<(), Text>,
    set_height: fn(&Area, height: f32) -> Result<(), Text>,
    hide: fn(&Area) -> Result<(), Text>,
    reveal: fn(&Area) -> Result<(), Text>,
    width_of_text: fn(&Area, opts: PrintOpts, text: &Text) -> Result<f32, Text>,
    set_as_active: fn(&Area),
    print: fn(&Area, text: &Text, opts: PrintOpts, painter: Painter),
    print_with: for<'a> fn(&Area, &Text, PrintOpts, Painter, Box<dyn FnMut(&Caret, &Item) + 'a>),
    get_print_info: fn(&Area) -> PrintInfo,
    set_print_info: fn(&Area, PrintInfo),
    print_iter: for<'a> fn(
        &Area,
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a>,
    rev_print_iter: for<'a> fn(
        &Area,
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Box<dyn Iterator<Item = (Caret, Item)> + 'a>,
    scroll_ver: fn(&Area, text: &Text, dist: i32, opts: PrintOpts),
    scroll_around_points: fn(&Area, text: &Text, points: TwoPoints, opts: PrintOpts),
    scroll_to_points: fn(&Area, text: &Text, points: TwoPoints, opts: PrintOpts),
    start_points: fn(&Area, text: &Text, opts: PrintOpts) -> TwoPoints,
    end_points: fn(&Area, text: &Text, opts: PrintOpts) -> TwoPoints,
    has_changed: fn(&Area) -> bool,
    is_master_of: fn(&Area, &Area) -> bool,
    get_cluster_master: fn(&Area) -> Option<RwArea>,
    store_cache: fn(&Area, &str) -> Result<(), Text>,
    eq: fn(&Area, &Area) -> bool,
    width: fn(&Area) -> f32,
    height: fn(&Area) -> f32,
    is_active: fn(&Area) -> bool,
}

impl AreaFunctions {
    const fn new<U: RawUi>() -> &'static Self {
        &Self {
            push: |area, file_path, specs, on_files| {
                let cache = get_cache::<U>(file_path);
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                let (child, parent) = CoreAccess::new(area).push(specs, on_files, cache)?;

                Some((RwArea::new::<U>(child), parent.map(RwArea::new::<U>)))
            },
            spawn: |area, file_path, spawn_id, specs| {
                let cache = get_cache::<U>(file_path);
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                let spawned = CoreAccess::new(area).spawn(spawn_id, specs, cache)?;

                Some(RwArea::new::<U>(spawned))
            },
            delete: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                let (do_rm_window, removed) = CoreAccess::new(area).delete();

                (
                    do_rm_window,
                    removed.into_iter().map(RwArea::new::<U>).collect(),
                )
            },
            swap: |lhs, rhs| {
                let lhs = lhs.inner.downcast_ref::<U::Area>().unwrap();
                let rhs = rhs.inner.downcast_ref::<U::Area>().unwrap();

                if lhs == rhs {
                    context::warn!("Attempted two swap an Area with itself");
                    return false;
                }

                CoreAccess::new(lhs).swap(rhs)
            },
            set_width: |area, width| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).set_width(width)
            },
            set_height: |area, height| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).set_height(height)
            },
            hide: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).hide()
            },
            reveal: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).reveal()
            },
            width_of_text: |area, opts, text| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).width_of_text(opts, text)
            },
            set_as_active: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).set_as_active()
            },
            print: |area, text, opts, painter| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).print(text, opts, painter)
            },
            print_with: |area, text, print_opts, painter, f| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).print_with(text, print_opts, painter, f);
            },
            get_print_info: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                PrintInfo::new::<U>(CoreAccess::new(area).get_print_info())
            },
            set_print_info: |area, info| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                let Some(info) = info
                    .info
                    .as_ref()
                    .downcast_ref::<<U::Area as RawArea>::PrintInfo>()
                else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                CoreAccess::new(area).set_print_info(info.clone());
            },
            print_iter: |area, text, points, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                Box::new(CoreAccess::new(area).print_iter(text, points, opts))
            },
            rev_print_iter: |area, text, points, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                Box::new(CoreAccess::new(area).rev_print_iter(text, points, opts))
            },
            scroll_ver: |area, text, dist, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).scroll_ver(text, dist, opts)
            },
            scroll_around_points: |area, text, dist, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).scroll_around_points(text, dist, opts)
            },
            scroll_to_points: |area, text, dist, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).scroll_to_points(text, dist, opts)
            },
            start_points: |area, text, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).start_points(text, opts)
            },
            end_points: |area, text, opts| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).end_points(text, opts)
            },
            has_changed: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).has_changed()
            },
            eq: |lhs, rhs| {
                let lhs = lhs.inner.downcast_ref::<U::Area>().unwrap();
                let rhs = rhs.inner.downcast_ref::<U::Area>().unwrap();

                lhs == rhs
            },
            is_master_of: |lhs, rhs| {
                let lhs = lhs.inner.downcast_ref::<U::Area>().unwrap();
                let rhs = rhs.inner.downcast_ref::<U::Area>().unwrap();

                CoreAccess::new(lhs).is_master_of(rhs)
            },
            get_cluster_master: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area)
                    .get_cluster_master()
                    .map(RwArea::new::<U>)
            },
            store_cache: |area, path| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();

                if let Some(area_cache) = CoreAccess::new(area).cache() {
                    Cache::new().store(path, area_cache)?;
                }

                Ok(())
            },
            width: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).width()
            },
            height: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).height()
            },
            is_active: |area| {
                let area = area.inner.downcast_ref::<U::Area>().unwrap();
                CoreAccess::new(area).is_active()
            },
        }
    }
}

/// Type erased [`RawArea::PrintInfo`]
///
/// This is information mostly about _from where_ to print a [`Text`]
/// from.
pub struct PrintInfo {
    info: Box<dyn Any + Send>,
    fns: &'static PrintInfoFunctions,
}

impl PrintInfo {
    /// Creates a new `PrintInfo`
    fn new<U: RawUi>(info: <U::Area as RawArea>::PrintInfo) -> Self {
        Self {
            info: Box::new(info),
            fns: PrintInfoFunctions::new::<U>(),
        }
    }
}

impl Default for PrintInfo {
    fn default() -> Self {
        DEFAULT_PRINT_INFO.get().expect("PrintInfo wasn't setup")()
    }
}

impl Clone for PrintInfo {
    fn clone(&self) -> Self {
        (self.fns.clone)(self.info.as_ref())
    }
}

impl PartialEq for PrintInfo {
    fn eq(&self, other: &Self) -> bool {
        (self.fns.eq)(self.info.as_ref(), other.info.as_ref())
    }
}

struct PrintInfoFunctions {
    clone: fn(info: &(dyn Any + Send)) -> PrintInfo,
    eq: fn(lhs: &(dyn Any + Send), rhs: &(dyn Any + Send)) -> bool,
}

impl PrintInfoFunctions {
    fn new<U: RawUi>() -> &'static Self {
        &Self {
            clone: |info| {
                let Some(info) = info.downcast_ref::<<U::Area as RawArea>::PrintInfo>() else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                PrintInfo {
                    info: Box::new(info.clone()),
                    fns: Self::new::<U>(),
                }
            },
            eq: |lhs, rhs| {
                let [Some(lhs), Some(rhs)] = [
                    lhs.downcast_ref::<<U::Area as RawArea>::PrintInfo>(),
                    rhs.downcast_ref(),
                ] else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                lhs == rhs
            },
        }
    }
}

fn get_cache<U: RawUi>(path: Option<&Path>) -> <<U as RawUi>::Area as RawArea>::Cache {
    if let Some(file_path) = path {
        match Cache::new().load::<<U::Area as RawArea>::Cache>(file_path) {
            Ok(cache) => cache,
            Err(err) => {
                context::error!("{err}");
                <U::Area as RawArea>::Cache::default()
            }
        }
    } else {
        <U::Area as RawArea>::Cache::default()
    }
}

static DEFAULT_PRINT_INFO: OnceLock<fn() -> PrintInfo> = OnceLock::new();
