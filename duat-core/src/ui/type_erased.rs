//! Type erased versions of [`ui`] elements
//!
//! This module serves the purpose of keeping Duat code less verbose,
//! by making it so end users and plugin writers alike don't have to
//! worry about implementing their code for specific
//! [`Ui`](super::traits::Ui) versions.
//!
//! This works by type erasing the `Ui` and [`Area`] traits, as well
//! as [`Area::PrintInfo`], so that they become `dyn` traits, even
//! though they have many elements that should not be `dyn`
//! compatible.
//!
//! Of course, you can still go into specifics about certain `Area`
//! implementations with the [`Area::read_as`] method.
//!
//! [`ui`]: super
//! [`Area`]: super::traits::Area
//! [`Area::PrintInfo`]: super::traits::Area::PrintInfo
use std::{
    any::Any,
    cell::UnsafeCell,
    sync::{Arc, OnceLock},
};

use crate::{
    cfg::PrintCfg,
    context::{self, Cache},
    data::{Pass, RwData},
    file::File,
    form::Painter,
    text::{FwdIter, Item, Point, RevIter, SpawnId, Text, TwoPoints},
    ui::{
        Caret, PushSpecs, SpawnSpecs, Widget,
        traits::{self, Area as AreaTrait},
    },
};

/// A type erased [`Ui`]
#[derive(Clone, Copy)]
pub struct Ui {
    ui: &'static dyn traits::Ui,
    fns: &'static UiFunctions,
    default_print_info: fn() -> PrintInfo,
}

impl Ui {
    /// Returns a new type erased [`Ui`]
    ///
    /// Given the [`Ui::get_once`] function, this should only be
    /// callable _once_.
    pub fn new<U: traits::Ui>() -> Self
    where
        U::Area: PartialEq,
    {
        Ui {
            ui: U::get_once().expect("Ui was acquired more than once"),
            fns: UiFunctions::new::<U>(),
            default_print_info: || {
                PrintInfo::new::<U>(<U::Area as traits::Area>::PrintInfo::default())
            },
        }
    }

    /// Initiates and returns a new "master" [`Area`]
    ///
    /// This [`Area`] must not have any parents, and must be placed on
    /// a new window, that is, a plain region with nothing in it.
    ///
    /// [`Area`]: Ui::Area
    pub fn new_root(&self, pa: &Pass, widget: &RwData<dyn Widget>) -> Area {
        (self.fns.new_root)(pa, self.ui, widget)
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
    ) -> Area {
        (self.fns.new_spawned)(pa, self.ui, widget, spawn_id, specs, win)
    }

    /// Sets the default [`PrintInfo`]
    pub(crate) fn setup_default_print_info(&self) {
        DEFAULT_PRINT_INFO
            .set(self.default_print_info)
            .expect("PrintInfo was set twice");
    }
}

struct UiFunctions {
    new_root: fn(pa: &Pass, &dyn traits::Ui, widget: &RwData<dyn Widget>) -> Area,
    new_spawned: fn(
        pa: &Pass,
        &dyn traits::Ui,
        widget: &RwData<dyn Widget>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
        win: usize,
    ) -> Area,
}

impl UiFunctions {
    fn new<U: traits::Ui>() -> &'static Self
    where
        U::Area: PartialEq,
    {
        &Self {
            new_root: |pa, ui, widget| {
                let ui = unsafe { (ui as *const dyn traits::Ui as *const U).as_ref() }.unwrap();

                Area::new::<U>(ui.new_root(get_cache::<U>(pa, widget)))
            },
            new_spawned: |pa, ui, widget, spawn_id, specs, win| {
                let ui = unsafe { (ui as *const dyn traits::Ui as *const U).as_ref() }.unwrap();

                Area::new::<U>(ui.new_spawned(spawn_id, specs, get_cache::<U>(pa, widget), win))
            },
        }
    }
}

impl std::ops::Deref for Ui {
    type Target = dyn traits::Ui + 'static;

    fn deref(&self) -> &Self::Target {
        self.ui
    }
}

/// A type erased [`Area`]
///
/// This type houses the inner `Area`, and provides type erased access
/// to its functions.
#[derive(Clone)]
pub struct Area {
    area: RwData<dyn traits::Area>,
    fns: &'static AreaFunctions,
}

impl Area {
    /// Returns a new type erased [`Area`]
    ///
    /// This is the only moment where the [`Ui`] and `Area` will be
    /// statically known.
    ///
    /// [`Ui`]: super::traits::Ui
    /// [`Area`]: super::traits::Area
    fn new<U: traits::Ui>(area: U::Area) -> Self
    where
        U::Area: PartialEq,
        <U::Area as traits::Area>::PrintInfo: Default + Clone + Send + PartialEq + Eq,
    {
        Self {
            area: unsafe { RwData::new_unsized::<U::Area>(Arc::new(UnsafeCell::new(area))) },
            fns: AreaFunctions::new::<U>(),
        }
    }

    /// Shared access to an [`Area`] trait object
    ///
    /// You can use this to call any of the non restricted functions
    /// from the [`Area`] trait, which include a bunch internal
    /// mutability, like with [`Area::set_width`],
    /// [`Area::set_height`], [`Area::reveal`], etc.
    ///
    /// [`Area`]: super::traits::Area
    /// [`Area::set_width`]: super::traits::Area::set_width
    /// [`Area::set_height`]: super::traits::Area::set_height
    /// [`Area::reveal`]: super::traits::Area::reveal
    pub fn read<'a>(&'a self, pa: &'a Pass) -> &'a dyn traits::Area {
        self.area.read(pa)
    }

    /// Attempts to read this [`Area`]
    pub fn read_as<'a, A: traits::Area>(&'a self, pa: &'a Pass) -> Option<&'a A> {
        self.area.read_as(pa)
    }

    ////////// Area Modification functions

    /// Pushes a [`Widget`] to this [`Area`]
    pub(super) fn push(
        &self,
        pa: &mut Pass,
        widget: &RwData<dyn Widget>,
        specs: PushSpecs,
        on_files: bool,
    ) -> Option<(Self, Option<Self>)> {
        (self.fns.push)(pa, &self.area, widget, specs, on_files)
    }

    /// Spawns a [`Widget`] on this [`Area`]
    pub(super) fn spawn(
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
    pub(super) fn delete(&self, pa: &mut Pass) -> (bool, Vec<Self>) {
        (self.fns.delete)(pa, &self.area)
    }

    /// Swaps this [`Area`] with another
    pub(super) fn swap(&self, pa: &mut Pass, rhs: &Self) -> bool {
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
    pub fn get_print_info(&self, pa: &Pass) -> PrintInfo {
        (self.fns.get_print_info)(pa, &self.area)
    }

    /// Sets a previously acquired [`PrintInfo`] to the area
    ///
    /// [`PrintInfo`]: Area::PrintInfo
    pub fn set_print_info(&self, pa: &mut Pass, info: PrintInfo) {
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
        points: impl TwoPoints,
        cfg: PrintCfg,
    ) {
        self.area
            .read(pa)
            .scroll_around_points(text, points.to_points(), cfg);
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
    pub fn area_is_eq(&self, pa: &Pass, other: &Area) -> bool {
        (self.fns.eq)(pa, &self.area, &other.area)
    }
}

#[derive(Clone, Copy)]
struct AreaFunctions {
    /// Push one [`Area`] to another
    push: fn(
        pa: &mut Pass,
        area: &RwData<dyn traits::Area>,
        widget: &RwData<dyn Widget>,
        specs: PushSpecs,
        on_files: bool,
    ) -> Option<(Area, Option<Area>)>,
    /// Spawn an [`Area`] on another
    spawn: fn(
        pa: &mut Pass,
        area: &RwData<dyn traits::Area>,
        widget: &RwData<dyn Widget>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
    ) -> Option<Area>,
    /// Deletes an [`Area`]
    delete: fn(pa: &mut Pass, area: &RwData<dyn traits::Area>) -> (bool, Vec<Area>),
    /// Swaps two [`Area`]s
    swap: fn(pa: &mut Pass, lhs: &Area, rhs: &Area) -> bool,
    /// Prints to an [`Area`], with a callback function
    print_with: for<'a> fn(
        pa: &Pass,
        area: &RwData<dyn traits::Area>,
        text: &Text,
        cfg: PrintCfg,
        painter: Painter,
        f: Box<dyn FnMut(&Caret, &Item) + 'a>,
    ),
    /// Gets the type erased [`Area::PrintInfo`]
    get_print_info: fn(pa: &Pass, area: &RwData<dyn traits::Area>) -> PrintInfo,
    /// Sets the type erased [`Area::PrintInfo`]
    set_print_info: fn(pa: &mut Pass, area: &RwData<dyn traits::Area>, info: PrintInfo),
    /// Wether this [`Area`] is the master of another
    is_master_of:
        fn(pa: &Pass, lhs: &RwData<dyn traits::Area>, rhs: &RwData<dyn traits::Area>) -> bool,
    /// Gets the master [`Area`] of another's cluster
    get_cluster_master: fn(pa: &Pass, area: &RwData<dyn traits::Area>) -> Option<Area>,
    /// Store the [`Area::Cache`] of this [`Area`]
    store_cache: fn(pa: &Pass, area: &RwData<dyn traits::Area>, path: &str) -> Result<(), Text>,
    /// Wether two [`Area`]s are the same
    eq: fn(pa: &Pass, lhs: &RwData<dyn traits::Area>, rhs: &RwData<dyn traits::Area>) -> bool,
}

impl AreaFunctions {
    const fn new<U: traits::Ui>() -> &'static Self
    where
        U::Area: PartialEq,
    {
        &Self {
            push: |pa, area, widget, specs, on_files| {
                let cache = get_cache::<U>(pa, widget);
                let area = area.write_as::<U::Area>(pa).unwrap();
                let (child, parent) = area.push(specs, on_files, cache)?;

                let child = Area::new::<U>(child);
                let parent = parent.map(Area::new::<U>);

                Some((child, parent))
            },
            spawn: |pa, area, widget, spawn_id, specs| {
                let cache = get_cache::<U>(pa, widget);
                let area = area.write_as::<U::Area>(pa).unwrap();
                let spawned = area.spawn(spawn_id, specs, cache)?;

                Some(Area::new::<U>(spawned))
            },
            delete: |pa, area| {
                let area: &mut U::Area = area.write_as(pa).unwrap();

                let (do_rm_window, removed) = area.delete();

                (
                    do_rm_window,
                    removed.into_iter().map(Area::new::<U>).collect(),
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

                PrintInfo::new::<U>(area.get_print_info())
            },
            set_print_info: |pa, area, info| {
                let area = area.write_as::<U::Area>(pa).unwrap();
                let Some(info) = info
                    .info
                    .as_ref()
                    .downcast_ref::<<U::Area as traits::Area>::PrintInfo>()
                else {
                    panic!("Attempted to get PrintInfo of wrong type");
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
                area.get_cluster_master().map(Area::new::<U>)
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
///
/// [`Area::PrintInfo`]: traits::Area::PrintInfo
pub struct PrintInfo {
    info: Box<dyn Any + Send>,
    fns: &'static PrintInfoFunctions,
}

impl PrintInfo {
    fn new<U: traits::Ui>(info: <U::Area as traits::Area>::PrintInfo) -> Self {
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
        (self.fns.clone)(&self.info)
    }
}

impl PartialEq for PrintInfo {
    fn eq(&self, other: &Self) -> bool {
        (self.fns.eq)(&(*self.info), &(*other.info))
    }
}

struct PrintInfoFunctions {
    clone: fn(info: &(dyn Any + Send)) -> PrintInfo,
    eq: fn(lhs: &(dyn Any + Send), rhs: &(dyn Any + Send)) -> bool,
}

impl PrintInfoFunctions {
    fn new<U: traits::Ui>() -> &'static Self {
        &Self {
            clone: |info| {
                let Some(info) = info.downcast_ref::<<U::Area as traits::Area>::PrintInfo>() else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                PrintInfo {
                    info: Box::new(info.clone()),
                    fns: Self::new::<U>(),
                }
            },
            eq: |lhs, rhs| {
                let [Some(lhs), Some(rhs)] = [
                    lhs.downcast_ref::<<U::Area as traits::Area>::PrintInfo>(),
                    rhs.downcast_ref(),
                ] else {
                    panic!("Attempted to get PrintInfo of wrong type");
                };

                lhs == rhs
            },
        }
    }
}

fn get_cache<U: traits::Ui>(
    pa: &Pass,
    widget: &RwData<dyn Widget>,
) -> <<U as traits::Ui>::Area as traits::Area>::Cache {
    if let Some(file) = widget.read_as::<File>(pa) {
        match Cache::new().load::<<U::Area as traits::Area>::Cache>(file.path()) {
            Ok(cache) => cache,
            Err(err) => {
                context::error!("{err}");
                <U::Area as traits::Area>::Cache::default()
            }
        }
    } else {
        <U::Area as traits::Area>::Cache::default()
    }
}

static DEFAULT_PRINT_INFO: OnceLock<fn() -> PrintInfo> = OnceLock::new();
