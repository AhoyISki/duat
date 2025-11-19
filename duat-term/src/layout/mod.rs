use std::{cell::RefCell, rc::Rc, sync::Arc};

use duat_core::{
    text::Text,
    ui::{Axis, DynSpawnSpecs, Orientation, PushSpecs, SpawnId, StaticSpawnSpecs},
};
use kasuari::{Constraint, WeightedRelation::*};

pub use self::rect::{Deletion, Rect, recurse_length, transfer_vars};
use crate::{
    AreaId, Border, CONS_SPAWN_LEN_PRIO, Coords, HIDDEN_PRIO, LEN_PRIO, MANUAL_LEN_PRIO,
    area::{Coord, PrintInfo},
    printer::{Lines, Printer},
};

mod rect;

/// The list of opened layouts, one for each window
///
/// Also contains a list of "free widgets", which can dynamically
/// change which layout they belong to, which _could_ happen if the
/// user decides to switch two widgets's [`Text`]s
///
/// [`Text`]: duat_core::text::Text
#[derive(Clone, Default)]
pub(crate) struct Layouts(Rc<RefCell<InnerLayouts>>);

impl Layouts {
    /// Adds a new layout, returning the main [`AreaId`]
    pub fn new_layout(&self, printer: Arc<Printer>, border: Border, cache: PrintInfo) -> AreaId {
        let layout = Layout::new(printer, border, cache);
        let main_id = layout.main_id();

        let mut layouts = self.0.borrow_mut();
        layouts.list.push(layout);

        if layouts.active_id.is_none() {
            layouts.active_id = Some(main_id);
        }

        main_id
    }

    ////////// Layout modification

    /// Push a new [`Area`] to an existing one, returning its
    /// [`AreaId`], as well as a parent [`AreaId`], if it was created
    ///
    /// May return [`None`] in the situation where the `Area` of the
    /// id was already deleted.
    ///
    /// [`Area`]: crate::area::Area
    pub fn push(
        &self,
        target: AreaId,
        specs: PushSpecs,
        on_files: bool,
        cache: PrintInfo,
    ) -> Option<(AreaId, Option<AreaId>)> {
        let mut layouts = self.0.borrow_mut();
        layouts
            .list
            .iter_mut()
            .find_map(|l| l.push(target, specs, on_files, cache))
    }

    /// Spawnss a new [`Area`] on an existing one, returning its
    /// [`AreaId`]
    ///
    /// May return [`None`] in the situation where the `Area` of the
    /// id was already deleted.
    ///
    /// [`Area`]: crate::area::Area
    pub fn spawn_on_widget(
        &self,
        target: AreaId,
        spawn_id: SpawnId,
        specs: DynSpawnSpecs,
        cache: PrintInfo,
    ) -> Option<AreaId> {
        let mut layouts = self.0.borrow_mut();
        layouts
            .list
            .iter_mut()
            .find_map(|layout| layout.spawn_on_widget(target, spawn_id, specs, cache))
    }

    /// Spawns a new [`Rect`] from a [`SpawnId`], which is supposed to
    /// go on [`Text`]
    ///
    /// [`Text`]: duat_core::text::Text
    pub fn spawn_on_text(
        &self,
        id: SpawnId,
        specs: DynSpawnSpecs,
        cache: PrintInfo,
        win: usize,
    ) -> AreaId {
        let mut layouts = self.0.borrow_mut();
        layouts.list[win].spawn_on_text(id, specs, cache)
    }

    /// Spawns a new statically positioned [`Rect`] from a [`SpawnId`]
    pub fn spawn_static(
        &self,
        id: SpawnId,
        specs: StaticSpawnSpecs,
        cache: PrintInfo,
        win: usize,
    ) -> AreaId {
        let mut layouts = self.0.borrow_mut();
        layouts.list[win].spawn_static(id, specs, cache)
    }

    /// Deletes the [`Area`] of a given id
    ///
    /// Returns wether this deletion resulted in the window being
    /// removed, and also returns every [`AreaId`] belonging to a
    /// [`Rect`] that was removed as a consequence of this.
    ///
    /// [`Area`]: crate::area::Area
    pub fn delete(&self, id: AreaId) -> (bool, Vec<AreaId>) {
        let mut layouts = self.0.borrow_mut();
        if let Some((i, rm_areas)) = layouts
            .list
            .iter_mut()
            .enumerate()
            .find_map(|(i, layout)| Some(i).zip(layout.delete(id)))
        {
            if rm_areas.contains(&layouts.list[i].main.id()) {
                layouts.list.remove(i);
                (true, rm_areas)
            } else {
                layouts.list[i].printer.update(false, false);
                (false, rm_areas)
            }
        } else {
            (false, Vec::new())
        }
    }

    /// Swaps two [`Area`]s around
    ///
    /// Will also swap their clusters, if they belong to different
    /// cluster masters
    ///
    /// Returns `false` if no swap occurred, either due to one or both
    /// of `Area`s having already been deleted, or because they belong
    /// to the same cluster master.
    ///
    /// [`Area`]: crate::area::Area
    pub fn swap(&self, lhs: AreaId, rhs: AreaId) -> bool {
        let mut inner = self.0.borrow_mut();
        let list = &mut inner.list;
        let (Some((l_layout_i, l_main_id)), Some((r_layout_i, r_main_id))) = (
            list.iter()
                .enumerate()
                .find_map(|(i, layout)| Some(i).zip(layout.get_main_id(lhs))),
            list.iter()
                .enumerate()
                .find_map(|(i, layout)| Some(i).zip(layout.get_main_id(rhs))),
        ) else {
            return false;
        };

        if l_main_id == r_main_id {
            let layout = &mut list[l_layout_i];
            let l_id = layout.get_cluster_master(lhs).unwrap_or(lhs);
            let r_id = layout.get_cluster_master(rhs).unwrap_or(rhs);

            if l_id == r_id {
                return false;
            }

            layout.swap(l_id, r_id);
        } else {
            let l_p = list[l_layout_i].printer.clone();
            let r_p = list[r_layout_i].printer.clone();

            let (l_main, r_main) = if l_layout_i == r_layout_i {
                list[l_layout_i]
                    .get_disjoint_mains_mut(l_main_id, r_main_id)
                    .unwrap()
            } else {
                let layouts_i = [l_layout_i, r_layout_i];
                let [l_layout, r_layout] = list.get_disjoint_mut(layouts_i).unwrap();
                let l_main = l_layout.get_mut(l_main_id).unwrap();
                let r_main = r_layout.get_mut(r_main_id).unwrap();
                (l_main, r_main)
            };

            let l_id = l_main.get_cluster_master(lhs).unwrap_or(lhs);
            let r_id = r_main.get_cluster_master(rhs).unwrap_or(rhs);

            let l_rect = l_main.get_mut(l_id).unwrap();
            let r_rect = r_main.get_mut(r_id).unwrap();

            transfer_vars(&l_p, &r_p, l_rect);
            transfer_vars(&r_p, &l_p, r_rect);

            std::mem::swap(l_rect, r_rect);

            list[l_layout_i].reset_eqs(r_id);
            list[r_layout_i].reset_eqs(l_id);
        }

        for layout_i in [l_layout_i, r_layout_i] {
            list[layout_i].printer.update(false, false);
        }

        true
    }

    /// Moves the [`Rect`] of a [`SpawnId`] to some [`Coord`]
    ///
    /// The [`Rect`] will dynamically relocate itself if there isn't
    /// enough space in its preferred position.
    ///
    /// Returns `false` if the [`Rect`] was deleted.
    pub fn move_spawn_to(&self, id: SpawnId, coord: Coord, char_width: u32) -> bool {
        let mut inner = self.0.borrow_mut();
        let Some((i, _rect)) = inner.list.iter_mut().enumerate().find_map(|(i, layout)| {
            layout
                .spawned
                .iter_mut()
                .find_map(|(info, rect)| (info.id == id).then_some((i, rect)))
        }) else {
            return false;
        };

        inner.list[i].printer.move_spawn_to(id, coord, char_width);
        inner.list[i].printer.update(false, false);

        true
    }

    /// Sets the constraints on a given [`Rect`]
    ///
    /// Returns `false` if the [`Rect`] was deleted or if there was
    /// nothing to be done.
    pub fn set_constraints(
        &self,
        id: AreaId,
        width: Option<f32>,
        height: Option<f32>,
        is_hidden: Option<bool>,
    ) -> bool {
        self.0.borrow_mut()
            .list
            .iter_mut()
            .any(|layout| layout.set_constraints(id, width, height, is_hidden))
    }

    /// Sets the [`Frame`] on a given [`Rect`]'s spawned master
    ///
    /// If a spawned `Rect` has multiple `Rect`s within it, then
    /// setting the `Frame` on any of themwill change the `Frame` of
    /// the spawned master instead, since the children of spawned
    /// `Rect`s don't have `Frame`s
    ///
    /// Returns `false` if the [`Rect`] was deleted.
    pub fn set_frame(&self, id: AreaId, mut frame: Frame) -> bool {
        let mut inner = self.0.borrow_mut();
        let set_frame = inner
            .list
            .iter_mut()
            .any(|layout| layout.set_frame(id, &mut frame));

        set_frame
            || inner
                .list
                .iter()
                .any(|layout| layout.main.get(id).is_some())
    }

    /// Removes all windows and all spawned widgets
    pub fn reset(&self) {
        let mut inner = self.0.borrow_mut();
        inner.list.clear();
        inner.active_id = None;
    }

    /// Removes a window from the list
    pub fn remove_window(&self, win: usize) {
        self.0.borrow_mut().list.remove(win);
    }

    /// Sets the [`PrintInfo`] of an [`AreaId`]'s [`Rect`]
    ///
    /// Returns `false` if the `Rect` was deleted or if it is not a
    /// leaf node
    pub fn set_info_of(&self, id: AreaId, new: PrintInfo) -> bool {
        let mut inner = self.0.borrow_mut();
        inner.list.iter_mut().any(|layout| {
            layout
                .get_mut(id)
                .and_then(|rect| rect.print_info_mut())
                .map(|info| *info = new)
                .is_some()
        })
    }

    ////////// Functions for printing

    /// Sends lines to be printed on screen
    pub fn send_lines(
        &self,
        area_id: AreaId,
        spawn_id: Option<SpawnId>,
        lines: Lines,
        spawns: impl Iterator<Item = SpawnId>,
        observed_spawns: &[SpawnId],
    ) {
        let mut inner = self.0.borrow_mut();
        let layout = inner
            .list
            .iter_mut()
            .find(|layout| layout.get(area_id).is_some())
            .unwrap();

        let mut revealed_at_least_one = false;
        for spawn_id in spawns {
            if let Some((_, rect)) = layout.spawned.iter().find(|(info, _)| info.id == spawn_id) {
                let hidden = !observed_spawns.contains(&spawn_id);
                recurse_set_hidden(layout, rect.id(), hidden);
                revealed_at_least_one = !hidden;
            }
        }

        if revealed_at_least_one {
            layout.printer.update(false, false);
        }

        if let Some(spawn_id) = spawn_id {
            layout.printer.send_spawned_lines(area_id, spawn_id, lines);
        } else {
            layout.printer.send_lines(lines);
        }
    }

    /// Sets the active [`AreaId`]
    ///
    /// Does nothing if the [`Rect`] of that `AreaId` was deleted.
    pub fn set_active_id(&self, id: AreaId) {
        let mut inner = self.0.borrow_mut();
        if inner.list.iter().any(|layout| layout.get(id).is_some()) {
            inner.active_id = Some(id);
        }
    }

    ////////// Querying functions

    /// The active [`AreaId`]
    pub fn get_active_id(&self) -> AreaId {
        self.0.borrow().active_id.unwrap()
    }

    /// Reads the [`Rect`] of an [`AreaId`], if it still exists
    ///
    /// Can return [`None`] if the [`Rect`] has deleted.
    pub fn inspect<Ret>(&self, id: AreaId, f: impl FnOnce(&Rect, &Layout) -> Ret) -> Option<Ret> {
        let inner = self.0.borrow();
        inner
            .list
            .iter()
            .find_map(|layout| layout.get(id).zip(Some(layout)))
            .map(|(rect, layout)| f(rect, layout))
    }

    /// Get the [`Coords`] of an [`AreaId`]'s [`Rect`]
    ///
    /// Also returns wether or not they have changed.
    ///
    /// Returns [`None`] if the `Rect` in question was deleted.
    pub fn coords_of(&self, id: AreaId, is_printing: bool) -> Option<Coords> {
        let inner = self.0.borrow();
        inner
            .list
            .iter()
            .find_map(|layout| layout.coords_of(id, is_printing))
    }

    /// Get the [`PrintInfo`] of an [`AreaId`]'s [`Rect`]
    ///
    /// Returns [`None`] if the `Rect` was deleted or if it is not a
    /// leaf node
    pub fn get_info_of(&self, id: AreaId) -> Option<PrintInfo> {
        let inner = self.0.borrow();
        inner
            .list
            .iter()
            .find_map(|layout| layout.get(id))
            .and_then(|rect| rect.print_info())
            .cloned()
    }
}

#[derive(Default)]
struct InnerLayouts {
    list: Vec<Layout>,
    active_id: Option<AreaId>,
}

/// The overrall structure of a window on `duat_term`.
///
/// The [`Layout`] handles all of the [`Rect`]s inside of it,
/// including all of the [`Variable`]s and
/// [`Constraint`](Constraint)s that define said [`Rect`]s.
/// All external interactions seeking to change these values do so
/// through the [`Layout`].
///
/// The [`Layout`] also handles the [`Edge`]s that are supposed to be
/// printed to the screen.
///
/// The [`Layout`] will also hold floating [`Rect`]s, once those
/// become a thing.
pub struct Layout {
    main: Rect,
    spawned: Vec<(SpawnInfo, Rect)>,
    printer: Arc<Printer>,
}

impl Layout {
    /// Returns a new instance of [`Layout`], applying a given
    /// [`Border`] to all inner [`Rect`]s.
    pub fn new(printer: Arc<Printer>, border: Border, cache: PrintInfo) -> Self {
        let main = Rect::new_main(&printer, border, cache);
        Layout { main, spawned: Vec::new(), printer }
    }

    /// The index of the main [`Rect`], which holds all (non floating)
    /// others.
    pub fn main_id(&self) -> AreaId {
        self.main.id()
    }

    /// Pushes a new [`Area`] into an existing one
    ///
    /// Returns a new [`AreaId`] for the child, and a possible new
    /// `AreaId` for the parent, if one was created.
    ///
    /// Will return [`None`] if the targeted [`AreaId`] is not part of
    /// this
    ///
    /// [`Area`]: super::Area
    fn push(
        &mut self,
        target: AreaId,
        specs: PushSpecs,
        on_files: bool,
        cache: PrintInfo,
    ) -> Option<(AreaId, Option<AreaId>)> {
        self.main
            .push(&self.printer, specs, target, on_files, cache, None)
            .or_else(|| {
                self.spawned.iter_mut().find_map(|(info, rect)| {
                    rect.push(&self.printer, specs, target, on_files, cache, Some(info))
                })
            })
    }

    /// Deletes the [`Rect`] of the given [`AreaId`]
    ///
    /// Returns a [`Vec`] of every leaf `AreaId` that was deleted by
    /// this action. This doesn't include the main [`AreaId`], since
    /// that one can't be deleted from within [`Layout`]
    ///
    /// Returns [`None`] if this `Layout` does not contain the
    /// `AreaId`'s `Rect`.
    ///
    /// [`Area`]: crate::area::Area
    fn delete(&mut self, id: AreaId) -> Option<Vec<AreaId>> {
        let ret = if let Some(deletion) = self.main.delete(&self.printer, id) {
            if let Deletion::Child(rect, cons, rm_list) = deletion {
                (rect, cons, rm_list)
            } else {
                self.printer.remove_rect(&mut self.main);

                return Some(remove_dependents(
                    &mut self.main,
                    &mut self.spawned,
                    &self.printer,
                    Vec::new(),
                ));
            }
        } else if let Some((i, deletion)) = self
            .spawned
            .iter_mut()
            .enumerate()
            .find_map(|(i, (_, rect))| Some(i).zip(rect.delete(&self.printer, id)))
        {
            if let Deletion::Child(rect, cons, rm_list) = deletion {
                (rect, cons, rm_list)
            } else {
                let (mut info, mut rect) = self.spawned.remove(i);
                self.printer.remove_rect(&mut rect);
                self.printer.remove_eqs(info.cons.drain());
                self.printer.remove_spawn_info(info.id);

                return Some(remove_dependents(
                    &mut rect,
                    &mut self.spawned,
                    &self.printer,
                    Vec::new(),
                ));
            }
        } else {
            return None;
        };

        let (mut rect, mut cons, rm_list) = ret;

        self.printer.remove_eqs(cons.drain());

        Some(remove_dependents(
            &mut rect,
            &mut self.spawned,
            &self.printer,
            rm_list,
        ))
    }

    /// Swaps the [`Rect`]s of two [`AreaId`]s
    fn swap(&mut self, id0: AreaId, id1: AreaId) {
        self.main.swap(&self.printer, id0, id1);
    }

    /// Spawns a new [`Rect`] around another, returning the [`AreaId`]
    fn spawn_on_widget(
        &mut self,
        target: AreaId,
        id: SpawnId,
        specs: DynSpawnSpecs,
        cache: PrintInfo,
    ) -> Option<AreaId> {
        let (rect, cons) = [&mut self.main]
            .into_iter()
            .chain(self.spawned.iter_mut().map(|(_, rect)| rect))
            .find_map(|rect| rect.new_spawned_on_widget(id, specs, target, &self.printer, cache))?;

        let area_id = rect.id();
        let orientation = specs.orientation;

        self.spawned.push((
            SpawnInfo {
                id,
                spec: SpawnSpec::Dynamic(orientation),
                cons,
                frame: Default::default(),
            },
            rect,
        ));

        Some(area_id)
    }

    /// Spawns a new [`Rect`] from a [`SpawnId`], which is supposed to
    /// go on [`Text`]
    ///
    /// [`Text`]: duat_core::text::Text
    fn spawn_on_text(&mut self, id: SpawnId, specs: DynSpawnSpecs, cache: PrintInfo) -> AreaId {
        let (rect, cons) =
            Rect::new_spawned_on_text(&self.printer, id, self.main.border(), cache, specs);
        let rect_id = rect.id();

        self.spawned.push((
            SpawnInfo {
                id,
                spec: SpawnSpec::Dynamic(specs.orientation),
                cons,
                frame: Default::default(),
            },
            rect,
        ));

        rect_id
    }

    fn spawn_static(&mut self, id: SpawnId, specs: StaticSpawnSpecs, cache: PrintInfo) -> AreaId {
        let (rect, cons, orig_max) =
            Rect::new_static_spawned(&self.printer, id, self.main.border(), cache, specs);
        let rect_id = rect.id();

        self.spawned.push((
            SpawnInfo {
                id,
                spec: SpawnSpec::Static {
                    top_left: specs.top_left,
                    fractional_repositioning: specs.fractional_repositioning,
                    orig_max,
                },
                cons,
                frame: Default::default(),
            },
            rect,
        ));

        rect_id
    }

    /// Sets the [`Frame`] for an [`Area`], if it's a spawned one
    ///
    /// [`Area`]: super::Area
    pub fn set_frame(&mut self, id: AreaId, frame: &mut Frame) -> bool {
        let Some((info, rect)) = self
            .spawned
            .iter_mut()
            .find(|(_, rect)| rect.get(id).is_some())
        else {
            return false;
        };

        if info.frame == *frame {
            return true;
        }

        info.frame = std::mem::take(frame);

        match info.spec {
            SpawnSpec::Static {
                top_left,
                fractional_repositioning,
                orig_max,
            } => {
                let width = recurse_length(rect, &info.cons, Axis::Horizontal).unwrap() as f32;
                let height = recurse_length(rect, &info.cons, Axis::Vertical).unwrap() as f32;

                rect.set_static_spawned_eqs(
                    &self.printer,
                    orig_max,
                    StaticSpawnSpecs {
                        top_left,
                        size: duat_core::ui::Coord::new(width, height),
                        hidden: false,
                        fractional_repositioning,
                    },
                    &info.frame,
                );
            }
            SpawnSpec::Dynamic(orientation) => {
                let specs = DynSpawnSpecs { orientation, ..Default::default() };
                let (deps, tl, br) = self.printer.get_spawn_info(info.id).unwrap();
                rect.set_dyn_spawned_eqs(&self.printer, specs, deps, tl, br, &info.frame);
                self.printer.set_frame(info.id, &info.frame);
            }
        }

        self.printer.update(false, true);

        true
    }

    /// Sets the constraints on a given [`Rect`]
    ///
    /// Returns `false` if this `Layout` does not contain the
    /// [`AreaId`]'s [`Rect`].
    pub fn set_constraints(
        &mut self,
        id: AreaId,
        width: Option<f32>,
        height: Option<f32>,
        is_hidden: Option<bool>,
    ) -> bool {
        let is_eq = |cons: &mut Constraints| {
            width.is_none_or(|w| Some(w) == cons.on(Axis::Horizontal))
                && height.is_none_or(|h| Some(h) == cons.on(Axis::Vertical))
                && is_hidden.is_none_or(|ih| ih == cons.is_hidden)
        };

        let get_new_cons = |main: &Rect, mut cons: Constraints| {
            let old_eqs = cons.replace(width, height, is_hidden);

            let rect = main.get(id).unwrap();
            let (_, parent) = main.get_parent(id).unzip();

            let new_eqs = cons.apply(rect, parent);
            self.printer.replace(old_eqs, new_eqs);
            cons
        };

        if let Some(cons) = self.main.get_constraints_mut(id) {
            if is_eq(cons) {
                return true;
            }
            let cons = cons.clone();
            *self.main.get_constraints_mut(id).unwrap() = get_new_cons(&self.main, cons);
            self.printer.update(false, false);

            true
        } else if let Some((i, cons)) =
            self.spawned
                .iter_mut()
                .enumerate()
                .find_map(|(i, (info, rect))| {
                    (rect.id() == id)
                        .then_some((i, &mut info.cons))
                        .or_else(|| Some(i).zip(rect.get_constraints_mut(id)))
                })
        {
            if is_eq(cons) {
                return true;
            }
            let cons = cons.clone();

            let (SpawnInfo { cons: main_cons, .. }, main) = &mut self.spawned[i];
            if main.id() == id {
                *main_cons = get_new_cons(main, cons);
            } else {
                *main.get_constraints_mut(id).unwrap() = get_new_cons(main, cons);
            }

            if is_hidden == Some(true) || [width, height].contains(&Some(0.0)) {
                self.printer.clear_spawn(id);
            }

            let (SpawnInfo { id, spec: orientation, cons, .. }, rect) = &self.spawned[i];
            if let SpawnSpec::Dynamic(orientation) = orientation {
                let len = recurse_length(rect, cons, orientation.axis());
                self.printer.set_spawn_len(*id, len.map(|len| len as f64));
            }
            self.printer.update(false, true);

            true
        } else {
            false
        }
    }

    /// Resets the equalities of the [`Rect`] of an [`AreaId`]
    fn reset_eqs(&mut self, id: AreaId) {
        [&mut self.main]
            .into_iter()
            .chain(self.spawned.iter_mut().map(|(_, rect)| rect))
            .any(|rect| rect.reset_eqs(&self.printer, id));
    }

    ////////// Getters

    /// Gets the [`Rect`] of and [`AreaId`], if found
    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        [&self.main]
            .into_iter()
            .chain(self.spawned.iter().map(|(_, rect)| rect))
            .find_map(|rect| rect.get(id))
    }

    /// Gets the parent [`Rect`] of and [`AreaId`], as well as its
    /// index on the list of children, if found
    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        [&self.main]
            .into_iter()
            .chain(self.spawned.iter().map(|(_, rect)| rect))
            .find_map(|rect| rect.get_parent(id))
    }

    /// Gets the cluster master [`AreaId`] of another, if found
    pub fn get_cluster_master(&self, id: AreaId) -> Option<AreaId> {
        [&self.main]
            .into_iter()
            .chain(self.spawned.iter().map(|(_, rect)| rect))
            .find_map(|rect| rect.get_cluster_master(id))
    }

    /// Get the main [`Rect`] that contains this `AreaId`'s
    fn get_main_id(&self, id: AreaId) -> Option<AreaId> {
        [&self.main]
            .into_iter()
            .chain(self.spawned.iter().map(|(_, rect)| rect))
            .find_map(|rect| rect.get(id).is_some().then_some(rect.id()))
    }

    /// Gets the mut [`Rect`] of an [`AreaId`]
    fn get_mut(&mut self, id: AreaId) -> Option<&mut Rect> {
        [&mut self.main]
            .into_iter()
            .chain(self.spawned.iter_mut().map(|(_, rect)| rect))
            .find_map(|rect| rect.get_mut(id))
    }

    ////////// Querying functions

    /// Get the maximum value for this `Layout`
    pub fn max_value(&self) -> crate::area::Coord {
        self.printer.max_value()
    }

    /// Get the [`Coords`] of an [`AreaId`]'s [`Rect`]
    ///
    /// Also returns wether or not they have changed.
    ///
    /// Returns [`None`] if this `Layout` does not contain the
    /// [`AreaId`]'s [`Rect`].
    fn coords_of(&self, id: AreaId, is_printing: bool) -> Option<Coords> {
        let rect = self.get(id)?;
        Some(self.printer.coords(rect.var_points(), is_printing))
    }

    /// Gets two disjointed main [`Rect`]s
    ///
    /// Fails if they can't be found, or if they are the same
    /// [`AreaId`]s
    fn get_disjoint_mains_mut(
        &mut self,
        l_main_id: AreaId,
        r_main_id: AreaId,
    ) -> Option<(&mut Rect, &mut Rect)> {
        let l_con = |rect: &Rect| rect.id() == l_main_id;
        let r_con = |rect: &Rect| rect.id() == r_main_id;

        if l_main_id == self.main.id() {
            let (_, r_main) = self.spawned.iter_mut().find(|(_, rect)| r_con(rect))?;
            Some((&mut self.main, r_main))
        } else if r_main_id == self.main.id() {
            let (_, l_main) = self.spawned.iter_mut().find(|(_, rect)| l_con(rect))?;
            Some((l_main, &mut self.main))
        } else {
            let l_i = self.spawned.iter().position(|(_, rect)| l_con(rect))?;
            let r_i = self.spawned.iter().position(|(_, rect)| r_con(rect))?;

            let [(_, l_rect), (_, r_rect)] = self.spawned.get_disjoint_mut([l_i, r_i]).ok()?;
            Some((l_rect, r_rect))
        }
    }
}

/// A listed main spawned [`Rect`]
#[derive(Debug, Clone)]
struct SpawnInfo {
    id: SpawnId,
    spec: SpawnSpec,
    cons: Constraints,
    frame: Frame,
}

/// The specifics of a spawned [`Rect`]
#[derive(Debug, Clone, Copy)]
enum SpawnSpec {
    Static {
        top_left: duat_core::ui::Coord,
        fractional_repositioning: Option<bool>,
        orig_max: Coord,
    },
    Dynamic(Orientation),
}

/// A list of [`Constraint`] for [`Rect`]s to follow.
///
/// These [`Constraint`]s are specifically not related to a [`Rect`]s
/// location, in relation to other [`Rect`]s. They instead deal with
/// two things, affecting a [`Rect`]s length in its parent's [`Axis`]:
///
/// - `defined`: A [`Constraint`], provided by the user, which details
///   specific requests for the length of a [`Rect`].
/// - `ratio`: A [`Constraint`] which details the ratio between the
///   length of this [`Rect`] and the length of the [`Rect`] that
///   follows it, if there is any.
///
/// Both of these constraints are optional, and are meant to be
/// replaceable at runtime.
///
/// [`Constraint`]: Constraint
#[derive(Default, Debug, Clone)]
pub struct Constraints {
    hor_con: Option<Constraint>,
    ver_con: Option<Constraint>,
    width: Option<(f32, bool)>,
    height: Option<(f32, bool)>,
    is_hidden: bool,
}

impl Constraints {
    /// Returns a new instance of [`Constraints`]
    ///
    /// Will also add all equalities needed to make this constraint
    /// work.
    ///
    /// This operation can fail if the `parent` in question can't be
    /// found in the `main` [`Rect`]
    fn new(
        p: &Printer,
        [width, height]: [Option<f32>; 2],
        is_hidden: bool,
        rect: &Rect,
        parent: Option<&Rect>,
    ) -> Self {
        let width = width.zip(Some(false));
        let height = height.zip(Some(false));
        let is_spawned = rect.spawn_id().is_some();
        let [ver_con, hor_con] = get_cons([width, height], rect, is_hidden, is_spawned, parent);

        p.add_eqs(ver_con.clone().into_iter().chain(hor_con.clone()));

        Self {
            hor_con,
            ver_con,
            width,
            height,
            is_hidden,
        }
    }

    /// Replaces the inner elements of the `Constraints`
    pub fn replace(
        &mut self,
        width: Option<f32>,
        height: Option<f32>,
        is_hidden: Option<bool>,
    ) -> Vec<Constraint> {
        let hor_con = self.hor_con.take();
        let ver_con = self.ver_con.take();
        // A replacement means manual constraining, which is prioritized.

        self.width = width.map(|w| (w, true)).or(self.width);
        self.height = height.map(|h| (h, true)).or(self.height);
        self.is_hidden = is_hidden.unwrap_or(self.is_hidden);

        hor_con.into_iter().chain(ver_con).collect()
    }

    /// Reuses [`self`] in order to constrain a new child
    pub fn apply(&mut self, rect: &Rect, parent: Option<&Rect>) -> Vec<Constraint> {
        let constraints = [self.width, self.height];
        let is_spawned = rect.spawn_id().is_some();
        let [ver_con, hor_con] = get_cons(constraints, rect, self.is_hidden, is_spawned, parent);
        let new_eqs = ver_con.clone().into_iter().chain(hor_con.clone());

        self.ver_con = ver_con;
        self.hor_con = hor_con;

        new_eqs.collect()
    }

    pub fn drain(&mut self) -> impl Iterator<Item = Constraint> {
        self.ver_con.take().into_iter().chain(self.hor_con.take())
    }

    pub fn on(&self, axis: Axis) -> Option<f32> {
        match axis {
            Axis::Horizontal => self.width.map(|(w, _)| w),
            Axis::Vertical => self.height.map(|(h, _)| h),
        }
    }

    /// Whether or not [`self`] has flexibility in terms of its
    /// length.
    fn is_resizable_on(&self, axis: Axis) -> bool {
        self.on(axis).is_none()
    }
}

/// A frame around a spawned [`Area`]
///
/// This can be configured on an `Area` per `Area` basis, or you can
/// set a global configuration. In that case [`Widget`]s should strive
/// to respect the `style` field, even if they override the other
/// fields as necessary.
///
/// [`Area`]: super::Area
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    /// Show a frame above
    pub above: bool,
    // Show a frame below
    pub below: bool,
    // Show a frame on the left
    pub left: bool,
    // Show a frame on the right
    pub right: bool,
    // The `Frame`'s style
    pub style: FrameStyle,
    // The `Frame`'s title, shown above
    pub title: Option<Text>,
}

impl Frame {
    /// An [`Iterator`] over the sides of the `Frame`
    ///
    /// The order is: above, below, left, right
    pub fn sides(&self) -> impl Iterator<Item = bool> {
        [self.above, self.below, self.left, self.right].into_iter()
    }
}

/// The style for a spawned [`Area`]'s [`Frame`]
///
/// The default is [`FrameStyle::Regular`], which makes use of
/// characters like `─`, `│`, `┐`
///
/// [`Area`]: super::Area
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum FrameStyle {
    /// Uses `─`, `│`, `┐`
    #[default]
    Regular,
    /// Uses `━`, `┃`, `┓`
    Thick,
    /// Uses `╌`, `╎`, `┐`
    Dashed,
    /// Uses `╍`, `╏`, `┓`
    ThickDashed,
    /// Uses `═`, `║`, `╗`
    Double,
    /// Uses `─`, `│`, `╮`
    Rounded,
    /// Uses `▄`, `▌`, `▖`
    Halved,
    /// Uses `-`, `|`, `+`
    Ascii,
    /// Uses `char` for all positions
    Custom {
        /// The [`char`] to use for each side
        ///
        /// The order is: top, bottom, left, right
        sides: [char; 4],
        /// The [`char`] to use for the corners
        ///
        /// The order is: top-right, top-left, bottom-left,
        /// bottom-right
        corners: [char; 4],
        /// The [`char`] to use when two [`Area`]s with the same
        /// `FrameStyle` come in contact perpendicularly
        ///
        /// This is just like `sides`, but the "side" represents
        /// which line isn't included.
        ///
        /// ```text
        ///   ┌────┐
        ///   │    │
        ///   │ W1 │<- Right side
        ///   │    │
        /// ┌─┴────┤<- Right t_merge
        /// │  W2  │
        /// └──────┘
        /// ```
        ///
        /// The order is: tob, bottom, left, right
        ///
        /// [`Area`]: super::Area
        t_mergers: Option<[char; 4]>,
        /// The [`char`] to use when a merge happens on all sides,
        /// on [`FrameStyle::Regular`] for example, this is `┼`
        x_merger: Option<char>,
    },
}

fn get_cons(
    [width, height]: [Option<(f32, bool)>; 2],
    child: &Rect,
    is_hidden: bool,
    is_spawned: bool,
    parent: Option<&Rect>,
) -> [Option<Constraint>; 2] {
    if is_hidden {
        let hor_con = child.len(Axis::Horizontal) | EQ(HIDDEN_PRIO) | 0.0;
        let ver_con = child.len(Axis::Vertical) | EQ(HIDDEN_PRIO) | 0.0;
        if let Some(parent) = parent {
            if parent.aligns_with(Axis::Horizontal) {
                [
                    Some(hor_con),
                    height.map(|(h, _)| child.len(Axis::Vertical) | EQ(LEN_PRIO) | h),
                ]
            } else {
                [
                    width.map(|(w, _)| child.len(Axis::Horizontal) | EQ(LEN_PRIO) | w),
                    Some(ver_con),
                ]
            }
        } else {
            [Some(hor_con), Some(ver_con)]
        }
    } else {
        [(width, Axis::Horizontal), (height, Axis::Vertical)].map(|(constraint, axis)| {
            let (len, is_manual) = constraint?;
            let strength = match (is_spawned, is_manual) {
                (true, _) => CONS_SPAWN_LEN_PRIO,
                (false, true) => MANUAL_LEN_PRIO,
                (false, false) => LEN_PRIO,
            };
            Some(child.len(axis) | EQ(strength) | len)
        })
    }
}

/// Removes every [`Rect`] that's considered a "dependant" of this one
///
/// Returns the list of [`AreaId`]s that were removed by this action.
///
/// A `Rect` is considered dependant if it is a child of the removed
/// `Rect` or if it was spawned on it. This functions recurses until
/// all dependants of all removed `Rect`s are removed.
fn remove_dependents(
    rect: &mut Rect,
    spawned: &mut Vec<(SpawnInfo, Rect)>,
    p: &Printer,
    mut rm_list: Vec<AreaId>,
) -> Vec<AreaId> {
    rm_list.push(rect.id());

    let vars = {
        let [tl, br] = rect.var_points();
        [tl.x(), tl.y(), br.x(), br.y()]
    };

    let rm_spawned: Vec<(SpawnInfo, Rect)> = spawned
        .extract_if(.., |(info, _)| {
            let Some((_, tl, br)) = p.get_spawn_info(info.id) else {
                return false;
            };

            if tl
                .iter()
                .chain(br.iter())
                .any(|expr| expr.terms.iter().any(|term| vars.contains(&term.variable)))
            {
                p.remove_spawn_info(info.id);
                true
            } else {
                false
            }
        })
        .collect();

    for (mut info, mut rect) in rm_spawned {
        p.remove_rect(&mut rect);
        p.remove_eqs(info.cons.drain());

        rm_list = remove_dependents(&mut rect, spawned, p, rm_list);
    }

    for (rect, cons) in rect.children_mut().into_iter().flat_map(|c| c.iter_mut()) {
        p.remove_rect(rect);
        p.remove_eqs(cons.drain());

        rm_list = remove_dependents(rect, spawned, p, rm_list);
    }

    rm_list
}

/// Sets a [`Rect`], as well as all of its children, to be hidden or
/// revealed
fn recurse_set_hidden(layout: &mut Layout, id: AreaId, hidden: bool) {
    let Some(rect) = layout.get(id) else { return };

    if layout.get_parent(id).is_none() {
        let [tl, _] = rect.var_points();
        for i in 0..layout.spawned.len() {
            let (info, rect) = &layout.spawned[i];

            if let Some((_, [tl_x, _], _)) = layout.printer.get_spawn_info(info.id)
                && tl_x.terms.iter().any(|term| term.variable == tl.x())
            {
                recurse_set_hidden(layout, rect.id(), hidden);
            }
        }
    }

    let Some(rect) = layout.get(id) else { return };

    if let Some(children) = rect.children() {
        let children: Vec<_> = children.iter().map(|(rect, _)| rect.id()).collect();
        for child_id in children {
            recurse_set_hidden(layout, child_id, hidden);
        }
    }

    if hidden {
        layout.printer.clear_spawn(id);
    }
    layout.set_constraints(id, None, None, Some(hidden));
}
