use std::{cell::RefCell, rc::Rc, sync::Arc};

use cassowary::{Constraint, WeightedRelation::*};
use duat_core::{
    text::SpawnId,
    ui::{Axis, Orientation, PushSpecs, SpawnSpecs},
};

pub use self::rect::{Deletion, Rect, transfer_vars};
use crate::{
    AreaId, Coords, Frame,
    area::{Coord, PrintInfo},
    printer::{LinesBuilder, Printer},
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
    pub fn new_layout(&self, printer: Arc<Printer>, frame: Frame, cache: PrintInfo) -> AreaId {
        let layout = Layout::new(printer, frame, cache);
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
        specs: SpawnSpecs,
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
        specs: SpawnSpecs,
        cache: PrintInfo,
        win: usize,
    ) -> AreaId {
        let mut layouts = self.0.borrow_mut();
        layouts.list[win].spawn_on_text(id, specs, cache)
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
        let mut layouts = self.0.borrow_mut();
        let list = &mut layouts.list;
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
        let mut layouts = self.0.borrow_mut();
        let Some((i, _rect)) = layouts.list.iter_mut().enumerate().find_map(|(i, layout)| {
            layout
                .spawned
                .iter_mut()
                .find_map(|(info, rect)| (info.id == id).then_some((i, rect)))
        }) else {
            return false;
        };

        layouts.list[i].printer.move_spawn_to(id, coord, char_width);
        layouts.list[i].printer.update(false, false);

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
        self.0
            .borrow_mut()
            .list
            .iter_mut()
            .any(|layout| layout.set_constraints(id, width, height, is_hidden))
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
        let layouts = self.0.borrow();
        layouts.list.iter().any(|layout| {
            layout
                .get(id)
                .and_then(|rect| rect.print_info())
                .map(|info| info.set(new))
                .is_some()
        })
    }

    ////////// Functions for printing

    /// Sends lines to be printed on screen
    pub fn send_lines(
        &self,
        id: AreaId,
        lines: LinesBuilder,
        is_floating: bool,
        observed_spawns: &[SpawnId],
        _spawns: &[SpawnId],
    ) {
        let layouts = self.0.borrow();
        let layout = layouts
            .list
            .iter()
            .find(|layout| layout.get(id).is_some())
            .unwrap();

        if !observed_spawns.is_empty() {
            layout.printer.update(false, false);
        }

        if is_floating {
            layout.printer.send_spawned_lines(id, lines);
        } else {
            layout.printer.send_lines(lines);
        }
    }

    /// Sets the active [`AreaId`]
    ///
    /// Does nothing if the [`Rect`] of that `AreaId` was deleted.
    pub fn set_active_id(&self, id: AreaId) {
        let mut layouts = self.0.borrow_mut();
        if layouts.list.iter().any(|layout| layout.get(id).is_some()) {
            layouts.active_id = Some(id);
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
        let layouts = self.0.borrow();
        layouts
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
    pub fn coords_of(&self, id: AreaId, is_printing: bool) -> Option<(Coords, bool)> {
        let layouts = self.0.borrow();
        layouts
            .list
            .iter()
            .find_map(|layout| layout.coords_of(id, is_printing))
    }

    /// Get the [`PrintInfo`] of an [`AreaId`]'s [`Rect`]
    ///
    /// Returns [`None`] if the `Rect` was deleted or if it is not a
    /// leaf node
    pub fn get_info_of(&self, id: AreaId) -> Option<PrintInfo> {
        let layouts = self.0.borrow();
        layouts
            .list
            .iter()
            .find_map(|layout| layout.get(id))
            .and_then(|rect| rect.print_info())
            .map(|info| info.get())
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
    /// [`Frame`] to all inner [`Rect`]s.
    pub fn new(printer: Arc<Printer>, frame: Frame, cache: PrintInfo) -> Self {
        let main = Rect::new_main(&printer, frame, cache);
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
                self.printer.remove_spawn_info(info.id);
                self.printer.remove_rect(&mut rect);
                self.printer.remove_eqs(info.cons.drain());

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
        specs: SpawnSpecs,
        cache: PrintInfo,
    ) -> Option<AreaId> {
        let (rect, cons) = [&mut self.main]
            .into_iter()
            .chain(self.spawned.iter_mut().map(|(_, rect)| rect))
            .find_map(|rect| rect.new_spawned_on_widget(id, specs, target, &self.printer, cache))?;

        let area_id = rect.id();
        let orientation = specs.orientation;

        self.spawned
            .push((SpawnInfo { id, orientation, cons }, rect));

        Some(area_id)
    }

    fn spawn_on_text(&mut self, id: SpawnId, specs: SpawnSpecs, cache: PrintInfo) -> AreaId {
        let (rect, cons) =
            Rect::new_spawned_on_text(&self.printer, id, self.main.frame(), cache, specs);
        let rect_id = rect.id();

        let orientation = specs.orientation;

        self.spawned
            .push((SpawnInfo { id, orientation, cons }, rect));

        rect_id
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
        fn get_constraints_mut(id: AreaId, layout: &mut Layout) -> Option<&mut Constraints> {
            [&mut layout.main]
                .into_iter()
                .chain(layout.spawned.iter_mut().map(|(_, rect)| rect))
                .find_map(|rect| rect.get_constraints_mut(id))
        }

        let Some(mut cons) = get_constraints_mut(id, self).cloned() else {
            return false;
        };

        if width.is_none_or(|w| Some(w) == cons.on(Axis::Horizontal))
            && height.is_none_or(|h| Some(h) == cons.on(Axis::Vertical))
            && is_hidden.is_none_or(|ih| ih == cons.is_hidden)
        {
            return true;
        };

        *get_constraints_mut(id, self).unwrap() = {
            let old_eqs = cons.replace(width, height, is_hidden);

            let rect = self.get(id).unwrap();
            let (_, parent) = self.get_parent(id).unzip();

            let new_eqs = cons.apply(rect, parent);
            self.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        true
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
    fn coords_of(&self, id: AreaId, is_printing: bool) -> Option<(Coords, bool)> {
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
struct SpawnInfo {
    id: SpawnId,
    orientation: Orientation,
    cons: Constraints,
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
        let [ver_con, hor_con] =
            get_cons([width, height], rect, is_hidden, rect.is_spawned(), parent);
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
        let [ver_con, hor_con] =
            get_cons(constraints, rect, self.is_hidden, rect.is_spawned(), parent);
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
                (true, _) => SPAWN_LEN_PRIO,
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
            let (_, tl, br) = p.get_spawned_info(info.id).unwrap();

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

/// The priority for edges for areas that must not overlap
const EDGE_PRIO: f64 = cassowary::strength::REQUIRED;
/// The priority for manually defined lengths
const MANUAL_LEN_PRIO: f64 = cassowary::strength::STRONG + 2.0;
/// The priority for lengths defined when creating Areas
const LEN_PRIO: f64 = cassowary::strength::STRONG + 1.0;
/// The priority for frames
const FRAME_PRIO: f64 = cassowary::strength::STRONG;
/// The priority for hiding things
const HIDDEN_PRIO: f64 = cassowary::strength::STRONG - 1.0;
/// The priority for positioning of spawned Areas
const SPAWN_POS_PRIO: f64 = cassowary::strength::STRONG - 2.0;
/// The priority for the length of spawned Areas
const SPAWN_LEN_PRIO: f64 = cassowary::strength::STRONG - 3.0;
/// The priority for the alignment of spawned Areas
const SPAWN_ALIGN_PRIO: f64 = cassowary::strength::STRONG - 4.0;
/// The priority for lengths that should try to be equal (a.k.a Files)
const EQ_LEN_PRIO: f64 = cassowary::strength::STRONG - 5.0;
