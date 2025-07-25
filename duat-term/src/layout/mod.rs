use std::sync::Arc;

use cassowary::{WeightedRelation::*, strength::STRONG};
use duat_core::ui::{Axis, Constraint, PushSpecs, SpawnSpecs};

pub use self::rect::{Rect, Rects, transfer_vars};
use crate::{area::PrintInfo, print::Printer, AreaId, Equality, Frame};

mod rect;

/// The overrall structure of a window on `duat_term`.
///
/// The [`Layout`] handles all of the [`Rect`]s inside of it,
/// including all of the [`Variable`]s and
/// [`Constraint`](Equality)s that define said [`Rect`]s.
/// All external interactions seeking to change these values do so
/// through the [`Layout`].
///
/// The [`Layout`] also handles the [`Edge`]s that are supposed to be
/// printed to the screen.
///
/// The [`Layout`] will also hold floating [`Rect`]s, once those
/// become a thing.
pub struct Layout {
    pub rects: Rects,
    pub active_id: AreaId,
    pub printer: Arc<Printer>,
}

impl Layout {
    /// Returns a new instance of [`Layout`], applying a given
    /// [`Frame`] to all inner [`Rect`]s.
    pub fn new(fr: Frame, printer: Arc<Printer>, info: PrintInfo) -> Self {
        let rects = Rects::new(&printer, fr, info);
        let main_id = rects.main.id();

        Layout { rects, active_id: main_id, printer }
    }

    /// The index of the main [`Rect`], which holds all (non floating)
    /// others.
    pub fn main_id(&self) -> AreaId {
        self.rects.main.id()
    }

    /// Bisects a given [`Rect`] into two [`Rect`]s, returning the
    /// index of the new one.
    ///
    /// This bisection will sometimes cause the creation of a new
    /// parent to hold the bisected [`Rect`] and its new sibling. In
    /// these cases, an additional index associated with the parent
    /// will be returned.
    ///
    /// If `do_group`, and the bisected [`Rect`] was "not glued", a
    /// new parent will be created, which will act as the holder for
    /// all of its successors. If this parent is moved, the children
    /// will move with it.
    ///
    /// If `do_group`, and the bisected [`Rect`] was already glued,
    /// the creation of a new parent will follow regular rules, but
    /// the children will still be "glued".
    pub fn bisect(
        &mut self,
        id: AreaId,
        ps: PushSpecs,
        do_cluster: bool,
        on_files: bool,
        info: PrintInfo,
    ) -> (AreaId, Option<AreaId>) {
        let axis = ps.axis();

        let (can_be_sibling, can_be_child) = {
            let parent_is_cluster = self
                .rects
                .get_parent(id)
                .map(|(_, parent)| parent.is_clustered())
                .unwrap_or(do_cluster);
            let target_is_cluster = self.rects.get(id).is_some_and(Rect::is_clustered);

            // Clustering is what determines if a new rect can be a child or not.
            // In order to simplify, for example, swapping files around, it would
            // be helpful to keep only the stuff that is clustered to that file on
            // the same parent, even if other Areas could reasonable be placed
            // alognside it.
            let can_be_sibling = parent_is_cluster == do_cluster;
            let can_be_child = target_is_cluster == do_cluster;
            (can_be_sibling, can_be_child)
        };

        // Check if the target's parent has the same `Axis`.
        let (target, new_parent_id) = if can_be_sibling
            && let Some((_, parent)) = self.rects.get_parent_mut(id)
            && parent.aligns_with(axis)
        {
            (id, None)
        // Check if the target has the same `Axis`.
        } else if can_be_child
            && let Some(parent) = self.rects.get_mut(id)
            && parent.aligns_with(axis)
        {
            let children = parent.children().unwrap();
            let target = match ps.comes_earlier() {
                true => children.first().unwrap().0.id(),
                false => children.last().unwrap().0.id(),
            };

            (target, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            self.rects
                .new_parent_for(id, axis, &self.printer, do_cluster, on_files);
            let (_, parent) = self.rects.get_parent(id).unwrap();

            (id, Some(parent.id()))
        };

        let new_id = self.rects.push(ps, target, &self.printer, on_files, info);
        (new_id, new_parent_id)
    }

    pub fn delete(&mut self, id: AreaId) -> Option<AreaId> {
        let (mut rect, _, parent_id) = self.rects.delete(&self.printer, id)?;
        remove_children(&mut rect, &self.printer);

        self.printer.update(false);

        parent_id
    }

    pub fn swap(&mut self, id0: AreaId, id1: AreaId) {
        self.rects.swap(&self.printer, id0, id1);
    }

    pub fn reset_eqs(&mut self, target: AreaId) {
        self.rects.reset_eqs(&self.printer, target)
    }

    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        self.rects.get(id)
    }

    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        self.rects.get_parent(id)
    }

    pub fn active_id(&self) -> AreaId {
        self.active_id
    }

    pub(crate) fn new_floating(&mut self, id: AreaId, specs: SpawnSpecs) -> AreaId {
        self.rects.new_floating(&self.printer, id, specs)
    }
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
/// [`Constraint`]: Equality
#[derive(Default, Debug, Clone)]
pub struct Constraints {
    ver_eqs: Vec<Equality>,
    hor_eqs: Vec<Equality>,
    ver_cons: (Vec<Constraint>, bool),
    hor_cons: (Vec<Constraint>, bool),
    pub(crate) is_hidden: bool,
}

impl Constraints {
    /// Returns a new instance of [`Constraints`]
    ///
    /// Will also add all equalities needed to make this constraint
    /// work.
    fn new(
        p: &Printer,
        v_cons: impl Iterator<Item = Constraint> + Clone,
        h_cons: impl Iterator<Item = Constraint> + Clone,
        is_hidden: bool,
        new: &Rect,
        parent: AreaId,
        rects: &Rects,
    ) -> Self {
        let ver_cons = {
            let mut ver_cons: Vec<Constraint> = v_cons.clone().collect();
            ver_cons.sort_unstable();
            ver_cons
        };
        let hor_cons = {
            let mut hor_cons: Vec<Constraint> = h_cons.clone().collect();
            hor_cons.sort_unstable();
            hor_cons
        };
        let cons = ver_cons
            .into_iter()
            .map(|c| (c, Axis::Vertical))
            .chain(hor_cons.into_iter().map(|c| (c, Axis::Horizontal)))
            .map(|c| (c, false));

        let [ver_eqs, hor_eqs] = get_eqs(cons, is_hidden, new, parent, rects);
        p.add_eqs(ver_eqs.clone().into_iter().chain(hor_eqs.clone()));

        Self {
            ver_eqs,
            hor_eqs,
            ver_cons: (v_cons.collect(), false),
            hor_cons: (h_cons.collect(), false),
            is_hidden,
        }
    }

    pub fn replace(
        mut self,
        cons: impl Iterator<Item = Constraint>,
        axis: Axis,
    ) -> (Self, impl Iterator<Item = Equality>) {
        let hor_eqs = std::mem::take(&mut self.hor_eqs);
        let ver_eqs = std::mem::take(&mut self.ver_eqs);
        // A replacement means manual constraining, which is prioritized.

        match axis {
            Axis::Horizontal => self.hor_cons = (cons.collect(), true),
            Axis::Vertical => self.ver_cons = (cons.collect(), true),
        };
        (self, hor_eqs.into_iter().chain(ver_eqs))
    }

    /// Reuses [`self`] in order to constrain a new child
    pub fn apply(
        self,
        new: &Rect,
        parent: AreaId,
        rects: &Rects,
    ) -> (Self, impl Iterator<Item = Equality>) {
        let (ver_cons, ver_m) = &self.ver_cons;
        let (hor_cons, hor_m) = &self.hor_cons;
        let cons = ver_cons
            .iter()
            .map(|c| ((*c, Axis::Vertical), *ver_m))
            .chain(hor_cons.iter().map(|c| ((*c, Axis::Horizontal), *hor_m)));

        let [ver_eqs, hor_eqs] = get_eqs(cons, self.is_hidden, new, parent, rects);
        let new_eqs = ver_eqs.clone().into_iter().chain(hor_eqs.clone());

        (Self { ver_eqs, hor_eqs, ..self }, new_eqs)
    }

    pub fn get_eqs(&self) -> impl Iterator<Item = Equality> + use<> {
        self.hor_eqs.clone().into_iter().chain(self.ver_eqs.clone())
    }

    pub fn drain_eqs(&mut self) -> impl Iterator<Item = Equality> {
        self.ver_eqs.drain(..).chain(self.hor_eqs.drain(..))
    }

    pub fn on(&self, axis: Axis) -> impl Iterator<Item = Constraint> {
        match axis {
            Axis::Horizontal => self.hor_cons.0.iter().cloned(),
            Axis::Vertical => self.ver_cons.0.iter().cloned(),
        }
    }

    /// Whether or not [`self`] has flexibility in terms of its
    /// length.
    fn is_resizable_on(&self, axis: Axis) -> bool {
        self.on(axis)
            .all(|con| matches!(con, Constraint::Min(_) | Constraint::Max(_)))
    }
}

fn get_eqs(
    cons: impl Iterator<Item = ((Constraint, Axis), bool)>,
    is_hidden: bool,
    child: &Rect,
    parent: AreaId,
    rects: &Rects,
) -> [Vec<Equality>; 2] {
    if is_hidden {
        if rects
            .get(parent)
            .map(|p| p.aligns_with(Axis::Horizontal))
            .unwrap()
        {
            return [
                vec![child.len(Axis::Horizontal) | EQ(STRONG + 3.0) | 0.0],
                Vec::new(),
            ];
        } else {
            return [Vec::new(), vec![
                child.len(Axis::Vertical) | EQ(STRONG + 3.0) | 0.0,
            ]];
        }
    }

    let mut ver_eqs = Vec::new();
    let mut hor_eqs = Vec::new();

    for ((con, axis), is_manual) in cons {
        let strength = STRONG + if is_manual { 2.0 } else { 1.0 };
        let eq = match con {
            Constraint::Ratio(num, den) => {
                let (_, ancestor) = rects.get_ancestor_on(axis, parent).unwrap();
                (child.len(axis) * den as f64) | EQ(strength) | (ancestor.len(axis) * num as f64)
            }
            Constraint::Len(len) => child.len(axis) | EQ(strength) | len,
            Constraint::Min(min) => child.len(axis) | GE(strength) | min,
            Constraint::Max(max) => child.len(axis) | LE(strength) | max,
        };
        match axis {
            Axis::Horizontal => hor_eqs.push(eq),
            Axis::Vertical => ver_eqs.push(eq),
        }
    }

    [hor_eqs, ver_eqs]
}

fn remove_children(rect: &mut Rect, p: &Printer) {
    for (child, _) in rect.children_mut().into_iter().flat_map(|c| c.iter_mut()) {
        p.remove_rect(child);
        remove_children(child, p);
    }
}
