use std::cell::Cell;

use cassowary::{
    Expression, Variable,
    WeightedRelation::{EQ, GE, LE},
};
use duat_core::{
    text::SpawnId,
    ui::{
        Axis::{self, *},
        PushSpecs, SpawnSpecs,
    },
};

use super::{
    Constraints, EDGE_PRIO, FRAME_PRIO, Layout, SPAWN_ALIGN_PRIO, SPAWN_LEN_PRIO, SPAWN_POS_PRIO,
};
use crate::{
    AreaId, Equality, Frame,
    area::PrintInfo,
    layout::EQ_LEN_PRIO,
    printer::{Printer, VarPoint},
};

/// An area on the screen, which can hold other [`Rect`]s or be host
/// for printing a [`Widget`][duat_core::widgets::Widget].
///
/// [`Rect`]s hold the [`Constraint`]s that define them, handling
/// which sides of the [`Rect`] should be equal to that of other
/// specific [`Rect`]s. These [`Constraint`]s also serve the role of
/// framing the [`Rect`] in cases where it is deemed necessary and
/// correct.
///
/// They can also be tied to other, ancestor [`Rect`]s, such that, by
/// moving the parent [`Rect`], you will be moving all of its children
/// along with it.
///
/// [`Constraint`]: Equality
#[allow(clippy::type_complexity)]
pub struct Rect {
    id: AreaId,
    tl: VarPoint,
    br: VarPoint,
    eqs: Vec<Equality>,
    is_floating: bool,
    kind: Kind,
    on_files: bool,
    edge: Option<Variable>,
    pub frame: Frame,
}

impl Rect {
    /// Returns a new main `Rect`, which represents a full window
    pub fn new_main(p: &Printer, frame: Frame, cache: PrintInfo) -> Self {
        let mut main = Rect::new(p, true, Kind::Leaf(Cell::new(cache)), false, frame);

        main.eqs.extend([
            main.tl.x() | EQ(EDGE_PRIO) | 0.0,
            main.tl.y() | EQ(EDGE_PRIO) | 0.0,
            main.br.x() | EQ(EDGE_PRIO) | p.max().x(),
            main.br.y() | EQ(EDGE_PRIO) | p.max().y(),
        ]);
        p.add_eqs(main.eqs.clone());

        main
    }

    /// Returns a new `Rect` which is supposed to be spawned in
    /// [`Text`]
    ///
    /// [`Text`]: duat_core::text::Text
    pub fn new_spawned_on_text(
        p: &Printer,
        id: SpawnId,
        frame: Frame,
        cache: PrintInfo,
        specs: SpawnSpecs,
    ) -> (Self, Constraints) {
        let mut rect = Rect::new(p, false, Kind::Leaf(Cell::new(cache)), true, frame);

        let len = match specs.orientation.axis() {
            Axis::Horizontal => specs.width,
            Axis::Vertical => specs.height,
        };

        let ([center, len], tl) = p.new_text_spawned(
            id,
            len,
            specs.orientation.axis(),
            specs.orientation.prefers_before(),
        );

        rect.set_spawned_eqs(p, specs, [center, len], [tl.x().into(), tl.y().into()], [
            tl.x() + 1.0,
            tl.y() + 1.0,
        ]);

        let dims = [specs.width, specs.height];
        let cons = Constraints::new(p, dims, specs.hidden, &rect, None);

        (rect, cons)
    }

    /// Returns a new spawned `Rect`, placed around an existing one
    ///
    /// This can fail (returning [`None`]) if the `Rect` in question
    /// can't be found within this one.
    pub fn new_spawned_on_widget(
        &mut self,
        specs: SpawnSpecs,
        id: AreaId,
        p: &Printer,
        info: PrintInfo,
    ) -> Option<(Rect, Constraints)> {
        let parent = self.get(id)?;
        let mut rect = Rect::new(p, false, Kind::end(info), true, self.frame);

        // Left/bottom, center, right/top, above/left, below/right strengths
        let (deps, len) = match specs.orientation.axis() {
            Axis::Horizontal => ([parent.tl.x(), parent.br.x()], specs.width),
            Axis::Vertical => ([parent.tl.y(), parent.br.y()], specs.height),
        };

        let [center, len] = p.new_widget_spawned(
            deps,
            len,
            specs.orientation.axis(),
            specs.orientation.prefers_before(),
        );

        rect.set_spawned_eqs(
            p,
            specs,
            [center, len],
            [rect.tl.x().into(), rect.tl.y().into()],
            [rect.br.x().into(), rect.br.y().into()],
        );

        let dims = [specs.width, specs.height];
        let cons = Constraints::new(p, dims, specs.hidden, self, Some(parent));

        Some((rect, cons))
    }

    /// Creates a new parent for a given [`Rect`]
    pub fn new_parent_for(
        &mut self,
        p: &Printer,
        id: AreaId,
        axis: Axis,
        do_cluster: bool,
        on_files: bool,
    ) -> bool {
        let frame = self.frame;

        let (mut child, cons, parent_id) = if let Some((i, orig)) = self.get_parent_mut(id) {
            let kind = Kind::middle(axis, do_cluster);
            let mut parent = Rect::new(p, on_files, kind, false, frame);

            let axis = orig.kind.axis().unwrap();
            let (rect, cons) = orig.children_mut().unwrap().remove(i);

            let is_resizable = rect.is_resizable_on(axis, &cons);
            parent.set_pushed_eqs(i, orig, p, frame, is_resizable, None);

            let parent_id = parent.id;
            let entry = (parent, Constraints::default());
            orig.children_mut().unwrap().insert(i, entry);

            if i > 0 {
                let (mut rect, cons) = orig.children_mut().unwrap().remove(i - 1);
                let is_resizable = rect.is_resizable_on(axis, &cons);
                rect.set_pushed_eqs(i - 1, orig, p, frame, is_resizable, None);
                let entry = (rect, cons);
                orig.children_mut().unwrap().insert(i - 1, entry);
            }

            (rect, Some(cons), parent_id)
        } else if id == self.id {
            let kind = Kind::middle(axis, do_cluster);
            let mut parent = Rect::new(p, on_files, kind, false, frame);

            parent.eqs.extend([
                parent.tl.x() | EQ(EDGE_PRIO) | 0.0,
                parent.tl.y() | EQ(EDGE_PRIO) | 0.0,
                parent.br.x() | EQ(EDGE_PRIO) | p.max().x(),
                parent.br.y() | EQ(EDGE_PRIO) | p.max().y(),
            ]);
            p.add_eqs(parent.eqs.clone());

            let parent_id = parent.id;
            (std::mem::replace(self, parent), None, parent_id)
        } else {
            return false;
        };

        let parent = self.get(parent_id).unwrap();
        let cons = match cons.map(|cons| cons.apply(&child, Some(parent))) {
            Some((cons, eqs)) => {
                p.add_eqs(eqs);
                cons
            }
            None => Constraints::default(),
        };

        let parent = self.get_mut(parent_id).unwrap();
        let is_resizable = child.is_resizable_on(axis, &cons);
        child.set_pushed_eqs(0, parent, p, frame, is_resizable, None);

        parent.children_mut().unwrap().push((child, cons));

        true
    }

    /// Returns a new [`Rect`] with no default [`Constraints`]
    fn new(p: &Printer, on_files: bool, kind: Kind, is_floating: bool, frame: Frame) -> Self {
        let (tl, br) = (p.new_point(), p.new_point());
        Rect {
            id: AreaId::new(),
            tl,
            br,
            eqs: Vec::new(),
            is_floating,
            kind,
            on_files,
            edge: None,
            frame,
        }
    }

    ////////// Direct tree modification

    /// Pushes a new [`Rect`] onto another
    ///
    /// This assumes that there is a parent for the given `Rect`,
    /// returning [`None`] if that's not the case. You should call
    /// [`Rect::new_parent_for`], if that wasn't the case before.
    pub fn push(
        &mut self,
        p: &Printer,
        specs: PushSpecs,
        id: AreaId,
        on_files: bool,
        info: PrintInfo,
    ) -> Option<(AreaId, Option<AreaId>)> {
        let axis = specs.axis();

        let (can_be_sibling, can_be_child) = {
            let parent_is_cluster = if let Some((_, parent)) = self.get_parent(id) {
                parent.is_clustered()
            } else if id == self.id {
                specs.cluster
            } else {
                return None;
            };

            let target_is_cluster = self.get(id).is_some_and(Rect::is_clustered);

            // Clustering is what determines if a new rect can be a child or not.
            // In order to simplify, for example, swapping files around, it would
            // be helpful to keep only the stuff that is clustered to that file on
            // the same parent, even if other Areas could reasonable be placed
            // alognside it.
            let can_be_sibling = parent_is_cluster == specs.cluster;
            let can_be_child = target_is_cluster == specs.cluster;
            (can_be_sibling, can_be_child)
        };

        // Check if the target's parent has the same `Axis`.
        let (id, new_parent_id) = if can_be_sibling
            && let Some((_, parent)) = self.get_parent_mut(id)
            && parent.aligns_with(axis)
        {
            (id, None)
        // Check if the target has the same `Axis`.
        } else if can_be_child
            && let Some(parent) = self.get_mut(id)
            && parent.aligns_with(axis)
        {
            let children = parent.children().unwrap();
            let target = match specs.comes_earlier() {
                true => children.first().unwrap().0.id(),
                false => children.last().unwrap().0.id(),
            };

            (target, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            self.new_parent_for(p, id, axis, specs.cluster, on_files);
            let (_, parent) = self.get_parent(id).unwrap();

            (id, Some(parent.id()))
        };

        let frame = self.frame;

        let (i, mut rect, parent, cons, axis) = {
            let (i, parent) = self.get_parent(id)?;
            let rect = Rect::new(p, on_files, Kind::end(info), false, self.frame);

            let dims = [specs.width, specs.height];
            let cons = Constraints::new(p, dims, specs.hidden, &rect, Some(parent));

            let parent = self.get_mut(parent.id()).unwrap();
            let axis = parent.kind.axis().unwrap();

            if specs.comes_earlier() {
                (i, rect, parent, cons, axis)
            } else {
                (i + 1, rect, parent, cons, axis)
            }
        };

        let new_id = rect.id();
        rect.set_pushed_eqs(i, parent, p, frame, cons.is_resizable_on(axis), None);
        parent.children_mut().unwrap().insert(i, (rect, cons));

        let (i, (mut rect_to_fix, cons_to_fix)) = if i == 0 {
            (1, parent.children_mut().unwrap().remove(1))
        } else {
            (i - 1, parent.children_mut().unwrap().remove(i - 1))
        };
        let is_resizable = rect_to_fix.is_resizable_on(axis, &cons_to_fix);
        rect_to_fix.set_pushed_eqs(i, parent, p, frame, is_resizable, None);
        let entry = (rect_to_fix, cons_to_fix);
        parent.children_mut().unwrap().insert(i, entry);

        Some((new_id, new_parent_id))
    }

    /// Deletes a given [`Rect`], alongside all its children
    ///
    /// This can fail (returning [`None`]) if the `Rect` in question
    /// can't be found within this one.
    pub fn delete(
        &mut self,
        p: &Printer,
        id: AreaId,
    ) -> Option<(Rect, Constraints, Option<AreaId>)> {
        let frame = self.frame;

        let id = self.get_cluster_master(id)?;
        let (i, parent) = self.get_parent_mut(id)?;

        let (mut rm_rect, rm_cons) = parent.children_mut().unwrap().remove(i);
        p.remove_rect(&mut rm_rect);

        let (i, parent, rm_parent_id) = if parent.children().unwrap().len() == 1 {
            let parent_id = parent.id();
            let (mut rect, cons) = parent.children_mut().unwrap().remove(0);
            let (i, grandparent) = self.get_parent_mut(parent_id)?;
            let (mut rm_parent, _) = grandparent.children_mut().unwrap().remove(i);
            p.remove_rect(&mut rm_parent);

            let axis = grandparent.kind.axis().unwrap();
            let is_resizable = rect.is_resizable_on(axis, &cons);
            rect.set_pushed_eqs(i, grandparent, p, frame, is_resizable, None);
            grandparent.children_mut().unwrap().insert(i, (rect, cons));

            (i, grandparent, Some(parent_id))
        } else {
            (i, parent, None)
        };

        let (i, (mut rect_to_fix, cons)) = if i == 0 {
            (0, parent.children_mut().unwrap().remove(0))
        } else {
            (i - 1, parent.children_mut().unwrap().remove(i - 1))
        };
        let axis = parent.kind.axis().unwrap();
        let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
        rect_to_fix.set_pushed_eqs(i, parent, p, frame, is_resizable, None);
        let entry = (rect_to_fix, cons);
        parent.children_mut().unwrap().insert(i, entry);

        Some((rm_rect, rm_cons, rm_parent_id))
    }

    /// Swaps two given [`Rect`]s
    ///
    /// This can fail (returning `false`) if the `Rect`s in question
    /// can't be found in this one, or if one of them is the main
    /// `Rect`, since swapping a parent with one of its children
    /// makes no sense.
    pub fn swap(&mut self, p: &Printer, id0: AreaId, id1: AreaId) -> bool {
        if (id0 == self.id || id1 == self.id)
            || (self.get_parent(id0).is_none() || self.get_parent(id1).is_none())
        {
            return false;
        }

        let frame = self.frame;

        // We're gonna need to reconstrain a bunch of Areas, this is the most
        // ergonomic way of doing that.
        let mut to_constrain = Some(Vec::new());
        let mut old_eqs = Vec::new();

        let (i0, parent0) = self.get_parent_mut(id0).unwrap();
        let p0_id = parent0.id();
        let (mut rect0, mut cons0) = parent0.children_mut().unwrap().remove(i0);
        old_eqs.extend(cons0.drain_eqs());

        let (mut rect1, _) = {
            let (i1, parent1) = self.get_parent_mut(id1).unwrap();
            let (_, cons1) = &parent1.children().unwrap()[i1];
            let mut cons1 = cons1.clone();
            old_eqs.extend(cons1.drain_eqs());

            let axis = parent1.kind.axis().unwrap();
            let is_resizable = rect0.is_resizable_on(axis, &cons1);
            to_constrain = rect0.set_pushed_eqs(i1, parent1, p, frame, is_resizable, to_constrain);

            parent1
                .children_mut()
                .unwrap()
                .insert(i1, (rect0, cons1.clone()));

            let (i, (mut rect_to_fix, cons)) = if i1 == 0 {
                (1, parent1.children_mut().unwrap().remove(1))
            } else {
                (i1 - 1, parent1.children_mut().unwrap().remove(i1 - 1))
            };
            let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
            to_constrain =
                rect_to_fix.set_pushed_eqs(i, parent1, p, frame, is_resizable, to_constrain);
            let entry = (rect_to_fix, cons);
            parent1.children_mut().unwrap().insert(i, entry);

            parent1.children_mut().unwrap().remove(i1 + 1)
        };

        let parent0 = self.get_mut(p0_id).unwrap();
        let axis = parent0.kind.axis().unwrap();
        let is_resizable = rect1.is_resizable_on(axis, &cons0);
        to_constrain = rect1.set_pushed_eqs(i0, parent0, p, frame, is_resizable, to_constrain);

        parent0.children_mut().unwrap().insert(i0, (rect1, cons0));

        let (i, (mut rect_to_fix, cons)) = if i0 == 0 {
            (1, parent0.children_mut().unwrap().remove(1))
        } else {
            (i0 - 1, parent0.children_mut().unwrap().remove(i0 - 1))
        };
        let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
        to_constrain = rect_to_fix.set_pushed_eqs(i, parent0, p, frame, is_resizable, to_constrain);
        let entry = (rect_to_fix, cons);
        parent0.children_mut().unwrap().insert(i, entry);

        p.remove_eqs(old_eqs);
        constrain_areas(to_constrain.unwrap(), self, p);

        true
    }

    /// Resets the equalities of a given [`Rect`]
    ///
    /// This can fail (returning `false`) if the `Rect` in question
    /// can't be found within this one.
    pub fn reset_eqs(&mut self, p: &Printer, id: AreaId) -> bool {
        let frame = self.frame;
        let mut to_cons = Some(Vec::new());

        if let Some((i, parent)) = self.get_parent_mut(id) {
            let (mut rect, cons) = parent.children_mut().unwrap().remove(i);

            let axis = parent.kind.axis().unwrap();
            let is_resizable = rect.is_resizable_on(axis, &cons);
            to_cons = rect.set_pushed_eqs(i, parent, p, frame, is_resizable, to_cons);

            parent.children_mut().unwrap().insert(i, (rect, cons));

            let (i, (mut rect_to_fix, cons)) = if i == 0 {
                (1, parent.children_mut().unwrap().remove(1))
            } else {
                (i - 1, parent.children_mut().unwrap().remove(i - 1))
            };
            let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
            to_cons = rect_to_fix.set_pushed_eqs(i, parent, p, frame, is_resizable, to_cons);
            let entry = (rect_to_fix, cons);
            parent.children_mut().unwrap().insert(i, entry);
        } else if id == self.id {
            let old_eqs: Vec<Equality> = self.drain_eqs().collect();
            self.eqs.extend([
                self.tl.x() | EQ(EDGE_PRIO) | 0.0,
                self.tl.y() | EQ(EDGE_PRIO) | 0.0,
                self.br.x() | EQ(EDGE_PRIO) | p.max().x(),
                self.br.y() | EQ(EDGE_PRIO) | p.max().y(),
            ]);
            p.replace(old_eqs, self.eqs.clone());

            if let Kind::Branch { children, axis, .. } = &mut self.kind {
                let axis = *axis;
                for i in 0..children.len() {
                    let (mut child, cons) = self.children_mut().unwrap().remove(i);
                    let is_resizable = child.is_resizable_on(axis, &cons);
                    to_cons = child.set_pushed_eqs(i, self, p, frame, is_resizable, to_cons);
                    self.children_mut().unwrap().insert(i, (child, cons));
                }
            }
        } else {
            return false;
        }

        constrain_areas(to_cons.unwrap(), self, p);

        true
    }

    ////////// Constraint modification

    /// Sets base equalities for pushed [`Rect`]s
    ///
    /// These equalities guarantee that the [`Rect`]s will not go over
    /// the terminal size, and also won't intersect with any other
    /// [`Rect`]s.
    pub fn set_pushed_eqs(
        &mut self,
        i: usize,
        parent: &Rect,
        p: &Printer,
        fr: Frame,
        is_resizable: bool,
        mut to_constrain: Option<Vec<AreaId>>,
    ) -> Option<Vec<AreaId>> {
        if let Some(ids) = to_constrain.as_mut() {
            ids.push(self.id);
        }
        let axis = parent.kind.axis().unwrap();

        p.remove_eqs(self.drain_eqs());
        if let Some(width) = self.edge.take() {
            p.remove_edge(width);
        }

        self.eqs.extend([
            self.br.x() | GE(EDGE_PRIO) | self.tl.x(),
            self.br.y() | GE(EDGE_PRIO) | self.tl.y(),
        ]);

        if i == 0 {
            self.eqs
                .push(self.start(axis) | EQ(EDGE_PRIO) | parent.start(axis));
        }

        self.eqs.extend([
            self.start(axis.perp()) | EQ(EDGE_PRIO) | parent.start(axis.perp()),
            self.end(axis.perp()) | EQ(EDGE_PRIO) | parent.end(axis.perp()),
        ]);

        let Kind::Branch { children, clustered, .. } = &parent.kind else {
            unreachable!();
        };

        // If possible, try to make both Rects have the same length.
        // This may not necessarily be the next child.
        if is_resizable
            && !clustered
            && let Some((res, _)) = children[i..]
                .iter()
                .find(|(child, cons)| child.is_resizable_on(axis, cons))
        {
            self.eqs
                .push(self.len(axis) | EQ(EQ_LEN_PRIO) | res.len(axis));
        }

        if let Some((next, _)) = children.get(i) {
            let edge = match (self.on_files, next.on_files) {
                (true, true) => fr.border_edge_on(axis),
                (true, false) | (false, true) => fr.files_edge_on(axis),
                (false, false) => 0.0,
            };

            if edge == 1.0 && !*clustered {
                let width = p.set_edge(self.br, next.tl, axis, fr);
                self.eqs.extend([
                    width | EQ(FRAME_PRIO) | 1.0,
                    (self.end(axis) + width) | EQ(EDGE_PRIO) | next.start(axis),
                    // Makes the frame have len = 0 when either of its
                    // side widgets have len == 0.
                    width | GE(EDGE_PRIO) | 0.0,
                    width | LE(EDGE_PRIO) | 1.0,
                    self.len(axis) | GE(EDGE_PRIO) | width,
                    next.len(axis) | GE(EDGE_PRIO) | width,
                ]);
                self.edge = Some(width);
            } else {
                self.eqs
                    .push(self.end(axis) | EQ(EDGE_PRIO) | next.start(axis));
            }
        } else {
            self.eqs
                .push(self.end(axis) | EQ(EDGE_PRIO) | parent.end(axis));
        }

        if let Kind::Branch { children, axis, .. } = &mut self.kind {
            let axis = *axis;

            for i in 0..children.len() {
                // We have to do this, since set_base_eqs assumes that the child in
                // question wasn't in yet.
                let (mut child, cons) = self.children_mut().unwrap().remove(i);
                let is_resizable = child.is_resizable_on(axis, &cons);
                to_constrain = child.set_pushed_eqs(i, self, p, fr, is_resizable, to_constrain);
                self.children_mut().unwrap().insert(i, (child, cons));
            }
        }

        p.add_eqs(self.eqs.clone());

        to_constrain
    }

    /// Sets base equalities for spawned [`Rect`]s
    ///
    /// These equalities ensure that the [`Rect`]s stay within the
    /// borders of the terminal, but unlike with pushed [`Rect`]s,
    /// there are no requirement for no collisions with other
    /// [`Rect`]s
    pub fn set_spawned_eqs(
        &mut self,
        p: &Printer,
        specs: SpawnSpecs,
        [center, len]: [Variable; 2],
        [tl_x, tl_y]: [Expression; 2],
        [br_x, br_y]: [Expression; 2],
    ) {
        use duat_core::ui::Orientation::*;

        let ends = match specs.orientation.axis() {
            Axis::Horizontal => [self.tl.x(), self.br.x()],
            Axis::Vertical => [self.tl.y(), self.br.y()],
        };

        self.eqs.extend([
            self.tl.x() | GE(EDGE_PRIO) | 0.0,
            self.tl.y() | GE(EDGE_PRIO) | 0.0,
            self.br.x() | LE(EDGE_PRIO) | p.max().x(),
            self.tl.y() | LE(EDGE_PRIO) | p.max().y(),
            self.br.x() | GE(EDGE_PRIO) | self.tl.x(),
            self.br.y() | GE(EDGE_PRIO) | self.tl.y(),
        ]);

        let align_eq = match specs.orientation {
            VerLeftAbove | VerLeftBelow => self.tl.x() | EQ(SPAWN_ALIGN_PRIO) | br_x,
            VerCenterAbove | VerCenterBelow => {
                self.mean(Axis::Horizontal) | EQ(SPAWN_ALIGN_PRIO) | ((tl_x + br_x) / 2.0)
            }
            VerRightAbove | VerRightBelow => self.br.x() | EQ(SPAWN_ALIGN_PRIO) | br_x,
            HorTopLeft | HorTopRight => self.tl.y() | EQ(SPAWN_ALIGN_PRIO) | tl_y,
            HorCenterLeft | HorCenterRight => {
                self.mean(Axis::Vertical) | EQ(SPAWN_ALIGN_PRIO) | ((tl_y + br_y) / 2.0)
            }
            HorBottomLeft | HorBottomRight => self.br.y() | EQ(SPAWN_ALIGN_PRIO) | br_y,
        };

        self.eqs.extend(
            specs
                .width
                .map(|width| (self.br.x() - self.tl.x()) | EQ(SPAWN_LEN_PRIO) | width)
                .into_iter()
                .chain(
                    specs
                        .height
                        .map(|height| (self.br.y() - self.tl.y()) | EQ(SPAWN_LEN_PRIO) | height),
                )
                .chain([
                    align_eq,
                    ends[0] | EQ(SPAWN_POS_PRIO) | (center - len / 2.0),
                    ends[1] | EQ(SPAWN_POS_PRIO) | (center + len / 2.0),
                ]),
        );

        p.add_eqs(self.eqs.clone());
        p.update(false, true);
    }

    /// Removes all [`Equality`]s which define [`self`]
    pub fn drain_eqs(&mut self) -> impl Iterator<Item = Equality> {
        self.eqs.drain(..)
    }

    /////////// Rect getters

    /// Gets a mut reference to the parent of the `id`'s [`Rect`]
    pub fn get_mut(&mut self, id: AreaId) -> Option<&mut Rect> {
        fetch_mut(self, id)
    }

    /// Fetches the [`Rect`] which holds the [`Rect`]
    /// of the given index.
    ///
    /// Also returns the child's "position", going top to bottom or
    /// left to right.
    pub fn get_parent_mut(&mut self, id: AreaId) -> Option<(usize, &mut Rect)> {
        let (n, parent) = self.get_parent(id)?;
        let id = parent.id;
        Some((n, self.get_mut(id).unwrap()))
    }

    /// Gets a mut reference to the constraints of a [`Rect`]
    ///
    /// Can fail if the [`Rect`] is the main one of the window, thus
    /// being unable to be constrained.
    pub fn get_constraints_mut(&mut self, id: AreaId) -> Option<&mut Constraints> {
        self.get_parent_mut(id)
            .map(|(pos, parent)| &mut parent.children_mut().unwrap()[pos].1)
    }

    /// Fetches the parent of the [`RwData<Rect>`] with the given
    /// index, including its positional index and the [`Axis`] of
    /// its children. Fetches the [`RwData<Rect>`] of the given
    /// index, if there is one.
    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        fn fetch(rect: &Rect, id: AreaId) -> Option<&Rect> {
            if rect.id == id {
                Some(rect)
            } else if let Kind::Branch { children, .. } = &rect.kind {
                children.iter().find_map(|(child, _)| fetch(child, id))
            } else {
                None
            }
        }

        fetch(self, id)
    }

    /// Gets the parent of the `id`'s [`Rect`]
    ///
    /// Also returns the child's "position", given an [`Axis`],
    /// going top to bottom or left to right.
    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        fetch_parent(self, id)
    }

    /// Gets the "cluster master" of a [`Rect`]
    pub fn get_cluster_master(&self, id: AreaId) -> Option<AreaId> {
        let Some((_, mut rect)) = self.get_parent(id).filter(|(_, p)| p.is_clustered()) else {
            return self.get(id).map(|rect| rect.id());
        };

        loop {
            if let Some((_, parent)) = self.get_parent(rect.id())
                && parent.is_clustered()
            {
                rect = parent
            } else {
                break Some(rect.id());
            }
        }
    }

    /// The children of this [`Rect`], if it is a [parent]
    ///
    /// [parent]: Kind::Middle
    pub fn children(&self) -> Option<&[(Rect, Constraints)]> {
        self.kind.children()
    }

    /// The mutable children of this [`Rect`], if it is a [parent]
    ///
    /// [parent]: Kind::Middle
    pub fn children_mut(&mut self) -> Option<&mut Vec<(Rect, Constraints)>> {
        self.kind.children_mut()
    }

    /////////// Variables and Expressions

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]
    ///
    /// It will be the left or upper side of a [`Rect`].
    pub fn start(&self, axis: Axis) -> Variable {
        match axis {
            Horizontal => self.tl.x(),
            Vertical => self.tl.y(),
        }
    }

    /// A [`Variable`], representing the "end" of [`self`], given an
    /// [`Axis`]
    ///
    /// It will be the right or lower side of a [`Rect`].
    pub fn end(&self, axis: Axis) -> Variable {
        match axis {
            Horizontal => self.br.x(),
            Vertical => self.br.y(),
        }
    }

    /// An [`Expression`] representing the length of [`self`] on a
    /// given [`Axis`]
    pub fn len(&self, axis: Axis) -> Expression {
        match axis {
            Horizontal => self.br.x() - self.tl.x(),
            Vertical => self.br.y() - self.tl.y(),
        }
    }

    /// The mean of the [`Variable`]s in a given [`Axis`]
    pub fn mean(&self, axis: Axis) -> Expression {
        match axis {
            Horizontal => (self.tl.x() + self.br.x()) / 2.0,
            Vertical => (self.tl.y() + self.br.y()) / 2.0,
        }
    }

    /// The two [`VarPoint`]s determining this [`Rect`]'s shape
    pub fn var_points(&self) -> [VarPoint; 2] {
        [self.tl, self.br]
    }

    /// The edge of this [`Rect`], if it has one
    pub fn edge(&self) -> Option<Variable> {
        self.edge
    }

    ////////// Querying Functions

    /// Wether this [`Rect`]'s [`Coords`] have changed
    ///
    /// This is caused when [`Constraints`] change, be it directly, or
    /// indirectly through removals, swaps, or the [`Constraints`] of
    /// other [`Rect`]s changing.
    pub fn has_changed(&self, layout: &Layout) -> bool {
        layout.printer.coords_have_changed(self.var_points())
    }

    /// The [`AreaId`] of this [`Rect`]
    pub fn id(&self) -> AreaId {
        self.id
    }

    /// Wether this [`Rect`]'s [`Area`] is part of a cluster
    ///
    /// Clusters are groups of [`Area`]s that should stick together
    /// by being placed under a separated parent, for the purposes of
    /// backup constraints, colective removal, and easier swapping.
    pub fn is_clustered(&self) -> bool {
        match &self.kind {
            Kind::Branch { clustered, .. } => *clustered,
            Kind::Leaf(..) => false,
        }
    }

    /// Wether this [`Rect`] aligns with the given [`Axis`]
    ///
    /// This can only the case if the [`Rect`] is a [parent].
    ///
    /// [parent]: Kind::Middle
    pub fn aligns_with(&self, other: Axis) -> bool {
        match &self.kind {
            Kind::Branch { axis, .. } => *axis == other,
            Kind::Leaf(..) => false,
        }
    }

    /// The [`PrintInfo`] of this [`Rect`]
    ///
    /// It is only [`Some`] if the [`Rect`] is a [child]
    ///
    /// [child]: Kind::End
    pub fn print_info(&self) -> Option<&Cell<PrintInfo>> {
        match &self.kind {
            Kind::Leaf(info) => Some(info),
            Kind::Branch { .. } => None,
        }
    }

    /// Wether this [`Rect`] is resizable on a given [`Axis`]
    ///
    /// This depends on its [`Constraints`], if it is a [child], or on
    /// the constraints of its children, if it is a [parent]
    ///
    /// [child]: Kind::End
    /// [parent]: Kind::Middle
    pub fn is_resizable_on(&self, axis: Axis, cons: &Constraints) -> bool {
        if let Kind::Branch { children, axis: child_axis, .. } = &self.kind
            && !children.is_empty()
        {
            let mut children = children.iter();
            if *child_axis == axis {
                children.any(|(child, cons)| child.is_resizable_on(axis, cons))
            } else {
                children.all(|(child, cons)| child.is_resizable_on(axis, cons))
            }
        } else {
            cons.is_resizable_on(axis)
        }
    }

    pub fn is_floating(&self) -> bool {
        self.is_floating
    }
}

impl PartialEq for Rect {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

enum Kind {
    Leaf(Cell<PrintInfo>),
    Branch {
        children: Vec<(Rect, Constraints)>,
        axis: Axis,
        clustered: bool,
    },
}

impl Kind {
    fn end(info: PrintInfo) -> Self {
        Self::Leaf(Cell::new(info))
    }

    fn middle(axis: Axis, clustered: bool) -> Self {
        Self::Branch { children: Vec::new(), axis, clustered }
    }

    fn axis(&self) -> Option<Axis> {
        match self {
            Kind::Branch { axis, .. } => Some(*axis),
            Kind::Leaf(..) => None,
        }
    }

    fn children(&self) -> Option<&[(Rect, Constraints)]> {
        match self {
            Kind::Branch { children, .. } => Some(children),
            Kind::Leaf(..) => None,
        }
    }

    fn children_mut(&mut self) -> Option<&mut Vec<(Rect, Constraints)>> {
        match self {
            Kind::Leaf(..) => None,
            Kind::Branch { children, .. } => Some(children),
        }
    }
}

fn fetch_parent(main: &Rect, id: AreaId) -> Option<(usize, &Rect)> {
    if main.id == id {
        return None;
    }
    let Kind::Branch { children, .. } = &main.kind else {
        return None;
    };

    children.iter().enumerate().find_map(|(pos, (child, _))| {
        if child.id == id {
            Some((pos, main))
        } else {
            fetch_parent(child, id)
        }
    })
}

fn fetch_mut(rect: &mut Rect, id: AreaId) -> Option<&mut Rect> {
    if rect.id == id {
        Some(rect)
    } else if let Kind::Branch { children, .. } = &mut rect.kind {
        children
            .iter_mut()
            .find_map(|(child, _)| fetch_mut(child, id))
    } else {
        None
    }
}

fn constrain_areas(to_constrain: Vec<AreaId>, main: &mut Rect, p: &Printer) {
    let mut old_eqs = Vec::new();
    let mut new_eqs = Vec::new();

    for id in to_constrain {
        let Some((i, parent)) = main.get_parent_mut(id) else {
            continue;
        };
        let (rect, mut cons) = parent.children_mut().unwrap().remove(i);
        old_eqs.extend(cons.drain_eqs());

        let (cons, eqs) = cons.apply(&rect, Some(parent));
        new_eqs.extend(eqs);

        let parent_id = parent.id;
        let parent = main.get_mut(parent_id).unwrap();
        parent.children_mut().unwrap().insert(i, (rect, cons));
    }
    p.replace(old_eqs, new_eqs)
}

pub fn transfer_vars(from_p: &Printer, to_p: &Printer, rect: &mut Rect) {
    let vars = from_p.remove_rect(rect);
    if let Some(children) = rect.children_mut() {
        to_p.insert_rect_vars(vars);

        for (child, _) in children.iter_mut() {
            transfer_vars(from_p, to_p, child)
        }
    } else {
        to_p.insert_rect_vars(vars);
    }
}
