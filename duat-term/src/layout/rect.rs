use std::cell::Cell;

use cassowary::{
    Expression, Variable,
    WeightedRelation::{EQ, GE, LE},
    strength::{REQUIRED, STRONG, WEAK},
};
use duat_core::ui::{
    Axis::{self, *},
    Corner, PushSpecs, SpawnSpecs,
};

use super::{Constraints, Layout};
use crate::{
    Area, AreaId, Equality, Frame,
    area::PrintInfo,
    print::{Printer, VarPoint},
};

enum Kind {
    End(Cell<PrintInfo>),
    Middle {
        children: Vec<(Rect, Constraints)>,
        axis: Axis,
        clustered: bool,
    },
}

impl Kind {
    fn end(info: PrintInfo) -> Self {
        Self::End(Cell::new(info))
    }

    fn middle(axis: Axis, clustered: bool) -> Self {
        Self::Middle { children: Vec::new(), axis, clustered }
    }

    fn axis(&self) -> Option<Axis> {
        match self {
            Kind::Middle { axis, .. } => Some(*axis),
            Kind::End(..) => None,
        }
    }

    fn children(&self) -> Option<&[(Rect, Constraints)]> {
        match self {
            Kind::Middle { children, .. } => Some(children),
            Kind::End(..) => None,
        }
    }

    fn children_mut(&mut self) -> Option<&mut Vec<(Rect, Constraints)>> {
        match self {
            Kind::End(..) => None,
            Kind::Middle { children, .. } => Some(children),
        }
    }
}

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
    kind: Kind,
    on_files: bool,
    edge: Option<Variable>,
}

impl Rect {
    /// Returns a new [`Rect`] with no default [`Constraints`]
    fn new(p: &Printer, on_files: bool, kind: Kind) -> Self {
        let (tl, br) = (p.new_point(), p.new_point());
        Rect {
            id: AreaId::new(),
            tl,
            br,
            eqs: Vec::new(),
            kind,
            on_files,
            edge: None,
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
            self.br.x() | GE(REQUIRED) | self.tl.x(),
            self.br.y() | GE(REQUIRED) | self.tl.y(),
        ]);

        if i == 0 {
            self.eqs
                .push(self.start(axis) | EQ(REQUIRED) | parent.start(axis));
        }

        self.eqs.extend([
            self.start(axis.perp()) | EQ(REQUIRED) | parent.start(axis.perp()),
            self.end(axis.perp()) | EQ(REQUIRED) | parent.end(axis.perp()),
        ]);

        let Kind::Middle { children, clustered, .. } = &parent.kind else {
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
            self.eqs.push(self.len(axis) | EQ(WEAK) | res.len(axis));
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
                    width | EQ(STRONG) | 1.0,
                    (self.end(axis) + width) | EQ(REQUIRED) | next.start(axis),
                    // Makes the frame have len = 0 when either of its
                    // side widgets have len == 0.
                    width | GE(REQUIRED) | 0.0,
                    width | LE(REQUIRED) | 1.0,
                    self.len(axis) | GE(REQUIRED) | width,
                    next.len(axis) | GE(REQUIRED) | width,
                ]);
                self.edge = Some(width);
            } else {
                self.eqs
                    .push(self.end(axis) | EQ(REQUIRED) | next.start(axis));
            }
        } else {
            self.eqs
                .push(self.end(axis) | EQ(REQUIRED) | parent.end(axis));
        }

        if let Kind::Middle { children, axis, .. } = &mut self.kind {
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
    pub fn set_spawned_eqs(&mut self, p: &Printer) {
        self.eqs.extend([
            self.tl.x() | GE(REQUIRED) | 0.0,
            self.tl.y() | GE(REQUIRED) | 0.0,
            self.br.x() | LE(REQUIRED) | p.max().x(),
            self.tl.y() | LE(REQUIRED) | p.max().y(),
            self.br.x() | GE(REQUIRED) | self.tl.x(),
            self.br.y() | GE(REQUIRED) | self.tl.y(),
        ]);
    }

    /// Removes all [`Equality`]s which define [`self`]
    pub fn drain_eqs(&mut self) -> impl Iterator<Item = Equality> {
        self.eqs.drain(..)
    }

    ////////// General queries

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
            Kind::Middle { clustered, .. } => *clustered,
            Kind::End(..) => false,
        }
    }

    /// Wether this [`Rect`] aligns with the given [`Axis`]
    ///
    /// This can only the case if the [`Rect`] is a [parent].
    ///
    /// [parent]: Kind::Middle
    pub fn aligns_with(&self, other: Axis) -> bool {
        match &self.kind {
            Kind::Middle { axis, .. } => *axis == other,
            Kind::End(..) => false,
        }
    }

    /// The [`PrintInfo`] of this [`Rect`]
    ///
    /// It is only [`Some`] if the [`Rect`] is a [child]
    ///
    /// [child]: Kind::End
    pub fn print_info(&self) -> Option<&Cell<PrintInfo>> {
        match &self.kind {
            Kind::End(info) => Some(info),
            Kind::Middle { .. } => None,
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
        if let Kind::Middle { children, axis: child_axis, .. } = &self.kind
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

    /// The two [`VarPoint`]s determining this [`Rect`]'s shape
    pub fn var_points(&self) -> [VarPoint; 2] {
        [self.tl, self.br]
    }

    /// The edge of this [`Rect`], if it has one
    pub fn edge(&self) -> Option<Variable> {
        self.edge
    }

    /// Two [`Expression`]s representing a corner of the [`Rect`]
    ///
    /// The first one is the vertical component, the second one is the
    /// horizontal.
    pub fn corner_exprs(&self, corner: Corner) -> [Expression; 2] {
        match corner {
            Corner::TopLeft => [self.tl.y().into(), self.tl.x().into()],
            Corner::Top => [self.tl.y().into(), (self.tl.x() + self.br.x()) / 2.0],
            Corner::TopRight => [self.tl.y().into(), self.br.x().into()],
            Corner::Right => [(self.tl.y() + self.br.y()) / 2.0, self.br.x().into()],
            Corner::BottomRight => [self.br.y().into(), self.br.x().into()],
            Corner::Bottom => [self.br.y().into(), (self.tl.x() + self.br.x()) / 2.0],
            Corner::BottomLeft => [self.br.y().into(), self.tl.x().into()],
            Corner::Left => [(self.tl.y() + self.br.y()) / 2.0, self.tl.x().into()],
            Corner::Center => [
                (self.tl.y() + self.br.y()) / 2.0,
                (self.tl.x() + self.br.x()) / 2.0,
            ],
        }
    }
}

impl PartialEq for Rect {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq<Area> for Rect {
    fn eq(&self, other: &Area) -> bool {
        self.id() == other.id
    }
}

pub struct Rects {
    pub(super) main: Rect,
    pub floating: Vec<(Rect, Constraints)>,
    fr: Frame,
}

impl Rects {
    /// Returns a new instance of [`Rects`]
    pub fn new(p: &Printer, fr: Frame, info: PrintInfo) -> Self {
        let mut main = Rect::new(p, true, Kind::end(info));
        main.eqs.extend([
            main.tl.x() | EQ(REQUIRED) | 0.0,
            main.tl.y() | EQ(REQUIRED) | 0.0,
            main.br.x() | EQ(REQUIRED) | p.max().x(),
            main.br.y() | EQ(REQUIRED) | p.max().y(),
        ]);
        p.add_eqs(main.eqs.clone());

        Self { main, floating: Vec::new(), fr }
    }

    /// Pushes a new [`Rect`] onto another
    pub fn push(
        &mut self,
        specs: PushSpecs,
        id: AreaId,
        p: &Printer,
        on_files: bool,
        info: PrintInfo,
    ) -> AreaId {
        let fr = self.fr;

        let mut rect = Rect::new(p, on_files, Kind::end(info));
        let new_id = rect.id();

        let (i, parent, cons, axis) = {
            let (i, parent) = self.get_parent(id).unwrap();
            let (vc, hc) = (specs.ver_cons(), specs.hor_cons());
            let cons = Constraints::new(p, vc, hc, specs.is_hidden(), &rect, parent.id(), self);

            let parent = self.get_mut(parent.id()).unwrap();
            let axis = parent.kind.axis().unwrap();

            if specs.comes_earlier() {
                (i, parent, cons, axis)
            } else {
                (i + 1, parent, cons, axis)
            }
        };

        rect.set_pushed_eqs(i, parent, p, fr, cons.is_resizable_on(axis), None);
        parent.children_mut().unwrap().insert(i, (rect, cons));

        let (i, (mut rect_to_fix, cons_to_fix)) = if i == 0 {
            (1, parent.children_mut().unwrap().remove(1))
        } else {
            (i - 1, parent.children_mut().unwrap().remove(i - 1))
        };
        let is_resizable = rect_to_fix.is_resizable_on(axis, &cons_to_fix);
        rect_to_fix.set_pushed_eqs(i, parent, p, fr, is_resizable, None);
        let entry = (rect_to_fix, cons_to_fix);
        parent.children_mut().unwrap().insert(i, entry);

        new_id
    }

    /// Spawns a new floating [`Rect`]
    pub fn spawn(&mut self, specs: SpawnSpecs, id: AreaId, p: &Printer, info: PrintInfo) -> AreaId {
        let parent = self.get(id).unwrap();
        let mut rect = Rect::new(p, false, Kind::end(info));

        rect.set_spawned_eqs(p);

        let mut strength = STRONG - 1.0;
        for &[from, anchor] in &specs.choices {
            let [from_y, from_x] = parent.corner_exprs(from);
            let [anchor_y, anchor_x] = rect.corner_exprs(anchor);
            rect.eqs.extend([
                // any value other than (x|y)_diff == 0.0 should result in an impossibly high (or
                // low) value, disabling both expressions if one of them fails.
                from_y.clone() | EQ(strength) | anchor_y,
                from_x.clone() | EQ(strength) | anchor_x,
            ]);
            strength -= 1.0;
        }

        let id = rect.id;

        let (vc, hc) = (specs.ver_cons(), specs.hor_cons());
        let cons = Constraints::new(p, vc, hc, specs.is_hidden(), &rect, parent.id(), self);
        p.add_eqs(rect.eqs.clone());
        p.update(false);
        
        self.floating.push((rect, cons));

        id
    }

    /// Deletes a given [`Rect`], alongside all its children
    pub fn delete(
        &mut self,
        p: &Printer,
        id: AreaId,
    ) -> Option<(Rect, Constraints, Option<AreaId>)> {
        let fr = self.fr;

        let id = self.get_cluster_master(id).unwrap_or(id);
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
            rect.set_pushed_eqs(i, grandparent, p, fr, is_resizable, None);
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
        rect_to_fix.set_pushed_eqs(i, parent, p, fr, is_resizable, None);
        let entry = (rect_to_fix, cons);
        parent.children_mut().unwrap().insert(i, entry);

        Some((rm_rect, rm_cons, rm_parent_id))
    }

    /// Swaps two given [`Rect`]s
    pub fn swap(&mut self, p: &Printer, id0: AreaId, id1: AreaId) {
        let fr = self.fr;
        // We're gonna need to reconstrain a bunch of Areas, this is the most
        // ergonomic way of doing that.
        let mut to_constrain = Some(Vec::new());
        let mut old_eqs = Vec::new();

        // We can't swap anything with the main Area, that makes no sense.
        if id0 == self.main.id || id1 == self.main.id {
            return;
        }
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
            to_constrain = rect0.set_pushed_eqs(i1, parent1, p, fr, is_resizable, to_constrain);

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
                rect_to_fix.set_pushed_eqs(i, parent1, p, fr, is_resizable, to_constrain);
            let entry = (rect_to_fix, cons);
            parent1.children_mut().unwrap().insert(i, entry);

            parent1.children_mut().unwrap().remove(i1 + 1)
        };

        let parent0 = self.get_mut(p0_id).unwrap();
        let axis = parent0.kind.axis().unwrap();
        let is_resizable = rect1.is_resizable_on(axis, &cons0);
        to_constrain = rect1.set_pushed_eqs(i0, parent0, p, fr, is_resizable, to_constrain);

        parent0.children_mut().unwrap().insert(i0, (rect1, cons0));

        let (i, (mut rect_to_fix, cons)) = if i0 == 0 {
            (1, parent0.children_mut().unwrap().remove(1))
        } else {
            (i0 - 1, parent0.children_mut().unwrap().remove(i0 - 1))
        };
        let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
        to_constrain = rect_to_fix.set_pushed_eqs(i, parent0, p, fr, is_resizable, to_constrain);
        let entry = (rect_to_fix, cons);
        parent0.children_mut().unwrap().insert(i, entry);

        p.remove_eqs(old_eqs);
        constrain_areas(to_constrain.unwrap(), self, p);
    }

    /// Resets the equalities of a given [`Rect`]
    pub fn reset_eqs(&mut self, p: &Printer, target: AreaId) {
        let fr = self.fr;
        let mut to_cons = Some(Vec::new());

        if let Some((i, parent)) = self.get_parent_mut(target) {
            let (mut rect, cons) = parent.children_mut().unwrap().remove(i);

            let axis = parent.kind.axis().unwrap();
            let is_resizable = rect.is_resizable_on(axis, &cons);
            to_cons = rect.set_pushed_eqs(i, parent, p, fr, is_resizable, to_cons);

            parent.children_mut().unwrap().insert(i, (rect, cons));

            let (i, (mut rect_to_fix, cons)) = if i == 0 {
                (1, parent.children_mut().unwrap().remove(1))
            } else {
                (i - 1, parent.children_mut().unwrap().remove(i - 1))
            };
            let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
            to_cons = rect_to_fix.set_pushed_eqs(i, parent, p, fr, is_resizable, to_cons);
            let entry = (rect_to_fix, cons);
            parent.children_mut().unwrap().insert(i, entry);
        } else if let Some(main) = self.get_mut(target) {
            let old_eqs: Vec<Equality> = main.drain_eqs().collect();
            main.eqs.extend([
                main.tl.x() | EQ(REQUIRED) | 0.0,
                main.tl.y() | EQ(REQUIRED) | 0.0,
                main.br.x() | EQ(REQUIRED) | p.max().x(),
                main.br.y() | EQ(REQUIRED) | p.max().y(),
            ]);
            p.replace(old_eqs, main.eqs.clone());

            if let Kind::Middle { children, axis, .. } = &mut main.kind {
                let axis = *axis;
                for i in 0..children.len() {
                    let (mut child, cons) = main.children_mut().unwrap().remove(i);
                    let is_resizable = child.is_resizable_on(axis, &cons);
                    to_cons = child.set_pushed_eqs(i, main, p, fr, is_resizable, to_cons);
                    main.children_mut().unwrap().insert(i, (child, cons));
                }
            }
        }

        constrain_areas(to_cons.unwrap(), self, p);
    }

    /// Creates a new parent for a given [`Rect`]
    pub fn new_parent_for(
        &mut self,
        id: AreaId,
        axis: Axis,
        p: &Printer,
        do_cluster: bool,
        on_files: bool,
    ) {
        let fr = self.fr;

        let (mut child, cons, parent_id) = {
            let mut parent = Rect::new(p, on_files, Kind::middle(axis, do_cluster));
            let parent_id = parent.id();

            let (target, cons) = if let Some((i, orig)) = self.get_parent_mut(id) {
                let axis = orig.kind.axis().unwrap();
                let (target, cons) = orig.children_mut().unwrap().remove(i);

                let is_resizable = target.is_resizable_on(axis, &cons);
                parent.set_pushed_eqs(i, orig, p, fr, is_resizable, None);

                let entry = (parent, Constraints::default());
                orig.children_mut().unwrap().insert(i, entry);

                if i > 0 {
                    let (mut rect, cons) = orig.children_mut().unwrap().remove(i - 1);
                    let is_resizable = rect.is_resizable_on(axis, &cons);
                    rect.set_pushed_eqs(i - 1, orig, p, fr, is_resizable, None);
                    let entry = (rect, cons);
                    orig.children_mut().unwrap().insert(i - 1, entry);
                }

                (target, Some(cons))
            } else {
                parent.eqs.extend([
                    parent.tl.x() | EQ(REQUIRED) | 0.0,
                    parent.tl.y() | EQ(REQUIRED) | 0.0,
                    parent.br.x() | EQ(REQUIRED) | p.max().x(),
                    parent.br.y() | EQ(REQUIRED) | p.max().y(),
                ]);
                p.add_eqs(parent.eqs.clone());
                (std::mem::replace(&mut self.main, parent), None)
            };

            (target, cons, parent_id)
        };

        let cons = match cons.map(|cons| cons.apply(&child, parent_id, self)) {
            Some((cons, eqs)) => {
                p.add_eqs(eqs);
                cons
            }
            None => Constraints::default(),
        };

        let parent = self.get_mut(parent_id).unwrap();

        let is_resizable = child.is_resizable_on(axis, &cons);
        child.set_pushed_eqs(0, parent, p, fr, is_resizable, None);

        parent.children_mut().unwrap().push((child, cons));
    }

    /// Gets a mut reference to the parent of the `id`'s [`Rect`]
    pub fn get_mut(&mut self, id: AreaId) -> Option<&mut Rect> {
        std::iter::once(&mut self.main)
            .chain(self.floating.iter_mut().map(|(rect, _)| rect))
            .find_map(|rect| fetch_mut(rect, id))
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

    /// Fetches the parent of the [`RwData<Rect>`] with the given
    /// index, including its positional index and the [`Axis`] of
    /// its children. Fetches the [`RwData<Rect>`] of the given
    /// index, if there is one.
    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        fn fetch(rect: &Rect, id: AreaId) -> Option<&Rect> {
            if rect.id == id {
                Some(rect)
            } else if let Kind::Middle { children, .. } = &rect.kind {
                children.iter().find_map(|(child, _)| fetch(child, id))
            } else {
                None
            }
        }

        std::iter::once(&self.main)
            .chain(self.floating.iter().map(|(rect, _)| rect))
            .find_map(|rect| fetch(rect, id))
    }

    /// Gets the parent of the `id`'s [`Rect`]
    ///
    /// Also returns the child's "position", given an [`Axis`],
    /// going top to bottom or left to right.
    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        std::iter::once(&self.main)
            .chain(self.floating.iter().map(|(rect, _)| rect))
            .find_map(|rect| fetch_parent(rect, id))
    }

    /// Gets the earliest parent that follows the [`Axis`]
    ///
    /// If the target's children are following the axis, return it,
    /// otherwise, keep looking back in order to find the latest
    /// ancestor whose axis matches `axis`.
    pub fn get_ancestor_on(&self, axis: Axis, id: AreaId) -> Option<(usize, &Rect)> {
        let mut target = self.get(id)?;
        let mut target_axis = target.kind.axis();
        let mut n = 0;

        while target_axis.is_none_or(|a| a != axis) {
            (n, target) = self.get_parent(target.id)?;
            target_axis = target.kind.axis();
        }

        Some((n, target))
    }

    /// Gets the siblings of the `id`'s [`Rect`]
    pub fn _get_siblings(&self, id: AreaId) -> Option<&[(Rect, Constraints)]> {
        self.get_parent(id).and_then(|(_, p)| p.kind.children())
    }

    pub fn get_cluster_master(&self, id: AreaId) -> Option<AreaId> {
        let (_, mut area) = self.get_parent(id).filter(|(_, p)| p.is_clustered())?;

        loop {
            if let Some((_, parent)) = self.get_parent(area.id())
                && parent.is_clustered()
            {
                area = parent
            } else {
                break Some(area.id());
            }
        }
    }

    pub fn get_constraints_mut(&mut self, id: AreaId) -> Option<&mut Constraints> {
        self.get_parent_mut(id)
            .map(|(pos, parent)| &mut parent.children_mut().unwrap()[pos].1)
    }
}

fn fetch_parent(main: &Rect, id: AreaId) -> Option<(usize, &Rect)> {
    if main.id == id {
        return None;
    }
    let Kind::Middle { children, .. } = &main.kind else {
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
    } else if let Kind::Middle { children, .. } = &mut rect.kind {
        children
            .iter_mut()
            .find_map(|(child, _)| fetch_mut(child, id))
    } else {
        None
    }
}

fn constrain_areas(to_constrain: Vec<AreaId>, rects: &mut Rects, p: &Printer) {
    let mut old_eqs = Vec::new();
    let mut new_eqs = Vec::new();

    for id in to_constrain {
        let Some((i, parent)) = rects.get_parent_mut(id) else {
            continue;
        };
        let (rect, mut cons) = parent.children_mut().unwrap().remove(i);
        old_eqs.extend(cons.drain_eqs());
        let parent_id = parent.id;

        let (cons, eqs) = cons.apply(&rect, parent_id, rects);
        new_eqs.extend(eqs);

        let parent = rects.get_mut(parent_id).unwrap();
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
