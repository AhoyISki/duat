use std::sync::atomic::Ordering;

use cassowary::{
    strength::{REQUIRED, STRONG, WEAK},
    Expression, Variable,
    WeightedRelation::{EQ, GE, LE},
};
use duat_core::{
    data::RwData,
    ui::{
        Axis::{self, *},
        Constraint,
    },
};

use super::{Constraints, Edge};
use crate::{
    area::{Coord, PrintInfo},
    print::{Printer, Sender, VarPoint, VarValue},
    Area, AreaId, Equality, Frame,
};

#[derive(Debug)]
enum Kind {
    End(Sender, RwData<PrintInfo>),
    Middle {
        children: Vec<(Rect, Constraints)>,
        axis: Axis,
        clustered: bool,
    },
}

impl Kind {
    fn middle(axis: Axis, grouped: bool) -> Self {
        Self::Middle {
            children: Vec::new(),
            axis,
            clustered: grouped,
        }
    }

    fn is_clustered(&self) -> bool {
        match self {
            Kind::End(..) => false,
            Kind::Middle { clustered, .. } => *clustered,
        }
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
#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct Rect {
    /// The index that this [`Rect`] is tied to.
    id: AreaId,
    tl: VarPoint,
    br: VarPoint,
    eqs: Vec<Equality>,
    kind: Kind,
}

impl Rect {
    /// Returns a new instance of [`Rect`], already adding its
    /// [`Variable`]s to the list.
    pub fn new_main(p: &mut Printer, fr: Frame) -> Self {
        let mut rect = Rect::new_raw(p);
        let (ver, hor) = fr.surround_edges();
        rect.eqs.extend([
            rect.tl.x() | EQ(REQUIRED) | ver,
            rect.tl.y() | EQ(REQUIRED) | hor,
            rect.br.x() | EQ(REQUIRED) | (p.max().x() - ver),
            rect.br.y() | EQ(REQUIRED) | (p.max().y() - hor),
        ]);
        p.add_equalities(&rect.eqs);

        rect
    }

    pub fn insert(i: usize, parent: AreaId, p: &mut Printer, fr: Frame, rects: &mut Rects) {
        let parent = rects.get_mut(parent).unwrap();

        let mut rect = Rect::new_raw(p);

        rect.set_base_eqs(i, parent, p, fr);

        let Kind::Middle { children, .. } = &mut parent.kind else {
            unreachable!();
        };

        children.insert(i, (rect, Constraints::default()));

        if i > 0 {
            let (mut prev, cons) = children.remove(i - 1);
            prev.set_base_eqs(i - 1, parent, p, fr);

            let Kind::Middle { children, .. } = &mut parent.kind else {
                unreachable!();
            };

            children.insert(i - 1, (prev, cons));
        }
    }

    pub fn set_base_eqs(&mut self, i: usize, parent: &mut Rect, p: &mut Printer, fr: Frame) {
        let axis = parent.kind.axis().unwrap();

        self.clear_eqs(p);

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

        if let Some((next, _)) = children.get(i) {
            if fr.border_edge_on(axis) == 1.0 && !*clustered {
                let frame = p.var_value();
                self.eqs.extend([
                    (self.end(axis) + &frame) | EQ(REQUIRED) | next.start(axis),
                    // Makes the frame have len = 0 when either of its
                    // side widgets have len == 0.
                    &frame | GE(REQUIRED) | 0.0,
                    &frame | EQ(STRONG) | -self.len(axis),
                    &frame | EQ(STRONG) | -next.len(axis),
                    &frame | EQ(WEAK) | 1.0,
                ]);
            } else {
                self.eqs
                    .push(self.end(axis) | EQ(REQUIRED) | next.start(axis));
            }
        } else {
            self.eqs
                .push(self.end(axis) | EQ(REQUIRED) | parent.end(axis));
        }

        p.add_equalities(&self.eqs);
    }

    /// Returns a new [`Rect`], which is supposed to replace an
    /// existing [`Rect`], as its new parent.
    pub fn new_parent_of(rect: &mut Rect, axis: Axis, p: &mut Printer, cluster: bool) -> Self {
        let parent = Rect {
            id: AreaId::new(),
            tl: rect.tl.clone(),
            br: rect.br.clone(),
            eqs: rect.eqs.clone(),
            kind: Kind::middle(axis, cluster),
        };

        rect.eqs.clear();
        rect.tl = VarPoint::new(p);
        rect.br = VarPoint::new(p);

        parent
    }

    fn new_raw(p: &mut Printer) -> Self {
        let tl = p.var_point();
        let br = p.var_point();
        let sender = p.sender(tl.clone(), br.clone());

        Rect {
            id: AreaId::new(),
            tl,
            br,
            eqs: Vec::new(),
            kind: Kind::End(sender, RwData::default()),
        }
    }

    /// Removes all [`Equality`]s which define the edges of
    /// [`self`].
    pub fn clear_eqs(&mut self, printer: &mut Printer) {
        for eq in self.eqs.drain(..) {
            printer.remove_equality(eq);
        }
    }

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]. It will be the left or upper side of a [`Rect`].
    pub fn start(&self, axis: Axis) -> &VarValue {
        match axis {
            Horizontal => self.tl.x(),
            Vertical => self.tl.y(),
        }
    }

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]. It will be the right or lower side of a [`Rect`].
    pub fn end(&self, axis: Axis) -> &VarValue {
        match axis {
            Horizontal => self.br.x(),
            Vertical => self.br.y(),
        }
    }

    /// An [`Expression`] representing the lenght of [`self`] on a
    /// given [`Axis`].
    pub fn len(&self, axis: Axis) -> Expression {
        match axis {
            Horizontal => self.br.x() - self.tl.x(),
            Vertical => self.br.y() - self.tl.y(),
        }
    }

    /// The current value for the lenght of [`self`] on a given
    /// [`Axis`].
    pub fn len_value(&self, axis: Axis) -> usize {
        match axis {
            Horizontal => self.br.x().value() - self.tl.x().value(),
            Vertical => self.br.y().value() - self.tl.y().value(),
        }
    }

    /// The top left corner of [`self`].
    pub fn tl(&self) -> Coord {
        Coord {
            x: self.tl.x().value(),
            y: self.tl.y().value(),
        }
    }

    /// The bottom right corner of [`self`].
    pub fn br(&self) -> Coord {
        Coord {
            x: self.br.x().value(),
            y: self.br.y().value(),
        }
    }

    /// Wether or not [`self`] has children.
    pub fn is_parent(&self) -> bool {
        matches!(self.kind, Kind::Middle { .. })
    }

    pub fn has_changed(&self) -> bool {
        let br_x_changed = self.br.x().has_changed();
        let br_y_changed = self.br.y().has_changed();
        let tl_x_changed = self.tl.x().has_changed();
        let tl_y_changed = self.tl.y().has_changed();

        br_x_changed || br_y_changed || tl_x_changed || tl_y_changed
    }

    pub fn id(&self) -> AreaId {
        self.id
    }

    pub fn is_clustered(&self) -> bool {
        match &self.kind {
            Kind::Middle { clustered: grouped, .. } => *grouped,
            Kind::End(..) => false,
        }
    }

    pub fn aligns_with(&self, other: Axis) -> bool {
        match &self.kind {
            Kind::Middle { axis, .. } => *axis == other,
            Kind::End(..) => false,
        }
    }

    pub fn children_len(&self) -> usize {
        match &self.kind {
            Kind::Middle { children, .. } => children.len(),
            Kind::End(..) => 0,
        }
    }

    pub fn sender(&self) -> Option<&Sender> {
        match &self.kind {
            Kind::End(sender, _) => Some(sender),
            Kind::Middle { .. } => None,
        }
    }

    pub fn print_info(&self) -> Option<&RwData<PrintInfo>> {
        match &self.kind {
            Kind::End(_, info) => Some(info),
            Kind::Middle { .. } => None,
        }
    }

    /// Sets the [`Equality`]s for the edges of [`self`],
    /// taking into account the potential frame, defined by [`Frame`]
    /// and the [`Rect`]'s position.
    fn set_equalities(
        &mut self,
        parent: &Rect,
        fr: Frame,
        edges: &mut Vec<Edge>,
        max: &VarPoint,
    ) -> (f64, f64) {
        let axis = parent.kind.axis().unwrap();
        self.eqs.extend([
            self.tl.x.var | GE(REQUIRED) | 0.0,
            self.tl.y.var | GE(REQUIRED) | 0.0,
            self.br.x.var | GE(REQUIRED) | self.tl.x.var,
            self.br.y.var | GE(REQUIRED) | self.tl.y.var,
        ]);

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        let (right, up, left, down) = if self.is_frameable(Some(parent)) {
            self.form_frame(fr, edges, max.coord())
        } else {
            (0.0, 0.0, 0.0, 0.0)
        };

        // para_left and para_right are the two sides of the axis.
        let (para_left, para_right, start, end) = match axis {
            Horizontal => (up, down, left, right),
            Vertical => (left, right, up, down),
        };

        // Setting the equalities of the sides parallel to the axis.
        self.eqs.extend([
            self.start(axis.perp()) | EQ(REQUIRED) | (parent.start(axis.perp()) + para_left),
            (self.end(axis.perp()) + para_right) | EQ(REQUIRED) | parent.end(axis.perp()),
        ]);

        (start, end)
    }

    /// Sets the [`Equality`]s for the main [`Rect`], which
    /// is supposed to be parentless. It takes into account a possible
    /// [`Frame`].
    fn set_main_edges(&mut self, fr: Frame, printer: &mut Printer, edges: &mut Vec<Edge>) {
        let (hor_edge, ver_edge) = if self.is_frameable(None) {
            fr.surround_edges()
        } else {
            (0.0, 0.0)
        };

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        if hor_edge == 1.0 {
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Vertical, fr));
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Vertical, fr));
        }
        if ver_edge == 1.0 {
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Horizontal, fr));
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Horizontal, fr));
        }

        self.eqs = vec![
            self.tl.x.var | EQ(REQUIRED) | hor_edge,
            self.tl.y.var | EQ(REQUIRED) | ver_edge,
            self.br.x.var | EQ(REQUIRED) | (printer.max().x.var - hor_edge),
            self.br.y.var | EQ(REQUIRED) | (printer.max().y.var - ver_edge),
        ];

        printer.add_equalities(&self.eqs);
    }

    /// Wheter or not [`self`] can be framed.
    fn is_frameable(&self, parent: Option<&Rect>) -> bool {
        if parent.is_some_and(|parent| parent.kind.is_clustered()) {
            false
        } else if let Kind::Middle { clustered: grouped, .. } = &self.kind {
            *grouped
        } else {
            true
        }
    }

    /// Forms the frame surrounding [`self`], considering its position
    /// and a [`Frame`].
    fn form_frame(&self, fr: Frame, edges: &mut Vec<Edge>, max: Coord) -> (f64, f64, f64, f64) {
        let (right, up, left, down) = fr.edges(&self.br, &self.tl, max);

        if right == 1.0 {
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Vertical, fr));
        }
        if up == 1.0 {
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Horizontal, fr));
        }
        if left == 1.0 {
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Vertical, fr));
        }
        if down == 1.0 {
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Horizontal, fr));
        }

        (right, up, left, down)
    }
}

impl PartialEq for Rect {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq<Area> for Rect {
    fn eq(&self, other: &Area) -> bool {
        self.id == other.id
    }
}

#[derive(Debug)]
pub struct Rects {
    pub main: Rect,
    floating: Vec<Rect>,
}

impl Rects {
    pub fn new(printer: &mut Printer, fr: Frame) -> Self {
        Self {
            main: Rect::new_main(printer, fr),
            floating: Vec::new(),
        }
    }

    /// Gets a mut reference to the parent of the `id`'s [`Rect`]
    pub fn get_mut(&mut self, id: AreaId) -> Option<&mut Rect> {
        std::iter::once(&mut self.main)
            .chain(&mut self.floating)
            .find_map(|rect| fetch_mut(rect, id))
    }

    /// Fetches the [`RwData<Rect>`] which holds the [`RwData<Rect>`]
    /// of the given index.
    ///
    /// Also returns the child's "position", going top to bottom or
    /// left to right.
    pub fn get_parent_mut(&mut self, id: AreaId) -> Option<(usize, &mut Rect)> {
        let (n, parent) = self.get_parent(id)?;
        let id = parent.id;
        Some((n, self.get_mut(id).unwrap()))
    }

    pub fn get_perp_parent_mut(&mut self, id: AreaId) -> Option<(usize, &mut Rect)> {
        let (n, parent) = self.get_perp_parent(id)?;
        let id = parent.id;
        Some((n, self.get_mut(id).unwrap()))
    }

    pub fn insert_child(&mut self, pos: usize, id: AreaId, child: Rect) {
        let Kind::Middle { children, .. } = &mut self.get_mut(id).unwrap().kind else {
            unreachable!();
        };

        children.insert(pos, (child, Constraints::default()));
    }

    /// Changes a child's constraint.
    pub fn set_ver_constraint(
        &mut self,
        id: AreaId,
        cons: Constraint,
        printer: &mut Printer,
        strength: f64,
    ) -> Option<Constraint> {
        self.set_constraint(id, cons, printer, Axis::Vertical, strength)
    }

    /// Changes a child's constraint.
    pub fn set_hor_constraint(
        &mut self,
        id: AreaId,
        cons: Constraint,
        printer: &mut Printer,
        strength: f64,
    ) -> Option<Constraint> {
        self.set_constraint(id, cons, printer, Axis::Horizontal, strength)
    }

    /// Changes a child's constraint.
    fn set_constraint(
        &mut self,
        id: AreaId,
        cons: Constraint,
        printer: &mut Printer,
        axis: Axis,
        strength: f64,
    ) -> Option<Constraint> {
        let (n, parent) = {
            let (n, parent) = self.get_parent_mut(id)?;
            if parent.kind.axis().unwrap() == axis {
                (n, parent)
            } else {
                self.get_perp_parent_mut(id)?
            }
        };

        let parent_len = parent.len(axis);
        let Kind::Middle { children, .. } = &mut parent.kind else {
            unreachable!();
        };

        let (child, conss) = &mut children[n];
        let eq = match cons {
            Constraint::Ratio(den, div) => {
                assert!(den < div, "Constraint::Ratio must be smaller than 1.");
                child.len(axis) | EQ(strength) | (parent_len * (den as f64 / div as f64))
            }
            Constraint::Length(len) => child.len(axis) | EQ(STRONG.max(strength)) | len,
            Constraint::Min(min) => child.len(axis) | GE(strength) | min,
            Constraint::Max(max) => child.len(axis) | LE(strength) | max,
        };

        let prev_cons = {
            let (prev_eq, prev_cons) = conss.on_mut(axis);
            if let Some(eq) = prev_eq.replace(eq.clone()) {
                printer.remove_equality(&eq)
            }
            prev_cons.replace(cons)
        };

        printer.add_equality(eq);

        prev_cons
    }

    pub fn take_constraints(
        &mut self,
        id: AreaId,
        vars: &mut Printer,
    ) -> (Option<Constraint>, Option<Constraint>) {
        self.get_parent_mut(id)
            .map(|(pos, parent)| {
                let Kind::Middle { children, .. } = &mut parent.kind else {
                    unreachable!();
                };
                let conss = std::mem::take(&mut children[pos].1);
                if let Some(equality) = conss.ver_eq {
                    vars.remove_equality(&equality);
                }
                if let Some(equality) = conss.hor_eq {
                    vars.remove_equality(&equality);
                }

                (conss.ver_cons, conss.hor_cons)
            })
            .unwrap_or_default()
    }

    pub fn set_edges(
        &mut self,
        id: AreaId,
        fr: Frame,
        printer: &mut Printer,
        edges: &mut Vec<Edge>,
    ) {
        if self.main.id == id {
            let rect = self.get_mut(id).unwrap();
            rect.clear_eqs(printer);
            rect.set_main_edges(fr, printer, edges);
        } else {
            let (pos, parent) = self.get_parent_mut(id).unwrap();
            prepare_child(parent, pos, printer, fr, edges)
        }
    }

    pub fn set_new_child_edges(
        &mut self,
        id: AreaId,
        fr: Frame,
        printer: &mut Printer,
        edges: &mut Vec<Edge>,
    ) {
        let child_is_main = self.main.id == self.get(id).unwrap().id;
        let (pos, parent) = self.get_parent_mut(id).unwrap();
        if child_is_main {
            parent.clear_eqs(printer);
            parent.set_main_edges(fr, printer, edges);
        } else {
            prepare_child(parent, pos, printer, fr, edges)
        }
    }

    pub fn set_edges_around(
        &mut self,
        id: AreaId,
        fr: Frame,
        printer: &mut Printer,
        edges: &mut Vec<Edge>,
    ) {
        let Some((pos, Kind::Middle { children, .. })) =
            self.get_parent(id).map(|(pos, parent)| (pos, &parent.kind))
        else {
            unreachable!();
        };

        let ids: Vec<_> = children.iter().map(|(child, _)| child.id).collect();

        for &pos in [pos.checked_sub(1), Some(pos), Some(pos + 1)]
            .iter()
            .flatten()
        {
            if let Some(&id) = ids.get(pos) {
                self.set_edges(id, fr, printer, edges)
            }
        }
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
            .chain(&self.floating)
            .find_map(|rect| fetch(rect, id))
    }

    /// Gets the parent of the `id`'s [`Rect`]
    ///
    /// Also returns the child's "position", given an [`Axis`],
    /// going top to bottom or left to right.
    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch_parent(rect, id))
    }

    /// Gets the perpendicular ancestor of the `id`'s [`Rect`]
    ///
    /// E.g. if the parent has an [`Axis::Horizontal`], then it will
    /// find the ancestor with an [`Axis::Vertical`].
    /// It also returns the index of the node that eventually contains
    /// the `id`'s [`Rect`]
    pub fn get_perp_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        let (mut n, mut parent) = self.get_parent(id)?;
        let axis = parent.kind.axis().unwrap();
        let mut parent_axis = axis;

        while parent_axis == axis {
            (n, parent) = self.get_parent(parent.id)?;
            parent_axis = parent.kind.axis().unwrap();
        }

        Some((n, parent))
    }

    pub fn get_adj_on(&self, id: AreaId, axis: Axis) -> Option<&Rect> {
        let (mut i, mut parent) = self.get_parent(id)?;
        loop {
            if parent.kind.axis().unwrap() == axis
                && let Some((adj, _)) = parent.kind.children().unwrap().get(i + 1)
            {
                break Some(adj);
            } else {
                (i, parent) = self.get_parent(parent.id)?;
            }
        }
    }

    /// Gets the siblings of the `id`'s [`Rect`]
    pub fn get_siblings(&self, id: AreaId) -> Option<&[(Rect, Constraints)]> {
        self.get_parent(id).and_then(|(_, p)| p.kind.children())
    }

    pub fn get_constraint_on(&self, id: AreaId, axis: Axis) -> Option<Constraint> {
        self.get_parent(id).and_then(|(pos, parent)| {
            let Kind::Middle { children, .. } = &parent.kind else {
                unreachable!();
            };
            children[pos].1.on(axis)
        })
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

/// Assigns all [`Equality`]s that are appropriate to a
/// given child, with the exception of [`Extent`]'s ones
fn prepare_child(
    parent: &mut Rect,
    n: usize,
    printer: &mut Printer,
    fr: Frame,
    edges: &mut Vec<Edge>,
) {
    let axis = parent.kind.axis().unwrap();
    let grouped = parent.kind.is_clustered();

    let temp = Kind::middle(axis, parent.kind.is_clustered());
    let Kind::Middle { mut children, .. } = std::mem::replace(&mut parent.kind, temp) else {
        unreachable!();
    };

    let target = &mut children[n].0;

    target.clear_eqs(printer);
    let (start, end) = target.set_equalities(parent, fr, edges, printer.max());

    if n == 0 {
        let constraint = target.start(axis) | EQ(REQUIRED) | (parent.start(axis) + start);
        target.eqs.push(constraint);
    }

    // Previous children carry the `Constraint`s for the `start` of their
    // successors.
    let constraint = if let Some((next, _)) = children.get(n + 1) {
        (children[n].0.end(axis) + end) | EQ(REQUIRED) | next.start(axis)
    } else {
        (children[n].0.end(axis) + end) | EQ(REQUIRED) | parent.end(axis)
    };

    let target = &mut children[n].0;
    target.eqs.push(constraint);

    printer.add_equalities(&target.eqs);

    if let Kind::Middle { children, .. } = &mut target.kind {
        let len = children.len();
        for pos in 0..len {
            prepare_child(target, pos, printer, fr, edges);
        }
    }

    parent.kind = Kind::Middle { children, axis, clustered: grouped };
}

/// Sets the ratio [`Equality`]s for the child of the given
/// index. This does include setting the [`Equality`] for
/// the previous child, if there is one.
pub fn set_ratios(parent: &mut Rect, pos: usize, printer: &mut Printer) {
    let axis = parent.kind.axis().unwrap();
    let Kind::Middle { children, .. } = &mut parent.kind else {
        unreachable!();
    };

    let (new, _) = &children[pos];
    let new_len = new.len(axis);
    let new_len_value = new.len_value(axis);

    let prev = children.iter().take(pos);
    let resizable_pos = prev
        .filter(|(_, conss)| conss.is_resizable_on(axis))
        .count();

    let mut iter = children
        .iter_mut()
        .filter(|(_, conss)| conss.is_resizable_on(axis));

    if resizable_pos > 0 {
        let (prev, conss) = iter.nth(resizable_pos - 1).unwrap();

        let ratio = if new_len_value == 0 {
            1.0
        } else {
            prev.len_value(axis) as f64 / new_len_value as f64
        };

        let eq = prev.len(axis) | EQ(WEAK) | (ratio * new_len.clone());
        *conss.on_mut(axis).0 = Some(eq.clone());
        printer.add_equality(eq);
    }

    if let Some((next, _)) = iter.nth(1) {
        let ratio = if next.len_value(axis) == 0 {
            1.0
        } else {
            new_len_value as f64 / next.len_value(axis) as f64
        };

        let equality = new_len | EQ(WEAK) | (ratio * next.len(axis));
        printer.add_equality(equality.clone());
    }
}
