use std::sync::atomic::Ordering;

use cassowary::{
    strength::{MEDIUM, REQUIRED, STRONG, WEAK},
    Expression, Variable,
    WeightedRelation::{EQ, GE, LE},
};
use duat_core::ui::{Axis, Constraint};

use super::{Edge, Equality, Length, VarPoint, Vars};
use crate::{area::Coord, Area, AreaId, Frame};

#[derive(Debug)]
struct Lineage {
    children: Vec<(Rect, Length)>,
    axis: Axis,
    clustered: bool,
}

impl Lineage {
    fn new(axis: Axis, clustered: bool) -> Self {
        Self {
            children: Vec::new(),
            axis,
            clustered,
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
    id: AreaId,
    /// The index that this [`Rect`] is tied to.
    tl: VarPoint,
    br: VarPoint,
    edge_equalities: Vec<Equality>,
    lineage: Option<Lineage>,
}

impl Rect {
    /// Returns a new instance of [`Rect`], already adding its
    /// [`Variable`]s to the list.
    pub fn new(constrainer: &mut Vars) -> Self {
        Rect {
            id: AreaId::new(),
            tl: VarPoint::new(constrainer),
            br: VarPoint::new(constrainer),
            edge_equalities: Vec::new(),
            lineage: None,
        }
    }

    /// Returns a new [`Rect`], which is supposed to replace an
    /// existing [`Rect`], as its new parent.
    pub fn new_parent_of(rect: &mut Rect, axis: Axis, vars: &mut Vars, clustered: bool) -> Self {
        let parent = Rect {
            id: AreaId::new(),
            tl: rect.tl.clone(),
            br: rect.br.clone(),
            edge_equalities: rect.edge_equalities.clone(),
            lineage: Some(Lineage::new(axis, clustered)),
        };

        rect.edge_equalities.clear();
        rect.tl = VarPoint::new(vars);
        rect.br = VarPoint::new(vars);

        parent
    }

    /// Removes all [`Equality`]s which define the edges of
    /// [`self`].
    pub fn clear_equalities(&mut self, vars: &mut Vars) {
        for constraint in self.edge_equalities.drain(..) {
            vars.remove_equality(&constraint);
        }
    }

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]. It will be the left or upper side of a [`Rect`].
    pub fn start(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.tl.x.var,
            Axis::Vertical => self.tl.y.var,
        }
    }

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]. It will be the right or lower side of a [`Rect`].
    pub fn end(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.br.x.var,
            Axis::Vertical => self.br.y.var,
        }
    }

    /// An [`Expression`] representing the lenght of [`self`] on a
    /// given [`Axis`].
    pub fn len(&self, axis: Axis) -> Expression {
        match axis {
            Axis::Horizontal => self.br.x.var - self.tl.x.var,
            Axis::Vertical => self.br.y.var - self.tl.y.var,
        }
    }

    /// The current value for the lenght of [`self`] on a given
    /// [`Axis`].
    pub fn len_value(&self, axis: Axis) -> usize {
        match axis {
            Axis::Horizontal => {
                self.br.x.value.load(Ordering::Relaxed) - self.tl.x.value.load(Ordering::Relaxed)
            }
            Axis::Vertical => {
                self.br.y.value.load(Ordering::Relaxed) - self.tl.y.value.load(Ordering::Relaxed)
            }
        }
    }

    /// The top left corner of [`self`].
    pub fn tl(&self) -> Coord {
        Coord {
            x: self.tl.x.value.load(Ordering::Acquire),
            y: self.tl.y.value.load(Ordering::Acquire),
        }
    }

    /// The bottom right corner of [`self`].
    pub fn br(&self) -> Coord {
        Coord {
            x: self.br.x.value.load(Ordering::Acquire),
            y: self.br.y.value.load(Ordering::Acquire),
        }
    }

    /// Wether or not [`self`] has children.
    pub fn is_parent(&self) -> bool {
        self.lineage.is_some()
    }

    /// The index that identifies [`self`].
    pub fn index(&self) -> AreaId {
        self.id
    }

    pub fn has_changed(&self) -> bool {
        let br_x_changed = self.br.x.has_changed.swap(false, Ordering::Release);
        let br_y_changed = self.br.y.has_changed.swap(false, Ordering::Release);
        let tl_x_changed = self.tl.x.has_changed.swap(false, Ordering::Release);
        let tl_y_changed = self.tl.y.has_changed.swap(false, Ordering::Release);

        br_x_changed || br_y_changed || tl_x_changed || tl_y_changed
    }

    pub fn id(&self) -> AreaId {
        self.id
    }

    pub fn is_clustered(&self) -> bool {
        self.lineage
            .as_ref()
            .is_some_and(|lineage| lineage.clustered)
    }

    pub fn aligns_with(&self, axis: Axis) -> bool {
        self.lineage
            .as_ref()
            .is_some_and(|lineage| lineage.axis == axis)
    }

    pub fn children_len(&self) -> usize {
        self.lineage
            .as_ref()
            .map(|lineage| lineage.children.len())
            .unwrap_or(0)
    }

    pub fn axis(&self) -> Option<Axis> {
        self.lineage.as_ref().map(|lineage| lineage.axis)
    }

    /// Sets the [`Equality`]s for the edges of [`self`],
    /// taking into account the potential frame, defined by [`Frame`]
    /// and the [`Rect`]'s position.
    fn set_equalities(
        &mut self,
        parent: &Rect,
        axis: Axis,
        frame: Frame,
        max: Coord,
        edges: &mut Vec<Edge>,
    ) -> (f64, f64) {
        self.edge_equalities.extend([
            self.tl.x.var | GE(REQUIRED) | 0.0,
            self.tl.y.var | GE(REQUIRED) | 0.0,
            self.br.x.var | GE(REQUIRED) | self.tl.x.var,
            self.br.y.var | GE(REQUIRED) | self.tl.y.var,
        ]);

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        let (right, up, left, down) = if self.is_frameable(Some(parent)) {
            self.form_frame(frame, max, edges)
        } else {
            (0.0, 0.0, 0.0, 0.0)
        };

        let (para_left, para_right, start, end) = match axis.perp() {
            Axis::Vertical => (up, down, left, right),
            Axis::Horizontal => (left, right, up, down),
        };

        self.edge_equalities.extend([
            self.start(axis.perp()) | EQ(REQUIRED) | (parent.start(axis.perp()) + para_left),
            (self.end(axis.perp()) + para_right) | EQ(REQUIRED) | parent.end(axis.perp()),
        ]);

        (start, end)
    }

    /// Sets the [`Equality`]s for the main [`Rect`], which
    /// is supposed to be parentless. It takes into account a possible
    /// [`Frame`].
    fn set_main_edges(&mut self, frame: Frame, vars: &mut Vars, edges: &mut Vec<Edge>, max: Coord) {
        let (hor_edge, ver_edge) = if self.is_frameable(None) {
            frame.main_edges()
        } else {
            (0.0, 0.0)
        };

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        if hor_edge == 1.0 {
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Vertical,
                frame,
            ));
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Vertical,
                frame,
            ));
        }
        if ver_edge == 1.0 {
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Horizontal,
                frame,
            ));
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Horizontal,
                frame,
            ));
        }

        self.edge_equalities = vec![
            self.tl.x.var | EQ(REQUIRED) | hor_edge,
            self.tl.y.var | EQ(REQUIRED) | ver_edge,
            self.br.x.var | EQ(REQUIRED) | (max.x as f64 - hor_edge),
            self.br.y.var | EQ(REQUIRED) | (max.y as f64 - ver_edge),
        ];

        vars.add_equalities(&self.edge_equalities);
    }

    /// Wheter or not [`self`] can be framed.
    fn is_frameable(&self, parent: Option<&Rect>) -> bool {
        if parent.is_some_and(|parent| parent.lineage.as_ref().unwrap().clustered) {
            false
        } else if let Some(Lineage { clustered, .. }) = &self.lineage {
            *clustered
        } else {
            true
        }
    }

    /// Forms the frame surrounding [`self`], considering its position
    /// and a [`Frame`].
    fn form_frame(&self, frame: Frame, max: Coord, edges: &mut Vec<Edge>) -> (f64, f64, f64, f64) {
        let (right, up, left, down) = frame.edges(&self.br, &self.tl, max);

        if right == 1.0 {
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Vertical,
                frame,
            ));
        }
        if up == 1.0 {
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Horizontal,
                frame,
            ));
        }
        if left == 1.0 {
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Vertical,
                frame,
            ));
        }
        if down == 1.0 {
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Horizontal,
                frame,
            ));
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

pub struct Rects {
    pub main: Rect,
    floating: Vec<Rect>,
}

impl Rects {
    pub fn new(vars: &mut Vars) -> Self {
        Self {
            main: Rect::new(vars),
            floating: Vec::new(),
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
            } else {
                rect.lineage.as_ref().and_then(|Lineage { children, .. }| {
                    children.iter().find_map(|(child, _)| fetch(child, id))
                })
            }
        }

        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch(rect, id))
    }

    /// Fetches the [`Rect`] which holds the [`Rect`]
    /// of the given index.
    ///
    /// Also returns the child's "position", given an [`Axis`],
    /// going top to bottom or left to right.
    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch_parent(rect, id))
    }

    pub fn get_siblings(&self, id: AreaId) -> Option<&[(Rect, Length)]> {
        self.get_parent(id).and_then(|(_, parent)| {
            parent
                .lineage
                .as_ref()
                .map(|lineage| lineage.children.as_slice())
        })
    }

    /// Fetches the [`RwData<Rect>`] of the given index, if there is
    /// one.
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
        let (pos, parent) = std::iter::once(&mut self.main)
            .chain(&mut self.floating)
            .find_map(|rect| fetch_parent(rect, id))?;
        let id = parent.id;

        let parent = self.get_mut(id)?;

        Some((pos, parent))
    }

    pub fn insert_child(&mut self, pos: usize, id: AreaId, child: Rect) {
        let lineage = self.get_mut(id).unwrap().lineage.as_mut().unwrap();
        lineage.children.insert(pos, (child, Length::default()));
    }

    /// Changes a child's constraint.
    pub fn set_constraint(
        &mut self,
        id: AreaId,
        constraint: Constraint,
        axis: Axis,
        vars: &mut Vars,
    ) -> Option<(Constraint, Axis)> {
        let Some((pos, parent)) = self.get_parent_mut(id) else {
            return None;
        };

        let parent_len = parent.len(axis);
        let lineage = parent.lineage.as_mut().unwrap();

        let (child, length) = &mut lineage.children[pos];
        if let Some((equality, cmp)) = length.constraint.as_mut() {
            if *cmp == (constraint, axis) {
                return Some(*cmp);
            }
            vars.remove_equality(equality);
        }

        let equality = match constraint {
            Constraint::Ratio(den, div) => {
                assert!(den < div, "Constraint::Ratio must be smaller than 1.");
                child.len(axis) | EQ(WEAK * 2.0) | (parent_len * (den as f64 / div as f64))
            }
            Constraint::Percent(percent) => {
                assert!(
                    percent <= 100,
                    "Constraint::Percent must be smaller than 100"
                );
                child.len(axis) | EQ(WEAK * 2.0) | (parent_len * (percent as f64 / 100.0))
            }
            Constraint::Length(len) => child.len(axis) | EQ(STRONG) | len,
            Constraint::Min(min) => child.len(axis) | GE(MEDIUM) | min,
            Constraint::Max(max) => child.len(axis) | LE(MEDIUM) | max,
        };

        vars.add_equality(equality.clone());

        length
            .constraint
            .replace((equality, (constraint, axis)))
            .map(|(_, prev)| prev)
    }

    pub fn take_constraint(&mut self, id: AreaId, vars: &mut Vars) -> Option<(Constraint, Axis)> {
        self.get_parent_mut(id).and_then(|(pos, parent)| {
            let lineage = parent.lineage.as_mut().unwrap();
            let length = std::mem::take(&mut lineage.children[pos].1);
            length.constraint.map(|(equality, constraint)| {
                vars.remove_equality(&equality);
                constraint
            })
        })
    }

    pub fn set_edges(
        &mut self,
        id: AreaId,
        frame: Frame,
        vars: &mut Vars,
        edges: &mut Vec<Edge>,
        max: Coord,
    ) {
        if self.main.id == id {
            let rect = self.get_mut(id).unwrap();
            rect.clear_equalities(vars);
            rect.set_main_edges(frame, vars, edges, max);
        } else {
            let (pos, parent) = self.get_parent_mut(id).unwrap();
            prepare_child(parent, pos, vars, frame, max, edges)
        }
    }

    pub fn set_new_child_edges(
        &mut self,
        id: AreaId,
        frame: Frame,
        vars: &mut Vars,
        edges: &mut Vec<Edge>,
        max: Coord,
    ) {
        let child_is_main = self.main.id == self.get(id).unwrap().id;
        let (pos, parent) = self.get_parent_mut(id).unwrap();
        if child_is_main {
            parent.clear_equalities(vars);
            parent.set_main_edges(frame, vars, edges, max);
        } else {
            prepare_child(parent, pos, vars, frame, max, edges)
        }
    }

    pub fn set_edges_around(
        &mut self,
        id: AreaId,
        frame: Frame,
        vars: &mut Vars,
        edges: &mut Vec<Edge>,
        max: Coord,
    ) {
        let (pos, lineage) = self
            .get_parent(id)
            .and_then(|(pos, parent)| Some(pos).zip(parent.lineage.as_ref()))
            .unwrap();

        let ids: Vec<_> = lineage.children.iter().map(|(child, _)| child.id).collect();

        for &pos in [pos.checked_sub(1), Some(pos), Some(pos + 1)]
            .iter()
            .flatten()
        {
            if let Some(&id) = ids.get(pos) {
                self.set_edges(id, frame, vars, edges, max)
            }
        }
    }
}

fn fetch_parent(main: &Rect, id: AreaId) -> Option<(usize, &Rect)> {
    if main.id == id {
        return None;
    }
    let Some(Lineage { children, .. }) = &main.lineage else {
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
    } else {
        rect.lineage.as_mut().and_then(|Lineage { children, .. }| {
            children
                .iter_mut()
                .find_map(|(child, _)| fetch_mut(child, id))
        })
    }
}

/// Assigns all [`Equality`]s that are appropriate to a
/// given child, with the exception of defined and ratio constraints.
fn prepare_child(
    parent: &mut Rect,
    pos: usize,
    vars: &mut Vars,
    frame: Frame,
    max: Coord,
    edges: &mut Vec<Edge>,
) {
    let Lineage {
        mut children,
        axis,
        clustered,
    } = parent.lineage.take().unwrap();

    parent.lineage = Some(Lineage::new(axis, clustered));
    let child = &mut children[pos].0;

    child.clear_equalities(vars);
    let (start, end) = child.set_equalities(parent, axis, frame, max, edges);

    if pos == 0 {
        let constraint = child.start(axis) | EQ(REQUIRED) | (parent.start(axis) + start);
        child.edge_equalities.push(constraint);
    }

    // Previous children carry the `Constraint`s for the `start` of their
    // successors.
    let constraint = if let Some((next, _)) = children.get(pos + 1) {
        (children[pos].0.end(axis) + end) | EQ(REQUIRED) | next.start(axis)
    } else {
        (children[pos].0.end(axis) + end) | EQ(REQUIRED) | parent.end(axis)
    };

    let child = &mut children[pos].0;
    child.edge_equalities.push(constraint);

    vars.add_equalities(&child.edge_equalities);

    if let Some(Lineage { children, .. }) = &mut child.lineage {
        let len = children.len();
        for pos in 0..len {
            prepare_child(child, pos, vars, frame, max, edges);
        }
    }

    parent.lineage = Some(Lineage {
        children,
        axis,
        clustered,
    });
}

/// Sets the ratio [`Equality`]s for the child of the given
/// index. This does include setting the [`Equality`] for
/// the previous child, if there is one.
pub fn set_ratios(parent: &mut Rect, pos: usize, vars: &mut Vars) {
    let &Lineage { axis, .. } = parent.lineage.as_ref().unwrap();
    let Lineage { children, .. } = parent.lineage.as_mut().unwrap();

    let (new, _) = &children[pos];
    let new_len = new.len(axis);
    let new_len_value = new.len_value(axis);

    let prev = children.iter().take(pos);
    let resizable_pos = prev.filter(|(_, length)| length.is_resizable()).count();

    let mut children = children
        .iter_mut()
        .filter(|(_, length)| length.is_resizable());

    if resizable_pos > 0 {
        let (prev, length) = children.nth(resizable_pos - 1).unwrap();

        let ratio = if new_len_value == 0 {
            1.0
        } else {
            prev.len_value(axis) as f64 / new_len_value as f64
        };

        let equality = prev.len(axis) | EQ(WEAK) | (ratio * new_len.clone());
        length.ratio = Some((equality.clone(), ratio));
        vars.add_equality(equality);
    }

    let ratio = children.nth(1).map(|(next, _)| {
        let ratio = if next.len_value(axis) == 0 {
            1.0
        } else {
            new_len_value as f64 / next.len_value(axis) as f64
        };

        let equality = new_len | EQ(WEAK) | (ratio * next.len(axis));
        vars.add_equality(equality.clone());

        (equality, ratio)
    });

    parent.lineage.as_mut().unwrap().children[pos].1.ratio = ratio;
}
