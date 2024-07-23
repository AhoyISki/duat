mod frame;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use cassowary::{strength::WEAK, Variable, WeightedRelation::*};
use duat_core::{
    data::RwData,
    ui::{Axis, Constraint, PushSpecs},
};

pub use self::frame::{Brush, Edge, EdgeCoords, Frame};
use self::rect::{set_ratios, Rect, Rects};
use crate::{area::Coord, print::Printer, AreaId, Equality};

mod rect;

/// A [`Variable`], attached to its value, which is automatically kept
/// up to date.
#[derive(Clone)]
struct VarValue {
    var: Variable,
    value: Arc<AtomicUsize>,
    has_changed: Arc<AtomicBool>,
}

impl VarValue {
    /// Returns a new instance of [`VarValue`]
    pub fn new() -> Self {
        Self {
            var: Variable::new(),
            value: Arc::new(AtomicUsize::new(0)),
            has_changed: Arc::new(AtomicBool::new(false)),
        }
    }
}

impl PartialEq<VarValue> for VarValue {
    fn eq(&self, other: &VarValue) -> bool {
        self.var == other.var
    }
}

impl PartialOrd<VarValue> for VarValue {
    fn partial_cmp(&self, other: &VarValue) -> Option<std::cmp::Ordering> {
        Some(
            self.value
                .load(Ordering::Acquire)
                .cmp(&other.value.load(Ordering::Acquire)),
        )
    }
}

/// A point on the screen, which can be calculated by [`cassowary`]
/// and interpreted by `duat_term`.
#[derive(Clone, PartialEq, PartialOrd)]
pub struct VarPoint {
    x: VarValue,
    y: VarValue,
}

impl VarPoint {
    /// Returns a new instance of [`VarPoint`]
    pub fn new(printer: &mut Printer) -> Self {
        let element = VarPoint {
            x: VarValue::new(),
            y: VarValue::new(),
        };

        printer.vars_mut().insert(
            element.x.var,
            (element.x.value.clone(), element.x.has_changed.clone()),
        );
        printer.vars_mut().insert(
            element.y.var,
            (element.y.value.clone(), element.y.has_changed.clone()),
        );

        element
    }

    /// Returns a new instance of [`VarPoint`]
    pub fn new_from_hash_map(
        map: &mut HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)>,
    ) -> Self {
        let element = VarPoint {
            x: VarValue::new(),
            y: VarValue::new(),
        };

        map.insert(
            element.x.var,
            (element.x.value.clone(), element.x.has_changed.clone()),
        );
        map.insert(
            element.y.var,
            (element.y.value.clone(), element.y.has_changed.clone()),
        );

        element
    }

    pub fn coord(&self) -> Coord {
        let x = self.x.value.load(Ordering::Relaxed);
        let y = self.y.value.load(Ordering::Relaxed);

        Coord::new(x, y)
    }

    pub fn x_var(&self) -> Variable {
        self.x.var
    }

    pub fn y_var(&self) -> Variable {
        self.y.var
    }
}

impl std::fmt::Debug for VarPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?}: {}, {:?}: {}",
            self.x.var,
            self.x.value.load(Ordering::Relaxed),
            self.y.var,
            self.y.value.load(Ordering::Relaxed)
        ))
    }
}

/// A list of [`Constraint`] for [`Rect`]s to follow.
///
/// These [`Constraint`]s are specifically not related to a [`Rect`]s
/// location, in relation to other [`Rect`]s. They instead deal with
/// two things, affecting a [`Rect`]s lenght in its parent's [`Axis`]:
///
/// - `defined`: A [`Constraint`], provided by the user, which details
///   specific requests for the lenght of a [`Rect`].
/// - `ratio`: A [`Constraint`] which details the ratio between the
///   lenght of this [`Rect`] and the lenght of the [`Rect`] that
///   follows it, if there is any.
///
/// Both of these constraints are optional, and are meant to be
/// replaceable at runtime.
///
/// [`Constraint`]: Equality
#[derive(Default, Debug, Clone)]
pub struct Constraints {
    ver_eq: Option<Equality>,
    hor_eq: Option<Equality>,
    ver_cons: Option<Constraint>,
    hor_cons: Option<Constraint>,
}

impl Constraints {
    /// Wether or not [`self`] has flexibility in terms of its lenght.
    fn is_resizable_on(&self, axis: Axis) -> bool {
        let con = self.on(axis);
        matches!(con, Some(Constraint::Min(_) | Constraint::Max(_)) | None)
    }

    fn on(&self, axis: Axis) -> Option<Constraint> {
        match axis {
            Axis::Vertical => self.ver_cons,
            Axis::Horizontal => self.hor_cons,
        }
    }

    fn on_mut(&mut self, axis: Axis) -> (&mut Option<Equality>, &mut Option<Constraint>) {
        match axis {
            Axis::Vertical => (&mut self.ver_eq, &mut self.ver_cons),
            Axis::Horizontal => (&mut self.hor_eq, &mut self.hor_cons),
        }
    }
}

/// The overrall structure of a window on `duat_term`.
///
/// The [`Layout`] handles all of the [`Rect`]s inside of it,
/// including all of the [`Variable`]s and
/// [`Constraint`][Equality]s that define said [`Rect`]s.
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
    frame: Frame,
    edges: Vec<Edge>,
    pub printer: RwData<Printer>,
}

impl Layout {
    /// Returns a new instance of [`Layout`], applying a given
    /// [`Frame`] to all inner [`Rect`]s.
    pub fn new(frame: Frame, printer: RwData<Printer>) -> Self {
        let rects = Rects::new(&mut printer.write());
        let main_id = rects.main.id();

        let mut layout = Layout {
            rects,
            active_id: main_id,
            frame,
            edges: Vec::new(),
            printer,
        };

        let (mut printer, edges) = (layout.printer.write(), &mut layout.edges);

        layout
            .rects
            .set_edges(main_id, Frame::Empty, &mut printer, edges);

        printer.update(false);

        layout.rects.main.clear_equalities(&mut printer);
        layout
            .rects
            .set_edges(main_id, layout.frame, &mut printer, edges);

        printer.update(false);

        layout.rects.set_senders(&mut printer);
        drop(printer);

        layout
    }

    pub fn resize(&mut self) {
        let mut printer = self.printer.write();
        printer.update(true);
        self.rects.set_senders(&mut printer);
    }

    /// The index of the main [`Rect`], which holds all (non floating)
    /// others.
    pub fn main_index(&self) -> AreaId {
        self.rects.main.id()
    }

    /// Bisects a given [`Rect`] into two [`Rect`]s, returning the
    /// index of the new one.
    ///
    /// This bisection will sometimes cause the creation of a new
    /// parent to hold the bisected [`Rect`] and its new sibling. In
    /// these cases, an aditional index associated with the parent
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
        specs: PushSpecs,
        do_group: bool,
    ) -> (AreaId, Option<AreaId>) {
        let mut printer = self.printer.write();
        let (printer, edges) = (&mut printer, &mut self.edges);
        let axis = specs.axis();

        let (can_be_sibling, can_be_child) = {
            let parent_is_grouped = self
                .rects
                .get_parent(id)
                .map(|(_, parent)| parent.is_grouped())
                .unwrap_or(do_group);

            let child_is_grouped = self.rects.get(id).is_some_and(Rect::is_grouped);

            (parent_is_grouped == do_group, child_is_grouped == do_group)
        };

        // Check if the target's parent has the same `Axis`.
        let (parent, pos, new_parent_id) = if can_be_sibling
            && let Some((pos, parent)) = self.rects.get_parent_mut(id)
            && parent.aligns_with(axis)
        {
            let pos = match specs.comes_earlier() {
                true => pos,
                false => pos + 1,
            };

            (parent, pos, None)
        // Check if the target has the same `Axis`.
        } else if can_be_child
            && let Some(parent) = self.rects.get_mut(id)
            && parent.aligns_with(axis)
        {
            let index = match specs.comes_earlier() {
                true => 0,
                false => parent.children_len(),
            };

            (parent, index, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            let (new_parent_id, child) = {
                let rect = self.rects.get_mut(id).unwrap();
                let parent = Rect::new_parent_of(rect, axis, printer, do_group);

                (parent.id(), std::mem::replace(rect, parent))
            };
            let child_id = child.id();

            let pos = match specs.comes_earlier() {
                true => 0,
                false => 1,
            };

            let (ver, hor) = self.rects.take_constraints(new_parent_id, printer);

            self.rects.insert_child(0, new_parent_id, child);

            if let Some(cons) = ver {
                self.rects.set_ver_constraint(child_id, cons, printer, WEAK);
            }
            if let Some(cons) = hor {
                self.rects.set_ver_constraint(child_id, cons, printer, WEAK);
            }

            // If the child is grouped, the frame doesn't need to be redone.
            if !do_group {
                self.rects
                    .set_new_child_edges(child_id, self.frame, printer, edges)
            }

            let rect = self.rects.get_mut(new_parent_id).unwrap();
            (rect, pos, Some(new_parent_id))
        };

        let parent_id = parent.id();
        printer.update(false);

        let (temp_eq, new_id) = {
            let new = Rect::new(printer);
            let new_id = new.id();

            self.rects.insert_child(pos, parent_id, new);

            if let Some(cons) = specs.ver_constraint() {
                self.rects.set_ver_constraint(new_id, cons, printer, WEAK);
            }

            if let Some(cons) = specs.hor_constraint() {
                self.rects.set_hor_constraint(new_id, cons, printer, WEAK);
            }

            // We initially set the frame to `Frame::Empty`, since that will make
            // the `Rect`s take their "full space". What this means is that, when
            // we calculate the real frame, using `self.frame`, we know for a fact
            // which `Rect`s reach the edge of the screen. Unfortunately, this
            // does mean that we have to run `prepare_child()` twice for each
            // affected area.
            self.rects
                .set_edges_around(new_id, Frame::Empty, printer, edges);

            // Add a constraint so that the new child `Rect` has a len equal to
            // `resizable_len / resizable_children`, a self imposed rule.
            // This will be useful later, when adding new ratio constraints to the
            // new child.
            let constraint = specs.ver_constraint();
            if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = constraint {
                let children = self.rects.get_siblings(new_id).unwrap();
                let axis = specs.axis();

                let (res_count, res_len) = children
                    .iter()
                    .filter(|(_, length)| length.is_resizable_on(axis))
                    .fold((0, 0), |(count, len), (child, _)| {
                        (count + 1, len + child.len_value(axis))
                    });

                let temp_eq = {
                    let (new, _) = &children[pos];
                    new.len(axis) | EQ(WEAK * 2.0) | (res_len as f64 / res_count as f64)
                };

                printer.add_equality(temp_eq.clone()).unwrap();

                (Some(temp_eq), new_id)
            } else {
                (None, new_id)
            }
        };

        printer.update(false);

        let parent = self.rects.get_mut(parent_id).unwrap();
        if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = specs.ver_constraint() {
            set_ratios(parent, pos, printer);
        }

        if let Some(temp_eq) = temp_eq {
            printer.remove_equality(&temp_eq).unwrap();
        }

        printer.update(false);

        self.rects
            .set_edges_around(new_id, self.frame, printer, edges);

        printer.update(false);

        (new_id, new_parent_id)
    }

    /// The current value for the width of [`self`].
    pub fn width(&self) -> usize {
        self.rects.main.len_value(Axis::Horizontal)
    }

    /// The current value for the height of [`self`].
    pub fn height(&self) -> usize {
        self.rects.main.len_value(Axis::Vertical)
    }

    /// The [`Edge`]s that are supposed to be printed to the screen.
    pub fn edges(&self) -> &[Edge] {
        self.edges.as_ref()
    }

    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        self.rects.get(id)
    }

    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        self.rects.get_parent(id)
    }
}
