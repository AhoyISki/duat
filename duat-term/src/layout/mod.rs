use cassowary::{WeightedRelation::*, strength::STRONG};
use duat_core::{
    cfg::PrintCfg,
    data::RwData,
    prelude::Text,
    text::TwoPoints,
    ui::{Axis, Constraint, PushSpecs},
};

pub use self::rect::{Rect, Rects};
use crate::{AreaId, Equality, Frame, area::PrintInfo, print::Printer};

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
    pub printer: RwData<Printer>,
}

impl Layout {
    /// Returns a new instance of [`Layout`], applying a given
    /// [`Frame`] to all inner [`Rect`]s.
    pub fn new(fr: Frame, printer: RwData<Printer>, info: PrintInfo) -> Self {
        printer.write().flush_equalities().unwrap();
        let rects = Rects::new(&mut printer.write(), fr, info);
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
        cluster: bool,
        on_files: bool,
        info: PrintInfo,
    ) -> (AreaId, Option<AreaId>) {
        let mut p = self.printer.write();
        let axis = ps.axis();

        let (can_be_sibling, can_be_child) = {
            let parent_is_cluster = self
                .rects
                .get_parent(id)
                .map(|(_, parent)| parent.is_clustered())
                .unwrap_or(cluster);

            let child_is_cluster = self.rects.get(id).is_some_and(Rect::is_clustered);

            (parent_is_cluster == cluster, child_is_cluster == cluster)
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
                .new_parent_of(id, axis, &mut p, cluster, on_files);
            let (_, parent) = self.rects.get_parent(id).unwrap();

            (id, Some(parent.id()))
        };

        let new_id = self.rects.push(ps, target, &mut p, on_files, info);
        (new_id, new_parent_id)
    }

    pub fn delete(&mut self, id: AreaId) -> Option<AreaId> {
        let mut p = self.printer.write();
        let (rect, _, parent_id) = self.rects.delete(&mut p, id)?;
        rect.set_to_zero();
        remove_children(&rect, &mut p);
        parent_id
    }

    pub fn swap(&mut self, id0: AreaId, id1: AreaId) {
        self.rects.swap(&mut self.printer.write(), id0, id1);
    }

    pub fn reset_eqs(&mut self, target: AreaId) {
        self.rects.reset_eqs(&mut self.printer.write(), target)
    }

    /// The current value for the width of [`self`].
    pub fn width(&self) -> u32 {
        self.rects.main.len_value(Axis::Horizontal)
    }

    /// The current value for the height of [`self`].
    pub fn height(&self) -> u32 {
        self.rects.main.len_value(Axis::Vertical)
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

    pub(crate) fn new_floating(
        &self,
        at: impl TwoPoints,
        specs: PushSpecs,
        text: &Text,
        cfg: PrintCfg,
    ) -> AreaId {
        self.rects
            .new_floating(at, specs, text, cfg, &mut self.printer.write())
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
    ver_eq: Option<Equality>,
    hor_eq: Option<Equality>,
    ver_con: Option<(Constraint, bool)>,
    hor_con: Option<(Constraint, bool)>,
}

impl Constraints {
    /// Returns a new instance of [`Constraints`]
    ///
    /// Will also add all equalities needed to make this constraint
    /// work.
    fn new(ps: PushSpecs, new: &Rect, parent: AreaId, rects: &Rects, p: &mut Printer) -> Self {
        let cons = [ps.ver_constraint(), ps.hor_constraint()].map(|con| con.zip(Some(false)));
        let [ver_eq, hor_eq] = get_eqs(cons, new, parent, rects);
        p.add_eqs([&ver_eq, &hor_eq].into_iter().flatten());

        Self {
            ver_eq,
            hor_eq,
            ver_con: cons[0],
            hor_con: cons[1],
        }
    }

    pub fn replace(mut self, con: Constraint, axis: Axis, p: &mut Printer) -> Self {
        // A replacement means manual constraining, which is prioritized.
        for eq in [self.ver_eq.take(), self.hor_eq.take()]
            .into_iter()
            .flatten()
        {
            p.remove_equality(eq);
        }
        match axis {
            Axis::Vertical => self.ver_con.replace((con, true)),
            Axis::Horizontal => self.hor_con.replace((con, true)),
        };
        self
    }

    /// Reuses [`self`] in order to constrain a new child
    pub fn apply(self, new: &Rect, parent: AreaId, rects: &Rects, p: &mut Printer) -> Self {
        let cons = [self.ver_con, self.hor_con];
        let [ver_eq, hor_eq] = get_eqs(cons, new, parent, rects);
        p.add_eqs([&ver_eq, &hor_eq].into_iter().flatten());

        Self { ver_eq, hor_eq, ..self }
    }

    pub fn remove(&mut self, p: &mut Printer) {
        for eq in [self.ver_eq.take(), self.hor_eq.take()]
            .into_iter()
            .flatten()
        {
            p.remove_equality(eq);
        }
    }

    pub fn on(&self, axis: Axis) -> Option<Constraint> {
        match axis {
            Axis::Vertical => self.ver_con.unzip().0,
            Axis::Horizontal => self.hor_con.unzip().0,
        }
    }

    /// Whether or not [`self`] has flexibility in terms of its
    /// length.
    fn is_resizable_on(&self, axis: Axis) -> bool {
        let con = self.on(axis);
        matches!(con, Some(Constraint::Min(_) | Constraint::Max(_)) | None)
    }
}

fn get_eqs(
    cons: [Option<(Constraint, bool)>; 2],
    new: &Rect,
    parent: AreaId,
    rects: &Rects,
) -> [Option<Equality>; 2] {
    let cons = [(cons[0], Axis::Vertical), (cons[1], Axis::Horizontal)];
    cons.map(|(cons, axis)| {
        cons.map(|(c, is_manual)| {
            let strength = STRONG + if is_manual { 2.0 } else { 1.0 };
            match c {
                Constraint::Ratio(num, den) => {
                    let (_, ancestor) = rects.get_ancestor_on(axis, parent).unwrap();
                    (new.len(axis) * den as f64) | EQ(strength) | (ancestor.len(axis) * num as f64)
                }
                Constraint::Length(len) => new.len(axis) | EQ(strength) | len,
                Constraint::Min(min) => new.len(axis) | GE(strength) | min,
                Constraint::Max(max) => new.len(axis) | LE(strength) | max,
            }
        })
    })
}

fn remove_children(rect: &Rect, p: &mut Printer) {
    for (child, _) in rect.children().iter().flat_map(|c| c.iter()) {
        child.set_to_zero();
        p.take_rect_parts(child);
        remove_children(child, p);
    }
}
