use cassowary::{
    strength::{REQUIRED, WEAK},
    Expression,
    WeightedRelation::{EQ, GE, LE},
};
use duat_core::{
    data::RwData, ui::{
        Axis::{self, *},
        Constraint, PushSpecs,
    }
};

use super::Constraints;
use crate::{
    area::{Coord, PrintInfo},
    print::{Printer, Sender, VarPoint, VarValue},
    Area, AreaId, Equality, Frame,
};

enum Kind {
    End(Sender, RwData<PrintInfo>),
    Middle {
        children: Vec<(Rect, Constraints)>,
        axis: Axis,
        clustered: bool,
    },
}

impl Kind {
    fn end(sender: Sender) -> Self {
        Self::End(sender, RwData::default())
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

    fn mut_children(&mut self) -> Option<&mut Vec<(Rect, Constraints)>> {
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
    /// The index that this [`Rect`] is tied to.
    id: AreaId,
    tl: VarPoint,
    br: VarPoint,
    eqs: Vec<Equality>,
    kind: Kind,
    on_files: bool,
}

impl Rect {
    /// Returns a new [`Rect`], which is supposed to replace an
    /// existing [`Rect`], as its new parent.
    fn new(tl: VarPoint, br: VarPoint, on_files: bool, kind: Kind) -> Self {
        Rect {
            id: AreaId::new(),
            tl,
            br,
            eqs: Vec::new(),
            kind,
            on_files,
        }
    }

    /// Sets the bare minimum equalities for a [`Rect`]
    ///
    /// This only includes equalities necessary so that `self` won't
    /// overlap with any other end [`Rect`]s. This means that
    /// [`Constraint`] and ratio equalities are not added in.
    pub fn set_base_eqs(
        &mut self,
        i: usize,
        parent: &Rect,
        p: &mut Printer,
        fr: Frame,
        is_resizable: bool,
    ) {
        let axis = parent.kind.axis().unwrap();

        self.clear_eqs(p);

        self.eqs.extend([
            self.tl.x() | GE(REQUIRED) | 0.0,
            self.tl.y() | GE(REQUIRED) | 0.0,
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

        if let Some((next, cons)) = children.get(i) {
            let edge = match self.on_files == next.on_files {
                true => fr.border_edge_on(axis),
                false => fr.files_egde_on(axis),
            };

            if edge == 1.0 && !*clustered {
                let frame = p.var_value();
                self.eqs.extend([
                    (self.end(axis) + &frame) | EQ(REQUIRED) | next.start(axis),
                    // Makes the frame have len = 0 when either of its
                    // side widgets have len == 0.
                    &frame | GE(REQUIRED) | 0.0,
                    &frame | LE(REQUIRED) | 1.0,
                    self.len(axis) | GE(REQUIRED) | &frame,
                    next.len(axis) | GE(REQUIRED) | &frame,
                    &frame | EQ(WEAK) | 1.0,
                ]);
            } else {
                self.eqs
                    .push(self.end(axis) | EQ(REQUIRED) | next.start(axis));
            }

            // If possible, try to make both Rects have the same length.
            if is_resizable && next.is_resizable_on(axis, cons) {
                self.eqs.push(self.len(axis) | EQ(WEAK * 2.0) | next.len(axis));
            }
        } else {
            self.eqs
                .push(self.end(axis) | EQ(REQUIRED) | parent.end(axis));
        }

        p.add_equalities(&self.eqs);
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

    pub fn is_resizable_on(&self, axis: Axis, cons: &Constraints) -> bool {
        if let Kind::Middle { children, axis: child_axis, .. } = &self.kind
            && !children.is_empty()
        {
            let mut children = children.iter();
            let ret = if *child_axis == axis {
                children.any(|(child, cons)| child.is_resizable_on(axis, cons))
            } else {
                children.all(|(child, cons)| child.is_resizable_on(axis, cons))
            };
            ret
        } else {
            cons.is_resizable_on(axis)
        }
    }

    pub(crate) fn children(&self) -> Option<&[(Rect, Constraints)]> {
        match &self.kind {
            Kind::End(..) => None,
            Kind::Middle { children, .. } => Some(children),
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
        self.id == other.id
    }
}

#[derive(Debug)]
pub struct Rects {
    pub main: Rect,
    floating: Vec<Rect>,
    fr: Frame,
}

impl Rects {
    pub fn new(p: &mut Printer, fr: Frame) -> Self {
        let (tl, br) = (p.var_point(), p.var_point());
        let kind = Kind::end(p.sender(&tl, &br));
        let mut main = Rect::new(tl, br, true, kind);
        main.eqs.extend([
            main.tl.x() | EQ(REQUIRED) | 0.0,
            main.tl.y() | EQ(REQUIRED) | 0.0,
            main.br.x() | EQ(REQUIRED) | p.max().x(),
            main.br.y() | EQ(REQUIRED) | p.max().y(),
        ]);
        p.add_equalities(&main.eqs);

        Self { main, floating: Vec::new(), fr }
    }

    pub fn push(&mut self, ps: PushSpecs, id: AreaId, p: &mut Printer, on_files: bool) -> AreaId {
        let fr = self.fr;

        let mut rect = {
            let (tl, br) = (p.var_point(), p.var_point());
            let kind = Kind::end(p.sender(&tl, &br));
            Rect::new(tl, br, on_files, kind)
        };
        let new_id = rect.id();

        let (i, parent, cons, axis) = {
            let (i, parent) = self.get_parent(id).unwrap();
            let cons = Constraints::new(ps, &rect, parent.id(), self, p);
            let parent = self.get_mut(parent.id()).unwrap();
            let axis = parent.kind.axis().unwrap();

            match ps.comes_earlier() {
                true => (i, parent, cons, axis),
                false => (i + 1, parent, cons, axis),
            }
        };

        rect.set_base_eqs(i, parent, p, fr, cons.is_resizable_on(axis));

        parent.kind.mut_children().unwrap().insert(i, (rect, cons));

        let (i, (mut rect_to_fix, cons)) = match i == 0 {
            true => (1, parent.kind.mut_children().unwrap().remove(1)),
            false => (i - 1, parent.kind.mut_children().unwrap().remove(i - 1)),
        };
        let is_resizable = rect_to_fix.is_resizable_on(axis, &cons);
        rect_to_fix.set_base_eqs(i, parent, p, fr, is_resizable);
        let entry = (rect_to_fix, cons);
        parent.kind.mut_children().unwrap().insert(i, entry);

        new_id
    }

    pub fn new_parent_of(
        &mut self,
        id: AreaId,
        axis: Axis,
        p: &mut Printer,
        cluster: bool,
        on_files: bool,
    ) {
        let fr = self.fr;

        let (mut child, cons, parent_id) = {
            let (tl, br) = (p.var_point(), p.var_point());
            let kind = Kind::middle(axis, cluster);
            let mut parent = Rect::new(tl, br, on_files, kind);
            let parent_id = parent.id();

            let (target, cons) = if let Some((i, orig)) = self.get_parent_mut(id) {
                let axis = orig.kind.axis().unwrap();
                let (target, cons) = orig.kind.mut_children().unwrap().remove(i);

                let is_resizable = target.is_resizable_on(axis, &cons);
                parent.set_base_eqs(i, orig, p, fr, is_resizable);

                let entry = (parent, Constraints::default());
                orig.kind.mut_children().unwrap().insert(i, entry);

                if i > 0 {
                    let (mut rect, cons) = orig.kind.mut_children().unwrap().remove(i - 1);
                    let is_resizable = rect.is_resizable_on(axis, &cons);
                    rect.set_base_eqs(i - 1, orig, p, fr, is_resizable);
                    let entry = (rect, cons);
                    orig.kind.mut_children().unwrap().insert(i - 1, entry);
                }

                (target, Some(cons))
            } else {
                parent.eqs.extend([
                    parent.tl.x() | EQ(REQUIRED) | 0.0,
                    parent.tl.y() | EQ(REQUIRED) | 0.0,
                    parent.br.x() | EQ(REQUIRED) | p.max().x(),
                    parent.br.y() | EQ(REQUIRED) | p.max().y(),
                ]);
                p.add_equalities(&parent.eqs);
                (std::mem::replace(&mut self.main, parent), None)
            };

            (target, cons, parent_id)
        };

        let cons = cons
            .map(|cons| cons.repurpose(&child, parent_id, self, p))
            .unwrap_or_default();

        let parent = self.get_mut(parent_id).unwrap();

        let is_resizable = child.is_resizable_on(axis, &cons);
        child.set_base_eqs(0, parent, p, fr, is_resizable);

        parent.kind.mut_children().unwrap().push((child, cons));
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

    pub fn insert_child(&mut self, pos: usize, id: AreaId, child: Rect) {
        let parent = self.get_mut(id).unwrap();

        let entry = (child, Constraints::default());
        parent.kind.mut_children().unwrap().insert(pos, entry);
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
    pub fn get_siblings(&self, id: AreaId) -> Option<&[(Rect, Constraints)]> {
        self.get_parent(id).and_then(|(_, p)| p.kind.children())
    }

    pub fn get_constraint_on(&self, id: AreaId, axis: Axis) -> Option<Constraint> {
        self.get_parent(id)
            .and_then(|(pos, parent)| parent.kind.children().unwrap()[pos].1.on(axis))
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

impl std::fmt::Debug for Rect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Rect")
            .field("tl", &self.tl)
            .field("br", &self.br)
            .field("kind", &self.kind)
            .field("on_files", &self.on_files)
            .field("eqs", &self.eqs)
            .finish_non_exhaustive()
    }
}

impl std::fmt::Debug for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::End(..) => f.debug_tuple("End").finish(),
            Kind::Middle { children, axis, clustered } => f
                .debug_struct("Middle")
                .field(
                    "children",
                    &children.iter().map(|(child, _)| child).collect::<Vec<_>>(),
                )
                .field("axis", axis)
                .field("clustered", clustered)
                .finish(),
        }
    }
}
