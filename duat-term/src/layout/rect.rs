use cassowary::{
    strength::{REQUIRED, STRONG, WEAK},
    Expression, Variable,
    WeightedRelation::{EQ, GE},
};
use duat_core::{
    data::RwData,
    ui::{
        Axis::{self, *},
        Constraint, PushSpecs,
    },
};

use super::Constraints;
use crate::{
    area::{Coord, PrintInfo},
    print::{Printer, Sender, VarPoint, VarValue},
    Area, AreaId, Equality, Frame,
};

struct Children {
    list: Vec<(Rect, Constraints)>,
    axis: Axis,
    clustered: bool,
    resizable_size: Variable,
    resizable_len: Variable,
}

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
#[derive(Debug)]
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
    /// Returns a new instance of [`Rect`], already adding its
    /// [`Variable`]s to the list.
    pub fn new_main(p: &mut Printer) -> Self {
        let tl = p.var_point();
        let br = p.var_point();
        let sender = p.sender(tl.clone(), br.clone());

        let eqs = vec![
            tl.x() | EQ(REQUIRED) | 0.0,
            tl.y() | EQ(REQUIRED) | 0.0,
            br.x() | EQ(REQUIRED) | p.max().x(),
            br.y() | EQ(REQUIRED) | p.max().y(),
        ];
        p.add_equalities(&eqs);

        Rect {
            id: AreaId::new(),
            tl,
            br,
            eqs,
            kind: Kind::End(sender, RwData::default()),
            on_files: true,
        }
    }

    /// Returns a new [`Rect`], which is supposed to replace an
    /// existing [`Rect`], as its new parent.
    fn new_raw(p: &mut Printer, on_files: bool) -> Self {
        let tl = p.var_point();
        let br = p.var_point();
        let sender = p.sender(tl.clone(), br.clone());

        let eqs = vec![
            tl.x() | GE(REQUIRED) | 0.0,
            tl.y() | GE(REQUIRED) | 0.0,
            br.x() | GE(REQUIRED) | tl.x(),
            br.x() | GE(REQUIRED) | tl.y(),
        ];

        Rect {
            id: AreaId::new(),
            tl,
            br,
            eqs,
            kind: Kind::End(sender, RwData::default()),
            on_files,
        }
    }

    pub fn set_base_eqs(&mut self, i: usize, parent: &Rect, p: &mut Printer, fr: Frame) {
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
    pub fn new(printer: &mut Printer, fr: Frame) -> Self {
        Self {
            main: Rect::new_main(printer),
            floating: Vec::new(),
            fr,
        }
    }

    pub fn push(&mut self, ps: PushSpecs, id: AreaId, p: &mut Printer, on_files: bool) -> AreaId {
        let fr = self.fr;
        let mut rect = Rect::new_raw(p, on_files);
        let new_id = rect.id();
        let (i, parent, cons) = {
            let (i, parent) = self.get_parent(id).unwrap();
            let cons = Constraints::new(ps, &rect, parent.id(), self, p);
            let parent = self.get_mut(parent.id()).unwrap();

            match ps.comes_earlier() {
                true => (i, parent, cons),
                false => (i + 1, parent, cons),
            }
        };

        rect.set_base_eqs(i, parent, p, fr);
        parent.kind.mut_children().unwrap().insert(i, (rect, cons));

        if i > 0 {
            let (mut prev, cons) = parent.kind.mut_children().unwrap().remove(i - 1);
            prev.set_base_eqs(i - 1, parent, p, fr);
            let entry = (prev, cons);
            parent.kind.mut_children().unwrap().insert(i - 1, entry);
        }

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
        let (child, parent_id) = {
            let target = self.get_mut(id).unwrap();
            let parent = Rect {
                id: AreaId::new(),
                tl: std::mem::replace(&mut target.tl, p.var_point()),
                br: std::mem::replace(&mut target.br, p.var_point()),
                eqs: std::mem::take(&mut target.eqs),
                kind: Kind::middle(axis, cluster),
                on_files,
            };
            let id = parent.id();

            (std::mem::replace(target, parent), id)
        };

        let cons = self
            .get_parent_mut(parent_id)
            .map(|(i, parent)| std::mem::take(&mut parent.kind.mut_children().unwrap()[i].1))
            .map(|cons| cons.repurpose(&child, parent_id, self, p))
            .unwrap_or_default();

        let parent = self.get_mut(parent_id).unwrap();
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
