use std::{cell::RefCell, rc::Rc, sync::Arc};

use cassowary::{WeightedRelation::*, strength::STRONG};
use duat_core::{
    text::SpawnId,
    ui::{Axis, PushSpecs, SpawnSpecs},
};

pub use self::rect::{Rect, transfer_vars};
use crate::{AreaId, Equality, Frame, area::PrintInfo, printer::Printer};

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
    pub fn new_layout(&self, fr: Frame, printer: Arc<Printer>, cache: PrintInfo) -> AreaId {
        let layout = Layout::new(fr, printer, cache);
        let main_id = layout.main_id();
        self.0.borrow_mut().list.push(layout);
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
        id: AreaId,
        specs: PushSpecs,
        on_files: bool,
        cache: PrintInfo,
    ) -> Option<(AreaId, Option<AreaId>)> {
        let mut layouts = self.0.borrow_mut();
        Some(
            layouts
                .get_layout_mut(|l| l.get(id).is_some())?
                .push(id, specs, on_files, cache),
        )
    }

    /// Spawnss a new [`Area`] on an existing one, returning its
    /// [`AreaId`]
    ///
    /// May return [`None`] in the situation where the `Area` of the
    /// id was already deleted.
    ///
    /// [`Area`]: crate::area::Area
    pub fn spawn(&self, id: AreaId, specs: SpawnSpecs, cache: PrintInfo) -> Option<AreaId> {
        let mut layouts = self.0.borrow_mut();
        Some(
            layouts
                .get_layout_mut(|l| l.get(id).is_some())?
                .spawn_on_widget(id, specs, cache),
        )
    }

    /// Deletes the [`Area`] of a given id
    ///
    /// Returns [`None`] if it was alread deleted
    ///
    /// [`Area`]: crate::area::Area
    pub fn delete(&self, id: AreaId) -> Option<AreaId> {
        let mut layouts = self.0.borrow_mut();
        if let Some(layout) = layouts.get_layout_mut(|l| l.get(id).is_some()) {
        layout.delete(id)
        } else {
            todo!()
        }
    }

    /// Removes all windows and all spawned widgets
    pub fn reset(&self) {
        let mut inner = self.0.borrow_mut();
        inner.list.clear();
        inner.text_spawned.clear();
        inner.active_id = None;
    }

    /// Removes a window from the list
    pub fn remove_window(&self, win: usize) {
        self.0.borrow_mut().list.remove(win);
    }

    ////////// Querying functions

    pub fn get_rect(&self, id: AreaId) -> Option<&Rect> {
        let layouts = self.0.borrow();
        let layout = layouts.get_layout(|l| l.get(id).is_some())
    }
}

#[derive(Default)]
struct InnerLayouts {
    list: Vec<Layout>,
    text_spawned: Vec<(SpawnId, Rect, Constraints)>,
    active_id: Option<AreaId>,
}

impl InnerLayouts {
    /// Gets the mut [`Layout`] that matches some predicate
    fn get_layout_mut(&mut self, predicate: impl Fn(&&mut Layout) -> bool) -> Option<&mut Layout> {
        self.list.iter_mut().find(predicate)
    }

	/// Gets the [`Layout`] thaat matches some predicate
    fn get_layout(&self, predicate: impl Fn(&&Layout) -> bool) -> Option<&Layout> {
        self.list.iter().find(predicate)
    }
}

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
pub(crate) struct Layout {
    pub main: Rect,
    spawned: Vec<(Rect, Constraints)>,
    pub printer: Arc<Printer>,
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
    pub fn push(
        &mut self,
        id: AreaId,
        specs: PushSpecs,
        on_files: bool,
        info: PrintInfo,
    ) -> (AreaId, Option<AreaId>) {
        let axis = specs.axis();

        let (can_be_sibling, can_be_child) = {
            let parent_is_cluster = self
                .main
                .get_parent(id)
                .map(|(_, parent)| parent.is_clustered())
                .unwrap_or(specs.cluster);
            let target_is_cluster = self.main.get(id).is_some_and(Rect::is_clustered);

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
        let (target, new_parent_id) = if can_be_sibling
            && let Some((_, parent)) = self.main.get_parent_mut(id)
            && parent.aligns_with(axis)
        {
            (id, None)
        // Check if the target has the same `Axis`.
        } else if can_be_child
            && let Some(parent) = self.main.get_mut(id)
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
            self.main
                .new_parent_for(id, axis, &self.printer, specs.cluster, on_files);
            let (_, parent) = self.main.get_parent(id).unwrap();

            (id, Some(parent.id()))
        };

        let new_id = self
            .main
            .push(specs, target, &self.printer, on_files, info);
        (new_id, new_parent_id)
    }

    pub fn delete(&mut self, id: AreaId) -> Option<AreaId> {
        let (mut rect, _, parent_id) = self.main.delete(&self.printer, id)?;
        remove_children(&mut rect, &self.printer);

        self.printer.update(false, false);

        parent_id
    }

    pub fn swap(&mut self, id0: AreaId, id1: AreaId) {
        self.main.swap(&self.printer, id0, id1);
    }

    pub fn reset_eqs(&mut self, target: AreaId) {
        self.main.reset_eqs(&self.printer, target)
    }

    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        self.main.get(id)
    }

    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        self.main.get_parent(id)
    }

    pub fn active_id(&self) -> AreaId {
        self.active_id
    }

    pub(crate) fn spawn_on_widget(
        &mut self,
        id: AreaId,
        specs: SpawnSpecs,
        info: PrintInfo,
    ) -> AreaId {
        self.main.new_spawned(specs, id, &self.printer, info)
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
    hor_eq: Option<Equality>,
    ver_eq: Option<Equality>,
    width: Option<(f32, bool)>,
    height: Option<(f32, bool)>,
    pub(crate) is_hidden: bool,
}

impl Constraints {
    /// Returns a new instance of [`Constraints`]
    ///
    /// Will also add all equalities needed to make this constraint
    /// work.
    ///
    /// This operation can fail if the `parent` in question can't be found in the `main` [`Rect`]
    fn new(
        p: &Printer,
        [width, height]: [Option<f32>; 2],
        is_hidden: bool,
        new: &Rect,
        parent: AreaId,
        main: &Rect,
    ) -> Option<Self> {
        let width = width.zip(Some(false));
        let height = height.zip(Some(false));
        let [ver_eq, hor_eq] = get_eqs([width, height], new, parent, main, is_hidden)?;
        p.add_eqs(ver_eq.clone().into_iter().chain(hor_eq.clone()));

        Some(Self { hor_eq, ver_eq, width, height, is_hidden })
    }

    pub fn replace(mut self, new: f32, axis: Axis) -> (Self, impl Iterator<Item = Equality>) {
        let hor_eq = self.hor_eq.take();
        let ver_eq = self.ver_eq.take();
        // A replacement means manual constraining, which is prioritized.

        match axis {
            Axis::Horizontal => self.width = Some((new, true)),
            Axis::Vertical => self.height = Some((new, true)),
        };
        (self, hor_eq.into_iter().chain(ver_eq))
    }

    /// Reuses [`self`] in order to constrain a new child
    pub fn apply(
        self,
        new: &Rect,
        parent: AreaId,
        rect: &Rect,
    ) -> Option<(Self, impl Iterator<Item = Equality>)> {
        let constraints = [self.width, self.height];
        let [ver_eq, hor_eq] = get_eqs(constraints, new, parent, rect, self.is_hidden)?;
        let new_eqs = ver_eq.clone().into_iter().chain(hor_eq.clone());

        Some((Self { ver_eq, hor_eq, ..self }, new_eqs))
    }

    pub fn get_eqs(&self) -> impl Iterator<Item = Equality> + use<> {
        self.hor_eq.clone().into_iter().chain(self.ver_eq.clone())
    }

    pub fn drain_eqs(&mut self) -> impl Iterator<Item = Equality> {
        self.ver_eq.take().into_iter().chain(self.hor_eq.take())
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

fn get_eqs(
    [width, height]: [Option<(f32, bool)>; 2],
    child: &Rect,
    parent: AreaId,
    main: &Rect,
    is_hidden: bool,
) -> Option<[Option<Equality>; 2]> {
    let is_horizontal = main
                .get(parent)
                .map(|p| p.aligns_with(Axis::Horizontal))?;
        
    if is_hidden {
        if is_horizontal
        {
            return Some([
                Some(child.len(Axis::Horizontal) | EQ(STRONG + 3.0) | 0.0),
                None,
            ]);
        } else {
            return Some([
                None,
                Some(child.len(Axis::Vertical) | EQ(STRONG + 3.0) | 0.0),
            ]);
        }
    }

    Some([(width, Axis::Horizontal), (height, Axis::Vertical)].map(|(constraint, axis)| {
        let (len, is_manual) = constraint?;
        let strength = STRONG + if is_manual { 2.0 } else { 1.0 };
        Some(child.len(axis) | EQ(strength) | len)
    }))
}

fn remove_children(rect: &mut Rect, p: &Printer) {
    for (child, _) in rect.children_mut().into_iter().flat_map(|c| c.iter_mut()) {
        p.remove_rect(child);
        remove_children(child, p);
    }
}
