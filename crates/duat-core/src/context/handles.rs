//! Widget handles for Duat.
//!
//! These are used pretty much everywhere, and are essentially just an
//! [`RwData<W>`] conjoined with an [`Area`].
use std::{
    any::Any,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use crate::{
    Ns,
    buffer::{Buffer, Change},
    context,
    data::{HandleFns, Pass, RwData, RwText, WriteableTuple},
    mode::{ModSelection, SelectionMut, Selections},
    opts::PrintOpts,
    text::{Spawn, Text, TextIndex, TextMut, TextParts, TwoPoints},
    ui::{Area, DynSpawnSpecs, PushSpecs, RwArea, SpawnId, Widget, Window},
};

/// A handle to a [`Widget`] in Duat.
///
/// The [`Handle`] lets you do all sorts of edits on a [`Widget`]. You
/// can, for example, make use of the [`Selection`]s in its [`Text`]
/// in order to edit the [`Text`] in a very declarative way.
///
/// One of the places where this is commonly done is within [`Mode`]s,
/// which provide a unified interface for handling keys:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use duat::prelude::*;
///
/// /// A very basic example Mode.
/// #[derive(Clone)]
/// struct PlacesCharactersAndMoves;
///
/// impl Mode for PlacesCharactersAndMoves {
///     fn send_key(&mut self, _: &mut Pass, _: KeyEvent) {
///         todo!();
///     }
/// }
/// ```
///
/// In order to modify the widget, you must implement the
/// [`Mode::send_key`] method. In it, you receive the following:
///
/// - A [`&mut Pass`], which will give you access to all of duat's
///   shared state;
/// - The [key] that was sent, may be a [mapped] key.
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use duat::prelude::*;
///
/// #[derive(Clone)]
/// struct PlacesCharactersAndMoves;
/// impl Mode for PlacesCharactersAndMoves {
///     fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent) {
///         match key_event {
///             // actions based on the key pressed
///             unmod!(KeyCode::Char(char)) => {
///                 // Do something when the character 's' is typed.
///             }
///             _ => todo!("The remaining keys"),
///         }
///     }
/// }
/// ```
///
/// Note the [`unmod!`] macro. It (alongside [`alt!`], [`ctrl!`] and
/// [`shift!`]) can be used to easily create [`KeyEvent`]s for
/// matching purposes. They are very useful for succinctly describing
/// an exact match in just a short pattern:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// use KeyCode::*;
/// use duat::prelude::*;
///
/// let key_event = KeyEvent::from(Char('a'));
/// match key_event {
///     unmod!('a' | 'b') => { /* .. */ }
///     shift!(Right | Left) => { /* .. */ }
///     ctrl!(alt!('d')) => { /* .. */ }
///     _ => { /* .. */ }
/// }
/// ```
///
/// With the [`Handle`], you can modify [`Text`] in a simplified
/// way. This is done by two actions, [editing] and [moving]. You
/// can only do one of these on any number of selections at the same
/// time.
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat::prelude::*;
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl Mode for PlacesCharactersAndMoves {
///     fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent) {
///         use KeyCode::*;
///
///         let buffer = context::current_buffer(pa);
///
///         match key_event {
///             unmod!(Char(char)) => buffer.edit_all(pa, |mut s| {
///                 s.insert('s');
///                 s.move_hor(1);
///             }),
///             shift!(Right) => buffer.edit_all(pa, |mut s| {
///                 if s.anchor().is_none() {
///                     s.set_anchor();
///                 }
///                 s.move_hor(1);
///             }),
///             unmod!(KeyCode::Right) => buffer.edit_all(pa, |mut s| {
///                 s.unset_anchor();
///                 s.move_hor(1);
///             }),
///             _ => todo!("Predictable remaining implementations"),
///         }
///     }
/// # }
/// ```
///
/// [`Selection`]: crate::mode::Selection
/// [`Mode`]: crate::mode::Mode
/// [`&mut Pass`]: Pass
/// [`PromptLine`]: https://docs.rs/duat/latest/duat/widgets/struct.PromptLine.html
/// [`Mode::send_key`]: crate::mode::Mode::send_key
/// [key]: crate::mode::KeyEvent
/// [mapped]: crate::mode::map
/// [`read`]: RwData::read
/// [`write`]: RwData::write
/// [`Some(selections)`]: Some
/// [`Area`]: crate::ui::Area
/// [commands]: crate::cmd
/// [`KeyEvent`]: crate::mode::KeyEvent
/// [editing]: SelectionMut
/// [moving]: SelectionMut
/// [`Mode`]: crate::mode::Mode
/// [`unmod!`]: crate::mode::unmod
/// [`alt!`]: crate::mode::alt
/// [`ctrl!`]: crate::mode::ctrl
/// [`shift!`]: crate::mode::shift
pub struct Handle<W: ?Sized = dyn Widget> {
    widget: RwData<W>,
    pub(crate) area: RwArea,
    related: RwData<Vec<(Handle, WidgetRelation)>>,
    is_closed: Arc<AtomicBool>,
    pub(crate) update_requested: Arc<AtomicBool>,
    spawn_id: Option<SpawnId>,
    sized: Arc<dyn Any + Send + Sync>,
    fns: &'static HandleFns,
}

impl<W: Widget> Handle<W> {
    /// Returns a new instance of a [`Handle<W, U>`].
    pub(crate) fn new(
        widget: RwData<W>,
        area: RwArea,
        main: Option<Handle>,
        is_closed: Arc<AtomicBool>,
        spawn_id: Option<SpawnId>,
    ) -> Self {
        Self {
            widget: widget.clone(),
            area,
            related: RwData::new(
                main.map(|handle| (handle, WidgetRelation::Main))
                    .into_iter()
                    .collect(),
            ),
            is_closed,
            update_requested: Arc::new(AtomicBool::new(false)),
            spawn_id,
            sized: Arc::new(widget),
            fns: &HandleFns {
                text: |widget, pa| {
                    let widget = unsafe {
                        (widget as *const (dyn Any + Send + 'static) as *const RwData<W>)
                            .as_ref_unchecked()
                    };
                    W::text(widget, pa)
                },
                text_mut: |widget, pa| {
                    let widget = unsafe {
                        (widget as *const (dyn Any + Send + 'static) as *const RwData<W>)
                            .as_ref_unchecked()
                    };
                    W::text_mut(widget, pa)
                },
            },
        }
    }
}

impl<W: 'static> Handle<W> {
    /// Writes to the [`Widget`], making use of a [`Pass`].
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    ///
    /// [`Area`]: crate::ui::Area
    pub fn write<'a>(&'a self, pa: &'a mut Pass) -> &'a mut W {
        self.widget.write(pa)
    }

    /// Writes to the [`Widget`] and [`Area`], making use of a
    /// [`Pass`].
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    ///
    /// [`Area`]: crate::ui::Area
    pub fn write_with_area<'p>(&'p self, pa: &'p mut Pass) -> (&'p mut W, &'p mut Area) {
        pa.write_many((&self.widget, &self.area.0))
    }

    /// The same as [`RwData::write_then`].
    ///
    /// This lets you write to a [`Widget`] and other [`RwData`]-like
    /// structs within said `Widget` at the same time.
    pub fn write_then<'p, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p self,
        pa: &'p mut Pass,
        tup_fn: impl FnOnce(&'p W) -> Tup,
    ) -> Tup::Return {
        self.widget.write_then(pa, tup_fn)
    }

    /// Like [`Handle::write_then`], but may return [`None`].
    pub fn write_then_try<'p, Tup: WriteableTuple<'p, impl std::any::Any>>(
        &'p self,
        pa: &'p mut Pass,
        tup_fn: impl FnOnce(&'p W) -> Option<Tup>,
    ) -> Option<Tup::Return> {
        self.widget.write_then_try(pa, tup_fn)
    }
}

impl<W: 'static + ?Sized> Handle<W> {
    ////////// Read and write access functions

    /// Reads from the [`Widget`], making use of a [`Pass`].
    ///
    /// The consistent use of a [`Pass`] for the purposes of
    /// reading/writing to the values of [`RwData`]s ensures that no
    /// panic or invalid borrow happens at runtime, even while working
    /// with untrusted code. More importantly, Duat uses these
    /// guarantees in order to give the end user a ridiculous amount
    /// of freedom in where they can do things, whilst keeping Rust's
    /// number one rule and ensuring thread safety, even with a
    /// relatively large amount of shareable state.
    ///
    /// [`Area`]: crate::ui::Area
    pub fn read<'a>(&'a self, pa: &'a Pass) -> &'a W {
        self.widget.read(pa)
    }

    /// Tries to read as a concrete [`Widget`] implementor.
    pub fn read_as<'a, W2: Widget>(&'a self, pa: &'a Pass) -> Option<&'a W2> {
        self.widget.read_as(pa)
    }

    /// Declares the [`Widget`] within as read.
    ///
    /// Same as calling `handle.widget().declare_as_read()`. You
    /// should use this function if you want to signal to others that
    /// the widget was read, even if you don't have access to a
    /// [`Pass`].
    pub fn declare_as_read(&self) {
        self.widget.declare_as_read();
    }

    /// Declares the [`Widget`] within as written.
    ///
    /// Same as calling `handle.widget().declare_written()`. You
    /// should use this function if you want to signal to others that
    /// the widget was written to, even if you don't have access to a
    /// [`Pass`].
    pub fn declare_written(&self) {
        self.widget.declare_written();
    }

    /// Tries to downcast from `dyn Widget` to a concrete [`Widget`].
    pub fn get_as<W2: Widget>(&self) -> Option<Handle<W2>> {
        Some(Handle {
            widget: self.widget.try_downcast()?,
            area: self.area.clone(),
            related: self.related.clone(),
            is_closed: self.is_closed.clone(),
            update_requested: self.update_requested.clone(),
            spawn_id: self.spawn_id,
            sized: self.sized.clone(),
            fns: self.fns,
        })
    }

    ////////// Querying functions

    /// This [`Handle`]'s [`Widget`].
    pub fn widget(&self) -> &RwData<W> {
        &self.widget
    }

    /// This [`Handle`]'s [`RwArea`]
    pub fn area(&self) -> &RwArea {
        &self.area
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or `write`.
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// [`write`]: RwData::write
    /// [`write_as`]: RwData::write_as
    /// [`read`]: RwData::read
    /// [`has_changed`]: RwData::has_changed
    pub fn has_changed(&self, pa: &Pass) -> bool {
        self.widget.has_changed() || self.area.has_changed(pa)
    }

    /// Wether the [`RwData`] within and another point to the same
    /// value.
    pub fn ptr_eq<T: ?Sized>(&self, other: &RwData<T>) -> bool {
        self.widget.ptr_eq(other)
    }

    /// Request that this [`Handle`] be updated.
    ///
    /// You can use this to request updates from other threads.
    pub fn request_update(&self) {
        self.update_requested.store(true, Ordering::Relaxed);
    }

    ////////// Related Handles

    /// Returns the [`Handle`] this one was pushed to, if it was
    /// pushed to another.
    ///
    /// Will return [`Some`] if this `self` was created by calling
    /// [`Handle::push_outer_widget`], [`Handle::push_inner_widget`],
    /// [`Handle::spawn_on_widget`], or [`Handle::spawn_on_text`].
    pub fn master(&self, pa: &Pass) -> Option<Handle> {
        self.related.read(pa).iter().find_map(|(handle, relation)| {
            (*relation == WidgetRelation::Main).then_some(handle.clone())
        })
    }

    /// Returns the [`Handle<Buffer>`] this one was pushed to, if it
    /// was pushed to one.
    ///
    /// Will return [`Some`] if this `self` was created by calling
    /// [`Handle::push_outer_widget`], [`Handle::push_inner_widget`],
    /// [`Handle::spawn_on_widget`], or [`Handle::spawn_on_text`].
    pub fn master_buffer(&self, pa: &Pass) -> Option<Handle<Buffer>> {
        self.related.read(pa).iter().find_map(|(handle, relation)| {
            handle
                .get_as()
                .filter(|_| *relation == WidgetRelation::Main)
        })
    }

    /// Returns a [`SpawnId`], if this `Handle` was spawned.
    ///
    /// You can use this if, for example, you want to compare a
    /// `Handle` with a specific [`TagPart`], when calling
    /// [`tags.remove_if`].
    ///
    /// [`TagPart`]: crate::text::TagPart
    /// [`tags.remove_if`]: crate::text::Tags::remove_if
    pub fn spawn_id(&self) -> Option<SpawnId> {
        self.spawn_id
    }

    /// Reads related [`Widget`]s of type `W2`, as well as its
    /// [`Area`].
    ///
    /// This can also be done by calling [`Handle::get_related`], and
    /// [`Handle::read`], but this function should generally be
    /// faster, since there is no cloning of [`Arc`]s going on.
    pub fn read_related<'a, W2: Widget>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = (&'a W2, &'a Area, WidgetRelation)> {
        self.read_as(pa)
            .map(|w| (w, self.area().read(pa), WidgetRelation::Main))
            .into_iter()
            .chain(self.related.read(pa).iter().filter_map(|(handle, rel)| {
                handle
                    .read_as(pa)
                    .map(|w| (w, handle.area().read(pa), *rel))
            }))
    }

    /// Gets related [`Handle`]s of type [`Widget`].
    ///
    /// If you are doing this just to read the [`Widget`] and
    /// [`Area`], consider using [`Handle::read_related`].
    pub fn get_related<'a, W2: Widget>(
        &'a self,
        pa: &'a Pass,
    ) -> Vec<(Handle<W2>, WidgetRelation)> {
        self.related
            .read(pa)
            .iter()
            .filter_map(|(handle, rel)| handle.get_as().zip(Some(*rel)))
            .collect()
    }

    /// The [`Window`] this `Handle` belongs to.
    ///
    /// Returns [`None`] if the [`Widget`] is [closed].
    ///
    /// [closed]: Self::is_closed
    #[track_caller]
    pub fn window(&self, pa: &Pass) -> Option<Window> {
        context::windows()
            .iter(pa)
            .find(|window| window.handles(pa).any(|handle| handle == self))
            .cloned()
    }

    /// Raw access to the related widgets.
    pub(crate) fn related(&self) -> &RwData<Vec<(Handle, WidgetRelation)>> {
        &self.related
    }

    ////////// Other methods

    /// Wether this `Handle` was already closed.
    pub fn is_closed(&self) -> bool {
        self.is_closed.load(Ordering::Relaxed)
    }

    /// Declares that this `Handle` has been closed.
    pub(crate) fn declare_closed(&self) {
        self.is_closed.store(true, Ordering::Relaxed);
    }

    /// Sets this [`Handle`] as "active".
    ///
    /// This has a few effects, but the most prominent ones is that
    /// [aliases] will show up on this widget's [`Text`] (if it has
    /// selections), and [`Completions`] will also be spawned on said
    /// `Text` (again, with selections).
    ///
    /// This function will fail if the `Self` [was already closed].
    ///
    /// [aliases]: crate::mode::alias
    /// [`Completions`]: https://docs.rs/crates/duat/latest/widgets/struct.Completions.html
    /// [was already closed]: Self::is_closed
    #[track_caller]
    pub fn set_as_active(&self, pa: &mut Pass) {
        if let Some((_, node)) = {
            context::windows()
                .entries(pa)
                .find(|(_, node)| node.handle() == self)
        } {
            let node = node.clone();
            context::set_current_node(pa, node);
        } else {
            context::error!("Tried setting a [a]closed[] Handle as active");
        }
    }
}

impl<W: Widget + ?Sized> Handle<W> {
    ////////// Refined access functions

    /// A shared reference to the [`Text`] of the [`Widget`].
    ///
    /// This is the same as calling `handle.read(pa).text()`.
    pub fn text<'p>(&'p self, pa: &'p Pass) -> &'p Text {
        (self.fns.text)(self.sized.as_ref(), pa)
    }

    /// A mutable reference to the [`Text`] of the [`Widget`].
    ///
    /// This is the same as calling `handle.write(pa).text_mut()`.
    pub fn text_mut<'p>(&'p self, pa: &'p mut Pass) -> TextMut<'p> {
        (self.fns.text_mut)(self.sized.as_ref(), pa)
    }

    /// The [`TextParts`] of the [`Widget`].
    ///
    /// You can use this in order to get a shared reference to the
    /// [`Strs`] and [`Selections`], while maintaining a mutable
    /// reference to the [`Tags`] of the [`Text`], letting you place
    /// [`Tag`]s while still reading other information from the
    /// [`Widget`].
    ///
    /// This is the same as calling `handle.text_mut().parts()`.
    ///
    /// [`Strs`]: crate::text::Strs
    /// [`Tags`]: crate::text::Tags
    /// [`Tag`]: crate::text::Tag
    pub fn text_parts<'p>(&'p self, pa: &'p mut Pass) -> TextParts<'p> {
        self.text_mut(pa).parts()
    }

    /// A shared reference to the [`Selections`] of the [`Widget`]'s
    /// [`Text`].
    ///
    /// This is the same as calling `handle.read(pa).selections()`.
    pub fn selections<'p>(&'p self, pa: &'p Pass) -> &'p Selections {
        self.text(pa).selections()
    }

    /// Removes all [`Selection`]s that aren't the main one.
    ///
    /// [`Selection`]: crate::mode::Selection
    pub fn remove_extra_selections(&self, pa: &mut Pass) {
        self.text_mut(pa).remove_extra_selections();
    }

    /// Rotates the index of the main [`Selection`].
    ///
    /// [`Selection`]: crate::mode::Selection
    pub fn rotate_main_selection(&self, pa: &mut Pass, amount: i32) {
        self.text_mut(pa).rotate_main_selection(amount);
    }

    /// Sets the index of the main [`Selection`].
    ///
    /// [`Selection`]: crate::mode::Selection
    pub fn set_main_selection(&self, pa: &mut Pass, n: usize) {
        self.text_mut(pa).set_main_selection(n);
    }

    /// Replace the [`Selections`] with a previously saved version.
    ///
    /// This will also correct all wrong [`Selection`]s.
    ///
    /// [`Selection`]: crate::mode::Selection
    pub fn replace_selections(&self, pa: &mut Pass, saved: Selections) {
        self.text_mut(pa).replace_selections(saved);
    }

    /// Writes to the [`Text`] and [`Area`] at the same time.
    ///
    /// This function is especially useful if you have a `Handle<dyn
    /// Widget>`, since you can't [write] to it directly.
    ///
    /// A lot of the time, you will also need the [`PrintOpts`] of the
    /// widget. To get all three things, you can do the following.
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::prelude::*;
    ///
    /// # fn test(handle: &Handle, pa: &mut Pass) {
    /// let popts = handle.read(pa).print_opts();
    /// let (mut text, area) = handle.write_text_and_area(pa);
    /// # }
    /// ```
    ///
    /// [write]: RwData::write
    pub fn write_text_and_area<'p>(&'p self, pa: &'p mut Pass) -> (TextMut<'p>, &'p mut Area) {
        pa.write_many((self.rw_text(), &self.area))
    }

    /// Returns a read-write handle (similar to [`RwData`]) for this
    /// [`Widget`]'s [`Text`].
    ///
    /// The only reason you'd want to use this is in order to pass
    /// this as an argument to [`Pass::write_many`], so you can get
    /// mutable access to the `Text` of a `Widget` alongside other
    /// things.
    pub fn rw_text(&self) -> RwText<'_> {
        RwText::new(&self.widget, self.sized.as_ref(), self.fns)
    }

    ////////// Selection Editing functions

    /// Edits the nth [`Selection`] in the [`Text`].
    ///
    /// Once dropped, the [`Selection`] in this [`SelectionMut`] will
    /// be added back to the list of [`Selection`]s, unless it is
    /// [destroyed].
    ///
    /// If you want to edit on the main selection, see [`edit_main`],
    /// if you want to edit on all [`Selection`]s, see [`edit_all`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Selections`], so if there are no [`Selection`]s, it will
    /// create one at [`Point::default`].
    ///
    /// [`Selection`]: crate::mode::Selection
    /// [destroyed]: SelectionMut::destroy
    /// [`edit_main`]: Self::edit_main
    /// [`edit_all`]: Self::edit_all
    /// [`Point::default`]: crate::text::Point::default
    #[track_caller]
    pub fn edit_nth<Ret>(
        &self,
        pa: &mut Pass,
        n: usize,
        edit: impl FnOnce(SelectionMut) -> Ret,
    ) -> Ret {
        let popts = self.widget.read(pa).print_opts();
        let (mut text, area) = self.text_and_area(pa);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let mut selections = {
                let selections = populate(&mut text);

                let Some((selection, was_main)) = selections.remove(n) else {
                    panic!("Selection index {n} out of bounds");
                };

                vec![Some(ModSelection::new(selection, n, was_main))]
            };

            let ret = edit(SelectionMut::new(
                &mut selections,
                0,
                (&mut text, area, popts),
                None,
            ));

            crate::mode::reinsert_selections(selections.into_iter().flatten(), &mut text, None);

            ret
        }));

        text.selections_mut().increment_version();

        match result {
            Ok(ret) => ret,
            Err(panic) => {
                text.selections_mut().populate();
                text.selections_mut().reset();
                std::panic::resume_unwind(panic);
            }
        }
    }

    /// Edits the main [`Selection`] in the [`Text`].
    ///
    /// Once dropped, the `Selection` in this [`SelectionMut`] will be
    /// added back to the list of `Selection`s, unless it is
    /// [destroyed].
    ///
    /// If you want to edit on the `nth` selection, see [`edit_nth`],
    /// same for [`edit_last`], if you want to edit on many
    /// `Selection`s, see [`edit_all`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// `Selections`, so if there are no `Selection`s, it will
    /// create one at [`Point::default`].
    ///
    /// [`Selection`]: crate::mode::Selection
    /// [destroyed]: SelectionMut::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_last`]: Self::edit_last
    /// [`edit_all`]: Self::edit_all
    /// [`Point::default`]: crate::text::Point::default
    pub fn edit_main<Ret>(&self, pa: &mut Pass, edit: impl FnOnce(SelectionMut) -> Ret) -> Ret {
        self.edit_nth(pa, self.selections(pa).main_index(), edit)
    }

    /// Edits the last [`Selection`] in the [`Text`].
    ///
    /// Once dropped, the `Selection` in this [`SelectionMut`] will be
    /// added back to the list of `Selection`s, unless it is
    /// [destroyed].
    ///
    /// If you want to edit on the `nth` selection, see [`edit_nth`],
    /// same for [`edit_main`], if you want to edit on all
    /// `Selection`s, see [`edit_all`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// `Selections`, so if there are no `Selection`s, it will
    /// create one at [`Point::default`].
    ///
    /// [`Selection`]: crate::mode::Selection
    /// [destroyed]: SelectionMut::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_main`]: Self::edit_main
    /// [`edit_all`]: Self::edit_all
    /// [`Point::default`]: crate::text::Point::default
    pub fn edit_last<Ret>(&self, pa: &mut Pass, edit: impl FnOnce(SelectionMut) -> Ret) -> Ret {
        let text = (self.fns.text)(self.sized.as_ref(), pa);
        let len = text.selections().len();
        self.edit_nth(pa, len.saturating_sub(1), edit)
    }

    /// A shortcut for iterating over all selections.
    ///
    /// But it can't return a value, and is meant to reduce the
    /// indentation that will inevitably come from using the
    /// equivalent long form call.
    pub fn edit_all(&self, pa: &mut Pass, edit: impl FnMut(SelectionMut)) {
        let popts = self.widget.read(pa).print_opts();
        let (mut text, area) = self.text_and_area(pa);
        populate(&mut text);
        crate::mode::on_each_sel(text, area, popts, edit);
    }

    ////////// Area functions

    /// Scrolls the [`Text`] veritcally by an amount.
    ///
    /// If [`PrintOpts::allow_overscroll`] is set, then the [`Text`]
    /// will be allowed to scroll beyond the last line, up until
    /// reaching the `scrolloff.y` value.
    ///
    /// NOTE: `f32::MAX as usize != usize::MAX`, so use a very large
    /// number, like a billion, instead of `f32::MAX`.
    pub fn scroll_ver(&self, pa: &mut Pass, dist: f32) {
        let popts = self.widget.read(pa).print_opts();
        let (text, area) = self.text_and_area(pa);
        area.scroll_ver(&text, dist, popts);
        self.widget.declare_written();
    }

    /// Scrolls the [`Text`] to the visual line of a [`TwoPoints`].
    ///
    /// If [`PrintOpts::allow_overscroll`] is set, then the [`Text`]
    /// will be allowed to scroll beyond the last line, up until
    /// reaching the `scrolloff.y` value.
    pub fn scroll_to(&self, pa: &mut Pass, points: TwoPoints, dist: f32) {
        let popts = self.widget.read(pa).print_opts();
        let (text, area) = self.text_and_area(pa);
        area.scroll_to(&text, points, dist, popts);
        self.widget.declare_written();
    }

    /// The start points that should be printed.
    pub fn start_points(&self, pa: &Pass) -> TwoPoints {
        let popts = self.widget.read(pa).print_opts();
        let text = (self.fns.text)(self.sized.as_ref(), pa);
        self.area.start_points(pa, text, popts)
    }

    /// The end points that should be printed.
    pub fn end_points(&self, pa: &Pass) -> TwoPoints {
        let widget = self.widget.read(pa);
        let text = (self.fns.text)(self.sized.as_ref(), pa);
        self.area.end_points(pa, text, widget.print_opts())
    }

    /// The [`Widget`]'s [`PrintOpts`].
    pub fn opts(&self, pa: &Pass) -> PrintOpts {
        self.widget.read(pa).print_opts()
    }

    /// Closes this `Handle`, removing the [`Widget`] from the.
    /// [`Window`]
    ///
    /// [`Window`]: crate::ui::Window
    pub fn close(&self, pa: &mut Pass) -> Result<(), Text> {
        context::windows().close(pa, self)
    }

    /// Get the [`TextMut`] and [`Area`] at once.
    pub(crate) fn text_and_area<'p>(&'p self, pa: &'p mut Pass) -> (TextMut<'p>, &'p mut Area) {
        // SAFETY: A Pass is borrowed, and I know that the same object won't
        // be borrowed multiple times.
        static INTERNAL_PASS: Pass = unsafe { Pass::new() };

        (
            (self.fns.text_mut)(self.sized.as_ref(), unsafe {
                (&raw const INTERNAL_PASS as *mut Pass).as_mut().unwrap()
            }),
            self.area.write(pa),
        )
    }
}

impl<W: Widget + ?Sized> Handle<W> {
    /// Pushes a [`Widget`] around this one.
    ///
    /// This `Widget` will be placed internally, i.e., around the
    /// [`Area`] of `self`. This is in contrast to
    /// [`Handle::push_outer_widget`], which will push around the
    /// "cluster master" of `self`.
    ///
    /// A cluster master is the collection of every `Widget` that was
    /// pushed around a central one with [`PushSpecs::cluster`] set to
    /// `true`.
    ///
    /// Both of these functions behave identically in the situation
    /// where no other [`Widget`]s were pushed around `self`.
    ///
    /// However, if, for example, a [`Widget`] was previously pushed
    /// below `self`, when pushing to the left, the following would
    /// happen:
    ///
    /// ```text
    /// ╭────────────────╮    ╭─────┬──────────╮
    /// │                │    │     │          │
    /// │      self      │    │ new │   self   │
    /// │                │ -> │     │          │
    /// ├────────────────┤    ├─────┴──────────┤
    /// │      old       │    │      old       │
    /// ╰────────────────╯    ╰────────────────╯
    /// ```
    ///
    /// While in [`Handle::push_outer_widget`], this happens instead:
    ///
    /// ```text
    /// ╭────────────────╮    ╭─────┬──────────╮
    /// │                │    │     │          │
    /// │      self      │    │     │   self   │
    /// │                │ -> │ new │          │
    /// ├────────────────┤    │     ├──────────┤
    /// │      old       │    │     │   old    │
    /// ╰────────────────╯    ╰─────┴──────────╯
    /// ```
    ///
    /// Note that `new` was pushed _around_ other clustered widgets in
    /// the second case, not just around `self`.
    #[track_caller]
    pub fn push_inner_widget<PW: Widget>(
        &self,
        pa: &mut Pass,
        widget: PW,
        specs: PushSpecs,
    ) -> Handle<PW> {
        let main = if let Some((main, _)) = self
            .related
            .read(pa)
            .iter()
            .find(|(_, relation)| *relation == WidgetRelation::Main)
        {
            main.clone()
        } else {
            context::windows()
                .handles(pa)
                .find(|handle| handle.ptr_eq(&self.widget))
                .unwrap()
        };

        let handle = context::windows()
            .push_widget(pa, (&self.area, None, specs), widget, Some(&self.area))
            .unwrap();

        main.related
            .write(pa)
            .push((handle.to_dyn(), WidgetRelation::Pushed));

        handle
    }

    /// Pushes a [`Widget`] around the "cluster master" of this one.
    ///
    /// A cluster master is the collection of every `Widget` that was
    /// pushed around a central one with [`PushSpecs::cluster`] set to
    /// `true`.
    ///
    /// This [`Widget`] will be placed externally, i.e., around every
    /// other [`Widget`] that was pushed around `self`. This is in
    /// contrast to [`Handle::push_inner_widget`], which will push
    /// only around `self`.
    ///
    /// Both of these functions behave identically in the situation
    /// where no other [`Widget`]s were pushed around `self`.
    ///
    /// However, if, for example, a [`Widget`] was previously pushed
    /// to the left of `self`, when pushing to the left again, the
    /// following would happen:
    ///
    /// ```text
    /// ╭──────┬──────────╮    ╭─────┬─────┬──────╮
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// │  old │   self   │ -> │ new │ old │ self │
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// ╰──────┴──────────╯    ╰─────┴─────┴──────╯
    /// ```
    ///
    /// While in [`Handle::push_inner_widget`], this happens instead:
    ///
    /// ```text
    /// ╭──────┬──────────╮    ╭─────┬─────┬──────╮
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// │  old │   self   │ -> │ old │ new │ self │
    /// │      │          │    │     │     │      │
    /// │      │          │    │     │     │      │
    /// ╰──────┴──────────╯    ╰─────┴─────┴──────╯
    /// ```
    ///
    /// Note that `new` was pushed _around_ other clustered widgets in
    /// the first case, not just around `self`.
    pub fn push_outer_widget<PW: Widget>(
        &self,
        pa: &mut Pass,
        widget: PW,
        specs: PushSpecs,
    ) -> Handle<PW> {
        let main = if let Some((main, _)) = self
            .related
            .read(pa)
            .iter()
            .find(|(_, relation)| *relation == WidgetRelation::Main)
        {
            main.clone()
        } else {
            context::windows()
                .handles(pa)
                .find(|handle| handle.ptr_eq(&self.widget))
                .unwrap()
        };

        let handle = if let Some(master) = self.area.get_cluster_master(pa) {
            context::windows()
                .push_widget(pa, (&master, None, specs), widget, Some(main.area()))
                .unwrap()
        } else {
            context::windows()
                .push_widget(pa, (&self.area, None, specs), widget, Some(main.area()))
                .unwrap()
        };

        main.related
            .write(pa)
            .push((handle.to_dyn(), WidgetRelation::Pushed));

        handle
    }

    /// Spawns a floating [`Widget`] on the `Handle`.
    pub fn spawn_on_widget<SW: Widget>(
        &self,
        pa: &mut Pass,
        widget: SW,
        specs: DynSpawnSpecs,
    ) -> Option<Handle<SW>> {
        let self_handle = context::windows()
            .handles(pa)
            .find(|handle| handle.ptr_eq(&self.widget))
            .unwrap();

        context::windows().spawn_on_widget(pa, (&self.area, specs), widget, move |pa, handle| {
            let related = self_handle.related.write(pa);

            related.push((handle.clone(), WidgetRelation::Spawned));

            if let Some((main, _)) = related
                .iter_mut()
                .find(|(_, relation)| *relation == WidgetRelation::Main)
                .cloned()
            {
                main.related
                    .write(pa)
                    .push((handle.clone(), WidgetRelation::Spawned));
                handle.related.write(pa).push((main, WidgetRelation::Main));
            } else {
                handle
                    .related
                    .write(pa)
                    .push((self_handle, WidgetRelation::Main));
            }
        })
    }

    /// Spawns a floating [`Widget`] on the `Text`.
    pub fn spawn_on_text<SW: Widget>(
        &self,
        pa: &mut Pass,
        widget: SW,
        index: impl TextIndex,
        ns: Ns,
        specs: DynSpawnSpecs,
    ) -> Option<Handle<SW>> {
        let byte_index = index.to_byte_index();
        if self.text(pa).len() < byte_index {
            context::warn!("Tried spawning a widget beyond the end of the Text");
            return None;
        }

        let (spawn, widget) = Spawn::new(pa, &self.area, widget, specs)?;

        self.text_mut(pa).insert_tag(ns, index, spawn);

        Some(widget)
    }
}

impl<W: Widget> Handle<W> {
    /// Transforms this [`Handle`] into a [`Handle`].
    pub fn to_dyn(&self) -> Handle {
        Handle {
            widget: self.widget.to_dyn_widget(),
            area: self.area.clone(),
            related: self.related.clone(),
            is_closed: self.is_closed.clone(),
            update_requested: self.update_requested.clone(),
            spawn_id: self.spawn_id,
            sized: self.sized.clone(),
            fns: self.fns,
        }
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<W: ?Sized> Send for Handle<W> {}
unsafe impl<W: ?Sized> Sync for Handle<W> {}

impl<W1: ?Sized, W2: ?Sized> PartialEq<Handle<W2>> for Handle<W1> {
    fn eq(&self, other: &Handle<W2>) -> bool {
        self.widget.ptr_eq(&other.widget)
    }
}

impl<W: ?Sized> Clone for Handle<W> {
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            area: self.area.clone(),
            related: self.related.clone(),
            is_closed: self.is_closed.clone(),
            update_requested: self.update_requested.clone(),
            spawn_id: self.spawn_id,
            sized: self.sized.clone(),
            fns: self.fns,
        }
    }
}

impl<W: ?Sized> std::fmt::Debug for Handle<W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Handle").finish_non_exhaustive()
    }
}

/// What relation this [`Widget`] has to its parent.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WidgetRelation {
    /// The main widget of the cluster, most commonly a [`Buffer`].
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    Main,
    /// A [`Widget`] that was pushed around the main `Widget`, e.g.
    /// [`LineNumbers`].
    ///
    /// [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
    Pushed,
    /// A [`Widget`] that was spawned on the `Widget`, e.g.
    /// completion. lists
    Spawned,
}

#[track_caller]
fn populate<'p, 't>(text: &'t mut TextMut<'p>) -> &'t mut Selections {
    if !text.ends_with('\n') {
        text.apply_change(
            None,
            Change::str_insert("\n", text.end_point()).to_string_change(),
        );
    }

    let selections = text.selections_mut();
    selections.populate();
    selections
}
