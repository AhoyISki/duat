//! Widget handles for Duat
//!
//! These are used pretty much everywhere, and are essentially just an
//! [`RwData<W>`] conjoined to an [`Ui::Area`].

use std::{
    cell::RefCell,
    sync::{Arc, Mutex},
};

use lender::Lender;

use crate::{
    cfg::PrintCfg,
    data::{Pass, RwData},
    mode::{Cursor, Cursors, Selection},
    text::{Point, Searcher, Text, TwoPoints},
    ui::{Area, AreaId, GetAreaId, MutArea, SpawnSpecs, Ui, Widget, WidgetCfg},
};

/// A handle to a [`Widget`] in Duat
///
/// The [`Handle`] lets you do all sorts of edits on a [`Widget`]. You
/// can, for example, make use of the [`Selection`]s in its [`Text`]
/// in order to edit the [`Text`] in a very declarative way.
///
/// One of the places where this is commonly done is within [`Mode`]s,
/// where you get access to the [`Handle`] of the currently active
/// [`Widget`]. Below is a very straightforward [`Mode`]:
///
/// ```rust
/// use duat_core::prelude::*;
/// /// A very basic example Mode.
/// #[derive(Clone)]
/// struct PlacesCharactersAndMoves;
///
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
///     type Widget = File<U>;
///
///     // ...
///     fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<Self::Widget, U>) {
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
/// - The [`Handle`] for a [`Mode::Widget`].
///
/// ```rust
/// use duat_core::prelude::*;
/// #[derive(Clone)]
/// struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
///     type Widget = File<U>;
///
///     fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<File<U>, U>) {
///         match key {
///             // actions based on the key pressed
///             key!(KeyCode::Char('c')) => {
///                 // Do something when the character 'c' is typed.
///             }
///             _ => todo!("The remaining keys"),
///         }
///     }
/// }
/// ```
///
/// (You can use the [`key!`] macro in order to match [`KeyEvent`]s).
///
/// With the [`Handle`], you can modify [`Text`] in a simplified
/// way. This is done by two actions, [editing] and [moving]. You
/// can only do one of these on any number of selections at the same
/// time.
///
/// ```rust
/// # use duat_core::prelude::*;
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File<U>;
///     /* ... */
///     fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
///         match key {
///             key!(KeyCode::Char(c)) => {
///                 handle.edit_all(pa, |mut e| {
///                     e.insert('c');
///                     e.move_hor(1);
///                 });
///             },
///             key!(KeyCode::Right, KeyMod::SHIFT) => {
///                 handle.edit_all(pa, |mut e| {
///                     if e.anchor().is_none() {
///                         e.set_anchor();
///                     }
///                     e.move_hor(1);
///                 });
///             }
///             key!(KeyCode::Right) => {
///                 handle.edit_all(pa, |mut e| {
///                     e.unset_anchor();
///                     e.move_hor(1);
///                 });
///             }
///             _ => todo!("Predictable remaining implementations")
///         }
///     }
/// # }
/// ```
///
/// [`Mode`]: crate::mode::Mode
/// [`Mode::Widget`]: crate::mode::Mode::Widget
/// [`&mut Pass`]: Pass
/// [`PromptLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
/// [`Mode::send_key`]: crate::mode::Mode::send_key
/// [key]: crate::mode::KeyEvent
/// [mapped]: crate::mode::map
/// [`read`]: RwData::read
/// [`write`]: RwData::write
/// [`U::Area`]: Ui::Area
/// [`Self::Widget`]: crate::mode::Mode::Widget
/// [`Some(selections)`]: Some
/// [`Ui::Area`]: crate::ui::Ui::Area
/// [commands]: crate::cmd
/// [`key!`]: crate::mode::key
/// [`KeyEvent`]: crate::mode::KeyEvent
/// [editing]: Cursor
/// [moving]: Cursor
/// [`Mode`]: crate::mode::Mode
/// [`U::Area`]: Ui::Area
pub struct Handle<W: Widget<U> + ?Sized, U: Ui, S = ()> {
    widget: RwData<W>,
    area: U::Area,
    mask: Arc<Mutex<&'static str>>,
    id: AreaId,
    related: RelatedWidgets<U>,
    searcher: RefCell<S>,
}

impl<W: Widget<U> + ?Sized, U: Ui> Handle<W, U> {
    /// Returns a new instance of a [`Handle<W, U>`]
    pub(crate) fn new(
        widget: RwData<W>,
        area: U::Area,
        mask: Arc<Mutex<&'static str>>,
        id: AreaId,
    ) -> Self {
        Self {
            widget,
            area,
            mask,
            id,
            related: RelatedWidgets(RwData::default()),
            searcher: RefCell::new(()),
        }
    }
}

impl<W: Widget<U> + ?Sized, U: Ui, S> Handle<W, U, S> {
    /// Reads from the [`Widget`], making use of a [`Pass`]
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

    /// Writes to the [`Widget`], making use of a [`Pass`]
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
    /// [`Pass`]
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
    pub fn write_with_area<'a>(&'a self, pa: &'a mut Pass) -> (&'a mut W, &'a U::Area) {
        (self.widget.write(pa), &self.area)
    }

    ////////// Selection Editing functions

    /// Edits the nth [`Selection`] in the [`Text`]
    ///
    /// Once dropped, the [`Selection`] in this [`Cursor`] will be
    /// added back to the list of [`Selection`]s, unless it is
    /// [destroyed]
    ///
    /// If you want to edit on the main selection, see [`edit_main`],
    /// if you want to edit on many [`Selection`]s, see
    /// [`edit_iter`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Selections`], so if there are no [`Selection`]s, it will
    /// create one at [`Point::default`].
    ///
    /// [destroyed]: Cursor::destroy
    /// [`edit_main`]: Self::edit_main
    /// [`edit_iter`]: Self::edit_iter
    /// [`Point::default`]: crate::text::Point::default
    pub fn edit_nth<Ret>(
        &self,
        pa: &mut Pass,
        n: usize,
        edit: impl FnOnce(Cursor<W, U::Area, S>) -> Ret,
    ) -> Ret {
        fn get_parts<'a, W: Widget<U> + ?Sized, U: Ui>(
            pa: &'a mut Pass,
            widget: &'a RwData<W>,
            n: usize,
        ) -> (Selection, bool, &'a mut W) {
            let widget = widget.write(pa);
            let selections = widget.text_mut().selections_mut();
            selections.populate();
            let Some((selection, was_main)) = selections.remove(n) else {
                panic!("Selection index {n} out of bounds");
            };

            (selection, was_main, widget)
        }

        let (selection, was_main, widget) = get_parts(pa, &self.widget, n);

        // This is safe because of the &mut Pass argument
        let mut searcher = self.searcher.borrow_mut();

        edit(Cursor::new(
            selection,
            n,
            was_main,
            &mut *widget,
            &self.area,
            None,
            &mut searcher,
        ))
    }

    /// Edits the main [`Selection`] in the [`Text`]
    ///
    /// Once dropped, the [`Selection`] in this [`Cursor`] will be
    /// added back to the list of [`Selection`]s, unless it is
    /// [destroyed]
    ///
    /// If you want to edit on the `nth` selection, see [`edit_nth`],
    /// same for [`edit_last`], if you want to edit on many
    /// [`Selection`]s, see [`edit_iter`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Selections`], so if there are no [`Selection`]s, it will
    /// create one at [`Point::default`].
    ///
    /// [destroyed]: Cursor::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_last`]: Self::edit_last
    /// [`edit_iter`]: Self::edit_iter
    /// [`Point::default`]: crate::text::Point::default
    pub fn edit_main<Ret>(
        &self,
        pa: &mut Pass,
        edit: impl FnOnce(Cursor<W, U::Area, S>) -> Ret,
    ) -> Ret {
        self.edit_nth(
            pa,
            self.widget.read(pa).text().selections().main_index(),
            edit,
        )
    }

    /// Edits the last [`Selection`] in the [`Text`]
    ///
    /// Once dropped, the [`Selection`] in this [`Cursor`] will be
    /// added back to the list of [`Selection`]s, unless it is
    /// [destroyed]
    ///
    /// If you want to edit on the `nth` selection, see [`edit_nth`],
    /// same for [`edit_main`], if you want to edit on many
    /// [`Selection`]s, see [`edit_iter`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Selections`], so if there are no [`Selection`]s, it will
    /// create one at [`Point::default`].
    ///
    /// [destroyed]: Cursor::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_main`]: Self::edit_main
    /// [`edit_iter`]: Self::edit_iter
    /// [`Point::default`]: crate::text::Point::default
    pub fn edit_last<Ret>(
        &self,
        pa: &mut Pass,
        edit: impl FnOnce(Cursor<W, U::Area, S>) -> Ret,
    ) -> Ret {
        let len = self.widget.read(pa).text().selections().len();
        self.edit_nth(pa, len.saturating_sub(1), edit)
    }

    /// A [`Lender`] over all [`Cursor`]s of the [`Text`]
    ///
    /// This lets you easily iterate over all [`Selection`]s, without
    /// having to worry about insertion affecting the order at which
    /// they are edited (like what repeated calls to [`edit_nth`]
    /// would do)
    ///
    /// Note however that you can't use a [`Lender`] (also known as a
    /// lending iterator) in a `for` loop, but you should be able
    /// to just `while let Some(e) = editors.next() {}` or
    /// `handle.edit_iter().for_each(|_| {})` instead.
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Selections`], so if there are no [`Selection`]s, it will
    /// create one at [`Point::default`].
    ///
    /// [`edit_nth`]: Self::edit_nth
    /// [`Point::default`]: crate::text::Point::default
    pub fn edit_iter<Ret>(
        &self,
        pa: &mut Pass,
        edit: impl FnOnce(Cursors<'_, W, U::Area, S>) -> Ret,
    ) -> Ret {
        edit(self.get_iter(pa))
    }

    /// A shortcut for iterating over all selections
    ///
    /// This is the equivalent of calling:
    ///
    /// ```rust
    /// # use duat_core::prelude::*;
    /// # fn test<U: Ui>(pa: &mut Pass, handle: Handle<File<U>, U, ()>) {
    /// handle.edit_iter(pa, |iter| iter.for_each(|e| { /* .. */ }));
    /// # }
    /// ```
    ///
    /// But it can't return a value, and is meant to reduce the
    /// indentation that will inevitably come from using the
    /// equivalent long form call.
    pub fn edit_all(&self, pa: &mut Pass, edit: impl FnMut(Cursor<W, U::Area, S>)) {
        self.get_iter(pa).for_each(edit);
    }

    fn get_iter<'a>(&'a self, pa: &'a mut Pass) -> Cursors<'a, W, U::Area, S> {
        let widget = self.widget.write(pa);
        widget.text_mut().selections_mut().populate();

        let searcher = self.searcher.borrow_mut();

        Cursors::new(0, widget, &self.area, searcher)
    }

    ////////// Area functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If [`PrintCfg.allow_overscroll`] is set, then the [`Text`]
    /// will be allowed to scroll beyond the last line, up until
    /// reaching the `scrolloff.y` value.
    ///
    /// [`PrintCfg.allow_overscroll`]: crate::cfg::PrintCfg::allow_overscroll
    pub fn scroll_ver(&self, pa: &Pass, dist: i32) {
        let widget = self.widget.read(pa);
        self.area(pa)
            .scroll_ver(widget.text(), dist, widget.print_cfg());
        self.widget.declare_written();
    }

    /// Scrolls the [`Text`] to the visual line of a [`TwoPoints`]
    ///
    /// If `scroll_beyond` is set, then the [`Text`] will be allowed
    /// to scroll beyond the last line, up until reaching the
    /// `scrolloff.y` value.
    pub fn scroll_to_points(&self, pa: &Pass, points: impl TwoPoints) {
        let widget = self.widget.read(pa);
        self.area
            .scroll_to_points(widget.text(), points, widget.print_cfg());
        self.widget.declare_written();
    }

    /// The start points that should be printed
    pub fn start_points(&self, pa: &Pass) -> (Point, Option<Point>) {
        let widget = self.widget.read(pa);
        self.area.start_points(widget.text(), widget.print_cfg())
    }

    /// The end points that should be printed
    pub fn end_points(&self, pa: &Pass) -> (Point, Option<Point>) {
        let widget = self.widget.read(pa);
        self.area.end_points(widget.text(), widget.print_cfg())
    }

    ////////// Querying functions

    /// This [`Handle`]'s [`Widget`]
    pub fn widget(&self) -> &RwData<W> {
        &self.widget
    }

    /// This [`Handle`]'s [`U::Area`]
    ///
    /// [`U::Area`]: crate::ui::Ui::Area
    pub fn area(&self, _: &Pass) -> &U::Area {
        &self.area
    }

    /// Gets this [`Handle`]'s mask
    ///
    /// This mask is going to be used to map [`Form`]s to other
    /// [`Form`]s when printing via [`Widget::print`]. To see more
    /// about how masks work, see [`form::enable_mask`].
    ///
    /// [`Form`]: crate::form::Form
    /// [`form::enable_mask`]: crate::form::enable_mask
    pub fn mask(&self) -> &Arc<Mutex<&'static str>> {
        &self.mask
    }

    /// Sets this [`Handle`]'s mask, returning the previous one
    ///
    /// This mask is going to be used to map [`Form`]s to other
    /// [`Form`]s when printing via [`Widget::print`]. To see more
    /// about how masks work, see [`form::enable_mask`].
    ///
    /// [`Form`]: crate::form::Form
    /// [`form::enable_mask`]: crate::form::enable_mask
    pub fn set_mask(&self, mask: &'static str) -> &'static str {
        self.widget.declare_written();
        std::mem::replace(&mut self.mask.lock().unwrap(), mask)
    }

    /// Wether someone else called [`write`] or [`write_as`] since the
    /// last [`read`] or [`write`]
    ///
    /// Do note that this *DOES NOT* mean that the value inside has
    /// actually been changed, it just means a mutable reference was
    /// acquired after the last call to [`has_changed`].
    ///
    /// Some types like [`Text`], and traits like [`Widget`] offer
    /// [`needs_update`] methods, you should try to determine what
    /// parts to look for changes.
    ///
    /// Generally though, you can use this method to gauge that.
    ///
    /// [`write`]: RwData::write
    /// [`write_as`]: RwData::write_as
    /// [`read`]: RwData::read
    /// [`has_changed`]: RwData::has_changed
    /// [`Text`]: crate::text::Text
    /// [`Widget`]: crate::ui::Widget
    /// [`needs_update`]: crate::ui::Widget::needs_update
    pub fn has_changed(&self) -> bool {
        self.widget.has_changed() || self.area.has_changed()
    }

    /// Wether the [`RwData`] within and another point to the same
    /// value
    pub fn ptr_eq<T: ?Sized>(&self, other: &RwData<T>) -> bool {
        self.widget.ptr_eq(other)
    }

    /// The [`Widget`]'s [`PrintCfg`]
    pub fn cfg(&self, pa: &Pass) -> PrintCfg {
        self.widget.read(pa).print_cfg()
    }

    /// Gets the [`Handle`] of a related [`Widget`]
    pub fn get_related<W2: Widget<U>>(&self, pa: &Pass) -> Option<Handle<W2, U>> {
        self.related
            .0
            .read(pa)
            .iter()
            .find_map(|handle| handle.try_downcast())
    }

    /// Raw access to the related widgets
    pub(crate) fn related(&self) -> &RwData<Vec<Handle<dyn Widget<U>, U>>> {
        &self.related.0
    }

    ////////// Other methods

    /// Attaches a [`Searcher`] to this [`Handle`], so you can do
    /// incremental search
    ///
    /// An [`Handle`] with a [`Searcher`] not only has its usual
    /// editing capabilities, but is also able to act on requested
    /// regex searches, like those from [`IncSearch`], in
    /// [`duat-utils`]. This means that a user can type up a
    /// [prompt] to search for something, and the [`Handle`]
    /// can use the [`Searcher`] to interpret how that search will
    /// be utilized. Examples of this can be found in the
    /// [`duat-utils`] crate, as well as the [`duat-kak`] crate,
    /// which has some more advanced usage.
    ///
    /// [`Searcher`]: crate::text::Searcher
    /// [`Selection`]: crate::mode::Selection
    /// [`Cursor`]: crate::mode::Cursor
    /// [`IncSearch`]: https://docs.rs/duat-utils/latest/duat_utils/modes/struct.IncSearch.html
    /// [`duat-utils`]: https://docs.rs/duat-utils/lastest/
    /// [prompt]: https://docs.rs/duat-utils/latest/duat_utils/modes/trait.PromptMode.html
    /// [`duat-kak`]: https://docs.rs/duat-kak/lastest/
    pub fn attach_searcher(&self, searcher: Searcher) -> Handle<W, U, Searcher> {
        Handle {
            widget: self.widget.clone(),
            area: self.area.clone(),
            mask: self.mask.clone(),
            id: self.id.clone(),
            related: self.related.clone(),
            searcher: RefCell::new(searcher),
        }
    }

    /// Spawns a floating [`Widget`]
    pub fn spawn_widget<Cfg: WidgetCfg<U>>(
        &self,
        pa: &mut Pass,
        cfg: Cfg,
        specs: SpawnSpecs,
    ) -> Result<AreaId, Text> {
        let area = MutArea(&self.area);
        let _spawned = area.spawn_floating(pa, cfg, specs)?;
        todo!();
    }
}

impl<U: Ui> Handle<dyn Widget<U>, U> {
    /// Tries to read as a concrete [`Widget`] implementor
    pub fn read_as<'a, W: Widget<U>>(&'a self, pa: &'a Pass) -> Option<&'a W> {
        self.widget.read_as(pa)
    }

    /// Tries to downcast from `dyn Widget` to a concrete [`Widget`]
    pub fn try_downcast<W: Widget<U>>(&self) -> Option<Handle<W, U>> {
        Some(Handle {
            widget: self.widget.try_downcast()?,
            area: self.area.clone(),
            mask: self.mask.clone(),
            id: self.id,
            related: self.related.clone(),
            searcher: self.searcher.clone(),
        })
    }
}

impl<W: Widget<U>, U: Ui> Handle<W, U> {
    pub fn to_dyn(&self) -> Handle<dyn Widget<U>, U> {
        Handle {
            widget: self.widget.to_dyn_widget(),
            // TODO: Arc wrapper, and Area: !Clone
            area: self.area.clone(),
            mask: self.mask.clone(),
            id: self.id,
            related: self.related.clone(),
            searcher: RefCell::new(()),
        }
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<W: Widget<U>, U: Ui, S> Send for Handle<W, U, S> {}
unsafe impl<W: Widget<U>, U: Ui, S> Sync for Handle<W, U, S> {}

impl<W: Widget<U> + ?Sized, U: Ui, S> GetAreaId for Handle<W, U, S> {
    fn area_id(&self) -> AreaId {
        self.id
    }
}

impl<T: GetAreaId, W: Widget<U> + ?Sized, U: Ui, S> PartialEq<T> for Handle<W, U, S> {
    fn eq(&self, other: &T) -> bool {
        self.area_id() == other.area_id()
    }
}

impl<W: Widget<U> + ?Sized, U: Ui> Clone for Handle<W, U> {
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            area: self.area.clone(),
            mask: self.mask.clone(),
            id: self.id.clone(),
            related: self.related.clone(),
            searcher: self.searcher.clone(),
        }
    }
}

#[derive(Clone)]
struct RelatedWidgets<U: Ui>(RwData<Vec<Handle<dyn Widget<U>, U>>>);


