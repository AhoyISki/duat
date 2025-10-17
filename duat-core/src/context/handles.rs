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
    context,
    data::{Pass, RwData},
    mode::{Cursor, Cursors, Selection, Selections},
    opts::PrintOpts,
    text::{Point, Searcher, Text, TextParts, TwoPoints, txt},
    ui::{Area, PushSpecs, SpawnSpecs, Widget, traits},
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
/// impl Mode for PlacesCharactersAndMoves {
///     type Widget = Buffer;
///
///     // ...
///     fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle) {
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
/// impl Mode for PlacesCharactersAndMoves {
///     type Widget = Buffer;
///
///     fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle) {
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
/// impl Mode for PlacesCharactersAndMoves {
/// #   type Widget = Buffer;
///     /* ... */
///     fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle) {
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
pub struct Handle<W: Widget + ?Sized = crate::buffer::Buffer, S = ()> {
    widget: RwData<W>,
    pub(crate) area: Area,
    mask: Arc<Mutex<&'static str>>,
    related: RelatedWidgets,
    searcher: RefCell<S>,
    is_closed: RwData<bool>,
    master: Option<Box<Handle<dyn Widget>>>,
}

impl<W: Widget + ?Sized> Handle<W> {
    /// Returns a new instance of a [`Handle<W, U>`]
    pub(crate) fn new(
        widget: RwData<W>,
        area: Area,
        mask: Arc<Mutex<&'static str>>,
        master: Option<Handle<dyn Widget>>,
    ) -> Self {
        Self {
            widget,
            area,
            mask,
            related: RelatedWidgets(RwData::default()),
            searcher: RefCell::new(()),
            is_closed: RwData::new(false),
            master: master.map(Box::new),
        }
    }
}

impl<W: Widget + ?Sized, S> Handle<W, S> {
    ////////// Read and write access functions

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

    /// Tries to read as a concrete [`Widget`] implementor
    pub fn read_as<'a, W2: Widget>(&'a self, pa: &'a Pass) -> Option<&'a W2> {
        self.widget.read_as(pa)
    }

    /// Declares the [`Widget`] within as read
    ///
    /// Same as calling `handle.widget().declare_as_read()`. You
    /// should use this function if you want to signal to others that
    /// the widget was read, even if you don't have access to a
    /// [`Pass`].
    pub fn declare_as_read(&self) {
        self.widget.declare_as_read();
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
    pub fn write_with_area<'a>(&'a self, pa: &'a mut Pass) -> (&'a mut W, &'a dyn traits::Area) {
        // SAFETY: It is known that these types can't possibly point to the
        // same data.
        static INTERNAL_PASS: &Pass = &unsafe { Pass::new() };

        (self.widget.write(pa), self.area.read(INTERNAL_PASS))
    }

    /// Declares the [`Widget`] within as written
    ///
    /// Same as calling `handle.widget().declare_written()`. You
    /// should use this function if you want to signal to others that
    /// the widget was written to, even if you don't have access to a
    /// [`Pass`].
    pub fn declare_written(&self) {
        self.widget.declare_written();
    }

    /// Tries to downcast from `dyn Widget` to a concrete [`Widget`]
    pub fn try_downcast<W2: Widget>(&self) -> Option<Handle<W2>> {
        Some(Handle {
            widget: self.widget.try_downcast()?,
            area: self.area.clone(),
            mask: self.mask.clone(),
            related: self.related.clone(),
            searcher: RefCell::new(()),
            is_closed: self.is_closed.clone(),
            master: self.master.clone(),
        })
    }

    ////////// Refined access functions

    /// A shared reference to the [`Text`] of the [`Widget`]
    ///
    /// This is the same as calling `handle.read(pa).text()`.
    pub fn text<'a>(&'a self, pa: &'a Pass) -> &'a Text {
        self.read(pa).text()
    }

    /// A mutable reference to the [`Text`] of the [`Widget`]
    ///
    /// This is the same as calling `handle.write(pa).text_mut()`.
    pub fn text_mut<'a>(&'a self, pa: &'a mut Pass) -> &'a mut Text {
        self.write(pa).text_mut()
    }

    /// The [`TextParts`] of the [`Widget`]
    ///
    /// You can use this in order to get a shared reference to the
    /// [`Bytes`] and [`Selections`], while maintaining a mutable
    /// reference to the [`Tags`] of the [`Text`], letting you place
    /// [`Tag`]s while still reading other information from the
    /// [`Widget`]
    ///
    /// This is the same as calling `handle.text_mut().parts()`.
    ///
    /// [`Bytes`]: crate::text::Bytes
    /// [`Tags`]: crate::text::Tags
    /// [`Tag`]: crate::text::Tag
    pub fn text_parts<'a>(&'a self, pa: &'a mut Pass) -> TextParts<'a> {
        self.write(pa).text_mut().parts()
    }

    /// A shared reference to the [`Selections`] of the [`Widget`]'s
    /// [`Text`]
    ///
    /// This is the same as calling `handle.read(pa).selections()`.
    pub fn selections<'a>(&'a self, pa: &'a Pass) -> &'a Selections {
        self.read(pa).text().selections()
    }

    /// A mutable reference to the [`Selections`] of the [`Widget`]'s
    /// [`Text`]
    ///
    /// This is the same as calling
    /// `handle.write(pa).selections_mut()`.
    pub fn selections_mut<'a>(&'a self, pa: &'a mut Pass) -> &'a mut Selections {
        self.write(pa).text_mut().selections_mut()
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
        edit: impl FnOnce(Cursor<W, S>) -> Ret,
    ) -> Ret {
        fn get_parts<'a, W: Widget + ?Sized, S>(
            pa: &'a mut Pass,
            handle: &'a Handle<W, S>,
            n: usize,
        ) -> (Selection, bool, &'a mut W, &'a dyn traits::Area) {
            let (widget, area) = handle.write_with_area(pa);
            let selections = widget.text_mut().selections_mut();
            selections.populate();
            let Some((selection, was_main)) = selections.remove(n) else {
                panic!("Selection index {n} out of bounds");
            };

            (selection, was_main, widget, area)
        }

        let (selection, was_main, widget, area) = get_parts(pa, self, n);

        // This is safe because of the &mut Pass argument
        let mut searcher = self.searcher.borrow_mut();

        edit(Cursor::new(
            (selection, n, was_main),
            (widget, area),
            None,
            &mut searcher,
            false,
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
    pub fn edit_main<Ret>(&self, pa: &mut Pass, edit: impl FnOnce(Cursor<W, S>) -> Ret) -> Ret {
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
    pub fn edit_last<Ret>(&self, pa: &mut Pass, edit: impl FnOnce(Cursor<W, S>) -> Ret) -> Ret {
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
        edit: impl FnOnce(Cursors<'_, W, S>) -> Ret,
    ) -> Ret {
        edit(self.get_iter(pa))
    }

    /// A shortcut for iterating over all selections
    ///
    /// This is the equivalent of calling:
    ///
    /// ```rust
    /// # use duat_core::prelude::*;
    /// # fn test<U: Ui>(pa: &mut Pass, handle: Handle<Buffer<U>, U, ()>) {
    /// handle.edit_iter(pa, |iter| iter.for_each(|e| { /* .. */ }));
    /// # }
    /// ```
    ///
    /// But it can't return a value, and is meant to reduce the
    /// indentation that will inevitably come from using the
    /// equivalent long form call.
    pub fn edit_all(&self, pa: &mut Pass, edit: impl FnMut(Cursor<W, S>)) {
        self.get_iter(pa).for_each(edit);
    }

    fn get_iter<'a>(&'a self, pa: &'a mut Pass) -> Cursors<'a, W, S> {
        let (widget, area) = self.write_with_area(pa);
        widget.text_mut().selections_mut().populate();

        let searcher = self.searcher.borrow_mut();

        Cursors::new(0, widget, area, searcher)
    }

    ////////// Area functions

    /// Scrolls the [`Text`] veritcally by an amount
    ///
    /// If [`PrintOpts.allow_overscroll`] is set, then the [`Text`]
    /// will be allowed to scroll beyond the last line, up until
    /// reaching the `scrolloff.y` value.
    ///
    /// [`PrintOpts.allow_overscroll`]: crate::opts::PrintOpts::allow_overscroll
    pub fn scroll_ver(&self, pa: &Pass, dist: i32) {
        let widget = self.widget.read(pa);
        self.area()
            .scroll_ver(pa, widget.text(), dist, widget.get_print_opts());
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
            .scroll_to_points(pa, widget.text(), points, widget.get_print_opts());
        self.widget.declare_written();
    }

    /// The start points that should be printed
    pub fn start_points(&self, pa: &Pass) -> (Point, Option<Point>) {
        let widget = self.widget.read(pa);
        self.area
            .start_points(pa, widget.text(), widget.get_print_opts())
    }

    /// The end points that should be printed
    pub fn end_points(&self, pa: &Pass) -> (Point, Option<Point>) {
        let widget = self.widget.read(pa);
        self.area
            .end_points(pa, widget.text(), widget.get_print_opts())
    }

    ////////// Querying functions

    /// This [`Handle`]'s [`Widget`]
    pub fn widget(&self) -> &RwData<W> {
        &self.widget
    }

    /// This [`Handle`]'s [`U::Area`]
    ///
    /// [`U::Area`]: crate::ui::Ui::Area
    pub fn area(&self) -> &Area {
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
    pub fn has_changed(&self, pa: &Pass) -> bool {
        self.widget.has_changed() || self.area.has_changed(pa)
    }

    /// Wether the [`RwData`] within and another point to the same
    /// value
    pub fn ptr_eq<T: ?Sized>(&self, other: &RwData<T>) -> bool {
        self.widget.ptr_eq(other)
    }

    /// The [`Widget`]'s [`PrintOpts`]
    pub fn opts(&self, pa: &Pass) -> PrintOpts {
        self.widget.read(pa).get_print_opts()
    }

    ////////// Related Handles

    /// Returns the [`Handle`] this one was pushed to, if it was
    /// pushed to another
    ///
    /// Will return [`Some`] if this `self` was created by calling
    /// [`Handle::push_outer_widget`], [`Handle::push_inner_widget`],
    /// [`Handle::spawn_widget`], or if the [`Widget`] was [spawned]
    /// on the master's [`Text`]
    ///
    /// [spawned]: crate::text::SpawnTag
    pub fn master(&self) -> Result<&Handle<dyn Widget>, Text> {
        self.master
            .as_ref()
            .map(|handle| handle.as_ref())
            .ok_or_else(|| txt!("Widget was not pushed to another").build())
    }

    /// Returns the [`Handle<Buffer>`] this one was pushed to, if it
    /// was pushed to one
    ///
    /// Will return [`Some`] if this `self` was created by calling
    /// [`Handle::push_outer_widget`], [`Handle::push_inner_widget`],
    /// [`Handle::spawn_widget`], or if the [`Widget`] was [spawned]
    /// on the master's [`Text`]
    ///
    /// [spawned]: crate::text::SpawnTag
    pub fn buffer(&self) -> Result<Handle, Text> {
        self.master
            .as_ref()
            .and_then(|handle| handle.try_downcast())
            .ok_or_else(|| txt!("Widget was not pushed to a [a]Buffer").build())
    }

    /// Reads related [`Widget`]s of type `W2`, as well as it s
    /// [`Ui::Area`]
    ///
    /// This can also be done by calling [`Handle::get_related`], and
    /// [`Handle::read`], but this function should generally be
    /// faster, since there is no cloning of [`Arc`]s going on.
    pub fn read_related<'a, W2: Widget>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = (&'a W2, &'a Area, WidgetRelation)> {
        self.read_as(pa)
            .map(|w| (w, self.area(), WidgetRelation::Main))
            .into_iter()
            .chain(
                self.related.0.read(pa).iter().filter_map(|(handle, rel)| {
                    handle.read_as(pa).map(|w| (w, handle.area(), *rel))
                }),
            )
    }

    /// Gets related [`Handle`]s of type [`Widget`]
    ///
    /// If you are doing this just to read the [`Widget`] and
    /// [`Ui::Area`], consider using [`Handle::read_related`].
    pub fn get_related<'a, W2: Widget>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = (Handle<W2>, WidgetRelation)> + 'a {
        self.try_downcast()
            .zip(Some(WidgetRelation::Main))
            .into_iter()
            .chain(
                self.related
                    .0
                    .read(pa)
                    .iter()
                    .filter_map(|(handle, rel)| handle.try_downcast().zip(Some(*rel))),
            )
    }

    /// Raw access to the related widgets
    pub(crate) fn related(&self) -> &RwData<Vec<(Handle<dyn Widget>, WidgetRelation)>> {
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
    pub fn attach_searcher(&self, searcher: Searcher) -> Handle<W, Searcher> {
        Handle {
            widget: self.widget.clone(),
            area: self.area.clone(),
            mask: self.mask.clone(),
            related: self.related.clone(),
            searcher: RefCell::new(searcher),
            is_closed: self.is_closed.clone(),
            master: self.master.clone(),
        }
    }

    /// Pushes a [`Widget`] around this one
    ///
    /// This `Widget` will be placed internally, i.e., around the
    /// [`Ui::Area`] of `self`. This is in contrast to
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
    pub fn push_inner_widget<PW: Widget>(
        &self,
        pa: &mut Pass,
        widget: PW,
        specs: PushSpecs,
    ) -> Handle<PW> {
        context::windows()
            .push_widget(pa, (&self.area, None, specs), widget, Some(&self.area))
            .unwrap()
    }

    /// Pushes a [`Widget`] around the "cluster master" of this one
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
        if let Some(master) = self.area().get_cluster_master(pa) {
            context::windows()
                .push_widget(pa, (&master, None, specs), widget, Some(self.area()))
                .unwrap()
        } else {
            context::windows()
                .push_widget(pa, (&self.area, None, specs), widget, Some(self.area()))
                .unwrap()
        }
    }

    /// Spawns a floating [`Widget`]
    pub fn spawn_widget<SW: Widget>(
        &self,
        pa: &mut Pass,
        widget: SW,
        specs: SpawnSpecs,
    ) -> Option<Handle<SW>> {
        context::windows().spawn_on_widget(pa, (&self.area, specs), widget)
    }

    /// Closes this `Handle`, removing the [`Widget`] from the [`Ui`]
    pub fn close(&self, pa: &mut Pass) -> Result<(), Text> {
        context::windows().close(pa, self)
    }

    /// Wether this `Handle` was already closed
    pub fn is_closed(&self, pa: &Pass) -> bool {
        *self.is_closed.read(pa)
    }

    /// Declares that this `Handle` has been closed
    pub(crate) fn declare_closed(&self, pa: &mut Pass) {
        *self.is_closed.write(pa) = true;
    }
}

impl<W: Widget, S> Handle<W, S> {
    /// Transforms this [`Handle`] into a [`Handle<dyn Widget>`]
    pub fn to_dyn(&self) -> Handle<dyn Widget> {
        Handle {
            widget: self.widget.to_dyn_widget(),
            // TODO: Arc wrapper, and Area: !Clone
            area: self.area.clone(),
            mask: self.mask.clone(),
            related: self.related.clone(),
            searcher: RefCell::new(()),
            is_closed: self.is_closed.clone(),
            master: self.master.clone(),
        }
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<W: Widget + ?Sized, S> Send for Handle<W, S> {}
unsafe impl<W: Widget + ?Sized, S> Sync for Handle<W, S> {}

impl<W1, W2, S1, S2> PartialEq<Handle<W2, S2>> for Handle<W1, S1>
where
    W1: Widget + ?Sized,
    W2: Widget + ?Sized,
{
    fn eq(&self, other: &Handle<W2, S2>) -> bool {
        self.widget().ptr_eq(other.widget())
    }
}

impl<W: Widget + ?Sized> Clone for Handle<W> {
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            area: self.area.clone(),
            mask: self.mask.clone(),
            related: self.related.clone(),
            searcher: self.searcher.clone(),
            is_closed: self.is_closed.clone(),
            master: self.master.clone(),
        }
    }
}

impl<W: Widget + ?Sized, S> std::fmt::Debug for Handle<W, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Handle")
            .field("mask", &self.mask)
            .finish_non_exhaustive()
    }
}

#[derive(Clone)]
struct RelatedWidgets(RwData<Vec<(Handle<dyn Widget>, WidgetRelation)>>);

/// What relation this [`Widget`] has to its parent
#[derive(Clone, Copy, Debug)]
pub enum WidgetRelation {
    /// The main widget of the cluster, most commonly a [`Buffer`]
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    Main,
    /// A [`Widget`] that was pushed around the main `Widget`, e.g.
    /// [`LineNumbers`]
    ///
    /// [`LineNumbers`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.LineNumbers.html
    Pushed,
    /// A [`Widget`] that was spawned on the `Widget`, e.g. completion
    /// lists
    Spawned,
}
