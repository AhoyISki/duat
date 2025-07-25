//! Widget handles for Duat
//!
//! These are used pretty much everywhere, and are essentially just an
//! [`RwData<W>`] conjoined to an [`Ui::Area`].

use std::{
    any::TypeId,
    cell::{RefCell, RefMut},
    sync::{Arc, Mutex},
};

use lender::Lender;

use super::FileParts;
use crate::{
    cfg::PrintCfg,
    data::{Pass, RwData},
    file::File,
    mode::{Cursor, Cursors, Selection, Selections},
    text::{Point, Searcher, Text, TwoPoints},
    ui::{Node, RawArea, Ui, Widget},
};

/// A handle to a [`File`] widget
///
/// This handle acts much like an [`RwData<File>`], but it also
/// includes an [`Area`] that can be acted upon alongside the
/// [`File`].
///
/// This is the only way you are supposed to read information about
/// the [`File`], in order to display it on [`Widget`]s, create
/// [`Text`]s, and do all sorts of things. You can, of course, also
/// modify a [`File`] from within this struct, but you should be
/// careful to prevent infinite loops, where you modify a [`File`], it
/// gets updated, and then you modify it again after noticing that it
/// has changed.
///
/// The main difference between a [`FileHandle<U>`] and a
/// [`Handle<File<U>, U>`] is that the [`Handle`] is capable of acting
/// on selections, but is fixed to just one [`File`], while the
/// [`FileHandle`] can automatically point to the current [`File`].
///
/// [`Area`]: crate::ui::RawArea
/// [`Text`]: crate::text::Text
#[derive(Clone)]
pub struct FileHandle<U: Ui> {
    fixed: Option<FileParts<U>>,
    current: RwData<Option<FileParts<U>>>,
}

impl<U: Ui> FileHandle<U> {
    /// Returns a new [`FileHandle`] from its parts
    pub(crate) fn from_parts(
        fixed: Option<FileParts<U>>,
        current: RwData<Option<FileParts<U>>>,
    ) -> Self {
        Self { fixed, current }
    }

    /// Reads from the [`File`] and the [`Area`] using a [`Pass`]
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
    /// # Panics
    ///
    /// Panics if there is a mutable borrow of this struct somewhere,
    /// which could happen if you use [`RwData::write_unsafe`] or
    /// [`RwData::write_unsafe_as`] from some other place
    ///
    /// [`Area`]: crate::ui::RawArea
    pub fn read<Ret>(&self, pa: &Pass, f: impl FnOnce(&File<U>, &U::Area) -> Ret) -> Ret {
        if let Some((handle, _)) = self.fixed.as_ref() {
            f(&handle.widget.acquire(pa), &handle.area)
        } else {
            self.current.read(pa, |parts| {
                let (handle, _) = parts.as_ref().unwrap();
                f(&handle.widget.acquire(pa), &handle.area)
            })
        }
    }

    /// Writes to the [`File`] and [`Area`] within using a [`Pass`]
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
    /// # Panics
    ///
    /// Panics if there is any type of borrow of this struct
    /// somewhere, which could happen if you use
    /// [`RwData::read_unsafe`] or [`RwData::write_unsafe`], for
    /// example.
    ///
    /// [`Area`]: crate::ui::RawArea
    pub fn write<Ret>(&self, pa: &mut Pass, f: impl FnOnce(&mut File<U>, &U::Area) -> Ret) -> Ret {
        if let Some((handle, _)) = self.fixed.as_ref() {
            f(&mut handle.widget.acquire_mut(pa), &handle.area)
        } else {
            self.current.read(pa, |parts| {
                // SAFETY: Since the update closure only uses a write method, the
                // Pass becomes unusable for other purposes, making it impossible
                // to make further borrows, asserting that there is no other borrow
                // for self.current.
                let pa = unsafe { &mut Pass::new() };
                let (handle, _) = parts.as_ref().unwrap();
                f(&mut handle.widget.acquire_mut(pa), &handle.area)
            })
        }
    }

    /// Reads a [`Widget`] related to this [`File`], alongside its
    /// [`Area`], with a [`Pass`]
    ///
    /// A related [`Widget`] is one that was pushed to this [`File`]
    /// during the [`OnFileOpen`] [hook].
    ///
    /// [`Area`]: crate::ui::Area
    /// [`OnFileOpen`]: crate::hook::OnFileOpen
    /// [hook]: crate::hook
    pub fn read_related<W: 'static, R>(
        &self,
        pa: &Pass,
        f: impl FnOnce(&W, &U::Area) -> R,
    ) -> Option<R> {
        let read = |(handle, related): &FileParts<U>| {
            if TypeId::of::<W>() == TypeId::of::<File<U>>() {
                let area = handle.area(pa);
                handle.widget().read_as(pa, |w| f(w, area))
            } else {
                let related = related.acquire(pa);
                related
                    .iter()
                    .find(|node| node.data_is::<W>())
                    .and_then(|node| node.widget().read_as(pa, |w| f(w, node.area())))
            }
        };

        if let Some(parts) = self.fixed.as_ref() {
            read(parts)
        } else {
            self.current.read(pa, |parts| read(parts.as_ref().unwrap()))
        }
    }

    /// Gets the [`RwData`] and [`Area`] of a related widget, with a
    /// [`Pass`]
    ///
    /// A related [`Widget`] is one that was pushed to this [`File`]
    /// during the [`OnFileOpen`] [hook].
    ///
    /// [`Area`]: crate::ui::Area
    /// [`OnFileOpen`]: crate::hook::OnFileOpen
    /// [hook]: crate::hook
    pub fn get_related_widget<W: Widget<U> + 'static>(&self, pa: &Pass) -> Option<Handle<W, U>> {
        let get_related = |(handle, related): &FileParts<U>| {
            if TypeId::of::<W>() == TypeId::of::<File<U>>() {
                let widget = handle.widget().try_downcast()?;
                Some(Handle::from_parts(
                    widget,
                    handle.area(pa).clone(),
                    handle.mask().clone(),
                ))
            } else {
                related.read(pa, |related| {
                    related.iter().find_map(|node| {
                        let (widget, area, mask, _) = node.parts();
                        widget
                            .try_downcast()
                            .map(|data| Handle::from_parts(data, area.clone(), mask.clone()))
                    })
                })
            }
        };

        if let Some(parts) = self.fixed.as_ref() {
            get_related(parts)
        } else {
            self.current
                .read(pa, |parts| get_related(parts.as_ref().unwrap()))
        }
    }

    /// Writes to the related widgets
    pub(crate) fn write_related_widgets(&self, pa: &mut Pass, f: impl FnOnce(&mut Vec<Node<U>>)) {
        if let Some((.., related)) = self.fixed.as_ref() {
            related.write(pa, f)
        } else {
            self.current.read(pa, |parts| {
                let pa = unsafe { &mut Pass::new() };
                parts.as_ref().unwrap().1.write(pa, f)
            })
        }
    }

    ////////// Querying functions

    /// Gets a [`Handle`] from this [`FileHandle`]
    pub fn handle(&self, pa: &Pass) -> Handle<File<U>, U> {
        if let Some((handle, _)) = self.fixed.as_ref() {
            handle.clone()
        } else {
            self.current.acquire(pa).as_ref().unwrap().0.clone()
        }
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
        if let Some((handle, _)) = self.fixed.as_ref() {
            handle.has_changed()
        } else {
            self.current.has_changed()
                || self.current.read_raw(|parts| {
                    let (handle, _) = parts.as_ref().unwrap();
                    handle.has_changed()
                })
        }
    }

    /// Wether the [`File`] within has swapped to another
    ///
    /// This can only happen when this is a
    pub fn has_swapped(&self) -> bool {
        let has_changed = self.current.has_changed();
        self.current.declare_as_read();
        has_changed
    }

    /// Wether the [`RwData`] within and another point to the same
    /// value
    pub fn ptr_eq<T: ?Sized>(&self, pa: &Pass, other: &RwData<T>) -> bool {
        if let Some((handle, ..)) = self.fixed.as_ref() {
            handle.ptr_eq(other)
        } else {
            self.current
                .read(pa, |parts| parts.as_ref().unwrap().0.ptr_eq(other))
        }
    }

    /// The name of the [`File`] in question
    pub fn name(&self, pa: &Pass) -> String {
        if let Some((handle, ..)) = self.fixed.as_ref() {
            handle.read(pa, |f, _| f.name())
        } else {
            self.current.read(pa, |parts| {
                parts.as_ref().unwrap().0.read(pa, |f, _| f.name())
            })
        }
    }

    /// The path of the [`File`] in question
    pub fn path(&self, pa: &Pass) -> String {
        if let Some((handle, ..)) = self.fixed.as_ref() {
            handle.read(pa, |f, _| f.path())
        } else {
            self.current.read(pa, |parts| {
                parts.as_ref().unwrap().0.read(pa, |f, _| f.path())
            })
        }
    }

    /// The path of the [`File`] in question, if it was set
    pub fn set_path(&self, pa: &Pass) -> Option<String> {
        if let Some((handle, ..)) = self.fixed.as_ref() {
            handle.read(pa, |f, _| f.path_set())
        } else {
            self.current.read(pa, |parts| {
                parts.as_ref().unwrap().0.read(pa, |f, _| f.path_set())
            })
        }
    }
}

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
#[derive(Debug)]
pub struct Handle<W: Widget<U> + ?Sized, U: Ui, S = ()> {
    widget: RwData<W>,
    area: U::Area,
    mask: Arc<Mutex<&'static str>>,
    searcher: RefCell<S>,
}

impl<W: Widget<U> + ?Sized, U: Ui> Handle<W, U> {
    /// Returns a new instance of a [`Handle<W, U>`]
    pub(crate) fn from_parts(
        widget: RwData<W>,
        area: U::Area,
        mask: Arc<Mutex<&'static str>>,
    ) -> Self {
        Self {
            widget,
            area,
            mask,
            searcher: RefCell::new(()),
        }
    }
}

impl<W: Widget<U> + ?Sized, U: Ui, S> Handle<W, U, S> {
    /// Reads from the [`Widget`] and the [`Area`] using a [`Pass`]
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
    /// # Panics
    ///
    /// Panics if there is a mutable borrow of this struct somewhere,
    /// which could happen if you use [`RwData::write_unsafe`] or
    /// [`RwData::write_unsafe_as`] from some other place
    ///
    /// [`Area`]: crate::ui::RawArea
    pub fn read<Ret>(&self, pa: &Pass, f: impl FnOnce(&W, &U::Area) -> Ret) -> Ret {
        f(&self.widget.acquire(pa), &self.area)
    }

    /// Writes to the [`Widget`] and [`Area`] within using a [`Pass`]
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
    /// # Panics
    ///
    /// Panics if there is any type of borrow of this struct
    /// somewhere, which could happen if you use
    /// [`RwData::read_unsafe`] or [`RwData::write_unsafe`], for
    /// example.
    ///
    /// [`Area`]: crate::ui::RawArea
    pub fn write<Ret>(&self, pa: &mut Pass, f: impl FnOnce(&mut W, &U::Area) -> Ret) -> Ret {
        f(&mut self.widget.acquire_mut(pa), &self.area)
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
            pa: &mut Pass,
            widget: &'a RwData<W>,
            n: usize,
        ) -> (Selection, bool, RefMut<'a, W>) {
            let mut widget = widget.acquire_mut(pa);
            let selections = widget.text_mut().selections_mut();
            selections.populate();
            let Some((selection, was_main)) = selections.remove(n) else {
                panic!("Selection index {n} out of bounds");
            };

            (selection, was_main, widget)
        }

        let (selection, was_main, mut widget) = get_parts(pa, &self.widget, n);

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
            self.widget
                .read(pa, |wid| wid.text().selections().main_index()),
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
        self.edit_nth(
            pa,
            self.widget
                .read(pa, |wid| wid.text().selections().len())
                .saturating_sub(1),
            edit,
        )
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

    fn get_iter(&self, pa: &mut Pass) -> Cursors<'_, W, U::Area, S> {
        let mut widget = self.widget.acquire_mut(pa);
        widget.text_mut().selections_mut().populate();

        let searcher = self.searcher.borrow_mut();

        Cursors::new(0, widget, &self.area, searcher)
    }

    ////////// Functions derived from RwData

    /// Reads the [`Text`] of the [`Widget`]
    pub fn read_text<Ret>(&self, pa: &Pass, read: impl FnOnce(&Text) -> Ret) -> Ret {
        let widget = self.widget.acquire(pa);
        read(widget.text())
    }

    /// Writes to the [`Text`] of the [`Widget`]
    pub fn write_text<Ret>(&self, pa: &mut Pass, write: impl FnOnce(&mut Text) -> Ret) -> Ret {
        let mut widget = self.widget.acquire_mut(pa);
        write(widget.text_mut())
    }

    /// Reads the [`Selections`] of the [`Widget`]
    pub fn read_selections<Ret>(&self, pa: &Pass, read: impl FnOnce(&Selections) -> Ret) -> Ret {
        let widget = self.widget.acquire(pa);
        read(widget.text().selections())
    }

    /// Writes to the [`Selections`] of the [`Widget`]
    pub fn write_selections<Ret>(
        &self,
        pa: &mut Pass,
        write: impl FnOnce(&mut Selections) -> Ret,
    ) -> Ret {
        let mut widget = self.widget.acquire_mut(pa);
        write(widget.text_mut().selections_mut())
    }

    ////////// Direct Text manipulation

    /// Clones the [`Text`] within
    pub fn clone_text(&self, pa: &Pass) -> Text {
        self.widget.clone_text(pa)
    }

    /// Replaces the [`Text`] within with a [`Default`] version
    pub fn take_text(&self, pa: &mut Pass) -> Text {
        self.widget.take_text(pa)
    }

    /// Replaces the [`Text`] of the [`Widget`], returning the
    /// previous value
    pub fn replace_text(&self, pa: &mut Pass, text: impl Into<Text>) -> Text {
        self.widget.replace_text(pa, text.into())
    }

    /// Undoes the last moment in the history, if there is one
    pub fn undo(&self, pa: &mut Pass) {
        self.widget.write(pa, |wid| wid.text_mut().undo());
    }

    /// Redoes the last moment in the history, if there is one
    pub fn redo(&self, pa: &mut Pass) {
        self.widget.write(pa, |wid| wid.text_mut().redo());
    }

    /// Finishes the current moment and adds a new one to the history
    pub fn new_moment(&self, pa: &mut Pass) {
        self.widget.write(pa, |wid| wid.text_mut().new_moment());
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
        let widget = self.widget.acquire(pa);
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
        let widget = self.widget.acquire(pa);
        self.area
            .scroll_to_points(widget.text(), points, widget.print_cfg());
        self.widget.declare_written();
    }

    /// The start points that should be printed
    pub fn start_points(&self, pa: &Pass) -> (Point, Option<Point>) {
        let widget = self.widget.acquire(pa);
        self.area.start_points(widget.text(), widget.print_cfg())
    }

    /// The end points that should be printed
    pub fn end_points(&self, pa: &Pass) -> (Point, Option<Point>) {
        let widget = self.widget.acquire(pa);
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
        self.widget.read(pa, Widget::print_cfg)
    }

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
            searcher: RefCell::new(searcher),
        }
    }
}

// SAFETY: The only parts that are accessible from other threads are
// the atomic counters from the Arcs. Everything else can only be
// acquired when there is a Pass, i.e., on the main thread.
unsafe impl<W: Widget<U>, U: Ui, S> Send for Handle<W, U, S> {}
unsafe impl<W: Widget<U>, U: Ui, S> Sync for Handle<W, U, S> {}

impl<W: Widget<U> + ?Sized, U: Ui> Clone for Handle<W, U, ()> {
    fn clone(&self) -> Self {
        Self {
            widget: self.widget.clone(),
            area: self.area.clone(),
            mask: self.mask.clone(),
            searcher: self.searcher.clone(),
        }
    }
}
