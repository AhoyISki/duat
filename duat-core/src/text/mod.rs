//! The primary data structure in Duat
//!
//! This struct is responsible for all of the text that will be
//! printed to the screen, as well as any modifications on it.
//!
//! The [`Text`] is a very versatile holder for characters, below is a
//! list of some of its capabilities:
//!
//! - Be cheaply* edited at any point, due to its two [gap buffers];
//! - Be [colored] in any way, at any point;
//! - Have any arbitrary range concealed, that is, hidden from view,
//!   but still in there;
//! - Arbitrary [ghost text], that is, [`Text`] that shows up, but is
//!   not actually part of the [`Text`], i.e., it can be easily
//!   ignored by external modifiers (like an LSP or tree-sitter) of
//!   the file, without any special checks;
//! - [Left]/[right]/[center] alignment of output (although that is
//!   implemented by the [`Ui`]);
//! - [Spacers] for even more advanced alignment
//! - The ability to [undo]/[redo] changes in the history;
//! - In the future, button ranges that can interact with the mouse;
//!
//! The [`Text`] struct is created in two different ways:
//!
//! - By calling [`Text::new`] or one of its [`From`] implementations;
//! - By building it with the [`txt!`] macro;
//!
//! The first method is recommended if you want a [`Text`] that will
//! be modified by input. This is often the case if your [`Widget`] is
//! some sort of text box, chief of which is the [`File`], which is
//! the central [`Widget`] of every text editor.
//!
//! The second method is what should be used most of the time, as it
//! lets you quickly create formatted [`Widget`]s/[`StatusLine`] parts
//! in a very modular way:
//!
//! ```rust
//! # use duat_core::prelude::*;
//! fn number_of_horses(count: usize) -> Text {
//!     if count == 1 {
//!         txt!("[horses.count]1[horses] horse").build()
//!     } else {
//!         txt!("[horses.count]{}[horses] horses", count).build()
//!     }
//! }
//!
//! fn inlined_number_of_horses(count: usize) -> Text {
//!     txt!(
//!         "[horses.count]{count} [horses]{}",
//!         if count == 1 { "horse" } else { "horses" }
//!     )
//!     .build()
//! }
//! ```
//!
//! You can use this whenever you need to update a widget, for
//! example, just create a new [`Text`] to printed to the screen.
//!
//! However, when recreating the entire [`Text`] with a [`txt!`]
//! macro would be too expensive, you can use [`Text`] modifying
//! functions:
//!
//! ```rust
//! # use duat_core::prelude::*;
//! let mut prompted = txt!("[prompt]type a key: ").build();
//! let end = prompted.len();
//! prompted.replace_range(end..end, "a")
//! ```
//!
//! A general rule of thumb for "too expensive" is this: if your
//! [`Text`] can't scroll more than a few lines, it is not too
//! expensive to rebuild. This way of editing the [`Text`] is mostly
//! used on the [`File`] widget and other textbox-like [`Widget`]s.
//!
//! [Left]: AlignLeft
//! [right]: AlignRight
//! [center]: AlignCenter
//! [Spacers]: Spacer
//! [undo]: Text::undo
//! [redo]: Text::redo
//! [gap buffers]: gapbuf::GapBuffer
//! [colored]: crate::form::Form
//! [ghost text]: Ghost
//! [`Ui`]: crate::ui::Ui
//! [`File`]: crate::file::File
//! [`Widget`]: crate::ui::Widget
//! [`StatusLine`]: https://docs.rs/duat-utils/latest/duat_utils/widgets/struct.StatusLine.html
//! [`Mode`]: crate::mode::Mode
mod builder;
mod bytes;
mod history;
mod iter;
mod ops;
mod records;
mod search;
mod tags;

use std::{
    path::Path,
    rc::Rc,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

pub(crate) use self::history::History;
use self::tags::{FwdTags, RevTags, Tags};
pub use self::{
    builder::{Builder, BuilderPart, txt},
    bytes::{Buffers, Bytes, Lines, RefBytes, Strs},
    history::{Change, Moment},
    iter::{FwdIter, Item, Part, RevIter},
    ops::{Point, TextRange, TextRangeOrPoint, TwoPoints, utf8_char_width},
    search::{Matcheable, RegexPattern, Searcher},
    tags::{
        AlignCenter, AlignLeft, AlignRight, Conceal, ExtraCaret, FormTag, Ghost, GhostId,
        MainCaret, MutTags, RawTag, Spacer, Tag, Tagger, Taggers, ToggleId,
    },
};
use crate::{
    cfg::PrintCfg,
    context, form,
    mode::{Selection, Selections},
    ui::RawArea,
};

/// The text of a given [`Widget`]
///
/// The [`Text`] is the backbone of Duat. It is the thing responsible
/// for everything that shows up on screen.
///
/// You can build a [`Text`] manually, by using [`Text::new`], or with
/// some convenience, by using the [`txt!`] macro, making use of a
/// [`Builder`].
///
/// [`Widget`]: crate::ui::Widget
pub struct Text(Box<InnerText>);

struct InnerText {
    bytes: Bytes,
    tags: Tags,
    selections: Selections,
    // Specific to Files
    history: Option<History>,
    has_changed: bool,
    has_unsaved_changes: AtomicBool,
}

impl Text {
    ////////// Creation and Destruction of Text

    /// Returns a new empty [`Text`]
    pub fn new() -> Self {
        Self::from_bytes(Bytes::default(), Selections::new_empty(), false)
    }

    /// Returns a new empty [`Text`] with [`Selections`] enabled
    pub fn new_with_selections() -> Self {
        Self::from_bytes(
            Bytes::default(),
            Selections::new(Selection::default()),
            false,
        )
    }

    /// Returns a new empty [`Text`] with history enabled
    pub(crate) fn new_with_history() -> Self {
        Self::from_bytes(
            Bytes::default(),
            Selections::new(Selection::default()),
            true,
        )
    }

    /// Creates a [`Text`] from a file's [path]
    ///
    /// [path]: Path
    pub(crate) fn from_file(
        bytes: Bytes,
        selections: Selections,
        path: impl AsRef<Path>,
        has_unsaved_changes: bool,
    ) -> Self {
        let selections = if let Some(selection) = selections.get_main()
            && let Some(_) = bytes.char_at(selection.caret())
        {
            selections
        } else {
            Selections::new(Selection::default())
        };

        let mut text = Self::from_bytes(bytes, selections, true);
        text.0
            .has_unsaved_changes
            .store(has_unsaved_changes, Ordering::Relaxed);

        if let Some(history) = context::load_cache(path.as_ref()) {
            text.0.history = Some(history);
        }

        text
    }

    /// Creates a [`Text`] from [`Bytes`]
    pub(crate) fn from_bytes(mut bytes: Bytes, selections: Selections, with_history: bool) -> Self {
        if bytes.buffers(..).next_back().is_none_or(|b| b != b'\n') {
            let end = bytes.len();
            bytes.apply_change(Change::str_insert("\n", end));
        }
        let tags = Tags::new(bytes.len().byte());

        Self(Box::new(InnerText {
            bytes,
            tags,
            selections,
            history: with_history.then(History::new),
            has_changed: false,
            has_unsaved_changes: AtomicBool::new(false),
        }))
    }

    /// Returns an empty [`Text`], only for [`Builder`]s
    fn empty() -> Self {
        Self(Box::new(InnerText {
            bytes: Bytes::default(),
            tags: Tags::new(0),
            selections: Selections::new_empty(),
            history: None,
            has_changed: false,
            has_unsaved_changes: AtomicBool::new(false),
        }))
    }

    /// Returns a [`Builder`] for [`Text`]
    ///
    /// This builder can be used to iteratively create text, by
    /// assuming that the user wants no* [`Tag`] overlap, and that
    /// they want to construct the [`Text`] in [`Tag`]/content pairs.
    ///
    /// ```rust
    /// use duat_core::prelude::*;
    /// let mut builder = Text::builder();
    /// ```
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Takes the [`Bytes`] from this [`Text`], consuming it
    pub(crate) fn take_bytes(self) -> Bytes {
        self.0.bytes
    }

    ////////// Querying functions

    /// Whether the [`Bytes`] and `Tags` are empty
    ///
    /// This ignores the last `'\n'` in the [`Text`], since it is
    /// always there no matter what.
    ///
    /// If you only want to check for the [`Bytes`], ignoring possible
    /// [`Ghost`]s, see [`is_empty`].
    ///
    /// [`is_empty`]: Bytes::is_empty
    pub fn is_empty_empty(&self) -> bool {
        self.0.bytes == "\n" && self.0.tags.is_empty()
    }

    /// The inner bytes of the [`Text`]
    ///
    /// Note that, since [`Text`] has an implementation of
    /// [`std::ops::Deref<Target = Bytes>`], you mostly don't need
    /// to call this method. Also, some things require partial
    /// mutability of the [`Bytes`], such as [regex searching] and
    /// [getting contiguous `&str`s]. For those, see
    /// [`Text::ref_bytes`].
    ///
    /// [regex searching]: Bytes::search_fwd
    /// [getting contiguous `&str`s]: Bytes::contiguous
    pub fn bytes(&self) -> &Bytes {
        &self.0.bytes
    }

    /// The parts that make up a [`Text`]
    ///
    /// This function is used when you want to [insert]/[remove]
    /// [`Tag`]s (i.e., borrow the inner `Tags` mutably via
    /// [`MutTags`]), while still being able to read from the
    /// [`Bytes`] (through [`RefBytes`]) and the [`Selections`].
    ///
    /// [insert]: MutTags::insert
    /// [remove]: MutTags::remove
    /// [`&mut Bytes`]: Bytes
    pub fn parts(&mut self) -> TextParts<'_> {
        TextParts {
            bytes: RefBytes(&mut self.0.bytes),
            tags: MutTags(&mut self.0.tags),
            selections: &self.0.selections,
        }
    }

    /// Gets the indentation level on the current line
    pub fn indent(&self, p: Point, area: &impl RawArea, cfg: PrintCfg) -> usize {
        let [start, _] = self.points_of_line(p.line());
        let t_iter = self.iter_fwd(start).no_ghosts().no_conceals();
        area.print_iter(t_iter, cfg.new_line_as('\n'))
            .filter_map(|(caret, item)| Some(caret).zip(item.part.as_char()))
            .find(|(_, char)| !char.is_whitespace() || *char == '\n')
            .map(|(caret, _)| caret.x as usize)
            .unwrap_or(0)
    }

    ////////// Tag related query functions

    /// The maximum [points] in the `at`th byte
    ///
    /// This point is essentially the [point] at that byte, plus the
    /// last possible [`Point`] of any [`Ghost`]s in that
    /// position.
    ///
    /// [points]: TwoPoints
    /// [point]: Bytes::point_at
    #[inline(always)]
    pub fn ghost_max_points_at(&self, at: usize) -> (Point, Option<Point>) {
        let point = self.point_at(at);
        (point, self.0.tags.ghosts_total_at(point.byte()))
    }

    /// The [points] at the end of the text
    ///
    /// This will essentially return the [last point] of the text,
    /// alongside the last possible [`Point`] of any
    /// [`Ghost`] at the end of the text.
    ///
    /// [points]: TwoPoints
    /// [last point]: Bytes::len
    pub fn len_points(&self) -> (Point, Option<Point>) {
        self.ghost_max_points_at(self.len().byte())
    }

    /// Points visually after the [`TwoPoints`]
    ///
    /// If the [`TwoPoints`] in question is concealed, treats the
    /// next visible character as the first character, and returns
    /// the points of the next visible character.
    ///
    /// This method is useful if you want to iterator reversibly
    /// right after a certain point, thus including the character
    /// of said point.
    pub fn points_after(&self, tp: impl TwoPoints) -> Option<(Point, Option<Point>)> {
        self.iter_fwd(tp)
            .filter_map(|item| item.part.as_char().map(|_| item.points()))
            .chain([self.len_points()])
            .nth(1)
    }

    /// The visual start of the line
    ///
    /// This point is defined not by where the line actually begins,
    /// but by where the last '\n' was located. For example, if
    /// [`Tag`]s create ghost text or omit text from multiple
    /// different lines, this point may differ from where in the
    /// [`Text`] the physical line actually begins.
    pub fn visual_line_start(&self, p: impl TwoPoints) -> (Point, Option<Point>) {
        let (real, ghost) = p.to_points();

        let mut iter = self.iter_rev((real, ghost)).peekable();
        let mut points = (real, ghost);
        while let Some(peek) = iter.peek() {
            match peek.part {
                Part::Char('\n') => {
                    return points;
                }
                Part::Char(_) => points = iter.next().unwrap().to_points(),
                _ => drop(iter.next()),
            }
        }

        points
    }

    /// Gets the [`Ghost`] of a given [`GhostId`]
    pub fn get_ghost(&self, id: GhostId) -> Option<&Text> {
        self.0.tags.get_ghost(id)
    }

    ////////// Modification functions

    /// Replaces a [range] in the [`Text`]
    ///
    /// # [`TextRange`] behavior:
    ///
    /// If you give a single [`usize`]/[`Point`], it will be
    /// interpreted as a range from.
    ///
    /// [range]: TextRange
    pub fn replace_range(&mut self, range: impl TextRange, edit: impl ToString) {
        let range = range.to_range(self.len().byte());
        let (start, end) = (self.point_at(range.start), self.point_at(range.end));
        let change = Change::new(edit, [start, end], self);

        self.0.has_changed = true;
        self.apply_change_inner(0, change.as_ref());
        self.0
            .history
            .as_mut()
            .map(|h| h.apply_change(None, change));
    }

    pub(crate) fn apply_change(
        &mut self,
        guess_i: Option<usize>,
        change: Change,
    ) -> (Option<usize>, usize) {
        self.0.has_changed = true;

        let selections_taken = self.apply_change_inner(guess_i.unwrap_or(0), change.as_ref());
        let history = self.0.history.as_mut();
        let insertion_i = history.map(|h| h.apply_change(guess_i, change));
        (insertion_i, selections_taken)
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace
    fn apply_change_inner(&mut self, guess_i: usize, change: Change<&str>) -> usize {
        self.0.bytes.apply_change(change);
        self.0.tags.transform(
            change.start().byte()..change.taken_end().byte(),
            change.added_end().byte(),
        );

        *self.0.has_unsaved_changes.get_mut() = true;
        self.0.selections.apply_change(guess_i, change)
    }

    fn without_last_nl(mut self) -> Self {
        let change = Change::remove_last_nl(self.len());
        self.apply_change_inner(0, change);
        self
    }

    /// Updates bounds, so that [`Tag`] ranges can visibly cross the
    /// screen
    ///
    /// This is used in order to allow for very long [`Tag`] ranges
    /// (say, a [`Form`] being applied on the range `3..999`) to show
    /// up properly without having to lookback a bazillion [`Tag`]s
    /// which could be in the way.
    ///
    /// [`Form`]: crate::form::Form
    pub fn update_bounds(&mut self) {
        self.0.tags.update_bounds();
    }

    /// Inserts a [`Text`] into this [`Text`], in a specific [`Point`]
    pub fn insert_text(&mut self, p: Point, mut text: Text) {
        let insert = if p.byte() == 1 && self.0.bytes == "\n" {
            let change = Change::new(text.0.bytes.contiguous(..), [Point::default(), p], self);
            self.apply_change_inner(0, change.as_ref());
            Point::default()
        } else {
            let change = Change::str_insert(text.0.bytes.contiguous(..), p);
            self.apply_change_inner(0, change);
            p
        };

        if insert == self.len() {
            self.0.tags.extend(text.0.tags);
        } else {
            self.0.tags.insert_tags(insert, text.0.tags);
        }
    }

    ////////// History functions

    /// Undoes the last moment, if there was one
    pub fn undo(&mut self) {
        let mut history = self.0.history.take();

        if let Some(history) = history.as_mut()
            && let Some(moment) = history.move_backwards()
        {
            self.apply_and_process_changes(moment);
            self.0.has_changed = true;
        }

        self.0.history = history;
    }

    /// Redoes the last moment in the history, if there is one
    pub fn redo(&mut self) {
        let mut history = self.0.history.take();

        if let Some(history) = history.as_mut()
            && let Some(moment) = history.move_forward()
        {
            self.apply_and_process_changes(moment);
            self.0.has_changed = true;
        }

        self.0.history = history;
    }

    /// Finishes the current moment and adds a new one to the history
    pub fn new_moment(&mut self) {
        if let Some(h) = self.0.history.as_mut() {
            h.new_moment()
        }
    }

    /// Returns a [`Moment`] containing all [`Change`]s since the last
    /// call to this function
    ///
    /// This is useful if you want to figure out what has changed
    /// after a certain period of time has passed.
    pub fn unprocessed_moments(&self) -> Option<Vec<Moment>> {
        self.0.history.as_ref().map(|h| h.unprocessed_moments())
    }

    fn apply_and_process_changes(&mut self, moment: Moment) {
        self.0.selections.clear();

        for (i, change) in moment.changes().enumerate() {
            self.apply_change_inner(0, change);

            let start = change.start();
            let added_end = match change.added_str().chars().next_back() {
                Some(last) => change.added_end().rev(last),
                None => change.start(),
            };

            let selection = Selection::new(added_end, (start != added_end).then_some(start));
            self.0
                .selections
                .insert(i, selection, i == moment.len() - 1);
        }
    }

    ////////// Writing functions

    /// Clones the inner [`Bytes`] as a [`String`]
    ///
    /// This function will also cut out a final '\n' from the string.
    // NOTE: Inherent because I don't want this to implement Display
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(&self) -> String {
        let [s0, s1] = self.strs(..).to_array();
        if !s1.is_empty() {
            s0.to_string() + s1.strip_suffix('\n').unwrap_or(s1)
        } else {
            s0.strip_suffix('\n').unwrap_or(s0).to_string()
        }
    }

    /// Writes the contents of this [`Text`] to a [writer]
    ///
    /// [writer]: std::io::Write
    pub fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<usize> {
        self.0.has_unsaved_changes.store(false, Ordering::Relaxed);
        let [s0, s1] = self.0.bytes.buffers(..).to_array();
        Ok(writer.write(s0)? + writer.write(s1)?)
    }

    /// Wether or not the content has changed since the last [write]
    ///
    /// Returns `true` only if the actual bytes of the [`Text`] have
    /// been changed, ignoring [`Tag`]s and all the other things,
    /// since those are not written to the filesystem.
    ///
    /// [write]: Text::write_to
    pub fn has_unsaved_changes(&self) -> bool {
        self.0.has_unsaved_changes.load(Ordering::Relaxed)
    }

    ////////// Tag addition/deletion functions

    /// Inserts a [`Tag`] at the given position
    pub fn insert_tag<R>(&mut self, tagger: Tagger, r: R, tag: impl Tag<R>) {
        self.0.tags.insert(tagger, r, tag);
    }

    /// Removes the [`Tag`]s of a [key] from a region
    ///
    /// # Caution
    ///
    /// While it is fine to do this on your own widgets, you should
    /// refrain from using this function in a [`File`]s [`Text`], as
    /// it must iterate over all tags in the file, so if there are a
    /// lot of other tags, this operation may be slow.
    ///
    /// # [`TextRange`] behavior
    ///
    /// If you give it a [`Point`] or [`usize`], it will be treated as
    /// a one byte range.
    ///
    /// [key]: Taggers
    /// [`File`]: crate::file::File
    pub fn remove_tags(&mut self, taggers: impl Taggers, range: impl TextRangeOrPoint) {
        let range = range.to_range(self.len().byte());
        self.0.tags.remove_from(taggers, range)
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`File`]s, as there may be
    /// other [`Tag`] providers, and you should avoid messing with
    /// their tags.
    ///
    /// [`File`]: crate::file::File
    pub fn clear_tags(&mut self) {
        self.0.tags = Tags::new(self.0.bytes.len().byte());
    }

    /////////// Selection functions

    /// Returns a [`Text`] without [`Selections`]
    ///
    /// You should use this if you want to send the [`Text`] across
    /// threads.
    pub fn no_selections(mut self) -> Selectionless {
        self.0.selections.clear();
        Selectionless(self)
    }

    /// Removes the tags for all the selections, used before they are
    /// expected to move
    pub(crate) fn add_selections(&mut self, area: &impl RawArea, cfg: PrintCfg) {
        let within = (self.0.selections.len() >= 500).then(|| {
            let (start, _) = area.start_points(self, cfg);
            let (end, _) = area.end_points(self, cfg);
            (start, end)
        });

        let mut add_selection = |selection: &Selection, bytes: &mut Bytes, is_main: bool| {
            let (caret, selection) = selection.tag_points(bytes);

            let key = Tagger::for_selections();
            let form = if is_main {
                self.0.tags.insert(key, caret.byte(), MainCaret);
                form::M_SEL_ID
            } else {
                self.0.tags.insert(key, caret.byte(), ExtraCaret);
                form::E_SEL_ID
            };

            bytes.add_record([caret.byte(), caret.char(), caret.line()]);

            if let Some([start, end]) = selection {
                let range = start.byte()..end.byte();
                self.0.tags.insert(key, range, form.to_tag(99));
            }
        };

        if let Some((start, end)) = within {
            for (selection, is_main) in self.0.selections.iter() {
                let range = selection.range(&self.0.bytes);
                if range.end > start.byte() && range.start < end.byte() {
                    add_selection(selection, &mut self.0.bytes, is_main);
                }
            }
        } else {
            for (selection, is_main) in self.0.selections.iter() {
                add_selection(selection, &mut self.0.bytes, is_main);
            }
        }
    }

    /// Adds the tags for all the selections, used after they are
    /// expected to have moved
    pub(crate) fn remove_selections(&mut self) {
        self.remove_tags(Tagger::for_selections(), ..);
    }

    /////////// Iterator methods

    /// A forward iterator of the [chars and tags] of the [`Text`]
    ///
    /// [chars and tags]: Part
    pub fn iter_fwd(&self, at: impl TwoPoints) -> FwdIter<'_> {
        FwdIter::new_at(self, at)
    }

    /// A reverse iterator of the [chars and tags] of the [`Text`]
    ///
    /// [chars and tags]: Part
    pub fn iter_rev(&self, at: impl TwoPoints) -> RevIter<'_> {
        RevIter::new_at(self, at)
    }

    /// A forward iterator of the [`char`]s of the [`Text`]
    ///
    /// Each [`char`] will be accompanied by a [`Point`], which is the
    /// position where said character starts, e.g.
    /// [`Point::default()`] for the first character
    pub fn chars_fwd(&self, p: Point) -> impl Iterator<Item = (Point, char)> + '_ {
        self.0.bytes.chars_fwd(p)
    }

    /// A reverse iterator of the [`char`]s of the [`Text`]
    ///
    /// Each [`char`] will be accompanied by a [`Point`], which is the
    /// position where said character starts, e.g.
    /// [`Point::default()`] for the first character
    pub fn chars_rev(&self, p: Point) -> impl Iterator<Item = (Point, char)> + '_ {
        self.0.bytes.chars_rev(p)
    }

    /// A forward iterator over the [`Tag`]s of the [`Text`]
    ///
    /// This iterator will consider some [`Tag`]s before `b`, since
    /// their ranges may overlap with `b`
    ///
    /// # Note
    ///
    /// Duat works fine with [`Tag`]s in the middle of a codepoint,
    /// but external utilizers may not, so keep that in mind.
    pub fn tags_fwd(&self, b: usize) -> FwdTags<'_> {
        self.0.tags.fwd_at(b)
    }

    /// An reverse iterator over the [`Tag`]s of the [`Text`]
    ///
    /// This iterator will consider some [`Tag`]s ahead of `b`, since
    /// their ranges may overlap with `b`
    ///
    /// # Note
    ///
    /// Duat works fine with [`Tag`]s in the middle of a codepoint,
    /// but external utilizers may not, so keep that in mind.
    pub fn tags_rev(&self, b: usize) -> RevTags<'_> {
        self.0.tags.rev_at(b)
    }

    /// A forward [`Iterator`] over the [`RawTag`]s
    ///
    /// This [`Iterator`] does not take into account [`Tag`] ranges
    /// that intersect with the starting point, unlike
    /// [`Text::tags_fwd`]
    pub fn raw_tags_fwd(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> {
        self.0.tags.raw_fwd_at(b)
    }

    /// A reverse [`Iterator`] over the [`RawTag`]s
    ///
    /// This [`Iterator`] does not take into account [`Tag`] ranges
    /// that intersect with the starting point, unlike
    /// [`Text::tags_rev`]
    pub fn raw_tags_rev(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> {
        self.0.tags.raw_rev_at(b)
    }

    /// The [`Selections`] printed to this [`Text`], if they exist
    pub fn selections(&self) -> &Selections {
        &self.0.selections
    }

    /// A mut reference to this [`Text`]'s [`Selections`] if they
    /// exist
    pub fn selections_mut(&mut self) -> &mut Selections {
        &mut self.0.selections
    }

    pub(crate) fn history(&self) -> Option<&History> {
        self.0.history.as_ref()
    }

    ////////// One str functions

    /// Gets a single [`&str`] from a given [range]
    ///
    /// This is the equivalent of calling
    /// [`Bytes::make_contiguous`] and [`Bytes::get_contiguous`].
    /// While this takes less space in code, calling the other two
    /// functions means that you won't be mutably borrowing the
    /// [`Bytes`] anymore, so if that matters to you, you should do
    /// that.
    ///
    /// [`&str`]: str
    /// [range]: TextRange
    pub fn contiguous(&mut self, range: impl TextRange) -> &str {
        self.make_contiguous(range.clone());
        self.get_contiguous(range).unwrap()
    }

    /// Moves the [`GapBuffer`]'s gap, so that the `range` is whole
    ///
    /// The return value is the value of the gap, if the second `&str`
    /// is the contiguous one.
    ///
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn make_contiguous(&mut self, range: impl TextRange) {
        self.0.bytes.make_contiguous(range);
    }
}

impl std::ops::Deref for Text {
    type Target = Bytes;

    fn deref(&self) -> &Self::Target {
        self.bytes()
    }
}

impl Default for Text {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Text")
            .field("bytes", &self.0.bytes)
            .field("tags", &self.0.tags)
            .finish_non_exhaustive()
    }
}

impl Clone for Text {
    fn clone(&self) -> Self {
        Self(Box::new(InnerText {
            bytes: self.0.bytes.clone(),
            tags: self.0.tags.clone(),
            selections: self.0.selections.clone(),
            history: self.0.history.clone(),
            has_changed: self.0.has_changed,
            has_unsaved_changes: AtomicBool::new(false),
        }))
    }
}

impl From<std::io::Error> for Text {
    fn from(value: std::io::Error) -> Self {
        txt!("{}", value.kind().to_string()).build()
    }
}

impl From<Box<dyn std::error::Error>> for Text {
    fn from(value: Box<dyn std::error::Error>) -> Self {
        txt!("{}", value.to_string()).build()
    }
}

impl From<&std::path::PathBuf> for Text {
    fn from(value: &std::path::PathBuf) -> Self {
        let value = value.to_str().unwrap_or("");
        Self::from(value)
    }
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        self.0.bytes == other.0.bytes && self.0.tags == other.0.tags
    }
}

impl PartialEq<&str> for Text {
    fn eq(&self, other: &&str) -> bool {
        self.0.bytes == *other
    }
}

impl PartialEq<String> for Text {
    fn eq(&self, other: &String) -> bool {
        self.0.bytes == *other
    }
}

impl_from_to_string!(u8);
impl_from_to_string!(u16);
impl_from_to_string!(u32);
impl_from_to_string!(u64);
impl_from_to_string!(u128);
impl_from_to_string!(usize);
impl_from_to_string!(i8);
impl_from_to_string!(i16);
impl_from_to_string!(i32);
impl_from_to_string!(i64);
impl_from_to_string!(i128);
impl_from_to_string!(isize);
impl_from_to_string!(f32);
impl_from_to_string!(f64);
impl_from_to_string!(char);
impl_from_to_string!(&str);
impl_from_to_string!(String);
impl_from_to_string!(Box<str>);
impl_from_to_string!(Rc<str>);
impl_from_to_string!(Arc<str>);

/// Implements [`From<$t>`] for [`Text`]
macro impl_from_to_string($t:ty) {
    impl From<$t> for Text {
        fn from(value: $t) -> Self {
            let string = <$t as ToString>::to_string(&value);
            let bytes = Bytes::new(&string);
            Self::from_bytes(bytes, Selections::new_empty(), false)
        }
    }
}

/// A [`Text`] that is guaranteed not to have [`Selections`] in it
///
/// Useful for sending across threads, especially when it comes to
/// [`Logs`].
///
/// [`Logs`]: crate::context::Logs
#[derive(Clone, Debug)]
pub struct Selectionless(Text);

impl Selectionless {
    /// Gets the [`Text`] within, allowing for mutation again
    pub fn get(&self) -> Text {
        self.0.clone()
    }
}

impl std::ops::Deref for Selectionless {
    type Target = Text;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Selectionless> for Text {
    fn from(value: Selectionless) -> Self {
        value.0
    }
}

// SAFETY: This struct is defined by the lack of Selections, the only
// non Send/Sync part of a Text
unsafe impl Send for Selectionless {}
unsafe impl Sync for Selectionless {}

/// Either [`Text`] or [`Bytes`]
pub trait AsRefBytes {
    /// Returns a [`RefBytes`]
    fn ref_bytes(&mut self) -> RefBytes<'_>;
}

impl AsRefBytes for Bytes {
    /// Why are you using this?
    #[doc(hidden)]
    fn ref_bytes(&mut self) -> RefBytes<'_> {
        RefBytes(self)
    }
}

impl AsRefBytes for RefBytes<'_> {
    /// Duh
    #[doc(hidden)]
    fn ref_bytes(&mut self) -> RefBytes<'_> {
        RefBytes(self.0)
    }
}

impl AsRefBytes for Text {
    /// A [`RefBytes`] of the [`Bytes`] within
    ///
    /// This takes a mutable reference because [`RefBytes`] grants
    /// access to some methods that require "non altering mutation",
    /// such as [`contiguous`] and [`search_fwd`], which require
    /// that the gap of the inner [`GapBuffer`] be moved.
    ///
    /// [`contiguous`]: Bytes::contiguous
    /// [`search_fwd`]: Bytes::search_fwd
    /// [`GapBuffer`]: gapbuf::GapBuffer
    fn ref_bytes(&mut self) -> RefBytes<'_> {
        RefBytes(&mut self.0.bytes)
    }
}

/// The Parts that make up a [`Text`]
pub struct TextParts<'a> {
    /// The [`Bytes`] of the [`Text`]
    ///
    /// [`RefBytes`], unlike [`&Bytes`], permits [contiguous `&str`]s
    /// and [regex searching].
    ///
    /// [`&Bytes`]: Bytes
    /// [contiguous `&str`]: RefBytes::contiguous
    /// [regex searching]: RefBytes::search_fwd
    pub bytes: RefBytes<'a>,
    /// The [`MutTags`] of the [`Text`]
    ///
    /// This, unlike [`RefBytes`], allows mutation in the form of
    /// [adding] and [removing] [`Tag`]s.
    ///
    /// [adding]: MutTags::insert
    /// [removing]: MutTags::remove
    pub tags: MutTags<'a>,
    /// The [`Selections`] of the [`Text`]
    ///
    /// For most [`Widget`]s, there should be no [`Selection`], since
    /// they are just visual.
    ///
    /// [`Widget`]: crate::ui::Widget
    pub selections: &'a Selections,
}
