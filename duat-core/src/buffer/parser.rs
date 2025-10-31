//! Struct that can react to change in the [`Text`]
//!
//! These structs will be informed of every [`Change`] that happens in
//! the [`Text`], and are allowed to act accordingly. This action will
//! be done by telling the [`Text`] what parts need to be updated.
//! They will then be updated when deemed relevant by the [`Ui`] in
//! use (usually when these become visible).
//!
//! [`Ui`]: crate::ui::Ui
//! [`Change`]: crate::text::Change
use std::{
    any::TypeId,
    cell::RefCell,
    ops::Range,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use parking_lot::Mutex;

use super::Buffer;
use crate::{
    context::Handle,
    data::Pass,
    opts::PrintOpts,
    ranges::Ranges,
    text::{Bytes, Change, Moment, MomentFetcher, Point, Text, TextRange, txt},
};

/// A [`Buffer`] parser, that can keep up with every [`Change`] that
/// took place
///
/// A parser's purpose is generally to look out for changes to the
/// `Buffer`'s [`Bytes`], and update some internal state that
/// represents them. Examples of things that should be implemented as
/// `Parser`s are:
///
/// - A tree-sitter parser, or other syntax tree representations;
/// - Regex parsers;
/// - Language server protocols;
///
/// But `Parser`s don't have to necessarily do "big and complex"
/// things like creating a language tree, they can be more simple,
/// by, for example, acting on each [`Selection`] on the screen.
///
/// If you want a walkthrough on how to make a `Parser`, I would
/// recommend reading the book (TODO). The rest of the documentation
/// here is mostly just describing a final implementation, not
/// walking through its creation.
///
/// # What a parser does
///
/// The gist of it is that a `Parser` will be called to read the
/// `Bytes` of the `Buffer` as well as any [`Change`]s that are done
/// to said `Buffer`. Duat will then call upon the `Parser` to "act"
/// on a region of the `Buffer`'s [`Text`], this region being
/// determined by what is shown on screen, in order to help plugin
/// writers minimize the work done.
///
/// When creating a `Parser`, you will also be given a
/// [`BufferTracker`]. It will be used to keep track of the
/// [`Change`]s, and it is also used by the `Parser` to tell which
/// [`Range<usize>`]s of the [`Text`] the `Parser` cares about. So,
/// for example, if you're matching non-multiline regex patterns, for
/// every `Change`, you might want to add the lines of that `Change`
/// to the `BufferTracker`, and when Duat decides which ranges need to
/// be updated, it will inform you: "Hey, you asked for this range to
/// be updated, it's on screen now, so update it.".
///
/// # The functions from the `Parser` trait
///
/// There are 4 functions that you need to care about, but you may
/// choose to implement only some of them:
///
/// ## [`Parser::parse`]
///
/// This function's purpose is for the `Parser` to update its internal
/// state after [`Change`]s take place. Here's the _general_ layout
/// for a _synchronous_ version of this function:
///
/// ```rust
/// use duat_core::prelude::*;
///
/// struct CharCounter {
///     count: usize,
///     ch: char,
///     tracker: BufferTracker,
/// }
///
/// impl<U: Ui> Parser<U> for CharCounter {
///     fn parse(&mut self) -> bool {
///         // Fetches the latest Changes and Bytes of the Buffer
///         self.tracker.update();
///
///         // A Moment is a list of Changes
///         // For the sake of efficiency, Changes are sent in bulk,
///         // rather than individually
///         for change in self.tracker.moment().changes() {
///             let bef_count = change.taken_str().matches(self.ch).count();
///             let aft_count = change.taken_str().matches(self.ch).count();
///             self.count += aft_count - bef_count;
///         }
///
///         // Return true if you want to call `Parser::update`, for
///         // this Parser, since we never change the Buffer, it's fine
///         // to always return false.
///         false
///     }
/// }
/// ```
///
/// The example above just keeps track of every occurance of a
/// specific `char`. Every time the `Buffer` is updated, the `parse`
/// function will be called, and you can use [`BufferTracker::update`]
/// to be notified of _every_ `Change` that takes place in the
/// `Buffer`.
///
/// ## [`Parser::update`]
///
/// The purpose of this funcion is for the `Parser` to modify the
/// `Buffer` itself. In the previous funcion, you may notice that you
/// are not given access to the `Buffer` directly, nor are you given a
/// [`Pass`] in order to access global state. That's what this
/// function is for.
///
/// Below is the rough layout for an implementation of this function,
/// in this case, this function "resets its modifications" every time
/// it is called:
///
/// ```rust
/// use std::ops::Range;
///
/// use duat_core::prelude::*;
///
/// struct HighlightMatch {
///     _tracker: BufferTracker,
///     tagger: Tagger,
/// }
///
/// impl<U: Ui> Parser<U> for HighlightMatch {
///     fn update(&mut self, pa: &mut Pass, handle: &Handle<Buffer<U>, U>, on: Vec<Range<Point>>) {
///         // Remove all Tags previously added with self.tagger
///         handle.text_mut(pa).remove_tags(self.tagger, ..);
///
///         // Get the range of the main cursor's word.
///         let Some([s, e]) = handle.edit_main(pa, |c| c.search_fwd(r"\A\w+", None).next()) else {
///             return;
///         };
///         let s = handle
///             .edit_main(pa, |c| c.search_rev(r"\w*\z", None).next().map(|[s, _]| s))
///             .unwrap_or(s);
///         // Get a regex pattern for said range.
///         let pat = handle.text(pa).strs(s..e).unwrap().to_string();
///         let pat = format!(r"\b{pat}\b");
///
///         // Add the "same_word" Form to every range that matches the pattern.
///         let form_id = form::id_of!("same_word");
///         let mut parts = handle.text_parts(pa);
///         // The `on` list of ranges represents a the printed area,
///         // So no unnecessary Tags are added.
///         for range in on {
///             for [s, e] in parts.bytes.search_fwd(&pat, range.clone()).unwrap() {
///                 parts.tags.insert(self.tagger, s..e, form_id.to_tag(50));
///             }
///         }
///     }
/// }
/// ```
///
/// The `Parser` above reads the word under the main cursor (if there
/// is one) and highlights every ocurrence of said word _on screen_.
/// This function would be called if [`Parser::parse`] returns `true`,
/// i.e. when the `Parser` is "ready" to update the `Buffer`. The
/// default implementation of `Parser::parse` is to just return
/// `true`.
///
/// > [!IMPORTANT]
/// >
/// > In the example above, the [`BufferTracker`] is acting _slightly_
/// > differently. When setting up this `Parser` with a [`ParserCfg`],
/// > I called [`BufferTracker::track_area`]. This function makes it
/// > so,
/// > instead of tracking changed [`Range<Point>`]s,
/// > [`Parser::update`] will always return a list of ranges
/// > equivalent to the printed region of the [`Text`].
///
/// In general, given the [`Parser::parse`] and `Parser::update`
/// functions, you can roughly divide which ones you'll implement
/// based on the following criteria:
///
/// - If your `Parser` does not update the `Buffer`, and just keeps
///   track of [`Change`]s, e.g. a word counter, or a filetype
///   checker, etc, then you should only have to implement the
///   [`Parser::parse`] function.
/// - If your `Parser` actively updates the `Buffer` every time it is
///   printed, e.g. the word match finder above, or a current line
///   highlighter, then you should only have to implement the
///   [`Parser::update`] function.
/// - If, in order to update the [`Buffer`], you need to keep track of
///   some current state, and you may even update the `Parser`'s state
///   in other threads, like a treesitter parser for example, then you
///   should implement both.
///
/// ## [`Parser::before_get`] and [`Parser::before_try_get`]
///
/// These functions have the same purpose as [`Parser::parse`], but
/// they are called before calls to [`Buffer::read_parser`],
/// [`Buffer::write_parser`], and their [try equivalents].
///
/// They serve to kind of "prepare" the `Parser` for functions that
/// access it, much like [`Parser::parse`] "prepares" the `Parser` for
/// a call to [`Parser::update`].
///
/// The purpose of these functions is to only ever update the `Parser`
/// when that is actually necessary. The most notable example of this
/// is the [`duat-jump-list`] crate. That crate defines a `Parser`
/// that only ever updates its internal state when it is accessed
/// externally. The reason for that is because it is only used to
/// store and retrieve previous versions of the [`Selections`] of the
/// [`Buffer`], so it doesn't need to update itself _every time_ there
/// are new changes to the [`Buffer`], but only when it is requested.
///
/// > [!TIP]
/// >
/// > You can keep a `Parser` private in your plugin in order to
/// > prevent the end user from reading or writing to it. You can
/// > then create standalone functions or implement traits on the
/// > [`Buffer`] widget in order to give controled access to the
/// > parser. For an example of this, you can see the
/// > [`duat-jump-list`] crate, which defines traits for saving and
/// > retrieving jumps, but doesn't grant direct access to the parser.
///
/// [`Change`]: crate::text::Change
/// [`Selection`]: crate::mode::Selection
/// [try equivalents]: Buffer::try_read_parser
/// [`duat-jump-list`]: https://github.com/AhoyISki/duat-jump-list
#[allow(unused_variables)]
pub trait Parser: Send + 'static {
    /// Parses the [`Bytes`] of the [`Buffer`]
    ///
    /// This function is called every time the [`Buffer`] is updated,
    /// and it's where you should update the internal state of the
    /// [`Parser`] to reflect any [`Change`]s that took place.
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Change`]: crate::text::Change
    /// [`Buffer`]: crate::buffer::Buffer
    /// [add]: Ranges::add
    /// [remove]: Ranges::remove
    /// [`update`]: Parser::update
    fn parse(&mut self) -> bool {
        true
    }

    /// Updates the [`Buffer`] in some given [`Range<Point>`]s
    ///
    /// As this function is called, the state of the [`Parser`] needs
    /// to already be synced up with the latest [`Change`]s to the
    /// [`Buffer`].
    ///
    /// The list of [`Range`]s is the collection of [`Range`]s that
    /// were requested to be updated and are within the printed region
    /// of the [`Buffer`].
    ///
    /// Do note that, if more regions become visible on the screen
    /// (this could happen if a [`Conceal`] tag is placed, for
    /// example), this function will be called again, until the whole
    /// screen has been parsed by every [`Parser`]
    ///
    /// # NOTES
    ///
    /// One other thing to note is that the `on` [range] is just a
    /// suggestion. In most circumstances, it would be a little
    /// convenient to go slightly over that range. For example, a
    /// regex searcher should look only at the range provided, but if
    /// a match goes slightly beyond the [range], it is fine to add
    /// [`Tag`]s in there.
    ///
    /// Finally, keep in mind that [`Tag`]s are not allowed to be
    /// repeated, and you can use this to your advantage, as in,
    /// instead of checking if you need to place a [`Tag`] in a
    /// certain spot, you can just place it, and Duat will ignore that
    /// request if that [`Tag`] was already there.
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`Change`]: crate::text::Change
    /// [range]: std::ops::Range
    /// [`parse`]: Parser::parse
    /// [`Conceal`]: crate::text::Conceal
    fn update(&mut self, pa: &mut Pass, buffer: &Handle, on: Vec<Range<Point>>) {}

    /// Prepare this [`Parser`] before [`Buffer::read_parser`] call
    ///
    /// The [`Buffer::read_parser`]/[`Buffer::write_parser`] functions
    /// block the current thread until the [`Parser`] is
    /// available. Therefore, [`before_get`] should finish _all_
    /// parsing, so if the parsing is taking place in
    /// another thread, you're gonna want to join said thread and
    /// finish it.
    ///
    /// If the [`Parser`]'s availability doesn't rely on other threads
    /// (which should be the case for almost every single [`Parser`]),
    /// then this function can just be left empty.
    ///
    /// [`before_get`]: Parser::before_get
    fn before_get(&mut self) {
        self.parse();
    }

    /// Prepare the [`Parser`] before [`Buffer::try_read_parser`] call
    ///
    /// The purpose of [`try_read_parser`], unlike [`read_parser`], is
    /// to _only_ call the function passed if the [`Parser`] is ready
    /// to be read. If it relies on a thread finishing its processing,
    /// this function should return `true` _only_ if said thread is
    /// ready to be merged.
    ///
    /// If the [`Parser`]'s availability doesn't rely on other threads
    /// (which should be the case for almost every single [`Parser`]),
    /// then this function should just return `true` all the time.
    ///
    /// [`try_read_parser`]: Buffer::try_read_parser
    /// [`read_parser`]: Buffer::read_parser
    fn before_try_get(&mut self) -> bool {
        self.before_get();
        true
    }
}

#[derive(Default)]
pub(super) struct Parsers {
    list: RefCell<Vec<ParserParts>>,
}

impl Parsers {
    /// Attempts to add  a [`Parser`]
    pub(super) fn add<P: Parser>(
        &self,
        buffer: &Buffer,
        f: impl FnOnce(BufferTracker) -> P,
    ) -> Result<(), Text> {
        let mut parsers = self.list.borrow_mut();
        if parsers.iter().any(|rb| rb.ty == TypeId::of::<P>()) {
            Err(txt!(
                "There is already a parser of type [a]{}",
                crate::utils::duat_name::<P>()
            ))?;
        }

        let update_requested = Arc::new(AtomicBool::new(false));
        let ranges = Arc::new(Mutex::new(RangesTracker::Manual(Ranges::new(
            0..buffer.bytes().len().byte(),
        ))));

        let tracker = BufferTracker {
            bytes: buffer.bytes().clone(),
            opts: buffer.sync_opts.clone(),
            fetcher: buffer.text.history().unwrap().new_fetcher(),
            moment: Moment::default(),
            ranges: ranges.clone(),
            update_requested: update_requested.clone(),
        };

        parsers.push(ParserParts {
            parser: Some(Box::new(f(tracker))),
            ranges: ranges.clone(),
            update_requested,
            ty: TypeId::of::<P>(),
        });

        Ok(())
    }

    /// Reads a specific [`Parser`]
    pub(super) fn read_parser<P: Parser, Ret>(&self, read: impl FnOnce(&P) -> Ret) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;

            parser.before_get();

            let ptr: *const dyn Parser = &*parser;
            let ret = read(unsafe { (ptr as *const P).as_ref() }.unwrap());

            self.list.borrow_mut()[i].parser = Some(parser);

            Some(ret)
        } else {
            None
        }
    }

    /// Tries to read from a specific [`Parser`]. Fails if it is not
    /// available
    pub(super) fn try_read_parser<P: Parser, Ret>(
        &self,
        read: impl FnOnce(&P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;

            let ptr: *const dyn Parser = &*parser;
            let ret = parser
                .before_try_get()
                .then(|| read(unsafe { (ptr as *const P).as_ref() }.unwrap()));

            self.list.borrow_mut()[i].parser = Some(parser);

            ret
        } else {
            None
        }
    }

    /// Writes to a specific [`Parser`]
    pub(super) fn write_parser<P: Parser, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;

            parser.before_get();

            let ptr: *const dyn Parser = &*parser;
            let ret = write(unsafe { (ptr as *mut P).as_mut() }.unwrap());

            self.list.borrow_mut()[i].parser = Some(parser);

            Some(ret)
        } else {
            None
        }
    }

    /// Tries to write to a specific [`Parser`]. Fails if it is not
    /// available
    pub(super) fn try_write_parser<P: Parser, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;

            let ptr: *const dyn Parser = &*parser;
            let ret = parser
                .before_try_get()
                .then(|| write(unsafe { (ptr as *mut P).as_mut() }.unwrap()));

            self.list.borrow_mut()[i].parser = Some(parser);

            ret
        } else {
            None
        }
    }

    /// Updates the [`Parser`]s on a given range
    // TODO: Deal with reparsing if Changes took place.
    pub(super) fn update(&self, pa: &mut Pass, handle: &Handle, on: Range<usize>) {
        let len = self.list.borrow().len();
        for i in 0..len {
            let mut parts = self.list.borrow_mut().remove(i);
            let parser = parts.parser.as_mut().unwrap();

            if parser.parse() {
                let ranges_to_update = parts
                    .ranges
                    .lock()
                    .remove(on.clone(), handle.read(pa).bytes());

                parser.update(pa, handle, ranges_to_update)
            }

            parts.update_requested.store(false, Ordering::Relaxed);

            self.list.borrow_mut().insert(i, parts)
        }
    }

    /// Wether this [`Parsers`] is ready to be updated
    pub fn needs_update(&self) -> bool {
        let parsers = self.list.borrow();

        parsers
            .iter()
            .any(|parts| parts.update_requested.load(Ordering::Relaxed))
    }
}

/// Things related to an individual [`Parser`]
struct ParserParts {
    parser: Option<Box<dyn Parser>>,
    ranges: Arc<Mutex<RangesTracker>>,
    update_requested: Arc<AtomicBool>,
    ty: TypeId,
}

/// A tracker for [`Change`]s that happen to a [`Buffer`]
///
/// [`Change`]: crate::text::Change
#[derive(Debug)]
pub struct BufferTracker {
    bytes: Bytes,
    opts: Arc<Mutex<PrintOpts>>,
    fetcher: MomentFetcher,
    moment: Moment,
    ranges: Arc<Mutex<RangesTracker>>,
    update_requested: Arc<AtomicBool>,
}

impl BufferTracker {
    /// Updates the inner [`Bytes`] and retrieves latest [`Moment`]
    ///
    /// [`Change`]: crate::text::Change
    pub fn update(&mut self) {
        self.moment = self.fetcher.get_moment();
        for change in self.moment.changes() {
            self.bytes.apply_change(change);
            self.ranges.lock().apply_change(change, &self.bytes);
        }
    }

    /// Request for the [`Buffer`] to call [`Parser::parse`]
    ///
    /// This will prompt the [`Buffer`] to be updated and only update
    /// relevant [`Range`]s, i.e., those that actually show up on
    /// area and that have been added by to the [`BufferTracker`] via
    /// [`add_range`].
    ///
    /// [`add_range`]: Self::add_range
    pub fn request_parse(&self) {
        self.update_requested.store(true, Ordering::Relaxed);
    }

    ////////// Range tracking functions

    /// Adds a byte/[`Point`] [`Range`] to be updated on
    /// [`Parser::update`]
    ///
    /// This [`Range`] will be merged with other [`Ranges`] on the
    /// list, and when a [parse is requested], the intersections
    /// between the [`Ranges`] sent to this method and the [`Range`]
    /// of the [`Text`] shown on area will be updated.
    ///
    /// For example, if you add the ranges  `3..20` and `50..87`, and
    /// the range shown on area is `17..61`, then two calls to
    /// [`Parser::update`] will be made, one with the range
    /// `17..20` and one with the range `50..61`. After that, the
    /// ranges `3..17` and `61..87` will remain on the list of
    /// [`Range`]s to be updated
    ///
    /// [parse is requested]: Self::request_parse
    pub fn add_range(&mut self, range: impl TextRange) {
        let range = range.to_range(self.bytes.len().byte());
        self.ranges.lock().add_range(range);
    }

    /// Same as [`add_range`], but add many [`Range`]s at once
    ///
    /// [`add_range`]: Self::add_range
    pub fn add_ranges<R: TextRange>(&mut self, new_ranges: impl IntoIterator<Item = R>) {
        let mut ranges = self.ranges.lock();
        for range in new_ranges {
            let range = range.to_range(self.bytes.len().byte());
            ranges.add_range(range);
        }
    }

    /// Automatically add every [`Change`]'s [added range] to update
    ///
    /// This function turns on automatic [`Range`] addition for every
    /// [`Change`] that takes place. You can still add [`Range`]s
    /// manually through [`add_range`].
    ///
    /// [added range]: Change::added_range
    /// [`add_range`]: BufferTracker::add_range
    pub fn track_changed_ranges(&mut self) {
        let mut tracker = self.ranges.lock();
        *tracker = RangesTracker::ChangedRanges(tracker.take_ranges());
    }

    /// Automatically add every changed line to the ranges to update
    ///
    /// This function turns on automatic [`Range`] addition for every
    /// line that a [`Change`]'s [added range] belongs to. You can
    /// still add [`Range`]s manually through [`add_range`].
    ///
    /// It's probably the most common type of automatic range tracking
    /// that you'd want, since, for example, regex patterns are very
    /// convenient to separate line-wise, since most of them don't
    /// tend to go over lines.
    ///
    /// Additionally, this option will also make it so only _whole
    /// lines_ are updated. For example, if a line is partially cutoff
    /// from the area, the range passed to [`Parser::update`] will
    /// include the parts of the line that are outside of the area.
    /// For the other tracking options, this won't be the case.
    ///
    /// [added range]: Change::added_range
    /// [`add_range`]: BufferTracker::add_range
    pub fn track_changed_lines(&mut self) {
        let mut tracker = self.ranges.lock();
        *tracker = RangesTracker::ChangedLines(tracker.take_ranges());
    }

    /// Always update the whole area
    ///
    /// This function basically turns off [`Range`] tracking and
    /// assumes that you want to update the whole printed area.
    ///
    /// One very common usecase of this is to do with parsing the
    /// [`Selections`]. You may want to, for example, do something for
    /// every selection that is visible on screen, maybe something
    /// like this:
    ///
    /// ```rust
    /// use std::ops::Range;
    ///
    /// use duat_core::prelude::*;
    ///
    /// // When construction the SelectionLen, I turned on area tracking
    /// struct SelectionLen {
    ///     _tracker: BufferTracker,
    ///     tagger: Tagger,
    /// }
    ///
    /// impl Parser for SelectionLen {
    ///     fn update(&mut self, pa: &mut Pass, buffer: &Handle, on: Vec<Range<Point>>) {
    ///         let mut parts = buffer.text_mut(pa).parts();
    ///         // This is efficient even in very large `Text`s.
    ///         parts.tags.remove(self.tagger, ..);
    ///
    ///         for range in on {
    ///             for (_, selection, is_main) in parts.selections.iter_within(range) {
    ///                 let range = selection.byte_range(&parts.bytes);
    ///                 let (start, end) = (range.start, range.end);
    ///                 let len = end - start;
    ///
    ///                 if len > 2 {
    ///                     let nums = len.ilog10() as usize + 1;
    ///                     let ghost = Ghost(if is_main {
    ///                         txt!("[sel_len.main]{len}")
    ///                     } else {
    ///                         txt!("[sel_len]{len}")
    ///                     });
    ///
    ///                     parts.tags.insert(self.tagger, start, ghost);
    ///                     parts.tags.insert(self.tagger, start..start + nums, Conceal);
    ///                 }
    ///             }
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// This function will make it so, for every visible
    /// [`Selection`], its length in bytes will be displayed within
    /// that selection with the `"sel_len"` [`Form`].
    ///
    /// > [!NOTE]
    /// >
    /// > This function is actually incorrect. It doesn't take into
    /// > account the possibility of intercepting newlines or the
    /// > number being outside the printed area. The corrected version
    /// > would be much too long for a simple example.
    ///
    /// [`Selection`]: crate::mode::Selection
    /// [`Selections`]: crate::mode::Selections
    /// [`Form`]: crate::form::Form
    pub fn track_area(&mut self) {
        *self.ranges.lock() = RangesTracker::Area;
    }

    /// Disable the automatic [`Change`] tracking functionality
    ///
    /// This makes it so no [`Range`]s to update are added
    /// automatically whenever a [`Change`] takes place. Instead, they
    /// all must be added through [`add_range`].
    ///
    /// This is the default method of tracking [`Change`]s.
    ///
    /// [`add_range`]: BufferTracker::add_range
    pub fn turn_off_tracking(&mut self) {
        let mut tracker = self.ranges.lock();
        *tracker = RangesTracker::Manual(tracker.take_ranges());
    }

    ////////// Querying functions

    /// The [`Bytes`], as they are since the last call to [`update`]
    ///
    /// Do note that, because this tracks calls to [`update`], this
    /// version of the [`Bytes`] may not reflect what the [`Bytes`]
    /// look like in the [`Buffer`] _right now_, as more [`Change`]s
    /// could have taken place since the last call.
    ///
    /// [`update`]: Self::update
    /// [`Change`]: crate::text::Change
    pub fn bytes(&self) -> &Bytes {
        &self.bytes
    }

    /// The last [`Moment`] that was processed
    ///
    /// This is **not** the last [`Moment`] sent to the [`Buffer`],
    /// nor does it necessarily correspond to a [`Moment`] in the
    /// [`Text`]'s history. It's just a collection of [`Change`]s that
    /// took place in between calls to [`Self::update`].
    ///
    /// [`Change`]: crate::text::Change
    pub fn moment(&self) -> &Moment {
        &self.moment
    }

    /// The indentation in the line at a [`Point`]
    ///
    /// This value only takes ascii spaces and tabs into account,
    /// which may differ from the value from [`Text::indent`],
    /// since that one calculates the indent through the [`Area`],
    /// while this one only makes use of the [`PrintOpts`]'s
    /// [`TabStops`].
    ///
    /// [`Area`]: crate::ui::Area
    /// [`TabStops`]: crate::opts::TabStops
    pub fn indent(&self, p: Point) -> usize {
        self.bytes.indent(p, *self.opts.lock())
    }

    /// The [`PrintOpts`] of the [`Buffer`]
    ///
    /// Unlike the other parts of this struct, the [`PrintOpts`] will
    /// always be up to date with what it currently is in the
    /// [`Buffer`]
    pub fn opts(&self) -> PrintOpts {
        *self.opts.lock()
    }
}

/// Tracker for which [`Range`]s to update
#[derive(Debug)]
enum RangesTracker {
    Manual(Ranges),
    ChangedRanges(Ranges),
    ChangedLines(Ranges),
    Area,
}

impl RangesTracker {
    /// Manually adds a [`Range`] to be tracked
    fn add_range(&mut self, range: Range<usize>) {
        match self {
            RangesTracker::Manual(ranges)
            | RangesTracker::ChangedRanges(ranges)
            | RangesTracker::ChangedLines(ranges) => ranges.add(range),
            RangesTracker::Area => {}
        }
    }

    /// Applies a [`Change`] to the [`Range`]s within
    fn apply_change(&mut self, change: Change<&str>, bytes: &Bytes) {
        match self {
            RangesTracker::Manual(ranges) => {
                ranges.shift_by(change.start().byte(), change.shift()[0])
            }
            RangesTracker::ChangedRanges(ranges) => {
                ranges.shift_by(change.start().byte(), change.shift()[0]);
                ranges.add(change.start().byte()..change.added_end().byte())
            }
            RangesTracker::ChangedLines(ranges) => {
                ranges.shift_by(change.start().byte(), change.shift()[0]);
                let start = bytes.point_at_line(change.start().line());
                let end =
                    bytes.point_at_line((change.added_end().line() + 1).min(bytes.len().line()));
                ranges.add(start.byte()..end.byte());
            }
            RangesTracker::Area => {}
        }
    }

    /// Takes the [`Ranges`] from the tracker
    fn take_ranges(&mut self) -> Ranges {
        match self {
            RangesTracker::Manual(ranges)
            | RangesTracker::ChangedRanges(ranges)
            | RangesTracker::ChangedLines(ranges) => std::mem::take(ranges),
            RangesTracker::Area => Ranges::empty(),
        }
    }

    /// Gets the list of [`Range`]s that need to be updated
    fn remove(&mut self, range: Range<usize>, bytes: &Bytes) -> Vec<Range<Point>> {
        match self {
            RangesTracker::Manual(ranges)
            | RangesTracker::ChangedRanges(ranges)
            | RangesTracker::ChangedLines(ranges) => ranges
                .remove(range)
                .map(|r| bytes.point_at_byte(r.start)..bytes.point_at_byte(r.end))
                .collect(),
            RangesTracker::Area => {
                vec![bytes.point_at_byte(range.start)..bytes.point_at_byte(range.end)]
            }
        }
    }
}

/// Wether the type of the [`ParserParts`]'s parser is `P`
fn type_eq<P: 'static>(parts: &ParserParts) -> bool {
    parts.ty == TypeId::of::<P>()
}
