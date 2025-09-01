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

use super::File;
use crate::{
    cfg::PrintCfg,
    context::Handle,
    data::Pass,
    prelude::Ranges,
    text::{Bytes, Change, Moment, MomentFetcher, Point, Text, TextRange, txt},
    ui::Ui,
};

/// A [`File`] parser, that can keep up with every [`Change`] that
/// took place
///
/// A parser's purpose is generally to look out for changes to the
/// [`File`]'s [`Bytes`], and update some internal state that
/// represents them. Examples of things that should be implemented as
/// [`Parser`]s are:
///
/// - A tree-sitter parser, or other syntax tree representations as
///   well;
/// - Regex parsers;
/// - Language server protocols;
///
/// But [`Parser`]s don't have to necessarily do "big and complex"
/// things like creating a language tree, they can be more simple,
/// by, for example, acting on each [`Selection`] on the screen.
///
/// If you want a walkthrough on how to make a [`Parser`], I would
/// recommend reading the book (TODO). The rest of the documentation
/// here is mostly just describing a final implementation, not
/// walking through its creation.
///
/// # Parsing strategies
///
/// From the [`FileTracker`] that is given to the [`ParserCfg`] when
/// calling [`ParserCfg::build`], three main parsing strategies
/// emerge:
///
/// - Not parsing at all;
/// - Parsing synchronously;
/// - Parsing asynchronously (via threads, not async);
///
/// You should reach out for these in the following circumstances:
///
/// - If you don't care about [`Change`]s, then you can be doing "no
///   parsing";
/// - If you care about [`Change`]s, you should almost always do
///   synchronous parsing, since multithreading is often not really
///   faster than using single threaded code;
/// - If you care about [`Change`]s and you know that your [`Parser`]
///   can be _really_ slow (very unlikely), then you should use
///   asynchronous parsing;
///
/// Keep in mind that the picked strategy could change, depending
/// on the circumstance.
///
/// ## No parsing
///
/// TODO: Document this
///
/// ## Synchronous parsing
///
/// Synchronous parsing is done by calling the
/// [`FileTracker::update`] function _once_ per call to
/// [`Parser::parse`]. If you don't continue parsing on another
/// thread, it is guaranteed that the [`File`] will not change
/// until the next call to [`Parser::parse`], so
/// [`FileTracker::bytes`] is equal to the [`File`]'s [`Bytes`].
///
/// Generally, this is the rough structure of synchronous parsing
///
/// ```rust
/// use duat_core::prelude::*;
///
/// struct MyParser {
///     tracker: FileTracker,
///     // Other fields.
/// };
///
/// impl MyParser {
///     /// this function represents some update to the internal
///     /// state of the Parser, in order to reflect the Changes
///     /// that took place.
///     fn parse_bytes_and_moment(&mut self, pa: &Pass, bytes: &Bytes, moment: &Moment) {
///         todo!();
///     }
///
///     /// In this function, you add Ranges to be updated when
///     /// calling Parser::update. This is done because Parsers
///     /// should try to only update the things that are actually
///     /// visible on screen, in order to not slow the whole
///     /// program down.
///     ///
///     /// If you want more information about this, you can check
///     /// out the documentation for Parser::update.
///     fn add_ranges(&mut self, bytes: &Bytes, moment: &Moment) {
///         todo!();
///     }
///
///     /// this function should make the requested changes to the
///     /// File, like adding Tags, inserting or removing text,
///     /// resizing the Area, etc.
///     ///
///     /// The Range argument is a soft limit on where you should
///     /// limit those changes to. For example, if you are adding
///     /// Tags on every instance of the word "the", you should
///     /// limit the search to the given range.
///     fn update_file(&mut self, file: &mut File, area: &Area, on: Vec<Range<Point>>) {
///         todo!();
///     }
/// }
///
/// impl<U: Ui> Parser<U> for MyParser {
///     // You are not given a &mut Pass, because while parsing,
///     // you aren't supposed to be changing the File.
///     fn parse(&mut self) -> bool {
///         // Updates the internal Bytes and Moment to the latest.
///         self.tracker.update();
///
///         let bytes = self.tracker.bytes();
///         let moment = self.tracker.moment();
///
///         // If there were no Changes, there's usually no reason to
///         // parse again.
///         if !moment.is_empty() {
///             self.parse_bytes_and_moment(pa, bytes, moment);
///             self.add_ranges(bytes, moment);
///         }
///
///         // The true here means that this Parser is ready to call
///         // Parser::update. This should be true if you want mutate
///         // the File.
///         true
///     }
///
///     // Since you have &mut Pass, you can actually do any sort of
///     // mutation, not just limited to this File in particular.
///     fn update(&mut self, pa: &mut Pass, handle: &Handle<File<U>, U>, on: Vec<Range<Point>>) {
///         let (file, area) = handle.write_with_area(pa);
///
///         /// Addition of Tags, modification of the text, etc.
///         self.update_file(file, area, on);
///     }
///
///     // This is usually what you'd want to do in this scenario
///     fn before_get(&mut self) {
///         self.parse(pa);
///     }
///
///     // Same thing here. Since the parsing is always done
///     fn before_try_get(&mut self) -> bool {
///         self.parse(pa);
///         true
///     }
/// }
/// ```
///
/// # Asynchronous parsing
///
/// TODO: Document this
///
/// # External access to parsers
///
/// TODO: Document this
///
/// > [!TIP]
/// >
/// > You can keep a [`Parser`] private in your plugin in order to
/// > prevent the end user from reading or writing to it. You can
/// > then create standalone functions or implement traits on the
/// > [`File`] widget in order to give controled access to the
/// > parser.
///
/// [`Change`]: crate::text::Change
/// [`Selection`]: crate::mode::Selection
#[allow(unused_variables)]
pub trait Parser<U: Ui>: Send + 'static {
    /// Parses the [`Bytes`] of the [`File`]
    ///
    /// This function is called every time the [`File`] is updated,
    /// and it's where you should update the internal state of the
    /// [`Parser`] to reflect any [`Change`]s that took place.
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Change`]: crate::text::Change
    /// [`File`]: crate::file::File
    /// [add]: Ranges::add
    /// [remove]: Ranges::remove
    /// [`update`]: Parser::update
    fn parse(&mut self) -> bool {
        true
    }

    /// Updates the [`File`] in some given [`Range<Point>`]s
    ///
    /// As this function is called, the state of the [`Parser`] needs
    /// to already be synced up with the latest [`Change`]s to the
    /// [`File`].
    ///
    /// The list of [`Range`]s is the collection of [`Range`]s that
    /// were requested to be updated and are within the printed region
    /// of the [`File`].
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
    /// [`File`]: crate::file::File
    /// [`Change`]: crate::text::Change
    /// [range]: std::ops::Range
    /// [`parse`]: Parser::parse
    /// [`Conceal`]: crate::text::Conceal
    fn update(&mut self, pa: &mut Pass, file: &Handle<File<U>, U>, on: Vec<Range<Point>>) {}

    /// Prepare this [`Parser`] before [`File::read_parser`] call
    ///
    /// The [`File::read_parser`]/[`File::write_parser`] functions
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

    /// Prepare the [`Parser`] before [`File::try_read_parser`] call
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
    /// [`try_read_parser`]: File::try_read_parser
    /// [`read_parser`]: File::read_parser
    fn before_try_get(&mut self) -> bool {
        self.before_get();
        true
    }
}

/// A [`Parser`] builder struct
pub trait ParserCfg<U: Ui> {
    /// The [`Parser`] that this [`ParserCfg`] will construct
    type Parser: Parser<U>;

    /// Builds the [`Parser`]
    ///
    /// The [`FileTracker`] argument is very important for parsing.
    /// You are going to keep track of [`Change`]s to the [`File`], as
    /// well as where the [`File`] needs updates, through methods in
    /// the [`FileTracker`].
    ///
    /// # Ranges to update
    ///
    /// In order to update the [`File`], you will need to tell Duat
    /// which byte/[`Point`] ranges need to be updated. This can be
    /// done manually through the [`FileTracker::add_range`]
    /// function, which will tell Duat that the added range should be
    /// updated by calling [`Parser::update`].
    ///
    /// However, the [`FileTracker`] can also _automatically_ decide
    /// which [`Range`]s need to be updated. This is done through the
    /// following methods:
    ///
    /// - [`FileTracker::track_changed_ranges`]: Will add every
    ///   [`Change::added_range`] to the list of [`Ranges`] to update.
    /// - [`FileTracker::track_changed_lines`]: Will add every line
    ///   that was changed to the list. This can be useful for, for
    ///   example, single line limited regex parsing, where you know
    ///   that matches won't cross lines.
    /// - [`FileTracker::track_area`]: Instead of tracking which
    ///   ranges need updating, it will instead track the range that
    ///   was printed to the screen. You can use this, for example,
    ///   highlight every occurance of a word on screen.
    /// - [`FileTracker::turn_off_tracking`]: Disables the automatic
    ///   tracking feature.
    ///
    /// Of course, with automatic tracking, you can still manually add
    /// ranges.
    ///
    /// This all means that you should decide which tracking strategy
    /// you want to use. You can change it later, but I don't really
    /// see a purpose to that to be honest.
    fn build(self, file: &File<U>, tracker: FileTracker) -> Result<Self::Parser, Text>;
}

#[derive(Default)]
pub(super) struct Parsers<U: Ui> {
    list: RefCell<Vec<ParserParts<U>>>,
}

impl<U: Ui> Parsers<U> {
    /// Attempts to add  a [`Parser`]
    pub(super) fn add<Cfg: ParserCfg<U>>(&self, file: &File<U>, cfg: Cfg) -> Result<(), Text> {
        let mut parsers = self.list.borrow_mut();
        if parsers
            .iter()
            .any(|rb| rb.ty == TypeId::of::<Cfg::Parser>())
        {
            Err(txt!(
                "There is already a parser of type [a]{}",
                crate::utils::duat_name::<Cfg::Parser>()
            ))?;
        }

        let update_requested = Arc::new(AtomicBool::new(false));
        let ranges = Arc::new(Mutex::new(RangesTracker::Manual(Ranges::new(
            0..file.bytes().len().byte(),
        ))));

        let tracker = FileTracker {
            bytes: file.bytes().clone(),
            cfg: file.cfg.clone(),
            fetcher: file.text.history().unwrap().new_fetcher(),
            moment: Moment::default(),
            ranges: ranges.clone(),
            update_requested: update_requested.clone(),
        };

        parsers.push(ParserParts {
            parser: Some(Box::new(cfg.build(file, tracker)?)),
            ranges: ranges.clone(),
            update_requested,
            ty: TypeId::of::<Cfg::Parser>(),
        });

        Ok(())
    }

    /// Reads a specific [`Parser`]
    pub(super) fn read_parser<P: Parser<U>, Ret>(
        &self,
        read: impl FnOnce(&P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<U, P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;

            parser.before_get();

            let ret = read(unsafe { (Box::as_ptr(&parser) as *const P).as_ref() }.unwrap());

            self.list.borrow_mut()[i].parser = Some(parser);

            Some(ret)
        } else {
            None
        }
    }

    /// Tries to read from a specific [`Parser`]. Fails if it is not
    /// available
    pub(super) fn try_read_parser<P: Parser<U>, Ret>(
        &self,
        read: impl FnOnce(&P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<U, P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;
            let ret = parser
                .before_try_get()
                .then(|| read(unsafe { (Box::as_ptr(&parser) as *const P).as_ref() }.unwrap()));

            self.list.borrow_mut()[i].parser = Some(parser);

            ret
        } else {
            None
        }
    }

    /// Writes to a specific [`Parser`]
    pub(super) fn write_parser<P: Parser<U>, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<U, P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;

            parser.before_get();

            let ret = write(unsafe { (Box::as_mut_ptr(&mut parser) as *mut P).as_mut() }.unwrap());

            self.list.borrow_mut()[i].parser = Some(parser);

            Some(ret)
        } else {
            None
        }
    }

    /// Tries to write to a specific [`Parser`]. Fails if it is not
    /// available
    pub(super) fn try_write_parser<P: Parser<U>, Ret>(
        &self,
        write: impl FnOnce(&mut P) -> Ret,
    ) -> Option<Ret> {
        let position = self.list.borrow().iter().position(type_eq::<U, P>);
        if let Some(i) = position {
            let mut parser = self.list.borrow_mut()[i].parser.take()?;
            let ret = parser.before_try_get().then(|| {
                write(unsafe { (Box::as_mut_ptr(&mut parser) as *mut P).as_mut() }.unwrap())
            });

            self.list.borrow_mut()[i].parser = Some(parser);

            ret
        } else {
            None
        }
    }

    /// Updates the [`Parser`]s on a given range
    // TODO: Deal with reparsing if Changes took place.
    pub(super) fn update(&self, pa: &mut Pass, handle: &Handle<File<U>, U>, on: Range<usize>) {
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
struct ParserParts<U: Ui> {
    parser: Option<Box<dyn Parser<U>>>,
    ranges: Arc<Mutex<RangesTracker>>,
    update_requested: Arc<AtomicBool>,
    ty: TypeId,
}

/// A tracker for [`Change`]s that happen to a [`File`]
///
/// [`Change`]: crate::text::Change
#[derive(Debug)]
pub struct FileTracker {
    bytes: Bytes,
    cfg: Arc<Mutex<PrintCfg>>,
    fetcher: MomentFetcher,
    moment: Moment,
    ranges: Arc<Mutex<RangesTracker>>,
    update_requested: Arc<AtomicBool>,
}

impl FileTracker {
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

    /// Request for the [`File`] to call [`Parser::parse`]
    ///
    /// This will prompt the [`File`] to be updated and only update
    /// relevant [`Range`]s, i.e., those that actually show up on
    /// area and that have been added by to the [`FileTracker`] via
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
    /// [`add_range`]: FileTracker::add_range
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
    /// [`add_range`]: FileTracker::add_range
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
    ///     _tracker: RangesTracker,
    ///     tagger: Tagger,
    /// }
    ///
    /// impl<U: Ui> Parser<U> for SelectionLen {
    ///     fn update(&mut self, pa: &mut Pass, handle: &Handle<File>, on: Range<usize>) {
    ///         let parts = handle.write(pa).text_mut().parts();
    ///         // This is efficient even in very large `Text`s.
    ///         parts.tags.remove(self.tagger, ..);
    ///
    ///         for (_, selection, is_main) in parts.selections.iter_within(on) {
    ///             let range = selection.range(&parts.bytes);
    ///             let (start, end) = (range.start, range.end);
    ///             let len = end - start;
    ///
    ///             if len > 2 {
    ///                 let nums = len.ilog10() as usize + 1;
    ///                 let ghost = Ghost(if is_main {
    ///                     txt!("[sel_len.main]{len}")
    ///                 } else {
    ///                     txt!("[sel_len]{len}")
    ///                 });
    ///
    ///                 parts.tags.insert(self.tagger, start, ghost);
    ///                 parts.tags.insert(self.tagger, start..start + nums, Conceal);
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
    /// [`add_range`]: FileTracker::add_range
    pub fn turn_off_tracking(&mut self) {
        let mut tracker = self.ranges.lock();
        *tracker = RangesTracker::Manual(tracker.take_ranges());
    }

    ////////// Querying functions

    /// The [`Bytes`], as they are since the last call to [`update`]
    ///
    /// Do note that, because this tracks calls to [`update`], this
    /// version of the [`Bytes`] may not reflect what the [`Bytes`]
    /// look like in the [`File`] _right now_, as more [`Change`]s
    /// could have taken place since the last call.
    ///
    /// [`update`]: Self::update
    /// [`Change`]: crate::text::Change
    pub fn bytes(&self) -> &Bytes {
        &self.bytes
    }

    /// The last [`Moment`] that was processed
    ///
    /// This is **not** the last [`Moment`] sent to the [`File`], nor
    /// does it necessarily correspond to a [`Moment`] in the
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
    /// while this one only makes use of the [`PrintCfg`]'s
    /// [`TabStops`].
    ///
    /// [`Area`]: crate::ui::Area
    /// [`TabStops`]: crate::cfg::TabStops
    pub fn indent(&self, p: Point) -> usize {
        self.bytes.indent(p, *self.cfg.lock())
    }

    /// The [`PrintCfg`] of the [`File`]
    ///
    /// Unlike the other parts of this struct, the [`PrintCfg`] will
    /// always be up to date with what it currently is in the [`File`]
    pub fn cfg(&self) -> PrintCfg {
        *self.cfg.lock()
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
fn type_eq<U: Ui, P: 'static>(parts: &ParserParts<U>) -> bool {
    parts.ty == TypeId::of::<P>()
}
