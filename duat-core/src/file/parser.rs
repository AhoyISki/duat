//! Struct that can react to change in the [`Text`]
//!
//! These structs will be informed of every [`Change`] that happens in
//! the [`Text`], and are allowed to act accordingly. This action will
//! be done by telling the [`Text`] what parts need to be updated.
//! They will then be updated when deemed relevant by the [`Ui`] in
//! use (usually when these become visible).
//!
//! [`Ui`]: crate::ui::Ui
use std::{
    any::TypeId,
    ops::Range,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
        mpsc,
    },
    thread,
};

use crate::{
    context,
    data::{Pass, RwData},
    file::PathKind,
    mode::Selections,
    prelude::Ranges,
    text::{AsRefBytes, Bytes, Moment, MutTags, Point, RefBytes, Text, TextParts, txt},
    ui::Ui,
};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
///
/// [`Change`]: crate::text::Change
#[allow(unused_variables)]
pub trait Parser<U: Ui>: 'static {
    /// Applies the [`Change`]s to this [`Parser`]
    ///
    /// After this point, even if no other functions are called, the
    /// state of this [`Parser`] should reflect the state of the
    /// [`RefBytes`] that were passed.
    ///
    /// The [`Range`]s argument, if given, represents the ranges in
    /// the [`Text`] that [`Parser::update_range`] should care about
    /// updating. You can [add] and [remove] [`Range<usize>`]s from
    /// it, and later, when these byte ranges are printed to the
    /// screen, Duat will request that this reader update that range
    /// via [`Parser::update_range`], allowing for efficient updating
    /// only where it would matter.
    ///
    /// If you don't implement this function, it is assumed that the
    /// [`Ranges`] should always encompass the full breadth of the
    /// [`File`], i.e., this [`Parser`] would update the entire
    /// screen, whenever it is updated.
    ///
    /// # Notes
    ///
    /// This is not where [`Tag`]s will be added or removed, as you
    /// can see by the lack of an appropriate argument for that. That
    /// is done in [`Parser::update_range`].
    ///
    /// And while you _could_ still do that because of the [`Pass`],
    /// this is not recommended, since [`update_range`] gives you the
    /// exact span that you need to care about in order to update
    /// efficiently.
    ///
    /// # Warning
    ///
    /// If you use the [`Pass`] in order to get access to the [`File`]
    /// this [`Parser`] was sent to, *be careful!*. The [`RefBytes`]
    /// sent to this function won't necessarily be the same as the
    /// [`RefBytes`] returned by the [`Text::ref_bytes`] from that
    /// [`File`], since that one is kept up to date with every new
    /// [`Moment`], while the passed [`RefBytes`] is only updated upto
    /// the current [`Moment`].
    ///
    /// Most of the time, these two will be aligned, but if for some
    /// reason, another [`Parser`] were to change the [`Text`] by use
    /// of the [`Pass`], they could become desynchronized.
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Change`]: crate::text::Change
    /// [`File`]: crate::file::File
    /// [add]: Ranges::add
    /// [remove]: Ranges::remove
    /// [`update_range`]: Parser::update_range
    fn apply_changes(
        &mut self,
        pa: &mut Pass,
        bytes: RefBytes,
        moment: Moment,
        ranges_to_update: Option<&mut Ranges>,
    ) {
        if let Some(ranges) = ranges_to_update {
            *ranges = Ranges::full(bytes.len().byte());
        }
    }

    /// Updates in a given [`Range`]
    ///
    /// This should take into account all changes that have taken
    /// place before this point.
    ///
    /// It also grants you access to other [`Parsers`], which, if they
    /// are present, are guaranteed to be synchronized with the state
    /// of [`File`].
    ///
    /// # NOTES
    ///
    /// The state of the [`Parser`] must be "finished" by this point,
    /// as in, nothing within is actually updated to reflect what the
    /// [`Text`] is like now, as that should have already been done in
    /// [`Parser::apply_changes`]. This specific function is only
    /// called when all [`Change`]s have already been passed to the
    /// [`Parser`], so no need to worry about that.
    ///
    /// One other thing to note is that the `within` [range] is just a
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
    fn update_range(&mut self, parts: FileParts<U>, within: Option<Range<Point>>) {}

    /// Same as [`apply_changes`], but on another thread
    ///
    /// The biggest difference between this function and
    /// [`apply_changes`] is that, since this one doesn't take place
    /// on the main thread, you lose access to Duat's shared state
    /// afforded by the [`Pass`], the only things you will have access
    /// to are the [`RefBytes`] and the latest [`Moment`] of the
    /// [`File`] that was updated
    ///
    /// # Warning
    ///
    /// You should *NEVER* try to get over the lack of access to
    /// Duat's global state, by using something like
    /// [`RwData::read_unsafe`]. That's because Duat's global state is
    /// only meant to be accessed from the main thread, since the
    /// constructs are _not_ thread safe.
    ///
    /// You can still do things like queue [commands] and [hooks],
    /// which can access global state since they are triggered on the
    /// main thread. But don't try to use [`RwData::read_unsafe`]
    /// or [`RwData::write_unsafe`] outside of the main thread.
    /// That's not their purpose!
    ///
    /// [`apply_changes`]: Parser::apply_changes
    /// [commands]: crate::cmd::queue
    /// [hooks]: crate::hook::queue
    /// [`File`]: super::File
    fn apply_remote_changes(
        &mut self,
        bytes: RefBytes,
        moment: Moment,
        ranges_to_update: Option<&mut Ranges>,
    ) where
        Self: Send,
    {
        if let Some(ranges) = ranges_to_update {
            *ranges = Ranges::full(bytes.len().byte());
        }
    }

    /// Wether this [`Parser`] should be sent to update its internal
    /// state in another thread
    ///
    /// This should pretty much never be used, unless in very specific
    /// circumstances, such as with very large files, or with broken
    /// syntax trees, whose update time has become slow
    fn make_remote(&self) -> bool
    where
        Self: Send,
    {
        false
    }
}

/// A [`Parser`] builder struct
pub trait ParserCfg<U: Ui> {
    /// The [`Parser`] that this [`ParserCfg`] will construct
    type Parser: Parser<U>;

    /// Constructs the [`Parser`]
    ///
    /// The `path` may be one of three types:
    /// - Set with an existing file.
    /// - Set, but with no existing file yet.
    /// - Unset, in which case it would be called something like
    ///   `"*scratch file#{num}*"`.
    fn init(self, bytes: RefBytes, path: PathKind) -> Result<ParserBox<U>, Text>;
}

#[derive(Clone, Default)]
pub(super) struct InnerParsers<U: Ui>(RwData<Vec<ParserBox<U>>>);

impl<U: Ui> InnerParsers<U> {
    /// Attempts to add  a [`Parser`]
    pub(super) fn add<Rd: ParserCfg<U>>(
        &self,
        pa: &mut Pass,
        bytes: RefBytes,
        path: PathKind,
        reader_cfg: Rd,
    ) -> Result<(), Text> {
        if self.0.read(pa, |rds| {
            rds.iter().any(|rb| rb.ty == TypeId::of::<Rd::Parser>())
        }) {
            Err(txt!(
                "There is already a reader of type [a]{}",
                crate::duat_name::<Rd::Parser>()
            ))?;
        }

        let mut parsers = self.0.acquire_mut(pa);
        parsers.push(reader_cfg.init(bytes, path)?);

        Ok(())
    }

    /// Reads a specific [`Parser`]
    pub(super) fn read_parser<Rd: Parser<U>, Ret>(
        &self,
        pa: &mut Pass,
        read: impl FnOnce(&Rd) -> Ret,
    ) -> Option<Ret> {
        if TypeId::of::<Rd>() == TypeId::of::<()>() {
            return None;
        }

        let position = self.0.acquire(pa).iter().position(type_eq::<U, Rd>);
        if let Some(i) = position {
            let status = self.0.acquire_mut(pa)[i].status.take()?;

            let (status, ret) = status_from_read(read, status);

            self.0.acquire_mut(pa)[i].status = Some(status);

            ret
        } else {
            None
        }
    }

    /// Tries to read a specific [`Parser`]. Fails if it was sent
    pub(super) fn try_read_parser<Rd: Parser<U>, Ret>(
        &self,
        pa: &mut Pass,
        read: impl FnOnce(&Rd) -> Ret,
    ) -> Option<Ret> {
        if TypeId::of::<Rd>() == TypeId::of::<()>() {
            return None;
        }

        let position = self.0.acquire(pa).iter().position(type_eq::<U, Rd>);
        if let Some(i) = position {
            let status = self.0.acquire_mut(pa)[i].status.take()?;

            let (status, ret) = status_from_try_read(read, status);

            self.0.acquire_mut(pa)[i].status = Some(status);

            ret
        } else {
            None
        }
    }

    /// Makes each [`Parser`] process a [`Moment`]
    pub(super) fn process_moment(&self, pa: &mut Pass, moment: Moment) {
        let mut parsers = std::mem::take(&mut *self.0.acquire_mut(pa));
        for reader_box in parsers.iter_mut() {
            let status = reader_box.status.take().unwrap();

            reader_box.status = Some(match status {
                ParserStatus::Local(mut lr) => {
                    for change in moment.changes() {
                        lr.bytes.apply_change(change);
                    }

                    let bytes = lr.bytes.ref_bytes();
                    let ranges = get_ranges(&mut lr.ranges, &bytes, moment);
                    lr.reader.apply_changes(pa, bytes, moment, ranges);

                    ParserStatus::Local(lr)
                }
                ParserStatus::Present(mut ms, mut sr) => {
                    if sr.reader.make_remote() {
                        ms.latest_state += 1;
                        ms.sender.send(Some(moment)).unwrap();

                        let jh = thread::spawn(move || {
                            while let Some(moment) = sr.receiver.recv().unwrap() {
                                apply_moment(&mut sr, moment, None);
                            }

                            sr
                        });

                        ParserStatus::Sent(ms, jh)
                    } else {
                        ms.latest_state += 1;
                        apply_moment(&mut sr, moment, Some(pa));

                        ParserStatus::Present(ms, sr)
                    }
                }
                ParserStatus::Sent(mut ms, jh) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some(moment)).unwrap();

                    ParserStatus::Sent(ms, jh)
                }
                ParserStatus::BeingBuilt(mut ms, jh) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some(moment)).unwrap();
                    ParserStatus::BeingBuilt(ms, jh)
                }
                ParserStatus::MarkedForDeletion => ParserStatus::MarkedForDeletion,
            })
        }

        self.0.acquire_mut(pa).extend(parsers);
    }

    /// Updates the [`Parser`]s on a given range
    pub(super) fn update_range(&self, pa: &mut Pass, text: &mut Text, within: Range<Point>) {
        fn update<U: Ui>(
            text: &mut Text,
            reader: &mut dyn Parser<U>,
            ranges: &mut Ranges,
            parsers: &mut [ParserBox<U>],
            within: Range<Point>,
        ) {
            let to_remove = ranges.remove(within.start.byte()..within.end.byte());

            if to_remove.len() == 0 {
                let parts = FileParts::new(text, Parsers(parsers), within);
                reader.update_range(parts, None);
                drop(to_remove);
            } else {
                for range in to_remove {
                    let parts = FileParts::new(text, Parsers(parsers), within.clone());
                    let start = parts.bytes.point_at(range.start);
                    let end = parts.bytes.point_at(range.end);

                    reader.update_range(parts, Some(start..end));
                }
            }
        }

        let mut parsers = self.0.acquire_mut(pa);

        for i in 0..parsers.len() {
            let status = parsers[i].status.take().unwrap();

            parsers[i].status = Some(match status {
                ParserStatus::Local(mut lr) => {
                    let reader = &mut *lr.reader;
                    let ranges = &mut lr.ranges;
                    update(text, reader, ranges, &mut parsers, within.clone());
                    ParserStatus::Local(lr)
                }
                ParserStatus::Present(ms, mut sr) => {
                    let reader = &mut *sr.reader;
                    let ranges = &mut sr.ranges;
                    update(text, reader, ranges, &mut parsers, within.clone());
                    ParserStatus::Present(ms, sr)
                }
                ParserStatus::Sent(ms, jh) => {
                    // In this case, all moments have been processed, and we can bring the
                    // Parser back in order to update it.
                    if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                        ms.sender.send(None).unwrap();
                        let mut sr = jh.join().unwrap();
                        let reader = &mut *sr.reader;
                        let ranges = &mut sr.ranges;
                        update(text, reader, ranges, &mut parsers, within.clone());
                        ParserStatus::Present(ms, sr)
                    } else {
                        ParserStatus::Sent(ms, jh)
                    }
                }
                ParserStatus::BeingBuilt(ms, jh) => {
                    if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                        let _ = ms.sender.send(None);
                        match jh.join().unwrap() {
                            Ok(mut sr) => {
                                let reader = &mut *sr.reader;
                                let ranges = &mut sr.ranges;
                                update(text, reader, ranges, &mut parsers, within.clone());
                                ParserStatus::Present(ms, sr)
                            }
                            Err(err) => {
                                context::error!("{err}");
                                ParserStatus::MarkedForDeletion
                            }
                        }
                    } else {
                        ParserStatus::BeingBuilt(ms, jh)
                    }
                }
                ParserStatus::MarkedForDeletion => ParserStatus::MarkedForDeletion,
            });
        }
    }

    /// Wether this [`InnerParsers`] is ready to be updated
    pub fn needs_update(&self) -> bool {
        // SAFETY: The File is read, which means I can safely acquire this.
        let pa = &unsafe { Pass::new() };
        let parsers = self.0.acquire(pa);

        parsers.iter().any(|reader_box| {
            let status = reader_box.status.as_ref();
            if let Some(ParserStatus::Sent(ms, _) | ParserStatus::BeingBuilt(ms, ..)) = status {
                ms.latest_state == ms.remote_state.load(Ordering::Relaxed)
            } else {
                false
            }
        })
    }
}

fn apply_moment<U: Ui>(sr: &mut SendParser<U>, moment: Moment, pa: Option<&mut Pass>) {
    for change in moment.changes() {
        sr.bytes.apply_change(change);
    }

    let bytes = sr.bytes.ref_bytes();
    let ranges = get_ranges(&mut sr.ranges, &bytes, moment);

    if let Some(pa) = pa {
        sr.reader.apply_changes(pa, bytes, moment, ranges);
    } else {
        sr.reader.apply_remote_changes(bytes, moment, ranges);
    }
    sr.state.fetch_add(1, Ordering::Relaxed);
}

/// A container for a [`Parser`]
///
/// This struct should be created inside of the [`ParserCfg::init`]
/// function, and nowhere else really.
///
/// This is used internally by Duat to coordinate how [`Parser`]s
/// should be updated. Externally, it can be created in three ways:
///
/// - [`ParserBox::new_local`]: Use this for any [`Parser`] that you
///   want to process _only_ locally, i.e., don't send them to other
///   threads. This should be used pretty much all the time, and is
///   required if your [`Parser`] is not [`Send`]
/// - [`ParserBox::new_send`]: Use this to create a [`Parser`]
///   locally, like with the previous function, but give it the option
///   to be sent to other threads. The sending to other threads is
///   done by returning `true` from [`Parser::make_remote`]. The
///   [`Parser`] must be [`Send`].
/// - [`ParserBox::new_remote`]: Like the previous function, but the
///   [`Parser`] will be created in another thread entirely. This
///   means that you can free up the main thread for faster startup
///   for example. This does also mean that the effects of the
///   [`Parser`] won't be immediate, just like with other times where
///   the [`Parser`] is made to be remote.
pub struct ParserBox<U: Ui> {
    status: Option<ParserStatus<U>>,
    ty: TypeId,
}

impl<U: Ui> ParserBox<U> {
    /// Returns a [`ParserBox`] that doesn't implement [`Send`]
    pub fn new_local<Rd: Parser<U>>(bytes: RefBytes, reader: Rd) -> ParserBox<U> {
        ParserBox {
            status: Some(ParserStatus::Local(LocalParser {
                reader: Box::new(reader),
                bytes: bytes.clone(),
                ranges: Ranges::full(bytes.len().byte()),
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ParserBox`] that implements [`Send`]
    pub fn new_send<Rd: Parser<U> + Send>(bytes: RefBytes, reader: Rd) -> ParserBox<U> {
        let (sender, receiver) = mpsc::channel();

        let state = Arc::new(AtomicUsize::new(1));
        let moment_sender = MomentSender {
            sender,
            latest_state: 1,
            remote_state: state.clone(),
        };

        ParserBox {
            status: Some(ParserStatus::Present(moment_sender, SendParser {
                reader: Box::new(reader),
                bytes: bytes.clone(),
                receiver,
                ranges: Ranges::full(bytes.len().byte()),
                state,
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ParserBox`] from a function evaluated in another
    /// thread
    pub fn new_remote<Rd: Parser<U> + Send>(
        bytes: RefBytes,
        f: impl FnOnce(RefBytes) -> Result<Rd, Text> + Send + 'static,
    ) -> ParserBox<U> {
        let (sender, receiver) = mpsc::channel();
        let mut bytes = bytes.clone();

        let state = Arc::new(AtomicUsize::new(0));
        let moment_sender = MomentSender {
            sender,
            latest_state: 1,
            remote_state: state.clone(),
        };

        ParserBox {
            status: Some(ParserStatus::BeingBuilt(
                moment_sender,
                thread::spawn(move || {
                    let reader = Box::new(match f(bytes.ref_bytes()) {
                        Ok(reader) => {
                            state.fetch_add(1, Ordering::Relaxed);
                            reader
                        }
                        Err(err) => {
                            state.fetch_add(1, Ordering::Relaxed);
                            return Err(err);
                        }
                    });

                    let ranges = Ranges::full(bytes.len().byte());

                    let mut sr = SendParser { reader, bytes, receiver, ranges, state };

                    while let Some(moment) = sr.receiver.recv().unwrap() {
                        apply_moment(&mut sr, moment, None);
                    }

                    Ok(sr)
                }),
            )),
            ty: TypeId::of::<Rd>(),
        }
    }
}

enum ParserStatus<U: Ui> {
    Local(LocalParser<U>),
    Present(MomentSender, SendParser<U>),
    Sent(MomentSender, thread::JoinHandle<SendParser<U>>),
    BeingBuilt(
        MomentSender,
        thread::JoinHandle<Result<SendParser<U>, Text>>,
    ),
    MarkedForDeletion,
}

struct MomentSender {
    sender: mpsc::Sender<Option<Moment>>,
    latest_state: usize,
    remote_state: Arc<AtomicUsize>,
}

struct LocalParser<U: Ui> {
    reader: Box<dyn Parser<U>>,
    bytes: Bytes,
    ranges: Ranges,
}

struct SendParser<U: Ui> {
    reader: Box<dyn Parser<U> + Send>,
    bytes: Bytes,
    receiver: mpsc::Receiver<Option<Moment>>,
    ranges: Ranges,
    state: Arc<AtomicUsize>,
}

/// A list of the _other_ [`Parser`]s for reading
///
/// This struct lets you read other [`Parser`]s, if you want to gather
/// information for [`Parser::update_range`].
///
/// Those [`Parser`]s could be in another thread, updating. If you
/// want to wait for them to return, see [`Parsers::read`]. If you
/// wish to call the passed function only if the [`Parser`] is not
/// currently updating, see [`Parsers::try_read`].
pub struct Parsers<'a, U: Ui>(&'a mut [ParserBox<U>]);

impl<U: Ui> Parsers<'_, U> {
    /// Reads a specific [`Parser`], if it was [added]
    ///
    /// If the [`Parser`] was sent to another thread, this function
    /// will block until it returns to this thread. If you don't wish
    /// for this behaviour, see [`File::try_read_parser`].
    ///
    /// This function will never return [`Some`] if you call it from a
    /// [`Parser`] that is the same as the requested one.
    ///
    /// [added]: crate::context::Handle::add_parser
    /// [`File::try_read_parser`]: super::File::try_read_parser
    pub fn read<Rd: Parser<U>, Ret>(&mut self, read: impl FnOnce(&Rd) -> Ret) -> Option<Ret> {
        if let Some(reader_box) = self.0.iter_mut().find(|rb| rb.ty == TypeId::of::<Rd>()) {
            let status = reader_box.status.take()?;

            let (status, ret) = status_from_read(read, status);

            reader_box.status = Some(status);

            ret
        } else {
            None
        }
    }

    /// Tries to read a specific [`Parser`], if it was [added]
    ///
    /// Not only does it not trigger if the [`Parser`] doesn't exist,
    /// also will not trigger if it was sent to another thread, and
    /// isn't ready to be brought back. If you wish to wait for the
    ///
    /// This function will never return [`Some`] if you call it from a
    /// [`Parser`] that is the same as the requested one.
    ///
    /// [added]: crate::context::Handle::add_parser
    pub fn try_read<Rd: Parser<U>, Ret>(&mut self, read: impl FnOnce(&Rd) -> Ret) -> Option<Ret> {
        if let Some(reader_box) = self.0.iter_mut().find(|rb| rb.ty == TypeId::of::<Rd>()) {
            let status = reader_box.status.take()?;

            let (status, ret) = status_from_try_read(read, status);

            reader_box.status = Some(status);

            ret
        } else {
            None
        }
    }
}

fn status_from_read<Rd: Parser<U>, Ret, U: Ui>(
    read: impl FnOnce(&Rd) -> Ret,
    status: ParserStatus<U>,
) -> (ParserStatus<U>, Option<Ret>) {
    match status {
        ParserStatus::Local(lr) => {
            let ptr = Box::as_ptr(&lr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Local(lr), Some(ret))
        }
        ParserStatus::Present(sender, sr) => {
            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Present(sender, sr), Some(ret))
        }
        ParserStatus::Sent(ms, jh) => {
            ms.sender.send(None).unwrap();
            let sr = jh.join().unwrap();

            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Present(ms, sr), Some(ret))
        }
        ParserStatus::BeingBuilt(ms, jh) => {
            let _ = ms.sender.send(None);
            match jh.join().unwrap() {
                Ok(sr) => {
                    let ptr = Box::as_ptr(&sr.reader);
                    let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });
                    (ParserStatus::Present(ms, sr), Some(ret))
                }
                Err(err) => {
                    context::error!("{err}");
                    (ParserStatus::MarkedForDeletion, None)
                }
            }
        }
        ParserStatus::MarkedForDeletion => (ParserStatus::MarkedForDeletion, None),
    }
}

fn status_from_try_read<Rd: Parser<U>, Ret, U: Ui>(
    read: impl FnOnce(&Rd) -> Ret,
    status: ParserStatus<U>,
) -> (ParserStatus<U>, Option<Ret>) {
    match status {
        ParserStatus::Local(lr) => {
            let ptr = Box::as_ptr(&lr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Local(lr), Some(ret))
        }
        ParserStatus::Present(sender, sr) => {
            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Present(sender, sr), Some(ret))
        }
        ParserStatus::Sent(ms, jh) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                let sr = jh.join().unwrap();

                let ptr = Box::as_ptr(&sr.reader);
                let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

                (ParserStatus::Present(ms, sr), Some(ret))
            } else {
                (ParserStatus::Sent(ms, jh), None)
            }
        }
        ParserStatus::BeingBuilt(ms, jh) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                match jh.join().unwrap() {
                    Ok(sr) => {
                        let ptr = Box::as_ptr(&sr.reader);
                        let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });
                        (ParserStatus::Present(ms, sr), Some(ret))
                    }
                    Err(err) => {
                        context::error!("{err}");
                        (ParserStatus::MarkedForDeletion, None)
                    }
                }
            } else {
                (ParserStatus::BeingBuilt(ms, jh), None)
            }
        }
        ParserStatus::MarkedForDeletion => (ParserStatus::MarkedForDeletion, None),
    }
}

/// Decides wether a [`Ranges`] should be used or not
fn get_ranges<'a>(
    ranges: &'a mut Ranges,
    bytes: &RefBytes<'_>,
    moment: Moment,
) -> Option<&'a mut Ranges> {
    const FOLDING_COULD_UPDATE_A_LOT: usize = 1_000_000;
    const MAX_CHANGES_TO_CONSIDER: usize = 100;

    if moment.len() <= MAX_CHANGES_TO_CONSIDER || bytes.len().byte() >= FOLDING_COULD_UPDATE_A_LOT {
        for change in moment.changes() {
            let diff = change.added_end().byte() as i32 - change.taken_end().byte() as i32;
            ranges.shift_by(change.start().byte(), diff);
        }

        Some(ranges)
    } else {
        *ranges = Ranges::full(bytes.len().byte());
        None
    }
}

fn type_eq<U: Ui, Rd: Parser<U>>(rb: &ParserBox<U>) -> bool {
    rb.ty == TypeId::of::<Rd>()
}

/// The parts of a [`File`], primarily for updating [`MutTags`]
///
/// This struct consists of the following:
///
/// - [`parts.bytes`]: The [`RefBytes`] of the [`File`].
/// - [`parts.tags`]: The [`MutTags`] of the [`File`], this is the
///   primary things that [`Parser`]s should update.
/// - [`parts.selections`]: The [`Selections`] of the [`File`].
/// - [`parts.parsers`]: An immutable reference to all the other
///   [`Parser`]s of the [`File`].
///
/// You should use these, as well as the internally updated state of
/// the [`Parser`] through the [`Parser::apply_changes`] and
/// [`Parser::apply_remote_changes`], in order to update the
/// [`File`]'s [`Tag`]s. Or other things outside of the [`File`], it
/// depends on what you're doing really.
///
/// [`File`]: super::File
/// [`Tag`]: crate::text::Tag
pub struct FileParts<'a, U: Ui> {
    /// The [`RefBytes`] of the [`File`]
    ///
    /// Since this is a [`RefBytes`], you can't _actually_ modify the
    /// [`Bytes`]. But you can do any non-mutating method. This
    /// includes [searches], which require internal "non changing"
    /// mutation.
    ///
    /// [`File`]: super::File
    /// [searches]: RefBytes::search_fwd
    pub bytes: RefBytes<'a>,
    /// The [`MutTags`] of the [`File`]
    ///
    /// This is the primary purpose of (most) [`Parser`]s, they read
    /// the [`File`]'s [`Bytes`] and add or remove [`Tag`]s through
    /// the [`update_range`] method.
    ///
    /// [`File`]: super::File
    /// [`Tag`]: crate::text::Tag
    /// [`update_range`]: Parser::update_range
    pub tags: MutTags<'a>,
    /// The [`Selections`] of the [`File`]
    ///
    /// Unlike [`Change`]s in [`Moment`]s, the movement of
    /// [`Selection`]s is not "tracked" and kept up to date via a
    /// [`Parser::apply_changes`] analogue. Instead, if you want to
    /// add [`Tag`]s in the [`Parser::update_range`] function based on
    /// the [`Selections`], I recommend something like the following:
    ///
    /// ```rust
    /// use std::ops::Range;
    ///
    /// use duat_core::prelude::*;
    ///
    /// struct MyParser {
    ///     tagger: Tagger,
    /// }
    ///
    /// impl<U: Ui> Parser<U> for MyParser {
    ///     fn update_range(&mut self, mut parts: FileParts<U>, within: Option<Range<Point>>) {
    ///         // This is efficient even in very large `Text`s.
    ///         parts.tags.remove(self.tagger, ..);
    ///
    ///         // Here, since we don't check for Changes, `within` will always
    ///         // be `None`, so no need to check it anyways.
    ///         let within = parts.suggested_max_range;
    ///
    ///         for (_, selection, is_main) in parts.selections.iter_within(within) {
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
    /// For every [`Selection`] on screen, the [`Parser`] above will
    /// print the length of the selection, in bytes, at the start of
    /// the each [`Selection`] with a length greater than 2.
    ///
    /// The [`parts.suggested_max_range`] is a visible portion of the
    /// screen. Adding [`Tag`]s that don't intersect with this
    /// [`Range`] is pointless, since they won't be visible anyway.
    ///
    /// [`File`]: super::File
    /// [`Change`]: crate::text::Change
    /// [`Tag`]: crate::text::Tag
    /// [`Selection`]: crate::mode::Selection
    /// [`parts.suggested_max_range`]: FileParts::suggested_max_range
    pub selections: &'a Selections,
    /// Other [`Parser`]s that were added to this [`File`]
    ///
    /// This can be useful if you want to access the state of other
    /// [`Parser`]s, a particular [`Parser`] this can  be useful is
    /// with the [`TsParser`] [`Parser`], which can grant you
    /// access to a syntax tree, letting you do queries and such.
    ///
    /// [`File`]: super::File
    /// [`TsParser`]: https://https://docs.rs/duat-treesitter/latest/duat_treesitter/struct.TsParser.html
    pub parsers: Parsers<'a, U>,
    /// The suggested maximum [`Range`] for [inserting] and [removing]
    /// [`Tag`]s
    ///
    /// This [`Range`] represents a "visible" portion of the screen,
    /// going over it is not recommended, since the added [`Tag`]s
    /// won't be visible anyway. The only exception is for ranged
    /// [`Tag`]s which "contain" this [`Range`], such as a very large
    /// [`FormId`] [tag].
    ///
    /// [inserting]: MutTags::insert
    /// [removing]: MutTags::remove
    /// [`Tag`]: crate::text::Tag
    /// [`FormId`]: crate::form::FormId
    /// [tag]: crate::form::FormId::to_tag
    pub suggested_max_range: Range<Point>,
}

impl<'a, U: Ui> FileParts<'a, U> {
    /// Returns a new [`FileParts`]
    pub fn new(text: &'a mut Text, parsers: Parsers<'a, U>, range: Range<Point>) -> Self {
        let TextParts { bytes, tags, selections } = text.parts();
        Self {
            bytes,
            tags,
            selections,
            parsers,
            suggested_max_range: range,
        }
    }
}
