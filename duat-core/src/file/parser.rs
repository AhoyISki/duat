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

use super::File;
use crate::{
    cfg::PrintCfg,
    context,
    data::{Pass, RwData},
    mode::Selections,
    prelude::Ranges,
    text::{Bytes, Moment, Point, Tags, Text, TextParts, txt},
    ui::{Ui, Widget},
};

/// A [`Text`] reader, modifying it whenever [`Moment`]s happen
///
/// [`Change`]: crate::text::Change
#[allow(unused_variables)]
pub trait Parser<U: Ui>: 'static {
    /// Parses the [`File`] after a [`Moment`] takes place
    ///
    /// After this point, even if no other functions are called, the
    /// state of this [`Parser`] should reflect the state of the
    /// [`FileSnapshot`] that were passed.
    ///
    /// The [`FileSnapshot`] has three item in it: The [`Bytes`] after
    /// the [`Moment`] was applied, the [`Moment`] itself, and the
    /// [`PrintCfg`] of the [`File`] _at that point in time_.
    ///
    /// The `ranges` argument, if given, represents the [`Ranges`]
    /// in the [`Text`] that [`Parser::update_range`] should care
    /// about updating. You can [add] and [remove]
    /// [`Range<usize>`]s from it, and later, when these byte
    /// ranges are printed to the screen, Duat will request that
    /// this reader update that them via [`Parser::update_range`],
    /// allowing for efficient updating only when it would matter.
    /// If the argument is [`None`], you don't need to care about
    /// that.
    ///
    /// # Notes
    ///
    /// This is not where [`Tag`]s will be added or removed, as you
    /// can see by the lack of an appropriate argument for that (that
    /// argument would be [`Tags`]). That is instead done in
    /// [`Parser::update_range`].
    ///
    /// And while you _could_ still do that because of the [`Pass`],
    /// this is not recommended, since [`update_range`] gives you the
    /// exact span that you need to care about in order to update
    /// efficiently.
    ///
    /// # Note
    ///
    /// You _can_ access the [`File`] _as it is right now_ via the
    /// [`Pass`] argument, but there is no guarantee of synchronicity
    /// between the [`Bytes`] of the [`File`] and the [`Bytes`] in
    /// the [`FileSnapshot`].
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Change`]: crate::text::Change
    /// [`File`]: crate::file::File
    /// [add]: Ranges::add
    /// [remove]: Ranges::remove
    /// [`update_range`]: Parser::update_range
    fn parse(&mut self, pa: &mut Pass, snap: FileSnapshot, ranges: Option<&mut Ranges>) {}

    /// Updates in a given [`Range`]
    ///
    /// This function, unlike [`parse`] and
    /// [`parse_remote`] does *not* give you a snapshot of the
    /// [`File`]. Since it is supposed to add and remove [`Tag`]s, it
    /// gives you direct access to the [`File`]'s [parts], like a
    /// [`Pass`] would.
    ///
    /// The `within`
    ///
    /// # NOTES
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
    /// [parts]: FileParts
    /// [`Change`]: crate::text::Change
    /// [range]: std::ops::Range
    /// [`parse`]: Parser::parse
    /// [`parse_remote`]: Parser::parse_remote
    fn update_range(&mut self, parts: FileParts<U>, within: Option<Range<Point>>) {}

    /// Same as [`parse`], but on another thread
    ///
    /// The biggest difference between this function and
    /// [`parse`] is that, since this one doesn't take place
    /// on the main thread, you lose access to Duat's shared state
    /// afforded by the [`Pass`], the only things you will have access
    /// to are the [`Bytes`], [`PrintCfg`] and the latest [`Moment`]
    /// of the [`File`] that was updated, all within
    /// the [`FileSnapshot`].
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
    /// [`parse`]: Parser::parse
    /// [commands]: crate::cmd::queue
    /// [hooks]: crate::hook::queue
    /// [`File`]: super::File
    fn parse_remote(&mut self, snap: FileSnapshot, ranges: Option<&mut Ranges>)
    where
        Self: Send,
    {
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
    fn init(self, file: &File<U>) -> Result<ParserBox<U>, Text>;
}

#[derive(Clone, Default)]
pub(super) struct InnerParsers<U: Ui>(RwData<Vec<ParserBox<U>>>);

impl<U: Ui> InnerParsers<U> {
    /// Attempts to add  a [`Parser`]
    pub(super) fn add<Rd: ParserCfg<U>>(
        &self,
        pa: &mut Pass,
        file: &File<U>,
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
        parsers.push(reader_cfg.init(file)?);

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
    pub(super) fn process_moment(&self, pa: &mut Pass, moment: Moment, cfg: PrintCfg) {
        let mut parsers = std::mem::take(&mut *self.0.acquire_mut(pa));
        for reader_box in parsers.iter_mut() {
            let status = reader_box.status.take().unwrap();

            reader_box.status = Some(match status {
                ParserStatus::Local(mut lp) => {
                    for change in moment.changes() {
                        lp.bytes.apply_change(change);
                    }

                    let ranges = get_ranges(&mut lp.ranges, &lp.bytes, moment);
                    let snap = FileSnapshot { bytes: &lp.bytes, cfg, moment };
                    lp.reader.parse(pa, snap, ranges);

                    ParserStatus::Local(lp)
                }
                ParserStatus::Present(mut ms, mut sp) => {
                    if sp.reader.make_remote() {
                        ms.latest_state += 1;
                        ms.sender.send(Some((moment, cfg))).unwrap();

                        let jh = thread::spawn(move || {
                            while let Some((moment, cfg)) = sp.receiver.recv().unwrap() {
                                parse(&mut sp, (moment, cfg), None);
                            }

                            sp
                        });

                        ParserStatus::Sent(ms, jh)
                    } else {
                        ms.latest_state += 1;
                        parse(&mut sp, (moment, cfg), Some(pa));

                        ParserStatus::Present(ms, sp)
                    }
                }
                ParserStatus::Sent(mut ms, jh) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some((moment, cfg))).unwrap();

                    ParserStatus::Sent(ms, jh)
                }
                ParserStatus::BeingBuilt(mut ms, jh) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some((moment, cfg))).unwrap();
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
            parser: &mut dyn Parser<U>,
            ranges: &mut Ranges,
            parsers: &mut [ParserBox<U>],
            within: Range<Point>,
        ) {
            let to_remove = ranges.remove(within.start.byte()..within.end.byte());

            if to_remove.len() == 0 {
                let parts = FileParts::new(text, Parsers(parsers), within);
                parser.update_range(parts, None);
                drop(to_remove);
            } else {
                for range in to_remove {
                    let parts = FileParts::new(text, Parsers(parsers), within.clone());
                    let start = parts.bytes.point_at(range.start);
                    let end = parts.bytes.point_at(range.end);

                    parser.update_range(parts, Some(start..end));
                }
            }
        }

        let mut parsers = self.0.acquire_mut(pa);

        for i in 0..parsers.len() {
            let status = parsers[i].status.take().unwrap();

            parsers[i].status = Some(match status {
                ParserStatus::Local(mut lp) => {
                    let reader = &mut *lp.reader;
                    let ranges = &mut lp.ranges;
                    update(text, reader, ranges, &mut parsers, within.clone());
                    ParserStatus::Local(lp)
                }
                ParserStatus::Present(ms, mut sp) => {
                    let reader = &mut *sp.reader;
                    let ranges = &mut sp.ranges;
                    update(text, reader, ranges, &mut parsers, within.clone());
                    ParserStatus::Present(ms, sp)
                }
                ParserStatus::Sent(ms, jh) => {
                    // In this case, all moments have been processed, and we can bring the
                    // Parser back in order to update it.
                    if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                        ms.sender.send(None).unwrap();
                        let mut sp = jh.join().unwrap();
                        let reader = &mut *sp.reader;
                        let ranges = &mut sp.ranges;
                        update(text, reader, ranges, &mut parsers, within.clone());
                        ParserStatus::Present(ms, sp)
                    } else {
                        ParserStatus::Sent(ms, jh)
                    }
                }
                ParserStatus::BeingBuilt(ms, jh) => {
                    if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                        let _ = ms.sender.send(None);
                        match jh.join().unwrap() {
                            Ok(mut sp) => {
                                let reader = &mut *sp.reader;
                                let ranges = &mut sp.ranges;
                                update(text, reader, ranges, &mut parsers, within.clone());
                                ParserStatus::Present(ms, sp)
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

fn parse<U: Ui>(sp: &mut SendParser<U>, (moment, cfg): (Moment, PrintCfg), pa: Option<&mut Pass>) {
    for change in moment.changes() {
        sp.bytes.apply_change(change);
    }

    let ranges = get_ranges(&mut sp.ranges, &sp.bytes, moment);

    let snap = FileSnapshot { bytes: &sp.bytes, cfg, moment };

    if let Some(pa) = pa {
        sp.reader.parse(pa, snap, ranges);
    } else {
        sp.reader.parse_remote(snap, ranges);
    }
    sp.state.fetch_add(1, Ordering::Relaxed);
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
    pub fn new_local<Rd: Parser<U>>(file: &File<U>, reader: Rd) -> ParserBox<U> {
        ParserBox {
            status: Some(ParserStatus::Local(LocalParser {
                reader: Box::new(reader),
                bytes: file.bytes().clone(),
                ranges: Ranges::full(file.text().len().byte()),
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ParserBox`] that implements [`Send`]
    pub fn new_send<Rd: Parser<U> + Send>(file: &File<U>, reader: Rd) -> ParserBox<U> {
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
                bytes: file.bytes().clone(),
                receiver,
                ranges: Ranges::full(file.text().len().byte()),
                state,
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ParserBox`] from a function evaluated in another
    /// thread
    pub fn new_remote<Rd: Parser<U> + Send>(
        file: &File<U>,
        f: impl FnOnce(&Bytes) -> Result<Rd, Text> + Send + 'static,
    ) -> ParserBox<U> {
        let (sender, receiver) = mpsc::channel();
        let bytes = file.bytes().clone();

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
                    let reader = Box::new(match f(&bytes) {
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

                    let mut sp = SendParser { reader, bytes, receiver, ranges, state };

                    while let Some(moment) = sp.receiver.recv().unwrap() {
                        parse(&mut sp, moment, None);
                    }

                    Ok(sp)
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
    sender: mpsc::Sender<Option<(Moment, PrintCfg)>>,
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
    receiver: mpsc::Receiver<Option<(Moment, PrintCfg)>>,
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
        ParserStatus::Local(lp) => {
            let ptr = Box::as_ptr(&lp.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Local(lp), Some(ret))
        }
        ParserStatus::Present(sender, sp) => {
            let ptr = Box::as_ptr(&sp.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Present(sender, sp), Some(ret))
        }
        ParserStatus::Sent(ms, jh) => {
            ms.sender.send(None).unwrap();
            let sp = jh.join().unwrap();

            let ptr = Box::as_ptr(&sp.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Present(ms, sp), Some(ret))
        }
        ParserStatus::BeingBuilt(ms, jh) => {
            let _ = ms.sender.send(None);
            match jh.join().unwrap() {
                Ok(sp) => {
                    let ptr = Box::as_ptr(&sp.reader);
                    let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });
                    (ParserStatus::Present(ms, sp), Some(ret))
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
        ParserStatus::Local(lp) => {
            let ptr = Box::as_ptr(&lp.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Local(lp), Some(ret))
        }
        ParserStatus::Present(sender, sp) => {
            let ptr = Box::as_ptr(&sp.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ParserStatus::Present(sender, sp), Some(ret))
        }
        ParserStatus::Sent(ms, jh) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                let sp = jh.join().unwrap();

                let ptr = Box::as_ptr(&sp.reader);
                let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

                (ParserStatus::Present(ms, sp), Some(ret))
            } else {
                (ParserStatus::Sent(ms, jh), None)
            }
        }
        ParserStatus::BeingBuilt(ms, jh) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                match jh.join().unwrap() {
                    Ok(sp) => {
                        let ptr = Box::as_ptr(&sp.reader);
                        let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });
                        (ParserStatus::Present(ms, sp), Some(ret))
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
    bytes: &'a Bytes,
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

/// The parts of a [`File`], primarily for updating [`Tags`]
///
/// This struct consists of the following:
///
/// - [`parts.bytes`]: The [`Bytes`] of the [`File`].
/// - [`parts.tags`]: The [`Tags`] of the [`File`], this is the
///   primary things that [`Parser`]s should update.
/// - [`parts.selections`]: The [`Selections`] of the [`File`].
/// - [`parts.parsers`]: An immutable reference to all the other
///   [`Parser`]s of the [`File`].
///
/// You should use these, as well as the internally updated state of
/// the [`Parser`] through the [`Parser::parse`] and
/// [`Parser::parse_remote`], in order to update the
/// [`File`]'s [`Tag`]s. Or other things outside of the [`File`], it
/// depends on what you're doing really.
///
/// [`File`]: super::File
/// [`Tag`]: crate::text::Tag
pub struct FileParts<'a, U: Ui> {
    /// The [`Bytes`] of the [`File`]
    ///
    /// [`File`]: super::File
    pub bytes: &'a Bytes,
    /// The [`Tags`] of the [`File`]
    ///
    /// This is the primary purpose of (most) [`Parser`]s, they read
    /// the [`File`]'s [`Bytes`] and add or remove [`Tag`]s through
    /// the [`update_range`] method.
    ///
    /// [`File`]: super::File
    /// [`Tag`]: crate::text::Tag
    /// [`update_range`]: Parser::update_range
    pub tags: Tags<'a>,
    /// The [`Selections`] of the [`File`]
    ///
    /// Unlike [`Change`]s in [`Moment`]s, the movement of
    /// [`Selection`]s is not "tracked" and kept up to date via a
    /// [`Parser::parse`] analogue. Instead, if you want to
    /// add [`Tag`]s in the [`Parser::update_range`] function based on
    /// the [`Selections`], I recommend something like the following:
    ///
    /// ```rust
    /// use std::ops::Range;
    ///
    /// use duat_core::prelude::*;
    ///
    /// struct SelectionLen {
    ///     tagger: Tagger,
    /// }
    ///
    /// impl<U: Ui> Parser<U> for SelectionLen {
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
    /// [inserting]: Tags::insert
    /// [removing]: Tags::remove
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

/// What the [`File`]'s [`Bytes`] and [`PrintCfg`] looked like at a
/// point in time, as well as the last [`Moment`] applied
///
/// This struct is sent to [`Parser`]'s [`parse`] and
/// [`parse_remote`], and represents what the [`File`] looked
/// like _after_ the [`Moment`] was applied to the [`Bytes`].
///
/// # Note
///
/// In [`parse`], you _can_ access the current state of the
/// [`File`], since you have a [`Pass`], however, there is no
/// guarantee of synchronicity between the [`File`] gotten from
/// something like [`context::file_named`] and the [`Bytes`] and
/// [`PrintCfg`] within [`FileSnapshot`].
///
/// [`File`]: super::File
/// [`parse`]: Parser::parse
/// [`parse_remote`]: Parser::parse_remote
pub struct FileSnapshot<'a> {
    /// The [`Bytes`] of the [`File`] at _this_ moment in time
    ///
    /// [`File`]: super::File
    pub bytes: &'a Bytes,
    /// The [`PrintCfg`] of the [`File`] at _this_ moment in time
    ///
    /// [`File`]: super::File
    pub cfg: PrintCfg,
    /// The last [`Moment`] applied to the [`Bytes`], getting them to
    /// their state in this snapshot
    pub moment: Moment,
}

impl FileSnapshot<'_> {
    /// The indentation in the current line
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
        self.bytes.indent(p, self.cfg)
    }
}
