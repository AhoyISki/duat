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
    mode::Selections,
    prelude::Ranges,
    text::{AsRefBytes, Bytes, Moment, MutTags, Point, RefBytes, Text, TextParts, txt},
    ui::Ui,
};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
///
/// [`Change`]: crate::text::Change
#[allow(unused_variables)]
pub trait Reader<U: Ui>: 'static {
    /// Applies the [`Change`]s to this [`Reader`]
    ///
    /// After this point, even if no other functions are called, the
    /// state of this [`Reader`] should reflect the state of the
    /// [`RefBytes`] that were passed.
    ///
    /// The [`Range`]s argument, if given, represents the ranges in
    /// the [`Text`] that [`Reader::update_range`] should care about
    /// updating. You can [add] and [remove] [`Range<usize>`]s from
    /// it, and later, when these byte ranges are printed to the
    /// screen, Duat will request that this reader update that range
    /// via [`Reader::update_range`], allowing for efficient updating
    /// only where it would matter.
    ///
    /// If you don't implement this function, it is assumed that the
    /// [`Ranges`] should always encompass the full breadth of the
    /// [`File`], i.e., this [`Reader`] would update the entire
    /// screen, whenever it is updated.
    ///
    /// # Notes
    ///
    /// This is not where [`Tag`]s will be added or removed, as you
    /// can see by the lack of an appropriate argument for that. That
    /// is done in [`Reader::update_range`].
    ///
    /// And while you _could_ still do that because of the [`Pass`],
    /// this is not recommended, since [`update_range`] gives you the
    /// exact span that you need to care about in order to update
    /// efficiently.
    ///
    /// # Warning
    ///
    /// If you use the [`Pass`] in order to get access to the [`File`]
    /// this [`Reader`] was sent to, *be careful!*. The [`RefBytes`]
    /// sent to this function won't necessarily be the same as the
    /// [`RefBytes`] returned by the [`Text::ref_bytes`] from that
    /// [`File`], since that one is kept up to date with every new
    /// [`Moment`], while the passed [`RefBytes`] is only updated upto
    /// the current [`Moment`].
    ///
    /// Most of the time, these two will be aligned, but if for some
    /// reason, another [`Reader`] were to change the [`Text`] by use
    /// of the [`Pass`], they could become desynchronized.
    ///
    /// [`Tag`]: crate::text::Tag
    /// [`Change`]: crate::text::Change
    /// [`File`]: crate::file::File
    /// [add]: Ranges::add
    /// [remove]: Ranges::remove
    /// [`update_range`]: Reader::update_range
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
    /// It also grants you access to other [`Readers`], which, if they
    /// are present, are guaranteed to be synchronized with the state
    /// of [`File`].
    ///
    /// # NOTES
    ///
    /// The state of the [`Reader`] must be "finished" by this point,
    /// as in, nothing within is actually updated to reflect what the
    /// [`Text`] is like now, as that should have already been done in
    /// [`Reader::apply_changes`]. This specific function is only
    /// called when all [`Change`]s have already been passed to the
    /// [`Reader`], so no need to worry about that.
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
    /// [`apply_changes`]: Reader::apply_changes
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

    /// Wether this [`Reader`] should be sent to update its internal
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

/// A [`Reader`] builder struct
pub trait ReaderCfg<U: Ui> {
    /// The [`Reader`] that this [`ReaderCfg`] will construct
    type Reader: Reader<U>;

    /// Constructs the [`Reader`]
    fn init(self, bytes: RefBytes) -> Result<ReaderBox<U>, Text>;
}

#[derive(Clone, Default)]
pub(super) struct InnerReaders<U: Ui>(RwData<Vec<ReaderBox<U>>>);

impl<U: Ui> InnerReaders<U> {
    /// Attempts to add  a [`Reader`]
    pub(super) fn add<Rd: ReaderCfg<U>>(
        &self,
        pa: &mut Pass,
        bytes: RefBytes,
        reader_cfg: Rd,
    ) -> Result<(), Text> {
        if self.0.read(pa, |rds| {
            rds.iter().any(|rb| rb.ty == TypeId::of::<Rd::Reader>())
        }) {
            Err(txt!(
                "There is already a reader of type [a]{}",
                crate::duat_name::<Rd::Reader>()
            ))?;
        }

        let mut readers = self.0.acquire_mut(pa);
        readers.push(reader_cfg.init(bytes)?);

        Ok(())
    }

    /// Reads a specific [`Reader`]
    pub(super) fn read_reader<Rd: Reader<U>, Ret>(
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

    /// Tries to read a specific [`Reader`]. Fails if it was sent
    pub(super) fn try_read_reader<Rd: Reader<U>, Ret>(
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

    /// Makes each [`Reader`] process a [`Moment`]
    pub(super) fn process_moment(&self, pa: &mut Pass, moment: Moment) {
        let mut readers = std::mem::take(&mut *self.0.acquire_mut(pa));
        for reader_box in readers.iter_mut() {
            let status = reader_box.status.take().unwrap();

            reader_box.status = Some(match status {
                ReaderStatus::Local(mut lr) => {
                    for change in moment.changes() {
                        lr.bytes.apply_change(change);
                    }

                    let bytes = lr.bytes.ref_bytes();
                    let ranges = get_ranges(&mut lr.ranges, &bytes, moment);
                    lr.reader.apply_changes(pa, bytes, moment, ranges);

                    ReaderStatus::Local(lr)
                }
                ReaderStatus::Present(mut ms, mut sr) => {
                    if sr.reader.make_remote() {
                        ms.latest_state += 1;
                        ms.sender.send(Some(moment)).unwrap();

                        let jh = thread::spawn(move || {
                            while let Some(moment) = sr.receiver.recv().unwrap() {
                                apply_moment(&mut sr, moment, None);
                            }

                            sr
                        });

                        ReaderStatus::Sent(ms, jh)
                    } else {
                        ms.latest_state += 1;
                        apply_moment(&mut sr, moment, Some(pa));

                        ReaderStatus::Present(ms, sr)
                    }
                }
                ReaderStatus::Sent(mut ms, jh) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some(moment)).unwrap();

                    ReaderStatus::Sent(ms, jh)
                }
                ReaderStatus::BeingBuilt(mut ms, build_status, jh) => {
                    match build_status.load(Ordering::Relaxed) {
                        FAILED_BUILDING => {
                            let Err(err) = jh.join().unwrap() else {
                                unreachable!()
                            };
                            context::error!("{err}");
                            ReaderStatus::MarkedForDeletion
                        }
                        _ => {
                            ms.latest_state += 1;
                            ms.sender.send(Some(moment)).unwrap();
                            ReaderStatus::BeingBuilt(ms, build_status, jh)
                        }
                    }
                }
                ReaderStatus::MarkedForDeletion => ReaderStatus::MarkedForDeletion,
            })
        }

        self.0.acquire_mut(pa).extend(readers);
    }

    /// Updates the [`Reader`]s on a given range
    pub(super) fn update_range(&self, pa: &mut Pass, text: &mut Text, within: Range<Point>) {
        fn update<U: Ui>(
            text: &mut Text,
            reader: &mut dyn Reader<U>,
            ranges: &mut Ranges,
            readers: &mut [ReaderBox<U>],
            within: Range<Point>,
        ) {
            let to_remove = ranges.remove(within.start.byte()..within.end.byte());

            if to_remove.len() == 0 {
                let parts = FileParts::new(text, Readers(readers), within);
                reader.update_range(parts, None);
                drop(to_remove);
            } else {
                for range in to_remove {
                    let parts = FileParts::new(text, Readers(readers), within.clone());
                    let start = parts.bytes.point_at(range.start);
                    let end = parts.bytes.point_at(range.end);

                    reader.update_range(parts, Some(start..end));
                }
            }
        }

        let mut readers = self.0.acquire_mut(pa);

        for i in 0..readers.len() {
            let status = readers[i].status.take().unwrap();

            readers[i].status = Some(match status {
                ReaderStatus::Local(mut lr) => {
                    let reader = &mut *lr.reader;
                    let ranges = &mut lr.ranges;
                    update(text, reader, ranges, &mut readers, within.clone());
                    ReaderStatus::Local(lr)
                }
                ReaderStatus::Present(ms, mut sr) => {
                    let reader = &mut *sr.reader;
                    let ranges = &mut sr.ranges;
                    update(text, reader, ranges, &mut readers, within.clone());
                    ReaderStatus::Present(ms, sr)
                }
                ReaderStatus::Sent(ms, jh) => {
                    // In this case, all moments have been processed, and we can bring the
                    // Reader back in order to update it.
                    if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                        ms.sender.send(None).unwrap();
                        let mut sr = jh.join().unwrap();
                        let reader = &mut *sr.reader;
                        let ranges = &mut sr.ranges;
                        update(text, reader, ranges, &mut readers, within.clone());
                        ReaderStatus::Present(ms, sr)
                    } else {
                        ReaderStatus::Sent(ms, jh)
                    }
                }
                ReaderStatus::BeingBuilt(ms, build_status, jh) => {
                    match build_status.load(Ordering::Relaxed) {
                        FAILED_BUILDING => {
                            let Err(err) = jh.join().unwrap() else {
                                unreachable!()
                            };
                            context::error!("{err}");
                            ReaderStatus::MarkedForDeletion
                        }
                        SUCCEEDED_BUILDING => {
                            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                                ms.sender.send(None).unwrap();
                                let mut sr = jh.join().unwrap().unwrap();
                                let reader = &mut *sr.reader;
                                let ranges = &mut sr.ranges;
                                update(text, reader, ranges, &mut readers, within.clone());
                                ReaderStatus::Present(ms, sr)
                            } else {
                                ReaderStatus::BeingBuilt(ms, build_status, jh)
                            }
                        }
                        _ => ReaderStatus::BeingBuilt(ms, build_status, jh),
                    }
                }
                ReaderStatus::MarkedForDeletion => ReaderStatus::MarkedForDeletion,
            });
        }
    }

    /// Wether this [`InnerReaders`] is ready to be updated
    pub fn needs_update(&self) -> bool {
        // SAFETY: The File is read, which means I can safely acquire this.
        let pa = &unsafe { Pass::new() };
        let readers = self.0.acquire(pa);

        readers.iter().any(|reader_box| {
            let status = reader_box.status.as_ref();
            if let Some(ReaderStatus::Sent(ms, _) | ReaderStatus::BeingBuilt(ms, ..)) = status {
                ms.latest_state == ms.remote_state.load(Ordering::Relaxed)
            } else {
                false
            }
        })
    }
}

fn apply_moment<U: Ui>(sr: &mut SendReader<U>, moment: Moment, pa: Option<&mut Pass>) {
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

/// A container for a [`Reader`]
///
/// This struct should be created inside of the [`ReaderCfg::init`]
/// function, and nowhere else really.
///
/// This is used internally by Duat to coordinate how [`Reader`]s
/// should be updated. Externally, it can be created in three ways:
///
/// - [`ReaderBox::new_local`]: Use this for any [`Reader`] that you
///   want to process _only_ locally, i.e., don't send them to other
///   threads. This should be used pretty much all the time, and is
///   required if your [`Reader`] is not [`Send`]
/// - [`ReaderBox::new_send`]: Use this to create a [`Reader`]
///   locally, like with the previous function, but give it the option
///   to be sent to other threads. The sending to other threads is
///   done by returning `true` from [`Reader::make_remote`]. The
///   [`Reader`] must be [`Send`].
/// - [`ReaderBox::new_remote`]: Like the previous function, but the
///   [`Reader`] will be created in another thread entirely. This
///   means that you can free up the main thread for faster startup
///   for example. This does also mean that the effects of the
///   [`Reader`] won't be immediate, just like with other times where
///   the [`Reader`] is made to be remote.
pub struct ReaderBox<U: Ui> {
    status: Option<ReaderStatus<U>>,
    ty: TypeId,
}

impl<U: Ui> ReaderBox<U> {
    /// Returns a [`ReaderBox`] that doesn't implement [`Send`]
    pub fn new_local<Rd: Reader<U>>(bytes: RefBytes, reader: Rd) -> ReaderBox<U> {
        ReaderBox {
            status: Some(ReaderStatus::Local(LocalReader {
                reader: Box::new(reader),
                bytes: bytes.clone(),
                ranges: Ranges::full(bytes.len().byte()),
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ReaderBox`] that implements [`Send`]
    pub fn new_send<Rd: Reader<U> + Send>(bytes: RefBytes, reader: Rd) -> ReaderBox<U> {
        let (sender, receiver) = mpsc::channel();

        let state = Arc::new(AtomicUsize::new(0));
        let moment_sender = MomentSender {
            sender,
            latest_state: 0,
            remote_state: state.clone(),
        };

        ReaderBox {
            status: Some(ReaderStatus::Present(moment_sender, SendReader {
                reader: Box::new(reader),
                bytes: bytes.clone(),
                receiver,
                ranges: Ranges::full(bytes.len().byte()),
                state,
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ReaderBox`] from a function evaluated in another
    /// thread
    pub fn new_remote<Rd: Reader<U> + Send>(
        bytes: RefBytes,
        f: impl FnOnce(RefBytes) -> Result<Rd, Text> + Send + 'static,
    ) -> ReaderBox<U> {
        let (sender, receiver) = mpsc::channel();
        let mut bytes = bytes.clone();

        let state = Arc::new(AtomicUsize::new(0));
        let moment_sender = MomentSender {
            sender,
            latest_state: 0,
            remote_state: state.clone(),
        };

        let status = Arc::new(AtomicUsize::new(0));

        ReaderBox {
            status: Some(ReaderStatus::BeingBuilt(
                moment_sender,
                status.clone(),
                thread::spawn(move || {
                    let reader = Box::new(match f(bytes.ref_bytes()) {
                        Ok(reader) => reader,
                        Err(err) => {
                            status.store(FAILED_BUILDING, Ordering::Relaxed);
                            return Err(err);
                        }
                    });
                    status.store(SUCCEEDED_BUILDING, Ordering::Relaxed);

                    let ranges = Ranges::full(bytes.len().byte());

                    let mut sr = SendReader { reader, bytes, receiver, ranges, state };

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

enum ReaderStatus<U: Ui> {
    Local(LocalReader<U>),
    Present(MomentSender, SendReader<U>),
    Sent(MomentSender, thread::JoinHandle<SendReader<U>>),
    BeingBuilt(
        MomentSender,
        Arc<AtomicUsize>,
        thread::JoinHandle<Result<SendReader<U>, Text>>,
    ),
    MarkedForDeletion,
}

struct MomentSender {
    sender: mpsc::Sender<Option<Moment>>,
    latest_state: usize,
    remote_state: Arc<AtomicUsize>,
}

struct LocalReader<U: Ui> {
    reader: Box<dyn Reader<U>>,
    bytes: Bytes,
    ranges: Ranges,
}

struct SendReader<U: Ui> {
    reader: Box<dyn Reader<U> + Send>,
    bytes: Bytes,
    receiver: mpsc::Receiver<Option<Moment>>,
    ranges: Ranges,
    state: Arc<AtomicUsize>,
}

/// A list of the _other_ [`Reader`]s for reading
///
/// This struct lets you read other [`Reader`]s, if you want to gather
/// information for [`Reader::update_range`].
///
/// Those [`Reader`]s could be in another thread, updating. If you
/// want to wait for them to return, see [`Readers::read`]. If you
/// wish to call the passed function only if the [`Reader`] is not
/// currently updating, see [`Readers::try_read`].
pub struct Readers<'a, U: Ui>(&'a mut [ReaderBox<U>]);

impl<U: Ui> Readers<'_, U> {
    /// Reads a specific [`Reader`], if it was [added]
    ///
    /// If the [`Reader`] was sent to another thread, this function
    /// will block until it returns to this thread. If you don't wish
    /// for this behaviour, see [`File::try_read_reader`].
    ///
    /// This function will never return [`Some`] if you call it from a
    /// [`Reader`] that is the same as the requested one.
    ///
    /// [added]: crate::context::Handle::add_reader
    /// [`File::try_read_reader`]: super::File::try_read_reader
    pub fn read<Rd: Reader<U>, Ret>(&mut self, read: impl FnOnce(&Rd) -> Ret) -> Option<Ret> {
        if let Some(reader_box) = self.0.iter_mut().find(|rb| rb.ty == TypeId::of::<Rd>()) {
            let status = reader_box.status.take()?;

            let (status, ret) = status_from_read(read, status);

            reader_box.status = Some(status);

            ret
        } else {
            None
        }
    }

    /// Tries to read a specific [`Reader`], if it was [added]
    ///
    /// Not only does it not trigger if the [`Reader`] doesn't exist,
    /// also will not trigger if it was sent to another thread, and
    /// isn't ready to be brought back. If you wish to wait for the
    ///
    /// This function will never return [`Some`] if you call it from a
    /// [`Reader`] that is the same as the requested one.
    ///
    /// [added]: crate::context::Handle::add_reader
    pub fn try_read<Rd: Reader<U>, Ret>(&mut self, read: impl FnOnce(&Rd) -> Ret) -> Option<Ret> {
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

fn status_from_read<Rd: Reader<U>, Ret, U: Ui>(
    read: impl FnOnce(&Rd) -> Ret,
    status: ReaderStatus<U>,
) -> (ReaderStatus<U>, Option<Ret>) {
    match status {
        ReaderStatus::Local(lr) => {
            let ptr = Box::as_ptr(&lr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Local(lr), Some(ret))
        }
        ReaderStatus::Present(sender, sr) => {
            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Present(sender, sr), Some(ret))
        }
        ReaderStatus::Sent(ms, jh) => {
            ms.sender.send(None).unwrap();
            let sr = jh.join().unwrap();

            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Present(ms, sr), Some(ret))
        }
        ReaderStatus::BeingBuilt(ms, _, jh) => {
            ms.sender.send(None).unwrap();
            match jh.join().unwrap() {
                Ok(sr) => {
                    let ptr = Box::as_ptr(&sr.reader);
                    let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });
                    (ReaderStatus::Present(ms, sr), Some(ret))
                }
                Err(err) => {
                    context::error!("{err}");
                    (ReaderStatus::MarkedForDeletion, None)
                }
            }
        }
        ReaderStatus::MarkedForDeletion => (ReaderStatus::MarkedForDeletion, None),
    }
}

fn status_from_try_read<Rd: Reader<U>, Ret, U: Ui>(
    read: impl FnOnce(&Rd) -> Ret,
    status: ReaderStatus<U>,
) -> (ReaderStatus<U>, Option<Ret>) {
    match status {
        ReaderStatus::Local(lr) => {
            let ptr = Box::as_ptr(&lr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Local(lr), Some(ret))
        }
        ReaderStatus::Present(sender, sr) => {
            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Present(sender, sr), Some(ret))
        }
        ReaderStatus::Sent(ms, jh) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                let sr = jh.join().unwrap();

                let ptr = Box::as_ptr(&sr.reader);
                let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

                (ReaderStatus::Present(ms, sr), Some(ret))
            } else {
                (ReaderStatus::Sent(ms, jh), None)
            }
        }
        ReaderStatus::BeingBuilt(ms, build_status, jh) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                match jh.join().unwrap() {
                    Ok(sr) => {
                        let ptr = Box::as_ptr(&sr.reader);
                        let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });
                        (ReaderStatus::Present(ms, sr), Some(ret))
                    }
                    Err(err) => {
                        context::error!("{err}");
                        (ReaderStatus::MarkedForDeletion, None)
                    }
                }
            } else {
                (ReaderStatus::BeingBuilt(ms, build_status, jh), None)
            }
        }
        ReaderStatus::MarkedForDeletion => (ReaderStatus::MarkedForDeletion, None),
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

fn type_eq<U: Ui, Rd: Reader<U>>(rb: &ReaderBox<U>) -> bool {
    rb.ty == TypeId::of::<Rd>()
}

/// The parts of a [`File`], primarily for updating [`MutTags`]
///
/// This struct consists of the following:
///
/// - [`parts.bytes`]: The [`RefBytes`] of the [`File`].
/// - [`parts.tags`]: The [`MutTags`] of the [`File`], this is the
///   primary things that [`Reader`]s should update.
/// - [`parts.selections`]: The [`Selections`] of the [`File`].
/// - [`parts.readers`]: An immutable reference to all the other
///   [`Reader`]s of the [`File`].
///
/// You should use these, as well as the internally updated state of
/// the [`Reader`] through the [`Reader::apply_changes`] and
/// [`Reader::apply_remote_changes`], in order to update the
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
    /// This is the primary purpose of (most) [`Reader`]s, they read
    /// the [`File`]'s [`Bytes`] and add or remove [`Tag`]s through
    /// the [`update_range`] method.
    ///
    /// [`File`]: super::File
    /// [`Tag`]: crate::text::Tag
    /// [`update_range`]: Reader::update_range
    pub tags: MutTags<'a>,
    /// The [`Selections`] of the [`File`]
    ///
    /// Unlike [`Change`]s in [`Moment`]s, the movement of
    /// [`Selection`]s is not "tracked" and kept up to date via a
    /// [`Reader::apply_changes`] analogue. Instead, if you want to
    /// add [`Tag`]s in the [`Reader::update_range`] function based on
    /// the [`Selections`], I recommend something like the following:
    ///
    /// ```rust
    /// use std::ops::Range;
    ///
    /// use duat_core::prelude::*;
    ///
    /// struct MyReader {
    ///     tagger: Tagger,
    /// }
    ///
    /// impl<U: Ui> Reader<U> for MyReader {
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
    /// For every [`Selection`] on screen, the [`Reader`] above will
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
    /// Other [`Reader`]s that were added to this [`File`]
    ///
    /// This can be useful if you want to access the state of other
    /// [`Reader`]s, a particular [`Reader`] this can  be useful is
    /// with the [`TsParser`] [`Reader`], which can grant you
    /// access to a syntax tree, letting you do queries and such.
    ///
    /// [`File`]: super::File
    /// [`TsParser`]: https://https://docs.rs/duat-treesitter/latest/duat_treesitter/struct.TsParser.html
    pub readers: Readers<'a, U>,
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
    pub fn new(text: &'a mut Text, readers: Readers<'a, U>, range: Range<Point>) -> Self {
        let TextParts { bytes, tags, selections } = text.parts();
        Self {
            bytes,
            tags,
            selections,
            readers,
            suggested_max_range: range,
        }
    }
}

const FAILED_BUILDING: usize = 1;
const SUCCEEDED_BUILDING: usize = 2;
