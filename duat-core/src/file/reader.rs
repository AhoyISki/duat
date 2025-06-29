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
    data::{Pass, RwData},
    text::{AsRefBytes, Bytes, Change, Moment, RefBytes, Text, TextParts, txt},
    ui::Ui,
};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
#[allow(unused_variables)]
pub trait Reader<U: Ui>: 'static {
    /// Applies the [`Change`]s to this [`Reader`]
    ///
    /// After this point, even if no other functions are called, the
    /// state of this [`Reader`] should reflect the state of the
    /// [`Text`].
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
    fn apply_changes(
        &mut self,
        pa: &mut Pass,
        bytes: RefBytes,
        moment: Moment,
        ranges_to_update: Option<&mut RangeList>,
    ) {
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
    /// One other thing to note is that the updated range is just a
    /// suggestion. In most circumstances, it would be a little
    /// convenient to go slightly over that range, just keep in
    /// mind that that overhang might be updated later.
    ///
    /// Finally, keep in mind that [`Tag`]s are not allowed to be
    /// repeated, and you can use this to your advantage, as in,
    /// instead of checking if you need to place a [`Tag`] in a
    /// certain spot, you can just place it, and Duat will ignore that
    /// request if that [`Tag`] was already there.
    ///
    /// [`Tag`]: crate::text::Tag
    fn update_range(
        &mut self,
        parts: TextParts,
        readers: Readers<U>,
        within: Option<Range<usize>>,
    ) {
    }

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
    /// but don't try to use [`RwData::read_unsafe`] or
    /// [`RwData::write_unsafe`] outside of the main thread. That's
    /// not their purpose!
    ///
    /// [`apply_changes`]: Reader::apply_changes
    /// [commands]: crate::cmd::queue
    /// [hooks]: crate::hook::queue
    fn apply_remote_changes(
        &mut self,
        bytes: RefBytes,
        moment: Moment,
        ranges_to_update: Option<&mut RangeList>,
    ) where
        Self: Send,
    {
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

        if let Some(i) = self.0.acquire(pa).iter().position(type_equals::<U, Rd>) {
            let status = self.0.acquire_mut(pa)[i].status.take()?;

            let (status, ret) = status_from_read(read, status);

            self.0.acquire_mut(pa)[i].status = Some(status);

            Some(ret)
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

        if let Some(i) = self.0.acquire(pa).iter().position(type_equals::<U, Rd>) {
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
                    let range_list = get_range_list(&mut lr.range_list, &bytes, moment);
                    lr.reader.apply_changes(pa, bytes, moment, range_list);

                    ReaderStatus::Local(lr)
                }
                ReaderStatus::Present(mut ms, mut sr) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some(moment)).unwrap();

                    if sr.reader.make_remote() {
                        let join_handle = thread::spawn(move || {
                            while let Some(moment) = sr.receiver.recv().unwrap() {
                                for change in moment.changes() {
                                    sr.bytes.apply_change(change);
                                }

                                let bytes = sr.bytes.ref_bytes();
                                let range_list = get_range_list(&mut sr.range_list, &bytes, moment);

                                sr.reader.apply_remote_changes(bytes, moment, range_list);
                                sr.state.fetch_add(1, Ordering::Relaxed);
                            }

                            sr
                        });

                        ReaderStatus::Sent(ms, join_handle)
                    } else {
                        for change in moment.changes() {
                            sr.bytes.apply_change(change);
                        }

                        let bytes = sr.bytes.ref_bytes();
                        let range_list = get_range_list(&mut sr.range_list, &bytes, moment);

                        sr.reader.apply_changes(pa, bytes, moment, range_list);
                        sr.state.fetch_add(1, Ordering::Relaxed);

                        ReaderStatus::Present(ms, sr)
                    }
                }
                ReaderStatus::Sent(mut ms, join_handle) => {
                    ms.latest_state += 1;
                    ms.sender.send(Some(moment)).unwrap();

                    ReaderStatus::Sent(ms, join_handle)
                }
            })
        }

        self.0.acquire_mut(pa).extend(readers);
    }

    /// Updates the [`Reader`]s on a given range
    pub(super) fn update_range(&self, pa: &mut Pass, text: &mut Text, within: Range<usize>) {
        fn update<U: Ui>(
            text: &mut Text,
            reader: &mut dyn Reader<U>,
            range_list: &mut RangeList,
            readers: &mut [ReaderBox<U>],
            within: Range<usize>,
        ) {
            let old_ranges = std::mem::replace(range_list, RangeList::empty());

            for range in old_ranges {
                let parts = text.parts();
                let (within, split_off) = split_range_within(range.clone(), within.clone());

                reader.update_range(parts, Readers(readers), within);

                for range in split_off.into_iter().flatten() {
                    range_list.add(range);
                }
            }
        }

        let mut readers = self.0.acquire_mut(pa);

        for i in 0..readers.len() {
            let status = readers[i].status.take().unwrap();

            readers[i].status = Some(match status {
                ReaderStatus::Local(mut lr) => {
                    let reader = &mut *lr.reader;
                    let range_list = &mut lr.range_list;
                    update(text, reader, range_list, &mut readers, within.clone());
                    ReaderStatus::Local(lr)
                }
                ReaderStatus::Present(ms, mut sr) => {
                    let reader = &mut *sr.reader;
                    let range_list = &mut sr.range_list;
                    update(text, reader, range_list, &mut readers, within.clone());
                    ReaderStatus::Present(ms, sr)
                }
                ReaderStatus::Sent(ms, join_handle) => {
                    // In this case, all moments have been processed, and we can bring the
                    // Reader back in order to update it.
                    if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                        let mut sr = join_handle.join().unwrap();
                        let reader = &mut *sr.reader;
                        let range_list = &mut sr.range_list;
                        update(text, reader, range_list, &mut readers, within.clone());
                        ReaderStatus::Present(ms, sr)
                    } else {
                        ReaderStatus::Sent(ms, join_handle)
                    }
                }
            });
        }
    }
}

/// A list of non intersecting exclusive [`Range<usize>`]s
///
/// The primary purpose of this struct is to serve [`Reader`]s by
/// telling Duat which ranges need to be updated. This lets Duat
/// minimize as much as possible the amount of work done to update the
/// [`Text`] when it changes in a [`File`].
///
/// [`File`]: super::File
#[derive(Clone)]
pub struct RangeList(Vec<Range<usize>>);

impl RangeList {
    /// Return a new instance of [`RangeList`]
    ///
    /// By default, the whole [`Text`] should be updated.
    pub fn new(max: usize) -> Self {
        Self(vec![0..max])
    }

    /// Returns a new empty [`RangeList`]
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    /// Adds a range to the list of [`Range<usize>`]s
    ///
    /// This range will be merged in with the others on the list, so
    /// it may bridge gaps between ranges or for longer ranges within,
    /// without allowing for the existance of intersecting ranges.
    pub fn add(&mut self, range: Range<usize>) {
        let (r_range, start) = match self.0.binary_search_by_key(&range.start, |r| r.start) {
            // Same thing here
            Ok(i) => (i..i + 1, range.start),
            Err(i) => {
                // This is if we intersect the added part
                if let Some(older_i) = i.checked_sub(1)
                    && range.start <= self.0[older_i].end
                {
                    (older_i..i, self.0[older_i].start)
                // And here is if we intersect nothing on the
                // start, no changes drained.
                } else {
                    (i..i, range.start)
                }
            }
        };

        let start_i = r_range.start;

        // Otherwise search ahead for another change to be merged
        let (r_range, end) = match self.0[start_i..].binary_search_by_key(&range.end, |r| r.start) {
            Ok(i) => (r_range.start..start_i + i + 1, self.0[start_i + i].end),
            Err(i) => match (start_i + i).checked_sub(1).and_then(|i| self.0.get(i)) {
                Some(older) => (r_range.start..start_i + i, range.end.max(older.end)),
                None => (r_range.start..start_i + i, range.end),
            },
        };

        self.0.splice(r_range, [start..end]);
    }

    /// Applies the [`add`] function to a list of [`Range<usize>`]s
    ///
    /// [`add`]: Self::add
    pub fn merge(&mut self, other: Self) {
        for range in other.0 {
            self.add(range)
        }
    }

    /// Returns the number of [`Range<usize>`]s
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if there are no [`Range<usize>`]s
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Shifts the [`Range<usize>`]s by a list of [`Change`]s
    fn shift_by_changes<'a>(&mut self, changes: impl Iterator<Item = Change<&'a str>>) {
        let mut bounds = {
            let iter = self.0.iter_mut().flat_map(|r| [&mut r.start, &mut r.end]);
            iter.peekable()
        };
        let mut shift = 0;

        for change in changes {
            while let Some(bound) = bounds.next_if(|b| **b < change.start().byte()) {
                *bound = (*bound as i32 + shift) as usize;
            }
            shift += change.added_end().byte() as i32 - change.taken_end().byte() as i32;
        }
        for bound in bounds {
            *bound = (*bound as i32 + shift) as usize
        }
    }
}

impl IntoIterator for RangeList {
    type IntoIter = std::vec::IntoIter<Range<usize>>;
    type Item = Range<usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
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
                range_list: RangeList::new(bytes.len().byte()),
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
                range_list: RangeList::new(bytes.len().byte()),
                state,
            })),
            ty: TypeId::of::<Rd>(),
        }
    }

    /// Returns a [`ReaderBox`] from a function evaluated in another
    /// thread
    pub fn new_remote<Rd: Reader<U> + Send>(
        bytes: RefBytes,
        f: impl FnOnce(RefBytes) -> Rd + Send + 'static,
    ) -> ReaderBox<U> {
        let (sender, receiver) = mpsc::channel();
        let mut bytes = bytes.clone();

        let state = Arc::new(AtomicUsize::new(0));
        let moment_sender = MomentSender {
            sender,
            latest_state: 0,
            remote_state: state.clone(),
        };

        ReaderBox {
            status: Some(ReaderStatus::Sent(
                moment_sender,
                thread::spawn(move || {
                    let reader = Box::new(f(bytes.ref_bytes()));
                    let range_list = RangeList::new(bytes.len().byte());

                    SendReader {
                        reader,
                        bytes,
                        receiver,
                        range_list,
                        state,
                    }
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
}

struct MomentSender {
    sender: mpsc::Sender<Option<Moment>>,
    latest_state: usize,
    remote_state: Arc<AtomicUsize>,
}

struct LocalReader<U: Ui> {
    reader: Box<dyn Reader<U>>,
    bytes: Bytes,
    range_list: RangeList,
}

struct SendReader<U: Ui> {
    reader: Box<dyn Reader<U> + Send>,
    bytes: Bytes,
    receiver: mpsc::Receiver<Option<Moment>>,
    range_list: RangeList,
    state: Arc<AtomicUsize>,
}

/// A list of the _other_ [`Reader`]s for reading
///
/// This struct lets you read other [`Reader`]s, if you want to gather
/// information for [`Reader::update_range`].
///
/// Those [`Reader`]s could be in another thread, updating. If you
/// want to wait for them to return, see [`ReaderList::read`]. If you
/// wish to call the passed function only if the [`Reader`] is not
/// currently updating, see [`ReaderList::try_read`].
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
    /// [added]: Handle::add_reader
    pub fn read<Rd: Reader<U>, Ret>(&mut self, read: impl FnOnce(&Rd) -> Ret) -> Option<Ret> {
        if let Some(reader_box) = self.0.iter_mut().find(|rb| rb.ty == TypeId::of::<Rd>()) {
            let status = reader_box.status.take()?;

            let (status, ret) = status_from_read(read, status);

            reader_box.status = Some(status);

            Some(ret)
        } else {
            None
        }
    }

    /// Tries tor read a specific [`Reader`], if it was [added]
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

/// Splits a range within a region
///
/// The first return is the part of `within` that must be updated.
/// The second return is what is left of `range`.
///
/// If `range` is fully inside `within`, remove `range`;
/// If `within` is fully inside `range`, split `range` in 2;
/// If `within` intersects `range` in one side, cut it out;
fn split_range_within(
    range: Range<usize>,
    within: Range<usize>,
) -> (Option<Range<usize>>, [Option<Range<usize>>; 2]) {
    if range.start >= within.end || within.start >= range.end {
        (None, [Some(range), None])
    } else {
        let start_range = (within.start > range.start).then_some(range.start..within.start);
        let end_range = (range.end > within.end).then_some(within.end..range.end);
        let split_ranges = [start_range, end_range];
        let range_to_check = range.start.max(within.start)..(range.end.min(within.end));
        (Some(range_to_check), split_ranges)
    }
}

/// Decides wether a [`RangeList`] should be used or not
fn get_range_list<'a>(
    ranges_to_update: &'a mut RangeList,
    bytes: &RefBytes<'_>,
    moment: Moment,
) -> Option<&'a mut RangeList> {
    const FOLDING_COULD_UPDATE_A_LOT: usize = 1_000_000;
    const MAX_CHANGES_TO_CONSIDER: usize = 100;

    if moment.len() <= MAX_CHANGES_TO_CONSIDER || bytes.len().byte() >= FOLDING_COULD_UPDATE_A_LOT {
        ranges_to_update.shift_by_changes(moment.changes());
        Some(ranges_to_update)
    } else {
        *ranges_to_update = RangeList::new(bytes.len().byte());
        None
    }
}

fn type_equals<U: Ui, Rd: Reader<U>>(rb: &ReaderBox<U>) -> bool {
    rb.ty == TypeId::of::<Rd>()
}

fn status_from_read<Rd: Reader<U>, Ret, U: Ui>(
    read: impl FnOnce(&Rd) -> Ret,
    status: ReaderStatus<U>,
) -> (ReaderStatus<U>, Ret) {
    let (status, ret) = match status {
        ReaderStatus::Local(lr) => {
            let ptr = Box::as_ptr(&lr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Local(lr), ret)
        }
        ReaderStatus::Present(sender, sr) => {
            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Present(sender, sr), ret)
        }
        ReaderStatus::Sent(ms, join_handle) => {
            ms.sender.send(None).unwrap();
            let sr = join_handle.join().unwrap();

            let ptr = Box::as_ptr(&sr.reader);
            let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

            (ReaderStatus::Present(ms, sr), ret)
        }
    };
    (status, ret)
}

fn status_from_try_read<Rd: Reader<U>, Ret, U: Ui>(
    read: impl FnOnce(&Rd) -> Ret,
    status: ReaderStatus<U>,
) -> (ReaderStatus<U>, Option<Ret>) {
    let (status, ret) = match status {
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
        ReaderStatus::Sent(ms, join_handle) => {
            if ms.latest_state == ms.remote_state.load(Ordering::Relaxed) {
                ms.sender.send(None).unwrap();
                let sr = join_handle.join().unwrap();

                let ptr = Box::as_ptr(&sr.reader);
                let ret = read(unsafe { (ptr as *const Rd).as_ref().unwrap() });

                (ReaderStatus::Present(ms, sr), Some(ret))
            } else {
                (ReaderStatus::Sent(ms, join_handle), None)
            }
        }
    };
    (status, ret)
}
