//! Struct that can react to change in the [`Text`]
//!
//! These structs will be informed of every [`Change`] that happens in
//! the [`Text`], and are allowed to act accordingly. This action will
//! be done by telling the [`Text`] what parts need to be updated.
//! They will then be updated when deemed relevant by the [`Ui`] in
//! use (usually when these become visible).
//!
//! [`Ui`]: crate::ui::Ui
use std::{any::TypeId, cell::RefCell, ops::Range, rc::Rc};

use super::BytesDataMap;
use crate::{
    data::{Pass, RwData},
    text::{Bytes, Change, Moment, MutTags, Text, err},
    ui::Ui,
};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
pub trait Reader<U: Ui>: 'static {
    /// Applies the [`Change`]s to this [`Reader`]
    ///
    /// After this point, even if no other functions are called, the
    /// state of this [`Reader`] should reflect the state of the
    /// [`Text`].
    ///
    /// # NOTES
    ///
    /// This is not where [`Tag`]s will be added or removed, as you
    /// can see by the lack of an appropriate argument for that. That
    /// is done in [`Reader::update_range`].
    ///
    /// There is also a
    fn apply_changes(
        pa: Pass,
        reader: RwData<Self>,
        bytes: BytesDataMap<U>,
        moment: Moment,
        ranges_to_update: Option<&mut RangeList>,
    ) where
        Self: Sized;

    /// Updates a given [`Range`]
    ///
    /// This should take into account all changes that have taken
    /// place before this point.
    ///
    /// # NOTES
    ///
    /// The state of the [`Reader`] must be "finished" by this point,
    /// as in, nothing within is actually updated to reflect what the
    /// [`Text`] is like now, as that should have already been done in
    /// [`Reader::apply_changes`].
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
    fn update_range(&mut self, bytes: &mut Bytes, tags: MutTags, within: Range<usize>);
}

pub trait ReaderCfg<U: Ui> {
    type Reader: Reader<U>;

    fn init(self, buffer: &mut Bytes) -> Result<Self::Reader, Text>;
}

#[derive(Default, Clone)]
pub struct Readers<U: Ui>(RwData<Vec<ReaderEntry<U>>>);

impl<U: Ui> Readers<U> {
    pub fn add<R: ReaderCfg<U>>(
        &mut self,
        pa: &mut Pass,
        bytes: &mut Bytes,
        reader_cfg: R,
    ) -> Result<(), Text> {
        self.0.write(pa, |readers| {
            if readers.iter().any(|re| re.ty == TypeId::of::<R::Reader>()) {
                Err(err!(
                    "There is already a reader of type [a]{}",
                    crate::duat_name::<R::Reader>()
                ))?;
            }

            readers.push(ReaderEntry {
                reader: unsafe {
                    RwData::new_unsized::<R::Reader>(Rc::new(RefCell::new(reader_cfg.init(bytes)?)))
                },
                apply_changes: |reader, bytes, moment, mut ranges_to_update| {
                    let reader = reader.try_downcast().unwrap();
                    // SAFETY: Since this is an async block, it cannot be .awaited while
                    // an RwData is borrowed.
                    let pa = unsafe { Pass::new() };

                    let mut new_ranges = if ranges_to_update.is_some() {
                        Some(RangeList::new(bytes.read(&pa, |bytes| bytes.len().byte())))
                    } else {
                        None
                    };

                    <R::Reader>::apply_changes(pa, reader, bytes, moment, new_ranges.as_mut());

                    // SAFETY: Since the last Pass was consumed, we can create new
                    // ones.
                    let mut pa = unsafe { Pass::new() };
                    if let Some(ranges_to_update) = ranges_to_update.take() {
                        ranges_to_update.write(&mut pa, |ru| {
                            ru.shift_by_changes(moment.changes());
                            ru.merge(new_ranges.unwrap())
                        });
                    }
                },
                ranges_to_update: RwData::new(RangeList::new(bytes.len().byte())),
                ty: TypeId::of::<R::Reader>(),
            });

            Ok(())
        })
    }

    pub fn get<R: Reader<U>>(&self) -> Option<RwData<R>> {
        if TypeId::of::<R>() == TypeId::of::<()>() {
            return None;
        }

        // SAFETY: You can't get another copy of Readers.
        let entry = unsafe {
            self.0.read_unsafe(|readers| {
                readers
                    .iter()
                    .find(|re| re.ty == TypeId::of::<R>())
                    .cloned()
            })?
        };

        // Since I am forcibly borrowing self for 'a, this should be safe, as
        // a mutable reference will prevent the removal of this Box's entry in
        // the list.
        entry.reader.try_downcast()
    }

    pub fn process_changes(&self, bytes: BytesDataMap<U>, moment: Moment) {
        const MAX_CHANGES_TO_CONSIDER: usize = 100;
        // SAFETY: Firstly, it is impossible to aqcuire a copy of this RwData,
        // nor is it possible to call this function from somewhere else, so
        // this first read_unsafe is valid. Secondly, the same applies to the
        // second write_unsafe of the RangeList RwData.
        unsafe {
            self.0.read_unsafe(|readers| {
                for entry in readers.iter() {
                    let new_ranges = if moment.len() <= MAX_CHANGES_TO_CONSIDER {
                        Some(entry.ranges_to_update.clone())
                    } else {
                        // If there are too many changes, cut on processing and
                        // just assume that everything needs to be updated.
                        entry.ranges_to_update.write_unsafe(|ru| {
                            // SAFETY: Since this is an async function, which will be executed via
                            // spawn_local, no other RwData borrows could be happening.
                            let pa = Pass::new();
                            *ru = RangeList::new(bytes.read(&pa, |bytes| bytes.len().byte()))
                        });
                        None
                    };

                    (entry.apply_changes)(entry.reader.clone(), bytes.clone(), moment, new_ranges);
                }
            });
        }
    }

    // TODO: Improve safety and throughput by creating a Text proxy and
    // making it async.
    pub fn update_range(&self, mut pa: Pass, text: &mut Text, within: Range<usize>) {
        // SAFETY: The same as the SAFETY section above.
        unsafe {
            self.0.read_unsafe(|readers| {
                for entry in readers.iter() {
                    entry.reader.write(&mut pa, |reader| {
                        entry.ranges_to_update.write_unsafe(|ranges| {
                            let old_ranges = std::mem::replace(ranges, RangeList::empty());

                            for range in old_ranges {
                                let (bytes, tags) = text.bytes_and_tags();
                                let (to_check, split_off) =
                                    split_range_within(range.clone(), within.clone());
                                    
                                if let Some(range) = to_check {
                                    reader.update_range(bytes, tags, range);
                                }

                                for range in split_off.into_iter().flatten() {
                                    ranges.add(range);
                                }
                            }
                        });
                    });
                }
            });
        }
    }

    pub fn needs_update(&self) -> bool {
        // SAFETY: This function is only called on File::update within a
        // region where a Pass was mutably borrowed, so we can create
        // another here.
        let pa = unsafe { Pass::new() };
        self.0.read(&pa, |readers| {
            readers
                .iter()
                .any(|re| re.ranges_to_update.read(&pa, |ru| !ru.is_empty()))
        })
    }
}

#[derive(Clone)]
pub struct RangeList(Vec<Range<usize>>);

impl RangeList {
    /// Return a new instance of [`RangeList`]
    ///
    /// By default, the whole [`Text`] should be updated.
    pub fn new(max: usize) -> Self {
        Self(vec![0..max])
    }

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

#[derive(Clone)]
struct ReaderEntry<U: Ui> {
    reader: RwData<dyn Reader<U>>,
    apply_changes: fn(RwData<dyn Reader<U>>, BytesDataMap<U>, Moment, Option<RwData<RangeList>>),
    ranges_to_update: RwData<RangeList>,
    ty: TypeId,
}

impl IntoIterator for RangeList {
    type IntoIter = std::vec::IntoIter<Range<usize>>;
    type Item = Range<usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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
