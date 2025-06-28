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
    text::{Change, Moment, Text, TextParts, txt},
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
    /// [`Tag`]: crate::text::Tag
    #[allow(unused_variables)]
    fn apply_changes(
        pa: &mut Pass,
        reader: RwData<Self>,
        bytes: BytesDataMap<U>,
        moment: Moment,
        ranges_to_update: Option<&mut RangeList>,
    ) where
        Self: Sized,
    {
    }

    /// Updates in a given [`Range`]
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
    ///
    /// [`Tag`]: crate::text::Tag
    fn update_range(&mut self, parts: TextParts, within: Option<Range<usize>>);
}

/// A [`Reader`] builder struct
pub trait ReaderCfg<U: Ui> {
    /// The [`Reader`] that this [`ReaderCfg`] will construct
    type Reader: Reader<U>;

    /// Constructs the [`Reader`]
    fn init(self, pa: &mut Pass, bytes: BytesDataMap<U>) -> Result<Self::Reader, Text>;
}

#[derive(Default, Clone)]
pub(super) struct Readers<U: Ui>(RwData<Vec<ReaderEntry<U>>>);

impl<U: Ui> Readers<U> {
    /// Attempts to add  a [`Reader`]
    pub(super) fn add<R: ReaderCfg<U>>(
        &self,
        pa: &mut Pass,
        bytes: BytesDataMap<U>,
        reader_cfg: R,
    ) -> Result<(), Text> {
        if self.0.read(pa, |rds| {
            rds.iter().any(|re| re.ty == TypeId::of::<R::Reader>())
        }) {
            Err(txt!(
                "There is already a reader of type [a]{}",
                crate::duat_name::<R::Reader>()
            ))?;
        }

        let reader_entry = ReaderEntry {
            reader: unsafe {
                RwData::new_unsized::<R::Reader>(Rc::new(RefCell::new(
                    reader_cfg.init(pa, bytes.clone())?,
                )))
            },
            apply_changes: |pa, reader, bytes, moment, mut ranges_to_update| {
                let reader = reader.try_downcast().unwrap();
                let mut new_ranges = if ranges_to_update.is_some() {
                    Some(RangeList::new(bytes.write(pa, |bytes| bytes.len().byte())))
                } else {
                    None
                };

                <R::Reader>::apply_changes(pa, reader, bytes, moment, new_ranges.as_mut());

                if let Some(ranges_to_update) = ranges_to_update.take() {
                    ranges_to_update.write(pa, |ru| {
                        ru.shift_by_changes(moment.changes());
                        ru.merge(new_ranges.unwrap())
                    });
                }
            },
            ranges_to_update: RwData::new(RangeList::new(bytes.write(pa, |b| b.len().byte()))),
            ty: TypeId::of::<R::Reader>(),
        };

        let mut readers = self.0.acquire_mut(pa);
        readers.push(reader_entry);

        Ok(())
    }

    /// Gets a specific [`Reader`], if it was added in
    pub(super) fn get<R: Reader<U>>(&self) -> Option<RwData<R>> {
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

    /// Makes each [`Reader`] process a [`Moment`]
    pub(super) fn process_moment(&self, pa: &mut Pass, bytes: BytesDataMap<U>, moment: Moment) {
        const MAX_CHANGES_TO_CONSIDER: usize = 100;

        let readers = std::mem::take(&mut *self.0.acquire_mut(pa));
        for entry in readers.iter() {
            let new_ranges = if moment.len() <= MAX_CHANGES_TO_CONSIDER {
                Some(entry.ranges_to_update.clone())
            } else {
                // If there are too many changes, cut on processing and
                // just assume that everything needs to be updated.
                let new_ru = RangeList::new(bytes.write(pa, |bytes| bytes.len().byte()));
                entry.ranges_to_update.write(pa, |ru| *ru = new_ru);
                None
            };

            (entry.apply_changes)(pa, entry.reader.clone(), bytes.clone(), moment, new_ranges);
        }

        self.0.acquire_mut(pa).extend(readers);
    }

    /// Updates the [`Reader`]s on a given range
    pub(super) fn update_range(&self, pa: &mut Pass, text: &mut Text, within: Range<usize>) {
        // SAFETY: The same as the SAFETY section above.
        unsafe {
            self.0.read_unsafe(|readers| {
                for entry in readers.iter() {
                    entry.reader.write(pa, |reader| {
                        entry.ranges_to_update.write_unsafe(|ranges| {
                            let old_ranges = std::mem::replace(ranges, RangeList::empty());

                            for range in old_ranges {
                                let parts = text.parts();
                                let (to_check, split_off) =
                                    split_range_within(range.clone(), within.clone());

                                reader.update_range(parts, to_check);

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

    /// Wether any of the [`Reader`]s need to be updated
    pub(super) fn needs_update(&self) -> bool {
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

#[derive(Clone)]
struct ReaderEntry<U: Ui> {
    reader: RwData<dyn Reader<U>>,
    apply_changes:
        fn(&mut Pass, RwData<dyn Reader<U>>, BytesDataMap<U>, Moment, Option<RwData<RangeList>>),
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
