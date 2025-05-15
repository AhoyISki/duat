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
    cell::RefCell,
    fs::File,
    ops::{Range, RangeBounds},
    pin::Pin,
    rc::Rc,
};

use tokio::task;

use super::{
    Builder, Bytes, Change, Key, Keys, Tag, Text, TextRange, err,
    history::Moment,
    split_range_within,
    tags::{RawTagsFn, Tags},
};
use crate::data::{DataMap, RwData2};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
#[allow(unused_variables)]
pub trait Reader: Send + Sync + 'static {
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
    async fn apply_changes(
        reader: RwData2<Self>,
        bytes: DataMap<File, &mut Bytes>,
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

pub trait ReaderCfg {
    type Reader: Reader;

    fn init(self, buffer: &mut Bytes) -> Result<Self::Reader, Text>;
}

pub struct MutTags<'a>(&'a mut Tags);

impl MutTags<'_> {
    /// Inserts a [`Tag`] at the given position
    pub fn insert(&mut self, key: Key, tag: Tag<impl RangeBounds<usize>, impl RawTagsFn>) {
        self.0.insert(key, tag);
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
    /// [key]: Keys
    /// [`File`]: crate::widgets::File
    /// [`Point`]: super::Point
    pub fn remove(&mut self, range: impl TextRange, keys: impl Keys) {
        let range = range.to_range_at(self.0.len_bytes());
        self.0.remove_from(range, keys)
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`File`]s, as there may be
    /// other [`Tag`] providers, and you should avoid messing with
    /// their tags.
    ///
    /// [`File`]: crate::widgets::File
    pub fn clear(&mut self) {
        *self.0 = Tags::new(self.0.len_bytes());
    }
}

impl<'a> std::fmt::Debug for MutTags<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Default)]
pub struct Readers(Vec<ReaderEntry>);

impl Readers {
    pub fn add<R: ReaderCfg>(&mut self, bytes: &mut Bytes, reader_cfg: R) -> Result<(), Builder> {
        if self.0.iter().any(|re| re.ty == TypeId::of::<R::Reader>()) {
            Err(err!(
                "There is already a reader of type [a]{}",
                crate::duat_name::<R::Reader>()
            ))?;
        }

        self.0.push(ReaderEntry {
            reader: unsafe {
                RwData2::new_unsized::<R::Reader>(Rc::new(RefCell::new(reader_cfg.init(bytes)?)))
            },
            apply_changes: |reader, mut bytes, moment, mut ranges_to_update| {
                let reader = reader.try_downcast().unwrap();
                Box::pin(async move {
                    let mut new_ranges = if ranges_to_update.is_some() {
                        Some(RangeList::new(bytes.read(|bytes| bytes.len().byte())))
                    } else {
                        None
                    };

                    <R::Reader>::apply_changes(reader, bytes, moment, new_ranges.as_mut()).await;

                    if let Some(ranges_to_update) = ranges_to_update.take() {
                        ranges_to_update.write(|ru| ru.merge(new_ranges.unwrap()));
                    }
                })
            },
            ranges_to_update: RwData2::new(RangeList::new(bytes.len().byte())),
            ty: TypeId::of::<R::Reader>(),
        });

        Ok(())
    }

    pub fn get_mut<'a, R: Reader>(&'a mut self) -> Option<RwData2<R>> {
        if TypeId::of::<R>() == TypeId::of::<()>() {
            return None;
        }

        let entry = self.0.iter_mut().find(|re| re.ty == TypeId::of::<R>())?;
        // Since I am forcibly borrowing self for 'a, this should be safe, as
        // a mutable reference will prevent the removal of this Box's entry in
        // the list.
        entry.reader.try_downcast()
    }

    pub async fn process_changes<'a>(
        &mut self,
        bytes: DataMap<File, &'a mut Bytes>,
        moment: Moment,
    ) {
        const MAX_CHANGES_TO_CONSIDER: usize = 100;
        for entry in self.0.iter_mut() {
            let new_ranges = if moment.clone().len() <= MAX_CHANGES_TO_CONSIDER {
                Some(entry.ranges_to_update.clone())
            } else {
                // If there are too many changes, cut on processing and
                // just assume that everything needs to be updated.
                entry
                    .ranges_to_update
                    .write(|ru| *ru = RangeList::new(bytes.read(|bytes| bytes.len().byte())));
                None
            };

            task::spawn_local((entry.apply_changes)(
                entry.reader.clone(),
                bytes.clone(),
                moment.clone(),
                new_ranges,
            ));
        }
    }

    pub fn update_range(&mut self, bytes: &mut Bytes, tags: &mut Tags, within: Range<usize>) {
        for (reader, ranges, _) in self.0.iter_mut() {
            let mut new_ranges = Vec::new();

            for range in ranges.iter() {
                let (to_check, split_off) = split_range_within(range.clone(), within.clone());
                if let Some(range) = to_check {
                    reader.update_range(bytes, MutTags(tags), range);
                }
                new_ranges.extend(split_off.into_iter().flatten());
            }
            *ranges = new_ranges;
        }
    }

    pub fn needs_update(&self) -> bool {
        self.0
            .iter()
            .any(|re| re.ranges_to_update.read(|ru| ru.is_empty()))
    }
}

#[derive(Clone)]
pub struct RangeList(Vec<Range<usize>>);

impl RangeList {
    /// Return a new instance of [`RangesList`]
    ///
    /// By default, the whole [`Text`] should be updated.
    pub fn new(max: usize) -> Self {
        Self(vec![0..max])
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

struct ReaderEntry {
    reader: RwData2<dyn Reader>,
    apply_changes: fn(
        RwData2<dyn Reader>,
        DataMap<File, &mut Bytes>,
        Moment,
        Option<RwData2<RangeList>>,
    ) -> Pin<Box<dyn Future<Output = ()>>>,
    ranges_to_update: RwData2<RangeList>,
    ty: TypeId,
}
