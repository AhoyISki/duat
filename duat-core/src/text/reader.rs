//! Struct that can react to change in the [`Text`]
//!
//! These structs will be informed of every [`Change`] that happens in
//! the [`Text`], and are allowed to act accordingly. This action will
//! be done by telling the [`Text`] what parts need to be updated.
//! They will then be updated when deemed relevant by the [`Ui`] in
//! use (usually when these become visible).
//!
//! [`Ui`]: crate::ui::Ui
use std::{any::TypeId, ops::Range};

use super::{
    Bytes, Change, Key, Keys, Tag, Text, TextRange, err, merge_range_in, split_range_within,
    tags::Tags,
};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
#[allow(unused_variables)]
pub trait Reader: Send + Sync + 'static {
    /// A convenience type for public facing APIs
    ///
    /// For many public facing APIs, it would be convenient for the
    /// [`Reader`] to have access to the [`Buffer`] of the [`Text`],
    /// this type is essentially a way to attach a [`Buffer`] to this
    /// [`Reader`], simplifying the public API.
    ///
    /// If you don't need a public reader API, just set this to `()`.
    type PublicReader<'a>
    where
        Self: Sized;

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
    /// Another thing to note is the [`Reader::ranges_to_update`]
    /// function. Since Duat might not care about what ranges actually
    /// have to be updated, you should limit the processing of said
    /// ranges to only that function, not this one.
    fn apply_changes(&mut self, bytes: &mut Bytes, changes: &[Change<&str>]);

    /// Returns ranges that must be updated after a [`Change`]
    ///
    /// Critically, this may not necessarily be called, so the state
    /// must have been finished being updated by this point, via
    /// [`Reader::apply_changes`].
    fn ranges_to_update(
        &mut self,
        bytes: &mut Bytes,
        changes: &[Change<&str>],
    ) -> Vec<Range<usize>>;

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
    fn update_range(&mut self, bytes: &mut Bytes, tags: MutTags, within: Range<usize>);

    /// The public facing portion of this [`Reader`]
    ///
    /// The [`PublicReader`] is essentially just the [`Reader`], but
    /// with a [`Buffer`] attached to it for convenience.
    ///
    /// [`PublicReader`]: Reader::PublicReader
    fn public_reader<'a>(&'a mut self, bytes: &'a mut Bytes) -> Self::PublicReader<'a>
    where
        Self: Sized,
    {
        unreachable!("This Reader hasn't set up a public facing API.");
    }
}

pub trait ReaderCfg {
    type Reader: Reader;

    fn init(self, buffer: &mut Bytes, tags: MutTags) -> Result<Self::Reader, Text>;
}

pub struct MutTags<'a>(&'a mut Tags);

impl MutTags<'_> {
    /// Inserts a [`Tag`] at the given position
    pub fn insert(&mut self, at: usize, tag: Tag, key: Key) {
        self.0.insert(at, tag, key);
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
    pub fn remove(&mut self, range: impl TextRange, keys: impl Keys) {
        let range = range.to_range_at(self.0.len_bytes());
        if range.end == range.start + 1 {
            self.0.remove_at(range.start, keys)
        } else {
            self.0.remove_from(range, keys)
        }
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

#[derive(Default)]
pub struct Readers(Vec<(Box<dyn Reader>, Vec<Range<usize>>, TypeId)>);

impl Readers {
    pub fn add<R: ReaderCfg>(
        &mut self,
        bytes: &mut Bytes,
        tags: &mut Tags,
        reader_cfg: R,
    ) -> Result<(), Text> {
        if self.0.iter().any(|(.., t)| *t == TypeId::of::<R::Reader>()) {
            return Err(
                err!("There is already a reader of type " [*a] { crate::duat_name::<R::Reader>() }),
            );
        }

        let reader = Box::new(reader_cfg.init(bytes, MutTags(tags))?);
        self.0.push((reader, Vec::new(), TypeId::of::<R::Reader>()));
        Ok(())
    }

    pub fn get_mut<'a, R: Reader>(
        &'a mut self,
        bytes: &'a mut Bytes,
    ) -> Option<R::PublicReader<'a>> {
        if TypeId::of::<R::PublicReader<'static>>() == TypeId::of::<()>() {
            return None;
        }

        let (reader, ranges, _) = self.0.iter_mut().find(|(.., t)| *t == TypeId::of::<R>())?;
        // Since it's a mutable reference, it's possible that more ranges will
        // need to be changed, so do this just to be sure.
        *ranges = vec![0..bytes.len().byte()];
        let ptr = Box::as_mut_ptr(reader);

        // Since I am forcibly borrowing self for 'a, this should be safe, as
        // a mutable reference will prevent the removal of this Box's entry in
        // the list.
        let reader = unsafe { (ptr as *mut R).as_mut() }.unwrap();

        Some(reader.public_reader(bytes))
    }

    pub fn process_changes(&mut self, bytes: &mut Bytes, changes: &[Change<&str>]) {
        const MAX_RANGES_TO_CONSIDER: usize = 15;
        for (reader, ..) in self.0.iter_mut() {
            reader.apply_changes(bytes, changes);
        }

        if changes.len() <= MAX_RANGES_TO_CONSIDER {
            for (reader, ranges, _) in self.0.iter_mut() {
                for range in reader.ranges_to_update(bytes, changes) {
                    if !range.is_empty() {
                        merge_range_in(ranges, range);
                    }
                }
            }
        // If there are too many changes, cut on processing and
        // just assume that everything needs to be updated.
        } else {
            for (_, ranges, _) in self.0.iter_mut() {
                *ranges = vec![0..bytes.len().byte()];
            }
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
        self.0.iter().any(|(_, ranges, _)| !ranges.is_empty())
    }
}
