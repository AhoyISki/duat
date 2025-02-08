use std::ops::Range;

use super::{Text};
use crate::text::Change;

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
pub trait Reader: Send + Sync + 'static {
    #[allow(unused_variables)]
    fn before_change(&mut self, text: &Text, change: Change<&str>) {}

    /// Returns ranges that must be updated after a [`Change`]
    fn after_change(&mut self, text: &Text, change: Change<&str>) -> Vec<Range<u32>>;

    /// Updates a given [`Range`]
    ///
    /// This should take into account all changes that have taken
    /// place before this point.
    ///
    /// Must return a [`Vec`] of ranges that have been checked, so
    /// those can be removed from the checking list.
    fn update_range(&mut self, text: &mut Text, within: Range<u32>);
}
