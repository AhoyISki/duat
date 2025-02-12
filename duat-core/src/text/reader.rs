//! Struct that can react to change in the [`Text`]
//!
//! These structs will be informed of every [`Change`] that happens in
//! the [`Text`], and are allowed to act accordingly. This action will
//! be done by telling the [`Text`] what parts need to be updated.
//! They will then be updated when deemed relevant by the [`Ui`] in
//! use (usually when these become visible).
//!
//! [`Ui`]: crate::ui::Ui
use std::ops::Range;

use super::Text;
use crate::text::Change;

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
pub trait Reader: Send + Sync + 'static {
    /// Applies the [`Change`]s to this [`Reader`]
    ///
    /// After this point, even if no other functions are called, the
    /// state of this [`Reader`] should reflect the state of the
    /// [`Text`].
    #[allow(unused_variables)]
    fn apply_changes(&mut self, text: &Text, changes: &[Change<&str>]);

    /// Returns ranges that must be updated after a [`Change`]
    ///
    /// Critically, this may not necessarily be called, so the state must have been finished by
    fn ranges_to_update(&mut self, text: &Text, changes: &[Change<&str>]) -> Vec<Range<usize>>;

    /// Updates a given [`Range`]
    ///
    /// This should take into account all changes that have taken
    /// place before this point.
    ///
    /// Must return a [`Vec`] of ranges that have been checked, so
    /// those can be removed from the checking list.
    fn update_range(&mut self, text: &mut Text, within: Range<usize>);
}
