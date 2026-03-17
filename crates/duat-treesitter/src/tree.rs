use std::ops::Range;

use duat_core::Ranges;
use tree_sitter::{InputEdit, Tree as TsTree};

/// An injected tree, may span multiple [`Range`]s
#[derive(Debug)]
pub struct Tree {
    pub region: Ranges,
    pub ts_tree: Option<TsTree>,
    pub needs_parse: bool,
    pub _combined_pattern_index: Option<usize>,
}

impl Tree {
    /// Returns a new
    pub fn new(region: Ranges) -> Self {
        Self {
            region,
            ts_tree: None,
            needs_parse: true,
            _combined_pattern_index: None,
        }
    }
}

#[derive(Debug)]
pub struct Trees(Vec<Tree>);

impl Trees {
    /// Returns a new [`Regions`]
    pub fn new(regions: impl IntoIterator<Item = Ranges>) -> Self {
        let mut regions: Vec<_> = regions.into_iter().map(Tree::new).collect();
        regions.sort_unstable_by(|lhs, rhs| lhs.region.cmp(&rhs.region));

        Self(regions)
    }

    /// Returns an [`Iterator`] over the [`Tree`]s
    pub fn iter(&self) -> std::slice::Iter<'_, Tree> {
        self.0.iter()
    }

    /// Returns an [`Iterator`] over all regions that intersect the
    /// [`Range`] given
    #[track_caller]
    pub fn intersecting(&self, range: Range<usize>) -> impl Iterator<Item = (usize, &Tree)> + '_ {
        self.0
            .iter()
            .enumerate()
            .filter(move |(_, tree)| tree.region.intersects_with(range.clone()))
    }

    /// Returns an [`Iterator`] over all regions that intersect the
    /// [`Range`] given
    pub fn intersecting_mut(
        &mut self,
        range: Range<usize>,
    ) -> impl Iterator<Item = (usize, &mut Tree)> + '_ {
        self.0
            .iter_mut()
            .enumerate()
            .filter(move |(_, tree)| tree.region.intersects_with(range.clone()))
    }

    /// Adds [`Ranges`] to be the `Regions`
    ///
    /// Returns `true` if a new `Ranges` was added.
    pub fn add_region(&mut self, region: Ranges) -> bool {
        let mut add_new = true;

        for range in region.iter() {
            let mut regions_to_remove = Vec::new();

            for (i, tree) in self.intersecting(range) {
                if tree.region == region {
                    add_new = false;
                } else {
                    regions_to_remove.push(i);
                }
            }

            for i in regions_to_remove.into_iter().rev() {
                self.0.remove(i);
            }
        }

        if add_new {
            let i = self
                .0
                .binary_search_by(|tree| tree.region.cmp(&region))
                .unwrap_err();

            self.0.insert(i, Tree::new(region));
            true
        } else {
            false
        }
    }

    /// Removes the `n`th element from the list of [`Range`]s
    pub fn remove(&mut self, n: usize) {
        self.0.remove(n);
    }

    /// Edits all the [`Ranges`] through an [`InputEdit`]
    pub fn edit(&mut self, edit: &InputEdit) {
        for tree in self.0.iter_mut() {
            if let Some(ts_tree) = tree.ts_tree.as_mut() {
                ts_tree.edit(edit);
            }

            tree.region.shift_by(
                edit.start_byte,
                edit.new_end_byte as i32 - edit.old_end_byte as i32,
            );
        }
    }
}
