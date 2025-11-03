//! This plugin is supposed to be a jump list for [`Selections`]
//! movements to be used by text editing paradigms like `duat-kak` and
//! `duat-vim`.
//!
//! It works by recording the [`Selections`] and can retrieve previous
//! [`Selections`] values, taking [`Change`]s that took place into
//! account.
//!
//! [`Selections`]: duat_core::mode::Selections
//! [`Change`]: duat_core::text::Change
#![feature(decl_macro, iter_order_by)]

use std::ops::Range;

use gapbuf::GapBuffer;

/// [`Plugin`]: Adds a [`Jumps`] parser to every [`Buffer`]
///
/// This [`Jumps`] parser can be used to retrieve previous
/// [`Selections`] values, "jumping" around in the history.
#[derive(Default)]
pub struct JumpList;

impl Plugin for JumpList {
    fn plug(self, _: &Plugins) {
        hook::add::<Buffer>(|pa, handle| handle.add_parser(pa, Jumps::new));
    }
}

/// The state of the [`Selections`] at some point in time
///
/// It can either be a single selection, represented by an exclusive
/// [`Range<usize>`] of bytes, or a list of selections, also
/// represented by [`Range<usize>`], alongside the index for the main
/// selection.
///
/// [`Selections`]: duat_core::mode::Selections
#[derive(Clone, Debug)]
pub enum Jump {
    Single(Range<usize>),
    Multiple(Vec<Range<usize>>, usize),
}

impl Jump {
    fn shift(&mut self, changes: &Changes) -> bool {
        match self {
            Jump::Single(selection) => {
                if let Some(new) = changes.shift_selection(selection.clone()) {
                    *selection = new;
                    true
                } else {
                    false
                }
            }
            Jump::Multiple(selections, _) => {
                changes.shift_selections(selections);
                !selections.is_empty()
            }
        }
    }
}

#[derive(Debug)]
struct Jumps {
    list: GapBuffer<Saved>,
    tracker: BufferTracker,
    cur: usize,
}

impl Jumps {
    fn new(tracker: BufferTracker) -> Self {
        Self { list: GapBuffer::new(), tracker, cur: 0 }
    }
}

impl Parser for Jumps {
    fn parse(&mut self) -> bool {
        false
    }

    fn before_get(&mut self) {
        self.tracker.update();

        // If there are no elements, every future jump is already correctly
        // shifted, so no need to add these Changes lists
        if self.list.is_empty() || self.tracker.moment().is_empty() {
            return;
        }
        self.list.truncate(self.cur);

        if self.cur == 0 {
            return;
        }

        let changes = if let Saved::Changes(changes) = &mut self.list[self.cur - 1] {
            changes
        } else {
            self.list.insert(self.cur, Saved::Changes(Box::default()));
            self.cur += 1;
            let Some(Saved::Changes(changes)) = self.list.get_mut(self.cur - 1) else {
                unreachable!();
            };
            changes
        };

        for change in self.tracker.moment().changes() {
            let change = (
                change.start().byte() as i32,
                change.taken_end().byte() as i32,
                change.added_end().byte() as i32,
            );
            changes.add_change(change);
        }
    }

    fn before_try_get(&mut self) -> bool {
        self.before_get();
        true
    }
}

#[derive(Debug)]
enum Saved {
    Jump(Jump),
    Changes(Box<Changes>),
}

/// A trait for recording and jumping [`Selections`]
///
/// This trait makes use of an internal [`Parser`], so it
/// automatically takes into consideration any [`Change`]s that may
/// have taken place in between jumps.
///
/// [`Selections`]: duat_core::mode::Selections
/// [`Change`]: duat_core::text::Change
pub trait BufferJumps {
    /// Record the [`Buffer`]'s [`Selections`]
    ///
    /// If `allow_duplicates` is set to `false`, then the selections
    /// will not be recorded if that would mean two identical jumps in
    /// sequence.
    ///
    /// This function will return `false` if no jump was recorded.
    ///
    /// [`Selections`]: duat_core::mode::Selections
    fn record_selections(&mut self, allow_duplicates: bool) -> bool;

    /// Jumps forwards or backwards through the [`Jump`]s on the list
    ///
    /// The [`Jump`] can either be a single selection, represented by
    /// an exclusive [`Range<usize>`], or it can be multiple
    /// selections, also represented by a [`Range<usize>`], with the
    /// main selection's index included.
    ///
    /// This will return [`None`] if the [`JumpList`] plugin was not
    /// plugged, or if no jumps have been saved/all jumps have been
    /// removed.
    fn jump_selections_by(&mut self, by: i32) -> Option<Jump>;

    /// Jumps to the `n`th [`Jump`]
    ///
    /// The [`Jump`] can either be a single selection, represented by
    /// an exclusive [`Range<usize>`], or it can be multiple
    /// selections, also represented by a [`Range<usize>`], with the
    /// main selection's index included.
    ///
    /// This will return [`None`] if the [`JumpList`] plugin was not
    /// plugged, or if no jumps have been saved/all jumps have been
    /// removed.
    fn jump_to_selections(&mut self, n: usize) -> Option<Jump>;
}

impl BufferJumps for Buffer {
    fn record_selections(&mut self, allow_duplicates: bool) -> bool {
        self.write_parser(|jumps: &mut Jumps| {
            let selections = self.selections();

            if !allow_duplicates {
                for i in [Some(jumps.cur), jumps.cur.checked_sub(1)]
                    .into_iter()
                    .flatten()
                {
                    let Some(Saved::Jump(jump)) = jumps.list.get(i) else {
                        continue;
                    };

                    match jump {
                        Jump::Single(sel) => {
                            if selections.len() == 1
                                && selections.get_main().unwrap().byte_range(self.bytes()) == *sel
                            {
                                return false;
                            }
                        }
                        Jump::Multiple(sels, main) => {
                            if *main == selections.main_index()
                                && sels.iter().eq_by(selections.iter(), |lhs, (rhs, _)| {
                                    *lhs == rhs.byte_range(self.bytes())
                                })
                            {
                                return false;
                            }
                        }
                    }
                }
            }

            if selections.len() == 1 {
                jumps.list.insert(
                    jumps.cur,
                    Saved::Jump(Jump::Single(
                        selections.get_main().unwrap().byte_range(self.bytes()),
                    )),
                );
            } else if selections.len() > 1 {
                jumps.list.insert(
                    jumps.cur,
                    Saved::Jump(Jump::Multiple(
                        selections
                            .iter()
                            .map(|(sel, _)| sel.byte_range(self.bytes()))
                            .collect(),
                        selections.main_index(),
                    )),
                );
            }
            jumps.cur += 1;

            true
        })
        .unwrap()
    }

    fn jump_selections_by(&mut self, mut by: i32) -> Option<Jump> {
        self.write_parser(|jumps: &mut Jumps| {
            let mut changes = Changes::default();
            let mut last_seen = None;

            let jump = if by >= 0 {
                loop {
                    match jumps.list.get_mut(jumps.cur) {
                        Some(Saved::Jump(jump)) => {
                            if by == 0 {
                                break Some(jump.clone());
                            } else if jumps.cur + 1 < jumps.list.len() {
                                last_seen = Some(jumps.cur);
                                by -= 1;
                                jumps.cur += 1;
                            } else {
                                break None;
                            }
                        }
                        Some(Saved::Changes(_)) => unreachable!(),
                        None => break None,
                    }
                }
            } else {
                loop {
                    match jumps.cur.checked_sub(1).and_then(|j| jumps.list.get_mut(j)) {
                        Some(Saved::Jump(jump)) => {
                            jumps.cur -= 1;
                            if jump.shift(&changes) {
                                by += 1;
                                if by == 0 {
                                    let jump = jump.clone();
                                    if !changes.list.is_empty() && jumps.cur > 0 {
                                        jumps
                                            .list
                                            .insert(jumps.cur, Saved::Changes(Box::new(changes)));
                                        jumps.cur += 1;
                                    }
                                    break Some(jump);
                                } else {
                                    last_seen = Some(jumps.cur);
                                }
                            } else {
                                if let Some(last_seen) = last_seen.as_mut() {
                                    *last_seen -= 1;
                                }
                                jumps.list.remove(jumps.cur);
                            }
                        }
                        Some(Saved::Changes(_)) => {
                            if let Some(last_seen) = last_seen.as_mut() {
                                *last_seen -= 1;
                            }
                            jumps.cur -= 1;
                            let Saved::Changes(rev) = jumps.list.remove(jumps.cur) else {
                                unreachable!();
                            };
                            changes.merge(*rev);
                        }
                        None => break None,
                    }
                }
            };

            jump.or_else(|| {
                last_seen.map(|i| {
                    let Some(Saved::Jump(jump)) = jumps.list.get(i) else {
                        unreachable!();
                    };
                    jump.clone()
                })
            })
        })
        .flatten()
    }

    fn jump_to_selections(&mut self, n: usize) -> Option<Jump> {
        let cur_n = self.write_parser(|jumps: &mut Jumps| {
            jumps
                .list
                .iter()
                .take(jumps.cur)
                .filter(|s| matches!(s, Saved::Jump(_)))
                .count()
        })?;

        self.jump_selections_by(n as i32 - cur_n as i32)
    }
}

#[derive(Default, Debug)]
struct Changes {
    list: GapBuffer<(i32, i32, i32)>,
    from: usize,
    by: i32,
}

impl Changes {
    fn add_change(&mut self, (start, taken_end, added_end): (i32, i32, i32)) {
        let m_range = duat::utils::merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (0, [start, taken_end]),
            (self.from, self.by, 0, std::ops::Add::add),
            (|(start, ..)| *start, |(.., added_end)| *added_end),
        );

        if self.by != 0 && self.from < m_range.end {
            for (start, taken_end, added_end) in &mut self.list.range_mut(self.from..m_range.end) {
                *start += self.by;
                *taken_end += self.by;
                *added_end += self.by;
            }
        } else if self.by != 0 && m_range.end < self.from {
            for (start, taken_end, added_end) in &mut self.list.range_mut(self.from..m_range.end) {
                *start -= self.by;
                *taken_end -= self.by;
                *added_end -= self.by;
            }
        }

        let (start, taken_end, added_end) = if m_range.start < m_range.end {
            let (first_start, ..) = self.list[m_range.start];
            let (_, last_taken_end, last_added_end) = self.list[m_range.end - 1];
            (
                start.min(first_start),
                taken_end.max(last_taken_end),
                added_end.max(last_added_end),
            )
        } else {
            (start, taken_end, added_end)
        };

        self.list.splice(
            m_range.clone(),
            (taken_end != added_end).then_some((start, taken_end, added_end)),
        );

        let new_from = m_range.start + (taken_end != added_end) as usize;
        if new_from < self.list.len() {
            self.from = new_from;
        } else {
            self.from = 0;
            self.by = 0;
        }
    }

    fn merge(&mut self, other: Self) {
        for change in other.iter() {
            self.add_change(change);
        }
    }

    fn iter(&self) -> impl Iterator<Item = (i32, i32, i32)> {
        self.list
            .iter()
            .enumerate()
            .map(|(i, &(start, taken_end, added_end))| {
                if self.by != 0 && i >= self.from {
                    (start + self.by, taken_end + self.by, added_end + self.by)
                } else {
                    (start, taken_end, added_end)
                }
            })
    }

    fn shift_selection(&self, mut selection: Range<usize>) -> Option<Range<usize>> {
        for change in self.iter() {
            selection = shift_selection(selection.clone(), change)?;

            if change.0 as usize >= selection.end {
                break;
            }
        }

        Some(selection)
    }

    fn shift_selections(&self, selections: &mut Vec<Range<usize>>) {
        let mut total_shift = 0;
        let mut changes = self.iter().peekable();

        selections.retain_mut(|selection| {
            selection.start = (selection.start as i32 + total_shift) as usize;
            selection.end = (selection.end as i32 + total_shift) as usize;

            while let Some(&change) = changes.peek() {
                let Some(new) = shift_selection(selection.clone(), change) else {
                    return false;
                };
                *selection = new;

                if change.1 as usize <= selection.start {
                    changes.next();
                    total_shift += change.2 - change.1;
                } else if change.0 as usize >= selection.end {
                    break;
                }
            }

            true
        });
    }
}

fn shift_selection(
    mut selection: Range<usize>,
    (start, taken_end, added_end): (i32, i32, i32),
) -> Option<Range<usize>> {
    let shift = added_end - taken_end;
    let (start, taken_end) = (start as usize, taken_end as usize);

    if taken_end <= selection.start {
        selection.start = (selection.start as i32 + shift) as usize;
        selection.end = (selection.end as i32 + shift) as usize;
    } else if taken_end < selection.end {
        selection.start = added_end as usize;
        selection.end = (selection.end as i32 + shift) as usize;
    } else if start <= selection.start {
        return None;
    } else if start < selection.end {
        selection.end = start
    }
    Some(selection)
}
