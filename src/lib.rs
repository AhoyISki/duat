//! This plugin is supposed to be a jump list for [`Selections`]
//! movements to be used by text editing paradigms like `duat-kak` and
//! `duat-vim`.
//!
//! It works by recording the [`Selections`] and can retrieve previous
//! `Selections` values, taking [`Change`]s that took place into
//! account.
//!
//! [`Selections`]: duat_core::mode::Selections
//! [`Change`]: duat_core::buffer::Change
use std::{
    collections::HashMap,
    ops::Range,
    sync::atomic::{AtomicUsize, Ordering},
};

use duat_core::{
    Plugin, Plugins,
    buffer::{Buffer, BufferParts, BufferTracker, PerBuffer},
    context::Handle,
    data::Pass,
    hook::{self, BufferClosed, BufferOpened},
};
use gapbuf::GapBuffer;

static TRACKER: BufferTracker = BufferTracker::new();
static PARSERS: PerBuffer<Parser> = PerBuffer::new();

/// [`Plugin`]: Adds a `Jumps` parser to every [`Buffer`]
///
/// This `Jumps` parser can be used to retrieve previous
/// [`Selections`] values, "jumping" around in the history.
///
/// [`Selections`]: duat_core::mode::Selections
#[derive(Default)]
pub struct JumpList;

impl Plugin for JumpList {
    fn plug(self, _: &Plugins) {
        hook::add::<BufferOpened>(|pa, handle| {
            TRACKER.register_buffer(handle.write(pa));
            PARSERS.register(pa, handle, Parser::new());
        });

        hook::add::<BufferClosed>(|pa, handle| {
            PARSERS.unregister(pa, handle);
        });
    }
}

/// The state of the [`Selections`] at some point in time
///
/// It can either be a single selection, represented by an exclusive
/// [`Range<usize>`] of bytes, or a list of selections, also
/// represented by `Range<usize>`, alongside the index for the main
/// selection.
///
/// [`Selections`]: duat_core::mode::Selections
#[derive(Clone, Debug)]
pub enum Jump {
    Single(Range<usize>),
    Multiple(Vec<Range<usize>>, usize),
}

impl Jump {
    /// Wether the `Jump`'s selections are the same as those of the
    /// [`Selections`]
    ///
    /// [`Selections`]: duat_core::mode::Selections
    pub fn is_eq(&self, buf: &Buffer) -> bool {
        match self {
            Jump::Single(range) => {
                buf.selections().len() == 1
                    && buf.selections().main().byte_range(buf.bytes()) == *range
            }
            Jump::Multiple(ranges, main_i) => {
                buf.selections().len() == ranges.len()
                    && buf.selections().iter().zip(ranges.iter()).enumerate().all(
                        |(i, ((sel, is_main), range))| {
                            sel.byte_range(buf.bytes()) == *range && ((i == *main_i) == is_main)
                        },
                    )
            }
        }
    }

    /// Applies the `Jump` to the [`Selections`] of a [`Buffer`]
    ///
    /// [`Selections`]: duat_core::mode::Selections
    pub fn apply(&self, pa: &mut Pass, handle: &Handle) {
        match self {
            Jump::Single(selection) => {
                handle.write(pa).selections_mut().remove_extras();
                handle.edit_main(pa, |mut c| {
                    let start = c.text().point_at_byte(selection.start);
                    let end = c.text().point_at_byte(selection.end);
                    c.move_to(start..end)
                });
            }
            Jump::Multiple(selections, main) => {
                handle.write(pa).selections_mut().remove_extras();

                handle.edit_main(pa, |mut c| {
                    let mut is_first = true;
                    for selection in selections {
                        if !is_first {
                            c.copy();
                        }

                        let start = c.text().point_at_byte(selection.start);
                        let end = c.text().point_at_byte(selection.end);
                        c.move_to(start..end);
                        is_first = false;
                    }
                });
                handle.write(pa).selections_mut().set_main(*main);
            }
        }
    }

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
struct Parser(HashMap<JumpListId, (GapBuffer<Saved>, usize)>);

impl Parser {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn update(&mut self, parts: BufferParts) {
        // If there are no elements, every future jump is already correctly
        // shifted, so no need to add these Changes lists
        if parts.changes.len() == 0 || self.0.values().all(|(list, _)| list.is_empty()) {
            return;
        }

        for (list, cur) in self.0.values_mut() {
            list.truncate(*cur);

            if *cur == 0 {
                continue;
            }

            let changes = if let Saved::Changes(changes) = &mut list[*cur - 1] {
                changes
            } else {
                list.insert(*cur, Saved::Changes(Box::default()));
                *cur += 1;
                let Some(Saved::Changes(changes)) = list.get_mut(*cur - 1) else {
                    unreachable!();
                };
                changes
            };

            for change in parts.changes.clone() {
                let change = (
                    change.start().byte() as i32,
                    change.taken_end().byte() as i32,
                    change.added_end().byte() as i32,
                );
                changes.add_change(change);
            }
        }
    }
}

#[derive(Debug)]
enum Saved {
    Jump(Jump, JumpId),
    Changes(Box<Changes>),
}

/// A trait for recording and jumping [`Selections`]
///
/// This trait makes use of an internal [`BufferTracker`], so it
/// automatically takes into consideration any [`Change`]s that may
/// have taken place in between jumps.
///
/// [`Selections`]: duat_core::mode::Selections
/// [`Change`]: duat_core::buffer::Change
pub trait BufferJumps {
    /// Record the [`Buffer`]'s [`Selections`]
    ///
    /// If `allow_duplicates` is set to `false`, then the selections
    /// will not be recorded if that would mean two identical jumps in
    /// sequence.
    ///
    /// This function will return `None` if no jump was recorded,
    /// otherwise it will return a `Some(JumptId)`], which can be used
    /// to jump to specific selections via [`Handle::go_to_jump`].
    ///
    /// [`Selections`]: duat_core::mode::Selections
    fn record_jump(
        &self,
        pa: &mut Pass,
        jump_list_id: JumpListId,
        allow_duplicates: bool,
    ) -> Option<JumpId>;

    /// Jumps forwards or backwards through the [`Jump`]s on the list
    ///
    /// The `Jump` can either be a single selection, represented by
    /// an exclusive [`Range<usize>`], or it can be multiple
    /// selections, also represented by a `Range<usize>`, with the
    /// main selection's index included.
    ///
    /// This will return [`None`] if the [`JumpList`] plugin was not
    /// plugged, or if no jumps have been saved/all jumps have been
    /// removed.
    fn move_jumps_by(&self, pa: &mut Pass, jump_list_id: JumpListId, by: i32) -> Option<Jump>;

    /// Jumps to the [`Jump`] specified by a [`JumpId`]
    ///
    /// The `Jump` can either be a single selection, represented by
    /// an exclusive [`Range<usize>`], or it can be multiple
    /// selections, also represented by a `Range<usize>`, with the
    /// main selection's index included.
    ///
    /// This will return [`None`] if the [`JumpList`] plugin was not
    /// plugged, or if the [`JumpId`] in question doesn't belong to
    /// this [`Buffer`].
    ///
    /// If you want the `Jump` without actually jumping, see
    /// [`Handle::get_jump`].
    fn go_to_jump(&self, pa: &mut Pass, jump_list_id: JumpListId, id: JumpId) -> Option<Jump>;

    /// Gets the [`Jump`] specified by a [`JumpId`]
    ///
    /// The `Jump` can either be a single selection, represented by
    /// an exclusive [`Range<usize>`], or it can be multiple
    /// selections, also represented by a `Range<usize>`, with the
    /// main selection's index included.
    ///
    /// This will return [`None`] if the [`JumpList`] plugin was not
    /// plugged, or if the [`JumpId`] in question doesn't belong to
    /// this [`Buffer`].
    fn get_jump(&self, pa: &mut Pass, jump_list_id: JumpListId, id: JumpId) -> Option<Jump>;

    /// Records a non duplicated selection if there is none, returning
    /// it if successful. Otherwise returns the current [`JumpId`]
    fn record_or_get_current_jump(&self, pa: &mut Pass, jump_list_id: JumpListId) -> JumpId;
}

impl BufferJumps for Handle {
    fn record_jump(
        &self,
        pa: &mut Pass,
        jump_list_id: JumpListId,
        allow_duplicates: bool,
    ) -> Option<JumpId> {
        let (parser, buffer) = PARSERS.write(pa, self).unwrap();
        parser.update(TRACKER.parts(buffer).unwrap());

        let selections = buffer.selections();
        let (list, cur) = parser.0.entry(jump_list_id).or_default();

        if !allow_duplicates {
            for i in [Some(*cur), cur.checked_sub(1)].into_iter().flatten() {
                let Some(Saved::Jump(jump, _)) = list.get(i) else {
                    continue;
                };

                match jump {
                    Jump::Single(sel) => {
                        if selections.len() == 1
                            && selections.main().byte_range(buffer.bytes()) == *sel
                        {
                            return None;
                        }
                    }
                    Jump::Multiple(sels, main) => {
                        if *main == selections.main_index()
                            && sels.len() == selections.len()
                            && sels
                                .iter()
                                .zip(selections.iter())
                                .all(|(lhs, (rhs, _))| *lhs == rhs.byte_range(buffer.bytes()))
                        {
                            return None;
                        }
                    }
                }
            }
        }

        list.truncate(*cur);
        let jump_id = JumpId::new();

        if selections.len() == 1 {
            list.push_back(Saved::Jump(
                Jump::Single(selections.main().byte_range(buffer.bytes())),
                jump_id,
            ));
            *cur += 1;
        } else if selections.len() > 1 {
            list.push_back(Saved::Jump(
                Jump::Multiple(
                    selections
                        .iter()
                        .map(|(sel, _)| sel.byte_range(buffer.bytes()))
                        .collect(),
                    selections.main_index(),
                ),
                jump_id,
            ));
            *cur += 1;
        }

        Some(jump_id)
    }

    fn move_jumps_by(&self, pa: &mut Pass, jump_list_id: JumpListId, mut by: i32) -> Option<Jump> {
        let (parser, buffer) = PARSERS.write(pa, self).unwrap();
        parser.update(TRACKER.parts(buffer).unwrap());

        let mut changes = Changes::default();
        let mut last_seen = None;

        let (list, cur) = parser.0.entry(jump_list_id).or_default();

        let jump = if by >= 0 {
            loop {
                match list.get_mut(*cur) {
                    Some(Saved::Jump(jump, _)) => {
                        if by == 0 {
                            break Some(jump.clone());
                        } else if *cur + 1 < list.len() {
                            last_seen = Some(*cur);
                            by -= 1;
                            *cur += 1;
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
                match cur.checked_sub(1).and_then(|j| list.get_mut(j)) {
                    Some(Saved::Jump(jump, _)) => {
                        *cur -= 1;
                        if jump.shift(&changes) {
                            by += 1;
                            if by == 0 {
                                let jump = jump.clone();
                                if !changes.list.is_empty() && *cur > 0 {
                                    list.insert(*cur, Saved::Changes(Box::new(changes)));
                                    *cur += 1;
                                }
                                break Some(jump);
                            } else {
                                last_seen = Some(*cur);
                            }
                        } else {
                            if let Some(last_seen) = last_seen.as_mut() {
                                *last_seen -= 1;
                            }
                            list.remove(*cur);
                        }
                    }
                    Some(Saved::Changes(_)) => {
                        if let Some(last_seen) = last_seen.as_mut() {
                            *last_seen -= 1;
                        }
                        *cur -= 1;
                        let Saved::Changes(rev) = list.remove(*cur) else {
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
                let Some(Saved::Jump(jump, _)) = list.get(i) else {
                    unreachable!();
                };
                jump.clone()
            })
        })
    }

    fn go_to_jump(&self, pa: &mut Pass, jump_list_id: JumpListId, id: JumpId) -> Option<Jump> {
        get_jump(self, pa, jump_list_id, id, true)
    }

    fn get_jump(&self, pa: &mut Pass, jump_list_id: JumpListId, id: JumpId) -> Option<Jump> {
        get_jump(self, pa, jump_list_id, id, false)
    }

    fn record_or_get_current_jump(&self, pa: &mut Pass, jump_list_id: JumpListId) -> JumpId {
        if let Some(jump_id) = self.record_jump(pa, jump_list_id, false) {
            jump_id
        } else {
            let (parser, buffer) = PARSERS.write(pa, self).unwrap();
            parser.update(TRACKER.parts(buffer).unwrap());

            let (list, cur) = parser.0.get_mut(&jump_list_id).unwrap();

            list.range(..(*cur + 1).min(list.len()))
                .iter()
                .rev()
                .find_map(|saved| {
                    if let Saved::Jump(_, jump_id) = saved {
                        Some(*jump_id)
                    } else {
                        None
                    }
                })
                .unwrap()
        }
    }
}

fn get_jump(
    handle: &Handle,
    pa: &mut Pass,
    jump_list_id: JumpListId,
    id: JumpId,
    do_jump: bool,
) -> Option<Jump> {
    let (parser, buffer) = PARSERS.write(pa, handle).unwrap();
    parser.update(TRACKER.parts(buffer).unwrap());

    let (list, cur) = parser.0.entry(jump_list_id).or_default();

    let mut changes = Changes::default();
    let mut new_cur = *cur;

    let jump = (|| {
        // For better readability, consider `jumps.cur <= new_cur` to mean
        // "moving forwards", and `jumps.cur >= new_cur` to mean "moving
        // backwards".
        loop {
            match list.get_mut(new_cur) {
                Some(Saved::Jump(jump, other)) => {
                    if jump.shift(&changes) {
                        if *other == id {
                            let jump = jump.clone();
                            if !changes.list.is_empty() && new_cur > 0 {
                                list.insert(*cur, Saved::Changes(Box::new(changes)));
                                *cur += 1;
                            }
                            break Some(jump);
                        } else if *other < id && *cur <= new_cur {
                            new_cur += 1;
                        } else if *other > id && *cur >= new_cur {
                            new_cur = new_cur.checked_sub(1)?;
                        } else {
                            if !changes.list.is_empty() && new_cur > 0 {
                                list.insert(*cur, Saved::Changes(Box::new(changes)));
                                *cur += 1;
                            }
                            return None;
                        }
                    } else {
                        list.remove(new_cur);
                        *cur = cur.checked_sub(1)?;
                        new_cur -= 1;
                    }
                }
                Some(Saved::Changes(_)) => {
                    let Saved::Changes(rev) = list.remove(new_cur) else {
                        unreachable!();
                    };
                    *cur -= 1;
                    new_cur -= 1;
                    changes.merge(*rev);
                }
                None if *cur >= new_cur => new_cur = new_cur.checked_sub(1)?,
                None => break None,
            }
        }
    })();

    if jump.is_some() && do_jump {
        *cur = new_cur
    }

    jump
}

#[derive(Default, Debug)]
struct Changes {
    list: GapBuffer<(i32, i32, i32)>,
    from: usize,
    by: i32,
}

impl Changes {
    fn add_change(&mut self, (start, taken_end, added_end): (i32, i32, i32)) {
        let m_range = duat_core::utils::merging_range_by_guess_and_lazy_shift(
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

/// A struct representing a specific jump's position
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JumpId(usize);

impl JumpId {
    /// Returns a new unique `JumpId`
    fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

/// A struct representing a list of jumps
///
/// You can use this to maintain multiple separate jump lists for a
/// single [`Buffer`]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct JumpListId(usize);

impl JumpListId {
    /// Returns a new unique `JumpListId`
    ///
    /// Each id represents a unique sequence of [`Jump`] recordings
    /// for a given [`Buffer`]. You can use this to modify jump lists
    /// without invalidating other jumps. As an example, the
    /// `duatmode` crate uses this feature to keep track of all
    /// selection changes in a `Buffer`, as well as a separate list
    /// for specific jumps with the `g` key.
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for JumpListId {
    fn default() -> Self {
        Self::new()
    }
}
