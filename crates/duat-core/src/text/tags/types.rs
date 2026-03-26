//! Types for convenience and efficiency.
//!
//! There are two "types" of tag: [`Tag`]s and [`RawTag`]s. [`Tag`]s
//! are what is show to the end user, being convenient in the way they
//! include extra information. [`RawTag`]s, on the other hand, are
//! meant to be as small as possible in order not to waste memory, as
//! they will be stored in the [`Text`]. As such, they have as little
//! information as possible, occupying only 8 bytes.
use std::{
    ops::Range,
    sync::{Arc, Mutex, atomic::AtomicBool},
};

use RawTag::*;
use crossterm::event::{MouseButton, MouseEventKind};

use super::{GhostId, SpawnId, Tagger};
use crate::{
    context::{self, Handle},
    data::Pass,
    form::{self, FormId},
    mode::ToggleEvent,
    text::{Point, Text, TextRange, tags::ToggleId},
    ui::{DynSpawnSpecs, Widget},
};

/// [`Tag`]s are used for every visual modification to [`Text`].
///
/// `Tag`s allow for all sorts of configuration on the `Text`, like
/// changing colors throug [`Form`]s, or text alignment, or
/// [`Spacer`]s, or even concealing and ghost `Text`.
///
/// Currently, these are the [`Tag`]s in Duat:
///
/// - [`FormTag`]: Applies a `Form` on a [range]; `Text`. Can be an
///   actual `caret` or just a temporary `Form`.
/// - [`Spacer`]: Lets you put arbitrary equally sized spaces on a
///   line.
/// - [`Ghost`] represents `Text` that "isn't really there", and is
///   subdivided in two categories:
///   - [`Ghost::inlay`]: Places ghost `Text` _inside_ the `Text`.
///     When printing, this will move the regular `Text` around in
///     order to fit. Example: diagnostics.
///   - [`Ghost::overlay`]: Places  ghost `Text` _over_ the `Text`.
///     When printing, this will be printed over the regularly printed
///     `Text`. It will also "pass through" cursor positions. Example:
///     indent lines.
/// - [`Conceal`]: Hides a range in the `Text`, mostly only useful in
///   the [`Buffer`] [`Widget`].
/// - [`Toggle`]: Creates a region that can be interacted with through
///   the mouse pointer.
/// - [`SpawnTag`]: Spawns a floating `Widget` on a position in
///   `Text`. Said floating widget will move around as the position
///   does the same.
///
/// [`Form`]: crate::form::Form
/// [range]: TextRange
/// [`Buffer`]: crate::buffer::Buffer
/// [`Widget`]: crate::ui::Widget
pub trait Tag<Index>: Sized {
    /// A meta `Tag` is one that changes the layout of the [`Text`]
    /// itself.
    ///
    /// The only meta `Tag`s are [`Ghost`], [`Conceal`] and
    /// [`Spacer`].
    const IS_META: bool;

    /// Gets the [`RawTag`]s and a possible return id from the `Tag`.
    #[doc(hidden)]
    fn get_raw(
        &mut self,
        tags: &super::InnerTags,
        index: Index,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>);

    /// An action to take place if the [`RawTag`]s are successfully
    /// added.
    #[doc(hidden)]
    #[allow(unused_variables)]
    fn on_insertion(self, tags: &mut super::InnerTags) {}
}

////////// Form-like InnerTags

/// [`Tag`]: Applies a [`Form`] to a given [range] in the [`Text`].
///
/// This struct can be created from the [`FormId::to_tag`] method,
/// granting it a priority that is used to properly order the
/// [`RawTag`]s within.
///
/// The [`Form`] is able to intersect with other [`Form`]s, which,
/// unlike when pushing [`Form`]s to a [`Builder`], interfere
/// constructively, with the latest attributes and colors winning out.
///
/// [`Form`]: crate::form::Form
/// [range]: TextRange
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormTag {
    /// The [`FormId`] that will be added.
    pub id: FormId,
    /// The priority with which it will be added.
    ///
    /// The priority determines which forms should affect a character,
    /// given that multiple are used at the same time.
    ///
    /// For example, if a `FormTag` of priority 50 is applying a form
    /// with a blue foreground, and another of priority 100 is
    /// applying a red foreground, the latter one will be used.
    pub priority: u8,
}

impl<I: TextRange> Tag<I> for FormTag {
    const IS_META: bool = false;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &super::InnerTags,
        index: I,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let FormTag { id, priority } = *self;
        let range = index.to_range(max);
        {
            let s_tag = PushForm(tagger, id, priority);
            let e_tag = PopForm(tagger, id);
            ((range.start, s_tag), Some((range.end, e_tag)))
        }
    }
}

////////// Meta Tags

/// [`Tag`]: A spacer for more advanced alignment.
///
/// When printing this screen line (one row on screen, i.e. until
/// it wraps), Instead of following the current alignment, will
/// put spacing between the next and previous characters. The
/// length of the space will be roughly equal to the available
/// space on this line divided by the number of [`Spacer`]s on it.
///
/// # Example
///
/// Let's say that this is the line being printed:
///
/// ```
/// # duat_core::doc_duat!(duat);
/// # use duat::prelude::*;
/// txt!("This is my line,please,pretend it has tags");
/// ```
///
/// If we were to print it with `{Spacer}` like this:
///
/// ```
/// # duat_core::doc_duat!(duat);
/// # use duat::prelude::*;
/// txt!("This is my line,{Spacer}please,{Spacer}pretend it has tags");
/// ```
///
/// In a region with a width of 50, it would come out like:
///
/// ```text
/// This is my line,    please,    pretend it has tags
/// ```
///
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone, Copy)]
pub struct Spacer;
impl Tag<usize> for Spacer {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &super::InnerTags,
        byte: usize,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        assert!(
            byte <= max,
            "byte out of bounds: the len is {max}, but the byte is {byte}",
        );
        ((byte, RawTag::Spacer(tagger)), None)
    }
}

impl Tag<Point> for Spacer {
    const IS_META: bool = true;

    fn get_raw(
        &mut self,
        tags: &super::InnerTags,
        point: Point,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let byte = point.byte();
        self.get_raw(tags, byte, max, tagger)
    }
}

/// [`Builder`] part and [`Tag`]: Places ghost text.
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
///
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone)]
pub struct Ghost {
    text: Arc<Text>,
    id: Option<GhostId>,
    is_new: bool,
    is_overlay: bool,
}

impl Ghost {
    /// Returns a new `Ghost`, which can be inserted on [`Text`].
    #[track_caller]
    pub fn inlay(value: impl Into<Text>) -> Self {
        let text = value.into();

        assert!(
            text.0.tags.ghosts.is_empty(),
            "Can't place Ghosts inside of Ghosts"
        );

        Self {
            text: Arc::new(text),
            id: None,
            is_new: false,
            is_overlay: false,
        }
    }

    /// Returns a new "inlay" type `Ghost`.
    ///
    /// This `Ghost` type, instead of being printed in the same byte
    /// that it was placed, will instead be printed at the end of the
    /// line where it was placed. Inlay text will also never be
    /// wrapped, if it is too long, it will simply be truncated when
    /// printed.
    ///
    /// You can use this to place text on the right border of lines,
    /// without interrupting the flow of other things.
    ///
    /// Note that earlier positioned inlay `Ghost`s are printed before
    /// latter ones, when those are placed on the same line.
    #[track_caller]
    pub fn overlay(value: impl Into<Text>) -> Self {
        Self { is_overlay: true, ..Self::inlay(value) }
    }

    /// The [`Text`] of this `Ghost`
    pub fn text(&self) -> &Text {
        &self.text
    }
}

impl Tag<usize> for Ghost {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        tags: &super::InnerTags,
        byte: usize,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        assert!(
            byte <= max,
            "index out of bounds: the len is {max}, but the index is {byte}",
        );
        let id = if let Some((id, _)) = tags
            .ghosts
            .iter()
            .find(|(_, arc)| Arc::ptr_eq(arc, &self.text))
        {
            self.is_new = false;
            *id
        } else {
            self.is_new = true;
            GhostId::new()
        };

        self.id = Some(id);
        if self.is_overlay {
            ((byte, RawTag::Overlay(tagger, id)), None)
        } else {
            ((byte, RawTag::Inlay(tagger, id)), None)
        }
    }

    fn on_insertion(self, tags: &mut super::InnerTags) {
        if self.is_new {
            tags.ghosts.push((self.id.unwrap(), self.text.clone()))
        }
    }
}

impl Tag<Point> for Ghost {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        tags: &super::InnerTags,
        point: Point,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let byte = point.byte();
        self.get_raw(tags, byte, max, tagger)
    }

    fn on_insertion(self, tags: &mut super::InnerTags) {
        if self.is_new {
            tags.ghosts.push((self.id.unwrap(), self.text))
        }
    }
}

/// [`Tag`]: Conceals a [range] in the [`Text`].
///
/// This range is completely arbitrary, being able to partially
/// contain lines, as long as it is contained within the length of the
/// [`Text`].
///
/// [range]: TextRange
#[derive(Debug, Clone, Copy)]
pub struct Conceal;
impl<I: TextRange> Tag<I> for Conceal {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &super::InnerTags,
        index: I,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let range = index.to_range(max);
        (
            (range.start, (RawTag::StartConceal)(tagger)),
            Some((range.end, (RawTag::EndConceal)(tagger))),
        )
    }
}

/// [`Tag`]: Adds a toggleable region to the [`Text`].
///
/// This region can be interacted with through the mouse pointer via
/// [`ToggleEvent`]s, which allow for clicks, hovers, drags, things of
/// that nature.
#[derive(Clone)]
pub struct Toggle {
    toggle_fn: ToggleFn,
    id: Option<ToggleId>,
    is_new: bool,
}

impl Toggle {
    /// Returns a new `Toggle` with added feedback.
    ///
    /// This function will take the following arguments:
    ///
    /// - A [`&mut Pass`], used to access duat's global state.
    /// - A [`ToggleEvent`] containing all the information about the
    ///   interaction.
    /// - A [`Range<Point>`], which is the range of the [`Text`] that
    ///   this `Toggle` was placed in.
    ///
    /// With these arguments, you should be able to do anything to
    /// duat from this toggle call.
    ///
    /// # Visual feedback
    ///
    /// This function comes with added feedback that you don't have to
    /// implement yourself:
    ///
    /// - If hovered, applies the `toggle.hovered` [`Form`].
    /// - If clicked, briefly applies the `toggle.clicked` [`Form`].
    ///   - It has three inherited variants `toggle.clicked.left`,
    ///     `toggle.clicked.right` and `toggle.clicked.middle`, which
    ///     are all initially the same.
    ///
    /// If you want a toggleable region with no visual feedback, check
    /// out [`Toggle::new_raw`].
    ///
    /// [`&mut Pass`]: Pass
    /// [`Form`]: crate::form::Form
    pub fn new(
        mut func: impl FnMut(&mut Pass, ToggleEvent, Range<Point>) + Send + 'static,
    ) -> Self {
        Self {
            toggle_fn: Arc::new(Mutex::new(
                move |pa: &mut Pass, event: ToggleEvent<'_>, range: Range<Point>| {
                    let tagger = Tagger::for_toggle();
                    let mut parts = event.handle.text_parts(pa);

                    match event.kind {
                        MouseEventKind::Down(button) => {
                            let form = match button {
                                MouseButton::Left => form::id_of!("toggle.click.left"),
                                MouseButton::Right => form::id_of!("toggle.click.right"),
                                MouseButton::Middle => form::id_of!("toggle.click.middle"),
                            };
                            parts.tags.remove(tagger, ..);
                            parts.tags.insert(tagger, range.clone(), form.to_tag(140))
                        }
                        MouseEventKind::Up(_) | MouseEventKind::Moved => {
                            let form = form::id_of!("toggle.hover");
                            parts.tags.remove(tagger, ..);
                            parts.tags.insert(tagger, range.clone(), form.to_tag(140))
                        }
                        _ => {}
                    }

                    func(pa, event, range)
                },
            )),
            id: None,
            is_new: false,
        }
    }

    /// Returns a new `Toggle` with no visual feedback.
    ///
    /// This function will take the following arguments:
    ///
    /// - A [`&mut Pass`], used to access duat's global state.
    /// - A [`ToggleEvent`] containing all the information about the
    ///   interaction.
    /// - A [`Range<Point>`], which is the range of the [`Text`] that
    ///   this `Toggle` was placed in.
    ///
    /// With these arguments, you should be able to do anything to
    /// duat from this toggle call.
    ///
    /// If you want a toggleable region with visual feedback, check
    /// out [`Toggle::new`].
    ///
    /// [`&mut Pass`]: Pass
    /// [`Form`]: crate::form::Form
    pub fn new_raw(
        func: impl FnMut(&mut Pass, ToggleEvent, Range<Point>) + Send + 'static,
    ) -> Self {
        Self {
            toggle_fn: Arc::new(Mutex::new(func)),
            id: None,
            is_new: false,
        }
    }
}

impl<I: TextRange> Tag<I> for Toggle {
    const IS_META: bool = false;

    fn get_raw(
        &mut self,
        tags: &super::InnerTags,
        index: I,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let range = index.to_range(max);
        let id = if let Some((id, _)) = tags
            .toggles
            .iter()
            .find(|(_, arc)| Arc::ptr_eq(arc, &self.toggle_fn))
        {
            self.is_new = false;
            *id
        } else {
            self.is_new = true;
            ToggleId::new()
        };

        self.id = Some(id);
        (
            (range.start, RawTag::StartToggle(tagger, id)),
            Some((range.end, RawTag::EndToggle(tagger, id))),
        )
    }

    fn on_insertion(self, tags: &mut super::InnerTags) {
        if self.is_new {
            tags.toggles.push((self.id.unwrap(), self.toggle_fn))
        }
    }
}

////////// Layout modification Tags

/// [`Tag`]: Spawns a [`Widget`] in the [`Text`].
///
/// The [`Widget`] will be placed according to the [`DynSpawnSpecs`],
/// and should move automatically as the `SpawnTag` moves around the
/// screen.
pub struct SpawnTag {
    id: SpawnId,
    spawn_fn: Box<dyn FnOnce(&mut Pass, usize, Handle<dyn Widget>) + Send>,
    is_closed: Arc<AtomicBool>,
}

impl SpawnTag {
    /// Returns a new instance of `SpawnTag`.
    ///
    /// You can then place this [`Tag`] inside of the [`Text`] via
    /// [`Text::insert_tag`] or [`Tags::insert`], and the [`Widget`]
    /// should be placed according to the [`DynSpawnSpecs`], and
    /// should move around automatically reflecting where the
    /// `Tag` is at.
    ///
    /// Do note that this [`Widget`] will only be added to Duat and be
    /// able to be printed to the screen once the [`Text`] itself
    /// is printed. And it will be removed once the [`RawTag`] within
    /// gets dropped, either by being removed from the `Text`, or by
    /// the `Text` itself being dropped.
    ///
    /// > [!NOTE]
    /// >
    /// > For now, if you clone a [`Text`] with spawned [`Widget`]s
    /// > within, those `Widget`s will not be cloned to the new
    /// > `Text`, and the [`RawTag::SpawnedWidget`]s within will also
    /// > be removed.
    ///
    /// [`Tags::insert`]: super::Tags::insert
    pub fn new(widget: impl Widget, specs: DynSpawnSpecs) -> Self {
        let id = SpawnId::new();
        let is_closed = Arc::new(AtomicBool::new(false));
        Self {
            id,
            spawn_fn: Box::new({
                let is_closed = is_closed.clone();
                move |pa, win, master| {
                    context::windows().spawn_on_text(
                        pa,
                        (id, specs),
                        widget,
                        win,
                        master,
                        is_closed,
                    );
                }
            }),
            is_closed,
        }
    }
}

impl Tag<Point> for SpawnTag {
    const IS_META: bool = false;

    fn get_raw(
        &mut self,
        _: &super::InnerTags,
        index: Point,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        (
            (
                index.byte().min(max),
                RawTag::SpawnedWidget(tagger, self.id),
            ),
            None,
        )
    }

    fn on_insertion(self, tags: &mut super::InnerTags) {
        tags.spawns.push(super::SpawnCell(self.id, self.is_closed));
        tags.spawn_fns.0.push((self.id, self.spawn_fn));
    }
}

impl Tag<usize> for SpawnTag {
    const IS_META: bool = false;

    fn get_raw(
        &mut self,
        _: &super::InnerTags,
        index: usize,
        max: usize,
        tagger: Tagger,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        (
            (index.min(max), RawTag::SpawnedWidget(tagger, self.id)),
            None,
        )
    }

    fn on_insertion(self, tags: &mut super::InnerTags) {
        tags.spawns.push(super::SpawnCell(self.id, self.is_closed));
        tags.spawn_fns.0.push((self.id, self.spawn_fn));
    }
}

/// An internal representation of [`Tag`]s.
///
/// Unlike [`Tag`]s, however, each variant here is only placed in a
/// single position, and [`Tag`]s that occupy a range are replaced by
/// two [`RawTag`]s, like [`PushForm`] and [`PopForm`], for example.
#[derive(Clone, Copy, Eq)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(Tagger, FormId, u8),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(Tagger, FormId),

    /// A spacer for the current screen line, replaces alignment.
    Spacer(Tagger),

    /// Text that shows up on screen, but is ignored otherwise.
    Overlay(Tagger, GhostId),
    /// Text that shows up on screen, after the end of the line.
    Inlay(Tagger, GhostId),

    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`EndConceal`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`EndConceal`]: RawTag::EndConceal
    StartConceal(Tagger),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    EndConceal(Tagger),
    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    ///
    /// This variant is not actually stored in the buffer, but is
    /// created when iterating.
    ConcealUntil(u32),

    /// Starts a toggleable region of the [`Text`], that can be
    /// interacted with through a mouse pointer.
    StartToggle(Tagger, ToggleId),
    /// Ends a toggleable region of the [`Text`].
    EndToggle(Tagger, ToggleId),

    /// A spawned floating [`Widget`]
    SpawnedWidget(Tagger, SpawnId),
}

impl RawTag {
    /// Inverts a [`RawTag`] that occupies a range.
    pub fn inverse(&self) -> Option<Self> {
        match self {
            Self::PushForm(tagger, id, _) => Some(Self::PopForm(*tagger, *id)),
            Self::PopForm(tagger, id) => Some(Self::PushForm(*tagger, *id, 0)),
            Self::StartConceal(tagger) => Some(Self::EndConceal(*tagger)),
            Self::EndConceal(tagger) => Some(Self::StartConceal(*tagger)),
            Self::StartToggle(tagger, id) => Some(Self::EndToggle(*tagger, *id)),
            Self::EndToggle(tagger, id) => Some(Self::StartToggle(*tagger, *id)),
            _ => None,
        }
    }

    /// Wether this [`RawTag`] ends with another.
    pub fn ends_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_tagger, l_id, _), Self::PopForm(r_tagger, r_id)) => {
                l_id == r_id && l_tagger == r_tagger
            }
            (Self::StartConceal(l_tagger), Self::EndConceal(r_tagger)) => l_tagger == r_tagger,
            (Self::StartToggle(l_tagger, l_id), Self::EndToggle(r_tagger, r_id)) => {
                l_id == r_id && l_tagger == r_tagger
            }
            _ => false,
        }
    }

    /// Wether this [`RawTag`] is the start of a range.
    pub fn is_start(&self) -> bool {
        matches!(
            self,
            Self::PushForm(..) | Self::StartConceal(_) | Self::StartToggle(..)
        )
    }

    /// Wether this [`RawTag`] is the end of a range.
    pub fn is_end(&self) -> bool {
        matches!(
            self,
            Self::PopForm(..) | Self::EndConceal(_) | Self::EndToggle(..)
        )
    }

    /// Wether this is a "meta" tag, that is, wether it alters the
    /// structure of the text itself.
    pub fn is_meta(&self) -> bool {
        matches!(
            self,
            Self::StartConceal(_) | Self::EndConceal(_) | Self::Overlay(..) | Self::Inlay(..)
        )
    }

    /// The [`Tagger`] of this [`RawTag`].
    pub(in crate::text) fn tagger(&self) -> Tagger {
        match self.get_tagger() {
            Some(tagger) => tagger,
            None => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }

    /// Gets the [`Tagger`] of this [`RawTag`], if it is not
    /// [`ConcealUntil`], since that one is never actually stored in
    /// [`InnerTags`].
    ///
    /// [`InnerTags`]: super::InnerTags
    fn get_tagger(&self) -> Option<Tagger> {
        match self {
            Self::PushForm(tagger, ..)
            | Self::PopForm(tagger, _)
            | Self::Spacer(tagger)
            | Self::Overlay(tagger, _)
            | Self::Inlay(tagger, _)
            | Self::StartConceal(tagger)
            | Self::EndConceal(tagger)
            | Self::StartToggle(tagger, _)
            | Self::EndToggle(tagger, _)
            | Self::SpawnedWidget(tagger, _) => Some(*tagger),
            Self::ConcealUntil(_) => None,
        }
    }

    /// The prioriy of this [`RawTag`], only varies with [`Form`]
    /// [`RawTag`]s.
    ///
    /// [`Form`]: crate::form::Form
    pub(super) fn priority(&self) -> u8 {
        match self {
            Self::PushForm(.., priority) => *priority + 5,
            Self::PopForm(..) | Self::Inlay(..) | Self::EndToggle(..) => 1,
            Self::StartConceal(..) | Self::StartToggle(..) | Self::SpawnedWidget(..) => 3,
            Self::Spacer(..) | Self::EndConceal(..) => 0,
            Self::Overlay(..) => 2,
            Self::ConcealUntil(_) => unreachable!("This shouldn't be queried"),
        }
    }
}

impl PartialEq for RawTag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_tagger, l_id, _), Self::PushForm(r_tagger, r_id, _)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::PopForm(l_tagger, l_id), Self::PopForm(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::Inlay(l_tagger, l_id), Self::Inlay(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::Overlay(l_tagger, l_id), Self::Overlay(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::Spacer(l_tagger), Self::Spacer(r_tagger)) => l_tagger == r_tagger,
            (Self::StartConceal(l_tagger), Self::StartConceal(r_tagger)) => l_tagger == r_tagger,
            (Self::EndConceal(l_tagger), Self::EndConceal(r_tagger)) => l_tagger == r_tagger,
            (Self::ConcealUntil(l_tagger), Self::ConcealUntil(r_tagger)) => l_tagger == r_tagger,
            (Self::StartToggle(l_tagger, l_id), Self::StartToggle(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            (Self::EndToggle(l_tagger, l_id), Self::EndToggle(r_tagger, r_id)) => {
                l_tagger == r_tagger && l_id == r_id
            }
            _ => false,
        }
    }
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for RawTag {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(match (self, other) {
            (PushForm(l_tagger, l_id, l_prio), PushForm(r_tagger, r_id, r_prio)) => l_prio
                .cmp(r_prio)
                .then(l_id.cmp(r_id))
                .then(l_tagger.cmp(r_tagger)),
            (PopForm(l_tagger, l_id), PopForm(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            (Inlay(l_tagger, l_id), Inlay(r_tagger, r_id))
            | (Overlay(l_tagger, l_id), Overlay(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            (RawTag::Spacer(l_tagger), RawTag::Spacer(r_tagger))
            | (StartConceal(l_tagger), StartConceal(r_tagger))
            | (EndConceal(l_tagger), EndConceal(r_tagger)) => l_tagger.cmp(r_tagger),
            (ConcealUntil(l_byte), ConcealUntil(r_byte)) => l_byte.cmp(r_byte),
            (StartToggle(l_tagger, l_id), StartToggle(r_tagger, r_id))
            | (EndToggle(l_tagger, l_id), EndToggle(r_tagger, r_id)) => {
                l_id.cmp(r_id).then(l_tagger.cmp(r_tagger))
            }
            _ => self.priority().cmp(&other.priority()),
        })
    }
}

impl Ord for RawTag {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::fmt::Debug for RawTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PushForm(tagger, id, prio) => {
                write!(f, "PushForm({tagger:?}, {}, {prio})", id.name())
            }
            Self::PopForm(tagger, id) => write!(f, "PopForm({tagger:?}, {})", id.name()),
            Self::Spacer(tagger) => write!(f, "Spacer({tagger:?})"),
            Self::Inlay(tagger, id) => write!(f, "Inlay({tagger:?}, {id:?})"),
            Self::Overlay(tagger, id) => write!(f, "Overlay({tagger:?}, {id:?})"),
            Self::StartConceal(tagger) => write!(f, "StartConceal({tagger:?})"),
            Self::EndConceal(tagger) => write!(f, "EndConceal({tagger:?})"),
            Self::ConcealUntil(tagger) => write!(f, "ConcealUntil({tagger:?})"),
            Self::StartToggle(tagger, id) => write!(f, "StartToggle({tagger:?}, {id:?}"),
            Self::EndToggle(tagger, id) => write!(f, "EndToggle({tagger:?}, {id:?}"),
            Self::SpawnedWidget(tagger, id) => write!(f, "SpawnedWidget({tagger:?}, {id:?}"),
        }
    }
}

pub type ToggleFn = Arc<Mutex<dyn FnMut(&mut Pass, ToggleEvent, Range<Point>) + Send>>;
