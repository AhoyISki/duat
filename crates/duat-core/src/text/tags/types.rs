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

use super::{InnerTags, SpawnId};
use crate::{
    Ns,
    context::{self, Handle},
    data::Pass,
    form::{self, FormId, MaskId},
    mode::ToggleEvent,
    text::{
        Point, Text, TextIndex, TextRange,
        tags::{Ghost, reflist_insert, reflist_pos},
    },
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
///   actual `cursor` or just a temporary `Form`.
/// - [`Spacer`]: Lets you put arbitrary equally sized spaces on a
///   line.
/// - [`Inlay`] represents `Text` that "isn't really there", and is
///   placed in the middle of the rest of the `Text`, taking up space
///   on screen
/// - [`Overlay`]: Places  ghost `Text` _over_ the `Text`. Unlike
///   `Inlay`, this `Text` will essentially "cover" up whatever was
///   underneath it, not _actually_ taking up screen space.
/// - [`Conceal`]: Hides a range in the `Text`, mostly only useful in
///   the [`Buffer`] [`Widget`].
/// - [`Toggle`]: Creates a region that can be interacted with through
///   the mouse pointer.
/// - [`Spawn`]: Spawns a floating `Widget` on a position in `Text`.
///   Said floating widget will move around as the position does the
///   same.
/// - [`Mask`]: Maps all [`Form`]s in a region, given a certain `&str`
///   suffix.
///
/// [`Form`]: crate::form::Form
/// [range]: TextRange
/// [`Buffer`]: crate::buffer::Buffer
/// [`Widget`]: crate::ui::Widget
pub trait Tag<Index>: Sealed<Index> {}

/// Sealed requirements of the [`Tag`] trait.
pub(super) trait Sealed<Index>: Sized {
    /// A meta `Tag` is one that changes the layout of the [`Text`]
    /// itself.
    ///
    /// The only meta `Tag`s are [`Inlay`], [`Conceal`] and
    /// [`Spacer`].
    const IS_META: bool;

    /// Gets the [`RawTag`]s and a possible return id from the `Tag`.
    #[doc(hidden)]
    fn get_raw(
        &mut self,
        tags: &InnerTags,
        index: Index,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>);

    /// An action to take place if the [`RawTag`]s are successfully
    /// added.
    #[doc(hidden)]
    #[allow(unused_variables)]
    fn on_insertion(self, tags: &mut InnerTags) {}
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

impl<I: TextRange> Sealed<I> for FormTag {
    const IS_META: bool = false;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let FormTag { id, priority } = *self;
        let range = index.to_range(max);

        let s_tag = PushForm(ns, id, priority);
        let e_tag = PopForm(ns, id);
        ((range.start, s_tag), Some((range.end, e_tag)))
    }
}
impl<I: TextRange> Tag<I> for FormTag {}

////////// Meta Tags

/// [`Tag`]: A spacer for more advanced alkignment.
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
impl<I: TextIndex> Sealed<I> for Spacer {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let byte = index.to_byte_index();
        assert!(
            byte <= max,
            "byte out of bounds: the len is {max}, but the byte is {byte}",
        );
        ((byte, RawTag::Spacer(ns)), None)
    }
}
impl<I: TextIndex> Tag<I> for Spacer {}

/// [`Builder`] part and [`Tag`]: Places ghost `Text` _within_ real `Text`.
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
///
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone)]
pub struct Inlay {
    text: Arc<Text>,
    idx: Option<usize>,
}

impl Inlay {
    /// Returns a new inlay type `Inlay`, which can be inserted on
    /// [`Text`].
    ///
    /// This ghost `Text` will be placed as if it were regular text,
    /// taking up space on screen, shifting the real text around to
    /// accomodate itself. One of the most prominent uses of it is in
    /// the prompt text of the `PromptLine`.
    ///
    /// If you want ghost `Text` that goes _over_ the real `Text`,
    /// check out [`Overlay`].
    #[track_caller]
    pub fn new(value: impl Into<Text>) -> Self {
        let mut text = value.into();
        text.0
            .tags
            .transform(text.len() - 1..text.len(), text.len() - 1, false);

        assert!(
            text.0
                .tags
                .ghosts
                .iter()
                .all(|ghost| !matches!(ghost, Some((Ghost::Inlay(_), _)))),
            "Can't place Inlays inside of Inlays"
        );

        Self { text: Arc::new(text), idx: None }
    }

    /// The [`Text`] of this `Inlay`
    pub fn text(&self) -> &Text {
        &self.text
    }
}

impl<I: TextIndex> Sealed<I> for Inlay {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        tags: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let byte = index.to_byte_index();
        assert!(
            byte <= max,
            "index out of bounds: the len is {max}, but the index is {byte}",
        );
        let idx = reflist_pos(&tags.ghosts, self);

        self.idx = Some(idx);
        ((byte, RawTag::Inlay(ns, idx as u32)), None)
    }

    fn on_insertion(self, tags: &mut InnerTags) {
        let idx = self.idx.unwrap();
        reflist_insert(&mut tags.ghosts, Ghost::Inlay(self), idx);
    }
}
impl<I: TextIndex> Tag<I> for Inlay {}

impl PartialEq for Inlay {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.text, &other.text)
    }
}

impl Eq for Inlay {}

/// [`Builder`] part and [`Tag`]: Places ghost `Text` _within_ real `Text`.
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
///
/// [`Builder`]: crate::text::Builder
#[derive(Debug, Clone)]
pub struct Overlay {
    text: Arc<Text>,
    idx: Option<usize>,
}

impl Overlay {
    /// Returns a new "overlay" type `Inlay`, which can be inserted on
    /// [`Text`].
    ///
    /// This ghost `Text` will cover up the real text, while still
    /// letting the `Form` tags of the real text affect it. One of the
    /// most prominent uses of it is in the indent guides.
    ///
    /// If you want ghost `Text` that goes _over_ the real `Text`,
    /// check out [`Overlay`].
    #[track_caller]
    pub fn new(value: impl Into<Text>) -> Self {
        let mut text = value.into();
        text.0
            .tags
            .transform(text.len() - 1..text.len(), text.len() - 1, false);

        assert!(
            text.0
                .tags
                .ghosts
                .iter()
                .all(|ghost| !matches!(ghost, Some((Ghost::Overlay(_), _)))),
            "Can't place Overlays inside of Overlays"
        );

        Self { text: Arc::new(text), idx: None }
    }

    /// The [`Text`] of this `Overlay`.
    pub fn text(&self) -> &Text {
        &self.text
    }
}

impl<I: TextIndex> Sealed<I> for Overlay {
    const IS_META: bool = false;

    #[track_caller]
    fn get_raw(
        &mut self,
        tags: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let byte = index.to_byte_index();
        assert!(
            byte <= max,
            "index out of bounds: the len is {max}, but the index is {byte}",
        );
        let idx = reflist_pos(&tags.ghosts, self);

        self.idx = Some(idx);
        ((byte, RawTag::Overlay(ns, idx as u32)), None)
    }

    fn on_insertion(self, tags: &mut InnerTags) {
        let idx = self.idx.unwrap();
        reflist_insert(&mut tags.ghosts, Ghost::Overlay(self), idx);
    }
}
impl<I: TextIndex> Tag<I> for Overlay {}

impl PartialEq for Overlay {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.text, &other.text)
    }
}

impl Eq for Overlay {}

/// [`Tag`]: Conceals a [range] in the [`Text`].
///
/// This range is completely arbitrary, being able to partially
/// contain lines, as long as it is contained within the length of the
/// [`Text`].
///
/// [range]: TextRange
#[derive(Debug, Clone, Copy)]
pub struct Conceal;

impl<I: TextRange> Sealed<I> for Conceal {
    const IS_META: bool = true;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let range = index.to_range(max);
        (
            (range.start, (RawTag::StartConceal)(ns)),
            Some((range.end, (RawTag::EndConceal)(ns))),
        )
    }
}
impl<I: TextRange> Tag<I> for Conceal {}

/// [`Tag`]: Adds a toggleable region to the [`Text`].
///
/// This region can be interacted with through the mouse pointer via
/// [`ToggleEvent`]s, which allow for clicks, hovers, drags, things of
/// that nature.
#[derive(Clone)]
pub struct Toggle {
    pub(super) func: ToggleFn,
    idx: Option<usize>,
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
            func: Arc::new(Mutex::new(
                move |pa: &mut Pass, event: ToggleEvent<'_>, range: Range<Point>| {
                    let ns = Ns::for_toggle();
                    let mut parts = event.handle.text_parts(pa);

                    match event.kind {
                        MouseEventKind::Down(button) => {
                            let form = match button {
                                MouseButton::Left => form::id_of!("toggle.click.left"),
                                MouseButton::Right => form::id_of!("toggle.click.right"),
                                MouseButton::Middle => form::id_of!("toggle.click.middle"),
                            };
                            parts.tags.remove(ns, ..);
                            parts.tags.insert(ns, range.clone(), form.to_tag(140))
                        }
                        MouseEventKind::Up(_) | MouseEventKind::Moved => {
                            let form = form::id_of!("toggle.hover");
                            parts.tags.remove(ns, ..);
                            parts.tags.insert(ns, range.clone(), form.to_tag(140))
                        }
                        _ => {}
                    }

                    func(pa, event, range)
                },
            )),
            idx: None,
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
            func: Arc::new(Mutex::new(func)),
            idx: None,
        }
    }
}

impl<I: TextRange> Sealed<I> for Toggle {
    const IS_META: bool = false;

    fn get_raw(
        &mut self,
        tags: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let range = index.to_range(max);
        let idx = reflist_pos(&tags.toggles, self);

        self.idx = Some(idx);
        (
            (range.start, RawTag::StartToggle(ns, idx as u32)),
            Some((range.end, RawTag::EndToggle(ns, idx as u32))),
        )
    }

    fn on_insertion(self, tags: &mut InnerTags) {
        let idx = self.idx.unwrap();
        reflist_insert(&mut tags.toggles, self, idx);
    }
}
impl<I: TextRange> Tag<I> for Toggle {}

impl PartialEq for Toggle {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.func, &other.func)
    }
}

impl Eq for Toggle {}

impl std::fmt::Debug for Toggle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Toggle").finish_non_exhaustive()
    }
}

////////// Layout modification Tags

/// [`Tag`]: Spawns a [`Widget`] in the [`Text`].
///
/// The [`Widget`] will be placed according to the [`DynSpawnSpecs`],
/// and should move automatically as the `Spawn` moves around the
/// screen.
pub struct Spawn {
    id: SpawnId,
    spawn_fn: Box<dyn FnOnce(&mut Pass, usize, Handle<dyn Widget>) + Send>,
    is_closed: Arc<AtomicBool>,
}

impl Spawn {
    /// Returns a new instance of `Spawn`.
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

impl<I: TextIndex> Sealed<I> for Spawn {
    const IS_META: bool = false;

    fn get_raw(
        &mut self,
        _: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let byte = index.to_byte_index().min(max);
        ((byte, RawTag::SpawnedWidget(ns, self.id)), None)
    }

    fn on_insertion(self, tags: &mut InnerTags) {
        tags.spawns.push(super::SpawnCell(self.id, self.is_closed));
        tags.spawn_fns.0.push((self.id, self.spawn_fn));
    }
}
impl<I: TextIndex> Tag<I> for Spawn {}

/// [`Tag`]: A mask that maps [`Form`]s based on a suffix.
///
/// This works like this: If the form named `form.middle` was applied
/// to a region, and said region had the `mask1` mask applied to it
/// (via this struct), then instead of applying the `form.middle`
/// form, duat would apply `form.middle.mask1`.
///
/// Unless `form.middle.mask1` is [explicitely set], then it is
/// automatically defined as the same thing as `form.middle`.
///
/// [`Form`]: form::Form
/// [explicitely set]: form::set
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Mask(pub &'static str);

impl Mask {
    /// Returns a `Mask` that doesn't map [`Form`]s.
    ///
    /// [`Form`]: form::Form
    pub fn no_mask() -> Self {
        Self("")
    }
}

impl<I: TextRange> Sealed<I> for Mask {
    const IS_META: bool = false;

    #[track_caller]
    fn get_raw(
        &mut self,
        _: &InnerTags,
        index: I,
        max: usize,
        ns: Ns,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>) {
        let range = index.to_range(max);
        let Some(mask_id) = form::mask_id_for(self.0) else {
            panic!("Mask not enabled: {}", self.0);
        };

        let s_tag = PushMask(ns, mask_id);
        let e_tag = PopMask(ns, mask_id);
        ((range.start, s_tag), Some((range.end, e_tag)))
    }
}
impl<I: TextRange> Tag<I> for Mask {}

/// An internal representation of [`Tag`]s.
///
/// Unlike [`Tag`]s, however, each variant here is only placed in a
/// single position, and [`Tag`]s that occupy a range are replaced by
/// two [`RawTag`]s, like [`PushForm`] and [`PopForm`], for example.
#[derive(Clone, Copy, Eq)]
pub(in crate::text) enum RawTag {
    /// Appends a form to the stack.
    PushForm(Ns, FormId, u8),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(Ns, FormId),

    /// A spacer for the current screen line, replaces alignment.
    Spacer(Ns),

    /// Text that shows up on screen, but is ignored otherwise.
    Overlay(Ns, u32),
    /// Text that shows up on screen, after the end of the line.
    Inlay(Ns, u32),

    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`EndConceal`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`EndConceal`]: RawTag::EndConceal
    StartConceal(Ns),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    EndConceal(Ns),
    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    ///
    /// This variant is not actually stored in the buffer, but is
    /// created when iterating.
    ConcealUntil(u32),

    /// Starts a toggleable region of the [`Text`], that can be
    /// interacted with through a mouse pointer.
    StartToggle(Ns, u32),
    /// Ends a toggleable region of the [`Text`].
    EndToggle(Ns, u32),

    /// A spawned floating [`Widget`].
    SpawnedWidget(Ns, SpawnId),

    /// Appends mask to map [`Form`]s given a suffix, to the stack.
    ///
    /// [`Form`]: crate::form::Form
    PushMask(Ns, MaskId),
    /// Removes a mask from the stack. It won't always be the last
    /// one.
    PopMask(Ns, MaskId),
}

impl RawTag {
    /// Wether this [`RawTag`] ends with another.
    pub fn ends_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_ns, l_id, _), Self::PopForm(r_ns, r_id)) => {
                l_id == r_id && l_ns == r_ns
            }
            (Self::StartConceal(l_ns), Self::EndConceal(r_ns)) => l_ns == r_ns,
            (Self::StartToggle(l_ns, l_id), Self::EndToggle(r_ns, r_id)) => {
                l_id == r_id && l_ns == r_ns
            }
            (Self::PushMask(l_ns, l_id), Self::PopMask(r_ns, r_id)) => l_id == r_id && l_ns == r_ns,
            _ => false,
        }
    }

    /// Wether this [`RawTag`] is the start of a range.
    pub fn is_start(&self) -> bool {
        matches!(
            self,
            Self::PushForm(..) | Self::StartConceal(_) | Self::StartToggle(..) | Self::PushMask(..)
        )
    }

    /// Wether this [`RawTag`] is the end of a range.
    pub fn is_end(&self) -> bool {
        matches!(
            self,
            Self::PopForm(..) | Self::EndConceal(_) | Self::EndToggle(..) | Self::PopMask(..)
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

    /// The [`Ns`] of this [`RawTag`].
    pub(in crate::text) fn ns(&self) -> Ns {
        match self.get_ns() {
            Some(ns) => ns,
            None => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }

    /// Gets the [`Ns`] of this [`RawTag`], if it is not
    /// [`ConcealUntil`], since that one is never actually stored in
    /// [`InnerTags`].
    ///
    /// [`InnerTags`]: super::InnerTags
    fn get_ns(&self) -> Option<Ns> {
        match self {
            Self::PushForm(ns, ..)
            | Self::PopForm(ns, _)
            | Self::Spacer(ns)
            | Self::Overlay(ns, _)
            | Self::Inlay(ns, _)
            | Self::StartConceal(ns)
            | Self::EndConceal(ns)
            | Self::StartToggle(ns, _)
            | Self::EndToggle(ns, _)
            | Self::SpawnedWidget(ns, _)
            | Self::PushMask(ns, _)
            | Self::PopMask(ns, _) => Some(*ns),
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
            Self::Spacer(..) => 0,
            Self::EndConceal(..) => 1,
            Self::PopForm(..) => 2,
            Self::Inlay(..) => 3,
            Self::Overlay(..) => 4,
            Self::EndToggle(..) => 5,
            Self::PushMask(..) => 6,
            Self::PopMask(..) => 7,
            Self::StartConceal(..) => 8,
            Self::StartToggle(..) => 9,
            Self::SpawnedWidget(..) => 10,
            Self::ConcealUntil(_) => unreachable!("This shouldn't be queried"),
        }
    }
}

impl PartialEq for RawTag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_ns, l_id, _), Self::PushForm(r_ns, r_id, _)) => {
                l_ns == r_ns && l_id == r_id
            }
            (Self::PopForm(l_ns, l_id), Self::PopForm(r_ns, r_id)) => l_ns == r_ns && l_id == r_id,
            (Self::Inlay(l_ns, l_id), Self::Inlay(r_ns, r_id)) => l_ns == r_ns && l_id == r_id,
            (Self::Overlay(l_ns, l_id), Self::Overlay(r_ns, r_id)) => l_ns == r_ns && l_id == r_id,
            (Self::Spacer(l_ns), Self::Spacer(r_ns)) => l_ns == r_ns,
            (Self::StartConceal(l_ns), Self::StartConceal(r_ns)) => l_ns == r_ns,
            (Self::EndConceal(l_ns), Self::EndConceal(r_ns)) => l_ns == r_ns,
            (Self::ConcealUntil(l_ns), Self::ConcealUntil(r_ns)) => l_ns == r_ns,
            (Self::StartToggle(l_ns, l_id), Self::StartToggle(r_ns, r_id)) => {
                l_ns == r_ns && l_id == r_id
            }
            (Self::EndToggle(l_ns, l_id), Self::EndToggle(r_ns, r_id)) => {
                l_ns == r_ns && l_id == r_id
            }
            (Self::PushMask(l_ns, l_id), Self::PushMask(r_ns, r_id)) => {
                l_ns == r_ns && l_id == r_id
            }
            (Self::PopMask(l_ns, l_id), Self::PopMask(r_ns, r_id)) => l_ns == r_ns && l_id == r_id,
            _ => false,
        }
    }
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for RawTag {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(match (self, other) {
            (PushForm(l_ns, l_id, l_prio), PushForm(r_ns, r_id, r_prio)) => {
                l_prio.cmp(r_prio).then(l_id.cmp(r_id)).then(l_ns.cmp(r_ns))
            }
            (PopForm(l_ns, l_id), PopForm(r_ns, r_id)) => l_id.cmp(r_id).then(l_ns.cmp(r_ns)),
            (Inlay(l_ns, l_id), Inlay(r_ns, r_id)) | (Overlay(l_ns, l_id), Overlay(r_ns, r_id)) => {
                l_id.cmp(r_id).then(l_ns.cmp(r_ns))
            }
            (RawTag::Spacer(l_ns), RawTag::Spacer(r_ns))
            | (StartConceal(l_ns), StartConceal(r_ns))
            | (EndConceal(l_ns), EndConceal(r_ns)) => l_ns.cmp(r_ns),
            (ConcealUntil(l_byte), ConcealUntil(r_byte)) => l_byte.cmp(r_byte),
            (StartToggle(l_ns, l_id), StartToggle(r_ns, r_id))
            | (EndToggle(l_ns, l_id), EndToggle(r_ns, r_id)) => l_id.cmp(r_id).then(l_ns.cmp(r_ns)),
            (PushMask(l_ns, l_id), PushMask(r_ns, r_id)) => l_id.cmp(r_id).then(l_ns.cmp(r_ns)),
            (PopMask(l_ns, l_id), PopMask(r_ns, r_id)) => l_id.cmp(r_id).then(l_ns.cmp(r_ns)),
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
            Self::PushForm(ns, id, prio) => {
                write!(f, "PushForm({ns:?}, {}, {prio})", id.name())
            }
            Self::PopForm(ns, id) => write!(f, "PopForm({ns:?}, {})", id.name()),
            Self::Spacer(ns) => write!(f, "Spacer({ns:?})"),
            Self::Inlay(ns, idx) => write!(f, "Inlay({ns:?}, {idx:?})"),
            Self::Overlay(ns, idx) => write!(f, "Overlay({ns:?}, {idx:?})"),
            Self::StartConceal(ns) => write!(f, "StartConceal({ns:?})"),
            Self::EndConceal(ns) => write!(f, "EndConceal({ns:?})"),
            Self::ConcealUntil(ns) => write!(f, "ConcealUntil({ns:?})"),
            Self::StartToggle(ns, idx) => write!(f, "StartToggle({ns:?}, {idx:?}"),
            Self::EndToggle(ns, idx) => write!(f, "EndToggle({ns:?}, {idx:?}"),
            Self::SpawnedWidget(ns, id) => write!(f, "SpawnedWidget({ns:?}, {id:?}"),
            Self::PushMask(ns, id) => write!(f, "PushMask({ns:?}, {})", id.name()),
            Self::PopMask(ns, id) => write!(f, "PopMask({ns:?}, {})", id.name()),
        }
    }
}

pub type ToggleFn = Arc<Mutex<dyn FnMut(&mut Pass, ToggleEvent, Range<Point>) + Send>>;

/// A part of an inserted [`Tag`].
///
/// Ranged tags like [`FormTag`] and [`Toggle`] are broken up into two
/// tags, a starting one and an ending one.
///
/// In [`Tags::remove_if`], removing either side of the tags will
/// result in both being removed.
///
/// [`Tags::remove_if`]: super::Tags::remove_if
#[derive(Debug, PartialEq, Eq)]
pub enum TagPart<'t> {
    /// Appends a form to the stack.
    PushForm(FormId, u8),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(FormId),

    /// A spacer for the current screen line, replaces alignment.
    Spacer,

    /// Text that shows up on screen, but is ignored otherwise.
    Overlay(&'t Overlay),
    /// Text that shows up on screen, after the end of the line.
    Inlay(&'t Inlay),

    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`EndConceal`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`EndConceal`]: RawTag::EndConceal
    StartConceal,
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    EndConceal,

    /// Starts a toggleable region of the [`Text`], that can be
    /// interacted with through a mouse pointer.
    StartToggle(&'t Toggle),
    /// Ends a toggleable region of the [`Text`].
    EndToggle(&'t Toggle),

    /// A spawned floating [`Widget`].
    SpawnedWidget(SpawnId),

    /// Appends mask to map [`Form`]s given a suffix, to the stack.
    ///
    /// [`Form`]: crate::form::Form
    PushMask(MaskId),
    /// Removes a mask from the stack. It won't always be the last
    /// one.
    PopMask(MaskId),
}

impl<'t> TagPart<'t> {
    /// Returns a [`TagPart`] from a [`RawTag`].
    pub(super) fn from_raw(
        raw_tag: RawTag,
        ghosts: &'t [Option<(Ghost, usize)>],
        toggles: &'t [Option<(Toggle, usize)>],
    ) -> Self {
        match raw_tag {
            PushForm(_, form_id, priority) => Self::PushForm(form_id, priority),
            PopForm(_, form_id) => Self::PopForm(form_id),
            RawTag::Spacer(_) => Self::Spacer,
            RawTag::Overlay(_, idx) => Self::Overlay(match &ghosts[idx as usize] {
                Some((Ghost::Overlay(overlay), _)) => overlay,
                _ => unreachable!(),
            }),
            RawTag::Inlay(_, idx) => Self::Inlay(match &ghosts[idx as usize] {
                Some((Ghost::Inlay(overlay), _)) => overlay,
                _ => unreachable!(),
            }),
            StartConceal(_) => Self::StartConceal,
            EndConceal(_) => Self::EndConceal,
            ConcealUntil(_) => unreachable!(),
            StartToggle(_, idx) => Self::StartToggle(&toggles[idx as usize].as_ref().unwrap().0),
            EndToggle(_, idx) => Self::EndToggle(&toggles[idx as usize].as_ref().unwrap().0),
            SpawnedWidget(_, spawn_id) => Self::SpawnedWidget(spawn_id),
            PushMask(_, mask_id) => Self::PushMask(mask_id),
            PopMask(_, mask_id) => Self::PopMask(mask_id),
        }
    }
}
