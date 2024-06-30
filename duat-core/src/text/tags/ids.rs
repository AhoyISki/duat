use std::{
    ops::Range,
    sync::atomic::{AtomicU16, Ordering},
};

static TEXT_COUNT: AtomicU16 = AtomicU16::new(0);
static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);
static MARKER_COUNT: AtomicU16 = AtomicU16::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextId(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToggleId(u16);

impl TextId {
    pub fn new() -> Self {
        Self(TEXT_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for TextId {
    fn default() -> Self {
        Self::new()
    }
}

impl ToggleId {
    pub fn new() -> Self {
        Self(TOGGLE_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for ToggleId {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Marker(u16);

impl Marker {
    pub fn new() -> Self {
        Self(MARKER_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    pub fn new_many(amount: usize) -> Range<Self> {
        let start = Self(MARKER_COUNT.fetch_add(1, Ordering::Relaxed));
        let end = Self(MARKER_COUNT.fetch_add(amount as u16, Ordering::Relaxed));

        start..end
    }
}

impl Default for Marker {
    fn default() -> Self {
        Self::new()
    }
}

impl std::iter::Step for Marker {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        (end.0 as usize).checked_sub(start.0 as usize)
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(Self(start.0 + count as u16))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        start.0.checked_sub(count as u16).map(Self)
    }
}

pub trait Markers: Clone + PartialEq + Eq {
    fn range(self) -> Range<Marker>;

    fn contains(self, marker: Marker) -> bool {
        let range = self.range();
        marker >= range.start && range.end > marker
    }
}

impl Markers for Marker {
    fn range(self) -> Range<Marker> {
        Marker(self.0)..Marker(self.0 + 1)
    }
}

impl Markers for Range<Marker> {
    fn range(self) -> Range<Marker> {
        self
    }
}
