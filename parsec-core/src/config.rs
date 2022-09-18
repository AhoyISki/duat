use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    Width,
    Capped(u16),
    Word,
    #[default]
    NoWrap,
}

// Pretty much only exists because i wanted one of these with usize as its builtin type.
#[derive(Debug, Copy, Clone)]
pub struct ScrollOff {
    pub d_y: usize,
    pub d_x: usize,
}

impl Default for ScrollOff {
    fn default() -> Self {
        ScrollOff { d_y: 5, d_x: 3 }
    }
}

/// How to show the line numbers on screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum LineNumbers {
    None,
    #[default]
    Absolute,
    Relative,
    Hybrid,
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Debug, Clone)]
pub enum TabPlaces {
    Regular(usize),
    Varied(Vec<usize>),
}

impl Default for TabPlaces {
    fn default() -> Self {
        TabPlaces::Regular(4)
    }
}

impl TabPlaces {
    /// Returns the amount of spaces between a position and the next tab place.
    pub fn get_tab_len(&self, x: usize) -> usize {
        match self {
            TabPlaces::Regular(step) => step - (x % step),
            TabPlaces::Varied(steps) => {
                steps.iter().find(|&s| *s > x).expect("not enough tabs") - x
            }
        }
    }
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Some standard parsec options.
#[derive(Default, Debug, Clone)]
pub struct ConfigOptions {
    /// How to wrap the file.
    pub wrap_method: WrapMethod,
    /// The distance between the cursor and the edges of the screen when scrolling.
    pub scrolloff: ScrollOff,
    /// How to show the line numbers.
    pub line_numbers: LineNumbers,
    /// How to indent.
    pub tab_places: TabPlaces,
    /// Wether to indent wrapped lines or not.
    pub wrap_indent: bool,
    /// Wether to convert tabs to spaces.
    pub tabs_as_spaces: bool,
}

pub struct RwState<T>(Arc<RwLock<T>>);

impl<T> RwState<T> {
    pub fn new(data: T) -> Self {
        RwState(Arc::new(RwLock::new(data)))
    }

    pub fn read(&self) -> RwLockReadGuard<T> {
        self.0.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<T> {
        self.0.write().unwrap()
    }

    pub fn to_ro(&self) -> RoState<T> {
        RoState(self.0.clone())
    }
}

impl<T> Clone for RwState<T> {
	fn clone(&self) -> Self {
    	RwState(self.0.clone())
	}
}

pub struct RoState<T>(Arc<RwLock<T>>);

impl<T> RoState<T> {
    pub fn new(rw_state: RwState<T>) -> Self {
        RoState(rw_state.0.clone())
    }

    pub fn read(&self) -> RwLockReadGuard<T> {
        self.0.read().unwrap()
    }
}

impl<T> Clone for RoState<T> {
	fn clone(&self) -> Self {
    	RoState(self.0.clone())
	}
}
