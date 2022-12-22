use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use crate::ui::{Ui, RawEndNode};

/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    /// Wrap at the end of the screen.
    Width,
    /// Wrap at a given width.
    Capped(u16),
    /// Wrap at the end of the screen, on word boundaries.
    Word,
    /// Don't wrap at all.
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
    /// No line numbers.
    None,
    #[default]
    /// Line numbers relative to the beginning of the file.
    Absolute,
    /// Line numbers relative to the main cursor's line, including that line.
    Relative,
    /// Relative line numbers on every line, except the main cursor's.
    Hybrid,
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Debug, Clone)]
pub enum TabPlaces {
    /// The same lenght for every tab.
    Regular(usize),
    /// Varying lenghts for different tabs.
    Varied(Vec<usize>),
}

impl Default for TabPlaces {
    fn default() -> Self {
        TabPlaces::Regular(4)
    }
}

impl TabPlaces {
    /// Returns the amount of spaces between a position and the next tab place.
    pub(crate) fn get_tab_len(&self, x: usize, printer: &RawEndNode<impl Ui>) -> usize {
        let space_len = printer.get_char_len(' ');
        match self {
            TabPlaces::Regular(step) => (step - (x % step)) * space_len,
            TabPlaces::Varied(steps) => {
                (steps.iter().find(|&s| *s > x).expect("not enough tabs") - x) * space_len
            }
        }
    }
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Some standard parsec options.
#[derive(Default, Debug, Clone)]
pub struct Config {
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

/// A read-write reference to information, and can tell readers if said information has changed.
pub struct RwState<T>(Arc<RwLock<T>>, Arc<RwLock<usize>>, RwLock<usize>);

impl<T> RwState<T> {
    /// Returns a new instance of `RwState`.
    pub fn new(data: T) -> Self {
        // It's 1 here so that any `RoState`s created from this will have `has_changed()` return
        // `true` at least once, by copying the second value - 1.
        RwState(Arc::new(RwLock::new(data)), Arc::new(RwLock::new(1)), RwLock::new(1))
    }

	/// Reads the information.
	///
	/// Also makes it so that `has_changed()` returns false.
    pub fn read(&self) -> RwLockReadGuard<T> {
        let updated_version = self.1.read().unwrap();
        let mut current_version = self.2.write().unwrap();

        if *updated_version > *current_version {
            *current_version = *updated_version;
        }
        
        self.0.read().unwrap()
    }

	/// Returns a writeable reference to the state.
	///
	/// Also makes it so that `has_changed()` on it or any of its clones returns `true`.
    pub fn write(&mut self) -> RwLockWriteGuard<T> {
        *self.1.write().unwrap() += 1;
        self.0.write().unwrap()
    }

	/// Wether or not it has changed since it was last read.
    pub fn has_changed(&self) -> bool {
        let last_version = self.1.read().unwrap();
        let mut current_version = self.2.write().unwrap();
        let has_changed = *last_version > *current_version;
        *current_version = *last_version;

        has_changed
    }
}

impl<T> Clone for RwState<T> {
	fn clone(&self) -> Self {
    	RwState(self.0.clone(), self.1.clone(), RwLock::new(*self.2.read().unwrap() - 1))
	}
}

/// A read-only reference to information, and can tell readers if said information has changed.
pub struct RoState<T>(Arc<RwLock<T>>, Arc<RwLock<usize>>, RwLock<usize>);

impl<T> RoState<T> {
	/// Returns a new instance of `RwState`.
	pub fn new(data: T) -> Self {
        RoState(Arc::new(RwLock::new(data)), Arc::new(RwLock::new(1)), RwLock::new(1))
    }

	/// Reads the information.
	///
	/// Also makes it so that `has_changed()` returns false.
    pub fn read(&self) -> RwLockReadGuard<T> {
        let updated_version = self.1.read().unwrap();
        let mut current_version = self.2.write().unwrap();

        if *updated_version > *current_version {
            *current_version = *updated_version;
        }
        
        self.0.read().unwrap()
    }

	/// Checks if the state within has changed.
	///
	/// If you have called `has_changed()` or `read()`, without any changes, it will return false.
    pub fn has_changed(&self) -> bool {
        let updated_version = self.1.read().unwrap();
        let mut current_version = self.2.write().unwrap();

        if *updated_version > *current_version {
            *current_version = *updated_version;

            true
        } else {
            false
        }
    }
}

impl<T> From<&RwState<T>> for RoState<T> {
    fn from(state: &RwState<T>) -> Self {
        RoState(state.0.clone(), state.1.clone(), RwLock::new(*state.1.read().unwrap() - 1))
    }
}

// NOTE: Each `RoState` of a given state will have its own internal update counter.
impl<T> Clone for RoState<T> {
	fn clone(&self) -> Self {
    	RoState(self.0.clone(), self.1.clone(), RwLock::new(*self.2.read().unwrap()))
	}
}
