//! An [`Mode`] that remaps keys and sends them forward
use duat_core::{
    data::RwData,
    input::{Cursors, KeyCode::*, KeyEvent as Event, Mode},
};

use super::Ui;

/// A sequence of characters that should be turned into another
/// sequence of characters.
pub struct Remap<M: Mode<Ui>> {
    /// Takes this sequence of [`Event`]s.
    takes: Vec<Event>,
    /// And turns it into this new sequence of [`Event`]s.
    gives: Vec<Event>,
    /// Wether or not to also send the keys in `self.takes`.
    send_taken: bool,
    /// A condition to ask the `InputScheme`, if it returns `true`,
    /// then we remap.
    condition: std::rc::Rc<dyn Fn(&M) -> bool>,
}

impl<M: Mode<Ui>> Remap<M> {
    pub fn new(
        takes: impl Into<Vec<Event>>,
        gives: impl Into<Vec<Event>>,
        send_taken: bool,
        condition: impl Fn(&M) -> bool + 'static,
    ) -> Self {
        Self {
            takes: takes.into(),
            gives: gives.into(),
            send_taken,
            condition: std::rc::Rc::new(condition),
        }
    }
}

unsafe impl<M: Mode<Ui>> Send for Remap<M> {}
unsafe impl<M: Mode<Ui>> Sync for Remap<M> {}

/// The structure responsible for remapping sequences of characters.
pub struct Remapper<M: Mode<Ui>> {
    /// The [`Mode`] to send the characters to.
    input_method: M,
    /// The list of remapped sequences to be used with the
    /// `EditingScheme`.
    remaps: Vec<Remap<M>>,
    /// The sequence of yet to be fully matched characters that have
    /// been typed.
    current_sequence: Vec<Event>,
    /// A list of sequences that have been at least partially matched
    /// with `current_sequence`.
    to_check: Vec<usize>,
}

impl<M: Mode<Ui>> Remapper<M> {
    pub fn new(input_method: M) -> Self {
        Remapper {
            remaps: Vec::new(),
            current_sequence: Vec::new(),
            input_method,
            to_check: Vec::new(),
        }
    }

    /// Maps a sequence of characters to another.
    pub fn remap(&mut self, takes: impl Into<Vec<Event>>, gives: impl Into<Vec<Event>>) {
        self.remaps.push(Remap::new(takes, gives, false, |_| true))
    }

    pub fn remap_on(
        &mut self,
        check: impl Fn(&M) -> bool + 'static,
        takes: impl Into<Vec<Event>>,
        gives: impl Into<Vec<Event>>,
    ) {
        self.remaps.push(Remap::new(takes, gives, false, check));
    }

    pub fn unmap(&mut self, takes: impl Into<Vec<Event>>) {
        self.unmap_on(takes, |_| true)
    }

    pub fn unmap_on(&mut self, takes: impl Into<Vec<Event>>, check: impl Fn(&M) -> bool + 'static) {
        self.remaps.insert(0, Remap::new(takes, [], false, check))
    }

    pub fn alias(&mut self, takes: impl Into<Vec<Event>>, gives: impl Into<Vec<Event>>) {
        self.alias_on(takes, gives, |_| true)
    }

    pub fn alias_on(
        &mut self,
        takes: impl Into<Vec<Event>>,
        gives: impl Into<Vec<Event>>,
        checker: impl Fn(&M) -> bool + 'static,
    ) {
        let takes: Vec<Event> = takes.into();
        let mut gives: Vec<Event> = gives.into();
        gives.splice(
            0..0,
            std::iter::repeat(Event::from(Backspace)).take(takes.len()),
        );

        self.remaps.push(Remap::new(takes, gives, true, checker))
    }
}

impl<M: Mode<Ui>> Mode<Ui> for Remapper<M> {
    type Widget = M::Widget;

    fn new() -> Self {
        Self::new(M::new())
    }

    fn send_key(
        &mut self,
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &<Ui as duat_core::ui::Ui>::Area,
        mut cursors: Option<Cursors>,
    ) -> Option<Cursors> {
        let remaps = self
            .remaps
            .iter()
            .enumerate()
            .filter(|(i, _)| self.to_check.contains(i));

        let mut new_to_check = Vec::new();
        let mut keys_to_send = Vec::new();

        for (index, remap) in remaps.filter(|(_, r)| (r.condition)(&self.input_method)) {
            // If the sequence isn't long enough, no need to process it.
            if let Some(next_key) = remap.takes.get(self.current_sequence.len()) {
                // Only continue if the sequence, with the newly pressed key, still
                // matches.
                if *next_key == key {
                    // If the `editing_scheme` requires it, send the intermediary keys
                    // individually.
                    if remap.send_taken {
                        keys_to_send.push(key);
                    }

                    // This means that the sequence has been fully completed.
                    if remap.takes.len() == self.current_sequence.len() + 1 {
                        keys_to_send.extend_from_slice(remap.gives.as_slice());
                        self.current_sequence.clear();
                        new_to_check.clear();
                    // This would mean that there are still characters
                    // to be matched.
                    } else {
                        // Keep this sequence in mind while continuing to match more keys.
                        self.current_sequence.push(key);
                        new_to_check.push(index);
                    }
                }
            }
        }

        for key in keys_to_send {
            cursors = self.input_method.send_key(key, widget, area, cursors);
        }

        if new_to_check.is_empty() {
            cursors = self.input_method.send_key(key, widget, area, cursors);
        }

        self.to_check = new_to_check;

        cursors
    }
}

pub trait Remapable: Mode<Ui> + Sized {
    fn remapper(self) -> Remapper<Self>;
}

impl<M: Mode<Ui>> Remapable for M {
    fn remapper(self) -> Remapper<Self> {
        Remapper::new(self)
    }
}
