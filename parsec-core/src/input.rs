use std::collections::BTreeSet;

use crossterm::event::{KeyEvent, KeyCode, KeyModifiers};

use crate::{config::RoState, layout::FileWidget, ui::Ui};

/// An object (preferably a widget) that can receive and process input.
pub trait KeyTaker {
    /// Processes a given key.
    fn process_key(&mut self, key: &KeyEvent);

	/// What mode is active at any given moment.
	///
	/// This can be used beyond just vim like modes. For example, a file manager could have a
	/// `files` mode, and any remapping on that mode would only affect that file manager.
    fn current_mode(&self) -> RoState<String>;
}

/// A method of editing a file.
pub trait EditingScheme {
    /// Affects a file, given a certain key input.
    fn process_key_on_file<U>(&mut self, key: &KeyEvent, file: &mut FileWidget<U>)
    where
        U: Ui;

	/// What mode is active at any given moment.
	///
	/// For modeless editing, this should always be `insert`.
    fn current_mode(&self) -> RoState<String>;
}

/// A sequence of characters that should be turned into another sequence of characters.
pub struct Remap {
    /// Takes this sequence of `KeyEvent`s.
    takes: Vec<KeyEvent>,
    /// And turns it into this new sequence of `KeyEvent`s.
    gives: Vec<KeyEvent>,
}

// TODO: Add the ability to send keys to an arbitrary object.
impl Remap {
    /// Sends the transformed keys to an editing scheme to affect a given file.
    fn send_keys_to_file<E, U>(&self, editing_scheme: &mut E, file: &mut FileWidget<U>)
    where
        E: EditingScheme,
        U: Ui,
    {
        for key in &self.gives {
            editing_scheme.process_key_on_file(key, file);
        }
    }

	/// Undoes the effects that the trasnformed keys had on the file.
    fn undo_keys_on_file<E, U>(&self, editing_scheme: &mut E, file: &mut FileWidget<U>)
    where
        E: EditingScheme,
        U: Ui,
    {
        for key in &self.takes {
            if key.modifiers.is_empty() {
                editing_scheme.process_key_on_file(&new_unmodified_key(KeyCode::Backspace), file);
            }
        }
    }
}

/// A list of remaps associated with a given state of an `EditingScheme`.
pub struct Mode {
    /// The list of remappings on the mode.
    remaps: Vec<Remap>,
    /// The name of the mode.
    name: String,
    /// Whether or not the keys should be sent before the final key is pressed.
    ///
    /// Also deals with deleting text that was typed on the sequence.
    ///
    /// # Examples
    ///
    /// * If you remap `jk` to `<Esc>`, pressing `jk` will send `j` first, then `k`, and then send
    /// `<Bs><Bs><Esc>`. This is akin to an alias in (neo)vim.
    /// * If you remap `<A-j>k` to `<Esc>`, pressing `<A-j>k` will send: nothing, then `k`, and then
    /// send `<Bs><Esc>`.   name: String,
    send_on_every_key: bool,
}

/// The structure responsible for remapping sequences of characters.
pub struct KeyRemapper<E>
where
    E: EditingScheme,
{
    /// The list of modes where remapping has taken place.
    modes: Vec<Mode>,
    /// The sequence of yet to be fully matched characters that have been typed.
    current_sequence: Vec<KeyEvent>,
    /// How these characters should modify the file.
    editing_scheme: E,
    /// The mode that is currently active in the `EditingScheme`.
    current_mode: RoState<String>,
    /// A list of sequences that have been at least partially matched with `current_sequence`.
    should_check: BTreeSet<usize>,
}

impl<E> KeyRemapper<E>
where
    E: EditingScheme,
{
    /// Removes all remappings with the given sequence.
    pub fn unmap(&mut self, takes: &Vec<KeyEvent>, mode: &str) {
        for Mode { remaps: mappings, name, .. } in &mut self.modes {
            if mode == *name {
                mappings.retain(|m| &m.takes != takes)
            }
        }
    }

	/// Maps a sequence of characters to another.
    pub fn remap(&mut self, takes: &Vec<KeyEvent>, gives: &Vec<KeyEvent>, mode: &str) {
        self.unmap(takes, mode);
        if let Some(mode) = self.modes.iter_mut().find(|m| m.name == mode) {
            mode.remaps.push(Remap { takes: takes.clone(), gives: gives.clone() });
        }
    }

	/// Send a given key to be processed.
    pub fn send_key_to_file(&mut self, key: KeyEvent, file: &mut FileWidget<impl Ui>) {
        let mode =
            &self.modes.iter().find(|m| m.name == self.current_mode.read().as_str()).unwrap();
        let empty = self.should_check.is_empty();
        let remaps =
            mode.remaps.iter().enumerate().filter(|(i, _)| empty || self.should_check.contains(i));

        let mut should_check_new = BTreeSet::new();

        for (index, remap) in remaps {
            // If the sequence isn't long enough, no need to process it.
            if let Some(next_key) = remap.takes.get(self.current_sequence.len()) {
                // Only continue if the sequence, with the newly pressed key, still matches.
                if *next_key == key {
                    // This would mean that the typed sequence has fully matched.
                    if remap.takes.len() == self.current_sequence.len() + 1 {
                        if mode.send_on_every_key {
                            remap.undo_keys_on_file(&mut self.editing_scheme, file);
                        }

                        remap.send_keys_to_file(&mut self.editing_scheme, file);

                        self.current_sequence.clear();

					// This would mean that there are still characters to be matched.
                    } else {
                        // Keep this sequence in mind while continuing to match more keys.
                        self.current_sequence.push(key);

                        if mode.send_on_every_key {
                            self.editing_scheme.process_key_on_file(&key, file);
                        }

                        should_check_new.insert(index);
                    }
                }
            }
        }

        self.should_check = should_check_new;
    }
}

/// Returns a new `KeyEvent`, with no modifiers.
pub fn new_unmodified_key(key: KeyCode) -> KeyEvent {
    KeyEvent::new(key, KeyModifiers::empty())
}
