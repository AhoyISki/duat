use std::collections::BTreeSet;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::{layout::file_widget::FileWidget, ui::Ui};

/// A widget that can receive and process input.
pub trait KeyTakingWidget {
    /// A mode, for use in remapping.
    ///
    /// Intended for modal editing, but you may use it for other reasons.
    type Mode;

    /// Processes a given key.
    fn process_key(&mut self, key: &KeyEvent);

    /// What mode is active at any given moment.
    ///
    /// This can be used beyond just vim like modes. For example, a file manager could have a
    /// `files` mode, and any remapping on that mode would only affect that file manager.
    fn current_mode(&self) -> Self::Mode;

    /// Wether or not remapped keys should be sent.
    fn send_remapped_keys(&self) -> bool {
        false
    }
}

/// A method of editing a file.
pub trait EditingScheme {
    /// A mode, for use in remapping.
    ///
    /// Intended for modal editing, but you may use it for other reasons.
    type Mode: PartialEq + Clone;

    /// Affects a file, given a certain key input.
    fn process_key<U>(&mut self, key: &KeyEvent, file: &mut FileWidget<U>)
    where
        U: Ui;

    /// What mode is active at any given moment.
    ///
    /// For modeless editing, this should always be `insert`.
    fn cur_mode(&self) -> Self::Mode;

    /// Wether or not remapped keys should be sent.
    fn send_remapped_keys(&self) -> bool;
}

/// A sequence of characters that should be turned into another sequence of characters.
#[derive(PartialEq)]
pub struct FileRemap<E>
where
    E: EditingScheme,
{
    /// Takes this sequence of `KeyEvent`s.
    takes: Vec<KeyEvent>,
    /// And turns it into this new sequence of `KeyEvent`s.
    gives: Vec<KeyEvent>,
    /// The mode where this remapping applies.
    mode: E::Mode,
}

// TODO: Add the ability to send keys to an arbitrary object.
impl<E> FileRemap<E>
where
    E: EditingScheme,
{
    pub fn new(takes: &Vec<KeyEvent>, gives: &Vec<KeyEvent>, mode: &E::Mode) -> Self {
        Self { takes: takes.clone(), gives: gives.clone(), mode: mode.clone() }
    }

    /// Sends the transformed keys to an editing scheme to affect a given file.
    fn send_keys<U>(&self, editing_scheme: &mut E, file: &mut FileWidget<U>)
    where
        U: Ui,
    {
        for key in &self.gives {
            editing_scheme.process_key(key, file);
        }
    }
}

/// The structure responsible for remapping sequences of characters.
pub struct FileRemapper<E>
where
    E: EditingScheme,
{
    /// The list of remapped sequences to be used with the `EditingScheme`.
    remaps: Vec<FileRemap<E>>,
    /// The sequence of yet to be fully matched characters that have been typed.
    current_sequence: Vec<KeyEvent>,
    /// How these characters should modify the file.
    editing_scheme: E,
    /// A list of sequences that have been at least partially matched with `current_sequence`.
    should_check: BTreeSet<usize>,
}

impl<E> FileRemapper<E>
where
    E: EditingScheme,
{
    pub fn new(editing_scheme: E) -> Self {
        FileRemapper {
            remaps: Vec::new(),
            current_sequence: Vec::new(),
            editing_scheme,
            should_check: BTreeSet::new(),
        }
    }

    /// Removes all remappings with the given sequence.
    pub fn unmap(&mut self, takes: &Vec<KeyEvent>, mode: &E::Mode) {
        self.remaps.retain(|r| &r.mode != mode || &r.takes != takes);
    }

    /// Maps a sequence of characters to another.
    pub fn remap(&mut self, takes: &Vec<KeyEvent>, gives: &Vec<KeyEvent>, mode: E::Mode) {
        self.unmap(takes, &mode);
        self.remaps.push(FileRemap::new(takes, gives, &mode));
    }

    /// Send a given key to be processed.
    pub fn send_key_to_file(&mut self, key: KeyEvent, file: &mut FileWidget<impl Ui>) {
        let found_or_empty =
            |i: usize| -> bool { self.should_check.is_empty() || self.should_check.contains(&i) };

        let remaps = self.remaps.iter().enumerate().filter(|(i, _)| found_or_empty(*i));

        let mut should_check_new = BTreeSet::new();
        let mode = self.editing_scheme.cur_mode();

        for (index, remap) in remaps.filter(|(_, r)| r.mode == mode) {
            // If the sequence isn't long enough, no need to process it.
            if let Some(next_key) = remap.takes.get(self.current_sequence.len()) {
                // Only continue if the sequence, with the newly pressed key, still matches.
                if *next_key == key {
                    // Send the remapped keys, like `jk` in `jk -> <Esc>`.
                    if self.editing_scheme.send_remapped_keys() {
                        self.editing_scheme.process_key(&key, file);
                    }

                    // This means that the sequence has been fully completed.
                    if remap.takes.len() == self.current_sequence.len() + 1 {
                        remap.send_keys(&mut self.editing_scheme, file);
                        self.current_sequence.clear();
                        should_check_new.clear();
                    // This would mean that there are still characters to be matched.
                    } else {
                        // Keep this sequence in mind while continuing to match more keys.
                        self.current_sequence.push(key);
                        should_check_new.insert(index);
                    }
                }
            }
        }


        if should_check_new.is_empty() {
            self.editing_scheme.process_key(&key, file);
        }

        self.should_check = should_check_new;
    }
}

/// Returns a new `KeyEvent`, with no modifiers.
pub fn new_unmodified_key(key: KeyCode) -> KeyEvent {
    KeyEvent::new(key, KeyModifiers::empty())
}
