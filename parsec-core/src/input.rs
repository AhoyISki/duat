use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use crate::{
    data::RwData,
    ui::Ui,
    widgets::{SchemeInputWidget, WidgetActor},
    Controler
};

/// A widget that can receive and process input.
pub trait KeyTakingWidget {
    /// A mode, for use in remapping.
    ///
    /// Intended for modal editing, but you may use it for other
    /// reasons.
    type Mode;

    /// Processes a given key.
    fn process_key(&mut self, key: &KeyEvent);

    /// Wether or not remapped keys should be sent.
    fn send_remapped_keys(&self) -> bool {
        false
    }
}

/// A method of editing a file.
pub trait Scheme: Send + Sync {
    /// Affects a file, given a certain key input.
    fn process_key<U, A>(
        &mut self, key: &KeyEvent, widget_actor: WidgetActor<U, A>, controler: &Controler<U>
    ) where
        U: Ui + 'static,
        A: SchemeInputWidget<U> + ?Sized;

    /// Wether or not remapped keys should be sent.
    fn send_remapped_keys(&self) -> bool;
}

/// A sequence of characters that should be turned into another
/// sequence of characters.
pub struct Remap<E>
where
    E: Scheme
{
    /// Takes this sequence of `KeyEvent`s.
    takes: Vec<KeyEvent>,
    /// And turns it into this new sequence of `KeyEvent`s.
    gives: Vec<KeyEvent>,
    /// A condition to ask the `InputScheme`, if it returns `true`,
    /// then we remap.
    condition: Box<dyn Fn(&E) -> bool>
}

// TODO: Add the ability to send keys to an arbitrary object.
impl<I> Remap<I>
where
    I: Scheme
{
    pub fn new(takes: &[KeyEvent], gives: &[KeyEvent], condition: Box<dyn Fn(&I) -> bool>) -> Self {
        Self {
            takes: takes.to_vec(),
            gives: gives.to_vec(),
            condition
        }
    }
}

/// The structure responsible for remapping sequences of characters.
pub struct KeyRemapper<I>
where
    I: Scheme
{
    /// The list of remapped sequences to be used with the
    /// `EditingScheme`.
    remaps: Vec<Remap<I>>,
    /// The sequence of yet to be fully matched characters that have
    /// been typed.
    current_sequence: Vec<KeyEvent>,
    /// How these characters should modify the file.
    input_scheme: I,
    /// A list of sequences that have been at least partially matched
    /// with `current_sequence`.
    should_check: Vec<usize>
}

impl<I> KeyRemapper<I>
where
    I: Scheme
{
    pub fn new(editing_scheme: I) -> Self {
        KeyRemapper {
            remaps: Vec::new(),
            current_sequence: Vec::new(),
            input_scheme: editing_scheme,
            should_check: Vec::new()
        }
    }

    /// Removes all remappings with the given sequence.
    pub fn unmap(&mut self, takes: &[KeyEvent], condition: impl Fn(&I) -> bool) {
        self.remaps.retain(|r| condition(&self.input_scheme) || r.takes != takes);
    }

    /// Maps a sequence of characters to another.
    pub fn remap(
        &mut self, takes: &[KeyEvent], gives: &[KeyEvent], condition: Box<dyn Fn(&I) -> bool>
    ) {
        self.unmap(takes, &condition);
        self.remaps.push(Remap::new(takes, gives, condition));
    }

    /// Send a given key to be processed.
    pub fn send_key<U, Sw>(
        &mut self, key: KeyEvent, widget: &RwData<Sw>, area: &U::Area, controler: &Controler<U>
    ) where
        U: Ui + 'static,
        Sw: SchemeInputWidget<U> + ?Sized
    {
        let found_or_empty =
            |i: usize| -> bool { self.should_check.is_empty() || self.should_check.contains(&i) };

        let remaps = self.remaps.iter().enumerate().filter(|(i, _)| found_or_empty(*i));

        let mut should_check_new = Vec::new();

        let mut keys_to_send = Vec::new();

        for (index, remap) in remaps.filter(|(_, r)| (r.condition)(&self.input_scheme)) {
            // If the sequence isn't long enough, no need to process it.
            if let Some(next_key) = remap.takes.get(self.current_sequence.len()) {
                // Only continue if the sequence, with the newly pressed key, still
                // matches.
                if *next_key == key {
                    // If the `editing_scheme` requires it, send the intermediary keys
                    // individually.
                    if self.input_scheme.send_remapped_keys() {
                        keys_to_send.push(key);
                    }

                    // This means that the sequence has been fully completed.
                    if remap.takes.len() == self.current_sequence.len() + 1 {
                        keys_to_send.extend_from_slice(remap.gives.as_slice());
                        self.current_sequence.clear();
                        should_check_new.clear();
                    // This would mean that there are still characters
                    // to be matched.
                    } else {
                        // Keep this sequence in mind while continuing to match more keys.
                        self.current_sequence.push(key);
                        should_check_new.push(index);
                    }
                }
            }
        }

        for key in keys_to_send {
            let widget_actor = WidgetActor::new(widget, area);
            self.input_scheme.process_key(&key, widget_actor, controler);
        }

        if should_check_new.is_empty() {
            let widget_actor = WidgetActor::new(widget, area);
            self.input_scheme.process_key(&key, widget_actor, controler);
        }

        self.should_check = should_check_new;
    }
}

/// Returns a new `KeyEvent`, with no modifiers.
pub fn new_unmodified_key(key: KeyCode) -> KeyEvent {
    KeyEvent::new(key, KeyModifiers::empty())
}
