use parsec_core::input::InputMethod;

/// A sequence of characters that should be turned into another
/// sequence of characters.
pub struct Remap<I>
where
    I: InputMethod<super::Ui> + Clone,
{
    /// Takes this sequence of [`KeyEvent`]s.
    takes: Vec<KeyEvent>,
    /// And turns it into this new sequence of [`KeyEvent`]s.
    gives: Vec<KeyEvent>,
    /// Wether or not to also send the keys in `self.takes`.
    send_taken: bool,
    /// A condition to ask the `InputScheme`, if it returns `true`,
    /// then we remap.
    condition: std::rc::Rc<dyn Fn(&I) -> bool>,
}

impl<I> Remap<I>
where
    I: InputMethod + Clone,
{
    pub fn new(
        takes: impl Into<Vec<KeyEvent>>,
        gives: impl Into<Vec<KeyEvent>>,
        send_taken: bool,
        condition: impl Fn(&I) -> bool + 'static,
    ) -> Self {
        Self {
            takes: takes.into(),
            gives: gives.into(),
            send_taken,
            condition: std::rc::Rc::new(condition),
        }
    }
}

impl<I> Clone for Remap<I>
where
    I: InputMethod + Clone,
{
    fn clone(&self) -> Self {
        Self {
            takes: self.takes.clone(),
            gives: self.gives.clone(),
            send_taken: self.send_taken,
            condition: self.condition.clone(),
        }
    }
}

unsafe impl<I: InputMethod + Clone> Send for Remap<I> {}
unsafe impl<I: InputMethod + Clone> Sync for Remap<I> {}

/// The structure responsible for remapping sequences of characters.
#[derive(Clone)]
pub struct Remapper<I>
where
    I: InputMethod + Clone,
{
    /// The [`InputMethod`] to send the characters to.
    input_method: I,
    /// The list of remapped sequences to be used with the
    /// `EditingScheme`.
    remaps: Vec<Remap<I>>,
    /// The sequence of yet to be fully matched characters that have
    /// been typed.
    current_sequence: Vec<KeyEvent>,
    /// A list of sequences that have been at least partially matched
    /// with `current_sequence`.
    to_check: Vec<usize>,
}

impl<I> Remapper<I>
where
    I: InputMethod + Clone,
{
    pub fn new(input_method: I) -> Self {
        Remapper {
            remaps: Vec::new(),
            current_sequence: Vec::new(),
            input_method,
            to_check: Vec::new(),
        }
    }

    /// Maps a sequence of characters to another.
    pub fn remap(&mut self, takes: impl Into<Vec<KeyEvent>>, gives: impl Into<Vec<KeyEvent>>) {
        self.remaps.push(Remap::new(takes, gives, false, |_| true))
    }

    pub fn remap_on(
        &mut self,
        check: impl Fn(&I) -> bool + 'static,
        takes: impl Into<Vec<KeyEvent>>,
        gives: impl Into<Vec<KeyEvent>>,
    ) {
        self.remaps.push(Remap::new(takes, gives, false, check));
    }

    pub fn unmap(&mut self, takes: impl Into<Vec<KeyEvent>>) {
        self.unmap_on(takes, |_| true)
    }

    pub fn unmap_on(
        &mut self,
        takes: impl Into<Vec<KeyEvent>>,
        check: impl Fn(&I) -> bool + 'static,
    ) {
        self.remaps.insert(0, Remap::new(takes, [], false, check))
    }

    pub fn alias(&mut self, takes: impl Into<Vec<KeyEvent>>, gives: impl Into<Vec<KeyEvent>>) {
        self.alias_on(takes, gives, |_| true)
    }

    pub fn alias_on(
        &mut self,
        takes: impl Into<Vec<KeyEvent>>,
        gives: impl Into<Vec<KeyEvent>>,
        checker: impl Fn(&I) -> bool + 'static,
    ) {
        let takes: Vec<KeyEvent> = takes.into();
        let mut gives: Vec<KeyEvent> = gives.into();
        gives.splice(
            0..0,
            std::iter::repeat(KeyEvent::from(KeyCode::Backspace)).take(takes.len()),
        );

        self.remaps.push(Remap::new(takes, gives, true, checker))
    }
}

impl<I> InputMethod for Remapper<I>
where
    I: InputMethod + Clone,
{
    type Widget = I::Widget;

    fn send_key(&mut self, key: KeyEvent, widget: &RwData<Self::Widget>, area: &impl Area) {
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
            self.input_method.send_key(key, widget, area);
        }

        if new_to_check.is_empty() {
            self.input_method.send_key(key, widget, area);
        }

        self.to_check = new_to_check;
    }
}

pub trait Remap: InputMethod<super::Ui> + Sized {
    fn remapper(self) -> Remapper<Self>;
}
