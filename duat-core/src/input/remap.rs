use std::any::TypeId;

use crossterm::event::KeyEvent;
use parking_lot::Mutex;

pub use self::global::*;
use super::Mode;
use crate::{commands, ui::Ui};

mod global {
    use crossterm::event::KeyEvent;
    use parking_lot::Mutex;

    use super::Remapper;
    use crate::{input::Mode, ui::Ui};

    static REMAPPER: Remapper = Remapper::new();
    static SEND_KEY: Mutex<fn(KeyEvent)> = Mutex::new(empty);

    pub fn map<M: Mode<U>, U: Ui>(take: impl Into<Vec<KeyEvent>>, give: impl Into<Vec<KeyEvent>>) {
        REMAPPER.remap::<M, U>(take, give);
    }

    pub fn alias<M: Mode<U>, U: Ui>(
        take: impl Into<Vec<KeyEvent>>,
        give: impl Into<Vec<KeyEvent>>,
    ) {
        REMAPPER.alias::<M, U>(take, give)
    }

    pub fn send_key(key: KeyEvent) {
        let f = { *SEND_KEY.lock() };
        f(key)
    }

    pub(crate) fn set_send_key<M: Mode<U>, U: Ui>() {
        *SEND_KEY.lock() = send_key_fn::<M, U>;
    }

    fn send_key_fn<M: Mode<U>, U: Ui>(key: KeyEvent) {
        REMAPPER.send_key::<M, U>(key);
    }

    fn empty(_: KeyEvent) {}
}

/// A sequence of characters that should be turned into another
/// sequence of characters.
#[derive(Clone)]
struct Remap {
    /// Takes this sequence of [`KeyEvent`]s.
    takes: Vec<KeyEvent>,
    /// And turns it into this new sequence of [`KeyEvent`]s.
    gives: Vec<KeyEvent>,
}

impl Remap {
    pub fn new(takes: impl Into<Vec<KeyEvent>>, gives: impl Into<Vec<KeyEvent>>) -> Self {
        Self { takes: takes.into(), gives: gives.into() }
    }
}

unsafe impl Send for Remap {}
unsafe impl Sync for Remap {}

/// The structure responsible for remapping sequences of characters.
struct Remapper {
    /// The list of remapped sequences to be used with the
    /// `EditingScheme`.
    remaps: Mutex<Vec<(TypeId, Vec<Remap>)>>,
    /// The sequence of yet to be fully matched characters that have
    /// been typed.
    cur_seq: Mutex<Vec<KeyEvent>>,
}

impl Remapper {
    const fn new() -> Self {
        Remapper {
            remaps: Mutex::new(Vec::new()),
            cur_seq: Mutex::new(Vec::new()),
        }
    }

    /// Maps a sequence of characters to another.
    fn remap<M: Mode<U>, U: Ui>(
        &self,
        take: impl Into<Vec<KeyEvent>>,
        give: impl Into<Vec<KeyEvent>>,
    ) {
        let ty = TypeId::of::<M>();
        let remap = Remap::new(take, give);

        let mut remaps = self.remaps.lock();

        if let Some((_, remaps)) = remaps.iter_mut().find(|(m, _)| ty == *m) {
            remaps.push(remap);
        } else {
            remaps.push((ty, vec![remap]));
        }
    }

    fn alias<M: Mode<U>, U: Ui>(
        &self,
        takes: impl Into<Vec<KeyEvent>>,
        gives: impl Into<Vec<KeyEvent>>,
    ) {
        use super::KeyCode::*;

        let takes: Vec<KeyEvent> = takes.into();
        let gives: Vec<KeyEvent> = {
            let mut gives = gives.into();
            gives.splice(
                0..0,
                std::iter::repeat(KeyEvent::from(Backspace)).take(takes.len()),
            );
            gives
        };

        let ty = TypeId::of::<M>();
        let remap = Remap::new(takes, gives);

        let mut remaps = self.remaps.lock();

        if let Some((_, remaps)) = remaps.iter_mut().find(|(m, _)| ty == *m) {
            remaps.push(remap);
        } else {
            remaps.push((ty, vec![remap]));
        }
    }

    fn send_key<M: Mode<U>, U: Ui>(&self, key: KeyEvent) {
        let remaps = self.remaps.lock();
        let Some((_, remaps)) = remaps.iter().find(|(m, _)| TypeId::of::<M>() == *m) else {
            commands::send_key(key);
            return;
        };

        let mut cur_seq = self.cur_seq.lock();
        cur_seq.push(key);

        if let Some(remap) = remaps.iter().find(|r| r.takes.starts_with(&cur_seq)) {
            if remap.takes.len() == cur_seq.len() {
                cur_seq.clear();
                for key in remap.gives.iter() {
                    commands::send_key(*key);
                }
            }
        } else {
            for key in cur_seq.drain(..) {
                commands::send_key(key);
            }
        }
    }
}
