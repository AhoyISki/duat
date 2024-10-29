use std::{any::TypeId, sync::LazyLock};

use crossterm::event::KeyEvent;
use parking_lot::Mutex;

pub use self::global::*;
use super::Mode;
use crate::{
    commands, context,
    data::RwData,
    text::{Key, Tag, Text, text},
    ui::Ui,
};

mod global {
    use crossterm::event::KeyEvent;
    use parking_lot::Mutex;

    use super::{Gives, Remapper};
    use crate::{
        commands,
        data::RoData,
        input::{Mode, str_to_keys},
        text::{Text, text},
        ui::Ui,
    };

    static REMAPPER: Remapper = Remapper::new();
    static SEND_KEY: Mutex<fn(KeyEvent)> = Mutex::new(empty);

    /// Maps a sequence of [keys] to another
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    ///
    /// [keys]: crate::input::keys
    pub fn map<M: Mode<U>, U: Ui>(take: &str, give: impl AsGives<U>) {
        REMAPPER.remap::<M, U>(str_to_keys(take), give.into_gives(), false);
    }

    /// Aliases a sequence of [keys] to another
    ///
    /// The difference between aliasing and mapping is that an alias
    /// will be displayed on the text as a [ghost text], making it
    /// seem like you are pressing the sequence in real time.
    ///
    /// The main use usecase of aliasing is mapping words to other
    /// words, so that it seems like you are typing normally, but the
    /// sequence changes at the end.
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    ///
    /// [keys]: crate::input::keys
    pub fn alias<M: Mode<U>, U: Ui>(take: &str, give: impl AsGives<U>) {
        REMAPPER.remap::<M, U>(str_to_keys(take), give.into_gives(), true);
    }

    pub fn send_key(key: KeyEvent) {
        let f = { *SEND_KEY.lock() };
        f(key)
    }

    pub fn cur_sequence() -> RoData<(Vec<KeyEvent>, bool)> {
        RoData::from(&*REMAPPER.cur_seq)
    }

    /// Turns a sequence of [`KeyEvent`]s into a [`Text`]
    pub fn keys_to_text(keys: &[KeyEvent]) -> Text {
        use crossterm::event::KeyCode::*;
        let mut seq = Text::builder();

        for key in keys {
            match key.code {
                Backspace => text!(seq, [SeqSpecialKey] "BS"),
                Enter => text!(seq, [SeqSpecialKey] "Enter"),
                Left => text!(seq, [SeqSpecialKey] "Left"),
                Right => text!(seq, [SeqSpecialKey] "Right"),
                Up => text!(seq, [SeqSpecialKey] "Up"),
                Down => text!(seq, [SeqSpecialKey] "Down"),
                Home => text!(seq, [SeqSpecialKey] "Home"),
                End => text!(seq, [SeqSpecialKey] "End"),
                PageUp => text!(seq, [SeqSpecialKey] "PageU"),
                PageDown => text!(seq, [SeqSpecialKey] "PageD"),
                Tab => text!(seq, [SeqSpecialKey] "Tab"),
                BackTab => text!(seq, [SeqSpecialKey] "BTab"),
                Delete => text!(seq, [SeqSpecialKey] "Del"),
                Insert => text!(seq, [SeqSpecialKey] "Ins"),
                F(num) => text!(seq, [SeqSpecialKey] "F" num),
                Char(char) => text!(seq, [SeqCharKey] char),
                Null => text!(seq, [SeqSpecialKey] "Null"),
                Esc => text!(seq, [SeqSpecialKey] "Esc"),
                CapsLock => text!(seq, [SeqSpecialKey] "CapsL"),
                ScrollLock => text!(seq, [SeqSpecialKey] "ScrollL"),
                NumLock => text!(seq, [SeqSpecialKey] "NumL"),
                PrintScreen => text!(seq, [SeqSpecialKey] "PrSc"),
                Pause => text!(seq, [SeqSpecialKey] "Pause"),
                Menu => text!(seq, [SeqSpecialKey] "Menu"),
                KeypadBegin => text!(seq, [SeqSpecialKey] "KeypadBeg"),
                Media(m_code) => text!(seq, [SeqSpecialKey] "Media" m_code),
                Modifier(m_code) => text!(seq, [SeqSpecialKey] "Mod" m_code),
            }
        }

        seq.finish()
    }

    /// Turns a string of [`KeyEvent`]s into a [`String`]
    pub fn keys_to_string(keys: &[KeyEvent]) -> String {
        use std::fmt::Write;

        use crossterm::event::KeyCode::*;
        let mut seq = String::new();

        for key in keys {
            match key.code {
                Backspace => seq.push_str("BS"),
                Enter => seq.push_str("Enter"),
                Left => seq.push_str("Left"),
                Right => seq.push_str("Right"),
                Up => seq.push_str("Up"),
                Down => seq.push_str("Down"),
                Home => seq.push_str("Home"),
                End => seq.push_str("End"),
                PageUp => seq.push_str("PageU"),
                PageDown => seq.push_str("PageD"),
                Tab => seq.push_str("Tab"),
                BackTab => seq.push_str("BTab"),
                Delete => seq.push_str("Del"),
                Insert => seq.push_str("Ins"),
                F(num) => write!(seq, "F{num}").unwrap(),
                Char(char) => write!(seq, "{char}").unwrap(),
                Null => seq.push_str("Null"),
                Esc => seq.push_str("Esc"),
                CapsLock => seq.push_str("CapsL"),
                ScrollLock => seq.push_str("ScrollL"),
                NumLock => seq.push_str("NumL"),
                PrintScreen => seq.push_str("PrSc"),
                Pause => seq.push_str("Pause"),
                Menu => seq.push_str("Menu"),
                KeypadBegin => seq.push_str("KeypadBeg"),
                Media(m_code) => write!(seq, "Media{m_code}").unwrap(),
                Modifier(m_code) => write!(seq, "Mod{m_code}").unwrap(),
            }
        }

        seq
    }

    pub trait AsGives<U> {
        fn into_gives(self) -> Gives;
    }

    impl<U: Ui> AsGives<U> for &str {
        fn into_gives(self) -> Gives {
            Gives::Keys(str_to_keys(self))
        }
    }

    impl<M: Mode<U>, U: Ui> AsGives<U> for &M {
        fn into_gives(self) -> Gives {
            let mode = self.clone();
            Gives::Mode(Box::new(move || commands::set_mode(mode.clone())))
        }
    }

    pub(crate) fn set_send_key<M: Mode<U>, U: Ui>() {
        *SEND_KEY.lock() = send_key_fn::<M, U>;
    }

    fn send_key_fn<M: Mode<U>, U: Ui>(key: KeyEvent) {
        REMAPPER.send_key::<M, U>(key);
    }

    fn empty(_: KeyEvent) {}
}

/// The structure responsible for remapping sequences of characters.
struct Remapper {
    /// The list of remapped sequences to be used with the
    /// `EditingScheme`.
    remaps: Mutex<Vec<(TypeId, Vec<Remap>)>>,
    /// The sequence of yet to be fully matched characters that have
    /// been typed.
    cur_seq: LazyLock<RwData<(Vec<KeyEvent>, bool)>>,
}

impl Remapper {
    const fn new() -> Self {
        Remapper {
            remaps: Mutex::new(Vec::new()),
            cur_seq: LazyLock::new(RwData::default),
        }
    }

    /// Maps a sequence of characters to another.
    fn remap<M: Mode<U>, U: Ui>(&self, take: Vec<KeyEvent>, give: Gives, is_alias: bool) {
        let ty = TypeId::of::<M>();
        let remap = Remap::new(take, give, is_alias);

        let mut remaps = self.remaps.lock();

        if let Some((_, remaps)) = remaps.iter_mut().find(|(m, _)| ty == *m) {
            if remaps
                .iter()
                .all(|r| !(r.takes.starts_with(&remap.takes) || remap.takes.starts_with(&r.takes)))
            {
                remaps.push(remap);
            }
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

        let mut cur_seq = self.cur_seq.write();
        let (cur_seq, is_alias) = &mut *cur_seq;
        cur_seq.push(key);

        if let Some(remap) = remaps.iter().find(|r| r.takes.starts_with(cur_seq)) {
            *is_alias = remap.is_alias;
            if remap.takes.len() == cur_seq.len() {
                if *is_alias {
                    remove_alias_and::<U>(|_, _| {});
                }

                *is_alias = false;

                cur_seq.clear();
                match &remap.gives {
                    Gives::Keys(keys) => {
                        for key in keys {
                            commands::send_key(*key);
                        }
                    }
                    Gives::Mode(f) => f(),
                }
            } else if *is_alias {
                remove_alias_and::<U>(|text, main| {
                    text.insert_tag(
                        main,
                        Tag::ghost_text(text!([Alias] { keys_to_string(cur_seq) })),
                        Key::for_alias(),
                    );
                })
            }
        } else {
            if *is_alias {
                remove_alias_and::<U>(|_, _| {});
            }
            *is_alias = false;
            for key in cur_seq.drain(..) {
                commands::send_key(key);
            }
        }
    }
}

/// A sequence of characters that should be turned into another
/// sequence of characters.
struct Remap {
    takes: Vec<KeyEvent>,
    gives: Gives,
    is_alias: bool,
}

impl Remap {
    pub fn new(takes: Vec<KeyEvent>, gives: Gives, is_alias: bool) -> Self {
        Self { takes, gives, is_alias }
    }
}

pub enum Gives {
    Keys(Vec<KeyEvent>),
    Mode(Box<dyn Fn() + Send>),
}

fn remove_alias_and<U: Ui>(f: impl FnOnce(&mut Text, usize)) {
    let file = context::cur_file::<U>().unwrap();
    file.mutate_data(|file, _, cursors| {
        let cursors = cursors.read();
        let mut file = file.write();

        if let Some(main) = cursors.get_main() {
            let main = main.byte();
            file.text_mut().remove_tags_on(main, Key::for_alias());
            f(file.text_mut(), main)
        }
    })
}
