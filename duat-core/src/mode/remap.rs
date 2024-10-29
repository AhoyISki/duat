use std::{any::TypeId, sync::LazyLock};

use crossterm::event::KeyEvent;
use parking_lot::Mutex;

pub use self::global::*;
use super::Mode;
use crate::{
    context,
    data::RwData,
    text::{Key, Tag, Text, text},
    ui::Ui,
};

mod global {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};
    use parking_lot::Mutex;

    use super::{Gives, Remapper};
    use crate::{
        data::RoData,
        mode::Mode,
        text::{Text, text},
        ui::Ui,
    };

    static REMAPPER: Remapper = Remapper::new();
    static SEND_KEY: Mutex<fn(KeyEvent)> = Mutex::new(empty);

    /// Maps a sequence of keys to another
    ///
    /// The keys follow the same rules as Vim, so regular, standalone
    /// characters are mapped verbatim, while "`<{mod}-{key}>`" and
    /// "`<{special}>`" sequences are mapped like in Vim.
    ///
    /// Here are the available special keys:
    ///
    /// - `<Enter> => Enter`,
    /// - `<Tab> => Tab`,
    /// - `<Bspc> => Backspace`,
    /// - `<Del> => Delete`,
    /// - `<Esc> => Esc`,
    /// - `<Up> => Up`,
    /// - `<Down> => Down`,
    /// - `<Left> => Left`,
    /// - `<Right> => Right`,
    /// - `<PageU> => PageUp`,
    /// - `<PageD> => PageDown`,
    /// - `<Home> => Home`,
    /// - `<End> => End`,
    /// - `<Ins> => Insert`,
    /// - `<F{1-12}> => F({1-12})`,
    ///
    /// And the following modifiers are available:
    ///
    /// - `C => Control`,
    /// - `A => Alt`,
    /// - `S => Shift`,
    /// - `M => Meta`,
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    pub fn map<M: Mode<U>, U: Ui>(take: &str, give: impl AsGives<U>) {
        REMAPPER.remap::<M, U>(str_to_keys(take), give.into_gives(), false);
    }

    /// Aliases a sequence of keys to another
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
    /// # Note
    ///
    /// This sequence is not like Vim typing, in that if you make a
    /// mistake while typing the sequence, the alias is undone, and
    /// you will be just typing normally.
    ///
    /// [ghost text]: crate::text::Tag::GhostText
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

    /// Converts an `&str` to a sequence of [`KeyEvent`]s
    ///
    /// The conversion follows the same rules as remaps in Vim, that
    /// is:
    pub fn str_to_keys(str: &str) -> Vec<KeyEvent> {
        const MODS: [(char, KeyMod); 4] = [
            ('C', KeyMod::CONTROL),
            ('A', KeyMod::ALT),
            ('S', KeyMod::SHIFT),
            ('M', KeyMod::META),
        ];

        let mut keys = Vec::new();
        let mut on_special = false;

        for seq in str.split_inclusive(['<', '>']) {
            if !on_special {
                let end = if seq.ends_with('<') {
                    on_special = true;
                    seq.len() - 1
                } else {
                    seq.len()
                };

                keys.extend(seq[..end].chars().map(|c| KeyEvent::from(KeyCode::Char(c))));
            } else if seq.ends_with('>') {
                let trimmed = seq.trim_end_matches('>');
                let mut parts = trimmed.split('-');

                let modifs = if trimmed.contains('-')
                    && let Some(seq) = parts.next()
                {
                    let mut modifs = KeyMod::empty();

                    for (str, modif) in MODS {
                        if seq.contains(str) {
                            modifs.set(modif, true);
                        }
                    }

                    let doubles = ['C', 'A', 'S', 'M']
                        .iter()
                        .any(|c| seq.chars().filter(|char| char == c).count() > 1);

                    let not_modifs = seq.chars().any(|c| !['C', 'A', 'S', 'M'].contains(&c));

                    if modifs.is_empty() || doubles || not_modifs {
                        keys.push(KeyEvent::from(KeyCode::Char('<')));
                        keys.extend(seq.chars().map(|c| KeyEvent::from(KeyCode::Char(c))));
                        on_special = false;
                        continue;
                    }

                    modifs
                } else {
                    KeyMod::empty()
                };

                let code = match parts.next() {
                    Some("Enter") => KeyCode::Enter,
                    Some("Tab") => KeyCode::Tab,
                    Some("Bspc") => KeyCode::Backspace,
                    Some("Del") => KeyCode::Delete,
                    Some("Esc") => KeyCode::Esc,
                    Some("Up") => KeyCode::Up,
                    Some("Down") => KeyCode::Down,
                    Some("Left") => KeyCode::Left,
                    Some("Right") => KeyCode::Right,
                    Some("PageU") => KeyCode::PageUp,
                    Some("PageD") => KeyCode::PageDown,
                    Some("Home") => KeyCode::Home,
                    Some("End") => KeyCode::End,
                    Some("Ins") => KeyCode::Insert,
                    Some("F1") => KeyCode::F(1),
                    Some("F2") => KeyCode::F(2),
                    Some("F3") => KeyCode::F(3),
                    Some("F4") => KeyCode::F(4),
                    Some("F5") => KeyCode::F(5),
                    Some("F6") => KeyCode::F(6),
                    Some("F7") => KeyCode::F(7),
                    Some("F8") => KeyCode::F(8),
                    Some("F9") => KeyCode::F(9),
                    Some("F10") => KeyCode::F(10),
                    Some("F11") => KeyCode::F(11),
                    Some("F12") => KeyCode::F(12),
                    Some(seq)
                        if let Some(char) = seq.chars().next()
                            && (char.is_lowercase()
                                || (char.is_uppercase() && modifs.contains(KeyMod::SHIFT)))
                            && seq.chars().count() == 1 =>
                    {
                        KeyCode::Char(char)
                    }
                    _ => {
                        keys.push(KeyEvent::from(KeyCode::Char('<')));
                        keys.extend(seq.chars().map(|c| KeyEvent::from(KeyCode::Char(c))));
                        on_special = false;
                        continue;
                    }
                };

                on_special = false;
                keys.push(KeyEvent::new(code, modifs));
            }
        }

        keys
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
            Gives::Mode(Box::new(move || crate::mode::set(mode.clone())))
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

/// The structure responsible for remapping sequences of characters
struct Remapper {
    remaps: Mutex<Vec<(TypeId, Vec<Remap>)>>,
    cur_seq: LazyLock<RwData<(Vec<KeyEvent>, bool)>>,
}

impl Remapper {
    const fn new() -> Self {
        Remapper {
            remaps: Mutex::new(Vec::new()),
            cur_seq: LazyLock::new(RwData::default),
        }
    }

    /// Maps a sequence of characters to another
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

    /// Sends a key to be remapped or not
    fn send_key<M: Mode<U>, U: Ui>(&self, key: KeyEvent) {
        let remaps = self.remaps.lock();
        let Some((_, remaps)) = remaps.iter().find(|(m, _)| TypeId::of::<M>() == *m) else {
            super::send_key_to(key);
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
                            super::send_key_to(*key);
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
                super::send_key_to(key);
            }
        }
    }
}

/// A sequence of characters that should be turned into another
/// sequence of characters
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
