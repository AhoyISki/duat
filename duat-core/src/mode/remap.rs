use std::{
    any::TypeId,
    sync::{LazyLock, Mutex},
};

use crossterm::event::KeyEvent;

pub use self::global::*;
use super::Mode;
use crate::{
    context,
    data::{Pass, RwData},
    mode,
    text::{Ghost, Tagger, txt},
    ui::{Ui, Widget},
};

mod global {
    use std::{cell::RefCell, str::Chars, sync::LazyLock};

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

    use super::{Gives, Remapper};
    use crate::{
        context,
        data::{DataMap, Pass},
        main_thread_only::MainThreadOnly,
        mode::Mode,
        text::{Builder, Text, txt},
        ui::Ui,
    };

    static REMAPPER: MainThreadOnly<Remapper> = MainThreadOnly::new(Remapper::new());
    static SEND_KEY: LazyLock<MainThreadOnly<RefCell<fn(&mut Pass, KeyEvent)>>> =
        LazyLock::new(|| MainThreadOnly::new(RefCell::new(|_, _| {})));

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
    /// - `super => Super`,
    /// - `hyper => Hyper`,
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    pub fn map<M: Mode<U>, U: Ui>(take: &str, give: impl AsGives<U>) {
        context::assert_is_on_main_thread();
        let remapper = unsafe { REMAPPER.get() };
        remapper.remap::<M, U>(str_to_keys(take), give.into_gives(), false);
    }

    /// Aliases a sequence of keys to another
    ///
    /// The difference between aliasing and mapping is that an alias
    /// will be displayed on the text as a [ghost text], making it
    /// seem like you are typing normally. This text will be printed
    /// with the `Alias` [form].
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    ///
    /// # Note
    ///
    /// This sequence is not like Vim's `alias`, in that if you make a
    /// mistake while typing the sequence, the alias is undone, and
    /// you will be just typing normally.
    ///
    /// The alias command also works on any [`Mode`], not just
    /// "insert like" modes. You can also use any key in the input or
    /// output of this `alias`
    ///
    /// [ghost text]: crate::text::Ghost
    /// [form]: crate::form::Form
    pub fn alias<M: Mode<U>, U: Ui>(take: &str, give: impl AsGives<U>) {
        context::assert_is_on_main_thread();
        let remapper = unsafe { REMAPPER.get() };
        remapper.remap::<M, U>(str_to_keys(take), give.into_gives(), true);
    }

    /// The current sequence of [`KeyEvent`]s being mapped
    pub fn cur_sequence() -> DataMap<(Vec<KeyEvent>, bool), (Vec<KeyEvent>, bool)> {
        context::assert_is_on_main_thread();
        let remapper = unsafe { REMAPPER.get() };
        remapper.cur_seq.map(|seq| seq.clone())
    }

    /// Turns a sequence of [`KeyEvent`]s into a [`Text`]
    pub fn keys_to_text(keys: &[KeyEvent]) -> Builder {
        use crossterm::event::KeyCode::*;
        let mut seq = Text::builder();

        for key in keys {
            match key.code {
                Backspace => seq.push(txt!("[key.special]BS")),
                Enter => seq.push(txt!("[key.special]Enter")),
                Left => seq.push(txt!("[key.special]Left")),
                Right => seq.push(txt!("[key.special]Right")),
                Up => seq.push(txt!("[key.special]Up")),
                Down => seq.push(txt!("[key.special]Down")),
                Home => seq.push(txt!("[key.special]Home")),
                End => seq.push(txt!("[key.special]End")),
                PageUp => seq.push(txt!("[key.special]PageU")),
                PageDown => seq.push(txt!("[key.special]PageD")),
                Tab => seq.push(txt!("[key.special]Tab")),
                BackTab => seq.push(txt!("[key.special]BTab")),
                Delete => seq.push(txt!("[key.special]Del")),
                Insert => seq.push(txt!("[key.special]Ins")),
                F(num) => seq.push(txt!("[key.special]F{num}")),
                Char(char) => seq.push(txt!("[key]{char}")),
                Null => seq.push(txt!("[key.special]Null")),
                Esc => seq.push(txt!("[key.special]Esc")),
                CapsLock => seq.push(txt!("[key.special]CapsL")),
                ScrollLock => seq.push(txt!("[key.special]ScrollL")),
                NumLock => seq.push(txt!("[key.special]NumL")),
                PrintScreen => seq.push(txt!("[key.special]PrSc")),
                Pause => seq.push(txt!("[key.special]Pause")),
                Menu => seq.push(txt!("[key.special]Menu")),
                KeypadBegin => seq.push(txt!("[key.special]KeypadBeg")),
                Media(m_code) => seq.push(txt!("[key.special]Media{m_code}")),
                Modifier(m_code) => seq.push(txt!("[key.special]Mod{m_code}")),
            }
        }

        seq
    }

    /// Turns a string of [`KeyEvent`]s into a [`String`]
    pub fn keys_to_string(keys: &[KeyEvent]) -> String {
        use std::fmt::Write;

        use crossterm::event::KeyCode::*;
        let mut seq = String::new();

        for key in keys {
            match key.code {
                Backspace => seq.push_str("<BS>"),
                Enter => seq.push_str("<Enter>"),
                Left => seq.push_str("<Left>"),
                Right => seq.push_str("<Right>"),
                Up => seq.push_str("<Up>"),
                Down => seq.push_str("<Down>"),
                Home => seq.push_str("<Home>"),
                End => seq.push_str("<End>"),
                PageUp => seq.push_str("<PageU>"),
                PageDown => seq.push_str("<PageD>"),
                Tab => seq.push_str("<Tab>"),
                BackTab => seq.push_str("<BTab>"),
                Delete => seq.push_str("<Del>"),
                Insert => seq.push_str("<Ins>"),
                F(num) => write!(seq, "<F{num}>").unwrap(),
                Char(char) => write!(seq, "{char}").unwrap(),
                Null => seq.push_str("<Null>"),
                Esc => seq.push_str("<Esc>"),
                CapsLock => seq.push_str("<CapsL>"),
                ScrollLock => seq.push_str("<ScrollL>"),
                NumLock => seq.push_str("<NumL>"),
                PrintScreen => seq.push_str("<PrSc>"),
                Pause => seq.push_str("<Pause>"),
                Menu => seq.push_str("<Menu>"),
                KeypadBegin => seq.push_str("<KeypadBeg>"),
                Media(m_code) => write!(seq, "<Media{m_code}>").unwrap(),
                Modifier(m_code) => write!(seq, "<Mod{m_code}>").unwrap(),
            }
        }

        seq
    }

    /// Converts an `&str` to a sequence of [`KeyEvent`]s
    ///
    /// The conversion follows the same rules as remaps in Vim, that
    /// is:
    pub fn str_to_keys(str: &str) -> Vec<KeyEvent> {
        const SPECIAL: &[(&str, KeyCode)] = &[
            ("Enter", KeyCode::Enter),
            ("Tab", KeyCode::Tab),
            ("Bspc", KeyCode::Backspace),
            ("Del", KeyCode::Delete),
            ("Esc", KeyCode::Esc),
            ("Up", KeyCode::Up),
            ("Down", KeyCode::Down),
            ("Left", KeyCode::Left),
            ("Right", KeyCode::Right),
            ("PageU", KeyCode::PageUp),
            ("PageD", KeyCode::PageDown),
            ("Home", KeyCode::Home),
            ("End", KeyCode::End),
            ("Ins", KeyCode::Insert),
            ("F1", KeyCode::F(1)),
            ("F2", KeyCode::F(2)),
            ("F3", KeyCode::F(3)),
            ("F4", KeyCode::F(4)),
            ("F5", KeyCode::F(5)),
            ("F6", KeyCode::F(6)),
            ("F7", KeyCode::F(7)),
            ("F8", KeyCode::F(8)),
            ("F9", KeyCode::F(9)),
            ("F10", KeyCode::F(10)),
            ("F11", KeyCode::F(11)),
            ("F12", KeyCode::F(12)),
        ];
        const MODS: &[(&str, KeyMod)] = &[
            ("C", KeyMod::CONTROL),
            ("A", KeyMod::ALT),
            ("S", KeyMod::SHIFT),
            ("M", KeyMod::META),
            ("super", KeyMod::SUPER),
            ("hyper", KeyMod::HYPER),
        ];
        fn match_key(chars: Chars) -> Option<(KeyEvent, Chars)> {
            let matched_mods = {
                let mut chars = chars.clone();
                let mut mods = KeyMod::empty();
                let mut seq = String::new();

                loop {
                    let char = chars.next()?;
                    if char == '-' {
                        if mods.is_empty() {
                            break None;
                        } else {
                            break Some((mods, chars));
                        }
                    }

                    seq.push(char);

                    if let Some((_, m)) = MODS.iter().find(|(str, _)| str == &seq)
                        && !mods.contains(*m)
                    {
                        mods = mods.union(*m);
                        seq.clear();
                    } else if !MODS[4..6].iter().any(|(str, _)| str.starts_with(&seq)) {
                        break None;
                    }
                }
            };

            let (mut mods, mut chars) = match matched_mods {
                Some((mods, chars)) => (mods, chars),
                None => (KeyMod::empty(), chars),
            };

            let mut code = Some(chars.next().map(KeyCode::Char)?);
            let mut seq = code.unwrap().to_string();

            loop {
                if let Some(c) = code.take() {
                    match chars.next()? {
                        '>' if seq.len() > 1 || !mods.is_empty() => {
                            // Characters are sent as-is, no shifting required.
                            if let KeyCode::Char(_) = c {
                                mods.remove(KeyMod::SHIFT);
                            }
                            break Some((KeyEvent::new(c, mods), chars));
                        }
                        _ if seq.len() > 1 => break None,
                        char => seq.push(char),
                    }
                }

                if let Some((str, c)) = SPECIAL.iter().find(|(str, _)| str.starts_with(&seq)) {
                    if str == &seq {
                        code = Some(*c);
                    } else {
                        seq.push(chars.next()?);
                    }
                } else {
                    break None;
                }
            }
        }

        let mut keys = Vec::new();
        let mut chars = str.chars();
        let mut next = chars.next();

        while let Some(char) = next {
            if char == '<'
                && let Some((key, ahead)) = match_key(chars.clone())
            {
                keys.push(key);
                chars = ahead;
            } else {
                keys.push(KeyEvent::from(KeyCode::Char(char)));
            }

            next = chars.next();
        }

        keys
    }

    /// Trait to distinguish [`Mode`]s from [`KeyEvent`]s
    #[doc(hidden)]
    pub trait AsGives<U> {
        fn into_gives(self) -> Gives;
    }

    impl<M: Mode<U>, U: Ui> AsGives<U> for M {
        fn into_gives(self) -> Gives {
            if let Some(keys) = self.just_keys() {
                Gives::Taggers(str_to_keys(keys))
            } else {
                Gives::Mode(Box::new(move || crate::mode::set(self.clone())))
            }
        }
    }

    /// Sends a key to be remapped
    pub(crate) fn send_key(pa: &mut Pass, mut key: KeyEvent) {
        // No need to send shift to, for example, Char('L').
        if let KeyCode::Char(_) = key.code {
            key.modifiers.remove(KeyMod::SHIFT);
        }

        if let Some(set_mode) = crate::mode::take_set_mode_fn() {
            set_mode(pa);
        }

        // SAFETY: This function takes a Pass.
        let send_key = *unsafe { SEND_KEY.get() }.borrow();
        send_key(pa, key)
    }

    /// Sets the key sending function
    pub(in crate::mode) unsafe fn set_send_key<M: Mode<U>, U: Ui>() {
        *unsafe { SEND_KEY.get() }.borrow_mut() = |pa, key| send_key_fn::<M, U>(pa, key)
    }

    /// The key sending function, to be used as a pointer
    fn send_key_fn<M: Mode<U>, U: Ui>(pa: &mut Pass, key: KeyEvent) {
        // SAFETY: This function takes a Pass.
        unsafe { REMAPPER.get() }.send_key::<M, U>(pa, key);
    }
}

/// The structure responsible for remapping sequences of characters
struct Remapper {
    remaps: Mutex<Vec<(TypeId, Vec<Remap>)>>,
    cur_seq: LazyLock<RwData<(Vec<KeyEvent>, bool)>>,
}

impl Remapper {
    /// Returns a new instance of [`Remapper`]
    const fn new() -> Self {
        Remapper {
            remaps: Mutex::new(Vec::new()),
            cur_seq: LazyLock::new(RwData::default),
        }
    }

    /// Maps a sequence of characters to another
    fn remap<M: Mode<U>, U: Ui>(&self, take: Vec<KeyEvent>, give: Gives, is_alias: bool) {
        fn remap_inner(
            remapper: &Remapper,
            type_id: TypeId,
            take: Vec<KeyEvent>,
            give: Gives,
            is_alias: bool,
        ) {
            let remap = Remap::new(take, give, is_alias);

            let mut remaps = remapper.remaps.lock().unwrap();

            if let Some((_, remaps)) = remaps.iter_mut().find(|(m, _)| type_id == *m) {
                if remaps.iter().all(|r| {
                    !(r.takes.starts_with(&remap.takes) || remap.takes.starts_with(&r.takes))
                }) {
                    remaps.push(remap);
                }
            } else {
                remaps.push((type_id, vec![remap]));
            }
        }

        remap_inner(self, TypeId::of::<M>(), take, give, is_alias);
    }

    /// Sends a key to be remapped or not
    #[allow(clippy::await_holding_lock)]
    fn send_key<M: Mode<U>, U: Ui>(&self, pa: &mut Pass, key: KeyEvent) {
        fn send_key_inner<U: Ui>(remapper: &Remapper, pa: &mut Pass, ty: TypeId, key: KeyEvent) {
            // Lock acquired here
            let remaps_list = remapper.remaps.lock().unwrap();

            let Some(i) = remaps_list.iter().position(|(m, _)| ty == *m) else {
                drop(remaps_list);
                mode::send_keys_to(pa, vec![key]);
                return;
            };

            let (_, remaps) = &remaps_list[i];

            let (cur_seq, is_alias) = remapper.cur_seq.write(pa, |(cur_seq, is_alias)| {
                cur_seq.push(key);
                (cur_seq.clone(), *is_alias)
            });

            let clear_cur_seq = |pa| {
                remapper.cur_seq.write(pa, |(cur_seq, is_alias)| {
                    (*cur_seq, *is_alias) = (Vec::new(), false)
                })
            };

            if let Some(remap) = remaps.iter().find(|r| r.takes.starts_with(&cur_seq)) {
                if remap.takes.len() == cur_seq.len() {
                    if remap.is_alias {
                        remove_alias_and::<U>(pa, |_, _| {});
                    }

                    clear_cur_seq(pa);

                    match &remap.gives {
                        Gives::Taggers(keys) => {
                            let keys = keys.clone();
                            // Lock dropped here, before any .awaits
                            mode::send_keys_to(pa, keys)
                        }
                        Gives::Mode(f) => f(),
                    }
                } else if remap.is_alias {
                    remapper.cur_seq.write(pa, |(_, is_alias)| *is_alias = true);

                    remove_alias_and::<U>(pa, |widget, main| {
                        widget.text_mut().insert_tag(
                            Tagger::for_alias(),
                            main,
                            Ghost(txt!("[alias]{}", keys_to_string(&cur_seq))),
                        );
                    });
                }
            } else if is_alias {
                // Lock dropped here, before any .awaits
                remove_alias_and::<U>(pa, |_, _| {});
                clear_cur_seq(pa);
                mode::send_keys_to(pa, cur_seq);
            } else {
                // Lock dropped here, before any .awaits
                clear_cur_seq(pa);
                mode::send_keys_to(pa, cur_seq);
            }
        }

        send_key_inner::<U>(self, pa, TypeId::of::<M>(), key);
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

///
#[doc(hidden)]
pub enum Gives {
    Taggers(Vec<KeyEvent>),
    Mode(Box<dyn Fn()>),
}

fn remove_alias_and<U: Ui>(pa: &mut Pass, f: impl FnOnce(&mut dyn Widget<U>, usize)) {
    let widget = context::cur_widget::<U>(pa).unwrap();
    // SAFETY: Given that the Pass is immediately mutably borrowed, it
    // can't be used to act on CurWidget.current.
    unsafe {
        widget.mutate_data(|widget, _, _| {
            widget.write(pa, |widget| {
                if let Some(main) = widget.text().selections().unwrap().get_main() {
                    let main = main.byte();
                    widget.text_mut().remove_tags(Tagger::for_alias(), main);
                    f(&mut *widget, main)
                }
            });
        })
    }
}
