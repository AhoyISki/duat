//! Remapping functionality for Duat
//!
//! This module's purpose is to support the [`map`] and [`alias`]
//! commands, not only by giving having these two functions, but also
//! in limiting what can be mapped, and making use of [`bindings`] in
//! order to properly document everything.
//!
//! [`bindings`]: super::bindings
use std::{any::TypeId, collections::HashMap, sync::Mutex};

use crossterm::event::KeyEvent;

pub use self::global::*;
use super::Mode;
use crate::{
    context,
    data::{Pass, RwData},
    mode::{self, Binding, Bindings},
    text::{Ghost, Selectionless, Tagger, Text, txt},
    ui::Widget,
};

mod global {
    use std::{
        any::TypeId,
        str::Chars,
        sync::{LazyLock, Mutex},
    };

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

    use super::{Gives, Remapper};
    use crate::{
        data::{DataMap, Pass, RwData},
        mode::{Description, MappedBindings, Mode, remap::InnerRemapper},
        text::{Text, txt},
    };

    static REMAPPER: LazyLock<Remapper> = LazyLock::new(Remapper::new);
    static MODE_TYPE_ID: Mutex<TypeId> = Mutex::new(TypeId::of::<()>());
    static SEND_KEY: LazyLock<RwData<fn(&mut Pass, KeyEvent)>> =
        LazyLock::new(|| RwData::new(|_, _| {}));

    /// A mapping constructor, used by the [`map`] and [`alias`]
    /// commands
    ///
    /// This builder's purpose is pretty much just to let you document
    /// your mappings. This can be useful especially for [`Plugin`]
    /// writers.
    ///
    /// The mapping is done once the `RemapBuilder` is [dropped], so
    /// assigning it to a variable is not recommended
    ///
    /// [`Plugin`]: crate::Plugin
    /// [dropped]: Drop::drop
    pub struct RemapBuilder {
        pub(super) takes: Vec<KeyEvent>,
        pub(super) gives: Gives,
        pub(super) is_alias: bool,
        pub(super) doc: Option<Text>,
        pub(super) remap: fn(&mut Pass, Vec<KeyEvent>, Gives, bool, Option<Text>),
    }

    impl RemapBuilder {
        /// Adds documentation for the mapped sequence
        ///
        /// This documentation will be shown alongside the rest of the
        /// [`Bindings`] for the [`Mode`]. If this function is not
        /// called, then the sequence will be shown by itself.
        ///
        /// [`Bindings`]: crate::mode::Bindings
        pub fn doc<T: Into<Text>>(self, doc: T) {
            let mut builder = self;
            builder.doc = Some(doc.into());
        }
    }

    impl Drop for RemapBuilder {
        fn drop(&mut self) {
            let remap = self.remap;
            let takes = std::mem::take(&mut self.takes);
            let gives = std::mem::replace(&mut self.gives, Gives::Keys(Vec::new()));
            let is_alias = self.is_alias;
            let doc = self.doc.take();
            REMAPPER
                .remaps_builders
                .lock()
                .unwrap()
                .push(Box::new(move |pa| remap(pa, takes, gives, is_alias, doc)));
        }
    }

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
    /// If another sequence already exists on the same mode which
    /// would intersect with this one, the new sequence will not be
    /// added.
    pub fn map<M: Mode>(takes: &str, gives: impl AsGives) -> RemapBuilder {
        let takes = str_to_keys(takes);
        RemapBuilder {
            takes,
            gives: gives.into_gives(),
            is_alias: false,
            doc: None,
            remap: |pa, takes, gives, is_alias, doc| {
                REMAPPER.remap::<M>(pa, takes, gives, is_alias, doc)
            },
        }
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
    pub fn alias<M: Mode>(takes: &str, gives: impl AsGives) -> RemapBuilder {
        let takes = str_to_keys(takes);
        RemapBuilder {
            takes,
            gives: gives.into_gives(),
            is_alias: true,
            doc: None,
            remap: |pa, takes, gives, is_alias, doc| {
                REMAPPER.remap::<M>(pa, takes, gives, is_alias, doc)
            },
        }
    }

    /// The current sequence of [`KeyEvent`]s being mapped
    pub fn current_sequence() -> DataMap<InnerRemapper, (Vec<KeyEvent>, bool)> {
        REMAPPER
            .inner
            .map(|inner| (inner.seq.clone(), inner.seq_is_alias))
    }

    /// The base [`MappedBindings`] for the current [`Mode`]
    ///
    /// This does not take into account the [current_sequence]. For
    /// the [`Description`]s of the bindings and remaps given the
    /// current sequence, you can see [`current_seq_descriptions`]
    ///
    /// The reason why this function takes a [`&mut Pass`] rather than
    /// a `&Pass` is because there might have been [`map`]s and
    /// [`alias`]es that happened asynchronously before this call, so
    /// those need to be added in before returning the [`Iterator`]
    ///
    /// [`&mut Pass`]: Pass
    pub fn current_mode_bindings(pa: &mut Pass) -> &MappedBindings {
        let mut remapper = REMAPPER.remaps_builders.lock().unwrap();
        remapper.drain(..).for_each(|remap| remap(pa));

        &REMAPPER.inner.read(pa).mapped_bindings[&MODE_TYPE_ID.lock().unwrap()]
    }

    /// The [`Description`]s of all [`Binding`]s and remaps of the
    /// current [`Mode`], given the current sequence
    ///
    /// For example, in Vim, you can type `ciw` to `c`hange `i`nside a
    /// `w`ord. If the current sequence of keys was `ci`, then this
    /// function would return a list of bindings and remappings that
    /// could follow. Which in this case would include `w` and many
    /// other keys.
    ///
    /// The reason why this function takes a [`&mut Pass`] rather than
    /// a `&Pass` is because there might have been [`map`]s and
    /// [`alias`]es that happened asynchronously before this call, so
    /// those need to be added in before returning the [`Iterator`]
    ///
    /// [`&mut Pass`]: Pass
    pub fn current_seq_descriptions(pa: &mut Pass) -> impl Iterator<Item = Description<'_>> {
        let mut remapper = REMAPPER.remaps_builders.lock().unwrap();
        remapper.drain(..).for_each(|remap| remap(pa));

        let inner = REMAPPER.inner.read(pa);
        inner.mapped_bindings[&*MODE_TYPE_ID.lock().unwrap()].descriptions_for(&inner.seq)
    }

    /// Turns a sequence of [`KeyEvent`]s into a [`Text`]
    pub fn keys_to_text(keys: &[KeyEvent]) -> Text {
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

        seq.build()
    }

    /// Turns a string of [`KeyEvent`]s into a [`String`]
    pub fn keys_to_string(keys: &[KeyEvent]) -> String {
        use std::fmt::Write;

        use crossterm::event::{KeyCode::*, KeyModifiers as Mod};
        let mut seq = String::new();

        for key in keys {
            if !key.modifiers.is_empty() {
                seq.push('<');
                for modif in key.modifiers.iter() {
                    seq.push_str(match modif {
                        Mod::SHIFT => "S",
                        Mod::CONTROL => "C",
                        Mod::ALT => "A",
                        Mod::SUPER => "Super",
                        Mod::HYPER => "Hyper",
                        Mod::META => "Meta",
                        _ => "",
                    });
                }
                seq.push('-');
            } else if !matches!(key.code, Char(_)) {
                seq.push('<');
            }

            match key.code {
                Backspace => seq.push_str("BS>"),
                Enter => seq.push_str("Enter>"),
                Left => seq.push_str("Left>"),
                Right => seq.push_str("Right>"),
                Up => seq.push_str("Up>"),
                Down => seq.push_str("Down>"),
                Home => seq.push_str("Home>"),
                End => seq.push_str("End>"),
                PageUp => seq.push_str("PageU>"),
                PageDown => seq.push_str("PageD>"),
                Tab => seq.push_str("Tab>"),
                BackTab => seq.push_str("BTab>"),
                Delete => seq.push_str("Del>"),
                Insert => seq.push_str("Ins>"),
                F(num) => write!(seq, "F{num}>").unwrap(),
                Char(char) => {
                    write!(seq, "{char}").unwrap();
                    if !key.modifiers.is_empty() {
                        seq.push('>');
                    }
                }
                Null => seq.push_str("Null>"),
                Esc => seq.push_str("Esc>"),
                CapsLock => seq.push_str("CapsL>"),
                ScrollLock => seq.push_str("ScrollL>"),
                NumLock => seq.push_str("NumL>"),
                PrintScreen => seq.push_str("PrSc>"),
                Pause => seq.push_str("Pause>"),
                Menu => seq.push_str("Menu>"),
                KeypadBegin => seq.push_str("KeypadBeg>"),
                Media(m_code) => write!(seq, "Media{m_code}>").unwrap(),
                Modifier(m_code) => write!(seq, "Mod{m_code}>").unwrap(),
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
    pub trait AsGives: Send + 'static {
        fn into_gives(self) -> Gives;
    }

    impl<M: Mode> AsGives for M {
        fn into_gives(self) -> Gives {
            if let Some(keys) = self.just_keys() {
                Gives::Keys(str_to_keys(keys))
            } else {
                Gives::Mode(Box::new(move || crate::mode::set(self.clone())))
            }
        }
    }

    /// Sends a key to be remapped
    pub(crate) fn send_key_event(pa: &mut Pass, mut key: KeyEvent) {
        // No need to send shift to, for example, Char('L').
        if let KeyCode::Char(_) = key.code {
            key.modifiers.remove(KeyMod::SHIFT);
        }

        if let Some(set_mode) = crate::mode::take_set_mode_fn(pa) {
            set_mode(pa);
        }

        // SAFETY: This function takes a Pass.
        SEND_KEY.read(pa)(pa, key);
    }

    /// Sets the key sending function
    pub(in crate::mode) fn set_mode_for_remapper<M: Mode>(pa: &mut Pass) {
        *SEND_KEY.write(pa) = |pa, key| REMAPPER.send_key::<M>(pa, key);
        *MODE_TYPE_ID.lock().unwrap() = TypeId::of::<M>();
    }
}

/// The structure responsible for remapping sequences of characters
struct Remapper {
    remaps_builders: Mutex<Vec<Box<dyn FnOnce(&mut Pass) + Send + 'static>>>,
    inner: RwData<InnerRemapper>,
}

/// An inner struct for the remapping, you don't need to care about it
#[doc(hidden)]
pub struct InnerRemapper {
    mapped_bindings: HashMap<TypeId, MappedBindings>,
    seq: Vec<KeyEvent>,
    seq_is_alias: bool,
}

impl Remapper {
    /// Returns a new instance of [`Remapper`]
    fn new() -> Self {
        Remapper {
            remaps_builders: Mutex::default(),
            inner: RwData::new(InnerRemapper {
                mapped_bindings: HashMap::new(),
                seq: Vec::new(),
                seq_is_alias: false,
            }),
        }
    }

    /// Maps a sequence of characters to another
    fn remap<M: Mode>(
        &self,
        pa: &mut Pass,
        takes: Vec<KeyEvent>,
        gives: Gives,
        is_alias: bool,
        doc: Option<Text>,
    ) {
        fn remap_inner(
            inner: &mut InnerRemapper,
            type_id: TypeId,
            takes: Vec<KeyEvent>,
            gives: Gives,
            is_alias: bool,
            doc: Option<Text>,
        ) {
            let mapped_bindings = inner.mapped_bindings.get_mut(&type_id).unwrap();

            if let Gives::Keys(keys) = &gives
                && !mapped_bindings.bindings.matches_sequence(keys)
            {
                context::warn!("Tried mapping to unbound sequence");
                return;
            }

            let remap = Remap::new(takes, gives, is_alias, doc.map(Text::no_selections));

            if let Some(i) = mapped_bindings.remaps.iter().position(|r| {
                r.takes.starts_with(&remap.takes) || remap.takes.starts_with(&r.takes)
            }) {
                mapped_bindings.remaps[i] = remap;
            } else {
                mapped_bindings.remaps.push(remap);
            }
        }

        let inner = self.inner.write(pa);
        inner
            .mapped_bindings
            .entry(TypeId::of::<M>())
            .or_insert_with(MappedBindings::for_mode::<M>);

        remap_inner(inner, TypeId::of::<M>(), takes, gives, is_alias, doc);
    }

    /// Sends a key to be remapped or not
    fn send_key<M: Mode>(&self, pa: &mut Pass, key: KeyEvent) {
        fn send_key_inner(
            key_event: KeyEvent,
            remapper: &Remapper,
            pa: &mut Pass,
            ty: TypeId,
            bindings_fn: fn() -> MappedBindings,
        ) {
            let inner = remapper.inner.write(pa);
            let mapped_bindings = inner.mapped_bindings.entry(ty).or_insert_with(bindings_fn);

            inner.seq.push(key_event);
            let (seq, is_alias) = (inner.seq.clone(), inner.seq_is_alias);

            let clear_cur_seq = |pa| {
                let inner = remapper.inner.write(pa);
                inner.seq = Vec::new();
                inner.seq_is_alias = false;
            };

            let keys_to_send = if let Some(i) = mapped_bindings
                .remaps
                .iter()
                .position(|r| r.takes.starts_with(&seq))
            {
                let remap = &mapped_bindings.remaps[i];
                if remap.takes.len() == seq.len() {
                    if remap.is_alias {
                        remove_alias_and(pa, |_, _| {});
                    }

                    clear_cur_seq(pa);

                    let mapped_bindings = &remapper.inner.read(pa).mapped_bindings;

                    match &mapped_bindings[&ty].remaps[i].gives {
                        Gives::Keys(keys) => keys.clone(),
                        Gives::Mode(set_mode) => {
                            set_mode();
                            if let Some(mode_fn) = super::take_set_mode_fn(pa) {
                                mode_fn(pa);
                            }
                            return;
                        }
                    }
                } else {
                    if remap.is_alias {
                        remapper.inner.write(pa).seq_is_alias = true;

                        remove_alias_and(pa, |widget, main| {
                            widget.text_mut().insert_tag(
                                Tagger::for_alias(),
                                main,
                                Ghost::new(txt!("[alias]{}", keys_to_string(&seq))),
                            );
                        });
                    }
                    return;
                }
            } else {
                if is_alias {
                    remove_alias_and(pa, |_, _| {});
                }
                clear_cur_seq(pa);
                seq
            };

            mode::send_keys_to(pa, keys_to_send);
        }

        for remap in self.remaps_builders.lock().unwrap().drain(..) {
            remap(pa)
        }

        send_key_inner(
            key,
            self,
            pa,
            TypeId::of::<M>(),
            MappedBindings::for_mode::<M>,
        );
    }
}

/// A sequence of characters that should be turned into another
/// sequence of characters or a [`Mode`]
struct Remap {
    takes: Vec<KeyEvent>,
    gives: Gives,
    is_alias: bool,
    doc: Option<Selectionless>,
}

impl Remap {
    /// Returns a new `Remap`
    pub fn new(
        takes: Vec<KeyEvent>,
        gives: Gives,
        is_alias: bool,
        doc: Option<Selectionless>,
    ) -> Self {
        Self { takes, gives, is_alias, doc }
    }
}

/// What a [`map`] or [`alias`] gives, can be a sequence of
/// [`KeyEvent`]s or a [`Mode`]
#[doc(hidden)]
pub enum Gives {
    Keys(Vec<KeyEvent>),
    Mode(Box<dyn Fn() + Send>),
}

/// The set of regular [`Mode`] [`Bindings`], as well as all
/// [`Remap`]s
pub struct MappedBindings {
    bindings: Bindings,
    remaps: Vec<Remap>,
}

impl MappedBindings {
    /// Returns the available [`Bindings`] for a [`Mode`]
    fn for_mode<M: Mode>() -> Self {
        Self {
            bindings: M::bindings(),
            remaps: Vec::new(),
        }
    }
}

impl MappedBindings {
    /// Wether the given sequence of [`KeyEvent`]s is bound by these
    /// `MappedBindings`
    ///
    /// This will be true if either the normal [`Mode`] provided
    /// [`Bindings`] match the sequence, or if a remap binds it.
    pub fn matches_sequence(&self, seq: &[KeyEvent]) -> bool {
        self.bindings.matches_sequence(seq)
            || self.remaps.iter().any(|remap| {
                seq.starts_with(&remap.takes) && self.matches_sequence(&seq[remap.takes.len()..])
            })
    }

    /// The [`Description`]s for the bindings available, given the
    /// keys sent so far
    pub fn descriptions_for<'a>(
        &'a self,
        seq: &'a [KeyEvent],
    ) -> impl Iterator<Item = Description<'a>> {
        let bindings = self.bindings.bindings_for(seq);

        bindings
            .into_iter()
            .flat_map(|bindings| bindings.results.iter())
            .map(|(pats, desc, _)| Description {
                text: Some(desc.text()),
                bindings_and_remaps: BindingsAndRemaps {
                    seq,
                    pats_or_remap: PatsOrRemap::Pats(pats, pats.iter(), &self.remaps, Vec::new()),
                },
            })
            .chain(
                self.remaps
                    .iter()
                    .filter(move |remap| {
                        if !remap.takes.starts_with(seq) {
                            return false;
                        }
                        if remap.doc.is_some() {
                            return true;
                        }

                        if let (Gives::Keys(gives), Some(bindings)) = (&remap.gives, bindings)
                            && gives.len() == 1
                            && bindings
                                .results
                                .iter()
                                .any(|(pats, ..)| pats.iter().any(|pat| pat.matches(gives[0])))
                        {
                            false
                        } else {
                            true
                        }
                    })
                    .map(|remap| Description {
                        text: remap.doc.as_ref().map(Selectionless::text),
                        bindings_and_remaps: BindingsAndRemaps {
                            seq,
                            pats_or_remap: PatsOrRemap::Remap(remap),
                        },
                    }),
            )
    }
}

/// The description for [`KeyEvent`]s that are mapped or bound in a
/// [`Mode`]
///
/// This description will include an explaining [`Text`] as well as
/// an [`Iterator`], which returns one of the following:
///
/// - A sequence of `KeyEvent`s that are mapped to the action
/// - A list of [`Binding`]s that are bound to the action
///
/// The first one happens when you call [`map`] or [`alias`], since
/// they let you map a sequence of [`KeyEvent`]s.
///
/// The second one comes from a [`Mode`]s own [`Bindings`] from
/// [`Mode::bindings`]. This is a list of _patterns_ for [`KeyEvent`]s
/// that are bound to actions. This list is immutable, and each item
/// is an alternation of _patterns_ (e.g. `'a'..='z'`, "any media
/// key", concrete [`KeyEvent`]s, etc).
///
/// Do note that [`Description::Pattern`] _may_ end up being empty
/// remaps "take over" all patterns. If you are displaying the
/// `Description`s in some [`Widget`], you should ignore those that
/// have no keys (or not, you decide I guess).
///
/// One other thing to note is that
pub struct Description<'a> {
    /// The [`Text`] describing what the [`KeyEvent`] will do
    pub text: Option<&'a Text>,
    /// The [`Mode`]'s native bindings and all [maps] and [aliases] to
    /// those bindings
    ///
    /// [maps]: map
    /// [aliases]: alias
    pub bindings_and_remaps: BindingsAndRemaps<'a>,
}

/// A [`Mode`]'s bound [`Binding`] or a mapped [`KeyEvent`]
/// sequence
pub enum BindingOrRemap<'a> {
    /// A [`Mode`]'s regular binding, comes from the [`Bindings`]
    /// struct
    Binding(Binding),
    /// A remapped sequence, comes from [`map`] or [`alias`]
    Remap(&'a [KeyEvent]),
}

/// An [`Iterator`] over the possible patterns that match a
/// [`Description`]
///
/// This returns a [`BindingOrRemap`], where
/// [`BindingOrRemap::Binding`] represents a pattern that is naturally
/// bound to a [`Mode`], via [`Mode::bindings`], while a
/// [`BindingOrRemap::Remap`] represents a [`KeyEvent`] sequence that
/// was [mapped] or [aliased] to it.
///
/// [mapped]: map
/// [aliased]: alias
pub struct BindingsAndRemaps<'a> {
    seq: &'a [KeyEvent],
    pats_or_remap: PatsOrRemap<'a>,
}

impl<'a> Iterator for BindingsAndRemaps<'a> {
    type Item = BindingOrRemap<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (pats, pats_iter, remaps, processed_takes) = match &mut self.pats_or_remap {
            PatsOrRemap::Pats(pats, pats_iter, remaps, processed) => {
                (pats, pats_iter, remaps, processed)
            }
            PatsOrRemap::Remap(remap) => {
                return remap
                    .takes
                    .strip_prefix(self.seq)
                    .map(BindingOrRemap::Remap);
            }
        };

        let takess = remaps.iter().filter_map(|r| r.takes.strip_prefix(self.seq));

        pats_iter
            .find_map(|pat| {
                pat.as_key_event()
                    .is_none_or(|key_event| !takess.clone().any(|r| r.starts_with(&[key_event])))
                    .then_some(BindingOrRemap::Binding(*pat))
            })
            .or_else(|| {
                remaps.iter().find_map(|r| {
                    let key_events = r.takes.strip_prefix(self.seq)?;

                    if r.doc.is_none()
                        && let Gives::Keys(gives) = &r.gives
                        && gives.len() == 1
                        && pats.iter().any(|pat| pat.matches(gives[0]))
                        && !processed_takes.contains(&key_events)
                    {
                        processed_takes.push(key_events);
                        Some(BindingOrRemap::Remap(gives))
                    } else {
                        None
                    }
                })
            })
    }
}

/// Two types of description
enum PatsOrRemap<'a> {
    Pats(
        &'a [Binding],
        std::slice::Iter<'a, Binding>,
        &'a [Remap],
        Vec<&'a [KeyEvent]>,
    ),
    Remap(&'a Remap),
}

fn remove_alias_and(pa: &mut Pass, f: impl FnOnce(&mut dyn Widget, usize)) {
    let widget = context::current_widget_node(pa);
    // SAFETY: Given that the Pass is immediately mutably borrowed, it
    // can't be used to act on CurWidget.current.
    widget.mutate_data(pa, |handle| {
        let pa = unsafe { &mut Pass::new() };
        let widget = handle.write(pa);
        if let Some(main) = widget.text().selections().get_main() {
            let byte = main.byte();
            widget.text_mut().remove_tags(Tagger::for_alias(), ..);
            f(&mut *widget, byte)
        }
    })
}
