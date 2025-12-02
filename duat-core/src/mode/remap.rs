//! Remapping functionality for Duat
//!
//! This module's purpose is to support the [`map`] and [`alias`]
//! commands, not only by giving having these two functions, but also
//! in limiting what can be mapped, and making use of [`bindings`] in
//! order to properly document everything.
//!
//! [`bindings`]: super::bindings
use std::{any::TypeId, collections::HashMap, slice, sync::LazyLock};

use crossterm::event::KeyEvent;

pub use self::global::*;
use super::Mode;
use crate::{
    context,
    data::{BulkDataWriter, Pass, RwData},
    mode::{self, Binding, Bindings},
    text::{Ghost, Selectionless, Tagger, Text, txt},
    ui::Widget,
    utils::catch_panic,
};

static CUR_SEQ: LazyLock<RwData<(Vec<KeyEvent>, bool)>> = LazyLock::new(RwData::default);

mod global {
    use std::{
        any::TypeId,
        str::Chars,
        sync::{LazyLock, Mutex},
    };

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers as KeyMod};

    use super::{Gives, Remapper};
    use crate::{
        data::{BulkDataWriter, DataMap, Pass, RwData},
        mode::{Description, MappedBindings, Mode},
        text::{Text, txt},
    };

    static REMAPPER: BulkDataWriter<Remapper> = BulkDataWriter::new();
    pub(in crate::mode) static MODE_TYPE_ID: Mutex<TypeId> = Mutex::new(TypeId::of::<()>());
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
        pub(super) remap: fn(&mut Remapper, Vec<KeyEvent>, Gives, bool, Option<Text>),
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
            REMAPPER.mutate(move |remapper| remap(remapper, takes, gives, is_alias, doc));
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
    pub fn map<M: Mode>(takes: &str, gives: impl IntoGives) -> RemapBuilder {
        let takes = str_to_keys(takes);
        RemapBuilder {
            takes,
            gives: gives.into_gives(),
            is_alias: false,
            doc: None,
            remap: |remapper, takes, gives, is_alias, doc| {
                remapper.remap::<M>(takes, gives, is_alias, doc)
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
    pub fn alias<M: Mode>(takes: &str, gives: impl IntoGives) -> RemapBuilder {
        let takes = str_to_keys(takes);
        RemapBuilder {
            takes,
            gives: gives.into_gives(),
            is_alias: true,
            doc: None,
            remap: |remapper, takes, gives, is_alias, doc| {
                remapper.remap::<M>(takes, gives, is_alias, doc)
            },
        }
    }

    /// The current sequence of [`KeyEvent`]s being mapped
    pub fn current_sequence() -> DataMap<(Vec<KeyEvent>, bool), (Vec<KeyEvent>, bool)> {
        super::CUR_SEQ.map(Clone::clone)
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
        &REMAPPER.write(pa).mapped_bindings[&MODE_TYPE_ID.lock().unwrap()]
    }

    /// Changes the description for a given [`KeyEvent`] sequence
    ///
    /// This will also change the description for every other sequence
    /// that is associated with the same description.
    pub fn change_binding_description<M: Mode>(seq: &[KeyEvent], new: Text) {
        let seq = seq.to_vec();
        REMAPPER.mutate(move |remapper| {
            let bindings = remapper
                .mapped_bindings
                .entry(TypeId::of::<M>())
                .or_insert_with(MappedBindings::for_mode::<M>);

            bindings.replace_seq_description(&seq, new);
        });
    }

    /// Replaces the [`Bindings`] for a [`Mode`]
    ///
    /// Do note that if you change the `Bindings`, you will replace
    /// _which_ keys get sent to the `Mode`. You can use this for your
    /// own gain, since you might want to block certain keybindings.
    ///
    /// The main purpose for this function, however, is to create
    /// "dynamic `Bindings`". In Duat, the [`Bindings`] struct kind of
    /// assumes that your mappings are static sequences of keys,
    /// however, sometimes that is not the case. This function serves
    /// the purpose of covering other possibilities.
    ///
    /// [`Bindings`]: super::Bindings
    pub fn change_bindings<M: Mode>(bindings: super::Bindings) {
        use std::collections::hash_map::Entry;

        let bindings = MappedBindings::new(bindings);

        REMAPPER.mutate(
            move |remapper| match remapper.mapped_bindings.entry(TypeId::of::<M>()) {
                Entry::Occupied(mut occupied_entry) => *occupied_entry.get_mut() = bindings,
                Entry::Vacant(vacant_entry) => _ = vacant_entry.insert(bindings),
            },
        );
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
    /// This function also returns a title in the form of
    /// [`Option<Text>`] in case the bindings has a custom title that
    /// better describes what the keys do.
    ///
    /// [`&mut Pass`]: Pass
    /// [`Binding`]: super::Binding
    pub fn current_seq_descriptions(
        pa: &mut Pass,
    ) -> (Option<&Text>, impl Iterator<Item = Description<'_>>) {
        let ((cur_seq, _), remapper) = pa.write_many((&*super::CUR_SEQ, &REMAPPER));
        remapper.mapped_bindings[&*MODE_TYPE_ID.lock().unwrap()].descriptions_for(cur_seq)
    }

    /// Turns a sequence of [`KeyEvent`]s into a [`Text`]
    pub fn keys_to_text(keys: &[KeyEvent]) -> Text {
        use crossterm::event::KeyCode::*;
        let mut builder = Text::builder();

        for key in keys {
            if key.modifiers != KeyMod::NONE
                || !matches!(key.code, KeyCode::Char(char) if char != ' ')
            {
                builder.push(txt!("[key.angle]<"));
            }

            builder.push(modifier_text(key.modifiers));

            match key.code {
                Backspace => builder.push(txt!("[key.special]BS")),
                Enter | Char('\n') => builder.push(txt!("[key.special]Enter")),
                Left => builder.push(txt!("[key.special]Left")),
                Right => builder.push(txt!("[key.special]Right")),
                Up => builder.push(txt!("[key.special]Up")),
                Down => builder.push(txt!("[key.special]Down")),
                Home => builder.push(txt!("[key.special]Home")),
                End => builder.push(txt!("[key.special]End")),
                PageUp => builder.push(txt!("[key.special]PageU")),
                PageDown => builder.push(txt!("[key.special]PageD")),
                Tab => builder.push(txt!("[key.special]Tab")),
                BackTab => builder.push(txt!("[key.special]BTab")),
                Delete => builder.push(txt!("[key.special]Del")),
                Insert => builder.push(txt!("[key.special]Ins")),
                F(num) => builder.push(txt!("[key.special]F{num}")),
                Char(' ') => builder.push(txt!("[key.char]Space")),
                Char(char) => builder.push(txt!("[key.char]{char}")),
                Null => builder.push(txt!("[key.special]Null")),
                Esc => builder.push(txt!("[key.special]Esc")),
                CapsLock => builder.push(txt!("[key.special]CapsL")),
                ScrollLock => builder.push(txt!("[key.special]ScrollL")),
                NumLock => builder.push(txt!("[key.special]NumL")),
                PrintScreen => builder.push(txt!("[key.special]PrSc")),
                Pause => builder.push(txt!("[key.special]Pause")),
                Menu => builder.push(txt!("[key.special]Menu")),
                KeypadBegin => builder.push(txt!("[key.special]KeypadBeg")),
                Media(m_code) => builder.push(txt!("[key.special]Media{m_code}")),
                Modifier(m_code) => builder.push(txt!("[key.special]Mod{m_code}")),
            }

            if key.modifiers != KeyMod::NONE
                || !matches!(key.code, KeyCode::Char(char) if char != ' ')
            {
                builder.push(txt!("[key.angle]>"));
            }
        }

        builder.build()
    }

    /// The [`Text`] for a [`KeyMod`], like `AS-`, is empty if `modif
    /// == KeyMod::NONE`
    pub fn modifier_text(modif: KeyMod) -> Text {
        if modif == KeyMod::NONE {
            return Text::new();
        }

        let mut builder = Text::builder();

        builder.push(crate::form::id_of!("key.mod"));

        for modif in modif.iter() {
            builder.push(match modif {
                KeyMod::ALT => "A",
                KeyMod::CONTROL => "C",
                KeyMod::SHIFT => "S",
                KeyMod::SUPER => "Super",
                KeyMod::HYPER => "Hyper",
                KeyMod::META => "Meta",
                _ => "",
            });
        }

        builder.push(txt!("[key.mod.separator]-"));

        builder.build()
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
                        Mod::ALT => "A",
                        Mod::CONTROL => "C",
                        Mod::SHIFT => "S",
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
    pub trait IntoGives: Send + 'static {
        fn into_gives(self) -> Gives;
    }

    impl<M: Mode> IntoGives for M {
        fn into_gives(self) -> Gives {
            if let Some(keys) = self.just_keys() {
                Gives::Keys(str_to_keys(keys))
            } else {
                Gives::Mode(super::GivenMode::new(self))
            }
        }
    }

    /// Sends a key to be remapped
    pub(crate) fn send_key_event(pa: &mut Pass, mut key: KeyEvent) {
        // No need to send shift to, for example, Char('L').
        if let KeyCode::Char(_) = key.code {
            key.modifiers.remove(KeyMod::SHIFT);
        }

        SEND_KEY.read(pa)(pa, key);

        crate::hook::trigger(pa, crate::hook::KeyTyped(key));
    }

    /// Sets the key sending function
    pub(in crate::mode) fn set_mode_for_remapper<M: Mode>(pa: &mut Pass) {
        REMAPPER
            .write(pa)
            .mapped_bindings
            .entry(TypeId::of::<M>())
            .or_insert_with(MappedBindings::for_mode::<M>);
        *SEND_KEY.write(pa) = |pa, key| super::send_key::<M>(&REMAPPER, pa, key);
        *MODE_TYPE_ID.lock().unwrap() = TypeId::of::<M>();
    }
}

/// The structure responsible for remapping sequences of characters
#[derive(Default)]
struct Remapper {
    mapped_bindings: HashMap<TypeId, MappedBindings>,
    mapped_seq: Vec<KeyEvent>,
}

impl Remapper {
    /// Maps a sequence of characters to another
    fn remap<M: Mode>(
        &mut self,
        takes: Vec<KeyEvent>,
        gives: Gives,
        is_alias: bool,
        doc: Option<Text>,
    ) {
        fn remap_inner(
            inner: &mut Remapper,
            ty: TypeId,
            takes: Vec<KeyEvent>,
            gives: Gives,
            is_alias: bool,
            doc: Option<Text>,
        ) {
            let mapped_bindings = inner.mapped_bindings.get_mut(&ty).unwrap();

            let remap = Remap::new(takes, gives, is_alias, doc.map(Text::no_selections));

            if let Some(i) = mapped_bindings.remaps.iter().position(|r| {
                r.takes.starts_with(&remap.takes) || remap.takes.starts_with(&r.takes)
            }) {
                mapped_bindings.remaps[i] = remap;
            } else {
                mapped_bindings.remaps.push(remap);
            }
        }

        self.mapped_bindings
            .entry(TypeId::of::<M>())
            .or_insert_with(MappedBindings::for_mode::<M>);

        remap_inner(self, TypeId::of::<M>(), takes, gives, is_alias, doc);
    }
}

/// Sends a key to be remapped or not
fn send_key<M: Mode>(bdw: &BulkDataWriter<Remapper>, pa: &mut Pass, key: KeyEvent) {
    fn send_key_inner(
        bdw: &BulkDataWriter<Remapper>,
        key_event: KeyEvent,
        pa: &mut Pass,
        ty: TypeId,
    ) {
        let ((cur_seq, is_alias), remapper) = pa.write_many((&*CUR_SEQ, bdw));
        let mapped_bindings = &remapper.mapped_bindings[&ty];

        cur_seq.push(key_event);
        if !mapped_bindings.sequence_has_followup(cur_seq) {
            cur_seq.clear();
        }
        remapper.mapped_seq.push(key_event);

        let clear_mapped_sequence = |pa: &mut Pass| {
            bdw.write(pa).mapped_seq.clear();
            CUR_SEQ.write(pa).1 = false;
        };

        let (mapped_seq, is_alias) = (remapper.mapped_seq.clone(), *is_alias);

        let keys_to_send = if let Some(i) = mapped_bindings
            .remaps
            .iter()
            .position(|r| r.takes.starts_with(&mapped_seq))
        {
            let remap = &mapped_bindings.remaps[i];
            if remap.takes.len() == mapped_seq.len() {
                if remap.is_alias {
                    remove_alias_and(pa, |_, _| {});
                }

                clear_mapped_sequence(pa);

                let mapped_bindings = &bdw.write(pa).mapped_bindings;

                match &mapped_bindings[&ty].remaps[i].gives {
                    Gives::Keys(keys) => keys.clone(),
                    Gives::Mode(given) => {
                        (given.setter)();
                        if let Some(mode_fn) = super::take_set_mode_fn(pa) {
                            catch_panic(|| mode_fn(pa));
                        }
                        return;
                    }
                }
            } else {
                if remap.is_alias {
                    CUR_SEQ.write(pa).1 = true;

                    remove_alias_and(pa, |widget, main| {
                        widget.text_mut().insert_tag(
                            Tagger::for_alias(),
                            main,
                            Ghost::new(txt!("[alias]{}", keys_to_string(&mapped_seq))),
                        );
                    });
                }
                return;
            }
        } else {
            if is_alias {
                remove_alias_and(pa, |_, _| {});
            }

            clear_mapped_sequence(pa);

            mapped_seq
        };

        mode::send_keys_to(pa, keys_to_send);
    }

    send_key_inner(bdw, key, pa, TypeId::of::<M>());
}

/// A sequence of characters that should be turned into another
/// sequence of characters or a [`Mode`]
#[derive(Debug)]
struct Remap {
    takes: Vec<KeyEvent>,
    gives: Gives,
    is_alias: bool,
    desc: Option<Selectionless>,
}

impl Remap {
    /// Returns a new `Remap`
    pub fn new(
        takes: Vec<KeyEvent>,
        gives: Gives,
        is_alias: bool,
        desc: Option<Selectionless>,
    ) -> Self {
        Self { takes, gives, is_alias, desc }
    }
}

/// What a [`map`] or [`alias`] gives, can be a sequence of
/// [`KeyEvent`]s or a [`Mode`]
#[derive(Debug)]
pub enum Gives {
    /// A sequence of [`KeyEvent`]s that a remap maps to
    Keys(Vec<KeyEvent>),
    /// A [`Mode`] that a remap switches to
    Mode(GivenMode),
}

/// A [`Mode`] that is "given" by a remap
pub struct GivenMode {
    setter: Box<dyn Fn() + Send>,
    name: &'static str,
}

impl GivenMode {
    /// Returns a new `GiveMode`
    fn new<M: Mode>(mode: M) -> Self {
        Self {
            setter: Box::new(move || crate::mode::set(mode.clone())),
            name: crate::utils::duat_name::<M>(),
        }
    }

    /// The name of the [`Mode`]
    pub fn name(&self) -> &'static str {
        self.name
    }
}

impl std::fmt::Debug for GivenMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GivenMode")
            .field("name", &self.name)
            .finish()
    }
}

/// The set of regular [`Mode`] [`Bindings`], as well as all
/// [`Remap`]s
#[derive(Debug)]
pub struct MappedBindings {
    bindings: Bindings,
    remaps: Vec<Remap>,
}

impl MappedBindings {
    /// Returns `Self` for the available [`Bindings`] for a [`Mode`]
    fn for_mode<M: Mode>() -> Self {
        Self {
            bindings: M::bindings(),
            remaps: Vec::new(),
        }
    }

    /// Returns `Self` from custom [`Bindings`]
    fn new(bindings: Bindings) -> Self {
        Self { bindings, remaps: Vec::new() }
    }
}

impl MappedBindings {
    /// Wether these `MappedBindings` accepts the sequence of
    /// [`KeyEvent`]s
    ///
    /// This will be true if either the normal [`Mode`] provided
    /// [`Bindings`] match the sequence, or if a remap binds it.
    pub fn matches_sequence(&self, seq: &[KeyEvent]) -> bool {
        self.remaps.iter().any(|remap| {
            seq.starts_with(&remap.takes) && self.matches_sequence(&seq[remap.takes.len()..])
        }) || self.bindings.matches_sequence(seq)
    }

    /// Wether the given sequence of [`KeyEvent`]s has a followup
    /// in these `MappedBindings`
    ///
    /// This will be true if either the normal [`Mode`] provided
    /// [`Bindings`] match the sequence, or if a remap binds it.
    pub fn sequence_has_followup(&self, seq: &[KeyEvent]) -> bool {
        self.remaps
            .iter()
            .any(|remap| remap.takes.starts_with(seq) && remap.takes.len() > seq.len())
            || self.bindings.sequence_has_followup(seq)
    }

    /// The [`Description`]s for the bindings available, given the
    /// keys sent so far
    pub fn descriptions_for<'a>(
        &'a self,
        seq: &'a [KeyEvent],
    ) -> (Option<&'a Text>, impl Iterator<Item = Description<'a>>) {
        let bindings = self.bindings.bindings_for(seq);

        let iter = bindings
            .into_iter()
            .flat_map(|bindings| bindings.list.iter())
            .map(|(pats, desc, _)| Description {
                text: Some(desc.text()),
                keys: KeyDescriptions {
                    seq,
                    ty: DescriptionType::Binding(pats, pats.iter(), StripPrefix {
                        seq,
                        remaps: self.remaps.iter(),
                    }),
                },
            })
            .chain(
                self.remaps
                    .iter()
                    .filter(move |remap| {
                        if !remap.takes.starts_with(seq) {
                            return false;
                        }
                        if remap.desc.is_some() {
                            return true;
                        }

                        if let (Gives::Keys(gives), Some(bindings)) = (&remap.gives, bindings)
                            && gives.len() == 1
                            && bindings
                                .list
                                .iter()
                                .any(|(pats, ..)| pats.iter().any(|pat| pat.matches(gives[0])))
                        {
                            false
                        } else {
                            true
                        }
                    })
                    .map(|remap| Description {
                        text: remap.desc.as_ref().map(Selectionless::text).or_else(|| {
                            if let Gives::Keys(keys) = &remap.gives {
                                self.bindings.description_for(keys)
                            } else {
                                None
                            }
                        }),
                        keys: KeyDescriptions {
                            seq,
                            ty: DescriptionType::Remap(Some(remap)),
                        },
                    }),
            );

        (bindings.and_then(|b| b.title.as_ref()), iter)
    }

    /// Replace the description for a sequence of [`KeyEvent`]s
    fn replace_seq_description(&mut self, seq: &[KeyEvent], new: Text) {
        if let Some(remap) = self.remaps.iter_mut().find(|remap| remap.takes == seq) {
            remap.desc = (!new.is_empty_empty()).then_some(new.no_selections());
        } else if let Some(desc) = self.bindings.description_for_mut(seq) {
            *desc = new.no_selections();
        }
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
    pub keys: KeyDescriptions<'a>,
}

/// A [`Mode`]'s bound [`Binding`] or a mapped [`KeyEvent`]
/// sequence
#[derive(Debug)]
pub enum KeyDescription<'a> {
    /// A [`Mode`]'s regular binding, comes from the [`Bindings`]
    /// struct
    Binding(Binding),
    /// A remapped sequence, comes from [`map`] or [`alias`]
    Remap(&'a [KeyEvent], &'a Gives),
}

/// An [`Iterator`] over the possible patterns that match a
/// [`Description`]
///
/// This returns a [`KeyDescription`], where
/// [`KeyDescription::Binding`] represents a pattern that is naturally
/// bound to a [`Mode`], via [`Mode::bindings`], while a
/// [`KeyDescription::Remap`] represents a [`KeyEvent`] sequence that
/// was [mapped] or [aliased] to it.
///
/// [mapped]: map
/// [aliased]: alias
pub struct KeyDescriptions<'a> {
    seq: &'a [KeyEvent],
    ty: DescriptionType<'a>,
}

impl KeyDescriptions<'_> {
    /// Gets a [`Text`] to describe the [`Binding`]s and remaps
    ///
    /// This function makes use of the `key.char`, `key.mod`,
    /// `key.special`, `key.range` and `key.any`, `separator` and
    /// `remap` [`Form`]s.
    pub fn into_text(self) -> Text {
        let mut builder = Text::builder();

        for (i, key_desc) in self.enumerate() {
            if i > 0 {
                builder.push(txt!("[separator],"));
            }
            match key_desc {
                KeyDescription::Binding(binding) => builder.push(binding.as_text()),
                KeyDescription::Remap(key_events, _) => {
                    builder.push(txt!("[remap:100]{}", super::keys_to_text(key_events)))
                }
            }
        }

        builder.build()
    }
}

impl<'a> Iterator for KeyDescriptions<'a> {
    type Item = KeyDescription<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (pats, pats_iter, deprefixed) = match &mut self.ty {
            DescriptionType::Binding(pats, pats_iter, deprefixed) => (pats, pats_iter, deprefixed),
            DescriptionType::Remap(remap) => {
                return remap.take().and_then(|remap| {
                    remap
                        .takes
                        .strip_prefix(self.seq)
                        .map(|takes| KeyDescription::Remap(takes, &remap.gives))
                });
            }
        };

        pats_iter
            .find_map(|pat| {
                pat.as_key_event()
                    .is_none_or(|key_event| {
                        !deprefixed
                            .clone()
                            .any(|(_, takes)| takes.starts_with(&[key_event]))
                    })
                    .then_some(KeyDescription::Binding(*pat))
            })
            .or_else(|| {
                deprefixed.find_map(|(remap, takes)| {
                    if remap.takes.starts_with(self.seq)
                        && remap.desc.is_none()
                        && let Gives::Keys(given_keys) = &remap.gives
                        && given_keys.len() == 1
                        && pats.iter().any(|pat| pat.matches(given_keys[0]))
                    {
                        Some(KeyDescription::Remap(takes, &remap.gives))
                    } else {
                        None
                    }
                })
            })
    }
}

/// Two types of description
enum DescriptionType<'a> {
    Binding(&'a [Binding], slice::Iter<'a, Binding>, StripPrefix<'a>),
    Remap(Option<&'a Remap>),
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

#[derive(Clone)]
struct StripPrefix<'a> {
    seq: &'a [KeyEvent],
    remaps: slice::Iter<'a, Remap>,
}

impl<'a> Iterator for StripPrefix<'a> {
    type Item = (&'a Remap, &'a [KeyEvent]);

    fn next(&mut self) -> Option<Self::Item> {
        let remap = self.remaps.next()?;
        Some((remap, remap.takes.strip_prefix(self.seq)?))
    }
}
