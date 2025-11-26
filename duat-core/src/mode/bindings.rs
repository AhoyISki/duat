use std::ops::{Bound, RangeBounds};

use crossterm::event::{
    KeyCode, KeyEvent, KeyEventKind, KeyEventState, MediaKeyCode, ModifierKeyCode,
};

pub use crate::__bindings__ as bindings;
use crate::{
    mode::KeyMod,
    text::{Selectionless, Text, txt},
};

/// A list of key bindings available in a given [`Mode`]
///
/// This list is used for two purposes:
///
/// - Provide information about which keys are available in any given
///   `Mode`.
/// - Tell the user when they typed in a unavailable binding.
/// - Warn them when they [map] or [alias] to a unavailable binding.
///
/// You should _always_ create this struct via
///
/// [`Mode`]: super::Mode
/// [map]: super::map
/// [alias]: super::alias
#[derive(Clone)]
pub struct Bindings {
    /// An optional title for this `Bindings`
    ///
    /// This should be used to more accurately describe an overall
    /// "theme" for all keybindings listed.
    pub title: Option<Text>,
    /// Descriptions for each of the key bindings
    ///
    /// The bindings of the first element are na _alternation_, not a
    /// _sequence_.
    ///
    /// Direct implementation is not recommended, use the
    /// [`bindings!`] macro instead.
    pub list: Vec<(Vec<Binding>, Selectionless, Option<Bindings>)>,
}

impl Bindings {
    /// Wether these `MappedBindings` accepts the sequence of
    /// [`KeyEvent`]s
    pub fn matches_sequence(&self, seq: &[KeyEvent]) -> bool {
        let mut list = &self.list;
        seq.iter().all(|key_event| {
            if let Some((.., bindings)) = list.iter().find(matches_event(*key_event)) {
                list = &bindings.as_ref().unwrap_or(self).list;
                true
            } else {
                false
            }
        })
    }

    /// Wether the given sequence of [`KeyEvent`]s has a followup
    /// in these `MappedBindings`
    pub fn sequence_has_followup(&self, seq: &[KeyEvent]) -> bool {
        let mut list = &self.list;
        seq.iter().all(|key_event| {
            if let Some((.., bindings)) = list.iter().find(matches_event(*key_event))
                && let Some(bindings) = bindings
            {
                list = &bindings.list;
                true
            } else {
                false
            }
        })
    }

    /// Which `Bindings` are available, given the passed sequence
    pub fn bindings_for(&self, seq: &[KeyEvent]) -> Option<&Bindings> {
        let mut bindings = self;
        if seq.is_empty() {
            Some(self)
        } else {
            seq.iter()
                .map_while(|key_event| {
                    let (.., nested) = bindings.list.iter().find(matches_event(*key_event))?;
                    bindings = nested.as_ref()?;
                    Some(bindings)
                })
                .nth(seq.len() - 1)
        }
    }

    /// The description for a particular sequence of bound [keys]
    ///
    /// [keys]: KeyEvent
    pub fn description_for<'a>(&'a self, seq: &[KeyEvent]) -> Option<&'a Text> {
        let mut bindings = Some(self);
        seq.iter()
            .map_while(|key_event| {
                let (_, selless, nested) = bindings?.list.iter().find(matches_event(*key_event))?;
                bindings = nested.as_ref();
                Some(selless.text())
            })
            .last()
    }

    /// The description for a particular sequence of bound [keys]
    ///
    /// [keys]: KeyEvent
    pub fn description_for_mut<'a>(
        &'a mut self,
        seq: &[KeyEvent],
    ) -> Option<&'a mut Selectionless> {
        let mut bindings = Some(self);
        seq.iter()
            .map_while(move |key_event| {
                let (_, selless, nested) =
                    bindings.take()?.list.iter_mut().find(|(list, ..)| {
                        list.iter().any(|binding| binding.matches(*key_event))
                    })?;
                bindings = nested.as_mut();
                Some(selless)
            })
            .last()
    }
}

fn matches_event(
    key_event: KeyEvent,
) -> impl FnMut(&&(Vec<Binding>, Selectionless, Option<Bindings>)) -> bool {
    move |(list, ..)| list.iter().any(|binding| binding.matches(key_event))
}

impl std::fmt::Debug for Bindings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Bindings")
            .field("list", &self.list)
            .finish()
    }
}

/// Possible ways to map keys
///
/// This struct serves the purpose of allowing the use of pattern-like
/// syntax in order to match keys in the [`bindings!`] macro, while
/// still creating a finitely known list of keys, which can then be
/// used for documentation.
#[derive(Debug, Clone, Copy)]
pub enum Binding {
    /// A range of [`KeyEvent`]s with [`KeyCode::Char`], like
    /// `KeyCode::Char('0'..='9')`
    CharRange(Bound<char>, Bound<char>, KeyMod),
    /// A range of [`KeyEvent`]s with [`KeyCode::F`], like
    /// `KeyCode::F(1..=3)`
    FnRange(Bound<u8>, Bound<u8>, KeyMod),
    /// Any modifier key, like [`ModifierKeyCode::LeftShift`]
    ///
    /// Unlikely to be bound
    AnyModifier(KeyMod),
    /// Any media key, like [`MediaKeyCode::MuteVolume`]
    AnyMedia(KeyMod),
    /// Any [`KeyCode`], might match anything, given the optional
    /// [`KeyMod`]
    Any(Option<KeyMod>),
    /// A specific [`KeyCode`]/[`KeyMod`] combo
    Event(KeyCode, KeyMod),
}

impl Binding {
    /// Returns a new [concrete `Binding`]
    ///
    /// [concrete `Binding`]: Binding::Pattern
    pub fn new(code: KeyCode, modif: KeyMod) -> Self {
        Self::Event(code, modif)
    }

    /// Returns a `Binding` that could match _anything_
    pub fn anything() -> Self {
        Self::Any(None)
    }

    /// The default [`Text`] formatting for a `Binding`
    ///
    /// This function makes use of the `key.char`, `key.mod`,
    /// `key.special`, `key.range` and `key.any` [`Form`]s.
    ///
    /// [`Form`]: crate::form::Form
    pub fn as_text(&self) -> Text {
        let mut builder = Text::builder();

        if !matches!(self, Binding::Event(..))
            && self.modifier().is_some_and(|modif| modif != KeyMod::NONE)
        {
            builder.push(txt!("[key.angle]<"));
            builder.push(super::modifier_text(
                self.modifier().unwrap_or(KeyMod::NONE),
            ))
        }

        builder.push(match self {
            Binding::CharRange(b0, b1, _) => match (b0, b1) {
                (Bound::Included(lhs), Bound::Included(rhs)) => {
                    txt!("[key.char]'{lhs}'[key.range]..=[key.char]'{rhs}'")
                }
                (Bound::Included(lhs), Bound::Excluded(rhs)) => {
                    txt!("[key.char]'{lhs}'[key.range]..[key.char]'{rhs}'")
                }
                (Bound::Included(char), Bound::Unbounded) => {
                    txt!("[key.char]'{char}'[key.range]..")
                }
                (Bound::Excluded(lhs), Bound::Included(rhs)) => {
                    txt!("[key.char]'{lhs}'[key.range]>..=[key.char]'{rhs}'")
                }
                (Bound::Excluded(lhs), Bound::Excluded(rhs)) => {
                    txt!("[key.char]'{lhs}'[key.range]>..[key.char]'{rhs}'")
                }
                (Bound::Excluded(char), Bound::Unbounded) => {
                    txt!("[key.char]'{char}'[key.range]>..")
                }
                (Bound::Unbounded, Bound::Included(char)) => {
                    txt!("[key.range]..=[key.char]'{char}'")
                }
                (Bound::Unbounded, Bound::Excluded(char)) => {
                    txt!("[key.range]..[key.char]'{char}'")
                }
                (Bound::Unbounded, Bound::Unbounded) => txt!("[key.any.char]{{char}}"),
            },
            Binding::FnRange(b0, b1, _) => match (b0, b1) {
                (Bound::Included(lhs), Bound::Included(rhs)) => {
                    txt!("[key.special]F{lhs}[key.range]..=[key.special]F{rhs}")
                }
                (Bound::Included(lhs), Bound::Excluded(rhs)) => {
                    txt!("[key.special]F{lhs}[key.range]..[key.special]F{rhs}")
                }
                (Bound::Included(num), Bound::Unbounded) => {
                    txt!("[key.special]F{num}[key.range]=..")
                }
                (Bound::Excluded(lhs), Bound::Included(rhs)) => {
                    txt!("[key.special]F{lhs}[key.range]>..=[key.special]F{rhs}")
                }
                (Bound::Excluded(lhs), Bound::Excluded(rhs)) => {
                    txt!("[key.special]F{lhs}[key.range]>..[key.special]F{rhs}")
                }
                (Bound::Excluded(num), Bound::Unbounded) => {
                    txt!("[key.special]F{num}[key.range]>..")
                }
                (Bound::Unbounded, Bound::Included(num)) => {
                    txt!("[key.range]..=[key.special]F{num}")
                }
                (Bound::Unbounded, Bound::Excluded(num)) => {
                    txt!("[key.range]..[key.special]F{num}")
                }
                (Bound::Unbounded, Bound::Unbounded) => txt!("[key.any]{{f key}}"),
            },
            Binding::AnyModifier(_) => txt!("[key.any]{{modifier}}"),
            Binding::AnyMedia(_) => txt!("[key.any]{{media key}}"),
            Binding::Any(_) => txt!("[key.any]{{any}}"),
            Binding::Event(code, modif) => super::keys_to_text(&[KeyEvent::new(*code, *modif)]),
        });

        if !matches!(self, Binding::Event(..))
            && self.modifier().is_some_and(|modif| modif != KeyMod::NONE)
        {
            builder.push(txt!("[key.angle]>"));
        }

        builder.build()
    }

    /// The [`KeyMod`] for this `Binding`
    ///
    /// Can be [`None`] in case the `Binding` accepts any [`KeyEvent`]
    /// whatsoever.
    pub fn modifier(&self) -> Option<KeyMod> {
        match self {
            Binding::CharRange(.., modif)
            | Binding::FnRange(.., modif)
            | Binding::AnyModifier(modif)
            | Binding::AnyMedia(modif)
            | Binding::Event(_, modif) => Some(*modif),
            Binding::Any(modif) => *modif,
        }
    }

    /// A [`KeyEvent`], with assumptions about less used options
    ///
    /// Only returns [`Some`] if this is [`Binding::Pattern`]
    /// with a concrete [`KeyCode`] and [`KeyMod`].
    pub fn as_key_event(&self) -> Option<KeyEvent> {
        let &Binding::Event(code, modifiers) = self else {
            return None;
        };

        Some(KeyEvent {
            code,
            modifiers,
            kind: KeyEventKind::Press,
            state: KeyEventState::NONE,
        })
    }

    /// Wether a [`KeyEvent`] would be matched by this `Binding`
    pub fn matches(&self, key_event: KeyEvent) -> bool {
        fn contains<T: Ord>(b0: Bound<T>, b1: Bound<T>, subject: T) -> bool {
            match b0 {
                Bound::Included(b0) if subject < b0 => return false,
                Bound::Excluded(b0) if subject <= b0 => return false,
                _ => {}
            }
            match b1 {
                Bound::Included(b1) if subject <= b1 => true,
                Bound::Excluded(b1) if subject < b1 => true,
                Bound::Unbounded => true,
                _ => false,
            }
        }

        if key_event.is_release() {
            return false;
        }

        match *self {
            Binding::CharRange(b0, b1, modifiers) => {
                if let KeyCode::Char(char) = key_event.code {
                    key_event.modifiers == modifiers && contains(b0, b1, char)
                } else {
                    false
                }
            }
            Binding::FnRange(b0, b1, modifiers) => {
                if let KeyCode::F(num) = key_event.code {
                    key_event.modifiers == modifiers && contains(b0, b1, num)
                } else {
                    false
                }
            }
            Binding::AnyModifier(modifiers) => {
                matches!(key_event.code, KeyCode::Modifier(_)) && key_event.modifiers == modifiers
            }
            Binding::AnyMedia(modifiers) => {
                matches!(key_event.code, KeyCode::Media(_)) && key_event.modifiers == modifiers
            }
            Binding::Any(modif) => modif.is_none_or(|modif| modif == key_event.modifiers),
            Binding::Event(code, modif) => code == key_event.code && modif == key_event.modifiers,
        }
    }
}

macro_rules! implFromRange {
    ($($range:ident)::+) => {
        impl From<($($range)::+<char>, KeyMod)> for Binding {
            fn from((chars, modif): ($($range)::+<char>, KeyMod)) -> Self {
                Binding::CharRange(
                    chars.start_bound().cloned(),
                    chars.end_bound().cloned(),
                    modif
                )
            }
        }

        impl From<($($range)::+<u8>, KeyMod)> for Binding {
            fn from((fns, modif): ($($range)::+<u8>, KeyMod)) -> Self {
                Binding::FnRange(fns.start_bound().cloned(), fns.end_bound().cloned(), modif)
            }
        }
    };
}

implFromRange!(std::ops::Range);
implFromRange!(std::ops::RangeFrom);
implFromRange!(std::ops::RangeInclusive);
implFromRange!(std::ops::RangeTo);
implFromRange!(std::ops::RangeToInclusive);

impl From<(char, KeyMod)> for Binding {
    fn from((char, modif): (char, KeyMod)) -> Self {
        Binding::Event(KeyCode::Char(char), modif)
    }
}

impl From<(u8, KeyMod)> for Binding {
    fn from((num, modif): (u8, KeyMod)) -> Self {
        Binding::Event(KeyCode::F(num), modif)
    }
}

impl From<(MediaKeyCode, KeyMod)> for Binding {
    fn from((media, modif): (MediaKeyCode, KeyMod)) -> Self {
        Binding::Event(KeyCode::Media(media), modif)
    }
}

impl From<(ModifierKeyCode, KeyMod)> for Binding {
    fn from((modifier, modif): (ModifierKeyCode, KeyMod)) -> Self {
        Binding::Event(KeyCode::Modifier(modifier), modif)
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! __bindings__ {
    (match _ $match:tt) => {{
        #[allow(clippy::vec_init_then_push)]
        let bindings: Vec<_> = $crate::mode::bindings!(@bindings $match);

        #[allow(clippy::vec_init_then_push)]
        let descriptions = $crate::mode::bindings!(@descriptions $match);

        #[allow(clippy::vec_init_then_push)]
        let followups = $crate::mode::bindings!(@followups $match);

        $crate::mode::Bindings {
            title: None,
            list: bindings
                .into_iter()
                .zip(descriptions)
                .zip(followups)
                .map(|((b, d), f)| (b, d, f))
                .collect()
        }
    }};

    (@bindings { $($patterns:tt)* }) => {{
        let mut list = vec![Vec::new()];
        $crate::mode::bindings!(@binding_entry list: $($patterns)*);
        list
    }};

    (@binding_entry $list:ident:) => {};
    (@binding_entry
        $list:ident: $modif:ident$excl:tt($($tokens:tt)*) | $($rest:tt)*
    ) => {
        let last = $list.last_mut().unwrap();
        $modif$excl(@bindings [] last, $($tokens)*);
        $crate::mode::bindings!(@binding_entry $list: $($rest)*);
    };
    (@binding_entry
        $list:ident: $modif:ident$excl:tt($($tokens:tt)*) => $result:expr $(, $($rest:tt)*)?
    ) => {
        let last = $list.last_mut().unwrap();
        $modif$excl(@bindings [] last, $($tokens)*);
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry
        $list:ident: $modif:ident$excl:tt($($tokens:tt)*) => $result:tt $(,)? $($rest:tt)*
    ) => {
        let last = $list.last_mut().unwrap();
        $modif$excl(@bindings last, [] $($tokens)*);
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ | $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::Binding::anything());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ => $result:expr, $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::Binding::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ => $result:tt $(,)? $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::Binding::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: $pattern:expr => $result:expr, $($rest:tt)*) => {
        $list.last_mut().push($crate::mode::Binding::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: $binding_pat:expr => $matcher:tt $(,)? $($rest:tt)*) => {
        $list.last_mut().unwrap().push($binding_pat);
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };

    (@descriptions { $($patterns:tt)* }) => {{
        let mut list = Vec::new();
        $crate::mode::bindings!(@description_entry list: $($patterns)*);
        list
    }};

    (@description_entry $list:ident:) => {};
    (@description_entry
        $list:ident:
        $pattern:pat => ($text:expr, $($matcher:tt)+)
        $(,$($rest:tt)*)?
    ) => {
        $list.push($text.no_selections());
        $crate::mode::bindings!(@description_entry $list: $($($rest)*)?);
    };
    (@description_entry $list:ident: $pattern:pat => $text:expr $(,$($rest:tt)*)?) => {
        $list.push($text.no_selections());
        $crate::mode::bindings!(@description_entry $list: $($($rest)*)?);
    };

    (@followups { $($patterns:tt)+ }) => {{
        let mut list = Vec::new();
        $crate::mode::bindings!(@followup_entry list: $($patterns)+);
        list
    }};

    (@followup_entry $list:ident:) => {};
    (@followup_entry
        $list:ident:
        $pattern:pat => ($texts:expr, match _ $match:tt)
        $(,$($rest:tt)*)?
    ) => {
        $list.push(Some($crate::mode::bindings! { match _ $match }));
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
    };
    (@followup_entry
        $list:ident:
        $pattern:pat => ($text:expr, $bindings:expr)
        $(,$($rest:tt)*)?
    ) => {
        $list.push(Some($bindings));
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
    };
    (@followup_entry $list:ident: $pattern:pat => $text:expr $(,$($rest:tt)*)?) => {
        $list.push(None);
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
    };
}
