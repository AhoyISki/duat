use std::{
    ops::{Bound, RangeBounds},
    sync::Arc,
};

use crossterm::event::{
    KeyCode, KeyEvent, KeyEventKind, KeyEventState, MediaKeyCode, ModifierKeyCode,
};

pub use crate::__bindings__ as bindings;
use crate::{mode::KeyMod, text::Selectionless};

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
    /// A function to determine which [`KeyEvent`] should result in
    /// which followup.
    ///
    /// Direct implementation is not recommended, use the
    /// [`bindings!`] macro instead
    #[doc(hidden)]
    pub matcher: Arc<dyn Fn(KeyEvent) -> Option<usize> + Send + Sync + 'static>,
    /// Descriptions for each of the key bindings
    ///
    /// The bindings of the first element are na _alternation_, not a
    /// _sequence_.
    ///
    /// Direct implementation is not recommended, use the
    /// [`bindings!`] macro instead.
    #[doc(hidden)]
    pub results: Vec<(Vec<KeyEventPat>, Selectionless, Option<Bindings>)>,
}

impl Bindings {
    /// Wether the given sequence of [`KeyEvent`]s is bound by these
    /// `Bindings`
    pub fn matches_sequence(&self, seq: &[KeyEvent]) -> bool {
        let mut matcher = &self.matcher;
        seq.iter().all(|key_event| {
            if let Some(i) = matcher(*key_event) {
                matcher = &self.results[i].2.as_ref().unwrap_or(self).matcher;
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
                    let i = (bindings.matcher)(*key_event)?;
                    bindings = bindings.results[i].2.as_ref()?;
                    Some(bindings)
                })
                .last()
        }
    }
}

/// Possible ways to map keys
///
/// This struct serves the purpose of allowing the use of pattern-like
/// syntax in order to match keys in the [`bindings!`] macro, while
/// still creating a finitely known list of keys, which can then be
/// used for documentation.
#[derive(Debug, Clone, Copy)]
pub enum KeyEventPat {
    CharRange(Bound<char>, Bound<char>, KeyMod),
    FnRange(Bound<u8>, Bound<u8>, KeyMod),
    AnyModifier(KeyMod),
    AnyMedia(KeyMod),
    Concrete(Binding),
}

impl KeyEventPat {
    /// Returns a new [concrete `KeyEventPat`]
    ///
    /// [concrete `KeyEventPat`]: KeyEventPat::Concrete
    pub fn new(code: KeyCode, modif: KeyMod) -> Self {
        Self::Concrete(Binding {
            code: Some(code),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }

    /// Returns a `KeyEventPat` that could match _anything_
    pub fn anything() -> Self {
        Self::Concrete(Binding::default())
    }

    /// A [`KeyEvent`], with assumptions about less used options
    ///
    /// Only returns [`Some`] if this is [`KeyEventPat::Concrete`]
    /// with a concrete [`KeyCode`] and [`KeyMod`].
    pub fn as_key_event(&self) -> Option<KeyEvent> {
        let &KeyEventPat::Concrete(Binding {
            code: Some(code),
            modif: Some(modifiers),
            kind,
            state,
        }) = self
        else {
            return None;
        };

        Some(KeyEvent {
            code,
            modifiers,
            kind: kind.unwrap_or(KeyEventKind::Press),
            state: state.unwrap_or(KeyEventState::NONE),
        })
    }

    /// Wether a [`KeyEvent`] would be matched by this `KeyEventPat`
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

        match *self {
            KeyEventPat::CharRange(b0, b1, modifiers) => {
                if let KeyCode::Char(char) = key_event.code {
                    key_event.modifiers == modifiers
                        && (key_event.kind.is_press() || key_event.kind.is_repeat())
                        && key_event.state == KeyEventState::NONE
                        && contains(b0, b1, char)
                } else {
                    false
                }
            }
            KeyEventPat::FnRange(b0, b1, modifiers) => {
                if let KeyCode::F(num) = key_event.code {
                    key_event.modifiers == modifiers
                        && (key_event.kind.is_press() || key_event.kind.is_repeat())
                        && key_event.state == KeyEventState::NONE
                        && contains(b0, b1, num)
                } else {
                    false
                }
            }
            KeyEventPat::AnyModifier(modifiers) => {
                matches!(key_event.code, KeyCode::Modifier(_))
                    && key_event.modifiers == modifiers
                    && (key_event.kind.is_press() || key_event.kind.is_repeat())
                    && key_event.state == KeyEventState::NONE
            }
            KeyEventPat::AnyMedia(modifiers) => {
                matches!(key_event.code, KeyCode::Media(_))
                    && key_event.modifiers == modifiers
                    && (key_event.kind.is_press() || key_event.kind.is_repeat())
                    && key_event.state == KeyEventState::NONE
            }
            KeyEventPat::Concrete(binding) => {
                binding.code.is_none_or(|code| code == key_event.code)
                    && binding
                        .modif
                        .is_none_or(|modif| modif == key_event.modifiers)
                    && binding.kind.is_none_or(|kind| kind == key_event.kind)
                    && binding.state.is_none_or(|state| state == key_event.state)
            }
        }
    }
}

macro_rules! implFromRange {
    ($($range:ident)::+) => {
        impl From<($($range)::+<char>, KeyMod)> for KeyEventPat {
            fn from((chars, modif): ($($range)::+<char>, KeyMod)) -> Self {
                KeyEventPat::CharRange(
                    chars.start_bound().cloned(),
                    chars.end_bound().cloned(),
                    modif
                )
            }
        }

        impl From<($($range)::+<u8>, KeyMod)> for KeyEventPat {
            fn from((fns, modif): ($($range)::+<u8>, KeyMod)) -> Self {
                KeyEventPat::FnRange(fns.start_bound().cloned(), fns.end_bound().cloned(), modif)
            }
        }
    };
}

implFromRange!(std::ops::Range);
implFromRange!(std::ops::RangeFrom);
implFromRange!(std::ops::RangeInclusive);
implFromRange!(std::ops::RangeTo);
implFromRange!(std::ops::RangeToInclusive);

impl From<(char, KeyMod)> for KeyEventPat {
    fn from((char, modif): (char, KeyMod)) -> Self {
        KeyEventPat::Concrete(Binding {
            code: Some(KeyCode::Char(char)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

impl From<(u8, KeyMod)> for KeyEventPat {
    fn from((f_key, modif): (u8, KeyMod)) -> Self {
        KeyEventPat::Concrete(Binding {
            code: Some(KeyCode::F(f_key)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

impl From<(MediaKeyCode, KeyMod)> for KeyEventPat {
    fn from((media, modif): (MediaKeyCode, KeyMod)) -> Self {
        KeyEventPat::Concrete(Binding {
            code: Some(KeyCode::Media(media)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

impl From<(ModifierKeyCode, KeyMod)> for KeyEventPat {
    fn from((modifier, modif): (ModifierKeyCode, KeyMod)) -> Self {
        KeyEventPat::Concrete(Binding {
            code: Some(KeyCode::Modifier(modifier)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

/// A key binding, which can have any number of defined fields.
///
/// The less specific the key binding (i.e., the more [`None`]s it
/// has), the less prioritized it is.
#[derive(Debug, Default, Clone, Copy)]
pub struct Binding {
    pub code: Option<KeyCode>,
    pub modif: Option<KeyMod>,
    pub kind: Option<KeyEventKind>,
    pub state: Option<KeyEventState>,
}

#[macro_export]
#[doc(hidden)]
macro_rules! __bindings__ {
    (match _ $match:tt) => {{
        #[allow(clippy::vec_init_then_push)]
        let matcher = $crate::mode::bindings!(@matcher $match);

        #[allow(clippy::vec_init_then_push)]
        let bindings: Vec<_> = $crate::mode::bindings!(@bindings $match);

        #[allow(clippy::vec_init_then_push)]
        let descriptions = $crate::mode::bindings!(@descriptions $match);

        #[allow(clippy::vec_init_then_push)]
        let followups = $crate::mode::bindings!(@followups $match);

        $crate::mode::Bindings {
            matcher,
            results: bindings
                .into_iter()
                .zip(descriptions)
                .zip(followups)
                .map(|((b, d), f)| (b, d, f))
                .collect()
        }
    }};

    (@matcher { $($patterns:tt)* }) => {{
        #[allow(unused_assignments, irrefutable_let_patterns)]
        std::sync::Arc::new(move |key_event: $crate::mode::KeyEvent| {
            let mut index = 0;
            $crate::mode::bindings!(@match_entry index, key_event: $($patterns)*);

            None
        })
    }};

    (@match_entry $index:ident, $key_event:ident:) => {};
    (@match_entry
        $index:ident,
        $key_event:ident: $modif:ident$excl:tt($($tokens:tt)*) => $result:expr
        $(,$($rest:tt)*)?
    ) => {
        if let $modif$excl($($tokens)*) = $key_event {
            return Some($index)
        }
        $index += 1;
        $crate::mode::bindings!(@match_entry $index, $key_event: $($($rest)*)?)
    };
    (@match_entry $index:ident, $key_event:ident: $pattern:pat => $result:expr $(,$($rest:tt)*)?) => {
        if let $pattern = $key_event {
            return Some($index)
        }
        $index += 1;
        $crate::mode::bindings!(@match_entry $index, $key_event: $($($rest)*)?)
    };

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
        $list.last_mut().unwrap().push($crate::mode::KeyEventPat::anything());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ => $result:expr, $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::KeyEventPat::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ => $result:tt $(,)? $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::KeyEventPat::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: $pattern:expr => $result:expr, $($rest:tt)*) => {
        $list.last_mut().push($crate::mode::KeyEventPat::anything());
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
