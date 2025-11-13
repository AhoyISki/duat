use std::{
    ops::{Bound, RangeBounds},
    sync::Arc,
};

use crossterm::event::{
    KeyCode, KeyEvent, KeyEventKind, KeyEventState, MediaKeyCode, ModifierKeyCode,
};

pub use crate::__bindings__ as bindings;
use crate::{mode::KeyMod, text::Text};

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
    pub matcher: Arc<dyn Fn(KeyEvent) -> Option<usize>>,
    /// Descriptions for each of the key bindings
    ///
    /// Direct implementation is not recommended, use the
    /// [`bindings!`] macro instead.
    #[doc(hidden)]
    pub results: Vec<(Vec<BindingPat>, Text, Option<Bindings>)>,
}

/// Possible ways to map keys
///
/// This struct serves the purpose of allowing the use of pattern-like
/// syntax in order to match keys in the [`bindings!`] macro, while
/// still creating a finitely known list of keys, which can then be
/// used for documentation.
#[derive(Debug, Clone)]
pub enum BindingPat {
    CharRange(Bound<char>, Bound<char>, KeyMod),
    FnRange(Bound<u8>, Bound<u8>, KeyMod),
    AnyModifier(KeyMod),
    AnyMedia(KeyMod),
    Concrete(Binding),
}

impl BindingPat {
    /// Returns a new [concrete `BindingPat`]
    ///
    /// [concrete `BindingPat`]: BindingPat::Concrete
    pub fn new(code: KeyCode, modif: KeyMod) -> Self {
        Self::Concrete(Binding {
            code: Some(code),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }

    /// Returns a `BindingPat` that could match _anything_
    pub fn anything() -> Self {
        Self::Concrete(Binding::default())
    }
}

macro_rules! implFromRange {
    ($($range:ident)::+) => {
        impl From<($($range)::+<char>, KeyMod)> for BindingPat {
            fn from((chars, modif): ($($range)::+<char>, KeyMod)) -> Self {
                BindingPat::CharRange(
                    chars.start_bound().cloned(),
                    chars.end_bound().cloned(),
                    modif
                )
            }
        }

        impl From<($($range)::+<u8>, KeyMod)> for BindingPat {
            fn from((fns, modif): ($($range)::+<u8>, KeyMod)) -> Self {
                BindingPat::FnRange(fns.start_bound().cloned(), fns.end_bound().cloned(), modif)
            }
        }
    };
}

implFromRange!(std::ops::Range);
implFromRange!(std::ops::RangeFrom);
implFromRange!(std::ops::RangeInclusive);
implFromRange!(std::ops::RangeTo);
implFromRange!(std::ops::RangeToInclusive);

impl From<(char, KeyMod)> for BindingPat {
    fn from((char, modif): (char, KeyMod)) -> Self {
        BindingPat::Concrete(Binding {
            code: Some(KeyCode::Char(char)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

impl From<(u8, KeyMod)> for BindingPat {
    fn from((f_key, modif): (u8, KeyMod)) -> Self {
        BindingPat::Concrete(Binding {
            code: Some(KeyCode::F(f_key)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

impl From<(MediaKeyCode, KeyMod)> for BindingPat {
    fn from((media, modif): (MediaKeyCode, KeyMod)) -> Self {
        BindingPat::Concrete(Binding {
            code: Some(KeyCode::Media(media)),
            modif: Some(modif),
            kind: None,
            state: None,
        })
    }
}

impl From<(ModifierKeyCode, KeyMod)> for BindingPat {
    fn from((modifier, modif): (ModifierKeyCode, KeyMod)) -> Self {
        BindingPat::Concrete(Binding {
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
        $list.last_mut().unwrap().push($crate::mode::BindingPat::anything());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ => $result:expr, $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::BindingPat::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: _ => $result:tt $(,)? $($rest:tt)*) => {
        $list.last_mut().unwrap().push($crate::mode::BindingPat::anything());
        $list.push(Vec::new());
        $crate::mode::bindings!(@binding_entry $list: $($($rest)*)?);
    };
    (@binding_entry $list:ident: $pattern:expr => $result:expr, $($rest:tt)*) => {
        $list.last_mut().push($crate::mode::BindingPat::anything());
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
        $list.push($text);
        $crate::mode::bindings!(@description_entry $list: $($($rest)*)?);
    };
    (@description_entry $list:ident: $pattern:pat => $text:expr $(,$($rest:tt)*)?) => {
        $list.push($text);
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
