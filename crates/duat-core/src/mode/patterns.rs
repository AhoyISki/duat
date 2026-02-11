//! Macros for very easy [`Mode`] pattern matching
//!
//! This module adds the [`event!`], [`alt!`], [`ctrl!`] and
//! [`shift!`] macros, which can be used to create very succinct
//! patterns for matching, greatly improving on the signal to noise
//! ratio, especially inside the [`Mode::send_key`] function.
//!
//! [`Mode`]: super::Mode
//! [`Mode::send_key`]: super::Mode::send_key
use crate::mode::KeyMod;
#[doc(inline)]
pub use crate::{__alt__ as alt, __ctrl__ as ctrl, __event__ as event, __shift__ as shift};

/// Macro for shortening [`KeyEvent`]s in pattern matching
///
/// This macro just turns this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, event};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     event!('c') => {
///         //...
///     }
///     event!(KeyCode::Backspace | KeyCode::Tab) => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// Into this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, KeyEventKind, KeyMod};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     KeyEvent {
///         code: KeyCode::Char('c'),
///         modifiers: KeyMod::NONE,
///         kind: KeyEventKind::Press | KeyEventKind::Repeat,
///         ..
///     } => {
///         //...
///     }
///     KeyEvent {
///         code: KeyCode::Backspace | KeyCode::Tab,
///         modifiers: KeyMod::NONE,
///         kind: KeyEventKind::Press | KeyEventKind::Repeat,
///         ..
///     } => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// This is very useful for pattern matching inside of [`Mode`]s,
/// allowing you to significantly cut down on the amount of
/// boilerplate being written.
///
/// You should also check out [`alt!`], [`ctrl!`] and [`shift!`],
/// which are like this macro, but also add their respective
/// [`KeyMod`]s, and can be nested.
///
/// [`KeyEvent`]: super::KeyEvent
/// [`Mode`]: super::Mode
#[macro_export]
#[doc(hidden)]
macro_rules! __event__ {
    (@code $($tokens:tt)*) => {
        $crate::__modified__!(@code $($tokens)*)
    };
    (@modif [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@modif [$($mods),*] $($tokens)*)
    };
    (@bindings [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@bindings [$($mods),*] $($tokens)*)
    };
    ($($tokens:tt)*) => {
        $crate::__modified__!([] $($tokens)*)
    };
}

/// Macro for pattern matching [`KeyEvent`]s with [`KeyMod::ALT`]
///
/// This macro essentially turns this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, alt};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     alt!('c') => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// Into this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, KeyMod, KeyEventKind};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     KeyEvent {
///         code: KeyCode::Char('c'),
///         modifiers: KeyMod::ALT,
///         kind: KeyEventKind::Press | KeyEventKind::Repeat,
///         ..
///     } => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// This is very useful for [`Mode`]s, which require matching on a
/// large number of different `KeyEvent` patterns in order to decide
/// what to do.
///
/// You can also use this with more complex patterns:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, alt, ctrl, shift};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     ctrl!(alt!('a' | 'b' | 'c')) | shift!(alt!(KeyCode::F(3 | 5))) => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// For the other two modifiers with this convenience, see [`ctrl!`]
/// and [`shift!`]. There is also [`event!`], which has no modifiers,
/// and it _cannot_ be nested with the other ones, for obvious
/// reasons.
///
/// [`KeyEvent`]: super::KeyEvent
/// [`Mode`]: super::Mode
#[macro_export]
#[doc(hidden)]
macro_rules! __alt__ {
    (@code $($tokens:tt)*) => {
        $crate::__modified__!(@code $($tokens)*)
    };
    (@modif [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@modif [ALT $(, $mods)*] $($tokens)*)
    };
    (@bindings [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@bindings [ALT $(, $mods)*] $($tokens)*)
    };
    ($($tokens:tt)*) => {
        $crate::__modified__!([ALT] $($tokens)*)
    };
}

/// Macro for pattern matching [`KeyEvent`]s with [`KeyMod::CONTROL`]
///
/// This macro essentially turns this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, ctrl};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     ctrl!(KeyCode::Backspace) => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// Into this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, KeyMod, KeyEventKind};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     KeyEvent {
///         code: KeyCode::Backspace,
///         modifiers: KeyMod::CONTROL,
///         kind: KeyEventKind::Press | KeyEventKind::Repeat,
///         ..
///     } => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// This is very useful for [`Mode`]s, which require matching on a
/// large number of different `KeyEvent` patterns in order to decide
/// what to do.
///
/// You can also use this with more complex patterns:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, alt, ctrl};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     ctrl!(alt!(KeyCode::Char('a' | 'b' | 'c') | KeyCode::BackTab)) => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// For the other two modifiers with this convenience, see [`alt!`]
/// and [`shift!`]. There is also [`event!`], which has no modifiers,
/// and it _cannot_ be nested with the other ones, for obvious
/// reasons.
///
/// [`KeyEvent`]: super::KeyEvent
/// [`Mode`]: super::Mode
#[macro_export]
#[doc(hidden)]
macro_rules! __ctrl__ {
    (@code $($tokens:tt)*) => {
        $crate::__modified__!(@code $($tokens)*)
    };
    (@modif [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@modif [CONTROL $(,$mods)*] $($tokens)*)
    };
    (@bindings [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@bindings [CONTROL $(,$mods)*] $($tokens)*)
    };
    ($($tokens:tt)*) => {
        $crate::__modified__!([CONTROL] $($tokens)*)
    };
}

/// Macro for pattern matching [`KeyEvent`]s with [`KeyMod::SHIFT`]
///
/// This macro essentially turns this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, shift};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     shift!(KeyCode::Enter) => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// Into this:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, KeyMod, KeyEventKind};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     KeyEvent {
///         code: KeyCode::Enter,
///         modifiers: KeyMod::SHIFT,
///         kind: KeyEventKind::Press | KeyEventKind::Repeat,
///         ..
///     } => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// This is very useful for [`Mode`]s, which require matching on a
/// large number of different `KeyEvent` patterns in order to decide
/// what to do.
///
/// You can also use this with more complex patterns:
///
/// ```rust
/// # use duat_core::mode::{KeyCode, KeyEvent, ctrl, shift};
/// # let key_event: KeyEvent = KeyCode::Char('c').into();
/// match key_event {
///     ctrl!(shift!(KeyCode::PageDown | KeyCode::PageUp)) => {
///         //...
///     }
///     _ => {
///         //...
///     }
/// }
/// ```
///
/// For the other two modifiers with this convenience, see [`alt!`]
/// and [`shift!`]. There is also [`event!`], which has no modifiers,
/// and it _cannot_ be nested with the other ones, for obvious
/// reasons.
///
/// # A note on [`shift!`] specifically
///
/// Unlike [`ctrl!`] and [`alt!`], the [`KeyMod::SHIFT`] modifier is
/// not always emitted, since some keys, like `'B'` can automatically
/// be inferred to be shifted, so the shift modifier is not included
/// with the [`KeyEvent`]. Given that, for now, you should avoid using
/// this macro with [`KeyCode::Char`], since it will probably never
/// match.
///
/// In the future, I might try to rectify this, by detecting when a
/// key is "supposed" to be shifted, and then manually sending a
/// modified [`KeyEvent`], which includes the shift modifier, so
/// pattern matching can be more reliable.
///
/// [`KeyEvent`]: super::KeyEvent
/// [`KeyCode::Char`]: super::KeyCode::Char
/// [`Mode`]: super::Mode
#[macro_export]
#[doc(hidden)]
macro_rules! __shift__ {
    (@code $($tokens:tt)*) => {
        $crate::__modified__!(@code $($tokens)*)
    };
    (@modif [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@modif [SHIFT $(, $mods)*] $($tokens)*)
    };
    (@bindings [$($mods:tt),*] $($tokens:tt)*) => {
        $crate::__modified__!(@bindings [SHIFT $(, $mods)*] $($tokens)*)
    };
    ($($tokens:tt)*) => {
        $crate::__modified__!([SHIFT] $($tokens)*)
    };
}

/// Macro that backs [`alt!`], [`ctrl!`] and [`shift!`]
#[macro_export]
#[doc(hidden)]
macro_rules! __modified__ {
    ([$($mods:ident),*] $($chars:literal)|*) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($($chars)|*),
            modifiers: $crate::__join_modifiers__![$($mods),*],
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    };
    // Straight up useless tbh.
    ([$($mods:ident),*] $char:ident @ $chars:literal) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ $chars),
            modifiers: $crate::__join_modifiers__![$($mods),*],
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    };
    ([$($mods:ident),*] $char:ident @ ($($chars:literal)|*)) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ ($($chars)|*)),
            modifiers: $crate::__join_modifiers__![$($mods),*],
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    };
    // I use $excl here in order to make it look like part of the macro,
    // so rust analyzer properly highlights it.
    ([$($mods:ident),*] $modif:ident$excl:tt($($tokens:tt)*)) => {
        $crate::mode::KeyEvent {
            code: $modif$excl(@code $($tokens)*),
            modifiers: $modif$excl(@modif [$($mods),*] $($tokens)*),
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    };
    ([$($mods:ident),*] $code:pat) => {
        $crate::mode::KeyEvent {
            code: $code,
            modifiers: $crate::__join_modifiers__![$($mods),*],
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    };

    (@code $modif:ident$excl:tt($($tokens:tt)*)) => {
        $modif$excl(@code $($tokens)*)
    };
    (@code $($chars:literal)|*) => {
        $crate::mode::KeyCode::Char($($chars)|*)
    };
    (@code $char:ident @ $chars:literal) => {
        $crate::mode::KeyCode::Char($char @ $chars)
    };
    (@code $char:ident @ ($($chars:literal)|*)) => {
        $crate::mode::KeyCode::Char($char @ ($($chars)|*))
    };
    (@code $code:pat) => {
        $code
    };

    (@modif [$($mods:ident),*] $modif:ident$excl:tt($($tokens:tt)*)) => {
        $modif$excl(@modif [$($mods),*] $($tokens)*)
    };
    (@modif [$($mods:ident),*] $($other:tt)*) => {
        $crate::__join_modifiers__![$($mods),*]
    };

    (@bindings [$($mods:ident),*] $list:ident, $modif:ident$excl:tt($($tokens:tt)*)) => {
        $modif$excl(@bindings [$($mods),*] $list, $($tokens)*)
    };
    (@bindings [$($mods:ident),*] $list:ident, $char:literal) => {{
        $list.push($crate::mode::Binding::new(
            $crate::mode::KeyCode::Char($char),
            $crate::__join_modifiers__![$($mods),*],
        ))
    }};
    (@bindings [$($mods:ident),*] $list:ident, $($chars:literal)|*) => {{
        let modif = $crate::__join_modifiers__![$($mods),*];
        $list.extend([$(
            $crate::mode::Binding::new($crate::mode::KeyCode::Char($chars), modif)
        ),*])
    }};
    (@bindings [$($mods:ident),*] $list:ident, $($tokens:tt)*) => {{
        let modif = $crate::__join_modifiers__![$($mods),*];
        $crate::__modified__!(@fill_bindings $list, modif, $($tokens)*);
    }};

    (@fill_bindings $list:ident, $modif:expr,) => {};
    (@fill_bindings $list:ident, $modif:expr, $($variant:ident)::+ $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::new($($variant)::+, $modif));
        $crate::__modified__!(@fill_bindings $list, $modif, $($($rest)*)?);
    };
    (@fill_bindings
        $list:ident,
        $modif:expr,
        $($variant:ident)::+(..)
        $(| $($rest:tt)*)?
    ) => {
        // For better diagnostics and pattern validation.
        let _unused = |code| matches!(code, $($variant)::+(..));
        $crate::__modified__!(@two_dots_entry $list, $modif, $($variant)::+);
        $crate::__modified__!(@fill_bindings $list, $modif, $($($rest)*)?);
    };
    (@fill_bindings
        $list:ident,
        $modif:expr,
        $($variant:ident)::+($($patterns:tt)*)
        $(| $($rest:tt)*)?
    ) => {
        // For better diagnostics and pattern validation.
        let _unused = |code| matches!(code, $($variant)::*($($patterns)*));
        $crate::__modified__!(@binding_entry $list, $modif, $($patterns)*);
        $crate::__modified__!(@fill_bindings $list, $modif, $($($rest)*)?);
    };
    (@fill_bindings $list:ident, $modif:expr, _) => {
        let mut binding = $crate::mode::Binding::anything();
        binding.modif = Some($modif);
        $list.push(binding);
    };
    (@fill_bindings $list:ident, $modif:expr, $($nonsense:tt)*) => {
        // For better diagnostics and pattern validation.
        let _unused = |code: $crate::mode::KeyCode| matches!(code, $($nonsense)*);
        compile_error!("Pattern may be valid, but can't create known list of bindings");
    };

    (@binding_entry $list:ident, $modif:expr,) => {};
    (@binding_entry $list:ident, $modif:expr, $s:literal..$e:literal $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from(($s..$e, $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };
    (@binding_entry $list:ident, $modif:expr, $s:literal..=$e:literal $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from(($s..=$e, $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };
    (@binding_entry $list:ident, $modif:expr, ..$e:literal $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from((..$e, $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };
    (@binding_entry $list:ident, $modif:expr, ..=$e:literal $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from((..=$e, $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };
    (@binding_entry $list:ident, $modif:expr, $s:literal.. $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from(($s.., $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };
    (@binding_entry $list:ident, $modif:expr, $elem:literal $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from(($elem, $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };
    (@binding_entry $list:ident, $modif:expr, $elem:literal $(| $($rest:tt)*)?) => {
        $list.push($crate::mode::Binding::from(($elem, $modif)));
        $crate::__modified__!(@binding_entry $list, $modif, $($($rest)*)?);
    };

    (@two_dots_entry $list:ident, $modif:expr, $path:ident $(::$rest:ident)+) => {{
        $crate::__modified__!(@two_dots_entry $list, $modif, $($rest)::+);
    }};
    (@two_dots_entry $list:ident, $modif:expr, Char) => {{
        $list.push($crate::mode::Binding::CharRange(
            ::std::ops::Bound::Unbounded,
            ::std::ops::Bound::Unbounded,
            $modif
        ))
    }};
    (@two_dots_entry $list:ident, $modif:expr, F) => {{
        $list.push($crate::mode::Binding::FnRange(
            ::std::ops::Bound::Unbounded,
            ::std::ops::Bound::Unbounded,
            $modif
        ))
    }};
    (@two_dots_entry $list:ident, $modif:expr, Media) => {{
        $list.push($crate::mode::Binding::AnyMedia($modif))
    }};
    (@two_dots_entry $list:ident, $modif:expr, Modifier) => {{
        $list.push($crate::mode::Binding::AnyModifier($modif))
    }}
}

/// A simple macro to join [`KeyMod`]s into a `const` `KeyMod` that
/// can be used in pattern matching
#[macro_export]
#[doc(hidden)]
macro_rules! __join_modifiers__ {
    [] => { $crate::mode::KeyMod::NONE };
    [ALT] => { $crate::mode::KeyMod::ALT };
    [CONTROL] => { $crate::mode::KeyMod::CONTROL };
    [SHIFT] => { $crate::mode::KeyMod::SHIFT };
    [ALT, CONTROL] => { $crate::mode::ALT_CONTROL };
    [CONTROL, ALT] => { $crate::mode::ALT_CONTROL };
    [ALT, SHIFT] => { $crate::mode::ALT_SHIFT };
    [SHIFT, ALT] => { $crate::mode::ALT_SHIFT };
    [CONTROL, SHIFT] => { $crate::mode::CONTROL_SHIFT };
    [SHIFT, CONTROL] => { $crate::mode::CONTROL_SHIFT };
    [ALT, CONTROL, SHIFT] => { $crate::mode::ALT_CONTROL_SHIFT };
    [ALT, SHIFT, CONTROL] => { $crate::mode::ALT_CONTROL_SHIFT };
    [CONTROL, ALT, SHIFT] => { $crate::mode::ALT_CONTROL_SHIFT };
    [CONTROL, SHIFT, ALT] => { $crate::mode::ALT_CONTROL_SHIFT };
    [SHIFT, ALT, CONTROL] => { $crate::mode::ALT_CONTROL_SHIFT };
    [SHIFT, CONTROL, ALT] => { $crate::mode::ALT_CONTROL_SHIFT };
    [$($other:tt)+] => {
        compile_error!("Don't nest identical modifier macros while forming patterns")
    }
}

/// `const` [`KeyMod`] used in pattern matching
#[doc(hidden)]
pub const ALT_CONTROL: KeyMod = KeyMod::ALT.union(KeyMod::CONTROL);

/// `const` [`KeyMod`] used in pattern matching
#[doc(hidden)]
pub const ALT_SHIFT: KeyMod = KeyMod::ALT.union(KeyMod::SHIFT);

/// `const` [`KeyMod`] used in pattern matching
#[doc(hidden)]
pub const CONTROL_SHIFT: KeyMod = KeyMod::CONTROL.union(KeyMod::SHIFT);

/// `const` [`KeyMod`] used in pattern matching
#[doc(hidden)]
pub const ALT_CONTROL_SHIFT: KeyMod = KeyMod::ALT.union(KeyMod::CONTROL).union(KeyMod::SHIFT);
