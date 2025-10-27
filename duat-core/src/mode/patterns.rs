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
/// [`KeyMod`]s, and can be nested. This macro _cannot_ be nested with
/// them however, for obvious reasons.
///
/// [`KeyEvent`]: super::KeyEvent
/// [`Mode`]: super::Mode
pub macro event {
    ($($chars:literal)|+) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($($chars)|+),
            modifiers: $crate::mode::KeyMod::NONE,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // Straight up useless tbh.
    ($char:ident @ $chars:literal) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ $chars),
            modifiers: $crate::mode::KeyMod::NONE,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ ($($chars)|+)),
            modifiers: $crate::mode::KeyMod::NONE,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($code:pat) => {
        $crate::mode::KeyEvent {
            code: $code,
            modifiers: $crate::mode::KeyMod::NONE,
            ..
        }
    }
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
pub macro alt {
    ($($chars:literal)|+) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($($chars)|+),
            modifiers: $crate::mode::KeyMod::ALT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // Straight up useless tbh.
    ($char:ident @ $chars:literal) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ $chars),
            modifiers: $crate::mode::KeyMod::ALT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ ($($chars)|+)),
            modifiers: $crate::mode::KeyMod::ALT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // I use $excl here in order to make it look like part of the macro,
    // so rust analyzer properly highlights it.
    ($modif:ident$excl:tt($($tokens:tt)+)) => {
        $crate::mode::KeyEvent {
            code: $modif$excl(@code $($tokens)+),
            modifiers: $modif$excl(@modif [ALT] $($tokens)+),
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($code:pat) => {
        $crate::mode::KeyEvent {
            code: $code,
            modifiers: $crate::mode::KeyMod::ALT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },

    (@code $modif:ident$excl:tt($($tokens:tt)+)) => {
        $modif$excl(@code $($tokens)+)
    },
    (@code $($chars:literal)|+) => {
        $crate::mode::KeyCode::Char($($chars)|+)
    },
    (@code $char:ident @ $chars:literal) => {
        $crate::mode::KeyCode::Char($char @ $chars)
    },
    (@code $char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyCode::Char($char @ ($($chars)|+))
    },
    (@code $code:pat) => {
        $code
    },

    (@modif [$($list:ident),+] $modif:ident$excl:tt($($tokens:tt)+)) => {
        $modif$excl(@modif [ALT, $($list),+] $($tokens)+)
    },
    (@modif [$($list:ident),+] $($other:tt)+) => {
        $crate::mode::join_modifiers![ALT, $($list),+]
    },
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
pub macro ctrl {
    ($($chars:literal)|+) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($($chars)|+),
            modifiers: $crate::mode::KeyMod::CONTROL,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // Straight up useless tbh.
    ($char:ident @ $chars:literal) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ $chars),
            modifiers: $crate::mode::KeyMod::CONTROL,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ ($($chars)|+)),
            modifiers: $crate::mode::KeyMod::CONTROL,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // I use $excl here in order to make it look like part of the macro,
    // so rust analyzer properly highlights it.
    ($modif:ident$excl:tt($($tokens:tt)+)) => {
        $crate::mode::KeyEvent {
            code: $modif$excl(@code $($tokens)+),
            modifiers: $modif$excl(@modif [CONTROL] $($tokens)+),
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($code:pat) => {
        $crate::mode::KeyEvent {
            code: $code,
            modifiers: $crate::mode::KeyMod::CONTROL,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },

    (@code $modif:ident$excl:tt($($tokens:tt)+)) => {
        $modif$excl(@code $($tokens)+)
    },
    (@code $($chars:literal)|+) => {
        $crate::mode::KeyCode::Char($($chars)|+)
    },
    (@code $char:ident @ $chars:literal) => {
        $crate::mode::KeyCode::Char($char @ $chars)
    },
    (@code $char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyCode::Char($char @ ($($chars)|+))
    },
    (@code $code:pat) => {
        $code
    },

    (@modif [$($list:ident),+] $modif:ident$excl:tt($($tokens:tt)+)) => {
        $modif$excl(@modif [CONTROL, $($list),+] $($tokens)+)
    },
    (@modif [$($list:ident),+] $($other:tt)+) => {
        $crate::mode::join_modifiers![CONTROL, $($list),+]
    },
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
pub macro shift {
    ($($chars:literal)|+) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($($chars)|+),
            modifiers: $crate::mode::KeyMod::SHIFT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // Straight up useless tbh.
    ($char:ident @ $chars:literal) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ $chars),
            modifiers: $crate::mode::KeyMod::SHIFT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyEvent {
            code: $crate::mode::KeyCode::Char($char @ ($($chars)|+)),
            modifiers: $crate::mode::KeyMod::SHIFT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    // I use $excl here in order to make it look like part of the macro,
    // so rust analyzer properly highlights it.
    ($modif:ident$excl:tt($($tokens:tt)+)) => {
        $crate::mode::KeyEvent {
            code: $modif$excl(@code $($tokens)+),
            modifiers: $modif$excl(@modif [SHIFT] $($tokens)+),
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },
    ($code:pat) => {
        $crate::mode::KeyEvent {
            code: $code,
            modifiers: $crate::mode::KeyMod::SHIFT,
            kind: $crate::mode::KeyEventKind::Press | $crate::mode::KeyEventKind::Repeat, ..
        }
    },

    (@code $modif:ident$excl:tt($($tokens:tt)+)) => {
        $modif$excl(@code $($tokens)+)
    },
    (@code $($chars:literal)|+) => {
        $crate::mode::KeyCode::Char($($chars)|+)
    },
    (@code $char:ident @ $chars:literal) => {
        $crate::mode::KeyCode::Char($char @ $chars)
    },
    (@code $char:ident @ ($($chars:literal)|+)) => {
        $crate::mode::KeyCode::Char($char @ ($($chars)|+))
    },
    (@code $code:pat) => {
        $code
    },

    (@modif [$($list:ident),+] $modif:ident$excl:tt($($tokens:tt)+)) => {
        $modif$excl(@modif [SHIFT, $($list),+] $($tokens)+)
    },
    (@modif [$($list:ident),+] $($other:tt)+) => {
        $crate::mode::join_modifiers![SHIFT, $($list),+]
    },
}

/// A simple macro to join [`KeyMod`]s into a `const` `KeyMod` that
/// can be used in pattern matching
pub macro join_modifiers {
    [ALT, CONTROL] => { $crate::mode::ALT_CONTROL },
    [CONTROL, ALT] => { $crate::mode::ALT_CONTROL },
    [ALT, SHIFT] => { $crate::mode::ALT_SHIFT },
    [SHIFT, ALT] => { $crate::mode::ALT_SHIFT },
    [CONTROL, SHIFT] => { $crate::mode::CONTROL_SHIFT },
    [SHIFT, CONTROL] => { $crate::mode::CONTROL_SHIFT },
    [ALT, CONTROL, SHIFTl] => { $crate::mode::ALT_CONTROL_SHIFT },
    [ALT, SHIFT, CONTROLl] => { $crate::mode::ALT_CONTROL_SHIFT },
    [CONTROL, ALT, SHIFTl] => { $crate::mode::ALT_CONTROL_SHIFT },
    [CONTROL, SHIFT, ALTl] => { $crate::mode::ALT_CONTROL_SHIFT },
    [SHIFT, ALT, CONTROLl] => { $crate::mode::ALT_CONTROL_SHIFT },
    [SHIFT, CONTROL, ALTl] => { $crate::mode::ALT_CONTROL_SHIFT },
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
