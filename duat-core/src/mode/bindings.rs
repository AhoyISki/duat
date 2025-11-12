use std::sync::Arc;

use crossterm::event::KeyEvent;

pub use crate::__bindings__ as bindings;
use crate::text::Text;

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
    pub descriptions: Vec<BindingDescription>,
    /// Followup `Bindings` for further matching
    ///
    /// Direct implementation is not recommended, use the
    /// [`bindings!`] macro instead.
    #[doc(hidden)]
    pub followups: Vec<Option<Bindings>>,
}

/// A description for a key bindings, this is used to show which key
/// does what
#[derive(Debug, Clone)]
pub enum BindingDescription {
    /// A simple description, with no key
    ///
    /// This should be used for "catch-all" keys, for example, the key
    /// that follows `f` or `t` on vim.
    Simple(Text),
    /// The description for a key pattern
    ///
    /// This should be used whenever any key does something specific,
    /// i.e., in every situation where [`BindingDescription::Simple`]
    /// doesn't make sense.
    WithKey(Text, Text),
}

#[macro_export]
#[doc(hidden)]
macro_rules! __bindings__ {
    (@match_entry $index:ident, $key_event:ident:) => {};
    (@match_entry
        $index:ident,
        $key_event:ident: $modif:ident$excl:tt($($tokens:tt)*) => $result:tt
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
    (@matcher $key_event:ident { $($patterns:tt)+ }) => {{
        #[allow(unused_assignments, irrefutable_let_patterns)]
        std::sync::Arc::new(move |$key_event: $crate::mode::KeyEvent| {
            let mut index = 0;
            $crate::mode::bindings!(@match_entry index, $key_event: $($patterns)+);

            None
        })
    }};

    (@description_entry $list:ident:) => {};
    (@description_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => ([$text1:expr, $text2:expr], $($matcher:tt)+)
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::WithKey(
            $crate::text::Text::from($text1),
            $crate::text::Text::from($text2),
        ));
		$crate::mode::bindings!(@description_entry $list: $($($rest)*)?);
    };
    (@description_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => ($text:expr, $($matcher:tt)+)
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::Simple($crate::text::Text::from($text)));
		$crate::mode::bindings!(@description_entry $list: $(,$($rest)*)?);
    };
    (@description_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => [$text1:expr, $text2:expr]
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::WithKey(
            $crate::text::Text::from($text1),
            $crate::text::Text::from($text2),
        ));
		$crate::mode::bindings!(@description_entry $list: $($($rest)*)?);
    };
    (@description_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => $text:expr
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::Simple($crate::text::Text::from($text)));
		$crate::mode::bindings!(@description_entry $list: $($($rest)*)?);
    };
    (@description_entry
        $list:ident:
        $pattern:pat => ([$text1:expr, $text2:expr], $($matcher:tt)+)
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::WithKey(
            $crate::text::Text::from($text1),
            $crate::text::Text::from($text2),
        ));
		$crate::mode::bindings!(@description_entry $list: $(,$($rest)*)?);
    };
    (@description_entry
        $list:ident:
        $pattern:pat => ($text:expr, $($matcher:tt)+)
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::Simple($crate::text::Text::from($text)));
		$crate::mode::bindings!(@description_entry $list: $(,$($rest)*)?);
    };
    (@description_entry
        $list:ident:
        $pattern:pat => [$text1:expr, $text2:expr]
        $(,$($rest:tt)*)?
    ) => {
        $list.push($crate::mode::BindingDescription::WithKey(
            $crate::text::Text::from($text1),
            $crate::text::Text::from($text2),
        ))
		$crate::mode::bindings!(@description_entry $list: $(,$($rest)*)?);
    };
    (@description_entry $list:ident: $pattern:pat => $text:expr $(,$($rest:tt)*)?) => {
        $list.push($crate::mode::BindingDescription::Simple($crate::text::Text::from($text)));
    };
    (@descriptions { $($patterns:tt)+ }) => {{
        let mut list = Vec::new();
		$crate::mode::bindings!(@description_entry list: $($patterns)+);
		list
    }};

    (@followup_entry $list:ident:) => {};
    (@followup_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => ($texts:expr, match $key_event:ident $match:tt)
        $(,$($rest:tt)*)?
    ) => {
    	$list.push(Some($crate::mode::bindings! { match $key_event $match }));
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
	};
    (@followup_entry
        $list:ident:
        $pattern:pat => ($texts:expr, match $key_event:ident $match:tt)
        $(,$($rest:tt)*)?
    ) => {
    	$list.push(Some($crate::mode::bindings! { match $key_event $match }));
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
	};
    (@followup_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => ($texts:expr, $bindings:expr)
        $(,$($rest:tt)*)?
    ) => {
    	$list.push(Some($bindings));
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
	};
    (@followup_entry
        $list:ident:
        $pattern:pat => ($texts:expr, $bindings:expr)
        $(,$($rest:tt)*)?
    ) => {
    	$list.push(Some($bindings));
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
	};
    (@followup_entry
        $list:ident:
        $modif:ident$excl:tt($($tokens:tt)*) => $texts:expr
        $(,$($rest:tt)*)?
    ) => {
    	$list.push(None);
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
	};
	(@followup_entry $list:ident: $pattern:pat => $texts:expr $(,$($rest:tt)*)?) => {
    	$list.push(None);
        $crate::mode::bindings!(@followup_entry $list: $($($rest)*)?);
	};
    (@followups { $($patterns:tt)+ }) => {{
        let mut list = Vec::new();
        $crate::mode::bindings!(@followup_entry list: $($patterns)+);
        list
    }};

    (match $key_event:ident $match:tt) => {{
        #[allow(clippy::vec_init_then_push)]
        let matcher = $crate::mode::bindings!(@matcher $key_event $match);
        #[allow(clippy::vec_init_then_push)]
        let descriptions = $crate::mode::bindings!(@descriptions $match);
        #[allow(clippy::vec_init_then_push)]
        let followups = $crate::mode::bindings!(@followups $match);

        $crate::mode::Bindings {
            matcher,
            descriptions,
            followups
        }
    }};
}
