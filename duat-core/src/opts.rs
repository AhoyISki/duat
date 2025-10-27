//! General printing options for printing [`Buffer`]s
//!
//! This is essentially the things that you'd expect to change in a
//! text editor such as Neovim or Kakoune. They are contained in a
//! [`PrintOpts`] struct, which is very light and cheap to copy
//! around, and is used not only by the [`Buffer`], but by every other
//! [`Widget`] as well. Right now, these options are:
//!
//! - [`WrapMethod`]: How to wrap lines;
//! - [`TabStops`]: How many spaces a tab should occupy;
//! - [`NewLine`]: What to put in place of a `'\n'`, potentially only
//!   after trailing whitespace;
//! - [`ScrollOff`]: How many cells to keep between the main cursor
//!   and the edges of [`Area`]s;
//! - [`WordChars`]: Which [`char`]s are considered part of a word;
//! - [forced_scrolloff]: Keeps the scrolloff distance, preventing the
//!   main cursor from reaching the right edge;
//!
//! If you have any other recomendations of printing options that are
//! common, feel free to recomend them!
//!
//! [`Buffer`]: crate::buffer::Buffer
//! [`Widget`]: crate::ui::Widget
//! [`Area`]: crate::ui::Ui::Area
//! [forced_scrolloff]: PrintOpts::set_forced_horizontal_scrolloff
//! [show_ghosts]: PrintOpts::show_ghosts
use std::{collections::HashMap, sync::LazyLock};

use parking_lot::Mutex;

use crate::text::Matcheable;

/// The distance to keep between the [`Cursor`] and the edges of the
/// screen when scrolling
///
/// [`Cursor`]: crate::mode::Cursor
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScrollOff {
    /// The horizontal scrolloff
    pub x: u8,
    /// The vertical scrolloff
    pub y: u8,
}

/// Configuration options for printing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PrintOpts {
    /// Disable wrapping entirely
    ///
    /// The default is `true`
    pub dont_wrap: bool = true,
    /// Wrap on word boundaries, rather than on any character
    ///
    /// The default is `false`.
    pub wrap_on_word: bool = false,
    /// Where to start wrapping
    ///
    /// The default is `None`
    ///
    /// If this value is `None` and `opts.dont_wrap == false`, then
    /// wrapping will take place at the right edge of the screen.
    ///
    /// Otherwise, if it is `Some({cap})`, then wrapping will take
    /// place `{cap}` cells from the left edge. This value may or may
    /// not be greater than the width of the area. If it is greater
    /// than it, then wrapping will take place slightly outside the
    /// screen as a concequence.
    pub wrapping_cap: Option<u32> = None,
    /// Whether to indent wrapped lines or not
    ///
    /// In [`Buffer`]s, the default is `true`.
    ///
    /// This turns this:
    ///
    /// ```text
    ///     This is a very long line of text, so long that it
    /// wraps around
    /// ```
    ///
    /// Into this:
    ///
    /// ```text
    ///     This is a very long line of text, so long that it
    ///     wraps around
    /// ```
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub indent_wraps: bool = true,
    /// How long tabs should be on screen
    ///
    /// In [`Buffer`]s, the default is `4`
    ///
    /// This also affect other things, like if your tabs are converted
    /// into spaces, this will also set how many spaces should be
    /// added.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub tabstop: u8 = 4,
    /// Wether to print the `'\n'` character as an empty space (`' '`)
    ///
    /// In [`Buffer`]s, the default is `true`
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub print_new_line: bool,
    /// How much space to keep between the cursor and edges
    ///
    /// In [`Buffer`]s, the default is `ScrollOff { x: 3, y: 3 }`
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub scrolloff: ScrollOff,
    /// Whether to limit scrolloff at the end of lines
    ///
    /// In [`Buffer`]s, the default is `false`
    ///
    /// This makes it so, as you reach the end of a long line of text,
    /// the cursor line will continue scrolling to the left,
    /// maintaining the `scrolloff.x`'s gap.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub force_scrolloff: bool,
    /// Extra characters to be considered part of a word
    ///
    /// The default is `&[]`.
    ///
    /// Normally, word characters include all of those in the [`\w`]
    /// character set, which most importantly includes `[0-9A-Za-z_]`.
    ///
    /// You can use this setting to add more characters to that list,
    /// usually something like `-`, `$` or `@`, which are useful to
    /// consider as word characters in some circumstances.
    ///
    /// [`\w`]: https://www.unicode.org/reports/tr18/#word
    pub extra_word_chars: &'static [char],
    /// Whether to show [ghoxt text]
    ///
    /// In [`Buffer`]s, the default is `true`
    ///
    /// This is just a switch to decide if you want ghosts or not.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [ghoxt text]: crate::text::Ghost
    pub show_ghosts: bool,
    /// Wether to allow the [`Text`] to scroll until only
    /// `scrolloff.y` line are on screen
    ///
    /// In [`Buffer`]s, the default is `true`
    ///
    /// If you disable this, when your cursor reaches the end of the
    /// text, if try to scroll the text down, nothing will happen.
    /// Otherwise, the text will continue scrolling down until there
    /// are only `scrolloff.y` lines visible on screen.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`Text`]: crate::text::Text
    pub allow_overscroll: bool,
}

impl PrintOpts {
    /// The default [`PrintOpts`]
    ///
    /// There is, essentially, almost no reason to deviate from this
    /// in any [`Widget`] other than a [`Buffer`], since those most
    /// likely will only be printed with the [default `PrintInfo`]
    /// from a [`Area`], i.e., no scrolling is involved, and you
    /// should usually strive to control the other elements of
    /// [`Text`]s that the options of a [`PrintOpts`] will want to
    /// change.
    ///
    /// The lack of need to customize this is reflected in
    /// [`Widget::print_opts`], which calls this function by default.
    /// However, in a [`Buffer`], you'll probably want to look at the
    /// options below.
    ///
    /// The default value is:
    ///
    /// ```rust
    /// use duat_core::opts::*;
    /// PrintOpts {
    ///     dont_wrap: true,
    ///     wrap_on_word: false,
    ///     wrapping_cap: None,
    ///     indent_wrapped: true,
    ///     tab_stops: 4,
    ///     new_line: NewLine::AlwaysAs('\n'),
    ///     scrolloff: ScrollOff { x: 3, y: 3 },
    ///     extra_word_chars: &[],
    ///     force_scrolloff: false,
    ///     show_ghosts: true,
    ///     allow_overscroll: false,
    /// };
    /// ```
    ///
    /// [`Widget`]: crate::ui::Widget
    /// [`Buffer`]: crate::buffer::Buffer
    /// [default `PrintInfo`]: crate::ui::Area::PrintInfo
    /// [`Area`]: crate::ui::Area
    /// [`Text`]: crate::text::Text
    /// [`Widget::print_opts`]: crate::ui::Widget::print_opts
    pub const fn new() -> Self {
        Self {
            dont_wrap: true,
            wrap_on_word: false,
            wrapping_cap: None,
            indent_wraps: true,
            tabstop: 4,
            print_new_line: false,
            scrolloff: ScrollOff { x: 3, y: 3 },
            extra_word_chars: &[],
            force_scrolloff: false,
            show_ghosts: true,
            allow_overscroll: false,
        }
    }

    /// The default used in buffers and other such inputs
    ///
    /// This different default exists because on [`Widget`]s with
    /// [`Selection`]s, some extra considerations need to be taken
    /// into account, like new lines needing to be printed in order
    /// for the `Selection` to visually occupy a `\n` character.
    ///
    /// The default value is:
    ///
    /// ```rust
    /// use duat_core::opts::*;
    /// PrintOpts {
    ///     dont_wrap: true,
    ///     wrap_on_word: false,
    ///     wrapping_cap: None,
    ///     indent_wrapped: true,
    ///     tab_stops: 4,
    ///     print_new_line: true,
    ///     scrolloff: ScrollOff { x: 3, y: 3 },
    ///     extra_word_chars: &[],
    ///     force_scrolloff: false,
    ///     show_ghosts: true,
    ///     allow_overscroll: false,
    /// };
    /// ```
    ///
    /// [`Widget`]: crate::ui::Widget
    /// [`Selection`]: crate::mode::Selection
    pub const fn default_for_input() -> Self {
        Self {
            dont_wrap: true,
            wrap_on_word: false,
            wrapping_cap: None,
            indent_wraps: true,
            tabstop: 4,
            print_new_line: true,
            scrolloff: ScrollOff { x: 3, y: 3 },
            extra_word_chars: &[],
            force_scrolloff: false,
            show_ghosts: true,
            allow_overscroll: true,
        }
    }

    ////////// Queries

    /// What the wrap width should be, given an area of a certain
    /// width
    #[inline]
    pub const fn wrap_width(&self, width: u32) -> Option<u32> {
        if self.dont_wrap {
            None
        } else if let Some(cap) = self.wrapping_cap {
            Some(cap)
        } else {
            Some(width)
        }
    }

    /// How many spaces should come at a given x position
    #[inline]
    pub fn tabstop_spaces_at(&self, x: u32) -> u32 {
        self.tabstop as u32 - (x % self.tabstop as u32)
    }

    /// Gets a `&str` that matches every character that should be part
    /// of a word
    ///
    /// This will come in the form of the regex `[\w{other chars}]`.
    pub fn word_chars_regex(&self) -> &'static str {
        PATTERNS
            .lock()
            .entry(self.extra_word_chars)
            .or_insert_with(|| {
                format!(
                    r"[\w{}]",
                    escape_special(self.extra_word_chars.iter().collect()),
                )
                .leak()
            })
    }

    /// Wether a given `char` is considered a word character
    pub fn is_word_char(&self, char: char) -> bool {
        let pat = *PATTERNS
            .lock()
            .entry(self.extra_word_chars)
            .or_insert_with(|| {
                format!(
                    r"[\w{}]",
                    escape_special(self.extra_word_chars.iter().collect()),
                )
                .leak()
            });

        let mut bytes = [b'\0'; 4];
        let str = char.encode_utf8(&mut bytes);
        str.reg_matches(pat, ..).unwrap()
    }
}

impl Default for PrintOpts {
    fn default() -> Self {
        Self::new()
    }
}

/// Escapes regex characters
#[doc(hidden)]
pub fn escape_special(mut regex: String) -> String {
    for (i, char) in regex.char_indices().collect::<Vec<_>>() {
        if let '(' | ')' | '{' | '}' | '[' | ']' | '$' | '^' | '.' | '*' | '+' | '?' | '|' = char {
            regex.insert(i, '\\');
        }
    }
    regex
}

static PATTERNS: LazyLock<Mutex<HashMap<&[char], &'static str>>> = LazyLock::new(|| {
    let mut map: HashMap<&[char], _> = HashMap::new();
    map.insert(&[], r"[\w]");
    Mutex::new(map)
});
