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
use std::{ops::RangeInclusive, sync::LazyLock};

use regex_cursor::regex_automata::meta::Regex;

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

/// Which characters should count as "word" characters
///
/// This can be useful for many things, such as deciding when to wrap
/// given [`WrapMethod::Word`], or how many characters should be
/// included in certain Vim/Neovim/Kakoune/Helix movements.
#[derive(Clone, Copy, Debug)]
pub struct WordChars(pub &'static LazyLock<(Regex, &'static [RangeInclusive<char>])>);

impl WordChars {
    /// The default [`WordChars`]
    ///
    /// Is equivalent to the regex character class `[A-Za-z0-9_]`.
    pub const fn default() -> Self {
        word_chars!("A-Za-z0-9_-_")
    }

    /// Checks if a [`char`] is a word char
    #[inline]
    pub fn contains(&self, char: char) -> bool {
        let mut bytes = [0; 4];
        let str = char.encode_utf8(&mut bytes);

        self.0.0.is_match(str as &str)
    }

    /// The ranges of [`char`]s that are included as "word" chars
    pub fn ranges(&self) -> &'static [RangeInclusive<char>] {
        self.0.1
    }
}

impl std::hash::Hash for WordChars {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.1.hash(state);
    }
}

impl std::cmp::PartialEq for WordChars {
    fn eq(&self, other: &Self) -> bool {
        self.0.1 == other.0.1
    }
}

impl std::cmp::Eq for WordChars {}

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
    /// Characters that are considered to be part of a word
    ///
    /// The default is [`word_chars!("A-Za-z0-9_-_")`].
    ///
    /// Which characters should be considered "word characters". This
    /// matters not only for [`WrapMethod::Word`], as you'd expect,
    /// but also for certain control schemes, that make distinctions
    /// between `word` (word characters) and `WORD` (non whitespace
    /// characters).
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`word_chars!("A-Za-z0-9_-_")`]: word_chars
    pub word_chars: WordChars,
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
    ///     word_chars: word_chars!("A-Za-z0-9_-_"),
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
            word_chars: WordChars::default(),
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
    ///     word_chars: word_chars!("A-Za-z0-9_-_"),
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
            word_chars: WordChars::default(),
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
}

impl Default for PrintOpts {
    fn default() -> Self {
        Self::new()
    }
}

/// Returns a new [`WordChars`] composed of inclusive ranges of
/// [`char`]s
///
/// The syntax is as follows:
///
/// ```rust
/// # use duat_core::opts::word_chars;
/// let word_chars = word_chars!("a-zA-Z0-9---_-_");
/// ```
///
/// That is, it must be a list of inclusive ranges, denoted by
/// `{char1}-{char2}`. Any character is valid, as long as a `' '`,
/// `'\t'`, or `'\n'` is not part of the range, and `char2 >= char1`.
///
/// This is all evaluated at compile time, so you don't have to worry
/// about it.
pub macro word_chars($ranges:literal) {{
    const { validate_ranges($ranges) };
    static MATCHERS: LazyLock<(Regex, &'static [RangeInclusive<char>])> = LazyLock::new(|| {
        let regex = $crate::opts::escaped_regex($ranges);
        let regex = Regex::new(&format!("[{regex}]")).unwrap();
        (regex, $crate::opts::vec_from_ranges($ranges).leak())
    });
    WordChars(&MATCHERS)
}}

/// Creates a vector from a ranges `&str`
///
/// Assumes that [`validate_ranges`] has verified its formatting.
#[doc(hidden)]
pub fn vec_from_ranges(ranges: &str) -> Vec<RangeInclusive<char>> {
    ranges
        .chars()
        .array_chunks::<3>()
        .map(|[char1, _dash, char2]| char1..=char2)
        .collect()
}

/// Escapes regex characters
#[doc(hidden)]
pub fn escaped_regex(ranges: &str) -> String {
    let mut escaped = String::new();
    for char in ranges.chars() {
        if let '(' | ')' | '{' | '}' | '[' | ']' | '$' | '^' | '.' | '*' | '+' | '?' | '|' = char {
            escaped.push('\\');
        }
        escaped.push(char);
    }
    escaped
}

/// Validates the ranges of a `&str`
#[doc(hidden)]
pub const fn validate_ranges(mut ranges: &str) {
    if ranges.is_empty() {
        return;
    }

    while !ranges.is_empty() {
        let mut split = 1;
        // Move the split until it is between the first character and a '-'.
        while !ranges.is_char_boundary(split) {
            split += 1;
        }

        let (start, remainder) = ranges.split_at(split);
        let (dash, remainder) = remainder.split_at(1);

        assert!(
            first_char(dash) == '-',
            "ranges must be formatted like {{char1}}-{{char2}}"
        );

        let start = first_char(start);
        let end = first_char(remainder);

        assert!(
            start <= end,
            "the end of the range must be bigger than the start"
        );
        assert!(!(start <= ' ' && ' ' <= end), "range contains ' '");
        assert!(!(start <= '\n' && '\n' <= end), "range contains '\\n'");
        assert!(!(start <= '\t' && '\t' <= end), "range contains '\\t'");

        let (_, remainder) = remainder.split_at(end.len_utf8());
        ranges = remainder;
    }
}

const fn first_char(str: &str) -> char {
    let bytes = str.as_bytes();

    let x = bytes[0];
    if bytes[0] < 128 {
        return unsafe { char::from_u32_unchecked(x as u32) };
    }

    let init = utf8_first_byte(x, 2);
    let y = bytes[1];
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xe0 {
        let z = bytes[2];
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xf0 {
            let w = bytes[3];
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    unsafe { char::from_u32_unchecked(ch) }
}

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4
/// bits for width 3, and 3 bits for width 4.
#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7f >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Mask of the value bits of a continuation byte.
const CONT_MASK: u8 = 0b0011_1111;
