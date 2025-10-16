//! General printing options for printing [`Buffer`]s
//!
//! This is essentially the things that you'd expect to change in a
//! text editor such as Neovim or Kakoune. They are contained in a
//! [`PrintOpts`] struct, which is very light and cheap to copy around,
//! and is used not only by the [`Buffer`], but by every other
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

/// If and how to wrap lines at the end of the screen.
#[derive(Clone, Copy, Debug)]
pub enum WrapMethod {
    /// Wrap on the edge of the screen
    Edge,
    /// Wraps after a certain number of cells.
    ///
    /// Note that this can be a number greater than the width, which
    /// will wrap outside of the screen, sort of like a mix of
    /// [`WrapMethod::Edge`] and [`WrapMethod::NoWrap`].
    Capped(u8),
    /// Wraps on [word] terminations
    ///
    /// [word]: word_chars
    Word,
    /// No wrapping
    NoWrap,
}

impl WrapMethod {
    /// Returns `true` if the wrap method is [`NoWrap`].
    ///
    /// [`NoWrap`]: WrapMethod::NoWrap
    #[must_use]
    pub fn is_no_wrap(&self) -> bool {
        matches!(self, Self::NoWrap)
    }

    /// What the cap should be, given a certain [`Area`] width
    ///
    /// [`Area`]: crate::ui::Area
    pub fn cap(&self, width: usize) -> usize {
        match self {
            WrapMethod::Capped(cap) => *cap as usize,
            _ => width,
        }
    }
}

/// Where the tabs are placed on screen
#[derive(Clone, Copy, Debug)]
pub struct TabStops(pub u8);

impl TabStops {
    /// How many spaces to put in place of a tab
    pub fn size(&self) -> u32 {
        self.0 as u32
    }

    /// How many spaces should come at a given x position
    #[inline]
    pub fn spaces_at(&self, x: u32) -> u32 {
        self.0 as u32 - (x % self.0 as u32)
    }
}

impl Default for TabStops {
    fn default() -> Self {
        TabStops(4)
    }
}

/// Whether to show the new line or not.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NewLine {
    /// Show the given character on every new line.
    AlwaysAs(char),
    /// Show the given character only when there is whitespace at end
    /// of the line.
    AfterSpaceAs(char),
}

impl NewLine {
    /// Given the previous character, which character should show up
    #[inline]
    pub fn char(&self, last_char: Option<char>) -> char {
        match *self {
            NewLine::AlwaysAs(char) => char,
            NewLine::AfterSpaceAs(char) => {
                if last_char.is_some_and(|lc| lc.is_whitespace() && lc != '\n') {
                    char
                } else {
                    ' '
                }
            }
        }
    }
}

/// The distance to keep between the [`Cursor`] and the edges of the
/// screen when scrolling
///
/// [`Cursor`]: crate::mode::Cursor
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
pub struct PrintOpts {
    /// How to wrap the file
    pub wrap_method: WrapMethod,
    /// Whether to indent wrapped lines or not
    pub indent_wrapped: bool,
    /// Which places are considered a "tab stop"
    pub tab_stops: TabStops,
    /// Whether (and how) to show new lines
    pub new_line: NewLine,
    /// How much space to keep between the cursor and edges
    pub scrolloff: ScrollOff,
    /// Whether to limit scrolloff on the end of lines
    pub force_scrolloff: bool,
    /// Characters that are considered to be part of a word
    pub word_chars: WordChars,
    /// Whether to show ghosts
    pub show_ghosts: bool,
    /// Wether to allow the [`Text`] to scroll until only
    /// `scrolloff.y` line are on screen
    ///
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
    /// [`Widget::print_cfg`], which calls this function by default.
    /// However, in a [`Buffer`], you'll probably want to look at the
    /// options below.
    ///
    /// The default value is:
    ///
    /// ```rust
    /// use duat_core::opts::*;
    /// PrintOpts {
    ///     wrap_method: WrapMethod::Edge,
    ///     indent_wrapped: true,
    ///     tab_stops: TabStops(4),
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
    /// [`Widget::print_cfg`]: crate::ui::Widget::print_cfg
    pub const fn new() -> Self {
        Self {
            wrap_method: WrapMethod::Edge,
            indent_wrapped: true,
            tab_stops: TabStops(4),
            new_line: NewLine::AlwaysAs('\n'),
            scrolloff: ScrollOff { x: 3, y: 3 },
            word_chars: WordChars::default(),
            force_scrolloff: false,
            show_ghosts: true,
            allow_overscroll: false,
        }
    }

    ////////// Configuration

    /// Don't wrap when reaching the end of the area
    pub const fn dont_wrap(&mut self) -> &mut Self {
        self.wrap_method = WrapMethod::NoWrap;
        self
    }

    /// Wrap on the right edge of the area
    pub const fn wrap_on_edge(&mut self) -> &mut Self {
        self.wrap_method = WrapMethod::Edge;
        self
    }

    /// Wrap on [word] terminations
    ///
    /// [word]: word_chars
    pub const fn wrap_on_word(&mut self) -> &mut Self {
        self.wrap_method = WrapMethod::Word;
        self
    }

    /// Wrap on a given distance from the left edge
    ///
    /// This can wrap beyond the screen, being a mix of [`dont_wrap`]
    /// and [`wrap_on_edge`].
    ///
    /// [`dont_wrap`]: Self::dont_wrap
    /// [`wrap_on_edge`]: Self::wrap_on_edge
    pub const fn wrap_at(&mut self, cap: u8) -> &mut Self {
        self.wrap_method = WrapMethod::Capped(cap);
        self
    }

    /// Reindent wrapped lines to the same level of indentation
    pub const fn indent_wraps(&mut self, value: bool) -> &mut Self {
        self.indent_wrapped = value;
        self
    }

    /// Sets the size of tabs
    pub const fn set_tabstop(&mut self, tabstop: u8) -> &mut Self {
        self.tab_stops = TabStops(tabstop);
        self
    }

    /// Sets a character to replace `'\n'`s with
    pub const fn new_line_as(&mut self, char: char) -> &mut Self {
        self.new_line = NewLine::AlwaysAs(char);
        self
    }

    /// Sets a character to replace `'\n'` only with trailing white
    /// space
    pub const fn trailing_new_line_as(&mut self, char: char) -> &mut Self {
        self.new_line = NewLine::AfterSpaceAs(char);
        self
    }

    /// Sets the horizontal and vertical scrolloff, respectively
    pub const fn set_scrolloff(&mut self, x: u8, y: u8) -> &mut Self {
        self.scrolloff = ScrollOff { x, y };
        self
    }

    /// Sets the horizontal scrolloff
    pub const fn set_x_scrolloff(&mut self, x: u8) -> &mut Self {
        self.scrolloff = ScrollOff { y: self.scrolloff.y, x };
        self
    }

    /// Sets the vertical scrolloff
    pub const fn set_y_scrolloff(&mut self, y: u8) -> &mut Self {
        self.scrolloff = ScrollOff { y, x: self.scrolloff.x };
        self
    }

    /// Sets the [`WordChars`]
    pub const fn set_word_chars(&mut self, word_chars: WordChars) -> &mut Self {
        self.word_chars = word_chars;
        self
    }

    /// Sets a forced horizontal scrolloff
    ///
    /// Without forced horizontal scrolloff, when you reach the end of
    /// a long line of text, the cursor will also reach the edge of
    /// the screen. With this enabled, Duat will keep a distance
    /// between the cursor and the edge of the screen.
    ///
    /// This is particularly useful in a situation like the
    /// [`PromptLine`] widget, in order to keep good visibility of the
    /// command.
    ///
    /// [`PromptLine`]: docs.rs/duat-utils/latest/duat_utils/widgets/struct.PromptLine.html
    pub const fn set_forced_horizontal_scrolloff(&mut self, value: bool) -> &mut Self {
        self.force_scrolloff = value;
        self
    }

    ////////// Queries

    /// What the wrap width should be, given an area of a certain
    /// width
    ///
    /// If there is no cap ([`WrapMethod::NoWrap`]), returns [`None`]
    #[inline]
    pub const fn wrap_width(&self, width: u32) -> Option<u32> {
        match self.wrap_method {
            WrapMethod::Edge | WrapMethod::Word => Some(width),
            WrapMethod::Capped(cap) => Some(cap as u32),
            WrapMethod::NoWrap => None,
        }
    }

    /// The default used in files and other such inputs
    pub const fn default_for_input() -> Self {
        Self {
            wrap_method: WrapMethod::NoWrap,
            indent_wrapped: true,
            tab_stops: TabStops(4),
            new_line: NewLine::AlwaysAs(' '),
            scrolloff: ScrollOff { x: 3, y: 3 },
            word_chars: WordChars::default(),
            force_scrolloff: false,
            show_ghosts: true,
            allow_overscroll: true,
        }
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
