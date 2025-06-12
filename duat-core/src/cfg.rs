//! General printing options for printing [`File`]s
//!
//! This is essentially the things that you'd expect to change in a
//! text editor such as Neovim or Kakoune. They are contained in a
//! [`PrintCfg`] struct, which is very light and cheap to copy around,
//! and is used not only by the [`File`], but by every other
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
//! [`File`]: crate::file::File
//! [`Widget`]: crate::widget::Widget
//! [`Area`]: crate::ui::Ui::Area
//! [forced_scrolloff]: PrintCfg::with_forced_horizontal_scrolloff
//! [show_ghosts]: PrintCfg::show_ghosts
use std::{ops::RangeInclusive, sync::LazyLock};

use regex_automata::meta::Regex;

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
        word_chars!('A'-'Z''a'-'z''0'-'9''_'-'_')
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
pub struct PrintCfg {
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
}

impl PrintCfg {
    /// The default [`PrintCfg`]
    ///
    /// There is, essentially, almost no reason to deviate from this
    /// in any [`Widget`] other than a [`File`], since those most
    /// likely will only be printed with the [default `PrintInfo`]
    /// from a [`RawArea`], i.e., no scrolling is involved, and you
    /// should usually strive to control the other elements of
    /// [`Text`]s that the options of a [`PrintCfg`] will want to
    /// change.
    ///
    /// The lack of need to customize this is reflected in
    /// [`Widget::print_cfg`], which calls this function by default.
    /// However, in a [`File`], you'll probably want to look at the
    /// options below.
    ///
    /// The default value is:
    ///
    /// ```rust
    /// use duat_core::cfg::*;
    /// PrintCfg {
    ///     wrap_method: WrapMethod::Edge,
    ///     indent_wrapped: true,
    ///     tab_stops: TabStops(4),
    ///     new_line: NewLine::AlwaysAs('\n'),
    ///     scrolloff: ScrollOff { x: 3, y: 3 },
    ///     word_chars: word_chars!('A'-'Z''a'-'z''0'-'9''_'-'_'),
    ///     force_scrolloff: false,
    ///     show_ghosts: true,
    /// };
    /// ```
    ///
    /// [`Widget`]: crate::widget::Widget
    /// [`File`]: crate::file::File
    /// [default `PrintInfo`]: crate::ui::RawArea::PrintInfo
    /// [`RawArea`]: crate::ui::RawArea
    /// [`Text`]: crate::text::Text
    /// [`Widget::print_cfg`]: crate::widget::Widget::print_cfg
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
        }
    }

    ////////// Configuration

    /// Don't wrap when reaching the end of the area
    pub const fn unwrapped(self) -> Self {
        Self { wrap_method: WrapMethod::NoWrap, ..self }
    }

    /// Wrap on the right edge of the area
    pub const fn edge_wrapped(self) -> Self {
        Self { wrap_method: WrapMethod::Edge, ..self }
    }

    /// Wrap on [word] terminations
    ///
    /// [word]: word_chars
    pub const fn word_wrapped(self) -> Self {
        Self { wrap_method: WrapMethod::Word, ..self }
    }

    /// Wrap on a given distance from the left edge
    ///
    /// This can wrap beyond the screen, being a mix of [`unwrapped`]
    /// and [`edge_wrapped`].
    ///
    /// [`unwrapped`]: Self::unwrapped
    /// [`edge_wrapped`]: Self::edge_wrapped
    pub const fn cap_wrapped(self, cap: u8) -> Self {
        Self {
            wrap_method: WrapMethod::Capped(cap),
            ..self
        }
    }

    /// Reindent wrapped lines to the same level of indentation
    pub const fn indent_wraps(self) -> Self {
        Self { indent_wrapped: true, ..self }
    }

    /// Sets the size of tabs
    pub const fn with_tabstop(self, tab_size: u8) -> Self {
        Self { tab_stops: TabStops(tab_size), ..self }
    }

    /// Sets a character to replace `'\n'`s with
    pub const fn new_line_as(self, char: char) -> Self {
        Self {
            new_line: NewLine::AlwaysAs(char),
            ..self
        }
    }

    /// Sets a character to replace `'\n'` only with trailing white
    /// space
    pub const fn trailing_new_line_as(self, char: char) -> Self {
        Self {
            new_line: NewLine::AfterSpaceAs(char),
            ..self
        }
    }

    /// Sets the horizontal and vertical scrolloff, respectively
    pub const fn with_scrolloff(self, x: u8, y: u8) -> Self {
        Self { scrolloff: ScrollOff { x, y }, ..self }
    }

    /// Sets the horizontal scrolloff
    pub const fn with_x_scrolloff(self, x_gap: u8) -> Self {
        Self {
            scrolloff: ScrollOff { y: self.scrolloff.y, x: x_gap },
            ..self
        }
    }

    /// Sets the vertical scrolloff
    pub const fn with_y_scrolloff(self, y_gap: u8) -> Self {
        Self {
            scrolloff: ScrollOff { x: self.scrolloff.x, y: y_gap },
            ..self
        }
    }

    /// Sets the [`WordChars`]
    pub const fn with_word_chars(self, word_chars: WordChars) -> Self {
        Self { word_chars, ..self }
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
    pub const fn with_forced_horizontal_scrolloff(self) -> Self {
        Self { force_scrolloff: true, ..self }
    }

    ////////// Queries

    /// What the wrap width should be, given an area of a certain
    /// width
    #[inline]
    pub const fn wrap_width(&self, width: u32) -> u32 {
        match self.wrap_method {
            WrapMethod::Edge | WrapMethod::Word => width,
            WrapMethod::Capped(cap) => cap as u32,
            WrapMethod::NoWrap => u32::MAX,
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
        }
    }
}

impl Default for PrintCfg {
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
/// # use duat_core::cfg::word_chars;
/// let word_chars = word_chars!('a'-'z''A'-'Z''0'-'9''-'-'-''_'-'_');
/// ```
// TODO: Deal with characters that need to be escaped.
// TODO: Probably turn this into a proc-macro that acts on strings.
pub macro word_chars {
    (@range $regex:expr, [$($slice:tt)*],) => {{
        static MATCHERS: LazyLock<(Regex, &'static [RangeInclusive<char>])> =
            LazyLock::new(|| (
                Regex::new(concat!($regex , "]")).unwrap(),
                &[$($slice)*]
            ));
        WordChars(&MATCHERS)
    }},
    (@range $regex:expr, [$($slice:tt)*], $start:literal - $end:literal $($rest:tt)*) => {{
        const {
            assert!($start <= $end, concat!("\"", $start, "-", $end, "\" is not a valid range."));
            assert!(
                !($start <= ' ' && ' ' <= $end),
                concat!("\"", $start, "-", $end, "\" contains ' '.")
            );
            assert!(
                !($start <= '\n' && '\n' <= $end),
                concat!("\"", $start, "-", $end, "\" contains '\\n'.")
            );
            assert!(
                !($start <= '\t' && '\t' <= $end),
                concat!("\"", $start, "-", $end, "\" contains '\\t '.")
            );
        }
        word_chars!(@range
            concat!($regex, $start, "-", $end),
            [$start..=$end, $($slice)*], $($rest)*
        )
    }},
    (@range $regex:expr, $($rest:tt)*) => {
        compile_error!("The syntax must be a sequence of \"{char}-{char}\"s")
    },
    ($($start:literal-$end:literal)+) => {
        word_chars!(@range "[", [], $($start-$end)+)
    }
}
