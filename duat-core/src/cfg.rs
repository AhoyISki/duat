use std::{ops::RangeInclusive, sync::LazyLock};

use regex_automata::meta::Regex;

/// If and how to wrap lines at the end of the screen.
#[derive(Clone, Copy, Debug)]
pub enum WrapMethod {
    Width,
    Capped(u8),
    Word,
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

    pub fn cap(&self, width: usize) -> usize {
        match self {
            WrapMethod::Capped(cap) => *cap as usize,
            _ => width,
        }
    }
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Clone, Copy, Debug)]
pub struct TabStops(pub u8);

impl TabStops {
    pub fn size(&self) -> u32 {
        self.0 as u32
    }

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
    /// Don't print anything for a new line character.
    Hidden,
}

impl NewLine {
    #[inline]
    pub fn char(&self, last_char: Option<char>) -> Option<char> {
        match self {
            NewLine::AlwaysAs(char) => Some(*char),
            NewLine::AfterSpaceAs(char) => {
                if last_char.is_some_and(|char| char.is_whitespace() && char != '\n') {
                    Some(*char)
                } else {
                    Some(' ')
                }
            }
            NewLine::Hidden => None,
        }
    }
}

// Pretty much only exists because i wanted one of these with
// usize as its builtin type.
#[derive(Clone, Copy, Debug)]
pub struct ScrollOff {
    x: u8,
    y: u8,
}

impl ScrollOff {
    #[inline]
    pub fn x(&self) -> u32 {
        self.x as u32
    }

    #[inline]
    pub fn y(&self) -> u32 {
        self.y as u32
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WordChars(&'static LazyLock<(Regex, &'static [RangeInclusive<char>])>);

impl WordChars {
    pub const fn default() -> Self {
        static REGEX: LazyLock<(Regex, &'static [RangeInclusive<char>])> = LazyLock::new(|| {
            (Regex::new("[A-Za-z0-9_]").unwrap(), &[
                'A'..='Z',
                'a'..='z',
                '0'..='9',
                '_'..='_',
            ])
        });
        WordChars(&REGEX)
    }

    /// Checks if a `char` is a word char
    #[inline]
    pub fn contains(&self, char: char) -> bool {
        let mut bytes = [0; 4];
        let str = char.encode_utf8(&mut bytes);

        self.0.0.is_match(str as &str)
    }

    pub fn ranges(&self) -> &'static [RangeInclusive<char>] {
        self.0.1
    }
}

/// Configuration options for printing.
#[derive(Clone, Copy, Debug)]
pub struct PrintCfg {
    /// How to wrap the file
    pub wrap_method: WrapMethod,
    /// Whether to indent wrapped lines or not
    pub indent_wrap: bool,
    /// Which places are considered a "tab stop"
    pub tab_stops: TabStops,
    /// Whether (and how) to show new lines
    pub new_line: NewLine,
    /// How much space to keep between the cursor and edges
    pub scrolloff: ScrollOff,
    // NOTE: This is relevant for printing with `WrapMethod::Word`
    /// Characters that are considered to be part of a word
    pub word_chars: WordChars,
    /// Whether to limit scrolloff on the end of lines
    pub force_scrolloff: bool,
    /// Wheter to show ghosts
    pub show_ghosts: bool,
}

impl PrintCfg {
    pub const fn new() -> Self {
        Self {
            wrap_method: WrapMethod::NoWrap,
            indent_wrap: true,
            tab_stops: TabStops(4),
            new_line: NewLine::Hidden,
            scrolloff: ScrollOff { x: 3, y: 3 },
            word_chars: WordChars::default(),
            force_scrolloff: false,
            show_ghosts: true,
        }
    }

    ////////// Modification functions

    pub const fn unwrapped(self) -> Self {
        Self { wrap_method: WrapMethod::NoWrap, ..self }
    }

    pub const fn width_wrapped(self) -> Self {
        Self { wrap_method: WrapMethod::Width, ..self }
    }

    pub const fn word_wrapped(self) -> Self {
        Self { wrap_method: WrapMethod::Word, ..self }
    }

    pub const fn cap_wrapped(self, cap: u8) -> Self {
        Self {
            wrap_method: WrapMethod::Capped(cap),
            ..self
        }
    }

    pub const fn indent_wrapped(self) -> Self {
        Self { indent_wrap: true, ..self }
    }

    pub const fn tab_sized(self, tab_size: u8) -> Self {
        Self { tab_stops: TabStops(tab_size), ..self }
    }

    pub const fn with_no_new_line(self) -> Self {
        Self { new_line: NewLine::Hidden, ..self }
    }

    pub const fn new_line_as(self, char: char) -> Self {
        Self {
            new_line: NewLine::AlwaysAs(char),
            ..self
        }
    }

    pub const fn trailing_new_line_as(self, char: char) -> Self {
        Self {
            new_line: NewLine::AfterSpaceAs(char),
            ..self
        }
    }

    pub const fn with_scrolloff(self, x: u8, y: u8) -> Self {
        Self { scrolloff: ScrollOff { x, y }, ..self }
    }

    pub const fn with_x_scrolloff(self, x_gap: u8) -> Self {
        Self {
            scrolloff: ScrollOff { y: self.scrolloff.y, x: x_gap },
            ..self
        }
    }

    pub const fn with_y_scrolloff(self, y_gap: u8) -> Self {
        Self {
            scrolloff: ScrollOff { x: self.scrolloff.x, y: y_gap },
            ..self
        }
    }

    pub const fn with_word_chars(self, word_chars: WordChars) -> Self {
        Self { word_chars, ..self }
    }

    pub const fn with_forced_scrolloff(self) -> Self {
        Self { force_scrolloff: true, ..self }
    }

    ////////// Queries

    #[inline]
    pub const fn wrap_width(&self, width: u32) -> u32 {
        match self.wrap_method {
            WrapMethod::Width | WrapMethod::Word => width,
            WrapMethod::Capped(cap) => cap as u32,
            WrapMethod::NoWrap => u32::MAX,
        }
    }

    /// The default used in files and other such inputs
    pub const fn default_for_input() -> Self {
        Self {
            wrap_method: WrapMethod::NoWrap,
            indent_wrap: true,
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

pub macro word_chars {
    (@range $ranges:expr, ) => {{
        static REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(concat!($ranges , "]")).unwrap());
        WordChars(&REGEX)
    }},
    (@range $ranges:expr, $start:literal - $end:literal $($rest:tt)*) => {{
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
        word_chars!(@range concat!($ranges, $start, "-", $end), $($rest)*)
    }},
    (@range $ranges:expr, $($rest:tt)*) => {
        compile_error!("The syntax must be a sequence of \"{char}-{char}\"s")
    },
    ($($ranges:tt)+) => {
        word_chars!(@range "[", $($ranges)+)
    }
}
